{-# LANGUAGE GADTs #-}
module Prob.CoreOpt
  ( optimizeProgram,
  )
where

import Control.Monad.Trans.State.Strict
import Data.Foldable
import qualified Data.Set as Set
import Prob.CoreAST
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as MM

-- | Optimize a program so that the inference runs faster. We do constant
-- propagation to eliminate some variables, and hopefully get the loop footprint
-- smaller. Afterwards, we also remove statements that write to variables that
-- are unnecessary, starting backwards from the returned variables. The idea is
-- that if not all variables are returned, then we can work backwards from the
-- returned variable to remove all other variables that do not influence the
-- value of the returned variable. For loops however, we insert assignments to
-- dead variables (always False) at the end of the loop as well as just before
-- the loop to keep the number of program states small.
optimizeProgram :: (Show vt, Ord vt) => Prog r vt -> Prog r vt
optimizeProgram = sliceProgram . substituteProgram

type Live vt = Set.Set vt

-- | Slice a program, simplifying it by removing statements that do not matter.
sliceProgram :: Ord vt => Prog r vt -> Prog r vt
sliceProgram (ReturnMult s es) = ReturnMult (evalState (sliceStmts s) (Set.fromList (foldMap toList es))) es
sliceProgram (ReturnAll s) = ReturnAll (evalState (sliceStmts s) (Set.fromList (foldMap toList s)))

sliceStmts :: Ord vt => [Stmt vt] -> State (Live vt) [Stmt vt]
sliceStmts = foldrM step []
  where
    step s kept = (++ kept) <$> sliceStmt s

sliceStmt :: Ord vt => Stmt vt -> State (Live vt) [Stmt vt]
sliceStmt s =
  case s of
    -- An observe changes the normalization of the whole program, so it is
    -- always kept.
    Observe e ->
      modify' (`Set.union` Set.fromList (toList e)) >> pure [s]
    -- Every loop is kept. A loop that might diverge removes probability mass
    -- conditioned on the state it was entered in, so deleting it changes the
    -- answer even when nothing reads what it writes: @x ~ bernoulli 0.5; while
    -- x do {}; return x@ answers "false with probability 1", but would answer
    -- uniform were the loop sliced away.
    While o e body -> do
      slicedBody <- sliceLoop e body
      let loopVars = Set.unions (Set.fromList (toList e) : map (Set.fromList .  toList) slicedBody)
      resetVars <- gets (loopVars `Set.difference`)
      let resetStmts = [v := Constant False | v <- Set.toList resetVars]
      pure (resetStmts ++ [While o e (slicedBody ++ resetStmts)])
    x := e -> do
      isMember <- gets (x `Set.member`)
      if isMember
        then modify' ((`Set.union` Set.fromList (toList e)) . Set.delete x) >> pure [s]
        else pure []
    x :~ _ -> do
      isMember <- gets (x `Set.member`)
      if isMember
        then modify' (Set.delete x) >> pure [s]
        else pure []
    If e s1 s2 -> do
      live <- get
      let (k1, l1) = runState (sliceStmts s1) live
          (k2, l2) = runState (sliceStmts s2) live
      if null k1 && null k2
        then pure []
        else put (Set.unions [Set.fromList (toList e), l1, l2]) >> pure [If e k1 k2]

sliceLoop :: Ord vt => Expr vt -> [Stmt vt] -> State (Live vt) [Stmt vt]
sliceLoop guard body = do
  live0 <- gets (`Set.union` Set.fromList (toList guard))
  let go = do
        prevLive <- get
        r <- sliceStmts body
        modify' (`Set.union` live0)
        nextLive <- get
        if prevLive == nextLive -- Fixed point reached.
          then pure r
          else go
  go


type KnownConstant vt = M.Map vt Bool

initialKnownConstant :: Ord vt => Prog r vt -> KnownConstant vt
initialKnownConstant = M.fromSet (const False) . Set.fromList . toList

substituteProgram :: Ord vt => Prog r vt -> Prog r vt
substituteProgram p@(ReturnAll s) = evalState (ReturnAll <$> substituteStmts s) (initialKnownConstant p)
substituteProgram p@(ReturnMult s es) = evalState (ReturnMult <$> substituteStmts s <*> traverse substituteExpr es) (initialKnownConstant p)

substituteExpr :: Ord vt => Expr vt -> State (KnownConstant vt) (Expr vt)
substituteExpr (a `And` b) = mkAnd <$> substituteExpr a <*> substituteExpr b
substituteExpr (a `Or` b) = mkOr <$> substituteExpr a <*> substituteExpr b
substituteExpr (a `Xor` b) = mkXor <$> substituteExpr a <*> substituteExpr b
substituteExpr (Not a) = mkNot <$> substituteExpr a
substituteExpr expr@Constant{} = pure expr
substituteExpr expr@(Var v) = do
  value <- gets (M.lookup v)
  case value of
    Nothing -> pure expr
    Just b -> pure (Constant b)

substituteStmts :: Ord vt => [Stmt vt] -> State (KnownConstant vt) [Stmt vt]
substituteStmts = (concat <$>) . traverse substituteStmt

substituteStmt :: Ord vt => Stmt vt -> State (KnownConstant vt) [Stmt vt]
substituteStmt (v := expr) = do
  expr' <- substituteExpr expr
  case expr' of
    Constant b -> modify' (M.insert v b)
    _ -> modify' (M.delete v)
  pure [v := expr']
substituteStmt (v :~ Bernoulli 0) =
  modify' (M.insert v False) >> pure [v := Constant False]
substituteStmt (v :~ Bernoulli 1) =
  modify' (M.insert v True) >> pure [v := Constant True]
substituteStmt stmt@(v :~ _) =
  modify' (M.delete v) >> pure [stmt]
substituteStmt (Observe expr) = do
  expr' <- substituteExpr expr
  case expr' of
    Constant True -> pure []
    _ -> pure [Observe expr']
substituteStmt (If expr s1 s2) = do
  expr' <- substituteExpr expr
  case expr' of
    Constant True -> substituteStmts s1
    Constant False -> substituteStmts s2
    _ -> do
      orig <- get
      let trueState = assuming expr' True orig
          falseState = assuming expr' False orig
          (s1', out1) = runState (substituteStmts s1) trueState
          (s2', out2) = runState (substituteStmts s2) falseState
          newState = MM.merge MM.dropMissing MM.dropMissing (MM.zipWithMaybeMatched (\_ b1 b2 -> if b1 == b2 then Just b1 else Nothing)) out1 out2
      put newState
      pure [If expr' s1' s2']
substituteStmt (While o expr s) = do
  firstIterExpr <- substituteExpr expr
  case firstIterExpr of
    Constant False -> pure []
    _ -> do
      modify' (`M.withoutKeys` variablesWritten s)
      expr' <- substituteExpr expr
      beforeStmt <- get
      -- The body may assume the guard held on entry.
      modify' (assuming expr' True)
      s' <- substituteStmts s
      -- Now we have constants established within the loop, but we cannot use it
      -- in case the loop runs zero times.
      put beforeStmt
      -- After the loop the guard came out false.
      modify' (assuming expr' False)
      pure [While o expr' s']

-- | The variable assignments that must hold whenever the expression evaluates
-- to the given value: a conjunction being true pins every conjunct, a
-- disjunction being false pins every disjunct, and a negation flips the sense.
-- An unsatisfiable expression may pin a variable both ways; either constant is
-- then fine, because the code under the assumption can never run.
assuming :: Ord vt => Expr vt -> Bool -> KnownConstant vt -> KnownConstant vt
assuming (Var v) b = M.insert v b
assuming (Not e) b = assuming e (not b)
assuming (a `And` b) True = assuming a True . assuming b True
assuming (a `Or` b) False = assuming a False . assuming b False
assuming _ _ = id

-- | Find variables that are assigned to in statements.
variablesWritten :: Ord vt => [Stmt vt] -> Set.Set vt
variablesWritten = foldMap go
  where
    go (v := _) = Set.singleton v
    go (v :~ _) = Set.singleton v
    go (If _ s1 s2) = variablesWritten s1 <> variablesWritten s2
    go (While _ _ s) = variablesWritten s
    go (Observe _) = Set.empty
