-- | Optimize a program so that the inference runs faster. We do constant and
-- copy propagation to eliminate some variables, and hopefully get the loop
-- footprint smaller. Removing the statements that write to variables no one
-- reads (slicing) lives in "Prob.Alloc", fused with the liveness analysis the
-- register allocation runs anyway.
module Prob.CoreOpt
  ( substituteProgram,
  )
where

import Control.Monad.Trans.State.Strict
import Data.Foldable
import qualified Data.Set as Set
import Prob.CoreAST
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as MM

-- | What a variable is known to equal: a constant or another variable (a copy).
data VarIs vt = VarIsBool Bool | VarIsCopy vt deriving Eq

-- | What each variable is currently known to equal.
type Known vt = M.Map vt (VarIs vt)

initialKnown :: Ord vt => Prog vt -> Known vt
initialKnown = M.fromSet (const (VarIsBool False)) . Set.fromList . toList

-- | Remove the facts invalidated by writing to these variables: their own
-- entries, and any copy that reads from them.
forgetWrites :: Ord vt => Set.Set vt -> Known vt -> Known vt
forgetWrites vs = M.filter notMember . (`M.withoutKeys` vs)
  where notMember (VarIsBool _) = True
        notMember (VarIsCopy x) = x `Set.notMember` vs

substituteProgram :: Ord vt => Prog vt -> Prog vt
substituteProgram p@(ReturnMult s es) = evalState (ReturnMult <$> substituteStmts s <*> traverse substituteExpr es) (initialKnown p)

substituteExpr :: Ord vt => Expr vt -> State (Known vt) (Expr vt)
substituteExpr (a `And` b) = mkAnd <$> substituteExpr a <*> substituteExpr b
substituteExpr (a `Or` b) = mkOr <$> substituteExpr a <*> substituteExpr b
substituteExpr (a `Xor` b) = mkXor <$> substituteExpr a <*> substituteExpr b
substituteExpr (Not a) = mkNot <$> substituteExpr a
substituteExpr expr@Constant{} = pure expr
substituteExpr expr@(Var v) = do
  value <- gets (M.lookup v)
  case value of
    Nothing -> pure expr
    Just (VarIsBool b) -> pure (Constant b)
    -- The source may since have been pinned to a constant by 'assuming', so
    -- look it up too. Copy chains are acyclic, so this terminates.
    Just (VarIsCopy u) -> substituteExpr (Var u)

substituteStmts :: Ord vt => [Stmt vt] -> State (Known vt) [Stmt vt]
substituteStmts = (concat <$>) . traverse substituteStmt

substituteStmt :: Ord vt => Stmt vt -> State (Known vt) [Stmt vt]
substituteStmt (v := expr) = do
  expr' <- substituteExpr expr
  known <- gets (M.lookup v)
  case expr' of
    Var u | u == v -> pure [] -- Assigning a variable to itself does nothing.
          | known == Just (VarIsCopy u) -> pure [] -- Repeated assignment of the same variable.
    Constant c | known == Just (VarIsBool c) -> pure [] -- Repeated assignmnt of the same constant.
    _ -> do
      modify' (forgetWrites (Set.singleton v))
      case expr' of
        Constant b -> modify' (M.insert v (VarIsBool b))
        Var u -> modify' (M.insert v (VarIsCopy u))
        _ -> pure ()
      pure [v := expr']
substituteStmt (v :~ Bernoulli 0) =
  modify' (M.insert v (VarIsBool False) . forgetWrites (Set.singleton v)) >> pure [v := Constant False]
substituteStmt (v :~ Bernoulli 1) =
  modify' (M.insert v (VarIsBool True) . forgetWrites (Set.singleton v)) >> pure [v := Constant True]
substituteStmt stmt@(v :~ _) =
  modify' (forgetWrites (Set.singleton v)) >> pure [stmt]
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
      modify' (forgetWrites (variablesWritten s))
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
assuming :: Ord vt => Expr vt -> Bool -> Known vt -> Known vt
assuming (Var v) b = M.insert v (VarIsBool b)
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
