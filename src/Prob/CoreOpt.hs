{-# LANGUAGE GADTs #-}

-- | Optimize a program so that the inference runs faster. This is not available
-- for ReturnAll mode. The idea is that if not all variables are returned, then
-- we can work backwards from the returned variable to remove all other
-- variables that are unnecessary, i.e. do not influence the value of the
-- returned variable. We implement it from starting from the end with the
-- returned variable as the only live variable and working backwards to modify
-- the live set and deleting statements. For loops however, we insert unnecessary
-- assignments inside them. If the loop internally needs a variable but such a
-- variable is not in the live set, it follows that the variable is not needed
-- beyond the loop. Therefore, we can insert statements to reset these variables.
module Prob.CoreOpt
  ( sliceProgram,
  )
where

import Control.Monad.Trans.State.Strict
import Data.Foldable
import qualified Data.Set as Set
import Prob.CoreAST

type Live vt = Set.Set vt

-- | Slice a program, simplifying it by removing statements that do not matter.
-- Takes a list of statements and a set of variables that matter.
sliceProgram :: Ord vt => Prog r vt -> Prog r vt
sliceProgram (Return s e) = Return (evalState (sliceStmts s) (Set.fromList (toList e))) e
sliceProgram p@ReturnAll {} = p

sliceStmts :: Ord vt => [Stmt vt] -> State (Live vt) [Stmt vt]
sliceStmts = foldrM step []
  where
    step s kept = do
      sliced <- sliceStmt s
      pure $ case sliced of
        Nothing -> kept
        Just s' -> s' : kept

sliceStmt :: Ord vt => Stmt vt -> State (Live vt) (Maybe (Stmt vt))
sliceStmt s =
  case s of
    -- An observe changes the normalization of the whole program, so it is
    -- always kept.
    Observe e ->
      modify' (`Set.union` Set.fromList (toList e)) >> pure (Just s)
    -- Every loop is kept. A loop that might diverge removes probability mass
    -- conditioned on the state it was entered in, so deleting it changes the
    -- answer even when nothing reads what it writes: @x ~ bernoulli 0.5; while
    -- x do {}; return x@ answers "false with probability 1", but would answer
    -- uniform were the loop sliced away.
    While o e body -> do
      slicedBody <- sliceLoop e body
      let loopVars = Set.unions (Set.fromList (toList e) : map (Set.fromList . toList) body)
      resetVars <- gets (loopVars `Set.difference`)
      pure (Just (While o e (slicedBody ++ [v := Constant False | v <- Set.toList resetVars])))
    x := e -> do
      isMember <- gets (x `Set.member`)
      if isMember
        then modify' ((`Set.union` Set.fromList (toList e)) . Set.delete x) >> pure (Just s)
        else pure Nothing
    x :~ _ -> do
      isMember <- gets (x `Set.member`)
      if isMember
        then modify' (Set.delete x) >> pure (Just s)
        else pure Nothing
    If e s1 s2 -> do
      live <- get
      let (k1, l1) = runState (sliceStmts s1) live
          (k2, l2) = runState (sliceStmts s2) live
      if null k1 && null k2
        then pure Nothing
        else put (Set.unions [Set.fromList (toList e), l1, l2]) >> pure (Just (If e k1 k2))

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
