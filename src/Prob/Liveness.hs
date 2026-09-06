module Prob.Liveness (liveIn, liveIns) where

import Data.Foldable
import qualified Data.Set as Set
import Prob.CoreAST

--------------------------------------------------------------------------------
-- Liveness
--------------------------------------------------------------------------------

-- | Ordinary backward liveness: the live-in set of a statement, given its
-- live-out set.
liveIn :: (Ord vt) => Stmt vt -> Set.Set vt -> Set.Set vt
liveIn (x := e) out = Set.delete x out `Set.union` Set.fromList (toList e)
liveIn (x :~ _) out = Set.delete x out
liveIn (Observe e) out = out `Set.union` Set.fromList (toList e)
liveIn (If e s1 s2) out = Set.unions [Set.fromList (toList e), liveIns s1 out, liveIns s2 out]
liveIn (While _ e body) out = go live0
  where
    live0 = out `Set.union` Set.fromList (toList e)
    go l =
      let next = liveIns body l `Set.union` live0
       in if next == l then l else go next
liveIn (Par _ blocks) out = foldr liveIns out blocks

-- | 'liveIn' for a block of statements.
liveIns :: (Ord vt) => [Stmt vt] -> Set.Set vt -> Set.Set vt
liveIns ss out = foldr liveIn out ss
