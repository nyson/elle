{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
module Elle.Proof where

import Elle.LogicExp

import qualified Control.Monad.State as St
import qualified Data.IntMap.Lazy as M
import           Data.IntMap.Lazy (IntMap)
import qualified Control.Monad.State.Lazy as St

import Debug.Trace

import Control.Monad (unless)

type Proof = IntMap LE

type ProofM = St.State (Row, Proof)

newtype Row = Row {unRow :: Int}
  deriving (Num, Show)

defProof :: (Row, Proof)
defProof = (1, M.empty)

runFresh :: ProofM a -> (a, (Row, Proof))
runFresh = flip St.runState defProof

lookupP :: Row -> ProofM LE
lookupP (Row r) = do
  p <- snd <$> St.get
  case M.lookup r p of
    Just le -> return le
    Nothing -> fail $ show r ++ "That's not a row, that's a space station"

insertP :: LE -> ProofM Row
insertP e = do
  (r, s) <- St.get
  St.put (r + 1, M.insert (unRow r) e s)
  return r

test :: ProofM LE
test = do
  let i = insertP
  r1 <- i $ Var "x"
  r2 <- i $ Var "y"
  e <- lookupP r1
  trace (show e) (return ())
  return e

-- An assumption is a sequence of rows
type Assumption = (Row, Row)

-- And introduction from two rows
andI :: Row -> Row -> ProofM Row
andI r1 r2  = do
  [e1, e2] <- mapM lookupP [r1, r2]
  insertP $ e1 `And` e2

-- And elemination, the first expression remaining
andE1 :: Row -> ProofM Row
andE1 r = lookupP r >>= \case
  e `And` _ -> insertP e
  e         -> fail $ "Can't perform ∧-elimination on non-∧ expression: " ++ show e

-- And elemination, the second expression remaining
andE2 :: Row -> ProofM Row
andE2 r = lookupP r >>= \case
  _ `And` e -> insertP e
  e         -> fail $ "Can't perform ∧-elimination on non-∧ expression: " ++ show e

-- Or introduction of new expression on LHS
orI1 :: LE -> Row -> ProofM Row
orI1 e1 r = lookupP r >>= \e2 -> insertP (e1 `Or` e2)

-- Or introduction of new expression on RHS
orI2 :: Row -> LE -> ProofM Row
orI2 r e2 = lookupP r >>= \e1 -> insertP (e1 `Or` e2)

orE :: Row -> Assumption -> Assumption -> ProofM Row
orE r (a1s, a1e) (a2s, a2e) = do
  e <- lookupP r
  unless (isOr e) $ fail (notOr e)

  [e1s, e1e, e2s, e2e] <- mapM lookupP [a1s, a1e, a2s, a2e]

  let el `Or` er = e
  unless (e1s == el && e2s == er)
    $ fail (assumptionMismatch e e1s e2s)

  unless (e1e == e2e)
    $ fail (conclusionMismatch e1e e2e)

  insertP e1e
  where notOr exp
          = concat ["'", show exp, "' isn't an ∨ expression!"]
        assumptionMismatch exp ass1 ass2
          = concat [ "'", show exp
                   , "' have to match the start of the assumptions!"
                   , "\n\tl: '", show ass1, "'"
                   , "\n\tr: '", show ass2, "'"
                   ]
        conclusionMismatch con1 con2
          = concat [ " The conclusions have to match!"
                   , "\n\tl: '", show con1, "'"
                   , "\n\tr: '", show con2, "'"
                   ]
