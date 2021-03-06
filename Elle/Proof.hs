{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, OverloadedStrings, GADTs, DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Elle.Proof where

import Elle.LogicExp
import Elle.Parser

import qualified Control.Monad.State as St
import qualified Data.IntMap.Lazy as M
import           Data.IntMap.Lazy (IntMap)
import qualified Control.Monad.State.Lazy as St

import Debug.Trace

import Control.Monad (unless)

newtype Proof = Proof {unProof :: IntMap LE}

instance Show Proof where
  show = ("\n" ++) . ppList . M.toList . unProof
    where ppList ((k, v):xs) = concat [show k, ". ", ppLE v, "\n"]
                               ++ ppList xs
          ppList [] = ""

type ProofM = St.State (Int, Proof)

newtype Row = Row {unRow :: Int}
  deriving (Show)

defProof :: (Int, Proof)
defProof = (1, Proof M.empty)

runFresh :: ProofM a -> (a, (Int, Proof))
runFresh = flip St.runState defProof

lookupP :: Row -> ProofM LE
lookupP (Row r) = do
  Proof p <- snd <$> St.get
  case M.lookup r p of
    Just le -> return le
    Nothing -> fail $ show r ++ "That's not a row, that's a space station"

assume :: LE -> ProofM Row
assume e = do
  (r, Proof s) <- St.get
  St.put (r + 1, Proof $ M.insert r e s)
  return (Row r)

test :: ProofM LE
test = do
  let i = either (error . show) assume . runParser
  r1 <- i "x"
  r2 <- i "y"
  andI r1 r2
  e <- lookupP r1
  trace (show e) (return ())
  return e

-- An assumption is a sequence of rows
data Assumption :: LET -> LET -> * where
  Ass :: Row -> Row -> Assumption a b

-- And introduction from two rows
andI :: Row -> Row -> ProofM Row
andI r1 r2  = do
  e1 <- lookupP r1
  e2 <- lookupP r2
  assume $ e1 `And` e2

-- And elemination, the first expression remaining
andE1 :: Row -> ProofM Row
andE1 r = lookupP r >>= \(e `And` _) -> assume e

-- And elemination, the second expression remaining
andE2 :: Row -> ProofM Row
andE2 r = lookupP r >>= \(_ `And` e) -> assume e

-- Or introduction of new expression on LHS
orI1 :: LE -> Row -> ProofM Row
orI1 e1 r = lookupP r >>= \e2 -> assume (e1 `Or` e2)

-- Or introduction of new expression on RHS
orI2 :: Row -> LE -> ProofM Row
orI2 r e2 = lookupP r >>= \e1 -> assume (e1 `Or` e2)

orE :: Row
    -> Assumption a c
    -> Assumption b c
    -> ProofM Row
orE r (Ass a1s a1e) (Ass a2s a2e) = do
  e <- lookupP r

  e1s <- lookupP a1s
  e1e <- lookupP a1e
  e2s <- lookupP a2s
  e2e <- lookupP a2e

  let el `Or` er = e
  unless (e1s == el && e2s == er)
    $ fail (assumptionMismatch e e1s e2s)

  unless (e1e == e2e)
    $ fail (conclusionMismatch e1e e2e)

  assume e1e
  where assumptionMismatch exp ass1 ass2
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
