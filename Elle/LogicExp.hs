{-# LANGUAGE GADTs, DataKinds, LambdaCase, MultiWayIf #-}
module Elle.LogicExp
  ( LET(..), LE(..)
  , isAnd, isOr, isImplies, isNot, isVar
  ) where

import Test.QuickCheck ( Arbitrary(..), Gen(..)
                       , elements, sized, frequency, scale, oneof)
import Data.Text (Text, pack, unpack)

-- Logical Expression types
data LET = AndT LET LET
         | OrT LET LET
         | NotT LET
         | ImpliesT LET LET
         | VarT
         | LitT
         | VoidT

-- Logical expression
data LE where
  And     :: LE -> LE -> LE
  Or      :: LE -> LE -> LE
  Implies :: LE -> LE -> LE
  Not     :: LE -> LE
  Var     :: Text -> LE
  Bottom  :: LE
  deriving (Eq, Ord)

instance Show LE where
  show = ppLE

identifier :: Gen Text
identifier = (:"") <$> elements "αβγδεζηθικλμνξοπρςστυφχψω" >>= return . pack

instance Arbitrary LE where
  arbitrary = sized $ \s -> if
    | s < 1 -> frequency [
        (10, Var <$> identifier)
        , (1,  return Bottom)
        ]
    | otherwise -> scale (`div` 2) $ oneof [
        And <$> arbitrary <*> arbitrary
        , Or <$> arbitrary <*> arbitrary
        , Implies <$> arbitrary <*> arbitrary
        , Not <$> arbitrary
        ]

isAnd :: LE -> Bool
isAnd (And _ _) = True
isAnd _ = False

isOr :: LE -> Bool
isOr (Or _ _) = True
isOr _ = False

isImplies :: LE -> Bool
isImplies (Implies _ _) = True
isImplies _ = False

isNot :: LE -> Bool
isNot (Not _) = True
isNot _ = False

isVar :: LE -> Bool
isVar (Var _) = True
isVar _ = False

ppLE :: LE -> String
ppLE = \case
  Var x -> unpack x
  Bottom -> "⊥"
  Not e -> "¬" ++ condParens e
  And a b -> bin a "∧" b
  Or  a b -> bin a "∨" b
  a `Implies` b -> bin a "→" b

  where
    -- Binary operations formatting
    bin :: LE -> String -> LE -> String
    bin a dec b = concat [condParens a, " ", dec, " ", condParens b]

    -- add parenthesis if the expression is complex enough
    condParens :: LE -> String
    condParens e | simple e = ppLE e
                 | otherwise = concat ["(", ppLE e, ")"]

    -- simple expressions doesn't need parens
    simple :: LE -> Bool
    simple = \case
      Var _  -> True
      Bottom -> True
      Not _  -> True
      _      -> False

-- type Assumption = [(String, LE)]

-- -- type Proof = [(Int, LE)]
-- type ProofM = IO
-- type IBox a b = a -> b

-- andI :: LE -> LE -> ProofM LE
-- andI a b = return $ And a b

-- andE1 :: LE -> ProofM LE
-- andE1 (And a _b) = return a

-- andE2 :: LE -> ProofM LE
-- andE2 (And _a b) = return b

-- orI :: LE -> LE -> ProofM LE
-- orI a b = return $ Or a b

-- orE :: Eq a => LE -> IBox LE a -> IBox LE a -> ProofM a
-- orE (Or a b) b1 b2 | b1 a == b2 b = return $ b1 a

-- impI :: LE -> LE -> ProofM LE
-- impI a b = return $ a `Implies` b

-- impE :: LE -> LE -> ProofM LE
-- impE (a `Implies` b) v | a == v = return b

-- negI :: LE -> ProofM LE
-- negI (a `Implies` Bottom) = return $ Not a

-- negE :: LE -> LE -> ProofM LE
-- negE a b | a == Not b || Not a == b = return Bottom

-- botE :: LE -> LE -> ProofM LE
-- botE Bottom a = return a

-- mt :: LE -> LE -> ProofM LE
-- mt (a `Implies` b) (Not v) | b == v = return $ Not a

-- notnotI :: LE -> ProofM LE
-- notnotI = return . Not . Not

-- pbc :: LE -> ProofM LE
-- pbc (a `Implies` Bottom) = return $ Not a

-- lem :: LE -> ProofM LE
-- lem a = return $ a `Or` Not a
