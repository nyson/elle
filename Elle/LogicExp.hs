{-# LANGUAGE GADTs, DataKinds, LambdaCase, MultiWayIf #-}
module Elle.LogicExp
  ( LET(..), LE(..)
  , ppLE
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
  show x = concat ["'", ppLE x, "'"]

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
