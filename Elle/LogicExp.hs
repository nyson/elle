{-# LANGUAGE GADTs, LambdaCase, MultiWayIf #-}
module Elle.LogicExp
  ( LET(..), LE(..)
  , ppLE
  ) where

import Test.QuickCheck ( Arbitrary(..), Gen(..)
                       , elements, sized, frequency, scale, oneof)
import Data.Text (Text, pack, unpack)

-- Logical Expression types
data LET = AndT     LET LET
         | OrT      LET LET
         | ImpliesT LET LET
         | NotT     LET
         | VarT
         | BottomT
         deriving (Eq, Show)

-- Logical expression
data LE where
  And     :: LE -> LE -> LE
  Or      :: LE -> LE -> LE
  Implies :: LE -> LE -> LE
  Not     :: LE -> LE
  Var     :: Text -> LE
  Bottom  :: LE
  deriving (Eq, Show)

instance Arbitrary LE where
  arbitrary = sized $ \s -> if
    | s < 1 -> frequency
      [ (10, Var <$> identifier)
      , (1,  return Bottom)
      ]
    | otherwise -> scale (`div` 2) $ oneof
      [ And     <$> arbitrary <*> arbitrary
      , Or      <$> arbitrary <*> arbitrary
      , Implies <$> arbitrary <*> arbitrary
      , Not     <$> arbitrary
      ]

identifier :: Gen Text
identifier = (:"") <$> elements "αβγδεζηθικλμνξοπρςστυφχψω" >>= return . pack

ppLE :: LE -> String
ppLE = \case
  Var x  -> unpack x
  Bottom -> "⊥"
  Not e  -> "¬" ++ condParens e
  And a b -> bin a "∧" b
  Or  a b -> bin a "∨" b
  Implies a b -> bin a "→" b

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

typeOf :: LE -> LET
typeOf = \case
  And     a b -> typeOf a `AndT` typeOf b
  Or      a b -> typeOf a `OrT` typeOf b
  Implies a b -> typeOf a `ImpliesT` typeOf b
  Not a -> NotT $ typeOf a
  Var a -> VarT
  Bottom -> BottomT
