{-# LANGUAGE OverloadedStrings #-}
module Elle.Parser where

import Elle.LogicExp
import Data.Text (Text, pack, unpack)
import Text.Megaparsec
import qualified Text.Megaparsec.Expr as E
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Monoid ((<>))

type Parser = Parsec Void Text

spaceEater :: Parser ()
spaceEater = L.space C.space1 lineComment blockComment
  where lineComment  = L.skipLineComment "--"
        blockComment = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceEater

symbol :: Text -> Parser Text
symbol = L.symbol spaceEater

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reservedWords :: [Text]
reservedWords = ["bottom"]

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where p = (:) <$> C.letterChar <*> many C.alphaNumChar >>= return . pack
        check x | x `elem` reservedWords = fail ("'" <> unpack x <> "' is a reserved word!")
                | otherwise = return x

operators :: [[E.Operator Parser LE]]
operators
  = [ [ E.InfixL $ Implies <$ symbol "->"
      , E.InfixL $ Implies <$ symbol "→"]
    , [ E.InfixL $ And <$ symbol "&&"
      , E.InfixL $ And <$ symbol "∧"]
    , [ E.InfixL $ Or <$ symbol "||"
      , E.InfixL $ Or <$ symbol "∨"]
    ]

negateParser :: Parser LE
negateParser = foldr1 (.) <$> some singleNot >>= (<$> leTerm)
  where singleNot = try $ symbol "!" <|> symbol "¬" >> return Not

bottomParser :: Parser LE
bottomParser = try $ do
  symbol "⊥" <|> symbol "bottom" <|> symbol "F"
  return Bottom

logicalExpressionParser :: Parser LE
logicalExpressionParser = E.makeExprParser leTerm operators

leTerm :: Parser LE
leTerm = parens logicalExpressionParser
  <|> negateParser
  <|> bottomParser
  <|> Var <$> identifier
