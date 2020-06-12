{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Token.Parse where

import Data.Char
import           Control.Monad
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Token.Token

type Parser t = Parsec Void Text t

parseMonkeyTokens :: Text -> Either (ParseErrorBundle Text Void) [MonkeyToken]
parseMonkeyTokens = runParser (manyTill monkeyTokenParser eof) ""

monkeyTokenParser :: Parser MonkeyToken
monkeyTokenParser =
  Eof <$ eof
  <|> Assign <$ stringS "="
  <|> Plus <$ stringS "+"
  <|> Comma <$ stringS ","
  <|> Semicolon <$ charS ';'
  <|> Lparen <$ stringS "("
  <|> Rparen <$ stringS ")"
  <|> Lbrace <$ stringS "{"
  <|> Rbrace <$ stringS "}"
  <|> Function <$ stringS "fn"
  <|> Let <$ stringS "let"
  <|> Ident <$> identParser
  <|> Int <$> intParser
  <|> Illegal <$ anySingle

stringS :: Text -> Parser Text
stringS s = do
    result <- string s
    space
    pure s

charS :: Char -> Parser Char
charS s = do
    result <- char s
    space
    pure s
    -- L.lexeme L.string
    -- try space *> string s

identParser :: Parser Text
identParser = do
  first :: Char <- letterChar
  rest :: [Char] <- many alphaNumChar 
  space
  pure $ T.pack (first : rest) 

intParser :: Parser Integer
intParser = read <$> some digitChar