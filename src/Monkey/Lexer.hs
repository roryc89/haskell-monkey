{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monkey.Lexer where

import Data.Char
import           Control.Monad
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Monkey.Token

type Parser t = Parsec Void Text t

parseMonkeyTokens :: Text -> Either (ParseErrorBundle Text Void) MonkeyTokens
parseMonkeyTokens =  runParser (MonkeyTokens <$> manyTill (L.lexeme sc monkeyTokenParser) eof) ""

sc :: Parser ()
sc = L.space
  space1                        
  (L.skipLineComment "---")      
  (L.skipBlockComment "///*" "*///")
  

monkeyTokenParser :: Parser MonkeyToken
monkeyTokenParser = choice
  [ Eq <$ string "=="
  , NotEq <$ string "!="
  , Newline <$ char '\n'
  , Assign <$ char '='
  , Plus <$ char '+'
  , Comma <$ char ','
  , Semicolon <$ char ';'
  , Lparen <$ char '('
  , Rparen <$ char ')'
  , Lbrace <$ char '{'
  , Rbrace <$ char '}'
  , Minus  <$ char '-'
  , Bang <$ char '!'
  , Asterix <$ char '*'
  , Slash <$ char '/'
  , Lt <$ char '<'
  , Gt <$ char '>'
  , Function <$ string "fn"
  , Let <$ string "let"
  , Ident <$> identParser
  , Int <$> intParser
  , Illegal <$> anySingle
  ]

identParser :: Parser Text
identParser = do
  first :: Char <- letterChar
  rest :: [Char] <- many alphaNumChar 
  pure $ T.pack (first : rest) 

intParser :: Parser Integer
intParser = read <$> some digitChar