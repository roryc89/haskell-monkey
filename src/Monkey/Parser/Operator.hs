{-# LANGUAGE OverloadedStrings #-}

module Monkey.Parser.Operator where

import Control.Monad.Combinators.Expr
-- import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Text (Text)
import Data.Void
import Monkey.Ast
import qualified Monkey.Token as T
import Monkey.Token (MonkeyToken, MonkeyTokens)
import Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void MonkeyTokens 

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix T.Minus Negate
    , prefix T.Bang Bang
    ]
  , [ binary T.Asterix Times
    , binary T.Slash Divide
    ]
  , [ binary T.Plus Plus
    , binary T.Minus Minus
    ]
  , [ binary T.Gt Gt
    , binary T.Lt Lt
    ]
  , [ binary T.Eq Eq
    , binary T.NotEq NotEq
    ]
  ]

binary :: MonkeyToken -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  token f = InfixL  (f <$ monkeys token)

prefix, postfix :: MonkeyToken -> (Expr -> Expr) -> Operator Parser Expr
prefix  token f = Prefix  (f <$ monkeys token)
postfix token f = Postfix (f <$ monkeys token)

monkeys :: MonkeyToken -> Parser MonkeyTokens
monkeys t = T.wrap <$> single t 

