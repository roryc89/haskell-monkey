{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Monkey.Parser where 

import Control.Monad.Combinators.Expr
import Control.Monad
import Data.Text (Text)
import Data.Void (Void)
import Monkey.Ast
import Monkey.Lexer (parseMonkeyTokens)
import Monkey.Parser.Operator (operatorTable)
import Monkey.Token (MonkeyTokens)
import qualified Monkey.Token as T
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char

type Parser = Parsec Void MonkeyTokens

parseProgram :: Text -> Either ErrorContainer [Statement]
parseProgram input = 
    case parseMonkeyTokens input of 
    Left tokenErrs -> Left $ TextParseErrors tokenErrs
    Right tokens -> case parseAst tokens of 
        Left tokenErrs -> Left $ TokenParseErrors tokenErrs
        Right statements -> Right statements

data TokenParseError
    = TokenParseError
    deriving(Eq, Show)

parseAst :: MonkeyTokens -> Either (ParseErrorBundle MonkeyTokens Void) [Statement]
parseAst = runParser statementsParser ""

statementsParser :: Parser [Statement]
statementsParser = manyTill statementParser eof

statementParser :: Parser Statement
statementParser = 
  choice 
    [ letParser
    , expressionStatementParser
    ]

letParser :: Parser Statement
letParser = do 
    single T.Let 
    ident <- identParser
    single T.Assign 
    expr <- exprParser
    single T.Semicolon  
    pure $ Let ident expr 

expressionStatementParser :: Parser Statement
expressionStatementParser = do
    e <- exprParser
    single T.Semicolon  
    pure $ ExpressionStatement e

identParser :: Parser Text
identParser = 
    tokenNoErr
        \t -> case t of
            T.Ident t -> Just t
            _ -> Nothing

exprParser :: Parser Expr
exprParser = makeExprParser exprTerm operatorTable

exprTerm :: Parser Expr
exprTerm = choice $
  [ intParser
  , prefixParser T.Bang Bang
  , prefixParser T.Minus MinusPrefix
  , Identifier <$> identParser
  ]

-- infixParsers = map (\(tok, operator) -> infixParser tok operator)
--     [ (T.Plus, Plus)
--     , (T.Minus, Minus)
--     , (T.Asterix, Times)
--     , (T.Gt, Gt)
--     , (T.Lt, Lt)
--     , (T.Eq, Eq)
--     , (T.NotEq, NotEq)
--     ]

prefixParser :: 
    T.MonkeyToken
    -> (Expr -> a)
    -> Parser a
prefixParser token prefix = do 
    single token
    prefix <$> exprParser

infixParser :: 
    T.MonkeyToken
    -> (Expr -> Expr -> a)
    -> Parser a
infixParser token operator = do
    left <- exprParser
    single token
    right <- exprParser
    pure $ operator left right

intParser :: Parser Expr
intParser = 
    tokenNoErr
        \t -> case t of
            T.Int i -> Just $ Int i
            _ -> Nothing

tokenNoErr :: (Token MonkeyTokens -> Maybe a) -> Parser a
tokenNoErr test = token test mempty

data ErrorContainer 
    = TokenParseErrors  (ParseErrorBundle MonkeyTokens Void)
    | TextParseErrors (ParseErrorBundle Text Void)
    deriving(Eq, Show)

-- operatorTable :: [[Operator Parser Expr]]
-- operatorTable = undefined -- TODO