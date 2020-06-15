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
  [ parens exprParser
  , intParser
  , boolParser
  , Identifier <$> identParser
  ]

parens :: Parser Expr -> Parser Expr
parens p = Parens <$> between (single T.Lparen) (single T.Rparen) p

-- braces :: Parser Expr -> Parser Expr
-- braces = between (single T.Lbrace) (single T.Rbrace)

intParser :: Parser Expr
intParser = 
    tokenNoErr
        \t -> case t of
            T.Int i -> Just $ Int i
            _ -> Nothing

boolParser :: Parser Expr
boolParser = 
    tokenNoErr
        \t -> case t of
            T.Ident "true" -> Just $ BoolE True
            T.Ident "false" -> Just $ BoolE False
            _ -> Nothing

tokenNoErr :: (Token MonkeyTokens -> Maybe a) -> Parser a
tokenNoErr test = token test mempty

data ErrorContainer 
    = TokenParseErrors  (ParseErrorBundle MonkeyTokens Void)
    | TextParseErrors (ParseErrorBundle Text Void)
    deriving(Eq, Show)

-- operatorTable :: [[Operator Parser Expr]]
-- operatorTable = undefined -- TODO