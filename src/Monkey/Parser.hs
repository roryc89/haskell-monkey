{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Monkey.Parser where 

import Debug.Trace
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
parseProgram = parseTokensThenRunParser statementsParser

parseTokensThenRunParser :: Parsec Void MonkeyTokens a -> Text -> Either ErrorContainer a
parseTokensThenRunParser parser input = 
    case parseMonkeyTokens input of 
    Left tokenErrs -> Left $ TextParseErrors tokenErrs
    Right tokens -> case runParser parser "" tokens of 
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
    , ifParser
    , functionDeclarationParser
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

ifParser :: Parser Statement
ifParser = do
    single $ T.Ident "if"
    condition <- betweenParens exprParser
    consequence <- blockParserMany
    else_ <- optional $ single $ T.Ident "else"
    case else_ of 
        Nothing -> pure $ If condition consequence []
        Just _ -> do
            alternative <- blockParserMany
            pure $ If condition consequence alternative

blockParserMany ::  Parser [Statement]
blockParserMany = blockParser (many statementParser)

blockParser :: Parser [Statement] -> Parser [Statement]
blockParser p = between (single T.Lbrace) (single T.Rbrace) p

functionDeclarationParser :: Parser Statement
functionDeclarationParser = do 
    single T.Function 
    args <- betweenParens $ sepBy identParser (single T.Comma)
    body <- blockParserMany
    pure $ FunctionDeclaration args body

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
parens p = Parens <$> betweenParens p

betweenParens :: Parser a -> Parser a
betweenParens p = between (single T.Lparen) (single T.Rparen) p

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