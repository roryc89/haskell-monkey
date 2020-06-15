{-# LANGUAGE OverloadedStrings #-}

module Monkey.Ast where 

import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Foldable


data Program = 
    Program 
        { statements :: [Statement] 
        }
        deriving(Show, Eq)

prettyPrintStatements :: [Statement] -> Text
prettyPrintStatements = T.intercalate "\n" . map prettyPrintStatement

data Node 
    = ExprNode Expr 
    | StatementNode Statement
    deriving(Show, Eq)

data Statement
    = Let Text Expr
    | ExpressionStatement Expr
    | If Expr [Statement] [Statement]
    | Return Expr
    deriving(Show, Eq)

prettyPrintStatement :: Statement -> Text
prettyPrintStatement (Let i e) = 
    "let " +++ i +++ " = " +++ prettyPrintExpr e +++ "; "
prettyPrintStatement (ExpressionStatement e) = 
    prettyPrintExpr e +++ ";"
prettyPrintStatement (If cond cons alt) =
    "if(" +++ prettyPrintExpr cond +++ "){\n" -- +++ foldMap +++ 
        
prettyPrintStatement (Return e) = 
    "return " +++ prettyPrintExpr e +++ ";"


data Expr
    = Identifier Text
    | FunctionDeclaration [Text] [Statement]
    | CallExpression Expr [Expr]
    | Parens Expr
    | BoolE Bool
    | Int Integer
    | Bang Expr
    | Negate Expr
    | Plus Expr Expr
    | Minus Expr Expr
    | Times Expr Expr
    | Divide Expr Expr
    | Gt Expr Expr
    | Lt Expr Expr
    | Eq Expr Expr
    | NotEq Expr Expr
    deriving(Show, Eq)

prettyPrintExpr :: Expr -> Text
prettyPrintExpr (Identifier a) = a
prettyPrintExpr (FunctionDeclaration params body) = 
    "fn(" +++ T.intercalate ", " params +++ "){" 
       +++ prettyPrintStatements body
       +++ "\n}"
prettyPrintExpr (CallExpression e args) = 
    prettyPrintExpr e +++ "(" +++  T.intercalate ", " (map prettyPrintExpr args) +++ ")"
prettyPrintExpr (Parens e) = 
    " (" +++ prettyPrintExpr e +++ ") "
prettyPrintExpr (BoolE b) = T.pack $ show b
prettyPrintExpr (Int i) = T.pack $ show i
prettyPrintExpr (Bang e) = " !" +++ prettyPrintExpr e
prettyPrintExpr (Negate i) = " -" +++ prettyPrintExpr i
prettyPrintExpr (Plus l r) = prettyPrintExpr l +++ " + " +++ prettyPrintExpr r
prettyPrintExpr (Minus l r) = prettyPrintExpr l +++ " - " +++ prettyPrintExpr r
prettyPrintExpr (Times l r) = prettyPrintExpr l +++ " * " +++ prettyPrintExpr r
prettyPrintExpr (Divide l r) = prettyPrintExpr l +++ " / " +++ prettyPrintExpr r
prettyPrintExpr (Gt l r) = prettyPrintExpr l +++ " > " +++ prettyPrintExpr r
prettyPrintExpr (Lt l r) = prettyPrintExpr l +++ " < " +++ prettyPrintExpr r
prettyPrintExpr (Eq l r) = prettyPrintExpr l +++ " == " +++ prettyPrintExpr r
prettyPrintExpr (NotEq l r) = prettyPrintExpr l +++ " != " +++ prettyPrintExpr r

infixl 5 +++
(+++) = T.append
