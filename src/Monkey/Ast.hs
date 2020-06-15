module Monkey.Ast where 

import Data.Text (Text)
import Monkey.Token


data Program = 
    Program 
        { statements :: [Statement] 
        }
        deriving(Show, Eq)

data Node 
    = ExprNode Expr 
    | StatementNode Statement
    deriving(Show, Eq)

data Statement
    = Let Text Expr
    | ExpressionStatement Expr
    | If Expr [Statement] [Statement]
    | FunctionDeclaration [Text] [Statement]
    deriving(Show, Eq)

data Expr
    = Identifier Text
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
