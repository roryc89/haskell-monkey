module Token.Token where

import           Data.Text                  (Text)

data MonkeyToken
    = Illegal Char
    | Ident Text
    | Int Integer
    | Comma
    | Semicolon
    | Lparen
    | Rparen
    | Lbrace
    | Rbrace
    | Function
    | Let
    -- OPERATORS
    | Eq
    | NotEq
    | Lt
    | Gt
    | Plus
    | Assign
    | Minus 
    | Bang
    | Asterix
    | Slash


    deriving(Eq, Show)

