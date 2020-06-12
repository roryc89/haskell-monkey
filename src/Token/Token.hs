module Token.Token where
    
import           Data.Text                  (Text)

data MonkeyToken
    = Illegal
    | Eof
    | Ident Text
    | Int Integer
    | Assign
    | Plus
    | Comma
    | Semicolon
    | Lparen
    | Rparen
    | Lbrace
    | Rbrace
    | Function
    | Let
    deriving(Eq, Show)

