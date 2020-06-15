module Monkey.Object where
    
import Data.Text

data Object 
    = OString Text
    | OInt Integer
    | OBool Bool
    | ONull
    deriving (Eq, Show)

prettyPrintObject :: Object -> String
prettyPrintObject o = case o of 
    OString t -> unpack t
    OInt i -> show i
    OBool b -> show b
    ONull -> "null"