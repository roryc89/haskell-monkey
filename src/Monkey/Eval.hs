{-# LANGUAGE OverloadedStrings #-}

module Monkey.Eval where 

import Control.Applicative (liftA2)
import Data.Text (Text)
import qualified Data.Text as T
import Monkey.Ast
import Monkey.Object

type Evald = Either Text Object

eval :: [Statement] -> Evald
eval = evalStatement . head

evalStatement :: Statement -> Evald
evalStatement statement = case statement of 
    ExpressionStatement e -> evalExpr e
    a -> Right ONull

evalExpr :: Expr -> Evald
evalExpr (Identifier a) = Left "TODO"
evalExpr (FunctionDeclaration params body) =  Left "TODO"
evalExpr (CallExpression e args) = Left "TODO"
-- evalExpr e +++ "(" +++  T.intercalate ", " (map evalExpr args) +++ ")"
evalExpr (Parens e) = evalExpr e
-- evalExpr e +++ ") "
evalExpr (BoolE b) = Right $ OBool b
evalExpr (Int i) = Right $ OInt i
evalExpr (Bang e) = boolPreOp not (evalExpr e)
evalExpr (Negate i) = intPreOp negate (evalExpr i)
evalExpr (Plus l r) = intBinOp (+) (evalExpr l) (evalExpr r)
evalExpr (Minus l r) = intBinOp (-) (evalExpr l) (evalExpr r)
evalExpr (Times l r) = intBinOp (*) (evalExpr l) (evalExpr r)
evalExpr (Divide l r) = intBinOp div (evalExpr l) (evalExpr r)
evalExpr (Gt l r) = intBoolBinOp (>) (evalExpr l) (evalExpr r) 
evalExpr (Lt l r) = intBoolBinOp (<) (evalExpr l) (evalExpr r) 
evalExpr (Eq l r) = intBoolBinOp (==) (evalExpr l) (evalExpr r) 
evalExpr (NotEq l r) = intBoolBinOp (/=) (evalExpr l) (evalExpr r) 

boolPreOp ::  (Bool -> Bool) -> Evald -> Evald
boolPreOp op (Right (OBool i)) = Right $ OBool $ op i
boolPreOp op (Right other) =
    Left $ "Left hand side of bool operator passed wrong type: " +++ T.pack (show other)
boolPreOp op left = left

intPreOp ::  (Integer -> Integer) -> Evald -> Evald
intPreOp op (Right (OInt i)) = Right $ OInt $ op i
intPreOp op (Right other) =
    Left $ "Left hand side of int operator passed wrong type: " +++ T.pack (show other)
intPreOp op left = left

intBinOp :: (Integer -> Integer -> Integer) -> Evald -> Evald -> Evald
intBinOp op (Right (OInt i1)) (Right (OInt i2)) =
    Right $ OInt $ op i1 i2

intBinOp op (Right l) (Right (OInt i1)) = 
    Left $ "Left hand side of int operator passed wrong type: " +++ T.pack (show l)

intBinOp op (Right (OInt i1)) (Right r) = 
    Left $ "Left hand side of int operator passed wrong type: " +++ T.pack (show r)

intBinOp op err@(Left _) _ = err
intBinOp op _ err@(Left _) = err

intBoolBinOp :: (Integer -> Integer -> Bool) -> Evald -> Evald -> Evald
intBoolBinOp op (Right (OInt i1)) (Right (OInt i2)) =
    Right $ OBool $ op i1 i2

intBoolBinOp op (Right l) (Right (OInt i1)) = 
    Left $ "Left hand side of int operator passed wrong type: " +++ T.pack (show l)

intBoolBinOp op (Right (OInt i1)) (Right r) = 
    Left $ "Left hand side of int operator passed wrong type: " +++ T.pack (show r)

intBoolBinOp op err@(Left _) _ = err
intBoolBinOp op _ err@(Left _) = err