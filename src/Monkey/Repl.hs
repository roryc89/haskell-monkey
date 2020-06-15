module Monkey.Repl where 

import           Control.Monad     (unless)
import qualified Data.Text as T
import Monkey.Parser
import Monkey.Object
import Monkey.Ast
import qualified Monkey.Eval as Eval
import           System.IO

main :: IO ()
main = do
  input <- read'
  unless 
    (input == ":q") 
      $ print' (evalProgram input)
      >> main


read' :: IO String
read' = 
    putStr "REPL> "
    >> hFlush stdout
    >> getLine

evalProgramAndAst :: String -> String
evalProgramAndAst input = show (evalProgram input, evalAst input)

evalProgram :: String -> String
evalProgram = either show showEvaled . fmap Eval.eval . parseProgram . T.pack

showEvaled :: Eval.Evald -> String
showEvaled = either show prettyPrintObject

evalAst :: String -> String
evalAst = either show (T.unpack . prettyPrintStatements) . parseProgram . T.pack


print' :: String -> IO ()
print' = putStrLn