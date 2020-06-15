module Monkey.Repl where 

import           Control.Monad     (unless)
import Data.Text as T
import Monkey.Lexer
import           System.IO

main :: IO ()
main = do
  input <- read'
  unless 
    (input == ":q") 
      $ print' (eval' input)
      >> main


read' :: IO String
read' = 
    putStr "REPL> "
    >> hFlush stdout
    >> getLine


eval' :: String -> String
eval' = show . parseMonkeyTokens . T.pack
  -- Try the following ones from `EvaluatorExamples.hs`:
  -- capitalizer input
  -- simpleCalc input
  -- emojiFinder input


print' :: String -> IO ()
print' = putStrLn