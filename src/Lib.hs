module Lib  where

import qualified Monkey.Repl as Repl


someFunc :: IO ()
someFunc = putStrLn "someFunc"

repl :: IO ()
repl = Repl.main