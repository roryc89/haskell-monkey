
import Test.Hspec
import Control.Exception (evaluate)
import qualified Spec.Monkey.Lexer as Lexer
import qualified Spec.Monkey.Parser as Parser

main :: IO ()
main = hspec $ do
    Lexer.test
    Parser.test

--   describe "Prelude.head" $ do
--     it "returns the first element of a list" $ do
--       head [23 ..] `shouldBe` (23 :: Int)

--     it "throws an exception if used with an empty list" $ do
--       evaluate (head []) `shouldThrow` anyException