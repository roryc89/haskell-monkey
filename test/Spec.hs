
import Test.Hspec
import Control.Exception (evaluate)
import qualified Spec.Token.Parse as Token.Parse

main :: IO ()
main = hspec $ do
    Token.Parse.test

--   describe "Prelude.head" $ do
--     it "returns the first element of a list" $ do
--       head [23 ..] `shouldBe` (23 :: Int)

--     it "throws an exception if used with an empty list" $ do
--       evaluate (head []) `shouldThrow` anyException