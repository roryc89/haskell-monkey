
import Test.Hspec
import Control.Exception (evaluate)
import qualified Spec.Monkey.Lexer as Lexer
import qualified Spec.Monkey.Parser as Parser

main :: IO ()
main = hspec $ do
    Lexer.test
    Parser.test
