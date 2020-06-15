{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Spec.Monkey.Parser where

import           Data.Text         as T
import           Monkey.Ast
import           Monkey.Parser
import           NeatInterpolation
import           Test.Hspec

test =
  describe "Monkey.Parser" $ do
      it "should parse a single expression statment" $ do
          let input = "foobar;";

          let expected = 
                [ ExpressionStatement $ Identifier "foobar"]

          parseProgram input `shouldBe` Right expected

      it "should parse multiple let statments" $ do
          let input = [text|
let x = 5;
let y = 10;
let foobar = 83838383;|];

          let expected = 
                [ Let "x" (Int 5)
                , Let "y" (Int 10)
                , Let "foobar" (Int 83838383) 
                ]

          parseProgram input `shouldBe` Right expected
          
      it "should parse prefix operators" $ do
          let input = [text|
!15;
-15;|];

          let expected = 
                [ ExpressionStatement $ Bang $ Int 15
                , ExpressionStatement $ Negate $ Int 15
                ]

          parseProgram input `shouldBe` Right expected
      it "should a single infix operator" $ do
          let input = "5 + 5;"

          let expected = 
                [ ExpressionStatement $ Plus (Int 5) (Int 5)
                ]
                
          parseProgram input `shouldBe` Right expected

      it "should parse infix operators" $ do
          let input = [text|
5 + 5;
5 - 5;
5 * 5;
5 / 5;
5 > 5;
5 < 5;
5 == 5;
5 != 5;|];

          let expected = 
                [ ExpressionStatement $ Plus (Int 5) (Int 5)
                , ExpressionStatement $ Minus (Int 5) (Int 5)
                , ExpressionStatement $ Times (Int 5) (Int 5)
                , ExpressionStatement $ Divide (Int 5) (Int 5)
                , ExpressionStatement $ Gt (Int 5) (Int 5)
                , ExpressionStatement $ Lt (Int 5) (Int 5)
                , ExpressionStatement $ Eq (Int 5) (Int 5)
                , ExpressionStatement $ NotEq (Int 5) (Int 5)
                ]

          parseProgram input `shouldBe` Right expected

      it "should parse booleans" $ do
          let input = [text|
true;
false;
3 > 5 == false;
3 < 5 == true;
|];

          let expected = 
                [ ExpressionStatement $ BoolE True
                , ExpressionStatement $ BoolE False
                , ExpressionStatement $ Eq (Gt (Int 3) (Int 5)) (BoolE False)
                , ExpressionStatement $ Eq (Lt (Int 3) (Int 5)) (BoolE True)
                ]

          parseProgram input `shouldBe` Right expected
      it "should parse parenthesis" $ do
          let input = [text|
(1);
|];

          let expected = 
                [ ExpressionStatement $ Parens (Int 1)
                ]

          parseProgram input `shouldBe` Right expected
          let input2 = [text|
(1 + 2);
|];

          let expected2 = 
                [ ExpressionStatement $ Parens (Plus (Int 1) (Int 2))
                ]

          parseProgram input2 `shouldBe` Right expected2

      it "should give precedence to expressions in parenthesis" $ do
          let input = [text|
1 + (2 + 3) + 4;
(5  + 5) * 2;
2 / (5 + 5);
-(5 + 5);
!(true == true);
|];

          let expected = 
                [ ExpressionStatement $ Plus ((Plus (Int 1) (Parens (Plus (Int 2) (Int 3))))) (Int 4)
                , ExpressionStatement $ Times (Parens (Plus (Int 5) (Int 5))) (Int 2)
                , ExpressionStatement $ Divide (Int 2) (Parens (Plus (Int 5) (Int 5))) 
                , ExpressionStatement $ Negate (Parens (Plus (Int 5) (Int 5)))
                , ExpressionStatement $ Bang (Parens (Eq (BoolE True) (BoolE True)))
                ]

          parseProgram input `shouldBe` Right expected

      it "should parse if statemenst" $ do
          let input = [text|
if(true){
  true;
}else{
  false;
}
|];

          let expected = 
                [ If (BoolE True) 
                    [ ExpressionStatement $ BoolE True
                    ]
                    [ ExpressionStatement $ BoolE False
                    ]
                ]
                
          parseProgram input `shouldBe` Right expected

      it "should parse function declarations" $ do
          let input = [text|
fn (x, y){ 
  x + y; 
}

fn(){ 
  return x + 1;
}
|];

          let expected = 
                [ FunctionDeclaration 
                    ["x", "y"]
                    [ ExpressionStatement $ Plus (Identifier "x") (Identifier "y")
                    ]
                , FunctionDeclaration 
                    []
                    [ Return $ Plus (Identifier "x") (Int 1)
                    ]
                ]
                

          parseProgram input `shouldBe` Right expected

      it "should parse simple call expressions" $ do
          let input = [text|
myFn(1, 2);
otherFn();
|];

          let expected = 
                [ ExpressionStatement $ CallExpression (Identifier "myFn") [Int 1, Int 2]
                , ExpressionStatement $ CallExpression (Identifier "otherFn") []
                ]
                

          parseProgram input `shouldBe` Right expected
--       it "should parse higher order call expressions" $ do
--           let input = [text|
-- higherFn1(x)();
-- higherFn2(1, fn(x) {
-- });
-- |];

--           let expected = 
--                 [ ExpressionStatement $ CallExpression (CallExpression (Identifier "higherFn1") [Identifier "x"]) []
--                 , ExpressionStatement $ CallExpression (Identifier "otherFn") []
--                 ]
                

--           parseProgram input `shouldBe` Right expected
-- ExpressionStatement $ CallExpression (CallExpression (Identifier "higherFn1") [Identifier "x"]) []