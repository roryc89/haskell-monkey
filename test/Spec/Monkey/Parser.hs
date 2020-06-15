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
                , ExpressionStatement $ MinusPrefix $ Int 15
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
