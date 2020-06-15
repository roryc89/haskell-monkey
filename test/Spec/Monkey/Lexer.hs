{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Monkey.Lexer where

import Data.Text as T
import NeatInterpolation
import Test.Hspec
import Monkey.Token
import Monkey.Lexer

rightMonkeys l = Right (MonkeyTokens l)

test = 
  describe "Monkey.Lexer" $ do
      it "should parse a simple input" $ do
          let input = "=+ () {},;"
          parseMonkeyTokens input `shouldBe` rightMonkeys [ Assign, Plus, Lparen, Rparen, Lbrace, Rbrace, Comma, Semicolon ]


      it "should parse functions" $ do 
          let input = [text|let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);|];

          let expected = [ 
                Let,
                Ident "five",
                Assign,
                Int 5,
                Semicolon,
                Let,
                Ident "ten",
                Assign,
                Int 10,
                Semicolon,
                Let,
                Ident "add",
                Assign,
                Function,
                Lparen,
                Ident "x",
                Comma,
                Ident "y",
                Rparen,
                Lbrace,
                Ident "x",
                Plus,
                Ident "y",
                Semicolon,
                Rbrace,
                Semicolon,
                Let,
                Ident "result",
                Assign,
                Ident "add",
                Lparen,
                Ident "five",
                Comma,
                Ident "ten",
                Rparen,
                Semicolon ]

          parseMonkeyTokens input `shouldBe` rightMonkeys expected

      it "should parse operators" $ do 
          let input = [text|!-/*5;
5 < 10 > 5;

          |];

          let expected = [
                Bang,
                Minus,
                Slash,
                Asterix,
                Int 5,
                Semicolon,
                Int 5,
                Lt,
                Int 10,
                Gt, 
                Int 5,
                Semicolon]

          parseMonkeyTokens input `shouldBe` rightMonkeys expected



