
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monkey.Token where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Void
import Data.List
import Data.Bifunctor (bimap)
import           Data.Text                  (Text)
import Text.Megaparsec
import Text.Megaparsec.Stream
import qualified Data.List          as DL
import qualified Data.Text          as T
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set

data MonkeyToken
    = Newline
    | Illegal Char
    | Ident Text
    | Int Integer
    | Comma
    | Semicolon
    | Lparen
    | Rparen
    | Lbrace
    | Rbrace
    | Function
    | Let
    -- OPERATORS
    | Eq
    | NotEq
    | Lt
    | Gt
    | Plus
    | Assign
    | Minus
    | Bang
    | Asterix
    | Slash
    deriving(Eq, Show, Ord)

newtype MonkeyTokens = MonkeyTokens [MonkeyToken]
    deriving(Eq, Show, Ord)

instance Stream MonkeyTokens where
  type Token MonkeyTokens = MonkeyToken
  type Tokens MonkeyTokens = MonkeyTokens
  tokenToChunk Proxy = MonkeyTokens . pure
  tokensToChunk Proxy = MonkeyTokens
  chunkToTokens Proxy (MonkeyTokens tks) = tks
  chunkLength Proxy (MonkeyTokens tks) = length tks
  chunkEmpty Proxy (MonkeyTokens tks) = null tks
  take1_ (MonkeyTokens []) = Nothing
  take1_ (MonkeyTokens (t:ts)) = Just (t, MonkeyTokens ts)
  takeN_ n (MonkeyTokens s)
    | n <= 0    = Just (MonkeyTokens [], MonkeyTokens s)
    | null s    = Nothing
    | otherwise = Just $ doubleMonkeys (splitAt n s)

  takeWhile_ f (MonkeyTokens tks) = doubleMonkeys $ span f tks

  showTokens Proxy = unwords . map prettyPrint . NE.toList

  reachOffset = reachOffset' takeToks foldTokens showTokens_ prettyPrint Newline
    where 
        showTokens_ (MonkeyTokens toks)=  unwords $ map prettyPrint $ toks
        takeToks i (MonkeyTokens toks) = doubleMonkeys $ splitAt i toks
        foldTokens f b (MonkeyTokens toks) = DL.foldl' f b toks

wrap :: MonkeyToken -> MonkeyTokens
wrap t = MonkeyTokens [t]

doubleMonkeys :: ([MonkeyToken], [MonkeyToken]) -> (MonkeyTokens, MonkeyTokens)
doubleMonkeys = bimap MonkeyTokens MonkeyTokens

prettyPrint :: MonkeyToken -> String
prettyPrint token = case token of
    Eq -> "=="
    NotEq -> "!="
    Newline -> "\n"
    Assign -> "="
    Plus -> "+"
    Comma -> ","
    Semicolon -> ";"
    Lparen -> "("
    Rparen -> ")"
    Lbrace -> "{"
    Rbrace -> "}"
    Minus -> "-"
    Bang -> "!"
    Asterix -> "*"
    Slash -> "/"
    Lt -> "<"
    Gt -> ">"
    Function -> "fn"
    Let -> "let"
    Illegal ch -> show ch
    Ident txt -> T.unpack txt
    Int i -> show i


data St = St SourcePos ShowS

-- | A helper definition to facilitate defining 'reachOffset' for various
-- stream types.

reachOffset'
  :: forall s. Stream s
  => (Int -> s -> (Tokens s, s))
     -- ^ How to split input stream at given offset
  -> (forall b. (b -> Token s -> b) -> b -> Tokens s -> b)
     -- ^ How to fold over input stream
  -> (Tokens s -> String)
     -- ^ How to convert chunk of input stream into a 'String'
  -> (Token s -> String)
     -- ^ How to convert a token into a 'Char'
  -> Token s
     -- ^ Newline token and tab token
  -> Int
     -- ^ Offset to reach
  -> PosState s
     -- ^ Initial 'PosState' to use
  -> (SourcePos, String, PosState s)
     -- ^ Reached 'SourcePos', line at which 'SourcePos' is located, updated
     -- 'PosState'
reachOffset' splitAt'
             foldl''
             fromToks
             fromTok
             newlineTok
             o
             PosState {..} =
  ( spos
  , case expandTab pstateTabWidth
           . addPrefix
           . f
           . fromToks
           . fst
           $ takeWhile_ (/= newlineTok) post of
      "" -> "<empty line>"
      xs -> xs
  , PosState
      { pstateInput = post
      , pstateOffset = max pstateOffset o
      , pstateSourcePos = spos
      , pstateTabWidth = pstateTabWidth
      , pstateLinePrefix =
          if sameLine
            -- NOTE We don't use difference lists here because it's
            -- desirable for 'PosState' to be an instance of 'Eq' and
            -- 'Show'. So we just do appending here. Fortunately several
            -- parse errors on the same line should be relatively rare.
            then pstateLinePrefix ++ f ""
            else f ""
      }
  )
  where
    addPrefix xs =
      if sameLine
        then pstateLinePrefix ++ xs
        else xs
    sameLine = sourceLine spos == sourceLine pstateSourcePos
    (pre, post) = splitAt' (o - pstateOffset) pstateInput
    St spos f = foldl'' go (St pstateSourcePos id) pre
    go (St apos g) ch =
      let SourcePos n l c = apos
          c' = unPos c
          w  = unPos pstateTabWidth
      in if | ch == newlineTok ->
                St (SourcePos n (l <> pos1) pos1)
                   id
            -- | ch == tabTok ->
            --     St (SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w)))
            --        (g . (fromTok ch :))
            | otherwise ->
                St (SourcePos n l (c <> pos1))
                   (g . (fromTok ch <>))
-- {-# INLINE reachOffset' #-}

expandTab
  :: Pos
  -> String
  -> String
expandTab w' = go 0
  where
    go 0 []        = []
    go 0 ('\t':xs) = go w xs
    go 0 (x:xs)    = x : go 0 xs
    go n xs        = ' ' : go (n - 1) xs
    w              = unPos w'