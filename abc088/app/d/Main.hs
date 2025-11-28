{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies, UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Char8 qualified as B
import Data.Maybe
import Data.Ord

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Array
import Data.Bits
import Data.Bool
import Data.Char
import Data.Function
import Data.List
import Text.Printf

import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Tree qualified as T
import Data.Sequence qualified as Q
import Data.Vector qualified as V

import Debug.Trace qualified as Debug

debug :: Bool
debug = () /= ()

type I = Int
type O = Int

type Dom   = (Int, [Int])
type Codom = Int

type Solver = Dom -> Codom

solve :: Solver
solve = \ case
    (a, _) -> a

decode :: [[I]] -> Dom
decode = \ case
    [n]:as:_ -> (n, as)
    _   -> invalid $ "toDom: " ++ show @Int __LINE__

encode :: Codom -> [[O]]
encode = \ case
    r -> [[r]]

main :: IO ()
main = B.interact (detokenize . encode . solve . decode . entokenize)

{- Common Decode Patterns (Reference) -}
{-
Pattern 1: Single Value
  Input:  N
  type Dom = Int
  decode = \case
      [n]:_ -> n
      _     -> invalid $ "decode: " ++ show @Int __LINE__

Pattern 2: N + Array
  Input:  N
          A_1 A_2 ... A_N
  type Dom = (Int, [Int])
  decode = \case
      [n]:as:_ -> (n, as)
      _        -> invalid $ "decode: " ++ show @Int __LINE__

Pattern 3: Direct Array
  Input:  A_1 A_2 ... A_N
  type Dom = [Int]
  decode = \case
      as:_ -> as
      _    -> invalid $ "decode: " ++ show @Int __LINE__

Pattern 4: Multiple Parameters
  Input:  N M
  type Dom = (Int, Int)
  decode = \case
      [n, m]:_ -> (n, m)
      _        -> invalid $ "decode: " ++ show @Int __LINE__

Pattern 5: N Pairs
  Input:  N
          A_1 B_1
          ...
          A_N B_N
  type Dom = [(Int, Int)]
  decode = \case
      _:pairs -> map toPair pairs
      _       -> invalid $ "decode: " ++ show @Int __LINE__

Pattern 6: Grid (H×W)
  Input:  H W
          S_{1,1}...S_{1,W}
          ...
          S_{H,1}...S_{H,W}
  type I = Char
  type Dom = (Int, Int, [[Char]])
  decode = \case
      hw:grid ->
          let [h, w] = map digitToInt hw
          in (h, w, grid)
      _ -> invalid $ "decode: " ++ show @Int __LINE__

Pattern 7: Two Arrays
  Input:  N M
          A_1 ... A_N
          B_1 ... B_M
  type Dom = (Int, Int, [Int], [Int])
  decode = \case
      [n, m]:as:bs:_ -> (n, m, as, bs)
      _              -> invalid $ "decode: " ++ show @Int __LINE__
-}

class AsToken a where
    readB :: B.ByteString -> a
    readBs :: B.ByteString -> [a]
    readBs = map readB . B.words
    entokenize :: B.ByteString -> [[a]]
    entokenize = map readBs . B.lines

    showB :: a -> B.ByteString
    showBs :: [a] -> B.ByteString
    showBs = B.unwords . map showB
    detokenize :: [[a]] -> B.ByteString
    detokenize = B.unlines . map showBs

instance AsToken B.ByteString where
    readB = id
    showB = id

instance AsToken Int where
    readB = readInt
    showB = showInt

instance AsToken Integer where
    readB = readInteger
    showB = showInteger

instance AsToken String where
    readB = readStr
    showB = showStr

instance AsToken Double where
    readB = readDbl
    showB = showDbl

instance AsToken Char where
    readB = B.head
    showB = B.singleton
    readBs = B.unpack
    showBs = B.pack

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

showInt :: Int -> B.ByteString
showInt = B.pack . show

readInteger :: B.ByteString -> Integer
readInteger = fst . fromJust . B.readInteger

showInteger :: Integer -> B.ByteString
showInteger = B.pack . show

readStr :: B.ByteString -> String
readStr = B.unpack

showStr :: String -> B.ByteString
showStr = B.pack

readDbl :: B.ByteString -> Double
readDbl = read . B.unpack

showDbl :: Double -> B.ByteString
showDbl = B.pack . show

{- debug -}
trace :: String -> a -> a
trace | debug     = Debug.trace
      | otherwise = const id

tracing :: Show a => a -> a
tracing = trace . show <*> id

{- error -}
impossible :: String -> a
impossible msg = error $ msg ++ ", impossible"

invalid :: String -> a
invalid msg = error $ msg ++ ", invalid input"

{- Start Bonsai -}

{- End Bonsai -}
