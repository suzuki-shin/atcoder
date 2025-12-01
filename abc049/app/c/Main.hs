{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}

{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array
import Data.Array.IArray qualified as IArray
import Data.Array.Unboxed (UArray)
import Data.Bits
import Data.Bool
import Data.ByteString.Char8 qualified as B
import Data.Char
import Data.Function
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Sequence qualified as Q
import Data.Set qualified as S
import Data.Tree qualified as T
-- import Data.Tuple.Extra (both)
-- import Data.Vector qualified as V
import Debug.Trace qualified as Debug
import Text.Printf

debug :: Bool
debug = () == ()

type I = String

type O = String

type Dom = String

type Codom = String

type Solver = Dom -> Codom

solve :: Solver
solve as = bool "NO" "YES" $ canDivide $ reverse as

rParts :: [String]
rParts = map reverse ["dream", "dreamer", "erase", "eraser"]

canDivide :: String -> Bool
canDivide "" = True
canDivide str = any (tryPart str) rParts
    where
        tryPart :: String -> String -> Bool
        tryPart str part = maybe False canDivide (part `stripPrefix` str)

decode :: [[I]] -> Dom
decode = \case
  [n] : _ -> n
  _ -> invalid $ "toDom: " ++ show @Int 61

encode :: Codom -> [[O]]
encode = \case
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
      _     -> invalid $ "decode: " ++ show @Int 77

Pattern 2: N + Array
  Input:  N
          A_1 A_2 ... A_N
  type Dom = (Int, [Int])
  decode = \case
      [n]:as:_ -> (n, as)
      _        -> invalid $ "decode: " ++ show @Int 85

Pattern 3: Direct Array
  Input:  A_1 A_2 ... A_N
  type Dom = [Int]
  decode = \case
      as:_ -> as
      _    -> invalid $ "decode: " ++ show @Int 92

Pattern 4: Multiple Parameters
  Input:  N M
  type Dom = (Int, Int)
  decode = \case
      [n, m]:_ -> (n, m)
      _        -> invalid $ "decode: " ++ show @Int 99

Pattern 5: N Pairs
  Input:  N
          A_1 B_1
          ...
          A_N B_N
  type Dom = [(Int, Int)]
  decode = \case
      _:pairs -> map toPair pairs
      _       -> invalid $ "decode: " ++ show @Int 109

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
      _ -> invalid $ "decode: " ++ show @Int 122

Pattern 7: Two Arrays
  Input:  N M
          A_1 ... A_N
          B_1 ... B_M
  type Dom = (Int, Int, [Int], [Int])
  decode = \case
      [n, m]:as:bs:_ -> (n, m, as, bs)
      _              -> invalid $ "decode: " ++ show @Int 131
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
trace
  | debug = Debug.trace
  | otherwise = const id

tracing :: (Show a) => a -> a
tracing = trace . show <*> id

-- https://zenn.dev/toyboot4e/books/seriously-haskell/viewer/2-1-stderr のものを一部改変

-- ローカル環境
dbg :: (Show a) => a -> ()
dbg = (`Debug.traceShow` ())

dbgS :: String -> ()
dbgS = (`Debug.trace` ())

dbgId :: (Show a) => a -> a
dbgId x = Debug.traceShow x x

note :: (Show a) => String -> a -> ()
note s x = trace (s ++ ": " ++ show x) ()

dbgGrid :: (IArray.IArray a e, Show e) => a (Int, Int) e -> ()
dbgGrid !grid = Data.List.foldl' step () rows
  where
    -- 幅を (x1 - x0 + 1) で計算
    -- bounds grid :: ((Int,Int),(Int,Int))
    ((x0, _), (x1, _)) = IArray.bounds grid -- 下限/上限を直接分解
    w = x1 - x0 + 1 -- 列数
    -- elems grid は行優先 (通常はインデックス昇順) なので幅 w で分割
    rows = chunk w (IArray.elems grid)

    -- 行を整形して trace (副作用を Unit に畳み込む)
    step () xs = trace (unwords (map (align . show) xs)) ()

    -- 右寄せ簡易フォーマット (幅 5 固定)
    align s =
      let l = length s
          d = 5
       in replicate (d - l) ' ' ++ s

-- | リストを幅 n ずつ分割
chunk :: Int -> [e] -> [[e]]
chunk n = Data.List.unfoldr f
  where
    f [] = Nothing
    f xs = Just (splitAt n xs)



















{- error -}
impossible :: String -> a
impossible msg = error $ msg ++ ", impossible"

invalid :: String -> a
invalid msg = error $ msg ++ ", invalid input"

{- Start Bonsai -}

-- 偶数番目の要素を抽出
evenPositions :: [a] -> [a]
evenPositions = positionsBy even

-- 奇数番目の要素を抽出
oddPositions :: [a] -> [a]
oddPositions = positionsBy odd

positionsBy :: (Int -> Bool) -> [a] -> [a]
positionsBy idxPred xs = [x | (i, x) <- zip [0 ..] xs, idxPred i]

tupleToList2 :: (a, a) -> [a]
tupleToList2 (x, y) = [x, y]

tupleToList3 :: (a, a, a) -> [a]
tupleToList3 (x, y, z) = [x, y, z]

listToTuple2 :: [a] -> (a, a)
listToTuple2 [x, y] = (x, y)
listToTuple2 _ = error "invalid input"

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 [x, y, z] = (x, y, z)
listToTuple3 _ = error "invalid input"

{- Start from oceajigger -}
yn :: Bool -> String
yn = bool "No" "Yes"

{- End from oceajigger -}

{- End Bonsai -}


