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
import Data.Array.IArray
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Bits
import Data.Bool
import Data.Char
import Data.Function
import Data.List
import Text.Printf

import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Ix
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Tree qualified as T
import Data.Sequence qualified as Q
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Fusion.Bundle qualified as VFB
import Data.Vector.Generic qualified as VG

import Debug.Trace qualified as Debug

debug :: Bool
debug = False

type I = Int
type O = Int

type Dom   = (Int, [Int])
type Codom = Int

type Solver = Dom -> Codom

{-# INLINE solve #-}
solve :: Solver
solve x =
  trace (show x) def

{-# INLINE decode #-}
decode :: [[I]] -> Dom
decode = \ case
    [n]:as:_ -> (n, as)
    _   -> invalid $ "toDom: " ++ show @Int __LINE__

{-# INLINE encode #-}
encode :: Codom -> [[O]]
encode r = [[r]]
-- encode = map (:[])

main :: IO ()
main = B.interact (detokenize . encode . solve . decode . entokenize)

{- Decode Patterns -}
{-
Pattern: Grid (H×W)
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
-}
{- Encode Pattern -}
{-
Pattern: リストを改行区切りにして出力
Output: win
        draw
        lose
type O = String
type Codom = [String]
encode = map (:[])
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

{- 古いAtcoder環境だとdata-defaultがないため -}
class Default a where
  def :: a
instance Default Int where def = 0
instance Default Double where def = 0.0
instance Default Bool where def = False
instance Default [a] where def = []
instance Default () where def = ()
instance (Default a, Default b) => Default (a, b) where def = (def, def)
instance (Default a, Default b, Default c) => Default (a, b, c) where def = (def, def, def)

-- 偶数番目の要素を抽出
evenPositions :: [a] -> [a]
evenPositions = positionsBy even
-- 奇数番目の要素を抽出
oddPositions :: [a] -> [a]
oddPositions = positionsBy odd

positionsBy :: (Int -> Bool) -> [a] -> [a]
positionsBy idxPred xs = [x | (i, x) <- zip [0 ..] xs, idxPred i]

tuple2 :: (a, a) -> [a]
tuple2 (x, y) = [x, y]

tuple3 :: (a, a, a) -> [a]
tuple3 (x, y, z) = [x, y, z]

class ToVector s a where
    toVector :: VU.Unbox a => Int -> s -> VU.Vector a

instance ToVector [a] a where
    toVector n = VU.unfoldrN n uncons

instance ToVector B.ByteString Char where
    toVector n bs = VU.generate (min n (B.length bs)) (B.index bs)

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

{-# INLINE yn #-}
yn :: Bool -> String
yn = bool "No" "Yes"

{- 累積和 -}
-- | 1次元の累積和（Vector版）を作成する
-- >>> csum1 $ VU.generate 4 (+1)
-- [0,1,3,6,10]
{-# INLINE csum1 #-}
csum1 :: (Num a, VU.Unbox a) => VU.Vector a -> VU.Vector a
csum1 = VU.scanl' (+) 0

-- | 1 次元の累積和配列を元に区間和を求める。(1-based。問題の(l,r)をそのまま渡せば良い)
-- >>> csum +! (2, 3)
-- 5
{-# INLINE (+!) #-}
(+!) :: (Num a, VU.Unbox a) => VU.Vector a -> (Int, Int) -> a
(+!) csum (!l, !r) = csum VG.! r - csum VG.! (l - 1)

-- | いもす法（Array版）
-- >>> let lrs = [(2,3),(3,6),(5,7),(3,7),(1,5)] -- [(li, ri)] 参加者iがli日目からri日目まで参加する
-- >>> imosA 8 lrs
-- [1,2,4,3,4,3,2,0]
{-# INLINE imosA #-}
imosA :: Int -> [(Int, Int)] -> [Int]
imosA d lrs = elems result
  where
    diffArray :: UArray Int Int
    diffArray =
      accumArray (+) 0 (0, d + 1) $
      concatMap (\(l, r) -> [(l, 1), (r + 1, -1)]) lrs
    -- 累積和
    result :: UArray Int Int
    result = listArray (1, d) $ drop 2 $ scanl (+) 0 (elems diffArray)

-- | 2 次元の累積和配列を作成する。
-- 入力: (H, W) と HxW のグリッドデータ
-- 出力: ((0,0), (H,W)) の範囲を持つ累積和配列
-- >>> csum2 (2,2) [[1,2],[3,4]] :: UArray (Int,Int) Int
-- array ((0,0),(2,2)) [
--  ((0,0),0),((0,1),0),((0,2),0),
--  ((1,0),0),((1,1),1),((1,2),3),
--  ((2,0),0),((2,1),4),((2,2),10)]
csum2 :: (IArray UArray e, Num e) => (Int, Int) -> [[e]] -> UArray (Int, Int) e
csum2 (h, w) rows = listArray ((0, 0), (h, w)) flatList
  where
    rowSums = map (scanl (+) 0) rows
    zeroRow = replicate (w + 1) 0
    colSums = scanl (zipWith (+)) zeroRow rowSums
    flatList = concat colSums
{-# INLINE csum2 #-}

-- | 2 次元の累積和配列を元に矩形和を求める。 0-based index
-- 左上 (r1, c1), 右下 (r2, c2)
-- >>> let csum = csum2 (2,2) [[1,2],[3,4]] :: UArray (Int,Int) Int
-- >>> csum +!! ((0,0),(1,0))
-- 4
(+!!) :: (IArray UArray e, Num e) => UArray (Int, Int) e -> ((Int, Int), (Int, Int)) -> e
(+!!) ary ((r1, c1), (r2, c2)) =
  ary ! (r2 + 1, c2 + 1) - ary ! (r1, c2 + 1) - ary ! (r2 + 1, c1) + ary ! (r1, c1)
{-# INLINE (+!!) #-}

{- End Bonsai -}
