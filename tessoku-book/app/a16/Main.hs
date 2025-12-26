{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import AtCoder.Extra.Bisect qualified as AB
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (bimap)
import Data.Bits
import Data.Bool
import Data.ByteString.Char8 qualified as B
import Data.Char
import Data.Function
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Ix
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Sequence qualified as Q
import Data.Set qualified as S
import Data.Tree qualified as T
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Fusion.Bundle qualified as VFB
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Debug.Trace qualified as Debug
import Text.Printf

debug :: Bool
debug = False

type I = Int

type O = Int

type Dom = (Int, [Int], [Int])

type Codom = Int

type Solver = Dom -> Codom

{-# INLINE solve #-}
solve :: Solver
-- solve x = trace (show x) def
solve (_, a2:as, bs) = final
  where
    av = VU.fromList as
    bv = VU.fromList bs
    (final, _) = VU.foldl' step (a2, 0) (VU.zip av bv)
    -- (final, _) = [(a2,0) `step` (a3, b3) `step` (a4, b4) ...]
    step :: (Int, Int) -> (Int, Int) -> (Int, Int)
    -- pre1: 部屋i-1までにかかった累積時間
    -- pre2: 部屋i-2までにかかった累積時間
    -- a: 部屋i-iから部屋iへ向かう通路にかかる時間
    -- b: 部屋i-2から部屋iへ向かう通路にかかる時間
    step (pre1, pre2) (a, b) = (min (pre1 + a) (pre2 + b), pre1)

{-# INLINE decode #-}
decode :: [[I]] -> Dom
decode = \case
  [n] : as : bs : _ -> (n, as, bs)
  _ -> invalid $ "toDom: " ++ show @Int __LINE__

{-# INLINE encode #-}
encode :: Codom -> [[O]]
encode r = [[r]]

-- encode = map (:[])

main :: IO ()
main = B.interact (detokenize . encode . solve . decode . entokenize)

{- Decode Patterns -}
{-
-- Pattern: Grid (H×W)
--  Input:  H W
--          S_{1,1}...S_{1,W}
--          ...
--          S_{H,1}...S_{H,W}
type I = Char
type Dom = (Int, Int, [[Char]])
decode = \case
    hw:grid ->
        let [h, w] = map digitToInt hw
        in (h, w, grid)
    _ -> invalid $ "decode: " ++ show @Int __LINE__

-- Pattern: Int & String
-- Input: 5
--        WEEWW
type I = Char
type Dom (Int, String)
decode :: [[I]] -> Dom
decode = \ case
    n:as:_ -> (read n, as)
    _   -> invalid $ "toDom: " ++ show @Int __LINE__
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
trace
  | debug = Debug.trace
  | otherwise = const id

tracing :: (Show a) => a -> a
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
  toVector :: (VU.Unbox a) => Int -> s -> VU.Vector a

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

{- しゃくとり法 -}
{-# INLINE shakutori #-}
-- p: 条件を満たすかどうかを判定する関数 (l -> r -> Bool)
-- lls: 左端 L が指す要素以降のリスト
-- rrs: 右端 R が指す要素以降のリスト
-- len: 現在の条件を満たす区間 [L, R) の要素数 (Rを含まない)
shakutori :: (Int -> Int -> Bool) -> [Int] -> [Int] -> Int -> [Int]
-- 右端 R がリストの終端に達した場合
-- 以降のすべての L について、残りの len がそのまま解になる
-- L を進めるごとに len を減らしていく
shakutori p (_ : ls) [] len = len : shakutori p ls [] (len - 1)
shakutori p lls@(l : ls) rrs@(r : rs) len
  -- 条件を満たすなら、右端 R を右へ進める
  -- 区間の長さ len は +1 される
  | p l r = shakutori p lls rs (len + 1)
  -- 条件を満たさなくなった場合、現在の L に対するペアの数は len 個 (A[L]...A[R-1])
  -- len を結果リストに追加し、左端 L を右へ進める
  -- L が進むと区間の長さ len は -1 される
  | otherwise = len : shakutori p ls rrs (len - 1)
-- 左端 L が終端に達したら終了
shakutori _ _ _ _ = []


-- https://publish.obsidian.md/naoya/articles/%E9%96%A2%E6%95%B0%E9%81%A9%E7%94%A8%E3%81%AB%E3%82%88%E3%82%8B%E7%8A%B6%E6%85%8B%E9%81%B7%E7%A7%BB%E3%81%A7+DP+%E3%82%92%E8%A7%A3%E3%81%8F
-- ex) accumArrayDP @UArray f max minBound (0, wx) [(0, 0)] wvs
{-# INLINE accumArrayDP #-}
accumArrayDP ::
  ( IArray a e,
    Ix v,
    Eq e,
    Show e,
    Show v,
    Show (a v e),
    Foldable t
  ) =>
  ((v, e) -> x -> [(v, e')]) -> -- 状態遷移関数 f v / x をみて v の次の遷移可能性を返す
  (e -> e' -> e) -> -- 緩和の二項演算
  e -> -- 初期値 (0, minBound, maxBound, False など)
  (v, v) -> -- 状態空間の下界、上界
  [(v, e')] -> -- 開始時点の状態
  t x -> -- 入力 (時間遷移)
  a v e -- Array or UArray
accumArrayDP f op initial (l, u) v0s xs = do
  let dp = accumArray op initial (l, u) v0s
  foldl' transition dp xs
  where
    transition dp x =
      accumArray op initial (l, u) $
        concatMap (filter (inRange (bounds dp) . fst) . (`f` x)) (assocs dp)

{- End Bonsai -}


