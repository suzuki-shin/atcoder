{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Main where

import AtCoder.Extra.Bisect qualified as AB
import Control.Applicative
import Control.Arrow hiding ((<+>))
import Control.Arrow qualified as Arrow
import Control.Monad
import Control.Monad.ST (ST, runST)
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.MArray (MArray, newArray, newArray_, readArray, writeArray)
import Data.Array.ST (STArray, STUArray, runSTArray, runSTUArray)
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (bimap)
import Data.Bits
import Data.Bool
import Data.ByteString.Char8 qualified as B
import Data.Char
import Data.Function
import Data.HashMap.Strict qualified as HM
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Ix
import Data.List
import Data.List.Extra
import Data.Map.Strict qualified as M
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

type I = Int

type O = Char

type Dom   = (Int, [Int])
type Codom = (String, [Int])

type Solver = Dom -> Codom

debug :: Bool
debug = False

{-# INLINE solve #-}
solve :: Solver
solve (n,as) = (yn $ maximum as' == 1, fromJust $ listToMaybe bs)
  where
    as' = map length $ group $ sort $ filter (/= (-1)) as
    bs = filter f $ permutations [1..n]
    f :: [Int] -> Bool
    f xs = all (\(a,x) -> (a == -1) || (x == a)) (zip as xs)

{-# INLINE decode #-}
decode :: [[I]] -> Dom
decode = \case
  [n] : as : _ -> (n, as)
  _ -> invalid $ "toDom: " ++ show @Int 85

{-# INLINE encode #-}
encode :: Codom -> [[O]]
encode ("No", _) = ["No"]
encode (s, ps) = s : [unwords $ map show ps]
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
  nm : grid ->
    let [n, m] = map (read @Int) $ words nm
     in (n, m, grid)
  _x -> invalid $ "toDom: " ++ show @Int 108
-- Pattern: Int & String
-- Input: 5
--        WEEWW
type I = Char
type Dom (Int, String)
decode :: [[I]] -> Dom
decode = \ case
    n:as:_ -> (read n, as)
    _   -> invalid $ "toDom: " ++ show @Int 117
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

{- DP -}
-- https://zenn.dev/osushi0x/articles/198bce676e2841#%E5%8B%95%E7%9A%84%E8%A8%88%E7%94%BB%E6%B3%95%E3%81%AB%E5%85%B1%E9%80%9A%E3%81%99%E3%82%8B%E6%A7%8B%E9%80%A0%E3%81%AF%E5%8D%8A%E7%92%B0%E3%81%A7%E3%81%82%E3%82%8B
class Semiring s where
  (<+>) :: s -> s -> s
  (<.>) :: s -> s -> s
  zero :: s
  one :: s

newtype MaxPlus = MaxPlus {unMaxPlus :: Int} deriving (Eq, Ord, Show)

newtype MinPlus = MinPlus {unMinPlus :: Int} deriving (Eq, Ord, Show)

newtype Boolean = Boolean {unBoolean :: Bool} deriving (Eq, Ord, Show)

-- 通常の数え上げ（Int）
newtype Count = Count {getCount :: Int} deriving (Eq, Show)

instance Semiring MaxPlus where
  {-# INLINE (<+>) #-}
  (MaxPlus v1) <+> (MaxPlus v2) = MaxPlus (max v1 v2)
  {-# INLINE (<.>) #-}
  t1@(MaxPlus v1) <.> t2@(MaxPlus v2)
    | t1 == zero = zero
    | t2 == zero = zero
    | otherwise = MaxPlus (v1 + v2)
  {-# INLINE zero #-}
  zero = MaxPlus minBound
  {-# INLINE one #-}
  one = MaxPlus 0

instance Semiring MinPlus where
  {-# INLINE (<+>) #-}
  (MinPlus v1) <+> (MinPlus v2) = MinPlus (min v1 v2)
  {-# INLINE (<.>) #-}
  t1@(MinPlus v1) <.> t2@(MinPlus v2)
    | t1 == zero = zero
    | t2 == zero = zero
    | otherwise = MinPlus (v1 + v2)
  {-# INLINE zero #-}
  zero = MinPlus maxBound
  {-# INLINE one #-}
  one = MinPlus 0

instance Semiring Boolean where
  {-# INLINE (<+>) #-}
  (Boolean v1) <+> (Boolean v2) = Boolean (v1 || v2)
  {-# INLINE (<.>) #-}
  t1@(Boolean v1) <.> t2@(Boolean v2)
    | t1 == zero = zero
    | t2 == zero = zero
    | otherwise = Boolean (v1 && v2)
  {-# INLINE zero #-}
  zero = Boolean False
  {-# INLINE one #-}
  one = Boolean True

instance Semiring Count where
  {-# INLINE (<+>) #-}
  Count a <+> Count b = Count (a + b) -- 足し算
  {-# INLINE (<.>) #-}
  Count a <.> Count b = Count (a * b) -- 掛け算
  {-# INLINE zero #-}
  zero = Count 0
  {-# INLINE one #-}
  one = Count 1

-- 確率（Double）
newtype Prob = Prob {getProb :: Double} deriving (Eq, Show)

instance Semiring Prob where
  {-# INLINE (<+>) #-}
  Prob a <+> Prob b = Prob (a + b)
  {-# INLINE (<.>) #-}
  Prob a <.> Prob b = Prob (a * b)
  {-# INLINE zero #-}
  zero = Prob 0.0
  {-# INLINE one #-}
  one = Prob 1.0

-- XxxWithRoute は交換法則を満たさないため厳密な半環ではない。そのため以下の注意点がある
-- 左側優先:
--   スコアが同点 (@s1 == s2@) の場合、演算の__左側（先に計算された方）__の経路が採用される
--   したがって、最短経路が複数存在する場合、どの経路が選ばれるかは探索順序に依存する
-- 零化の不成立:
--   @x <.> zero@ が完全な @zero@ （経路情報なし）にならない場合がありますが、
--   DPの最適化プロセス（<+>）で自然に淘汰されるため、実用上の計算結果には影響しないはず

data MinPlusWithRoute s p = MinPlusWithRoute !s (JoinList p)
  deriving (Show, Eq)

instance (Ord s, Semiring s) => Semiring (MinPlusWithRoute s p) where
  -- ※ s1 == s2 の場合、左側（先に計算した方）の経路が優先されます（Left-biased）。
  {-# INLINE (<+>) #-}
  (MinPlusWithRoute s1 r1) <+> (MinPlusWithRoute s2 r2)
    | s1 <= s2 = MinPlusWithRoute s1 r1
    | otherwise = MinPlusWithRoute s2 r2

  {-# INLINE (<.>) #-}
  (MinPlusWithRoute s1 r1) <.> (MinPlusWithRoute s2 r2) =
    MinPlusWithRoute (s1 <.> s2) (r1 <> r2)

  {-# INLINE zero #-}
  zero = MinPlusWithRoute zero mempty
  {-# INLINE one #-}
  one = MinPlusWithRoute one mempty

-- | 最大化問題用の経路復元付きラッパー
-- Semiring s の演算を行いつつ、経路 p の履歴を結合・選択します。
data MaxPlusWithRoute s p = MaxPlusWithRoute !s (JoinList p)
  deriving (Show, Eq)

instance (Ord s, Semiring s) => Semiring (MaxPlusWithRoute s p) where
  -- ※ s1 == s2 の場合、左側（先に計算した方）の経路が優先されます（Left-biased）。
  {-# INLINE (<+>) #-}
  (MaxPlusWithRoute s1 r1) <+> (MaxPlusWithRoute s2 r2)
    | s1 >= s2 = MaxPlusWithRoute s1 r1
    | otherwise = MaxPlusWithRoute s2 r2

  {-# INLINE (<.>) #-}
  (MaxPlusWithRoute s1 r1) <.> (MaxPlusWithRoute s2 r2) =
    MaxPlusWithRoute (s1 <.> s2) (r1 <> r2)

  {-# INLINE zero #-}
  zero = MaxPlusWithRoute zero mempty
  {-# INLINE one #-}
  one = MaxPlusWithRoute one mempty

-- | 到達可能性判定（Boolean）における経路復元付き型
data BooleanWithRoute p = BooleanWithRoute !Bool (JoinList p)
  deriving (Show, Eq)

instance Semiring (BooleanWithRoute p) where
  -- 左側が到達可能(True)なら即採用（左側優先）
  -- 左がダメで右が到達可能なら右を採用。両方ダメならFalse
  {-# INLINE (<+>) #-}
  lhs@(BooleanWithRoute True _) <+> _ = lhs
  _ <+> rhs = rhs

  -- 両方が到達可能(True)な場合のみ、経路を連結して True とする
  -- どちらかが False なら、結果も False (zero) になる
  {-# INLINE (<.>) #-}
  (BooleanWithRoute True r1) <.> (BooleanWithRoute True r2) = BooleanWithRoute True (r1 <> r2)
  _ <.> _ = zero

  -- \| 到達不能（False）
  {-# INLINE zero #-}
  zero = BooleanWithRoute False Empty

  -- \| 到達可能（True, 経路なし）
  {-# INLINE one #-}
  one = BooleanWithRoute True Empty

-- | 結合がO(1)のデータ構造。ListにするのはO(N)
data JoinList a
  = Empty
  | Single !a
  | Append (JoinList a) (JoinList a)
  deriving (Show, Eq)

instance Semigroup (JoinList a) where
  Empty <> b = b
  a <> Empty = a
  a <> b = Append a b

instance Monoid (JoinList a) where
  mempty = Empty

toList :: JoinList a -> [a]
toList jl = go jl []
  where
    go Empty acc = acc
    go (Single x) acc = x : acc
    go (Append l r) acc = go l (go r acc)

data DPProblem p sc = DPProblem
  { start :: p, -- 計算（メモ化再帰）を開始したい状態（例: dp[N] の N）。Nへ行くには->N-1が、、という順序
    getRange :: (p, p), -- 状態 p のとりうる範囲（メモ化テーブルのサイズ用）
    isTrivial :: p -> Maybe sc, -- 基底条件（漸化式の終了条件）
    subproblems :: p -> [(sc, p)] -- 遷移（部分問題への分解）
  }

-- ex)
-- n = 5
-- as = listArray (2,n) [2,4,1,3]
-- bs = listArray (3,n) [5,3,7]
-- dp :: (Array Int Int, Array Int Int, Int) -> DPProblem Int MinPlus
-- dp (as, bs, n) = DPProblem {...}
-- solve (n, as, bs) = unMinPlus $ runST $ dpSolve $ dp (as, bs, n)

{-# INLINE dpSolve #-}
dpSolve :: forall i sc s. (Semiring sc, Ix i) => DPProblem i sc -> ST s sc
dpSolve dp = do
  let rng = getRange dp
  memo <- newArray_ rng :: ST s (STArray s i sc)
  visited <- newArray rng False :: ST s (STUArray s i Bool)

  go (start dp) memo visited
  where
    go p memo visited
      | Just val <- isTrivial dp p = return val
      | otherwise = do
          isVisited <- readArray visited p
          if isVisited
            then readArray memo p
            else do
              let f acc (s, sp) = do
                    v <- go sp memo visited
                    return $! acc <+> (s <.> v)

              ret <- foldM f zero (subproblems dp p)
              writeArray memo p ret
              writeArray visited p True
              return ret

-- | 2次元配列から矩形領域を切り出し、インデックスを (0,0) 起点にリベースする
subArray2Rebased :: (IArray a e) => ((Int, Int), (Int, Int)) -> a (Int, Int) e -> a (Int, Int) e
subArray2Rebased ((r1, c1), (r2, c2)) = ixmap ((0, 0), (r2 - r1, c2 - c1)) (\(r, c) -> (r + r1, c + c1))

-- | 汎用版: 要素を show してスペース区切りで表示
printArray :: (IArray UArray a, Show a) => UArray (Int, Int) a -> IO ()
printArray ary = do
  let ((minR, minC), (maxR, maxC)) = bounds ary
  forM_ [minR .. maxR] $ \r -> do
    putStrLn $ unwords [show (ary ! (r, c)) | c <- [minC .. maxC]]

-- | Char専用版: 文字をそのまま連結してグリッド表示
printArrayChar :: UArray (Int, Int) Char -> IO ()
printArrayChar ary = do
  let ((minR, minC), (maxR, maxC)) = bounds ary
  forM_ [minR .. maxR] $ \r -> do
    putStrLn [ary ! (r, c) | c <- [minC .. maxC]]

-- | 整数を10進数の桁リストを返す。負の数は絶対値の桁リストを返す
-- >>> toDigits 1234
-- [1,2,3,4]
-- >>> toDigits 0
-- [0]
-- >>> toDigits (-123)
-- [1,2,3]
{-# INLINE toDigits #-}
toDigits :: (Integral a) => a -> [a]
toDigits 0 = [0]
toDigits n = reverse $ go (abs n)
  where
    go 0 = []
    go x = let (q, r) = x `quotRem` 10 in r : go q

-- | 桁リストを整数に復元する
-- >>> fromDigits [1,2,3,4]
-- 1234
{-# INLINE fromDigits #-}
fromDigits :: (Integral a) => [a] -> a
fromDigits = foldl' (\acc d -> acc * 10 + d) 0

{- STUArray -}
asSTU :: ST s (STUArray s i e) -> ST s (STUArray s i e)
asSTU = id

newSTUArray :: (MArray (STUArray s) e (ST s), Ix i) => (i, i) -> e -> ST s (STUArray s i e)
newSTUArray b v = asSTU $ newArray b v

-- | 部分文字列の取得 0-indexed で開始位置を指定、指定した長さの部分文字列を返す
-- >>> substring 0 5 "Hello, World"
-- "Hello"
{-# INLINE substring #-}
substring :: Int -> Int -> String -> String
substring start len = take len . drop start

-- | s に含まれる長さ k の部分文字列をすべて返す　　０（NK)
-- >>> substringsK 3 "TOYOTA"
-- ["TOY","OYO","YOT","OTA"]
{-# INLINE substringsK #-}
substringsK :: Int -> String -> [String]
substringsK k s = map (take k) $ take (length s - k + 1) $ tails s

{- End Bonsai -}



