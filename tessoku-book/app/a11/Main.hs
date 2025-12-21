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

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
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
import Data.Vector.Fusion.Bundle qualified as VFB
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Debug.Trace qualified as Debug
import Text.Printf

debug :: Bool
debug = False

type I = Int
type O = Int

type Dom = (Int, Int, [Int])
type Codom = Int

type Solver = Dom -> Codom

{-# INLINE solve #-}
solve :: Solver
solve (n,x,as) =
  let v = VU.fromList as
      bSearch :: Int -> Int -> Int
      bSearch !l !r
        | l - r == 1 = r
        | otherwise =
          let mid = (l+r) `div` 2
              val = v VU.! mid
          in case compare val x of
             EQ -> mid + 1
             LT -> bSearch (mid + 1) r
             GT -> bSearch l (mid - 1)
  in bSearch 0 (n-1)

  -- trace (show x) def

{-# INLINE decode #-}
decode :: [[I]] -> Dom
decode = \case
  [n,x] : as : _ -> (n, x, as)
  _ -> invalid $ "toDom: " ++ show @Int __LINE__

{-# INLINE encode #-}
encode :: Codom -> [[O]]
encode r = [[r]]

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
encode r = map (:[]) r
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

-- | 1 次元の累積和配列を作成する。
-- >>> csum1D 4 [1..10] :: UArray Int Int
-- array (0,4) [(0,0),(1,1),(2,3),(3,6),(4,10)]
csum1D :: (IArray UArray e, Num e) => Int -> [e] -> UArray Int e
csum1D n = listArray (0, n) . scanl' (+) 0
{-# INLINE csum1D #-}

-- | 1 次元の累積和配列を元に区間和を求める。
-- >>> let csum = csum1D 4 [1..10] :: UArray Int Int
-- >>> csum +! (1, 2)
-- 5
(+!) :: (IArray UArray e, Num e) => UArray Int e -> (Int, Int) -> e
(+!) ary (!l, !r) = ary ! succ r - ary ! l
{-# INLINE (+!) #-}

{- End Bonsai -}
