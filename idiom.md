## List

```haskell
{----- 分割 -----}
-- リストを要素n個ずつに分割
-- Data.List.Split.chunksOf
ghci> chunksOf 3 [1..10]
[[1,2,3],[4,5,6],[7,8,9],[10]]
```

```haskell
{- partition :: (a -> Bool) -> [a] -> ([a], [a]) -}
-- リストを条件を満たす要素と満たさない要素に分割する

ghci> let (evens, odds) = partition even [1..10]
ghci> (evens, odds)
([2,4,6,8,10],[1,3,5,7,9])

ghci> let (passed, failed) = partition (\(_, score) -> score >= 60) [("Alice", 85), ("Bob", 40), ("Charlie", 90), ("Dave", 55)]
ghci> passed
[("Alice",85),("Charlie",90)]
ghci> failed
[("Bob",40),("Dave",55)]
ghci>

{- bimap :: Data.Bifunctor.Bifunctor p => (a -> b) -> (c -> d) -> p a c -> p b d -}
-- bimap f g (x, y) === (f x, g y) -- bimapを使うとデータ構造を分解（パターンマッチ）せずに中身を変換できるため、コードが宣言的で読みやすくなる
-- 2つの型引数を持つデータ構造（タプル (a, b) や Either a b など）の両方の要素に対して、関数を適用するためのモジュール Data.Bifunctor

-- タプルの両側を同時に変更する
ghci> bimap (+1) length (10, "hello")
(11,5)

-- 座標(x,y)の両方を同じ倍率で拡大
ghci> bimap (*2) (*2) (3.0, 4.0)
(6.0,8.0)

-- bimapとpartition両方使う例 abc255_b
ghci> let as = [2,3] -- 光を持っている人のindex(1-based)リスト
ghci> let xys = [(0,0),(0,1),(1,2),(2,0)] -- 全員の座標
ghci> let bm = map snd
ghci> let (lights, unLights) = bimap bm bm . partition (flip elem as . fst) $ zip [1..] xys
([(0,1),(1,2)],[(0,0),(2,0)]) -- (光を持っている人の座標、持っていない人の座標)
```

```haskell
{----- uniq -----}
-- 順序は保存しなくよければSetを使う（Ordのインスタンスである必要がある）O(NlogN)
ghci> S.fromList [["..","##"],["##","#."],["..","##"],["##",".#"]]
fromList [["##","#."],["##",".#"],["..","##"]]

-- 順序保存したい場合はnubOrd O(NlogN)
ghci> nubOrd [["..","##"],["##","#."],["..","##"],["##",".#"]]
[["..","##"],["##","#."],["##",".#"]]

{- 条件を満たす最初の -}
-- 条件を満たす最初の要素
ghci> fromJust $ find (15 <=) [10..]
15
-- 条件を満たす最初のインデックス
ghci> fromJust $ findIndex (15 <=) [10..]
5
```

```haskell
-- 挿入（intercalateを思い出そうとするとjoinが出てきてしまっていつも思い出せないのでメモ）
>>> intercalate ", " ["Lorem", "ipsum", "dolor"]
"Lorem, ipsum, dolor"
```

```haskell
-- 奇数列挙
ghci> n = 10
ghci> [1,3..n]
[1,3,5,7,9]
ghci>
```

```haskell
-- `iterate` は「同じ関数を繰り返し適用した結果のリスト」が欲し意図機の定番
ghci > take 5 $ iterate (*2) 1 -- ２の累乗列
[1,2,4,8,16]
```

```haskell
-- ２次元グリッドの90度回転
ghci> rotateR90 = transpose . reverse
ghci> rotageR90 [[0,1,2],[3,4,5],[6,7,8]]
[[6,3,0],[7,4,1],[8,5,2]]
```

```haskell
-- ２次元グリッドの切り出し(0-based index)
subGrid :: Show a => Int -> (Int, Int) -> [[a]] -> [[a]]
subGrid m (r, c) = map (take m . drop r) . (take m . drop c)
ghci> grid = chunksOf 3 [1..9] :: [[Int]]
ghci> subGrid 2 (1,0) grid
[[2,3],[5,6]]
```

```haskell
{----- all / and / any / or の使い分け -----}
-- and  :: [Bool] -> Bool         — Bool のリストを受け取る
-- all  :: (a -> Bool) -> [a] -> Bool  — 述語を受け取る（map + and のショートカット）
-- or   :: [Bool] -> Bool
-- any  :: (a -> Bool) -> [a] -> Bool  — 述語を受け取る（map + or のショートカット）

ghci> and [True, True, False]
False
ghci> all even [2, 4, 6]
True
ghci> all even [2, 3, 6]
False
-- all f xs == and (map f xs)
-- any f xs == or  (map f xs)
```

## Map

```haskell
-- MapのminimumとminView, minViewWithKey
ghci> minimum $ M.fromList [(1,'x'),(2,'b')] -- 最小の値を返す（キーは関係無し）
'b'
ghci> M.minView $ M.fromList [(1,'x'),(2,'b')] -- キーが最小の値とそれを除いたMapを返す
Just ('x',fromList [(2,'b')])
ghci> M.minViewWithKey  $ M.fromList [(1,'a'),(2,'b')] -- キーが最小のキーと値のタプルとそれを除いたMapを返す
Just ((1,'a'),fromList [(2,'b')])

-- ===== fromListWith: キーごとに集約する万能ツール =====
-- 型: fromListWith :: (a -> a -> a) -> [(k, a)] -> Map k a
-- 結合関数 f は f new old の順で渡される（同じキーが衝突したとき、新しい方が左）
-- foldl' + insertWith を書くより簡潔。グループ集約で頻出。

-- パターン1: 出現数カウント
ghci> M.fromListWith (+) $ map (,1) "abracadabra"
fromList [('a',5),('b',2),('c',1),('d',1),('r',2)]

-- パターン2: キーごとに最大値
ghci> IM.fromListWith max [(1,3),(2,5),(1,7),(2,2)]
fromList [(1,7),(2,5)]

-- パターン3: キーごとにリスト集約 (Group By)
-- (++) は左側が常に長さ1なので O(1)/挿入で安全。
-- 「左が伸びる foldl (++)」のような O(N²) パターンとは別物。
ghci> IM.fromListWith (++) [(1,[3]),(2,[5]),(1,[7]),(2,[2])]
fromList [(1,[7,3]),(2,[2,5])]

-- ===== insertWith: 走査しながら逐次更新 =====
-- 型: insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
-- 結合関数 f は f new old の順で渡される（fromListWith と同じ）
-- key が無ければ new をそのまま挿入。あれば f new old を挿入。
-- 「未登場なら初期値、既出なら関数適用」が分岐なしで書ける。

-- パターン: 出現数を逐次カウント（mapAccumL と組み合わせ）
-- 例: ABC261 C — S_i 以前に同じ文字列が X 個あれば "S_i(X)" を出力
f :: M.Map String Int -> String -> (M.Map String Int, String)
f cntMap s =
  let cnt     = M.findWithDefault 0 s cntMap            -- 未登場は 0
      cntMap' = M.insertWith (+) s 1 cntMap             -- 「未登場→1, 既出→+1」を一本化
      out     = if cnt == 0 then s else s ++ "(" ++ show cnt ++ ")"
  in (cntMap', out)

-- M.!? で Maybe を受けて case 分岐するより、findWithDefault でセンチネル値
-- (この例では 0) を使うと分岐が消える。格納値が常に >0 のときに有効。

-- ===== findWithDefault: lookup の Maybe を消す =====
-- 型: findWithDefault :: Ord k => a -> k -> Map k a -> a
ghci> M.findWithDefault 0 'z' (M.fromList [('a',3),('b',5)])
0
ghci> M.findWithDefault 0 'a' (M.fromList [('a',3),('b',5)])
3
```

## Array

```haskell
-- 部分配列を取り出す(インデックスは元のarrのインデックスのまま）
ixmap ((row1,col1),(row2,col2)) id arr

ghci> printArray arr
1 2 3
4 5 6
7 8 9
ghci> let arr1 = ixmap ((1,1),(2,2)) id arr
ghci> printArray arr1
5 6
8 9
ghci> arr1
array ((1,1),(2,2)) [((1,1),5),((1,2),6),((2,1),8),((2,2),9)]

-- 部分配列を取り出す（インデックスをrebaseする）
-- | 2次元配列から矩形領域を切り出し、インデックスを (0,0) 起点にリベースする
subArrayRebased :: (IArray a e) => ((Int, Int), (Int, Int)) -> a (Int, Int) e -> a (Int, Int) e
subArrayRebased ((r1, c1), (r2, c2)) = ixmap ((0, 0), (r2 - r1, c2 - c1)) (\(r, c) -> (r + r1, c + c1))

ghci> subArrayRebased ((1,1),(2,2)) arr
array ((0,0),(1,1)) [((0,0),5),((0,1),6),((1,0),8),((1,1),9)]
ghci> printArray $ subArrayRebased ((1,1),(2,2)) arr
5 6
8 9

-- 連想配列をarrayにする
ghci> aList
[((1,2),1),((2,2),2),((2,1),3),((1,1),4)]
ghci> array ((1,1), (2,2)) aList  :: Array (Int, Int) Int
array ((1,1),(2,2)) [((1,1),4),((1,2),1),((2,1),3),((2,2),2)]

-- accumArrayで出現回数をカウントする（accumArrayはインデックスごとに畳み込む）
ghci> accumArray (+) 0 ('a','e') [(x,1)| x <- "abbacadaba"] :: UArray Char Int
array ('a','e') [('a',5),('b',3),('c',1),('d',1),('e',0)]

{- グループごとの最大・最小（Max/Min per Bucket)
用途： DP（動的計画法）の初期化や、グリッド上の特定の行・列における最大値を求める場合。「インデックス $i$ に対応する値の中で、最大のものはどれか？」を一括で計算します。蓄積関数: max または min配列型: UArray Int Int などコード例：「重さ $w$ の荷物の中で、最大の価値 $v$ を知りたい」という場合（ナップサック問題の前処理など）。データ：(重さ1, 価値10), (重さ2, 価値20), (重さ1, 価値15)-}
items :: [(Int, Int)]
items = [(1, 10), (2, 20), (1, 15)]
maxValByWeight :: UArray Int Int
maxValByWeight = accumArray max 0 (1, 2) items
-- 結果:
-- maxValByWeight ! 1 == 15 (10と15のmax)
-- maxValByWeight ! 2 == 20

{- 隣接リストでグラフ -}
ghci> n
4
ghci> xs -- 無向辺のリスト
[[1,2],[2,3],[3,4],[3,5]]
ghci> edges = concatMap (\[a,b] -> [(a,b),(b,a)]) xs
ghci> edges -- xsを両方向に展開したもの
[(1,2),(2,1),(2,3),(3,2),(3,4),(4,3),(3,5),(5,3)]
ghci> adj = accumArray (flip (:)) [] (1, n) edges -- 隣接リスト
ghci> adj :: Array Int [Int]
array (1,5) [(1,[2]),(2,[3,1]),(3,[5,4,2]),(4,[3]),(5,[3])]
ghci>

-- mapAccumL を使った貪欲法の例
-- 区間スケジューリング問題：終了時間でソートした区間のリストに対して、貪欲法で最大非重複部分集合のサイズを求める
(_, res) = mapAccumL f (minBound :: Int) $ sortOn snd [(123,86399),(1,86400),(86399,86400)]
  where
    f acc (l,r)
      | acc <= l = (r, 1)
      | otherwise = (acc, 0)
sum res -- 2
```

### MArray(STUArray)
```haskell
{-
問題文

1,2,…,N の番号が付いた N 個の箱があります。最初は全ての箱が空です。

これから Q 個のボールが順番にやってきます。
高橋君は、数列 X=(X1​,X2​,…,XQ​) に従ってボールを箱に入れます。
具体的には、 i 番目にやってきたボールに次の処理を行います。

    Xi​≥1 である場合 : このボールを、箱 Xi​ に入れる。
    Xi​=0 である場合 : このボールを、現在入っているボールが最も少ない箱のうち番号が最小である箱に入れる。

それぞれのボールをどの箱に入れたかを求めてください。

制約

    入力は全て整数
    1≤N≤100
    1≤Q≤100
    0≤Xi​≤N
-}
{-# INLINE solve #-}
solve :: Solver
solve (n,q,as) = runST $ do
  cnt <- newArray (1,n) 0 :: ST s (STUArray s Int Int)
  forM as $ \x -> do
    i <- if x /= 0
      then pure x
      else do
        (frozen :: UArray Int Int) <- freeze cnt
        pure $ fst $ minimumBy (comparing snd) $ assocs frozen
    modifyArray cnt i (+1)
    pure i
```

## Vector

```haskell
{- 生成 -}
-- replicate すべての要素を同じ値で埋めたいとき
let v = VU.replicate 5 0
-- [0, 0, 0, 0, 0]

-- generate インデックス (i) だけで値が決まるとき.前の要素の値や、外部の状態に依存しない場合に使います。並列処理や最適化が効きやすく、最も汎用的です。
let v = VU.generate 5 (\i -> i * i)
-- [0, 1, 4, 9, 16]

-- iterateN 直前の値から次の値を計算するとき.単純な漸化式 A[i+1]=f(A[i]) の場合に使います。
let v = VU.iterateN 5 (*2) 1
-- [1, 2, 4, 8, 16]

-- unfoldrN 状態を持ち回って値を生成したいとき.iterateN よりも柔軟です。「出力する値」と「次に渡す状態」を別に管理できます。
-- フィボナッチ数列のように状態が2つある場合
-- 状態: (現在の値, 次の値)
let v = VU.unfoldrN 10 (\(a, b) -> Just (a, (b, a + b))) (0, 1)
-- [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

-- constructN これまでに生成したベクタ全体（または一部）を参照したいとき.生成中のベクタ v を引数に取ります。例えば、「累積和」や「素数判定（過去の素数で割ってみる）」などを1パスで作るのに使えます。
-- 累積和のような処理（前の要素までの合計 + 現在のインデックス）
-- v は「現在までに生成されたベクタ」
let v = VU.constructN 5 (\v ->
            if VU.null v
            then 0
            else VU.last v + VU.length v
        )
-- i=0: [] -> 0
-- i=1: [0] -> 0 + 1 = 1
-- i=2: [0,1] -> 1 + 2 = 3
-- i=3: [0,1,3] -> 3 + 3 = 6
-- i=4: [0,1,3,6] -> 6 + 4 = 10
-- 結果: [0, 1, 3, 6, 10]

{- sort -}
VU.modify VAI.sort $ VU.fromList qs -- ListをsortしてからVectorにするより、Vectorにしてからsortしたほうがだいぶ速い、lengthもListのlengthより、VectorのvLengthのほうがだいぶ速い
VU.modify (VAI.sortBy (comparing Down)) $ VU.fromList as -- 降順

-- sndで昇順ソート
sortBySnd :: VU.Vector (Int, Int) -> VU.Vector (Int, Int)
sortBySnd v = VU.modify (VAI.sortBy (comparing snd)) v

-- sndで降順ソート
sortBySndDesc :: VU.Vector (Int, Int) -> VU.Vector (Int, Int)
sortBySndDesc v = VU.modify (VAI.sortBy (comparing (Down . snd))) v

{- nub -}
-- https://publish.obsidian.md/naoya/atcoder/ABC390+%E6%8C%AF%E3%82%8A%E8%BF%94%E3%82%8A
-- 普通のnubOrdだと遅いということで
nubOrd' :: (VUM.Unbox a, Ord a) => [a] -> [a]
nubOrd' xs = (VU.toList . VU.uniq . VU.modify (VAI.sortBy compare) . VU.fromList) xs
```

## Mutable Vector (VUM)
```haskell
{-
問題文

1,2,…,N の番号が付いた N 個の箱があります。最初は全ての箱が空です。

これから Q 個のボールが順番にやってきます。
高橋君は、数列 X=(X1​,X2​,…,XQ​) に従ってボールを箱に入れます。
具体的には、 i 番目にやってきたボールに次の処理を行います。

    Xi​≥1 である場合 : このボールを、箱 Xi​ に入れる。
    Xi​=0 である場合 : このボールを、現在入っているボールが最も少ない箱のうち番号が最小である箱に入れる。

それぞれのボールをどの箱に入れたかを求めてください。

制約

    入力は全て整数
    1≤N≤100
    1≤Q≤100
    0≤Xi​≤N
-}
{-# INLINE solve #-}
solve :: Solver
solve (n,q,as) = runST $ do
  cnt <- VUM.replicate n (0 :: Int)
  forM as $ \x -> do
    i <- if x /= 0
      then pure x
      else do
        frozen <- VU.freeze cnt
        pure $ VU.minIndex frozen + 1
    VUM.modify cnt (+1) (i-1)
    pure i
```
```haskell
{- foldM + VUM.read/VUM.write で状態更新しながらクエリ処理 -}
-- パターン: immutable Vector を thaw して mutable にし、foldM でクエリを順に処理
-- [Int] から直接 mutable にする場合は VU.unsafeThaw . VU.fromList（一時値なので安全）

-- 例: 数列 A, B があり、クエリごとに要素を更新して Σmin(A_k, B_k) を出力 (ABC420 C)
solve (n, q, as0, bs0, qs) = runST $ do
  av <- VU.thaw as0           -- VU.Vector → VUM.MVector に変換 (O(N) コピー)
  bv <- VU.thaw bs0
  let initSum = VU.sum $ VU.zipWith min as0 bs0
  reverse . snd <$> foldM (\(!s, acc) (c, x, v) -> do
    let i = x - 1
    a <- VUM.read av i         -- 現在の値を読む O(1)
    b <- VUM.read bv i
    let oldMin = min a b
    case c of
      'A' -> VUM.write av i v >> let s' = s - oldMin + min v b in return (s', s' : acc)  -- 書き換え O(1)
      _   -> VUM.write bv i v >> let s' = s - oldMin + min a v in return (s', s' : acc)
    ) (initSum, []) qs

-- ポイント:
-- ・アキュムレータに !s で BangPattern → 遅延蓄積による TLE 防止
-- ・結果は逆順で溜まるので最後に reverse
-- ・VU.thaw の代わりに VU.unsafeThaw を使うとコピー1回分節約できる
```

## Maybe

```haskell
-- [Maybe a] から Justのものだけ取り出す
catMaybes [Just "hoge", Nothing, Just "fuga", Nothing]
-- 結果: ["hoge", "fuga"]
```

## Bits

```haskell
-- ビット全探索: 部分集合を列挙
-- n個の要素からの全部分集合を列挙
solve n =
  [ selected
  | mask <- [0 .. bit n - 1]                    -- 0 から 2^n - 1 まで
  , let selected = [i | i <- [0..n-1], testBit mask i]  -- maskで選ばれた要素
  ]

-- 例: solve 3
-- [[],[0],[1],[0,1],[2],[0,2],[1,2],[0,1,2]]
```

## 二分探索

```haskell
-- 二分探索で値に対する条件で検索してindexを返す
ghci> let xs = VU.fromList [10, 10, 20, 20, 30, 30]
ghci> let n = vLength xs
ghci> let p = (>= 20)
ghci> AB.minLeft 0 n (p . (xs VG.!))
2

ghci> bisect 0 n (p . (xs VG.!))
(1,2) -- (ng, ok)
ghci>

-- 二分探索まじめにやらなくてもIntSetのmemberやlookupGTなどで（性能的にも十分）いけるケースもある
-- 鉄則本A14 半分全列挙 （制限時間5s）
-- Vectorで二分探索版 44ms
solve :: Solver
solve (_,k,as,bs,cs,ds) =
      qs = [c + d | c <- cs, d <- ds, c + d < k]
      qv = VU.modify VAI.sort $ VU.fromList qs
      lenQs = vLength qv
      -- qsの中にp + qi == kとなるqiがあるか？(k-p以上の値を持つ最小のindexがあり、かつそれがk-pと一致するか？)
      check p =
        let idx = AB.minLeft 0 lenQs (\i -> p + qv VG.! i >= k)
        in idx < lenQs && qv VG.! idx == k - p
  in yn $ any check ps

-- IntSetでmember版 197ms。check関数の部分が簡潔でロジックもわかりやすくバグりにくい
solve (_,k,as,bs,cs,ds) =
  let ps = [a + b | a <- as, b <- bs, a + b < k]
      qs = IS.fromList [c + d | c <- cs, d <- ds, c + d < k]
  in yn $ any (\p -> IS.member (k - p) qs) ps
```

## Data.Sequence (キュー / 両端キュー)

```haskell
import Data.Sequence qualified as Q

{----- 生成 -----}
Q.empty              -- 空の Seq
Q.singleton 42       -- 要素1つの Seq
Q.fromList [1,2,3]   -- リストから生成

{----- 追加 (すべて O(1)) -----}
Q.empty Q.|> 3       -- 末尾に追加 (enqueue): fromList [3]
5 Q.<| Q.singleton 3 -- 先頭に追加:           fromList [5,3]

{----- 先頭の取り出し (dequeue) O(1) -----}
case q of
  Q.Empty      -> ...        -- キューが空
  x Q.:<| rest -> ...        -- x: 先頭要素, rest: 残り

{----- 末尾の取り出し O(1) -----}
case q of
  Q.Empty      -> ...        -- キューが空
  rest Q.:|> x -> ...        -- x: 末尾要素, rest: 残り

{----- その他の操作 -----}
Q.length q            -- 長さ O(1)
Q.index q i           -- i番目の要素 O(log(min(i,n-i)))
q Q.>< q'             -- 2つのSeqを結合 O(log(min(n1,n2)))
Q.null q              -- 空かどうか O(1)

{----- 典型的な使い方: クエリ逐次処理 (mapAccumL) -----}
-- 状態(キュー)を持ちながらクエリを処理し、出力はMaybeで表現
solve (_, qs) = catMaybes mRes
  where
    (_, mRes) = mapAccumL f Q.empty qs
    f :: Q.Seq Int -> [Int] -> (Q.Seq Int, Maybe Int)
    f queue [1,x] = (queue Q.|> x, Nothing)   -- enqueue
    f (h Q.:<| rest) [2] = (rest, Just h)      -- dequeue & 出力
    f _ _ = error "unreach"

{----- 典型的な使い方: BFS -----}
-- Bonsai の bfs 関数で使われているパターン
let loop q = case q of
      Q.Empty -> ...                      -- 探索終了
      v Q.:<| q' -> do
        ...
        let q'' = q' Q.|> next            -- 次の頂点をenqueue
        loop q''
in loop (Q.singleton start)
```

## 数値計算

```haskell
-- 一桁の数字 a を 1 ずつ増やして一桁の数字 b にするために必要な回数
(b - a) mod 10
```

```haskell
-- 組み合わせの数

-- k個のなかから2個選ぶ組み合わせの数
comb2 :: Int -> Int
comb2 k = k * (k-1) `div` 2

-- k個のなかから3個選ぶ組み合わせの数
comb3 :: Int -> Int
comb3 k = k * (k-1) * (k-2) `div` 6
```

## ループ

```haskell
-- ループで回して特定の値になったら終了の例（終了の時の値が単一ならunfoldrで書けるかもだけど、複数（この場合はTrue,False）ある場合はunfoldrでは書けない気がする）
loop is k
  | k == 1 = True           -- 終了条件と戻り値1
  | IS.member k is = False  -- 終了条件と戻り値2
  | otherwise = loop is1 k1 -- それ以外は再帰でループ
    where
      is1 = IS.insert k is
      k1 = sum $ map (^2) $ toDigits k
```

```haskell
{- unfoldr :: (b -> Maybe (a, b)) -> b -> [a] -}
-- foldr の逆。シード b から「出力要素 a と次の状態 b」を繰り返し生成してリストを作る。
-- Nothing を返した時点で終了。
-- 「初期値から繰り返し変換してリストを作る」場面の定番。
-- mapAccumL は「既存リストを走査しつつ状態を持つ」、unfoldr は「シードから新しいリストを生成」。

-- n から 1 までの降順リスト
ghci> unfoldr (\x -> if x <= 0 then Nothing else Just (x, x-1)) 5
[5,4,3,2,1]

-- 整数を 2 進数の各桁(下位から)に分解
ghci> unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 2, x `div` 2)) 13
[1,0,1,1]

-- ABC216 C: N から 0 まで ÷2/−1 で逆走させて操作列を作る
-- (N が偶数なら ×2 を逆向きに、奇数なら +1 を逆向きに)
solve :: Int -> String
solve = reverse . unfoldr go
  where
    go 0 = Nothing
    go x
      | odd x     = Just ('A', x - 1)
      | otherwise = Just ('B', x `div` 2)
```
