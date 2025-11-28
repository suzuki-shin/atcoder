# trace 関数と tracing 関数の使い方 - まとめ

## 質問

trace 関数や tracing 関数の使い方を教えて

## 回答

このコードには、デバッグ用の 2 つのヘルパー関数が定義されています。

### 1. trace 関数

```haskell
trace :: String -> a -> a
trace
  | debug = Debug.trace
  | otherwise = const id
```

**用途**: デバッグメッセージを出力しながら値を返す

**使用例**:

```haskell
solve :: Solver
solve (a, b, c, x) =
  trace "solve called" $  -- "solve called"を出力
  trace ("Input: " ++ show (a, b, c, x)) $  -- 入力値を出力
  length [(a', b', c') | a' <- [0 .. a], b' <- [0 .. b], c' <- [0 .. c], a' * 500 + b' * 100 + c' * 50 == x]
```

### 2. tracing 関数

```haskell
tracing :: (Show a) => a -> a
tracing = trace . show <*> id
```

**用途**: 値を`show`した文字列を出力しながら、その値をそのまま返す

**使用例**:

```haskell
solve :: Solver
solve (a, b, c, x) =
  let candidates = [(a', b', c') | a' <- [0 .. a], b' <- [0 .. b], c' <- [0 .. c], a' * 500 + b' * 100 + c' * 50 == x]
      result = length candidates
  in tracing result  -- resultの値を出力して、resultを返す

-- パイプライン内での使用例
calculateSum :: [Int] -> Int
calculateSum xs =
  xs
  |> filter (> 0)
  |> tracing  -- フィルター後の値を確認
  |> sum
  |> tracing  -- 合計値を確認
```

### 重要なポイント

1. **debug フラグで制御**: `debug = () == ()` (常に True)なので、現在はデバッグ出力は**有効**です

   - `debug = () /= ()` に変更すると無効化されます

2. **純粋性を保つ**: `trace`は副作用を持ちますが、Haskell では型システム上は純粋な関数として扱われます

3. **パフォーマンス**: 本番環境では`debug`を False にすることで、オーバーヘッドを最小化できます

### 実用例

```haskell
solve :: Solver
solve (a, b, c, x) =
  let combinations = [(a', b', c') | a' <- [0 .. a], b' <- [0 .. b], c' <- [0 .. c]]
      validCombos = filter (\(a', b', c') -> a' * 500 + b' * 100 + c' * 50 == x) combinations
      result = length validCombos
  in trace ("Valid combinations: " ++ show (take 5 validCombos)) $  -- 最初の5つを表示
     tracing result  -- 最終結果を表示
```

### ベストプラクティス

1. **段階的なデバッグ**: 複雑な計算の各ステップで`trace`を使って中間結果を確認
2. **条件付きデバッグ**: `debug`フラグを使って、開発時と本番時で出力を切り替え
3. **読みやすいメッセージ**: デバッグメッセージには何を出力しているか明確に記述
4. **本番前に無効化**: コミット前に`debug`フラグを False に設定することを忘れずに
