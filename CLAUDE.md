# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## プロジェクト概要

Haskell で AtCoder の問題を解くための競技プログラミングリポジトリ。各コンテスト (`abc{NNN}/`) ごとにディレクトリがあり、問題ごとに `app/{a,b,c,...}/Main.hs` にソリューションを置く。

## 開発ワークフロー

コンテストディレクトリ (例: `abc409/`) 内で作業する:

```bash
./setting b          # 問題 b に切り替え (.curname更新 + HLS設定 + ビルド)
# app/b/Main.hs を編集
./building           # stack build (現在の問題のみ)
./checking           # ビルド + oj t でサンプルケーステスト
./executing          # 実行
./ghcing             # GHCi 起動
```

- `.curname` ファイルが現在の問題を保持 (`source .curname` で `$CURRENT_STACK_EXE_ENTRY` を取得)
- テストケースは `app/{letter}/tests/sample-{N}.{in,out}` に配置

## ビルドシステム

- **Stack** (LTS 23.28) + **ac-library-hs** 1.5.3.1
- 言語: GHC2021 + 拡張 (CPP, DataKinds, LambdaCase, OverloadedStrings 等)
- 各コンテストディレクトリが独立した Stack プロジェクト

## Main.hs テンプレート構造

各問題の `Main.hs` は固定構造を持つ。**`solve` 関数のみを実装**し、`decode`/`encode`/型定義を問題に合わせて変更する:

```
型定義 (Dom, Codom, Solver) → decode (入力パース) → encode (出力整形) → solve (ロジック) → main (パイプライン) → AsToken (固定) → Bonsai ライブラリ (末尾の汎用関数群)
```

Bonsai ライブラリはテンプレート末尾に埋め込まれた汎用関数群 (累積和、BFS、Dijkstra、DP半環、ModInt、nCr 等)。

## 学習支援エージェント

`.github/agents/ac-haskell.agent.md` にヒント提供・コードレビュー・デバッグ支援・振り返りメモ生成の詳細仕様がある。コードレビュー時は `.github/agents/ac-haskell-patterns.md` のパターン集と照合すること。

## Haskell コーディング規約 (AtCoder)

- 入出力: `ByteString` (高速I/O)
- 正格評価: `foldl'`, `Data.Map.Strict`, `BangPatterns` で TLE 回避
- データ構造: `Data.Vector.Unboxed` / `Data.Array.Unboxed` を優先
- `Int` (64bit) のオーバーフローに注意 → 必要なら `Integer` or `ModInt`
- ac-library-hs (`AtCoder.SegTree`, `AtCoder.Dsu`, `AtCoder.Extra.Bisect` 等) を積極活用

## 参考ファイル

- `idiom.md` — Haskell イディオム集 (List, Map, Array, Vector, 二分探索等)
- `AtCoder振り返りNotionガイド.md` — 振り返りデータベースのスキーマ定義
