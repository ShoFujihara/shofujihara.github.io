# Site Management Notes

## Workflow Shortcuts

### rcp - Render, Commit, Push

サイト更新時の一連の作業:

```bash
# 1. Render (Quartoでサイトをビルド)
quarto render

# 2. Commit (変更をコミット)
git add .
git commit -m "Update site"

# 3. Push (GitHubにプッシュ)
git push
```

## Build Commands

- `quarto render` - サイト全体をビルド
- `quarto preview` - ローカルプレビュー

## Directory Structure

- `docs/` - ビルド出力 (GitHub Pages用)
- `*.qmd` - Quartoマークダウンソースファイル
- `_quarto.yml` - Quarto設定
- `custom.scss` - カスタムテーマ (ダークグリーン)
