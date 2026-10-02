#!/usr/bin/env bash
# One-time, explicit setup for the elisp complexity check.
#
# check-elisp-complexity.py drives two vendored, non-MELPA tree-sitter-based
# tools (codemetrics + cognitive-complexity) over this repo's Elisp source.
# Both need native, per-platform tree-sitter grammar binaries that must
# never be committed to git (compiled dylibs, not source) and that this
# script builds/downloads ONCE into a gitignored cache dir. The pre-commit
# hook itself never does network installs -- if this hasn't been run, it
# WARNs and skips (manual lane, mandatory: false per tools.json).
#
# The two tools need two DIFFERENT, incompatible tree-sitter stacks:
#   - codemetrics uses the OLD third-party `tree-sitter'/`tsc' package
#     (emacs-tree-sitter org) -- needs the `tsc-dyn' dynamic module plus
#     the `tree-sitter-langs' grammar bundle (which includes an elisp.dylib).
#   - cognitive-complexity uses Emacs's OWN built-in `treesit' (29+) --
#     needs the Elisp grammar built via `treesit-install-language-grammar'
#     from Wilfred/tree-sitter-elisp (no ELPA grammar package exists for it).
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CACHE_DIR="$REPO_ROOT/.cache/elisp-complexity-tools"
ELPA_DIR="$CACHE_DIR/elpa"
GRAMMAR_DIR="$CACHE_DIR/treesit-grammars"

mkdir -p "$ELPA_DIR" "$GRAMMAR_DIR"

echo "=== Installing tree-sitter + tsc + tree-sitter-langs (codemetrics stack) into $ELPA_DIR ==="
emacs -Q --batch \
  --eval "(require 'package)" \
  --eval "(setq package-user-dir \"$ELPA_DIR\")" \
  --eval "(setq package-archives '((\"melpa\" . \"https://melpa.org/packages/\") (\"gnu\" . \"https://elpa.gnu.org/packages/\")))" \
  --eval "(package-initialize)" \
  --eval "(package-refresh-contents)" \
  --eval "(dolist (p '(tree-sitter tree-sitter-langs)) (unless (package-installed-p p) (package-install p)))"

echo "=== Installing Elisp treesit grammar (cognitive-complexity stack) into $GRAMMAR_DIR ==="
emacs -Q --batch \
  --eval "(require 'treesit)" \
  --eval "(add-to-list 'treesit-extra-load-path \"$GRAMMAR_DIR\")" \
  --eval "(setq treesit-language-source-alist '((elisp \"https://github.com/Wilfred/tree-sitter-elisp\")))" \
  --eval "(if (file-expand-wildcards (expand-file-name \"libtree-sitter-elisp.*\" \"$GRAMMAR_DIR\")) (message \"Grammar already installed, skipping\") (treesit-install-language-grammar 'elisp \"$GRAMMAR_DIR\"))"

echo "=== Done. scripts/checks/check-elisp-complexity.py will now run for real (was WARN-skipping before this). ==="
