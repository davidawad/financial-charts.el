#!/usr/bin/env python3
"""Pre-commit gate: report cyclomatic/cognitive complexity for this repo's
Emacs Lisp source (materialized by `swe-repo fix` — see rules.py's
_fix_elisp_complexity_gate).

WARN-only, matching AI/plugins/swe-project-plugin-pack-elisp/tools.json's
cyclomatic_complexity and cognitive_complexity rows (both `mandatory: false`
/ `gate: "manual lane"`). This script never fails the commit: it always
exits 0. It exists so the numbers are visible on every relevant commit
instead of requiring a developer to remember to run something manually.

The two tools it drives (codemetrics + cognitive-complexity, vendored under
scripts/elisp-quality/lint/vendor/ — see that dir's own header comments for
pinned commit SHAs, neither has a real tagged release to pin to) both need
native, per-platform tree-sitter grammar binaries that must never be
committed to git and are NOT installed automatically here — that would mean
this hook doing a fresh network install on every commit, which is exactly
wrong for a non-blocking, opt-in check. Run
scripts/checks/setup-elisp-complexity-tools.sh once to build them into the
gitignored .cache/elisp-complexity-tools/ dir; until that's been done, this
script prints one WARN line and exits 0 immediately.

Scope: every *.el file in the repo, excluding vendored/generated/build noise
dirs and test-*.el files — generic, not hardcoded to any one repo's layout
(portable counterpart of the config-repo-specific
check-elisp-config-file-length.py's config/terminal/emacs/-scoped walk).

The printed WARN lines use an informational cutoff (cyclomatic >= 20 or
cognitive >= 15) so the report highlights real outliers instead of listing
every function in the codebase on every commit — this is NOT a
commit-blocking threshold (there is none; see module docstring above), just
what's worth a human's attention. Every function's numbers are still
reported by --all for anyone who wants the full picture.
"""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
LINT_DIR = REPO_ROOT / "scripts" / "elisp-quality" / "lint"
CACHE_DIR = REPO_ROOT / ".cache" / "elisp-complexity-tools"
ELPA_DIR = CACHE_DIR / "elpa"
GRAMMAR_DIR = CACHE_DIR / "treesit-grammars"
BATCH_DRIVER = LINT_DIR / "elisp-complexity-batch.el"

# Directories pruned from the repo-wide *.el walk: standard package-manager/
# build noise (mirrors AI/skills/swe-repo/rules.py's _ELISP_NOISE_DIRS) plus
# this gate's own vendored tooling and any test/ subtree (test files aren't
# held to the same complexity scrutiny as hand-authored library/config code
# — same reasoning as the reference implementation this was
# generalized from).
_NOISE_DIRS = {
    ".git",
    "straight",
    ".local",
    "elpa",
    "node_modules",
    "dist",
    "build",
    "__pycache__",
    ".claude",
    ".agent-hooks",
    "vendor",
    # This gate's own batch driver + setup script live in
    # scripts/elisp-quality/lint/ (elisp-complexity-batch.el sits directly
    # there, not under vendor/) — excluded so this script never scores its
    # own tooling as if it were repo code under scrutiny.
    "elisp-quality",
}

# Informational-only "worth printing" cutoffs — see module docstring.
CYCLOMATIC_NOTEWORTHY = 20
COGNITIVE_NOTEWORTHY = 15


def _target_files() -> list[Path]:
    files: list[Path] = []
    for path in sorted(REPO_ROOT.rglob("*.el")):
        rel_parts = path.relative_to(REPO_ROOT).parts
        if any(
            part in _NOISE_DIRS or part.startswith(".git") for part in rel_parts[:-1]
        ):
            continue
        if "test" in rel_parts[:-1]:
            continue
        files.append(path)
    return files


def _tools_installed() -> bool:
    grammar_present = any(GRAMMAR_DIR.glob("libtree-sitter-elisp.*"))
    elpa_present = ELPA_DIR.is_dir() and any(ELPA_DIR.glob("tree-sitter-langs-*"))
    return grammar_present and elpa_present


def _emacs_available() -> bool:
    return shutil.which("emacs") is not None


def _run_batch(files: list[Path]) -> list[dict]:
    cmd = [
        "emacs",
        "-Q",
        "--batch",
        "--eval",
        "(require 'package)",
        "--eval",
        f'(setq package-user-dir "{ELPA_DIR}")',
        "--eval",
        "(package-initialize)",
        "--eval",
        "(require 'tree-sitter)",
        "--eval",
        "(require 'tree-sitter-langs)",
        "--eval",
        f'(add-to-list \'treesit-extra-load-path "{GRAMMAR_DIR}")',
        "--eval",
        f'(add-to-list \'load-path "{LINT_DIR / "vendor"}")',
        "--eval",
        "(require 'codemetrics)",
        "--eval",
        "(require 'cognitive-complexity)",
        "-l",
        str(BATCH_DRIVER),
        *[str(f) for f in files],
    ]
    proc = subprocess.run(cmd, capture_output=True, text=True, cwd=REPO_ROOT)
    rows = []
    for line in proc.stdout.splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            rows.append(json.loads(line))
        except json.JSONDecodeError:
            continue
    if not rows and proc.returncode != 0:
        print("WARN: check-elisp-complexity: batch driver failed:", file=sys.stderr)
        print(proc.stderr, file=sys.stderr)
    return rows


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--all",
        action="store_true",
        help="print every function's scores, not just noteworthy ones",
    )
    args = parser.parse_args()

    if not _emacs_available():
        print(
            "WARN: check-elisp-complexity: emacs not found on PATH, skipping.",
            file=sys.stderr,
        )
        return 0

    if not _tools_installed():
        print(
            "WARN: check-elisp-complexity: native tree-sitter grammars not installed "
            "(manual lane, mandatory: false per tools.json) — run "
            "scripts/checks/setup-elisp-complexity-tools.sh once to enable real complexity "
            "reporting. Skipping for now.",
            file=sys.stderr,
        )
        return 0

    files = _target_files()
    if not files:
        return 0

    rows = _run_batch(files)
    if not rows:
        return 0

    def _rel(row: dict) -> str:
        try:
            return str(Path(row["file"]).resolve().relative_to(REPO_ROOT))
        except ValueError:
            return row["file"]

    if args.all:
        for r in sorted(rows, key=lambda r: (-r["cyclomatic"], r["file"], r["line"])):
            print(
                f"  {_rel(r)}:{r['line']} {r['name']} — cyclomatic {r['cyclomatic']}, "
                f"cognitive {r['cognitive']}",
                file=sys.stderr,
            )
        return 0

    noteworthy = [
        r
        for r in rows
        if r["cyclomatic"] >= CYCLOMATIC_NOTEWORTHY
        or r["cognitive"] >= COGNITIVE_NOTEWORTHY
    ]
    if noteworthy:
        print(
            f"WARN: check-elisp-complexity: {len(noteworthy)} function(s) at/above the "
            f"informational cutoff (cyclomatic >= {CYCLOMATIC_NOTEWORTHY} or cognitive >= "
            f"{COGNITIVE_NOTEWORTHY}) — not blocking, just worth a look "
            f"(run with --all for the full {len(rows)}-function report):",
            file=sys.stderr,
        )
        for r in sorted(noteworthy, key=lambda r: -r["cyclomatic"]):
            print(
                f"  {_rel(r)}:{r['line']} {r['name']} — cyclomatic {r['cyclomatic']}, "
                f"cognitive {r['cognitive']}",
                file=sys.stderr,
            )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
