#!/usr/bin/env python3
"""Pre-commit gate: elisp-autofmt formatting check for this repo's Emacs
Lisp source (materialized by `swe-repo fix` — see rules.py's
_fix_elisp_format_gate).

WARN-only, exits 0 unconditionally. This is a deliberate first-rollout
posture: elisp-autofmt's check is BINARY per file (compliant or not) rather
than a continuous metric like a line-count ceiling, so there is no natural
"grandfathered ceiling" to record for a file that's simply reformat-or-not.
On a repo's FIRST run the expected result is "most files differ" — a
grandfather list would have to enumerate nearly the entire tree, which
inverts what a grandfather list is for. WARN reports the real number on
every relevant commit; flipping this to a hard gate (outright, or a
grandfather-ceiling-style ratchet keyed off a per-file "already compliant"
set captured at flip time) is a deliberate follow-up a human decides on,
not something this gate does automatically.

Scope: every *.el file in the repo, excluding vendored/generated/build noise
dirs and test-*.el files — generic, not hardcoded to any one repo's layout.

Runs the vendored elisp-autofmt (scripts/elisp-quality/lint/vendor/
elisp-autofmt.el — no tagged release exists upstream, pinned to a commit SHA
recorded in that file's own header comment) via the non-mutating check
driver scripts/elisp-quality/lint/elisp-autofmt-batch.el: each file is
loaded into a throwaway buffer, formatted in memory, compared against its
on-disk content, and the buffer is killed unsaved — never writes to a file
(gofmt --check / rustfmt --check semantics, not `elisp-autofmt --check`
because upstream has no such flag; see that driver's own header comment).
Requires Python 3.10+ on PATH (elisp-autofmt's own runtime dependency,
shelled out to from the Elisp side) in addition to Emacs.
"""

from __future__ import annotations

import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
LINT_DIR = REPO_ROOT / "scripts" / "elisp-quality" / "lint"
VENDOR_DIR = LINT_DIR / "vendor"
BATCH_DRIVER = LINT_DIR / "elisp-autofmt-batch.el"

# Same noise-dir pruning as check-elisp-complexity.py (kept duplicated
# rather than shared-imported — these two scripts are meant to be
# self-contained and independently copyable, same as the reference
# implementation they were generalized from).
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
    # This gate's own batch driver lives in scripts/elisp-quality/lint/
    # (elisp-autofmt-batch.el sits directly there, not under vendor/) —
    # excluded so this script never format-checks its own tooling as if
    # it were repo code under scrutiny.
    "elisp-quality",
}


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


def _emacs_available() -> bool:
    return shutil.which("emacs") is not None


def _run_batch(
    files: list[Path], python_bin: str, cache_dir: str
) -> tuple[list[str], list[str], str]:
    cmd = [
        "emacs",
        "-Q",
        "--batch",
        "--eval",
        f'(add-to-list \'load-path "{VENDOR_DIR}")',
        "--eval",
        f'(setq elisp-autofmt-python-bin "{python_bin}")',
        "--eval",
        f'(setq elisp-autofmt-cache-directory "{cache_dir}")',
        "-l",
        str(BATCH_DRIVER),
        *[str(f) for f in files],
    ]
    proc = subprocess.run(cmd, capture_output=True, text=True, cwd=REPO_ROOT)
    diff_files: list[str] = []
    error_lines: list[str] = []
    for line in proc.stdout.splitlines():
        if line.startswith("DIFF "):
            diff_files.append(line[len("DIFF ") :])
        elif line.startswith("ERROR "):
            error_lines.append(line[len("ERROR ") :])
    return diff_files, error_lines, proc.stderr


def main() -> int:
    if not _emacs_available():
        print(
            "WARN: check-elisp-format: emacs not found on PATH, skipping.",
            file=sys.stderr,
        )
        return 0

    if sys.version_info < (3, 10):
        print(
            f"WARN: check-elisp-format: elisp-autofmt requires Python 3.10+ at runtime "
            f"(this interpreter is {sys.version_info.major}.{sys.version_info.minor}) — skipping.",
            file=sys.stderr,
        )
        return 0

    python_bin = sys.executable
    files = _target_files()
    if not files:
        return 0

    def _rel(p: str) -> str:
        try:
            return str(Path(p).resolve().relative_to(REPO_ROOT))
        except ValueError:
            return p

    with tempfile.TemporaryDirectory(prefix="elisp-autofmt-cache-") as cache_dir:
        diff_files, error_lines, stderr = _run_batch(files, python_bin, cache_dir)

    if error_lines:
        print(
            f"WARN: check-elisp-format: elisp-autofmt could not format {len(error_lines)} "
            f"file(s) at all (not a formatting-style issue — likely a real syntax/parse "
            f"problem worth looking at):",
            file=sys.stderr,
        )
        for line in error_lines:
            print(f"  {line}", file=sys.stderr)
    elif not diff_files and stderr.strip():
        print(
            "WARN: check-elisp-format: batch driver produced no DIFF/ERROR lines but wrote "
            "to stderr — it may have crashed before checking any file:",
            file=sys.stderr,
        )
        print(stderr, file=sys.stderr)

    if diff_files:
        print(
            f"WARN: check-elisp-format: {len(diff_files)}/{len(files)} file(s) would be "
            f"reformatted by elisp-autofmt — not blocking (first-rollout WARN posture, see "
            f"this script's own module docstring). Preview a diff with:\n"
            f"  python3 {VENDOR_DIR / 'elisp-autofmt.py'} --stdout <file> | diff <file> -",
            file=sys.stderr,
        )
        for f in diff_files:
            print(f"  {_rel(f)}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
