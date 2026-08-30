#!/usr/bin/env bash
# shellcheck disable=SC2034,SC2148,SC1091
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT"

python3 ops/check_secrets.py
python3 -m py_compile admin/app.py admin/test_app.py
python3 admin/test_app.py -v
python3 -m unittest discover -s fm-domain -p 'test_*.py' -v
if command -v cabal >/dev/null 2>&1; then
  project_args=()
  if [[ -f cabal.project.local ]]; then
    project_args+=(--project-file=cabal.project.production)
  fi
  cabal "${project_args[@]}" build cosmobot cosmocode -j all
  cabal "${project_args[@]}" test cosmobot -j all --test-options=--hide-successes
else
  echo "SKIP: cabal/ghc unavailable; run the Haskell build in CI or a Cabal-enabled environment." >&2
fi
git diff --check
