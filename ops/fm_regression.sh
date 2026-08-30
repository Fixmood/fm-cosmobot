#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT"

python3 ops/check_secrets.py
python3 -m unittest discover -s fm-domain -p 'test_*.py' -v
project_args=()
if [[ -f cabal.project.local ]]; then
  project_args+=(--project-file=cabal.project.production)
fi
cabal "${project_args[@]}" build cosmobot cosmocode -j all
cabal "${project_args[@]}" test cosmobot -j all --test-options=--hide-successes
git diff --check
