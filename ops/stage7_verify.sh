#!/usr/bin/env bash
# shellcheck disable=SC2034,SC2148,SC1091
set -Eeuo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT"

echo "== FM stage 7 verification =="
python3 ops/check_secrets.py
python3 -m py_compile admin/app.py admin/test_app.py
python3 admin/test_app.py -v
python3 -m unittest discover -s fm-domain -p 'test_*.py' -v

if command -v docker >/dev/null 2>&1; then
  echo "== Docker Compose syntax =="
  FM_ADMIN_TOKEN=stage7-placeholder docker compose -f deploy/fm-admin.compose.yaml config --quiet
  docker compose -f deploy/fm-domain.compose.yaml config --quiet
  docker compose -f deploy/cosmobot.compose.yaml config --quiet
  if [[ "${FM_BUILD_DOCKER_IMAGES:-1}" == 1 ]]; then
    echo "== Admin image build =="
    docker build --tag "${FM_ADMIN_TEST_IMAGE:-fm-admin:stage7-test}" -f admin/Dockerfile admin
  else
    echo "SKIP: FM_BUILD_DOCKER_IMAGES=0"
  fi
else
  echo "SKIP: Docker unavailable; compose and image verification must run on a Docker host."
fi

if command -v cabal >/dev/null 2>&1; then
  echo "== Haskell build and tests =="
  project_args=()
  [[ -f cabal.project.local ]] && project_args+=(--project-file=cabal.project.production)
  cabal "${project_args[@]}" build cosmobot cosmocode -j all
  cabal "${project_args[@]}" test cosmobot -j all --test-options=--hide-successes
else
  echo "SKIP: Cabal/GHC unavailable; Haskell verification is not a stage 7 local blocker." >&2
fi

git diff --check
echo "Stage 7 verification completed."
