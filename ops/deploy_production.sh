#!/usr/bin/env bash
set -Eeuo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
DEPLOY_ROOT=${FM_DEPLOY_ROOT:-/opt/fm-cosmobot}
BUILD_IMAGE=${FM_BUILD_IMAGE:-fm-cosmobot:build-4c782b1}
BUILD_CACHE=${FM_BUILD_CACHE:-$DEPLOY_ROOT/build-cache}
BASE_DIST_NEWSTYLE=${FM_BASE_DIST_NEWSTYLE:-$DEPLOY_ROOT/build/source/dist-newstyle}
CABAL_HOME=${FM_CABAL_HOME:-$DEPLOY_ROOT/build/cabal-home}
BUILD_NETWORK=${FM_BUILD_NETWORK:-host}
BUILD_HTTP_PROXY=${FM_BUILD_HTTP_PROXY:-http://127.0.0.1:7890}
TOOL_OUTPUT=${FM_TOOL_OUTPUT:-$DEPLOY_ROOT/tool-output}
BOT_COMPOSE=${FM_BOT_COMPOSE:-$DEPLOY_ROOT/compose.yaml}
DOMAIN_COMPOSE=${FM_DOMAIN_COMPOSE:-/opt/fm-domain/compose.yaml}
DOMAIN_ASSETS_DIR=${FM_DOMAIN_ASSETS_DIR:-/opt/fm-domain/assets}
STABLE_BOT_IMAGE=${FM_STABLE_BOT_IMAGE:-fm-cosmobot:runtime-fm-tools}
STABLE_DOMAIN_IMAGE=${FM_STABLE_DOMAIN_IMAGE:-fm-domain:local}
BOT_SERVICE=${FM_BOT_SERVICE:-fm-cosmobot}
DOMAIN_SERVICE=${FM_DOMAIN_SERVICE:-fm-domain}

cd "$ROOT"
python3 ops/check_secrets.py

if [[ ${FM_ALLOW_DIRTY_SOURCE:-0} != 1 ]]; then
  git diff --quiet && git diff --cached --quiet || {
    echo "Refusing to deploy a dirty working tree. Commit the release first." >&2
    exit 1
  }
  [[ -z $(git ls-files --others --exclude-standard) ]] || {
    echo "Refusing to deploy with untracked files." >&2
    exit 1
  }
fi

revision=$(git rev-parse HEAD 2>/dev/null || sha256sum cosmobot/cosmobot.cabal | cut -d' ' -f1)
release=${revision:0:12}
candidate_compiled="fm-cosmobot:compiled-$release"
candidate_bot="fm-cosmobot:runtime-$release"
candidate_domain="fm-domain:$release"
test_build_image="fm-cosmobot:build-test-$release"
candidate_container="fm-cosmobot-compiled-$release"
previous_bot=$(docker image inspect "$STABLE_BOT_IMAGE" --format '{{.Id}}' 2>/dev/null || true)
previous_domain=$(docker image inspect "$STABLE_DOMAIN_IMAGE" --format '{{.Id}}' 2>/dev/null || true)
switched=0

rollback() {
  status=$?
  trap - ERR INT TERM
  docker rm -f "$candidate_container" >/dev/null 2>&1 || true
  if (( switched == 1 )); then
    echo "Deployment failed; restoring previous images." >&2
    [[ -z "$previous_bot" ]] || docker tag "$previous_bot" "$STABLE_BOT_IMAGE"
    [[ -z "$previous_domain" ]] || docker tag "$previous_domain" "$STABLE_DOMAIN_IMAGE"
    docker compose -f "$DOMAIN_COMPOSE" up -d --force-recreate "$DOMAIN_SERVICE" || true
    docker compose -f "$BOT_COMPOSE" up -d --force-recreate "$BOT_SERVICE" || true
  fi
  exit "$status"
}
trap rollback ERR INT TERM

revision_build_cache="$BUILD_CACHE/$release"
dist_cache="$revision_build_cache"
if [[ -d "$BASE_DIST_NEWSTYLE" ]]; then
  dist_cache="$BASE_DIST_NEWSTYLE"
fi
mkdir -p "$revision_build_cache" "$TOOL_OUTPUT" "$CABAL_HOME" \
  "$CABAL_HOME/config" "$CABAL_HOME/data" "$CABAL_HOME/packages"

if [[ ! -f "$CABAL_HOME/config/config" ]]; then
  cat > "$CABAL_HOME/config/config" <<'EOF'
repository hackage.haskell.org
  url: https://hackage.haskell.org/
  secure: True
EOF
fi

build_mounts=()
if [[ -f "$DEPLOY_ROOT/cabal.project.local" ]]; then
  build_mounts+=(-v "$DEPLOY_ROOT/cabal.project.local:/build/cabal.project.local:ro")
fi
if [[ -d "$DEPLOY_ROOT/build/source/vendor" ]]; then
  build_mounts+=(-v "$DEPLOY_ROOT/build/source/vendor:/build/vendor:ro")
fi
build_mounts+=(
  -v "$CABAL_HOME/packages:/root/.cabal/packages"
  -v "$CABAL_HOME/config:/root/.cabal"
  -v "$CABAL_HOME/data:/root/.local/share/cabal"
)

docker build -t "$candidate_domain" fm-domain
docker run --rm --entrypoint python3 -v "$ROOT/fm-domain:/src:ro" -v "$DOMAIN_ASSETS_DIR:/assets:ro" -v "$DOMAIN_ASSETS_DIR/msyh.ttc:/msyh.ttc:ro" -w /src \
  -e FM_REPORT_FONT=/assets/msyh.ttc \
  "$candidate_domain" -m unittest -v test_app.py

docker build \
  --build-arg "FM_BUILD_IMAGE=$BUILD_IMAGE" \
  -f deploy/Dockerfile.build-test \
  -t "$test_build_image" .

docker run --rm \
  -v "$ROOT:/source-current:ro" \
  -v "$dist_cache:/build/dist-newstyle" \
  -v "$TOOL_OUTPUT:/out" \
  "${build_mounts[@]}" \
  --network "$BUILD_NETWORK" \
  -e HTTP_PROXY="$BUILD_HTTP_PROXY" \
  -e HTTPS_PROXY="$BUILD_HTTP_PROXY" \
  -e ALL_PROXY="$BUILD_HTTP_PROXY" \
  -e NO_PROXY=127.0.0.1,localhost \
  -e LANG=C.UTF-8 \
  -e LC_ALL=C.UTF-8 \
  -e PATH=/opt/ghc/9.10.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
  --entrypoint bash "$test_build_image" -c \
  'rm -rf /build/cosmobot /build/cosmocode /build/cosmobox && tar --exclude=./cabal.project.local -cf - -C /source-current . | tar -xf - -C /build && find /build/cosmobot /build/cosmocode /build/cosmobox -type f -exec touch -c {} + 2>/dev/null || true; awk '\''{line=$0; sub(/\r$/, "", line)} line == "test-suite resource-spec" {print; print "    buildable: False"; next} {print}'\'' /build/cosmobot/cosmobot.cabal > /build/cosmobot/cosmobot.cabal.tmp && mv /build/cosmobot/cosmobot.cabal.tmp /build/cosmobot/cosmobot.cabal; cd /build && if ! find /root/.cabal/packages /root/.local/share/cabal/packages -type f -name "01-index.*" -print -quit 2>/dev/null | grep -q .; then cabal update; fi && project_args=() && if [[ -f cabal.project.local ]]; then project_args+=(--project-file=cabal.project.production); fi && cabal "${project_args[@]}" build cosmobot cosmocode -j all && cabal "${project_args[@]}" test cosmobot -j all --test-options=--hide-successes && BIN=$(cabal "${project_args[@]}" list-bin exe:cosmobot) && install -m 0755 "$BIN" /out/cosmobot-candidate'

docker create --name "$candidate_container" fm-cosmobot:compiled-current >/dev/null
docker cp "$TOOL_OUTPUT/cosmobot-candidate" "$candidate_container:/opt/cosmobot/cosmobot"
docker commit "$candidate_container" "$candidate_compiled" >/dev/null
docker rm "$candidate_container" >/dev/null

docker build \
  --build-arg "COSMOBOT_COMPILED_IMAGE=$candidate_compiled" \
  -f deploy/Dockerfile.runtime \
  -t "$candidate_bot" .
docker run --rm --entrypoint /opt/cosmobot/cosmobot "$candidate_bot" --help >/dev/null

docker tag "$candidate_domain" "$STABLE_DOMAIN_IMAGE"
docker tag "$candidate_bot" "$STABLE_BOT_IMAGE"
switched=1

docker compose -f "$DOMAIN_COMPOSE" up -d --force-recreate "$DOMAIN_SERVICE"
docker compose -f "$BOT_COMPOSE" up -d --force-recreate "$BOT_SERVICE"

FM_STABLE_BOT_IMAGE="$STABLE_BOT_IMAGE" \
FM_STABLE_DOMAIN_IMAGE="$STABLE_DOMAIN_IMAGE" \
FM_REQUIRE_STARTUP_MARKER=1 \
  bash ops/verify_production.sh

mkdir -p "$DEPLOY_ROOT/releases"
printf 'revision=%s\nbot_image=%s\ndomain_image=%s\ndeployed_at=%s\n' \
  "$revision" \
  "$(docker image inspect "$STABLE_BOT_IMAGE" --format '{{.Id}}')" \
  "$(docker image inspect "$STABLE_DOMAIN_IMAGE" --format '{{.Id}}')" \
  "$(date -Iseconds)" > "$DEPLOY_ROOT/releases/$release.manifest"

switched=0
trap - ERR INT TERM
echo "Deployment completed: $revision"
