#!/usr/bin/env bash
set -Eeuo pipefail
ROOT=/opt/fm-cosmobot/source
docker run --rm --network host \
  -v "$ROOT:/source-current:ro" \
  -v /opt/fm-cosmobot/build/source/dist-newstyle:/build/dist-newstyle \
  -v /opt/fm-cosmobot/tool-output:/out \
  -v /opt/fm-cosmobot/cabal.project.local:/build/cabal.project.local:ro \
  -v /opt/fm-cosmobot/build/source/vendor:/build/vendor:ro \
  -v /opt/fm-cosmobot/build/cabal-home/config:/root/.cabal \
  -v /opt/fm-cosmobot/build/cabal-home/packages:/root/.cabal/packages \
  -v /opt/fm-cosmobot/build/cabal-home/data:/root/.local/share/cabal \
  -e LANG=C.UTF-8 -e LC_ALL=C.UTF-8 \
  -e PATH=/opt/ghc/9.10.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
  --entrypoint bash fm-cosmobot:build-test-471e7690b694 -c '
set -e
tar --exclude=./cabal.project.local -cf - -C /source-current . | tar -xf - -C /build
cd /build
cabal --project-file=cabal.project.production build exe:cosmobot test:chat-platform-spec -j4
cabal --project-file=cabal.project.production test chat-platform-spec --test-options=--hide-successes
install -m 0755 "$(cabal --project-file=cabal.project.production list-bin exe:cosmobot)" /out/cosmobot-candidate
'
docker build -f "$ROOT/ops/Dockerfile.prefix-final" -t fm-cosmobot:prefix-final-20260905 /opt/fm-cosmobot/tool-output
docker run --rm --entrypoint /opt/cosmobot/cosmobot fm-cosmobot:prefix-final-20260905 --help >/dev/null
