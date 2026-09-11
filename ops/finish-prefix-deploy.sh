#!/usr/bin/env bash
set -Eeuo pipefail
old=$(docker image inspect fm-cosmobot:runtime-fm-tools --format '{{.Id}}')
docker tag "$old" fm-cosmobot:rollback-before-prefix-final-20260905
rollback() {
  trap - ERR
  docker tag "$old" fm-cosmobot:runtime-fm-tools
  docker rm -f fm-cosmobot >/dev/null 2>&1 || true
  docker compose -p fm-cosmobot -f /opt/fm-cosmobot/compose.yaml up -d --no-deps fm-cosmobot
  exit 1
}
trap rollback ERR
docker tag fm-cosmobot:prefix-final-20260905 fm-cosmobot:runtime-fm-tools
docker stop fm-cosmobot
docker rm fm-cosmobot
docker compose -p fm-cosmobot -f /opt/fm-cosmobot/compose.yaml up -d --no-deps fm-cosmobot
for attempt in $(seq 1 18); do
  status=$(docker inspect fm-cosmobot --format '{{.State.Health.Status}}')
  if [[ "$status" == healthy ]]; then
    docker inspect fm-cosmobot --format '{{.Image}} {{.State.Health.Status}}'
    trap - ERR
    exit 0
  fi
  sleep 5
done
false
