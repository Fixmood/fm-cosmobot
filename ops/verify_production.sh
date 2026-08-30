#!/usr/bin/env bash
set -euo pipefail

BOT_CONTAINER=${FM_BOT_CONTAINER:-fm-cosmobot}
DOMAIN_CONTAINER=${FM_DOMAIN_CONTAINER:-fm-domain}
BOT_IMAGE=${FM_STABLE_BOT_IMAGE:-fm-cosmobot:runtime-fm-tools}
DOMAIN_IMAGE=${FM_STABLE_DOMAIN_IMAGE:-fm-domain:local}
TIMEOUT_SECONDS=${FM_VERIFY_TIMEOUT_SECONDS:-120}
REQUIRE_STARTUP_MARKER=${FM_REQUIRE_STARTUP_MARKER:-0}

deadline=$((SECONDS + TIMEOUT_SECONDS))
while (( SECONDS < deadline )); do
  bot_health=$(docker inspect "$BOT_CONTAINER" --format '{{if .State.Health}}{{.State.Health.Status}}{{else}}{{.State.Status}}{{end}}' 2>/dev/null || true)
  domain_health=$(docker inspect "$DOMAIN_CONTAINER" --format '{{if .State.Health}}{{.State.Health.Status}}{{else}}{{.State.Status}}{{end}}' 2>/dev/null || true)
  if [[ "$bot_health" == "healthy" && "$domain_health" =~ ^(healthy|running)$ ]]; then
    break
  fi
  sleep 5
done

[[ "${bot_health:-}" == "healthy" ]] || { echo "FM bot is not healthy: ${bot_health:-missing}" >&2; exit 1; }
[[ "${domain_health:-}" =~ ^(healthy|running)$ ]] || { echo "FM domain is not healthy: ${domain_health:-missing}" >&2; exit 1; }

bot_container_image=$(docker inspect "$BOT_CONTAINER" --format '{{.Image}}')
bot_stable_image=$(docker image inspect "$BOT_IMAGE" --format '{{.Id}}')
domain_container_image=$(docker inspect "$DOMAIN_CONTAINER" --format '{{.Image}}')
domain_stable_image=$(docker image inspect "$DOMAIN_IMAGE" --format '{{.Id}}')

[[ "$bot_container_image" == "$bot_stable_image" ]] || { echo "Bot container image does not match stable tag." >&2; exit 1; }
[[ "$domain_container_image" == "$domain_stable_image" ]] || { echo "Domain container image does not match stable tag." >&2; exit 1; }

docker exec "$DOMAIN_CONTAINER" python3 -c \
  "import json,urllib.request; value=json.load(urllib.request.urlopen('http://127.0.0.1:8077/health', timeout=3)); assert value.get('ok') is True"

recent_logs=$(docker logs "$BOT_CONTAINER" --since 5m 2>&1 || true)
if [[ "$REQUIRE_STARTUP_MARKER" == 1 ]]; then
  grep -q "Cosmobot stand by" <<<"$recent_logs" || { echo "Bot startup marker is missing." >&2; exit 1; }
fi
if grep -Eqi 'uncaught exception|panic|segmentation fault' <<<"$recent_logs"; then
  echo "Fatal error found in recent bot logs." >&2
  exit 1
fi

echo "Production verification passed."
echo "bot_image=$bot_container_image"
echo "domain_image=$domain_container_image"
