# Production Deployment

Production releases are built from a clean Git commit. Runtime configuration,
credentials, databases, user data, report assets, and media caches remain on
the server and are mounted into the containers.

## Required server state

- Docker with Compose v2
- External Docker network `fm-runtime`
- Persistent directories `/opt/fm-cosmobot/runtime`, `/opt/fm-cosmobot/work`,
  `/opt/fm-domain/data`, and `/opt/fm-domain/assets`
- A production `config.toml` under `/opt/fm-cosmobot/runtime`
- Build image `fm-cosmobot:build-4c782b1` and CosmoBox image
  `fm-cosmobox:latest`
- The production build image contains pinned Haskell dependencies under
  `/build/vendor`. `/opt/fm-cosmobot/cabal.project.local` selects those
  dependencies and is mounted automatically when present. The checked-in
  `cabal.project.production` overlay limits regression tests to FM packages.

The checked-in Compose files are credential-free templates. Existing
production Compose paths can be selected with `FM_BOT_COMPOSE` and
`FM_DOMAIN_COMPOSE`.

## Release procedure

```bash
git fetch origin fm/main
git switch --detach origin/fm/main
bash ops/fm_regression.sh
bash ops/deploy_production.sh
```

The deployment script:

1. rejects dirty source and credential-like tracked values;
2. tests the FM Domain candidate image;
3. builds and tests the complete Haskell project;
4. creates immutable candidate images tagged with the Git revision;
5. switches the stable image tags only after candidate validation;
6. recreates FM Domain and Cosmobot;
7. verifies health and exact container image IDs;
8. restores the previous images if verification fails.

Successful releases write a manifest under
`/opt/fm-cosmobot/releases/<revision>.manifest`.

## Manual verification

```bash
bash ops/verify_production.sh
```

After automated verification, exercise the affected user workflows in a test
QQ group and Matrix room. Container health alone does not prove message
delivery.
