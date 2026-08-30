# FM Domain

FM Domain is the stateful service behind FM's typing library, score archive,
contest archive, leaderboards, reports, and group capability controls.

Production data is mounted at `/data` and is never stored in Git. Report fonts
and ranker assets are mounted at `/assets`; provide `FM_REPORT_FONT` when the
default `/assets/msyh.ttc` is unavailable.

Run the test suite from the repository root:

```bash
python3 -m unittest discover -s fm-domain -p 'test_*.py' -v
```

Start a local service with an empty database:

```bash
python3 fm-domain/app.py serve --db /tmp/fm-domain.sqlite3 --host 127.0.0.1 --port 8077
```
