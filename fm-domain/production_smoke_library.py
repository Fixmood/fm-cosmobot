import json
import sqlite3
import sys
import time
import urllib.request


BASE_URL = "http://127.0.0.1:8077"
CHAT_ID = "codex-library-mode-smoke"
DIFFICULTIES = ("淼", "水", "易", "普", "难", "虐")


def post(path, payload):
    request = urllib.request.Request(
        BASE_URL + path,
        data=json.dumps(payload, ensure_ascii=False).encode("utf-8"),
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    started = time.monotonic()
    with urllib.request.urlopen(request, timeout=180) as response:
        result = json.loads(response.read().decode("utf-8"))
    return result, round(time.monotonic() - started, 3)


def identity(name):
    return {
        "platform": "qq",
        "chat_id": CHAT_ID,
        "requester_id": f"codex-smoke-{name}",
        "requester_name": "Codex smoke test",
    }


def require(condition, message):
    if not condition:
        raise AssertionError(message)


def cleanup():
    db = sqlite3.connect("/data/fm-domain.sqlite3")
    session_ids = [
        row[0]
        for row in db.execute(
            "SELECT session_id FROM library_sessions WHERE chat_id=?", (CHAT_ID,)
        )
    ]
    if session_ids:
        placeholders = ",".join("?" for _ in session_ids)
        segment_ids = [
            row[0]
            for row in db.execute(
                f"SELECT segment_id FROM library_segment_sessions "
                f"WHERE session_id IN ({placeholders})",
                session_ids,
            )
        ]
        db.execute(
            f"DELETE FROM library_segment_sessions WHERE session_id IN ({placeholders})",
            session_ids,
        )
        db.execute(
            f"DELETE FROM library_session_modes WHERE session_id IN ({placeholders})",
            session_ids,
        )
        db.execute(
            f"DELETE FROM library_sessions WHERE session_id IN ({placeholders})",
            session_ids,
        )
        if segment_ids:
            segment_placeholders = ",".join("?" for _ in segment_ids)
            db.execute(
                f"DELETE FROM library_segment_ids WHERE segment_id IN ({segment_placeholders})",
                segment_ids,
            )
    db.commit()
    db.close()


def main():
    rows = []
    cleanup()
    try:
        for difficulty in DIFFICULTIES:
            payload = {**identity(difficulty), "difficulty": difficulty, "length": 240}
            started, start_seconds = post("/library/session/start", payload)
            require(started.get("status") == "segment", f"{difficulty} start: {started}")
            require(started.get("difficulty") == difficulty, f"{difficulty} mismatch: {started}")

            continued, continue_seconds = post(
                "/library/session/continue", identity(difficulty)
            )
            require(
                continued.get("status") == "segment",
                f"{difficulty} continue: {continued}",
            )
            require(
                continued.get("difficulty") == difficulty,
                f"{difficulty} continuation mismatch: {continued}",
            )
            require(
                continued.get("title") != started.get("title"),
                f"{difficulty} continuation repeated the same article",
            )
            sent_message_id = f"smoke-current-{difficulty}"
            acknowledged, _ = post(
                "/library/session/sent",
                {
                    "session_id": continued["session_id"],
                    "message_id": sent_message_id,
                },
            )
            require(acknowledged.get("stored"), f"{difficulty} acknowledge failed")
            same, same_seconds = post(
                "/library/session/continue-previous", identity(difficulty)
            )
            require(
                same.get("status") in {"segment", "article_completed"},
                f"{difficulty} previous: {same}",
            )
            if same["status"] == "segment":
                require(
                    same.get("title") == started.get("title"),
                    f"{difficulty} previous article was not restored",
                )
                require(
                    same.get("recall_message_id") == sent_message_id,
                    f"{difficulty} current article recall ID was lost",
                )
            else:
                require(
                    same.get("requested_difficulty") == difficulty,
                    f"{difficulty} completion lost requested mode: {same}",
                )
            rows.append(
                {
                    "mode": difficulty,
                    "start_difficulty": started["difficulty"],
                    "continued_difficulty": continued["difficulty"],
                    "start_seconds": start_seconds,
                    "continue_seconds": continue_seconds,
                    "same_seconds": same_seconds,
                }
            )

        random_start, random_start_seconds = post(
            "/library/session/start", {**identity("random"), "length": 240}
        )
        random_next, random_continue_seconds = post(
            "/library/session/continue", identity("random")
        )
        require(random_start.get("status") == "segment", f"random start: {random_start}")
        require(random_next.get("status") == "segment", f"random continue: {random_next}")
        require(
            random_next.get("title") != random_start.get("title"),
            "random continuation repeated the same article",
        )
        rows.append(
            {
                "mode": "随机",
                "start_difficulty": random_start["difficulty"],
                "continued_difficulty": random_next["difficulty"],
                "start_seconds": random_start_seconds,
                "continue_seconds": random_continue_seconds,
            }
        )
        print(json.dumps(rows, ensure_ascii=False, indent=2))
    finally:
        cleanup()


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(f"SMOKE FAILED: {error}", file=sys.stderr)
        raise
