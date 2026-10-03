#!/usr/bin/env python3
import fcntl
import json
import random
import re
import sys
import time
import urllib.error
import urllib.request
import os
from datetime import datetime, timezone, timedelta
from pathlib import Path

ROOT = Path("/opt/fm-cosmobot")
CONFIG = ROOT / "runtime" / "config.toml"
LOCK = Path(os.environ.get("FM_AI_CONTEST_LOCK", "/tmp/ai_contest_daily.lock"))
DOMAIN = os.environ.get("FM_DOMAIN_URL", "http://fm-domain:8077")
CHINA = timezone(timedelta(hours=8))


def request_json(url, payload=None):
    data = None if payload is None else json.dumps(payload, ensure_ascii=False).encode()
    request = urllib.request.Request(url, data=data, headers={"Content-Type": "application/json"}, method="POST" if data else "GET")
    with urllib.request.urlopen(request, timeout=45) as response:
        return json.load(response)


def read_policy():
    policy = request_json(f"{DOMAIN}/ai-contest/policy")
    return policy if isinstance(policy, dict) else {}


def read_provider():
    text = CONFIG.read_text(encoding="utf-8")
    chat_name = None
    match_chat = re.search(r"(?m)^chat\s*=\s*\"([^\"]+)\"", text)
    if match_chat:
        chat_name = match_chat.group(1).strip()
    candidates = [name for name in [chat_name, "deepseek-flash", "deepseek-v4-flash"] if name]
    section = None
    chosen = None
    for name in candidates:
        match = re.search(rf"(?ms)^\[llm\.chat_provider\.{re.escape(name)}\]\s*(.*?)(?=^\[|\Z)", text)
        if match:
            section = match.group(1)
            chosen = name
            break
    if not section:
        raise ValueError("chat provider is missing")
    values = {}
    for key, value in re.findall(r"(?m)^([A-Za-z_]+)\s*=\s*\"([^\"]*)\"", section):
        values[key] = value
    if not values.get("base_url") or not values.get("api_key") or not values.get("model"):
        raise ValueError(f"{chosen} provider is incomplete")
    values["timeout"] = 120
    values["profile"] = chosen
    return values


def clean_json(text):
    text = re.sub(r"^```(?:json)?\s*|\s*```$", "", text.strip(), flags=re.I)
    match = re.search(r"\{.*\}", text, flags=re.S)
    if not match:
        raise ValueError("model did not return a JSON object")
    value = json.loads(match.group(0))
    if not isinstance(value, dict):
        raise ValueError("model result is not an object")
    return value


def pick_difficulty():
    # 所有者 2026-10-03 定的：**一律「虐」**。
    # 原来是 random.choices(["难", "虐"], weights=[7, 3]) —— 70% 概率出「难」，
    # 实测太简单、没有挑战性。注意这里是**日常预生成**的难度来源
    # （宿主机 crontab 每 6 小时跑本脚本、预填 7 天），跟 agent 那条按需生成路径是两套，
    # 两边都要改，只改一边等于没改。
    return "虐"


def difficulty_instructions(difficulty):
    if difficulty == "虐":
        return (
            "Difficulty: 虐. Write dense, demanding Chinese suitable for advanced typists. "
            "Use long nested sentences, uncommon literary words, and some rarer characters, "
            "but keep the prose grammatical and typeable."
        )
    return (
        "Difficulty: 难. Write clearly harder-than-normal Chinese: compact written style, "
        "longer clauses, less common vocabulary, and slightly complex syntax. Avoid kids-easy wording."
    )


def existing_text(date):
    return request_json(f"{DOMAIN}/ai-contest/text?date={date}")


def generate(policy, date, difficulty, previous_title="unknown"):
    provider = read_provider()
    prompt = {
        "title": "Generate one daily Chinese typing-practice contest text.",
        "instructions": [
            f"China date: {date}",
            f"Body length must be {policy.get('min_chars', 200)} to {policy.get('max_chars', 300)} Chinese characters.",
            difficulty_instructions(difficulty),
            "Style and topic are unrestricted, but choose a genuinely new topic and title.",
            "Return JSON only with exactly two fields: title and body.",
            "Do not include Markdown, explanations, word-count labels, dates, or metadata in body.",
            str(policy.get("style") or ""),
            f"Do not imitate previous titles such as: {previous_title}.",
        ],
    }
    body = json.dumps({
        "model": provider["model"],
        "messages": [
            {"role": "system", "content": "You write clean Chinese typing-practice prose."},
            {"role": "user", "content": json.dumps(prompt, ensure_ascii=False)},
        ],
        "temperature": 1,
        "response_format": {"type": "json_object"},
    }, ensure_ascii=False).encode()
    request = urllib.request.Request(
        provider["base_url"].rstrip("/") + "/chat/completions",
        data=body,
        headers={"Authorization": "Bearer " + provider["api_key"], "Content-Type": "application/json"},
        method="POST",
    )
    with urllib.request.urlopen(request, timeout=int(provider.get("timeout", 120))) as response:
        result = json.load(response)
    content = result["choices"][0]["message"]["content"]
    return clean_json(content)


def publish_one(policy, date, replace=False, previous_title="unknown"):
    difficulty = pick_difficulty()
    generated = generate(policy, date, difficulty, previous_title=previous_title)
    title = str(generated.get("title") or "").strip()
    body = str(generated.get("body") or "").strip()
    if not title or not body:
        raise ValueError("model returned empty title or body")
    saved = request_json(f"{DOMAIN}/ai-contest/text", {
        "date": date,
        "title": title,
        "body": body,
        "difficulty": difficulty,
        "provider": "deepseek-flash-daily",
        "replace": bool(replace),
    })
    print(json.dumps({
        "date": date,
        "status": saved.get("status"),
        "difficulty": difficulty,
        "title": (saved.get("text") or {}).get("title") or title,
    }, ensure_ascii=False), flush=True)
    return title


def main():
    LOCK.parent.mkdir(parents=True, exist_ok=True)
    with LOCK.open("w") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            return
        policy = read_policy()
        if policy.get("daily_refresh", True) is False:
            return
        replace_today = os.environ.get("FM_AI_CONTEST_REPLACE", "").strip().lower() in {"1", "true", "yes"}
        horizon = int(os.environ.get("FM_AI_CONTEST_PREFILL_DAYS", "7"))
        horizon = max(1, min(horizon, 14))
        today = datetime.now(CHINA).date()
        previous_title = "unknown"
        errors = []
        for offset in range(horizon):
            day = (today + timedelta(days=offset)).isoformat()
            existing = existing_text(day)
            if existing and isinstance(existing, dict) and existing.get("title"):
                previous_title = str(existing.get("title") or previous_title)
                if not (replace_today and offset == 0):
                    print(json.dumps({"date": day, "status": "existing", "difficulty": existing.get("difficulty"), "title": existing.get("title")}, ensure_ascii=False), flush=True)
                    continue
            try:
                previous_title = publish_one(policy, day, replace=(replace_today and offset == 0), previous_title=previous_title)
            except Exception as exc:
                errors.append(f"{day}: {exc}")
                print(f"daily AI contest generation failed: {day}: {exc}", file=sys.stderr, flush=True)
        if errors:
            raise RuntimeError("; ".join(errors))


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        print(f"daily AI contest generation failed: {exc}", file=sys.stderr)
        sys.exit(1)
