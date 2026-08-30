#!/usr/bin/env python3
"""Reject common credentials accidentally added to tracked source files."""

from __future__ import annotations

import re
import subprocess
from pathlib import Path


PRIVATE_KEY = re.compile(r"-----BEGIN (?:RSA |OPENSSH |EC )?PRIVATE KEY-----", re.IGNORECASE)
OPENAI_KEY = re.compile(r"\bsk-[A-Za-z0-9_-]{16,}\b")
ASSIGNMENT = re.compile(
    r"(?i)^\s*(?:[\"']?)(api[_-]?key|access[_-]?token|refresh[_-]?token|password|secret)"
    r"(?:[\"']?)\s*[:=]\s*[\"']([^\"']*)[\"']\s*(?:$|[,#])"
)
PLACEHOLDERS = ("replace", "example", "something", "changeme", "<", "${")


def tracked_files() -> list[Path]:
    try:
        output = subprocess.check_output(["git", "ls-files", "-z"])
    except (OSError, subprocess.CalledProcessError):
        # Source archives used by release smoke tests do not contain .git.
        return [
            path
            for path in Path(".").rglob("*")
            if path.is_file()
            and ".git" not in path.parts
            and path.suffix.lower() in {".cabal", ".conf", ".hs", ".py", ".sh", ".toml", ".yaml", ".yml"}
        ]
    return [Path(value.decode()) for value in output.split(b"\0") if value]


def is_placeholder(value: str) -> bool:
    normalized = value.strip().lower()
    return not normalized or any(marker in normalized for marker in PLACEHOLDERS)


def main() -> int:
    findings: list[str] = []
    for path in tracked_files():
        try:
            text = path.read_text(encoding="utf-8")
        except (UnicodeDecodeError, OSError):
            continue
        for number, line in enumerate(text.splitlines(), 1):
            if PRIVATE_KEY.search(line) or OPENAI_KEY.search(line):
                findings.append(f"{path}:{number}: credential-like value")
                continue
            match = ASSIGNMENT.search(line)
            if match and not is_placeholder(match.group(2)):
                findings.append(f"{path}:{number}: non-placeholder {match.group(1)}")
    if findings:
        print("Potential credentials found in tracked files:")
        print("\n".join(findings))
        return 1
    print("Tracked-source credential scan passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
