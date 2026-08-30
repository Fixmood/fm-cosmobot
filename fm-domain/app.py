#!/usr/bin/env python3
"""FM's retained domain data importer and read-only query service."""

import argparse
import html
import hashlib
import io
import json
import os
import random
import re
import sqlite3
import string
import threading
import time
import unicodedata
from datetime import datetime, timedelta, timezone
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import parse_qs, urlencode, urlparse
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen


SCHEMA = """
PRAGMA journal_mode=WAL;
CREATE TABLE IF NOT EXISTS groups (
  group_id TEXT PRIMARY KEY, display_name TEXT NOT NULL, status TEXT NOT NULL,
  features_json TEXT NOT NULL, source_json TEXT NOT NULL, updated_at TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS settings (key TEXT PRIMARY KEY, value_json TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS library_texts (
  text_id TEXT PRIMARY KEY, category TEXT NOT NULL, relative_path TEXT NOT NULL UNIQUE,
  title TEXT NOT NULL, content TEXT NOT NULL, char_count INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS library_title_idx ON library_texts(title);
CREATE TABLE IF NOT EXISTS recall_records (
  message_id TEXT PRIMARY KEY, group_id TEXT, group_name TEXT, sender_id TEXT,
  sender_name TEXT, text TEXT NOT NULL, recalled_ts REAL,
  image_urls_json TEXT NOT NULL DEFAULT '[]', source_json TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS recall_group_idx ON recall_records(group_id, recalled_ts DESC);
CREATE TABLE IF NOT EXISTS score_records (
  record_id TEXT PRIMARY KEY, group_id TEXT, sender_id TEXT, occurred_at REAL,
  source_json TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS score_group_idx ON score_records(group_id, occurred_at DESC);
CREATE TABLE IF NOT EXISTS contest_texts (
  text_id TEXT PRIMARY KEY, title TEXT NOT NULL, source_group TEXT NOT NULL,
  competition_date TEXT NOT NULL, relative_path TEXT NOT NULL UNIQUE,
  content TEXT NOT NULL, char_count INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS contest_text_source_idx ON contest_texts(source_group, competition_date DESC);
CREATE TABLE IF NOT EXISTS contest_sessions (
  session_id TEXT PRIMARY KEY, platform TEXT NOT NULL, chat_id TEXT NOT NULL,
  requester_id TEXT NOT NULL, requester_name TEXT NOT NULL,
  query_text TEXT NOT NULL, source_group TEXT NOT NULL,
  initial_date TEXT NOT NULL, last_text_id TEXT NOT NULL,
  title TEXT NOT NULL, status TEXT NOT NULL, last_message_id TEXT,
  created_at REAL NOT NULL, updated_at REAL NOT NULL,
  UNIQUE(platform, chat_id, requester_id)
);
CREATE INDEX IF NOT EXISTS contest_session_scope_idx
  ON contest_sessions(platform, chat_id, status, updated_at DESC);
CREATE TABLE IF NOT EXISTS ai_contest_texts (
  competition_date TEXT PRIMARY KEY, title TEXT NOT NULL, body TEXT NOT NULL,
  difficulty TEXT NOT NULL, provider TEXT NOT NULL, generated_at REAL NOT NULL,
  source_json TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS ai_contest_scores (
  record_id TEXT PRIMARY KEY, competition_date TEXT NOT NULL, user_id TEXT NOT NULL,
  user_name TEXT NOT NULL, group_id TEXT NOT NULL, speed REAL NOT NULL,
  key_rate REAL NOT NULL, accuracy REAL NOT NULL, occurred_at REAL NOT NULL,
  source_json TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS ai_contest_score_date_idx ON ai_contest_scores(competition_date, speed DESC);
CREATE TABLE IF NOT EXISTS competition_scores (
  record_id TEXT PRIMARY KEY, competition_date TEXT NOT NULL, user_id TEXT NOT NULL,
  user_name TEXT NOT NULL, group_id TEXT NOT NULL, source_group TEXT NOT NULL,
  speed REAL NOT NULL, occurred_at REAL NOT NULL, source_json TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS competition_score_user_idx ON competition_scores(user_id, occurred_at DESC);
CREATE TABLE IF NOT EXISTS group_runtime (
  group_id TEXT PRIMARY KEY, paused INTEGER NOT NULL DEFAULT 0, updated_at REAL NOT NULL
);
CREATE TABLE IF NOT EXISTS recent_messages (
  platform TEXT NOT NULL, message_id TEXT NOT NULL, group_id TEXT NOT NULL,
  group_name TEXT NOT NULL, sender_id TEXT NOT NULL, sender_name TEXT NOT NULL,
  text TEXT NOT NULL, occurred_at REAL NOT NULL, raw_json TEXT NOT NULL,
  image_urls_json TEXT NOT NULL DEFAULT '[]',
  PRIMARY KEY(platform, message_id)
);
CREATE INDEX IF NOT EXISTS recent_message_time_idx ON recent_messages(occurred_at DESC);
CREATE TABLE IF NOT EXISTS repeat_recent (
  id INTEGER PRIMARY KEY AUTOINCREMENT, group_id TEXT NOT NULL, sender_id TEXT NOT NULL,
  normalized TEXT NOT NULL, text TEXT NOT NULL, occurred_at REAL NOT NULL
);
CREATE INDEX IF NOT EXISTS repeat_recent_group_idx ON repeat_recent(group_id, occurred_at DESC);
CREATE TABLE IF NOT EXISTS repeat_cooldowns (
  group_id TEXT NOT NULL, normalized TEXT NOT NULL, repeated_at REAL NOT NULL,
  PRIMARY KEY(group_id, normalized)
);
CREATE TABLE IF NOT EXISTS bot_refusal_cooldowns (
  group_id TEXT NOT NULL, sender_id TEXT NOT NULL, replied_at REAL NOT NULL,
  PRIMARY KEY(group_id, sender_id)
);
CREATE TABLE IF NOT EXISTS library_rankings (
  text_id TEXT PRIMARY KEY, difficulty TEXT NOT NULL, score REAL NOT NULL,
  ranked_at REAL NOT NULL
);
CREATE INDEX IF NOT EXISTS library_ranking_difficulty_idx
  ON library_rankings(difficulty, ranked_at DESC);
CREATE TABLE IF NOT EXISTS library_classifications (
  text_id TEXT PRIMARY KEY, primary_genre TEXT NOT NULL, genres_json TEXT NOT NULL,
  form TEXT NOT NULL, difficulty TEXT NOT NULL, difficulty_score REAL NOT NULL,
  confidence REAL NOT NULL, evidence_json TEXT NOT NULL,
  classifier_version TEXT NOT NULL, classified_at REAL NOT NULL
);
CREATE INDEX IF NOT EXISTS library_classification_genre_idx
  ON library_classifications(primary_genre, classified_at DESC);
CREATE TABLE IF NOT EXISTS library_sessions (
  session_id TEXT PRIMARY KEY, platform TEXT NOT NULL, chat_id TEXT NOT NULL,
  requester_id TEXT NOT NULL, requester_name TEXT NOT NULL, text_id TEXT NOT NULL,
  title TEXT NOT NULL, difficulty TEXT NOT NULL, difficulty_score REAL NOT NULL,
  segment_length INTEGER NOT NULL, next_offset INTEGER NOT NULL,
  segment_no INTEGER NOT NULL, total_chars INTEGER NOT NULL,
  status TEXT NOT NULL, last_message_id TEXT, created_at REAL NOT NULL,
  updated_at REAL NOT NULL,
  UNIQUE(platform, chat_id, requester_id)
);
CREATE INDEX IF NOT EXISTS library_session_scope_idx
  ON library_sessions(platform, chat_id, status, updated_at DESC);
CREATE TABLE IF NOT EXISTS library_session_modes (
  session_id TEXT PRIMARY KEY, requested_difficulty TEXT NOT NULL,
  requested_length INTEGER NOT NULL DEFAULT 0,
  requested_genre TEXT NOT NULL DEFAULT ''
);
CREATE TABLE IF NOT EXISTS library_previous_sessions (
  platform TEXT NOT NULL, chat_id TEXT NOT NULL, requester_id TEXT NOT NULL,
  text_id TEXT NOT NULL, title TEXT NOT NULL, difficulty TEXT NOT NULL,
  difficulty_score REAL NOT NULL, segment_length INTEGER NOT NULL,
  next_offset INTEGER NOT NULL, segment_no INTEGER NOT NULL,
  total_chars INTEGER NOT NULL, requested_difficulty TEXT NOT NULL,
  requested_genre TEXT NOT NULL DEFAULT '',
  requested_length INTEGER NOT NULL DEFAULT 0,
  created_at REAL NOT NULL, saved_at REAL NOT NULL,
  PRIMARY KEY(platform, chat_id, requester_id)
);
CREATE TABLE IF NOT EXISTS library_segment_ids (
  segment_id INTEGER PRIMARY KEY, created_at REAL NOT NULL
);
CREATE TABLE IF NOT EXISTS library_segment_sessions (
  segment_id INTEGER PRIMARY KEY, session_id TEXT NOT NULL,
  platform TEXT NOT NULL, chat_id TEXT NOT NULL, requester_id TEXT NOT NULL,
  created_at REAL NOT NULL, consumed_at REAL
);
CREATE INDEX IF NOT EXISTS library_segment_session_idx
  ON library_segment_sessions(session_id, consumed_at);
CREATE TABLE IF NOT EXISTS library_sent_messages (
  id INTEGER PRIMARY KEY AUTOINCREMENT, session_id TEXT NOT NULL,
  platform TEXT NOT NULL, chat_id TEXT NOT NULL, requester_id TEXT NOT NULL,
  message_id TEXT NOT NULL UNIQUE, sent_at REAL NOT NULL, recalled_at REAL
);
CREATE INDEX IF NOT EXISTS library_sent_message_scope_idx
  ON library_sent_messages(platform, chat_id, requester_id, sent_at DESC);
CREATE TABLE IF NOT EXISTS single_sessions (
  session_id TEXT PRIMARY KEY, platform TEXT NOT NULL, chat_id TEXT NOT NULL,
  requester_id TEXT NOT NULL, requester_name TEXT NOT NULL, text_id TEXT NOT NULL,
  title TEXT NOT NULL, sequence TEXT NOT NULL, order_name TEXT NOT NULL,
  key_req REAL NOT NULL, acc_req REAL NOT NULL, segment_length INTEGER NOT NULL,
  next_offset INTEGER NOT NULL, segment_no INTEGER NOT NULL, total_chars INTEGER NOT NULL,
  status TEXT NOT NULL, last_message_id TEXT, created_at REAL NOT NULL,
  updated_at REAL NOT NULL,
  UNIQUE(platform, chat_id, requester_id)
);
CREATE INDEX IF NOT EXISTS single_session_scope_idx
  ON single_sessions(platform, chat_id, status, updated_at DESC);
"""

GROUP_CAPABILITIES = {
    "agent", "library", "contest", "ai_contest", "scores", "recall", "repeat", "poke",
    "score_archive",
}

AI_CONTEST_POLICY_DEFAULTS = {
    "min_chars": 200,
    "max_chars": 300,
    "daily_refresh": True,
    "unique_topic": True,
    "unique_body": True,
    "style": "",
}

LIBRARY_DIFFICULTIES = ["淼", "水", "易", "普", "难", "虐"]
LIBRARY_DIFFICULTY_ALIASES = {
    "miao": "淼", "淼级": "淼", "极易": "淼",
    "shui": "水", "水级": "水",
    "yi": "易", "易级": "易", "简单": "易",
    "pu": "普", "普通": "普", "普通难度": "普",
    "nan": "难", "难级": "难", "困难": "难",
    "nue": "虐", "虐级": "虐", "爆表": "虐", "爆虐": "虐",
}
LIBRARY_DEFAULT_MIN_LENGTH = 200
LIBRARY_DEFAULT_MAX_LENGTH = 400
LIBRARY_MAX_LENGTH = 1400
SINGLE_DEFAULT_LENGTH = 100
SINGLE_SET_NAMES = [
    "前500", "中500", "后500", "前1500", "黄500", "玄500",
    "地500", "天500", "王500", "皇500", "帝500",
]
RANK_TRIE_WORD_KEY = "\0word"
RANK_TRIE_PRE_KEY = "\0pre"
LETTER_DIGIT = string.ascii_letters + string.digits
_RANKER = None
LIBRARY_RANKING_VERSION = "full-text-v2"
LIBRARY_CLASSIFIER_VERSION = "genre-form-v2"

LIBRARY_GENRE_ALIASES = {
    "恐怖": "恐怖惊悚", "惊悚": "恐怖惊悚", "灵异": "恐怖惊悚",
    "悬疑": "悬疑推理", "推理": "悬疑推理", "刑侦": "悬疑推理",
    "修仙": "仙侠修真", "修真": "仙侠修真", "仙侠": "仙侠修真",
    "玄幻": "玄幻奇幻", "奇幻": "玄幻奇幻", "魔幻": "玄幻奇幻",
    "武侠": "武侠江湖", "江湖": "武侠江湖",
    "言情": "言情爱情", "爱情": "言情爱情", "恋爱": "言情爱情",
    "科幻": "科幻未来", "未来": "科幻未来", "星际": "科幻未来",
    "历史": "历史古风", "古风": "历史古风",
    "都市": "都市职场", "职场": "都市职场",
    "校园": "校园青春", "青春": "校园青春",
    "亲情": "亲情家庭", "家庭": "亲情家庭",
    "励志": "励志成长", "成长": "励志成长",
    "寓言": "寓言童话", "童话": "寓言童话",
    "诗词": "诗词古典", "古典": "诗词古典", "诗歌": "诗词古典",
    "散文": "散文随笔", "随笔": "散文随笔",
    "科普": "科普说明", "说明文": "科普说明",
}

# Title hits carry more weight than body hits. Evidence is stored with every
# result so a later classifier revision can explain and reproduce the label.
LIBRARY_GENRE_RULES = {
    "恐怖惊悚": ("恐怖", "惊悚", "灵异", "鬼魂", "怨灵", "僵尸", "幽灵", "诡异", "阴森", "尸体", "棺材", "亡魂", "血腥", "噩梦", "鬼屋", "闹鬼"),
    "悬疑推理": ("悬疑", "推理", "侦探", "案件", "凶案", "线索", "证据", "真相", "密室", "犯人", "刑侦", "谜案", "失踪", "谋杀", "尸检"),
    "仙侠修真": ("修仙", "修真", "仙侠", "灵气", "丹田", "元婴", "筑基", "金丹", "渡劫", "宗门", "法宝", "剑修", "飞升", "仙人", "灵根", "真元", "洞府"),
    "玄幻奇幻": ("玄幻", "奇幻", "魔法", "魔王", "精灵", "勇者", "异世界", "斗气", "巫师", "魔兽", "龙族", "召唤", "机甲", "末世", "末日"),
    "武侠江湖": ("武侠", "江湖", "武林", "少侠", "门派", "剑客", "侠客", "内力", "掌门", "镖局", "大侠", "刀客", "轻功", "秘籍", "帮派"),
    "言情爱情": ("言情", "爱情", "恋爱", "爱上", "心动", "初恋", "情人", "失恋", "深情", "相爱", "表白", "暗恋", "告白", "婚礼"),
    "科幻未来": ("科幻", "未来", "星际", "宇宙", "太空", "飞船", "机器人", "人工智能", "基因", "量子", "星球", "外星", "航天", "时空", "殖民"),
    "历史古风": ("历史", "古风", "皇帝", "皇宫", "朝廷", "王朝", "将军", "古代", "清朝", "明朝", "唐朝", "宋朝", "三国", "战国", "太子", "丞相", "后宫"),
    "都市职场": ("都市", "职场", "公司", "总裁", "经理", "上班", "老板", "同事", "创业", "职员", "会议", "项目", "办公室", "加班"),
    "校园青春": ("校园", "学校", "同学", "老师", "青春", "高考", "教室", "学生", "毕业", "校服", "宿舍", "社团", "课堂"),
    "亲情家庭": ("亲情", "家庭", "父亲", "母亲", "妈妈", "爸爸", "孩子", "姐姐", "兄弟", "家人", "夫妻", "爷爷", "奶奶", "女儿", "儿子"),
    "励志成长": ("励志", "成长", "梦想", "坚持", "成功", "奋斗", "勇气", "逆袭", "蜕变", "突破", "挑战", "自律"),
    "寓言童话": ("寓言", "童话", "故事会", "王子", "公主", "森林", "魔法世界", "从前有一天", "小动物", "巨人"),
    "诗词古典": ("诗经", "楚辞", "唐诗", "宋词", "元曲", "古诗", "诗词", "绝句", "律诗", "乐府", "文言", "古文"),
    "散文随笔": ("散文", "随笔", "杂记", "感悟", "抒情", "回忆", "心情", "絮语", "札记", "漫谈"),
    "科普说明": ("科普", "说明", "百科", "知识", "原理", "实验", "科学", "技术", "指南", "教程", "机制", "结构"),
}

LIBRARY_GENRE_STRONG_TERMS = {
    "恐怖惊悚": {"恐怖", "惊悚", "灵异", "鬼魂", "怨灵", "僵尸", "幽灵", "鬼屋", "闹鬼"},
    "悬疑推理": {"悬疑", "推理", "侦探", "凶案", "密室", "刑侦", "谜案", "谋杀", "尸检"},
    "仙侠修真": {"修仙", "修真", "仙侠", "丹田", "元婴", "筑基", "金丹", "渡劫", "宗门", "灵根", "真元"},
    "玄幻奇幻": {"玄幻", "奇幻", "魔法", "魔王", "精灵", "勇者", "异世界", "斗气", "巫师", "魔兽", "龙族"},
    "武侠江湖": {"武侠", "江湖", "武林", "少侠", "门派", "剑客", "侠客", "内力", "轻功", "秘籍"},
    "科幻未来": {"科幻", "星际", "宇宙", "太空", "飞船", "机器人", "人工智能", "量子", "外星", "时空"},
    "历史古风": {"皇帝", "皇宫", "朝廷", "王朝", "将军", "清朝", "明朝", "唐朝", "宋朝", "三国", "战国", "丞相", "后宫"},
    "寓言童话": {"寓言", "童话", "王子", "公主", "从前有一天", "小动物", "巨人"},
    "诗词古典": {"诗经", "楚辞", "唐诗", "宋词", "元曲", "古诗", "诗词", "绝句", "律诗", "乐府"},
    "言情爱情": {"言情", "爱情", "恋爱", "初恋", "暗恋", "表白", "告白", "婚礼"},
    "都市职场": {"都市", "职场", "总裁", "公司", "办公室", "会议", "项目", "加班"},
    "校园青春": {"校园", "学校", "同学", "老师", "高考", "教室", "毕业", "校服", "宿舍"},
    "亲情家庭": {"亲情", "家庭", "父亲", "母亲", "妈妈", "爸爸", "孩子", "家人", "夫妻"},
    "励志成长": {"励志", "成长", "梦想", "坚持", "成功", "奋斗", "勇气", "逆袭", "蜕变"},
    "散文随笔": {"散文", "随笔", "杂记", "感悟", "抒情", "回忆", "札记", "漫谈"},
    "科普说明": {"科普", "百科", "原理", "实验", "科学", "技术", "教程", "机制", "结构"},
}

LIBRARY_FORM_RULES = {
    "诗歌": ("诗", "词", "曲", "歌", "绝句", "律诗", "诗经", "楚辞"),
    "书信": ("书信", "来信", "致信", "信件", "敬爱的", "此致"),
    "寓言童话": ("寓言", "童话", "睡前故事", "民间故事"),
    "科普说明": ("科普", "百科", "说明", "指南", "教程", "知识"),
    "议论随笔": ("随笔", "杂文", "评论", "感悟", "思考", "议论"),
    "对话剧本": ("剧本", "场景", "幕", "旁白", "台词"),
}

LIBRARY_CLASSIFICATION_SAMPLE_SIZE = 16000

PUBLIC_COMPETITION_GROUPS = {
    "540678308": "极速联赛",
    "1021522088": "五笔修炼基地",
    "151040026": "帝隆",
    "776227233": "梦幻打字阁",
    "391047371": "092五笔正规闲聊群",
    "201323122": "倉頡之友",
    "488748631": "小鹤进修班",
}
PUBLIC_COMPETITION_SOURCE_IDS = {"391047371": "251561500", "488748631": "229346935"}
PUBLIC_COMPETITION_SEGMENTS = {
    "540678308": "jsls", "1021522088": "wbxl", "151040026": "dl",
    "776227233": "mhdzg", "391047371": "092wb", "201323122": "cjjy",
    "488748631": "xh",
}
DAZI_API = "https://www.dazi.club/api"
JSXIAOSHI = "https://www.jsxiaoshi.com"
TIGER_API = "https://race.tiger-code.com/api"
PUBLIC_COMPETITION_CACHE_TTL = 60
CONTEST_LIBRARY_ROOT = Path(os.environ.get("FM_CONTEST_LIBRARY_ROOT", "/data/contest-library"))


def contest_file_path(relative_path: str) -> Path:
    """Resolve an indexed contest path without allowing it to escape the archive root."""
    relative = Path(str(relative_path or ""))
    if relative.is_absolute() or ".." in relative.parts:
        raise ValueError("invalid contest relative path")
    return CONTEST_LIBRARY_ROOT / relative


def read_contest_content(row) -> str:
    """Use the server-side TXT archive first, retaining DB content as a compatibility fallback."""
    try:
        path = contest_file_path(row["relative_path"])
        content = path.read_text(encoding="utf-8").strip()
        if content:
            return content
    except (OSError, UnicodeError, ValueError, KeyError, TypeError):
        pass
    return str(row["content"] or "")
_PUBLIC_COMPETITION_CACHE = {}
_PUBLIC_COMPETITION_CACHE_LOCK = threading.Lock()


class PublicCompetitionError(RuntimeError):
    def __init__(self, code: str, message: str, status=HTTPStatus.BAD_GATEWAY):
        super().__init__(message)
        self.code = code
        self.status = status


def clean_public_text(value) -> str:
    text = html.unescape(str(value or ""))
    text = re.sub(r"<br\s*/?>", "\n", text, flags=re.I)
    text = re.sub(r"</p\s*>", "\n", text, flags=re.I)
    text = re.sub(r"<[^>]+>", "", text)
    text = text.replace("\u200b", "").replace("\r\n", "\n").replace("\r", "\n")
    return "\n".join(line.strip() for line in text.split("\n") if line.strip())


def public_number(value) -> float:
    try:
        text = str(value or "").replace("%", "").strip()
        return float(text) if text else 0.0
    except (TypeError, ValueError):
        return 0.0


class PublicCompetitionTableParser(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.in_table = False
        self.depth = 0
        self.in_row = False
        self.in_cell = False
        self.cell = []
        self.row = []
        self.rows = []

    def handle_starttag(self, tag, attrs):
        attributes = dict(attrs)
        if tag == "table" and attributes.get("id") == "sdph":
            self.in_table = True
            self.depth = 1
            return
        if not self.in_table:
            return
        if tag == "table":
            self.depth += 1
        elif tag == "tr":
            self.in_row = True
            self.row = []
        elif tag in ("td", "th") and self.in_row:
            self.in_cell = True
            self.cell = []

    def handle_endtag(self, tag):
        if not self.in_table:
            return
        if tag in ("td", "th") and self.in_cell:
            self.row.append(clean_public_text("".join(self.cell)).replace("\n", " "))
            self.in_cell = False
        elif tag == "tr" and self.in_row:
            if self.row:
                self.rows.append(self.row)
            self.in_row = False
        elif tag == "table":
            self.depth -= 1
            if self.depth <= 0:
                self.in_table = False

    def handle_data(self, data):
        if self.in_table and self.in_cell:
            self.cell.append(data)


def public_competition_date(value="") -> str:
    value = str(value or "").strip()
    if not value:
        return datetime.now(timezone(timedelta(hours=8))).date().isoformat()
    match = re.search(r"(20\d{2})[-./](\d{1,2})[-./](\d{1,2})", value)
    if not match:
        raise PublicCompetitionError("invalid_date", "日期必须是 YYYY-MM-DD 格式。", HTTPStatus.BAD_REQUEST)
    try:
        return datetime(int(match.group(1)), int(match.group(2)), int(match.group(3))).date().isoformat()
    except ValueError as exc:
        raise PublicCompetitionError("invalid_date", "日期不存在。", HTTPStatus.BAD_REQUEST) from exc


def resolve_public_competition(source="", current_group="") -> dict:
    compact = re.sub(r"\s+", "", str(source or "")).lower()
    aliases = {
        "极速杯": "comp", "键神杯": "comp", "jsb": "comp", "comp": "comp",
        "锦标赛": "champ", "jbs": "champ", "champ": "champ",
        "虎杯": "tiger", "hb": "tiger", "tiger": "tiger",
    }
    if compact in aliases:
        kind = aliases[compact]
        return {
            "kind": kind,
            "source": {"comp": "极速杯", "champ": "锦标赛", "tiger": "虎杯"}[kind],
            "group_id": "", "query_group_id": "", "segment": "",
        }
    selected = ""
    for group_id, name in PUBLIC_COMPETITION_GROUPS.items():
        if group_id in compact or re.sub(r"\s+", "", name).lower() in compact:
            selected = group_id
            break
    if not selected and str(current_group or "").strip() in PUBLIC_COMPETITION_GROUPS:
        selected = str(current_group).strip()
    if not selected:
        raise PublicCompetitionError(
            "unsupported_source",
            "没有识别到可查询的公开赛事来源，请说明群名、群号、极速杯或锦标赛。",
            HTTPStatus.BAD_REQUEST,
        )
    return {
        "kind": "group", "source": PUBLIC_COMPETITION_GROUPS[selected],
        "group_id": selected,
        "query_group_id": PUBLIC_COMPETITION_SOURCE_IDS.get(selected, selected),
        "segment": PUBLIC_COMPETITION_SEGMENTS.get(selected, ""),
    }


def public_http_json(path: str, payload: dict) -> dict:
    request = Request(
        f"{DAZI_API}{path}", data=json.dumps(payload).encode("utf-8"), method="POST",
        headers={"Content-Type": "application/json", "User-Agent": "Mozilla/5.0 FM-CosmoBot/1.0"},
    )
    try:
        with urlopen(request, timeout=15) as response:
            value = json.loads(response.read().decode("utf-8", errors="replace"))
    except HTTPError as exc:
        raise PublicCompetitionError("upstream_http", f"公开赛事网站返回 HTTP {exc.code}。") from exc
    except (URLError, TimeoutError, OSError) as exc:
        raise PublicCompetitionError("upstream_unreachable", "暂时连接不上公开赛事网站。") from exc
    except json.JSONDecodeError as exc:
        raise PublicCompetitionError("upstream_invalid", "公开赛事网站返回了无法解析的数据。") from exc
    if not isinstance(value, dict):
        raise PublicCompetitionError("upstream_invalid", "公开赛事网站返回的数据格式不正确。")
    if value.get("status") not in (None, 1, "1", True):
        raise PublicCompetitionError("upstream_rejected", str(value.get("msg") or "公开赛事网站拒绝了查询。"))
    return value


def public_http_text(url: str) -> str:
    request = Request(url, headers={"User-Agent": "Mozilla/5.0 FM-CosmoBot/1.0"})
    try:
        with urlopen(request, timeout=15) as response:
            return response.read().decode("utf-8", errors="replace")
    except HTTPError as exc:
        raise PublicCompetitionError("upstream_http", f"公开赛事网站返回 HTTP {exc.code}。") from exc
    except (URLError, TimeoutError, OSError) as exc:
        raise PublicCompetitionError("upstream_unreachable", "暂时连接不上公开赛事网站。") from exc


def public_http_get_json(url: str) -> dict:
    request = Request(url, headers={"User-Agent": "Mozilla/5.0 FM-CosmoBot/1.0"})
    try:
        with urlopen(request, timeout=15) as response:
            value = json.loads(response.read().decode("utf-8", errors="replace"))
    except HTTPError as exc:
        raise PublicCompetitionError("upstream_http", f"公开赛事网站返回 HTTP {exc.code}。") from exc
    except (URLError, TimeoutError, OSError) as exc:
        raise PublicCompetitionError("upstream_unreachable", "暂时连接不上公开赛事网站。") from exc
    except json.JSONDecodeError as exc:
        raise PublicCompetitionError("upstream_invalid", "公开赛事网站返回了无法解析的数据。") from exc
    if not isinstance(value, dict):
        raise PublicCompetitionError("upstream_invalid", "公开赛事网站返回的数据格式不正确。")
    return value


def public_dazi_row(row: dict) -> dict:
    return {
        "rank": int(public_number(row.get("ranking"))),
        "name": clean_public_text(row.get("username") or row.get("third_username") or row.get("user_qq_number") or "未知"),
        "speed": public_number(row.get("speed")), "key": public_number(row.get("keystrokes")),
        "code": public_number(row.get("ma_chang")), "back": clean_public_text(row.get("hui_gai") or "0"),
        "wrong": clean_public_text(row.get("wrong_number") or "0"), "acc": public_number(row.get("jian_zhun")),
        "ime": clean_public_text(row.get("input_method") or row.get("from") or ""),
    }


def fetch_public_group_competition(target: dict, period: str) -> dict:
    group_id = target["query_group_id"]
    page_size = 100
    rank = public_http_json("/rank/getGroupRankList", {
        "groupNumber": group_id, "period": period, "pageSize": page_size, "pos": 0,
    })
    text = public_http_json("/text/getGroupCompetitionTextInfo", {"groupNumber": group_id, "period": period})
    rank_data = rank.get("data") if isinstance(rank.get("data"), dict) else {}
    rows = list(rank_data.get("list") or [])
    total = int(public_number(rank_data.get("total") or rank_data.get("totalCount") or rank_data.get("count")))
    while len(rows) >= page_size and len(rows) < min(total or 500, 500):
        next_page = public_http_json("/rank/getGroupRankList", {
            "groupNumber": group_id, "period": period, "pageSize": page_size, "pos": len(rows),
        })
        batch_data = next_page.get("data") if isinstance(next_page.get("data"), dict) else {}
        batch = list(batch_data.get("list") or [])
        if not batch:
            break
        rows.extend(batch)
        if len(batch) < page_size:
            break
    info = text.get("data") if isinstance(text.get("data"), dict) else {}
    content = clean_public_text(info.get("content"))
    if content and target["source"] not in content.split("\n", 1)[0]:
        lines = content.split("\n")
        lines[0] = f"{target['source']}日赛｜{lines[0]}"
        content = "\n".join(lines)
    if content and target["segment"]:
        lines = content.split("\n")
        lines[-1] = re.sub(r"第\s*\d{1,10}\s*段", f"第{target['segment']}段", lines[-1], count=1)
        content = "\n".join(lines)
    return {
        **target, "date": period, "rows": [public_dazi_row(row) for row in rows],
        "title": content.split("\n", 1)[0] if content else "", "word_number": int(public_number(info.get("word_number"))),
        "content": content,
    }


def regex_public_text(pattern: str, value: str) -> str:
    match = re.search(pattern, value, flags=re.S | re.I)
    return clean_public_text(match.group(1)) if match else ""


def fetch_public_js_competition(target: dict, period: str) -> dict:
    page = "competition_rank.html" if target["kind"] == "comp" else "championships_rank.html"
    document = public_http_text(f"{JSXIAOSHI}/{page}?{urlencode({'swnum': period})}")
    parser = PublicCompetitionTableParser()
    parser.feed(document)
    rows = []
    for cells in parser.rows:
        if len(cells) < 16 or "排名" in cells:
            continue
        rank = int(public_number(cells[2]))
        if rank <= 0:
            continue
        rows.append({
            "rank": rank, "name": clean_public_text(cells[1] or cells[6]),
            "speed": public_number(cells[8]), "key": public_number(cells[9]),
            "code": public_number(cells[10]), "back": clean_public_text(cells[12]),
            "wrong": "", "acc": public_number(cells[13]), "ime": clean_public_text(cells[15]),
        })
    title = regex_public_text(r"赛文标题：\s*<span[^>]*id=[\"']title[\"'][^>]*>(.*?)</span>", document)
    word_number = int(public_number(regex_public_text(r"赛文总字数：\s*(\d+)", document)))
    content = regex_public_text(r"<p[^>]*id=[\"']content[\"'][^>]*>(.*?)</p>", document)
    if target["kind"] == "champ":
        content = ""
    return {**target, "date": period, "rows": rows, "title": title, "word_number": word_number, "content": content}


def fetch_public_tiger_competition(target: dict, period: str) -> dict:
    value = public_http_get_json(
        f"{TIGER_API}/leaderboard/date/{period}?{urlencode({'limit': 100})}"
    )
    if value.get("success") not in (True, 1, "1"):
        raise PublicCompetitionError(
            "upstream_rejected",
            str(value.get("message") or value.get("msg") or "虎杯网站拒绝了查询。"),
        )
    data = value.get("data") if isinstance(value.get("data"), dict) else {}
    leaderboard = data.get("leaderboard") if isinstance(data.get("leaderboard"), list) else []
    rows = []
    for item in leaderboard:
        if not isinstance(item, dict):
            continue
        rank = int(public_number(item.get("rank")))
        if rank <= 0:
            continue
        rows.append({
            "rank": rank,
            "name": clean_public_text(item.get("username") or "未知"),
            "speed": public_number(item.get("speed")),
            "key": public_number(item.get("hit_rate")),
            "code": public_number(item.get("kpw")),
            "back": clean_public_text(item.get("correction_count") or "0"),
            "wrong": "0",
            "acc": public_number(item.get("accuracy")),
            "ime": clean_public_text(item.get("input_method") or ""),
        })
    return {
        **target,
        "date": str(data.get("date") or period),
        "rows": rows,
        "title": "虎杯",
        "word_number": 0,
        "content": "",
    }


def get_public_competition(source="", current_group="", period="", refresh=False) -> dict:
    target = resolve_public_competition(source, current_group)
    period = public_competition_date(period)
    cache_key = (target["kind"], target.get("query_group_id", ""), period)
    now = time.time()
    with _PUBLIC_COMPETITION_CACHE_LOCK:
        cached = _PUBLIC_COMPETITION_CACHE.get(cache_key)
        if not refresh and cached and now - cached[0] < PUBLIC_COMPETITION_CACHE_TTL:
            return cached[1]
    if target["kind"] == "group":
        result = fetch_public_group_competition(target, period)
    elif target["kind"] == "tiger":
        result = fetch_public_tiger_competition(target, period)
    else:
        result = fetch_public_js_competition(target, period)
    result["row_count"] = len(result["rows"])
    result["page_count"] = max(1, (len(result["rows"]) + 19) // 20)
    with _PUBLIC_COMPETITION_CACHE_LOCK:
        _PUBLIC_COMPETITION_CACHE[cache_key] = (now, result)
    return result


def connect(path: str) -> sqlite3.Connection:
    db = sqlite3.connect(path)
    db.row_factory = sqlite3.Row
    db.executescript(SCHEMA)
    ensure_column(db, "library_session_modes", "requested_length", "INTEGER NOT NULL DEFAULT 0")
    ensure_column(db, "library_session_modes", "requested_genre", "TEXT NOT NULL DEFAULT ''")
    ensure_column(db, "library_previous_sessions", "requested_length", "INTEGER NOT NULL DEFAULT 0")
    ensure_column(db, "library_previous_sessions", "requested_genre", "TEXT NOT NULL DEFAULT ''")
    ensure_column(db, "recall_records", "image_urls_json", "TEXT NOT NULL DEFAULT '[]'")
    ensure_column(db, "recent_messages", "image_urls_json", "TEXT NOT NULL DEFAULT '[]'")
    ranking_version = db.execute(
        "SELECT value_json FROM settings WHERE key='library_ranking_version'"
    ).fetchone()
    if not ranking_version or ranking_version["value_json"] != json.dumps(LIBRARY_RANKING_VERSION):
        # Rankings are derived data. Drop only this cache when the scoring
        # algorithm changes; source texts and saved classification remain intact.
        db.execute("DELETE FROM library_rankings")
        db.execute(
            "INSERT INTO settings(key,value_json) VALUES (?,?) "
            "ON CONFLICT(key) DO UPDATE SET value_json=excluded.value_json",
            ("library_ranking_version", json.dumps(LIBRARY_RANKING_VERSION)),
        )
        db.commit()
    return db


def ensure_column(db: sqlite3.Connection, table: str, column: str, declaration: str) -> None:
    columns = {row["name"] for row in db.execute(f"PRAGMA table_info({table})")}
    if column not in columns:
        db.execute(f"ALTER TABLE {table} ADD COLUMN {column} {declaration}")


class LibraryRanker:
    def __init__(self, data_dir: Path):
        self.words = load_json(data_dir / "zongCiPin.json", {})
        self.valid_chars = set(load_json(data_dir / "validChar.json", {}).get("k") or [])
        prefixes = set(load_json(data_dir / "pre.json", {}).get("k") or [])
        self.trie = {}
        for word, value in self.words.items():
            if not word:
                continue
            node = self.trie
            for char in str(word):
                node = node.setdefault(char, {})
            try:
                node[RANK_TRIE_WORD_KEY] = value[-1]
            except (IndexError, TypeError):
                node[RANK_TRIE_WORD_KEY] = 1
        for prefix in prefixes:
            if not prefix:
                continue
            node = self.trie
            for char in str(prefix):
                node = node.setdefault(char, {})
            node[RANK_TRIE_PRE_KEY] = True

    @property
    def available(self) -> bool:
        return bool(self.words and self.valid_chars and self.trie)

    def rank(self, text: str) -> tuple[float, str]:
        if not self.available:
            return heuristic_library_rank(text)
        source = (text or "").strip()
        cleaned = []
        for pos, char in enumerate(source):
            if char in self.valid_chars:
                cleaned.append(char)
            elif char == " " and 0 < pos < len(source) - 1:
                if source[pos - 1] in LETTER_DIGIT and source[pos + 1] in LETTER_DIGIT:
                    cleaned.append(char)
            elif char in ":,.;!'\"" and (
                (pos > 0 and source[pos - 1] in LETTER_DIGIT)
                or (pos + 1 < len(source) and source[pos + 1] in LETTER_DIGIT)
            ):
                cleaned.append(char)
        source = "".join(cleaned)
        if not source:
            return heuristic_library_rank(text)

        paths = [(-1, -1, -1)] * (len(source) + 1)
        paths[0] = (0, 0, -1)
        for pos, char in enumerate(source):
            if paths[pos][0] == -1:
                continue
            node = self.trie.get(char)
            word_len = 1
            if node is not None and RANK_TRIE_WORD_KEY in node:
                word_len = node[RANK_TRIE_WORD_KEY]
            elif char in self.words:
                word_len = self.words[char][-1]
            self._update_path(paths, pos, pos + 1, word_len)

            if node is None or not node.get(RANK_TRIE_PRE_KEY):
                continue
            current = pos + 1
            while current < len(source):
                node = node.get(source[current])
                if node is None:
                    break
                current += 1
                if RANK_TRIE_WORD_KEY in node:
                    self._update_path(paths, pos, current, node[RANK_TRIE_WORD_KEY])
                if not node.get(RANK_TRIE_PRE_KEY):
                    break

        current = len(source)
        water = 1.0
        hard = 0.0
        while current:
            previous = paths[current][2]
            if previous < 0:
                break
            word = source[previous:current]
            if word in self.words:
                frequency = self.words[word][0]
                if len(word) == 1:
                    hard += min(10, pow(frequency, 1.5) / 100000)
                else:
                    water += 2000 / (frequency + 2000)
            current = previous
        return library_difficulty(round(hard / water, 2))

    @staticmethod
    def _update_path(paths, start: int, end: int, word_len) -> None:
        target = paths[end]
        code_len = paths[start][0] + word_len
        word_count = paths[start][1] + 1
        if target[0] == -1 or target[0] > code_len or (
            target[0] == code_len and target[1] > word_count
        ):
            paths[end] = (code_len, word_count, start)


def get_library_ranker() -> LibraryRanker:
    global _RANKER
    if _RANKER is None:
        data_dir = Path(os.environ.get("FM_RANK_DATA", "/assets/rank_data"))
        if not data_dir.exists():
            data_dir = Path(__file__).parent / "assets" / "rank_data"
        _RANKER = LibraryRanker(data_dir)
    return _RANKER


def library_difficulty(score: float) -> tuple[float, str]:
    if score < 0.1:
        return score, "淼"
    if score < 0.3:
        return score, "水"
    if score < 0.8:
        return score, "易"
    if score < 5:
        return score, "普"
    if score < 15:
        return score, "难"
    return score, "虐"


def heuristic_library_rank(text: str) -> tuple[float, str]:
    compact = re.sub(r"\s+", "", text or "")
    if not compact:
        return 0.0, "普"
    cjk = re.findall(r"[\u4e00-\u9fff]", compact)
    uncommon = re.findall(r"[\u3400-\u4dff\u9fa6-\u9fff]", compact)
    punctuation = re.findall(r"[，。！？；：、,.!?;:]", compact)
    score = min(20.0, len(uncommon) * 2.5 + len(set(cjk)) / max(1, len(cjk)) * 4 + len(punctuation) / max(1, len(compact)))
    return library_difficulty(round(score, 2))


def normalize_library_difficulty(value) -> str:
    value = str(value or "").strip().lower()
    if not value or value in {"随机", "默认", "random", "任意"}:
        return ""
    value = LIBRARY_DIFFICULTY_ALIASES.get(value, value)
    if value not in LIBRARY_DIFFICULTIES:
        raise ValueError("difficulty must be one of 随机、淼、水、易、普、难、虐")
    return value


def normalize_library_genre(value) -> str:
    normalized = re.sub(r"\s+", "", str(value or "").strip().lower())
    if not normalized:
        return ""
    if normalized in LIBRARY_GENRE_RULES:
        return normalized
    return LIBRARY_GENRE_ALIASES.get(normalized, "")


def infer_library_genre(value) -> str:
    normalized = re.sub(r"\s+", "", str(value or "").strip().lower())
    for genre in sorted(LIBRARY_GENRE_RULES, key=len, reverse=True):
        if genre in normalized:
            return genre
    for alias in sorted(LIBRARY_GENRE_ALIASES, key=len, reverse=True):
        if alias in normalized:
            return LIBRARY_GENRE_ALIASES[alias]
    return ""


def resolve_library_genre(value) -> str:
    """Accept both canonical labels and natural-language genre phrases."""
    return normalize_library_genre(value) or infer_library_genre(value)


def library_query_without_genre(query: str, genre: str) -> str:
    value = str(query or "").strip()
    if not genre:
        return value
    aliases = [genre] + [alias for alias, target in LIBRARY_GENRE_ALIASES.items() if target == genre]
    for alias in sorted(set(aliases), key=len, reverse=True):
        value = re.sub(re.escape(alias), " ", value, flags=re.IGNORECASE)
    value = re.sub(r"(?:题材|类型|文体|小说|文章|故事|文|来一篇|发一篇|给我|找一篇)", " ", value)
    return re.sub(r"\s+", " ", value).strip()


def library_classification_sample(content: str) -> str:
    """Keep both the opening and ending when a source is unusually long."""
    value = str(content or "")
    if len(value) <= LIBRARY_CLASSIFICATION_SAMPLE_SIZE:
        return value
    tail_size = LIBRARY_CLASSIFICATION_SAMPLE_SIZE // 3
    return value[:LIBRARY_CLASSIFICATION_SAMPLE_SIZE - tail_size] + value[-tail_size:]


def classify_library_text(title: str, content: str) -> dict:
    """Classify a complete article without a model call or network dependency."""
    title_compact = re.sub(r"\s+", "", str(title or "")).lower()
    body_compact = re.sub(r"\s+", "", str(content or "")).lower()
    sample = re.sub(r"\s+", "", library_classification_sample(content)).lower()
    genre_scores = {}
    evidence = {}
    genre_quality = {}
    for genre, terms in LIBRARY_GENRE_RULES.items():
        score = 0.0
        hits = {}
        for term in terms:
            title_hits = title_compact.count(term)
            body_hits = min(sample.count(term), 8)
            if title_hits or body_hits:
                hits[term] = {"title": title_hits, "body": body_hits}
                weight = 2.5 if term in LIBRARY_GENRE_STRONG_TERMS.get(genre, set()) else 1.0
                score += title_hits * weight * 2.5 + min(body_hits, 3) * weight
                score += max(0, body_hits - 3) * weight * 0.25
        title_hits_total = sum(item["title"] for item in hits.values())
        strong_hits = sum(
            item["title"] + item["body"]
            for term, item in hits.items()
            if term in LIBRARY_GENRE_STRONG_TERMS.get(genre, set())
        )
        # A single generic word is not enough to claim a genre. This keeps
        # ordinary essays from being mislabeled as romance, family, or
        # motivational texts while still accepting an explicit title signal.
        reliable = bool(
            score >= 2
            and (title_hits_total > 0 or strong_hits > 0 or len(hits) >= 2)
        )
        if score and reliable:
            genre_scores[genre] = round(score, 2)
            evidence[genre] = hits
            genre_quality[genre] = {
                "distinct_terms": len(hits),
                "title_hits": title_hits_total,
                "strong_hits": strong_hits,
            }

    ordered = sorted(genre_scores.items(), key=lambda item: (-item[1], item[0]))
    if not ordered or ordered[0][1] < 2:
        primary_genre = "综合文学"
        genres = [primary_genre]
        confidence = 0.28
    else:
        top_score = ordered[0][1]
        second_score = ordered[1][1] if len(ordered) > 1 else 0.0
        primary_genre = ordered[0][0]
        genres = [
            genre for genre, score in ordered[:3]
            if score >= max(2.0, top_score * 0.42)
        ]
        quality = genre_quality[primary_genre]
        confidence = 0.42 + min(0.3, quality["strong_hits"] * 0.06)
        confidence += min(0.18, quality["distinct_terms"] * 0.04)
        confidence += min(0.09, quality["title_hits"] * 0.03)
        confidence += min(0.1, max(0.0, top_score - second_score) / 40.0)
        confidence = round(min(0.99, confidence), 3)

    form_scores = {"小说/叙事": 1.0}
    title_lines = [line.strip() for line in str(content or "").splitlines() if line.strip()]
    compact_lines = [re.sub(r"\s+", "", line) for line in title_lines]
    body_text = str(content or "")
    for form, terms in LIBRARY_FORM_RULES.items():
        title_hits = sum(title_compact.count(term) for term in terms)
        body_hits = sum(min(sample.count(term), 4) for term in terms)
        if title_hits or body_hits:
            form_scores[form] = float(title_hits * 4 + body_hits)
    if len(compact_lines) >= 3 and len(compact_lines) <= 20:
        short_lines = sum(len(line) <= 32 for line in compact_lines)
        if short_lines >= len(compact_lines) * 0.7 and len(body_compact) <= 500:
            form_scores["诗歌"] = form_scores.get("诗歌", 0) + 4
    dialogue_turns = len(re.findall(r"(?m)^[^\n：:]{1,16}[：:]", body_text))
    if dialogue_turns >= 2 or len(re.findall(r"[“”‘’]", body_text)) >= 8:
        form_scores["对话剧本"] = form_scores.get("对话剧本", 0) + dialogue_turns * 2 + 2
    if re.search(r"(?:他说|她说|问道|答道|说道|回答)[：:]", body_text):
        form_scores["小说/叙事"] += 2
    form = max(form_scores.items(), key=lambda item: (item[1], item[0]))[0]
    return {
        "primary_genre": primary_genre,
        "genres": genres,
        "form": form,
        "confidence": round(confidence, 3),
        "evidence": evidence,
        "genre_scores": genre_scores,
        "genre_quality": genre_quality,
    }


def upsert_library_classification(db: sqlite3.Connection, text_id: str, metadata: dict) -> None:
    db.execute(
        "INSERT INTO library_classifications "
        "(text_id,primary_genre,genres_json,form,difficulty,difficulty_score,confidence,evidence_json,classifier_version,classified_at) "
        "VALUES (?,?,?,?,?,?,?,?,?,?) "
        "ON CONFLICT(text_id) DO UPDATE SET primary_genre=excluded.primary_genre,genres_json=excluded.genres_json,"
        "form=excluded.form,difficulty=excluded.difficulty,difficulty_score=excluded.difficulty_score,"
        "confidence=excluded.confidence,evidence_json=excluded.evidence_json,classifier_version=excluded.classifier_version,"
        "classified_at=excluded.classified_at",
        (
            text_id, metadata["primary_genre"], json.dumps(metadata["genres"], ensure_ascii=False),
            metadata["form"], metadata["difficulty"], metadata["difficulty_score"],
            metadata["confidence"], json.dumps(metadata["evidence"], ensure_ascii=False),
            LIBRARY_CLASSIFIER_VERSION, time.time(),
        ),
    )


def classify_library_row(db: sqlite3.Connection, row: sqlite3.Row, refresh: bool = False) -> dict:
    cached = db.execute(
        "SELECT * FROM library_classifications WHERE text_id=?", (row["text_id"],)
    ).fetchone()
    if cached and not refresh and cached["classifier_version"] == LIBRARY_CLASSIFIER_VERSION:
        metadata = dict(cached)
        metadata["genres"] = json.loads(metadata["genres_json"] or "[]")
        metadata["evidence"] = json.loads(metadata["evidence_json"] or "{}")
        return metadata
    content = normalize_library_content(row["content"])
    score, difficulty = get_library_ranker().rank(content)
    metadata = classify_library_text(row["title"], row["content"])
    metadata.update({
        "text_id": row["text_id"],
        "difficulty": difficulty,
        "difficulty_score": score,
        "classifier_version": LIBRARY_CLASSIFIER_VERSION,
        "classified_at": time.time(),
    })
    upsert_library_classification(db, row["text_id"], metadata)
    return metadata


def library_metadata_summary(metadata: dict, segment_difficulty: str = "", segment_score=None) -> str:
    score = float(metadata.get("difficulty_score") or 0)
    summary = (
        f"整篇难度={metadata.get('difficulty') or '普'}({score:.2f})，"
        f"题材={metadata.get('primary_genre') or '综合文学'}，"
        f"文体={metadata.get('form') or '小说/叙事'}，"
        f"置信度={float(metadata.get('confidence') or 0):.0%}"
    )
    if segment_difficulty:
        segment_value = f"{float(segment_score):.2f}" if segment_score is not None else "未知"
        summary += f"；当前段={segment_difficulty}({segment_value})"
    return summary


def library_has_genre(metadata: dict, genre: str) -> bool:
    return genre and (
        metadata.get("primary_genre") == genre
        or genre in (metadata.get("genres") or [])
    )


def library_genre_matches(db: sqlite3.Connection, row: sqlite3.Row, genre: str) -> bool:
    """Use the cheap genre pass while scanning candidates; rank only selected rows."""
    if not genre:
        return True
    cached = db.execute(
        "SELECT primary_genre,genres_json,classifier_version FROM library_classifications WHERE text_id=?",
        (row["text_id"],),
    ).fetchone()
    if cached and cached["classifier_version"] == LIBRARY_CLASSIFIER_VERSION:
        try:
            genres = json.loads(cached["genres_json"] or "[]")
        except (TypeError, json.JSONDecodeError):
            genres = []
        return cached["primary_genre"] == genre or genre in genres
    return library_has_genre(classify_library_text(row["title"], row["content"]), genre)


LIBRARY_SEPARATOR_RE = re.compile(r"(?:[=*_~#]{5,}|[-—–]{6,}|[·•]{8,})")
LIBRARY_DATE_PREFIX_RE = re.compile(
    r"^(?:(?:19|20)\d{2}(?:[-/.年]\d{1,2}){1,2}(?:日)?"
    r"(?:\s*\d{1,2}[:：]\d{2}(?::\d{2})?)?\s*)+"
)
LIBRARY_DYNASTY_AUTHOR_RE = re.compile(
    r"^(?:(?:先秦|秦代|汉代|两汉|魏晋|南北朝|隋代|唐代|五代|宋代|元代|"
    r"明代|清代|近代|现代|近现代|当代|朝代|作者|译者|编者)[：:]\s*"
    r"[\u3400-\u9fff·]{1,12}\s*)+"
)
LIBRARY_SOURCE_PREFIX_RE = re.compile(
    r"^(?:(?:来源|出处|转自|转载自|文章来源|文章作者|发布者|责任编辑)[：:]\s*"
    r"(?:散文网|美文网|作文网|诗词网|古诗文网|网络|互联网|百度|知乎|"
    r"搜狐|新浪|网易|腾讯|微信公众号|未知|原创|佚名|[A-Za-z0-9_.-]{2,40})\s*)+",
    re.IGNORECASE,
)
LIBRARY_METADATA_LINE_RE = re.compile(
    r"^(?:(?:来源|出处|转自|转载|作者|译者|编者|责任编辑|发布时间|发布日期|"
    r"更新时间|文章时间|时间|阅读|阅读次数|字号|本文网址|原文地址|网站|栏目|频道)"
    r"(?:[：:].{0,100})?|(?:先秦|秦代|汉代|两汉|魏晋|南北朝|隋代|唐代|五代|"
    r"宋代|元代|明代|清代|近代|现代|近现代|当代)|"
    r"(?:19|20)\d{2}[-/.年]\d{1,2}(?:[-/.月]\d{1,2}日?)?"
    r"(?:\s*\d{1,2}[:：]\d{2}(?::\d{2})?)?"
    r"(?:\s*[（(]?(?:来源|分类)[：:]?.{0,100}[）)]?)?)$",
    re.IGNORECASE,
)
LIBRARY_NAVIGATION_LINE_RE = re.compile(
    r"^(?:首页|返回首页|返回目录|上一页|下一页|上一篇|下一篇|目录|正文开始|正文结束|"
    r"点击阅读|点击查看|点击下载|阅读全文|展开全文|收起全文|加入收藏|收藏本站|"
    r"请收藏|最新网址|手机阅读|扫码阅读|广告|赞助商链接|相关阅读|相关推荐|"
    r"本文由.*(?:整理发布|版权归).*|版权归原作者所有.*|[>]|大|中|小|次|"
    r"Copyright(?:\s+©)?.*|All Rights Reserved\.?)$",
    re.IGNORECASE,
)
LIBRARY_LITERARY_TITLE_RE = re.compile(
    r"(?:诗经|楚辞|唐诗|宋词|元曲|古诗|诗词|绝句|律诗|乐府|古文|名篇|"
    r"辞|赋|颂|铭|序|表|传|经|歌|行|吟|咏|令|引|曲|词)$"
)
LIBRARY_ANTHOLOGY_TITLE_RE = re.compile(
    r"(?:合集|大全|汇总|精选\d*篇|通用\d*篇|推荐|必背|名篇\d+|\d+篇|"
    r"名言|语录|诗句|句子|说说|祝福语|承诺书|读后感|获奖名单)"
)
LIBRARY_EXPLICIT_BUNDLE_TITLE_RE = re.compile(
    r"(?:合集|大全|汇总|精选\s*\d*\s*篇|通用\s*\d+\s*篇|\d+\s*篇|获奖名单)"
)
LIBRARY_COLLECTION_TITLE_RE = re.compile(
    r"(?:签名|短信|祝福语|问候语|语录|名言|句子|诗句|流行语)"
)
LIBRARY_SOURCE_VALUE_RE = re.compile(
    r"^(?:散文网|美文网|作文网|诗词网|古诗文网|网络|互联网|百度|知乎|"
    r"搜狐|新浪|网易|腾讯|微信公众号|未知|原创|佚名)$",
    re.IGNORECASE,
)


def _strip_library_leading_metadata(value: str, title: str) -> tuple[str, list[str]]:
    reasons = []
    value = value.lstrip(" \t\r\n:：|｜-—_=·•")
    clean_title = re.sub(r"\s+", "", title or "").strip()
    for _ in range(6):
        before = value
        if clean_title:
            value, count = re.subn(
                rf"^(?:[《〈【\[]?{re.escape(clean_title)}[》〉】\]]?\s*)+",
                "", value, count=1,
            )
            if count:
                reasons.append("repeated_title")
        value, count = LIBRARY_SEPARATOR_RE.subn("\n", value, count=1)
        if count:
            reasons.append("separator")
        value = value.lstrip(" \t\r\n:：|｜-—_=·•")
        for pattern, reason in (
            (LIBRARY_DATE_PREFIX_RE, "web_date"),
            (LIBRARY_SOURCE_PREFIX_RE, "source_metadata"),
            (LIBRARY_DYNASTY_AUTHOR_RE, "author_metadata"),
        ):
            value, count = pattern.subn("", value, count=1)
            if count:
                reasons.append(reason)
                value = value.lstrip(" \t\r\n:：|｜-—_=·•")
        if value == before:
            break
    return value, reasons


def _library_heading_key(value: str) -> str:
    return re.sub(r"[^A-Za-z0-9\u3400-\u9fff]", "", value or "").lower()


def clean_library_content(title: str, value: str) -> tuple[str, list[str]]:
    """Remove deterministic scrape noise while preserving actual prose and verse."""
    original = value or ""
    value = unicodedata.normalize("NFC", original)
    value = html.unescape(value).replace("\ufeff", "").replace("\u200b", "")
    value = value.replace("\r\n", "\n").replace("\r", "\n").replace("\u00a0", " ")
    value = re.sub(r"(?is)<(?:script|style)\b[^>]*>.*?</(?:script|style)>", "", value)
    value = re.sub(r"(?i)<br\s*/?>|</?p\b[^>]*>|</?div\b[^>]*>", "\n", value)
    value = re.sub(r"<[^>]{1,500}>", "", value)
    value = re.sub(r"!\[[^\]]*\]\([^)]*\)", "", value)
    value = re.sub(r"\[(?:图片|插图|配图|图\s*\d*|image)[^\]]*\]", "", value, flags=re.IGNORECASE)
    value = re.sub(r"https?://[^\s<>\u3000]+|www\.[^\s<>\u3000]+", "", value, flags=re.IGNORECASE)
    value, reading_site_count = re.subn(
        r"[（(]\s*文章阅读网\s*[：:]?\s*[）)]", "", value, flags=re.IGNORECASE
    )
    value, prefix_reasons = _strip_library_leading_metadata(value, title)

    reasons = list(prefix_reasons)
    if reading_site_count:
        reasons.append("navigation")
    cleaned_lines = []
    previous = None
    skip_metadata_value = ""
    for raw_line in value.split("\n"):
        line = re.sub(r"[ \t\u3000]+", " ", raw_line).strip()
        if not line:
            continue
        if skip_metadata_value:
            if re.fullmatch(r"[:：|｜\-—_=·•]+", line):
                continue
            if skip_metadata_value == "source" and LIBRARY_SOURCE_VALUE_RE.fullmatch(line):
                reasons.append("source_metadata")
                skip_metadata_value = ""
                continue
            if skip_metadata_value == "person" and re.fullmatch(r"[\u3400-\u9fff·]{1,12}", line):
                reasons.append("author_metadata")
                skip_metadata_value = ""
                continue
            skip_metadata_value = ""
        if LIBRARY_SEPARATOR_RE.fullmatch(line):
            reasons.append("separator")
            continue
        if LIBRARY_METADATA_LINE_RE.fullmatch(line):
            reasons.append("web_metadata")
            if re.fullmatch(r"(?:来源|出处|转自|转载)[：:]?", line):
                skip_metadata_value = "source"
            elif re.fullmatch(
                r"(?:作者|译者|编者|责任编辑|先秦|秦代|汉代|两汉|魏晋|南北朝|隋代|"
                r"唐代|五代|宋代|元代|明代|清代|近代|现代|近现代|当代)[：:]?",
                line,
            ):
                skip_metadata_value = "person"
            continue
        if LIBRARY_SOURCE_VALUE_RE.fullmatch(line):
            reasons.append("source_metadata")
            continue
        if LIBRARY_NAVIGATION_LINE_RE.fullmatch(line):
            reasons.append("navigation")
            continue
        if re.fullmatch(r"[^\s<>]{1,100}\.(?:html?|shtml|php|aspx?|jpe?g|png|gif|webp)", line, re.IGNORECASE):
            reasons.append("file_or_image")
            continue
        if not cleaned_lines and title:
            line_key = _library_heading_key(line)
            title_key = _library_heading_key(title)
            if line_key and title_key and (
                line_key == title_key
                or (min(len(line_key), len(title_key)) >= 6 and (line_key in title_key or title_key in line_key))
            ):
                reasons.append("repeated_title")
                continue
            line, count = re.subn(
                rf"^(?:[《〈【\[]?{re.escape(title)}[》〉】\]]?\s*)+", "", line, count=1
            )
            if count:
                reasons.append("repeated_title")
        line = LIBRARY_SEPARATOR_RE.sub("", line).strip(" \t:：|｜-—_=·•")
        if not line:
            continue
        if line == previous:
            reasons.append("duplicate_line")
            continue
        cleaned_lines.append(line)
        previous = line

    cleaned = "\n".join(cleaned_lines).strip()
    cleaned, trailing_source_count = re.subn(
        r"(?:(?:\s*[（(](?:文章)?(?:来源|出处)[：:]\s*[^\n]{1,240})|"
        r"(?:^|\n)\s*(?:文章)?(?:来源|出处)[：:]\s*[^\n]{1,240})\s*$",
        "",
        cleaned,
        flags=re.IGNORECASE,
    )
    if trailing_source_count:
        reasons.append("source_metadata")
    cleaned, trailing_reasons = _strip_library_leading_metadata(cleaned, title)
    reasons.extend(trailing_reasons)
    cleaned = re.sub(r"\n{3,}", "\n\n", cleaned).strip()
    if cleaned != original.strip() and not reasons:
        reasons.append("normalized_markup")
    return cleaned, sorted(set(reasons))


def library_content_quality(title: str, content: str, category: str = "fm_texts") -> tuple[bool, str]:
    compact = re.sub(r"\s+", "", content or "")
    cjk = len(re.findall(r"[\u3400-\u9fff]", compact))
    latin = len(re.findall(r"[A-Za-z]", compact))
    useful = cjk + latin + len(re.findall(r"\d", compact))
    punctuation = len(re.findall(r"[，。！？；：、,.!?;:“”‘’（）()《》]", compact))
    lines = [line for line in (content or "").splitlines() if line.strip()]
    title_text = re.sub(r"\s+", "", title or "")
    classical_hits = len(re.findall(r"(?:之|乎|者|也|矣|焉|兮|曰|吾|尔|其|乃|故|遂)", compact))
    verse_lines = (
        3 <= len(lines) <= 16
        and cjk <= 120
        and sum(len(re.sub(r"\s+", "", line)) <= 32 for line in lines) >= len(lines) * 0.7
    )
    literary = bool(
        LIBRARY_LITERARY_TITLE_RE.search(title_text)
        or ("·" in title_text and len(title_text) <= 40)
        or verse_lines
        or (cjk >= 20 and classical_hits >= max(3, cjk // 30))
    )
    anthology_entries = re.findall(
        r"(?:^|[\s。！？])\d{1,3}[.、．]?\s*[《【]", (content or "")[:5000]
    )
    numbered_entries = re.findall(r"(?:^|\n)\s*\d{1,3}[、.．]", (content or "")[:10000])
    chinese_numbered_entries = re.findall(
        r"(?:^|\n)\s*[一二三四五六七八九十百]{1,3}[、.．]", (content or "")[:10000]
    )
    multi_document_entries = re.findall(
        r"(?:心得体会|日记|委托书|读后感|活动总结|自荐信)\s*(?:篇)?\s*\d{1,3}",
        (content or "")[:50000],
    )
    if LIBRARY_EXPLICIT_BUNDLE_TITLE_RE.search(title_text):
        return False, "anthology_bundle"
    if len(multi_document_entries) >= 2:
        return False, "anthology_bundle"
    collection_preface = bool(re.search(r"(?:以下|下面).{0,20}(?:小编|整理|收集)", (content or "")[:500]))
    if LIBRARY_COLLECTION_TITLE_RE.search(title_text) and (
        len(lines) >= 20
        or len(numbered_entries) + len(chinese_numbered_entries) >= 5
        or collection_preface
    ):
        return False, "anthology_bundle"
    if LIBRARY_ANTHOLOGY_TITLE_RE.search(title_text) and (
        len(anthology_entries) >= 3 or len(numbered_entries) >= 5
    ):
        return False, "anthology_bundle"
    if not compact or useful == 0:
        return False, "empty_after_cleaning"
    if "\ufffd" in compact and compact.count("\ufffd") / len(compact) > 0.01:
        return False, "broken_encoding"
    if cjk >= 10 and cjk / max(1, len(compact)) < 0.35 and latin < 40:
        return False, "mostly_symbols"
    if useful < 80:
        poetic_short = cjk >= 20 and punctuation >= 2 and punctuation / max(1, cjk) >= 0.04
        if (literary or poetic_short) and cjk >= 20:
            return True, "literary_short_text"
        return False, "too_short"
    if cjk >= 100 and punctuation == 0 and not literary:
        return False, "punctuation_free_non_literary"
    return True, "ok"


def normalize_library_content(value: str) -> str:
    value = (value or "").replace("\u00a0", " ")
    letters = len(re.findall(r"[A-Za-z]", value))
    cjk = len(re.findall(r"[\u4e00-\u9fff]", value))
    if letters >= 80 and letters > cjk * 2:
        return re.sub(r"\s+", " ", value).strip()
    return re.sub(r"\s+", "", value)


def library_session_id(platform: str, chat_id: str, requester_id: str) -> str:
    value = f"{platform}:{chat_id}:{requester_id}".encode("utf-8")
    return hashlib.sha256(value).hexdigest()[:32]


def library_payload_identity(payload: dict) -> tuple[str, str, str, str]:
    platform = str(payload.get("platform") or "qq").strip().lower()
    chat_id = str(payload.get("chat_id") or "").strip()
    requester_id = canonical_user_id(payload.get("requester_id"))
    requester_name = str(payload.get("requester_name") or requester_id or "用户").strip()
    if not platform or not chat_id or not requester_id:
        raise ValueError("platform, chat_id, and requester_id are required")
    return platform, chat_id, requester_id, requester_name


def rank_library_row(db: sqlite3.Connection, row: sqlite3.Row) -> tuple[float, str]:
    cached = db.execute(
        "SELECT score,difficulty FROM library_rankings WHERE text_id=?", (row["text_id"],)
    ).fetchone()
    if cached:
        score = float(cached["score"])
        _, difficulty = library_difficulty(score)
        if difficulty != str(cached["difficulty"]):
            db.execute(
                "UPDATE library_rankings SET difficulty=?,ranked_at=? WHERE text_id=?",
                (difficulty, time.time(), row["text_id"]),
            )
        return score, difficulty
    content = normalize_library_content(row["content"])
    score, difficulty = get_library_ranker().rank(content)
    db.execute(
        "INSERT INTO library_rankings VALUES (?, ?, ?, ?) "
        "ON CONFLICT(text_id) DO UPDATE SET difficulty=excluded.difficulty,score=excluded.score,ranked_at=excluded.ranked_at",
        (row["text_id"], difficulty, score, time.time()),
    )
    return score, difficulty


def pick_library_row(db: sqlite3.Connection, query: str, difficulty: str) -> tuple[sqlite3.Row, float, str] | None:
    requested_genre = resolve_library_genre(query)
    search_query = library_query_without_genre(query, requested_genre)
    pattern = f"%{search_query}%"
    if difficulty and not requested_genre:
        cached = db.execute(
            "SELECT l.*,r.score,r.difficulty AS ranked_difficulty FROM library_texts l "
            "JOIN library_rankings r ON r.text_id=l.text_id "
            "WHERE (l.title LIKE ? OR l.content LIKE ?) AND r.difficulty=? "
            "AND l.category<>'fm_single_chars' "
            "ORDER BY RANDOM() LIMIT 1",
            (pattern, pattern, difficulty),
        ).fetchone()
        if cached:
            return cached, float(cached["score"]), difficulty

    if search_query:
        candidates = db.execute(
            "SELECT * FROM library_texts WHERE category<>'fm_single_chars' "
            "AND (title LIKE ? OR content LIKE ?) ORDER BY RANDOM() LIMIT ?",
            (pattern, pattern, 300 if difficulty else 500),
        ).fetchall()
    else:
        candidates = db.execute(
            "SELECT * FROM library_texts WHERE category<>'fm_single_chars' "
            "ORDER BY RANDOM() LIMIT ?",
            (2000 if requested_genre else (300 if difficulty else 50),),
        ).fetchall()
    if not candidates:
        return None
    if not difficulty and not requested_genre:
        row = random.choice(candidates)
        score, actual = rank_library_row(db, row)
        db.commit()
        return row, score, actual

    ranked = []
    for row in candidates:
        if requested_genre and not library_genre_matches(db, row, requested_genre):
            continue
        score, actual = rank_library_row(db, row)
        ranked.append((row, score, actual))
        if not difficulty or actual == difficulty:
            db.commit()
            return row, score, actual
    if not ranked:
        db.commit()
        return None
    db.commit()
    target = LIBRARY_DIFFICULTIES.index(difficulty)
    return min(ranked, key=lambda item: abs(LIBRARY_DIFFICULTIES.index(item[2]) - target))


def pick_library_segment(
    db: sqlite3.Connection,
    query: str,
    difficulty: str,
    length: int,
    exclude_text_id: str = "",
    genre: str = "",
) -> tuple[sqlite3.Row, str, float, str] | None:
    requested_genre = resolve_library_genre(genre) or resolve_library_genre(query)
    search_query = library_query_without_genre(query, requested_genre)
    pattern = f"%{search_query}%"
    base_where = "l.category<>'fm_single_chars'"
    base_params: list = []
    if search_query:
        base_where += " AND (l.title LIKE ? OR l.content LIKE ?)"
        base_params.extend([pattern, pattern])
    if exclude_text_id:
        base_where += " AND l.text_id<>?"
        base_params.append(exclude_text_id)

    if not difficulty:
        candidates = db.execute(
            f"SELECT l.* FROM library_texts l WHERE {base_where} "
            "AND LENGTH(l.content)>=? ORDER BY RANDOM() LIMIT ?",
            tuple(base_params + [length, 2000 if requested_genre else 200]),
        ).fetchall()
        if not candidates and exclude_text_id:
            candidates = db.execute(
                "SELECT l.* FROM library_texts l WHERE l.category<>'fm_single_chars' "
                + ("AND (l.title LIKE ? OR l.content LIKE ?) " if search_query else "")
                + "AND LENGTH(l.content)>=? "
                "ORDER BY RANDOM() LIMIT 200",
                tuple(([pattern, pattern] if search_query else []) + [length]),
            ).fetchall()
        for row in candidates:
            if requested_genre and not library_genre_matches(db, row, requested_genre):
                continue
            content = normalize_library_content(row["content"])
            if len(content) < length:
                continue
            body = content[:length]
            segment_score, segment_difficulty = get_library_ranker().rank(body)
            return row, body, segment_score, segment_difficulty
        return None

    if requested_genre:
        candidates = db.execute(
            f"SELECT l.* FROM library_texts l WHERE {base_where} "
            "AND LENGTH(l.content)>=? ORDER BY RANDOM() LIMIT 2000",
            tuple(base_params + [length]),
        ).fetchall()
    else:
        candidates = db.execute(
            "SELECT l.* FROM library_texts l JOIN library_rankings r ON r.text_id=l.text_id "
            f"WHERE {base_where} AND LENGTH(l.content)>=? AND r.difficulty=? "
            "ORDER BY RANDOM() LIMIT 300",
            tuple(base_params + [length, difficulty]),
        ).fetchall()
    candidates += db.execute(
        f"SELECT l.* FROM library_texts l WHERE {base_where} "
        "AND LENGTH(l.content)>=? ORDER BY RANDOM() LIMIT 2000",
        tuple(base_params + [length]),
    ).fetchall()
    if not candidates and exclude_text_id:
        return pick_library_segment(db, query, difficulty, length, genre=genre)
    seen = set()
    for row in candidates:
        if row["text_id"] in seen:
            continue
        seen.add(row["text_id"])
        if requested_genre and not library_genre_matches(db, row, requested_genre):
            continue
        content = normalize_library_content(row["content"])
        if len(content) < length:
            continue
        body = content[:length]
        metadata = classify_library_row(db, row)
        if metadata["difficulty"] == difficulty:
            segment_score, segment_difficulty = get_library_ranker().rank(body)
            return row, body, segment_score, segment_difficulty
    return None


def next_library_segment_id(db: sqlite3.Connection) -> int:
    for _ in range(100):
        segment_id = random.randint(100000, 999999)
        cursor = db.execute(
            "INSERT OR IGNORE INTO library_segment_ids VALUES (?, ?)",
            (segment_id, time.time()),
        )
        if cursor.rowcount:
            return segment_id
    for segment_id in range(100000, 1000000):
        cursor = db.execute(
            "INSERT OR IGNORE INTO library_segment_ids VALUES (?, ?)",
            (segment_id, time.time()),
        )
        if cursor.rowcount:
            return segment_id
    raise RuntimeError("all six-digit library segment ids are exhausted")


def clean_historical_contest_body(value: str, title: str) -> str:
    lines = [line.strip() for line in str(value or "").splitlines() if line.strip()]
    normalized_title = str(title or "").strip()
    if lines and normalized_title:
        first = lines[0]
        if first == normalized_title or first.endswith(normalized_title) or normalized_title.endswith(first):
            lines.pop(0)
    return "\n".join(lines).strip()


def contest_session_id(platform: str, chat_id: str, requester_id: str) -> str:
    value = f"contest:{platform}:{chat_id}:{requester_id}".encode("utf-8")
    return hashlib.sha256(value).hexdigest()[:32]


def pick_contest_row(
    db: sqlite3.Connection, query: str, source_group: str,
    competition_date: str = "", exclude_text_id: str = "",
):
    where = ["(title LIKE ? OR content LIKE ?)", "source_group LIKE ?", "competition_date LIKE ?"]
    params = [f"%{query}%", f"%{query}%", f"%{source_group}%", f"%{competition_date}%"]
    if exclude_text_id:
        where.append("text_id<>?")
        params.append(exclude_text_id)
    rows = db.execute(
        "SELECT * FROM contest_texts WHERE " + " AND ".join(where) +
        " ORDER BY competition_date DESC LIMIT 500",
        tuple(params),
    ).fetchall()
    return random.choice(rows) if rows else None


def format_contest_segment(db: sqlite3.Connection, session: dict, row) -> tuple[str, int, str]:
    body = clean_historical_contest_body(read_contest_content(row), row["title"])
    if not body:
        raise ValueError("selected contest text has no usable body")
    segment_id = next_library_segment_id(db)
    now = time.time()
    db.execute(
        "UPDATE library_segment_sessions SET consumed_at=? "
        "WHERE session_id=? AND consumed_at IS NULL",
        (now, session["session_id"]),
    )
    db.execute(
        "INSERT INTO library_segment_sessions VALUES (?, ?, ?, ?, ?, ?, NULL)",
        (
            segment_id, session["session_id"], session["platform"], session["chat_id"],
            session["requester_id"], now,
        ),
    )
    message = (
        f"[FM/赛文·{row['source_group']}] [日期{row['competition_date']}] "
        f"《{row['title']}》 [字数{len(body)}]\n"
        f"{body}\n"
        f"-----第{segment_id}段-FM发文"
    )
    return message, segment_id, body


def start_contest_session(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, requester_name = library_payload_identity(payload)
    query = str(payload.get("query") or "").strip()
    source_group = str(payload.get("source") or "").strip()
    competition_date = str(payload.get("date") or "").strip()
    row = pick_contest_row(db, query, source_group, competition_date)
    if not row:
        return {"status": "not_found", "message": "没有找到符合条件的历史赛文。"}
    now = time.time()
    session = {
        "session_id": contest_session_id(platform, chat_id, requester_id),
        "platform": platform, "chat_id": chat_id,
        "requester_id": requester_id, "requester_name": requester_name,
        "query_text": query, "source_group": source_group,
        "initial_date": competition_date, "last_text_id": row["text_id"],
        "title": row["title"], "status": "active", "last_message_id": None,
        "created_at": now, "updated_at": now,
    }
    db.execute(
        "INSERT INTO contest_sessions VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?) "
        "ON CONFLICT(platform,chat_id,requester_id) DO UPDATE SET "
        "session_id=excluded.session_id,requester_name=excluded.requester_name,"
        "query_text=excluded.query_text,source_group=excluded.source_group,"
        "initial_date=excluded.initial_date,last_text_id=excluded.last_text_id,"
        "title=excluded.title,status='active',last_message_id=NULL,"
        "created_at=excluded.created_at,updated_at=excluded.updated_at",
        tuple(session.values()),
    )
    message, segment_id, body = format_contest_segment(db, session, row)
    db.commit()
    return {
        "status": "segment", "message": message, "segment_id": segment_id,
        "session_id": session["session_id"], "text_id": row["text_id"],
        "title": row["title"], "source_group": row["source_group"],
        "competition_date": row["competition_date"], "total_chars": len(body),
        "completed": True,
    }


def continue_contest_session(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, requester_name = library_payload_identity(payload)
    row = db.execute(
        "SELECT * FROM contest_sessions WHERE platform=? AND chat_id=? "
        "AND requester_id=? AND status='active'",
        (platform, chat_id, requester_id),
    ).fetchone()
    if not row:
        return {"status": "idle", "message": "你在当前会话里没有正在进行的赛文。"}
    selected = pick_contest_row(
        db, row["query_text"], row["source_group"], "", row["last_text_id"],
    )
    if not selected:
        return {
            "status": "contest_completed",
            "message": "当前赛文条件下没有另一篇可续发的文章，可以换一个来源或关键词再试。",
            "session_id": row["session_id"],
        }
    now = time.time()
    db.execute(
        "UPDATE contest_sessions SET requester_name=?,last_text_id=?,title=?,updated_at=? "
        "WHERE session_id=?",
        (requester_name, selected["text_id"], selected["title"], now, row["session_id"]),
    )
    session = dict(row)
    session.update({
        "requester_name": requester_name, "last_text_id": selected["text_id"],
        "title": selected["title"], "updated_at": now,
    })
    message, segment_id, body = format_contest_segment(db, session, selected)
    db.commit()
    return {
        "status": "segment", "message": message, "segment_id": segment_id,
        "session_id": row["session_id"], "text_id": selected["text_id"],
        "title": selected["title"], "source_group": selected["source_group"],
        "competition_date": selected["competition_date"], "total_chars": len(body),
        "completed": True, "continuation_mode": "next_contest",
    }


def format_library_segment(
    db: sqlite3.Connection, session: dict, body: str, metadata: dict | None = None
) -> tuple[str, int]:
    score = float((metadata or {}).get("difficulty_score") or session["difficulty_score"])
    score_text = "爆表" if score > 100 else f"{score:.2f}"
    genre = (metadata or {}).get("primary_genre") or "综合文学"
    form = (metadata or {}).get("form") or "小说/叙事"
    difficulty = (metadata or {}).get("difficulty") or session["difficulty"]
    label = f"{difficulty}{score_text}·{genre}·{form}"
    article_no = str(session["text_id"])[:8].upper()
    segment_id = next_library_segment_id(db)
    now = time.time()
    db.execute(
        "UPDATE library_segment_sessions SET consumed_at=? "
        "WHERE session_id=? AND consumed_at IS NULL",
        (now, session["session_id"]),
    )
    db.execute(
        "INSERT INTO library_segment_sessions VALUES (?, ?, ?, ?, ?, ?, NULL)",
        (
            segment_id, session["session_id"], session["platform"], session["chat_id"],
            session["requester_id"], now,
        ),
    )
    return (
        f"[FM/{label}] No.FM-{article_no}《{session['title']}》 [字数{len(body)}]\n"
        f"{body}\n"
        f"-----第{segment_id}段-FM发文｜进度{session['next_offset']}/{session['total_chars']}字"
    ), segment_id


def save_previous_library_session(
    db: sqlite3.Connection, platform: str, chat_id: str, requester_id: str
) -> None:
    row = db.execute(
        "SELECT * FROM library_sessions WHERE platform=? AND chat_id=? "
        "AND requester_id=? AND status='active'",
        (platform, chat_id, requester_id),
    ).fetchone()
    if not row:
        return
    mode = db.execute(
        "SELECT requested_difficulty,requested_length,requested_genre FROM library_session_modes WHERE session_id=?",
        (row["session_id"],),
    ).fetchone()
    requested_difficulty = str(mode["requested_difficulty"]) if mode else ""
    requested_length = int(mode["requested_length"]) if mode else 0
    requested_genre = str(mode["requested_genre"]) if mode else ""
    db.execute(
        "INSERT INTO library_previous_sessions "
        "(platform,chat_id,requester_id,text_id,title,difficulty,difficulty_score,segment_length,"
        "next_offset,segment_no,total_chars,requested_difficulty,requested_genre,requested_length,created_at,saved_at) "
        "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) "
        "ON CONFLICT(platform,chat_id,requester_id) DO UPDATE SET "
        "text_id=excluded.text_id,title=excluded.title,difficulty=excluded.difficulty,"
        "difficulty_score=excluded.difficulty_score,segment_length=excluded.segment_length,"
        "next_offset=excluded.next_offset,segment_no=excluded.segment_no,"
        "total_chars=excluded.total_chars,requested_difficulty=excluded.requested_difficulty,"
        "requested_genre=excluded.requested_genre,"
        "requested_length=excluded.requested_length,"
        "created_at=excluded.created_at,saved_at=excluded.saved_at",
        (
            platform, chat_id, requester_id, row["text_id"], row["title"],
            row["difficulty"], row["difficulty_score"], row["segment_length"],
            row["next_offset"], row["segment_no"], row["total_chars"],
            requested_difficulty, requested_genre, requested_length,
            row["created_at"], time.time(),
        ),
    )


def start_library_session(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, requester_name = library_payload_identity(payload)
    query = str(payload.get("query") or "").strip()
    difficulty = normalize_library_difficulty(payload.get("difficulty"))
    requested_genre = resolve_library_genre(payload.get("genre")) or resolve_library_genre(query)
    requested_length = payload.get("length")
    if requested_length in (None, "", 0, "0"):
        fixed_length = 0
        length = random.randint(LIBRARY_DEFAULT_MIN_LENGTH, LIBRARY_DEFAULT_MAX_LENGTH)
    else:
        length = int(requested_length)
        if not 1 <= length <= LIBRARY_MAX_LENGTH:
            raise ValueError(f"length must be between 1 and {LIBRARY_MAX_LENGTH}")
        fixed_length = length
    selected = pick_library_segment(
        db, query, difficulty, length, str(payload.get("exclude_text_id") or "").strip(), requested_genre
    )
    if not selected:
        detail = f"“{difficulty}”难度区间" if difficulty else "要求"
        return {"status": "not_found", "message": f"没有找到符合{detail}的文库文章。"}
    row, body, score, actual_difficulty = selected
    content = normalize_library_content(row["content"])
    metadata = classify_library_row(db, row)
    if not body:
        return {"status": "not_found", "message": "找到的文章没有可发送正文。"}
    save_previous_library_session(db, platform, chat_id, requester_id)
    end = len(body)
    now = time.time()
    session = {
        "session_id": library_session_id(platform, chat_id, requester_id),
        "platform": platform, "chat_id": chat_id,
        "requester_id": requester_id, "requester_name": requester_name,
        "text_id": row["text_id"], "title": row["title"],
        "difficulty": actual_difficulty, "difficulty_score": score,
        "segment_length": length, "next_offset": end, "segment_no": 1,
        "total_chars": len(content), "status": "active",
        "last_message_id": None, "created_at": now, "updated_at": now,
    }
    db.execute(
        "UPDATE single_sessions SET status='stopped',updated_at=? "
        "WHERE platform=? AND chat_id=? AND requester_id=? AND status='active'",
        (now, platform, chat_id, requester_id),
    )
    db.execute(
        "INSERT INTO library_sessions VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) "
        "ON CONFLICT(platform,chat_id,requester_id) DO UPDATE SET "
        "session_id=excluded.session_id,requester_name=excluded.requester_name,text_id=excluded.text_id,title=excluded.title,"
        "difficulty=excluded.difficulty,difficulty_score=excluded.difficulty_score,segment_length=excluded.segment_length,"
        "next_offset=excluded.next_offset,segment_no=excluded.segment_no,total_chars=excluded.total_chars,status=excluded.status,"
        "last_message_id=NULL,created_at=excluded.created_at,updated_at=excluded.updated_at",
        tuple(session.values()),
    )
    db.execute(
        "INSERT INTO library_session_modes (session_id,requested_difficulty,requested_length,requested_genre) "
        "VALUES (?, ?, ?, ?) ON CONFLICT(session_id) DO UPDATE SET "
        "requested_difficulty=excluded.requested_difficulty,requested_length=excluded.requested_length,"
        "requested_genre=excluded.requested_genre",
        (session["session_id"], difficulty, fixed_length, requested_genre),
    )
    db.commit()
    message, segment_id = format_library_segment(db, session, body, metadata)
    db.commit()
    return {
        "status": "segment", "message": message, "segment_id": segment_id,
        "session_id": session["session_id"], "title": row["title"],
        "difficulty": actual_difficulty, "score": score, "segment_no": 1,
        "next_offset": end, "total_chars": len(content), "completed": end >= len(content),
        "genre": metadata["primary_genre"], "genres": metadata["genres"],
        "form": metadata["form"], "article_difficulty": metadata["difficulty"],
        "article_score": metadata["difficulty_score"], "confidence": metadata["confidence"],
        "metadata_summary": library_metadata_summary(metadata, actual_difficulty, score),
    }


def continue_library_session(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, _ = library_payload_identity(payload)
    row = db.execute(
        "SELECT s.*,l.content FROM library_sessions s JOIN library_texts l ON l.text_id=s.text_id "
        "WHERE s.platform=? AND s.chat_id=? AND s.requester_id=? AND s.status='active'",
        (platform, chat_id, requester_id),
    ).fetchone()
    if not row:
        return {"status": "idle", "message": "你在当前会话里没有正在进行的发文。"}
    mode = db.execute(
        "SELECT requested_difficulty,requested_length,requested_genre FROM library_session_modes WHERE session_id=?",
        (row["session_id"],),
    ).fetchone()
    requested_difficulty = str(mode["requested_difficulty"]) if mode else str(row["difficulty"])
    requested_length = int(mode["requested_length"]) if mode else 0
    requested_genre = str(mode["requested_genre"]) if mode else ""
    return start_library_session(db, {
        "platform": platform,
        "chat_id": chat_id,
        "requester_id": requester_id,
        "requester_name": row["requester_name"],
        "difficulty": requested_difficulty,
        "length": requested_length,
        "genre": requested_genre,
        "exclude_text_id": row["text_id"],
    })


def continue_same_library_session(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, _ = library_payload_identity(payload)
    row = db.execute(
        "SELECT s.*,l.content FROM library_sessions s JOIN library_texts l ON l.text_id=s.text_id "
        "WHERE s.platform=? AND s.chat_id=? AND s.requester_id=? AND s.status='active'",
        (platform, chat_id, requester_id),
    ).fetchone()
    if not row:
        return {"status": "idle", "message": "你在当前会话里没有正在进行的发文。"}
    content = normalize_library_content(row["content"])
    start = int(row["next_offset"])
    mode = db.execute(
        "SELECT requested_difficulty,requested_genre FROM library_session_modes WHERE session_id=?",
        (row["session_id"],),
    ).fetchone()
    requested_difficulty = str(mode["requested_difficulty"]) if mode else str(row["difficulty"])
    requested_genre = str(mode["requested_genre"]) if mode else ""
    mode_name = "随机文来" if not requested_difficulty else f"{requested_difficulty}难度"
    if start >= len(content):
        return {
            "status": "article_completed",
            "message": f"《{row['title']}》已经发完了。要不要继续沿用刚才的“{mode_name}”模式，重新抽一篇？",
            "session_id": row["session_id"],
            "requested_difficulty": requested_difficulty,
            "requested_genre": requested_genre,
        }
    end = min(start + int(row["segment_length"]), len(content))
    body = content[start:end]
    score, actual_difficulty = get_library_ranker().rank(body)
    metadata = classify_library_row(db, row)
    session = dict(row)
    session["segment_no"] = int(row["segment_no"]) + 1
    session["next_offset"] = end
    session["total_chars"] = len(content)
    session["difficulty"] = actual_difficulty
    session["difficulty_score"] = score
    db.execute(
        "UPDATE library_sessions SET difficulty=?,difficulty_score=?,next_offset=?,segment_no=?,"
        "total_chars=?,updated_at=? WHERE session_id=?",
        (
            actual_difficulty, score, end, session["segment_no"], len(content),
            time.time(), row["session_id"],
        ),
    )
    message, segment_id = format_library_segment(db, session, body, metadata)
    db.commit()
    return {
        "status": "segment", "message": message, "segment_id": segment_id,
        "session_id": row["session_id"], "title": row["title"],
        "difficulty": actual_difficulty, "score": score,
        "segment_no": session["segment_no"], "next_offset": end,
        "total_chars": len(content), "completed": end >= len(content),
        "continuation_mode": "same_article",
        "genre": metadata["primary_genre"], "genres": metadata["genres"],
        "form": metadata["form"], "article_difficulty": metadata["difficulty"],
        "article_score": metadata["difficulty_score"], "confidence": metadata["confidence"],
        "metadata_summary": library_metadata_summary(metadata, actual_difficulty, score),
    }


def continue_previous_library_session(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, _ = library_payload_identity(payload)
    current = db.execute(
        "SELECT * FROM library_sessions WHERE platform=? AND chat_id=? "
        "AND requester_id=? AND status='active'",
        (platform, chat_id, requester_id),
    ).fetchone()
    previous = db.execute(
        "SELECT p.*,l.content FROM library_previous_sessions p "
        "JOIN library_texts l ON l.text_id=p.text_id "
        "WHERE p.platform=? AND p.chat_id=? AND p.requester_id=?",
        (platform, chat_id, requester_id),
    ).fetchone()
    if not current or not previous:
        return {
            "status": "no_previous",
            "message": "没有找到可以恢复的上一篇文章。",
        }
    content = normalize_library_content(previous["content"])
    if int(previous["next_offset"]) >= len(content):
        return {
            "status": "article_completed",
            "message": f"上一篇《{previous['title']}》已经发完了，当前新抽的文章先为你保留。",
            "session_id": current["session_id"],
            "requested_difficulty": previous["requested_difficulty"],
        }

    now = time.time()
    db.execute(
        "UPDATE library_sessions SET text_id=?,title=?,difficulty=?,difficulty_score=?,"
        "segment_length=?,next_offset=?,segment_no=?,total_chars=?,status='active',"
        "last_message_id=NULL,created_at=?,updated_at=? WHERE session_id=?",
        (
            previous["text_id"], previous["title"], previous["difficulty"],
            previous["difficulty_score"], previous["segment_length"],
            previous["next_offset"], previous["segment_no"], previous["total_chars"],
            previous["created_at"], now, current["session_id"],
        ),
    )
    db.execute(
        "INSERT INTO library_session_modes (session_id,requested_difficulty,requested_length,requested_genre) "
        "VALUES (?,?,?,?) ON CONFLICT(session_id) DO UPDATE SET "
        "requested_difficulty=excluded.requested_difficulty,requested_length=excluded.requested_length,"
        "requested_genre=excluded.requested_genre",
        (
            current["session_id"], previous["requested_difficulty"],
            previous["requested_length"], previous["requested_genre"],
        ),
    )
    db.execute(
        "DELETE FROM library_previous_sessions WHERE platform=? AND chat_id=? AND requester_id=?",
        (platform, chat_id, requester_id),
    )
    db.commit()
    result = continue_same_library_session(db, payload)
    result["continuation_mode"] = "previous_article"
    result["recall_message_id"] = current["last_message_id"]
    return result


def continue_library_session_from_score(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, requester_name = library_payload_identity(payload)
    parsed = parse_typing_score(str(payload.get("text") or ""))
    if not parsed or not str(parsed["segment_id"]).isdigit():
        return {"status": "ignored"}
    segment_id = int(parsed["segment_id"])
    row = db.execute(
        "SELECT m.session_id, 'article' AS session_kind FROM library_segment_sessions m "
        "JOIN library_sessions s ON s.session_id=m.session_id "
        "WHERE m.segment_id=? AND m.platform=? AND m.chat_id=? AND m.requester_id=? "
        "AND m.consumed_at IS NULL AND s.status='active' "
        "UNION ALL "
        "SELECT m.session_id, 'single' AS session_kind FROM library_segment_sessions m "
        "JOIN single_sessions s ON s.session_id=m.session_id "
        "WHERE m.segment_id=? AND m.platform=? AND m.chat_id=? AND m.requester_id=? "
        "AND m.consumed_at IS NULL AND s.status='active' "
        "UNION ALL "
        "SELECT m.session_id, 'contest' AS session_kind FROM library_segment_sessions m "
        "JOIN contest_sessions s ON s.session_id=m.session_id "
        "WHERE m.segment_id=? AND m.platform=? AND m.chat_id=? AND m.requester_id=? "
        "AND m.consumed_at IS NULL AND s.status='active'",
        (segment_id, platform, chat_id, requester_id,
         segment_id, platform, chat_id, requester_id,
         segment_id, platform, chat_id, requester_id),
    ).fetchone()
    # Single-character practice uses its own sequential segment number. It
    # intentionally does not consume the article six-digit segment registry.
    if not row:
        row = db.execute(
            "SELECT session_id, 'single' AS session_kind FROM single_sessions "
            "WHERE platform=? AND chat_id=? AND requester_id=? "
            "AND segment_no=? AND status='active'",
            (platform, chat_id, requester_id, segment_id),
        ).fetchone()
    if not row:
        return {"status": "ignored"}
    if row["session_kind"] in {"article", "contest"}:
        now = time.time()
        db.execute(
            "UPDATE library_segment_sessions SET consumed_at=? "
            "WHERE segment_id=? AND consumed_at IS NULL",
            (now, segment_id),
        )
        if not db.execute("SELECT changes()").fetchone()[0]:
            db.rollback()
            return {"status": "ignored"}
    identity = {
        "platform": platform,
        "chat_id": chat_id,
        "requester_id": requester_id,
        "requester_name": requester_name,
    }
    result = (
        continue_single_session(db, identity)
        if row["session_kind"] == "single"
        else continue_contest_session(db, identity)
        if row["session_kind"] == "contest"
        else continue_library_session(db, identity)
    )
    result["trigger_segment_id"] = segment_id
    return result


def single_session_id(platform: str, chat_id: str, requester_id: str) -> str:
    value = f"single:{platform}:{chat_id}:{requester_id}".encode("utf-8")
    return hashlib.sha256(value).hexdigest()[:32]


def format_single_segment(
    db: sqlite3.Connection, session: dict, body: str, end: int
) -> tuple[str, int]:
    total = int(session["total_chars"])
    length = int(session["segment_length"])
    segment_no = int(session["segment_no"])
    total_segments = max(1, (total + length - 1) // length)
    requirements = []
    if float(session["key_req"]) > 0:
        requirements.append(f"击{float(session['key_req']):g}")
    if float(session["acc_req"]) > 0:
        requirements.append(f"准{float(session['acc_req']):g}")
    requirement_text = f" {' '.join(requirements)}" if requirements else ""
    single_no = SINGLE_SET_NAMES.index(session["title"]) + 1
    segment_id = next_library_segment_id(db)
    now = time.time()
    return (
        f"[FM/单字·{session['title']}·{session['order_name']}{requirement_text}] "
        f"No.FM-S{single_no:03d}《{session['title']}》 [字数{len(body)}]\n"
        f"{body}\n"
        f"-----第{segment_no}段-FM发文｜{segment_no}/{total_segments}｜进度{end}/{total}字"
    ), segment_no


def start_single_session(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, requester_name = library_payload_identity(payload)
    name = str(payload.get("name") or "").strip()
    if name not in SINGLE_SET_NAMES:
        raise ValueError("unknown single-character set")
    order_name = str(payload.get("order") or "乱").strip()
    if order_name not in {"乱", "顺"}:
        raise ValueError("single-character order must be 乱 or 顺")
    requested_length = payload.get("length")
    length = SINGLE_DEFAULT_LENGTH if requested_length in (None, "", 0, "0") else int(requested_length)
    if not 1 <= length <= LIBRARY_MAX_LENGTH:
        raise ValueError(f"length must be between 1 and {LIBRARY_MAX_LENGTH}")
    row = db.execute(
        "SELECT * FROM library_texts WHERE category='fm_single_chars' AND title=? "
        "ORDER BY relative_path LIMIT 1",
        (name,),
    ).fetchone()
    if not row:
        return {"status": "not_found", "message": f"单字库《{name}》还没装好。"}
    sequence = re.sub(r"\s+", "", row["content"])
    if order_name == "乱":
        characters = list(sequence)
        random.shuffle(characters)
        sequence = "".join(characters)
    if not sequence:
        return {"status": "not_found", "message": f"单字库《{name}》没有可发送内容。"}
    end = min(length, len(sequence))
    now = time.time()
    session = {
        "session_id": single_session_id(platform, chat_id, requester_id),
        "platform": platform, "chat_id": chat_id,
        "requester_id": requester_id, "requester_name": requester_name,
        "text_id": row["text_id"], "title": name, "sequence": sequence,
        "order_name": order_name, "key_req": float(payload.get("key_req") or 0),
        "acc_req": float(payload.get("acc_req") or 0), "segment_length": length,
        "next_offset": end, "segment_no": 1, "total_chars": len(sequence),
        "status": "active", "last_message_id": None,
        "created_at": now, "updated_at": now,
    }
    db.execute(
        "UPDATE library_sessions SET status='stopped',updated_at=? "
        "WHERE platform=? AND chat_id=? AND requester_id=? AND status='active'",
        (now, platform, chat_id, requester_id),
    )
    db.execute(
        "INSERT INTO single_sessions VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) "
        "ON CONFLICT(platform,chat_id,requester_id) DO UPDATE SET "
        "session_id=excluded.session_id,requester_name=excluded.requester_name,text_id=excluded.text_id,title=excluded.title,"
        "sequence=excluded.sequence,order_name=excluded.order_name,key_req=excluded.key_req,acc_req=excluded.acc_req,"
        "segment_length=excluded.segment_length,next_offset=excluded.next_offset,segment_no=excluded.segment_no,"
        "total_chars=excluded.total_chars,status=excluded.status,last_message_id=NULL,"
        "created_at=excluded.created_at,updated_at=excluded.updated_at",
        tuple(session.values()),
    )
    db.commit()
    message, segment_id = format_single_segment(db, session, sequence[:end], end)
    db.commit()
    return {
        "status": "segment", "message": message, "segment_id": segment_id,
        "session_id": session["session_id"], "title": name, "segment_no": 1,
        "next_offset": end, "total_chars": len(sequence), "completed": end >= len(sequence),
    }


def continue_single_session(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, requester_name = library_payload_identity(payload)
    row = db.execute(
        "SELECT * FROM single_sessions WHERE platform=? AND chat_id=? "
        "AND requester_id=? AND status='active'",
        (platform, chat_id, requester_id),
    ).fetchone()
    if not row:
        return {"status": "idle", "message": "你在当前会话里没有正在进行的单字练习。"}
    start = int(row["next_offset"])
    sequence = str(row["sequence"])
    if start >= len(sequence):
        return {
            "status": "completed", "message": f"《{row['title']}》单字练习已经完成了。",
            "session_id": row["session_id"],
        }
    end = min(start + int(row["segment_length"]), len(sequence))
    session = dict(row)
    session["requester_name"] = requester_name or session["requester_name"]
    session["segment_no"] = int(row["segment_no"]) + 1
    session["next_offset"] = end
    db.execute(
        "UPDATE single_sessions SET next_offset=?,segment_no=?,updated_at=? WHERE session_id=?",
        (end, session["segment_no"], time.time(), row["session_id"]),
    )
    message, segment_id = format_single_segment(db, session, sequence[start:end], end)
    db.commit()
    return {
        "status": "segment", "message": message, "segment_id": segment_id,
        "session_id": row["session_id"], "title": row["title"],
        "segment_no": session["segment_no"], "next_offset": end,
        "total_chars": len(sequence), "completed": end >= len(sequence),
    }


def stop_library_session(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, _ = library_payload_identity(payload)
    owner = bool(payload.get("owner"))
    candidates = []
    for table in ("library_sessions", "single_sessions", "contest_sessions"):
        row = db.execute(
            f"SELECT * FROM {table} WHERE platform=? AND chat_id=? AND requester_id=? AND status='active'",
            (platform, chat_id, requester_id),
        ).fetchone()
        if row:
            candidates.append((row, table))
    if not candidates and owner:
        for table in ("library_sessions", "single_sessions", "contest_sessions"):
            row = db.execute(
                f"SELECT * FROM {table} WHERE platform=? AND chat_id=? AND status='active' "
                "ORDER BY updated_at DESC LIMIT 1",
                (platform, chat_id),
            ).fetchone()
            if row:
                candidates.append((row, table))
    if not candidates:
        return {"status": "idle", "message": "当前会话没有正在进行的发文。"}
    row, table = max(candidates, key=lambda item: float(item[0]["updated_at"]))
    db.execute(
        f"UPDATE {table} SET status='stopped',updated_at=? WHERE session_id=?",
        (time.time(), row["session_id"]),
    )
    db.commit()
    return {
        "status": "stopped", "message": "发文已停止。", "session_id": row["session_id"],
        "last_message_id": row["last_message_id"], "title": row["title"],
    }


def acknowledge_library_message(db: sqlite3.Connection, payload: dict) -> dict:
    session_id = str(payload.get("session_id") or "").strip()
    message_id = str(payload.get("message_id") or "").strip()
    if not session_id or not message_id:
        raise ValueError("session_id and message_id are required")
    changed = False
    now = time.time()
    for table in ("library_sessions", "single_sessions", "contest_sessions"):
        session = db.execute(
            f"SELECT platform,chat_id,requester_id FROM {table} WHERE session_id=?",
            (session_id,),
        ).fetchone()
        if not session:
            continue
        db.execute(
            f"UPDATE {table} SET last_message_id=?,updated_at=? WHERE session_id=?",
            (message_id, now, session_id),
        )
        changed = bool(db.execute("SELECT changes()").fetchone()[0]) or changed
        db.execute(
            "INSERT INTO library_sent_messages "
            "(session_id,platform,chat_id,requester_id,message_id,sent_at,recalled_at) "
            "VALUES (?,?,?,?,?,?,NULL) ON CONFLICT(message_id) DO UPDATE SET "
            "session_id=excluded.session_id,platform=excluded.platform,chat_id=excluded.chat_id,"
            "requester_id=excluded.requester_id,sent_at=excluded.sent_at,recalled_at=NULL",
            (
                session_id, session["platform"], session["chat_id"],
                session["requester_id"], message_id, now,
            ),
        )
    db.commit()
    return {"stored": changed, "session_id": session_id}


def recent_library_messages(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, _ = library_payload_identity(payload)
    count = max(1, min(int(payload.get("count") or 5), 20))
    cutoff = time.time() - 120
    rows = db.execute(
        "SELECT message_id,sent_at FROM library_sent_messages "
        "WHERE platform=? AND chat_id=? AND requester_id=? "
        "AND recalled_at IS NULL AND sent_at>=? ORDER BY sent_at DESC LIMIT ?",
        (platform, chat_id, requester_id, cutoff, count),
    ).fetchall()
    return {
        "status": "ok",
        "message_ids": [row["message_id"] for row in rows],
        "count": len(rows),
        "window_seconds": 120,
    }


def mark_library_messages_recalled(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, _ = library_payload_identity(payload)
    message_ids = payload.get("message_ids") or []
    if not isinstance(message_ids, list):
        raise ValueError("message_ids must be a list")
    normalized = [str(message_id).strip() for message_id in message_ids if str(message_id).strip()]
    changed = 0
    now = time.time()
    for message_id in normalized[:20]:
        db.execute(
            "UPDATE library_sent_messages SET recalled_at=? "
            "WHERE platform=? AND chat_id=? AND requester_id=? "
            "AND message_id=? AND recalled_at IS NULL",
            (now, platform, chat_id, requester_id, message_id),
        )
        changed += int(db.execute("SELECT changes()").fetchone()[0])
    db.commit()
    return {"status": "ok", "marked": changed}


def library_stats(db: sqlite3.Connection) -> dict:
    categories = {
        row["category"]: row["count"]
        for row in db.execute("SELECT category,COUNT(*) AS count FROM library_texts GROUP BY category")
    }
    difficulties = {
        row["difficulty"]: row["count"]
        for row in db.execute("SELECT difficulty,COUNT(*) AS count FROM library_rankings GROUP BY difficulty")
    }
    genres = {
        row["primary_genre"]: row["count"]
        for row in db.execute(
            "SELECT primary_genre,COUNT(*) AS count FROM library_classifications GROUP BY primary_genre ORDER BY count DESC"
        )
    }
    totals = db.execute("SELECT COUNT(*) AS texts,COALESCE(SUM(char_count),0) AS chars FROM library_texts").fetchone()
    eligible_total = db.execute(
        "SELECT COUNT(*) FROM library_texts WHERE category<>'fm_single_chars'"
    ).fetchone()[0]
    active = sum(
        db.execute(f"SELECT COUNT(*) FROM {table} WHERE status='active'").fetchone()[0]
        for table in ("library_sessions", "single_sessions")
    )
    ranked = sum(difficulties.values())
    classified = db.execute("SELECT COUNT(*) FROM library_classifications").fetchone()[0]
    return {
        "texts": totals["texts"], "characters": totals["chars"], "categories": categories,
        "ranked_texts": ranked, "unranked_texts": max(0, totals["texts"] - ranked),
        "difficulties": difficulties, "active_sessions": active,
        "classified_texts": classified,
        "unclassified_texts": max(0, eligible_total - classified),
        "genres": genres,
    }


def library_session_status(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, _ = library_payload_identity(payload)
    article = db.execute(
        "SELECT s.*,m.requested_difficulty,m.requested_length,m.requested_genre FROM library_sessions s "
        "LEFT JOIN library_session_modes m ON m.session_id=s.session_id "
        "WHERE s.platform=? AND s.chat_id=? AND s.requester_id=? AND s.status='active'",
        (platform, chat_id, requester_id),
    ).fetchone()
    single = db.execute(
        "SELECT * FROM single_sessions WHERE platform=? AND chat_id=? "
        "AND requester_id=? AND status='active'",
        (platform, chat_id, requester_id),
    ).fetchone()
    previous = db.execute(
        "SELECT title,text_id,next_offset,total_chars,segment_no,requested_difficulty,"
        "requested_genre,requested_length,saved_at FROM library_previous_sessions "
        "WHERE platform=? AND chat_id=? AND requester_id=?",
        (platform, chat_id, requester_id),
    ).fetchone()
    active = article or single
    if not active:
        return {
            "status": "idle", "platform": platform, "chat_id": chat_id,
            "requester_id": requester_id, "previous_available": bool(previous),
            "previous": dict(previous) if previous else None,
        }
    result = dict(active)
    result.update({
        "status": "active",
        "kind": "article" if article else "single_character",
        "progress": {
            "current": int(active["next_offset"]),
            "total": int(active["total_chars"]),
            "segment_no": int(active["segment_no"]),
        },
        "previous_available": bool(previous),
        "previous": dict(previous) if previous else None,
    })
    if article:
        requested_length = int(article["requested_length"] or 0)
        result["length_mode"] = "fixed" if requested_length else "random_200_400"
        result["requested_length"] = requested_length
        article_row = db.execute(
            "SELECT * FROM library_texts WHERE text_id=?", (article["text_id"],)
        ).fetchone()
        if article_row:
            metadata = classify_library_row(db, article_row)
            result.update({
                "genre": metadata["primary_genre"], "genres": metadata["genres"],
                "form": metadata["form"], "article_difficulty": metadata["difficulty"],
                "article_score": metadata["difficulty_score"], "confidence": metadata["confidence"],
                "metadata_summary": library_metadata_summary(metadata),
            })
            db.commit()
        article_row = db.execute(
            "SELECT * FROM library_texts WHERE text_id=?", (article["text_id"],)
        ).fetchone()
        if article_row:
            metadata = classify_library_row(db, article_row)
            result.update({
                "genre": metadata["primary_genre"], "genres": metadata["genres"],
                "form": metadata["form"], "article_difficulty": metadata["difficulty"],
                "article_score": metadata["difficulty_score"], "confidence": metadata["confidence"],
            })
    else:
        result["length_mode"] = "fixed"
        result["requested_length"] = int(single["segment_length"])
    return result


def archive_status(db: sqlite3.Connection) -> dict:
    sources = {
        "messages": ("recent_messages", "occurred_at"),
        "recalls": ("recall_records", "recalled_ts"),
        "typing_scores": ("score_records", "occurred_at"),
        "ai_contest_scores": ("ai_contest_scores", "occurred_at"),
        "competition_scores": ("competition_scores", "occurred_at"),
        "ai_contest_texts": ("ai_contest_texts", "generated_at"),
    }
    result = {}
    for name, (table, timestamp_column) in sources.items():
        row = db.execute(
            f"SELECT COUNT(*) AS count,MAX({timestamp_column}) AS latest_at FROM {table}"
        ).fetchone()
        result[name] = {"count": int(row["count"]), "latest_at": row["latest_at"]}
    return {
        "status": "ok",
        "checked_at": time.time(),
        "sources": result,
        "active_library_sessions": library_stats(db)["active_sessions"],
    }


def rank_library(db: sqlite3.Connection, limit: int = 0) -> dict:
    query = (
        "SELECT l.* FROM library_texts l LEFT JOIN library_rankings r ON r.text_id=l.text_id "
        "WHERE r.text_id IS NULL ORDER BY l.text_id"
    )
    params = ()
    if limit > 0:
        query += " LIMIT ?"
        params = (limit,)
    rows = db.execute(query, params).fetchall()
    counts = {difficulty: 0 for difficulty in LIBRARY_DIFFICULTIES}
    for index, row in enumerate(rows, 1):
        _, difficulty = rank_library_row(db, row)
        counts[difficulty] += 1
        if index % 200 == 0:
            db.commit()
    db.commit()
    classified = classify_library(db, limit)
    return {"ranked": len(rows), "difficulties": counts, "classified": classified, **library_stats(db)}


def classify_library(db: sqlite3.Connection, limit: int = 0) -> dict:
    """Backfill or refresh metadata for all non-single-character library texts."""
    query = (
        "SELECT l.* FROM library_texts l LEFT JOIN library_classifications c ON c.text_id=l.text_id "
        "WHERE l.category<>'fm_single_chars' AND "
        "(c.text_id IS NULL OR c.classifier_version<>?) ORDER BY l.text_id"
    )
    params: list = [LIBRARY_CLASSIFIER_VERSION]
    if limit > 0:
        query += " LIMIT ?"
        params.append(limit)
    rows = db.execute(query, tuple(params)).fetchall()
    genres = {}
    difficulties = {}
    for index, row in enumerate(rows, 1):
        metadata = classify_library_row(db, row, refresh=True)
        genres[metadata["primary_genre"]] = genres.get(metadata["primary_genre"], 0) + 1
        difficulties[metadata["difficulty"]] = difficulties.get(metadata["difficulty"], 0) + 1
        if index % 200 == 0:
            db.commit()
    db.commit()
    return {"classified": len(rows), "genres": genres, "difficulties": difficulties}


def load_json(path: Path, default):
    try:
        with path.open(encoding="utf-8") as source:
            return json.load(source)
    except FileNotFoundError:
        return default


def read_setting(db: sqlite3.Connection, key: str, default):
    row = db.execute("SELECT value_json FROM settings WHERE key=?", (key,)).fetchone()
    if not row:
        return default
    try:
        value = json.loads(row["value_json"])
    except (TypeError, json.JSONDecodeError):
        return default
    return value if isinstance(value, type(default)) else default


def write_setting(db: sqlite3.Connection, key: str, value) -> None:
    db.execute(
        "INSERT INTO settings VALUES (?, ?) ON CONFLICT(key) DO UPDATE SET value_json=excluded.value_json",
        (key, json.dumps(value, ensure_ascii=False)),
    )


def group_features(db: sqlite3.Connection, group_id: str) -> dict:
    row = db.execute("SELECT features_json FROM groups WHERE group_id=?", (group_id,)).fetchone()
    if not row:
        return {}
    try:
        value = json.loads(row["features_json"])
    except (TypeError, json.JSONDecodeError):
        return {}
    return value if isinstance(value, dict) else {}


def group_capability_enabled(db: sqlite3.Connection, group_id: str, capability: str) -> bool:
    features = group_features(db, group_id)
    value = features.get(capability)
    return True if value is None else bool(value)


def set_group_capability(db: sqlite3.Connection, group_id: str, capability: str, enabled: bool) -> dict:
    if capability not in GROUP_CAPABILITIES:
        raise ValueError("unknown capability")
    features = group_features(db, group_id)
    features[capability] = enabled
    updated_at = datetime.now(timezone.utc).isoformat()
    db.execute(
        "INSERT INTO groups VALUES (?, ?, ?, ?, ?, ?) "
        "ON CONFLICT(group_id) DO UPDATE SET features_json=excluded.features_json,updated_at=excluded.updated_at",
        (group_id, "", "observed", json.dumps(features, ensure_ascii=False), "{}", updated_at),
    )
    if capability == "repeat":
        state = read_setting(db, "repeat_follow", {})
        overrides = state.setdefault("group_overrides", {})
        if not isinstance(overrides, dict):
            overrides = {}
            state["group_overrides"] = overrides
        overrides[group_id] = enabled
        state["updated_at"] = time.time()
        write_setting(db, "repeat_follow", state)
    db.commit()
    return {"group_id": group_id, "capability": capability, "enabled": enabled, "features": features}


def repeat_group_enabled(state: dict, group_id: str) -> bool:
    if not bool(state.get("enabled", True)):
        return False
    overrides = state.get("group_overrides")
    if not isinstance(overrides, dict) or group_id not in overrides:
        return True
    return bool(overrides[group_id])


def normalize_repeat_text(value: str) -> str:
    value = unicodedata.normalize("NFKC", value or "").lower()
    value = re.sub(r"[\u200b\u200c\u200d\ufeff\s_]", "", value)
    return re.sub(r"[^\w\u4e00-\u9fff]", "", value)


def repeat_candidate(payload: dict) -> tuple[str, str] | None:
    text = str(payload.get("text") or "").strip()
    if not text or "\n" in text or len(text) > 40:
        return None
    if payload.get("has_media") or payload.get("mentions"):
        return None
    if re.search(r"^\s*(?:fm|fim|/|!|！|#|＃|\.|。|／)", text, re.IGNORECASE):
        return None
    if re.search(r"https?://|www\.|\.(?:com|cn|net|org)\b|\[CQ:", text, re.IGNORECASE):
        return None
    if re.search(r"速度|击键|键准|码长|回改|退格|用时|排名|成绩|字数|错字|-----第|进度\s*\d+/\d+|No\.FM", text, re.IGNORECASE):
        return None
    normalized = normalize_repeat_text(text)
    if not normalized or len(normalized) > 40:
        return None
    return text, normalized


def repeat_check(db: sqlite3.Connection, payload: dict) -> dict:
    group_id = str(payload.get("group_id") or "").strip()
    sender_id = canonical_user_id(payload.get("sender_id"))
    state = read_setting(db, "repeat_follow", {})
    capability_enabled = group_capability_enabled(db, group_id, "repeat") if group_id else False
    enabled = capability_enabled and repeat_group_enabled(state, group_id)
    if not group_id or not sender_id or not enabled:
        return {"repeat": False, "enabled": enabled}
    candidate = repeat_candidate(payload)
    if not candidate:
        return {"repeat": False, "enabled": True}
    text, normalized = candidate
    now = float(payload.get("occurred_at") or time.time())
    db.execute("DELETE FROM repeat_recent WHERE occurred_at < ?", (now - 20,))
    senders = {
        row["sender_id"]
        for row in db.execute(
            "SELECT DISTINCT sender_id FROM repeat_recent WHERE group_id=? AND normalized=? AND occurred_at>=?",
            (group_id, normalized, now - 10),
        )
    }
    senders.add(sender_id)
    db.execute(
        "INSERT INTO repeat_recent(group_id,sender_id,normalized,text,occurred_at) VALUES (?,?,?,?,?)",
        (group_id, sender_id, normalized, text, now),
    )
    row = db.execute(
        "SELECT repeated_at FROM repeat_cooldowns WHERE group_id=? AND normalized=?",
        (group_id, normalized),
    ).fetchone()
    cooldown_ready = row is None or now - float(row["repeated_at"]) >= 600
    should_repeat = len(senders) >= 2 and cooldown_ready
    if should_repeat:
        db.execute(
            "INSERT INTO repeat_cooldowns VALUES (?,?,?) ON CONFLICT(group_id,normalized) DO UPDATE SET repeated_at=excluded.repeated_at",
            (group_id, normalized, now),
        )
    db.execute("DELETE FROM repeat_cooldowns WHERE repeated_at < ?", (now - 1200,))
    db.commit()
    return {"repeat": should_repeat, "enabled": True, "text": text if should_repeat else ""}


def bot_guard_accounts(state: dict) -> dict:
    for key in ("bots", "loop_watch_accounts"):
        value = state.get(key)
        if isinstance(value, dict) and value:
            return value
    return {}


def bot_guard_check(db: sqlite3.Connection, payload: dict) -> dict:
    state = read_setting(db, "bot_guard", {})
    enabled = bool(state.get("enabled", True))
    sender_id = canonical_user_id(payload.get("sender_id"))
    group_id = str(payload.get("group_id") or "").strip()
    blocked = enabled and bool(sender_id) and sender_id in bot_guard_accounts(state)
    should_reply = False
    if blocked and bool(payload.get("explicit")):
        now = float(payload.get("occurred_at") or time.time())
        row = db.execute(
            "SELECT replied_at FROM bot_refusal_cooldowns WHERE group_id=? AND sender_id=?",
            (group_id, sender_id),
        ).fetchone()
        should_reply = not row or now - float(row["replied_at"]) >= 600
        if should_reply:
            db.execute(
                "INSERT INTO bot_refusal_cooldowns VALUES (?,?,?) ON CONFLICT(group_id,sender_id) DO UPDATE SET replied_at=excluded.replied_at",
                (group_id, sender_id, now),
            )
            db.commit()
    replies = [
        "你先停一下，FM 不接机器人互聊，免得群里绕成循环。",
        "这轮不跟机器人对话，再接下去就要开始无限套话了。",
        "识别到机器人消息了，FM 到这里收手，不陪你自动循环。",
        "机器人互聊先打住，这句我不往下接。",
    ]
    index = sum(ord(char) for char in f"{group_id}:{sender_id}") % len(replies)
    return {"blocked": blocked, "reply": replies[index] if should_reply else "", "enabled": enabled}


def update_bot_guard_account(db: sqlite3.Connection, payload: dict) -> dict:
    action = str(payload.get("action") or "list").strip().lower()
    state = read_setting(db, "bot_guard", {})
    accounts = dict(bot_guard_accounts(state))
    if action == "list":
        return {"enabled": bool(state.get("enabled", True)), "accounts": accounts}
    account_id = canonical_user_id(payload.get("account_id"))
    if not account_id:
        raise ValueError("account_id is required")
    if action in {"add", "set", "upsert"}:
        label = str(payload.get("label") or account_id).strip()[:120]
        accounts[account_id] = {"label": label, "updated_at": time.time()}
    elif action in {"remove", "delete"}:
        accounts.pop(account_id, None)
        db.execute("DELETE FROM bot_refusal_cooldowns WHERE sender_id=?", (account_id,))
    else:
        raise ValueError("action must be list, add, or remove")
    state["loop_watch_accounts"] = accounts
    state.pop("bots", None)
    state["updated_at"] = time.time()
    write_setting(db, "bot_guard", state)
    db.commit()
    return {"action": action, "account_id": account_id, "accounts": accounts}


def publish_ai_contest_text(db: sqlite3.Connection, payload: dict) -> dict:
    policy = read_setting(db, "ai_contest_policy", AI_CONTEST_POLICY_DEFAULTS)
    if not isinstance(policy, dict):
        policy = dict(AI_CONTEST_POLICY_DEFAULTS)
    min_chars = int(policy.get("min_chars", 200))
    max_chars = int(policy.get("max_chars", 300))
    china_time = timezone(timedelta(hours=8))
    competition_date = str(payload.get("date") or datetime.now(china_time).date().isoformat()).strip()
    try:
        datetime.strptime(competition_date, "%Y-%m-%d")
    except ValueError as exc:
        raise ValueError("date must use YYYY-MM-DD") from exc
    existing = db.execute(
        "SELECT * FROM ai_contest_texts WHERE competition_date=?", (competition_date,)
    ).fetchone()
    if existing and not bool(payload.get("replace")):
        return {"status": "existing", "text": dict(existing)}
    body = normalize_library_content(str(payload.get("body") or ""))
    if not min_chars <= len(body) <= max_chars:
        raise ValueError(f"AI contest body must contain {min_chars} to {max_chars} characters")
    title = str(payload.get("title") or "").strip()[:160]
    if not title:
        first_sentence = re.split(r"[。！？!?\n]", body, maxsplit=1)[0].strip()
        title = first_sentence[:32].strip() or f"AI赛文 {competition_date}"
    title_key = re.sub(r"[^0-9A-Za-z\u4e00-\u9fff]+", "", title).casefold()
    body_key = re.sub(r"\s+", "", body).casefold()
    previous = db.execute(
        "SELECT title,body FROM ai_contest_texts WHERE competition_date<>?",
        (competition_date,),
    ).fetchall()
    for previous_row in previous:
        previous_title = re.sub(
            r"[^0-9A-Za-z\u4e00-\u9fff]+", "", str(previous_row["title"] or "")
        ).casefold()
        previous_body = re.sub(r"\s+", "", str(previous_row["body"] or "")).casefold()
        if bool(policy.get("unique_topic", True)) and title_key and title_key == previous_title:
            raise ValueError(f"AI contest title already used: {title}")
        if bool(policy.get("unique_body", True)) and body_key and body_key == previous_body:
            raise ValueError("AI contest text must use a new topic")
    difficulty = str(payload.get("difficulty") or "普").strip()[:40]
    provider = str(payload.get("provider") or "cosmobot-agent").strip()[:80]
    source = {
        "date": competition_date, "title": title, "body": body,
        "difficulty": difficulty, "provider": provider,
        "generated_at": time.time(), "source": "cosmobot_live",
    }
    db.execute(
        "INSERT INTO ai_contest_texts VALUES (?, ?, ?, ?, ?, ?, ?) "
        "ON CONFLICT(competition_date) DO UPDATE SET title=excluded.title,body=excluded.body,"
        "difficulty=excluded.difficulty,provider=excluded.provider,generated_at=excluded.generated_at,source_json=excluded.source_json",
        (
            competition_date, title, body, difficulty, provider,
            source["generated_at"], json.dumps(source, ensure_ascii=False),
        ),
    )
    db.commit()
    return {"status": "published", "text": source}


def update_ai_contest_policy(db: sqlite3.Connection, payload: dict) -> dict:
    policy = read_setting(db, "ai_contest_policy", AI_CONTEST_POLICY_DEFAULTS)
    if not isinstance(policy, dict):
        policy = dict(AI_CONTEST_POLICY_DEFAULTS)
    fields = {
        "min_chars": int,
        "max_chars": int,
        "daily_refresh": bool,
        "unique_topic": bool,
        "unique_body": bool,
        "style": str,
    }
    for name, value_type in fields.items():
        if name not in payload or payload[name] is None:
            continue
        value = payload[name]
        if value_type is int:
            value = int(value)
            if not 1 <= value <= 5000:
                raise ValueError(f"{name} must be between 1 and 5000")
        elif value_type is bool:
            if not isinstance(value, bool):
                raise ValueError(f"{name} must be boolean")
        else:
            value = str(value).strip()[:160]
        policy[name] = value
    if int(policy.get("min_chars", 200)) > int(policy.get("max_chars", 300)):
        raise ValueError("min_chars cannot exceed max_chars")
    write_setting(db, "ai_contest_policy", policy)
    db.commit()
    return policy


def competition_score_summary(db: sqlite3.Connection, user_id: str = "", name: str = "", source: str = "") -> dict:
    rows = db.execute(
        "SELECT source_json FROM competition_scores WHERE user_id LIKE ? AND user_name LIKE ? "
        "AND source_group LIKE ? ORDER BY occurred_at DESC LIMIT 500",
        (f"%{user_id}%", f"%{name}%", f"%{source}%"),
    ).fetchall()
    records = [json.loads(row["source_json"]) for row in rows]
    # Keep summary cards consistent with charts.  Historical live-score
    # imports are exposed by competition_score_history when the classified
    # table has no matching rows, so do the same here instead of reporting
    # zero while the chart has usable legacy records.
    if not records:
        records = competition_score_history(db, user_id, name, source, days=0)
    def numeric(value):
        try:
            return float(str(value or 0).replace("%", "").strip())
        except ValueError:
            return 0.0

    speeds = [value for item in records if (value := numeric(item.get("speed"))) > 0]
    keys = [numeric(item.get("key") or item.get("keystroke")) for item in records]
    accuracies = [numeric(item.get("acc") or item.get("accuracy")) for item in records]
    sources = {}
    for item in records:
        source_name = str(item.get("source_group") or item.get("source") or "未知来源")
        sources[source_name] = sources.get(source_name, 0) + 1
    return {
        "query": {"user_id": user_id, "name": name, "source": source},
        "count": len(records),
        "best_speed": max(speeds, default=0),
        "average_speed": round(sum(speeds) / len(speeds), 2) if speeds else 0,
        "average_key": round(sum(keys) / len(keys), 2) if keys else 0,
        "average_accuracy": round(sum(accuracies) / len(accuracies), 2) if accuracies else 0,
        "sources": dict(sorted(sources.items(), key=lambda item: (-item[1], item[0]))),
        "recent": records[:20],
    }


def competition_score_history(
    db: sqlite3.Connection,
    user_id: str = "",
    name: str = "",
    source: str = "",
    days: int = 30,
) -> list[dict]:
    """Return the complete matching history for charting, filtered by calendar date."""
    rows = db.execute(
        "SELECT source_json FROM competition_scores "
        "WHERE user_id LIKE ? AND user_name LIKE ? AND source_group LIKE ? "
        "ORDER BY occurred_at ASC",
        (f"%{user_id}%", f"%{name}%", f"%{source}%"),
    ).fetchall()
    records = [json.loads(row["source_json"]) for row in rows]
    if not records:
        # Older live-score imports are kept in score_records. Map the known
        # contest room names into the same chart shape when the newer table
        # has no classified record for the requested contest.
        source_patterns = {
            "虎杯": "%虎码%",
            "锦标赛": "%剑气冲霄堂%",
        }
        group_pattern = source_patterns.get(source, source)
        if group_pattern:
            legacy_rows = db.execute(
                "SELECT source_json FROM score_records WHERE source_json LIKE ? "
                "ORDER BY occurred_at ASC",
                (f"%{group_pattern.strip('%')}%",),
            ).fetchall()
            for row in legacy_rows:
                item = json.loads(row["source_json"])
                item_id = canonical_user_id(item.get("sender_id") or item.get("user_id"))
                item_name = str(item.get("sender_name") or item.get("user_name") or "")
                if user_id and user_id not in item_id:
                    continue
                if name and name not in item_name:
                    continue
                item["user_id"] = item_id
                item["user_name"] = item_name
                item["source_group"] = source
                raw_date = item.get("competition_date") or item.get("date") or item.get("received_at", "")
                try:
                    if isinstance(raw_date, (int, float)) or str(raw_date).strip().isdigit():
                        raw_date = datetime.fromtimestamp(float(raw_date), timezone(timedelta(hours=8))).date().isoformat()
                except (TypeError, ValueError, OverflowError, OSError):
                    pass
                item["competition_date"] = str(raw_date)[:10]
                item["key"] = item.get("key") or item.get("keystrokes") or item.get("key_count")
                item["acc"] = item.get("acc") or item.get("accuracy")
                records.append(item)
    if days <= 0:
        return records
    cutoff = (datetime.now(timezone.utc) - timedelta(days=days - 1)).date().isoformat()
    return [
        item for item in records
        if str(item.get("competition_date") or item.get("date") or "") >= cutoff
    ]


def first_value(data, names, default=None):
    if not isinstance(data, dict):
        return default
    for name in names:
        if data.get(name) is not None:
            return data[name]
    return default


def stable_record_id(item: dict, names=None) -> str:
    names = names or ["record_id", "history_key", "score_message_id", "message_id", "id"]
    value = first_value(item, names)
    if value is not None and str(value).strip():
        return str(value).strip()
    payload = json.dumps(item, ensure_ascii=False, sort_keys=True)
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def canonical_user_id(value) -> str:
    text = str(value or "").strip()
    matrix_qq = re.match(r"^@qq_(\d+):", text, re.IGNORECASE)
    return matrix_qq.group(1) if matrix_qq else text


def number_after(text: str, labels) -> float:
    for label in labels:
        match = re.search(rf"{re.escape(label)}\s*([+-]?\d+(?:\.\d+)?)", text, re.IGNORECASE)
        if match:
            return float(match.group(1))
    return 0.0


def parse_typing_score(text: str):
    segment = re.search(r"第\s*([A-Za-z0-9_-]{1,24})\s*段", text)
    speed = number_after(text, ["速度", "速"])
    if not segment or speed <= 0:
        return None
    return {
        "segment_id": segment.group(1),
        "speed": speed,
        "keystroke": number_after(text, ["击键", "键速"]),
        "accuracy": number_after(text, ["键准", "准确率"]),
        "characters": int(number_after(text, ["字数", "字"])),
    }


def score_archive_groups(db: sqlite3.Connection) -> set[str]:
    groups: set[str] = set()

    # Keep the migrated score list for compatibility. Explicit per-group
    # settings take precedence so archive collection can be controlled safely.
    row = db.execute("SELECT value_json FROM settings WHERE key='score_state'").fetchone()
    if row:
        try:
            value = json.loads(row["value_json"])
        except (TypeError, json.JSONDecodeError):
            value = {}
        if isinstance(value, dict):
            groups.update(str(item) for item in value.get("groups", []) if str(item).strip())

    for row in db.execute("SELECT group_id, features_json FROM groups"):
        try:
            features = json.loads(row["features_json"] or "{}")
        except (TypeError, json.JSONDecodeError):
            features = {}
        if not isinstance(features, dict) or "score_archive" not in features:
            continue
        group_id = str(row["group_id"])
        if features["score_archive"] is True:
            groups.add(group_id)
        elif features["score_archive"] is False:
            groups.discard(group_id)
    return groups


def sender_names(payload: dict) -> tuple[str, str]:
    sender_name = str(payload.get("sender_name") or "").strip()
    group_name = str(payload.get("group_name") or "").strip()
    raw = payload.get("raw") if isinstance(payload.get("raw"), dict) else {}
    sender = raw.get("sender") if isinstance(raw.get("sender"), dict) else {}
    if not sender_name:
        sender_name = str(sender.get("card") or sender.get("nickname") or "").strip()
    if not group_name:
        group_name = str(raw.get("group_name") or "").strip()
    return sender_name[:120], group_name[:120]


def load_jsonl(path: Path):
    if not path.exists():
        return
    with path.open(encoding="utf-8") as source:
        for line in source:
            try:
                item = json.loads(line)
            except json.JSONDecodeError:
                continue
            if isinstance(item, dict):
                yield item


def import_snapshot(db: sqlite3.Connection, snapshot: Path) -> dict:
    data = snapshot / "plugin_data"
    group_registry = load_json(data / "astrbot_plugin_fm_group_registry" / "group_registry.json", {})
    imported_groups = 0
    for group_id, group in group_registry.get("groups", {}).items():
        db.execute(
            "INSERT INTO groups VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT(group_id) DO UPDATE SET display_name=excluded.display_name,status=excluded.status,features_json=excluded.features_json,source_json=excluded.source_json,updated_at=excluded.updated_at",
            (str(group_id), group.get("display_name") or "", group.get("status") or "observed",
             json.dumps(group.get("features", {}), ensure_ascii=False), json.dumps(group, ensure_ascii=False),
             str(group.get("last_seen_at") or "")),
        )
        imported_groups += 1

    for name, path in {
        "global_switch": data / "astrbot_plugin_fm_global_switch" / "fm_global_switch.json",
        "repeat_follow": data / "astrbot_plugin_fm_repeat_follow" / "fm_repeat_follow.json",
        "bot_guard": data / "astrbot_plugin_fm_bot_guard" / "fm_bot_guard.json",
        "score_state": data / "astrbot_plugin_fm_group_score_archive" / "fm_group_score_archive_state.json",
    }.items():
        db.execute("INSERT INTO settings VALUES (?, ?) ON CONFLICT(key) DO UPDATE SET value_json=excluded.value_json", (name, json.dumps(load_json(path, {}), ensure_ascii=False)))

    imported_library = 0
    for folder in sorted((snapshot / "library").glob("*")):
        if not folder.is_dir():
            continue
        for text_file in folder.rglob("*.txt"):
            raw_content = text_file.read_text(encoding="utf-8", errors="replace").strip()
            title = text_file.stem
            if folder.name == "fm_single_chars":
                content = raw_content
                accepted = bool(content)
            else:
                content, _ = clean_library_content(title, raw_content)
                accepted, _ = library_content_quality(title, content, folder.name)
            if not accepted:
                continue
            relative = str(text_file.relative_to(snapshot / "library")).replace("\\", "/")
            text_id = hashlib.sha256(relative.encode("utf-8")).hexdigest()[:24]
            db.execute("INSERT INTO library_texts VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT(text_id) DO UPDATE SET content=excluded.content,char_count=excluded.char_count,title=excluded.title",
                       (text_id, folder.name, relative, title, content, len(normalize_library_content(content))))
            imported_library += 1

    recalls = load_json(data / "astrbot_plugin_fm_recall_viewer" / "fm_recall_viewer.json", {}).get("recall_records", [])
    for item in recalls:
        message_id = str(item.get("message_id") or hashlib.sha256(json.dumps(item, sort_keys=True).encode()).hexdigest())
        imported_images = item.get("image_urls")
        if not isinstance(imported_images, list):
            imported_images = []
        db.execute("INSERT INTO recall_records (message_id,group_id,group_name,sender_id,sender_name,text,recalled_ts,image_urls_json,source_json) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(message_id) DO UPDATE SET source_json=excluded.source_json,recalled_ts=excluded.recalled_ts,image_urls_json=excluded.image_urls_json",
                   (message_id, str(item.get("group_id") or ""), item.get("group_name") or "", str(item.get("sender_id") or ""), item.get("sender_name") or "", item.get("text") or "", item.get("recalled_ts"), json.dumps(imported_images, ensure_ascii=False), json.dumps(item, ensure_ascii=False)))

    score_file = data / "astrbot_plugin_fm_group_score_archive" / "fm_group_score_records.jsonl"
    imported_scores = 0
    if score_file.exists():
        db.execute("DELETE FROM score_records")
        with score_file.open(encoding="utf-8") as source:
            for line in source:
                try:
                    item = json.loads(line)
                except json.JSONDecodeError:
                    continue
                payload = json.dumps(item, ensure_ascii=False, sort_keys=True)
                record_id_value = first_value(item, ["record_id", "id", "archive_key", "message_id"])
                record_id = str(record_id_value).strip() if record_id_value is not None else ""
                if not record_id:
                    record_id = hashlib.sha256(payload.encode()).hexdigest()
                db.execute("INSERT INTO score_records VALUES (?, ?, ?, ?, ?) ON CONFLICT(record_id) DO UPDATE SET group_id=excluded.group_id,sender_id=excluded.sender_id,occurred_at=excluded.occurred_at,source_json=excluded.source_json",
                           (record_id, str(first_value(item, ["group_id", "group"] , "")), str(first_value(item, ["sender_id", "user_id", "qq"], "")), first_value(item, ["ts", "timestamp", "created_at", "archived_at", "received_at"], 0), payload))
                imported_scores += 1
    db.commit()
    return {"groups": imported_groups, "library_texts": imported_library, "recalls": len(recalls), "scores": imported_scores}


def import_retained_snapshot(db: sqlite3.Connection, snapshot: Path) -> dict:
    contest_root = snapshot / "contest_library"
    imported_contest_texts = 0
    if contest_root.exists():
        db.execute("DELETE FROM contest_texts")
        for text_file in contest_root.rglob("*.txt"):
            content = text_file.read_text(encoding="utf-8", errors="replace").strip()
            if not content:
                continue
            relative = str(text_file.relative_to(contest_root)).replace("\\", "/")
            parts = text_file.relative_to(contest_root).parts
            source_group = parts[-2] if len(parts) > 1 else "contest library"
            date_match = re.search(r"20\d{2}-\d{2}-\d{2}", text_file.name)
            competition_date = date_match.group(0) if date_match else ""
            title = text_file.stem
            title = re.sub(r"^20\d{2}-\d{2}-\d{2}[_ -]*", "", title).strip() or text_file.stem
            text_id = hashlib.sha256(relative.encode("utf-8")).hexdigest()[:24]
            db.execute(
                "INSERT INTO contest_texts VALUES (?, ?, ?, ?, ?, ?, ?)",
                (text_id, title, source_group, competition_date, relative, content, len(content)),
            )
            imported_contest_texts += 1

    outputpro = snapshot / "plugin_data" / "astrbot_plugin_outputpro"
    daily = load_json(outputpro / "fm_ai_competition_daily.json", {})
    imported_ai_texts = 0
    if isinstance(daily, dict) and daily.get("date") and daily.get("body"):
        db.execute(
            "INSERT INTO ai_contest_texts VALUES (?, ?, ?, ?, ?, ?, ?) "
            "ON CONFLICT(competition_date) DO UPDATE SET title=excluded.title,body=excluded.body,difficulty=excluded.difficulty,provider=excluded.provider,generated_at=excluded.generated_at,source_json=excluded.source_json",
            (
                str(daily.get("date")), str(daily.get("title") or "AI contest"),
                str(daily.get("body")), str(daily.get("difficulty") or ""),
                str(daily.get("provider") or ""), float(daily.get("generated_at") or 0),
                json.dumps(daily, ensure_ascii=False),
            ),
        )
        imported_ai_texts = 1

    db.execute("DELETE FROM ai_contest_scores")
    imported_ai_scores = 0
    for item in load_jsonl(outputpro / "fm_ai_competition_history.jsonl") or ():
        record_id = stable_record_id(item)
        db.execute(
            "INSERT INTO ai_contest_scores VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) "
            "ON CONFLICT(record_id) DO UPDATE SET source_json=excluded.source_json",
            (
                record_id, str(item.get("competition_date") or ""), canonical_user_id(item.get("user_id")),
                str(item.get("user_name") or ""), str(item.get("group_id") or ""),
                float(item.get("speed") or 0), float(item.get("key") or 0),
                float(item.get("acc") or 0), float(item.get("ts") or 0),
                json.dumps(item, ensure_ascii=False),
            ),
        )
        imported_ai_scores += 1

    competition_data = load_json(outputpro / "fm_competition_scores.json", {})
    records = competition_data.get("records", []) if isinstance(competition_data, dict) else []
    db.execute("DELETE FROM competition_scores")
    imported_competition_scores = 0
    for item in records:
        if not isinstance(item, dict):
            continue
        db.execute(
            "INSERT INTO competition_scores VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) "
            "ON CONFLICT(record_id) DO UPDATE SET source_json=excluded.source_json",
            (
                stable_record_id(item), str(item.get("competition_date") or ""),
                canonical_user_id(item.get("user_id")), str(item.get("user_name") or ""),
                str(item.get("group_id") or ""), str(item.get("source_group") or ""),
                float(item.get("speed") or 0), float(item.get("ts") or 0),
                json.dumps(item, ensure_ascii=False),
            ),
        )
        imported_competition_scores += 1
    db.commit()
    return {
        "contest_texts": imported_contest_texts,
        "ai_contest_texts": imported_ai_texts,
        "ai_contest_scores": imported_ai_scores,
        "competition_scores": imported_competition_scores,
    }


def render_ai_leaderboard(db: sqlite3.Connection, competition_date: str) -> bytes:
    from PIL import Image, ImageDraw, ImageFont

    if not competition_date:
        competition_date = public_competition_date()
    score_rows = db.execute(
        "SELECT user_id,source_json FROM ai_contest_scores WHERE competition_date=? AND speed>0 ORDER BY speed DESC,occurred_at ASC",
        (competition_date,),
    ).fetchall()
    best = {}
    for row in score_rows:
        user_id = str(row["user_id"] or "")
        if user_id and user_id not in best:
            best[user_id] = json.loads(row["source_json"])
    rows = list(best.values())
    text_row = db.execute(
        "SELECT title FROM ai_contest_texts WHERE competition_date=?", (competition_date,)
    ).fetchone()
    title = str(text_row["title"] if text_row else "").strip()
    if not title:
        # Scores can arrive before the daily-text record is persisted. Recover
        # the title from the raw score so the leaderboard remains identifiable.
        ignored_titles = {"首打认证", "词提开"}
        for item in rows:
            raw = str(item.get("raw") or "")
            match = re.search(r"【([^】]{2,80})】", raw)
            if match and match.group(1).strip() not in ignored_titles:
                title = match.group(1).strip()
                break
    title = title or "当日赛文"

    def speed_color(value):
        if value < 100:
            return (145, 164, 190)
        if value < 130:
            return (100, 181, 246)
        if value < 150:
            return (91, 219, 194)
        if value < 180:
            return (247, 199, 95)
        if value < 200:
            return (255, 154, 102)
        return (239, 116, 166)

    def accuracy_color(value):
        return (246, 116, 130) if value < 95 else (91, 219, 194)

    font_path = os.environ.get("FM_REPORT_FONT", "/assets/msyh.ttc")
    font = lambda size: ImageFont.truetype(font_path, size)
    width = 1080
    row_height = 74
    height = 230 + max(1, len(rows)) * row_height + 70
    image = Image.new("RGB", (width, height), (20, 24, 31))
    draw = ImageDraw.Draw(image)
    draw.rectangle((0, 0, width, 12), fill=(78, 205, 181))
    draw.text((48, 38), "FM AI赛文排行榜", font=font(40), fill=(244, 247, 252))
    draw.text((48, 98), f"{competition_date or '日期未知'}  |  《{title}》  |  第555段", font=font(22), fill=(159, 172, 193))
    columns = [(52, "排名"), (146, "成员"), (560, "速度"), (710, "击键"), (842, "键准")]
    draw.rounded_rectangle((40, 154, 1040, 210), radius=8, fill=(35, 43, 55))
    for x, label in columns:
        draw.text((x, 168), label, font=font(20), fill=(184, 196, 214))
    if not rows:
        draw.text((52, 240), "当天还没有保存的成绩。", font=font(24), fill=(184, 196, 214))
    for index, item in enumerate(rows, 1):
        top = 224 + (index - 1) * row_height
        fill = (29, 36, 47) if index % 2 else (25, 31, 41)
        draw.rounded_rectangle((40, top, 1040, top + 62), radius=7, fill=fill)
        rank_color = (247, 199, 95) if index <= 3 else (219, 226, 237)
        name = str(item.get("user_name") or item.get("user_id") or "unknown")[:24]
        draw.text((58, top + 17), str(index), font=font(22), fill=rank_color)
        draw.text((146, top + 17), name, font=font(22), fill=(235, 239, 245))
        speed = float(item.get("speed") or 0)
        accuracy = float(item.get("acc") or 0)
        draw.text((560, top + 17), f"{speed:.2f}", font=font(22), fill=speed_color(speed))
        draw.text((710, top + 17), f"{float(item.get('key') or 0):.2f}", font=font(22), fill=(113, 164, 255))
        draw.text((842, top + 17), f"{accuracy:.2f}%", font=font(22), fill=accuracy_color(accuracy))
    footer = f"共 {len(rows)} 人 | 每人保留当日最高速度"
    draw.text((48, height - 48), footer, font=font(18), fill=(128, 141, 160))
    output = io.BytesIO()
    image.save(output, "PNG", optimize=True)
    return output.getvalue()


def render_competition_score(db: sqlite3.Connection, user_id: str, name: str, source: str) -> bytes:
    from PIL import Image, ImageDraw, ImageFont

    summary = competition_score_summary(db, user_id, name, source)
    recent = summary["recent"][:12]
    display_name = name or user_id
    if recent:
        display_name = str(recent[0].get("user_name") or recent[0].get("name") or display_name)
    display_name = display_name or "未指定成员"
    font_path = os.environ.get("FM_REPORT_FONT", "/assets/msyh.ttc")
    font = lambda size: ImageFont.truetype(font_path, size)
    width = 1080
    height = 420 + max(1, len(recent)) * 62
    image = Image.new("RGB", (width, height), (20, 24, 31))
    draw = ImageDraw.Draw(image)
    draw.rectangle((0, 0, width, 12), fill=(78, 205, 181))
    draw.text((48, 38), "FM 赛文成绩", font=font(40), fill=(244, 247, 252))
    draw.text((48, 98), display_name[:28], font=font(26), fill=(159, 172, 193))
    metrics = [
        ("记录", str(summary["count"])),
        ("最佳速度", f"{summary['best_speed']:.2f}"),
        ("平均速度", f"{summary['average_speed']:.2f}"),
        ("平均键准", f"{summary['average_accuracy']:.2f}%"),
    ]
    for index, (label, value) in enumerate(metrics):
        left = 40 + index * 255
        draw.rounded_rectangle((left, 150, left + 230, 250), radius=8, fill=(34, 42, 54))
        draw.text((left + 18, 168), label, font=font(18), fill=(147, 160, 181))
        draw.text((left + 18, 204), value, font=font(26), fill=(235, 239, 245))
    draw.text((48, 286), "最近成绩", font=font(24), fill=(235, 239, 245))
    columns = [(50, "日期"), (245, "来源"), (610, "速度"), (750, "击键"), (884, "键准")]
    draw.rounded_rectangle((40, 326, 1040, 378), radius=7, fill=(35, 43, 55))
    for x, label in columns:
        draw.text((x, 341), label, font=font(18), fill=(184, 196, 214))
    if not recent:
        draw.text((52, 402), "没有找到对应的赛文成绩。", font=font(22), fill=(184, 196, 214))
    for index, item in enumerate(recent):
        top = 390 + index * 62
        draw.rounded_rectangle((40, top, 1040, top + 52), radius=6, fill=(29, 36, 47) if index % 2 == 0 else (25, 31, 41))
        date = str(item.get("competition_date") or item.get("date") or "-")[:16]
        source_name = str(item.get("source_group") or item.get("source") or "未知")[:18]
        speed = float(item.get("speed") or 0)
        key = float(item.get("key") or item.get("keystroke") or 0)
        accuracy = float(item.get("acc") or item.get("accuracy") or 0)
        values = [date, source_name, f"{speed:.2f}", f"{key:.2f}", f"{accuracy:.2f}%"]
        for (x, _), value in zip(columns, values):
            draw.text((x, top + 14), value, font=font(18), fill=(229, 235, 244))
    output = io.BytesIO()
    image.save(output, "PNG", optimize=True)
    return output.getvalue()


def render_score_chart(db: sqlite3.Connection, user_id: str, name: str, source: str, days: int, metric: str) -> bytes:
    """Render a deterministic, readable score chart instead of model-generated artwork."""
    from PIL import Image, ImageDraw, ImageFont

    summary = competition_score_summary(db, user_id, name, source)
    records = competition_score_history(db, user_id, name, source, days)
    metric_key = {"accuracy": "acc", "keystrokes": "key", "key": "key", "speed": "speed"}.get(metric.lower(), "speed")
    metric_name = {"speed": "速度（字/分）", "acc": "键准（%）", "key": "击键"}[metric_key]
    display_name = name or user_id or "未指定成员"
    if records:
        display_name = str(records[-1].get("user_name") or records[-1].get("name") or display_name)
    source_name = source or "全部赛事"
    font_path = os.environ.get("FM_REPORT_FONT", "/assets/msyh.ttc")

    def font(size):
        try:
            return ImageFont.truetype(font_path, size)
        except OSError:
            return ImageFont.load_default()

    def number(item):
        try:
            return float(str(item.get(metric_key) or item.get({"acc": "accuracy", "key": "keystroke"}.get(metric_key, "")) or 0).replace("%", ""))
        except (TypeError, ValueError):
            return 0.0

    detail_records = list(reversed(records))
    row_height = 31
    width = 1600
    height = max(1040, 790 + max(1, len(detail_records)) * row_height + 100)
    image = Image.new("RGB", (width, height), (246, 248, 251))
    draw = ImageDraw.Draw(image)
    navy, muted, grid, teal = (25, 39, 61), (91, 105, 121), (218, 225, 233), (23, 143, 137)
    draw.rectangle((0, 0, width, 12), fill=teal)
    draw.text((72, 46), f"{display_name} · {source_name}成绩趋势", font=font(42), fill=navy)
    draw.text((74, 108), f"指标：{metric_name}    时间范围：最近 {days} 天    记录：{len(records)} 条", font=font(23), fill=muted)
    if not records:
        draw.rounded_rectangle((72, 190, width - 72, 440), radius=12, fill=(255, 255, 255), outline=grid, width=2)
        draw.text((width // 2, 305), "没有找到符合条件的成绩记录", font=font(30), fill=muted, anchor="mm")
    else:
        chart = (104, 190, 1496, 610)
        left, top, right, bottom = chart
        draw.rounded_rectangle(chart, radius=12, fill=(255, 255, 255), outline=grid, width=2)
        values = [number(item) for item in records]
        low, high = min(values), max(values)
        span = max(high - low, max(abs(high), 1) * 0.12, 1)
        low -= span * 0.12
        high += span * 0.12
        for step in range(5):
            y = bottom - 54 - step * ((bottom - top - 90) / 4)
            draw.line((left + 78, y, right - 32, y), fill=grid, width=1)
            label = f"{low + (high - low) * step / 4:.1f}"
            draw.text((left + 18, y), label, font=font(17), fill=muted, anchor="lm")
        plot_left, plot_right = left + 100, right - 42
        plot_top, plot_bottom = top + 34, bottom - 54
        points = []
        for index, value in enumerate(values):
            x = plot_left if len(values) == 1 else plot_left + index * (plot_right - plot_left) / (len(values) - 1)
            y = plot_bottom - (value - low) / (high - low) * (plot_bottom - plot_top)
            points.append((x, y))
        if len(points) > 1:
            draw.line(points, fill=teal, width=5, joint="curve")
        # Keep the exact value beside every point, as requested. The complete
        # table below remains available for histories whose labels overlap.
        show_point_labels = True
        for index, ((x, y), item, value) in enumerate(zip(points, records, values)):
            accuracy = float(str(item.get("acc") or item.get("accuracy") or 0).replace("%", "") or 0)
            color = (35, 163, 108) if accuracy >= 95 else (224, 105, 71)
            draw.ellipse((x - 9, y - 9, x + 9, y + 9), fill=color, outline=(255, 255, 255), width=3)
            if show_point_labels:
                draw.text((x, y - 24), f"{value:.2f}", font=font(17), fill=navy, anchor="ms")
        date_labels = [str(item.get("competition_date") or item.get("date") or "-")[-5:] for item in records]
        label_indexes = range(len(date_labels)) if len(date_labels) <= 12 else sorted({0, len(date_labels) - 1, *range(0, len(date_labels), max(1, len(date_labels) // 10))})
        for index in label_indexes:
            label = date_labels[index]
            x = plot_left if len(date_labels) == 1 else plot_left + index * (plot_right - plot_left) / (len(date_labels) - 1)
            draw.text((x, bottom - 28), label, font=font(16), fill=muted, anchor="ms")
        draw.text((right - 250, top + 25), "● 键准≥95%", font=font(17), fill=(35, 163, 108))
        draw.text((right - 250, top + 51), "● 键准<95%", font=font(17), fill=(224, 105, 71))

    draw.text((72, 660), "明细记录", font=font(28), fill=navy)
    columns = [(76, "日期"), (300, "赛事"), (810, metric_name), (1050, "键准"), (1250, "击键")]
    draw.rounded_rectangle((72, 710, width - 72, 765), radius=8, fill=(31, 55, 80))
    for x, label in columns:
        draw.text((x, 727), label, font=font(19), fill=(255, 255, 255))
    for row_index, item in enumerate(detail_records):
        y = 778 + row_index * 31
        if row_index % 2 == 0:
            draw.rectangle((72, y - 4, width - 72, y + 27), fill=(255, 255, 255))
        values_text = [
            str(item.get("competition_date") or item.get("date") or "-")[:10],
            str(item.get("source_group") or item.get("source") or "未知")[:34],
            f"{number(item):.2f}",
            f"{float(str(item.get('acc') or item.get('accuracy') or 0).replace('%', '') or 0):.2f}%",
            f"{float(item.get('key') or item.get('keystroke') or 0):.2f}",
        ]
        for (x, _), value in zip(columns, values_text):
            draw.text((x, y), value, font=font(17), fill=navy)
    draw.text((72, height - 48), "数据来源：FM 成绩归档    绿色表示键准≥95%，橙色表示键准<95%", font=font(17), fill=muted)
    output = io.BytesIO()
    image.save(output, "PNG", optimize=True)
    return output.getvalue()


def public_competition_response(result: dict, include_rows=True) -> dict:
    response = {
        "status": "ok" if result["row_count"] else "no_data",
        "source": result["source"], "kind": result["kind"], "group_id": result.get("group_id", ""),
        "date": result["date"], "title": result.get("title", ""),
        "word_number": result.get("word_number", 0), "content": result.get("content", ""),
        "row_count": result["row_count"], "page_count": result["page_count"],
    }
    if include_rows:
        response["rows"] = result["rows"]
    if not result["row_count"]:
        response["message"] = "公开赛事网站当前没有返回这一天的排行榜数据。"
    return response


def render_public_competition_rank(result: dict, page=1, combined=False) -> bytes:
    from PIL import Image, ImageDraw, ImageFont

    page_count = result["page_count"]
    all_rows = sorted(result["rows"], key=lambda row: row.get("rank") or 999999)
    if combined:
        page = None
        page_rows = all_rows
        row_offset = 0
    else:
        page = min(max(int(page), 1), page_count)
        row_offset = (page - 1) * 20
        page_rows = all_rows[row_offset:row_offset + 20]
    font_path = os.environ.get("FM_REPORT_FONT", "/assets/msyh.ttc")

    def font(size):
        try:
            return ImageFont.truetype(font_path, size)
        except OSError:
            return ImageFont.load_default()

    width = 1200
    height = 294 + max(1, len(page_rows)) * 66
    image = Image.new("RGB", (width, height), (18, 23, 31))
    draw = ImageDraw.Draw(image)
    draw.rectangle((0, 0, width, 12), fill=(65, 179, 171))

    def fit(value, selected_font, max_width):
        value = str(value or "")
        if draw.textlength(value, font=selected_font) <= max_width:
            return value
        while value and draw.textlength(value + "...", font=selected_font) > max_width:
            value = value[:-1]
        return value + "..." if value else ""

    title_font, sub_font = font(40), font(21)
    head_font, row_font, small_font = font(20), font(21), font(17)
    draw.text((52, 38), fit(f"FM {result['source']}排行榜", title_font, 800), fill=(244, 248, 255), font=title_font)
    subtitle = result.get("title") or "实时公开赛事数据"
    draw.text((54, 96), fit(f"{result['date']}  |  {subtitle}", sub_font, 900), fill=(158, 170, 190), font=sub_font)
    if page is not None:
        draw.rounded_rectangle((950, 46, 1148, 102), radius=14, fill=(35, 48, 65), outline=(70, 92, 116))
        draw.text((1049, 74), f"{page}/{page_count}", fill=(126, 220, 213), font=font(22), anchor="mm")

    columns = [
        ("排名", 54, 72), ("用户名", 146, 306), ("速度", 466, 88), ("击键", 570, 84),
        ("码长", 672, 84), ("回改", 774, 84), ("错字", 876, 70), ("键准", 960, 88),
        ("输入法", 1060, 86),
    ]
    draw.rounded_rectangle((42, 150, 1158, 202), radius=12, fill=(35, 46, 63), outline=(62, 82, 105))
    for label, x, _ in columns:
        draw.text((x, 164), label, fill=(178, 190, 210), font=head_font)
    y = 214
    if not page_rows:
        draw.text((54, y + 16), "这一天暂时没有排行榜数据。", fill=(232, 237, 245), font=font(26))
    for index, row in enumerate(page_rows):
        rank = int(row.get("rank") or row_offset + index + 1)
        fill_color = (29, 38, 52) if index % 2 == 0 else (24, 32, 44)
        draw.rounded_rectangle((42, y, 1158, y + 58), radius=10, fill=fill_color, outline=(55, 72, 94))
        values = [
            str(rank), fit(row.get("name"), row_font, 300), f"{public_number(row.get('speed')):.2f}",
            f"{public_number(row.get('key')):.2f}", f"{public_number(row.get('code')):.2f}",
            str(row.get("back") or "0"), str(row.get("wrong") or "0"),
            f"{public_number(row.get('acc')):.2f}%", fit(row.get("ime"), small_font, 86),
        ]
        for (label, x, _), value in zip(columns, values):
            selected_font = small_font if label == "输入法" else row_font
            color = (255, 202, 93) if label == "排名" and rank <= 3 else (232, 237, 245)
            draw.text((x, y + 17), value, fill=color, font=selected_font)
        y += 66
    output = io.BytesIO()
    image.save(output, "PNG", optimize=True)
    return output.getvalue()


class Api(BaseHTTPRequestHandler):
    db_path = ""

    def json(self, value, status=HTTPStatus.OK):
        body = json.dumps(value, ensure_ascii=False).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def bytes(self, body, content_type, status=HTTPStatus.OK):
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        request = urlparse(self.path)
        query = parse_qs(request.query)
        db = connect(self.db_path)
        try:
            if request.path == "/health":
                return self.json({"ok": True})
            if request.path == "/groups":
                rows = db.execute("SELECT g.group_id,g.display_name,g.status,g.features_json,g.updated_at,COALESCE(r.paused,0) AS paused FROM groups g LEFT JOIN group_runtime r ON r.group_id=g.group_id ORDER BY g.display_name,g.group_id").fetchall()
                return self.json([dict(row) for row in rows])
            if request.path == "/group-state":
                group_id = query.get("group_id", [""])[0].strip()
                row = db.execute("SELECT paused,updated_at FROM group_runtime WHERE group_id=?", (group_id,)).fetchone()
                return self.json({
                    "group_id": group_id,
                    "online": not bool(row["paused"]) if row else True,
                    "updated_at": row["updated_at"] if row else None,
                })
            if request.path == "/repeat-follow/state":
                group_id = query.get("group_id", [""])[0].strip()
                state = read_setting(db, "repeat_follow", {})
                return self.json({
                    "global_enabled": bool(state.get("enabled", True)),
                    "group_id": group_id,
                    "enabled": repeat_group_enabled(state, group_id),
                })
            if request.path == "/bot-guard/state":
                state = read_setting(db, "bot_guard", {})
                return self.json({
                    "enabled": bool(state.get("enabled", True)),
                    "accounts": bot_guard_accounts(state),
                })
            if request.path == "/group-capability":
                group_id = query.get("group_id", [""])[0].strip()
                capability = query.get("capability", [""])[0].strip()
                if not group_id:
                    return self.json({"error": "group_id is required"}, HTTPStatus.BAD_REQUEST)
                if capability:
                    if capability not in GROUP_CAPABILITIES:
                        return self.json({"error": "unknown capability"}, HTTPStatus.BAD_REQUEST)
                    return self.json({
                        "group_id": group_id,
                        "capability": capability,
                        "enabled": group_capability_enabled(db, group_id, capability),
                    })
                features = group_features(db, group_id)
                return self.json({
                    "group_id": group_id,
                    "capabilities": {
                        name: bool(features.get(name, True)) for name in sorted(GROUP_CAPABILITIES)
                    },
                })
            if request.path == "/library/search":
                text = query.get("q", [""])[0].strip()
                limit = min(max(int(query.get("limit", ["10"])[0]), 1), 30)
                requested_genre = resolve_library_genre(query.get("genre", [""])[0]) or resolve_library_genre(text)
                search_query = library_query_without_genre(text, requested_genre)
                if search_query:
                    rows = db.execute(
                        "SELECT * FROM library_texts WHERE (title LIKE ? OR content LIKE ?) "
                        "ORDER BY char_count LIMIT ?",
                        (f"%{search_query}%", f"%{search_query}%", max(limit * 8, 30)),
                    ).fetchall()
                else:
                    rows = db.execute(
                        "SELECT * FROM library_texts ORDER BY "
                        + ("RANDOM()" if requested_genre else "char_count")
                        + " LIMIT ?",
                        (max(limit * 8, 1000) if requested_genre else max(limit * 8, 30),),
                    ).fetchall()
                result = []
                for row in rows:
                    if row["category"] == "fm_single_chars":
                        continue
                    if requested_genre and not library_genre_matches(db, row, requested_genre):
                        continue
                    metadata = classify_library_row(db, row)
                    result.append({
                        "text_id": row["text_id"], "title": row["title"],
                        "category": row["category"], "relative_path": row["relative_path"],
                        "char_count": row["char_count"], "genre": metadata["primary_genre"],
                        "genres": metadata["genres"], "form": metadata["form"],
                        "difficulty": metadata["difficulty"],
                        "difficulty_score": metadata["difficulty_score"],
                        "confidence": metadata["confidence"],
                    })
                    if len(result) >= limit:
                        break
                db.commit()
                return self.json(result)
            if request.path == "/library/pick":
                text = query.get("q", [""])[0].strip()
                requested_genre = resolve_library_genre(query.get("genre", [""])[0]) or resolve_library_genre(text)
                search_query = library_query_without_genre(text, requested_genre)
                if search_query:
                    rows = db.execute(
                        "SELECT * FROM library_texts WHERE (title LIKE ? OR content LIKE ?) "
                        "ORDER BY char_count LIMIT 200",
                        (f"%{search_query}%", f"%{search_query}%"),
                    ).fetchall()
                else:
                    rows = db.execute(
                        "SELECT * FROM library_texts WHERE category<>'fm_single_chars' "
                        "ORDER BY RANDOM() LIMIT 200"
                    ).fetchall()
                candidates = []
                for row in rows:
                    if row["category"] == "fm_single_chars":
                        continue
                    if requested_genre and not library_genre_matches(db, row, requested_genre):
                        continue
                    metadata = classify_library_row(db, row)
                    item = dict(row)
                    item.update({
                        "genre": metadata["primary_genre"], "genres": metadata["genres"],
                        "form": metadata["form"], "difficulty": metadata["difficulty"],
                        "difficulty_score": metadata["difficulty_score"],
                        "confidence": metadata["confidence"],
                    })
                    candidates.append(item)
                db.commit()
                return self.json(random.choice(candidates) if candidates else None)
            if request.path == "/library/stats":
                return self.json(library_stats(db))
            if request.path == "/library/session-status":
                return self.json(library_session_status(db, {
                    "platform": query.get("platform", ["qq"])[0],
                    "chat_id": query.get("chat_id", [""])[0],
                    "requester_id": query.get("requester_id", [""])[0],
                }))
            if request.path == "/library/session":
                platform = query.get("platform", ["qq"])[0].strip().lower()
                chat_id = query.get("chat_id", [""])[0].strip()
                requester_id = canonical_user_id(query.get("requester_id", [""])[0])
                row = db.execute(
                    "SELECT * FROM library_sessions WHERE platform=? AND chat_id=? AND requester_id=?",
                    (platform, chat_id, requester_id),
                ).fetchone()
                return self.json(dict(row) if row else None)
            if request.path == "/recalls":
                text = query.get("q", [""])[0]
                group_id = query.get("group_id", [""])[0]
                rows = db.execute("SELECT message_id,group_id,group_name,sender_id,sender_name,text,recalled_ts,image_urls_json FROM recall_records WHERE group_id LIKE ? AND text LIKE ? ORDER BY recalled_ts DESC LIMIT 30", (f"%{group_id}%", f"%{text}%")).fetchall()
                result = []
                for row in rows:
                    item = dict(row)
                    try:
                        item["image_urls"] = json.loads(item.pop("image_urls_json") or "[]")
                    except (TypeError, json.JSONDecodeError):
                        item["image_urls"] = []
                    result.append(item)
                return self.json(result)
            if request.path == "/scores":
                group_id = query.get("group_id", [""])[0]
                sender_id = query.get("sender_id", [""])[0]
                limit = min(max(int(query.get("limit", ["20"])[0]), 1), 100)
                rows = db.execute(
                    "SELECT source_json FROM score_records "
                    "WHERE group_id LIKE ? AND sender_id LIKE ? "
                    "ORDER BY CASE WHEN typeof(occurred_at)='text' "
                    "THEN CAST(strftime('%s', occurred_at) AS REAL) "
                    "ELSE CAST(occurred_at AS REAL) END DESC LIMIT ?",
                    (f"%{group_id}%", f"%{sender_id}%", limit),
                ).fetchall()
                return self.json([json.loads(row["source_json"]) for row in rows])
            if request.path == "/contest/search":
                text = query.get("q", [""])[0].strip()
                source_group = query.get("source", [""])[0].strip()
                competition_date = query.get("date", [""])[0].strip()
                limit = min(max(int(query.get("limit", ["10"])[0]), 1), 30)
                rows = db.execute(
                    "SELECT text_id,title,source_group,competition_date,relative_path,char_count "
                    "FROM contest_texts WHERE (title LIKE ? OR content LIKE ?) AND source_group LIKE ? "
                    "AND competition_date LIKE ? ORDER BY competition_date DESC,title LIMIT ?",
                    (f"%{text}%", f"%{text}%", f"%{source_group}%", f"%{competition_date}%", limit),
                ).fetchall()
                return self.json([dict(row) for row in rows])
            if request.path == "/contest/pick":
                text = query.get("q", [""])[0].strip()
                source_group = query.get("source", [""])[0].strip()
                competition_date = query.get("date", [""])[0].strip()
                rows = db.execute(
                    "SELECT text_id,title,source_group,competition_date,relative_path,content,char_count "
                    "FROM contest_texts WHERE (title LIKE ? OR content LIKE ?) AND source_group LIKE ? "
                    "AND competition_date LIKE ? ORDER BY competition_date DESC LIMIT 100",
                    (f"%{text}%", f"%{text}%", f"%{source_group}%", f"%{competition_date}%"),
                ).fetchall()
                if not rows:
                    return self.json(None)
                result = dict(random.choice(rows))
                result["content"] = read_contest_content(result)
                result["char_count"] = len(result["content"])
                result["segment_id"] = next_library_segment_id(db)
                db.commit()
                return self.json(result)
            if request.path == "/ai-contest/text":
                competition_date = query.get("date", [""])[0].strip()
                today_only = query.get("today", [""])[0].strip().lower() in {"1", "true", "yes"}
                if today_only or not competition_date:
                    competition_date = public_competition_date()
                row = db.execute("SELECT * FROM ai_contest_texts WHERE competition_date=?", (competition_date,)).fetchone()
                return self.json(dict(row) if row else None)
            if request.path == "/ai-contest/policy":
                return self.json(read_setting(db, "ai_contest_policy", AI_CONTEST_POLICY_DEFAULTS))
            if request.path == "/ai-contest/leaderboard":
                competition_date = query.get("date", [""])[0].strip()
                if not competition_date:
                    competition_date = public_competition_date()
                rows = db.execute(
                    "SELECT user_id,source_json FROM ai_contest_scores WHERE competition_date=? AND speed>0 ORDER BY speed DESC,occurred_at ASC",
                    (competition_date,),
                ).fetchall()
                best = {}
                for row in rows:
                    item = json.loads(row["source_json"])
                    user_id = str(row["user_id"] or "")
                    if user_id and user_id not in best:
                        best[user_id] = item
                return self.json({"date": competition_date, "rows": list(best.values())})
            if request.path == "/reports/ai-leaderboard.png":
                competition_date = query.get("date", [""])[0].strip()
                return self.bytes(render_ai_leaderboard(db, competition_date), "image/png")
            if request.path == "/competition/scores":
                user_id = query.get("user_id", [""])[0].strip()
                name = query.get("name", [""])[0].strip()
                source_group = query.get("source", [""])[0].strip()
                limit = min(max(int(query.get("limit", ["20"])[0]), 1), 100)
                rows = db.execute(
                    "SELECT source_json FROM competition_scores WHERE user_id LIKE ? AND user_name LIKE ? "
                    "AND source_group LIKE ? ORDER BY occurred_at DESC LIMIT ?",
                    (f"%{user_id}%", f"%{name}%", f"%{source_group}%", limit),
                ).fetchall()
                return self.json([json.loads(row["source_json"]) for row in rows])
            if request.path == "/competition/summary":
                user_id = query.get("user_id", [""])[0].strip()
                name = query.get("name", [""])[0].strip()
                source_group = query.get("source", [""])[0].strip()
                return self.json(competition_score_summary(db, user_id, name, source_group))
            if request.path == "/reports/competition-score.png":
                user_id = query.get("user_id", [""])[0].strip()
                name = query.get("name", [""])[0].strip()
                source_group = query.get("source", [""])[0].strip()
                return self.bytes(render_competition_score(db, user_id, name, source_group), "image/png")
            if request.path == "/reports/score-chart.png":
                user_id = query.get("user_id", [""])[0].strip()
                name = query.get("name", [""])[0].strip()
                source_group = query.get("source", [""])[0].strip()
                metric = query.get("metric", ["speed"])[0].strip() or "speed"
                try:
                    days = min(max(int(query.get("days", ["30"])[0]), 1), 3650)
                except ValueError:
                    days = 30
                return self.bytes(render_score_chart(db, user_id, name, source_group, days, metric), "image/png")
            if request.path in {
                "/competition/live", "/competition/live/text", "/reports/live-competition.png",
            }:
                source = query.get("source", [""])[0].strip()
                group_id = query.get("group_id", [""])[0].strip()
                period = query.get("date", [""])[0].strip()
                refresh = query.get("refresh", [""])[0].lower() in {"1", "true", "yes"}
                soft_error = query.get("soft", [""])[0].lower() in {"1", "true", "yes"}
                try:
                    live = get_public_competition(source, group_id, period, refresh)
                except PublicCompetitionError as exc:
                    return self.json({
                        "status": "error", "code": exc.code, "message": str(exc),
                        "source": source, "group_id": group_id, "date": period,
                    }, HTTPStatus.OK if soft_error else exc.status)
                if request.path == "/competition/live":
                    return self.json(public_competition_response(live))
                if request.path == "/competition/live/text":
                    response = public_competition_response(live, include_rows=False)
                    if live["kind"] == "champ":
                        response.update({"status": "unavailable", "message": "锦标赛公开页面不提供赛文正文。"})
                    elif not live.get("content"):
                        response.update({"status": "no_text", "message": "公开赛事网站当前没有返回这一天的赛文正文。"})
                    else:
                        response["message"] = live["content"]
                    return self.json(response)
                if not live["row_count"]:
                    return self.json(public_competition_response(live, include_rows=False), HTTPStatus.NOT_FOUND)
                requested_page = query.get("page", [""])[0].strip()
                combined = query.get("combined", [""])[0].lower() in {"1", "true", "yes"}
                # The public image endpoint is complete by default. An explicit
                # page is retained only for backwards-compatible callers.
                if not requested_page:
                    combined = True
                page = 1 if not requested_page else min(max(int(requested_page), 1), live["page_count"])
                return self.bytes(render_public_competition_rank(live, page, combined), "image/png")
            if request.path == "/stats":
                tables = [
                    "groups", "library_texts", "library_classifications", "contest_texts", "recall_records", "score_records",
                    "ai_contest_texts", "ai_contest_scores", "competition_scores", "recent_messages",
                ]
                return self.json({table: db.execute(f"SELECT COUNT(*) FROM {table}").fetchone()[0] for table in tables})
            if request.path == "/archive/status":
                return self.json(archive_status(db))
            return self.json({"error": "not found"}, HTTPStatus.NOT_FOUND)
        finally:
            db.close()

    def do_POST(self):
        request = urlparse(self.path)
        if request.path not in {
            "/group-state", "/group-capability", "/events/message", "/events/recall",
            "/repeat-follow/state", "/repeat-follow/check", "/bot-guard/check",
            "/library/session/start", "/library/session/continue", "/library/session/continue-same",
            "/library/session/continue-previous",
            "/library/session/score", "/library/session/stop",
            "/library/session/recall-recent", "/library/session/recalled",
            "/library/classify",
            "/library/single/start", "/contest/session/start",
            "/library/session/sent", "/ai-contest/text", "/ai-contest/policy", "/bot-guard/accounts",
        }:
            return self.json({"error": "not found"}, HTTPStatus.NOT_FOUND)
        try:
            length = min(int(self.headers.get("Content-Length", "0")), 1024 * 1024)
            payload = json.loads(self.rfile.read(length))
            if not isinstance(payload, dict):
                raise ValueError
        except (TypeError, ValueError, json.JSONDecodeError):
            return self.json({"error": "valid JSON object required"}, HTTPStatus.BAD_REQUEST)

        if request.path == "/group-state":
            try:
                group_id = str(payload["group_id"]).strip()
                online = payload["online"]
                if not group_id or not isinstance(online, bool):
                    raise ValueError
            except (KeyError, TypeError, ValueError):
                return self.json({"error": "group_id and boolean online are required"}, HTTPStatus.BAD_REQUEST)
            db = connect(self.db_path)
            try:
                db.execute("INSERT INTO group_runtime VALUES (?, ?, ?) ON CONFLICT(group_id) DO UPDATE SET paused=excluded.paused,updated_at=excluded.updated_at", (group_id, 0 if online else 1, time.time()))
                db.commit()
                return self.json({"group_id": group_id, "online": online})
            finally:
                db.close()

        if request.path == "/repeat-follow/state":
            group_id = str(payload.get("group_id") or "").strip()
            enabled = payload.get("enabled")
            if not isinstance(enabled, bool):
                return self.json({"error": "boolean enabled is required"}, HTTPStatus.BAD_REQUEST)
            db = connect(self.db_path)
            try:
                state = read_setting(db, "repeat_follow", {})
                if group_id:
                    overrides = state.setdefault("group_overrides", {})
                    if not isinstance(overrides, dict):
                        overrides = {}
                        state["group_overrides"] = overrides
                    overrides[group_id] = enabled
                else:
                    state["enabled"] = enabled
                state["updated_at"] = time.time()
                write_setting(db, "repeat_follow", state)
                db.commit()
                return self.json({"group_id": group_id, "enabled": repeat_group_enabled(state, group_id)})
            finally:
                db.close()

        if request.path == "/group-capability":
            group_id = str(payload.get("group_id") or "").strip()
            capability = str(payload.get("capability") or "").strip()
            enabled = payload.get("enabled")
            if not group_id or capability not in GROUP_CAPABILITIES or not isinstance(enabled, bool):
                return self.json({"error": "valid group_id, capability, and boolean enabled are required"}, HTTPStatus.BAD_REQUEST)
            db = connect(self.db_path)
            try:
                return self.json(set_group_capability(db, group_id, capability, enabled))
            finally:
                db.close()

        if request.path == "/repeat-follow/check":
            db = connect(self.db_path)
            try:
                return self.json(repeat_check(db, payload))
            finally:
                db.close()

        if request.path == "/bot-guard/check":
            db = connect(self.db_path)
            try:
                return self.json(bot_guard_check(db, payload))
            finally:
                db.close()

        if request.path == "/bot-guard/accounts":
            db = connect(self.db_path)
            try:
                try:
                    return self.json(update_bot_guard_account(db, payload))
                except ValueError as exc:
                    return self.json({"status": "invalid", "message": str(exc)}, HTTPStatus.BAD_REQUEST)
            finally:
                db.close()

        if request.path == "/ai-contest/text":
            db = connect(self.db_path)
            try:
                try:
                    return self.json(publish_ai_contest_text(db, payload))
                except (TypeError, ValueError) as exc:
                    return self.json({"status": "invalid", "message": str(exc)}, HTTPStatus.BAD_REQUEST)
            finally:
                db.close()

        if request.path == "/ai-contest/policy":
            db = connect(self.db_path)
            try:
                try:
                    return self.json(update_ai_contest_policy(db, payload))
                except (TypeError, ValueError) as exc:
                    return self.json({"status": "invalid", "message": str(exc)}, HTTPStatus.BAD_REQUEST)
            finally:
                db.close()

        if request.path == "/library/single/start":
            db = connect(self.db_path)
            try:
                try:
                    return self.json(start_single_session(db, payload))
                except (TypeError, ValueError) as exc:
                    return self.json({"status": "invalid", "message": str(exc)}, HTTPStatus.BAD_REQUEST)
            finally:
                db.close()

        if request.path == "/library/classify":
            db = connect(self.db_path)
            try:
                try:
                    limit = int(payload.get("limit") or 0)
                    if limit < 0:
                        raise ValueError("limit must be non-negative")
                    return self.json(classify_library(db, limit))
                except (TypeError, ValueError) as exc:
                    return self.json({"status": "invalid", "message": str(exc)}, HTTPStatus.BAD_REQUEST)
            finally:
                db.close()

        if request.path == "/contest/session/start":
            db = connect(self.db_path)
            try:
                try:
                    return self.json(start_contest_session(db, payload))
                except (TypeError, ValueError) as exc:
                    return self.json({"status": "invalid", "message": str(exc)}, HTTPStatus.BAD_REQUEST)
            finally:
                db.close()

        if request.path.startswith("/library/session/"):
            db = connect(self.db_path)
            try:
                try:
                    if request.path == "/library/session/start":
                        result = start_library_session(db, payload)
                    elif request.path == "/library/session/continue":
                        result = continue_library_session(db, payload)
                    elif request.path == "/library/session/continue-same":
                        result = continue_same_library_session(db, payload)
                    elif request.path == "/library/session/continue-previous":
                        result = continue_previous_library_session(db, payload)
                    elif request.path == "/library/session/score":
                        result = continue_library_session_from_score(db, payload)
                    elif request.path == "/library/session/stop":
                        result = stop_library_session(db, payload)
                    elif request.path == "/library/session/recall-recent":
                        result = recent_library_messages(db, payload)
                    elif request.path == "/library/session/recalled":
                        result = mark_library_messages_recalled(db, payload)
                    else:
                        result = acknowledge_library_message(db, payload)
                except (TypeError, ValueError) as exc:
                    return self.json({"status": "invalid", "message": str(exc)}, HTTPStatus.BAD_REQUEST)
                return self.json(result)
            finally:
                db.close()

        if request.path == "/events/message":
            return self.record_message_event(payload)
        return self.record_recall_event(payload)

    def record_message_event(self, payload):
        import time
        platform = str(payload.get("platform") or "").strip()
        message_id = str(payload.get("message_id") or "").strip()
        group_id = str(payload.get("group_id") or "").strip()
        sender_id = canonical_user_id(payload.get("sender_id"))
        text = str(payload.get("text") or "").strip()
        sender_name, group_name = sender_names(payload)
        occurred_at = float(payload.get("occurred_at") or time.time())
        image_urls = payload.get("image_urls")
        if not isinstance(image_urls, list):
            image_urls = []
        image_urls = [str(url).strip() for url in image_urls if str(url).strip()]
        if not platform or not message_id:
            return self.json({"error": "platform and message_id are required"}, HTTPStatus.BAD_REQUEST)
        raw_json = json.dumps(payload, ensure_ascii=False)
        db = connect(self.db_path)
        try:
            db.execute(
                "INSERT INTO recent_messages (platform,message_id,group_id,group_name,sender_id,sender_name,text,occurred_at,raw_json,image_urls_json) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) "
                "ON CONFLICT(platform,message_id) DO UPDATE SET group_id=excluded.group_id,group_name=excluded.group_name,sender_id=excluded.sender_id,sender_name=excluded.sender_name,text=excluded.text,occurred_at=excluded.occurred_at,raw_json=excluded.raw_json,image_urls_json=excluded.image_urls_json",
                (platform, message_id, group_id, group_name, sender_id, sender_name, text, occurred_at, raw_json, json.dumps(image_urls, ensure_ascii=False)),
            )
            archived = False
            ai_contest_archived = False
            parsed = parse_typing_score(text)
            # Every recognized QQ typing score is part of the shared archive.
            # The old group allowlist made new or unlisted groups fail silently.
            if parsed and platform == "qq":
                record = {
                    **parsed,
                    "source": "cosmobot_live",
                    "group_id": group_id,
                    "group_name": group_name,
                    "sender_id": sender_id,
                    "sender_name": sender_name,
                    "message_id": message_id,
                    "received_at": occurred_at,
                    "raw": text,
                }
                db.execute(
                    "INSERT INTO score_records VALUES (?, ?, ?, ?, ?) ON CONFLICT(record_id) DO NOTHING",
                    (f"qq:{message_id}", group_id, sender_id, occurred_at, json.dumps(record, ensure_ascii=False)),
                )
                archived = bool(db.execute("SELECT changes()").fetchone()[0])
            if (
                parsed
                and platform == "qq"
                and str(parsed.get("segment_id")) == "555"
                and platform == "qq"
            ):
                china_time = timezone(timedelta(hours=8))
                competition_date = datetime.fromtimestamp(occurred_at, china_time).date().isoformat()
                record = {
                    **parsed,
                    "source": "cosmobot_live",
                    "competition_date": competition_date,
                    "group_id": group_id,
                    "user_id": sender_id,
                    "user_name": sender_name or sender_id,
                    "speed": parsed["speed"],
                    "key": parsed["keystroke"],
                    "acc": parsed["accuracy"],
                    "message_id": message_id,
                    "ts": occurred_at,
                    "raw": text,
                }
                db.execute(
                    "INSERT INTO ai_contest_scores VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) "
                    "ON CONFLICT(record_id) DO NOTHING",
                    (
                        f"qq:{message_id}", competition_date, sender_id,
                        sender_name or sender_id, group_id, parsed["speed"],
                        parsed["keystroke"], parsed["accuracy"], occurred_at,
                        json.dumps(record, ensure_ascii=False),
                    ),
                )
                ai_contest_archived = bool(db.execute("SELECT changes()").fetchone()[0])
            db.execute("DELETE FROM recent_messages WHERE occurred_at < ?", (time.time() - 172800,))
            db.commit()
            return self.json({
                "stored": True,
                "score_archived": archived,
                "ai_contest_archived": ai_contest_archived,
                "competition_date": competition_date if ai_contest_archived else "",
            })
        finally:
            db.close()

    def record_recall_event(self, payload):
        import time
        platform = str(payload.get("platform") or "").strip()
        message_id = str(payload.get("message_id") or "").strip()
        group_id = str(payload.get("group_id") or "").strip()
        if not platform or not message_id:
            return self.json({"error": "platform and message_id are required"}, HTTPStatus.BAD_REQUEST)
        db = connect(self.db_path)
        try:
            original = db.execute(
                "SELECT * FROM recent_messages WHERE platform=? AND message_id=?", (platform, message_id)
            ).fetchone()
            if not original:
                return self.json({"captured": False, "reason": "original message not cached"})
            source = {"recall_event": payload, "original": dict(original)}
            try:
                image_urls = json.loads(original["image_urls_json"] or "[]")
            except (KeyError, TypeError, json.JSONDecodeError):
                image_urls = []
            if not isinstance(image_urls, list):
                image_urls = []
            db.execute(
                "INSERT INTO recall_records (message_id,group_id,group_name,sender_id,sender_name,text,recalled_ts,image_urls_json,source_json) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) "
                "ON CONFLICT(message_id) DO UPDATE SET group_id=excluded.group_id,group_name=excluded.group_name,sender_id=excluded.sender_id,sender_name=excluded.sender_name,text=excluded.text,recalled_ts=excluded.recalled_ts,image_urls_json=excluded.image_urls_json,source_json=excluded.source_json",
                (
                    message_id, group_id or original["group_id"], original["group_name"],
                    original["sender_id"], original["sender_name"], original["text"],
                    float(payload.get("occurred_at") or time.time()), json.dumps(image_urls, ensure_ascii=False), json.dumps(source, ensure_ascii=False),
                ),
            )
            db.commit()
            return self.json({"captured": True, "message_id": message_id})
        finally:
            db.close()

    def log_message(self, *_):
        pass


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("command", choices=["import", "import-retained", "rank-library", "classify-library", "serve"])
    parser.add_argument("--db", default=os.environ.get("FM_DOMAIN_DB", "/data/fm-domain.sqlite3"))
    parser.add_argument("--snapshot", default=os.environ.get("FM_SNAPSHOT", "/snapshot"))
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8077)
    parser.add_argument("--limit", type=int, default=0)
    args = parser.parse_args()
    db = connect(args.db)
    db.close()
    if args.command == "import":
        print(json.dumps(import_snapshot(connect(args.db), Path(args.snapshot)), ensure_ascii=False))
    elif args.command == "import-retained":
        print(json.dumps(import_retained_snapshot(connect(args.db), Path(args.snapshot)), ensure_ascii=False))
    elif args.command == "rank-library":
        database = connect(args.db)
        try:
            print(json.dumps(rank_library(database, max(0, args.limit)), ensure_ascii=False))
        finally:
            database.close()
    elif args.command == "classify-library":
        database = connect(args.db)
        try:
            print(json.dumps(classify_library(database, max(0, args.limit)), ensure_ascii=False))
        finally:
            database.close()
    else:
        Api.db_path = args.db
        ThreadingHTTPServer((args.host, args.port), Api).serve_forever()


if __name__ == "__main__":
    main()
