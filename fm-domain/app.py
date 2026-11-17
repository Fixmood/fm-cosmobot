#!/usr/bin/env python3
"""FM's retained domain data importer and read-only query service."""

import huma_annotate
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
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timedelta, timezone
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import parse_qs, urlencode, urlparse
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen


# ── 顶层辅助函数 ─────────────────────────────────────────

# 必须放在文件**顶部**：这个文件末尾是 if __name__ == "__main__": main()，

# 追加到末尾的函数在 main() 执行时还没定义 —— 2026-10-05 因此连崩两次（NameError）。

def _raw_score_field(raw, name):
    """从跟打原始行里取一个数字字段；**取不到返回 0.0**。

    所有者 2026-10-05 定的：没报的字段一律记 0，规则简单一致，榜面不留空白。
    （代价：键准会显示成 0.00%，读起来像「打了 0 分」—— 所有者的取舍。）

    真实格式举例：
        第555段 速度35.05 击键0.00 码长0.00 字数257 时间07:18.246 回改39 重打0 ...
    「重打」只有约一半记录有 —— 缺失和「0 次」是两回事。
    """
    if not raw:
        return None
    m = re.search(rf"{name}\s*([0-9]+(?:\.[0-9]+)?)", str(raw))
    return float(m.group(1)) if m else 0.0




def _raw_retype(raw):
    """重打次数；**首打的成绩标 0**。

    首打＝一次打完，本来就不存在重打 —— 所以那不是「没报」，是真的 0。
    把确定的事实显示成「—」（未知）和把没报的显示成 0（谎报）一样糟，两个方向都要防。

    实测：带「首打认证」的记录里确实没有「重打」字段（只有打词、码长这些）。
    """
    value = _raw_score_field(raw, "重打")
    if value is not None:
        return value
    if raw and "首打" in str(raw):
        return 0.0
    # 所有者 2026-10-05：没报的也记 0（规则简单一致，榜面不留空白）。
    return 0.0






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
  source_json TEXT NOT NULL,
  -- 2026-10-05 加：从原始跟打行解析出来的两个字段。
  -- **可空**：不同跟打工具报的字段不同（实测「重打」只有约一半记录有），
  -- 缺失必须是 NULL —— 存 0 会在榜上显示成「0 次」，那是谎报成绩。
  code_length REAL,
  retype REAL,
  -- 打词率（%）。原始行里有两种写法：打词32.42% 和 打词15.93%(17)，
  -- 取百分号前那个数即可。同样可空。
  word_rate REAL
);
CREATE INDEX IF NOT EXISTS ai_contest_score_date_idx ON ai_contest_scores(competition_date, speed DESC);

-- 老库补列：CREATE TABLE IF NOT EXISTS 对已存在的表什么都不做，所以这里单独 ALTER。
-- 幂等：列已存在就跳过。

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
-- 番茄小说专区（2026-11-05）。
-- 设计要点：层级是 书 → 章节 → （按用户的）进度 + （每本书的）成绩流水。
-- 章节正文仍然存在 library_texts 里，这里只加「归属哪本书、第几章」的映射，
-- 这样难度评级、题材分类、分段发送全部复用现有逻辑。
CREATE TABLE IF NOT EXISTS typing_scores (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  platform TEXT NOT NULL DEFAULT 'qq',
  chat_id TEXT NOT NULL, user_id TEXT NOT NULL, user_name TEXT NOT NULL DEFAULT '',
  segment_label TEXT NOT NULL DEFAULT '',
  speed REAL, key_rate REAL, code_length REAL, accuracy REAL,
  char_count INTEGER, duration_ms INTEGER, word_rate REAL,
  backspace INTEGER, delete_count INTEGER, retype INTEGER, choice_count INTEGER,
  difficulty TEXT NOT NULL DEFAULT '', device TEXT NOT NULL DEFAULT '',
  meta TEXT NOT NULL DEFAULT '{}',
  novel_book_id TEXT, novel_chapter_index INTEGER, novel_segment_no INTEGER,
  raw_text TEXT NOT NULL DEFAULT '', recorded_at REAL NOT NULL
);
CREATE INDEX IF NOT EXISTS typing_scores_chat_idx ON typing_scores(chat_id, recorded_at DESC);
CREATE INDEX IF NOT EXISTS typing_scores_user_idx ON typing_scores(user_id, recorded_at DESC);
CREATE INDEX IF NOT EXISTS typing_scores_book_idx ON typing_scores(novel_book_id, user_id);

CREATE TABLE IF NOT EXISTS novel_books (
  book_id TEXT PRIMARY KEY,
  title TEXT NOT NULL,
  author TEXT NOT NULL DEFAULT '',
  total_chapters INTEGER NOT NULL DEFAULT 0,
  word_count INTEGER NOT NULL DEFAULT 0,
  source TEXT NOT NULL DEFAULT 'fanqie',
  imported_at REAL NOT NULL
);
CREATE INDEX IF NOT EXISTS novel_books_title_idx ON novel_books(title);
CREATE TABLE IF NOT EXISTS novel_chapters (
  text_id TEXT PRIMARY KEY,
  book_id TEXT NOT NULL,
  chapter_index INTEGER NOT NULL,
  title TEXT NOT NULL,
  word_count INTEGER NOT NULL DEFAULT 0
);
CREATE INDEX IF NOT EXISTS novel_chapters_book_idx ON novel_chapters(book_id, chapter_index);
CREATE TABLE IF NOT EXISTS novel_progress (
  platform TEXT NOT NULL,
  chat_id TEXT NOT NULL,
  requester_id TEXT NOT NULL,
  book_id TEXT NOT NULL,
  chapter_index INTEGER NOT NULL,
  char_offset INTEGER NOT NULL DEFAULT 0,
  segment_length INTEGER NOT NULL DEFAULT 300,
  last_segment_id TEXT NOT NULL DEFAULT '',
  last_segment_chars INTEGER NOT NULL DEFAULT 0,
  last_sent_at REAL NOT NULL DEFAULT 0,
  updated_at REAL NOT NULL,
  PRIMARY KEY (platform, chat_id, requester_id)
);
CREATE TABLE IF NOT EXISTS novel_records (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  book_id TEXT NOT NULL,
  chapter_index INTEGER NOT NULL,
  user_id TEXT NOT NULL,
  user_name TEXT NOT NULL DEFAULT '',
  speed REAL NOT NULL DEFAULT 0,
  accuracy REAL NOT NULL DEFAULT 0,
  wpm REAL NOT NULL DEFAULT 0,
  chars INTEGER NOT NULL DEFAULT 0,
  recorded_at REAL NOT NULL,
  source_text TEXT NOT NULL DEFAULT ''
);
CREATE INDEX IF NOT EXISTS novel_records_book_idx ON novel_records(book_id, recorded_at DESC);
CREATE INDEX IF NOT EXISTS novel_records_user_idx ON novel_records(user_id, book_id);
CREATE TABLE IF NOT EXISTS library_session_modes (
  session_id TEXT PRIMARY KEY, requested_difficulty TEXT NOT NULL,
  requested_length INTEGER NOT NULL DEFAULT 0,
  requested_genre TEXT NOT NULL DEFAULT '',
  auto_continue_same INTEGER NOT NULL DEFAULT 0
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
LIBRARY_CLASSIFIER_VERSION = "genre-form-v3"

LIBRARY_GENRE_ALIASES = {
    "恐怖": "恐怖惊悚", "惊悚": "恐怖惊悚", "灵异": "恐怖惊悚",
    "悬疑": "悬疑推理", "推理": "悬疑推理", "刑侦": "悬疑推理",
    "修仙": "仙侠修真", "修真": "仙侠修真", "仙侠": "仙侠修真",
    "玄幻": "玄幻奇幻", "奇幻": "玄幻奇幻", "魔幻": "玄幻奇幻",
    "武侠": "武侠江湖", "江湖": "武侠江湖",
    "言情": "言情爱情", "爱情": "言情爱情", "恋爱": "言情爱情",
    "科幻": "科幻未来", "未来": "科幻未来", "星际": "科幻未来",
    "历史": "历史古风", "古风": "历史古风", "都市": "都市职场", "职场": "都市职场",
    "校园": "校园青春", "青春": "校园青春", "亲情": "亲情家庭", "家庭": "亲情家庭",
    "励志": "励志成长", "成长": "励志成长", "寓言": "寓言童话", "童话": "寓言童话",
    "诗词": "诗词古典", "古典": "诗词古典", "诗歌": "诗词古典",
    "散文": "散文随笔", "随笔": "散文随笔", "科普": "科普说明", "说明文": "科普说明",
}

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
    "寓言童话": ("寓言", "童话", "王子", "公主", "森林", "从前有一天", "小动物", "巨人"),
    "诗词古典": ("诗经", "楚辞", "唐诗", "宋词", "元曲", "古诗", "诗词", "绝句", "律诗", "乐府", "文言", "古文"),
    "散文随笔": ("散文", "随笔", "杂记", "感悟", "抒情", "回忆", "心情", "絮语", "札记", "漫谈"),
    "科普说明": ("科普", "说明", "百科", "知识", "原理", "实验", "科学", "技术", "指南", "教程", "机制", "结构"),
    "自然生态": ("自然", "生态", "森林", "山川", "河流", "湖泊", "海洋", "植物", "动物", "花草", "鸟类", "气候", "环境", "田野", "四季"),
}
LIBRARY_GENRE_STRONG_TERMS = {genre: set(terms[:min(10, len(terms))]) for genre, terms in LIBRARY_GENRE_RULES.items()}
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
CHINA_TIMEZONE = timezone(timedelta(hours=8))


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
        "user_id": canonical_user_id(
            row.get("user_qq_number") or row.get("qq") or row.get("user_id") or ""
        ),
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
            "rank": rank, "user_id": "", "name": clean_public_text(cells[1] or cells[6]),
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
            "user_id": canonical_user_id(
                item.get("user_id") or item.get("qq") or item.get("user_qq_number") or ""
            ),
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


def analysis_name(value) -> str:
    return re.sub(r"\s+", "", unicodedata.normalize("NFKC", str(value or ""))).casefold()


def analysis_identity_match(item: dict, user_id: str = "", name: str = "") -> str:
    target_id = canonical_user_id(user_id)
    candidate_ids = [
        canonical_user_id(item.get(key))
        for key in ("user_id", "sender_id", "qq", "user_qq_number")
        if str(item.get(key) or "").strip()
    ]
    if target_id and target_id in candidate_ids:
        return "user_id"
    target_name = analysis_name(name)
    candidate_name = analysis_name(
        item.get("name") or item.get("user_name") or item.get("sender_name") or ""
    )
    if target_name and candidate_name and target_name == candidate_name:
        return "name"
    return ""


def analysis_number(item: dict, keys, default=0.0) -> float:
    for key in keys:
        value = item.get(key)
        if value is None or str(value).strip() == "":
            continue
        try:
            return float(str(value).replace("%", "").strip())
        except (TypeError, ValueError):
            continue
    return float(default)


def analysis_date_for(item: dict, fallback_timestamp=None) -> str:
    for key in ("competition_date", "date", "occurred_date"):
        value = str(item.get(key) or "").strip()
        if value:
            try:
                return public_competition_date(value)
            except PublicCompetitionError:
                pass
    try:
        if fallback_timestamp is not None:
            return datetime.fromtimestamp(float(fallback_timestamp), CHINA_TIMEZONE).date().isoformat()
    except (TypeError, ValueError, OSError, OverflowError):
        pass
    for key in ("ts", "occurred_at", "received_at", "timestamp"):
        value = item.get(key)
        if value in (None, ""):
            continue
        try:
            return datetime.fromtimestamp(float(value), CHINA_TIMEZONE).date().isoformat()
        except (TypeError, ValueError, OSError, OverflowError):
            continue
    return ""


def analysis_projection(item: dict, source: str = "", period: str = "", match_by: str = "") -> dict:
    result = {
        "date": period or analysis_date_for(item),
        "source": source or str(item.get("source_group") or item.get("source") or "未知来源"),
        "name": str(item.get("name") or item.get("user_name") or item.get("sender_name") or ""),
        "user_id": canonical_user_id(item.get("user_id") or item.get("sender_id") or ""),
        "speed": round(analysis_number(item, ("speed", "rate")), 2),
        "keystroke": round(analysis_number(item, ("keystroke", "key", "hit_rate")), 2),
        "accuracy": round(analysis_number(item, ("accuracy", "acc", "key_rate")), 2),
    }
    for key in ("rank", "participants", "characters", "segment_id", "group_id", "group_name", "message_id"):
        if key in item and item[key] not in (None, ""):
            result[key] = item[key]
    if match_by:
        result["matched_by"] = match_by
    return result


def analysis_stats(records: list[dict]) -> dict:
    def values(keys):
        return [analysis_number(item, keys) for item in records if analysis_number(item, keys) > 0]

    def average(items):
        return round(sum(items) / len(items), 2) if items else 0

    speeds = values(("speed",))
    keystrokes = values(("keystroke", "key", "hit_rate"))
    accuracies = values(("accuracy", "acc", "key_rate"))
    dates = sorted({str(item.get("date") or "") for item in records if item.get("date")})
    return {
        "count": len(records),
        "days": len(dates),
        "date_start": dates[0] if dates else "",
        "date_end": dates[-1] if dates else "",
        "best_speed": round(max(speeds), 2) if speeds else 0,
        "average_speed": average(speeds),
        "lowest_speed": round(min(speeds), 2) if speeds else 0,
        "best_keystroke": round(max(keystrokes), 2) if keystrokes else 0,
        "average_keystroke": average(keystrokes),
        "best_accuracy": round(max(accuracies), 2) if accuracies else 0,
        "average_accuracy": average(accuracies),
        "lowest_accuracy": round(min(accuracies), 2) if accuracies else 0,
    }


def score_analysis_range(start_date="", end_date="", days=1):
    today = datetime.now(CHINA_TIMEZONE).date()

    def parse(value, fallback):
        value = str(value or "").strip()
        if not value:
            return fallback
        try:
            return datetime.strptime(public_competition_date(value), "%Y-%m-%d").date()
        except (PublicCompetitionError, ValueError) as exc:
            raise ValueError("日期必须是 YYYY-MM-DD 格式。") from exc

    end = parse(end_date, today)
    if str(start_date or "").strip():
        start = parse(start_date, end)
    else:
        try:
            span = max(1, min(3650, int(days or 1)))
        except (TypeError, ValueError) as exc:
            raise ValueError("days 必须是 1 到 3650 之间的整数。") from exc
        start = end - timedelta(days=span - 1)
    if start > end:
        raise ValueError("开始日期不能晚于结束日期。")
    return start.isoformat(), end.isoformat(), today.isoformat()


def score_analysis_targets(source="") -> list[tuple[str, str]]:
    compact = re.sub(r"\s+", "", str(source or "")).casefold()
    if not compact or compact in {"全部", "所有", "all"}:
        return [
            ("champ", "锦标赛"), ("tiger", "虎杯"), ("comp", "极速杯"),
            *[(group_id, name) for group_id, name in PUBLIC_COMPETITION_GROUPS.items()],
        ]
    if compact in {"555", "555赛文", "ai", "aicontest"}:
        return []
    target = resolve_public_competition(source)
    return [(str(source).strip(), target["source"])]


def collect_live_score_analysis(period: str, user_id: str, name: str, source="") -> list[dict]:
    targets = score_analysis_targets(source)
    if not targets:
        return []

    def fetch(target):
        source_key, label = target
        try:
            live = get_public_competition(source_key, "", period)
            matches = []
            for row in live.get("rows", []):
                matched_by = analysis_identity_match(row, user_id, name)
                if matched_by:
                    matches.append(analysis_projection(row, label, live.get("date") or period, matched_by))
            return {
                "source": label,
                "date": live.get("date") or period,
                "status": "ok" if live.get("row_count") else "no_data",
                "participants": live.get("row_count", 0),
                "records": matches,
            }
        except PublicCompetitionError as exc:
            return {
                "source": label,
                "date": period,
                "status": "error",
                "participants": 0,
                "records": [],
                "message": str(exc),
            }

    reports = []
    with ThreadPoolExecutor(max_workers=min(8, len(targets))) as executor:
        futures = [executor.submit(fetch, target) for target in targets]
        for future in as_completed(futures):
            reports.append(future.result())
    order = {label: index for index, (_, label) in enumerate(targets)}
    return sorted(reports, key=lambda item: order.get(item["source"], 999))


def archive_score_analysis_records(db, table: str, user_id: str, name: str, start: str, end: str) -> list[dict]:
    if table == "score_records":
        rows = db.execute("SELECT source_json,occurred_at FROM score_records ORDER BY occurred_at DESC").fetchall()
    elif table == "competition_scores":
        rows = db.execute("SELECT source_json,occurred_at FROM competition_scores ORDER BY occurred_at DESC").fetchall()
    else:
        rows = db.execute("SELECT source_json,occurred_at FROM ai_contest_scores ORDER BY occurred_at DESC").fetchall()
    records = []
    for row in rows:
        try:
            item = json.loads(row["source_json"] or "{}")
        except (TypeError, json.JSONDecodeError):
            continue
        matched_by = analysis_identity_match(item, user_id, name)
        if not matched_by:
            continue
        period = analysis_date_for(item, row["occurred_at"])
        if not period or not start <= period <= end:
            continue
        source = str(item.get("source_group") or item.get("group_name") or item.get("source") or "未知来源")
        if table == "ai_contest_scores":
            source = "555赛文"
        records.append(analysis_projection(item, source, period, matched_by))
    return records


def build_score_analysis(
    db: sqlite3.Connection,
    user_id: str = "",
    name: str = "",
    scope: str = "all",
    start_date: str = "",
    end_date: str = "",
    days: int = 1,
    source: str = "",
    live_results=None,
) -> dict:
    start, end, today = score_analysis_range(start_date, end_date, days)
    scope_key = re.sub(r"\s+", "", str(scope or "all")).casefold()
    include_contest = scope_key in {"all", "全部", "所有", "contest", "competition", "赛文", "赛事"}
    include_typing = scope_key in {"all", "全部", "所有", "typing", "practice", "跟打", "打字"}
    if scope_key not in {"all", "全部", "所有", "contest", "competition", "赛文", "赛事", "typing", "practice", "跟打", "打字"}:
        raise ValueError("scope 只能是 all、contest 或 typing。")
    if not canonical_user_id(user_id) and not analysis_name(name):
        raise ValueError("必须提供 user_id 或 name。")

    contest_history = []
    contest_today = []
    live_reports = []
    if include_contest:
        contest_history = archive_score_analysis_records(db, "competition_scores", user_id, name, start, end)
        if start <= today <= end and not re.sub(r"\s+", "", str(source or "")).casefold() in {"555", "555赛文", "ai", "aicontest"}:
            live_reports = live_results if live_results is not None else collect_live_score_analysis(today, user_id, name, source)
            for report in live_reports:
                for record in report.get("records", []):
                    record["participants"] = report.get("participants", 0)
                    contest_today.append(record)

    ai_records = []
    if include_contest and (not source or re.sub(r"\s+", "", str(source)).casefold() in {"555", "555赛文", "ai", "aicontest"}):
        ai_records = archive_score_analysis_records(db, "ai_contest_scores", user_id, name, start, end)

    typing_records = []
    if include_typing:
        typing_records = archive_score_analysis_records(db, "score_records", user_id, name, start, end)
        # Segment 555 is the dedicated AI contest archive and is reported
        # separately so an all-scores analysis does not count it twice.
        typing_records = [
            item for item in typing_records
            if str(item.get("segment_id") or "") != "555"
        ]

    errors = [
        {"source": report.get("source", ""), "message": report.get("message", "")}
        for report in live_reports if report.get("status") == "error"
    ]
    notes = []
    if include_contest and start <= today <= end and not contest_today:
        notes.append("今天公开排行榜没有匹配到该用户；可能尚未上榜，或该榜单只提供姓名，已使用姓名精确匹配作为回退。")
    if include_typing and not typing_records:
        notes.append("所选日期内没有找到已归档的跟打成绩。")
    if errors:
        notes.append("部分公开赛事暂时无法访问，已在 live_errors 中列出，未把缺失赛事当成零分。")
    all_contest_records = contest_history + contest_today + ai_records
    return {
        "status": "ok" if (all_contest_records or typing_records) else "no_data",
        "user": {"user_id": canonical_user_id(user_id), "name": name},
        "scope": scope_key,
        "date_range": {"start": start, "end": end, "today": today},
        "contest": {
            "today": sorted(contest_today, key=lambda item: (item.get("source", ""), item.get("rank", 999999))),
            "history": contest_history[:100],
            "summary": analysis_stats(contest_history + contest_today),
            "live_sources_checked": [report.get("source", "") for report in live_reports],
            "live_errors": errors,
        },
        "ai_contest": {"records": ai_records[:100], "summary": analysis_stats(ai_records)},
        "typing": {"records": typing_records[:100], "summary": analysis_stats(typing_records)},
        "notes": notes,
    }


def connect(path: str) -> sqlite3.Connection:
    db = sqlite3.connect(path)
    db.row_factory = sqlite3.Row
    db.executescript(SCHEMA)
    # 「文来」要排除这个会话最近发过的文章 —— 2026-10-30 之前只排除当前一篇，
    # 于是 A → B → A 会重复（难度高时候选池小，特别明显）。
    _ensure_library_recent_table(db)
    db.executescript(CONTEST_SCHEMA)
    # 老表补列：CREATE TABLE IF NOT EXISTS 不会给已经存在的表加列，
    # 2026-11-06 加 segment_length 时线上表还是旧的，直接崩了。这里逐个补。
    for table, column, ddl in (
        ("novel_progress", "segment_length", "INTEGER NOT NULL DEFAULT 300"),
        ("novel_progress", "last_segment_id", "TEXT NOT NULL DEFAULT ''"),
        ("novel_progress", "last_segment_chars", "INTEGER NOT NULL DEFAULT 0"),
        ("novel_progress", "last_sent_at", "REAL NOT NULL DEFAULT 0"),
    ):
        existing = {row[1] for row in db.execute(f"PRAGMA table_info({table})")}
        if existing and column not in existing:
            db.execute(f"ALTER TABLE {table} ADD COLUMN {column} {ddl}")
            db.commit()
    ensure_column(db, "library_session_modes", "requested_length", "INTEGER NOT NULL DEFAULT 0")
    ensure_column(db, "library_session_modes", "requested_genre", "TEXT NOT NULL DEFAULT ''")
    ensure_column(db, "library_session_modes", "auto_continue_same", "INTEGER NOT NULL DEFAULT 0")
    ensure_column(db, "library_previous_sessions", "requested_length", "INTEGER NOT NULL DEFAULT 0")
    ensure_column(db, "library_previous_sessions", "requested_genre", "TEXT NOT NULL DEFAULT ''")
    ensure_column(db, "recall_records", "image_urls_json", "TEXT NOT NULL DEFAULT '[]'")
    ensure_column(db, "recent_messages", "image_urls_json", "TEXT NOT NULL DEFAULT '[]'")
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
    if normalized in LIBRARY_GENRE_RULES:
        return normalized
    return LIBRARY_GENRE_ALIASES.get(normalized, "")


def resolve_library_genre(value) -> str:
    normalized = re.sub(r"\s+", "", str(value or "").strip().lower())
    return normalize_library_genre(normalized) or next(
        (LIBRARY_GENRE_RULES[name] and name for name in sorted(LIBRARY_GENRE_RULES, key=len, reverse=True) if name in normalized), ""
    ) or next((target for alias, target in sorted(LIBRARY_GENRE_ALIASES.items(), key=lambda item: len(item[0]), reverse=True) if alias in normalized), "")


def library_query_without_genre(query: str, genre: str) -> str:
    value = str(query or "").strip()
    if not genre:
        return value
    aliases = [genre] + [alias for alias, target in LIBRARY_GENRE_ALIASES.items() if target == genre]
    for alias in sorted(set(aliases), key=len, reverse=True):
        value = re.sub(re.escape(alias), " ", value, flags=re.IGNORECASE)
    value = re.sub(r"(?:题材|类型|文体|小说|文章|故事|文|来一篇|发一篇|给我|找一篇)", " ", value)
    return re.sub(r"\s+", " ", value).strip()


def classify_library_text(title: str, content: str) -> dict:
    title_text = re.sub(r"\s+", "", str(title or "")).lower()
    body = re.sub(r"\s+", "", str(content or "")).lower()[:LIBRARY_CLASSIFICATION_SAMPLE_SIZE]
    scores, evidence = {}, {}
    for genre, terms in LIBRARY_GENRE_RULES.items():
        hits = {term: title_text.count(term) * 3 + min(body.count(term), 6) for term in terms if term in title_text or term in body}
        score = sum(hits.values())
        strong_hits = sum(1 for term in hits if term in LIBRARY_GENRE_STRONG_TERMS.get(genre, set()))
        # Body mentions alone must form a real cluster; a passing reference
        # such as a school or teacher should not label a novel as campus fiction.
        reliable = any(term in title_text for term in hits) or strong_hits >= 2 or len(hits) >= 3
        if score >= 2 and reliable:
            scores[genre] = score
            evidence[genre] = hits
    ordered = sorted(scores.items(), key=lambda item: (-item[1], item[0]))
    primary = ordered[0][0] if ordered else "综合文学"
    genres = [genre for genre, score in ordered[:3] if score >= max(2, ordered[0][1] * .42)] if ordered else [primary]
    form_scores = {"小说/叙事": 1}
    for form, terms in LIBRARY_FORM_RULES.items():
        score = sum(title_text.count(term) * 4 + min(body.count(term), 4) for term in terms)
        if score:
            form_scores[form] = score
    if len([line for line in str(content or "").splitlines() if line.strip()]) >= 3 and len(body) <= 500:
        form_scores["诗歌"] = form_scores.get("诗歌", 0) + 4
    if len(re.findall(r"[：:]", str(content or ""))) >= 2:
        form_scores["对话剧本"] = form_scores.get("对话剧本", 0) + 2
    return {"primary_genre": primary, "genres": genres, "form": max(form_scores.items(), key=lambda item: (item[1], item[0]))[0],
            "confidence": round(min(.99, .35 + min(.5, (ordered[0][1] if ordered else 0) / 30)), 3), "evidence": evidence}


def classify_library_row(db: sqlite3.Connection, row: sqlite3.Row, refresh: bool = False) -> dict:
    cached = db.execute("SELECT * FROM library_classifications WHERE text_id=?", (row["text_id"],)).fetchone()
    if cached and not refresh and cached["classifier_version"] == LIBRARY_CLASSIFIER_VERSION:
        metadata = dict(cached)
        metadata["genres"] = json.loads(metadata["genres_json"] or "[]")
        metadata["evidence"] = json.loads(metadata["evidence_json"] or "{}")
        return metadata
    score, difficulty = rank_library_row(db, row)
    metadata = classify_library_text(row["title"], row["content"])
    metadata.update(text_id=row["text_id"], difficulty=difficulty, difficulty_score=score, classifier_version=LIBRARY_CLASSIFIER_VERSION, classified_at=time.time())
    db.execute("INSERT INTO library_classifications (text_id,primary_genre,genres_json,form,difficulty,difficulty_score,confidence,evidence_json,classifier_version,classified_at) VALUES (?,?,?,?,?,?,?,?,?,?) ON CONFLICT(text_id) DO UPDATE SET primary_genre=excluded.primary_genre,genres_json=excluded.genres_json,form=excluded.form,difficulty=excluded.difficulty,difficulty_score=excluded.difficulty_score,confidence=excluded.confidence,evidence_json=excluded.evidence_json,classifier_version=excluded.classifier_version,classified_at=excluded.classified_at",
               (row["text_id"], metadata["primary_genre"], json.dumps(metadata["genres"], ensure_ascii=False), metadata["form"], difficulty, score, metadata["confidence"], json.dumps(metadata["evidence"], ensure_ascii=False), LIBRARY_CLASSIFIER_VERSION, time.time()))
    return metadata


def library_genre_matches(db: sqlite3.Connection, row: sqlite3.Row, genre: str) -> bool:
    metadata = classify_library_row(db, row)
    if not genre or not (metadata["primary_genre"] == genre or genre in metadata["genres"]):
        return False
    evidence = metadata.get("evidence", {}).get(genre, {})
    strong_terms = LIBRARY_GENRE_STRONG_TERMS.get(genre, set())
    strong_hits = sum(1 for term, count in evidence.items() if term in strong_terms and count)
    return strong_hits >= 2


# ==================== 番茄小说专区（2026-11-05） ====================
# 所有者要的形态：直接说打某本小说，有就发；进度按发起人记；跳章按书的章号；
# 成绩单独按书存。文来随机完全不受影响，只有走这里的请求才用这套。

NOVEL_DEFAULT_SEGMENT = 300
NOVEL_IDLE_MINUTES = 10   # 所有者定的：超过这么久没打自动结束


def novel_identity(payload: dict) -> tuple[str, str, str, str]:
    return (
        str(payload.get("platform") or "qq"),
        str(payload.get("chat_id") or ""),
        str(payload.get("requester_id") or ""),
        str(payload.get("requester_name") or ""),
    )


# ==================== 成绩分析（2026-11-06） ====================
# 数据在 typing_scores：全字段保留 + raw_text，所以任何分析都能现算。
# 这里只封装几个最常问的，返回 message 是可以直接念的中文。

TYPING_MIN_FOR_RANKING = 50


def typing_identity(payload: dict) -> tuple[str, str]:
    return (str(payload.get("requester_id") or ""), str(payload.get("requester_name") or ""))


def typing_days_clause(days: int) -> tuple[str, list]:
    if days <= 0:
        return ("", [])
    return (" AND recorded_at >= ?", [time.time() - days * 86400])


def typing_stats_me(db: sqlite3.Connection, payload: dict) -> dict:
    user_id, user_name = typing_identity(payload)
    days = int(payload.get("days") or 0)
    clause, params = typing_days_clause(days)
    row = db.execute(
        "SELECT count(*) n, avg(speed) speed, avg(accuracy) acc, avg(key_rate) key_rate, "
        "avg(code_length) code_len, avg(word_rate) word_rate, max(speed) best, sum(char_count) chars "
        "FROM typing_scores WHERE user_id=?" + clause,
        tuple([user_id] + params),
    ).fetchone()
    if not row or not row["n"]:
        return {"status": "empty", "message": "还没有你的成绩记录。"}
    span = f"最近 {days} 天" if days > 0 else "全部时间"
    return {
        "status": "ok",
        "message": (
            f"{span}你打了 {row['n']} 段：均速 {round(float(row['speed'] or 0), 2)}，"
            f"最快 {round(float(row['best'] or 0), 2)}，"
            f"平均键准 {round(float(row['acc'] or 0), 2)}%，"
            f"击键 {round(float(row['key_rate'] or 0), 2)}，"
            f"码长 {round(float(row['code_len'] or 0), 2)}，"
            f"打词 {round(float(row['word_rate'] or 0), 2)}%，"
            f"累计 {int(row['chars'] or 0)} 字。"
        ),
        "count": int(row["n"]),
    }


def typing_stats_trend(db: sqlite3.Connection, payload: dict) -> dict:
    user_id, _ = typing_identity(payload)
    days = int(payload.get("days") or 14)
    since = time.time() - days * 86400
    rows = db.execute(
        "SELECT date(recorded_at,'unixepoch','+8 hours') d, count(*) n, "
        "avg(speed) speed, avg(accuracy) acc FROM typing_scores "
        "WHERE user_id=? AND recorded_at>=? AND recorded_at>0 GROUP BY d ORDER BY d",
        (user_id, since),
    ).fetchall()
    if not rows:
        return {"status": "empty", "message": f"最近 {days} 天你没有成绩记录。"}
    lines = [
        f"{row['d']}: {row['n']} 段，均速 {round(float(row['speed'] or 0), 1)}，键准 {round(float(row['acc'] or 0), 1)}%"
        for row in rows
    ]
    return {
        "status": "ok",
        "message": f"最近 {days} 天的趋势：" + "；".join(lines),
        "points": [{"date": row["d"], "count": row["n"], "speed": round(float(row["speed"] or 0), 1)} for row in rows],
    }


def typing_stats_ranking(db: sqlite3.Connection, payload: dict) -> dict:
    days = int(payload.get("days") or 7)
    limit = max(1, min(20, int(payload.get("limit") or 5)))
    since = time.time() - days * 86400
    rows = db.execute(
        "SELECT user_name, user_id, count(*) n, avg(speed) speed, avg(accuracy) acc "
        "FROM typing_scores WHERE recorded_at>=? AND recorded_at>0 "
        "GROUP BY user_id HAVING n>=? ORDER BY speed DESC LIMIT ?",
        (since, TYPING_MIN_FOR_RANKING, limit),
    ).fetchall()
    if not rows:
        return {"status": "empty", "message": f"最近 {days} 天还没有足够的数据做排行。"}
    parts = [
        f"{index + 1}. {row['user_name'] or '无名'} 均速 {round(float(row['speed'] or 0), 1)}（{row['n']} 段）"
        for index, row in enumerate(rows)
    ]
    return {
        "status": "ok",
        "message": f"最近 {days} 天均速排行（至少 {TYPING_MIN_FOR_RANKING} 段）：" + "；".join(parts),
    }


def typing_stats_book(db: sqlite3.Connection, payload: dict) -> dict:
    """某本书的成绩。需要成绩被关联到书（novel_book_id），没关联就如实说。"""
    user_id, _ = typing_identity(payload)
    keyword = str(payload.get("book") or "").strip()
    book = novel_find_book(db, keyword) if keyword else None
    if book:
        rows = db.execute(
            "SELECT count(*) n, avg(speed) speed, avg(accuracy) acc, sum(char_count) chars "
            "FROM typing_scores WHERE user_id=? AND novel_book_id=?",
            (user_id, book["book_id"]),
        ).fetchone()
        title = book["title"]
    else:
        rows = None
        title = keyword or "这本书"
    if not rows or not rows["n"]:
        return {
            "status": "empty",
            "message": f"{title}还没有和你关联的成绩记录。可能是成绩还没和番茄段落对上（要等发段后打的那些）。",
        }
    return {
        "status": "ok",
        "message": (
            f"{title}你打了 {rows['n']} 段：均速 {round(float(rows['speed'] or 0), 2)}，"
            f"键准 {round(float(rows['acc'] or 0), 2)}%，累计 {int(rows['chars'] or 0)} 字。"
        ),
        "count": int(rows["n"]),
    }


def typing_stats_dispatch(db: sqlite3.Connection, payload: dict) -> dict:
    scope = str(payload.get("scope") or "me").strip().lower()
    if scope == "trend":
        return typing_stats_trend(db, payload)
    if scope == "ranking":
        return typing_stats_ranking(db, payload)
    if scope == "book":
        return typing_stats_book(db, payload)
    return typing_stats_me(db, payload)


def novel_find_book(db: sqlite3.Connection, keyword: str) -> sqlite3.Row | None:
    """按书名关键词找书。精确优先，其次模糊；找不到返回 None。

    找不到时上层要告诉用户「让 fix 哥加上」—— 不列书架，也不假装有。
    """
    keyword = keyword.strip()
    if not keyword:
        return None
    row = db.execute("SELECT * FROM novel_books WHERE title=?", (keyword,)).fetchone()
    if row:
        return row
    row = db.execute(
        "SELECT * FROM novel_books WHERE title LIKE ? ORDER BY total_chapters DESC LIMIT 1",
        (f"%{keyword}%",),
    ).fetchone()
    if row:
        return row
    return db.execute(
        "SELECT * FROM novel_books WHERE book_id=? LIMIT 1", (keyword,)
    ).fetchone()


def novel_chapter_row(db: sqlite3.Connection, book_id: str, chapter_index: int) -> sqlite3.Row | None:
    return db.execute(
        "SELECT c.*, l.content FROM novel_chapters c JOIN library_texts l ON l.text_id=c.text_id "
        "WHERE c.book_id=? AND c.chapter_index=?",
        (book_id, chapter_index),
    ).fetchone()


def novel_save_progress(
    db: sqlite3.Connection, platform: str, chat_id: str, requester_id: str,
    book_id: str, chapter_index: int, char_offset: int, segment_length: int = 0,
    segment_id: int = 0, segment_chars: int = 0, sent_at: float = 0.0,
) -> None:
    # 段长 0 表示「沿用上次的」，所以只在有值时覆盖（跟库文 requested_length 一个思路）
    db.execute(
        "INSERT INTO novel_progress "
        "(platform,chat_id,requester_id,book_id,chapter_index,char_offset,segment_length,"
        "last_segment_id,last_segment_chars,last_sent_at,updated_at) "
        "VALUES (?,?,?,?,?,?,COALESCE(NULLIF(?,''),300),?,?,?,?) "
        "ON CONFLICT(platform,chat_id,requester_id) DO UPDATE SET "
        "book_id=excluded.book_id,chapter_index=excluded.chapter_index,"
        "char_offset=excluded.char_offset,"
        "segment_length=CASE WHEN excluded.segment_length>0 THEN excluded.segment_length "
        "ELSE novel_progress.segment_length END,"
        "last_segment_id=CASE WHEN excluded.last_segment_id<>'' THEN excluded.last_segment_id "
        "ELSE novel_progress.last_segment_id END,"
        "last_segment_chars=CASE WHEN excluded.last_segment_chars>0 "
        "THEN excluded.last_segment_chars ELSE novel_progress.last_segment_chars END,"
        "last_sent_at=CASE WHEN excluded.last_sent_at>0 THEN excluded.last_sent_at "
        "ELSE novel_progress.last_sent_at END,"
        "updated_at=excluded.updated_at",
        (platform, chat_id, requester_id, book_id, chapter_index, char_offset,
         segment_length, str(segment_id) if segment_id else "", segment_chars,
         sent_at if sent_at else time.time(), time.time()),
    )
    db.commit()


def novel_segment_length(payload: dict, progress: sqlite3.Row | None = None) -> int:
    """段长：本次传了就用本次的，没传就沿用上次存的，都没有就 300。"""
    requested = payload.get("length")
    if requested:
        try:
            value = int(requested)
            if 1 <= value <= 1400:
                return value
        except (TypeError, ValueError):
            pass
    if progress is not None:
        try:
            saved = int(progress["segment_length"])
            if 1 <= saved <= 1400:
                return saved
        except (KeyError, IndexError, TypeError, ValueError):
            pass
    return NOVEL_DEFAULT_SEGMENT


def novel_emit_segment(db: sqlite3.Connection, book: sqlite3.Row, chapter: sqlite3.Row,
                       segment_no: int, length: int) -> dict:
    """从一章的 char_offset 开始发一段，返回给调用方要发到群里的文本。"""
    content = normalize_library_content(chapter["content"])
    total = len(content)
    start = max(0, min(int(chapter["char_offset"]), total))
    # 段号按偏移推出来，避免调用方每次都传 1（2026-11-05 实测：段号全是 1）
    segment_no = start // max(1, length) + 1
    end = min(start + length, total)
    body = content[start:end]
    # 格式必须跟库文完全一致，否则群里的跟打器识别不到（所有者 2026-11-06 实测）。
    # 库文格式：[FM/难度分·题材·形式] No.FM-XXXXXXXX《标题》 [字数N]
    #          正文
    #          -----第N段-FM发文｜进度X/Y字
    article = db.execute(
        "SELECT * FROM library_texts WHERE text_id=?", (chapter["text_id"],)
    ).fetchone()
    metadata = classify_library_row(db, article) if article else {
        "primary_genre": "综合文学", "form": "小说/叙事"
    }
    ranking = db.execute(
        "SELECT score, difficulty FROM library_rankings WHERE text_id=?",
        (chapter["text_id"],),
    ).fetchone()
    score = float(ranking["score"]) if ranking else 0.0
    difficulty = str(ranking["difficulty"]) if ranking else "普"
    score_text = "爆表" if score > 100 else f"{score:.2f}"
    label = (
        f"{difficulty}{score_text}·"
        f"{metadata.get('primary_genre', '综合文学')}·"
        f"{metadata.get('form', '小说/叙事')}"
    )
    article_no = str(chapter["text_id"])[:8].upper()
    segment_id = next_library_segment_id(db)
    now = time.time()
    db.execute(
        "INSERT INTO library_segment_sessions VALUES (?, ?, ?, ?, ?, ?, NULL)",
        (segment_id, f"novel:{book['book_id']}:{chapter['chapter_index']}", "qq",
         "", "", now),
    )
    title = chapter["title"] if chapter["title"] else book["title"]
    message = (
        f"[FM/{label}] No.FM-{article_no}《{title}》 [字数{len(body)}]" + chr(10)
        + body + chr(10)
        + f"-----第{segment_id}段-FM发文｜进度{end}/{total}字"
    )
    return {
        "status": "segment",
        "message": message,
        "book_id": book["book_id"],
        "book_title": book["title"],
        "chapter_index": chapter["chapter_index"],
        "chapter_title": chapter["title"],
        "segment_no": segment_no,
        "segment_id": segment_id,
        "chars": len(body),
        "char_offset": end,
        "remaining_chars": max(0, total - end),
    }


def novel_pick_chapter(db: sqlite3.Connection, book: sqlite3.Row, chapter_index: int | None,
                       fallback_index: int = 1) -> sqlite3.Row | None:
    index = chapter_index if chapter_index and chapter_index > 0 else fallback_index
    row = novel_chapter_row(db, book["book_id"], index)
    if row:
        return row
    # 章号越界：退到最后可用的一章，避免直接报错
    return db.execute(
        "SELECT c.*, l.content FROM novel_chapters c JOIN library_texts l ON l.text_id=c.text_id "
        "WHERE c.book_id=? ORDER BY c.chapter_index DESC LIMIT 1",
        (book["book_id"],),
    ).fetchone()


def novel_start(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, requester_name = novel_identity(payload)
    keyword = str(payload.get("book") or "").strip()
    book = novel_find_book(db, keyword) if keyword else None
    if not book:
        return {
            "status": "not_found",
            "message": f"书库里没有「{keyword or '这本'}」。让 fix 哥加上吧，加好我就能发了。",
        }
    requested_chapter = payload.get("chapter")
    chapter_index = int(requested_chapter) if requested_chapter else 1
    chapter = novel_pick_chapter(db, book, chapter_index)
    length = novel_segment_length(payload)

    # 同一本书且有旧进度 → 从上次断点继续；换了章就从头
    progress = db.execute(
        "SELECT * FROM novel_progress WHERE platform=? AND chat_id=? AND requester_id=?",
        (platform, chat_id, requester_id),
    ).fetchone()
    # restart=true 时**不能**走「断点继续」这条路 ——
    # 2026-11-06 实测：用户说「从头发」，模型正确传了 restart=true，
    # 但这里只看「有没有旧进度」，于是又接着往下发了。
    if (
        progress
        and progress["book_id"] == book["book_id"]
        and not requested_chapter
        and not payload.get("restart")
    ):
        chapter = novel_chapter_row(db, book["book_id"], int(progress["chapter_index"])) or chapter
        if chapter:
            offset = int(progress["char_offset"])
            total = len(normalize_library_content(chapter["content"]))
            if offset >= total:
                nxt = novel_chapter_row(db, book["book_id"], int(progress["chapter_index"]) + 1)
                chapter = nxt or chapter
                offset = 0
            length = novel_segment_length(payload, progress)
            result = novel_emit_segment(db, book, {**dict(chapter), "char_offset": offset}, 1, length)
            novel_save_progress(db, platform, chat_id, requester_id, book["book_id"],
                                int(result["chapter_index"]), int(result["char_offset"]), length, int(result.get("segment_id") or 0), int(result.get("chars") or 0))
            return result

    if not chapter:
        return {"status": "not_found", "message": f"《{book['title']}》还没有可发的章节，让 fix 哥补一下。"}
    result = novel_emit_segment(db, book, {**dict(chapter), "char_offset": 0}, 1, novel_segment_length(payload))
    novel_save_progress(db, platform, chat_id, requester_id, book["book_id"],
                        int(result["chapter_index"]), int(result["char_offset"]), length, int(result.get("segment_id") or 0), int(result.get("chars") or 0))
    # 告诉上层「这是一次重开」，工具靠它去撤回上一条已发的段。
    if payload.get("restart"):
        result["restarted"] = True
        result["restart_reason"] = "从本章开头重发"
    return result


def novel_continue(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, _ = novel_identity(payload)
    progress = db.execute(
        "SELECT * FROM novel_progress WHERE platform=? AND chat_id=? AND requester_id=?",
        (platform, chat_id, requester_id),
    ).fetchone()
    if not progress:
        return {"status": "idle", "message": "你还没有在打的小说。说「打番茄小说《书名》」开一本。"}
    book = db.execute("SELECT * FROM novel_books WHERE book_id=?", (progress["book_id"],)).fetchone()
    if not book:
        return {"status": "idle", "message": "上次那本书已经不在书库里了，重新说一本吧。"}
    chapter = novel_chapter_row(db, book["book_id"], int(progress["chapter_index"]))
    if not chapter:
        return {"status": "idle", "message": f"《{book['title']}》的进度对不上，重新说一次书名吧。"}
    total = len(normalize_library_content(chapter["content"]))
    offset = int(progress["char_offset"])
    # 10 分钟没动过 → 自动结束发文状态（所有者 2026-11-06 的要求）。
    # 没有这个判断，僵尸进度会一直存在，谁发成绩都触发续段。
    last_active = float(progress["updated_at"] or 0)
    if last_active and time.time() - last_active > NOVEL_IDLE_MINUTES * 60:
        db.execute(
            "DELETE FROM novel_progress WHERE platform=? AND chat_id=? AND requester_id=?",
            (platform, chat_id, requester_id),
        )
        db.commit()
        return {"status": "idle", "message": "超过 %d 分钟没继续，发文已经自动结束了。" % NOVEL_IDLE_MINUTES}
    if offset >= total:
        return novel_next(db, payload)
    length = novel_segment_length(payload, progress)
    # 「改段长」和「从开头发」都要回到章首重发。
    # 2026-11-06 所有者实测：只说「我要 150 字」时，这里接着往下发了，章节偏移没动。
    saved_length = 0
    try:
        saved_length = int(progress["segment_length"])
    except (KeyError, IndexError, TypeError, ValueError):
        saved_length = 0
    wants_restart = bool(payload.get("restart")) or (
        saved_length > 0 and length != saved_length
    )
    start_offset = 0 if wants_restart else offset
    result = novel_emit_segment(db, book, {**dict(chapter), "char_offset": start_offset}, 1, length)
    novel_save_progress(db, platform, chat_id, requester_id, book["book_id"],
                        int(result["chapter_index"]), int(result["char_offset"]), length, int(result.get("segment_id") or 0), int(result.get("chars") or 0))
    result["restarted"] = wants_restart
    if wants_restart:
        result["restart_reason"] = "段长变化，从本章开头重发"
    return result


def novel_next(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, _ = novel_identity(payload)
    progress = db.execute(
        "SELECT * FROM novel_progress WHERE platform=? AND chat_id=? AND requester_id=?",
        (platform, chat_id, requester_id),
    ).fetchone()
    if not progress:
        return {"status": "idle", "message": "你还没有在打的小说。说「打番茄小说《书名》」开一本。"}
    book = db.execute("SELECT * FROM novel_books WHERE book_id=?", (progress["book_id"],)).fetchone()
    if not book:
        return {"status": "idle", "message": "上次那本书已经不在书库里了。"}
    next_index = int(progress["chapter_index"]) + 1
    chapter = novel_chapter_row(db, book["book_id"], next_index)
    if not chapter:
        return {
            "status": "completed",
            "message": f"《{book['title']}》已经打完了（共 {book['total_chapters']} 章）。要换一本就说书名。",
        }
    length = novel_segment_length(payload, progress)
    result = novel_emit_segment(db, book, {**dict(chapter), "char_offset": 0}, 1, length)
    novel_save_progress(db, platform, chat_id, requester_id, book["book_id"],
                        int(result["chapter_index"]), int(result["char_offset"]), length, int(result.get("segment_id") or 0), int(result.get("chars") or 0))
    return result


def novel_jump(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, _ = novel_identity(payload)
    progress = db.execute(
        "SELECT * FROM novel_progress WHERE platform=? AND chat_id=? AND requester_id=?",
        (platform, chat_id, requester_id),
    ).fetchone()
    book_id = str(payload.get("book_id") or (progress["book_id"] if progress else ""))
    book = db.execute("SELECT * FROM novel_books WHERE book_id=?", (book_id,)).fetchone()
    if not book:
        return {"status": "idle", "message": "还不知道你要跳哪本书。先说「打番茄小说《书名》」。"}
    chapter_index = int(payload.get("chapter") or 1)
    chapter = novel_chapter_row(db, book["book_id"], chapter_index)
    if not chapter:
        return {
            "status": "not_found",
            "message": f"《{book['title']}》没有第 {chapter_index} 章（共 {book['total_chapters']} 章）。",
        }
    length = novel_segment_length(payload, progress)
    result = novel_emit_segment(db, book, {**dict(chapter), "char_offset": 0}, 1, length)
    novel_save_progress(db, platform, chat_id, requester_id, book["book_id"],
                        int(result["chapter_index"]), int(result["char_offset"]), length, int(result.get("segment_id") or 0), int(result.get("chars") or 0))
    return result


# ==================== 赛文成绩归档（2026-11-06） ====================
# 段号 → (赛文名, 作用域)。所有者给的规则，段号就是赛文身份。
# scope='global' 表示所有群共用一份；scope='group' 表示每个群各自一份，必须分开存。
CONTEST_LABELS: "dict[str, tuple[str, str]]" = {
    "HB": ("虎杯", "global"),
    "100000": ("锦标赛", "global"),
    "10000": ("锦标赛", "global"),   # 兼容：所有者最初口述的段号，实际数据里是 100000
    "99999": ("极速杯", "global"),
    "100004": ("极速联赛", "global"),
    "mb": ("晴分身每日赛文", "global"),
    "666": ("1916996679每日赛文", "global"),
    "555": ("555赛文", "global"),
    "777": ("晴分身群赛文", "group"),
    "999": ("本群赛文", "group"),
    "groupmatch": ("1847992065群赛文", "group"),
}

# 段号 → 官网 /competition/live 的 source key（只有官网覆盖的赛文才在表里）。
# 2026-11-06 实测：虎杯/锦标赛/极速杯/极速联赛 + 各群自己的 999 赛文 官网都能拿到；
# 777 / mb / 666 / groupmatch 是别的机器人的赛文，官网没有，只能靠成绩库。
CONTEST_LIVE_SOURCES: "dict[str, str]" = {
    "HB": "tiger",
    "100000": "champ",
    "99999": "comp",
    "100004": "540678308",
}
# 各群 999 赛文在官网的 source 就是群号本身
CONTEST_GROUP_LIVE_GROUPS = {
    "776227233", "1021522088", "151040026", "391047371", "201323122", "488748631",
}

CONTEST_SCHEMA = """
CREATE TABLE IF NOT EXISTS contest_scores (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  contest_key TEXT NOT NULL,
  contest_name TEXT NOT NULL,
  scope TEXT NOT NULL DEFAULT 'global',
  group_id TEXT NOT NULL DEFAULT '',
  score_date TEXT NOT NULL DEFAULT '',
  user_id TEXT NOT NULL DEFAULT '',
  user_name TEXT NOT NULL DEFAULT '',
  segment_label TEXT NOT NULL DEFAULT '',
  speed REAL, key_rate REAL, code_length REAL, accuracy REAL,
  char_count INTEGER, duration_ms INTEGER, word_rate REAL,
  difficulty TEXT NOT NULL DEFAULT '', device TEXT NOT NULL DEFAULT '',
  raw_text TEXT NOT NULL DEFAULT '', recorded_at REAL NOT NULL
);
CREATE INDEX IF NOT EXISTS contest_scores_key_idx ON contest_scores(contest_key, scope, group_id, recorded_at DESC);
CREATE INDEX IF NOT EXISTS contest_scores_user_idx ON contest_scores(user_id, contest_key);
CREATE INDEX IF NOT EXISTS contest_scores_date_idx ON contest_scores(score_date);
"""


def contest_key_for(segment_label: str) -> tuple[str, str, str] | None:
    """段号 → (contest_key, contest_name, scope)。不认识的段号返回 None。"""
    label = (segment_label or "").strip()
    if not label:
        return None
    entry = CONTEST_LABELS.get(label)
    if not entry:
        return None
    name, scope = entry
    return label, name, scope


def _contest_parse_score(text: str) -> dict:
    """从成绩原文里抠出各项数值（复用番茄那套解析思路）。"""
    def num(pattern):
        match = re.search(pattern, text)
        if not match:
            return None
        try:
            return float(match.group(1))
        except (TypeError, ValueError):
            return None

    duration = re.search(r"时间\s*(\d+):(\d+)\.(\d+)", text)
    duration_ms = None
    if duration:
        duration_ms = int(duration.group(1)) * 60000 + int(duration.group(2)) * 1000 + int((duration.group(3) + "000")[:3])
    difficulty = re.search(r"(淼|水|易|普|难|虐)\s*([\d.]+)", text)
    device = re.search(r"(虎魄|大帅比打器|大帅比|极速|晴窗|五笔|仓颉|拼音|小鹤|音形)\s*v?([\d]+\.?[\d]*)", text)
    return {
        "speed": num(r"速度\s*([\d.]+)"),
        "key_rate": num(r"击键\s*([\d.]+)"),
        "code_length": num(r"码长\s*([\d.]+)"),
        "accuracy": num(r"键准\s*([\d.]+)"),
        "char_count": num(r"字数\s*(\d+)"),
        "duration_ms": duration_ms,
        "word_rate": num(r"打词\s*([\d.]+)"),
        "difficulty": (difficulty.group(1) + difficulty.group(2)) if difficulty else "",
        "device": (device.group(1) + "v" + (device.group(2) or "")) if device else "",
    }


def backfill_contest_scores(db: sqlite3.Connection) -> dict:
    """从 score_records 回填 contest_scores。幂等：已存在的 (raw_text+user) 跳过。"""
    rows = db.execute(
        "SELECT group_id, sender_id, occurred_at, source_json FROM score_records"
    ).fetchall()
    inserted = 0
    skipped = 0
    unknown: dict[str, int] = {}
    for group_id, sender_id, occurred_at, source_json in rows:
        try:
            payload = json.loads(source_json or "{}")
        except (TypeError, ValueError):
            continue
        text = str(payload.get("raw_text") or payload.get("text") or "")
        if not text:
            continue
        match = re.search(r"第\s*([A-Za-z0-9_-]{1,24})\s*段", text)
        if not match:
            continue
        mapped = contest_key_for(match.group(1))
        if not mapped:
            unknown[match.group(1)] = unknown.get(match.group(1), 0) + 1
            continue
        key, name, scope = mapped
        # 每个群不同的赛文必须带 group_id；全群共用的留空
        room = str(group_id or "") if scope == "group" else ""
        exists = db.execute(
            "SELECT 1 FROM contest_scores WHERE contest_key=? AND group_id=? AND user_id=? AND raw_text=? LIMIT 1",
            (key, room, str(payload.get("user_id") or sender_id or ""), text),
        ).fetchone()
        if exists:
            skipped += 1
            continue
        parsed = _contest_parse_score(text)
        score_date = ""
        try:
            score_date = time.strftime("%Y-%m-%d", time.localtime(float(occurred_at or 0)))
        except (TypeError, ValueError, OSError):
            pass
        user_name = str(payload.get("user_name") or payload.get("nickname") or "")
        db.execute(
            "INSERT INTO contest_scores (contest_key,contest_name,scope,group_id,score_date,"
            "user_id,user_name,segment_label,speed,key_rate,code_length,accuracy,char_count,"
            "duration_ms,word_rate,difficulty,device,raw_text,recorded_at) "
            "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
            (key, name, scope, room, score_date, str(payload.get("user_id") or sender_id or ""),
             user_name, match.group(0), parsed["speed"], parsed["key_rate"], parsed["code_length"],
             parsed["accuracy"], int(parsed["char_count"] or 0), parsed["duration_ms"],
             parsed["word_rate"], parsed["difficulty"], parsed["device"], text,
             float(occurred_at or 0)),
        )
        inserted += 1
    db.commit()
    return {"inserted": inserted, "skipped": skipped, "unknown_labels": unknown}


def contest_scores_query(db: sqlite3.Connection, payload: dict) -> dict:
    """按赛文 + 群查成绩（画图和查询共用）。

    网站取不到成绩时，这里就是唯一的数据源（所有者 2026-11-06 的要求）。
    """
    label = str(payload.get("contest") or "").strip()
    if not label:
        return {"status": "invalid", "message": "需要 contest 参数（段号或赛文名）。"}
    # 允许直接传赛文名
    key = None
    for candidate, (name, _scope) in CONTEST_LABELS.items():
        if label in (candidate, name):
            key = candidate
            break
    if not key:
        return {"status": "not_found", "message": f"不认识的赛文：{label}。已知：{', '.join(CONTEST_LABELS)}"}
    name, scope = CONTEST_LABELS[key]
    room = str(payload.get("group_id") or "") if scope == "group" else ""
    days = int(payload.get("days") or 0)
    clause, params = "", []
    if days > 0:
        clause = " AND recorded_at >= ?"
        params = [time.time() - days * 86400]
    rows = db.execute(
        "SELECT user_name, user_id, max(speed) best, avg(speed) avg_speed, count(*) n, "
        "max(recorded_at) last_at FROM contest_scores "
        "WHERE contest_key=? AND group_id=?" + clause +
        " GROUP BY user_id ORDER BY best DESC LIMIT 30",
        tuple([key, room] + params),
    ).fetchall()
    if not rows:
        where = f"{name}" + (f"（群 {room}）" if room else "")
        return {"status": "empty", "message": f"{where} 还没有成绩记录。"}
    lines = [
        f"{i}. {r['user_name'] or r['user_id']} 最快 {round(float(r['best'] or 0), 2)}（{r['n']} 次）"
        for i, r in enumerate(rows, 1)
    ]
    where = name + (f"（群 {room}）" if room else "（全群）")
    return {
        "status": "ok",
        "message": f"{where} 共 {len(rows)} 人： " + "；".join(lines),
        "contest": key,
        "contest_name": name,
        "scope": scope,
        "group_id": room,
        "rows": [{"user": r["user_name"] or r["user_id"], "best": r["best"], "count": r["n"]} for r in rows],
    }

def novel_books_list(db: sqlite3.Connection, payload: dict) -> dict:
    """列出书库里所有番茄小说（所有者 2026-11-06 要能问「有哪些书」）。"""
    rows = db.execute(
        "SELECT title, author, total_chapters, word_count FROM novel_books "
        "WHERE total_chapters > 0 ORDER BY word_count DESC"
    ).fetchall()
    if not rows:
        return {"status": "empty", "message": "书库里还没有番茄小说。让 fix 哥加几本。"}
    lines = [
        f"{index}. 《{row['title']}》{row['author'] or '未知作者'}（{row['total_chapters']}章 {row['word_count'] // 10000}万字）"
        for index, row in enumerate(rows, 1)
    ]
    # book_lines 是给「合并转发」用的：每条一本书，正好当一个节点。
    book_lines = [
        f"{index}. 《{row['title']}》\n作者：{row['author'] or '未知'}\n"
        f"{row['total_chapters']} 章 · {row['word_count'] // 10000} 万字"
        for index, row in enumerate(rows, 1)
    ]
    return {
        "status": "ok",
        "message": f"书库里有 {len(rows)} 本番茄小说：" + "；".join(lines),
        "count": len(rows),
        "book_lines": book_lines,
        "books": [{"title": row["title"], "author": row["author"], "chapters": row["total_chapters"]} for row in rows],
    }


# ---------- 打字成绩解析（番茄专区用） ----------
# 格式见群里的打字器输出：
#   第777段 速度172.32 击键6.36 码长2.22 键准94.41% 字数241 时间01:23.913
#   打词60.58% 重打1 普1.62 选重3 [词提开] 【fix】 虎魄v1.5.4
_SCORE_HINT = re.compile(r"速度\s*[\d.]+")
_SCORE_SEGMENT = re.compile(r"第\s*([0-9]+|groupmatch)\s*段")


def _score_number(pattern: str, text: str) -> float | None:
    match = re.search(pattern, text)
    if not match:
        return None
    try:
        return float(match.group(1))
    except (TypeError, ValueError):
        return None


def _score_duration_ms(text: str) -> int | None:
    match = re.search(r"时间\s*(\d+):(\d+)\.(\d+)", text)
    if not match:
        return None
    minutes, seconds, fraction = match.groups()
    return int(minutes) * 60000 + int(seconds) * 1000 + int((fraction + "000")[:3])


def parse_novel_typing_score(text: str) -> dict | None:
    """番茄专区用的成绩解析。

    注意函数名不能叫 parse_typing_score：app.py 里已经有一个同名的（成绩归档用），
    Python 里后定义的会覆盖前面的，2026-11-06 因此踩了 KeyError: 'raw_text'。
    """
    if not _SCORE_HINT.search(text):
        return None
    segment = _SCORE_SEGMENT.search(text)
    if not segment:
        return None
    difficulty = re.search(r"(淼|水|易|普|难|虐)\s*([\d.]+)", text)
    device = re.search(r"(虎魄|大帅比打器|大帅比|极速|晴窗|五笔|仓颉|拼音|小鹤|音形)\s*v?([\d]+\.?[\d]*)", text)
    score = {
        "segment_label": segment.group(0).replace(" ", ""),
        "speed": _score_number(r"速度\s*([\d.]+)", text),
        "key_rate": _score_number(r"击键\s*([\d.]+)", text),
        "code_length": _score_number(r"码长\s*([\d.]+)", text),
        "accuracy": _score_number(r"键准\s*([\d.]+)", text),
        "char_count": _score_number(r"字数\s*(\d+)", text),
        "duration_ms": _score_duration_ms(text),
        "word_rate": _score_number(r"打词\s*([\d.]+)", text),
        "backspace": _score_number(r"回改\s*(\d+)", text),
        "delete_count": _score_number(r"退格\s*(\d+)", text),
        "retype": _score_number(r"重打\s*(\d+)", text),
        "choice_count": _score_number(r"选重\s*(\d+)", text),
        "difficulty": (difficulty.group(1) + difficulty.group(2)) if difficulty else "",
        "device": (device.group(1) + "v" + (device.group(2) or "")) if device else "unknown",
        "raw_text": text,
    }
    if score["speed"] is None:
        return None
    # 合理性过滤，别让解析事故污染统计
    if not 0 <= score["speed"] <= 1200:
        return None
    if score["accuracy"] is not None and not 0 <= score["accuracy"] <= 100:
        return None
    if score["char_count"] is not None and not 1 <= score["char_count"] <= 200000:
        return None
    return score


def _record_typing_score(db: sqlite3.Connection, parsed: dict, platform: str, chat_id: str,
                         user_id: str, user_name: str, book_id: str, chapter_index: int,
                         segment_no: int) -> None:
    exists = db.execute(
        "SELECT 1 FROM typing_scores WHERE chat_id=? AND user_id=? AND raw_text=? LIMIT 1",
        (chat_id, user_id, parsed["raw_text"]),
    ).fetchone()
    if exists:
        # 已经由 cron 扫进去了 —— 只补关联
        db.execute(
            "UPDATE typing_scores SET novel_book_id=?, novel_chapter_index=?, novel_segment_no=? "
            "WHERE chat_id=? AND user_id=? AND raw_text=?",
            (book_id, chapter_index, segment_no, chat_id, user_id, parsed["raw_text"]),
        )
        return
    db.execute(
        "INSERT INTO typing_scores (platform,chat_id,user_id,user_name,segment_label,"
        "speed,key_rate,code_length,accuracy,char_count,duration_ms,word_rate,"
        "backspace,delete_count,retype,choice_count,difficulty,device,meta,"
        "novel_book_id,novel_chapter_index,novel_segment_no,raw_text,recorded_at) "
        "VALUES ('qq',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,'{}',?,?,?,?,?)",
        (chat_id, user_id, user_name, parsed["segment_label"],
         parsed["speed"], parsed["key_rate"], parsed["code_length"], parsed["accuracy"],
         int(parsed["char_count"] or 0), parsed["duration_ms"], parsed["word_rate"],
         int(parsed["backspace"] or 0), int(parsed["delete_count"] or 0),
         int(parsed["retype"] or 0), int(parsed["choice_count"] or 0),
         parsed["difficulty"], parsed["device"],
         book_id, chapter_index, segment_no, parsed["raw_text"], time.time()),
    )


def novel_score(db: sqlite3.Connection, payload: dict) -> dict:
    """收到一条打字成绩：先记账（关联到当前书/章），再自动续段。"""
    platform, chat_id, requester_id, requester_name = novel_identity(payload)
    text = str(payload.get("text") or "")
    parsed = parse_novel_typing_score(text)
    progress = db.execute(
        "SELECT * FROM novel_progress WHERE platform=? AND chat_id=? AND requester_id=?",
        (platform, chat_id, requester_id),
    ).fetchone()
    if not progress:
        return {"status": "idle", "message": "当前会话没有番茄小说进度，不续段。"}

    # 10 分钟超时：久了就不该再算「正在打」。
    last_active = float(progress["updated_at"] or 0)
    if last_active and time.time() - last_active > NOVEL_IDLE_MINUTES * 60:
        db.execute(
            "DELETE FROM novel_progress WHERE platform=? AND chat_id=? AND requester_id=?",
            (platform, chat_id, requester_id),
        )
        db.commit()
        return {"status": "idle", "message": f"超过 {NOVEL_IDLE_MINUTES} 分钟没打，发文自动结束了，不续段。"}

    # 判断这条成绩是不是打「刚发出去那一段」的（所有者 2026-11-06：
    # 之前任何成绩都会续段，很莫名其妙）。
    matched = False
    reason = ""
    if parsed:
        last_id = str(progress["last_segment_id"] or "")
        last_chars = int(progress["last_segment_chars"] or 0)
        last_sent = float(progress["last_sent_at"] or 0)
        score_seg = str(parsed.get("segment_label") or "").replace("第", "").replace("段", "")
        if last_id and score_seg == last_id:
            matched = True
            reason = "段号对上"
        elif last_chars and parsed.get("char_count") and abs(int(parsed["char_count"]) - last_chars) <= max(10, last_chars // 10):
            if last_sent and time.time() - last_sent <= 120:
                matched = True
                reason = "字数和时间对上"
    if not matched:
        return {
            "status": "not_matched",
            "message": "这条成绩不是刚发的那一段的，不续段。",
        }
    _record_typing_score(
        db, parsed, platform, chat_id, requester_id, requester_name,
        progress["book_id"], int(progress["chapter_index"]), 0,
    )
    db.commit()
    result = novel_continue(db, payload)
    result["score_recorded"] = True
    result["match_reason"] = reason
    return result


def novel_stop(db: sqlite3.Connection, payload: dict) -> dict:
    """停掉番茄阅读：清进度，告诉上层有没有停成（撤销由 bot 负责）。"""
    platform, chat_id, requester_id, _ = novel_identity(payload)
    row = db.execute(
        "SELECT book_id, chapter_index, char_offset FROM novel_progress "
        "WHERE platform=? AND chat_id=? AND requester_id=?",
        (platform, chat_id, requester_id),
    ).fetchone()
    if not row:
        return {"status": "idle", "message": "当前会话没有正在进行的番茄小说。"}
    db.execute(
        "DELETE FROM novel_progress WHERE platform=? AND chat_id=? AND requester_id=?",
        (platform, chat_id, requester_id),
    )
    db.commit()
    book = db.execute("SELECT title FROM novel_books WHERE book_id=?", (row["book_id"],)).fetchone()
    title = book["title"] if book else row["book_id"]
    return {
        "status": "stopped",
        "message": f"已停止《{title}》。",
        "book_title": title,
        "chapter_index": int(row["chapter_index"]),
    }


def novel_progress_info(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, _ = novel_identity(payload)
    progress = db.execute(
        "SELECT * FROM novel_progress WHERE platform=? AND chat_id=? AND requester_id=?",
        (platform, chat_id, requester_id),
    ).fetchone()
    if not progress:
        return {"status": "idle", "message": "你还没有在打的小说。"}
    book = db.execute("SELECT * FROM novel_books WHERE book_id=?", (progress["book_id"],)).fetchone()
    chapter = novel_chapter_row(db, book["book_id"], int(progress["chapter_index"])) if book else None
    if not book or not chapter:
        return {"status": "idle", "message": "进度记录对不上，重新说一次书名吧。"}
    total = len(normalize_library_content(chapter["content"]))
    offset = int(progress["char_offset"])
    return {
        "status": "progress",
        "message": (
            f"你正在打《{book['title']}》第 {progress['chapter_index']} 章"
            f"（共 {book['total_chapters']} 章），本章还剩 {max(0, total - offset)} 字。"
        ),
        "book_title": book["title"],
        "chapter_index": int(progress["chapter_index"]),
        "remaining_chars": max(0, total - offset),
    }


def novel_stats(db: sqlite3.Connection, payload: dict) -> dict:
    """这本书的成绩统计。所有者 2026-11-05：成绩要单独保留、每本书一份。"""
    platform, chat_id, requester_id, _ = novel_identity(payload)
    keyword = str(payload.get("book") or "").strip()
    book = novel_find_book(db, keyword) if keyword else None
    if not book:
        progress = db.execute(
            "SELECT book_id FROM novel_progress WHERE platform=? AND chat_id=? AND requester_id=?",
            (platform, chat_id, requester_id),
        ).fetchone()
        if progress:
            book = db.execute("SELECT * FROM novel_books WHERE book_id=?", (progress["book_id"],)).fetchone()
    if not book:
        return {"status": "idle", "message": "还没记录到你在打哪本书，先说书名或先打一段。"}
    row = db.execute(
        "SELECT count(*) AS n, avg(speed) AS speed, avg(accuracy) AS acc, avg(wpm) AS wpm, sum(chars) AS chars "
        "FROM novel_records WHERE book_id=? AND user_id=?",
        (book["book_id"], requester_id),
    ).fetchone()
    if not row or not row["n"]:
        return {"status": "empty", "message": f"《{book['title']}》还没有你的成绩记录（打完会有）。"}
    return {
        "status": "stats",
        "message": (
            f"《{book['title']}》你打了 {row['n']} 段，"
            f"均速 {round(float(row['speed'] or 0), 2)} 击键/秒，"
            f"平均准确率 {round(float(row['acc'] or 0), 2)}%，"
            f"平均打词 {round(float(row['wpm'] or 0), 2)}，"
            f"累计 {int(row['chars'] or 0)} 字。"
        ),
        "count": int(row["n"]),
    }


def library_metadata_summary(metadata: dict, segment_difficulty: str = "", segment_score=None) -> str:
    result = f"整篇难度={metadata.get('difficulty','普')}({float(metadata.get('difficulty_score') or 0):.2f})，题材={metadata.get('primary_genre','综合文学')}，文体={metadata.get('form','小说/叙事')}"
    if segment_difficulty:
        result += f"；当前段={segment_difficulty}({float(segment_score or 0):.2f})"
    return result


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
    score, difficulty = get_library_ranker().rank(content[:600])
    db.execute(
        "INSERT INTO library_rankings (text_id,difficulty,score,ranked_at) VALUES (?, ?, ?, ?) "
        "ON CONFLICT(text_id) DO UPDATE SET difficulty=excluded.difficulty,score=excluded.score,ranked_at=excluded.ranked_at",
        (row["text_id"], difficulty, score, time.time()),
    )
    return score, difficulty


def pick_library_row(db: sqlite3.Connection, query: str, difficulty: str) -> tuple[sqlite3.Row, float, str] | None:
    pattern = f"%{query.strip()}%"
    if difficulty:
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

    candidates = db.execute(
        "SELECT * FROM library_texts WHERE category<>'fm_single_chars' "
        "AND (title LIKE ? OR content LIKE ?) ORDER BY RANDOM() LIMIT ?",
        (pattern, pattern, 300 if difficulty else 50),
    ).fetchall()
    if not candidates:
        return None
    if not difficulty:
        row = random.choice(candidates)
        score, actual = rank_library_row(db, row)
        db.commit()
        return row, score, actual

    ranked = []
    for row in candidates:
        score, actual = rank_library_row(db, row)
        ranked.append((row, score, actual))
        if actual == difficulty:
            db.commit()
            return row, score, actual
    db.commit()
    target = LIBRARY_DIFFICULTIES.index(difficulty)
    return min(ranked, key=lambda item: abs(LIBRARY_DIFFICULTIES.index(item[2]) - target))


def _ensure_library_recent_table(db: sqlite3.Connection) -> None:
    """记录每个会话最近发过的文章，供「文来」排除已发过的。

    2026-10-30：原来是「只排除当前这一篇」，于是 A → B → A 会重复；
    难度高时候选池小，重复得非常明显。
    """
    db.executescript(
        "CREATE TABLE IF NOT EXISTS library_recent_sends ("
        " platform TEXT NOT NULL,"
        " chat_id TEXT NOT NULL,"
        " requester_id TEXT NOT NULL,"
        " text_id TEXT NOT NULL,"
        " sent_at REAL NOT NULL,"
        " PRIMARY KEY (platform, chat_id, requester_id, text_id));"
        "CREATE INDEX IF NOT EXISTS library_recent_sends_idx "
        " ON library_recent_sends(platform, chat_id, requester_id, sent_at DESC);"
    )


LIBRARY_RECENT_KEEP = 40


def remember_library_send(
    db: sqlite3.Connection, platform: str, chat_id: str, requester_id: str, text_id: str
) -> None:
    if not text_id:
        return
    db.execute(
        "INSERT INTO library_recent_sends VALUES (?, ?, ?, ?, ?) "
        "ON CONFLICT(platform, chat_id, requester_id, text_id) DO UPDATE SET sent_at=excluded.sent_at",
        (platform, chat_id, requester_id, text_id, time.time()),
    )
    # 只留最近 N 篇，避免表无限增长（也让排除列表不会被撑爆）
    db.execute(
        "DELETE FROM library_recent_sends WHERE platform=? AND chat_id=? AND requester_id=? "
        "AND text_id NOT IN (SELECT text_id FROM library_recent_sends "
        " WHERE platform=? AND chat_id=? AND requester_id=? ORDER BY sent_at DESC LIMIT ?)",
        (platform, chat_id, requester_id, platform, chat_id, requester_id, LIBRARY_RECENT_KEEP),
    )


def recent_library_text_ids(
    db: sqlite3.Connection, platform: str, chat_id: str, requester_id: str
) -> list[str]:
    try:
        rows = db.execute(
            "SELECT text_id FROM library_recent_sends "
            "WHERE platform=? AND chat_id=? AND requester_id=? ORDER BY sent_at DESC",
            (platform, chat_id, requester_id),
        ).fetchall()
    except sqlite3.Error:
        return []
    return [str(row[0]) for row in rows]


def pick_library_segment(
    db: sqlite3.Connection,
    query: str,
    difficulty: str,
    length: int,
    exclude_text_id: str = "",
    genre: str = "",
    avoid_text_ids: list[str] | None = None,
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
    # 最近发过的也排除掉（2026-10-30：只排除当前这一篇不够，A→B→A 会重复）
    for avoid_id in (avoid_text_ids or [])[:LIBRARY_RECENT_KEEP]:
        if avoid_id and avoid_id != exclude_text_id:
            base_where += " AND l.text_id<>?"
            base_params.append(avoid_id)

    if not difficulty:
        candidates = db.execute(
            f"SELECT l.* FROM library_texts l WHERE {base_where} "
            "AND LENGTH(l.content)>=? ORDER BY RANDOM() LIMIT ?",
            tuple(base_params + [length, 2000 if requested_genre else 200]),
        ).fetchall()
        if not candidates and exclude_text_id:
            candidates = db.execute(
                "SELECT l.* FROM library_texts l WHERE l.category<>'fm_single_chars' "
                "AND (l.title LIKE ? OR l.content LIKE ?) AND LENGTH(l.content)>=? "
                "ORDER BY RANDOM() LIMIT 200",
                (pattern, pattern, length),
            ).fetchall()
        for row in candidates:
            if requested_genre and not library_genre_matches(db, row, requested_genre):
                continue
            content = normalize_library_content(row["content"])
            if len(content) < length:
                continue
            body = content[:length]
            score, actual = get_library_ranker().rank(body)
            return row, body, score, actual
        return None

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
        return pick_library_segment(db, query, difficulty, length)
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
        score, actual = get_library_ranker().rank(body)
        if actual == difficulty:
            return row, body, score, actual
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


def format_library_segment(db: sqlite3.Connection, session: dict, body: str, metadata: dict | None = None) -> tuple[str, int]:
    score = float(session["difficulty_score"])
    score_text = "爆表" if score > 100 else f"{score:.2f}"
    if metadata is None:
        article = db.execute("SELECT * FROM library_texts WHERE text_id=?", (session["text_id"],)).fetchone()
        metadata = classify_library_row(db, article) if article else {"primary_genre": "综合文学", "form": "小说/叙事"}
    label = f"{session['difficulty']}{score_text}·{metadata.get('primary_genre', '综合文学')}·{metadata.get('form', '小说/叙事')}"
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
        "SELECT requested_difficulty,requested_length,requested_genre,auto_continue_same FROM library_session_modes WHERE session_id=?",
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
        "requested_genre=excluded.requested_genre,requested_length=excluded.requested_length,"
        "created_at=excluded.created_at,saved_at=excluded.saved_at",
        (
            platform, chat_id, requester_id, row["text_id"], row["title"],
            row["difficulty"], row["difficulty_score"], row["segment_length"],
            row["next_offset"], row["segment_no"], row["total_chars"],
            requested_difficulty, requested_genre, requested_length, row["created_at"], time.time(),
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
    avoid_text_ids = recent_library_text_ids(db, platform, chat_id, requester_id)
    selected = pick_library_segment(
        db,
        query,
        difficulty,
        length,
        str(payload.get("exclude_text_id") or "").strip(),
        requested_genre,
        avoid_text_ids,
    )
    if not selected and avoid_text_ids:
        # 排除之后一篇都抽不到（池子真的发完了）：退回不排除。
        # 宁可重复一篇，也不能让「文来」直接回答「没有文章」。
        selected = pick_library_segment(
            db, query, difficulty, length, str(payload.get("exclude_text_id") or "").strip(), requested_genre
        )
    if not selected:
        detail = f"“{difficulty}”难度区间" if difficulty else "要求"
        return {"status": "not_found", "message": f"没有找到符合{detail}的文库文章。"}
    row, body, score, actual_difficulty = selected
    content = normalize_library_content(row["content"])
    if not body:
        return {"status": "not_found", "message": "找到的文章没有可发送正文。"}
    metadata = classify_library_row(db, row)
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
        "INSERT INTO library_session_modes (session_id,requested_difficulty,requested_length,requested_genre,auto_continue_same) "
        "VALUES (?, ?, ?, ?, 0) ON CONFLICT(session_id) DO UPDATE SET "
        "requested_difficulty=excluded.requested_difficulty,requested_length=excluded.requested_length,requested_genre=excluded.requested_genre,auto_continue_same=0",
        (session["session_id"], difficulty, fixed_length, requested_genre),
    )
    db.commit()
    message, segment_id = format_library_segment(db, session, body)
    remember_library_send(db, platform, chat_id, requester_id, str(row["text_id"]))
    db.commit()
    return {
        "status": "segment", "message": message, "segment_id": segment_id,
        "session_id": session["session_id"], "title": row["title"],
        "difficulty": actual_difficulty, "score": score, "segment_no": 1,
        "next_offset": end, "total_chars": len(content), "completed": end >= len(content),
        "genre": metadata["primary_genre"], "genres": metadata["genres"], "form": metadata["form"],
        "article_difficulty": metadata["difficulty"], "article_score": metadata["difficulty_score"],
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
    article = db.execute("SELECT * FROM library_texts WHERE text_id=?", (row["text_id"],)).fetchone()
    metadata = classify_library_row(db, article) if article else {"primary_genre": "综合文学", "genres": ["综合文学"], "form": "小说/叙事", "difficulty": actual_difficulty, "difficulty_score": score, "confidence": 0}
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
        "genre": metadata["primary_genre"], "genres": metadata["genres"], "form": metadata["form"],
        "article_difficulty": metadata["difficulty"], "article_score": metadata["difficulty_score"],
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
        "INSERT INTO library_session_modes (session_id,requested_difficulty,requested_length,requested_genre,auto_continue_same) "
        "VALUES (?,?,?,?,1) ON CONFLICT(session_id) DO UPDATE SET "
        "requested_difficulty=excluded.requested_difficulty,requested_length=excluded.requested_length,requested_genre=excluded.requested_genre,auto_continue_same=1",
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
    if row["session_kind"] == "single":
        result = continue_single_session(db, identity)
    elif row["session_kind"] == "contest":
        result = continue_contest_session(db, identity)
    else:
        mode = db.execute(
            "SELECT auto_continue_same FROM library_session_modes WHERE session_id=?",
            (row["session_id"],),
        ).fetchone()
        if mode and int(mode["auto_continue_same"] or 0):
            result = continue_same_library_session(db, identity)
            if result.get("status") == "article_completed":
                db.execute(
                    "UPDATE library_session_modes SET auto_continue_same=0 WHERE session_id=?",
                    (row["session_id"],),
                )
                result = continue_library_session(db, identity)
        else:
            result = continue_library_session(db, identity)
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
    totals = db.execute("SELECT COUNT(*) AS texts,COALESCE(SUM(char_count),0) AS chars FROM library_texts").fetchone()
    active = sum(
        db.execute(f"SELECT COUNT(*) FROM {table} WHERE status='active'").fetchone()[0]
        for table in ("library_sessions", "single_sessions")
    )
    ranked = sum(difficulties.values())
    genre_rows = db.execute(
        "SELECT primary_genre AS name,COUNT(*) AS count FROM library_classifications "
        "WHERE TRIM(primary_genre)<>'' GROUP BY primary_genre ORDER BY primary_genre"
    ).fetchall()
    genres = [{"name": row["name"], "count": int(row["count"])} for row in genre_rows]
    genre_count = len(genres)
    return {
        "texts": totals["texts"], "characters": totals["chars"], "categories": categories,
        "genre_count": int(genre_count),
        "genres": genres,
        "ranked_texts": ranked, "unranked_texts": max(0, totals["texts"] - ranked),
        "difficulties": difficulties, "active_sessions": active,
    }


def library_session_status(db: sqlite3.Connection, payload: dict) -> dict:
    platform, chat_id, requester_id, _ = library_payload_identity(payload)
    article = db.execute(
        "SELECT s.*,m.requested_difficulty,m.requested_length FROM library_sessions s "
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
        "requested_length,saved_at FROM library_previous_sessions "
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
    return {"ranked": len(rows), "difficulties": counts, **library_stats(db)}


def classify_library(db: sqlite3.Connection, limit: int = 0) -> dict:
    query = "SELECT l.* FROM library_texts l LEFT JOIN library_classifications c ON c.text_id=l.text_id WHERE c.text_id IS NULL OR c.classifier_version<>? ORDER BY l.text_id"
    params = [LIBRARY_CLASSIFIER_VERSION]
    if limit > 0:
        query += " LIMIT ?"
        params.append(limit)
    rows = db.execute(query, params).fetchall()
    counts = {}
    for index, row in enumerate(rows, 1):
        metadata = classify_library_row(db, row, refresh=True)
        counts[metadata["primary_genre"]] = counts.get(metadata["primary_genre"], 0) + 1
        if index % 200 == 0:
            db.commit()
    db.commit()
    return {"classified": len(rows), "genres": counts, **library_stats(db)}


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
            "INSERT INTO ai_contest_scores VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) "
            "ON CONFLICT(record_id) DO UPDATE SET source_json=excluded.source_json",
            (
                record_id, str(item.get("competition_date") or ""), canonical_user_id(item.get("user_id")),
                str(item.get("user_name") or ""), str(item.get("group_id") or ""),
                float(item.get("speed") or 0), float(item.get("key") or 0),
                float(item.get("acc") or 0), float(item.get("ts") or 0),
                json.dumps(item, ensure_ascii=False),
                # 从原始跟打行里取。取不到就是 None（NULL），**不要写 0**。
                _raw_score_field(item.get("raw"), "码长"),
                _raw_retype(item.get("raw")),
                _raw_score_field(item.get("raw"), "打词"),
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
        "SELECT user_id,source_json,code_length,retype,word_rate FROM ai_contest_scores "
        "WHERE competition_date=? AND speed>0 ORDER BY speed DESC,occurred_at ASC",
        (competition_date,),
    ).fetchall()
    best = {}
    for row in score_rows:
        user_id = str(row["user_id"] or "")
        if user_id and user_id not in best:
            item = json.loads(row["source_json"])
            # 新列直接用表里已解析好的字段，不再从原文抠一遍
            item["code_length"] = row["code_length"]
            item["retype"] = row["retype"]
            item["word_rate"] = row["word_rate"]
            best[user_id] = item
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
    width = 1240
    row_height = 74
    height = 230 + max(1, len(rows)) * row_height + 70
    image = Image.new("RGB", (width, height), (20, 24, 31))
    draw = ImageDraw.Draw(image)
    draw.rectangle((0, 0, width, 12), fill=(78, 205, 181))
    draw.text((48, 38), "FM AI赛文排行榜", font=font(40), fill=(244, 247, 252))
    draw.text((48, 98), f"{competition_date or '日期未知'}  |  《{title}》  |  第555段", font=font(22), fill=(159, 172, 193))
    columns = [(52, "排名"), (120, "成员"), (430, "速度"), (580, "击键"), (700, "码长"), (820, "打词率"), (950, "重打"), (1060, "键准")]
    draw.rounded_rectangle((40, 156, 1200, 214), radius=8, fill=(37, 46, 60))
    for x, label in columns:
        draw.text((x, 168), label, font=font(20), fill=(184, 196, 214))
    if not rows:
        draw.text((52, 240), "当天还没有保存的成绩。", font=font(24), fill=(184, 196, 214))
    for index, item in enumerate(rows, 1):
        top = 224 + (index - 1) * row_height
        fill = (29, 36, 47) if index % 2 else (25, 31, 41)
        draw.rounded_rectangle((40, top, 1200, top + 62), radius=7, fill=fill)
        rank_color = (247, 199, 95) if index <= 3 else (219, 226, 237)
        name = str(item.get("user_name") or item.get("user_id") or "unknown")[:24]
        draw.text((58, top + 17), str(index), font=font(22), fill=rank_color)
        draw.text((120, top + 17), name, font=font(22), fill=(235, 239, 245))
        speed = float(item.get("speed") or 0)
        accuracy = float(item.get("acc") or 0)
        draw.text((430, top + 17), f"{speed:.2f}", font=font(22), fill=speed_color(speed))
        draw.text((580, top + 17), f"{float(item.get('key') or 0):.2f}", font=font(22), fill=(113, 164, 255))
        draw.text((700, top + 17), f"{float(item.get('code_length') or 0):.2f}", font=font(22), fill=(159, 172, 193))
        draw.text((820, top + 17), f"{float(item.get('word_rate') or 0):.2f}%", font=font(22), fill=(159, 172, 193))
        draw.text((950, top + 17), f"{float(item.get('retype') or 0):.0f}", font=font(22), fill=(159, 172, 193))
        draw.text((1060, top + 17), f"{accuracy:.2f}%", font=font(22), fill=accuracy_color(accuracy))
    footer = f"共 {len(rows)} 人 | 每人保留当日最高速度"
    draw.text((48, height - 48), footer, font=font(18), fill=(128, 141, 160))
    output = io.BytesIO()
    image.save(output, "PNG", optimize=True)
    return output.getvalue()


def render_competition_score(db: sqlite3.Connection, user_id: str, name: str, source: str) -> bytes:
    from PIL import Image, ImageDraw, ImageFont

    contest_title = ""
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
    # 2026-11-15 修复：这张图的表头引用了 _detail_label，但那个变量只在
    # render_score_chart 里定义过 —— 独立调用本函数必 NameError，
    # 后台 /api/domain/reports/competition-score.png 之前就是因此 503 的。
    _detail_label = lambda label: label  # noqa: E731
    columns = [(50, _detail_label("日期")), (245, _detail_label("来源")), (610, _detail_label("速度")), (750, _detail_label("击键")), (884, _detail_label("键准"))]
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


def ai_contest_score_history(db: sqlite3.Connection, user_id: str, name: str, days: int) -> list[dict]:
    """555 赛文的历史成绩，字段名对齐 render_score_chart 的期望（speed/acc/key/…）。

    时间窗口用 SQL 的 date('now', '-N days') 算，省得在这里再 import 一个 datetime。
    """
    rows = db.execute(
        "SELECT competition_date,user_name,speed,key_rate,accuracy,code_length,word_rate,retype "
        "FROM ai_contest_scores "
        "WHERE competition_date >= date('now', '-' || ? || ' days') "
        # 按 user_id 匹配；**只有 name 非空时**才把名字算进条件。
        # 原来最后一项是 f"%{name}%"，name 传空时得到 '%%' —— 匹配所有人，
        # 于是所有者要自己的曲线，画出来却是全群的（标题显示最后一条的人名）。
        # 两侧都要判空：user_id 空时 LIKE '%%' 会匹配所有人，name 空时同理。
        # 实测过两次翻车：只修 name 那侧 → 「只给名字」画出 17 个人的混合曲线 ✗。
        "  AND ((? != '' AND user_id LIKE ?) OR (? != '' AND user_name LIKE ?)) "
        "ORDER BY competition_date ASC",
        (max(1, int(days or 3650)), user_id, f"%{user_id}%", name, f"%{name}%"),
    ).fetchall()
    # 每天只留一条（当日最快）。所有者要的是「每天 555 的成绩曲线」——
    # 同一天打多次就有多个点，曲线会糊成一团。这和榜单「每人保留当日最高速度」一致。
    best_per_day = {}
    for row in rows:
        day = row["competition_date"]
        if day not in best_per_day or float(row["speed"] or 0) > float(best_per_day[day]["speed"] or 0):
            best_per_day[day] = row
    rows = [best_per_day[day] for day in sorted(best_per_day)]

    return [
        {
            "competition_date": row["competition_date"],
            "user_name": row["user_name"],
            "name": row["user_name"],
            "speed": row["speed"],
            "acc": row["accuracy"],
            "key": row["key_rate"],
            "code_length": row["code_length"],
            "word_rate": row["word_rate"],
            "retype": row["retype"],
            # 明细表那一列原来显示「未知」—— 555 的记录里没有赛事字段。
            # 几个可能的键都填上，免得去猜渲染层读的是哪一个。
            "source": "555赛文",
            "source_group": "555赛文",
            "competition": "555赛文",
            "contest": "555赛文",
        }
        for row in rows
    ]


def _last_score_date(db, user_id, name, is_555):
    """这个人在这项赛事里最近一次是哪天、一共多少条（不受时间窗口限制）。"""
    table = "ai_contest_scores" if is_555 else "competition_scores"
    try:
        row = db.execute(
            "SELECT max(competition_date), count(*) FROM " + table + " "
            "WHERE (? != '' AND user_id LIKE ?) OR (? != '' AND user_name LIKE ?)",
            (user_id, "%" + user_id + "%", name, "%" + name + "%"),
        ).fetchone()
    except Exception:
        return None, 0
    if not row or not row[0]:
        return None, 0
    return row[0], int(row[1] or 0)


def contest_live_rows(contest_key: str, group_id: str) -> list[dict]:
    """从官网 /competition/live 拿实时排名。拿不到就返回空 list（上层改用成绩库）。"""
    source = CONTEST_LIVE_SOURCES.get(contest_key)
    if not source and contest_key == "999" and group_id in CONTEST_GROUP_LIVE_GROUPS:
        source = group_id
    if not source:
        return []
    # 显式导入：app.py 顶部没有 import urllib.request，
    # 之前这里抛 NameError 被 except 吞掉，导致「官网优先」永远静默回落成绩库。
    import urllib.request as urllib_request
    url = f"http://127.0.0.1:8077/competition/live?source={source}&group_id={group_id}&soft=1"
    try:
        with urllib_request.urlopen(url, timeout=15) as response:
            payload = json.load(response)
    except Exception:
        return []
    if payload.get("status") != "ok":
        return []
    return payload.get("rows") or []


def contest_day_stats(db: sqlite3.Connection, contest_key: str, group_id: str, scope: str,
                      days: int, user: str = "", use_live: bool = True) -> list[dict]:
    """取某赛文按天的统计。官网优先，失败回落成绩库。

    返回 [{date, avg_speed, best_speed, avg_key_rate, avg_accuracy, users}]，按日期升序。
    2026-11-15 修复：原来的「回落成绩库」分支只剩 `return []`（真取数代码被删了），
    导致非官网赛文（555/666/777/groupmatch）的曲线永远是空的。
    """
    import urllib.request as urllib_request
    import json as json_module

    source = CONTEST_LIVE_SOURCES.get(contest_key)
    if not source and contest_key == "999" and group_id in CONTEST_GROUP_LIVE_GROUPS:
        source = group_id

    points: list[dict] = []
    today = china_now().date()
    start = today - timedelta(days=max(1, days) - 1)

    # ① 官网逐日取（支持 date 参数，实测能回溯一年）
    if source and use_live:
        day = start
        while day <= today:
            url = (f"http://127.0.0.1:8077/competition/live?source={source}"
                   f"&soft=1&date={day.isoformat()}")
            try:
                with urllib_request.urlopen(url, timeout=12) as response:
                    payload = json_module.load(response)
            except Exception:
                payload = {}
            rows = (payload.get("rows") or []) if payload.get("status") == "ok" else []
            if user:
                # 只看这个人（所有者 2026-11-06：问的肯定是个人成绩曲线）
                # 模糊匹配：官网的名字常带群头衔（「云南」Fixmood（三当家）」），
                # 用户只会传「Fixmood」，所以用包含判断。
                rows = [row for row in rows
                        if user in str(row.get("name") or "")
                        or str(row.get("user_id") or "") == user]
            speeds: list[float] = []
            keys: list[float] = []
            accs: list[float] = []
            for row in rows:
                try:
                    if row.get("speed") is not None:
                        speeds.append(float(row["speed"]))
                except (TypeError, ValueError):
                    pass
                for field, bucket in (("key", keys), ("keystroke", keys),
                                      ("acc", accs), ("accuracy", accs)):
                    try:
                        value = row.get(field)
                        if value is not None:
                            bucket.append(float(str(value).replace("%", "")))
                    except (TypeError, ValueError):
                        pass
            if speeds:
                points.append({
                    "date": day.isoformat(),
                    "avg_speed": round(sum(speeds) / len(speeds), 2),
                    "best_speed": round(max(speeds), 2),
                    "avg_key_rate": round(sum(keys) / len(keys), 2) if keys else None,
                    "avg_accuracy": round(sum(accs) / len(accs), 2) if accs else None,
                    "users": len(speeds),
                })
            day += timedelta(days=1)
        if points:
            return points

    # ② 回落成绩库（同一份 contest_scores，/contest/chart.png 的柱状图也用它）
    room = group_id if scope == "group" else ""
    since = (today - timedelta(days=max(1, days) - 1)).isoformat()
    user_clause, user_params = "", []
    if user:
        fragments = [f for f in re.split(r"[「」『』（）()\[\]【】\s]+", user) if len(f) >= 2]
        latin = [f for f in fragments if re.fullmatch(r"[A-Za-z0-9_]+", f)]
        best_fragment = max(latin or fragments, key=len) if fragments else ""
        clauses = ["user_name LIKE ?", "user_id = ?"]
        user_params = [f"%{user}%", user]
        if best_fragment and best_fragment != user:
            clauses.append("user_name LIKE ?")
            user_params.append(f"%{best_fragment}%")
        user_clause = " AND (" + " OR ".join(clauses) + ")"
    rows = db.execute(
        "SELECT score_date AS date, AVG(speed) AS avg_speed, MAX(speed) AS best_speed, "
        "AVG(key_rate) AS avg_key_rate, AVG(accuracy) AS avg_accuracy, COUNT(*) AS users "
        "FROM contest_scores WHERE contest_key=? AND group_id=? AND score_date<>'' "
        "AND score_date>=?" + user_clause +
        " GROUP BY score_date ORDER BY score_date",
        tuple([contest_key, room, since] + user_params),
    ).fetchall()
    return [
        {"date": row["date"],
         "avg_speed": round(float(row["avg_speed"] or 0), 2),
         "best_speed": round(float(row["best_speed"] or 0), 2),
         "avg_key_rate": maybe_float(row["avg_key_rate"]),
         "avg_accuracy": maybe_float(row["avg_accuracy"]),
         "users": int(row["users"] or 0)}
        for row in rows
    ]



def render_contest_trend(db: sqlite3.Connection, contest_key: str, group_id: str,
                         contest_name: str, scope: str, days: int, user: str = "") -> bytes:
    """按天画速度曲线（平均 + 最快两条线）。"""
    from PIL import Image, ImageDraw, ImageFont
    import datetime

    points = contest_day_stats(db, contest_key, group_id, scope, days, user)
    room = group_id if scope == "group" else ""
    if not points:
        # 官网没有就回落成绩库（按 score_date 聚合）
        room_key = group_id if scope == "group" else ""
        user_clause, user_params = "", []
        if user:
            user_clause = " AND (user_name LIKE ? OR user_id=?)"
            user_params = [f"%{user}%", user]
        rows = db.execute(
            "SELECT score_date, avg(speed) AS avg_speed, max(speed) AS best_speed, "
            "count(*) AS users FROM contest_scores "
            "WHERE contest_key=? AND group_id=? AND score_date<>''" + user_clause +
            " GROUP BY score_date ORDER BY score_date DESC LIMIT ?",
            tuple([contest_key, room_key] + user_params + [max(1, days)]),
        ).fetchall()
        points = [
            {"date": r["score_date"], "avg_speed": float(r["avg_speed"] or 0),
             "best_speed": float(r["best_speed"] or 0), "users": r["users"]}
            for r in reversed(rows)
        ]

    width, height = 960, 540
    image = Image.new("RGB", (width, height), (24, 26, 32))
    draw = ImageDraw.Draw(image)
    font_paths = [os.environ.get("FM_REPORT_FONT", "/assets/msyh.ttc"),
                  "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"]

    def font(size: int):
        for path in font_paths:
            try:
                return ImageFont.truetype(path, size)
            except (OSError, ValueError):
                continue
        return ImageFont.load_default()

    who = f"{user} · " if user else ""
    title = f"{who}{contest_name}" + (f"（群 {room}）" if room else "（全群）") + " · 成绩曲线"
    draw.text((32, 24), title, fill=(238, 240, 245), font=font(32))
    if points:
        span = f"{points[0]['date']} ~ {points[-1]['date']} · {len(points)} 天"
    else:
        span = f"最近 {days} 天"
    draw.text((32, 68), span, fill=(150, 158, 172), font=font(20))

    if not points:
        missing = f"{user} 在这段时间" if user else "这段时间"
        draw.text((32, height // 2), f"{missing}还没有成绩记录。", fill=(200, 120, 120), font=font(28))
        buffer = io.BytesIO()
        image.save(buffer, format="PNG", optimize=True)
        return buffer.getvalue()

    left, right, top, bottom = 90, width - 60, 130, height - 90
    max_speed = max(p["best_speed"] for p in points) or 1.0
    min_speed = min(p["avg_speed"] for p in points)
    floor = max(0.0, min_speed - 20)
    span_speed = max(1.0, max_speed - floor)

    # 网格 + Y 轴
    for step in range(5):
        ratio = step / 4
        y = int(bottom - (bottom - top) * ratio)
        value = floor + span_speed * ratio
        draw.line([(left, y), (right, y)], fill=(44, 48, 58), width=1)
        draw.text((16, y - 10), f"{value:.0f}", fill=(140, 148, 160), font=font(18))

    def x_of(index: int) -> int:
        if len(points) == 1:
            return (left + right) // 2
        return int(left + (right - left) * index / (len(points) - 1))

    def y_of(speed: float) -> int:
        return int(bottom - (bottom - top) * (speed - floor) / span_speed)

    for key, color in (("best_speed", (120, 180, 255)), ("avg_speed", (96, 220, 160))):
        coords = [(x_of(i), y_of(p[key])) for i, p in enumerate(points)]
        if len(coords) > 1:
            draw.line(coords, fill=color, width=3, joint="curve")
        for x, y in coords:
            draw.ellipse((x - 4, y - 4, x + 4, y + 4), fill=color)

    # X 轴标签（最多 8 个）
    label_step = max(1, len(points) // 8)
    for i in range(0, len(points), label_step):
        draw.text((x_of(i) - 28, bottom + 12), points[i]["date"][5:], fill=(150, 158, 172), font=font(16))

    # 图例
    draw.line([(left, height - 34), (left + 28, height - 34)], fill=(120, 180, 255), width=3)
    draw.text((left + 36, height - 44), "单日最快", fill=(190, 198, 210), font=font(18))
    draw.line([(left + 150, height - 34), (left + 178, height - 34)], fill=(96, 220, 160), width=3)
    draw.text((left + 186, height - 44), "单日平均", fill=(190, 198, 210), font=font(18))

    buffer = io.BytesIO()
    image.save(buffer, format="PNG", optimize=True)
    return buffer.getvalue()

def render_contest_chart(db: sqlite3.Connection, contest_key: str, group_id: str,
                        contest_name: str, scope: str, days: int, metric: str) -> bytes:
    """把一个赛文的成绩画成横向柱状图（最快的前 N 人）。

    数据全部来自 contest_scores —— 网站挂了也能出图（所有者 2026-11-06 的要求）。
    """
    from PIL import Image, ImageDraw, ImageFont

    room = group_id if scope == "group" else ""

    # 取数与 /contest/chart-data 共用同一个函数（官网优先，取不到回落成绩库）。
    rows, source_label = contest_rank_rows(db, contest_key, group_id, scope, days)

    width, row_height = 900, 54
    header = 120
    footer = 50
    height = header + max(1, len(rows)) * row_height + footer
    image = Image.new("RGB", (width, height), (24, 26, 32))
    draw = ImageDraw.Draw(image)

    # 用和现有成绩图一样的中文字体（/assets/msyh.ttc = 微软雅黑），
    # 2026-11-06 第一版用了 DejaVu，中文全变方块。
    font_paths = [
        os.environ.get("FM_REPORT_FONT", "/assets/msyh.ttc"),
        "/assets/fonts/NotoSansCJK-Regular.ttc",
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    ]

    def font(size: int):
        for path in font_paths:
            try:
                return ImageFont.truetype(path, size)
            except (OSError, ValueError):
                continue
        return ImageFont.load_default()

    title = f"{contest_name}" + (f"（群 {room}）" if room else "（全群）")
    draw.text((32, 26), title, fill=(238, 240, 245), font=font(34))
    subtitle = f"最近 {days} 天" if days > 0 else "全部时间"
    subtitle += " · 按最快成绩排序" if metric == "speed" else " · 按平均键准排序"
    subtitle += f" · 数据来源：{source_label}"
    subtitle += f" · 共 {sum(r['n'] for r in rows)} 条成绩" if rows else ""
    draw.text((32, 74), subtitle, fill=(150, 158, 172), font=font(22))

    if not rows:
        draw.text((32, header), "还没有这个赛文的成绩记录。", fill=(200, 120, 120), font=font(28))
    else:
        best_speed = max(float(r["best"] or 0) for r in rows) or 1.0
        bar_left, bar_width = 330, 450
        for index, row in enumerate(rows):
            y = header + index * row_height
            raw_name = str(row["user_name"] or row["user_id"] or "?")
            name = raw_name
            while name and draw.textlength(name, font=font(24)) > 250:
                name = name[:-1]
            if name != raw_name:
                name = name + "…"
            speed = float(row["best"] or 0)
            ratio = speed / best_speed
            draw.text((32, y + 8), f"{index + 1}. {name}", fill=(224, 228, 236), font=font(24))
            draw.rounded_rectangle(
                (bar_left, y + 8, bar_left + bar_width, y + 40),
                radius=6,
                fill=(38, 42, 52),
            )
            draw.rounded_rectangle(
                (bar_left, y + 8, bar_left + max(6, int(bar_width * ratio)), y + 40),
                radius=6,
                fill=(86, 156, 255),
            )
            draw.text(
                (bar_left + bar_width + 14, y + 10),
                f"{speed:.2f}",
                fill=(240, 244, 250),
                font=font(24),
            )
    # 底部说明要跟实际数据源一致 —— 之前不管哪种都写「成绩库」，走官网时是误导。
    footer_text = (
        "FM · 官网实时数据" if source_label == "官网实时"
        else "FM · 成绩库（官网取不到时以此为准）"
    )
    draw.text((32, height - footer + 8), footer_text, fill=(110, 116, 128), font=font(18))

    buffer = io.BytesIO()
    image.save(buffer, format="PNG", optimize=True)
    return buffer.getvalue()


def contest_chart_lookup(db: sqlite3.Connection, query: dict) -> tuple[str, str, str, str] | None:
    """把 query 里的 contest 参数解析成 (key, name, scope, group_id)。"""
    label = str(query.get("contest", [""])[0] if isinstance(query.get("contest"), list) else query.get("contest") or "").strip()
    if not label:
        return None
    # 自然说法 → 段号（2026-11-06：模型把「梦幻打字阁的群赛文」认成了 555，
    # 这里让 domain 直接认这些说法，不指望模型每次都猜对）。
    aliases = {
        "群赛文": "999", "本群赛文": "999", "这个群的赛文": "999", "群里的赛文": "999",
        "梦幻打字阁": "999", "五笔修炼基地": "999", "帝隆": "999",
        "092五笔": "999", "仓颉之友": "999", "倉頡之友": "999", "小鹤进修班": "999",
        "555": "555", "555赛文": "555", "ai赛文": "555", "ai赛文555": "555",
        "晴分身": "777", "晴分身赛文": "777", "777赛文": "777",
        "1847992065": "groupmatch", "groupmatch赛文": "groupmatch",
        "1916996679": "666", "666赛文": "666",
        "虎杯": "HB", "锦标赛": "100000", "极速杯": "99999", "极速联赛": "100004",
    }
    lowered = label.lower()
    if lowered in aliases:
        label = aliases[lowered]
    for key, (name, scope) in CONTEST_LABELS.items():
        if label in (key, name):
            group_id = str(query.get("group_id", [""])[0] if isinstance(query.get("group_id"), list) else query.get("group_id") or "")
            return key, name, scope, (group_id if scope == "group" else "")
    return None

CONTEST_ALIASES = {
    "群赛文": "999", "本群赛文": "999", "这个群的赛文": "999", "群里的赛文": "999", "本群的赛文": "999",
    "群寒文": "999", "寒文": "999",
    "梦幻打字阁": "999", "五笔修炼基地": "999", "帝隆": "999",
    "092五笔": "999", "仓颉之友": "999", "倉頡之友": "999", "小鹤进修班": "999",
    "555": "555", "555赛文": "555", "ai赛文": "555",
    "晴分身": "777", "晴分身赛文": "777", "777赛文": "777",
    "1847992065": "groupmatch", "groupmatch赛文": "groupmatch",
    "1916996679": "666", "666赛文": "666",
    "虎杯": "HB", "锦标赛": "100000", "极速杯": "99999", "极速联赛": "100004",
}


# 群名 → 群号（所有者 2026-11-06：不想记群号，直接说群名）
GROUP_NAME_ALIASES = {
    "梦幻打字阁": "776227233", "梦幻": "776227233",
    "五笔修炼基地": "1021522088", "五笔修炼": "1021522088",
    "帝隆": "151040026",
    "092五笔": "391047371", "092": "391047371",
    "仓颉之友": "201323122", "倉頡之友": "201323122",
    "小鹤进修班": "488748631", "小鹤": "488748631",
    "虎码训练营": "21037015", "虎码训练": "21037015", "虎营": "21037015",
    "剑气冲霄堂": "1025725817", "剑气": "1025725817",
    "五林风": "697015277",
}


# 群号 → 群名（标题里要写清楚是哪个群，2026-11-06）
GROUP_NAMES_BY_ID = {
    "776227233": "梦幻打字阁",
    "1021522088": "五笔修炼基地",
    "151040026": "帝隆",
    "391047371": "092五笔",
    "201323122": "仓颉之友",
    "488748631": "小鹤进修班",
    "21037015": "虎码训练营",
    "1025725817": "剑气冲霄堂",
    "697015277": "五林风",
    "906230260": "魔然魔改自然码",
    "216063968": "FM测试群2",
    "158408350": "FM测试群",
}


def group_display_name(db: sqlite3.Connection, group_id: str) -> str:
    """群号 → 群名。别名表优先（groups.display_name 常带后缀，如「梦幻打字阁&集训营」）。"""
    if not group_id:
        return ""
    if group_id in GROUP_NAMES_BY_ID:
        return GROUP_NAMES_BY_ID[group_id]
    row = db.execute("SELECT display_name FROM groups WHERE group_id=?", (group_id,)).fetchone()
    if row and row[0]:
        return str(row[0])
    return group_id


def resolve_group_id(db: sqlite3.Connection, value: str) -> str:
    """群号原样返回；群名就查表。认不出返回空串（上层用当前群）。"""
    raw = (value or "").strip()
    if not raw:
        return ""
    if raw.isdigit():
        return raw
    if raw in GROUP_NAME_ALIASES:
        return GROUP_NAME_ALIASES[raw]
    lowered = raw.lower()
    for name, gid in GROUP_NAME_ALIASES.items():
        if lowered in name.lower() or name.lower() in lowered:
            return gid
    row = db.execute(
        "SELECT group_id FROM groups WHERE display_name LIKE ? LIMIT 1", (f"%{raw}%",)
    ).fetchone()
    if row:
        return str(row[0])
    return ""


def resolve_contest_key(value: str) -> str:
    """把自然说法或段号解析成段号。认不出就返回空串。"""
    raw = (value or "").strip()
    if not raw:
        return ""
    if raw in CONTEST_LABELS:
        return raw
    lowered = raw.lower()
    if lowered in CONTEST_ALIASES:
        return CONTEST_ALIASES[lowered]
    for key, (name, _scope) in CONTEST_LABELS.items():
        if raw == name:
            return key
    return ""


def contest_score_history(db: sqlite3.Connection, contest_key: str, group_id: str,
                          user: str, days: int) -> list[dict]:
    """赛文成绩历史，返回 render_score_chart 认的形状。

    2026-11-06：所有者说老那张图比我自己画的曲线好看得多，所以赛文曲线也走老渲染器，
    这里只负责把 contest_scores 的数据整成同一形状。
    """
    scope = CONTEST_LABELS.get(contest_key, ("", "global"))[1]
    room = group_id if scope == "group" else ""
    clause, params = "", []
    if days > 0:
        # 按成绩日期过滤，不是入库时间 —— 回填的数据 recorded_at 全是入库时刻，
        # 用它过滤等于「全部时间」（2026-11-06 所有者实测：问一个月画出了五个月）。
        from datetime import date, timedelta
        since = (date.today() - timedelta(days=days - 1)).isoformat()
        clause = " AND score_date >= ?"
        params = [since]
    base_sql = (
        "SELECT score_date, speed, accuracy, key_rate, code_length, word_rate, "
        "user_name, contest_name FROM contest_scores "
        "WHERE contest_key=? AND group_id=?" + clause
    )
    order_sql = " ORDER BY score_date ASC, speed ASC"

    def query(user_filter: str, user_params: list) -> list:
        return db.execute(
            base_sql + user_filter + order_sql,
            tuple([contest_key, room] + params + user_params),
        ).fetchall()

    rows: list = []
    if not user:
        rows = query("", [])
    else:
        # 先按完整名字查；查不到再用名字片段查一次。
        # 2026-11-06：官网存的是「Fixmood」，群名片是「「云南」Fixmood（三当家）」，
        # 完整匹配永远 0 条，图就成了空白。
        # 全名 + 最佳片段一起 OR（2026-11-06：只按全名查会漏，之前还「1 条」了）
        fragments = [f for f in re.split(r"[「」『』（）()\[\]【】\s]+", user) if len(f) >= 2]
        latin = [f for f in fragments if re.fullmatch(r"[A-Za-z0-9_]+", f)]
        best_fragment = max(latin or fragments, key=len) if fragments else ""
        clauses = ["user_name LIKE ?", "user_id = ?"]
        params_user = [f"%{user}%", user]
        if best_fragment and best_fragment != user:
            clauses.append("user_name LIKE ?")
            params_user.append(f"%{best_fragment}%")
        rows = query(" AND (" + " OR ".join(clauses) + ")", params_user)
    return [
        {
            "competition_date": row["score_date"],
            "speed": row["speed"],
            "acc": row["accuracy"],
            "key_rate": row["key_rate"],
            "code_length": row["code_length"],
            "word_rate": row["word_rate"],
            "source_group": row["contest_name"],
            "user_name": row["user_name"],
        }
        for row in rows
    ]


def all_typing_history(db: sqlite3.Connection, user: str, days: int) -> list[dict]:
    """不分赛文的「所有打字成绩」：**一条成绩一个点**，不做按天平均。

    所有者 2026-11-06：「不是要当天的均速，怎么问的就怎么显示就行了」。
    点多的时候靠 2200px 宽画布 + 抽样标签，不做聚合。
    """
    from datetime import date, timedelta

    since = date.today() - timedelta(days=max(1, days) - 1)
    params: list = [time.mktime(since.timetuple())]
    clauses = ["recorded_at >= ?"]
    if user:
        fragments = [f for f in re.split(r"[「」『』（）()\[\]【】\s]+", user) if len(f) >= 2]
        latin = [f for f in fragments if re.fullmatch(r"[A-Za-z0-9_]+", f)]
        best = max(latin or fragments, key=len) if fragments else ""
        user_clauses = ["user_name LIKE ?", "user_id = ?"]
        user_params = [f"%{user}%", user]
        if best and best != user:
            user_clauses.append("user_name LIKE ?")
            user_params.append(f"%{best}%")
        clauses.append("(" + " OR ".join(user_clauses) + ")")
        params += user_params
    rows = db.execute(
        "SELECT substr(date(recorded_at, 'unixepoch', '+8 hours'), 6) AS day, "
        "speed, accuracy, key_rate, recorded_at "
        "FROM typing_scores WHERE " + " AND ".join(clauses) +
        " ORDER BY recorded_at",
        tuple(params),
    ).fetchall()
    return [
        {"competition_date": row["day"], "speed": row["speed"], "acc": row["accuracy"],
         "key_rate": row["key_rate"]}
        for row in rows if row["speed"] is not None
    ]


def contest_top_response(db: sqlite3.Connection, query: dict) -> dict:
    """某赛文最快的人（回答「谁最快/排名」这类问题）。"""
    contest_raw = (query.get("contest", [""])[0] or "").strip()
    key = resolve_contest_key(contest_raw)
    if not key:
        return {"status": "unknown_contest", "known": list(CONTEST_LABELS), "message": "不清楚是哪个赛文。"}
    name, scope = CONTEST_LABELS.get(key, (contest_raw, "global"))
    group = resolve_group_id(db, (query.get("group_id", [""])[0] or "").strip())
    room = group if scope == "group" else ""
    try:
        days = int(query.get("days", ["0"])[0] or 0)
    except ValueError:
        days = 0
    try:
        limit = max(1, min(20, int(query.get("limit", ["5"])[0] or 5)))
    except ValueError:
        limit = 5
    clause, params = "", []
    if days > 0:
        from datetime import date, timedelta
        since = (date.today() - timedelta(days=days - 1)).isoformat()
        clause = " AND score_date >= ?"
        params = [since]
    rows = db.execute(
        "SELECT user_name, speed, score_date FROM contest_scores "
        "WHERE contest_key=? AND group_id=?" + clause,
        tuple([key, room] + params),
    ).fetchall()

    # 同一人多个群名片要合并
    def canonical(value: str) -> str:
        parts = [p for p in re.split(r"[「」『』（）()\[\]【】\s]+", value or "") if len(p) >= 2]
        latin = [p for p in parts if re.fullmatch(r"[A-Za-z0-9_]+", p)]
        return max(latin, key=len) if latin else (max(parts, key=len) if parts else (value or ""))

    best: dict = {}
    for row in rows:
        person = canonical(row["user_name"])
        current = best.get(person)
        if current is None or (row["speed"] or 0) > current[0]:
            best[person] = (row["speed"] or 0, row["score_date"] or "")
    if not best:
        where = name + (f"（{group_display_name(db, group)}）" if scope == "group" and group else "")
        return {"status": "empty", "message": f"{where}还没有成绩记录。"}
    ranked = sorted(best.items(), key=lambda item: -item[1][0])[:limit]
    where = name + (f"（{group_display_name(db, group)}）" if scope == "group" and group else "")
    range_text = "全部时间" if days <= 0 or days >= 3650 else f"最近 {days} 天"
    parts = [f"{index}. {person} {speed:.2f}" for index, (person, (speed, _)) in enumerate(ranked, 1)]
    return {
        "status": "ok",
        "message": f"{where} · {range_text} 最快：" + "；".join(parts),
        "rows": [{"rank": i, "user": p, "best": s} for i, (p, (s, _)) in enumerate(ranked, 1)],
    }


def contest_count_response(db: sqlite3.Connection, query: dict) -> dict:
    """回答「某人某个赛文有多少条」—— 全群赛文不分群（555 就是全群共用）。"""
    contest_raw = (query.get("contest", [""])[0] or "").strip()
    user_raw = (query.get("user", [""])[0] or "").strip()
    if contest_raw in ("all", "所有", "全部", "全部赛文"):
        contest_raw = ""
    key = resolve_contest_key(contest_raw) if contest_raw else ""
    if contest_raw and not key:
        return {"status": "unknown_contest", "known": list(CONTEST_LABELS)}
    if not key:
        # 没说赛文：返回这个人每个赛文的条数，并核对有没有重复（答「有没有去重」这类问题）
        params: list = []
        clause = ""
        if user_raw:
            fragments = [f for f in re.split(r"[「」『』（）()\[\]【】\s]+", user_raw) if len(f) >= 2]
            latin = [f for f in fragments if re.fullmatch(r"[A-Za-z0-9_]+", f)]
            best = max(latin or fragments, key=len) if fragments else ""
            clauses = ["user_name LIKE ?", "user_id = ?"]
            params = [f"%{user_raw}%", user_raw]
            if best and best != user_raw:
                clauses.append("user_name LIKE ?")
                params.append(f"%{best}%")
            clause = " AND (" + " OR ".join(clauses) + ")"
        rows = db.execute(
            "SELECT contest_name, COUNT(*) AS n, COUNT(DISTINCT score_date) AS days "
            "FROM contest_scores WHERE 1=1" + clause + " GROUP BY contest_key ORDER BY n DESC",
            tuple(params),
        ).fetchall()
        who = user_raw or "全部人"
        if not rows:
            return {"status": "ok", "count": 0, "message": f"{who}还没有任何赛文成绩。"}
        parts = [f"{row['contest_name']} {row['n']} 条（{row['days']} 天）" for row in rows]
        total = sum(row["n"] for row in rows)
        # 重复检查：同 (赛文,群,日期,真人) 是否还有多条
        dup_rows = db.execute(
            "SELECT COUNT(*) FROM (SELECT contest_key, group_id, score_date, user_name, COUNT(*) c "
            "FROM contest_scores WHERE 1=1" + clause +
            " GROUP BY contest_key, group_id, score_date, user_name HAVING c > 1)", tuple(params),
        ).fetchone()[0]
        dup_note = "同一天同赛文只留最高分，没有重复 ✓" if dup_rows == 0 else f"还有 {dup_rows} 组重复 ✗"
        # 原始消息记录数（typing_scores 不参与去重，是原始流水）
        raw_params: list = []
        raw_clause = ""
        if user_raw:
            fragments = [f for f in re.split(r"[「」『』（）()\[\]【】\s]+", user_raw) if len(f) >= 2]
            latin = [f for f in fragments if re.fullmatch(r"[A-Za-z0-9_]+", f)]
            best = max(latin or fragments, key=len) if fragments else ""
            clauses = ["user_name LIKE ?", "user_id = ?"]
            raw_params = [f"%{user_raw}%", user_raw]
            if best and best != user_raw:
                clauses.append("user_name LIKE ?")
                raw_params.append(f"%{best}%")
            raw_clause = " WHERE (" + " OR ".join(clauses) + ")"
        raw_total = db.execute(
            "SELECT COUNT(*) FROM typing_scores" + raw_clause, tuple(raw_params)
        ).fetchone()[0]
        message = f"{who}共 {total} 条赛文成绩 —— " + "；".join(parts) + "。" + dup_note
        if raw_total and raw_total != total:
            message += f"（原始消息流水 {raw_total} 条，按「同人同天同赛文只留最高分」去重后 {total} 条）"
        return {"status": "ok", "count": total, "raw_count": raw_total, "message": message}
    name, scope = CONTEST_LABELS.get(key, (contest_raw, "global"))
    user = (query.get("user", [""])[0] or "").strip()
    group = resolve_group_id(db, (query.get("group_id", [""])[0] or "").strip())
    room = group if scope == "group" else ""
    clause, params = "", []
    if user:
        fragments = [f for f in re.split(r"[「」『』（）()\[\]【】\s]+", user) if len(f) >= 2]
        latin = [f for f in fragments if re.fullmatch(r"[A-Za-z0-9_]+", f)]
        best = max(latin or fragments, key=len) if fragments else ""
        clauses = ["user_name LIKE ?", "user_id = ?"]
        params = [f"%{user}%", user]
        if best and best != user:
            clauses.append("user_name LIKE ?")
            params.append(f"%{best}%")
        clause = " AND (" + " OR ".join(clauses) + ")"
    rows = db.execute(
        "SELECT COUNT(*) AS n, COUNT(DISTINCT score_date) AS days, "
        "MIN(score_date) AS first, MAX(score_date) AS last, MAX(speed) AS best "
        "FROM contest_scores WHERE contest_key=? AND group_id=?" + clause,
        tuple([key, room] + params),
    ).fetchone()
    if scope == "global":
        where = name
    else:
        room_name = group_display_name(db, group) if group else ""
        where = name + (f"（{room_name or group}）" if group else "")
    who = user or "全部人"
    message = f"{who}的{where}共 {rows['n']} 条，覆盖 {rows['days']} 天（{rows['first']} ~ {rows['last']}），最高 {rows['best']}。"
    if scope == "group" and group and rows["n"] == 0:
        message = f"{who}的{where}共 0 条 —— 这个群没有群赛文记录。要看别的群可以说群名。"
    return {
        "status": "ok",
        "contest": key,
        "contest_name": where,
        "user": who,
        "count": rows["n"],
        "days": rows["days"],
        "first": rows["first"],
        "last": rows["last"],
        "best": rows["best"],
        "message": message,
    }


def render_score_chart(db: sqlite3.Connection, user_id: str, name: str, source: str, days: int, metric: str,
                       contest: str = "", group_id: str = "", contest_user: str = "") -> bytes:
    """Render a deterministic, readable score chart instead of model-generated artwork."""
    from PIL import Image, ImageDraw, ImageFont

    contest_title = ""
    summary = competition_score_summary(db, user_id, name, source)
    records = competition_score_history(db, user_id, name, source, days)
    if contest == "all":
        # 不分赛文的所有打字成绩（所有者 2026-11-06）
        contest_title = "所有打字"
        records = all_typing_history(db, contest_user or name, days if days > 0 else 30)
        summary = competition_score_summary(db, user_id, name, source)
    elif contest:
        # 赛文曲线也走这个渲染器（所有者 2026-11-06：老的比新画的好看）。
        # contest 可能是「锦标赛」「群赛文」这种自然说法，必须先解析成段号，
        # 否则查不到数据会静默回落到虎杯（2026-11-06 实测就是这个 bug）。
        resolved = resolve_contest_key(contest)
        if resolved:
            contest_label = CONTEST_LABELS.get(resolved, (contest, "global"))[0]
            contest_scope = CONTEST_LABELS.get(resolved, (contest, "global"))[1]
            if contest_scope == "group" and group_id:
                # 按群的赛文：标题写清楚是哪个群（所有者 2026-11-06：
                # 「这个群赛文不要写本群，要写清楚是哪个群才对」）
                room_name = group_display_name(db, group_id)
                contest_label = (f"{room_name}赛文" if resolved == "999"
                                 else f"{room_name}·{contest_label}")
            records = contest_score_history(db, resolved, group_id, contest_user or name, days)
            # 标题也要用这个赛文名 —— 之前 source_name 硬编码成「虎杯」，
            # 所以查锦标赛也显示虎杯（2026-11-06 实测）。
            contest_title = contest_label
    # FM 实际传的是「555赛文」（审计里看到的 ✗），不是「555」——
    # 我以前只认 == "555"，于是真实调用落到虎杯那张表、出空图。
    # 改成包含判断，任何带 555 / AI赛文 的说法都算。
    if "555" in source or "AI赛文" in source or "ai赛文" in source:
        # 555 的成绩在 ai_contest_scores，和虎杯/公开赛事的 competition_scores 是两张表。
        # 实测（2026-10-05）：后者在 8 月 27 日之后就没有新数据了，所以只读它的曲线
        # 一直是空图（记录 0 条）。这里把记录源换成 555 —— 图的主体和「空图」判断
        # 都由 records 决定，所以只换它就够。
        records = ai_contest_score_history(db, user_id, name, days)
    metric_key = {"accuracy": "acc", "keystrokes": "key", "key": "key", "speed": "speed"}.get(metric.lower(), "speed")
    metric_name = {"speed": "速度（字/分）", "acc": "键准（%）", "key": "击键"}[metric_key]
    # 按群赛文时，模型有时把群名当成「谁」传进来 → 标题会变成「五林风 · 五林风赛文」。
    room_name_for_title = group_display_name(db, group_id) if group_id and CONTEST_LABELS.get(contest or "", ("", ""))[1] == "group" else ""
    if contest_user and room_name_for_title and contest_user == room_name_for_title:
        contest_user = ""
    display_name = (contest_user or name or user_id or "")
    if records:
        display_name = str(records[-1].get("user_name") or records[-1].get("name") or display_name)
    # 没指定 source 时别再写「全部赛事」：如果这批记录都来自同一个赛事，
    # 就用那个名字（所有者 2026-10-24：虎杯的图上写着「全部赛事」很怪）。
    # 这两个必须在**任何使用之前**定义：函数体是从上往下执行的，
    # 而 _is_555 原来在下面才赋值、_detail_label 更是压根没定义（明细表删掉时
    # 把它的定义一起删了，但 columns 列表里还在调用）→ 连着两次 UnboundLocalError。
    _is_555 = "555" in (source or "") or "AI赛文" in (source or "") or "ai赛文" in (source or "")
    _detail_label = lambda label: ""  # 明细表已整体移除（所有者 2026-10-24）

    if source:
        source_name = source
    else:
        # 按数据来自哪张表定，别去猜记录里的字段（有的老记录没有 source_group）。
        source_name = contest_title if contest else ("555赛文" if _is_555 else "虎杯")
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

    # 555 不出明细表（所有者 2026-10-05）：120 行明细把图拉到 4500px 高，
    # 想看的曲线反而要滚半天。空列表同时会让 height 自己缩下来。
    # 同一处判断有两份（这里和上面那个 records 分支）—— 上次只改了上面那份，
    # 于是 source="555赛文" 时明细表还是画了出来。判断要一致。
    # 所有者 2026-10-24：明细记录一律不要 —— 555 和虎杯都只看曲线。
    # 原来 555 隐藏、虎杯保留，现在统一隐藏（图更短、也更干净）。
    detail_records = []
    row_height = 31
    # 画布宽度跟着点数走（所有者 2026-11-06：非常密集可以适当宽一些）：
    # 少量点固定 2200；点越多越宽，最多 4400，保证相邻两点至少隔开几个像素。
    n_points = len(records)
    if n_points <= 150:
        width = 2200
    else:
        width = min(4400, int(2200 + (n_points - 150) * 2.4))
    # 没有明细表就没有下半部分，图收短。
    height = (790 + max(1, len(detail_records)) * row_height + 100) if detail_records else 760
    # 深色主题，和排行榜一致（所有者 2026-10-05 要求）。
    image = Image.new("RGB", (width, height), (20, 24, 31))
    draw = ImageDraw.Draw(image)
    navy, muted, grid, teal = (244, 247, 252), (159, 172, 193), (44, 53, 66), (78, 205, 181)
    draw.rectangle((0, 0, width, 12), fill=teal)
    title_text = (display_name + " · ") if display_name else ""
    draw.text((72, 46), f"{title_text}{source_name}成绩趋势", font=font(46), fill=navy)
    draw.text((74, 108), f"指标：{metric_name}    时间范围：{'全部' if days <= 0 or days >= 3650 else f'最近 {days} 天'}    记录：{len(records)} 条", font=font(24), fill=muted)
    if not records:
        draw.rounded_rectangle((72, 196, width - 72, 520), radius=12, fill=(29, 36, 47), outline=grid, width=2)
        # 空图别只说「没找到」—— 那看着像功能坏了。先查这个人在这项赛事里
        # 最近一次是什么时候（虎杯 2026-08-27 之后就停了，555 一直在跑）。
        _last, _total = _last_score_date(db, user_id, name, _is_555)
        if _last:
            draw.text((width // 2, 300), "这个时间范围里没有成绩", font=font(30), fill=navy, anchor="mm")
            draw.text((width // 2, 360),
                      f"最近一次是 {_last}，{source_name}共有 {_total} 条记录",
                      font=font(24), fill=muted, anchor="mm")
        else:
            empty_text = "没有找到符合条件的成绩记录"
            if contest_label:
                # 按群的赛文：说清楚是哪个群没有，而不是一句含糊的「没有记录」
                empty_text = f"{contest_label}还没有成绩记录"
            draw.text((width // 2, 320), empty_text, font=font(30), fill=muted, anchor="mm")
    else:
        chart = (104, 196, width - 104, 660)
        left, top, right, bottom = chart
        draw.rounded_rectangle(chart, radius=12, fill=(29, 36, 47), outline=grid, width=2)
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
        # 点多的时候缩小线和点，否则 700 个直径 20 的圆会糊成一团（2026-11-06）
        dense = len(records) > 200
        line_width = 3 if dense else 6
        dot_radius = 4 if len(records) > 1200 else (5 if dense else 10)
        if len(points) > 1:
            draw.line(points, fill=teal, width=line_width, joint="curve")
        # Keep the exact value beside every point, as requested. The complete
        # table below remains available for histories whose labels overlap.
        show_point_labels = True
        # 每个点都尝试标速度：用二维矩形碰撞检测，只要不和已放置的标签重叠就标。
        # （以前只跟「上一个标签」比，隔得远的空位也不标 —— 2026-11-06 所有者要求改掉）
        label_font_size = 15 if len(records) > 120 else 17
        label_char_w = 7.0 if label_font_size == 15 else 8.0
        max_labels = 500
        placed_boxes: list[tuple[float, float, float, float]] = []
        labeled_count = 0
        for index, ((x, y), item, value) in enumerate(zip(points, records, values)):
            accuracy = float(str(item.get("acc") or item.get("accuracy") or 0).replace("%", "") or 0)
            color = (35, 163, 108) if accuracy >= 95 else (224, 105, 71)
            draw.ellipse((x - dot_radius, y - dot_radius, x + dot_radius, y + dot_radius),
                         fill=color, outline=(255, 255, 255), width=2 if dense else 3)
            # 点太多时不要每个都标数字（180 个点会糊成一片 ✗）。
            if show_point_labels and labeled_count < max_labels:
                label_text = f"{value:.2f}"
                try:
                    # 用真实文字宽度算碰撞框（估算宽度偏小会漏判重叠）
                    text_w = draw.textlength(label_text, font=font(label_font_size))
                except Exception:
                    text_w = len(label_text) * label_char_w
                half = text_w / 2 + 2
                box = (x - half, y - 34, x + half, y - 16)
                collision = any(
                    not (box[2] <= b[0] or box[0] >= b[2] or box[3] <= b[1] or box[1] >= b[3])
                    for b in placed_boxes
                )
                if not collision:
                    draw.text((x, y - 22), label_text,
                              font=font(label_font_size), fill=navy, anchor="ms")
                    placed_boxes.append(box)
                    labeled_count += 1
        date_labels = [str(item.get("competition_date") or item.get("date") or "-")[-5:] for item in records]
        # 日期也「能放就放」：逐个做水平碰撞检测，不挤就标（2026-11-06 所有者要求）。
        # 同一天连续出现时只标一次，避免一串重复日期。
        date_boxes: list[tuple[float, float, float, float]] = []
        last_date_text = ""
        for index, label in enumerate(date_labels):
            x = plot_left if len(date_labels) == 1 else plot_left + index * (plot_right - plot_left) / (len(date_labels) - 1)
            if label == last_date_text:
                continue
            try:
                text_w = draw.textlength(label, font=font(16))
            except Exception:
                text_w = len(label) * 8.0
            box = (x - text_w / 2 - 6, bottom - 44, x + text_w / 2 + 6, bottom - 12)
            collision = any(
                not (box[2] <= b[0] or box[0] >= b[2] or box[3] <= b[1] or box[1] >= b[3])
                for b in date_boxes
            )
            if collision:
                continue
            draw.text((x, bottom - 28), label, font=font(16), fill=muted, anchor="ms")
            date_boxes.append(box)
            last_date_text = label
        # 图例放右上角空白（所有者 2026-11-06：原来压在曲线上）
        draw.text((width - 350, 48), "● 键准≥95%", font=font(24), fill=(35, 163, 108))
        draw.text((width - 350, 88), "● 键准<95%", font=font(24), fill=(224, 105, 71))

    # 555 不出明细表：空表下面挂个标题更奇怪，所以无明细时标题和表头都不画。
    _detail_label = (lambda s: s) if detail_records else (lambda s: "")
    draw.text((72, 660), _detail_label("明细记录"), font=font(28), fill=navy)
    columns = [(76, "日期"), (300, "赛事"), (810, metric_name), (1050, "键准"), (1250, "击键")]
    # 明细表已经不用了（detail_records 恒为空）；画布变高后原来的空表头会露出来，
    # 2026-11-06 加宽到 2200x760 时底部多出一个空框，所以这里跟着条件走。
    if detail_records:
        draw.rounded_rectangle((72, 710, width - 72, 765), radius=8, fill=(31, 55, 80))
    # 无明细就整段不画（所有者 2026-10-05：图里不要出现这个空表头）。
    for x, label in (columns if detail_records else []):
        draw.text((x, 727), label, font=font(19), fill=(29, 36, 47))
    for row_index, item in enumerate(detail_records):
        y = 778 + row_index * 31
        if row_index % 2 == 0:
            draw.rectangle((72, y - 4, width - 72, y + 27), fill=(29, 36, 47))
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


# ==================== 后台控制台只读接口（2026-11-15） ====================
# fm-admin 后台的数据页（番茄书架 / 赛文图表 / 文库 / 打字排行 / 日报 / 统计）
# 需要可直接 curl 的 GET 只读接口。本段约定：
#   * 新接口统一返回 {"ok": true, ...} 或 {"ok": false, "message": ...}；
#   * 老接口（/groups、/scores、/library/search、/contest/search…）保持原形状，
#     只做参数加固 + 双问号兼容（后台旧代理会拼出 "?limit=50?q=xx"）；
#   * 所有查询只做 SELECT，不写库。

APP_NAME = "fm-domain"
APP_VERSION = "2026.11.15-backend-readonly"
SERVER_STARTED_AT = time.time()
CHINA_TZ = timezone(timedelta(hours=8))


def query_value(query: dict, name: str, default: str = "") -> str:
    """从 parse_qs 结果里取第一个值（原样字符串）。"""
    if not isinstance(query, dict):
        return default
    value = query.get(name, default)
    if isinstance(value, (list, tuple)):
        return str(value[0]) if value else default
    if value is None:
        return default
    return str(value)


def query_int(query: dict, name: str, default: int = 0,
              minimum: int | None = None, maximum: int | None = None) -> int:
    """健壮地读整数参数：脏值回退默认值，绝不因为 ?limit=abc 把连接打断。"""
    raw = query_value(query, name, "").strip()
    value = default
    if raw:
        try:
            value = int(float(raw))
        except (TypeError, ValueError):
            value = default
    if minimum is not None:
        value = max(minimum, value)
    if maximum is not None:
        value = min(maximum, value)
    return value


def query_flag(query: dict, name: str, default: bool = False) -> bool:
    raw = query_value(query, name, "").strip().lower()
    if not raw:
        return default
    return raw in {"1", "true", "yes", "on"}


def query_payload(query: dict) -> dict:
    """parse_qs 结果 → 老函数认的扁平 payload（value 取第一个）。"""
    payload: dict = {}
    for key, values in (query or {}).items():
        if isinstance(values, (list, tuple)):
            payload[key] = values[0] if values else ""
        else:
            payload[key] = values
    return payload


def parse_backend_query(raw_query: str) -> dict:
    """parse_qs + 兼容后台旧代理的双问号拼接。

    fm-admin 的 DOMAIN_READS 里写死了 "/library/search?limit=50"、"scores?limit=50"、
    "/contest/search?limit=50"，代理又无条件补一个 "?"，于是真实请求是
    "/scores?limit=50?q=xxx" —— parse_qs 只会得到 {"limit": ["50?q=xxx"]}，
    取整数时直接抛 ValueError（之前后台看到的 503 就是这么来的）。
    这里把值里的 "?" 重新拆回键值，既修 503 也保住 q 参数。
    """
    query = parse_qs(raw_query)
    if not any("?" in str(value) for values in query.values() for value in values):
        return query
    fixed: dict = {}
    for key, values in query.items():
        for value in values:
            if "?" not in str(value):
                fixed.setdefault(key, []).append(value)
                continue
            head, _, tail = str(value).partition("?")
            fixed.setdefault(key, []).append(head)
            for extra_key, extra_values in parse_qs(tail).items():
                fixed.setdefault(extra_key, []).extend(extra_values)
    return fixed


def table_count(db: sqlite3.Connection, table: str) -> int | None:
    """表行数；表不存在/查询失败返回 None（扩展 /stats 时不能因为一张表把整页打挂）。"""
    try:
        return int(db.execute(f'SELECT COUNT(*) FROM "{table}"').fetchone()[0])
    except sqlite3.Error:
        return None


def table_counts(db: sqlite3.Connection, tables: list[str]) -> dict:
    return {table: (table_count(db, table) or 0) for table in tables}


def china_now() -> datetime:
    return datetime.now(CHINA_TZ)


def china_date_text(value: str = "") -> str:
    """规整成北京时间 YYYY-MM-DD；非法/空值取今天。"""
    text = str(value or "").strip()
    if text:
        try:
            return datetime.strptime(text[:10], "%Y-%m-%d").date().isoformat()
        except ValueError:
            pass
    return china_now().date().isoformat()


def china_day_range(date_text: str) -> tuple[float, float]:
    day = datetime.strptime(china_date_text(date_text), "%Y-%m-%d").replace(tzinfo=CHINA_TZ)
    start = day.timestamp()
    return start, start + 86400


def epoch_text(value, fmt: str = "iso") -> str:
    try:
        number = float(value)
    except (TypeError, ValueError):
        return ""
    if number <= 0:
        return ""
    if fmt == "iso":
        return datetime.fromtimestamp(number, CHINA_TZ).isoformat(timespec="seconds")
    return datetime.fromtimestamp(number, CHINA_TZ).strftime(fmt)


def maybe_float(value, digits: int = 2):
    try:
        return round(float(value), digits)
    except (TypeError, ValueError):
        return None


# ── 番茄小说（novel_*） ──────────────────────────────────

def novel_book_dict(row: sqlite3.Row) -> dict:
    """统一的书目形状：book_id（novel_books 主键）同时以 id 暴露，方便通用表格按 id 取值。"""
    book_id = str(row["book_id"])
    item = {
        "book_id": book_id,
        "id": book_id,
        "title": row["title"],
        "author": row["author"] or "",
        "total_chapters": int(row["total_chapters"] or 0),
        "chapters": int(row["total_chapters"] or 0),
        "word_count": int(row["word_count"] or 0),
        "source": row["source"] or "fanqie",
        "imported_at": row["imported_at"],
        "imported_at_text": epoch_text(row["imported_at"]),
    }
    try:
        item["internal_id"] = int(row["internal_id"])
    except (IndexError, KeyError, TypeError, ValueError):
        pass
    return item


def novel_internal_id(db: sqlite3.Connection, book_id: str) -> int | None:
    try:
        row = db.execute("SELECT rowid FROM novel_books WHERE book_id=?", (book_id,)).fetchone()
        return int(row[0]) if row else None
    except sqlite3.Error:
        return None


def novel_shelf_stats(db: sqlite3.Connection) -> dict:
    """番茄书架总览：后台「番茄小说」页直接渲染 items（书名/作者/章节/字数）。"""
    books = table_count(db, "novel_books") or 0
    chapters = table_count(db, "novel_chapters") or 0
    progress = table_count(db, "novel_progress") or 0
    records = table_count(db, "novel_records") or 0
    rows = db.execute(
        "SELECT b.rowid AS internal_id,b.book_id,b.title,b.author,b.total_chapters,b.word_count,"
        "b.source,b.imported_at,"
        "(SELECT COUNT(*) FROM novel_chapters c WHERE c.book_id=b.book_id) AS chapter_count,"
        "(SELECT COUNT(*) FROM novel_progress p WHERE p.book_id=b.book_id) AS progress_records,"
        "(SELECT COUNT(DISTINCT p.requester_id) FROM novel_progress p WHERE p.book_id=b.book_id) AS progress_users "
        "FROM novel_books b ORDER BY b.word_count DESC, b.title LIMIT 500"
    ).fetchall()
    items = []
    for row in rows:
        item = novel_book_dict(row)
        item["chapter_count"] = int(row["chapter_count"] or 0)
        item["progress_records"] = int(row["progress_records"] or 0)
        item["progress_users"] = int(row["progress_users"] or 0)
        items.append(item)
    return {
        "ok": True, "books": books, "chapters": chapters, "progress": progress,
        "records": records, "count": len(items), "items": items,
    }


def novel_books_shelf(db: sqlite3.Connection, query: dict | None = None) -> dict:
    """GET /novel/books：书架清单（支持 q 模糊搜书名/作者）。"""
    text = query_value(query or {}, "q").strip() or query_value(query or {}, "book").strip()
    clause, params = "", []
    if text:
        clause = " WHERE b.title LIKE ? OR b.author LIKE ?"
        params = [f"%{text}%", f"%{text}%"]
    rows = db.execute(
        "SELECT b.rowid AS internal_id,b.book_id,b.title,b.author,b.total_chapters,b.word_count,"
        "b.source,b.imported_at FROM novel_books b" + clause +
        " ORDER BY b.word_count DESC, b.title LIMIT 500",
        tuple(params),
    ).fetchall()
    items = [novel_book_dict(row) for row in rows]
    return {"ok": True, "count": len(items), "items": items}


def novel_book_detail(db: sqlite3.Connection, query: dict) -> dict:
    """GET /novel/book：书 + 章节目录 + 每人进度 + 单章成绩。

    入参二选一（都支持）：
      * book_id / id / internal_id / rowid —— novel_books 主键（也认内部 rowid）
      * q / book / title —— 书名关键词（“斩神” 这种）
    chapters 支持 offset/limit（默认 200，上限 1000），返回 total / truncated。
    找不到返回 404 + {"ok": false, "message": ...}（后台 v3 抽屉按 404 显示未就绪原因）。
    """
    raw_id = (query_value(query, "book_id").strip()
              or query_value(query, "id").strip()
              or query_value(query, "internal_id").strip()
              or query_value(query, "rowid").strip())
    # 客户端偶尔把 JSON 的引号一起带过来（?book_id="6982..."），这里统一去掉。
    raw_id = raw_id.strip().strip('"').strip("'").strip()
    keyword = (query_value(query, "q").strip()
               or query_value(query, "book").strip()
               or query_value(query, "title").strip()).strip().strip('"').strip("'").strip()
    book = None
    if raw_id:
        book = db.execute(
            "SELECT rowid AS internal_id,* FROM novel_books WHERE book_id=?", (raw_id,)
        ).fetchone()
        # 兼容「内部 id」：novel_books 的 rowid（1~N 这种短数字）
        if book is None and raw_id.isdigit() and len(raw_id) <= 9:
            book = db.execute(
                "SELECT rowid AS internal_id,* FROM novel_books WHERE rowid=?", (int(raw_id),)
            ).fetchone()
    if book is None and keyword:
        book = novel_find_book(db, keyword)
    if book is None and raw_id:
        book = novel_find_book(db, raw_id)
    if book is None:
        if not raw_id and not keyword:
            return {"ok": False, "status": 400,
                    "message": "缺少 book_id 参数（也支持 id / q 书名关键词），例如 /novel/book?book_id=6982529841564224526"}
        received = raw_id or keyword
        return {"ok": False, "status": 404, "book_id": raw_id, "query": keyword,
                "message": f"书库里没有 {received} 这本书（可用 book_id/id 主键或 q=书名关键词查询）",
                "items": [], "chapters": [], "progress": []}

    book_id = str(book["book_id"])
    internal_id = novel_internal_id(db, book_id)
    user = query_value(query, "user").strip() or query_value(query, "requester_id").strip()
    limit = query_int(query, "limit", 0, 0, 1000)
    if not query_value(query, "limit").strip():
        limit = query_int(query, "chapter_limit", 0, 0, 1000)
    if limit <= 0:
        limit = 200
    offset = query_int(query, "offset", -1, 0, 200000)
    if not query_value(query, "offset").strip():
        offset = query_int(query, "chapter_offset", 0, 0, 200000)
    if offset < 0:
        offset = 0
    total_chapters = int(
        db.execute("SELECT COUNT(*) FROM novel_chapters WHERE book_id=?", (book_id,)).fetchone()[0]
    )
    chapter_rows = db.execute(
        "SELECT chapter_index,title,word_count FROM novel_chapters "
        "WHERE book_id=? ORDER BY chapter_index LIMIT ? OFFSET ?",
        (book_id, limit, offset),
    ).fetchall()
    chapters = [
        {"chapter_index": int(row["chapter_index"]), "title": row["title"],
         "word_count": int(row["word_count"] or 0)}
        for row in chapter_rows
    ]
    truncated = offset + len(chapters) < total_chapters

    chapter_scores: list[dict] = []
    try:
        score_sql = (
            "SELECT novel_chapter_index AS chapter_index, COUNT(*) AS records, "
            "AVG(speed) AS avg_speed, MAX(speed) AS best_speed, AVG(accuracy) AS avg_accuracy, "
            "AVG(key_rate) AS avg_key_rate FROM typing_scores "
            "WHERE novel_book_id=? AND novel_chapter_index IS NOT NULL"
        )
        score_params: list = [book_id]
        if user:
            score_sql += " AND (user_name LIKE ? OR user_id=?)"
            score_params += [f"%{user}%", user]
        score_sql += " GROUP BY novel_chapter_index ORDER BY novel_chapter_index LIMIT 500"
        chapter_scores = [
            {"chapter_index": int(row["chapter_index"]), "records": int(row["records"]),
             "avg_speed": maybe_float(row["avg_speed"]), "best_speed": maybe_float(row["best_speed"]),
             "avg_accuracy": maybe_float(row["avg_accuracy"]),
             "avg_key_rate": maybe_float(row["avg_key_rate"])}
            for row in db.execute(score_sql, tuple(score_params)).fetchall()
        ]
    except sqlite3.Error:
        chapter_scores = []

    progress_sql = (
        "SELECT p.platform,p.chat_id,p.requester_id,p.chapter_index,p.char_offset,p.updated_at,"
        "p.segment_length,p.last_segment_id,p.last_segment_chars,p.last_sent_at,"
        "COALESCE(n.user_name,'') AS user_name,n.records,n.avg_speed,n.avg_accuracy,n.chars,n.last_recorded_at "
        "FROM novel_progress p LEFT JOIN ("
        "SELECT user_id,COUNT(*) AS records,AVG(speed) AS avg_speed,AVG(accuracy) AS avg_accuracy,"
        "SUM(chars) AS chars,MAX(recorded_at) AS last_recorded_at "
        "FROM novel_records WHERE book_id=? GROUP BY user_id"
        ") n ON n.user_id=p.requester_id WHERE p.book_id=?"
    )
    params: list = [book_id, book_id]
    if user:
        progress_sql += " AND (p.requester_id=? OR p.requester_id LIKE ? OR n.user_name LIKE ?)"
        params += [user, f"%{user}%", f"%{user}%"]
    progress_sql += " ORDER BY p.updated_at DESC LIMIT 200"
    try:
        progress_rows = db.execute(progress_sql, tuple(params)).fetchall()
    except sqlite3.Error:
        progress_rows = []
    progress = [
        {"platform": row["platform"], "chat_id": row["chat_id"],
         "requester_id": row["requester_id"], "user_name": row["user_name"] or "",
         "chapter_index": int(row["chapter_index"] or 0), "char_offset": int(row["char_offset"] or 0),
         "updated_at": row["updated_at"], "updated_at_text": epoch_text(row["updated_at"]),
         "segment_length": int(row["segment_length"] or 0),
         "last_segment_id": row["last_segment_id"] or "",
         "last_segment_chars": int(row["last_segment_chars"] or 0),
         "last_sent_at": row["last_sent_at"] or 0,
         "records": int(row["records"] or 0),
         "avg_speed": maybe_float(row["avg_speed"]), "avg_accuracy": maybe_float(row["avg_accuracy"]),
         "chars": int(row["chars"] or 0), "last_recorded_at": row["last_recorded_at"] or 0,
         "last_recorded_at_text": epoch_text(row["last_recorded_at"])}
        for row in progress_rows
    ]
    book_payload = novel_book_dict(book)
    if internal_id is not None:
        book_payload["internal_id"] = internal_id
    return {
        "ok": True, "book": book_payload, "book_id": book_id,
        # 章节分页：total/truncated 是 v3 后台要的字段，其余为兼容旧字段
        "total": total_chapters, "truncated": truncated,
        "offset": offset, "limit": limit, "count": len(chapters),
        "chapters": chapters,
        "total_chapters": total_chapters, "chapter_offset": offset,
        "chapter_limit": limit, "chapters_truncated": truncated,
        "progress": progress, "progress_count": len(progress),
        "chapter_scores": chapter_scores,
    }


def novel_progress_report(db: sqlite3.Connection, query: dict) -> dict:
    """GET /novel/progress?user=&days=：每本书/每人的最新阅读进度。"""
    days = query_int(query, "days", 0, 0, 3650)
    user = query_value(query, "user").strip() or query_value(query, "requester_id").strip()
    platform = query_value(query, "platform").strip()
    chat_id = query_value(query, "chat_id").strip() or query_value(query, "group_id").strip()
    clauses: list[str] = []
    params: list = []
    if user:
        clauses.append("(p.requester_id=? OR p.requester_id LIKE ?)")
        params += [user, f"%{user}%"]
    if platform:
        clauses.append("p.platform=?")
        params.append(platform)
    if chat_id:
        clauses.append("p.chat_id=?")
        params.append(chat_id)
    if days > 0:
        clauses.append("p.updated_at>=?")
        params.append(time.time() - days * 86400)
    where = (" WHERE " + " AND ".join(clauses)) if clauses else ""
    rows = db.execute(
        "SELECT p.platform,p.chat_id,p.requester_id,p.book_id,p.chapter_index,p.char_offset,"
        "p.updated_at,p.segment_length,p.last_segment_id,p.last_segment_chars,p.last_sent_at,"
        "b.title AS book_title,b.author AS book_author,b.total_chapters AS book_total_chapters,"
        "b.word_count AS book_word_count,c.title AS chapter_title,"
        "c.word_count AS chapter_word_count,l.char_count AS chapter_chars "
        "FROM novel_progress p LEFT JOIN novel_books b ON b.book_id=p.book_id "
        "LEFT JOIN novel_chapters c ON c.book_id=p.book_id AND c.chapter_index=p.chapter_index "
        "LEFT JOIN library_texts l ON l.text_id=c.text_id" + where +
        " ORDER BY p.updated_at DESC LIMIT 500",
        tuple(params),
    ).fetchall()
    items = []
    for row in rows:
        chapter_total = int(row["chapter_chars"] or row["chapter_word_count"] or 0)
        offset = int(row["char_offset"] or 0)
        score_row = db.execute(
            "SELECT COUNT(*) AS n,AVG(speed) AS speed,AVG(accuracy) AS acc,"
            "AVG(wpm) AS wpm,SUM(chars) AS chars,MAX(recorded_at) AS last_at "
            "FROM novel_records WHERE book_id=? AND user_id=?",
            (row["book_id"], row["requester_id"]),
        ).fetchone()
        score = None
        if score_row and score_row["n"]:
            score = {
                "records": int(score_row["n"]), "avg_speed": maybe_float(score_row["speed"]),
                "avg_accuracy": maybe_float(score_row["acc"]), "avg_word_rate": maybe_float(score_row["wpm"]),
                "chars": int(score_row["chars"] or 0),
                "last_recorded_at": score_row["last_at"] or 0,
                "last_recorded_at_text": epoch_text(score_row["last_at"]),
            }
        items.append({
            "platform": row["platform"], "chat_id": row["chat_id"],
            "requester_id": row["requester_id"], "user_name": row["requester_id"],
            "book_id": row["book_id"], "book_title": row["book_title"] or "",
            "book_author": row["book_author"] or "",
            "book_total_chapters": int(row["book_total_chapters"] or 0),
            "book_word_count": int(row["book_word_count"] or 0),
            "chapter_index": int(row["chapter_index"] or 0),
            "chapter_title": row["chapter_title"] or "",
            "char_offset": offset, "chapter_chars": chapter_total,
            "remaining_chars": max(0, chapter_total - offset),
            "segment_length": int(row["segment_length"] or 0),
            "last_segment_id": row["last_segment_id"] or "",
            "last_segment_chars": int(row["last_segment_chars"] or 0),
            "last_sent_at": row["last_sent_at"] or 0,
            "updated_at": row["updated_at"],
            "updated_at_text": epoch_text(row["updated_at"]),
            "score": score,
        })
    linked_typing = 0
    try:
        linked_typing = int(db.execute(
            "SELECT COUNT(*) FROM typing_scores WHERE novel_book_id IS NOT NULL AND novel_book_id<>''"
        ).fetchone()[0])
    except sqlite3.Error:
        linked_typing = 0
    return {
        "ok": True, "count": len(items), "days": days, "items": items,
        "progress_total": table_count(db, "novel_progress") or 0,
        "records_total": table_count(db, "novel_records") or 0,
        "typing_linked": linked_typing,
        "typing_total": table_count(db, "typing_scores") or 0,
    }


def stats_response(db: sqlite3.Connection) -> dict:
    """/stats：保留原来的 9 个字段（后台 v1 页面在用），只追加新表计数。"""
    tables = [
        "groups", "library_texts", "contest_texts", "recall_records", "score_records",
        "ai_contest_texts", "ai_contest_scores", "competition_scores", "recent_messages",
        "contest_scores", "novel_books", "novel_chapters", "novel_progress", "novel_records",
        "typing_scores", "message_archive", "library_classifications", "library_rankings",
        "known_senders", "group_runtime", "contest_sessions", "library_sessions",
        "single_sessions", "group_digests", "library_sent_messages", "repeat_cooldowns",
    ]
    return table_counts(db, tables)


# ── 文库（library_*） ────────────────────────────────────

def library_list_response(db: sqlite3.Connection, query: dict) -> dict:
    """GET /library/list：文库分页列表（标题/字数/难度/题材/来源/入库时间）。"""
    limit = query_int(query, "limit", 20, 1, 200)
    offset = query_int(query, "offset", 0, 0, 1000000)
    text = query_value(query, "q").strip()
    difficulty = query_value(query, "difficulty").strip()
    category = query_value(query, "category").strip()
    group_raw = query_value(query, "group_id").strip() or query_value(query, "chat_id").strip()
    group_id = resolve_group_id(db, group_raw) if group_raw else ""
    clauses = ["1=1"]
    params: list = []
    if text:
        clauses.append("(t.title LIKE ? OR t.content LIKE ?)")
        params += [f"%{text}%", f"%{text}%"]
    if difficulty:
        clauses.append("COALESCE(r.difficulty,c.difficulty)=?")
        params.append(difficulty)
    if category:
        clauses.append("t.category=?")
        params.append(category)
    if group_id:
        clauses.append(
            "(EXISTS (SELECT 1 FROM library_selection_history h "
            "WHERE h.text_id=t.text_id AND h.chat_id=?) OR "
            "EXISTS (SELECT 1 FROM library_sent_messages sm "
            "JOIN library_sessions s ON s.session_id=sm.session_id "
            "WHERE s.text_id=t.text_id AND sm.chat_id=?))"
        )
        params += [group_id, group_id]
    where = " WHERE " + " AND ".join(clauses)
    base = (
        "FROM library_texts t LEFT JOIN library_rankings r ON r.text_id=t.text_id "
        "LEFT JOIN library_classifications c ON c.text_id=t.text_id" + where
    )
    total = int(db.execute("SELECT COUNT(*) " + base, tuple(params)).fetchone()[0])
    rows = db.execute(
        "SELECT t.text_id,t.title,t.char_count,t.category,t.relative_path,"
        "COALESCE(r.difficulty,c.difficulty) AS difficulty,r.score AS difficulty_score,"
        "COALESCE(c.primary_genre,'') AS primary_genre,COALESCE(c.form,'') AS form,"
        "COALESCE(c.classified_at,r.ranked_at) AS added_at " + base +
        " ORDER BY added_at DESC, t.text_id LIMIT ? OFFSET ?",
        tuple(params + [limit, offset]),
    ).fetchall()
    items = [
        {"id": row["text_id"], "text_id": row["text_id"], "title": row["title"],
         "char_count": int(row["char_count"] or 0), "category": row["category"],
         "source": row["relative_path"], "relative_path": row["relative_path"],
         "difficulty": row["difficulty"] or "", "difficulty_score": maybe_float(row["difficulty_score"]),
         "primary_genre": row["primary_genre"], "form": row["form"],
         "added_at": row["added_at"], "added_at_text": epoch_text(row["added_at"])}
        for row in rows
    ]
    return {
        "ok": True, "total": total, "limit": limit, "offset": offset,
        "count": len(items), "has_more": offset + len(items) < total,
        "group_id": group_id, "items": items,
    }


def library_text_response(db: sqlite3.Connection, query: dict) -> dict:
    """GET /library/text?id=：单篇详情（正文前 2000 字，全文长度另给）。"""
    text_id = query_value(query, "id").strip() or query_value(query, "text_id").strip()
    if not text_id:
        return {"ok": False, "message": "id (text_id) is required"}
    row = db.execute(
        "SELECT t.text_id,t.title,t.char_count,t.category,t.relative_path,t.content,"
        "COALESCE(r.difficulty,c.difficulty) AS difficulty,r.score AS difficulty_score,"
        "COALESCE(c.primary_genre,'') AS primary_genre,COALESCE(c.form,'') AS form,"
        "COALESCE(c.classified_at,r.ranked_at) AS added_at "
        "FROM library_texts t LEFT JOIN library_rankings r ON r.text_id=t.text_id "
        "LEFT JOIN library_classifications c ON c.text_id=t.text_id "
        "WHERE t.text_id=?",
        (text_id,),
    ).fetchone()
    if row is None:
        return {"ok": False, "message": "text not found", "id": text_id}
    content = row["content"] or ""
    preview_chars = query_int(query, "chars", 2000, 100, 20000)
    preview = content[:preview_chars]
    item = {
        "id": row["text_id"], "text_id": row["text_id"], "title": row["title"],
        "char_count": int(row["char_count"] or 0), "category": row["category"],
        "relative_path": row["relative_path"], "source": row["relative_path"],
        "difficulty": row["difficulty"] or "", "difficulty_score": maybe_float(row["difficulty_score"]),
        "primary_genre": row["primary_genre"], "form": row["form"],
        "added_at": row["added_at"], "added_at_text": epoch_text(row["added_at"]),
        "content": preview, "content_preview": preview, "content_preview_chars": preview_chars,
        "content_length": len(content), "content_truncated": len(content) > len(preview),
    }
    return {"ok": True, "item": item}


# ── 打字成绩（typing_scores，原始流水） ──────────────────

def typing_scope(db: sqlite3.Connection, query: dict) -> tuple[str, list, int, str]:
    """typing_scores 的通用过滤：群（名字或群号）+ 最近 N 天。"""
    clauses = ["recorded_at > 0"]
    params: list = []
    group_raw = (query_value(query, "group_id").strip()
                 or query_value(query, "group").strip()
                 or query_value(query, "chat_id").strip())
    group_id = resolve_group_id(db, group_raw) if group_raw else ""
    if group_id:
        clauses.append("chat_id=?")
        params.append(group_id)
    days = query_int(query, "days", 7, 0, 3650)
    if days > 0:
        clauses.append("recorded_at >= ?")
        params.append(time.time() - days * 86400)
    return " AND ".join(clauses), params, days, group_id


def typing_leaderboard_response(db: sqlite3.Connection, query: dict) -> dict:
    """GET /typing/leaderboard：按均速/键准/条数排行（人数与活动天数一起给）。"""
    where, params, days, group_id = typing_scope(db, query)
    limit = query_int(query, "limit", 20, 1, 200)
    min_records = query_int(query, "min_count", 1, 1, 100000)
    metric = (query_value(query, "metric", "speed").strip().lower() or "speed")
    order_by = {
        "speed": "avg_speed DESC, records DESC",
        "accuracy": "avg_accuracy DESC, records DESC",
        "acc": "avg_accuracy DESC, records DESC",
        "key": "avg_key_rate DESC, records DESC",
        "key_rate": "avg_key_rate DESC, records DESC",
        "count": "records DESC, avg_speed DESC",
        "records": "records DESC, avg_speed DESC",
    }.get(metric, "avg_speed DESC, records DESC")
    rows = db.execute(
        "SELECT user_id,COALESCE(NULLIF(MAX(user_name),''),user_id) AS user_name,"
        "COUNT(*) AS records,AVG(speed) AS avg_speed,MAX(speed) AS best_speed,"
        "AVG(accuracy) AS avg_accuracy,AVG(key_rate) AS avg_key_rate,"
        "AVG(code_length) AS avg_code_length,AVG(word_rate) AS avg_word_rate,"
        "SUM(char_count) AS chars,"
        "COUNT(DISTINCT date(recorded_at,'unixepoch','+8 hours')) AS active_days,"
        "MIN(recorded_at) AS first_at,MAX(recorded_at) AS last_at "
        "FROM typing_scores WHERE " + where +
        " GROUP BY user_id HAVING COUNT(*)>=? ORDER BY " + order_by + " LIMIT ?",
        tuple(params + [min_records, limit]),
    ).fetchall()
    items = []
    for index, row in enumerate(rows, 1):
        items.append({
            "rank": index, "user_id": row["user_id"], "user_name": row["user_name"],
            "records": int(row["records"]), "count": int(row["records"]),
            "avg_speed": maybe_float(row["avg_speed"]), "best_speed": maybe_float(row["best_speed"]),
            "avg_accuracy": maybe_float(row["avg_accuracy"]),
            "avg_key_rate": maybe_float(row["avg_key_rate"]),
            "avg_code_length": maybe_float(row["avg_code_length"]),
            "avg_word_rate": maybe_float(row["avg_word_rate"]),
            "chars": int(row["chars"] or 0), "active_days": int(row["active_days"] or 0),
            "first_at": row["first_at"], "first_at_text": epoch_text(row["first_at"]),
            "last_at": row["last_at"], "last_at_text": epoch_text(row["last_at"]),
        })
    totals = db.execute(
        "SELECT COUNT(*) AS records,COUNT(DISTINCT user_id) AS users,"
        "COUNT(DISTINCT date(recorded_at,'unixepoch','+8 hours')) AS active_days,"
        "AVG(speed) AS avg_speed,MAX(speed) AS best_speed,AVG(accuracy) AS avg_accuracy,"
        "AVG(key_rate) AS avg_key_rate,SUM(char_count) AS chars "
        "FROM typing_scores WHERE " + where,
        tuple(params),
    ).fetchone()
    return {
        "ok": True, "group_id": group_id,
        "group_name": group_display_name(db, group_id) if group_id else "全部群",
        "days": days, "metric": metric, "min_count": min_records,
        "users": int(totals["users"] or 0), "records": int(totals["records"] or 0),
        "active_days": int(totals["active_days"] or 0),
        "avg_speed": maybe_float(totals["avg_speed"]),
        "best_speed": maybe_float(totals["best_speed"]),
        "avg_accuracy": maybe_float(totals["avg_accuracy"]),
        "avg_key_rate": maybe_float(totals["avg_key_rate"]),
        "items": items,
    }


def typing_summary_response(db: sqlite3.Connection, query: dict) -> dict:
    """GET /typing/summary：人数 / 总条数 / 均速均值 / 活跃天数（附每日点集）。"""
    where, params, days, group_id = typing_scope(db, query)
    totals = db.execute(
        "SELECT COUNT(*) AS records,COUNT(DISTINCT user_id) AS users,"
        "COUNT(DISTINCT date(recorded_at,'unixepoch','+8 hours')) AS active_days,"
        "AVG(speed) AS avg_speed,MAX(speed) AS best_speed,AVG(accuracy) AS avg_accuracy,"
        "AVG(key_rate) AS avg_key_rate,AVG(code_length) AS avg_code_length,"
        "AVG(word_rate) AS avg_word_rate,SUM(char_count) AS chars,"
        "MIN(recorded_at) AS first_at,MAX(recorded_at) AS last_at "
        "FROM typing_scores WHERE " + where,
        tuple(params),
    ).fetchone()
    daily_rows = db.execute(
        "SELECT date(recorded_at,'unixepoch','+8 hours') AS day,COUNT(*) AS records,"
        "COUNT(DISTINCT user_id) AS users,AVG(speed) AS avg_speed,MAX(speed) AS best_speed,"
        "AVG(accuracy) AS avg_accuracy,AVG(key_rate) AS avg_key_rate,SUM(char_count) AS chars "
        "FROM typing_scores WHERE " + where + " GROUP BY day ORDER BY day",
        tuple(params),
    ).fetchall()
    daily = [
        {"date": row["day"], "records": int(row["records"]), "users": int(row["users"]),
         "avg_speed": maybe_float(row["avg_speed"]), "best_speed": maybe_float(row["best_speed"]),
         "avg_accuracy": maybe_float(row["avg_accuracy"]),
         "avg_key_rate": maybe_float(row["avg_key_rate"]), "chars": int(row["chars"] or 0)}
        for row in daily_rows
    ]
    top_rows = db.execute(
        "SELECT COALESCE(NULLIF(MAX(user_name),''),user_id) AS user_name,user_id,"
        "COUNT(*) AS records,MAX(speed) AS best_speed,AVG(speed) AS avg_speed,"
        "AVG(accuracy) AS avg_accuracy FROM typing_scores WHERE " + where +
        " GROUP BY user_id ORDER BY best_speed DESC LIMIT 5",
        tuple(params),
    ).fetchall()
    top = [
        {"rank": index, "user_id": row["user_id"], "user_name": row["user_name"],
         "records": int(row["records"]), "best_speed": maybe_float(row["best_speed"]),
         "avg_speed": maybe_float(row["avg_speed"]), "avg_accuracy": maybe_float(row["avg_accuracy"])}
        for index, row in enumerate(top_rows, 1)
    ]
    return {
        "ok": True, "group_id": group_id,
        "group_name": group_display_name(db, group_id) if group_id else "全部群",
        "days": days,
        "users": int(totals["users"] or 0), "records": int(totals["records"] or 0),
        "total_records": int(totals["records"] or 0),
        "active_days": int(totals["active_days"] or 0),
        "avg_speed": maybe_float(totals["avg_speed"]),
        "best_speed": maybe_float(totals["best_speed"]),
        "avg_accuracy": maybe_float(totals["avg_accuracy"]),
        "avg_key_rate": maybe_float(totals["avg_key_rate"]),
        "avg_code_length": maybe_float(totals["avg_code_length"]),
        "avg_word_rate": maybe_float(totals["avg_word_rate"]),
        "chars": int(totals["chars"] or 0),
        "first_at": totals["first_at"], "first_at_text": epoch_text(totals["first_at"]),
        "last_at": totals["last_at"], "last_at_text": epoch_text(totals["last_at"]),
        "daily": daily, "items": daily, "top": top,
    }


def recent_messages_response(db: sqlite3.Connection, query: dict) -> list:
    """GET /recent_messages：后台全局搜索用（recent_messages + message_archive 合并）。"""
    limit = query_int(query, "limit", 20, 1, 300)
    offset = query_int(query, "offset", 0, 0, 100000)
    text = query_value(query, "q").strip()
    group_raw = query_value(query, "group_id").strip() or query_value(query, "chat_id").strip()
    group_id = resolve_group_id(db, group_raw) if group_raw else ""
    platform = query_value(query, "platform").strip()

    def build(table: str):
        clauses, params = ["occurred_at > 0"], []
        if text:
            clauses.append("(text LIKE ? OR sender_name LIKE ? OR group_name LIKE ? OR sender_id LIKE ?)")
            params += [f"%{text}%"] * 4
        if group_id:
            clauses.append("group_id=?")
            params.append(group_id)
        if platform:
            clauses.append("platform=?")
            params.append(platform)
        return (
            "SELECT platform,message_id,group_id,group_name,sender_id,sender_name,text,occurred_at "
            "FROM " + table + " WHERE " + " AND ".join(clauses)
        ), params

    sql_recent, params_recent = build("recent_messages")
    sql_archive, params_archive = build("message_archive")
    rows = db.execute(
        "SELECT * FROM (" + sql_recent + " UNION ALL " + sql_archive + ")"
        " ORDER BY occurred_at DESC LIMIT ? OFFSET ?",
        tuple(params_recent + params_archive + [limit, offset]),
    ).fetchall()
    return [
        {"platform": row["platform"], "message_id": row["message_id"],
         "group_id": row["group_id"], "group_name": row["group_name"],
         "sender_id": row["sender_id"], "sender_name": row["sender_name"],
         "text": row["text"], "occurred_at": row["occurred_at"],
         "occurred_at_text": epoch_text(row["occurred_at"])}
        for row in rows
    ]


def ai_contest_scores_response(db: sqlite3.Connection, query: dict) -> list:
    """GET /ai-contest/scores：AI 赛文成绩（后台成员成绩明细会用）。"""
    name = query_value(query, "name").strip() or query_value(query, "user").strip()
    user_id = query_value(query, "user_id").strip()
    group_raw = query_value(query, "group_id").strip() or query_value(query, "group").strip()
    group_id = resolve_group_id(db, group_raw) if group_raw else ""
    date_raw = query_value(query, "date").strip()
    days = query_int(query, "days", 0, 0, 3650)
    limit = query_int(query, "limit", 100, 1, 5000)
    clauses, params = ["1=1"], []
    if name:
        clauses.append("(user_name LIKE ? OR user_id=?)")
        params += [f"%{name}%", name]
    if user_id:
        clauses.append("user_id=?")
        params.append(user_id)
    if group_id:
        clauses.append("group_id=?")
        params.append(group_id)
    if date_raw:
        clauses.append("competition_date=?")
        params.append(china_date_text(date_raw))
    if days > 0:
        since = (china_now().date() - timedelta(days=days - 1)).isoformat()
        clauses.append("competition_date>=?")
        params.append(since)
    rows = db.execute(
        "SELECT source_json FROM ai_contest_scores WHERE " + " AND ".join(clauses) +
        " ORDER BY occurred_at DESC LIMIT ?",
        tuple(params + [limit]),
    ).fetchall()
    items = []
    for row in rows:
        try:
            value = json.loads(row["source_json"])
        except (TypeError, ValueError, json.JSONDecodeError):
            continue
        if isinstance(value, dict):
            items.append(value)
    return items


def version_response(db: sqlite3.Connection, db_path: str) -> dict:
    """GET /version：app / 启动时间 / 数据库路径 / 表清单（含行数）。"""
    rows = db.execute(
        "SELECT name FROM sqlite_master WHERE type IN ('table','view') ORDER BY name"
    ).fetchall()
    names = [str(row["name"]) for row in rows]
    return {
        "ok": True, "app": APP_NAME, "version": APP_VERSION,
        "started_at": SERVER_STARTED_AT,
        "started_at_text": epoch_text(SERVER_STARTED_AT),
        "uptime_seconds": round(time.time() - SERVER_STARTED_AT, 1),
        "db_path": db_path, "table_count": len(names), "tables": names,
        "table_counts": table_counts(db, names),
    }


def stats_advanced_response(db: sqlite3.Connection) -> dict:
    """GET /stats/advanced：老后台「高级面板」用的扩展统计（原来 404）。"""
    counts = stats_response(db)
    contest = [
        {"key": row["contest_key"], "name": row["contest_name"], "scores": int(row["scores"]),
         "users": int(row["users"]), "first_date": row["first_date"], "last_date": row["last_date"]}
        for row in db.execute(
            "SELECT contest_key,MAX(contest_name) AS contest_name,COUNT(*) AS scores,"
            "COUNT(DISTINCT user_id) AS users,MIN(score_date) AS first_date,"
            "MAX(score_date) AS last_date FROM contest_scores "
            "GROUP BY contest_key ORDER BY scores DESC"
        ).fetchall()
    ]
    typing_recent = {}
    for label, days in (("today", 1), ("7d", 7), ("30d", 30)):
        typing_recent[label] = typing_summary_response(db, {"days": [str(days)]})
    messages_daily = [
        {"date": row["day"], "messages": int(row["messages"]), "users": int(row["users"])}
        for row in db.execute(
            "SELECT date(occurred_at,'unixepoch','+8 hours') AS day,COUNT(*) AS messages,"
            "COUNT(DISTINCT sender_id) AS users FROM message_archive "
            "WHERE occurred_at>0 GROUP BY day ORDER BY day DESC LIMIT 30"
        ).fetchall()
    ][::-1]
    novel = novel_shelf_stats(db)
    novel.pop("items", None)
    return {
        "ok": True, "generated_at": time.time(),
        "counts": counts, "contest": contest, "typing": typing_recent,
        "library": library_stats(db), "novel": novel,
        "messages_daily": messages_daily, "archive": archive_status(db),
    }


# ── 赛文（contest_scores + 官网实时） ────────────────────

def known_contests_payload() -> list:
    return [
        {"key": key, "name": name, "scope": scope}
        for key, (name, scope) in CONTEST_LABELS.items()
    ]


def contest_rank_rows(db: sqlite3.Connection, contest_key: str, group_id: str, scope: str,
                      days: int, limit: int = 12, use_live: bool = True):
    """某赛文的排行数据 —— /contest/chart.png 与 /contest/chart-data 共用这一个取数函数。

    官网优先（所有者 2026-11-06 的要求），拿不到回落成绩库。
    返回 (rows, source_label)；rows 的元素是 dict（user_name/user_id/best/acc/n）。
    """
    room = group_id if scope == "group" else ""
    if use_live:
        live_rows = contest_live_rows(contest_key, room)
        if live_rows:
            rows = [
                {"user_name": row.get("name") or row.get("user_id") or "?",
                 "user_id": str(row.get("user_id") or ""),
                 "best": float(row.get("speed") or 0),
                 "acc": row.get("accuracy"), "n": 1}
                for row in live_rows
            ]
            rows.sort(key=lambda item: item["best"], reverse=True)
            return merge_contest_rows(rows[:limit]), "官网实时"
    clause, params = "", []
    if days > 0:
        clause = " AND c.recorded_at >= ?"
        params = [time.time() - days * 86400]
    rows = db.execute(
        "SELECT COALESCE(NULLIF(c.user_name, ''), "
        "  (SELECT t.user_name FROM typing_scores t WHERE t.user_id=c.user_id AND t.user_name<>'' LIMIT 1), "
        "  c.user_id) AS user_name, "
        "c.user_id, MAX(c.speed) AS best, AVG(c.accuracy) AS acc, "
        "AVG(c.key_rate) AS key_rate, COUNT(*) AS n "
        "FROM contest_scores c WHERE c.contest_key=? AND c.group_id=?" + clause +
        " GROUP BY c.user_id ORDER BY best DESC LIMIT ?",
        tuple([contest_key, room] + params + [limit]),
    ).fetchall()
    return merge_contest_rows([dict(row) for row in rows]), "成绩库"


def contest_user_points(db: sqlite3.Connection, contest_key: str, group_id: str,
                        days: int, user: str) -> list:
    """按天聚合某个人的赛文成绩（复用 /reports/score-chart.png 的 contest_score_history）。"""
    history = contest_score_history(db, contest_key, group_id, user, days)
    buckets: dict[str, dict] = {}
    for item in history:
        day = str(item.get("competition_date") or "")
        if not day:
            continue
        bucket = buckets.setdefault(day, {"speed": [], "acc": [], "key": [], "code": []})
        for field, target in (("speed", "speed"), ("acc", "acc"),
                              ("key_rate", "key"), ("code_length", "code")):
            value = item.get(field)
            if value is None:
                continue
            try:
                bucket[target].append(float(value))
            except (TypeError, ValueError):
                continue
    points = []
    for day in sorted(buckets):
        bucket = buckets[day]
        points.append({
            "date": day,
            "avg_speed": round(sum(bucket["speed"]) / len(bucket["speed"]), 2) if bucket["speed"] else None,
            "best_speed": round(max(bucket["speed"]), 2) if bucket["speed"] else None,
            "avg_accuracy": round(sum(bucket["acc"]) / len(bucket["acc"]), 2) if bucket["acc"] else None,
            "avg_key_rate": round(sum(bucket["key"]) / len(bucket["key"]), 2) if bucket["key"] else None,
            "avg_code_length": round(sum(bucket["code"]) / len(bucket["code"]), 2) if bucket["code"] else None,
            "users": 1, "records": len(bucket["speed"]),
        })
    return points


def contest_chart_data(db: sqlite3.Connection, query: dict) -> dict:
    """GET /contest/chart-data：给后台前端直接画曲线的数据点集。

    数据源和 /contest/chart.png 完全一致（同一个 contest_rank_rows / contest_day_stats），
    只是把「画图」换成「返回 JSON」。live=0 时只用成绩库（更快，默认）。
    """
    raw = query_value(query, "contest").strip()
    if not raw:
        return {"ok": False, "message": "contest is required", "known": known_contests_payload()}
    group_hint = query_value(query, "group_id").strip() or query_value(query, "group").strip()
    if group_hint:
        resolved = resolve_group_id(db, group_hint)
        if resolved:
            query = {**query, "group_id": [resolved]}
    lookup = contest_chart_lookup(db, query)
    if not lookup:
        return {"ok": False, "message": "unknown contest", "contest": raw,
                "known": known_contests_payload()}
    key, name, scope, group_id = lookup
    days = query_int(query, "days", 30, 0, 3660)
    user = query_value(query, "user").strip()
    mode = (query_value(query, "mode", "all").strip().lower() or "all")
    limit = query_int(query, "limit", 12, 1, 100)
    use_live = query_flag(query, "live", False)
    rank_rows, rank_source = contest_rank_rows(db, key, group_id, scope, days, limit, use_live)
    rows = []
    for index, item in enumerate(rank_rows, 1):
        rows.append({
            "rank": index,
            "user_id": str(item.get("user_id") or ""),
            "user_name": item.get("user_name") or item.get("user_id") or "?",
            "best_speed": maybe_float(item.get("best")),
            "avg_accuracy": maybe_float(item.get("acc")),
            "avg_key_rate": maybe_float(item.get("key_rate")),
            "records": int(item.get("n") or 1),
        })
    points: list = []
    point_source = ""
    # days=0 在柱状图里是「全部时间」，但曲线没法画全部（点数太多）——按 30 天给点集。
    point_days = days if days > 0 else 30
    if mode in {"all", "trend", "points", ""}:
        if user:
            points = contest_user_points(db, key, group_id, point_days, user)
            point_source = "成绩库（个人）"
        else:
            points = contest_day_stats(db, key, group_id, scope, point_days, use_live=use_live)
            point_source = "官网实时优先，取不到回落成绩库" if use_live else "成绩库"
    return {
        "ok": True,
        "contest": {"key": key, "name": name, "scope": scope,
                    "group_id": group_id,
                    "group_name": group_display_name(db, group_id) if group_id else ""},
        "days": days, "user": user, "mode": mode, "live": use_live,
        "rank_source": rank_source, "rows": rows, "count": len(rows),
        "point_source": point_source, "points": points, "point_count": len(points),
    }


def contests_catalog(db: sqlite3.Connection) -> dict:
    """GET /contests/list：赛文清单（CONTEST_LABELS + 每组 type/数量/最近日期）。"""
    items = []
    for key, (name, scope) in CONTEST_LABELS.items():
        total = db.execute(
            "SELECT COUNT(*) AS n,COUNT(DISTINCT user_id) AS users,"
            "MIN(score_date) AS first_date,MAX(score_date) AS last_date "
            "FROM contest_scores WHERE contest_key=?",
            (key,),
        ).fetchone()
        group_rows = db.execute(
            "SELECT group_id,COUNT(*) AS n,COUNT(DISTINCT user_id) AS users,"
            "MAX(score_date) AS last_date FROM contest_scores "
            "WHERE contest_key=? AND group_id<>'' GROUP BY group_id ORDER BY n DESC",
            (key,),
        ).fetchall()
        groups = [
            {"group_id": row["group_id"],
             "group_name": group_display_name(db, row["group_id"]),
             "scores": int(row["n"]), "users": int(row["users"]),
             "last_score_date": row["last_date"] or ""}
            for row in group_rows
        ]
        live_source = CONTEST_LIVE_SOURCES.get(key, "")
        if not live_source and key == "999":
            live_source = "按群号（官网 source=群号）"
        item = {
            "key": key, "name": name, "scope": scope, "type": scope,
            "scores": int(total["n"] or 0), "users": int(total["users"] or 0),
            "first_score_date": total["first_date"] or "",
            "last_score_date": total["last_date"] or "",
            "group_count": len(groups), "groups": groups,
            "live_source": live_source,
        }
        if key == "10000":
            item["alias_of"] = "100000"
        items.append(item)
    items.sort(key=lambda item: -item["scores"])
    return {"ok": True, "count": len(items), "items": items}


# ── 群日报（messages + typing + contest） ────────────────

def daily_report_response(db: sqlite3.Connection, query: dict) -> dict:
    """GET /reports/daily?group_id=&date=：群日报数据（消息量/活跃人数/Top打字/赛文参与）。"""
    group_raw = query_value(query, "group_id").strip() or query_value(query, "group").strip()
    group_id = resolve_group_id(db, group_raw) if group_raw else ""
    date_text = china_date_text(query_value(query, "date").strip())
    start, end = china_day_range(date_text)
    group_sql = " AND group_id=?" if group_id else ""
    group_params = [group_id] if group_id else []
    message_params = [start, end] + group_params + [start, end] + group_params
    messages_row = db.execute(
        "SELECT COUNT(*) AS messages,COUNT(DISTINCT sender_id) AS users FROM ("
        "SELECT platform,message_id,group_id,sender_id FROM recent_messages "
        "WHERE occurred_at>=? AND occurred_at<?" + group_sql +
        " UNION SELECT platform,message_id,group_id,sender_id FROM message_archive "
        "WHERE occurred_at>=? AND occurred_at<?" + group_sql + ")",
        tuple(message_params),
    ).fetchone()
    hour_rows = db.execute(
        "SELECT CAST((occurred_at + 28800) % 86400 / 3600 AS INTEGER) AS hour,"
        "COUNT(*) AS messages FROM ("
        "SELECT occurred_at,sender_id,group_id FROM recent_messages "
        "WHERE occurred_at>=? AND occurred_at<?" + group_sql +
        " UNION ALL SELECT occurred_at,sender_id,group_id FROM message_archive "
        "WHERE occurred_at>=? AND occurred_at<?" + group_sql +
        ") GROUP BY hour ORDER BY hour",
        tuple(message_params),
    ).fetchall()
    by_hour = [0] * 24
    for row in hour_rows:
        hour = int(row["hour"] or 0)
        if 0 <= hour < 24:
            by_hour[hour] = int(row["messages"] or 0)
    sender_rows = db.execute(
        "SELECT sender_id,MAX(sender_name) AS sender_name,COUNT(*) AS messages FROM ("
        "SELECT sender_id,sender_name,group_id FROM recent_messages "
        "WHERE occurred_at>=? AND occurred_at<?" + group_sql +
        " UNION ALL SELECT sender_id,sender_name,group_id FROM message_archive "
        "WHERE occurred_at>=? AND occurred_at<?" + group_sql +
        ") GROUP BY sender_id ORDER BY messages DESC LIMIT 8",
        tuple(message_params),
    ).fetchall()
    group_rows = db.execute(
        "SELECT group_id,MAX(group_name) AS group_name,COUNT(*) AS messages,"
        "COUNT(DISTINCT sender_id) AS users FROM ("
        "SELECT group_id,group_name,sender_id FROM recent_messages "
        "WHERE occurred_at>=? AND occurred_at<?"
        " UNION ALL SELECT group_id,group_name,sender_id FROM message_archive "
        "WHERE occurred_at>=? AND occurred_at<?"
        ") GROUP BY group_id ORDER BY messages DESC LIMIT 20",
        (start, end, start, end),
    ).fetchall()
    typing_where = "recorded_at>=? AND recorded_at<?" + (" AND chat_id=?" if group_id else "")
    typing_params = [start, end] + group_params
    typing_rows = db.execute(
        "SELECT COALESCE(NULLIF(MAX(user_name),''),user_id) AS user_name,user_id,"
        "COUNT(*) AS records,MAX(speed) AS best_speed,AVG(speed) AS avg_speed,"
        "AVG(accuracy) AS avg_accuracy,AVG(key_rate) AS avg_key_rate "
        "FROM typing_scores WHERE " + typing_where +
        " GROUP BY user_id ORDER BY best_speed DESC LIMIT 10",
        tuple(typing_params),
    ).fetchall()
    typing_totals = db.execute(
        "SELECT COUNT(*) AS records,COUNT(DISTINCT user_id) AS users "
        "FROM typing_scores WHERE " + typing_where,
        tuple(typing_params),
    ).fetchone()
    contest_rows = db.execute(
        "SELECT contest_key,MAX(contest_name) AS contest_name,COUNT(*) AS scores,"
        "COUNT(DISTINCT user_id) AS users,MAX(speed) AS best_speed "
        "FROM contest_scores WHERE score_date=?" + (" AND group_id=?" if group_id else "") +
        " GROUP BY contest_key ORDER BY scores DESC",
        tuple([date_text] + group_params),
    ).fetchall()
    ai_row = db.execute(
        "SELECT COUNT(*) AS scores,COUNT(DISTINCT user_id) AS users,MAX(speed) AS best_speed "
        "FROM ai_contest_scores WHERE competition_date=?" + (" AND group_id=?" if group_id else ""),
        tuple([date_text] + group_params),
    ).fetchone()
    sent_count = db.execute(
        "SELECT COUNT(*) FROM library_sent_messages WHERE sent_at>=? AND sent_at<?" +
        (" AND chat_id=?" if group_id else ""),
        tuple([start, end] + group_params),
    ).fetchone()[0]
    recall_count = db.execute(
        "SELECT COUNT(*) FROM recall_records WHERE recalled_ts>=? AND recalled_ts<?" +
        (" AND group_id=?" if group_id else ""),
        tuple([start, end] + group_params),
    ).fetchone()[0]
    digest = None
    if group_id:
        digest_row = db.execute(
            "SELECT * FROM group_digests WHERE platform='qq' AND group_id=? AND day=?",
            (group_id, date_text),
        ).fetchone()
        if digest_row:
            try:
                topics = json.loads(digest_row["topics_json"] or "[]")
            except (TypeError, ValueError, json.JSONDecodeError):
                topics = []
            digest = {
                "summary": digest_row["summary"], "topics": topics,
                "msg_count": int(digest_row["msg_count"] or 0),
                "char_count": int(digest_row["char_count"] or 0),
                "model": digest_row["model"], "verified": bool(digest_row["verified"]),
            }
    return {
        "ok": True, "group_id": group_id,
        "group_name": group_display_name(db, group_id) if group_id else "全部群",
        "date": date_text,
        "messages": {
            "total": int(messages_row["messages"] or 0),
            "active_users": int(messages_row["users"] or 0),
            "by_hour": by_hour,
            "top_senders": [
                {"sender_id": row["sender_id"], "sender_name": row["sender_name"],
                 "messages": int(row["messages"])}
                for row in sender_rows
            ],
            "by_group": [
                {"group_id": row["group_id"], "group_name": row["group_name"],
                 "messages": int(row["messages"]), "users": int(row["users"])}
                for row in group_rows
            ],
        },
        "typing": {
            "records": int(typing_totals["records"] or 0),
            "users": int(typing_totals["users"] or 0),
            "top": [
                {"rank": index, "user_id": row["user_id"], "user_name": row["user_name"],
                 "records": int(row["records"]), "best_speed": maybe_float(row["best_speed"]),
                 "avg_speed": maybe_float(row["avg_speed"]),
                 "avg_accuracy": maybe_float(row["avg_accuracy"]),
                 "avg_key_rate": maybe_float(row["avg_key_rate"])}
                for index, row in enumerate(typing_rows, 1)
            ],
        },
        "contest": {
            "scores": sum(int(row["scores"]) for row in contest_rows),
            "users": len({str(row["contest_key"]) for row in contest_rows}),
            "items": [
                {"key": row["contest_key"], "name": row["contest_name"],
                 "scores": int(row["scores"]), "users": int(row["users"]),
                 "best_speed": maybe_float(row["best_speed"])}
                for row in contest_rows
            ],
        },
        "ai_contest": {
            "scores": int(ai_row["scores"] or 0), "users": int(ai_row["users"] or 0),
            "best_speed": maybe_float(ai_row["best_speed"]),
        },
        "library": {"sent": int(sent_count or 0), "recalls": int(recall_count or 0)},
        "digest": digest,
    }


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
        """GET 一律回 JSON —— 后台代理把连接被重置显示成 503，任何异常都不该断连接。"""
        try:
            return self._do_get_impl()
        except Exception as exc:  # noqa: BLE001
            try:
                self.json({"ok": False, "message": f"{type(exc).__name__}: {exc}"},
                          HTTPStatus.INTERNAL_SERVER_ERROR)
            except Exception:
                pass

    def _do_get_impl(self):
        request = urlparse(self.path)
        query = parse_backend_query(request.query)
        db = connect(self.db_path)
        try:
            if request.path == "/health":
                return self.json({"ok": True})
            if request.path == "/groups":
                rows = db.execute("SELECT g.group_id,g.display_name,g.status,g.features_json,g.updated_at,COALESCE(r.paused,0) AS paused FROM groups g LEFT JOIN group_runtime r ON r.group_id=g.group_id ORDER BY g.display_name,g.group_id").fetchall()
                return self.json([dict(row) for row in rows])
            if request.path == "/groups/names":
                names = {}
                # 与排行榜标题同一套别名表（GROUP_NAMES_BY_ID），保证标题和群名一致
                for alias_gid, alias_name in GROUP_NAMES_BY_ID.items():
                    alias_name = str(alias_name or "").strip()
                    if alias_name:
                        names[str(alias_gid)] = alias_name
                for table, id_col, name_col in (
                    ("groups", "group_id", "display_name"),
                    ("group_digests", "group_id", "group_name"),
                    ("recent_messages", "group_id", "group_name"),
                ):
                    try:
                        rows = db.execute(
                            f"SELECT {id_col} AS gid, {name_col} AS name FROM {table} "
                            f"WHERE {name_col} IS NOT NULL AND {name_col} != ''"
                        ).fetchall()
                        for row in rows:
                            gid = str(row["gid"] or "").strip()
                            name = str(row["name"] or "").strip()
                            if gid and name and gid not in names and name not in ("本群", "本群赛文"):
                                names[gid] = name
                    except Exception:
                        continue
                # 群名兜底：群赛文在 contest_scores.contest_name 里就是群名（如「剑影赛文」）
                try:
                    for row in db.execute(
                        "SELECT group_id, contest_name FROM contest_scores "
                        "WHERE contest_name IS NOT NULL AND contest_name != '' "
                        "ORDER BY score_date DESC, rowid DESC"
                    ).fetchall():
                        gid = str(row["group_id"] or "").strip()
                        name = str(row["contest_name"] or "").strip()
                        if name.endswith("赛文"):
                            name = name[: -len("赛文")].strip()
                        if gid and name and gid not in names:
                            names[gid] = name
                except Exception:
                    pass
                body = "\n".join(f"{gid}\t{name}" for gid, name in names.items())
                return self.bytes(body.encode("utf-8"), "text/plain; charset=utf-8")
            if request.path == "/group-state":
                group_id = query.get("group_id", [""])[0].strip()
                row = db.execute("SELECT paused,updated_at FROM group_runtime WHERE group_id=?", (group_id,)).fetchone()
                return self.json({
                    "ok": True,
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
                    return self.json({"ok": False, "error": "group_id is required",
                                      "message": "group_id is required"}, HTTPStatus.BAD_REQUEST)
                if capability:
                    if capability not in GROUP_CAPABILITIES:
                        return self.json({"ok": False, "error": "unknown capability",
                                          "message": "unknown capability"}, HTTPStatus.BAD_REQUEST)
                    return self.json({
                        "ok": True,
                        "group_id": group_id,
                        "capability": capability,
                        "enabled": group_capability_enabled(db, group_id, capability),
                    })
                features = group_features(db, group_id)
                return self.json({
                    "ok": True,
                    "group_id": group_id,
                    "capabilities": {
                        name: bool(features.get(name, True)) for name in sorted(GROUP_CAPABILITIES)
                    },
                })
            if request.path == "/library/search":
                text = query.get("q", [""])[0].strip()
                limit = query_int(query, "limit", 10, 1, 30)
                rows = db.execute(
                    "SELECT t.text_id,t.title,t.category,t.relative_path,t.char_count,"
                    "COALESCE(r.difficulty,c.difficulty) AS difficulty,r.score AS difficulty_score "
                    "FROM library_texts t LEFT JOIN library_rankings r ON r.text_id=t.text_id "
                    "LEFT JOIN library_classifications c ON c.text_id=t.text_id "
                    "WHERE t.title LIKE ? OR t.content LIKE ? ORDER BY t.char_count LIMIT ?",
                    (f"%{text}%", f"%{text}%", limit),
                ).fetchall()
                return self.json([dict(row) for row in rows])
            if request.path == "/library/pick":
                text = query.get("q", [""])[0].strip()
                rows = db.execute("SELECT text_id,title,category,relative_path,content,char_count FROM library_texts WHERE title LIKE ? OR content LIKE ? ORDER BY char_count LIMIT 50", (f"%{text}%", f"%{text}%")).fetchall()
                return self.json(dict(random.choice(rows)) if rows else None)
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
                text = query.get("q", [""])[0].strip()
                limit = query_int(query, "limit", 20, 1, 100)
                sql = (
                    "SELECT source_json FROM score_records "
                    "WHERE group_id LIKE ? AND sender_id LIKE ?"
                )
                params: list = [f"%{group_id}%", f"%{sender_id}%"]
                if text:
                    # 后台全局搜索会带 q：原始成绩 JSON 里同时有名字和原文，直接大字段 LIKE。
                    sql += " AND source_json LIKE ?"
                    params.append(f"%{text}%")
                sql += (
                    " ORDER BY CASE WHEN typeof(occurred_at)='text' "
                    "THEN CAST(strftime('%s', occurred_at) AS REAL) "
                    "ELSE CAST(occurred_at AS REAL) END DESC LIMIT ?"
                )
                rows = db.execute(sql, tuple(params + [limit])).fetchall()
                return self.json([json.loads(row["source_json"]) for row in rows])
            if request.path == "/contest/search":
                text = query.get("q", [""])[0].strip()
                source_group = query.get("source", [""])[0].strip()
                competition_date = query.get("date", [""])[0].strip()
                limit = query_int(query, "limit", 10, 1, 30)
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
            if request.path == "/reports/all-ranks.png":
                db = connect(self.db_path)
                try:
                    room = (query.get("group_id", [""])[0] or "").strip()
                    top_n = query_int(query, "limit", 5, 1, 10)
                    return self.bytes(render_all_ranks(db, room, top_n), "image/png")
                finally:
                    db.close()
            if request.path == "/reports/ai-leaderboard.png":
                competition_date = query.get("date", [""])[0].strip()
                return self.bytes(render_ai_leaderboard(db, competition_date), "image/png")
            if request.path == "/competition/scores":
                user_id = query.get("user_id", [""])[0].strip()
                name = query.get("name", [""])[0].strip()
                source_group = query.get("source", [""])[0].strip()
                limit = query_int(query, "limit", 20, 1, 100)
                rows = db.execute(
                    "SELECT source_json FROM competition_scores WHERE user_id LIKE ? AND user_name LIKE ? "
                    "AND source_group LIKE ? ORDER BY occurred_at DESC LIMIT ?",
                    (f"%{user_id}%", f"%{name}%", f"%{source_group}%", limit),
                ).fetchall()
                return self.json([json.loads(row["source_json"]) for row in rows])
            if request.path == "/analysis/scores":
                user_id = query.get("user_id", [""])[0].strip()
                name = query.get("name", [""])[0].strip()
                scope = query.get("scope", ["all"])[0].strip() or "all"
                start_date = query.get("start_date", [""])[0].strip()
                end_date = query.get("end_date", [""])[0].strip()
                source = query.get("source", [""])[0].strip()
                try:
                    days = int(query.get("days", ["1"])[0])
                    result = build_score_analysis(
                        db, user_id, name, scope, start_date, end_date, days, source,
                    )
                except (TypeError, ValueError) as exc:
                    return self.json({"status": "invalid", "message": str(exc)}, HTTPStatus.BAD_REQUEST)
                return self.json(result)
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
                    # days=0 表示「全部」—— 以前 max(...,1) 把它改成 1 天，
                    # 于是「我全部的虎杯成绩」画出来是空的（2026-11-06 实测）。
                    days = min(max(int(query.get("days", ["3650"])[0]), 0), 3650)
                except ValueError:
                    days = 30
                contest_key = query.get("contest", [""])[0].strip()
                group_id_param = resolve_group_id(db, query.get("group_id", [""])[0].strip())
                return self.bytes(
                    render_score_chart(db, user_id, name, source_group, days, metric,
                                       contest=contest_key, group_id=group_id_param,
                                       contest_user=query.get("user", [""])[0].strip()),
                    "image/png",
                )
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
                page = 1 if not requested_page else min(
                    max(query_int(query, "page", 1, 1, 10000), 1), live["page_count"])
                return self.bytes(render_public_competition_rank(live, page, combined), "image/png")
            if request.path == "/contest/top":
                db = connect(self.db_path)
                try:
                    return self.json(contest_top_response(db, query))
                finally:
                    db.close()
            if request.path == "/contest/count":
                db = connect(self.db_path)
                try:
                    return self.json(contest_count_response(db, query))
                finally:
                    db.close()
            if request.path == "/contest/chart.png":
                db = connect(self.db_path)
                try:
                    lookup = contest_chart_lookup(db, query)
                    if not lookup:
                        return self.json({"error": "unknown contest"}, HTTPStatus.BAD_REQUEST)
                    key, name, scope, group_id = lookup
                    days = query_int(query, "days", 0, 0, 3660)
                    metric = (query.get("metric", ["speed"])[0] or "speed").lower()
                    mode = (query.get("mode", ["rank"])[0] or "rank").lower()
                    if mode == "trend":
                        # 曲线模式（所有者 2026-11-06 要的「近一个月成绩曲线图」）
                        span = days if days > 0 else 30
                        who = (query.get("user", [""])[0] or "").strip()
                        return self.bytes(render_contest_trend(db, key, group_id, name, scope, span, who), "image/png")
                    return self.bytes(render_contest_chart(db, key, group_id, name, scope, days, metric), "image/png")
                finally:
                    db.close()
            if request.path == "/contest/rank-table.png":
                db = connect(self.db_path)
                try:
                    if not contest_chart_lookup(db, query):
                        return self.json({"error": "unknown contest"}, HTTPStatus.BAD_REQUEST)
                    return self.bytes(render_contest_rank_table(db, query), "image/png")
                finally:
                    db.close()
            if request.path == "/stats":
                return self.json(stats_response(db))
            if request.path == "/archive/status":
                status = archive_status(db)
                status.setdefault("ok", True)
                return self.json(status)

            # ── 后台控制台只读接口（2026-11-15） ──
            if request.path == "/contests/today":
                db = connect(self.db_path)
                try:
                    room = (query.get("group_id", [""])[0] or "").strip()
                    return self.json(cached_contests_today_payload(db, room))
                finally:
                    db.close()
            if request.path == "/huma/annotate.png":
                raw = (query.get("text", [""])[0] or "").strip()
                if not raw:
                    return self.json({"ok": False, "error": "text 不能为空"}, HTTPStatus.BAD_REQUEST)
                try:
                    cleaned = _clean_huma_article(raw)
                    return self.bytes(huma_annotate.render_png(cleaned), "image/png")
                except Exception as exc:  # noqa: BLE001
                    return self.json(
                        {"ok": False, "error": f"{type(exc).__name__}: {exc}"},
                        HTTPStatus.BAD_REQUEST,
                    )
            if request.path == "/version":
                return self.json(version_response(db, self.db_path))
            if request.path == "/stats/advanced":
                return self.json(stats_advanced_response(db))
            if request.path == "/recent_messages":
                return self.json(recent_messages_response(db, query))
            if request.path == "/ai-contest/scores":
                return self.json(ai_contest_scores_response(db, query))
            # 番茄小说：读接口 GET / POST 都可以用（老 bot 仍走 POST，见 do_POST）
            if request.path == "/novel/stats":
                return self.json(novel_shelf_stats(db))
            if request.path == "/novel/books":
                return self.json(novel_books_shelf(db, query))
            if request.path == "/novel/book":
                result = novel_book_detail(db, query)
                status = result.pop("status", None)
                code = HTTPStatus(status) if status in (400, 404) else HTTPStatus.OK
                return self.json(result, code)
            if request.path in {"/novel/progress", "/novel/read-progress"}:
                return self.json(novel_progress_report(db, query))
            if request.path == "/novel/score":
                return self.json(novel_stats(db, query_payload(query)))
            if request.path in {"/novel/start", "/novel/continue", "/novel/next",
                                "/novel/jump", "/novel/stop"}:
                return self.json(
                    {"ok": False, "message": "该接口有写入行为，请使用 POST。", "path": request.path},
                    HTTPStatus.METHOD_NOT_ALLOWED,
                )
            # 赛文
            if request.path == "/contest/chart-data":
                return self.json(contest_chart_data(db, query))
            if request.path == "/contests/list":
                return self.json(contests_catalog(db))
            # 文库
            if request.path == "/library/list":
                return self.json(library_list_response(db, query))
            if request.path == "/library/text":
                return self.json(library_text_response(db, query))
            # 打字成绩
            if request.path == "/typing/leaderboard":
                return self.json(typing_leaderboard_response(db, query))
            if request.path == "/typing/summary":
                return self.json(typing_summary_response(db, query))
            if request.path == "/typing/stats":
                payload = query_payload(query)
                for key in ("days", "limit"):
                    if payload.get(key) not in (None, ""):
                        try:
                            payload[key] = int(float(payload[key]))
                        except (TypeError, ValueError):
                            payload.pop(key, None)
                return self.json(typing_stats_dispatch(db, payload))
            # 群日报
            if request.path == "/reports/daily":
                return self.json(daily_report_response(db, query))
            return self.json({"ok": False, "error": "not found", "message": "not found"},
                             HTTPStatus.NOT_FOUND)
        finally:
            db.close()

    def do_POST(self):
        """POST 同样兜底成 JSON 错误：后台代理/机器人都不该被断连接。"""
        try:
            return self._do_post_impl()
        except Exception as exc:  # noqa: BLE001
            try:
                self.json({"ok": False, "error": f"{type(exc).__name__}: {exc}"},
                          HTTPStatus.INTERNAL_SERVER_ERROR)
            except Exception:
                pass

    def _do_post_impl(self):
        request = urlparse(self.path)
        if request.path not in {
            "/novel/start", "/novel/continue", "/novel/next", "/novel/jump",
            "/novel/progress", "/novel/stats",
            "/novel/books",
            "/novel/stop",
            "/novel/score",
            "/typing/stats",
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
                limit = max(0, int(payload.get("limit") or 0))
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

        if request.path == "/typing/stats":
            db = connect(self.db_path)
            try:
                return self.json(typing_stats_dispatch(db, payload))
            finally:
                db.close()

        if request.path.startswith("/novel/"):
            # 番茄小说专区（2026-11-05）。和库文是两套逻辑：
            # 这里按「书 + 章 + 章内偏移」推进，进度只记发起人。
            db = connect(self.db_path)
            try:
                try:
                    if request.path == "/novel/start":
                        result = novel_start(db, payload)
                    elif request.path == "/novel/continue":
                        result = novel_continue(db, payload)
                    elif request.path == "/novel/next":
                        result = novel_next(db, payload)
                    elif request.path == "/novel/jump":
                        result = novel_jump(db, payload)
                    elif request.path == "/novel/progress":
                        result = novel_progress_info(db, payload)
                    elif request.path == "/novel/books":
                        result = novel_books_list(db, payload)
                    elif request.path == "/novel/stop":
                        result = novel_stop(db, payload)
                    elif request.path == "/novel/score":
                        result = novel_score(db, payload)
                    elif request.path == "/novel/stats":
                        # 后台 /api/v2/novels 会 POST 一个空对象问书架；
                        # 带 book/身份字段的调用仍走原来的「这本书的个人成绩」。
                        result = novel_shelf_stats(db) if not payload else novel_stats(db, payload)
                    else:
                        return self.json({"error": "not found"}, HTTPStatus.NOT_FOUND)
                except (TypeError, ValueError) as exc:
                    return self.json({"status": "invalid", "message": str(exc)}, HTTPStatus.BAD_REQUEST)
                return self.json(result)
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
                    # 13 个值：表在 2026-10-05 加了三列（码长/重打/打词率）。
                    # 当时只改了「重建」那条 INSERT，漏了这条实时收录的 ——
                    # 结果群友一发成绩就 "has 13 columns but 10 values were supplied"，
                    # 收录静默失败（榜也不更新）。
                    "INSERT INTO ai_contest_scores VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) "
                    "ON CONFLICT(record_id) DO NOTHING",
                    (
                        f"qq:{message_id}", competition_date, sender_id,
                        sender_name or sender_id, group_id, parsed["speed"],
                        parsed["keystroke"], parsed["accuracy"], occurred_at,
                        json.dumps(record, ensure_ascii=False),
                        _raw_score_field(text, "码长"),
                        _raw_retype(text),
                        _raw_score_field(text, "打词"),
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




def _clean_huma_article(raw_text):
    """从引用内容里取出正文（2026-11-17 所有者要求，逐轮修到稳）。

    群里的赛文/发文引用有多种前缀：可能有 [字数NN]，也可能只有 【难度】：名 0.24；
    括号写法也不统一（【难度】/[难度]/难度】/中间带空格）。这里一律用
    「定位 字数 / 难度 这两个关键词」+ 字符串扫描来截，不依赖具体括号样式。
    """
    text = (raw_text or "").replace("\r\n", "\n").replace("\r", "\n")

    # 1) 有「字数N」→ 截到数字后面
    idx = text.find("字数")
    if idx != -1 and idx < 300:
        j = idx + 2
        while j < len(text) and not text[j].isdigit():
            j += 1
        while j < len(text) and text[j].isdigit():
            j += 1
        while j < len(text) and (text[j].isspace() or text[j] == "]"):
            j += 1
        text = text[j:]

    # 2) 有「难度」→ 截到难度数值后面（不管括号怎么写）
    idx = text.find("难度")
    if idx != -1 and idx < 300:
        j = idx + 2
        while j < len(text) and not text[j].isdigit():
            j += 1
        while j < len(text) and (text[j].isdigit() or text[j] in ". "):
            j += 1
        text = text[j:]

    # 3) 还以【发文】之类开头的话，去掉前缀到第一个全角冒号
    text = text.strip()
    for prefix in ("【发文】", "[发文]", "【正文】", "【文】"):
        if text.startswith(prefix):
            for colon in ("：", ":"):
                pos = text.find(colon)
                if pos != -1 and pos < 20:
                    text = text[pos + 1:]
                    break
            break

    # 4) 尾部元信息
    cut = len(text)
    for marker in ("----", "sha1(", "FM赛文", "虎殇", "QQ群", "本段", "第5 5段"):
        pos = text.find(marker)
        if pos != -1:
            cut = min(cut, pos)
    probe = 0
    while True:
        probe = text.find("第", probe)
        if probe == -1:
            break
        k = probe + 1
        while k < len(text) and (text[k].isdigit() or text[k] in " \t"):
            k += 1
        if k > probe + 1 and k < len(text) and text[k] == "段":
            cut = min(cut, probe)
            break
        probe += 1
    text = text[:cut]

    return text.strip().lstrip('"“”‘’「」『』 -—')


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


def merge_contest_rows(rows):
    """同一人多个花式昵称合并成一行（2026-11-15：排行榜同一人出现多次、名字被截断）。"""
    import re as _re

    def canonical(value):
        parts = [p for p in _re.split(r"[\u300c\u300d\u300e\u300f\uff08\uff09()\[\]\u3010\u3011\s]+", value or "") if len(p) >= 2]
        latin = [p for p in parts if _re.fullmatch(r"[A-Za-z0-9_]+", p)]
        return max(latin, key=len) if latin else (max(parts, key=len) if parts else (value or ""))

    merged = {}
    for row in rows:
        name = str(row.get("user_name") or row.get("user_id") or "?")
        key = canonical(name) or name
        current = merged.get(key)
        if current is None:
            merged[key] = dict(row)
            continue
        if float(row.get("best") or 0) > float(current.get("best") or 0):
            current["best"] = row.get("best")
        current["n"] = int(current.get("n") or 0) + int(row.get("n") or 0)
        if len(name) > len(str(current.get("user_name") or "")):
            current["user_name"] = name
    return sorted(merged.values(), key=lambda item: -float(item.get("best") or 0))



def render_contest_rank_table(db: sqlite3.Connection, query: dict) -> bytes:
    """赛文排行榜「表格风格」图（官网优先；成绩库只用于**当天**兜底）。

    2026-11-15 所有者：排行榜要优先官网实时；成绩库也必须只取今天的记录，不能拿 30 天历史
    拼一张「今日榜」。群赛文（无官网）默认今天，明确说「最近 N 天」才展开。
    """
    from PIL import Image, ImageDraw, ImageFont
    from datetime import date

    raw_contest = query_value(query, "contest").strip()
    key = resolve_contest_key(raw_contest) or ""
    label, scope = CONTEST_LABELS.get(key, (raw_contest or "赛文", "global"))
    group = resolve_group_id(db, query_value(query, "group_id").strip())
    days = query_int(query, "days", 0, 0, 3660)
    limit = query_int(query, "limit", 12, 1, 60)
    today = date.today().isoformat()
    room = group if scope == "group" else ""

    official = key in ("HB", "100000", "99999", "100004") or (key == "999" and room in CONTEST_GROUP_LIVE_GROUPS)
    source_label = ""
    rows_data = []

    if official:
        def canonical_name(value):
            parts = [p for p in re.split(r"[\u300c\u300d\u300e\u300f\uff08\uff09()\[\]\u3010\u3011\s]+", value or "") if len(p) >= 2]
            latin = [p for p in parts if re.fullmatch(r"[A-Za-z0-9_]+", p)]
            return max(latin, key=len) if latin else (max(parts, key=len) if parts else (value or ""))

        archive = {}
        # 群赛文（999）没有单独的官方归档表，只走官网实时；四大赛文才有今日官网成绩库
        if scope != "group":
            for item in db.execute(
                "SELECT user_name, speed, source_json FROM competition_scores "
                "WHERE competition_date=? AND source_group=?", (today, label)
            ).fetchall():
                name = str(item["user_name"] or "?")
                try:
                    payload = json.loads(item["source_json"] or "{}")
                except (TypeError, ValueError):
                    payload = {}
                word_rate = float(payload.get("word_rate") or payload.get("word_ratio") or 0)
                archive[canonical_name(name)] = {
                    "user_name": name,
                    "best": float(item["speed"] or 0),
                    "accuracy": float(payload.get("acc") or payload.get("accuracy") or 0),
                    "key": float(payload.get("key") or payload.get("hit_rate") or 0),
                    "code_length": float(payload.get("code_length") or payload.get("kpw") or 0),
                    "word_rate": word_rate * 100 if 0 < word_rate <= 1 else word_rate,
                    "correction": float(payload.get("correction_count") or payload.get("correction") or 0),
                    "n": 1,
                }
        live = contest_live_rows(key, room)
        merged_official = {}
        for item in live:
            name = str(item.get("name") or item.get("user_name") or item.get("user_id") or "?")
            ident = canonical_name(name) or name
            base = archive.get(ident, {})
            ratio = float(item.get("word_ratio") or 0)
            merged_official[ident] = {
                "user_name": name,
                "best": float(item.get("speed") or base.get("best") or 0),
                "accuracy": float(item.get("acc") or base.get("accuracy") or 0),
                "key": float(item.get("key") or item.get("hit_rate") or base.get("key") or 0),
                "code_length": float(item.get("code") or base.get("code_length") or 0),
                "word_rate": float(item.get("word_rate") or base.get("word_rate") or 0) * (100 if ratio and ratio <= 1 else 1),
                "correction": float(item.get("back") or base.get("correction") or 0),
                "n": 1,
            }
        for ident, base in archive.items():
            merged_official.setdefault(ident, base)
        rows_data = sorted(merged_official.values(), key=lambda item: -float(item["best"] or 0))
        source_label = "官网实时" if live else ("官网成绩（今日）" if rows_data else "官网（今日暂无）")

    else:
        # 群赛文 / 555：默认今天；用户明确说最近 N 天才按区间
        sql = ("SELECT user_id, user_name, speed, accuracy, code_length, word_rate, raw_text "
               "FROM contest_scores WHERE contest_key=? AND group_id=?")
        params = [key, room]
        if days > 0:
            from datetime import timedelta
            since = (date.today() - timedelta(days=days - 1)).isoformat()
            sql += " AND score_date >= ?"
            params.append(since)
        else:
            sql += " AND score_date = ?"
            params.append(today)
        sql += " ORDER BY speed DESC"
        records = db.execute(sql, tuple(params)).fetchall()

        def canonical(value):
            parts = [p for p in re.split(r"[\u300c\u300d\u300e\u300f\uff08\uff09()\[\]\u3010\u3011\s]+", value or "") if len(p) >= 2]
            latin = [p for p in parts if re.fullmatch(r"[A-Za-z0-9_]+", p)]
            return max(latin, key=len) if latin else (max(parts, key=len) if parts else (value or ""))

        merged = {}
        for row in records:
            name = str(row["user_name"] or row["user_id"] or "?")
            ident = canonical(name) or str(row["user_id"] or name)
            entry = merged.get(ident)
            if entry is None:
                entry = {"user_name": name, "best": 0.0, "accuracy": 0.0, "code_length": 0.0,
                         "word_rate": 0.0, "key": 0.0, "correction": 0.0, "n": 0}
                merged[ident] = entry
            speed = float(row["speed"] or 0)
            entry["n"] += 1
            if speed >= float(entry["best"] or 0):
                entry["best"] = speed
                if len(name) >= len(str(entry["user_name"] or "")):
                    entry["user_name"] = name
                entry["accuracy"] = float(row["accuracy"] or 0)
                entry["code_length"] = float(row["code_length"] or 0)
                word_rate = float(row["word_rate"] or 0)
                entry["word_rate"] = word_rate * 100 if word_rate and word_rate <= 1 else word_rate
                try:
                    payload = json.loads(row["raw_text"] or "{}")
                except (TypeError, ValueError):
                    payload = {}
                entry["key"] = float(payload.get("key") or payload.get("hit_rate") or 0)
                entry["correction"] = float(payload.get("correction") or payload.get("correction_count") or 0)
        rows_data = sorted(merged.values(), key=lambda item: -float(item["best"] or 0))
        source_label = "成绩库（今日）" if days <= 0 else f"成绩库（最近 {days} 天）"

    rows = rows_data[:limit]
    if key == "555":
        # 2026-11-17：555 的成绩存在 ai_contest_scores（不是 contest_scores），
        # 之前 555 榜读错了表，所以群里新打的 555 成绩不上榜。
        rows = []
        seen555 = set()
        for row in db.execute(
            "SELECT user_id,user_name,speed,code_length,retype,word_rate,source_json "
            "FROM ai_contest_scores WHERE competition_date=? ORDER BY speed DESC",
            (today,),
        ).fetchall():
            # 每人当天只保留最快一条（2026-11-17：群友打多次时榜上出现了重复行）
            uid555 = str(row["user_id"] or "")
            if uid555:
                if uid555 in seen555:
                    continue
                seen555.add(uid555)
            try:
                payload555 = json.loads(row["source_json"] or "{}")
            except (TypeError, ValueError):
                payload555 = {}
            word_rate555 = float(row["word_rate"] or 0)
            rows.append({
                "user_name": str(row["user_name"] or row["user_id"] or "?"),
                "best": float(row["speed"] or 0),
                "key": float(payload555.get("keystroke") or payload555.get("key") or 0),
                "accuracy": float(payload555.get("acc") or payload555.get("accuracy") or 0),
                "code_length": float(row["code_length"] or 0),
                "word_rate": word_rate555 * 100 if 0 < word_rate555 <= 1 else word_rate555,
                "correction": float(row["retype"] or 0),
                "n": 1,
            })
        rows = sorted(rows, key=lambda item: -float(item["best"] or 0))[:limit]
        source_label = "成绩库（今日）"
    if official:
        rows.sort(key=lambda item: -float(item["best"] or 0))

    no_retype_contests = {"HB", "100000"}
    show_retype = key not in no_retype_contests and any(float(r.get("correction") or 0) > 0 for r in rows)

    GOLD, SILVER, BRONZE = (255, 216, 77), (226, 236, 248), (201, 123, 62)
    ROW_TINTS = [(54, 46, 22), (39, 45, 56), (50, 38, 28)]
    DIMMED = (128, 141, 160)
    CYAN = (91, 219, 194)
    RED = (246, 116, 130)

    def speed_color(value):
        if value < 100:
            return (145, 164, 190)
        if value < 130:
            return (100, 181, 246)
        if value < 150:
            return CYAN
        if value < 180:
            return GOLD
        if value < 200:
            return (255, 154, 102)
        return (239, 116, 166)

    def accuracy_color(value):
        if value <= 0:
            return DIMMED
        if value < 90:
            return RED
        if value < 95:
            return (255, 154, 102)
        if value < 98:
            return CYAN
        return (126, 224, 156)

    def correction_color(value):
        if value <= 0:
            return DIMMED
        if value <= 5:
            return (126, 224, 156)
        if value <= 15:
            return (113, 164, 255)
        if value <= 25:
            return (255, 154, 102)
        return RED

    def word_rate_color(value):
        if value <= 0:
            return DIMMED
        if value >= 55:
            return (126, 224, 156)
        if value >= 45:
            return CYAN
        return DIMMED

    def key_color(value):
        if value <= 0:
            return DIMMED
        if value >= 9:
            return (255, 154, 102)
        if value >= 7:
            return (100, 181, 246)
        return DIMMED

    font_path = os.environ.get("FM_REPORT_FONT", "/assets/msyh.ttc")
    try:
        font = lambda size: ImageFont.truetype(font_path, size)
    except Exception:
        font = lambda size: ImageFont.load_default()

    group_label = group_display_name(db, group) if scope == "group" and group else ""
    where = ""
    if scope == "group" and group_label:
        where = f"（{group_label}）"
    elif scope != "group":
        where = "（全群）"
    when = "今日" if days <= 0 or official else f"最近 {days} 天"
    # 2026-11-15 所有者：群赛文标题用**群名**（FM 梦幻打字阁赛文排行榜）。
    if key == "555":
        title = "FM AI赛文排行榜"
    elif scope == "group" and group_label:
        title = f"FM {group_label}赛文排行榜"
    else:
        title = f"FM {label}排行榜"
    where_text = ""
    if scope == "group" and group_label:
        where_text = f"  |  {group_label}"
    elif scope != "group":
        where_text = f"  |  {label}" if key == "555" else "  |  全群"
    title_prefix = "本群" if scope == "group" else label
    subtitle = f"{today}{where_text}  |  {when}  |  {source_label}"
    if key == "555":
        # 2026-11-17 所有者：555 榜要标出今天赛文的名称
        contest_title = ""
        try:
            text_row = db.execute(
                "SELECT title FROM ai_contest_texts WHERE competition_date=?", (today,)
            ).fetchone()
            contest_title = str(text_row["title"] if text_row else "").strip()
        except Exception:
            contest_title = ""
        if not contest_title:
            try:
                for score_row in db.execute(
                    "SELECT source_json FROM ai_contest_scores WHERE competition_date=? "
                    "ORDER BY speed DESC LIMIT 5",
                    (today,),
                ).fetchall():
                    payload = json.loads(score_row["source_json"] or "{}")
                    raw = str(payload.get("raw") or "")
                    match = re.search(r"【([^】]{2,80})】", raw)
                    if match and match.group(1).strip() not in {"首打认证", "词提开"}:
                        contest_title = match.group(1).strip()
                        break
            except Exception:
                pass
        if contest_title:
            subtitle = f"{today}  |  《{contest_title}》  |  第555段  |  {when}  |  {source_label}"

    width = 1240
    row_height = 78
    height = 238 + max(1, len(rows)) * row_height + 96
    image = Image.new("RGB", (width, height), (20, 24, 31))
    draw = ImageDraw.Draw(image)
    draw.rectangle((0, 0, width, 12), fill=(78, 205, 181))
    draw.text((48, 34), title, font=font(44), fill=(248, 250, 253))
    draw.text((48, 98), subtitle, font=font(24), fill=(198, 209, 224))

    if show_retype:
        columns = [(52, "排名"), (120, "成员"), (500, "速度"), (620, "击键"),
                   (730, "码长"), (830, "打词率"), (960, "重打"), (1050, "键准")]
        col_acc = 1050
    else:
        columns = [(52, "排名"), (120, "成员"), (500, "速度"), (620, "击键"),
                   (730, "码长"), (830, "打词率"), (960, "键准")]
        col_acc = 960
    draw.rounded_rectangle((40, 154, 1200, 210), radius=8, fill=(35, 43, 55))
    for x, name in columns:
        draw.text((x, 166), name, font=font(22), fill=(200, 210, 226))
    if not rows:
        text = "今天官网还没有这个赛文的成绩。" if official else "今天还没有这个赛文的成绩。"
        draw.text((52, 240), text, font=font(24), fill=(200, 120, 120))
    for index, item in enumerate(rows, 1):
        top = 224 + (index - 1) * row_height
        fill = ROW_TINTS[index - 1] if index <= 3 else ((29, 36, 47) if index % 2 else (25, 31, 41))
        draw.rounded_rectangle((40, top, 1200, top + 68), radius=7, fill=fill)
        if index == 1:
            draw.rounded_rectangle((40, top, 50, top + 68), radius=3, fill=GOLD)
        elif index == 2:
            draw.rounded_rectangle((40, top, 50, top + 68), radius=3, fill=SILVER)
        elif index == 3:
            draw.rounded_rectangle((40, top, 50, top + 68), radius=3, fill=BRONZE)
        rank_color = [GOLD, SILVER, BRONZE][index - 1] if index <= 3 else (219, 226, 237)
        raw_name = str(item.get("user_name") or "unknown")
        name = raw_name
        while name and draw.textlength(name, font=font(21)) > 330:
            name = name[:-1]
        if name != raw_name:
            name += "…"
        speed = float(item.get("best") or 0)
        draw.text((58, top + 17), str(index), font=font(24), fill=rank_color)
        draw.text((120, top + 17), name, font=font(23), fill=(244, 247, 252))
        draw.text((500, top + 17), f"{speed:.2f}", font=font(23), fill=speed_color(speed))
        key_val = float(item.get("key") or 0)
        draw.text((620, top + 17), f"{key_val:.2f}" if key_val > 0 else "—", font=font(23), fill=key_color(key_val))
        code_len = float(item.get("code_length") or 0)
        draw.text((730, top + 17), f"{code_len:.2f}" if code_len > 0 else "—", font=font(23), fill=(186, 197, 214))
        wr = float(item.get("word_rate") or 0)
        draw.text((830, top + 17), f"{wr:.2f}%" if wr > 0 else "—", font=font(23), fill=word_rate_color(wr))
        if show_retype:
            corr = float(item.get("correction") or 0)
            draw.text((960, top + 17), f"{corr:.0f}" if corr > 0 else "—", font=font(23), fill=correction_color(corr))
        acc = float(item.get("accuracy") or 0)
        draw.text((col_acc, top + 17), f"{acc:.2f}%" if acc > 0 else "—", font=font(23), fill=accuracy_color(acc))
    footer = f"共 {len(rows)} 人 | 每人保留当日最高速度" if official else f"共 {len(rows)} 人 | 每人保留最高速度（成绩条数 {sum(int(r.get('n') or 0) for r in rows)}）"
    draw.text((48, height - 62), footer, font=font(19), fill=(150, 163, 182))
    legend = f"数据来源：{source_label} · 颜色：速度分档 · 键准 ≥98 绿、<90 红"
    if not show_retype:
        legend += " · 本赛文不能重打，未列重打" if key in no_retype_contests else " · 今日暂无重打数据，未列"
    draw.text((48, height - 36), legend, font=font(16), fill=(124, 136, 154))
    output = io.BytesIO()
    image.save(output, "PNG", optimize=True)
    return output.getvalue()


def _official_rows_for(db, key, label, room, limit):
    """官方赛排行榜：官网实时优先，今日官网成绩库兜底。"""
    from datetime import date
    today = date.today().isoformat()

    def canon(value):
        parts = [p for p in re.split(r"[\u300c\u300d\u300e\u300f\uff08\uff09()\[\]\u3010\u3011\s]+", value or "") if len(p) >= 2]
        latin = [p for p in parts if re.fullmatch(r"[A-Za-z0-9_]+", p)]
        return max(latin, key=len) if latin else (max(parts, key=len) if parts else (value or ""))

    archive = {}
    for item in db.execute(
        "SELECT user_name, speed, source_json FROM competition_scores WHERE competition_date=? AND source_group=?",
        (today, label),
    ).fetchall():
        name = str(item["user_name"] or "?")
        try:
            payload = json.loads(item["source_json"] or "{}")
        except (TypeError, ValueError):
            payload = {}
        archive[canon(name)] = {"user_name": name, "best": float(item["speed"] or 0),
                                "accuracy": float(payload.get("acc") or payload.get("accuracy") or 0)}
    live = contest_live_rows(key, room)
    merged = {}
    for item in live:
        name = str(item.get("name") or "?")
        ident = canon(name) or name
        base = archive.get(ident, {})
        merged[ident] = {"user_name": name,
                         "best": float(item.get("speed") or base.get("best") or 0),
                         "accuracy": float(item.get("acc") or base.get("accuracy") or 0)}
    for ident, base in archive.items():
        merged.setdefault(ident, base)
    return sorted(merged.values(), key=lambda i: -float(i["best"] or 0))[:limit]


def render_all_ranks(db, group_id: str, limit: int = 5) -> bytes:
    """「全部排行榜」：各官方赛文统一表格风格，叠成一张图。"""
    from PIL import Image, ImageDraw, ImageFont
    from datetime import date

    sections = [("100000", "锦标赛"), ("HB", "虎杯"), ("99999", "极速杯"), ("100004", "极速联赛")]
    font_path = os.environ.get("FM_REPORT_FONT", "/assets/msyh.ttc")
    try:
        font = lambda size: ImageFont.truetype(font_path, size)
    except Exception:
        font = lambda size: ImageFont.load_default()

    data = [(label, _official_rows_for(db, key, label, "", limit)) for key, label in sections]
    row_h = 46
    header = 150
    per_section = 44 + max(1, limit) * row_h + 18
    height = header + len(data) * per_section + 60
    width = 1240
    image = Image.new("RGB", (width, height), (20, 24, 31))
    draw = ImageDraw.Draw(image)
    draw.rectangle((0, 0, width, 12), fill=(78, 205, 181))
    draw.text((48, 38), "FM 赛文排行榜 · 全部", font=font(40), fill=(244, 247, 252))
    draw.text((48, 98), f"{date.today().isoformat()}  |  锦标赛 / 虎杯 / 极速杯 / 极速联赛  |  今日  |  官网优先",
              font=font(22), fill=(159, 172, 193))

    GOLD, SILVER, BRONZE = (255, 207, 92), (196, 204, 216), (214, 150, 96)
    DIMMED = (128, 141, 160)

    def speed_color(value):
        if value < 100:
            return (145, 164, 190)
        if value < 130:
            return (100, 181, 246)
        if value < 150:
            return (91, 219, 194)
        if value < 180:
            return GOLD
        if value < 200:
            return (255, 154, 102)
        return (239, 116, 166)

    def accuracy_color(value):
        if value <= 0:
            return DIMMED
        if value < 90:
            return (246, 116, 130)
        if value < 95:
            return (255, 154, 102)
        if value < 98:
            return (91, 219, 194)
        return (126, 224, 156)

    y = header
    for label, rows in data:
        draw.text((48, y), f"FM {label}排行榜", font=font(30), fill=(235, 239, 245))
        draw.text((300, y + 8), "（今日 · 官网优先）", font=font(18), fill=DIMMED)
        y += 44
        draw.rounded_rectangle((40, y, 1200, y + 40), radius=6, fill=(35, 43, 55))
        for x, name in [(52, "排名"), (120, "成员"), (620, "速度"), (860, "键准")]:
            draw.text((x, y + 8), name, font=font(18), fill=(184, 196, 214))
        y += 46
        if not rows:
            draw.text((60, y + 4), "今天官网还没有这个赛文的成绩。", font=font(20), fill=(200, 120, 120))
            y += 40
        for index, item in enumerate(rows, 1):
            fill = (29, 36, 47) if index % 2 else (25, 31, 41)
            draw.rounded_rectangle((40, y, 1200, y + row_h - 8), radius=6, fill=fill)
            rank_color = [GOLD, SILVER, BRONZE][index - 1] if index <= 3 else (219, 226, 237)
            raw_name = str(item.get("user_name") or "?")
            name = raw_name
            while name and draw.textlength(name, font=font(20)) > 460:
                name = name[:-1]
            if name != raw_name:
                name += "…"
            speed = float(item["best"] or 0)
            acc = float(item.get("accuracy") or 0)
            draw.text((58, y + 6), str(index), font=font(20), fill=rank_color)
            draw.text((120, y + 6), name, font=font(20), fill=(235, 239, 245))
            draw.text((620, y + 6), f"{speed:.2f}", font=font(20), fill=speed_color(speed))
            draw.text((860, y + 6), f"{acc:.2f}%" if acc > 0 else "—", font=font(20), fill=accuracy_color(acc))
            y += row_h
        y += 10

    draw.text((48, height - 36), "颜色：速度分档（<130 蓝 / <150 青 / <180 金 / <200 橙 / ≥200 粉）· 键准 ≥98 绿、<90 红",
              font=font(15), fill=(96, 106, 122))
    output = io.BytesIO()
    image.save(output, "PNG", optimize=True)
    return output.getvalue()




def contests_today_payload(db, group_id: str) -> dict:
    """今日有成绩的全部赛文清单：官网四赛 + 555 + 每个可查群的群赛文（空的不列，不编造）。"""
    from datetime import date
    today = date.today().isoformat()
    group = (group_id or "").strip()
    official = {"HB", "100000", "99999", "100004"}
    items = []
    for key, (label, scope) in CONTEST_LABELS.items():
        if key == "999":
            continue  # 群赛文按群单独列
        room = group if scope == "group" else ""
        count = 0
        try:
            if key in official:
                row = db.execute(
                    "SELECT COUNT(*) n FROM competition_scores WHERE competition_date=? AND source_group=?",
                    (today, label),
                ).fetchone()
                count = int(row["n"] if row and row["n"] else 0)
                if count == 0:
                    count = len(contest_live_rows(key, room))
            else:
                row = db.execute(
                    "SELECT COUNT(*) n FROM contest_scores WHERE contest_key=? AND group_id=? AND score_date=?",
                    (key, room, today),
                ).fetchone()
                count = int(row["n"] if row and row["n"] else 0)
        except Exception:
            count = 0
        if count > 0:
            items.append({"contest": key, "label": label, "scope": scope, "group_id": room, "count": count})

    # 群赛文：所有官网可查的群各列一行（有今日成绩才列）
    groups = [group] if group else list(CONTEST_GROUP_LIVE_GROUPS)
    for gid in groups:
        try:
            row = db.execute(
                "SELECT COUNT(*) n FROM contest_scores WHERE contest_key='999' AND group_id=? AND score_date=?",
                (gid, today),
            ).fetchone()
            count = int(row["n"] if row and row["n"] else 0)
            if count == 0:
                count = len(contest_live_rows("999", gid))
        except Exception:
            count = 0
        if count > 0:
            name = group_display_name(db, gid) or gid
            items.append({"contest": "999", "label": f"{name}赛文", "scope": "group",
                          "group_id": gid, "count": count, "group_name": name})

    items.sort(key=lambda item: -int(item["count"]))
    return {"ok": True, "date": today, "count": len(items), "items": items}




# ── /contests/today 短期缓存（2026-11-15）──────────────────────────────────
# 这个端点会去官网抓实时数据，冷启动 3 秒以上；bot 的若干路由在 decide 阶段会探测它，
# 于是每条消息都被拖慢。数据本身 30 秒内不会变，直接做 TTL 缓存。
_CONTESTS_TODAY_CACHE = {}
_CONTESTS_TODAY_TTL_SECONDS = 30


def cached_contests_today_payload(db, room):
    import time as _time
    key = room or ""
    now = _time.time()
    cached = _CONTESTS_TODAY_CACHE.get(key)
    if cached and now - cached[0] < _CONTESTS_TODAY_TTL_SECONDS:
        return cached[1]
    payload = contests_today_payload(db, room)
    _CONTESTS_TODAY_CACHE[key] = (now, payload)
    return payload

if __name__ == "__main__":
    main()

