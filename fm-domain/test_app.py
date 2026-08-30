import json
import io
import importlib.util
import os
import tempfile
import threading
import time
import unittest
import urllib.request
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest.mock import patch

from app import (
    Api,
    ThreadingHTTPServer,
    bot_guard_check,
    connect,
    import_retained_snapshot,
    acknowledge_library_message,
    continue_library_session,
    continue_library_session_from_score,
    continue_previous_library_session,
    continue_same_library_session,
    clean_library_content,
    library_content_quality,
    classify_library,
    classify_library_text,
    resolve_library_genre,
    get_library_ranker,
    rank_library_row,
    recent_library_messages,
    library_stats,
    library_session_status,
    archive_status,
    competition_score_summary,
    competition_score_history,
    publish_ai_contest_text,
    render_competition_score,
    render_score_chart,
    render_ai_leaderboard,
    fetch_public_group_competition,
    fetch_public_js_competition,
    fetch_public_tiger_competition,
    public_competition_response,
    render_public_competition_rank,
    resolve_public_competition,
    repeat_check,
    group_capability_enabled,
    set_group_capability,
    start_library_session,
    start_single_session,
    stop_library_session,
    mark_library_messages_recalled,
    update_bot_guard_account,
    write_setting,
)


class RetainedSnapshotImportTest(unittest.TestCase):
    def test_library_difficulty_modes_are_strict_and_persist_after_score(self):
        class FixedRanker:
            scores = {
                "淼": 0.05,
                "水": 0.20,
                "易": 0.50,
                "普": 1.46,
                "难": 8.00,
                "虐": 18.00,
            }

            def rank(self, text):
                marker = text[0]
                score = self.scores[marker]
                return score, marker

        with tempfile.TemporaryDirectory() as root, patch(
            "app.get_library_ranker", return_value=FixedRanker()
        ):
            db = connect(str(Path(root) / "fm.sqlite3"))
            try:
                for difficulty in FixedRanker.scores:
                    for copy_no in (1, 2):
                        title = f"{difficulty}档测试文{copy_no}"
                        content = difficulty + (f"这是{difficulty}档测试正文，" * 30)
                        db.execute(
                            "INSERT INTO library_texts VALUES (?, ?, ?, ?, ?, ?)",
                            (
                                f"{difficulty}-{copy_no}",
                                "fm_texts",
                                f"fm_texts/{difficulty}-{copy_no}.txt",
                                title,
                                content,
                                len(content),
                            ),
                        )
                db.commit()

                for index, difficulty in enumerate(FixedRanker.scores, 1):
                    identity = {
                        "platform": "qq",
                        "chat_id": "difficulty-test",
                        "requester_id": str(index),
                        "requester_name": f"tester-{difficulty}",
                    }
                    started = start_library_session(
                        db, {**identity, "difficulty": difficulty, "length": 80}
                    )
                    self.assertEqual(started["status"], "segment")
                    self.assertEqual(started["difficulty"], difficulty)
                    self.assertTrue(started["title"].startswith(f"{difficulty}档测试文"))

                    continued = continue_library_session_from_score(
                        db,
                        {
                            **identity,
                            "text": (
                                f"第{started['segment_id']}段 速度140.01 "
                                "击键6.61 键准91.69% 字数80"
                            ),
                        },
                    )
                    self.assertEqual(continued["status"], "segment")
                    self.assertEqual(continued["difficulty"], difficulty)
                    self.assertNotEqual(continued["title"], started["title"])
                    self.assertEqual(continued["next_offset"], 80)

                unavailable = start_library_session(
                    db,
                    {
                        "platform": "qq",
                        "chat_id": "difficulty-test",
                        "requester_id": "missing",
                        "requester_name": "missing",
                        "query": "淼档测试文1",
                        "difficulty": "虐",
                        "length": 80,
                    },
                )
                self.assertEqual(unavailable["status"], "not_found")
                self.assertIn("“虐”难度区间", unavailable["message"])
            finally:
                db.close()

    def test_random_article_mode_rerolls_length_after_score(self):
        with tempfile.TemporaryDirectory() as root, patch(
            "app.random.randint", side_effect=[207, 111111, 333, 222222]
        ):
            db = connect(str(Path(root) / "fm.sqlite3"))
            try:
                for index in (1, 2):
                    content = f"第{index}篇随机长度测试正文，" * 100
                    db.execute(
                        "INSERT INTO library_texts VALUES (?, ?, ?, ?, ?, ?)",
                        (
                            f"random-{index}", "fm_texts", f"fm_texts/random-{index}.txt",
                            f"随机测试{index}", content, len(content),
                        ),
                    )
                db.commit()
                identity = {
                    "platform": "qq", "chat_id": "random-length", "requester_id": "tester",
                    "requester_name": "tester",
                }
                started = start_library_session(db, identity)
                self.assertEqual(started["next_offset"], 207)
                continued = continue_library_session_from_score(db, {
                    **identity,
                    "text": f"第{started['segment_id']}段 速度140 击键6 键准98% 字数207",
                })
                self.assertEqual(continued["status"], "segment")
                self.assertEqual(continued["next_offset"], 333)
                self.assertNotEqual(continued["title"], started["title"])
            finally:
                db.close()

    def test_library_cleaner_removes_scrape_prefixes_and_separators(self):
        title = "丑奴儿·书博山中壁"
        raw = (
            "丑奴儿·书博山中壁========================================\n"
            "丑奴儿·书博山中壁2020-11-20 15:23来源：散文网\n宋代\n辛弃疾\n"
            "少年不识愁滋味，爱上层楼。爱上层楼，为赋新词强说愁。\n"
            "而今识尽愁滋味，欲说还休。欲说还休，却道天凉好个秋。\n"
            "本文由散文网用户整理发布，版权归原作者所有。"
        )
        cleaned, reasons = clean_library_content(title, raw)
        self.assertNotIn(title, cleaned)
        self.assertNotIn("2020-11-20", cleaned)
        self.assertNotIn("来源", cleaned)
        self.assertNotIn("宋代", cleaned)
        self.assertNotIn("辛弃疾", cleaned)
        self.assertNotIn("版权", cleaned)
        self.assertNotIn("====", cleaned)
        self.assertTrue(cleaned.startswith("少年不识愁滋味"))
        self.assertIn("repeated_title", reasons)
        self.assertEqual(library_content_quality(title, cleaned), (True, "literary_short_text"))
        variant = (
            "。《 这个冬天 。迷上了温度。o(∩_∩)o…。暖风~~》 。\n"
            "2019-01-08 15:15 来源\n散文网\n2008-11-24 17:09（分类：默认分类）\n"
            + "这一段才是应该保留的正常正文。" * 8
        )
        variant_cleaned, _ = clean_library_content(
            "。《这个冬天。迷上了温度。o(∩_∩)o…。暖风~~》。", variant
        )
        self.assertNotIn("这个冬天", variant_cleaned)
        self.assertNotIn("2019-01-08", variant_cleaned)
        self.assertNotIn("散文网", variant_cleaned)
        self.assertNotIn("默认分类", variant_cleaned)
        self.assertTrue(variant_cleaned.startswith("这一段才是应该保留的正常正文"))

    def test_library_cleaner_rejects_tiny_punctuation_free_and_anthology_junk(self):
        tiny, _ = clean_library_content("网页摘录", "来源：散文网\n只有半句话")
        self.assertEqual(library_content_quality("网页摘录", tiny), (False, "too_short"))
        no_punctuation = "\n".join(["这是从网页抓取下来但完全没有任何标点的现代文字"] * 12)
        self.assertEqual(
            library_content_quality("伤心过后", no_punctuation),
            (False, "punctuation_free_non_literary"),
        )
        anthology = "1《蜀相》正文。 2《己亥杂诗》正文。 3《闻王昌龄左迁》正文。"
        self.assertEqual(
            library_content_quality("必背名篇100", anthology),
            (False, "anthology_bundle"),
        )
        self.assertEqual(
            library_content_quality("培训心得体会精选6篇", "这是一篇很长的正常正文。" * 20),
            (False, "anthology_bundle"),
        )
        multi_document = (
            "暑假医院见习心得体会篇1\n" + "第一篇正文内容。" * 20
            + "\n暑假医院见习心得体会篇2\n" + "第二篇正文内容。" * 20
        )
        self.assertEqual(
            library_content_quality("暑假医院见习心得体会", multi_document),
            (False, "anthology_bundle"),
        )
        signatures = "\n".join(f"第{i}条互不相关的短句。" for i in range(1, 25))
        self.assertEqual(
            library_content_quality("经典人生签名", signatures),
            (False, "anthology_bundle"),
        )

    def test_library_cleaner_preserves_short_poetry(self):
        poem = "床前明月光\n疑是地上霜\n举头望明月\n低头思故乡"
        cleaned, _ = clean_library_content("静夜思", poem)
        self.assertEqual(cleaned, poem)
        self.assertEqual(library_content_quality("静夜思", cleaned), (True, "literary_short_text"))
        punctuated_poem = "单车欲问边，属国过居延。征蓬出汉塞，归雁入胡天。大漠孤烟直，长河落日圆。"
        self.assertEqual(
            library_content_quality("使至塞上", punctuated_poem),
            (True, "literary_short_text"),
        )

    def test_library_cleaner_preserves_novel_and_chinese_punctuation(self):
        novel = (
            "这是一本完整小说的正文，标点必须保持原样。" * 20
            + " 1《偶然出现的书名》。 2《另一本书》。 3《第三本书》。"
        )
        cleaned, _ = clean_library_content("《百年孤独》", novel)
        self.assertIn("，", cleaned)
        self.assertNotIn(",", cleaned)
        self.assertEqual(library_content_quality("《百年孤独》", cleaned), (True, "ok"))

    def test_library_cleaner_removes_trailing_source_credit_only(self):
        raw = "正文说明这个消息的来源很可靠。这一句仍然属于正文。\n（文章来源：简书）"
        cleaned, reasons = clean_library_content("正常文章", raw)
        self.assertEqual(cleaned, "正文说明这个消息的来源很可靠。这一句仍然属于正文。")
        self.assertIn("source_metadata", reasons)
        long_credit = "正文到此结束。\n文章来源：江罗（ID：LF1992JL），原文有删节 | 作者：江罗，悦读专栏作者，一个较真的理工男"
        self.assertEqual(clean_library_content("文章", long_credit)[0], "正文到此结束。")
        inline_marker = "前半段正文。\n（ 文章阅读网： ）\n后半段正文。"
        self.assertEqual(clean_library_content("文章", inline_marker)[0], "前半段正文。\n后半段正文。")

    def test_public_competition_group_rank_text_and_image(self):
        target = resolve_public_competition("梦幻打字阁")
        rank_response = {
            "status": 1,
            "data": {"totalCount": 1, "list": [{
                "ranking": 1, "username": "tester", "speed": "123.45",
                "keystrokes": "6.70", "ma_chang": "2.50", "hui_gai": "3",
                "wrong_number": "1", "jian_zhun": "98.20%", "input_method": "五笔",
            }]},
        }
        text_response = {
            "status": 1,
            "data": {"content": "今日赛文\n这是一段测试正文。\n-----第100001段-共10字", "word_number": 10},
        }
        with patch("app.public_http_json", side_effect=[rank_response, text_response]):
            result = fetch_public_group_competition(target, "2026-08-22")
        result["row_count"] = len(result["rows"])
        result["page_count"] = 1
        self.assertEqual(result["rows"][0]["speed"], 123.45)
        self.assertTrue(result["content"].startswith("梦幻打字阁日赛｜今日赛文"))
        self.assertIn("第mhdzg段", result["content"])
        response = public_competition_response(result)
        self.assertEqual(response["status"], "ok")
        if importlib.util.find_spec("PIL"):
            os.environ["FM_REPORT_FONT"] = str(Path(__file__).parent.parent / "msyh.ttc")
            image = render_public_competition_rank(result)
            self.assertTrue(image.startswith(b"\x89PNG\r\n\x1a\n"))

    def test_public_competition_js_html_parser(self):
        target = resolve_public_competition("极速杯")
        cells = ["x", "tester", "1", "x", "x", "x", "fallback", "x", "188.8", "8.2", "2.6", "x", "2", "99.1%", "x", "五笔"]
        table = "<table id='sdph'><tr>" + "".join(f"<td>{cell}</td>" for cell in cells) + "</tr></table>"
        document = (
            "赛文标题：<span id='title'>测试赛文</span>赛文总字数： 300"
            "<p id='content'>这里是公开赛文正文。</p>" + table
        )
        with patch("app.public_http_text", return_value=document):
            result = fetch_public_js_competition(target, "2026-08-22")
        self.assertEqual(result["title"], "测试赛文")
        self.assertEqual(result["word_number"], 300)
        self.assertEqual(result["rows"][0]["rank"], 1)
        self.assertEqual(result["content"], "这里是公开赛文正文。")

    def test_public_tiger_competition_api_parser(self):
        target = resolve_public_competition("虎杯")
        response = {
            "success": True,
            "data": {"date": "2026-08-29", "leaderboard": [{
                "rank": 1, "username": "Fixmood", "speed": 148.93,
                "hit_rate": 7.07, "kpw": 2.85, "accuracy": 92.35,
                "correction_count": 20, "input_method": "慢打",
            }]},
        }
        with patch("app.public_http_get_json", return_value=response):
            result = fetch_public_tiger_competition(target, "2026-08-29")
        self.assertEqual(result["source"], "虎杯")
        self.assertEqual(result["rows"][0]["name"], "Fixmood")
        self.assertEqual(result["rows"][0]["speed"], 148.93)
        self.assertEqual(result["rows"][0]["acc"], 92.35)

    def test_public_competition_rank_combined_renders_all_rows(self):
        if not importlib.util.find_spec("PIL"):
            self.skipTest("Pillow is not installed")
        from PIL import Image

        result = {
            "source": "虎杯",
            "date": "2026-08-29",
            "title": "虎杯排行榜",
            "rows": [
                {"rank": index, "name": f"user-{index}", "speed": 100 + index,
                 "key": 6, "code": 2, "back": 0, "wrong": 0, "acc": 98, "ime": "虎码"}
                for index in range(1, 36)
            ],
            "row_count": 35,
            "page_count": 2,
        }
        os.environ["FM_REPORT_FONT"] = str(Path(__file__).parent.parent / "msyh.ttc")
        image_data = render_public_competition_rank(result, combined=True)
        with Image.open(io.BytesIO(image_data)) as image:
            self.assertEqual(image.size, (1200, 294 + 35 * 66))

    def test_ai_publish_score_summary_and_bot_accounts(self):
        with tempfile.TemporaryDirectory() as root:
            db = connect(str(Path(root) / "fm.sqlite3"))
            try:
                published = publish_ai_contest_text(db, {
                    "date": "2026-08-22", "title": "今日测试",
                    "body": "测试赛文正文" * 36, "difficulty": "普",
                })
                self.assertEqual(published["status"], "published")
                existing = publish_ai_contest_text(db, {
                    "date": "2026-08-22", "body": "不会覆盖" * 100,
                })
                self.assertEqual(existing["status"], "existing")

                record = {
                    "competition_date": "2026-08-22", "user_id": "10001",
                    "user_name": "tester", "source_group": "sample", "speed": 120.5,
                    "key": 6.1, "acc": 98.0,
                }
                db.execute(
                    "INSERT INTO competition_scores VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
                    ("score-1", "2026-08-22", "10001", "tester", "20001", "sample", 120.5, 1, json.dumps(record)),
                )
                db.commit()
                summary = competition_score_summary(db, user_id="10001")
                self.assertEqual(summary["count"], 1)
                self.assertEqual(summary["best_speed"], 120.5)

                added = update_bot_guard_account(db, {
                    "action": "add", "account_id": "90001", "label": "test bot",
                })
                self.assertIn("90001", added["accounts"])
                removed = update_bot_guard_account(db, {"action": "remove", "account_id": "90001"})
                self.assertNotIn("90001", removed["accounts"])
                if importlib.util.find_spec("PIL"):
                    os.environ["FM_REPORT_FONT"] = str(Path(__file__).parent.parent / "msyh.ttc")
                    report = render_competition_score(db, "10001", "", "")
                    self.assertTrue(report.startswith(b"\x89PNG\r\n\x1a\n"))
            finally:
                db.close()

    def test_library_session_start_continue_stop_and_stats(self):
        with tempfile.TemporaryDirectory() as root:
            db = connect(str(Path(root) / "fm.sqlite3"))
            try:
                content = "春风吹过小路，树叶轻轻摇晃。大家沿着河边慢慢散步。" * 12
                db.execute(
                    "INSERT INTO library_texts VALUES (?, ?, ?, ?, ?, ?)",
                    ("text-1", "fm_texts", "fm_texts/test.txt", "测试文章", content, len(content)),
                )
                content_2 = "冬雨落在窗前，远处灯火安静闪烁。人们收起雨伞走进车站。" * 12
                db.execute(
                    "INSERT INTO library_texts VALUES (?, ?, ?, ?, ?, ?)",
                    ("text-2", "fm_texts", "fm_texts/test-2.txt", "第二篇文章", content_2, len(content_2)),
                )
                short_content = "这是一篇很短的测试文章。"
                db.execute(
                    "INSERT INTO library_texts VALUES (?, ?, ?, ?, ?, ?)",
                    ("text-3", "fm_texts", "fm_texts/short.txt", "短文测试", short_content, len(short_content)),
                )
                db.execute(
                    "INSERT INTO library_rankings VALUES (?, ?, ?, ?)",
                    ("text-1", "难", 1.46, time.time()),
                )
                db.commit()
                cached_score, cached_difficulty = rank_library_row(
                    db, db.execute("SELECT * FROM library_texts WHERE text_id='text-1'").fetchone()
                )
                self.assertEqual((cached_score, cached_difficulty), (1.46, "普"))
                identity = {
                    "platform": "qq", "chat_id": "20001", "requester_id": "10001",
                    "requester_name": "tester",
                }
                started = start_library_session(db, {**identity, "query": "春风吹过小路", "length": 80})
                self.assertEqual(started["status"], "segment")
                self.assertRegex(
                    started["message"],
                    rf"-----第\d{{6}}段-FM发文｜进度80/{len(content)}字$",
                )
                self.assertNotIn("@tester", started["message"])
                self.assertEqual(started["next_offset"], 80)
                status = library_session_status(db, identity)
                self.assertEqual(status["status"], "active")
                self.assertEqual(status["kind"], "article")
                self.assertEqual(status["length_mode"], "fixed")
                self.assertEqual(status["requested_length"], 80)
                self.assertEqual(status["progress"]["current"], 80)
                expected_score, expected_difficulty = get_library_ranker().rank(content[:80])
                self.assertEqual((started["score"], started["difficulty"]), (expected_score, expected_difficulty))
                self.assertEqual(
                    db.execute("SELECT difficulty FROM library_rankings WHERE text_id='text-1'").fetchone()[0],
                    "普",
                )

                acknowledged = acknowledge_library_message(db, {
                    "session_id": started["session_id"], "message_id": "90001",
                })
                self.assertTrue(acknowledged["stored"])
                same_article = continue_same_library_session(db, identity)
                self.assertEqual(same_article["status"], "segment")
                self.assertEqual(same_article["title"], "测试文章")
                self.assertEqual(same_article["segment_no"], 2)
                self.assertIn(content[80:160], same_article["message"])
                self.assertRegex(
                    same_article["message"],
                    rf"-----第\d{{6}}段-FM发文｜进度160/{len(content)}字$",
                )
                wrong = continue_library_session_from_score(db, {
                    **identity, "text": "第999999段 速度140.01 击键6.61 键准91.69% 字数80",
                })
                self.assertEqual(wrong["status"], "ignored")
                continued = continue_library_session_from_score(db, {
                    **identity,
                    "text": f"第{same_article['segment_id']}段 速度140.01 击键6.61 键准91.69% 字数80",
                })
                self.assertEqual(continued["status"], "segment")
                self.assertRegex(
                    continued["message"],
                    rf"-----第\d{{6}}段-FM发文｜进度{continued['next_offset']}/{continued['total_chars']}字$",
                )
                self.assertNotEqual(continued["title"], same_article["title"])
                self.assertNotEqual(same_article["segment_id"], continued["segment_id"])
                duplicate = continue_library_session_from_score(db, {
                    **identity,
                    "text": f"第{same_article['segment_id']}段 速度140.01 击键6.61 键准91.69% 字数80",
                })
                self.assertEqual(duplicate["status"], "ignored")
                acknowledged = acknowledge_library_message(db, {
                    "session_id": continued["session_id"], "message_id": "90002",
                })
                self.assertTrue(acknowledged["stored"])
                recent = recent_library_messages(db, {**identity, "count": 5})
                self.assertEqual(recent["message_ids"], ["90002", "90001"])
                marked = mark_library_messages_recalled(db, {
                    **identity, "message_ids": ["90002"],
                })
                self.assertEqual(marked["marked"], 1)
                self.assertEqual(
                    recent_library_messages(db, {**identity, "count": 5})["message_ids"],
                    ["90001"],
                )
                previous = continue_previous_library_session(db, identity)
                self.assertEqual(previous["status"], "segment")
                self.assertEqual(previous["title"], "测试文章")
                self.assertEqual(previous["segment_no"], 3)
                self.assertIn(content[160:240], previous["message"])
                self.assertRegex(
                    previous["message"],
                    rf"-----第\d{{6}}段-FM发文｜进度240/{len(content)}字$",
                )
                self.assertEqual(previous["recall_message_id"], "90002")
                self.assertEqual(previous["continuation_mode"], "previous_article")
                self.assertEqual(
                    continue_previous_library_session(db, identity)["status"], "no_previous"
                )

                short = start_library_session(db, {**identity, "query": "短文测试", "length": 100})
                self.assertEqual(short["status"], "not_found")
                self.assertIn("没有找到", short["message"])
                stopped = stop_library_session(db, identity)
                self.assertEqual(stopped["status"], "stopped")
                self.assertIsNone(stopped["last_message_id"])
                self.assertEqual(continue_library_session(db, identity)["status"], "idle")

                stats = library_stats(db)
                self.assertEqual(stats["texts"], 3)
                self.assertEqual(stats["ranked_texts"], 1)
                self.assertEqual(stats["active_sessions"], 0)
                self.assertEqual(library_session_status(db, identity)["status"], "idle")
            finally:
                db.close()

    def test_library_classification_is_persisted_and_keeps_genre_on_continuation(self):
        horror = "午夜鬼屋里，阴森的走廊传来脚步声，怨灵在门后低声哭泣。"
        cultivation = "修仙者引灵气入丹田，在宗门中炼成金丹，准备渡劫飞升。"
        self.assertEqual(classify_library_text("午夜鬼屋", horror * 20)["primary_genre"], "恐怖惊悚")
        self.assertEqual(classify_library_text("宗门修行", cultivation * 20)["primary_genre"], "仙侠修真")
        self.assertEqual(resolve_library_genre("恐怖故事"), "恐怖惊悚")
        self.assertEqual(
            classify_library_text("普通文章", "今天阳光很好，大家在公园散步，记录一些日常见闻。" * 8)["primary_genre"],
            "综合文学",
        )
        self.assertEqual(
            classify_library_text("春日小诗", "春风拂过柳梢\n月色落在江心\n远山含着薄雾\n小舟驶向星河")["form"],
            "诗歌",
        )
        with tempfile.TemporaryDirectory() as root:
            db = connect(str(Path(root) / "fm.sqlite3"))
            try:
                for index in (1, 2):
                    content = horror * 30 + f"这是恐怖故事的第{index}篇。"
                    db.execute(
                        "INSERT INTO library_texts VALUES (?, ?, ?, ?, ?, ?)",
                        (f"horror-{index}", "fm_texts", f"fm_texts/horror-{index}.txt", f"午夜鬼屋{index}", content, len(content)),
                    )
                db.execute(
                    "INSERT INTO library_texts VALUES (?, ?, ?, ?, ?, ?)",
                    ("plain-1", "fm_texts", "fm_texts/plain.txt", "春日散步", "春风吹过河岸，阳光落在树梢。" * 30, 300),
                )
                db.commit()
                identity = {
                    "platform": "qq", "chat_id": "genre-test", "requester_id": "tester",
                    "requester_name": "tester",
                }
                started = start_library_session(db, {**identity, "query": "来一篇恐怖文", "length": 80})
                self.assertEqual(started["status"], "segment")
                self.assertEqual(started["genre"], "恐怖惊悚")
                self.assertIn("题材=恐怖惊悚", started["metadata_summary"])
                self.assertIn("文体=小说/叙事", started["metadata_summary"])
                self.assertIn("·恐怖惊悚·小说/叙事]", started["message"])
                self.assertIn("·恐怖惊悚", started["message"])
                self.assertEqual(
                    db.execute("SELECT COUNT(*) FROM library_classifications").fetchone()[0], 1
                )
                continued = continue_library_session(db, identity)
                self.assertEqual(continued["status"], "segment")
                self.assertEqual(continued["genre"], "恐怖惊悚")
                self.assertEqual(
                    db.execute(
                        "SELECT requested_genre FROM library_session_modes WHERE session_id=?",
                        (started["session_id"],),
                    ).fetchone()[0],
                    "恐怖惊悚",
                )
                batch = classify_library(db)
                self.assertEqual(batch["classified"], 1)
                self.assertEqual(library_stats(db)["classified_texts"], 3)
            finally:
                db.close()

    def test_single_character_session_uses_progress_footer_only(self):
        with tempfile.TemporaryDirectory() as root:
            db = connect(str(Path(root) / "fm.sqlite3"))
            try:
                characters = "天地玄黄宇宙洪荒日月盈昃辰宿列张"
                db.execute(
                    "INSERT INTO library_texts VALUES (?, ?, ?, ?, ?, ?)",
                    ("single-1", "fm_single_chars", "fm_single_chars/前500.txt", "前500", characters, len(characters)),
                )
                db.commit()
                identity = {
                    "platform": "qq", "chat_id": "20001", "requester_id": "10001",
                    "requester_name": "tester",
                }
                started = start_single_session(db, {
                    **identity, "name": "前500", "length": 5, "order": "顺",
                    "key_req": 6, "acc_req": 100,
                })
                self.assertEqual(started["status"], "segment")
                self.assertIn("[FM/单字·前500·顺 击6 准100]", started["message"])
                self.assertIn("\n天地玄黄宇\n", started["message"])
                self.assertTrue(started["message"].endswith("-----第1段-FM发文｜1/4｜进度5/16字"))
                self.assertNotIn("\n｜第", started["message"])
                self.assertNotIn("@tester", started["message"])
            finally:
                db.close()

    def test_repeat_follow_and_bot_loop_guard(self):
        with tempfile.TemporaryDirectory() as root:
            db = connect(str(Path(root) / "fm.sqlite3"))
            try:
                write_setting(db, "repeat_follow", {"enabled": True, "group_overrides": {}})
                write_setting(db, "bot_guard", {
                    "loop_watch_accounts": {"90001": {"label": "test bot"}}
                })
                db.commit()
                first = repeat_check(db, {
                    "group_id": "20001", "sender_id": "10001", "text": "同一句", "occurred_at": 100,
                })
                second = repeat_check(db, {
                    "group_id": "20001", "sender_id": "10002", "text": "同一句", "occurred_at": 105,
                })
                third = repeat_check(db, {
                    "group_id": "20001", "sender_id": "10003", "text": "同一句", "occurred_at": 106,
                })
                self.assertFalse(first["repeat"])
                self.assertTrue(second["repeat"])
                self.assertFalse(third["repeat"])
                set_group_capability(db, "20001", "repeat", False)
                disabled = repeat_check(db, {
                    "group_id": "20001", "sender_id": "10004", "text": "另一句", "occurred_at": 200,
                })
                self.assertFalse(disabled["repeat"])
                self.assertFalse(disabled["enabled"])
                blocked = bot_guard_check(db, {
                    "group_id": "20001", "sender_id": "90001", "explicit": True, "occurred_at": 100,
                })
                quiet = bot_guard_check(db, {
                    "group_id": "20001", "sender_id": "90001", "explicit": True, "occurred_at": 101,
                })
                self.assertTrue(blocked["blocked"])
                self.assertTrue(blocked["reply"])
                self.assertTrue(quiet["blocked"])
                self.assertFalse(quiet["reply"])
            finally:
                db.close()

    def test_imports_contest_and_score_data(self):
        with tempfile.TemporaryDirectory() as root:
            root = Path(root)
            contest = root / "snapshot" / "contest_library" / "auto" / "sample-group"
            outputpro = root / "snapshot" / "plugin_data" / "astrbot_plugin_outputpro"
            contest.mkdir(parents=True)
            outputpro.mkdir(parents=True)
            (contest / "2026-08-22_sample.txt").write_text("sample contest body", encoding="utf-8")
            (outputpro / "fm_ai_competition_daily.json").write_text(
                json.dumps({
                    "date": "2026-08-22", "title": "daily", "body": "daily body",
                    "difficulty": "easy", "provider": "test", "generated_at": 1,
                }),
                encoding="utf-8",
            )
            score = {
                "history_key": "score-1", "competition_date": "2026-08-22",
                "user_id": "@qq_10001:example.org", "user_name": "tester", "group_id": "20001",
                "speed": 120.5, "key": 6.1, "acc": 98.0, "ts": 2,
            }
            (outputpro / "fm_ai_competition_history.jsonl").write_text(
                json.dumps(score) + "\n", encoding="utf-8"
            )
            (outputpro / "fm_competition_scores.json").write_text(
                json.dumps({"records": [{**score, "source_group": "sample-group"}]}),
                encoding="utf-8",
            )

            database = root / "fm.sqlite3"
            db = connect(str(database))
            try:
                result = import_retained_snapshot(db, root / "snapshot")
                self.assertEqual(result, {
                    "contest_texts": 1,
                    "ai_contest_texts": 1,
                    "ai_contest_scores": 1,
                    "competition_scores": 1,
                })
                self.assertEqual(db.execute("SELECT source_group FROM contest_texts").fetchone()[0], "sample-group")
                self.assertEqual(db.execute("SELECT speed FROM ai_contest_scores").fetchone()[0], 120.5)
                self.assertEqual(db.execute("SELECT user_id FROM ai_contest_scores").fetchone()[0], "10001")
                if importlib.util.find_spec("PIL"):
                    os.environ["FM_REPORT_FONT"] = str(Path(__file__).parent.parent / "msyh.ttc")
                    report = render_ai_leaderboard(db, "2026-08-22")
                    self.assertTrue(report.startswith(b"\x89PNG\r\n\x1a\n"))
            finally:
                db.close()

    def test_message_score_and_recall_events(self):
        with tempfile.TemporaryDirectory() as root:
            database = Path(root) / "fm.sqlite3"
            db = connect(str(database))
            db.execute(
                "INSERT INTO settings VALUES (?, ?)",
                ("score_state", json.dumps({"groups": ["20001"]})),
            )
            content = "春风吹过小路，树叶轻轻摇晃。大家沿着河边慢慢散步。" * 12
            db.execute(
                "INSERT INTO library_texts VALUES (?, ?, ?, ?, ?, ?)",
                ("score-route-text", "fm_texts", "fm_texts/score-route.txt", "成绩续段测试", content, len(content)),
            )
            db.commit()
            db.close()
            Api.db_path = str(database)
            server = ThreadingHTTPServer(("127.0.0.1", 0), Api)
            thread = threading.Thread(target=server.serve_forever, daemon=True)
            thread.start()
            base = f"http://127.0.0.1:{server.server_port}"

            def post(path, payload):
                request = urllib.request.Request(
                    base + path,
                    data=json.dumps(payload).encode(),
                    headers={"Content-Type": "application/json"},
                    method="POST",
                )
                return json.load(urllib.request.urlopen(request))

            try:
                identity = {
                    "platform": "qq", "chat_id": "20001", "requester_id": "10001",
                    "requester_name": "tester",
                }
                started = post("/library/session/start", {
                    **identity, "query": "成绩续段测试", "length": 50,
                })
                continued = post("/library/session/score", {
                    **identity,
                    "text": f"第{started['segment_id']}段 速度123.45 击键6.70 键准98.20% 字数50",
                })
                self.assertEqual(continued["status"], "segment")

                stored = post("/events/message", {
                    "platform": "qq", "message_id": "m1", "group_id": "20001",
                    "sender_id": "10001", "sender_name": "tester",
                    "text": "第555段 速度123.45 击键6.70 键准98.20% 字数250",
                    "occurred_at": time.time(),
                })
                self.assertTrue(stored["score_archived"])
                self.assertTrue(stored["ai_contest_archived"])
                recalled = post("/events/recall", {
                    "platform": "qq", "message_id": "m1", "group_id": "20001",
                    "occurred_at": time.time(),
                })
                self.assertTrue(recalled["captured"])
                check = connect(str(database))
                self.assertEqual(check.execute("SELECT COUNT(*) FROM score_records").fetchone()[0], 1)
                self.assertEqual(check.execute("SELECT COUNT(*) FROM ai_contest_scores").fetchone()[0], 1)
                self.assertEqual(check.execute("SELECT text FROM recall_records").fetchone()[0], "第555段 速度123.45 击键6.70 键准98.20% 字数250")
                health = archive_status(check)
                self.assertEqual(health["status"], "ok")
                self.assertEqual(health["sources"]["typing_scores"]["count"], 1)
                self.assertEqual(health["sources"]["recalls"]["count"], 1)
                check.close()
            finally:
                server.shutdown()
                server.server_close()

    def test_poke_is_a_persistent_group_capability(self):
        with tempfile.TemporaryDirectory() as root:
            db = connect(str(Path(root) / "fm.sqlite3"))
            try:
                self.assertTrue(set_group_capability(db, "20001", "poke", False)["features"]["poke"] is False)
                self.assertFalse(group_capability_enabled(db, "20001", "poke"))
                self.assertTrue(set_group_capability(db, "20001", "poke", True)["features"]["poke"])
                self.assertTrue(group_capability_enabled(db, "20001", "poke"))
            finally:
                db.close()

    def test_score_chart_uses_full_history_and_requested_days(self):
        if not importlib.util.find_spec("PIL"):
            self.skipTest("Pillow is not installed")
        from PIL import Image

        with tempfile.TemporaryDirectory() as root:
            db = connect(str(Path(root) / "fm.sqlite3"))
            try:
                today = datetime.now(timezone.utc).date()
                for index in range(25):
                    date = (today - timedelta(days=index)).isoformat()
                    item = {
                        "record_id": f"chart-{index}", "competition_date": date,
                        "user_id": "10001", "user_name": "Fixmood", "group_id": "20001",
                        "source_group": "虎杯", "speed": 100 + index,
                        "key": 5 + index / 10, "acc": 94 + (index % 3),
                    }
                    db.execute(
                        "INSERT INTO competition_scores "
                        "(record_id,competition_date,user_id,user_name,group_id,source_group,speed,occurred_at,source_json) "
                        "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
                        (item["record_id"], date, item["user_id"], item["user_name"],
                         item["group_id"], item["source_group"], item["speed"],
                         time.time() - index, json.dumps(item, ensure_ascii=False)),
                    )
                db.commit()
                full = competition_score_history(db, name="Fixmood", source="虎杯", days=30)
                recent = competition_score_history(db, name="Fixmood", source="虎杯", days=7)
                self.assertEqual(len(full), 25)
                self.assertEqual(len(recent), 7)
                self.assertEqual(full[0]["competition_date"], (today - timedelta(days=24)).isoformat())
                os.environ["FM_REPORT_FONT"] = str(Path(__file__).parent.parent / "msyh.ttc")
                png = render_score_chart(db, "", "Fixmood", "虎杯", 30, "speed")
                self.assertTrue(png.startswith(b"\x89PNG\r\n\x1a\n"))
                with Image.open(io.BytesIO(png)) as image:
                    expected_height = max(1040, 790 + 25 * 31 + 100)
                    self.assertEqual(image.size, (1600, expected_height))
            finally:
                db.close()

    def test_score_chart_falls_back_to_legacy_score_archive(self):
        with tempfile.TemporaryDirectory() as root:
            db = connect(str(Path(root) / "fm.sqlite3"))
            try:
                raw = {
                    "sender_id": "10001", "sender_name": "Fixmood",
                    "group_name": "虎码训练♂营", "speed": 123.4,
                    "accuracy": "96.5%", "keystrokes": 6.8,
                    "received_at": "2026-08-28 12:00:00",
                }
                db.execute(
                    "INSERT INTO score_records VALUES (?, ?, ?, ?, ?)",
                    ("legacy-1", "20001", "10001", time.time(), json.dumps(raw, ensure_ascii=False)),
                )
                db.commit()
                records = competition_score_history(db, name="Fixmood", source="虎杯", days=30)
                self.assertEqual(len(records), 1)
                self.assertEqual(records[0]["source_group"], "虎杯")
                self.assertEqual(records[0]["competition_date"], "2026-08-28")
                summary = competition_score_summary(db, name="Fixmood", source="虎杯")
                self.assertEqual(summary["count"], 1)
                self.assertEqual(summary["best_speed"], 123.4)
            finally:
                db.close()


if __name__ == "__main__":
    unittest.main()
