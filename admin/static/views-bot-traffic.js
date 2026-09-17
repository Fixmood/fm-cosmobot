/* ============================================================================
   机器人发言 / 运行日志
   ────────────────────────────────────────────────────────────────────────────
   为什么有这一页
     后台原有的「消息与归档」只展示 message_archive —— 那是**用户**的发言。
     机器人自己的回复存在另一个库 cosmobot.sqlite3 的 chat_log 表里
     （实测 3,086 条），后台此前完全没有入口。
     「会话管理」那是一套 ACP 会话，库里只有 15 行测试数据、日志里零活动，
     占着导航位置但没有运营价值，所以换成了这一页。

   数据来源（fm-domain 的两个只读接口，读的是机器人自己的库）
     GET /bot/messages  机器人最近的发言 + 它回复的那条消息
     GET /bot/runs      最近的 agent 运行：耗时、轮数、下发工具数、token
   ========================================================================= */

import {
  h, num, pct, table, card, statCount, section, chip, blank, skeleton,
  errorState, domain, cached, soft, filters, select, clock, ago, parseRoute,
  patchRoute, cols, stack, unwrap,
} from './core.js';

/* 时间字段现在由后端统一换算成 Unix 时间戳（`at_unix`），
   和 /scores 的 `received_at`、归档页的 `occurred_at` 同一个口径，
   前端再也不用猜时区。

   ★ 这里踩过一个坑，留个记录：
   第一版后端原样返回 `recorded_at`（'2026-09-17 15:34:46.985508029+0000'），
   我写了个正则只取到秒、把 '+0000' 丢掉，于是 UTC 的 15:34 被浏览器
   当成本地时间显示 —— 而实际北京时间是 23:34，整页少了 8 小时。

   定位方法是拿同一条对话两头对：
     chat_log 这条的 recorded_at           → UTC 15:34:46
     它回复的那条在 message_archive 的时间   → UTC 15:34:39
   只差 8 秒，证明 '+0000' 是真的 UTC。

   所以下面的 isoUtc 只是**兜底**（万一哪天后端又开始返字符串），
   正常情况下走 at_unix 这个数字。 */
function isoUtc(value) {
  if (!value) return null;
  const text = String(value).trim();
  const match = text.match(
    /^(\d{4}-\d{2}-\d{2})[ T](\d{2}:\d{2}:\d{2})(?:\.(\d+))?(Z|[+-]\d{2}:?\d{2})?$/);
  if (!match) return text;
  const [, date, time, frac, tz] = match;
  const micros = frac ? frac.slice(0, 6) : '';
  let zone = tz || 'Z';
  if (zone !== 'Z' && !zone.includes(':')) zone = `${zone.slice(0, 3)}:${zone.slice(3)}`;
  return `${date}T${time}${micros ? `.${micros}` : ''}${zone}`;
}

/* 优先用后端给的 Unix 秒；没有才回退到解析原始串。 */
const stamp = (row) => clock(
  row && (row.at_unix ?? isoUtc(row.at_raw ?? row.at)));

const CHAT_LABEL = (id) => (id ? `群/人 ${id}` : '（未知会话）');

export async function renderBotTraffic(route) {
  const tab = route.query.tab || 'messages';
  const span = Number(route.query.limit || 60);
  const chat = route.query.chat || '';
  const keyword = route.query.q || '';

  const wrap = h('div');
  const host = h('div');

  const TABS = [['messages', '机器人发言'], ['runs', '运行日志']];
  const tabs = h('div', { class: 'tabs', role: 'tablist', style: 'margin-bottom:16px' },
    TABS.map(([key, label]) => h('button', {
      class: 'tab-btn', type: 'button', role: 'tab', 'data-tab': key,
      'aria-selected': String(tab === key),
      onclick: () => patchRoute({ tab: key }),
    }, label)));

  /* ── 顶部：先拿发言列表，用它的分布算指标 ─────────────────────────── */
  /* 注意要 unwrap：中控的 /api/domain/* 透传会把后端的返回再包一层，
     拿到的形状是 {ok, data:{available, count, messages}}。
     漏掉 unwrap 就会读到 undefined —— 这一页我第一版就是这么写错的。 */
  const [messagesRaw, runsRaw] = await Promise.all([
    soft(cached(`bot:msg:${span}:${chat}:${keyword}`,
      () => domain('bot/messages', { limit: span, chat_id: chat, q: keyword }))),
    soft(cached(`bot:runs:${span}`, () => domain('bot/runs', { limit: span }))),
  ]);
  const msgData = unwrap(messagesRaw) || {};
  const runData = unwrap(runsRaw) || {};
  const messages = Array.isArray(msgData.messages) ? msgData.messages : [];
  const runs = Array.isArray(runData.runs) ? runData.runs : [];
  const unavailable = msgData.available === false || runData.available === false;

  if (unavailable) {
    wrap.append(section('机器人发言', '读不到机器人的库',
      card(null, errorState(
        msgData.error || runData.error || '未知原因',
        () => window.dispatchEvent(new CustomEvent('fm:reload-view'))))));
    return wrap;
  }

  /* 按会话统计发言分布，挑出最活跃的几个做筛选下拉 */
  const byChat = new Map();
  for (const m of messages) {
    const key = m.chat_id || '';
    byChat.set(key, (byChat.get(key) || 0) + 1);
  }
  const chatOptions = [{ value: '', label: '全部会话' },
    ...[...byChat.entries()].sort((a, b) => b[1] - a[1]).slice(0, 12)
      .map(([id, n]) => ({ value: id, label: `${CHAT_LABEL(id)}（${n}）` }))];

  const finished = runs.filter((r) => r.elapsed_ms != null);
  const slowest = finished.length
    ? Math.max(...finished.map((r) => r.elapsed_ms)) : null;
  const toolHeavy = runs.length
    ? Math.max(...runs.map((r) => r.exposed_tools || 0)) : null;

  wrap.append(cols(4,
    statCount('近期发言', messages.length, { note: `取最近 ${span} 条` }),
    statCount('发言会话', byChat.size, { note: '有机器人发言的会话数' }),
    statCount('最慢一次运行', slowest == null ? null : (slowest / 1000).toFixed(1), {
      note: '秒', decimals: 1, suffix: ' s',
      tone: slowest != null && slowest > 15000 ? 'warn' : 'ok',
    }),
    statCount('单次下发工具', toolHeavy, {
      note: '最多的一次', tone: toolHeavy != null && toolHeavy > 60 ? 'warn' : undefined,
    })));

  /* ── 视图一：机器人发言 ───────────────────────────────────────────── */
  function drawMessages() {
    host.replaceChildren(skeleton(6));
    const input = h('input', {
      class: 'in', placeholder: '搜机器人说过的话…', value: keyword, style: 'min-width:220px',
    });
    const apply = (patch) => patchRoute({ ...patch, tab: 'messages' });
    input.addEventListener('keydown', (e) => {
      if (e.key === 'Enter') apply({ q: input.value.trim() });
    });

    const listHost = h('div');
    listHost.replaceChildren(
      messagesRaw?._err
        ? errorState(messagesRaw._err)
        : table([
            { label: '时间', mono: true, render: (r) => stamp(r) },
            { label: '会话', render: (r) => chip(String(r.kind || '').replace('Chat', '') || '—') },
            { label: '对象', mono: true, render: (r) => String(r.chat_id || '—') },
            { label: '机器人说', wrap: true, render: (r) => h('div', { style: 'max-width:620px;line-height:1.5' },
                h('div', { style: 'white-space:pre-wrap' }, r.body || '（空）'),
                (r.media_refs || []).length
                  ? h('div', { style: 'margin-top:6px;display:flex;gap:6px;flex-wrap:wrap' },
                      r.media_refs.slice(0, 4).map((ref) => chip(String(ref).replace('media:', '').slice(0, 10), 'info')))
                  : null) },
            { label: '回复的是', wrap: true, render: (r) => (r.reply_to
                ? h('div', { style: 'max-width:360px' },
                    h('div', { style: 'font-size:var(--t-sm);color:var(--fg-subtle)' },
                      r.reply_to.from_bot ? `${r.reply_to.sender || '机器人'}（自己）` : (r.reply_to.sender || '—')),
                    h('div', { style: 'white-space:pre-wrap;font-size:var(--t-base)' },
                      String(r.reply_to.text || '').slice(0, 200) || '（图片或空消息）'))
                : h('span', { class: 'stat-note' }, '主动发言')) },
          ], messages, { empty: '没有匹配的发言', emptyHint: '换个关键词，或清空会话筛选' }));

    host.replaceChildren(card(null,
      filters([
        { node: input },
        { button: '搜索', onclick: () => apply({ q: input.value.trim() }) },
        { node: select(chatOptions, chat, (v) => apply({ chat: v }), { label: '会话筛选' }) },
        { node: select([30, 60, 120, 300].map((n) => ({ value: String(n), label: `最近 ${n} 条` })),
            String(span), (v) => patchRoute({ limit: v, tab: 'messages' }), { label: '条数' }) },
        { push: true },
        { button: '刷新', icon: 'refresh',
          onclick: () => { window.dispatchEvent(new CustomEvent('fm:reload-view')); } },
      ]),
      listHost));
  }

  /* ── 视图二：运行日志 ─────────────────────────────────────────────── */
  function drawRuns() {
    const rows = [...runs].sort((a, b) => (b.elapsed_ms || 0) - (a.elapsed_ms || 0));
    const listHost = rows.length
      ? table([
          { label: '耗时', num: true, render: (r) => (r.elapsed_ms == null
              ? '—' : h('b', {
                  style: r.elapsed_ms > 15000 ? 'color:var(--warn)' : '',
                }, `${(r.elapsed_ms / 1000).toFixed(1)} s`)) },
          { label: '轮数', num: true, render: (r) => num(r.turns) },
          { label: '下发工具', num: true, render: (r) => h('span', {
              style: (r.exposed_tools || 0) > 60 ? 'color:var(--warn);font-weight:700' : '',
            }, num(r.exposed_tools)) },
          { label: '实际调用', num: true, render: (r) => num(r.tool_calls) },
          { label: '输入 token', num: true, render: (r) => num(r.prompt_tokens) },
          { label: '输出 token', num: true, render: (r) => num(r.completion_tokens) },
          { label: '结果', render: (r) => (r.status === 'answered'
              ? chip('已回复', 'ok') : chip(r.status || '—', r.status ? 'warn' : undefined)) },
          { label: '用过的工具', wrap: true, render: (r) => ((r.tools || []).length
              ? h('div', { style: 'display:flex;gap:4px;flex-wrap:wrap;max-width:320px' },
                  r.tools.slice(0, 8).map((t) => chip(t)))
              : h('span', { class: 'stat-note' }, '未调用工具')) },
        ], rows, { empty: '没有运行记录' })
      : blank('没有运行记录');

    host.replaceChildren(card(null,
      filters([
        { node: select([30, 60, 120, 300].map((n) => ({ value: String(n), label: `最近 ${n} 次` })),
            String(span), (v) => patchRoute({ limit: v, tab: 'runs' }), { label: '条数' }) },
        { push: true },
        { button: '刷新', icon: 'refresh',
          onclick: () => { window.dispatchEvent(new CustomEvent('fm:reload-view')); } },
      ]),
      h('p', { class: 'stat-note', style: 'padding:0 20px 12px' },
        '按耗时倒序。耗时由 audit_log 的首尾时间戳相减得出 —— 机器人没把耗时写进事件里。'
        + '「下发工具」是这次实际交给模型的工具数量，与耗时和 token 强相关。'),
      listHost));
  }

  function draw() {
    [...tabs.children].forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === tab)));
    if (tab === 'runs') drawRuns(); else drawMessages();
  }

  wrap.append(section('机器人发言与运行',
    '数据直接读机器人的库（只读挂载）：回答它说过什么、回复了谁、每次运行花了多久',
    tabs, host));
  draw();

  let last = tab;
  const onHash = () => {
    const next = parseRoute().query.tab || 'messages';
    if (next === last) return;
    last = next;
    [...tabs.children].forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === next)));
    if (next === 'runs') drawRuns(); else drawMessages();
  };
  window.addEventListener('hashchange', onHash);
  wrap.addEventListener('view:leave', () => window.removeEventListener('hashchange', onHash));

  return wrap;
}
