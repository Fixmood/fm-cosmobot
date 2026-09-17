/* ============================================================================
   业务视图（二）：赛文赛事 / 群与房间 / 消息与归档
   实测接口与字段：
     /contest/search                -> {items:[{text_id,title,source_group,
                                        competition_date,char_count,relative_path}],total}
     /ai-contest/leaderboard        -> {date,rows:[{segment_id,speed,keystroke,accuracy,
                                        characters,group_id,competition_date}]}
     /competition/summary           -> {count,best_speed,average_speed,average_key,
                                        average_accuracy,sources{},recent:[]}
     /competition/live?source=|group_id=  -> {status,code,message,text,scores[]}
     /groups                        -> {items:[{group_id,display_name,status,
                                        features_json,updated_at,paused}],total}
     /group-capability?group_id=    -> {group_id,capabilities:{agent,ai_contest,…}}
     /group-state?group_id=         -> {group_id,online,updated_at}
     /repeat-follow/state?group_id= -> {global_enabled,enabled}
     /recent_messages               -> {items:[{platform,group_id,group_name,sender_id,
                                        sender_name,text,occurred_at,image_urls_json}],total}
     /recalls                       -> {items:[{message_id,group_id,group_name,sender_id,
                                        sender_name,text,recalled_ts,image_urls}],total}
     /archive/status                -> {status,sources:{k:{count,latest_at}},active_library_sessions}
   ========================================================================= */

import {
  h, num, pct, table, card, stat, section, chip, blank, skeleton, errorState,
  domain, cached, soft, filters, toneFor, clock, ago, paginate, pager,
  patchRoute, cols, toast, unwrap, parseRoute, groupName,
} from './core.js';

/* ── 赛文赛事 ─────────────────────────────────────────────────────────── */
export async function renderContests(route) {
  const wrap = h('div');

  const [boardRaw, summaryRaw] = await Promise.all([
    soft(cached('ai:board', () => domain('ai-contest/leaderboard'))),
    soft(cached('competition:summary', () => domain('competition/summary'))),
  ]);
  const board = unwrap(boardRaw) || {};
  const summary = unwrap(summaryRaw) || {};
  const rows = Array.isArray(board.rows) ? board.rows : [];
  const sources = summary.sources && typeof summary.sources === 'object' ? summary.sources : {};

  wrap.append(cols(4,
    stat('AI 赛文榜', num(rows.length), board.date || '今日', rows.length ? 'ok' : 'warn'),
    stat('赛事成绩', num(summary.count), '全部来源'),
    stat('最佳速度', num(summary.best_speed), '字/分'),
    stat('平均准确率', pct(summary.average_accuracy), `平均击键 ${num(summary.average_key)}`)));

  /* 排行榜 */
  wrap.append(section('AI 赛文排行榜', board.date ? `日期 ${board.date}` : null,
    card(null, boardRaw?._err ? errorState(boardRaw._err) : table([
      { label: '排名', num: true, render: (r) => num(r.rank ?? r.position ?? '—') },
      { label: '速度', num: true, render: (r) => num(r.speed) },
      { label: '击键', num: true, render: (r) => num(r.keystroke) },
      { label: '准确率', num: true, render: (r) => pct(r.accuracy) },
      { label: '字数', num: true, render: (r) => num(r.characters) },
      { label: '段落', mono: true, render: (r) => String(r.segment_id || '').slice(0, 10) },
    ], rows, { empty: '今日暂无排行榜数据' }))));

  /* 来源分布 + 最近赛事成绩 */
  const recent = Array.isArray(summary.recent) ? summary.recent : [];
  wrap.append(section('赛事数据', '来源分布与最近成绩',
    cols(2,
      card('来源分布', table([
        { label: '来源', wrap: true, key: 'k' },
        { label: '成绩数', num: true, render: (r) => num(r.v) },
      ], Object.entries(sources).map(([k, v]) => ({ k, v })), { empty: '暂无来源数据' })),
      card('最近成绩', table([
        { label: '速度', num: true, render: (r) => num(r.speed) },
        { label: '击键', num: true, render: (r) => num(r.keystroke) },
        { label: '准确率', num: true, render: (r) => pct(r.accuracy) },
        { label: '来源', wrap: true, render: (r) => r.source || '—' },
      ], recent.slice(0, 10), { empty: '暂无成绩' })))));

  /* 历史赛文检索 */
  const state = { q: route.query.q || '', page: Number(route.query.page || 1), limit: 50 };
  const listHost = h('div');
  const pagerHost = h('div');

  async function loadContests() {
    listHost.replaceChildren(skeleton(4));
    pagerHost.replaceChildren();
    try {
      const data = await domain('contest/search', { limit: 300 });
      let items = Array.isArray(data) ? data : (data.items || []);
      if (!Array.isArray(items)) items = [];
      if (state.q) {
        const needle = state.q.toLowerCase();
        items = items.filter((r) => String(r.title || '').toLowerCase().includes(needle)
          || String(r.source_group || '').toLowerCase().includes(needle)
          || String(r.competition_date || '').includes(needle));
      }
      const paged = paginate(items, state.page, state.limit);
      listHost.replaceChildren(table([
        { label: '赛文标题', wrap: true, render: (r) => String(r.title || '（无标题）').slice(0, 92) },
        { label: '来源群', wrap: true, render: (r) => r.source_group || '—' },
        { label: '比赛日期', mono: true, render: (r) => r.competition_date || '—' },
        { label: '字数', num: true, render: (r) => num(r.char_count) },
        { label: 'ID', mono: true, render: (r) => String(r.text_id || '').slice(0, 10) },
      ], paged.rows, { empty: '没有匹配的赛文' }));
      pagerHost.replaceChildren(pager(paged, (next) => {
        state.page = next; patchRoute({ page: next }); loadContests();
      }));
    } catch (err) {
      listHost.replaceChildren(errorState(err.message, loadContests));
    }
  }

  wrap.append(section('赛文库', '历史赛文检索',
    card(null,
      filters([
        { input: true, placeholder: '搜索标题 / 来源群 / 日期…', value: state.q,
          onenter: (v) => { state.q = v; state.page = 1; patchRoute({ q: v, page: 1 }); loadContests(); } },
        { push: true },
        { button: '刷新', icon: 'refresh', onclick: loadContests },
      ]),
      listHost,
      h('div', { style: 'padding:0 20px' }, pagerHost))));

  /* 在线赛事：按来源查询 */
  const liveInput = h('input', {
    class: 'in', style: 'min-width:260px',
    placeholder: '群号，或赛事名（如「极速杯」）',
  });
  const liveHost = h('div', {}, blank('输入来源后查询', '在线赛事需要指定群号或赛事名'));

  async function loadLive() {
    const source = liveInput.value.trim();
    if (!source) { toast('请先输入群号或赛事名', 'warn'); return; }
    liveHost.replaceChildren(skeleton(3));
    try {
      const data = unwrap(await domain('competition/live', /^\d+$/.test(source) ? { group_id: source } : { source }));
      if (data?.status === 'error') {
        liveHost.replaceChildren(errorState(data.message || '该来源不可用'));
        return;
      }
      const text = data?.text || data?.title || '';
      const scores = Array.isArray(data?.scores) ? data.scores : (Array.isArray(data?.rows) ? data.rows : []);
      liveHost.replaceChildren(h('div', {},
        text ? h('p', {
          style: 'white-space:pre-wrap;max-height:280px;overflow:auto;margin-bottom:16px;'
            + 'padding:12px;background:var(--bg-muted);border-radius:var(--r);font-size:var(--t-sm)',
        }, String(text).slice(0, 1600)) : null,
        scores.length
          ? table([
              { label: '速度', num: true, render: (r) => num(r.speed) },
              { label: '击键', num: true, render: (r) => num(r.keystroke) },
              { label: '准确率', num: true, render: (r) => pct(r.accuracy) },
              { label: '字数', num: true, render: (r) => num(r.characters) },
            ], scores.slice(0, 40))
          : blank('该来源暂无可读数据')));
    } catch (err) {
      liveHost.replaceChildren(errorState(err.message, loadLive));
    }
  }

  wrap.append(section('在线赛事', '按群号或赛事名查询当前赛文与排名',
    card(null,
      filters([{ node: liveInput }, { button: '查询', primary: true, onclick: loadLive }]),
      liveHost)));

  loadContests();
  return wrap;
}

/* ── 群与房间 ─────────────────────────────────────────────────────────── */
export async function renderGroups() {
  const wrap = h('div');

  const groupsRaw = await soft(cached('groups', () => domain('groups')));
  const rows = Array.isArray(groupsRaw) ? groupsRaw : (groupsRaw?.items || []);
  const online = rows.filter((r) => String(r.status || '').toLowerCase() === 'online').length;
  const paused = rows.filter((r) => Number(r.paused) > 0).length;

  wrap.append(cols(4,
    stat('群 / 房间', num(rows.length), '已登记'),
    stat('在线', num(online), '状态为 online', online ? 'ok' : 'warn'),
    stat('暂停中', num(paused), paused ? '已停用响应' : '无', paused ? 'warn' : 'ok'),
    stat('数据来源', 'fm-domain', '/groups')));

  const listHost = h('div');
  const detailHost = h('div', {}, blank('选择一个群查看能力', '点击下方任意一行的「详情」'));

  function showDetail(group) {
    detailHost.replaceChildren(skeleton(3));
    detailHost.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
    Promise.all([
      soft(domain('group-capability', { group_id: group.group_id })),
      soft(domain('group-state', { group_id: group.group_id })),
      soft(domain('repeat-follow/state', { group_id: group.group_id })),
    ]).then(([capRaw, stateRaw, repeatRaw]) => {
      const cap = unwrap(capRaw) || {};
      const st = unwrap(stateRaw) || {};
      const repeat = unwrap(repeatRaw) || {};
      const entries = Object.entries(cap.capabilities || {});

      detailHost.replaceChildren(
        h('div', { style: 'display:flex;align-items:center;gap:8px;margin-bottom:12px' },
          h('b', {}, group.display_name || group.group_id),
          chip(group.status || '—', toneFor(group.status))),
        cols(3,
          card('能力开关', capRaw?._err ? errorState(capRaw._err)
            : entries.length
              ? h('div', {},
                  h('p', { class: 'stat-note', style: 'margin-bottom:10px' },
                    `已开启 ${entries.filter(([, v]) => v).length} / ${entries.length}`),
                  h('div', { style: 'display:flex;flex-wrap:wrap;gap:6px' },
                    entries.map(([name, on]) => chip(name, on ? 'ok' : undefined))))
              : blank('无能力数据')),
          card('在线状态', stateRaw?._err ? errorState(stateRaw._err) : table([
            { label: '项', key: 'k' }, { label: '值', key: 'v' },
          ], [
            { k: '群号', v: st.group_id ?? group.group_id },
            { k: '在线', v: st.online ? '是' : '否' },
            /* 用 last_active_at（后端从消息归档算的真实活跃时间）。
               groups.updated_at 只在 observe_group() 里写，群消息进来时不更新，
               实测停在 2026-08-28~09-02 —— 用它会把「21 天前」显示给一个刚有消息的群。 */
            { k: '最后活动', v: group.last_active_at ? ago(group.last_active_at) : '（无消息）' },
          ])),
          card('复读跟随', repeatRaw?._err ? errorState(repeatRaw._err) : table([
            { label: '项', key: 'k' }, { label: '值', key: 'v' },
          ], [
            { k: '全局开关', v: repeat.global_enabled ? '开启' : '关闭' },
            { k: '本群开关', v: repeat.enabled ? '开启' : '关闭' },
          ]))));
    });
  }

  listHost.replaceChildren(groupsRaw?._err ? errorState(groupsRaw._err)
    : table([
        { label: '群号', mono: true, render: (r) => String(r.group_id || '—') },
        { label: '群名', wrap: true, render: (r) => r.display_name || '（未命名）' },
        { label: '状态', render: (r) => chip(r.status || '—', toneFor(r.status)) },
        { label: '暂停', render: (r) => (Number(r.paused) > 0 ? chip('已暂停', 'warn') : '—') },
        { label: '最后活动', render: (r) => (r.last_active_at ? ago(r.last_active_at) : '（无消息）') },
        { label: '', render: (r) => h('button', {
            class: 'btn', type: 'button',
            onclick: (e) => { e.stopPropagation(); showDetail(r); },
          }, '详情') },
      ], rows, { empty: '暂无群数据' }));

  wrap.append(section('群列表', `${rows.length} 个`,
    card(null, listHost),
    h('div', { style: 'margin-top:16px' }, detailHost)));

  return wrap;
}

/* ── 消息与归档 ───────────────────────────────────────────────────────── */
export async function renderMessages(route) {
  const wrap = h('div');
  const tab = route.query.tab || 'recent';

  const [recentRaw, recallsRaw, archiveRaw] = await Promise.all([
    soft(cached('msg:recent', () => domain('recent_messages', { limit: 100 }))),
    soft(cached('msg:recalls', () => domain('recalls', { limit: 100 }))),
    soft(cached('archive', () => domain('archive/status'))),
  ]);

  const recentRows = Array.isArray(recentRaw) ? recentRaw : (recentRaw?.items || []);
  const recallRows = Array.isArray(recallsRaw) ? recallsRaw : (recallsRaw?.items || []);
  const archive = unwrap(archiveRaw) || {};
  const sources = archive.sources && typeof archive.sources === 'object' ? archive.sources : {};

  wrap.append(cols(4,
    stat('最近消息', num(recentRaw?.total ?? recentRows.length), '样本上限 100'),
    stat('撤回记录', num(recallsRaw?.total ?? recallRows.length), '样本上限 100'),
    stat('归档来源', num(Object.keys(sources).length), '数据源'),
    stat('活跃发文', num(archive.active_library_sessions), '进行中',
      archive.active_library_sessions ? 'ok' : undefined)));

  /* 归档来源明细 */
  if (Object.keys(sources).length) {
    wrap.append(section('归档来源', '各数据源的记录数与最新时间',
      card(null, table([
        { label: '来源', mono: true, key: 'k' },
        { label: '记录数', num: true, render: (r) => num(r.count) },
        { label: '最新', render: (r) => (r.latest_at ? clock(r.latest_at) : '—') },
      ], Object.entries(sources).map(([k, v]) => ({ k, ...v }))))));
  }

  /* 消息 / 撤回切换 */
  let active = tab;
  const host = h('div');
  const tabs = h('div', { class: 'tabs', role: 'tablist', style: 'margin-bottom:16px' },
    [['recent', '最近消息'], ['recalls', '撤回查询']].map(([key, label]) =>
      h('button', {
        class: 'tab-btn', type: 'button', role: 'tab', 'data-tab': key,
        'aria-selected': String(tab === key),
        onclick: () => { active = key; patchRoute({ tab: key }); },
      }, label)));

  function draw() {
    [...tabs.children].forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === active)));
    host.replaceChildren(active === 'recalls'
      ? table([
          { label: '时间', mono: true, render: (r) => clock(r.recalled_ts) },
          { label: '群', wrap: true, render: (r) => r.group_name || r.group_id || '—' },
          { label: '发送者', wrap: true, render: (r) => r.sender_name || r.sender_id || '—' },
          { label: '原文', wrap: true, render: (r) => String(r.text || '').slice(0, 140) || '（图片或空消息）' },
          { label: '消息号', mono: true, render: (r) => String(r.message_id || '').slice(0, 12) },
        ], recallRows, { empty: '暂无撤回记录' })
      : table([
          { label: '时间', mono: true, render: (r) => clock(r.occurred_at) },
          { label: '平台', render: (r) => chip(r.platform || '—') },
          { label: '群', wrap: true, render: (r) => r.group_name || r.group_id || '—' },
          { label: '发送者', wrap: true, render: (r) => r.sender_name || r.sender_id || '—' },
          { label: '内容', wrap: true, render: (r) => String(r.text || '').slice(0, 140) || '（图片或空消息）' },
        ], recentRows, { empty: '暂无消息' }));
  }

  wrap.append(section(null, null, tabs, card(null, host)));
  draw();

  /* 切换 tab 只改 hash，视图自己收敛，不重建整页 */
  const onHash = () => { active = parseRoute().query.tab || 'recent'; draw(); };
  window.addEventListener('hashchange', onHash);
  wrap.addEventListener('view:leave', () => window.removeEventListener('hashchange', onHash));

  return wrap;
}
