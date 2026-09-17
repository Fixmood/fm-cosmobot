/* ============================================================================
   业务视图（一）：跟打文库 / 成绩中心
   实测接口与字段：
     /library/stats   -> {texts,characters,categories{},genres{},difficulties{},
                          ranked_texts,unranked_texts,active_sessions}
     /library/search  -> {items:[{text_id,title,category,genre,form,difficulty,
                          char_count,relative_path}],total}
     /stats/daily     -> {days:[{date,count}],groups:[{group_id,group_name,count}],today,yesterday}
     /stats/advanced  -> {heatmap:[{day,hour,count}],speed_distribution:[{range,count}]}
     /scores          -> {items:[{segment_id,speed,keystroke,accuracy,characters,
                          group_name,sender_name,received_at,source}]}
   ========================================================================= */

import {
  h, num, pct, table, card, stat, statCount, section, chip, blank, skeleton,
  errorState, domain, cached, soft, filters, chart, timeSeriesOption, paginate,
  pager, patchRoute, cols, clock, unwrap, groupName, chartColors, alpha, meter,
  select, parseRoute, countUpIn,
} from './core.js';

const DIFF_TONE = { 易: 'ok', 普: 'info', 水: 'warn', 淼: 'warn', 难: 'bad', 虐: 'bad' };

/* ── 跟打文库 ─────────────────────────────────────────────────────────── */
export async function renderLibrary(route) {
  const state = {
    keyword: route.query.q || '',
    genre: route.query.genre || '',
    difficulty: route.query.difficulty || '',
    page: Number(route.query.page || 1),
    limit: 50,
  };

  const wrap = h('div');
  const stats = unwrap(await soft(cached('library:stats', () => domain('library/stats')))) || {};
  const archive = unwrap(await soft(cached('archive', () => domain('archive/status')))) || {};

  const genres = stats.genres && typeof stats.genres === 'object' ? Object.keys(stats.genres) : [];
  const difficulties = stats.difficulties && typeof stats.difficulties === 'object' ? Object.keys(stats.difficulties) : [];

  wrap.append(cols(4,
    stat('文章总数', num(stats.texts), `已排名 ${num(stats.ranked_texts)}`, 'ok'),
    stat('总字数', num(stats.characters), '字符'),
    stat('已分类', num(stats.classified_texts), `${num(stats.unclassified_texts)} 篇待分类`),
    stat('活跃发文', num(archive.active_library_sessions ?? stats.active_sessions), '进行中的会话')));

  /* 分类与体裁分布 */
  const distRows = Object.entries(stats.categories || {}).map(([k, v]) => ({ k, v }));
  const genreRows = Object.entries(stats.genres || {})
    .sort((a, b) => b[1] - a[1]).slice(0, 10).map(([k, v]) => ({ k, v }));

  wrap.append(section('分布', '按分类与体裁统计',
    cols(3,
      card('分类', table([
        { label: '分类', key: 'k' },
        { label: '篇数', num: true, render: (r) => num(r.v) },
      ], distRows, { empty: '暂无分类数据' })),
      card('体裁 Top 10', table([
        { label: '体裁', key: 'k' },
        { label: '篇数', num: true, render: (r) => num(r.v) },
      ], genreRows, { empty: '暂无体裁数据' })),
      card('难度分布', table([
        { label: '难度', render: (r) => chip(r.k, DIFF_TONE[r.k]) },
        { label: '篇数', num: true, render: (r) => num(r.v) },
      ], Object.entries(stats.difficulties || {}).map(([k, v]) => ({ k, v })), { empty: '暂无难度数据' })))));

  /* 检索与列表 */
  const listHost = h('div');
  const pagerHost = h('div');

  async function loadList() {
    listHost.replaceChildren(skeleton(5));
    pagerHost.replaceChildren();
    try {
      const data = await domain('library/search', { limit: 300 });
      let rows = Array.isArray(data) ? data : (data.items || data.data || []);
      if (!Array.isArray(rows)) rows = [];

      /* 后端只做 limit，筛选在前端兜底，保证体感一致 */
      if (state.keyword) {
        const needle = state.keyword.toLowerCase();
        rows = rows.filter((r) => String(r.title || '').toLowerCase().includes(needle)
          || String(r.text_id || '').toLowerCase().includes(needle)
          || String(r.genre || '').toLowerCase().includes(needle));
      }
      if (state.genre) rows = rows.filter((r) => String(r.genre || '') === state.genre);
      if (state.difficulty) rows = rows.filter((r) => String(r.difficulty || '') === state.difficulty);

      const paged = paginate(rows, state.page, state.limit);
      listHost.replaceChildren(table([
        { label: '标题', wrap: true, render: (r) => h('span', { title: r.relative_path || '' },
          String(r.title || '（无标题）').slice(0, 90)) },
        { label: '分类', render: (r) => (r.category ? chip(r.category) : '—') },
        { label: '体裁', render: (r) => r.genre || '—' },
        { label: '难度', render: (r) => (r.difficulty ? chip(r.difficulty, DIFF_TONE[r.difficulty]) : '—') },
        { label: '字数', num: true, render: (r) => num(r.char_count) },
        { label: 'ID', mono: true, render: (r) => String(r.text_id || '').slice(0, 10) },
      ], paged.rows, { empty: '没有匹配的文章', emptyHint: '换个关键词或清空筛选试试' }));

      pagerHost.replaceChildren(pager(paged, (next) => {
        state.page = next;
        patchRoute({ page: next });
        loadList();
      }));
    } catch (err) {
      listHost.replaceChildren(errorState(err.message, loadList));
    }
  }

  wrap.append(section('文章检索', '按关键词、体裁与难度筛选',
    card(null,
      filters([
        { input: true, placeholder: '搜索标题 / ID…', value: state.keyword,
          onenter: (v) => { state.keyword = v; state.page = 1; patchRoute({ q: v, page: 1 }); loadList(); } },
        { select: [{ value: '', label: '全部体裁' }, ...genres.map((g) => ({ value: g, label: g }))],
          value: state.genre,
          onchange: (v) => { state.genre = v; state.page = 1; patchRoute({ genre: v, page: 1 }); loadList(); } },
        { select: [{ value: '', label: '全部难度' }, ...difficulties.map((d) => ({ value: d, label: d }))],
          value: state.difficulty,
          onchange: (v) => { state.difficulty = v; state.page = 1; patchRoute({ difficulty: v, page: 1 }); loadList(); } },
        { push: true },
        { button: '刷新', icon: 'refresh', onclick: loadList },
      ]),
      listHost,
      h('div', { style: 'padding:0 20px' }, pagerHost))));

  loadList();
  return wrap;
}

/* ── 成绩中心 ─────────────────────────────────────────────────────────── */
/* 数据来自 P4 新加的三个聚合接口（fm-domain 侧只读）：
     /scores/leaderboard   按人聚合：最佳/均速/击键/准确率/次数/段落数
     /scores/group-compare 按群聚合：人均速、人数、日均条数
     /scores/player        个人档案：总览 + 每日曲线 + 拿手段落 + 最近记录
   后端过滤规则（实测分布定的，见 fm-domain 的 score_quality_ok）：
     字数 < 8 的记录不参与速度排名；速度 > 400 或击键 > 20 视为异常。
   实测 30 天窗口：21420 条参与、388 条被过滤（1.8%）。 */
export async function renderScores(route) {
  const days = Number(route.query.days || 30);
  const tab = route.query.tab || 'people';
  const metric = route.query.metric || 'speed';
  const who = route.query.who || '';
  const seg = route.query.seg || '';
  const wrap = h('div');

  /* 刷新方式：改 hash 只影响筛选，视图自己收敛，不重建整页 */
  const reroute = (patch) => patchRoute(patch);
  /* 这两个是各自 tab 内部的临时状态，不进 URL：
     下拉一改就重新拉数据并局部重绘，不必整页刷新。 */
  let activeMetric = metric;
  let activeWho = who;

  /* ── 顶部指标：来自聚合接口自己的统计口径 ─────────────────────────── */
  const [dailyRaw, advRaw, boardRaw, groupRaw] = await Promise.all([
    soft(cached(`daily:${days}`, () => domain('stats/daily', { days }))),
    /* 只要热力图那部分。不传 part 的话后端还会算一遍速度分布
       （全表扫 39652 条成绩并逐条解析 JSON，实测占这个接口 525ms 里的 460ms），
       而成绩页已经不用它了。 */
    soft(cached('stats:heat', () => domain('stats/advanced', { part: 'heat' }))),
    soft(cached(`board:${days}`, () => domain('scores/leaderboard', { days, limit: 200, min_attempts: 3 }))),
    soft(cached(`gc:${days}`, () => domain('scores/group-compare', { days }))),
  ]);
  const daily = unwrap(dailyRaw) || {};
  const adv = unwrap(advRaw) || {};
  const board = unwrap(boardRaw) || {};
  const groupCmp = unwrap(groupRaw) || {};
  const groupRows = Array.isArray(groupCmp.groups) ? groupCmp.groups : [];
  const peopleRows = Array.isArray(board.rows) ? board.rows : [];

  wrap.append(cols(4,
    statCount('今日成绩', daily.today, {
      note: daily.today ? '仍有人在打' : '今天还没人打', tone: daily.today ? 'ok' : 'warn',
    }),
    statCount('上榜打手', peopleRows.length, { note: `至少 3 次成绩 · 近 ${days} 天` }),
    statCount('参与成绩', board.total_records, {
      note: board.dropped ? `过滤掉 ${num(board.dropped)} 条异常` : '全部通过质量检查',
    }),
    statCount('活跃群', groupRows.length, { note: '有成绩记录' })));

  /* ── 主体：四个视图切换 ───────────────────────────────────────────────
     状态只有 URL 一个来源。之前 `active` 变量和 URL 里的 tab 会打架：
     点「档案」时先改了变量又写 hash，两边不一致，结果还停在个人榜。 */
  const host = h('div');
  const TABS = [['people', '个人榜'], ['groups', '群对比'], ['segment', '段位记录'],
    ['duel', '打手 PK'], ['profile', '打手档案']];
  const tabs = h('div', { class: 'tabs', role: 'tablist', style: 'margin-bottom:16px' },
    TABS.map(([key, label]) =>
      h('button', {
        class: 'tab-btn', type: 'button', role: 'tab', 'data-tab': key,
        'aria-selected': String(tab === key),
        onclick: () => patchRoute({ tab: key }),
      }, label)));

  const windowSelect = () => select(
    [7, 14, 30, 90, 365].map((n) => ({ value: String(n), label: `最近 ${n} 天` })),
    String(days),
    (v) => reroute({ days: v }),
    { label: '时间窗口' },
  );

  /* ① 个人榜 */
  function drawPeople() {
    host.replaceChildren(skeleton(6));
    const metricSelect = select([
      { value: 'speed', label: '按最佳速度' },
      { value: 'avg_speed', label: '按平均速度' },
      { value: 'keystroke', label: '按最佳击键' },
      { value: 'accuracy', label: '按最佳准确率' },
      { value: 'volume', label: '按成绩条数' },
      { value: 'characters', label: '按累计字数' },
    ], activeMetric, (v) => {
      activeMetric = v;
      reroute({ metric: v });
      loadBoard();
    }, { label: '排名依据' });

    const listHost = h('div');
    const bar = filters([{ node: windowSelect() }, { node: metricSelect }, { push: true },
      { button: '刷新', icon: 'refresh', onclick: loadBoard }]);

    async function loadBoard() {
      listHost.replaceChildren(skeleton(6));
      try {
        const data = unwrap(await domain('scores/leaderboard', {
          days, metric: activeMetric, limit: 100, min_attempts: 3,
        })) || {};
        const rows = Array.isArray(data.rows) ? data.rows : [];
        const top = rows[0];
        listHost.replaceChildren(
          top ? h('p', { class: 'stat-note', style: 'padding:0 20px 12px' },
            `${num(data.people)} 人上榜（至少 3 次成绩）· 参与 ${num(data.total_records)} 条`
            + (data.dropped ? ` · 过滤异常 ${num(data.dropped)} 条` : '')) : null,
          table([
            { label: '名次', width: '64px', render: (r) => (r.rank <= 3
                ? chip(`#${r.rank}`, r.rank === 1 ? 'ok' : 'info')
                : h('span', { class: 'mono' }, `#${r.rank}`)) },
            { label: '打手', wrap: true, render: (r) => h('div', { style: 'line-height:1.35' },
                h('div', { style: 'font-weight:600' }, r.sender_name || r.sender_id || '—'),
                h('div', { style: 'font-size:var(--t-sm);color:var(--fg-subtle)' },
                  r.group_name || '')) },
            { label: '最佳速度', num: true, render: (r) => (r.best_speed == null
                ? '—' : h('b', { style: 'font-size:var(--t-md)' }, num(r.best_speed))) },
            { label: '平均速度', num: true, render: (r) => num(r.avg_speed) },
            { label: '最佳击键', num: true, render: (r) => num(r.best_keystroke) },
            { label: '最佳准确率', num: true, render: (r) => pct(r.best_accuracy) },
            { label: '成绩数', num: true, render: (r) => num(r.attempts) },
            { label: '段落数', num: true, render: (r) => num(r.segments) },
            { label: '累计字数', num: true, render: (r) => num(r.characters) },
            { label: '速度对比', width: '120px', render: (r) => meter(
                (Number(r.best_speed) || 0) / (Number(rows[0]?.best_speed) || 1),
                { bars: 14, tone: 'var(--c2)' }) },
            { label: '', render: (r) => h('button', {
                class: 'btn', type: 'button',
                onclick: () => patchRoute({
                  tab: 'profile', who: r.sender_id || r.sender_name || '',
                }),
              }, '档案') },
          ], rows, { empty: '这个窗口内没有足够成绩（每人至少 3 次才上榜）' }));
      } catch (err) {
        listHost.replaceChildren(errorState(err.message, loadBoard));
      }
    }

    host.replaceChildren(card(null, bar, listHost));
    loadBoard();
  }

  /* ② 群对比 */
  function drawGroups() {
    host.replaceChildren(skeleton(6));
    const data = groupCmp;
    const rows = Array.isArray(data.groups) ? data.groups : [];
    const topSpeed = Math.max(...rows.map((r) => Number(r.avg_speed) || 0), 1);
    const topVolume = Math.max(...rows.map((r) => Number(r.attempts) || 0), 1);

    host.replaceChildren(
      filters([{ node: windowSelect() }, { push: true },
        { button: '刷新', icon: 'refresh', onclick: () => { invalidate('gc:'); reroute({ days }); } }]),
      card(null, table([
        { label: '群', wrap: true, render: (r) => r.group_name || r.group_id || '—' },
        { label: '人均速度', num: true, render: (r) => h('b', {}, num(r.avg_speed)) },
        { label: '速度水平', width: '110px', render: (r) => meter(
            (Number(r.avg_speed) || 0) / topSpeed, { bars: 12, tone: 'var(--c2)' }) },
        { label: '最高速', num: true, render: (r) => num(r.best_speed) },
        { label: '纪录保持者', wrap: true, render: (r) => r.best_who || '—' },
        { label: '打手数', num: true, render: (r) => num(r.people) },
        { label: '成绩数', num: true, render: (r) => num(r.attempts) },
        { label: '日均条数', num: true, render: (r) => num(r.per_day) },
        { label: '活跃度', width: '110px', render: (r) => meter(
            (Number(r.attempts) || 0) / topVolume, { bars: 12, tone: 'var(--c1)' }) },
        { label: '人均条数', num: true, render: (r) => num(r.per_person) },
        { label: '平均击键', num: true, render: (r) => num(r.avg_keystroke) },
        { label: '平均准确率', num: true, render: (r) => pct(r.avg_accuracy) },
      ], rows, { empty: '这个窗口内没有群数据' })));
  }

  /* ③ 段位记录：按段号查所有挑战记录 */
  function drawSegment() {
    host.replaceChildren(skeleton(4));
    const input = h('input', {
      class: 'in', placeholder: '段号，或打手名字 / 群名…', value: seg, style: 'min-width:280px',
    });
    const listHost = h('div', {}, blank('输入段号或关键词', '会按成绩原始内容全文匹配'));

    async function search() {
      const q = input.value.trim();
      reroute({ seg: q });
      if (!q) {
        listHost.replaceChildren(blank('输入段号或关键词', '会按成绩原始内容全文匹配'));
        return;
      }
      listHost.replaceChildren(skeleton(5));
      try {
        const data = await domain('scores', { q, limit: 200 });
        const rows = Array.isArray(data) ? data : (data.items || []);
        listHost.replaceChildren(table([
          { label: '段号', mono: true, render: (r) => String(r.segment_id || '—') },
          { label: '速度', num: true, render: (r) => num(r.speed) },
          { label: '击键', num: true, render: (r) => num(r.keystroke ?? r.keystrokes) },
          { label: '准确率', num: true, render: (r) => pct(r.accuracy) },
          { label: '字数', num: true, render: (r) => num(r.characters) },
          { label: '打手', wrap: true, render: (r) => r.sender_name || r.sender_id || '—' },
          { label: '群', wrap: true, render: (r) => r.group_name || r.group_id || '—' },
          { label: '来源', render: (r) => chip(r.source || '—') },
          { label: '时间', mono: true, render: (r) => clock(r.received_at, { seconds: false }) },
        ], rows, { empty: `没有匹配「${q}」的成绩` }));
      } catch (err) {
        listHost.replaceChildren(errorState(err.message, search));
      }
    }
    input.addEventListener('keydown', (e) => { if (e.key === 'Enter') search(); });

    host.replaceChildren(card(null,
      filters([{ node: input }, { button: '查询', primary: true, onclick: search }]),
      listHost));
    if (seg) search();
  }

  /* ④ 打手档案 */
  function drawProfile() {
    host.replaceChildren(skeleton(4));
    const input = h('input', {
      class: 'in', placeholder: '打手名字或 QQ 号…', value: activeWho, style: 'min-width:240px',
    });
    const body = h('div', { style: 'margin-top:16px' });
    let currentDays = days;

    async function load() {
      const q = input.value.trim();
      activeWho = q;
      reroute({ who: q });
      if (!q) { body.replaceChildren(blank('输入打手名字或 QQ 号')); return; }
      body.replaceChildren(skeleton(6));
      try {
        const isId = /^\d+$/.test(q);
        const data = unwrap(await domain('scores/player', {
          days: currentDays, ...(isId ? { user_id: q } : { name: q }),
        })) || {};
        if (!data.found) {
          body.replaceChildren(blank(`近 ${currentDays} 天没有「${q}」的成绩`,
            '可能是名字不完全一致，或者这段时间没打'));
          return;
        }
        const daily = Array.isArray(data.daily) ? data.daily : [];
        /* 精度按字段给：速度保留 2 位、击键 2 位、准确率 1 位。
           全都传 0 的话 399.02 会显示成 399，实测踩到过。 */
        const cards = cols(4,
          statCount('最佳速度', data.best_speed, {
            note: `平均 ${num(data.avg_speed)}`, tone: 'ok', decimals: 2,
          }),
          statCount('最高击键', data.best_keystroke, {
            note: `平均 ${num(data.avg_keystroke)}`, decimals: 2,
          }),
          statCount('最佳准确率', data.best_accuracy, {
            note: `平均 ${pct(data.avg_accuracy)}`, decimals: 1, suffix: '%',
          }),
          statCount('成绩条数', data.attempts, {
            note: `活跃 ${num(data.active_days)} 天 · 首打 ${num(data.new_segments)} 段`,
          }));
        body.replaceChildren(cards,
          section('成长曲线', `近 ${currentDays} 天每日最佳速度与平均速度`,
            card(null, daily.length ? await chart(timeSeriesOption({
              labels: daily.map((d) => d.date),
              series: [
                { name: '每日最佳', data: daily.map((d) => d.best_speed) },
                { name: '每日平均', data: daily.map((d) => d.avg_speed), area: true },
              ],
            })) : blank('这段时间没有每日数据'))),
          section('拿手段落', '按该打手的最好成绩排序',
            card(null, table([
              { label: '段号', mono: true, render: (r) => r.segment_id },
              { label: '最佳速度', num: true, render: (r) => num(r.best_speed) },
              { label: '击键', num: true, render: (r) => num(r.best_keystroke) },
              { label: '准确率', num: true, render: (r) => pct(r.best_accuracy) },
              { label: '试了几次', num: true, render: (r) => num(r.attempts) },
            ], data.segments, { empty: '没有可统计的段落' }))),
          section('最近成绩', `最新 ${(data.recent || []).length} 条`,
            card(null, table([
              { label: '时间', mono: true, render: (r) => clock(r.occurred_at, { seconds: false }) },
              { label: '段号', mono: true, render: (r) => String(r.segment_id || '—') },
              { label: '速度', num: true, render: (r) => num(r.speed) },
              { label: '击键', num: true, render: (r) => num(r.keystroke) },
              { label: '准确率', num: true, render: (r) => pct(r.accuracy) },
              { label: '字数', num: true, render: (r) => num(r.characters) },
              { label: '群', wrap: true, render: (r) => r.group_name || '—' },
              { label: '来源', render: (r) => chip(r.source || '—') },
            ], data.recent, { empty: '没有记录' }))));
        /* statCount 先渲染 0，靠 countUpIn 启动滚动。
           app.js 只在整页渲染后调一次，这里是视图挂载**之后**才换的内容，
           不自己再调一次就会停在 0（实测踩到过）。 */
        countUpIn(body);
      } catch (err) {
        body.replaceChildren(errorState(err.message, load));
      }
    }
    input.addEventListener('keydown', (e) => { if (e.key === 'Enter') load(); });

    host.replaceChildren(card(null,
      filters([
        { node: input },
        { button: '查询', primary: true, onclick: load },
        { node: select([7, 14, 30, 90, 365].map((n) => ({ value: String(n), label: `最近 ${n} 天` })),
            String(currentDays), (v) => { currentDays = Number(v); load(); }, { label: '时间窗口' }) },
      ])),
      body);
    if (activeWho) load();
    else body.replaceChildren(blank('输入打手名字或 QQ 号', '也可以从「个人榜」点「档案」直接跳过来'));
  }

  /* ⑤ 打手 PK：两个人放在一起比
     接口 /scores/duel 返回逐项并排 + 共同段落对决 + 每日走势。
     共同段落那部分最有意思：同一段文章两人都打过，直接比最好成绩。 */
  function drawDuel() {
    host.replaceChildren(skeleton(4));
    const left = h('input', {
      class: 'in', placeholder: '打手 A：名字或 QQ 号', value: route.query.a || '',
      style: 'min-width:200px',
    });
    const right = h('input', {
      class: 'in', placeholder: '打手 B：名字或 QQ 号', value: route.query.b || '',
      style: 'min-width:200px',
    });
    const body = h('div', { style: 'margin-top:16px' });
    let span = days;

    async function load() {
      const a = left.value.trim();
      const b = right.value.trim();
      reroute({ a, b });
      if (!a || !b) {
        body.replaceChildren(blank('填两个打手就能开始比', '名字或 QQ 号都行，从「个人榜」看到名字直接抄过来'));
        return;
      }
      body.replaceChildren(skeleton(6));
      try {
        const data = unwrap(await domain('scores/duel', { a, b, days: span })) || {};
        if (!data.found) {
          body.replaceChildren(blank('没找到这两个打手',
            `近 ${span} 天缺少：${(data.missing || []).join('、')}`));
          return;
        }
        const A = data.a || {};
        const B = data.b || {};
        const duel = data.duel || {};
        const trend = Array.isArray(data.trend) ? data.trend : [];

        /* 逐项并排：每行一项，谁更好高亮 */
        const METRICS = [
          ['最佳速度', 'best_speed', 'high', (v) => num(v)],
          ['平均速度', 'avg_speed', 'high', (v) => num(v)],
          ['中位速度', 'median_speed', 'high', (v) => num(v)],
          ['P90 速度', 'p90_speed', 'high', (v) => num(v)],
          ['最佳击键', 'best_keystroke', 'high', (v) => num(v)],
          ['平均击键', 'avg_keystroke', 'high', (v) => num(v)],
          ['最佳准确率', 'best_accuracy', 'high', (v) => pct(v)],
          ['平均准确率', 'avg_accuracy', 'high', (v) => pct(v)],
          ['成绩条数', 'attempts', 'high', (v) => num(v)],
          ['活跃天数', 'active_days', 'high', (v) => num(v)],
          ['段落数', 'segments', 'high', (v) => num(v)],
          ['累计字数', 'characters', 'high', (v) => num(v)],
        ];
        let scoreA = 0;
        let scoreB = 0;
        const rows = METRICS.map(([label, key, dir, fmt]) => {
          const av = Number(A[key]);
          const bv = Number(B[key]);
          const aBetter = Number.isFinite(av) && Number.isFinite(bv) && av > bv;
          const bBetter = Number.isFinite(av) && Number.isFinite(bv) && bv > av;
          if (aBetter) scoreA += 1;
          if (bBetter) scoreB += 1;
          const cell = (v, better) => h('td', {
            class: 'num',
            style: better ? 'color:var(--c4);font-weight:750' : null,
          }, fmt(v));
          return h('tr', {},
            cell(A[key], aBetter),
            h('td', { style: 'text-align:center;color:var(--fg-subtle);font-size:var(--t-sm)' }, label),
            cell(B[key], bBetter));
        });

        const shared = Number(duel.shared_segments) || 0;
        body.replaceChildren(
          cols(3,
            h('div', { class: 'card', 'data-k': scoreA > scoreB ? 'ok' : null },
              h('div', { class: 'stat' },
                h('span', { class: 'stat-key' }, `A · ${A.sender_name || a}`),
                h('strong', { class: 'stat-val' }, `${scoreA}`),
                h('span', { class: 'stat-note' }, `${METRICS.length} 项里领先的项数`))),
            h('div', { class: 'card', 'data-pad': '' },
              h('div', { class: 'stat', style: 'padding:0' },
                h('span', { class: 'stat-key' }, '共同段落对决'),
                h('strong', { class: 'stat-val' },
                  `${num(duel.a_wins)} : ${num(duel.b_wins)}`),
                h('span', { class: 'stat-note' },
                  shared ? `共 ${num(shared)} 个段落 · 平 ${num(duel.ties)}` : '两人没有打过同一段'))),
            h('div', { class: 'card', 'data-k': scoreB > scoreA ? 'ok' : null },
              h('div', { class: 'stat' },
                h('span', { class: 'stat-key' }, `B · ${B.sender_name || b}`),
                h('strong', { class: 'stat-val' }, `${scoreB}`),
                h('span', { class: 'stat-note' }, `${METRICS.length} 项里领先的项数`)))),

          section('逐项对比', `近 ${span} 天，绿色的那一侧更好`,
            card(null, h('table', { class: 'tbl' },
              h('thead', {}, h('tr', {},
                h('th', { style: 'text-align:right' }, A.sender_name || 'A'),
                h('th', { style: 'text-align:center;width:120px' }, '指标'),
                h('th', { style: 'text-align:right' }, B.sender_name || 'B'))),
              h('tbody', {}, rows)))),

          shared ? section('共同段落对决', '同一段文章两人都打过，比各自的最好成绩',
            card(null, table([
              { label: '段号', mono: true, render: (r) => r.segment_id },
              { label: `${A.sender_name || 'A'} 最好`, num: true, render: (r) => num(r.a_speed) },
              { label: `${B.sender_name || 'B'} 最好`, num: true, render: (r) => num(r.b_speed) },
              { label: '差', num: true, render: (r) => h('span', {
                  style: `color:${r.diff > 0 ? 'var(--c4)' : 'var(--bad)'};font-weight:700`,
                }, `${r.diff > 0 ? '+' : ''}${num(r.diff)}`) },
              { label: '谁快', render: (r) => chip(
                  r.winner === 'a' ? (A.sender_name || 'A')
                    : r.winner === 'b' ? (B.sender_name || 'B') : '持平',
                  r.winner === 'tie' ? undefined : 'ok') },
            ], duel.rows, { empty: '没有共同段落' }))) : null,

          trend.length ? section('每日走势', '两人每天的均速，缺的那天说明没打',
            card(null, await chart(timeSeriesOption({
              labels: trend.map((t) => t.date),
              series: [
                { name: A.sender_name || 'A', data: trend.map((t) => t.a) },
                { name: B.sender_name || 'B', data: trend.map((t) => t.b) },
              ],
            })))) : null);
      } catch (err) {
        body.replaceChildren(errorState(err.message, load));
      }
    }
    left.addEventListener('keydown', (e) => { if (e.key === 'Enter') load(); });
    right.addEventListener('keydown', (e) => { if (e.key === 'Enter') load(); });

    host.replaceChildren(card(null,
      filters([
        { node: left },
        h('span', { style: 'color:var(--fg-subtle);font-weight:600' }, 'VS'),
        { node: right },
        { button: '开始对比', primary: true, onclick: load },
        { node: select([7, 14, 30, 90, 365].map((n) => ({ value: String(n), label: `最近 ${n} 天` })),
            String(span), (v) => { span = Number(v); load(); }, { label: '时间窗口' }) },
        { push: true },
      ]),
      body));
    if (left.value.trim() && right.value.trim()) load();
    else body.replaceChildren(blank('填两个打手就能开始比', '名字或 QQ 号都行'));
  }

  function draw() {
    [...tabs.children].forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === tab)));
    if (tab === 'people') drawPeople();
    else if (tab === 'groups') drawGroups();
    else if (tab === 'segment') drawSegment();
    else if (tab === 'duel') drawDuel();
    else drawProfile();
  }

  wrap.append(section('排行与对比', '数据来自成绩记录聚合；字数过少或速度异常的记录已排除',
    tabs, host));
  draw();

  /* 切换 tab 只改 hash，事件到了之后按 URL 重新画一遍。
     注意要去重：hashchange 也会因为 days/metric 变化触发，那种情况不该整块重建。 */
  let lastTab = tab;
  const onHash = () => {
    const next = parseRoute().query.tab || 'people';
    if (next === lastTab) return;
    lastTab = next;
    [...tabs.children].forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === next)));
    if (next === 'people') drawPeople();
    else if (next === 'groups') drawGroups();
    else if (next === 'segment') drawSegment();
    else if (next === 'duel') drawDuel();
    else drawProfile();
  };
  window.addEventListener('hashchange', onHash);
  wrap.addEventListener('view:leave', () => window.removeEventListener('hashchange', onHash));

  return wrap;
}
