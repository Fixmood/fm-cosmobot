/* ============================================================================
   系统视图：仪表盘 / 会话管理 / 媒体资源 / 任务与并发 / 审计日志 / 设置
   实测接口与字段：
     /api/overview              {ok,domain{ok,data},stats{ok,data:{groups,
                                 library_texts,contest_texts,score_records,…}},
                                 archive{ok,data:{status,sources{},active_library_sessions}},
                                 state{personas,groups,models,triggers}}
     /api/health/score          {ok,score,issues:[{issue,suggestion}],
                                 dimensions:{service,errors,rpc,config,processing}}
     /api/errors/stats          {ok,data:{items:[{component,error,actor,at,path}],
                                 by_component:[{name,count}],trend:[{date,count}]}}
     /api/models/stats          {ok,data:{models:[{name,calls,tokens,elapsed,average_ms}],
                                 distribution,trend}}
     /api/observability/overview{ok,config_sync,rpc,domain,domain_stats,tasks{entries},
                                 recent_errors[],audit,state}
     /api/logs                  {ok,items:[{action,collection,item_id,actor,at}],total}
     /api/runtime/audit         {ok,data:[{event,id,occurredAt}],total}
     RPC chat.list_sessions     -> {ok,data:{sessions:[{sessionId,label,
                                     parentSessionId,parentMessageId}]}}
     RPC chat.history           -> {ok,data:{messages:[{role,content}]}}
     RPC media.stats            -> {ok,data:{files:[…],stats:{files,existingFiles,
                                     missingFiles,totalBytes,…}}}
     RPC resource.list          -> {ok,data:{resources:[…]}}
     RPC concurrency.list       -> {ok,data:{entries:[{id,label,startedAt,finishedAt,
                                     status,error}]}}
     RPC config.snapshot        -> {ok,data:{models,image_models,group_default,…}}
     /api/auth/tokens           {ok,items:[{id,name,created_at,created_by,expires_at,
                                     enabled,revoked_at}]}
     /api/runtime/config/versions {ok,items:[{id,at,actor,kind,operation,changes[]}]}
   ========================================================================= */

import {
  h, num, pct, bytes, ago, clock, duration, elapsed, table, card, stat, statCount,
  section, chip, blank, skeleton, errorState, alert, api, rpc, rpcData, domain,
  cached, soft, invalidate, filters, chart, timeSeriesOption, toneFor, toast,
  confirmAction, navigate, cols, unwrap, yesno, groupName, stack,
  ring, sparkline, meter, heatmap, activityBars,
  field, select, textarea, input, actions, taskButton,
} from './core.js';

/* ── 仪表盘 ───────────────────────────────────────────────────────────── */
/* 「媒体资源 / 任务与并发 / 审计日志」三个页面已撤掉：
   媒体缓存实际只有几个文件，页面报的数字来自不可靠的统计；
   任务列表是机器人内部并发槽位，已完成记录一大堆，看它没有任何产出；
   审计日志是原始事件流，没人会去翻。
   其中真正有价值的信号（机器人现在在忙什么、能否取消卡住的任务、存储趋势）
   并到这张仪表盘里，见下面的「机器人状态」与「近期事件」。 */
export async function renderDashboard() {
  const [overviewRaw, health, errorsRaw, modelsRaw, obsRaw, storageRaw, concRaw, advRaw] = await Promise.all([
    soft(cached('overview', () => api('/api/overview'))),
    cached('health', () => api('/api/health/score')).catch(() => null),
    soft(cached('errors', () => api('/api/errors/stats'))),
    soft(cached('models', () => api('/api/models/stats'))),
    soft(cached('observability', () => api('/api/observability/overview'))),
    soft(cached('archive:trend', () => domain('stats/daily', { days: 30 }))),
    soft(cached('concurrency', () => rpcData('concurrency.list'))),
    soft(cached('stats:advanced', () => domain('stats/advanced'))),
  ]);

  const overview = overviewRaw || {};
  const stats = unwrap(overview.stats) || {};
  const archive = unwrap(overview.archive) || {};
  const errData = unwrap(errorsRaw) || {};
  const modData = unwrap(modelsRaw) || {};
  const obs = obsRaw || {};
  const daily = unwrap(storageRaw) || {};
  const adv = unwrap(advRaw) || {};
  const taskEntries = Array.isArray(obs.tasks?.entries) ? obs.tasks.entries : [];
  const taskRows = Array.isArray(concRaw?.entries) ? concRaw.entries : taskEntries;

  const errRows = Array.isArray(errData.items) ? errData.items : [];
  const modelRows = Array.isArray(modData.models) ? modData.models : [];
  const issues = Array.isArray(health?.issues) ? health.issues : [];
  const dims = health?.dimensions && typeof health.dimensions === 'object'
    ? Object.entries(health.dimensions) : [];
  /* 任务计数交给后端：它已经按标签把常驻服务与底层噪声分开了。
     前端再算一遍只会两处不一致。 */
  const taskStats = obs.tasks || {};
  const running = Number(taskStats.running ?? 0);
  const stuckCount = Number(taskStats.stuck ?? 0);
  const doneRecent = Number(taskStats.done_recent ?? 0);
  const failedRecent = Number(taskStats.failed_recent ?? 0);
  /* state 现在来自机器人配置快照与 domain 统计，不再是中控本地集合 */
  const state = obs.state || overview.state || {};

  const wrap = h('div');

  /* 配置不一致告警：只有真出问题才出现 */
  const sync = obs.config_sync || {};
  if (sync.ok === false) {
    wrap.append(alert('bad', `配置同步失败：${sync.reason || '原因未知'}`));
  } else if (sync.saved && sync.applied === false) {
    wrap.append(alert('warn',
      `后台保存的配置与机器人当前配置不一致：${sync.reason || '可能尚未应用或已被其他来源修改'}`));
  }

  wrap.append(cols(4,
    stat('领域服务', overview._err ? '异常' : '正常', overview._err || 'domain 已连接',
      overview._err ? 'bad' : 'ok'),
    stat('健康评分', health?.score != null ? Math.round(health.score) : '—',
      issues.length ? `${issues.length} 项待查` : '无待查项', issues.length ? 'warn' : 'ok'),
    statCount('运行中任务', running, {
      note: running ? '仍在执行' : '空闲', tone: running ? 'warn' : 'ok',
    }),
    statCount('活跃发文', archive.active_library_sessions, {
      note: '进行中的会话',
      tone: archive.active_library_sessions ? 'ok' : undefined,
    })));

  /* 健康维度：后端给的是「得分」，每项都有自己的满分（加起来正好 100），
     所以要比着满分看，而不是当成百分制。这里用进度环画，比纯数字直观。 */
  const DIM_MAX = { service: 30, errors: 25, rpc: 20, config: 15, processing: 10 };
  const DIM_NAME = {
    service: '领域服务', errors: '错误控制', rpc: '机器人 RPC',
    config: '配置同步', processing: '任务处理',
  };
  if (dims.length) {
    wrap.append(section('健康维度', '每个环是该维度拿到的百分比，凑满 100 分即为健康',
      cols('auto', ...dims.map(([name, v]) => {
        const score = Number(typeof v === 'object' ? (v.score ?? v.value) : v);
        const max = DIM_MAX[name];
        const pctValue = max && Number.isFinite(score) ? (score / max) * 100 : 100;
        const tone = pctValue >= 100 ? 'ok' : pctValue >= 60 ? 'warn' : 'bad';
        return h('div', { class: 'card', 'data-pad': '', 'data-k': tone },
          h('div', { style: 'display:flex;align-items:center;gap:16px' },
            ring(pctValue, {
              size: 78, thickness: 7,
              tone: tone === 'ok' ? 'var(--c4)' : tone === 'warn' ? 'var(--c5)' : 'var(--bad)',
            }),
            h('div', { class: 'stat', style: 'padding:0;gap:3px' },
              h('span', { class: 'stat-key' }, DIM_NAME[name] || name),
              h('strong', { class: 'stat-val', style: 'font-size:var(--t-xl)' },
                max ? `${num(score)} / ${max}` : num(score)),
              h('span', { class: 'stat-note' },
                h('span', { class: 'pill', 'data-kind': tone }, pctValue >= 100 ? '满分' : '有扣分')))));
      }))));
  }

  /* 待查问题 */
  if (issues.length) {
    wrap.append(section('待查问题', '按健康检查的建议处理',
      card(null, table([
        { label: '问题', wrap: true, render: (r) => r.issue || '—' },
        { label: '建议', wrap: true, render: (r) => r.suggestion || '—' },
      ], issues))));
  }

  /* 规模概览 */
  const sizeRows = [
    { k: '群 / 房间', v: num(state.groups ?? stats.groups) },
    { k: '跟打文章', v: num(stats.library_texts) },
    { k: '赛文文章', v: num(stats.contest_texts) },
    { k: 'AI 赛文', v: num(stats.ai_contest_texts) },
    { k: '成绩记录', v: num(stats.score_records) },
    { k: '赛事成绩', v: num(stats.competition_scores) },
    { k: '撤回记录', v: num(stats.recall_records) },
    { k: '消息归档', v: num(stats.message_archive) },
    { k: '人格', v: num(state.personas) },
    { k: '模型', v: num(state.models) },
    { k: '触发器', v: num(state.triggers) },
  ].filter((r) => r.v !== '—');

  wrap.append(section('规模概览', '各业务表的记录数',
    cols(2,
      card('数据规模', table([
        { label: '项', key: 'k' },
        { label: '数量', num: true, render: (r) => r.v },
      ], sizeRows, { empty: '暂无统计数据' })),
      card('模型调用', table([
        { label: '模型', wrap: true, render: (r) => r.name || r.model || r.provider || '—' },
        { label: '调用', num: true, render: (r) => num(r.calls ?? r.count) },
        { label: 'Token', num: true, render: (r) => num(r.tokens) },
        { label: '平均耗时', num: true, render: (r) => (r.average_ms != null ? duration(r.average_ms) : '—') },
      ], modelRows, { empty: '暂无调用统计' })))));

  /* ── 机器人状态 ───────────────────────────────────────────────────────
     计数全部来自后端的 classify_task_entries()：它按标签把常驻服务
     （main.inner、rpc.server、qq.connection…）与底层噪声（command stdout…）
     分出去，只对真正在处理消息的活统计 running / stuck / 近 24 小时完成与失败。
     前端不再自己数一遍，免得两处口径不一致。 */
  const busy = taskRows.filter((e) => !e.finishedAt);

  /* 任务标签是机器人内部的黑话，翻成人话，不然没人看得懂在干什么 */
  const TASK_NAME = {
    'agent.typing': '模拟打字（发表情/分段回复）',
    'agent.reply': '生成回复',
    'agent.think': '思考中',
    'ask.fresh': '新鲜度追问',
    'ask.continue': '继续追问',
    'ask.followup': '追问下一句',
    'repeater': '复读跟随',
    'repeat-follow': '复读跟随',
    'recall': '处理撤回',
    'library.session': '跟打发文会话',
    'library.next': '发下一篇',
    'contest.auto': '自动赛文',
    'ai-contest.run': 'AI 赛文',
    'competition.score': '赛事成绩入库',
    'score.record': '成绩入库',
    'media.fetch': '下载图片',
    'media.gc': '清理媒体缓存',
    'image.generate': '生成图片',
    'qq.reconnect': '重连 QQ',
    'group.request': '处理入群申请',
    'notice.push': '推送通知',
  };
  const taskName = (label) => {
    const raw = String(label || '—');
    if (TASK_NAME[raw]) return TASK_NAME[raw];
    const hit = Object.keys(TASK_NAME).find((k) => raw.toLowerCase().startsWith(k));
    return hit ? TASK_NAME[hit] : raw;
  };

  wrap.append(section('机器人状态',
    `常驻服务 ${num(taskStats.services)} 条已排除，另外 ${num(taskStats.noise)} 条底层过程日志不计入`,
    cols(4,
      statCount('正在处理', running, {
        note: running ? '有活在手' : '空闲', tone: running ? 'warn' : 'ok',
      }),
      statCount('疑似卡住', stuckCount, {
        note: stuckCount ? '超过 5 分钟未结束' : '无', tone: stuckCount ? 'bad' : 'ok',
      }),
      statCount('近 24 小时完成', doneRecent, {
        note: `累计 ${num(taskStats.total)} 条记录`,
      }),
      statCount('近 24 小时失败', failedRecent, {
        note: failedRecent ? '需要关注' : '无', tone: failedRecent ? 'bad' : 'ok',
      })),
    busy.length ? card(null, table([
      { label: '任务', wrap: true, render: (r) => taskName(r.label) },
      { label: '开始', mono: true, render: (r) => clock(r.startedAt, { seconds: false }) },
      { label: '开始于', render: (r) => ago(r.startedAt) },
      { label: '状态', render: (r) => {
          const started = Date.parse(r.startedAt || '') || 0;
          return (started && Date.now() - started > 5 * 60 * 1000)
            ? chip('卡住', 'bad') : chip('运行中', 'warn');
        } },
      { label: '', render: (r) => h('button', {
          class: 'btn', 'data-kind': 'danger',
          onclick: async () => {
            if (!await confirmAction({
              title: '取消这个任务？', detail: r.label || String(r.id), confirmLabel: '取消任务',
            })) return;
            try {
              await rpc('concurrency.cancel', { id: r.id });
              toast('已请求取消', 'ok');
              invalidate('concurrency');
              invalidate('observability');
            } catch (err) { toast(err.message, 'bad'); }
          },
        }, '取消') },
    ], busy.slice(0, 12))) : null));

  /* ── 存储与增长：原「媒体与资源」的真实替身 ──────────────────────────
     媒体缓存只有几个文件，真正会涨的是消息归档与成绩表。
     这里的「活跃」列不是累计值，是最近 30 天每天的写入量 —— 一眼看出还在不在涨。 */
  const trend = Array.isArray(daily.days) ? daily.days.map((d) => Number(d.count) || 0) : [];
  const trendTail = trend.slice(-30);
  const growthRows = [
    { k: '消息归档', v: Number(stats.message_archive) || 0, live: true },
    { k: '成绩记录', v: Number(stats.score_records) || 0, live: true },
    { k: '赛事成绩', v: Number(stats.competition_scores) || 0, live: true },
    { k: '跟打文章', v: Number(stats.library_texts) || 0 },
    { k: '赛文文章', v: Number(stats.contest_texts) || 0 },
    { k: '撤回记录', v: Number(stats.recall_records) || 0, live: true },
  ].sort((a, b) => b.v - a.v);
  const totalRows = growthRows.reduce((sum, r) => sum + r.v, 0);
  const newest = archive.sources && typeof archive.sources === 'object'
    ? Object.entries(archive.sources)
        .map(([k, v]) => ({ k, at: v?.latest_at }))
        .filter((r) => r.at)
        .sort((a, b) => (b.at || 0) - (a.at || 0)) : [];

  /* 最近 30 天的写入量：折线 + 电平条 */
  const today = trend.length ? trend[trend.length - 1] : 0;
  const weekAvg = trendTail.length
    ? Math.round(trendTail.slice(-7).reduce((a, b) => a + b, 0) / Math.min(7, trendTail.length))
    : 0;

  wrap.append(section('存储与增长', '数据只会往上涨，这里看谁在涨、还涨不涨',
    cols('auto',
      statCount('今日写入', today, { note: `近 7 日均值 ${num(weekAvg)}`, tone: today ? 'ok' : 'warn' }),
      statCount('30 天累计', trendTail.reduce((a, b) => a + b, 0), { note: '全部来源合计' }),
      statCount('表内总量', totalRows, { note: `${growthRows.length} 张表` })),
    cols(2,
      card('记录数排行', table([
        { label: '数据表', mono: true, render: (r) => r.k },
        { label: '记录数', num: true, render: (r) => num(r.v) },
        { label: '占比', num: true, render: (r) => pct(totalRows ? (r.v / totalRows) * 100 : 0) },
        { label: '增速', width: '104px', render: (r) => (r.live && trendTail.length >= 2
            ? sparkline(trendTail, { w: 96, h: 24, tone: 'var(--c2)' })
            : h('span', { class: 'stat-note' }, '静态')) },
        { label: '量级', width: '120px', render: (r) => meter(r.v / (growthRows[0].v || 1), {
            bars: 14, tone: 'var(--c2)',
          }) },
      ], growthRows, { empty: '暂无统计' })),
      card('各来源最新一条', table([
        { label: '来源', mono: true, render: (r) => r.k },
        { label: '最新写入', mono: true, render: (r) => clock(r.at, { seconds: false }) },
        { label: '距今', render: (r) => ago(r.at) },
        { label: '新鲜度', width: '92px', render: (r) => {
            const mins = Math.max(0, (Date.now() - (Number(r.at) * 1000 || 0)) / 60000);
            const fresh = mins < 30 ? 1 : mins < 240 ? 0.6 : mins < 1440 ? 0.3 : 0.08;
            return meter(fresh, {
              bars: 10,
              tone: fresh > 0.5 ? 'var(--c4)' : fresh > 0.2 ? 'var(--c5)' : 'var(--bad)',
            });
          } },
      ], newest, { empty: '暂无归档信息' })))));

  /* ── 活跃热力图：星期 × 小时，用 /stats/advanced 的 heatmap ─────────── */
  const heat = Array.isArray(adv.heatmap) ? adv.heatmap : [];
  if (heat.length) {
    const total = heat.reduce((sum, c) => sum + (Number(c.count) || 0), 0);
    const peak = heat.reduce((best, c) => ((Number(c.count) || 0) > (Number(best?.count) || 0) ? c : best), null);
    wrap.append(section('活跃热力图', peak
      ? `合计 ${num(total)} 次 · 最活跃是周${['日', '一', '二', '三', '四', '五', '六'][Number(peak.day)] || '?'} ${String(Number(peak.hour)).padStart(2, '0')}:00（${num(peak.count)} 次）`
      : null,
    card(null, heatmap(heat))));
  }

  /* ── 近期事件：原「审计日志」的可读版本 ───────────────────────────────
     只列最近 24 小时真正处理过的活（后端已经滤掉常驻服务与底层噪声），
     压成一行看得懂的摘要。 */
  const recentWork = taskRows
    .filter((e) => (Date.parse(e.startedAt || '') || 0) >= Date.now() - 24 * 3600 * 1000)
    .sort((a, b) => (Date.parse(b.startedAt) || 0) - (Date.parse(a.startedAt) || 0));
  if (recentWork.length) {
    wrap.append(section('近期事件',
      taskStats.noise
        ? `机器人最近处理过的事情（另有 ${num(taskStats.noise)} 条底层过程日志未计入）`
        : '机器人最近处理过的事情',
      card(null, table([
        { label: '时间', mono: true, render: (r) => clock(r.startedAt, { seconds: false }) },
        { label: '事件', wrap: true, render: (r) => h('div', { style: 'line-height:1.4' },
            h('div', { style: 'font-weight:600' }, taskName(r.label)),
            r.label && taskName(r.label) !== r.label
              ? h('div', { style: 'font-size:var(--t-sm);color:var(--fg-subtle);font-family:var(--font-mono)' }, r.label)
              : null) },
        { label: '耗时', num: true, render: (r) => elapsed(r.startedAt, r.finishedAt) },
        { label: '结果', render: (r) => (r.error ? chip('失败', 'bad')
            : (r.finishedAt ? chip('完成', 'ok') : chip('进行中', 'warn'))) },
      ], recentWork.slice(0, 15), { empty: '最近 24 小时没有处理记录' }))));
  }

  /* 错误趋势 */
  const errTrend = Array.isArray(errData.trend) ? errData.trend : [];
  if (errTrend.length) {
    wrap.append(section('错误趋势', '按天统计',
      card(null, await chart(timeSeriesOption({
        labels: errTrend.map((r) => r.date),
        series: [{ name: '错误数', data: errTrend.map((r) => r.count), area: true }],
      })))));
  }

  /* 最近错误 */
  if (errRows.length) {
    const byComp = Array.isArray(errData.by_component) ? errData.by_component : [];
    wrap.append(section('最近错误', `共 ${errRows.length} 条，按组件分布：`
      + byComp.map((c) => `${c.name} ${c.count}`).join(' · '),
    card(null, table([
      { label: '时间', mono: true, render: (r) => clock(r.at || r.time) },
      { label: '组件', render: (r) => chip(r.component || '—', 'bad') },
      { label: '来源', render: (r) => r.actor || '—' },
      { label: '内容', wrap: true, render: (r) => String(r.error || r.message || '').slice(0, 200) },
    ], errRows.slice(0, 20)))));
  }

  return wrap;
}

/* ── 会话管理 ─────────────────────────────────────────────────────────── */
export async function renderSessions(route) {
  const wrap = h('div');
  const selected = route.arg || '';
  const listHost = h('div');
  const detailHost = h('div', {}, blank('选择一个会话', '点击左侧任意一行查看历史与回复'));

  async function loadSessions() {
    listHost.replaceChildren(skeleton(5));
    try {
      const data = unwrap(await rpc('chat.list_sessions')) || {};
      const sessions = Array.isArray(data.sessions) ? data.sessions : [];
      listHost.replaceChildren(table([
        { label: '会话', wrap: true, render: (r) => h('button', {
            class: 'btn', 'data-kind': 'ghost', style: 'height:auto;padding:2px 8px',
            onclick: () => navigate(`sessions/${encodeURIComponent(r.sessionId)}`),
          }, String(r.label || r.sessionId).slice(0, 30)) },
        { label: '会话 ID', mono: true, render: (r) => String(r.sessionId || '').slice(0, 18) },
        { label: '父会话', mono: true, render: (r) => String(r.parentSessionId || '—').slice(0, 14) },
      ], sessions, { empty: '当前没有会话' }));
      if (selected) loadHistory(decodeURIComponent(selected));
    } catch (err) {
      listHost.replaceChildren(errorState(err.message, loadSessions));
    }
  }

  async function loadHistory(sessionId) {
    detailHost.replaceChildren(skeleton(6));
    try {
      const data = unwrap(await rpc('chat.history', { sessionId })) || {};
      const messages = Array.isArray(data.messages) ? data.messages : [];

      const input = h('input', { class: 'in', style: 'flex:1', placeholder: '向该会话发送一条消息…' });
      const send = async () => {
        const text = input.value.trim();
        if (!text) return;
        input.value = '';
        try {
          const res = unwrap(await rpc('chat.send', { sessionId, text })) || {};
          const reply = res.reply ?? res.answer ?? '';
          toast(reply ? `已回复：${String(reply).slice(0, 80)}` : '已发送', 'ok');
          loadHistory(sessionId);
        } catch (err) { toast(err.message, 'bad'); }
      };
      input.addEventListener('keydown', (e) => { if (e.key === 'Enter') send(); });

      detailHost.replaceChildren(
        h('div', {},
          table([
            { label: '角色', render: (r) => chip(r.role || '—', r.role === 'user' ? 'info' : 'ok') },
            { label: '内容', wrap: true, render: (r) => String(r.content || '').slice(0, 400) || '（空）' },
          ], messages.slice(-60), { empty: '该会话暂无消息' }),
          filters([{ node: input }, { button: '发送', primary: true, onclick: send }])));
    } catch (err) {
      detailHost.replaceChildren(errorState(err.message, () => loadHistory(sessionId)));
    }
  }

  wrap.append(section('会话', '来自机器人 RPC chat.*',
    cols('split',
      card('会话列表', listHost,
        filters([{ push: true }, { button: '刷新', icon: 'refresh', onclick: loadSessions }])),
      card(selected ? `历史 · ${decodeURIComponent(selected).slice(0, 16)}` : '会话详情', detailHost))));

  await loadSessions();
  return wrap;
}

/* ── 设置页：写配置的统一入口 ───────────────────────────────────────────
   重要：**必须走 `/api/runtime/config/{kind}`，不能走 `/api/rpc`。**
   实测发现 `/api/rpc` 是直接转发给机器人的，不经过中控的
   `runtime_config_write()`，所以那条路**不会产生配置版本快照** ——
   改完既没有审计记录，也没法回滚。
   走 `/api/runtime/config/{kind}` 时中控会：写前存快照 → 转发 → 写后算 diff
   → 存成一个 config 版本，返回值里带 version_id。
   端点用 PUT（也支持 POST），body 就是机器人侧的 params。 */
async function writeConfig(kind, params) {
  return api(`/api/runtime/config/${kind}`, { method: 'PUT', body: params });
}

/* 写完配置后让 app.js 重绘整页。
   不这么做的话，「配置版本」表显示的还是写入之前的列表 ——
   paint() 的那批请求是在视图渲染前发的，写入发生在那之后。 */
function refreshPage() {
  window.dispatchEvent(new CustomEvent('fm:reload-view'));
}

/* ── 设置页：人格编辑器 ─────────────────────────────────────────────────
   机器人侧 config.persona 的契约（实测）：
     action = status|get|set|clear，scope = private_default|group_default
                                          |private|group|member
     private / group / member 三种 scope 必须带 id（QQ 号或群号）
     status 返回 { scope, content, saved, applied }
     set 需要 { action:'set', scope, id?, content }
   注意 config.persona 没有 list 动作（实测 502），所以只能按 scope 逐个读。 */
const PERSONA_SCOPES = [
  { key: 'private_default', label: '私聊默认', id: null,
    hint: '所有私聊里没单独设置时用它' },
  { key: 'group_default', label: '群聊默认', id: null,
    hint: '所有群里没单独设置时用它' },
];

function personaCard(scopeKey, labelChip, idValue) {
  const params = { scope: scopeKey };
  if (idValue) params.id = String(idValue);

  const body = h('div', {}, skeleton(3));
  const card = h('div', { class: 'card' },
    h('div', { class: 'card-head' },
      h('h3', {}, scopeKey === 'private_default' ? '私聊默认人格' : '群聊默认人格'),
      labelChip ? chip(labelChip) : null),
    h('div', { class: 'card-body' }, body));

  async function load() {
    body.replaceChildren(skeleton(3));
    try {
      const data = unwrap(await rpc('config.persona', { action: 'status', ...params })) || {};
      const content = String(data.content || '');
      const box = textarea(content, { rows: 10, maxlength: '1000' });
      const counter = h('span', { class: 'stat-note' }, `${content.length} / 1000 字`);

      box.addEventListener('input', () => {
        counter.textContent = `${box.value.length} / 1000 字`;
        counter.style.color = box.value.length > 1000 ? 'var(--bad)' : '';
      });

      body.replaceChildren(
        field('人格正文', box, '最多 1000 字。机器人会把这整段作为系统提示的一部分。'),
        actions(
          taskButton('保存', async () => {
            const next = box.value;
            if (!next.trim()) throw new Error('正文不能为空，要清空请用「清除」');
            if (next.length > 1000) throw new Error(`正文 ${next.length} 字，超过 1000 字上限`);
            const out = await writeConfig('persona', { action: 'set', ...params, content: next });
            const res = unwrap(out) || {};
            invalidate('versions');
            toast(out?.version_id
              ? `已保存并生效（版本 ${out.version_id}）`
              : '已保存并生效',
            res.applied === false ? 'warn' : 'ok');
            /* 重绘整页：版本表是在写入之前取的，不刷会显示旧列表 */
            refreshPage();
          }, { kind: 'solid' }),
          taskButton('清除', async () => {
            if (!await confirmAction({
              title: `清除「${scopeKey}」的人格？`,
              detail: '清除后该场景会回落到其它来源。此操作会记入配置版本，可撤销。',
              confirmLabel: '清除',
            })) return;
            await writeConfig('persona', { action: 'clear', ...params });
            invalidate('versions');
            toast('已清除', 'ok');
            load();
          }, { kind: 'danger' }),
          taskButton('重新读取', load, { icon: 'refresh' }),
          counter),
        h('p', { class: 'stat-note', style: 'margin-top:8px' },
          data.applied === false
            ? `机器人报告未生效：${data.reason || '可能已被其它来源修改'}`
            : '当前与机器人一致'),
      );
    } catch (err) {
      body.replaceChildren(errorState(err.message, load));
    }
  }

  load();
  return card;
}

/* ── 设置页：触发词编辑器 ───────────────────────────────────────────────
   机器人侧 config.trigger 的契约（实测 + 读源码 Bot/Trigger.hs:139-151）：
     action = list|get|status|set|clear，scope 必填
     set 需要 modes（至少一个）与 keywords
     可用的 modes：prefix/前缀/fm空格、mention/@、reply/回复、name/名字/叫名
     status 返回 { scope, config:{modes,keywords} | null, saved, applied } */
const TRIGGER_MODES = [
  { value: 'prefix', label: '前缀（fm 空格）' },
  { value: 'mention', label: '@ 机器人' },
  { value: 'reply', label: '回复机器人' },
  { value: 'name', label: '叫名字' },
];

function triggerCard(scopeKey, scopeLabel, idValue) {
  const params = { scope: scopeKey };
  if (idValue) params.id = String(idValue);

  const body = h('div', {}, skeleton(3));
  const card = h('div', { class: 'card' },
    h('div', { class: 'card-head' },
      h('h3', {}, `触发方式 · ${scopeLabel}`),
      h('div', { class: 'right' }, chip('scope: ' + scopeKey, 'info'))),
    h('div', { class: 'card-body' }, body));

  async function load() {
    body.replaceChildren(skeleton(3));
    try {
      const data = unwrap(await rpc('config.trigger', { action: 'status', ...params })) || {};
      const config = data.config || null;
      const activeModes = new Set((config?.modes || []).map((m) => String(m).toLowerCase()));
      const keywords = Array.isArray(config?.keywords) ? config.keywords.join(' ') : '';

      const boxes = TRIGGER_MODES.map((m) => {
        const box = h('input', {
          type: 'checkbox', checked: activeModes.has(m.value),
        });
        box.addEventListener('change', update);
        return h('label', {
          style: 'display:inline-flex;align-items:center;gap:6px;margin-right:14px;'
            + 'font-size:var(--t-base);cursor:pointer',
        }, box, m.label, h('span', { style: 'display:none' }, m.value));
      });

      const keywordInput = input(keywords, { placeholder: '额外触发词，空格分隔' });

      function picked() {
        return boxes.filter((l) => l.querySelector('input').checked)
          .map((l) => l.querySelector('span').textContent);
      }
      function update() {
        const n = picked().length;
        note.textContent = n ? `已选 ${n} 种触发方式` : '至少选一种，否则无法保存';
        note.style.color = n ? '' : 'var(--bad)';
      }
      const note = h('span', { class: 'stat-note' }, '');
      update();

      body.replaceChildren(
        field('触发方式', h('div', { style: 'margin-top:4px;display:flex;flex-wrap:wrap' }, ...boxes),
          '勾选哪些行为会让机器人响应。'),
        field('额外触发词', keywordInput, '可选。机器人看到这些词也会响应，空格分隔。'),
        actions(
          taskButton('保存', async () => {
            const modes = picked();
            if (!modes.length) throw new Error('至少要选一种触发方式');
            const words = keywordInput.value.split(/\s+/).map((s) => s.trim()).filter(Boolean);
            const out = await writeConfig('trigger', {
              action: 'set', ...params, modes, keywords: words,
            });
            const res = unwrap(out) || {};
            invalidate('versions');
            toast(out?.version_id
              ? `已保存并生效（版本 ${out.version_id}）`
              : '已保存并生效',
            res.applied === false ? 'warn' : 'ok');
            refreshPage();
          }, { kind: 'solid' }),
          taskButton('清除', async () => {
            if (!await confirmAction({
              title: `清除「${scopeLabel}」的触发词配置？`,
              detail: '清除后这里不再限制触发方式。此操作会记入配置版本，可撤销。',
              confirmLabel: '清除',
            })) return;
            await writeConfig('trigger', { action: 'clear', ...params });
            invalidate('versions');
            toast('已清除', 'ok');
            load();
          }, { kind: 'danger' }),
          taskButton('重新读取', load, { icon: 'refresh' }),
          note),
        h('p', { class: 'stat-note', style: 'margin-top:8px' },
          config
            ? `当前已配置：${(config.modes || []).join(' / ') || '（无模式）'}`
              + (config.keywords?.length ? `，触发词 ${config.keywords.length} 个` : '')
            : '当前未配置（机器人用默认的响应规则）'),
      );
    } catch (err) {
      body.replaceChildren(errorState(err.message, load));
    }
  }

  load();
  return card;
}

/* ── 设置页：版本历史与回滚 ─────────────────────────────────────────────
   接口 `GET /api/runtime/config/versions` 返回的是**最新在前**的顺序
   （state 文件里是追加，最旧在前，接口给反了过来）。
   这里不假设顺序，自己按时间倒序排一遍，免得以后后端改顺序就错位。

   注意语义：`rollback_payload()` 恢复的是版本快照里的 `before`，
   所以「这个版本」旁边的按钮实际是「撤销这次改动」。按钮文案按这个写。 */
function versionSection(versionRows) {
  const listHost = h('div');
  const detailHost = h('div');

  const rows = [...versionRows].sort((a, b) => (Number(b.at) || 0) - (Number(a.at) || 0));

  function draw() {
    listHost.replaceChildren(table([
      { label: '时间', mono: true, render: (r) => clock(r.at) },
      { label: '执行者', render: (r) => r.actor || '—' },
      { label: '类型', render: (r) => chip(r.kind || '—') },
      { label: '操作', render: (r) => (r.operation === 'rollback'
          ? chip('回滚', 'warn') : (r.operation || '—')) },
      { label: '改了哪些字段', wrap: true, render: (r) => (Array.isArray(r.changes) && r.changes.length
          ? r.changes.map((c) => h('code', { class: 'mono', style: 'margin-right:8px' }, c.path))
          : '—') },
      { label: '', render: (r) => h('button', {
          class: 'btn', type: 'button',
          onclick: async (e) => {
            const btn = e.currentTarget;
            btn.disabled = true;
            detailHost.replaceChildren(skeleton(4));
            try {
              const res = await api(`/api/runtime/config/versions/${encodeURIComponent(r.id)}/diff`);
              const changes = Array.isArray(res.changes) ? res.changes : [];
              detailHost.replaceChildren(
                h('div', { class: 'card-head', style: 'padding-left:0;border-bottom:0' },
                  h('h3', {}, `${clock(r.at)} 这次改动`),
                  h('div', { class: 'right' },
                    taskButton('撤销这次改动', async () => {
                      if (!await confirmAction({
                        title: '撤销这次改动？',
                        detail: `会把配置恢复到 ${clock(r.at)} 这次改动**之前**的样子`
                          + `（${r.kind} / ${(r.changes || []).map((c) => c.path).join('、') || '未知字段'}）。`
                          + '撤销本身也会记一个新版本，可以再撤销回来。',
                        confirmLabel: '撤销',
                      })) return;
                      const out = await api('/api/runtime/config/rollback', {
                        method: 'POST', body: { version_id: r.id },
                      });
                      invalidate('versions');
                      toast(out?.ok === false ? '撤销返回失败' : '已撤销', out?.ok === false ? 'bad' : 'ok');
                      location.reload();
                    }, { kind: 'danger' }))),
                table([
                  { label: '字段', mono: true, render: (c) => c.path },
                  { label: '改前', wrap: true, render: (c) => truncateCell(c.before) },
                  { label: '改后', wrap: true, render: (c) => truncateCell(c.after) },
                ], changes, { empty: '这个版本没有记录字段级差异' }));
            } catch (err) {
              detailHost.replaceChildren(errorState(err.message));
            } finally {
              btn.disabled = false;
            }
          },
        }, '看差异') },
    ], rows, { empty: '暂无版本记录（改一次配置就会出现）' }));
  }

  const truncateCell = (value) => {
    const text = typeof value === 'string' ? value : JSON.stringify(value);
    const shown = String(text ?? '').replace(/\s+/g, ' ').trim();
    if (!shown) return h('span', { class: 'stat-note' }, '（空）');
    return h('span', { title: text, style: 'font-size:var(--t-sm)' },
      shown.length > 120 ? `${shown.slice(0, 120)}…` : shown);
  };

  draw();
  /* data- 标记：给自动化测试一个稳定抓手。页面别处还有别的表格
     （当前账号、访问令牌），按文本定位容易点错行。 */
  return h('div', { 'data-version-list': '' },
    listHost, h('div', { style: 'margin-top:16px' }, detailHost));
}

/* ── 设置 ─────────────────────────────────────────────────────────────── */
export async function renderSettings() {
  const [me, tokensRaw, versionsRaw, configRaw] = await Promise.all([
    api('/api/auth/me').catch(() => ({})),
    soft(api('/api/auth/tokens')),
    soft(api('/api/runtime/config/versions')),
    soft(rpcData('config.snapshot')),
  ]);

  const tokenRows = Array.isArray(tokensRaw?.items) ? tokensRaw.items : [];
  const versionRows = Array.isArray(versionsRaw?.items) ? versionsRaw.items : [];

  const current = h('input', { class: 'in', type: 'password', placeholder: '当前密码', autocomplete: 'current-password' });
  const next = h('input', { class: 'in', type: 'password', placeholder: '新密码（至少 8 位）', autocomplete: 'new-password' });

  /* 模型配置 */
  const modelRows = [
    ...(Array.isArray(configRaw?.models) ? configRaw.models.map((m) => ({ ...m, kind: '对话' })) : []),
    ...(Array.isArray(configRaw?.image_models) ? configRaw.image_models.map((m) => ({ ...m, kind: '绘图' })) : []),
  ];

  /* 人格：后端返回的是正文，不是 ID，所以要截断展示 */
  const snippet = (text, n = 60) => {
    const s = String(text || '').replace(/\s+/g, ' ').trim();
    if (!s) return '—';
    return s.length > n ? `${s.slice(0, n)}…` : s;
  };
  const personaTable = (list) => table([
    { label: 'ID', mono: true, render: (r) => r.id || '—' },
    { label: '正文摘要', wrap: true, render: (r) => h('span', { title: String(r.content || '') }, snippet(r.content, 80)) },
    { label: '字数', num: true, render: (r) => num(String(r.content || '').length) },
  ], Array.isArray(list) ? list : [], { empty: '没有配置' });

  return h('div', {},
    cols(2,
      card('当前账号', table([
        { label: '用户名', key: 'username' },
        { label: '管理员', render: (r) => (r.is_admin ? chip('是', 'ok') : chip('否', 'warn')) },
      ], [me], { empty: '未登录' })),
      card('默认人格', table([
        { label: '场景', key: 'k' },
        { label: '正文摘要', wrap: true, render: (r) => h('span', { title: r.full || '' }, snippet(r.v, 70)) },
      ], [
        { k: '群聊默认', v: configRaw?.group_default, full: configRaw?.group_default },
        { k: '私聊默认', v: configRaw?.private_default, full: configRaw?.private_default },
      ]))),

    section('模型', '当前生效的模型与回退',
      card(null, table([
        { label: '用途', render: (r) => chip(r.kind) },
        { label: '提供方', mono: true, render: (r) => r.provider || '—' },
        { label: '模型', mono: true, render: (r) => r.model || '—' },
        { label: '当前', render: (r) => (r.current ? chip('使用中', 'ok') : '—') },
        { label: '默认', render: (r) => (r.configured_default ? chip('是', 'info') : '—') },
        { label: '密钥', render: (r) => (r.api_key_configured == null ? '—'
            : chip(r.api_key_configured ? '已配置' : '缺失', r.api_key_configured ? 'ok' : 'bad')) },
        { label: '可生成', render: (r) => (r.can_generate == null ? '—' : yesno(r.can_generate, '是', '否')) },
        { label: '回退', render: (r) => (r.fallback ? chip('可用', 'info') : '—') },
      ], modelRows, { empty: '暂无模型配置' }))),

    section('人格与风格', '这里改完直接作用于机器人，每次改动都会记入配置版本，可回滚',
      cols(2, ...PERSONA_SCOPES.map((s) => personaCard(s.key, s.label, s.id))),
      card('场景人格（按群 / 按人）',
        h('p', { class: 'stat-note', style: 'margin-bottom:12px' },
          '这些需要在机器人侧创建对应记录后才会出现。机器人配置快照里当前有：'
          + `群人格 ${(configRaw?.group_personas || []).length} 条、`
          + `私聊人格 ${(configRaw?.private_personas || []).length} 条、`
          + `成员风格 ${(configRaw?.member_styles || []).length} 条。`),
        cols(3,
          card('群人格', personaTable(configRaw?.group_personas)),
          card('私聊人格', personaTable(configRaw?.private_personas)),
          card('成员风格', personaTable(configRaw?.member_styles))))),

    section('响应触发方式', '控制机器人在什么情况下会回话（实测机器人支持的四种模式）',
      cols(2,
        triggerCard('private_default', '私聊默认'),
        triggerCard('group_default', '群聊默认'))),

    section('修改密码', '改完需要重新登录',
      card(null,
        cols(2,
          h('div', { class: 'field' }, h('label', {}, '当前密码'), current),
          h('div', { class: 'field' }, h('label', {}, '新密码'), next)),
        h('div', { style: 'margin-top:16px' },
          h('button', {
            class: 'btn', 'data-kind': 'solid',
            onclick: async (e) => {
              if (!current.value || !next.value) return toast('请填写当前密码与新密码', 'warn');
              const btn = e.currentTarget;
              btn.disabled = true;
              try {
                await api('/api/auth/password', {
                  method: 'POST', body: { current: current.value, new: next.value },
                });
                toast('密码已修改，请重新登录', 'ok');
                setTimeout(() => location.reload(), 1200);
              } catch (err) {
                toast(err.message, 'bad');
                btn.disabled = false;
              }
            },
          }, '修改密码')))),

    section('访问令牌', '供脚本或自动化使用，避免保存密码',
      card(null, table([
        { label: '名称', wrap: true, render: (r) => r.name || '—' },
        { label: '创建者', render: (r) => r.created_by || '—' },
        { label: '创建', render: (r) => ago(r.created_at) },
        { label: '过期', render: (r) => (r.expires_at ? clock(r.expires_at) : '长期有效') },
        { label: '状态', render: (r) => (r.revoked_at || r.enabled === false
            ? chip('已停用', 'warn') : chip('有效', 'ok')) },
        { label: '摘要', mono: true, render: (r) => String(r.value || '').slice(0, 12) },
      ], tokenRows, { empty: '当前没有令牌' }))),

    section('配置版本', '每次改动都会存一份快照，可以看差异也可以回滚',
      card(null, versionSection(versionRows))));
}
