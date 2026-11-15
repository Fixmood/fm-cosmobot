/* ══════════════════════════════════════════════════════════════════════════
 * pages-v4.js · FM Console v4 新页面
 *  - 仪表盘（v4 重做：Hero + KPI + 趋势 + 工具环 + 24×7 热力 + Top + 健康时间轴）
 *  - 分析：消息分析 / 群分析 / 成员活跃 / 响应延迟（/api/v2/analytics/*）
 *  - 内容中心：文库 / 赛文 / 番茄 / 打字排行 统一搜索 + 详情
 *  - 运维监控 v4（stats / images / logs / cron / rollback 卡片化）
 * 数据源优先 v4 接口，不可用时自动回退到 v2 旧接口并明确标注，绝不白屏。
 * ════════════════════════════════════════════════════════════════════════ */
(function () {
  'use strict';

  /* ── 通用小件 ─────────────────────────────────────────────────────────── */

  function deltaChip(now, prev, unit) {
    const a = Number(now);
    const b = Number(prev);
    if (!isFinite(a) || !isFinite(b) || b === 0) return '';
    const pct = ((a - b) / b) * 100;
    const dir = pct > 0.5 ? 'up' : pct < -0.5 ? 'down' : 'flat';
    const arrow = dir === 'up' ? icon('arrow-up') : dir === 'down' ? icon('arrow-down') : '';
    const label = (Math.abs(pct) >= 10 ? Math.abs(pct).toFixed(0) : Math.abs(pct).toFixed(1)) + '%';
    return '<span class="kpi-delta ' + dir + '">' + arrow + label + (unit ? ' ' + esc(unit) : '') + '</span>';
  }

  function miniStat(label, value, hint, iconName, tone) {
    return '<div class="ops-stat' + (tone ? ' tone-' + tone : '') + '">' +
      '<div class="k">' + (iconName ? icon(iconName) : '') + esc(label) + '</div>' +
      '<div class="v ' + (tone ? 'tone-' + tone : '') + '">' + value + '</div>' +
      (hint ? '<div class="h">' + hint + '</div>' : '') + '</div>';
  }

  function chartCard(id, title, sub, actions, bodyHtml) {
    return '<section class="card">' +
      '<div class="card-head"><h3>' + esc(title) + (sub ? '<span class="section-sub">' + esc(sub) + '</span>' : '') + '</h3>' +
      '<div class="card-head-actions">' + (actions || '') +
      '<button class="icon-btn sm" type="button" data-chart-export="#' + escAttr(id) + '" data-chart-name="' + escAttr(title) + '" title="导出 PNG">' + icon('download') + '</button>' +
      '</div></div>' +
      '<div class="card-body"><div id="' + escAttr(id) + '" class="chart"></div>' + (bodyHtml || '') + '</div></section>';
  }

  function daysSelector(current, options) {
    const list = options || [7, 14, 30];
    return '<select class="select select-sm" id="v4-days" aria-label="时间范围" style="width:auto">' +
      list.map((d) => '<option value="' + d + '"' + (Number(current) === d ? ' selected' : '') + '>近 ' + d + ' 天</option>').join('') +
      '</select>';
  }

  function bindDaysSelector(ctx, viewId, segments) {
    const select = document.getElementById('v4-days');
    if (!select) return;
    select.addEventListener('change', () => {
      const params = new URLSearchParams(ctx.query ? ctx.query.toString() : '');
      params.set('days', select.value);
      go(hashFor(viewId, segments || [], params));
    });
  }

  /* 24×7 热力图 */
  function heatmapHtml(hourly, opts) {
    const options = opts || {};
    if (!hourly || !Array.isArray(hourly.total) || !hourly.total.length) {
      return placeholderCard('热力图数据还没准备好', '接口尚未返回 hourly 矩阵。', '/api/v2/analytics/messages');
    }
    const weekdays = hourly.weekdays || ['周一', '周二', '周三', '周四', '周五', '周六', '周日'];
    const hours = hourly.hours || Array.from({ length: 24 }, (_, i) => i);
    const rows = hourly.total;
    const flat = [];
    rows.forEach((row) => row.forEach((value) => { if (Number(value) > 0) flat.push(Number(value)); }));
    flat.sort((a, b) => a - b);
    let max = flat.length ? flat[flat.length - 1] : 0;
    const quantile = (ratio) => (flat.length ? flat[Math.min(flat.length - 1, Math.floor(flat.length * ratio))] : 0);
    const t1 = quantile(0.2);
    const t2 = quantile(0.45);
    const t3 = quantile(0.7);
    const t4 = quantile(0.88);
    const level = (value) => {
      const v = Number(value) || 0;
      if (!max || v === 0) return 0;
      if (v >= t4) return 4;
      if (v >= t3) return 3;
      if (v >= t2) return 2;
      if (v >= t1) return 1;
      return 1;
    };
    const grid = [];
    grid.push('<div class="heatmap-corner"></div>');
    hours.forEach((hour) => { grid.push('<div class="heatmap-hour">' + (hour % 2 === 0 ? hour : '') + '</div>'); });
    rows.forEach((row, rowIndex) => {
      grid.push('<div class="heatmap-row-label">' + esc(weekdays[rowIndex] || 'D' + rowIndex) + '</div>');
      row.forEach((value, hourIndex) => {
        const count = Number(value) || 0;
        grid.push('<div class="heat-cell" data-l="' + level(count) + '" title="' + esc(weekdays[rowIndex] || '') + ' ' + String(hourIndex).padStart(2, '0') + ':00 · ' + fmtNum(count) + ' 条"></div>');
      });
    });
    return '<div class="heatmap"><div class="heatmap-grid">' + grid.join('') + '</div>' +
      '<div class="heatmap-legend"><span>少</span>' +
      '<i style="background:var(--heat-0)"></i><i style="background:var(--heat-1)"></i><i style="background:var(--heat-2)"></i><i style="background:var(--heat-3)"></i><i style="background:var(--heat-4)"></i>' +
      '<span>多</span><span class="mono" style="margin-left:6px">峰值 ' + fmtNum(max) + ' 条/小时</span></div></div>' +
      (options.footNote ? '<div class="muted-2 text-xs mt-8">' + options.footNote + '</div>' : '');
  }

  function placeholderCard(title, hint, path) {
    return '<div class="placeholder-card"><b>' + icon('clock') + ' ' + esc(title) + '</b><p>' + esc(hint) + '</p>' +
      (path ? '<p class="mono">' + esc(path) + '</p>' : '') + '</div>';
  }

  /* 缓存徽标：后端 ops 接口会返回 cached / cache_age_seconds / cache_ttl（P1-2） */
  function cacheAgeText(data) {
    if (!data) return '实时';
    const age = Number(data.cache_age_seconds);
    const ttl = Number(data.cache_ttl);
    const ageText = isFinite(age) && age >= 0 ? ' · ' + age.toFixed(1) + 's 前' : '';
    /* 后端语义：warming=首次采集；stale=缓存过期先返旧值、后台刷新；cached=缓存命中；否则实时 */
    if (data.warming === true) return '采集中（后台刷新中）';
    if (data.stale === true) {
      let text = '缓存（后台刷新中）' + ageText;
      if (isFinite(ttl) && ttl > 0) text += ' / TTL ' + ttl + 's';
      return text;
    }
    if (data.cached === true) {
      let text = '缓存命中' + ageText;
      if (isFinite(ttl) && ttl > 0) text += ' / TTL ' + ttl + 's';
      return text;
    }
    return '实时';
  }

  function cacheBadgeHtml(res, data) {
    if (!res || !res.ok) return '<span class="tag tag-warn">' + icon('x-circle') + '读取失败</span>';
    const warming = !!(data && data.warming === true);
    const stale = !!(data && data.stale === true);
    const cached = !!(data && data.cached === true);
    const tone = warming ? 'tag-warn' : (stale || cached) ? 'tag-info' : 'tag-ok';
    const iconName = warming ? 'loader' : stale ? 'refresh' : cached ? 'database' : 'activity';
    return '<span class="tag ' + tone + '">' + icon(iconName) + esc(cacheAgeText(data)) + '</span>';
  }

  /* 运行中进度条：慢接口等待期间显示（不提前判定失败） */
  function runProgressHtml(id, label, hint) {
    return '<div class="run-progress" id="' + escAttr(id) + '">' +
      '<div class="rp-head"><span class="rp-title mono">' + icon('loader', 'spin') + ' ' + esc(label) + '</span>' +
      '<span class="rp-time mono" data-elapsed>0s</span></div>' +
      '<div class="progress-track"><div class="progress-fill indeterminate"></div></div>' +
      (hint ? '<div class="rp-hint muted-2 text-xs">' + esc(hint) + '</div>' : '') +
      '</div>';
  }

  function startElapsedTimer(scope, id) {
    const root = scope && scope.querySelector ? scope.querySelector('#' + id) : null;
    const node = root ? root.querySelector('[data-elapsed]') : null;
    const startedAt = Date.now();
    if (!node) return function noop() {};
    const timer = setInterval(function () {
      if (node.isConnected) node.textContent = Math.round((Date.now() - startedAt) / 1000) + 's';
    }, 500);
    return function stop() { clearInterval(timer); };
  }

  function timelineHtml(points) {
    if (!points || !points.length) {
      return '<div class="empty" style="padding:22px"><div class="empty-ico">' + icon('activity') + '</div><b>还没有健康检查点</b>' +
        '<p>运维代理每 5 分钟写入一次 /data/health.jsonl，稍后回来即可看到时间轴。</p></div>';
    }
    return '<div class="timeline">' + points.map((point) => {
      const containers = point.containers || [];
      const bad = containers.filter((c) => !c.running || (c.health && c.health !== 'healthy'));
      const tone = bad.length ? 'bad' : 'ok';
      return '<div class="timeline-item" data-tone="' + tone + '">' +
        '<div class="timeline-head"><span class="timeline-title">' + esc(point.at_text || fmtTime(point.at)) + '</span>' +
        '<span class="timeline-time">负载 ' + esc(point.load1 == null ? '—' : point.load1) + ' · 内存 ' + esc(point.mem_used_percent == null ? '—' : point.mem_used_percent + '%') + ' · 磁盘 ' + esc(point.disk_percent == null ? '—' : point.disk_percent + '%') + '</span>' +
        '<span class="tag ' + (bad.length ? 'tag-bad' : 'tag-ok') + '">' + (bad.length ? bad.length + ' 个异常' : '全部健康') + '</span></div>' +
        '<div class="timeline-desc">' + containers.map((c) => esc(c.name) + ' <span class="mono">' + esc(c.health || c.state || '') + '</span>').join(' · ') + '</div>' +
        '</div>';
    }).join('') + '</div>';
  }

  function errorTrendHtml(trend) {
    if (!trend || !trend.length) return emptyState({ icon: 'check-circle', title: '没有错误记录', hint: '最近 7 天 /api/errors/stats 没有返回错误趋势。' });
    const max = Math.max.apply(null, trend.map((row) => Number(row.count) || 0)) || 1;
    return '<div class="rank-list">' + trend.map((row) => {
      const count = Number(row.count) || 0;
      return '<div class="rank-item" style="cursor:default">' +
        '<span class="rank-main"><span class="rank-title">' + esc(row.date) + '</span>' +
        '<span class="rank-bar"><i style="width:' + Math.max(2, Math.round(count / max * 100)) + '%;background:' + (count ? 'linear-gradient(90deg,#fb7185,#f97316)' : 'var(--line-3)') + '"></i></span></span>' +
        '<span class="rank-value">' + fmtNum(count) + '</span></div>';
    }).join('') + '</div>';
  }

  function guard(ctx, id, selector) {
    if (!ctx.alive()) return false;
    const root = ctx.root;
    const node = root && root.querySelector(selector || '.hero, .card, .empty, .error-box, .table-wrap, .kpi');
    if (!node) {
      console.warn('[v4] page guard: no real element', id);
      return false;
    }
    return true;
  }

  /* ══ 1. 仪表盘 ══════════════════════════════════════════════════════════ */

  VIEWS.dashboard = {
    id: 'dashboard',
    name: '仪表盘',
    icon: 'dash',
    skeleton: 'chart',
    render: async (ctx) => {
      const hour = new Date().getHours();
      const greeting = hour < 6 ? '夜深了' : hour < 11 ? '早上好' : hour < 14 ? '中午好' : hour < 18 ? '下午好' : '晚上好';
      const now = new Date();
      const dateText = now.getFullYear() + ' 年 ' + (now.getMonth() + 1) + ' 月 ' + now.getDate() + ' 日 · ' + ['周日', '周一', '周二', '周三', '周四', '周五', '周六'][now.getDay()];

      ctx.root.innerHTML =
        '<section class="hero"><div class="hero-inner">' +
          '<div>' +
            '<div class="hero-meta" style="margin:0 0 10px"><span class="tag tag-acc">' + icon('sparkles') + 'FM Console v4</span>' +
            '<span class="tag"><span class="hero-pulse"></span>实时运营总览</span>' +
            '<span class="tag tag-muted mono">' + esc(dateText) + '</span></div>' +
            '<div class="hero-title">' + esc(greeting) + '，这里是 <em>FM 控制台</em></div>' +
            '<div class="hero-sub">消息趋势、工具调用、24×7 活跃热力、Top 群与成员、健康时间轴 —— 一屏掌握机器人今天的运营状态。</div>' +
            '<div class="hero-meta">' +
              '<button class="btn btn-sm btn-soft" id="dash-search" type="button">' + icon('search') + '全局搜索 <kbd class="kbd">Ctrl</kbd><kbd class="kbd">K</kbd></button>' +
              '<button class="btn btn-sm btn-ghost" id="dash-refresh" type="button">' + icon('refresh') + '刷新数据</button>' +
              '<a class="btn btn-sm btn-ghost" href="#/analytics-msg">' + icon('analytics') + '深入分析</a>' +
              '<button class="btn btn-sm btn-ghost" id="dash-theme" type="button">' + icon('palette') + '切换主题</button>' +
            '</div>' +
          '</div>' +
          '<div class="hero-side"><div class="hero-clock"><b id="dash-clock">' + pad(now.getHours()) + ':' + pad(now.getMinutes()) + '</b><span id="dash-env">连接状态检查中…</span></div></div>' +
        '</div></section>' +
        '<div id="dash-kpis"><div class="sk-row"><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div></div></div>' +
        '<div class="grid grid-wide" style="margin-bottom:14px">' +
          '<section class="card"><div class="card-head"><h3>' + icon('analytics') + '消息趋势 <span class="section-sub">近 14 天 · 人类 / 机器人</span></h3>' +
            '<div class="card-head-actions"><a class="btn btn-sm btn-ghost" href="#/analytics-msg">' + icon('arrow-right') + '分析</a>' +
            '<button class="icon-btn sm" type="button" data-chart-export="#dash-trend" data-chart-name="消息趋势" title="导出 PNG">' + icon('download') + '</button></div></div>' +
            '<div class="card-body"><div id="dash-trend" class="chart chart-lg"></div></div></section>' +
          '<section class="card"><div class="card-head"><h3>' + icon('wrench') + '工具调用 <span class="section-sub">近 7 天</span></h3>' +
            '<div class="card-head-actions"><button class="icon-btn sm" type="button" data-chart-export="#dash-tools" data-chart-name="工具调用" title="导出 PNG">' + icon('download') + '</button></div></div>' +
            '<div class="card-body"><div id="dash-tools" class="chart chart-xs"></div><div id="dash-tools-legend" class="donut-legend mt-8"></div></div></section>' +
        '</div>' +
        '<section class="card" style="margin-bottom:14px"><div class="card-head"><h3>' + icon('calendar') + '24 × 7 活跃热力图 <span class="section-sub">按星期 × 小时的群消息密度</span></h3>' +
          '<div class="card-head-actions"><a class="btn btn-sm btn-ghost" href="#/analytics-msg">' + icon('arrow-right') + '明细</a></div></div>' +
          '<div class="card-body"><div id="dash-heat"><div class="load-inline">' + icon('loader', 'spin') + ' 正在计算热力矩阵…</div></div></div></section>' +
        '<div class="grid grid-2" style="margin-bottom:14px">' +
          '<section class="card"><div class="card-head"><h3>' + icon('groups') + 'Top 活跃群 <span class="section-sub">近 30 天</span></h3>' +
            '<div class="card-head-actions"><a class="btn btn-sm btn-ghost" href="#/groups">全部</a></div></div>' +
            '<div class="card-body" id="dash-top-groups"><div class="load-inline">' + icon('loader', 'spin') + ' 加载中…</div></div></section>' +
          '<section class="card"><div class="card-head"><h3>' + icon('members') + 'Top 活跃成员 <span class="section-sub">近 7 天</span></h3>' +
            '<div class="card-head-actions"><a class="btn btn-sm btn-ghost" href="#/analytics-senders">明细</a></div></div>' +
            '<div class="card-body" id="dash-top-senders"><div class="load-inline">' + icon('loader', 'spin') + ' 加载中…</div></div></section>' +
        '</div>' +
        '<div class="grid grid-wide">' +
          '<section class="card"><div class="card-head"><h3>' + icon('heart-pulse') + '健康状态时间轴 <span class="section-sub">运维代理每 5 分钟采样</span></h3>' +
            '<div class="card-head-actions"><a class="btn btn-sm btn-ghost" href="#/ops">运维监控</a></div></div>' +
            '<div class="card-body" id="dash-health"><div class="load-inline">' + icon('loader', 'spin') + ' 读取健康历史…</div></div></section>' +
          '<section class="card"><div class="card-head"><h3>' + icon('alert') + '最近 7 天错误 <span class="section-sub">按天</span></h3>' +
            '<div class="card-head-actions"><a class="btn btn-sm btn-ghost" href="#/logs">日志</a></div></div>' +
            '<div class="card-body" id="dash-errors"><div class="load-inline">' + icon('loader', 'spin') + ' 加载中…</div></div></section>' +
        '</div>';

      const searchBtn = $('#dash-search');
      if (searchBtn) searchBtn.addEventListener('click', () => Palette.openWith(''));
      const refreshBtn = $('#dash-refresh');
      if (refreshBtn) refreshBtn.addEventListener('click', () => route());
      const themeBtn = $('#dash-theme');
      if (themeBtn) themeBtn.addEventListener('click', () => Theme.toggle());

      /* 时钟 */
      const clock = $('#dash-clock');
      if (clock) {
        const tick = () => {
          const d = new Date();
          clock.textContent = pad(d.getHours()) + ':' + pad(d.getMinutes());
        };
        const timer = setInterval(tick, 30000);
        ctx.cleanup(() => clearInterval(timer));
      }

      /* ── 并行拉数据（每块独立渲染，慢接口不阻塞首屏） ── */
      const safe = (promise, fallback) => promise.then((value) => value).catch((err) => {
        if (err instanceof ApiError && err.status === 401) throw err;
        return fallback;
      });

      const analyticsP = safe(probeApi(['/api/v2/analytics/messages?days=14', '/api/v2/charts/messages?days=14']), { ok: false, pending: true });
      const chartsMsgP = safe(apiGet('/api/v2/charts/messages?days=14'), null);
      const chartsToolsP = safe(apiGet('/api/v2/charts/tools?days=14'), null);
      const toolsStatsP = safe(apiGet('/api/v2/tools/stats?days=7'), null);
      const topGroupsP = safe(apiGet('/api/v2/top/groups?days=30&limit=8'), null);
      const sendersP = safe(probeApi(['/api/v2/analytics/senders?days=7&limit=250', '/api/v2/members/all?limit=20']), { ok: false, pending: true });
      const healthP = safe(apiGet('/api/v2/health/history?days=7'), null);
      const errorsP = safe(apiGet('/api/errors/stats'), null);
      const overviewP = safe(apiGet('/api/v2/overview', { timeout: 15000 }), null);

      /* KPI */
      (async () => {
        const [analytics, overview, health, toolsStats] = await Promise.all([analyticsP, overviewP, healthP, toolsStatsP]);
        if (!ctx.alive()) return;
        const root = $('#dash-kpis');
        if (!root) return;
        const hourlyOk = analytics.ok && analytics.data;
        const daily = hourlyOk ? (analytics.data.daily || []) : [];
        const totals = hourlyOk ? (analytics.data.totals || {}) : {};
        const today = daily.length ? daily[daily.length - 1] : {};
        const yesterday = daily.length > 1 ? daily[daily.length - 2] : {};
        const ov = (overview && overview.data) || {};
        const stats = ov.stats || {};
        const healthData = (health && health.data) || {};
        const latest = healthData.latest || {};
        const containers = latest.containers || [];
        const healthyCount = containers.filter((c) => c.running && (!c.health || c.health === 'healthy')).length;
        const toolsRows = (toolsStats && toolsStats.data && toolsStats.data.rows) || [];
        const toolCalls = toolsRows.reduce((sum, row) => sum + (Number(row.calls) || 0), 0);
        const toolFails = toolsRows.reduce((sum, row) => sum + (Number(row.failures) || 0), 0);

        root.innerHTML = kpiGrid([
          {
            label: '今日消息', icon: 'message', tone: 'acc',
            valueHtml: fmtNum(today.total || 0), unit: '条',
            hint: '昨日全天 ' + fmtNum(yesterday.total || 0) + ' 条 · 今日至今',
          },
          {
            label: '今日机器人消息', icon: 'robot',
            valueHtml: fmtNum(today.bot || 0), unit: '条',
            hint: '占比 ' + fmtPct(today.bot || 0, today.total || 0) + ' · 人类 ' + fmtNum(today.human || 0),
          },
          {
            label: '今日活跃成员', icon: 'members', tone: 'ok',
            valueHtml: fmtNum(today.active_senders || 0), unit: '人',
            hint: '新增发言 ' + fmtNum(today.new_senders || 0) + ' 人 · 昨日 ' + fmtNum(yesterday.active_senders || 0),
          },
          {
            label: '近 7 天工具调用', icon: 'wrench',
            valueHtml: fmtNum(toolCalls), unit: '次',
            hint: '失败 ' + fmtNum(toolFails) + ' 次 · 成功率 ' + (toolCalls ? fmtPct(toolCalls - toolFails, toolCalls) : '—'),
          },
          {
            label: '区间消息总量', icon: 'database',
            valueHtml: fmtNum(totals.messages || daily.reduce((sum, row) => sum + (Number(row.total) || 0), 0)), unit: '条',
            hint: '近 ' + fmtNum(daily.length || 14) + ' 天 · 发言成员 ' + fmtNum(totals.active_senders || 0),
          },
          {
            label: '运行健康', icon: 'heart-pulse', tone: containers.length && healthyCount === containers.length ? 'ok' : (containers.length ? 'warn' : ''),
            valueHtml: (containers.length ? healthyCount + '<small>/' + containers.length + '</small>' : '—'),
            hint: latest.at_text ? '采样于 ' + esc(latest.at_text) : '等待运维代理采样',
          },
        ], { cols: 6 });

        const envNode = $('#dash-env');
        if (envNode) {
          const botContainer = containers[0] || (latest.bot || {});
          const botHealth = (latest.bot && latest.bot.health) || botContainer.health || (latest.bot && latest.bot.running ? 'running' : '');
          const parts = [];
          parts.push(botHealth ? '机器人 ' + botHealth : '机器人状态待采样');
          if (ov.health && ov.health.ok === false) parts.push('RPC 异常');
          envNode.textContent = parts.join(' · ');
        }
      })().catch((err) => { console.warn('[dashboard kpi]', err); });

      /* 消息趋势 */
      (async () => {
        const res = await chartsMsgP;
        if (!ctx.alive()) return;
        const el = $('#dash-trend');
        if (!el) return;
        const rows = unwrap(res) || [];
        if (!rows.length) { chartNoData(el, '没有拿到 /api/v2/charts/messages 数据'); return; }
        const dates = rows.map((row) => String(row.date || '').slice(5));
        const totals = rows.map((row) => Number(row.total) || 0);
        const human = rows.map((row) => Number(row.human) || 0);
        const bot = rows.map((row) => Number(row.bot) || 0);
        mountChart(el, chartBase({
          tooltip: { trigger: 'axis' },
          legend: { data: ['人类消息', '机器人消息'] },
          xAxis: { type: 'category', data: dates, boundaryGap: false },
          yAxis: [{ type: 'value', name: '条' }, { type: 'value', name: '机器人', splitLine: { show: false } }],
          series: [
            {
              name: '人类消息', type: 'line', smooth: true, symbol: 'circle', symbolSize: 4, showSymbol: false,
              data: human, lineStyle: { width: 2.4 }, itemStyle: { color: CHART_COLORS[0] },
              areaStyle: {
                color: {
                  type: 'linear', x: 0, y: 0, x2: 0, y2: 1,
                  colorStops: [{ offset: 0, color: 'rgba(124,157,255,.28)' }, { offset: 1, color: 'rgba(124,157,255,0)' }],
                },
              },
            },
            {
              name: '机器人消息', type: 'line', smooth: true, showSymbol: false, yAxisIndex: 1,
              data: bot, lineStyle: { width: 1.8 }, itemStyle: { color: CHART_COLORS[1] },
            },
          ],
        }), rows.map((row) => ({ label: row.date, value: row.total })));
      })().catch((err) => { console.warn('[dashboard trend]', err); });

      /* 工具调用环形 + 排行 */
      (async () => {
        const res = await chartsToolsP;
        if (!ctx.alive()) return;
        const el = $('#dash-tools');
        if (!el) return;
        const rows = unwrap(res) || [];
        if (!rows.length) { chartNoData(el, '没有拿到 /api/v2/charts/tools 数据'); return; }
        const last7 = rows.slice(-7);
        mountChart(el, chartBase({
          grid: { left: 0, right: 0, top: 4, bottom: 0, containLabel: false },
          tooltip: { trigger: 'axis', axisPointer: { type: 'shadow' } },
          xAxis: { type: 'category', data: last7.map((row) => String(row.date || '').slice(5)), axisLabel: { fontSize: 9.5 } },
          yAxis: { type: 'value', splitLine: { show: false }, axisLabel: { show: false } },
          series: [{
            type: 'bar', data: last7.map((row) => Number(row.count) || 0), barMaxWidth: 18,
            itemStyle: {
              borderRadius: [5, 5, 0, 0],
              color: {
                type: 'linear', x: 0, y: 0, x2: 0, y2: 1,
                colorStops: [{ offset: 0, color: CHART_COLORS[0] }, { offset: 1, color: 'rgba(124,157,255,.25)' }],
              },
            },
          }],
        }), rows.map((row) => ({ label: row.date, value: row.count })));

        const statsRes = await toolsStatsP;
        if (!ctx.alive()) return;
        const legend = $('#dash-tools-legend');
        const statsRows = (statsRes && statsRes.data && statsRes.data.rows) || [];
        if (!legend) return;
        if (!statsRows.length) { legend.innerHTML = '<div class="muted-2 text-xs">没有工具调用统计</div>'; return; }
        const top = statsRows.slice(0, 5);
        const max = Number(top[0].calls) || 1;
        legend.innerHTML = top.map((row, index) => {
          const pct = Math.round((Number(row.calls) || 0) / max * 100);
          return '<div class="donut-legend-row"><span class="donut-swatch" style="background:' + CHART_COLORS[index % CHART_COLORS.length] + '"></span>' +
            '<span class="mono" style="font-size:11px">' + esc(row.tool) + '</span><b>' + fmtNum(row.calls) + '</b>' +
            '<span class="muted-2 text-xs" style="width:34px;text-align:right">' + pct + '%</span></div>';
        }).join('');
      })().catch((err) => { console.warn('[dashboard tools]', err); });

      /* 热力图 */
      (async () => {
        const res = await analyticsP;
        if (!ctx.alive()) return;
        const el = $('#dash-heat');
        if (!el) return;
        const data = res.ok ? res.data : null;
        if (data && data.hourly) {
          el.innerHTML = heatmapHtml(data.hourly, { footNote: '数据源：' + esc(data.source || '/api/v2/analytics/messages') });
        } else {
          el.innerHTML = placeholderCard('24×7 热力图暂不可用', (res.error && res.error.message) || '后端 analytics 接口尚未就绪。', '/api/v2/analytics/messages');
        }
      })().catch((err) => { console.warn('[dashboard heat]', err); });

      /* Top 群 / Top 成员 */
      (async () => {
        const [groupsRes, sendersRes] = await Promise.all([topGroupsP, sendersP]);
        if (!ctx.alive()) return;
        const groupRoot = $('#dash-top-groups');
        const senderRoot = $('#dash-top-senders');
        const groups = unwrap(groupsRes) || [];
        if (groupRoot) {
          if (!groups.length) groupRoot.innerHTML = emptyState({ icon: 'groups', title: '没有群消息数据' });
          else {
            const max = Number(groups[0].messages) || 1;
            groupRoot.innerHTML = '<div class="rank-list">' + groups.slice(0, 7).map((row, index) =>
              '<div class="rank-item" data-group-id="' + escAttr(row.chat_id) + '">' +
              '<span class="rank-num">' + (index + 1) + '</span>' +
              '<span class="rank-main"><span class="rank-title">' + esc(row.name || row.chat_id) + '</span>' +
              '<span class="rank-sub">' + fmtNum(row.human) + ' 人类 · ' + fmtNum(row.bot) + ' 机器人 · 最近 ' + esc(ago(row.last_at) || '—') + '</span>' +
              '<span class="rank-bar"><i style="width:' + Math.max(3, Math.round(Number(row.messages) / max * 100)) + '%"></i></span></span>' +
              '<span class="rank-value">' + fmtNum(row.messages) + '</span></div>').join('') + '</div>';
            groupRoot.querySelectorAll('[data-group-id]').forEach((node) => {
              node.addEventListener('click', () => go('#/groups/' + encodeURIComponent(node.getAttribute('data-group-id'))));
            });
          }
        }
        if (senderRoot) {
          const data = sendersRes.ok ? sendersRes.data : null;
          const senders = (data && data.senders) || [];
          if (!senders.length) {
            const fallback = (data && data.members) || [];
            if (!fallback.length) senderRoot.innerHTML = emptyState({ icon: 'members', title: '没有成员活跃数据' });
            else {
              senderRoot.innerHTML = '<div class="rank-list">' + fallback.slice(0, 7).map((row, index) =>
                '<div class="rank-item"><span class="rank-num">' + (index + 1) + '</span>' +
                '<span class="rank-main"><span class="rank-title">' + esc((row.aliases && row.aliases[0]) || row.user_id) + '</span>' +
                '<span class="rank-sub">ID ' + esc(row.user_id) + ' · 最近 ' + esc(row.last_at || row.last_seen || '—') + '</span></span>' +
                '<span class="rank-value">' + fmtNum(row.messages) + '</span></div>').join('') + '</div>';
            }
          } else {
            const max = Number(senders[0].messages) || 1;
            senderRoot.innerHTML = '<div class="rank-list">' + senders.slice(0, 7).map((row) => {
              const rank = senders.indexOf(row) + 1;
              return '<div class="rank-item" data-sender-id="' + escAttr(row.sender_id) + '">' +
                '<span class="rank-num">' + rank + '</span>' +
                '<span class="rank-main"><span class="rank-title">' + esc(row.name || row.sender_id) + '</span>' +
                '<span class="rank-sub">' + esc(row.groups) + ' 个群 · 日均 ' + fmtNum(row.messages_per_day, 1) + ' 条</span>' +
                '<span class="rank-bar"><i style="width:' + Math.max(3, Math.round(Number(row.messages) / max * 100)) + '%"></i></span></span>' +
                '<span class="rank-value">' + fmtNum(row.messages) + '</span></div>';
            }).join('') + '</div>';
            senderRoot.querySelectorAll('[data-sender-id]').forEach((node) => {
              node.addEventListener('click', () => go('#/members/' + encodeURIComponent(node.getAttribute('data-sender-id'))));
            });
          }
        }
      })().catch((err) => { console.warn('[dashboard top]', err); });

      /* 健康时间轴 + 错误趋势 */
      (async () => {
        const [healthRes, errorRes] = await Promise.all([healthP, errorsP]);
        if (!ctx.alive()) return;
        const healthRoot = $('#dash-health');
        const errorRoot = $('#dash-errors');
        if (healthRoot) {
          const data = (healthRes && healthRes.data) || {};
          if (!data.available || !(data.points || []).length) {
            healthRoot.innerHTML = placeholderCard('健康历史还在积累', '运维代理每 5 分钟写一个检查点；当前文件 ' + esc(data.file || '/data/health.jsonl') + ' 里只有 ' + fmtNum(data.count || 0) + ' 条。', '/api/v2/health/history');
          } else {
            healthRoot.innerHTML = timelineHtml((data.points || []).slice(-6).reverse()) +
              '<div class="muted-2 text-xs mt-8">' + icon('clock') + ' 采样间隔 ' + esc(data.interval_seconds || 300) + ' 秒 · 共 ' + fmtNum(data.count) + ' 个检查点</div>';
          }
        }
        if (errorRoot) {
          const data = (errorRes && errorRes.data) || {};
          const trend = data.trend || [];
          const components = data.by_component || [];
          errorRoot.innerHTML = (components.length
            ? '<div class="row mb-12">' + components.map((row) => '<span class="tag tag-warn">' + esc(row.name) + ' · ' + fmtNum(row.count) + '</span>').join('') + '</div>'
            : '') + errorTrendHtml(trend);
        }
      })().catch((err) => { console.warn('[dashboard health]', err); });

      guard(ctx, 'dashboard');
    },
  };

  /* ══ 2. 分析：消息 ══════════════════════════════════════════════════════ */

  VIEWS['analytics-msg'] = {
    id: 'analytics-msg',
    name: '消息分析',
    icon: 'analytics',
    skeleton: 'chart',
    render: async (ctx) => {
      const days = ctx.n('days', 14);
      ctx.root.innerHTML = pageHead({
        eyebrow: 'ANALYTICS',
        title: '消息分析',
        icon: 'analytics',
        sub: '每天的消息量、人类 / 机器人构成、活跃成员、新增发言，以及 24×7 活跃热力矩阵。',
        actions: [daysSelector(days, [7, 14, 30]), '<button class="btn btn-sm btn-ghost" id="v4-refresh" type="button">' + icon('refresh') + '刷新</button>'],
      }) +
        '<div id="am-kpis"><div class="sk-row"><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div></div></div>' +
        '<div class="grid grid-wide" style="margin-bottom:14px">' +
          '<section class="card"><div class="card-head"><h3>' + icon('trending') + '每日消息趋势</h3><div class="card-head-actions">' +
            '<button class="icon-btn sm" type="button" data-chart-export="#am-trend" data-chart-name="每日消息趋势">' + icon('download') + '</button></div></div>' +
            '<div class="card-body"><div id="am-trend" class="chart chart-lg"></div></div></section>' +
          '<section class="card"><div class="card-head"><h3>' + icon('gauge') + 'Token 用量 <span class="section-sub">审计日志</span></h3></div>' +
            '<div class="card-body" id="am-tokens"><div class="load-inline">' + icon('loader', 'spin') + ' 加载中…</div></div></section>' +
        '</div>' +
        '<section class="card" style="margin-bottom:14px"><div class="card-head"><h3>' + icon('calendar') + '24 × 7 活跃热力图</h3></div>' +
          '<div class="card-body"><div id="am-heat"><div class="load-inline">' + icon('loader', 'spin') + ' 计算中…</div></div></div></section>' +
        '<section class="card"><div class="card-head"><h3>' + icon('list') + '每日明细</h3><div class="card-head-actions">' +
          '<button class="btn btn-sm btn-ghost" id="am-export" type="button">' + icon('download') + 'CSV</button></div></div>' +
          '<div class="card-body" id="am-table"><div class="load-inline">' + icon('loader', 'spin') + ' 加载中…</div></div></section>';

      bindDaysSelector(ctx, 'analytics-msg');
      const refreshBtn = $('#v4-refresh');
      if (refreshBtn) refreshBtn.addEventListener('click', () => route());

      const res = await probeApi(['/api/v2/analytics/messages' + qs({ days: days }), '/api/v2/charts/messages' + qs({ days: days })], { timeout: 20000 });
      if (!ctx.alive()) return;
      const data = res.ok ? res.data : {};
      const daily = (data && data.daily) || (Array.isArray(data) ? data : []);
      const totals = (data && data.totals) || {};
      const source = (data && data.source) || (res.ok ? '' : (res.error && res.error.message) || '接口未就绪');

      if (!res.ok) {
        $('#am-kpis').innerHTML = notice('warn', '<b>分析接口暂不可用</b>：' + esc(source) + '。已降级到 /api/v2/charts/messages 的基础趋势。');
      } else {
        const last = daily[daily.length - 1] || {};
        const prev = daily[daily.length - 2] || {};
        $('#am-kpis').innerHTML = kpiGrid([
          { label: '区间消息总量', icon: 'message', tone: 'acc', valueHtml: fmtNum(totals.messages || daily.reduce((s, r) => s + (Number(r.total) || 0), 0)), unit: '条', hint: esc(data.from ? data.from + ' → ' + data.to : '') },
          { label: '日均消息', icon: 'activity', valueHtml: fmtNum(totals.avg_per_day || Math.round((totals.messages || 0) / Math.max(1, daily.length))), unit: '条', hint: '最后一天 ' + fmtNum(last.total) + ' ' + deltaChip(last.total, prev.total) },
          { label: '机器人占比', icon: 'robot', valueHtml: totals.bot_ratio != null ? (totals.bot_ratio * 100).toFixed(1) + '<small>%</small>' : fmtPct(totals.bot, totals.messages), hint: '机器人 ' + fmtNum(totals.bot) + ' · 人类 ' + fmtNum(totals.human) },
          { label: '活跃成员', icon: 'members', tone: 'ok', valueHtml: fmtNum(totals.active_senders), unit: '人', hint: '新增发言 ' + fmtNum(totals.new_senders) + ' 人' },
          { label: '人均消息', icon: 'user', valueHtml: fmtNum(totals.avg_per_sender, 1), unit: '条', hint: '区间 ' + fmtNum(daily.length) + ' 天' },
        ]);
      }

      if (daily.length) {
        mountChart('#am-trend', chartBase({
          legend: { data: ['人类消息', '机器人消息', '活跃成员'] },
          xAxis: { type: 'category', data: daily.map((row) => String(row.date).slice(5)), boundaryGap: false },
          yAxis: [{ type: 'value', name: '条' }, { type: 'value', name: '人 / 机器人', splitLine: { show: false } }],
          tooltip: { trigger: 'axis' },
          series: [
            {
              name: '人类消息', type: 'line', smooth: true, showSymbol: false, data: daily.map((row) => Number(row.human) || 0),
              lineStyle: { width: 2.4 }, itemStyle: { color: CHART_COLORS[0] },
              areaStyle: { color: { type: 'linear', x: 0, y: 0, x2: 0, y2: 1, colorStops: [{ offset: 0, color: 'rgba(124,157,255,.26)' }, { offset: 1, color: 'rgba(124,157,255,0)' }] } },
            },
            { name: '机器人消息', type: 'line', smooth: true, showSymbol: false, yAxisIndex: 1, data: daily.map((row) => Number(row.bot) || 0), lineStyle: { width: 1.8 }, itemStyle: { color: CHART_COLORS[1] } },
            { name: '活跃成员', type: 'line', smooth: true, showSymbol: false, yAxisIndex: 1, data: daily.map((row) => Number(row.active_senders) || 0), lineStyle: { width: 1.2, type: 'dashed' }, itemStyle: { color: CHART_COLORS[2] } },
          ],
        }), daily.map((row) => ({ label: row.date, value: row.total })));
        renderTable($('#am-table'), {
          stateKey: 'analytics-msg', sortable: true,
          columns: [
            { key: 'date', label: '日期', mono: true, render: (row) => '<span class="mono">' + esc(row.date) + '</span>' },
            { key: 'total', label: '总消息', num: true, render: (row) => '<b>' + fmtNum(row.total) + '</b>' },
            { key: 'human', label: '人类', num: true, render: (row) => fmtNum(row.human) },
            { key: 'bot', label: '机器人', num: true, render: (row) => fmtNum(row.bot) },
            { key: 'bot_ratio', label: '机器人占比', num: true, hideSm: true, render: (row) => fmtPct(row.bot, row.total) },
            { key: 'active_senders', label: '活跃成员', num: true, render: (row) => fmtNum(row.active_senders) },
            { key: 'new_senders', label: '新增发言', num: true, hideSm: true, render: (row) => fmtNum(row.new_senders) },
          ],
          rows: daily,
        });
      } else {
        chartNoData('#am-trend', '没有拿到分析数据');
        $('#am-table').innerHTML = pendingState({ title: '分析接口尚未就绪', hint: res.error ? res.error.message : '等待 /api/v2/analytics/messages', path: '/api/v2/analytics/messages' });
      }

      $('#am-heat').innerHTML = data && data.hourly ? heatmapHtml(data.hourly, { footNote: '数据源：' + esc(data.source || '') }) : placeholderCard('热力图暂不可用', '接口没有返回 hourly 矩阵。', '/api/v2/analytics/messages');

      const exportBtn = $('#am-export');
      if (exportBtn) exportBtn.addEventListener('click', () => exportCsv(daily, ['date', 'total', 'human', 'bot', 'active_senders', 'new_senders'], 'fm-messages-' + days + 'd'));

      /* Token 用量 */
      (async () => {
        const tokenRes = await probeApi(['/api/v2/analytics/tokens?days=' + days]);
        if (!ctx.alive()) return;
        const root = $('#am-tokens');
        if (!root) return;
        if (!tokenRes.ok || !tokenRes.data || tokenRes.data.available === false) {
          root.innerHTML = placeholderCard('Token 用量暂不可用', (tokenRes.error && tokenRes.error.message) || '审计日志里还没有 tokenUsage 字段。', '/api/v2/analytics/tokens');
          return;
        }
        const t = tokenRes.data;
        const tokens = t.tokens || {};
        root.innerHTML =
          '<div class="ops-stats" style="grid-template-columns:repeat(2,minmax(0,1fr))">' +
          miniStat('总 Token', fmtNum(tokens.total), (t.runs || 0) + ' 次运行 / ' + (t.turns || 0) + ' 轮', 'zap', 'acc') +
          miniStat('Prompt', fmtNum(tokens.prompt), '缓存命中 ' + ((tokens.cached_ratio || 0) * 100).toFixed(1) + '%', 'code') +
          miniStat('Completion', fmtNum(tokens.completion), '每轮平均 ' + fmtNum(t.per_turn && t.per_turn.completion), 'send') +
          miniStat('缓存 Token', fmtNum(tokens.cached), '缓存比例 ' + ((tokens.cached_ratio || 0) * 100).toFixed(1) + '%', 'database', 'ok') +
          '</div>' +
          '<div class="muted-2 text-xs mt-12">' + icon('info') + ' ' + esc(t.note || '来源 audit_log') + '</div>';
      })().catch((err) => console.warn('[analytics tokens]', err));

      guard(ctx, 'analytics-msg');
    },
  };

  /* ══ 3. 分析：群 ════════════════════════════════════════════════════════ */

  VIEWS['analytics-groups'] = {
    id: 'analytics-groups',
    name: '群分析',
    icon: 'groups',
    skeleton: 'chart',
    render: async (ctx) => {
      const days = ctx.n('days', 30);
      ctx.root.innerHTML = pageHead({
        eyebrow: 'ANALYTICS',
        title: '群分析',
        icon: 'groups',
        sub: '每个群的消息量、活跃人数、人均条数、机器人占比与活跃高峰小时。',
        actions: [daysSelector(days, [7, 14, 30]), '<button class="btn btn-sm btn-ghost" id="v4-refresh" type="button">' + icon('refresh') + '刷新</button>'],
      }) +
        '<div id="ag-kpis"><div class="sk-row"><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div></div></div>' +
        '<div class="grid grid-wide" style="margin-bottom:14px">' +
          '<section class="card"><div class="card-head"><h3>' + icon('chart') + '群消息量 Top 12</h3><div class="card-head-actions">' +
            '<button class="icon-btn sm" type="button" data-chart-export="#ag-bar" data-chart-name="群消息量">' + icon('download') + '</button></div></div>' +
            '<div class="card-body"><div id="ag-bar" class="chart chart-lg"></div></div></section>' +
          '<section class="card"><div class="card-head"><h3>' + icon('gauge') + '群体对比</h3></div><div class="card-body" id="ag-side"><div class="load-inline">' + icon('loader', 'spin') + ' 加载中…</div></div></section>' +
        '</div>' +
        '<section class="card"><div class="card-head"><h3>' + icon('list') + '群明细 <span class="section-sub">可排序 · 行内展开</span></h3>' +
          '<div class="card-head-actions"><button class="btn btn-sm btn-ghost" id="ag-export" type="button">' + icon('download') + 'CSV</button></div></div>' +
          '<div class="card-body"><div class="v3-filter mb-12"><input class="input input-sm" id="ag-filter" placeholder="筛选群名 / 群号" style="max-width:240px"></div><div id="ag-table"><div class="load-inline">' + icon('loader', 'spin') + ' 加载中…</div></div></div></section>';

      bindDaysSelector(ctx, 'analytics-groups');
      const refreshBtn = $('#v4-refresh');
      if (refreshBtn) refreshBtn.addEventListener('click', () => route());

      const res = await probeApi(['/api/v2/analytics/groups' + qs({ days: days }), '/api/v2/top/groups' + qs({ days: days, limit: 50 })], { timeout: 20000 });
      if (!ctx.alive()) return;
      const data = res.ok ? res.data : {};
      let groups = (data && data.groups) || (Array.isArray(data) ? data : []);
      const totals = (data && data.totals) || {};
      const source = (data && data.source) || (res.error && res.error.message) || '';

      const draw = (filter) => {
        const keyword = String(filter || '').trim().toLowerCase();
        const rows = groups.filter((row) => !keyword || String(row.name || '').toLowerCase().indexOf(keyword) >= 0 || String(row.chat_id).indexOf(keyword) >= 0);
        renderTable($('#ag-table'), {
          stateKey: 'analytics-groups', sortable: true,
          columns: [
            { key: 'name', label: '群', render: (row) => '<span class="cell-truncate"><b>' + esc(row.name || row.chat_id) + '</b><span class="muted-2 mono text-xs" style="margin-left:7px">' + esc(row.chat_id) + '</span></span>' },
            { key: 'messages', label: '消息量', num: true, render: (row) => '<b>' + fmtNum(row.messages) + '</b>' },
            { key: 'human', label: '人类', num: true, render: (row) => fmtNum(row.human) },
            { key: 'bot', label: '机器人', num: true, hideSm: true, render: (row) => fmtNum(row.bot) },
            { key: 'bot_ratio', label: '机器人占比', num: true, hideSm: true, render: (row) => (row.bot_ratio != null ? (row.bot_ratio * 100).toFixed(1) + '%' : fmtPct(row.bot, row.messages)) },
            { key: 'active_senders', label: '活跃成员', num: true, render: (row) => fmtNum(row.active_senders) },
            { key: 'messages_per_sender', label: '人均条数', num: true, hideSm: true, render: (row) => fmtNum(row.messages_per_sender, 1) },
            { key: 'peak_hour', label: '活跃高峰', width: '110px', render: (row) => row.peak_hour ? '<span class="tag tag-acc mono">' + String(row.peak_hour.hour).padStart(2, '0') + ':00</span> <span class="muted-2 text-xs">' + fmtNum(row.peak_hour.count) + '</span>' : '—' },
            { key: 'last_at', label: '最近消息', width: '132px', hideSm: true, render: (row) => agoCell(row.last_at) },
          ],
          rows: rows,
          detail: (row) => '<div class="grid grid-2"><div>' +
            '<div class="stat-line"><span class="muted">会话类型</span><span>' + esc(row.kind || '—') + '</span></div>' +
            '<div class="stat-line"><span class="muted">首次消息</span><span class="mono">' + esc(row.first_at || '—') + '</span></div>' +
            '<div class="stat-line"><span class="muted">最近消息</span><span class="mono">' + esc(row.last_at || '—') + '</span></div>' +
            '<div class="stat-line"><span class="muted">活跃小时数</span><span>' + fmtNum(row.active_hours) + ' / 24</span></div>' +
            '</div><div>' +
            '<div class="stat-line"><span class="muted">人均条数</span><span>' + fmtNum(row.messages_per_sender, 1) + '</span></div>' +
            '<div class="stat-line"><span class="muted">机器人占比</span><span>' + (row.bot_ratio != null ? (row.bot_ratio * 100).toFixed(2) + '%' : '—') + '</span></div>' +
            '<div class="stat-line"><span class="muted">峰值小时</span><span>' + (row.peak_hour ? row.peak_hour.hour + ':00 · ' + fmtNum(row.peak_hour.count) + ' 条' : '—') + '</span></div>' +
            '<div class="stat-line"><span class="muted">操作</span><span class="row"><button class="btn btn-sm btn-ghost" data-goto="' + escAttr('#/groups/' + row.chat_id) + '" type="button">' + icon('external') + '群详情</button>' +
            '<button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(row).slice(0, 3200)) + '" type="button">' + icon('code') + 'JSON</button></span></div>' +
            '</div></div>',
          onRow: (row) => go('#/groups/' + encodeURIComponent(row.chat_id)),
        });
      };

      const filterInput = $('#ag-filter');
      if (filterInput) filterInput.addEventListener('input', debounce(() => draw(filterInput.value), 160));
      const exportBtn = $('#ag-export');
      if (exportBtn) exportBtn.addEventListener('click', () => exportCsv(groups, ['chat_id', 'name', 'kind', 'messages', 'human', 'bot', 'active_senders', 'messages_per_sender'], 'fm-groups-' + days + 'd'));

      if (!groups.length) {
        $('#ag-kpis').innerHTML = '';
        $('#ag-bar').innerHTML = '';
        $('#ag-table').innerHTML = pendingState({ title: '群分析接口尚未就绪', hint: source || '等待 /api/v2/analytics/groups', path: '/api/v2/analytics/groups' });
        guard(ctx, 'analytics-groups');
        return;
      }

      const botTotal = totals.bot != null ? totals.bot : groups.reduce((sum, row) => sum + (Number(row.bot) || 0), 0);
      const humanTotal = totals.human != null ? totals.human : groups.reduce((sum, row) => sum + (Number(row.human) || 0), 0);
      const botRatio = totals.bot_ratio != null ? totals.bot_ratio : (botTotal + humanTotal ? botTotal / (botTotal + humanTotal) : 0);
      $('#ag-kpis').innerHTML = kpiGrid([
        { label: '统计群数', icon: 'groups', tone: 'acc', valueHtml: fmtNum(data.total_groups || groups.length), hint: esc(data.from ? '自 ' + data.from : '') },
        { label: '消息总量', icon: 'message', valueHtml: fmtNum(totals.messages || groups.reduce((s, r) => s + (Number(r.messages) || 0), 0)), unit: '条' },
        { label: '活跃成员（去重）', icon: 'members', tone: 'ok', valueHtml: fmtNum(totals.active_senders || 0), unit: '人' },
        { label: '机器人消息', icon: 'robot', valueHtml: fmtNum(botTotal), unit: '条', hint: '占比 ' + (botRatio * 100).toFixed(1) + '%' },
      ]);

      const top = groups.slice(0, 12).slice().reverse();
      mountChart('#ag-bar', chartBase({
        grid: { left: 6, right: 30, top: 10, bottom: 4, containLabel: true },
        tooltip: { trigger: 'axis', axisPointer: { type: 'shadow' } },
        xAxis: { type: 'value', splitLine: { show: true } },
        yAxis: { type: 'category', data: top.map((row) => truncate(row.name || row.chat_id, 12)), axisLabel: { fontSize: 10.5 } },
        series: [{
          type: 'bar', data: top.map((row) => Number(row.messages) || 0), barMaxWidth: 16,
          itemStyle: {
            borderRadius: [0, 5, 5, 0],
            color: {
              type: 'linear', x: 0, y: 0, x2: 1, y2: 0,
              colorStops: [{ offset: 0, color: 'rgba(124,157,255,.35)' }, { offset: 1, color: CHART_COLORS[0] }],
            },
          },
          label: { show: true, position: 'right', color: cssVar('--text-3', '#748094'), fontSize: 10, formatter: (p) => fmtNum(p.value) },
        }],
      }), groups.map((row) => ({ label: row.name, value: row.messages })));

      const side = $('#ag-side');
      if (side) {
        const sorted = groups.slice().sort((a, b) => (Number(b.messages_per_sender) || 0) - (Number(a.messages_per_sender) || 0)).slice(0, 6);
        const max = Number(sorted[0] && sorted[0].messages_per_sender) || 1;
        side.innerHTML = '<div class="section-title">人均发言最活跃</div>' + sorted.map((row) =>
          '<div class="meter-row"><span class="meter-label" title="' + escAttr(row.name) + '">' + esc(truncate(row.name || row.chat_id, 10)) + '</span>' +
          '<span class="meter-track"><i style="width:' + Math.max(3, Math.round(Number(row.messages_per_sender) / max * 100)) + '%"></i></span>' +
          '<span class="meter-value">' + fmtNum(row.messages_per_sender, 0) + '</span></div>').join('') +
          '<div class="section-title mt-16">机器人占比最高</div>' + groups.slice().sort((a, b) => (Number(b.bot_ratio) || 0) - (Number(a.bot_ratio) || 0)).slice(0, 5).map((row) =>
            '<div class="meter-row"><span class="meter-label" title="' + escAttr(row.name) + '">' + esc(truncate(row.name || row.chat_id, 10)) + '</span>' +
            '<span class="meter-track"><i style="width:' + Math.max(2, Math.round((Number(row.bot_ratio) || 0) * 100 * 3)) + '%;background:linear-gradient(90deg,#b18cff,#7c9dff)"></i></span>' +
            '<span class="meter-value">' + ((Number(row.bot_ratio) || 0) * 100).toFixed(1) + '%</span></div>').join('');
      }

      draw(filterInput ? filterInput.value : '');
      guard(ctx, 'analytics-groups');
    },
  };

  /* ══ 4. 分析：成员活跃 ══════════════════════════════════════════════════ */

  VIEWS['analytics-senders'] = {
    id: 'analytics-senders',
    name: '成员活跃',
    icon: 'members',
    skeleton: 'chart',
    render: async (ctx) => {
      const days = ctx.n('days', 7);
      ctx.root.innerHTML = pageHead({
        eyebrow: 'ANALYTICS',
        title: '成员活跃',
        icon: 'members',
        sub: '谁在说话、说多少、活跃在哪些群 —— Top 发言者排行与群分布。',
        actions: [daysSelector(days, [7, 14, 30]), '<button class="btn btn-sm btn-ghost" id="v4-refresh" type="button">' + icon('refresh') + '刷新</button>'],
      }) +
        '<div id="as-kpis"><div class="sk-row"><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div></div></div>' +
        '<div class="grid grid-wide" style="margin-bottom:14px">' +
          '<section class="card"><div class="card-head"><h3>' + icon('chart') + 'Top 20 发言者</h3><div class="card-head-actions">' +
            '<button class="icon-btn sm" type="button" data-chart-export="#as-bar" data-chart-name="Top 发言者">' + icon('download') + '</button></div></div>' +
            '<div class="card-body"><div id="as-bar" class="chart chart-xl"></div></div></section>' +
          '<section class="card"><div class="card-head"><h3>' + icon('pin') + '群分布</h3></div><div class="card-body" id="as-side"><div class="load-inline">' + icon('loader', 'spin') + ' 加载中…</div></div></section>' +
        '</div>' +
        '<section class="card"><div class="card-head"><h3>' + icon('list') + '发言者明细</h3><div class="card-head-actions">' +
          '<button class="btn btn-sm btn-ghost" id="as-export" type="button">' + icon('download') + 'CSV</button></div></div>' +
          '<div class="card-body"><div id="as-table"><div class="load-inline">' + icon('loader', 'spin') + ' 加载中…</div></div></div></section>';

      bindDaysSelector(ctx, 'analytics-senders');
      const refreshBtn = $('#v4-refresh');
      if (refreshBtn) refreshBtn.addEventListener('click', () => route());

      const res = await probeApi(['/api/v2/analytics/senders' + qs({ days: days, limit: 50 }), '/api/v2/members/all?limit=50'], { timeout: 20000 });
      if (!ctx.alive()) return;
      const data = res.ok ? res.data : {};
      let senders = (data && data.senders) || [];
      if (!senders.length && data && data.members) {
        senders = data.members.map((row) => ({
          sender_id: row.user_id, name: (row.aliases && row.aliases[0]) || row.user_id,
          messages: row.messages, active_days: null, groups: null, messages_per_day: null, group_top: [],
        }));
      }
      const totals = (data && data.totals) || {};

      if (!senders.length) {
        $('#as-kpis').innerHTML = '';
        $('#as-table').innerHTML = pendingState({ title: '成员活跃接口尚未就绪', hint: (res.error && res.error.message) || '等待 /api/v2/analytics/senders', path: '/api/v2/analytics/senders' });
        $('#as-bar').innerHTML = '';
        $('#as-side').innerHTML = '';
        guard(ctx, 'analytics-senders');
        return;
      }

      $('#as-kpis').innerHTML = kpiGrid([
        { label: '活跃成员', icon: 'members', tone: 'acc', valueHtml: fmtNum(totals.active_senders || senders.length), unit: '人', hint: '近 ' + days + ' 天有发言' },
        { label: '人类消息', icon: 'message', valueHtml: fmtNum(totals.human_messages || senders.reduce((s, r) => s + (Number(r.messages) || 0), 0)), unit: '条' },
        { label: '人均消息', icon: 'user', valueHtml: fmtNum(totals.avg_messages_per_sender, 1), unit: '条' },
        { label: 'Top1 发言', icon: 'flame', tone: 'warn', valueHtml: esc(truncate(senders[0].name || senders[0].sender_id, 8)), hint: fmtNum(senders[0].messages) + ' 条 · 日均 ' + fmtNum(senders[0].messages_per_day, 1) },
      ]);

      const top = senders.slice(0, 20).slice().reverse();
      mountChart('#as-bar', chartBase({
        grid: { left: 6, right: 30, top: 10, bottom: 4, containLabel: true },
        tooltip: { trigger: 'axis', axisPointer: { type: 'shadow' } },
        xAxis: { type: 'value' },
        yAxis: { type: 'category', data: top.map((row) => truncate(row.name || row.sender_id, 12)), axisLabel: { fontSize: 10.5 } },
        series: [{
          type: 'bar', data: top.map((row) => Number(row.messages) || 0), barMaxWidth: 14,
          itemStyle: { borderRadius: [0, 5, 5, 0], color: { type: 'linear', x: 0, y: 0, x2: 1, y2: 0, colorStops: [{ offset: 0, color: 'rgba(177,140,255,.3)' }, { offset: 1, color: CHART_COLORS[1] }] } },
          label: { show: true, position: 'right', color: cssVar('--text-3', '#748094'), fontSize: 10, formatter: (p) => fmtNum(p.value) },
        }],
      }), senders.map((row) => ({ label: row.name, value: row.messages })));

      const side = $('#as-side');
      if (side) {
        const groupCount = new Map();
        senders.forEach((row) => (row.group_top || []).forEach((g) => {
          groupCount.set(g.name || g.chat_id, (groupCount.get(g.name || g.chat_id) || 0) + (Number(g.messages) || 0));
        }));
        const entries = Array.from(groupCount.entries()).sort((a, b) => b[1] - a[1]).slice(0, 8);
        const max = entries.length ? entries[0][1] : 1;
        side.innerHTML = entries.length
          ? '<div class="section-title">发言集中在哪些群</div>' + entries.map((entry) =>
            '<div class="meter-row"><span class="meter-label" title="' + escAttr(entry[0]) + '">' + esc(truncate(entry[0], 10)) + '</span>' +
            '<span class="meter-track"><i style="width:' + Math.max(3, Math.round(entry[1] / max * 100)) + '%"></i></span>' +
            '<span class="meter-value">' + fmtNum(entry[1]) + '</span></div>').join('')
          : '<div class="muted-2 text-sm">接口没有返回 group_top，无法展示群分布。</div>';
      }

      renderTable($('#as-table'), {
        stateKey: 'analytics-senders', sortable: true,
        columns: [
          { key: 'rank', label: '#', width: '52px', render: (row, index) => '<span class="idx">' + (index + 1) + '</span>' },
          { key: 'name', label: '成员', render: (row) => '<b>' + esc(row.name || row.sender_id) + '</b><span class="muted-2 mono text-xs" style="margin-left:7px">' + esc(row.sender_id) + '</span>' },
          { key: 'messages', label: '消息量', num: true, render: (row) => '<b>' + fmtNum(row.messages) + '</b>' },
          { key: 'messages_per_day', label: '日均', num: true, render: (row) => fmtNum(row.messages_per_day, 1) },
          { key: 'active_days', label: '活跃天数', num: true, hideSm: true, render: (row) => fmtNum(row.active_days) },
          { key: 'groups', label: '涉及群', num: true, hideSm: true, render: (row) => fmtNum(row.groups) },
          { key: 'last_at', label: '最近发言', width: '132px', hideSm: true, render: (row) => agoCell(row.last_at) },
        ],
        rows: senders,
        detail: (row) => '<div class="row" style="gap:8px;flex-wrap:wrap">' +
          (row.group_top || []).map((g) => '<span class="tag tag-acc">' + esc(g.name || g.chat_id) + ' · ' + fmtNum(g.messages) + '</span>').join('') +
          '<button class="btn btn-sm btn-ghost" data-goto="' + escAttr('#/members/' + row.sender_id) + '" type="button">' + icon('user') + '成员档案</button>' +
          '<button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(row).slice(0, 3200)) + '" type="button">' + icon('code') + 'JSON</button></div>',
        onRow: (row) => go('#/members/' + encodeURIComponent(row.sender_id)),
      });

      const exportBtn = $('#as-export');
      if (exportBtn) exportBtn.addEventListener('click', () => exportCsv(senders, ['sender_id', 'name', 'messages', 'messages_per_day', 'active_days', 'groups'], 'fm-senders-' + days + 'd'));

      guard(ctx, 'analytics-senders');
    },
  };

  /* ══ 5. 分析：响应延迟 ══════════════════════════════════════════════════ */

  VIEWS['analytics-latency'] = {
    id: 'analytics-latency',
    name: '响应延迟',
    icon: 'gauge',
    skeleton: 'chart',
    render: async (ctx) => {
      const days = ctx.n('days', 7);
      ctx.root.innerHTML = pageHead({
        eyebrow: 'ANALYTICS',
        title: '响应延迟',
        icon: 'gauge',
        sub: '从 audit_log 的 run started / finished 事件配对，统计机器人单次运行的耗时分布。',
        actions: [daysSelector(days, [7, 14, 30]), '<button class="btn btn-sm btn-ghost" id="v4-refresh" type="button">' + icon('refresh') + '刷新</button>'],
      }) +
        '<div id="al-kpis"><div class="sk-row"><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div></div></div>' +
        '<div class="grid grid-2" style="margin-bottom:14px">' +
          chartCard('al-hist', '耗时直方图', '每次运行的完成耗时', '') +
          '<section class="card"><div class="card-head"><h3>' + icon('trending') + '每日 p50 / p90</h3><div class="card-head-actions">' +
            '<button class="icon-btn sm" type="button" data-chart-export="#al-daily" data-chart-name="每日延迟">' + icon('download') + '</button></div></div>' +
            '<div class="card-body"><div id="al-daily" class="chart chart-sm"></div></div></section>' +
        '</div>' +
        '<div class="grid grid-wide">' +
          '<section class="card"><div class="card-head"><h3>' + icon('flame') + '最慢的运行 Top 10</h3></div><div class="card-body" id="al-slowest"><div class="load-inline">' + icon('loader', 'spin') + ' 加载中…</div></div></section>' +
          '<section class="card"><div class="card-head"><h3>' + icon('info') + '统计口径</h3></div><div class="card-body" id="al-meta"><div class="load-inline">' + icon('loader', 'spin') + ' 加载中…</div></div></section>' +
        '</div>';

      bindDaysSelector(ctx, 'analytics-latency');
      const refreshBtn = $('#v4-refresh');
      if (refreshBtn) refreshBtn.addEventListener('click', () => route());

      const res = await probeApi(['/api/v2/analytics/latency' + qs({ days: days })], { timeout: 20000 });
      if (!ctx.alive()) return;
      if (!res.ok) {
        $('#al-kpis').innerHTML = '';
        $('#al-hist').innerHTML = '';
        $('#al-daily').innerHTML = '';
        $('#al-slowest').innerHTML = pendingState({ title: '延迟分析接口尚未就绪', hint: (res.error && res.error.message) || '等待 /api/v2/analytics/latency', path: '/api/v2/analytics/latency' });
        $('#al-meta').innerHTML = notice('info', '这个页面依赖 audit_log 里成对出现的 <code class="inline">run started</code> / <code class="inline">run finished</code> 事件；接口就绪后会自动出图。');
        guard(ctx, 'analytics-latency');
        return;
      }
      const data = res.data || {};
      const ms = data.ms || {};
      const runs = data.runs || {};
      const hist = data.histogram || [];
      const daily = data.daily || [];
      const slowest = data.slowest || [];
      const counts = hist.reduce((sum, row) => sum + (Number(row.count) || 0), 0);

      $('#al-kpis').innerHTML = kpiGrid([
        { label: '配对运行数', icon: 'play', tone: 'acc', valueHtml: fmtNum(runs.paired || ms.count || 0), unit: '次', hint: '开始 ' + fmtNum(runs.started) + ' · 未完成 ' + fmtNum(runs.unfinished) },
        { label: 'p50 耗时', icon: 'gauge', valueHtml: fmtDuration(ms.p50), hint: '平均 ' + fmtDuration(ms.avg) },
        { label: 'p90 耗时', icon: 'trending', tone: 'warn', valueHtml: fmtDuration(ms.p90), hint: 'p99 ' + fmtDuration(ms.p99) },
        { label: '最慢一次', icon: 'flame', tone: 'bad', valueHtml: fmtDuration(ms.max), hint: '慢于 60s 的占 ' + (hist.filter((h) => (h.low_ms || 0) >= 60000).reduce((s, h) => s + (Number(h.count) || 0), 0) / Math.max(1, counts) * 100).toFixed(1) + '%' },
        { label: '状态分布', icon: 'check-circle', valueHtml: (data.by_status || []).map((row) => esc(row.status) + ' ' + fmtNum(row.count)).join(' · ') || '—', hint: '中断 ' + fmtNum(runs.interrupted) + ' 次' },
      ]);

      mountChart('#al-hist', chartBase({
        tooltip: { trigger: 'axis', axisPointer: { type: 'shadow' }, valueFormatter: (value) => fmtNum(value) + ' 次' },
        xAxis: { type: 'category', data: hist.map((row) => row.bucket), axisLabel: { rotate: 24, fontSize: 10 } },
        yAxis: { type: 'value', name: '次' },
        series: [{
          type: 'bar', data: hist.map((row) => Number(row.count) || 0), barMaxWidth: 26,
          itemStyle: {
            borderRadius: [5, 5, 0, 0],
            color: (params) => {
              const colors = ['#3ddc97', '#7c9dff', '#7c9dff', '#fbbf4e', '#fbbf4e', '#fb7185', '#fb7185', '#fb7185', '#fb7185'];
              return colors[params.dataIndex] || '#7c9dff';
            },
          },
          label: { show: true, position: 'top', color: cssVar('--text-3', '#748094'), fontSize: 9.5, formatter: (p) => (counts ? Math.round(p.value / counts * 100) + '%' : '') },
        }],
      }), hist.map((row) => ({ label: row.bucket, value: row.count })));

      mountChart('#al-daily', chartBase({
        legend: { data: ['平均耗时', 'p90'] },
        xAxis: { type: 'category', data: daily.map((row) => String(row.date).slice(5)), boundaryGap: false },
        yAxis: { type: 'value', name: '秒', axisLabel: { formatter: (value) => (value / 1000).toFixed(0) + 's' } },
        tooltip: { trigger: 'axis', valueFormatter: (value) => fmtDuration(value) },
        series: [
          { name: '平均耗时', type: 'line', smooth: true, showSymbol: false, data: daily.map((row) => Number(row.avg_ms) || 0), lineStyle: { width: 2.2 }, itemStyle: { color: CHART_COLORS[0] }, areaStyle: { color: { type: 'linear', x: 0, y: 0, x2: 0, y2: 1, colorStops: [{ offset: 0, color: 'rgba(124,157,255,.24)' }, { offset: 1, color: 'rgba(124,157,255,0)' }] } } },
          { name: 'p90', type: 'line', smooth: true, showSymbol: false, data: daily.map((row) => Number(row.p90_ms) || 0), lineStyle: { width: 1.6, type: 'dashed' }, itemStyle: { color: CHART_COLORS[3] } },
        ],
      }), daily.map((row) => ({ label: row.date, value: row.avg_ms })));

      renderTable($('#al-slowest'), {
        stateKey: 'analytics-latency-slow',
        columns: [
          { key: 'run_id_masked', label: 'Run ID', mono: true, render: (row) => '<span class="mono">' + esc(row.run_id_masked) + '</span>' },
          { key: 'ms', label: '耗时', num: true, render: (row) => '<b class="tone-' + (Number(row.ms) > 60000 ? 'bad' : Number(row.ms) > 20000 ? 'warn' : 'ok') + '">' + fmtDuration(row.ms) + '</b>' },
          { key: 'status', label: '状态', width: '120px', render: (row) => '<span class="tag ' + (row.status === 'answered' ? 'tag-ok' : 'tag-warn') + '">' + esc(row.status) + '</span>' },
          { key: 'finished_at', label: '完成时间', width: '170px', hideSm: true, render: (row) => '<span class="mono">' + esc(row.finished_at) + '</span>' },
        ],
        rows: slowest,
        empty: { icon: 'check-circle', title: '没有慢请求', hint: '最近 ' + days + ' 天没有超过阈值的运行。' },
      });

      $('#al-meta').innerHTML =
        '<div class="stat-line"><span class="muted">数据源</span><span class="mono text-xs">' + esc(data.source || 'audit_log') + '</span></div>' +
        '<div class="stat-line"><span class="muted">开始 / 完成 / 配对</span><span class="mono">' + fmtNum(runs.started) + ' / ' + fmtNum(runs.finished) + ' / ' + fmtNum(runs.paired) + '</span></div>' +
        '<div class="stat-line"><span class="muted">最小 / 平均 / 最大</span><span class="mono">' + fmtDuration(ms.min) + ' / ' + fmtDuration(ms.avg) + ' / ' + fmtDuration(ms.max) + '</span></div>' +
        '<div class="stat-line"><span class="muted">p50 / p90 / p99</span><span class="mono">' + fmtDuration(ms.p50) + ' / ' + fmtDuration(ms.p90) + ' / ' + fmtDuration(ms.p99) + '</span></div>' +
        notice('info', esc(data.note || '耗时 = finished - started（同 run_id 配对）。')) +
        '<div class="row mt-12"><a class="btn btn-sm btn-ghost" href="#/logs">' + icon('terminal') + '错误日志</a>' +
        '<a class="btn btn-sm btn-ghost" href="#/tools">' + icon('wrench') + '工具审计</a></div>';

      guard(ctx, 'analytics-latency');
    },
  };

  /* ══ 6. 内容中心 ════════════════════════════════════════════════════════ */

  const CONTENT_TABS = [
    { id: 'library', label: '文库', icon: 'list' },
    { id: 'contests', label: '赛文', icon: 'chart' },
    { id: 'novel', label: '番茄', icon: 'file' },
    { id: 'typing', label: '打字排行', icon: 'trending' },
  ];

  VIEWS.content = {
    id: 'content',
    name: '内容中心',
    icon: 'book',
    skeleton: 'table',
    render: async (ctx) => {
      const tab = ctx.segments[1] || 'library';
      const activeTab = CONTENT_TABS.find((item) => item.id === tab) || CONTENT_TABS[0];
      const keyword = ctx.q('q', '');
      ctx.root.innerHTML = pageHead({
        eyebrow: 'CONTENT',
        title: '内容中心',
        icon: 'book',
        sub: '文库、赛文、番茄书架、打字排行 —— 一个入口，统一搜索与详情。',
        actions: [
          '<button class="btn btn-sm btn-ghost" id="content-shortcuts" type="button">' + icon('command') + '快捷键</button>',
        ],
      }) +
        tabsHtml(CONTENT_TABS.map((item) => ({ id: item.id, label: item.label, icon: item.icon })), activeTab.id) +
        '<div class="filters mb-12" style="margin-bottom:14px">' +
          '<div class="row" style="flex:1;min-width:0">' + icon('search') + '<input class="input" id="content-q" placeholder="搜索' + esc(activeTab.label) + '标题 / 内容关键词…" value="' + escAttr(keyword) + '" style="flex:1;min-width:160px"></div>' +
          '<button class="btn btn-sm btn-soft" id="content-search" type="button">' + icon('search') + '搜索</button>' +
          '<button class="btn btn-sm btn-ghost" id="content-clear" type="button">' + icon('close') + '清空</button>' +
        '</div>' +
        '<div id="content-body"><div class="load-inline">' + icon('loader', 'spin') + ' 加载中…</div></div>';

      $$('.tab[data-tab]').forEach((node) => {
        node.addEventListener('click', () => {
          const id = node.getAttribute('data-tab');
          go(hashFor('content', [id], new URLSearchParams(keyword ? { q: keyword } : {})));
        });
      });
      const input = $('#content-q');
      const doSearch = () => go(hashFor('content', [activeTab.id], new URLSearchParams(input && input.value.trim() ? { q: input.value.trim() } : {})));
      if (input) input.addEventListener('keydown', (event) => { if (event.key === 'Enter') doSearch(); });
      const searchBtn = $('#content-search');
      if (searchBtn) searchBtn.addEventListener('click', doSearch);
      const clearBtn = $('#content-clear');
      if (clearBtn) clearBtn.addEventListener('click', () => go(hashFor('content', [activeTab.id], {})));
      const shortcuts = $('#content-shortcuts');
      if (shortcuts) shortcuts.addEventListener('click', () => openShortcuts());

      const body = $('#content-body');
      if (activeTab.id === 'library') await renderContentLibrary(ctx, body, keyword);
      else if (activeTab.id === 'contests') await renderContentContests(ctx, body, keyword);
      else if (activeTab.id === 'novel') await renderContentNovel(ctx, body, keyword);
      else await renderContentTyping(ctx, body, keyword);
      guard(ctx, 'content');
    },
  };

  async function renderContentLibrary(ctx, body, keyword) {
    const limit = 24;
    const res = await probeApi(['/api/v2/library' + qs({ q: keyword, limit: limit, offset: 0 }), '/api/domain/library' + qs({ q: keyword, limit: limit })], { timeout: 20000 });
    if (!ctx.alive()) return;
    if (!res.ok) {
      body.innerHTML = pendingState({ title: '文库接口尚未就绪', hint: (res.error && res.error.message) || '等待 /api/v2/library', path: '/api/v2/library' });
      return;
    }
    const data = res.data || {};
    const items = data.items || (Array.isArray(data) ? data : []);
    const total = Number(data.total || items.length);
    if (!items.length) {
      body.innerHTML = emptyState({ icon: 'book', title: '没有匹配的文库内容', hint: keyword ? '关键词「' + keyword + '」没有命中。' : '文库接口返回空列表。' });
      return;
    }
    body.innerHTML = '<div class="row mb-12" style="justify-content:space-between">' +
      '<span class="muted-2 text-sm">共 <b class="tone-acc">' + fmtNum(total) + '</b> 篇 · 当前 ' + items.length + ' 篇' + (keyword ? ' · 关键词「' + esc(keyword) + '」' : '') + '</span>' +
      '<span class="row"><select class="select select-sm" id="lib-diff" style="width:auto"><option value="">全部难度</option><option>普</option><option>水</option><option>淼</option></select>' +
      '<button class="btn btn-sm btn-ghost" id="lib-export" type="button">' + icon('download') + 'CSV</button></span></div>' +
      '<div class="grid grid-3" id="lib-grid">' + items.map((row) => contentCard('文库', row.title || row.text_id, (row.category || '') + ' · ' + (row.difficulty || '难度未知') + ' · ' + fmtNum(row.char_count) + ' 字', row.primary_genre || row.category, row)).join('') + '</div>';

    const bindCards = () => {
      $$('#lib-grid .content-card').forEach((node, index) => {
        node.addEventListener('click', () => openLibraryDetail(items[index]));
      });
    };
    bindCards();
    const diffSelect = $('#lib-diff');
    if (diffSelect) {
      diffSelect.addEventListener('change', () => {
        const value = diffSelect.value;
        $$('#lib-grid .content-card').forEach((node, index) => {
          const show = !value || String(items[index].difficulty || '').indexOf(value) >= 0;
          node.style.display = show ? '' : 'none';
        });
      });
    }
    const exportBtn = $('#lib-export');
    if (exportBtn) exportBtn.addEventListener('click', () => exportCsv(items, ['text_id', 'title', 'category', 'difficulty', 'char_count', 'primary_genre'], 'fm-library'));
  }

  function contentCard(kicker, title, meta, tag, raw) {
    return '<article class="content-card">' +
      '<div class="content-kicker">' + icon('book') + esc(kicker) + '</div>' +
      '<div class="content-title">' + esc(title || '（无标题）') + '</div>' +
      '<div class="content-meta">' + (tag ? '<span class="tag tag-muted">' + esc(tag) + '</span>' : '') + esc(meta || '') + '</div>' +
      '<div class="content-snippet">' + esc(raw.snippet || raw.source || raw.relative_path || '点击查看详情') + '</div>' +
      '</article>';
  }

  async function openLibraryDetail(row) {
    Drawer.open({
      title: row.title || row.text_id || '文库详情',
      eyebrow: '文库 · 跟打素材',
      sub: (row.category || '') + ' · ' + (row.difficulty || '难度未知') + ' · ' + fmtNum(row.char_count) + ' 字',
      raw: row,
      wide: true,
      body: '<div class="load-inline">' + icon('loader', 'spin') + ' 读取正文…</div>',
      onMount: async (bodyEl) => {
        const textRes = await probeApi(['/api/v2/library/text' + qs({ id: row.text_id || row.id }), '/api/domain/library/text' + qs({ id: row.text_id || row.id })], { timeout: 20000 });
        if (!bodyEl.isConnected) return;
        if (!textRes.ok) {
          bodyEl.innerHTML = kvBlock([
            ['text_id', row.text_id || row.id, 'code'],
            ['分类', row.category, 'book'],
            ['难度', (row.difficulty || '') + (row.difficulty_score != null ? ' · ' + row.difficulty_score : ''), 'gauge'],
            ['字数', fmtNum(row.char_count), 'list'],
            ['来源', row.source || row.relative_path, 'link'],
            ['入库', row.added_at_text || fmtTime(row.added_at), 'clock'],
          ]) + notice('warn', '正文接口尚未就绪：' + esc((textRes.error && textRes.error.message) || '/api/v2/library/text')) + '<div class="mt-12">' + jsonView(row) + '</div>';
          return;
        }
        const data = textRes.data || {};
        const item = data.item || (data.data && data.data.item) || data;
        const text = item.content || data.text || data.content || item.body || '';
        bodyEl.innerHTML = kvBlock([
          ['text_id', row.text_id || row.id, 'code'],
          ['分类', item.category || row.category, 'book'],
          ['难度', (item.difficulty || row.difficulty) + (item.difficulty_score != null ? ' · ' + item.difficulty_score : ''), 'gauge'],
          ['字数', fmtNum(item.char_count || row.char_count), 'list'],
          ['流派', item.primary_genre || row.primary_genre, 'sparkles'],
          ['形态', item.form || row.form, 'file'],
        ]) + (text
          ? '<div class="section-title mt-16">正文（前 6000 字）</div><div class="transcript v3-text" style="white-space:pre-wrap;max-height:44vh;overflow:auto">' + esc(text.slice(0, 6000)) + '</div>' +
            '<div class="muted-2 text-xs mt-8">' + icon('info') + ' 全文 ' + fmtNum((text || '').length) + ' 字，此处截取前 6000 字。</div>'
          : notice('warn', '接口返回里没有正文文本：' + esc(data.message || '可能只支持按 text_id 查询，或文件已归档'))) +
          '<div class="section-title mt-16">原始字段</div>' + jsonView(row);
      },
    });
  }

  function kvBlock(items) {
    return '<div class="kv-grid">' + (items || []).map((item) =>
      '<div class="kv-item"><div class="k">' + icon(item[2] || 'info') + esc(item[0]) + '</div><div class="v">' + esc(item[1] == null ? '—' : item[1]) + '</div></div>').join('') + '</div>';
  }

  async function renderContentContests(ctx, body, keyword) {
    const res = await probeApi(['/api/v2/contests', '/api/domain/contests?limit=50'], { timeout: 25000 });
    if (!ctx.alive()) return;
    if (!res.ok) {
      body.innerHTML = pendingState({ title: '赛文接口尚未就绪', hint: (res.error && res.error.message) || '等待 /api/v2/contests', path: '/api/v2/contests' });
      return;
    }
    const data = res.data || {};
    const contestInfo = data.contests || {};
    const items = contestInfo.items || (Array.isArray(data) ? data : []);
    const today = data.today || {};
    const todayItems = data.today_items || [];
    if (keyword) {
      // 关键词模式：用 domain 赛文清单做过滤
      const filtered = items.filter((row) => (String(row.name) + ' ' + String(row.key) + ' ' + String(row.live_source)).toLowerCase().indexOf(keyword.toLowerCase()) >= 0);
      body.innerHTML = '<div class="row mb-12 muted-2 text-sm">找到 ' + filtered.length + ' 个匹配的赛文类型</div>' +
        (filtered.length ? '<div class="grid grid-3">' + filtered.map((row) => contentContestCard(row)).join('') + '</div>' : emptyState({ icon: 'chart', title: '没有匹配的赛文类型' }));
      bindContestCards(ctx, filtered);
      return;
    }
    body.innerHTML =
      '<div class="kpis">' +
      miniKpi('赛文类型', fmtNum(contestInfo.count || items.length), '来自 /api/v2/contests', 'chart', 'acc') +
      miniKpi('今日赛文', fmtNum(today.count || today.count === 0 ? (today.count || todayItems.length) : todayItems.length), '今日新增 ' + fmtNum(todayItems.length) + ' 条', 'calendar') +
      miniKpi('全站成绩', fmtNum(items.reduce((sum, row) => sum + (Number(row.scores) || 0), 0)), '参与人数合计 ' + fmtNum(items.reduce((sum, row) => sum + (Number(row.users) || 0), 0)), 'trending') +
      miniKpi('覆盖群', fmtNum(items.reduce((sum, row) => sum + (Number(row.group_count) || 0), 0)), '按群号统计的群赛文', 'groups', 'ok') +
      '</div>' +
      '<section class="card" style="margin-bottom:14px"><div class="card-head"><h3>' + icon('calendar') + '今日赛文 <span class="section-sub">' + esc(today.date || fmtTime(Date.now(), { dateOnly: true })) + '</span></h3>' +
      '<div class="card-head-actions"><button class="btn btn-sm btn-ghost" id="contest-export" type="button">' + icon('download') + 'CSV</button></div></div>' +
      '<div class="card-body" id="contest-today">' + (todayItems.length
        ? '<div class="table-wrap"><table class="table"><thead><tr><th>#</th><th>赛文</th><th>群</th><th class="num">字数</th><th>日期</th></tr></thead><tbody>' +
          todayItems.slice(0, 20).map((row, index) => '<tr><td class="idx">' + (index + 1) + '</td><td><span class="cell-truncate">' + esc(row.title || row.text_id) + '</span></td>' +
            '<td>' + esc(row.source_group || '—') + '</td><td class="num">' + fmtNum(row.char_count) + '</td><td class="mono">' + esc(row.competition_date || '—') + '</td></tr>').join('') +
          '</tbody></table></div>'
        : emptyState({ icon: 'calendar', title: '今天还没有赛文', hint: '今日赛文清单为空，等自动同步任务跑完后再看。' })) +
      '</div></section>' +
      '<div class="section-title">全部赛文类型 <span class="section-sub">点击查看群分布</span></div>' +
      '<div class="grid grid-3" id="contest-grid">' + items.map((row) => contentContestCard(row)).join('') + '</div>';

    bindContestCards(ctx, items);
    const exportBtn = $('#contest-export');
    if (exportBtn) exportBtn.addEventListener('click', () => exportCsv(todayItems, ['title', 'source_group', 'char_count', 'competition_date', 'text_id'], 'fm-contests-today'));
  }

  function contentContestCard(row) {
    const scopeTag = row.scope === 'group' ? '群赛文' : '全站赛文';
    return '<article class="content-card" data-contest-key="' + escAttr(row.key) + '">' +
      '<div class="content-kicker">' + icon('chart') + esc(scopeTag) + '</div>' +
      '<div class="content-title">' + esc(row.name || row.key) + '</div>' +
      '<div class="content-meta"><span class="tag tag-acc">' + fmtNum(row.scores) + ' 份成绩</span>' +
      '<span class="tag tag-muted">' + fmtNum(row.users) + ' 人</span>' +
      (row.group_count ? '<span class="tag tag-muted">' + fmtNum(row.group_count) + ' 个群</span>' : '') + '</div>' +
      '<div class="content-snippet">最近成绩：' + esc(row.last_score_date || '—') + '</div>' +
      '</article>';
  }

  function bindContestCards(ctx, items) {
    $$('#contest-grid .content-card, .content-card[data-contest-key]').forEach((node) => {
      const key = node.getAttribute('data-contest-key');
      const row = items.find((item) => String(item.key) === String(key));
      if (!row) return;
      node.addEventListener('click', () => openContestDetail(row));
    });
  }

  function openContestDetail(row) {
    Drawer.open({
      title: row.name || row.key,
      eyebrow: '赛文 · ' + (row.scope === 'group' ? '群赛文' : '全站赛文'),
      sub: fmtNum(row.scores) + ' 份成绩 · ' + fmtNum(row.users) + ' 人参与 · 最近 ' + (row.last_score_date || '—'),
      raw: row,
      body: kvBlock([
        ['key', row.key, 'key'],
        ['类型', row.type, 'chart'],
        ['成绩数', fmtNum(row.scores), 'list'],
        ['参与人数', fmtNum(row.users), 'members'],
        ['首次成绩', row.first_score_date, 'calendar'],
        ['最近成绩', row.last_score_date, 'calendar'],
        ['数据源', row.live_source || '—', 'link'],
      ]) +
        (row.groups && row.groups.length
          ? '<div class="section-title mt-16">群分布</div><div class="table-wrap"><table class="table"><thead><tr><th>群</th><th>群号</th><th class="num">成绩</th><th class="num">人数</th><th>最近</th></tr></thead><tbody>' +
            row.groups.map((g) => '<tr><td>' + esc(g.group_name) + '</td><td class="mono">' + esc(g.group_id) + '</td><td class="num">' + fmtNum(g.scores) + '</td><td class="num">' + fmtNum(g.users) + '</td><td class="mono">' + esc(g.last_score_date || '—') + '</td></tr>').join('') +
            '</tbody></table></div>'
          : '') +
        '<div class="row mt-16"><a class="btn btn-sm btn-ghost" href="#/contests">' + icon('chart') + '赛文图表页</a>' +
        '<button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(row).slice(0, 3200)) + '" type="button">' + icon('code') + 'JSON</button></div>',
    });
  }

  async function renderContentNovel(ctx, body, keyword) {
    const res = await probeApi(['/api/v2/novels' + qs({ q: keyword, limit: 60 })], { timeout: 20000 });
    if (!ctx.alive()) return;
    if (!res.ok) {
      body.innerHTML = pendingState({ title: '番茄接口尚未就绪', hint: (res.error && res.error.message) || '等待 /api/v2/novels', path: '/api/v2/novels' });
      return;
    }
    const data = res.data || {};
    let items = data.items || [];
    if (keyword) items = items.filter((row) => (String(row.title) + ' ' + String(row.author)).toLowerCase().indexOf(keyword.toLowerCase()) >= 0);
    if (!items.length) {
      body.innerHTML = emptyState({ icon: 'file', title: '书架里没有匹配的书', hint: keyword ? '关键词「' + keyword + '」没有命中。' : '番茄书架为空。' });
      return;
    }
    body.innerHTML = '<div class="kpis">' +
      miniKpi('书籍', fmtNum(data.count || items.length), '番茄书架', 'file', 'acc') +
      miniKpi('章节', fmtNum(data.chapters), '已入库章节', 'list') +
      miniKpi('阅读进度', fmtNum(data.progress), 'novel_progress 记录', 'trending') +
      miniKpi('成绩记录', fmtNum(data.records), 'novel_records 记录', 'chart', 'ok') +
      '</div>' +
      '<div class="table-wrap" id="novel-table"></div>';
    renderTable($('#novel-table'), {
      stateKey: 'content-novel', sortable: true,
      columns: [
        { key: 'title', label: '书名', render: (row) => '<b class="cell-truncate">' + esc(row.title || row.book_id) + '</b>' },
        { key: 'author', label: '作者', width: '130px', hideSm: true, render: (row) => esc(row.author || '—') },
        { key: 'total_chapters', label: '章节', num: true, render: (row) => fmtNum(row.total_chapters != null ? row.total_chapters : row.chapters) },
        { key: 'word_count', label: '字数', num: true, render: (row) => fmtNum(row.word_count) },
        { key: 'imported_at_text', label: '入库时间', width: '160px', hideSm: true, render: (row) => '<span class="mono">' + esc(row.imported_at_text || fmtTime(row.imported_at)) + '</span>' },
      ],
      rows: items,
      onRow: (row) => openNovelDetail(row),
    });
  }

  async function openNovelDetail(row) {
    Drawer.open({
      title: row.title || row.book_id,
      eyebrow: '番茄 · 书架',
      sub: (row.author || '') + ' · ' + fmtNum(row.total_chapters || row.chapters) + ' 章 · ' + fmtNum(row.word_count) + ' 字',
      raw: row,
      body: '<div class="load-inline">' + icon('loader', 'spin') + ' 读取章节…</div>',
      onMount: async (bodyEl) => {
        const res = await probeApi(['/api/v2/novel' + qs({ id: row.book_id || row.id })], { timeout: 20000 });
        if (!bodyEl.isConnected) return;
        const data = res.ok ? res.data : {};
        const chapters = (data && (data.chapters || data.items)) || [];
        bodyEl.innerHTML = kvBlock([
          ['book_id', row.book_id || row.id, 'key'],
          ['作者', row.author, 'user'],
          ['章节', fmtNum(row.total_chapters || row.chapters), 'list'],
          ['字数', fmtNum(row.word_count), 'chart'],
          ['入库', row.imported_at_text || fmtTime(row.imported_at), 'clock'],
        ]) +
          (chapters.length
            ? '<div class="section-title mt-16">章节（前 40 章）</div>' + '<div class="table-wrap"><table class="table"><thead><tr><th>章</th><th>标题</th><th class="num">字数</th></tr></thead><tbody>' +
              chapters.slice(0, 40).map((item) => '<tr><td class="idx">' + fmtNum(item.chapter_index != null ? item.chapter_index : item.index) + '</td><td class="cell-truncate">' + esc(item.title || '—') + '</td><td class="num">' + fmtNum(item.word_count) + '</td></tr>').join('') +
              '</tbody></table></div>'
            : notice('warn', '章节接口未返回数据：' + esc((res.error && res.error.message) || '可能只支持按 internal_id 查询'))) +
          '<div class="mt-16">' + jsonView(row) + '</div>';
      },
    });
  }

  async function renderContentTyping(ctx, body, keyword) {
    const res = await probeApi(['/api/v2/members/leaderboard' + qs({ days: 7, limit: 50 })], { timeout: 20000 });
    if (!ctx.alive()) return;
    if (!res.ok) {
      body.innerHTML = pendingState({ title: '打字排行接口尚未就绪', hint: (res.error && res.error.message) || '等待 /api/v2/members/leaderboard', path: '/api/v2/members/leaderboard' });
      return;
    }
    const data = res.data || {};
    let items = data.items || [];
    if (keyword) items = items.filter((row) => String(row.user_name || row.user_id).toLowerCase().indexOf(keyword.toLowerCase()) >= 0);
    body.innerHTML = '<div class="kpis">' +
      miniKpi('参与人数', fmtNum(data.users), '最近 7 天', 'members', 'acc') +
      miniKpi('成绩条数', fmtNum(data.records), '覆盖 ' + fmtNum(data.active_days) + ' 天', 'list') +
      miniKpi('均速均值', fmtNum(data.avg_speed, 1) + '<small>字/分</small>', '全站平均', 'trending') +
      miniKpi('平均键准', data.avg_accuracy == null ? '—' : fmtNum(data.avg_accuracy, 1) + '<small>%</small>', '平均键率 ' + fmtNum(data.avg_key_rate, 2), 'check-circle', 'ok') +
      '</div>' +
      '<div class="table-wrap" id="typing-table"></div>';
    renderTable($('#typing-table'), {
      stateKey: 'content-typing', sortable: true,
      columns: [
        { key: 'rank', label: '#', width: '52px', num: true, render: (row) => '<span class="mono">' + fmtNum(row.rank) + '</span>' },
        { key: 'user_name', label: '成员', render: (row) => '<b>' + esc(row.user_name || row.user_id) + '</b>' },
        { key: 'avg_speed', label: '均速', num: true, render: (row) => '<b>' + fmtNum(row.avg_speed, 1) + '</b> <span class="text-xs muted-2">字/分</span>' },
        { key: 'best_speed', label: '最高均速', num: true, render: (row) => fmtNum(row.best_speed, 1) },
        { key: 'avg_accuracy', label: '键准', num: true, render: (row) => fmtNum(row.avg_accuracy, 1) + '%' },
        { key: 'count', label: '条数', num: true, hideSm: true, render: (row) => fmtNum(row.count) },
        { key: 'active_days', label: '活跃天数', num: true, hideSm: true, render: (row) => fmtNum(row.active_days) },
      ],
      rows: items,
      onRow: (row) => openJsonDrawer('打字成绩 · ' + (row.user_name || row.user_id), row, '来自 /api/v2/members/leaderboard'),
    });
  }

  function miniKpi(label, valueHtml, hint, iconName, tone) {
    return '<div class="kpi' + (tone ? ' tone-' + tone : '') + '">' +
      '<div class="kpi-top">' + (iconName ? icon(iconName) : '') + '<span>' + esc(label) + '</span></div>' +
      '<div class="kpi-value">' + valueHtml + '</div>' +
      (hint ? '<div class="kpi-hint">' + esc(hint) + '</div>' : '') + '</div>';
  }

  /* ══ 7. 运维监控 v4（卡片化） ══════════════════════════════════════════ */

  const OPS_TABS = [
    { id: 'overview', label: '概览', icon: 'activity' },
    { id: 'images', label: '镜像', icon: 'database' },
    { id: 'logs', label: '日志', icon: 'terminal' },
    { id: 'cron', label: '定时任务', icon: 'clock' },
    { id: 'rollback', label: '回滚', icon: 'refresh' },
  ];

  VIEWS.ops = {
    id: 'ops',
    name: '运维监控',
    icon: 'activity',
    skeleton: 'chart',
    crumb: (route) => {
      const tab = route.segments[1];
      const meta = OPS_TABS.find((item) => item.id === tab);
      return meta ? meta.label : null;
    },
    render: async (ctx) => {
      const tab = ctx.segments[1] || 'overview';
      const activeTab = OPS_TABS.find((item) => item.id === tab) || OPS_TABS[0];
      ctx.root.innerHTML = pageHead({
        eyebrow: 'OPS',
        title: '运维监控',
        icon: 'activity',
        sub: '主机资源、容器健康、镜像回滚、日志查看与定时任务 —— 全部卡片化，只读展示 + 安全操作。',
        actions: [
          '<button class="btn btn-sm btn-ghost" id="ops-refresh" type="button">' + icon('refresh') + '刷新</button>',
          '<button class="btn btn-sm btn-ghost" id="ops-deploy" type="button">' + icon('server') + '部署面板</button>',
        ],
      }) +
        tabsHtml(OPS_TABS.map((item) => ({ id: item.id, label: item.label, icon: item.icon })), activeTab.id) +
        '<div id="ops-body"><div class="load-inline">' + icon('loader', 'spin') + ' 加载中…</div></div>';

      $$('.tabs .tab').forEach((node) => {
        node.addEventListener('click', () => go(hashFor('ops', [node.getAttribute('data-tab')], {})));
      });
      const refreshBtn = $('#ops-refresh');
      if (refreshBtn) refreshBtn.addEventListener('click', () => route());
      const deployBtn = $('#ops-deploy');
      if (deployBtn) deployBtn.addEventListener('click', () => go('#/deploy'));

      const body = $('#ops-body');
      if (activeTab.id === 'overview') await renderOpsOverview(ctx, body);
      else if (activeTab.id === 'images') await renderOpsImages(ctx, body);
      else if (activeTab.id === 'logs') await renderOpsLogs(ctx, body);
      else if (activeTab.id === 'cron') await renderOpsCron(ctx, body);
      else await renderOpsRollback(ctx, body);
      guard(ctx, 'ops');
    },
  };

  async function renderOpsOverview(ctx, body, force) {
    /* 慢接口体验（P1-2）：先出骨架 + 进度条；ops/metrics 恒 ~3s、代理排队时更久，最长等 35s，不提前判失败 */
    body.innerHTML = '<div class="ops-stats" style="margin-bottom:14px">' +
      ['主机负载', '内存占用', '磁盘占用', '容器健康', '运行时长', '采样时间'].map((label) =>
        '<div class="ops-stat"><div class="k">' + esc(label) + '</div><div class="v"><span class="sk" style="display:block;height:20px;width:72%"></span></div></div>').join('') +
      '</div>' + runProgressHtml('ops-metrics-progress', '正在读取主机指标（宿主代理排队时最长 35 秒）…', '宿主机代理串行执行 docker stats，冷启动或排队时会明显变慢；这里不会提前判定失败。');
    const stopMetricsTimer = startElapsedTimer(body, 'ops-metrics-progress');
    const metricsPath = '/api/v2/ops/metrics' + qs(force ? { force: 1 } : {});
    const deadline = (promise, ms, fallback) => Promise.race([
      promise.catch(() => fallback),
      new Promise((resolve) => setTimeout(() => resolve(fallback), ms)),
    ]);
    const results = await Promise.all([
      deadline(probeApi([metricsPath], { timeout: 42000 }), 35000, { ok: false, pending: true, path: metricsPath, error: new ApiError('主机指标接口超时（>35s）', { status: 0 }) }),
      deadline(probeApi(['/api/v2/health/history?days=7'], { timeout: 20000 }), 15000, { ok: false, error: new ApiError('健康历史接口超时', { status: 0 }) }),
      deadline(probeApi(['/api/errors/stats'], { timeout: 20000 }), 15000, { ok: false }),
    ]);
    stopMetricsTimer();
    if (!ctx.alive()) return;
    const statsRes = results[0];
    const healthRes = results[1];
    const errorsRes = results[2];
    const stats = statsRes.ok ? (statsRes.data || {}) : {};
    const host = stats.host || {};
    const latestHealth = (healthRes.ok && healthRes.data && healthRes.data.latest) || {};
    const metricsContainer = stats.docker ? Object.assign({}, stats.docker.container || {}, stats.docker.stats || {}) : null;
    const containers = (latestHealth.containers && latestHealth.containers.length)
      ? latestHealth.containers.map((row) => (metricsContainer && row.name === metricsContainer.name ? Object.assign({}, row, metricsContainer) : row))
      : (metricsContainer ? [metricsContainer] : []);
    const health = healthRes.ok ? (healthRes.data || {}) : {};
    const points = health.points || [];

    if (!statsRes.ok && !healthRes.ok) {
      body.innerHTML = pendingState({
        title: '运维接口暂时不可用',
        hint: '主机运维代理（ops_agent）可能离线或排队超时。可点「强制刷新」再试；这条请求最长等 35s，超出才判定失败。',
        path: '/api/v2/ops/metrics',
        raw: statsRes.error || healthRes.error,
      });
      return;
    }

    const mem = host.mem || {};
    const disk = (host.disk && host.disk['/']) || {};
    const load = host.load || {};
    /* /api/v2/ops/metrics 超时/失败时，用 health.jsonl 的采样点兜底 */
    const memPercent = mem.used_percent != null ? mem.used_percent : latestHealth.mem_used_percent;
    const memHuman = mem.used_human ? mem.used_human + ' / ' + mem.total_human : (latestHealth.mem_used_human || '');
    const diskPercent = disk.percent != null ? disk.percent : latestHealth.disk_percent;
    const diskHuman = disk.free_human ? '剩余 ' + disk.free_human + ' / ' + disk.total_human : (latestHealth.disk_free_human ? '剩余 ' + latestHealth.disk_free_human : '');
    const load1 = load['1m'] != null ? load['1m'] : latestHealth.load1;
    const healthyContainers = containers.filter((c) => c.running && (!c.health || c.health === 'healthy'));
    body.innerHTML =
      (statsRes.ok ? '' : notice('warn', '<b>主机指标接口暂时不可用</b>（' + esc((statsRes.error && statsRes.error.message) || 'ops_agent 无响应') + '），已用健康历史采样兜底显示；可点右侧「强制刷新」绕过缓存重试。')) +
      '<div class="row" style="justify-content:space-between;margin-bottom:10px;flex-wrap:wrap;gap:8px">' +
        '<div class="row">' + cacheBadgeHtml(statsRes, stats) +
          '<span class="muted-2 text-xs mono">' + esc(stats.source || 'host-ops-agent') +
            (stats.duration_ms != null ? ' · duration ' + fmtNum(stats.duration_ms) + 'ms' : '') +
            (stats.request_id ? ' · ' + esc(String(stats.request_id).slice(0, 18)) : '') + '</span></div>' +
        '<div class="row">' +
          '<button class="btn btn-sm btn-ghost" id="ops-force-refresh" type="button" title="?force=1 绕过 15s 缓存">' + icon('zap') + '强制刷新</button>' +
          '<a class="btn btn-sm btn-ghost" href="#/ops/logs">' + icon('terminal') + '容器日志</a>' +
        '</div>' +
      '</div>' +
      '<div class="ops-stats" style="margin-bottom:14px">' +
        miniStat('主机负载 1m', fmtNum(load1, 2), (host.cpu_count ? host.cpu_count + ' 核 · 5m ' + fmtNum(load['5m'], 2) + ' · 15m ' + fmtNum(load['15m'], 2) : ''), 'activity', Number(load1) > (host.cpu_count || 4) ? 'warn' : 'ok') +
        miniStat('内存占用', memPercent == null ? '—' : fmtNum(memPercent, 1) + '<small>%</small>', memHuman, 'cpu', Number(memPercent) > 90 ? 'bad' : '') +
        miniStat('磁盘占用', diskPercent == null ? '—' : fmtNum(diskPercent, 1) + '<small>%</small>', diskHuman, 'database', Number(diskPercent) > 90 ? 'bad' : '') +
        miniStat('容器健康', healthyContainers.length + '<small>/' + containers.length + '</small>', containers.filter((c) => c.health === 'healthy').length + ' 个 healthy', 'server', containers.length && healthyContainers.length === containers.length ? 'ok' : 'warn') +
        miniStat('运行时长', esc(host.uptime_text || '—'), host.hostname || '', 'clock') +
        miniStat('采样时间', esc(stats.collected_at_text || host.now_text || '—'), stats.source || statsRes.path || '', 'refresh') +
      '</div>' +
      '<div class="grid grid-wide" style="margin-bottom:14px">' +
        '<section class="card"><div class="card-head"><h3>' + icon('server') + '容器状态</h3>' +
          '<div class="card-head-actions"><span class="tag ' + (health.latest && health.latest.bot && health.latest.bot.running ? 'tag-ok' : 'tag-muted') + '">' + (health.latest && health.latest.bot ? esc(health.latest.bot.health || health.latest.bot.state || '') : '未采样') + '</span>' +
          '<button class="btn btn-sm btn-ghost" id="ops-json" type="button">' + icon('code') + 'JSON</button></div></div>' +
          '<div class="card-body" id="ops-containers"><div class="table-wrap"><table class="table"><thead><tr><th>容器</th><th>镜像</th><th>状态</th><th class="num">CPU</th><th class="num">内存</th><th class="num hide-sm">PIDs</th></tr></thead><tbody>' +
          containers.map((row) => '<tr><td><b>' + esc(row.name) + '</b></td>' +
            '<td><span class="mono text-xs cell-truncate" title="' + escAttr(row.image) + '">' + esc(truncate(row.image, 44)) + '</span></td>' +
            '<td>' + tagForStatus(row) + '</td>' +
            '<td class="num mono">' + esc(row.cpu_percent || '—') + '</td>' +
            '<td class="num mono">' + esc(row.mem_usage || row.mem_percent || '—') + '</td>' +
            '<td class="num mono hide-sm">' + esc(row.pids || '—') + '</td></tr>').join('') +
          '</tbody></table></div></div></section>' +
        '<section class="card"><div class="card-head"><h3>' + icon('heart-pulse') + '健康历史</h3></div>' +
          '<div class="card-body" id="ops-health">' + (points.length ? timelineHtml(points.slice(-4).reverse()) : placeholderCard('还没有健康检查点', '运维代理每 5 分钟写一次 /data/health.jsonl。', '/api/v2/health/history')) + '</div></section>' +
      '</div>' +
      '<div class="grid grid-2">' +
        '<section class="card"><div class="card-head"><h3>' + icon('gauge') + '资源占用</h3></div><div class="card-body">' +
          '<div class="v3-meter-label"><span>内存</span><span class="v3-meter-value">' + fmtNum(memPercent, 1) + '%</span></div><div class="bar-track"><div class="bar-fill ' + (Number(memPercent) > 90 ? 'bad' : 'ok') + '" style="width:' + Math.min(100, Number(memPercent) || 0) + '%"></div></div>' +
          '<div class="v3-meter-label mt-12"><span>磁盘 /</span><span class="v3-meter-value">' + fmtNum(diskPercent, 1) + '%</span></div><div class="bar-track"><div class="bar-fill ' + (Number(diskPercent) > 90 ? 'bad' : 'warn') + '" style="width:' + Math.min(100, Number(diskPercent) || 0) + '%"></div></div>' +
          '<div class="v3-meter-label mt-12"><span>负载 1m</span><span class="v3-meter-value">' + fmtNum(load1, 2) + ' / ' + fmtNum(host.cpu_count || 1) + ' 核</span></div><div class="bar-track"><div class="bar-fill" style="width:' + Math.min(100, Math.round((Number(load1) || 0) / (host.cpu_count || 4) * 100)) + '%"></div></div>' +
          '</div></section>' +
        '<section class="card"><div class="card-head"><h3>' + icon('alert') + '最近错误</h3><div class="card-head-actions"><a class="btn btn-sm btn-ghost" href="#/logs">日志</a></div></div>' +
          '<div class="card-body">' + ((errorsRes.ok && errorsRes.data && errorsRes.data.items || []).slice(0, 6).map((row) =>
            '<div class="stat-line"><span class="muted-2 text-xs mono">' + esc(fmtTime(row.at || row.occurred_at, { timeOnly: true }) || '—') + '</span>' +
            '<span class="cell-truncate" style="flex:1;margin:0 8px">' + esc(truncate(row.message || row.detail || row.component || '', 70)) + '</span>' +
            '<span class="tag tag-warn">' + esc(row.component || row.kind || 'err') + '</span></div>').join('') || '<div class="muted-2 text-sm">最近没有错误记录</div>') + '</div></section>' +
      '</div>';

    const jsonBtn = $('#ops-json');
    if (jsonBtn) jsonBtn.addEventListener('click', () => openJsonDrawer('运维指标原始响应', stats, '/api/v2/ops/metrics'));
    const forceBtn = $('#ops-force-refresh');
    if (forceBtn) forceBtn.addEventListener('click', () => {
      toast('已请求强制刷新（?force=1）', 'info', { timeout: 1600 });
      renderOpsOverview(ctx, body, true);
    });
  }

  function tagForStatus(row) {
    if (!row.running) return '<span class="tag tag-bad">' + icon('x-circle') + '未运行</span>';
    if (row.health === 'healthy') return '<span class="tag tag-ok">' + icon('check') + 'healthy</span>';
    if (row.health) return '<span class="tag tag-warn">' + esc(row.health) + '</span>';
    return '<span class="tag tag-muted">' + esc(truncate(row.status || 'running', 18)) + '</span>';
  }

  async function renderOpsImages(ctx, body) {
    const res = await probeApi(['/api/v2/ops/images'], { timeout: 20000 });
    if (!ctx.alive()) return;
    const data = res.ok ? (res.data || {}) : {};
    const images = data.images || [];
    const warming = !!data.warming;
    body.innerHTML =
      '<div class="row" style="justify-content:space-between;margin-bottom:12px">' +
        '<div class="row"><span class="tag ' + (res.ok ? 'tag-ok' : 'tag-warn') + '">' + esc(data.source || (res.ok ? '缓存' : '未就绪')) + '</span>' +
        (warming ? '<span class="tag tag-warn">' + icon('loader') + '采集中（后台刷新中）</span>' : '') +
        '<span class="muted-2 text-sm">共 ' + fmtNum(images.length) + ' 个镜像</span></div>' +
        '<button class="btn btn-sm btn-ghost" id="ops-images-refresh" type="button">' + icon('refresh') + '刷新</button>' +
      '</div>' +
      '<div class="card"><div class="card-body flush"><div class="table-wrap"><table class="table"><thead><tr><th>镜像</th><th>ID</th><th class="num">大小</th><th class="hide-sm">创建时间</th><th>操作</th></tr></thead><tbody>' +
      (images.length ? images.map((row) => {
        const isRollback = String(row.image || '').indexOf('fm-cosmobot:runtime-') === 0;
        return '<tr><td><span class="mono cell-truncate" title="' + escAttr(row.image) + '">' + esc(row.image) + '</span></td>' +
          '<td class="mono">' + esc(row.id || '—') + '</td><td class="num mono">' + esc(row.size || '—') + '</td>' +
          '<td class="hide-sm mono text-xs">' + esc(row.created || '—') + '</td>' +
          '<td>' + (isRollback ? '<button class="btn btn-sm btn-ghost" data-rollback-image="' + escAttr(row.image) + '" type="button">' + icon('refresh') + '回滚</button>' : '<span class="muted-2 text-xs">基础镜像</span>') + '</td></tr>';
      }).join('') : '<tr><td colspan="5"><div class="empty" style="padding:26px"><b>镜像列表还没准备好</b><p>' + esc(data.note || '运维代理的镜像缓存正在预热，稍后点刷新。') + '</p></div></td></tr>') +
      '</tbody></table></div></div></div>' +
      notice('info', '只允许回滚到 <code class="inline">fm-cosmobot:runtime-*</code> 前缀的镜像；回滚会重启机器人容器，请在确认弹窗里输入镜像名。');

    const refreshBtn = $('#ops-images-refresh');
    if (refreshBtn) refreshBtn.addEventListener('click', () => route());
    $$('[data-rollback-image]').forEach((node) => node.addEventListener('click', () => askRollback(node.getAttribute('data-rollback-image'), () => route())));
  }

  async function askRollback(image, onDone) {
    const ok = await confirmAction({
      title: '回滚机器人到指定镜像',
      detail: '将停止当前容器并用镜像 ' + image + ' 重新创建，健康检查失败会自动恢复旧容器。',
      danger: true,
      confirmLabel: '确认回滚',
      requireText: image,
    });
    if (!ok) return;
    const toastRef = toast('正在提交回滚请求…', 'info', { timeout: 0, title: '回滚' });
    try {
      const response = await apiPost('/api/v2/ops/rollback', { image: image }, { timeout: 45000 });
      toastRef.close();
      const data = unwrap(response) || {};
      toast('回滚请求已入队：' + (data.request_id || data.id || '已提交') + '，结果可在镜像页刷新查看', 'ok', { timeout: 6000 });
      openJsonDrawer('回滚结果', data, '/api/v2/ops/rollback');
      if (onDone) setTimeout(onDone, 1200);
    } catch (err) {
      toastRef.close();
      toast('回滚失败：' + (err.message || err), 'bad', { timeout: 6000 });
    }
  }

  async function renderOpsLogs(ctx, body) {
    const tail = ctx.n('tail', 200);
    const grep = ctx.q('grep', '');
    body.innerHTML =
      '<div class="filters mb-12" style="margin-bottom:12px">' +
        '<select class="select select-sm" id="ops-tail" style="width:auto">' + [50, 100, 200, 500].map((n) => '<option value="' + n + '"' + (Number(tail) === n ? ' selected' : '') + '>最近 ' + n + ' 行</option>').join('') + '</select>' +
        '<input class="input input-sm" id="ops-grep" placeholder="grep 关键词（如 error / 776227233）" value="' + escAttr(grep) + '" style="max-width:260px">' +
        '<button class="btn btn-sm btn-soft" id="ops-logs-apply" type="button">' + icon('filter') + '应用</button>' +
        '<label class="switch" style="margin-left:auto"><input type="checkbox" id="ops-logs-auto"><span class="track"></span><span class="text-sm">自动刷新 10s</span></label>' +
        '<button class="btn btn-sm btn-ghost" id="ops-logs-force" type="button" title="?force=1 绕过缓存">' + icon('zap') + '强制刷新</button>' +
        '<button class="btn btn-sm btn-ghost" id="ops-logs-refresh" type="button">' + icon('refresh') + '刷新</button>' +
      '</div>' +
      '<div class="card"><div class="card-head"><h3>' + icon('terminal') + 'fm-cosmobot 容器日志</h3>' +
        '<div class="card-head-actions"><span id="ops-log-cache"></span><span class="muted-2 text-xs" id="ops-log-meta">加载中…</span></div></div>' +
        '<div class="card-body"><div id="ops-log-view"></div></div></div>' +
      notice('info', '日志通过宿主机运维代理执行 <code class="inline">docker logs fm-cosmobot</code> 获取，最多 500 行，只读。冷启动可能 20–30 秒，页面最长等 45 秒；「强制刷新」会带 ?force=1 绕过缓存。');

    let autoTimer = 0;
    const load = async (forceFetch) => {
      const view = $('#ops-log-view');
      const meta = $('#ops-log-meta');
      const cacheNode = $('#ops-log-cache');
      if (view) view.innerHTML = runProgressHtml('ops-log-progress', '正在读取容器日志（冷启动 20–30 秒，最长 45 秒）…', '宿主机代理需要执行 docker logs，排队/冷启动时会慢；等待期间不判定失败。');
      const stopTimer = startElapsedTimer(body, 'ops-log-progress');
      const res = await probeApi(['/api/v2/ops/logs' + qs({ tail: $('#ops-tail') ? $('#ops-tail').value : tail, grep: $('#ops-grep') ? $('#ops-grep').value : '', force: forceFetch ? 1 : '' })], { timeout: 50000 });
      stopTimer();
      if (!ctx.alive()) return;
      if (!view) return;
      if (!res.ok) {
        view.innerHTML = pendingState({ title: '容器日志暂不可用', hint: (res.error && res.error.message) || '运维代理离线或接口未就绪；可点「强制刷新」再试。', path: '/api/v2/ops/logs', raw: res.error });
        if (cacheNode) cacheNode.innerHTML = '<span class="tag tag-warn">' + icon('x-circle') + '读取失败</span>';
        return;
      }
      const data = res.data || {};
      const lines = data.lines || [];
      if (cacheNode) cacheNode.innerHTML = cacheBadgeHtml(res, data);
      const metaParts = [];
      if (data.total_lines != null) metaParts.push('总行 ' + fmtNum(data.total_lines));
      metaParts.push('返回 ' + lines.length + ' 行');
      if (data.grep_matched) metaParts.push('命中 ' + fmtNum(data.grep_matched));
      if (data.source) metaParts.push(String(data.source));
      if (data.duration_ms != null) metaParts.push(fmtNum(data.duration_ms) + 'ms');
      if (data.request_id) metaParts.push(String(data.request_id).slice(0, 18));
      if (meta) meta.textContent = metaParts.join(' · ');
      view.innerHTML = lines.length
        ? '<pre class="v3-log">' + lines.map((line) => {
          const text = typeof line === 'string' ? line : (line.text || '');
          const tone = /error|exception|traceback|failed|panic/i.test(text) ? 'err' : /warn/i.test(text) ? 'warn' : '';
          return '<span class="v3-log-line ' + tone + '">' + esc(text) + '</span>';
        }).join('') + '</pre>'
        : emptyState({ icon: 'terminal', title: '没有日志行', hint: '容器可能刚重启，或 grep 关键词没有命中。' });
    };
    const apply = () => go(hashFor('ops', ['logs'], new URLSearchParams({ tail: $('#ops-tail') ? $('#ops-tail').value : String(tail), grep: $('#ops-grep') ? $('#ops-grep').value : '' })));
    const applyBtn = $('#ops-logs-apply');
    if (applyBtn) applyBtn.addEventListener('click', apply);
    const refreshBtn = $('#ops-logs-refresh');
    if (refreshBtn) refreshBtn.addEventListener('click', () => load(false));
    const forceBtn = $('#ops-logs-force');
    if (forceBtn) forceBtn.addEventListener('click', () => {
      toast('已请求强制刷新（?force=1）', 'info', { timeout: 1600 });
      load(true);
    });
    const auto = $('#ops-logs-auto');
    if (auto) auto.addEventListener('change', () => {
      if (auto.checked) {
        autoTimer = setInterval(() => { if (!document.hidden) load(false); }, 10000);
        ctx.cleanup(() => clearInterval(autoTimer));
        toast('日志自动刷新已开启（10 秒）', 'ok', { timeout: 1800 });
      } else {
        clearInterval(autoTimer);
        toast('日志自动刷新已关闭', 'info', { timeout: 1400 });
      }
    });
    await load();
  }

  async function renderOpsCron(ctx, body) {
    body.innerHTML = runProgressHtml('ops-cron-progress', '正在读取宿主机 crontab（可能需要 5–20 秒）…', '宿主机代理串行执行 crontab -l，排队时会慢；等待期间不判定失败。');
    const stopCronTimer = startElapsedTimer(body, 'ops-cron-progress');
    const res = await probeApi(['/api/v2/ops/cron'], { timeout: 50000 });
    stopCronTimer();
    if (!ctx.alive()) return;
    if (!res.ok) {
      body.innerHTML = pendingState({ title: '定时任务暂不可用', hint: (res.error && res.error.message) || '运维代理离线或 crontab 接口未就绪。', path: '/api/v2/ops/cron', raw: res.error });
      return;
    }
    const data = res.data || {};
    const entries = data.entries || [];
    const lines = data.lines || [];
    body.innerHTML =
      '<div class="ops-stats" style="margin-bottom:14px">' +
        miniStat('生效任务', fmtNum(entries.length || lines.length), '宿主机 crontab', 'clock', 'acc') +
        miniStat('写入能力', data.writable || data.can_set ? '可写' : '只读', '为避免误操作，后台默认只读展示', 'lock') +
        miniStat('读取耗时', fmtNum(data.elapsed_ms) + ' ms', data.request_id || '', 'refresh') +
      '</div>' +
      '<div class="card"><div class="card-head"><h3>' + icon('clock') + 'crontab 任务</h3><div class="card-head-actions"><span class="muted-2 text-xs">只读</span></div></div>' +
      '<div class="card-body">' + (entries.length
        ? '<div class="v3-matrix-wrap"><table class="v3-matrix"><thead><tr><th>#</th>' + Object.keys(entries[0]).map((key) => '<th>' + esc(key) + '</th>').join('') + '</tr></thead><tbody>' +
          entries.map((row, index) => '<tr><td class="idx">' + (index + 1) + '</td>' + Object.keys(entries[0]).map((key) => '<td class="mono text-xs">' + esc(truncate(row[key], 180)) + '</td>').join('') + '</tr>').join('') +
          '</tbody></table></div>'
        : lines.length
          ? '<pre class="v3-log">' + lines.map((line) => '<span class="v3-log-line">' + esc(line) + '</span>').join('') + '</pre>'
          : emptyState({ icon: 'clock', title: 'crontab 是空的', hint: '宿主机没有配置定时任务。' })) +
      '</div></div>' +
      notice('info', '后台不会改写宿主机 crontab；如需调整请走部署脚本或人工登录服务器。');
  }

  async function renderOpsRollback(ctx, body) {
    const [imagesRes, metricsRes] = await Promise.all([
      probeApi(['/api/v2/ops/images'], { timeout: 20000 }),
      probeApi(['/api/v2/ops/metrics'], { timeout: 20000 }),
    ]);
    if (!ctx.alive()) return;
    const images = (imagesRes.ok && imagesRes.data && imagesRes.data.images) || [];
    const metrics = (metricsRes.ok && metricsRes.data) || {};
    const running = (metrics.docker && metrics.docker.container) || {};
    const rollbackImages = images.filter((row) => String(row.image || '').indexOf('fm-cosmobot:runtime-') === 0);
    body.innerHTML =
      '<div class="ops-stats" style="margin-bottom:14px">' +
        miniStat('当前运行镜像', '<span class="mono" style="font-size:11px">' + esc(truncate(running.image || '未知', 40)) + '</span>', running.started_at ? '启动于 ' + esc(fmtTime(running.started_at)) : '', 'server') +
        miniStat('可回滚镜像', fmtNum(rollbackImages.length), 'fm-cosmobot:runtime-* 前缀', 'database', 'acc') +
        miniStat('回滚通道', rollbackImages.length ? 'POST /api/v2/ops/rollback' : '暂无可用镜像', '失败会自动恢复旧容器', 'refresh', rollbackImages.length ? 'ok' : 'warn') +
      '</div>' +
      '<div class="card"><div class="card-head"><h3>' + icon('refresh') + '选择要回滚的镜像</h3><div class="card-head-actions">' +
        '<button class="btn btn-sm btn-ghost" id="rollback-metrics" type="button">' + icon('eye') + '查看容器</button></div></div>' +
      '<div class="card-body"><div class="table-wrap"><table class="table"><thead><tr><th>镜像</th><th class="num">大小</th><th class="hide-sm">创建时间</th><th>操作</th></tr></thead><tbody>' +
      (rollbackImages.length ? rollbackImages.slice(0, 60).map((row) => '<tr><td><span class="mono cell-truncate">' + esc(row.image) + '</span></td><td class="num mono">' + esc(row.size || '—') + '</td>' +
        '<td class="hide-sm mono text-xs">' + esc(row.created || '—') + '</td>' +
        '<td><button class="btn btn-sm btn-danger" data-rollback-image="' + escAttr(row.image) + '" type="button">' + icon('refresh') + '回滚</button></td></tr>').join('')
        : '<tr><td colspan="4"><div class="empty" style="padding:26px"><b>没有可回滚镜像</b><p>镜像缓存可能正在预热，稍后刷新。</p></div></td></tr>') +
      '</tbody></table></div></div></div>' +
      notice('warn', '回滚属于危险操作：会重启 <code class="inline">fm-cosmobot</code> 容器。确认弹窗需要输入完整镜像名，提交后由宿主机运维代理执行，并做 60 秒健康检查。' + (rollbackImages.length > 60 ? '（当前共 ' + fmtNum(rollbackImages.length) + ' 个可回滚镜像，列表只展示最近 60 个）' : '')) +
      '<div class="row mt-12"><button class="btn btn-sm btn-ghost" id="rollback-history" type="button">' + icon('list') + '最近回滚记录（审计日志）</button></div>' +
      '<div id="rollback-history-box" class="mt-12"></div>';

    const metricsBtn = $('#rollback-metrics');
    if (metricsBtn) metricsBtn.addEventListener('click', () => openJsonDrawer('容器与主机指标', metrics, '/api/v2/ops/metrics'));
    const historyBtn = $('#rollback-history');
    if (historyBtn) historyBtn.addEventListener('click', async () => {
      const box = $('#rollback-history-box');
      if (!box) return;
      box.innerHTML = '<div class="load-inline">' + icon('loader', 'spin') + ' 读取审计日志…</div>';
      const logsRes = await probeApi(['/api/logs?limit=50'], { timeout: 15000 });
      if (!ctx.alive() || !box.isConnected) return;
      const rows = (logsRes.ok && (logsRes.data || logsRes.raw || {}).items) || [];
      const rollbacks = rows.filter((row) => String(row.action || '').indexOf('rollback') >= 0 || String(row.collection || '') === 'ops');
      box.innerHTML = rollbacks.length
        ? '<div class="table-wrap"><table class="table"><thead><tr><th>动作</th><th>对象</th><th>操作人</th><th>时间</th></tr></thead><tbody>' +
          rollbacks.map((row) => '<tr><td>' + esc(row.action) + '</td><td class="mono cell-truncate">' + esc(row.item_id) + '</td><td>' + esc(row.actor || '—') + '</td><td class="mono">' + esc(fmtTime(row.at)) + '</td></tr>').join('') +
          '</tbody></table></div>'
        : emptyState({ icon: 'list', title: '没有回滚记录', hint: '审计日志里还没有 rollback 动作。' });
    });
    $$('[data-rollback-image]').forEach((node) => node.addEventListener('click', () => askRollback(node.getAttribute('data-rollback-image'), () => route())));
  }

  /* 对外暴露少量工具，方便控制台里调试 */
  window.FM_V4 = {
    version: '4.0.0',
    pages: ['dashboard', 'analytics-msg', 'analytics-groups', 'analytics-senders', 'analytics-latency', 'content', 'ops'],
    heatmapHtml: heatmapHtml,
    openShortcuts: openShortcuts,
  };
})();
