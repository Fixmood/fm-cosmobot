/* ══════════════════════════════════════════════════════════════════════════
 * pages-qq.js · 「QQ 登录」页（#/qq）
 * 把 snowluma 的 QQ 扫码登录界面搬进后台，同时提供状态卡与管理员重启操作。
 *
 * 依赖 console.js 公共 API：VIEWS / pageHead / card / kpiGrid / notice /
 *   pendingState / emptyState / confirmAction / toast / probeApi / apiPost /
 *   unwrap / icon / esc / fmtTime / ago / fmtNum / sleep / isReadonly …
 *
 * 后端接口（未就绪时占位 + 原因 + 重试，绝不白屏）：
 *   GET  /api/v2/qq/status   状态（snowluma / onebot / bot / groups / last_login_at / checked_at）
 *   GET  /api/v2/qq/screen   登录界面/二维码 PNG（Cache-Control: no-store）
 *   POST /api/v2/qq/refresh  重新抓图
 *   POST /api/v2/qq/restart  {target:"snowluma"|"bot"}（管理员）
 * ════════════════════════════════════════════════════════════════════════ */
(function () {
  'use strict';

  const STATUS_PATH = '/api/v2/qq/status';
  const SCREEN_PATH = '/api/v2/qq/screen';
  const REFRESH_PATH = '/api/v2/qq/refresh';
  const RESTART_PATH = '/api/v2/qq/restart';
  const AUTO_MS = 10000;
  const POLL_LIMIT_MS = 60000;

  function flagText(online) {
    return online ? '在线' : '离线';
  }

  /* 把不同形态的后端字段归一成页面需要的状态 */
  function normalizeStatus(raw) {
    const d = raw && typeof raw === 'object' ? raw : {};
    const snowluma = d.snowluma && typeof d.snowluma === 'object' ? d.snowluma : {};
    const onebot = d.onebot && typeof d.onebot === 'object' ? d.onebot : {};
    const bot = d.bot && typeof d.bot === 'object' ? d.bot : {};
    const snowText = String(snowluma.state || snowluma.status || '').toLowerCase();
    const snowUp = snowluma.running === true
      || /run|up|healthy|start/.test(snowText) && !/exit|stop|dead|error|fail/.test(snowText);
    const onebotInfo = onebot.listening;
    const onebotUp = typeof onebotInfo === 'boolean'
      ? onebotInfo
      : /true|run|listen|up|ok/.test(String(onebot.state || onebot.status || '').toLowerCase());
    const botText = String(bot.state || bot.status || '').toLowerCase();
    const botConnected = bot.connected_to_onebot === true
      || /connect|online|run|ok/.test(botText) && !/disconnect|offline|stop|dead/.test(botText);
    const groups = d.groups == null && d.group_count == null
      ? null
      : (Number(d.groups != null ? d.groups : d.group_count) || 0);
    const lastLogin = d.last_login_at || d.lastLoginAt || '';
    const checkedAt = d.checked_at || d.checkedAt || (d.screen && d.screen.mtime) || Date.now();
    const screen = d.screen && typeof d.screen === 'object' ? d.screen : {};
    const loggedIn = d.logged_in === true || (d.logged_in == null ? false : false);
    const onebotHttpPort = onebot.http_port != null
      ? onebot.http_port
      : (typeof onebot.http === 'number' ? onebot.http : 3000);
    const online = (loggedIn || (snowUp && onebotUp && botConnected));
    return {
      raw: d,
      snowluma: snowluma,
      snowlumaUp: snowUp,
      snowlumaText: snowluma.state || snowluma.status || (snowUp ? 'running' : 'unknown'),
      snowlumaStarted: snowluma.started_at || snowluma.startedAt || snowluma.started_at_text || '',
      snowlumaStartedText: snowluma.started_at_text || '',
      onebot: onebot,
      onebotUp: onebotUp,
      onebotPort: onebot.port != null ? onebot.port : 3001,
      onebotHttpPort: onebotHttpPort,
      onebotError: onebot.error || '',
      bot: bot,
      botConnected: botConnected,
      botText: bot.state || bot.status || (botConnected ? 'connected' : 'unknown'),
      botHealth: bot.health || '',
      groups: groups,
      lastLoginAt: lastLogin,
      checkedAt: checkedAt,
      loggedIn: loggedIn,
      uin: d.uin || '',
      screen: screen,
      offlineHints: Array.isArray(d.offline_hints) ? d.offline_hints : [],
      logLines: d.log_lines_scanned,
      online: online,
    };
  }

  function screenUrl(stamp) {
    return SCREEN_PATH + '?t=' + encodeURIComponent(String(stamp || Date.now()));
  }

  VIEWS.qq = {
    id: 'qq',
    name: 'QQ 登录',
    icon: 'message',
    skeleton: 'detail',
    render: async (ctx) => {
      const root = ctx.root;
      const state = {
        status: null,
        rawStatus: null,
        pending: false,
        error: null,
        loading: false,
        stamp: Date.now(),
        imageError: null,
        imageLoaded: false,
        expanded: false,
        busy: false,
        lastAction: null,
        autoTick: AUTO_MS / 1000,
      };

      root.innerHTML = pageHead({
        eyebrow: 'OPS',
        title: 'QQ 登录',
        icon: 'message',
        sub: 'snowluma QQ 扫码登录 / 掉线恢复：状态、登录二维码、刷新抓图与容器重启都在这一页。',
        actions: [
          '<button class="btn btn-sm btn-soft" id="qq-refresh" type="button">' + icon('refresh') + '刷新状态</button>',
          '<span class="tag tag-muted" id="qq-auto-tag">自动刷新 10s</span>',
          '<a class="btn btn-sm btn-ghost" href="#/ops" type="button">' + icon('activity') + '运维监控</a>',
        ],
      }) +
      '<div id="qq-alert" class="qq-alert"></div>' +
      '<div id="qq-status" class="qq-status"></div>' +
      '<div class="qq-layout">' +
        '<section class="card qq-screen-card">' +
          '<div class="card-head"><h3 id="qq-screen-title">' + icon('message') + '登录二维码</h3>' +
            '<div class="card-head-actions">' +
              '<span id="qq-screen-tag"></span>' +
              '<button class="btn btn-sm btn-ghost" id="qq-refresh-qr" type="button">' + icon('refresh') + '刷新二维码</button>' +
            '</div></div>' +
          '<div class="card-body"><div id="qq-screen-body"></div></div>' +
        '</section>' +
        '<div class="qq-side">' +
          '<section class="card"><div class="card-head"><h3>' + icon('info') + '扫码步骤</h3></div>' +
            '<div class="card-body"><div id="qq-steps-body"></div>' +
              '<div class="card-note mt-12">' + icon('camera') + ' 图片由 snowluma 实时抓取（/api/v2/qq/screen），二维码过期时点「刷新二维码」重新抓图。</div>' +
            '</div></section>' +
          '<section class="card"><div class="card-head"><h3>' + icon('wrench') + '管理员操作</h3></div>' +
            '<div class="card-body">' +
              '<div class="qq-actions">' +
                '<button class="btn btn-sm btn-danger" id="qq-restart-snowluma" type="button">' + icon('refresh') + '重启 snowluma</button>' +
                '<button class="btn btn-sm btn-danger" id="qq-restart-bot" type="button">' + icon('robot') + '重启 bot</button>' +
              '</div>' +
              '<div class="qq-action-progress" id="qq-restart-progress" hidden></div>' +
              '<div class="qq-action-log" id="qq-action-log"><div class="qq-log-row"><span class="qq-log-time">—</span><span class="qq-log-text">还没有执行过重启操作</span></div></div>' +
              '<div class="card-note mt-12">' + icon('shield') + ' 重启属于危险操作：需二次确认并输入目标名；只读账号按钮会自动置灰。重启后自动轮询状态直到恢复在线（最多 60 秒）。</div>' +
            '</div></section>' +
          '<section class="card"><div class="card-head"><h3>' + icon('list') + '状态详情</h3></div>' +
            '<div class="card-body"><div class="qq-detail-list" id="qq-detail"></div></div></section>' +
        '</div>' +
      '</div>';

      const alertRoot = $('#qq-alert', root);
      const statusRoot = $('#qq-status', root);
      const screenBody = $('#qq-screen-body', root);
      const screenTag = $('#qq-screen-tag', root);
      const screenTitle = $('#qq-screen-title', root);
      const detailRoot = $('#qq-detail', root);
      const logRoot = $('#qq-action-log', root);
      const progressRoot = $('#qq-restart-progress', root);
      const autoTag = $('#qq-auto-tag', root);

      /* ── 操作日志 ─────────────────────────────────────────────────── */
      function addLog(text, tone) {
        if (!logRoot) return;
        if (logRoot.querySelector('.qq-log-row') && logRoot.querySelector('.qq-log-row').textContent.indexOf('还没有执行过') >= 0) {
          logRoot.innerHTML = '';
        }
        const row = document.createElement('div');
        row.className = 'qq-log-row';
        row.setAttribute('data-tone', tone || 'info');
        const time = new Date();
        const pad = (n) => (n < 10 ? '0' + n : '' + n);
        row.innerHTML = '<span class="qq-log-time">' + pad(time.getHours()) + ':' + pad(time.getMinutes()) + ':' + pad(time.getSeconds()) + '</span>' +
          '<span class="qq-log-text">' + esc(text) + '</span>';
        logRoot.insertBefore(row, logRoot.firstChild);
        while (logRoot.children.length > 6) logRoot.removeChild(logRoot.lastChild);
      }

      function showProgress(text) {
        if (!progressRoot) return;
        progressRoot.hidden = false;
        progressRoot.innerHTML = '<div class="run-progress">' +
          '<div class="rp-head"><span class="rp-title mono">' + icon('loader', 'spin') + ' ' + esc(text) + '</span></div>' +
          '<div class="progress-track"><div class="progress-fill indeterminate"></div></div>' +
          '</div>';
      }

      function hideProgress() {
        if (!progressRoot) return;
        progressRoot.hidden = true;
        progressRoot.innerHTML = '';
      }

      /* ── 状态卡 ───────────────────────────────────────────────────── */
      function paintStatus() {
        if (!statusRoot) return;
        if (state.pending || state.error) {
          statusRoot.innerHTML = kpiGrid([
            { label: 'snowluma 容器', icon: 'server', valueHtml: '—', hint: state.pending ? '接口未就绪' : '读取失败' },
            { label: 'OneBot 监听', icon: 'link', valueHtml: '—', hint: '3001 端口' },
            { label: 'bot 连接 OneBot', icon: 'robot', valueHtml: '—', hint: '连接状态未知' },
            { label: '群数量', icon: 'groups', valueHtml: '—' },
            { label: '最后登录', icon: 'clock', valueHtml: '—' },
            { label: '最近检查', icon: 'refresh', valueHtml: '—' },
          ], { cols: 6 });
          return;
        }
        const s = state.status;
        if (!s) return;
        const snowTone = s.snowlumaUp ? 'ok' : 'bad';
        const oneTone = s.onebotUp ? 'ok' : 'bad';
        const botTone = s.botConnected ? 'ok' : 'bad';
        statusRoot.innerHTML = kpiGrid([
          {
            label: 'snowluma 容器', icon: 'server', tone: snowTone,
            valueHtml: s.snowlumaUp ? '运行中' : '未运行',
            hint: s.snowlumaStarted ? '启动于 ' + esc(fmtTime(s.snowlumaStarted)) : esc(truncateText(s.snowlumaText, 40)),
          },
          {
            label: 'OneBot 监听', icon: 'link', tone: oneTone,
            valueHtml: s.onebotUp ? '监听中' : '未监听',
            hint: '端口 ' + esc(s.onebotPort) + '（HTTP ' + esc(s.onebotHttpPort) + '）' + (s.onebotError ? ' · ' + esc(truncateText(s.onebotError, 30)) : ''),
          },
          {
            label: 'bot 连接 OneBot', icon: 'robot', tone: botTone,
            valueHtml: s.botConnected ? '已连接' : '未连接',
            hint: 'bot 状态：' + esc(truncateText(s.botText, 24)),
          },
          {
            label: '群数量', icon: 'groups',
            valueHtml: s.groups == null ? '—' : fmtNum(s.groups), unit: s.groups == null ? '' : '个',
            hint: s.groups == null ? '尚未上报群数量' : '已加入的群 / 会话',
          },
          {
            label: '最后登录', icon: 'clock',
            valueHtml: s.lastLoginAt ? '<span style="font-size:14px">' + esc(ago(s.lastLoginAt)) + '</span>' : '—',
            hint: s.lastLoginAt ? esc(fmtTime(s.lastLoginAt)) : '没有登录时间记录',
          },
          {
            label: '最近检查', icon: 'refresh',
            valueHtml: '<span style="font-size:14px">' + esc(ago(s.checkedAt)) + '</span>',
            hint: esc(fmtTime(s.checkedAt)),
          },
        ], { cols: 6 });
      }

      function truncateText(text, max) {
        const value = String(text == null ? '' : text);
        return value.length > max ? value.slice(0, max - 1) + '…' : value;
      }

      /* ── 顶部提示 ─────────────────────────────────────────────────── */
      function paintAlert() {
        if (!alertRoot) return;
        if (state.pending) {
          alertRoot.innerHTML = notice('warn', '<div><div class="qq-alert-title">QQ 状态接口尚未就绪</div>' +
            '<div class="qq-alert-desc">后端 <code class="inline">' + esc(STATUS_PATH) + '</code> 还没有返回数据（HTTP 404/501）。页面已按占位渲染，接口上线后会自动恢复；也可以点「刷新状态」重试。</div></div>' +
            '<div class="row mt-12"><button class="btn btn-sm btn-ghost" id="qq-alert-retry" type="button">' + icon('refresh') + '重试</button>' +
            '<a class="btn btn-sm btn-ghost" href="#/selftest" type="button">' + icon('shield-check') + '接口自检</a></div>');
        } else if (state.error) {
          alertRoot.innerHTML = notice('bad', '<div><div class="qq-alert-title">QQ 状态读取失败</div>' +
            '<div class="qq-alert-desc">' + esc(state.error.message || '未知错误') + '（' + esc(STATUS_PATH) + '）。可以点「刷新状态」重试，或去接口自检页确认后端状态。</div></div>' +
            '<div class="row mt-12"><button class="btn btn-sm btn-ghost" id="qq-alert-retry" type="button">' + icon('refresh') + '重试</button></div>');
        } else if (state.status && state.status.online) {
          alertRoot.innerHTML = notice('ok', '<div><div class="qq-alert-title">QQ 已在线</div>' +
            '<div class="qq-alert-desc">snowluma 运行中、OneBot 3001 监听中、bot 已连上 OneBot' +
            (state.status.lastLoginAt ? '；最后登录 ' + esc(ago(state.status.lastLoginAt)) : '') +
            '。FM 可正常收发消息，如需查看 QQ 客户端界面可展开右侧二维码区。</div></div>');
        } else if (state.status) {
          const s = state.status;
          const tone = (!s.snowlumaUp || !s.onebotUp) ? 'bad' : 'warn';
          alertRoot.innerHTML = notice(tone, '<div><div class="qq-alert-title">FM 当前离线，扫码即可上线</div>' +
            '<div class="qq-alert-desc">' + (s.snowlumaUp ? 'snowluma 容器运行中' : 'snowluma 容器未运行（可先在右侧「管理员操作」里重启）') +
            '；OneBot ' + (s.onebotUp ? '已监听 ' + esc(s.onebotPort) : '未监听') +
            '；bot ' + (s.botConnected ? '已连上 OneBot' : '尚未连上 OneBot') +
            '。用手机 QQ 扫描下方二维码即可恢复在线。</div></div>');
        } else {
          alertRoot.innerHTML = '';
        }
        const retry = $('#qq-alert-retry', alertRoot || root);
        if (retry) retry.addEventListener('click', (event) => { event.stopPropagation(); loadStatus(true); });
      }

      /* ── 登录界面 / 二维码 ────────────────────────────────────────── */
      function paintScreen() {
        if (screenBody) screenBody.innerHTML = '';
        if (screenTag) screenTag.innerHTML = '';
        if (!screenBody) return;
        const s = state.status;
        const online = !!(s && s.online);

        if (state.pending || state.error) {
          if (screenTitle) screenTitle.innerHTML = icon('image') + '登录二维码';
          screenBody.innerHTML = pendingState({
            title: '登录截图暂不可用',
            hint: state.pending
              ? '后端 /api/v2/qq/screen 尚未就绪（登录抓图接口 404/501）。接口上线后会自动恢复。'
              : '抓图失败：' + state.error.message,
            path: SCREEN_PATH,
          });
          const retry = $('#qq-screen-body .empty-actions [data-retry]', root);
          if (retry) retry.addEventListener('click', (event) => { event.stopPropagation(); state.stamp = Date.now(); loadStatus(true); });
          return;
        }

        if (online && !state.expanded) {
          if (screenTag) screenTag.innerHTML = '<span class="tag tag-ok">' + icon('check') + 'QQ 已在线</span>';
          if (screenTitle) screenTitle.innerHTML = icon('message') + '登录界面';
          screenBody.innerHTML = '<div class="qq-collapsed">' +
            '<div class="qq-collapsed-title">' + icon('check-circle') + 'QQ 已在线，无需扫码</div>' +
            '<p>FM 已经登录 QQ，本页无需再扫码。如需在后台查看 QQ 客户端界面（例如确认通知/群列表），可以展开实时截图。</p>' +
            '<button class="btn btn-sm btn-soft" id="qq-expand-screen" type="button">' + icon('eye') + '查看 QQ 界面</button>' +
            '</div>';
          const expand = $('#qq-expand-screen', screenBody);
          if (expand) expand.addEventListener('click', () => { state.expanded = true; state.imageError = null; state.stamp = Date.now(); paintScreen(); });
          return;
        }

        if (screenTag) {
          screenTag.innerHTML = online
            ? '<span class="tag tag-ok">' + icon('check') + 'QQ 已在线</span>'
            : '<span class="tag tag-warn">' + icon('alert') + '未登录 · 待扫码</span>';
        }
        if (screenTitle) screenTitle.innerHTML = icon('image') + (online ? 'QQ 客户端界面' : '登录二维码');
        const meta = '图片时间 ' + esc(fmtTime(state.stamp)) + (online ? ' · 已登录界面' : ' · 手机QQ → 扫一扫');
        screenBody.innerHTML =
          '<div class="qq-screen-frame">' +
            '<div class="qq-screen-loading" id="qq-screen-loading">' + icon('loader', 'spin') + ' 正在抓取登录界面…</div>' +
            '<img class="qq-screen-img" id="qq-screen-img" alt="' + (online ? 'QQ 客户端界面截图' : 'QQ 登录二维码截图') + '" src="' + escAttr(screenUrl(state.stamp)) + '">' +
          '</div>' +
          '<div class="qq-screen-meta">' + icon('camera') + meta +
            '<span class="spacer"></span>' +
            '<button class="btn btn-sm btn-ghost" id="qq-retry-screen" type="button">' + icon('refresh') + '重新抓图</button>' +
            (online ? '<button class="btn btn-sm btn-ghost" id="qq-collapse-screen" type="button">' + icon('chevron-up') + '收起</button>' : '') +
          '</div>' +
          (state.imageError ? '<div class="qq-screen-error">' + notice('bad', '<b>登录截图加载失败</b>：' + esc(state.imageError) +
            '<div class="row mt-8"><button class="btn btn-sm btn-ghost" id="qq-image-retry" type="button">' + icon('refresh') + '重试</button>' +
            '<a class="btn btn-sm btn-ghost" href="#/selftest">' + icon('shield-check') + '接口自检</a></div>') + '</div>' : '');

        const img = $('#qq-screen-img', screenBody);
        const loading = $('#qq-screen-loading', screenBody);
        if (img) {
          img.addEventListener('load', () => {
            state.imageLoaded = true;
            if (loading) loading.hidden = true;
            img.classList.add('is-loaded');
          });
          img.addEventListener('error', () => {
            state.imageLoaded = false;
            if (loading) loading.hidden = true;
            state.imageError = '抓图地址返回非图片或超时：' + screenUrl(state.stamp);
            paintScreen();
            /* 尽量从后端 JSON 里取真实原因展示（例如 500 v2 接口失败：...） */
            fetch(screenUrl(state.stamp), { credentials: 'same-origin', cache: 'no-store' })
              .then((res) => res.json().catch(() => null).then((data) => ({ res, data })))
              .then((payload) => {
                if (!ctx.alive()) return;
                const data = payload.data || {};
                if (data && data.error) state.imageError = '抓图接口返回 ' + payload.res.status + '：' + data.error;
                else if (!payload.res.ok) state.imageError = '抓图接口返回 HTTP ' + payload.res.status;
                paintScreen();
              })
              .catch(() => { /* 忽略探测失败 */ });
          });
        }
        const retry = $('#qq-retry-screen', screenBody) || $('#qq-image-retry', screenBody);
        if (retry) retry.addEventListener('click', (event) => { event.stopPropagation(); refreshScreen(true); });
        const collapse = $('#qq-collapse-screen', screenBody);
        if (collapse) collapse.addEventListener('click', () => { state.expanded = false; paintScreen(); });
      }

      /* ── 状态详情 ─────────────────────────────────────────────────── */
      function paintDetail() {
        if (!detailRoot) return;
        const s = state.status;
        const rows = [];
        rows.push(['数据源', '<span class="mono">' + esc(STATUS_PATH) + '</span>', '']);
        if (s) {
          rows.push(['snowluma 原文状态', esc(truncateText(s.snowlumaText, 80)), 'mono']);
          rows.push(['snowluma 启动时间', s.snowlumaStarted ? esc(fmtTime(s.snowlumaStarted)) : '—', 'mono']);
          rows.push(['OneBot 监听', s.onebotUp ? '是（端口 ' + esc(s.onebotPort) + '）' : '否（端口 ' + esc(s.onebotPort) + '）' + (s.onebotError ? '：' + esc(s.onebotError) : ''), '']);
          rows.push(['bot 原文状态', esc(truncateText(s.botText, 80)) + (s.botHealth ? '（health ' + esc(s.botHealth) + '）' : ''), 'mono']);
          rows.push(['登录状态', s.loggedIn ? '已登录' : '未登录' + (s.uin ? '（uin ' + esc(s.uin) + '）' : ''), '']);
          rows.push(['最近检查', esc(fmtTime(s.checkedAt)), 'mono']);
          if (s.screen && s.screen.available) {
            rows.push(['最近截图', (s.screen.bytes ? fmtNum(s.screen.bytes) + 'B' : '可用') +
              (s.screen.width && s.screen.height ? ' · ' + esc(s.screen.width) + '×' + esc(s.screen.height) : '') +
              (s.screen.mtime_text ? ' · ' + esc(s.screen.mtime_text) : (s.screen.age_seconds != null ? ' · ' + esc(Math.round(s.screen.age_seconds)) + 's 前' : '')), 'mono']);
          }
          if (s.logLines != null) rows.push(['日志扫描', fmtNum(s.logLines) + ' 行（snowluma 日志）', '']);
          rows.push(['截图接口', '<span class="mono">' + esc(SCREEN_PATH) + '</span>', '']);
        } else {
          rows.push(['状态', state.pending ? '接口未就绪' : '读取失败', '']);
          if (state.error) rows.push(['原因', esc(state.error.message || '未知错误'), '']);
        }
        detailRoot.innerHTML = rows.map((row) =>
          '<div class="qq-detail-row"><span class="qq-detail-label">' + esc(row[0]) + '</span>' +
          '<span class="qq-detail-value ' + (row[2] || '') + '">' + row[1] + '</span></div>').join('');
      }

      /* ── 扫码步骤 / 在线状态说明 ─────────────────────────────────── */
      function paintSteps() {
        const box = document.getElementById('qq-steps-body');
        if (!box) return;
        const s = state.status;
        if (s && s.online) {
          box.innerHTML = '<div class="qq-collapsed" style="padding:18px 14px">' +
            '<div class="qq-collapsed-title">' + icon('check-circle') + 'QQ 已在线</div>' +
            '<p>已经登录，无需再扫码。需要查看 QQ 客户端界面时，展开左侧的实时截图即可。</p>' +
            '</div>';
          return;
        }
        box.innerHTML = '<div class="qq-steps">' +
          '<div class="qq-step"><span class="qq-step-num">1</span><span class="qq-step-main"><span class="qq-step-title">打开手机 QQ</span><span class="qq-step-desc">确保手机与 FM 登录的是同一个 QQ 账号。</span></span></div>' +
          '<div class="qq-step"><span class="qq-step-num">2</span><span class="qq-step-main"><span class="qq-step-title">右上角「+」→ 扫一扫</span><span class="qq-step-desc">扫描左侧登录界面里的二维码。</span></span></div>' +
          '<div class="qq-step"><span class="qq-step-num">3</span><span class="qq-step-main"><span class="qq-step-title">手机上确认登录</span><span class="qq-step-desc">确认后本页状态会自动变为「QQ 已在线」，FM 恢复收发消息。</span></span></div>' +
          '</div>';
      }

      function paintAll() {
        paintStatus();
        paintAlert();
        paintScreen();
        paintSteps();
        paintDetail();
      }

      /* ── 数据加载 ─────────────────────────────────────────────────── */
      async function fetchStatus(force) {
        const res = await probeApi([STATUS_PATH], { timeout: 15000 });
        if (!ctx.alive()) return null;
        if (res.ok) {
          state.rawStatus = res.data || {};
          state.status = normalizeStatus(res.data);
          state.pending = false;
          state.error = null;
          return state.status;
        }
        state.rawStatus = null;
        state.status = null;
        state.pending = !!res.pending;
        state.error = res.error || new Error('QQ 状态接口不可用');
        if (force) {
          toast(state.pending ? 'QQ 状态接口尚未就绪（404/501）' : 'QQ 状态读取失败：' + state.error.message, state.pending ? 'warn' : 'bad');
        }
        return null;
      }

      async function loadStatus(manual) {
        if (state.loading) return;
        state.loading = true;
        if (manual) state.stamp = Date.now();
        try {
          await fetchStatus(!!manual);
          paintAll();
        } catch (err) {
          if (!ctx.alive()) return;
          state.pending = false;
          state.error = err;
          paintAll();
          if (manual) toast('刷新失败：' + (err.message || err), 'bad');
        } finally {
          state.loading = false;
        }
      }

      /* ── 刷新二维码（重新抓图） ──────────────────────────────────── */
      async function refreshScreen(manual) {
        const btn = $('#qq-refresh-qr', root);
        if (btn) { btn.disabled = true; btn.innerHTML = icon('loader', 'spin') + '抓取中…'; }
        try {
          const res = await apiPost(REFRESH_PATH, {}, { timeout: 30000 });
          const data = unwrap(res) || {};
          state.stamp = Date.now();
          state.imageError = null;
          state.expanded = state.status && state.status.online ? true : state.expanded;
          paintScreen();
          if (manual) toast('已重新抓取登录界面' + (data.bytes ? '（' + fmtNum(data.bytes) + ' 字节）' : ''), 'ok', { timeout: 2200 });
        } catch (err) {
          if (manual) toast('刷新登录界面失败：' + (err.message || err), 'bad', { timeout: 5000 });
          state.imageError = state.imageError || ('刷新接口失败：' + (err.message || err));
          paintScreen();
        } finally {
          if (btn) { btn.disabled = false; btn.innerHTML = icon('refresh') + '刷新二维码'; }
        }
      }

      /* ── 管理员：重启容器 + 轮询恢复 ─────────────────────────────── */
      function setBusy(busy) {
        state.busy = busy;
        ['#qq-restart-snowluma', '#qq-restart-bot'].forEach((selector) => {
          const btn = $(selector, root);
          if (btn) btn.disabled = busy;
        });
      }

      async function doRestart(target) {
        if (state.busy) return;
        const label = target === 'snowluma' ? 'snowluma 容器' : 'FM 机器人（bot）容器';
        const ok = await confirmAction({
          title: '重启 ' + label + '？',
          detail: '重启期间 QQ 会短暂离线，FM 无法收发消息。确认后会调用 ' + RESTART_PATH + ' 并自动轮询状态，最多等待 60 秒。',
          danger: true,
          confirmLabel: '确认重启',
          requireText: target,
        });
        if (!ok) return;
        const startedAt = Date.now();
        setBusy(true);
        addLog('提交重启请求：' + target + '（' + label + '）');
        showProgress('正在重启 ' + label + '…');
        try {
          const res = await apiPost(RESTART_PATH, { target: target }, { timeout: 30000 });
          const data = unwrap(res) || {};
          addLog('重启请求已受理' + (data.request_id ? '：' + data.request_id : data.message ? '：' + data.message : ''));
          toast('重启请求已受理，正在等待恢复…', 'ok', { timeout: 2400 });
        } catch (err) {
          addLog('重启失败：' + (err.message || err), 'bad');
          toast('重启失败：' + (err.message || err), 'bad', { timeout: 5000 });
          hideProgress();
          setBusy(false);
          return;
        }
        let recovered = false;
        let lastStatus = null;
        while (Date.now() - startedAt < POLL_LIMIT_MS && ctx.alive()) {
          await sleep(2000);
          if (!ctx.alive()) return;
          const status = await fetchStatus(false);
          lastStatus = status;
          paintStatus();
          paintAlert();
          paintDetail();
          const waited = Math.round((Date.now() - startedAt) / 1000);
          showProgress('等待恢复在线… ' + waited + 's / 60s');
          if (status && status.online) { recovered = true; break; }
        }
        hideProgress();
        setBusy(false);
        if (recovered) {
          const secs = Math.max(1, Math.round((Date.now() - startedAt) / 1000));
          addLog('已恢复在线，用时 ' + secs + 's', 'ok');
          toast('QQ 已恢复在线（用时 ' + secs + 's）', 'ok', { timeout: 4000 });
        } else {
          addLog('60 秒内未恢复在线（最后状态：' + (lastStatus ? (lastStatus.online ? '在线' : '离线') : '未知') + '），请查看 snowluma / bot 日志', 'warn');
          toast('等待超时：60 秒内未恢复在线，请查看日志', 'warn', { timeout: 6000 });
        }
        state.stamp = Date.now();
        state.expanded = false;
        paintScreen();
      }

      /* ── 事件绑定 ─────────────────────────────────────────────────── */
      const refreshBtn = $('#qq-refresh', root);
      if (refreshBtn) refreshBtn.addEventListener('click', () => loadStatus(true));
      const qrBtn = $('#qq-refresh-qr', root);
      if (qrBtn) {
        if (isReadonly()) {
          qrBtn.disabled = true;
          qrBtn.classList.add('is-admin-gated');
          qrBtn.setAttribute('title', '需要管理员权限（当前为只读账号）');
          qrBtn.setAttribute('aria-disabled', 'true');
        } else {
          qrBtn.addEventListener('click', () => refreshScreen(true));
        }
      }
      const restartSnow = $('#qq-restart-snowluma', root);
      if (restartSnow) restartSnow.addEventListener('click', () => doRestart('snowluma'));
      const restartBot = $('#qq-restart-bot', root);
      if (restartBot) restartBot.addEventListener('click', () => doRestart('bot'));

      /* 手动刷新时把二维码一起换一张（避免截图过期） */
      if (refreshBtn) refreshBtn.addEventListener('click', () => { state.stamp = Date.now(); paintScreen(); });

      /* ── 10 秒自动刷新（页面不可见时暂停） ─────────────────────────── */
      let tick = AUTO_MS / 1000;
      const timer = setInterval(() => {
        if (document.hidden || !ctx.alive()) return;
        if (state.busy) return;
        tick -= 1;
        if (tick <= 0) {
          tick = AUTO_MS / 1000;
          loadStatus(false);
          if (state.status && !state.status.online) { state.stamp = Date.now(); paintScreen(); }
        }
        if (autoTag) autoTag.textContent = '自动刷新 ' + tick + 's';
      }, 1000);
      ctx.cleanup(() => clearInterval(timer));
      document.addEventListener('visibilitychange', () => { tick = AUTO_MS / 1000; });

      /* 首次加载 + 骨架 */
      if (statusRoot) statusRoot.innerHTML = '<div class="sk-row"><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div></div>';
      if (screenBody) screenBody.innerHTML = '<div class="load-inline">' + icon('loader', 'spin') + ' 正在读取 QQ 登录状态…</div>';
      await loadStatus(false);

      /* 页面守卫：确保至少一个真实元素 */
      if (ctx.alive() && !$('#main .card, #main .kpi, #main .empty, #main .error-box')) {
        console.warn('[qq] page guard: no real element');
      }
    },
  };
})();
