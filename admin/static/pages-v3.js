'use strict';
/* ============================================================================
 * FM Console v3 — 扩展页面（配置 / 记忆 / 模型切换 / 能力矩阵 / 运维监控 /
 * 备份导出 / 自检 / 会话令牌 / 番茄·赛文·文库·打字·日报）
 *
 * 依赖 console.js（先加载）：VIEWS / NAV / NAV_INDEX / apiGet / apiPost /
 * unwrap / toast / openModal / confirmAction / Drawer / renderTable / card /
 * pageHead / kpiGrid / pendingState / errorState / emptyState / notice /
 * mountChart / chartBase / pickArray / esc / icon / fmtNum / fmtTime ...
 *
 * 约定：
 *   - 每个视图渲染结束调用 guardAssert()，至少断言 1 个真实元素，绝不白屏；
 *   - 后端 404/501 视为「接口未就绪」，展示路径 + 原因 + 重试，不用假数据；
 *   - 写操作一律 confirmAction，危险操作要求输入确认词；
 *   - 表格统一走 v3Table（排序 / 列显隐持久化 / 统一分页风格）。
 * ========================================================================== */

(function () {
  const V3 = {
    version: '3.0.0',
    assertions: {},
    lastErrors: [],
  };
  window.FM_V3 = V3;

  /* ── 1. 小工具 ──────────────────────────────────────────────────────── */

  const LS = {
    get(key, fallback) {
      try {
        const value = localStorage.getItem('fm-console-' + key);
        return value == null ? fallback : value;
      } catch (err) { return fallback; }
    },
    set(key, value) {
      try { localStorage.setItem('fm-console-' + key, String(value)); } catch (err) { /* 隐私模式 */ }
    },
    del(key) {
      try { localStorage.removeItem('fm-console-' + key); } catch (err) { /* 隐私模式 */ }
    },
  };

  function numOrNull(value) {
    if (value == null || value === '') return null;
    const n = Number(value);
    return isFinite(n) ? n : null;
  }

  function textOf(value, fallback) {
    if (value == null || value === '') return fallback == null ? '' : fallback;
    return String(value);
  }

  function clamp(n, min, max) {
    const value = Number(n);
    if (!isFinite(value)) return min;
    return Math.min(max, Math.max(min, value));
  }

  function percent(value) {
    const n = numOrNull(value);
    if (n == null) return null;
    return n <= 1 ? n * 100 : n;
  }

  function fmtPercent(value, digits) {
    const n = percent(value);
    if (n == null) return '—';
    return n.toFixed(digits == null ? 1 : digits) + '%';
  }

  function barHtml(value, tone) {
    const n = clamp(percent(value), 0, 100);
    const color = tone ? ' ' + tone : (n >= 90 ? ' bad' : n >= 75 ? ' warn' : ' ok');
    return '<div class="bar-track" style="min-width:70px"><div class="bar-fill' + color + '" style="width:' + n.toFixed(1) + '%"></div></div>';
  }

  function kvGrid(items) {
    return '<div class="kv-grid">' + (items || []).map((item) =>
      '<div class="kv-item"><div class="k">' + (item.icon ? icon(item.icon) : '') + esc(item.label || '') + '</div>' +
      '<div class="v' + (item.mono ? ' mono' : '') + '">' + (item.html == null ? esc(item.value == null ? '—' : item.value) : item.html) + '</div></div>').join('') + '</div>';
  }

  function codeTip(paths) {
    return '<div class="code-tip mono">' + (paths || []).map(function (path) { return esc(path); }).join('  ·  ') + '</div>';
  }

  function viewMeta(viewId) {
    return NAV_INDEX[viewId] || { name: viewId, id: viewId, section: '' };
  }

  function groupLabel(group) {
    if (!group) return '未知群';
    return textOf(group.name || group.display_name || group.group_name || group.group_id, '未知群');
  }

  /* ── 2. 接口探测：404/501 视为「未就绪」，其它错误暴露原因 ───────────── */

  function isPendingError(err) {
    if (!err) return false;
    const status = Number(err.status || 0);
    if (status === 404 || status === 501) return true;
    const message = String(err.message || '');
    if (status === 503 && /404|not found|not implemented|未知|未实现|未就绪|暂不可用/i.test(message)) return true;
    return false;
  }

  async function tryApi(paths, opts) {
    const list = Array.isArray(paths) ? paths : [paths];
    let lastError = null;
    let lastPath = list[0];
    for (let i = 0; i < list.length; i += 1) {
      const path = list[i];
      lastPath = path;
      try {
        const raw = await apiFetch(path, opts || {});
        return { ok: true, pending: false, data: unwrap(raw), raw: raw, path: path };
      } catch (err) {
        if (err && err.status === 401) throw err;
        lastError = err;
        if (!isPendingError(err)) {
          return { ok: false, pending: false, error: err, path: path };
        }
      }
    }
    return { ok: false, pending: true, error: lastError, path: lastPath };
  }

  /* domain 接口统一从 /api/domain/<name> 走（admin 已有通用代理） */
  function domainPaths(names) {
    return (Array.isArray(names) ? names : [names]).map(function (name) {
      return name.charAt(0) === '/' ? '/api/domain' + name : '/api/domain/' + name;
    });
  }

  function tryDomain(names, opts) {
    return tryApi(domainPaths(names), opts);
  }

  function fallbackNotice(source, target) {
    return notice('warn', '新接口 <span class="mono">' + esc(source) + '</span> 未就绪，本页已降级使用 <span class="mono">' + esc(target) + '</span> 的数据（可能缺少部分字段）。');
  }

  /* 未就绪 / 失败统一渲染：说明原因 + 重试 + 原始响应 */
  function renderApiFailure(ctx, result, cfg) {
    const config = cfg || {};
    const root = ctx.root;
    const meta = viewMeta(config.viewId || (ctx.route && ctx.route.viewId) || '');
    const paths = config.paths || (result && result.path ? [result.path] : []);
    const head = pageHead({
      title: config.title || meta.name || '接口未就绪',
      icon: config.icon || meta.icon || 'clock',
      sub: config.sub || '本页依赖的接口尚未就绪或暂不可用；页面保留占位与重试入口，不会白屏。',
      actions: [
        '<button class="btn btn-ghost btn-sm" data-retry="1" type="button">' + icon('refresh') + '重试</button>',
        '<a class="btn btn-ghost btn-sm" href="#/browse">' + icon('database') + '数据浏览器</a>',
        '<a class="btn btn-ghost btn-sm" href="#/selftest">' + icon('shield') + '接口自检</a>',
      ],
    });
    let body;
    if (result && result.pending) {
      body = pendingState({
        title: config.pendingTitle || '接口尚未就绪',
        hint: (config.hint ? config.hint + ' ' : '') + (paths.length ? '接口：' + paths.join(' / ') : ''),
        path: paths.join('  ·  '),
        raw: result.error && result.error.data ? result.error.data : null,
      }) + codeTip(paths);
    } else {
      const reason = result && result.error ? (result.error.message || String(result.error)) : '未知错误';
      body = errorState(result && result.error, 'v3-view-retry') +
        notice('bad', '接口返回失败：<b>' + esc(reason) + '</b>' +
          (result && result.error && result.error.status ? '（HTTP ' + result.error.status + '）' : '') +
          '<br>可先到「接口自检」定位是后端未就绪还是运行时报错。') + codeTip(paths);
    }
    root.innerHTML = head + body + (config.extraHtml || '');
    const retry = document.getElementById('v3-view-retry');
    if (retry) retry.addEventListener('click', function () { route(); });
    guardAssert(ctx, config.viewId);
  }

  /* ── 3. 渲染断言：每页至少 1 个真实元素，否则兜底提示（绝不白屏） ───── */

  const ASSERT_SELECTOR = [
    '.kpi', '.table tbody tr', 'canvas', '.empty', '.error-box', '.notice',
    '.kv-grid', '.chart', 'pre', '.v3-editor', '.v3-tree', '.v3-log', '.v3-matrix',
    '.v3-check-list', '.v3-text', '.v3-list', '.v3-session-list', '.v3-jsonfallback',
  ].join(', ');

  function guardAssert(ctx, viewId) {
    const root = ctx && ctx.root ? ctx.root : document.getElementById('main');
    const id = viewId || (ctx && ctx.route && ctx.route.viewId) || 'unknown';
    if (!root) return null;
    let hit = null;
    try { hit = root.querySelector(ASSERT_SELECTOR); } catch (err) { hit = null; }
    const info = { viewId: id, at: Date.now(), ok: !!hit };
    V3.assertions[id] = info;
    if (!hit) {
      root.innerHTML = pageHead({ title: '页面渲染异常（已兜底）', icon: 'alert', sub: '视图 ' + id + ' 没有渲染出可识别的内容，已显示本提示而不是白屏。' }) +
        '<div class="error-box"><div style="flex:1"><b>没有断言到真实元素</b>' +
        '<p>如果后端接口正常，请点重试；若持续出现，请到数据浏览器检查接口返回，并反馈给前端。</p>' +
        '<div class="err-actions"><button class="btn btn-sm btn-ghost" data-retry="1" type="button">' + icon('refresh') + '重试</button></div></div></div>';
      info.ok = false;
    }
    return info;
  }

  function assertSummary() {
    const rows = Object.keys(V3.assertions).map(function (key) { return { viewId: key, ok: V3.assertions[key].ok, at: V3.assertions[key].at }; });
    return rows;
  }
  V3.assertSummary = assertSummary;

  /* ── 4. 表格增强：排序 / 列显隐（本地持久化） ───────────────────────── */

  function loadTableState(key) {
    try {
      const raw = LS.get('tbl-' + key, '');
      if (!raw) return {};
      const parsed = JSON.parse(raw);
      return parsed && typeof parsed === 'object' ? parsed : {};
    } catch (err) { return {}; }
  }

  function saveTableState(key, state) {
    try { LS.set('tbl-' + key, JSON.stringify(state)); } catch (err) { /* ignore */ }
  }

  function v3Table(container, cfg) {
    if (!container) return null;
    const config = cfg || {};
    const stateKey = config.stateKey || 'generic';
    const initial = loadTableState(stateKey);
    const columns = (config.columns || []).filter(Boolean);
    if (!columns.length) return null;
    function labelOf(key) {
      const found = columns.find(function (col) { return col.key === key; });
      return found ? (found.label || found.key) : key;
    }
    let hidden = Array.isArray(initial.hidden) ? initial.hidden.filter(function (key) {
      return columns.some(function (col) { return col.key === key; });
    }) : [];
    let sortKey = columns.some(function (col) { return col.key === initial.sortKey; }) ? initial.sortKey : (config.defaultSort || '');
    let sortDir = initial.sortDir === 'desc' ? 'desc' : 'asc';
    let rows = Array.isArray(config.rows) ? config.rows.slice() : [];

    container.innerHTML = '';
    const bar = document.createElement('div');
    bar.className = 'tbl-toolbar v3-table-bar';
    const sortOptions = columns.map(function (col) {
      return '<option value="' + escAttr(col.key) + '"' + (col.key === sortKey ? ' selected' : '') + '>' + esc(col.label || col.key) + '</option>';
    }).join('');
    bar.innerHTML =
      '<span class="muted text-xs" data-v3-count></span>' +
      '<span class="spacer"></span>' +
      (config.toolbarHtml || '') +
      '<label class="row tight" style="gap:5px"><span class="muted text-xs">排序</span>' +
        '<select class="select select-sm" data-v3-sort>' + sortOptions + '</select></label>' +
      '<button class="btn btn-sm btn-ghost" data-v3-dir type="button">' + (sortDir === 'asc' ? '升序' : '降序') + '</button>' +
      '<button class="btn btn-sm btn-ghost" data-v3-cols type="button">' + icon('filter') + '列显隐</button>' +
      '<span class="muted-2 text-xs" data-v3-sortinfo></span>';
    container.appendChild(bar);
    const body = document.createElement('div');
    container.appendChild(body);

    function sortedRows() {
      if (!sortKey) return rows.slice();
      const key = sortKey;
      return rows.slice().sort(function (a, b) {
        const av = a ? a[key] : null;
        const bv = b ? b[key] : null;
        const an = Number(av);
        const bn = Number(bv);
        let cmp;
        if (av != null && bv != null && av !== '' && bv !== '' && isFinite(an) && isFinite(bn)) {
          cmp = an - bn;
        } else {
          cmp = String(av == null ? '' : av).localeCompare(String(bv == null ? '' : bv), 'zh-Hans-CN', { numeric: true });
        }
        return sortDir === 'desc' ? -cmp : cmp;
      });
    }

    function paint() {
      const countNode = bar.querySelector('[data-v3-count]');
      if (countNode) countNode.textContent = typeof config.countLabel === 'function' ? config.countLabel(rows.length) : rows.length + ' 行';
      const sortInfo = bar.querySelector('[data-v3-sortinfo]');
      if (sortInfo) sortInfo.textContent = sortKey ? '按「' + labelOf(sortKey) + '」' + (sortDir === 'asc' ? '升序' : '降序') : '';
      const visible = columns.filter(function (col) { return hidden.indexOf(col.key) < 0; });
      const holder = document.createElement('div');
      body.innerHTML = '';
      body.appendChild(holder);
      renderTable(holder, Object.assign({}, config, {
        columns: visible.map(function (col) { return Object.assign({}, col, { label: col.label || col.key }); }),
        rows: sortedRows(),
        stateKey: undefined,
      }));
    }

    function openColumnPicker() {
      const html = '<div class="v3-col-picker">' + columns.map(function (col) {
        return '<label class="check"><input type="checkbox" data-v3-col="' + escAttr(col.key) + '"' +
          (hidden.indexOf(col.key) < 0 ? ' checked' : '') + '><span>' + esc(col.label || col.key) + '</span></label>';
      }).join('') + '</div><div class="field-hint mt-8">至少保留一列；选择会保存在本机浏览器。</div>';
      openModal({
        title: '列显隐',
        desc: '勾选要显示的列',
        icon: 'filter',
        bodyHtml: html,
        buttons: [
          { label: '全选', variant: 'btn-ghost', onClick: function (modal) {
            modal.el.querySelectorAll('[data-v3-col]').forEach(function (input) { input.checked = true; });
          } },
          { label: '应用', variant: 'btn-solid', onClick: function (modal) {
            const checked = Array.prototype.slice.call(modal.el.querySelectorAll('[data-v3-col]'))
              .filter(function (input) { return input.checked; })
              .map(function (input) { return input.getAttribute('data-v3-col'); });
            if (!checked.length) { toast('至少保留一列', 'warn'); return; }
            hidden = columns.filter(function (col) { return checked.indexOf(col.key) < 0; }).map(function (col) { return col.key; });
            saveTableState(stateKey, { hidden: hidden, sortKey: sortKey, sortDir: sortDir });
            modal.close();
            paint();
          } },
        ],
      });
    }

    bar.addEventListener('change', function (event) {
      const select = event.target.closest('[data-v3-sort]');
      if (!select) return;
      sortKey = select.value;
      saveTableState(stateKey, { hidden: hidden, sortKey: sortKey, sortDir: sortDir });
      paint();
    });
    bar.addEventListener('click', function (event) {
      const dirBtn = event.target.closest('[data-v3-dir]');
      if (dirBtn) {
        sortDir = sortDir === 'asc' ? 'desc' : 'asc';
        dirBtn.textContent = sortDir === 'asc' ? '升序' : '降序';
        saveTableState(stateKey, { hidden: hidden, sortKey: sortKey, sortDir: sortDir });
        paint();
        return;
      }
      if (event.target.closest('[data-v3-cols]')) openColumnPicker();
    });

    paint();
    return {
      repaint: function (nextRows) {
        if (Array.isArray(nextRows)) rows = nextRows.slice();
        paint();
      },
      openColumnPicker: openColumnPicker,
    };
  }

  /* ── 5. 文件下载（CSV 导出） ─────────────────────────────────────────── */

  async function downloadFile(path, fallbackName) {
    const response = await fetch(path, { credentials: 'same-origin', cache: 'no-store' });
    if (!response.ok) {
      let message = 'HTTP ' + response.status;
      try {
        const text = await response.text();
        const data = JSON.parse(text);
        message = (data && (data.error || data.message)) || message;
      } catch (err) { /* 非 JSON 错误体 */ }
      throw new ApiError(message, { status: response.status, path: path });
    }
    const disposition = response.headers.get('content-disposition') || '';
    const match = /filename\*?=(?:UTF-8'')?["']?([^"';]+)/i.exec(disposition);
    let name = fallbackName;
    if (match && match[1]) {
      try { name = decodeURIComponent(match[1]); } catch (err) { name = match[1]; }
    }
    const blob = await response.blob();
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = name;
    document.body.appendChild(link);
    link.click();
    link.remove();
    setTimeout(function () { URL.revokeObjectURL(url); }, 5000);
    return name;
  }

  /* ── 6. 主题（深色 / 浅色） ─────────────────────────────────────────── */

  function currentTheme() {
    return document.documentElement.getAttribute('data-theme') === 'light' ? 'light' : 'dark';
  }

  function updateThemeButtons() {
    const mode = currentTheme();
    Array.prototype.forEach.call(document.querySelectorAll('[data-v3-theme-label]'), function (node) {
      node.textContent = mode === 'light' ? '浅色' : '深色';
    });
    const topBtn = document.getElementById('v3-theme');
    if (topBtn) {
      topBtn.title = mode === 'light' ? '当前浅色主题，点击切换深色' : '当前深色主题，点击切换浅色';
      topBtn.setAttribute('aria-label', topBtn.title);
    }
  }

  function applyTheme(mode, silent) {
    const value = mode === 'light' ? 'light' : 'dark';
    document.documentElement.setAttribute('data-theme', value);
    LS.set('theme', value);
    updateThemeButtons();
    if (!silent) toast('已切换到' + (value === 'light' ? '浅色' : '深色') + '主题', 'ok', { timeout: 1800 });
  }

  function toggleTheme() {
    applyTheme(currentTheme() === 'light' ? 'dark' : 'light');
  }

  function injectThemeButton() {
    const right = document.querySelector('.topbar-right');
    if (!right || document.getElementById('v3-theme')) return;
    const button = document.createElement('button');
    button.className = 'icon-btn';
    button.id = 'v3-theme';
    button.type = 'button';
    button.innerHTML = icon('moon');
    button.addEventListener('click', toggleTheme);
    const anchor = document.getElementById('palette-open');
    right.insertBefore(button, anchor || right.firstChild);
    updateThemeButtons();
  }

  /* ── 7. 导航扩展 + 命令面板扩展 + 设置页外观卡片 ────────────────────── */

  const EXTRA_SECTIONS = [
    {
      label: '配置与能力',
      items: [
        { id: 'config', name: '配置编辑器', icon: 'gear' },
        { id: 'memory', name: '记忆与人设', icon: 'book' },
        { id: 'mswitch', name: '模型切换', icon: 'cpu' },
        { id: 'capabilities', name: '群能力矩阵', icon: 'shield' },
      ],
    },
    {
      label: '运维中心',
      items: [
        { id: 'ops', name: '运维监控', icon: 'activity' },
        { id: 'backup', name: '备份与导出', icon: 'download' },
        { id: 'selftest', name: '接口自检', icon: 'shield' },
        { id: 'sessions', name: '会话与令牌', icon: 'key' },
      ],
    },
    {
      label: '运营图表',
      items: [
        { id: 'novel', name: '番茄阅读', icon: 'book' },
        { id: 'contests', name: '赛文图表', icon: 'chart' },
        { id: 'library', name: '文库浏览', icon: 'list' },
        { id: 'typing', name: '打字排行', icon: 'trending' },
        { id: 'reports', name: '群日报', icon: 'message' },
      ],
    },
  ];

  /* v4：导航分区由 console.js 的 NAV 统一编排（本文件只提供视图，不再改 NAV），
     这里只保证 NAV_INDEX 对每个 v3 视图都有回退信息。 */
  EXTRA_SECTIONS.forEach(function (section) {
    section.items.forEach(function (item) {
      if (!NAV_INDEX[item.id]) NAV_INDEX[item.id] = Object.assign({ section: section.label }, item);
    });
  });

  function defineView(view) {
    if (!view || !view.id) return;
    if (!NAV_INDEX[view.id]) NAV_INDEX[view.id] = Object.assign({ section: '' }, view);
    if (!view.skeleton) view.skeleton = 'default';
    VIEWS[view.id] = view;
  }

  /* 命令面板：补 v3 页面下的子视图与快捷操作 */
  if (typeof Palette !== 'undefined' && Palette && typeof Palette.staticItems === 'function') {
    const baseStaticItems = Palette.staticItems;
    Palette.staticItems = function () {
      const items = baseStaticItems.call(Palette);
      const extra = [
        { group: '页面 · 配置与能力', title: '配置编辑器 · config.toml', sub: '#/config', icon: 'gear', keywords: 'toml config 配置', run: function () { go('#/config'); } },
        { group: '页面 · 配置与能力', title: '记忆与人设 · 文件读写', sub: '#/memory', icon: 'book', keywords: 'memory persona 记忆 人设', run: function () { go('#/memory'); } },
        { group: '页面 · 配置与能力', title: '模型切换 · provider / effort', sub: '#/mswitch', icon: 'cpu', keywords: 'model provider effort 模型', run: function () { go('#/mswitch'); } },
        { group: '页面 · 配置与能力', title: '群能力矩阵', sub: '#/capabilities', icon: 'shield', keywords: 'capability 能力 开关 矩阵', run: function () { go('#/capabilities'); } },
        { group: '页面 · 运维中心', title: '运维监控 · 指标 / 镜像 / 回滚 / 定时', sub: '#/ops/metrics', icon: 'activity', keywords: 'ops metrics docker rollback cron 运维', run: function () { go('#/ops/metrics'); } },
        { group: '页面 · 运维中心', title: '运维监控 · 日志查看器', sub: '#/ops/logs', icon: 'terminal', keywords: 'log tail 日志 查看', run: function () { go('#/ops/logs'); } },
        { group: '页面 · 运维中心', title: '备份与导出', sub: '#/backup', icon: 'download', keywords: 'backup export csv 备份 导出', run: function () { go('#/backup'); } },
        { group: '页面 · 运维中心', title: '接口自检', sub: '#/selftest', icon: 'shield', keywords: 'selftest 自检 健康', run: function () { go('#/selftest'); } },
        { group: '页面 · 运维中心', title: '会话与令牌', sub: '#/sessions', icon: 'key', keywords: 'session token 令牌 revoke', run: function () { go('#/sessions'); } },
        { group: '页面 · 运营图表', title: '番茄阅读统计', sub: '#/novel', icon: 'book', keywords: 'novel 番茄 阅读', run: function () { go('#/novel'); } },
        { group: '页面 · 运营图表', title: '赛文成绩图表', sub: '#/contests', icon: 'chart', keywords: 'contest 赛文 图表', run: function () { go('#/contests'); } },
        { group: '页面 · 运营图表', title: '文库浏览', sub: '#/library', icon: 'list', keywords: 'library 文库 跟打', run: function () { go('#/library'); } },
        { group: '页面 · 运营图表', title: '打字排行', sub: '#/typing', icon: 'trending', keywords: 'typing 打字 排行 均速', run: function () { go('#/typing'); } },
        { group: '页面 · 运营图表', title: '群日报', sub: '#/reports', icon: 'message', keywords: 'report daily 日报', run: function () { go('#/reports'); } },
        { group: '快捷操作 · v3', title: '切换深色 / 浅色主题', sub: '当前 ' + (currentTheme() === 'light' ? '浅色' : '深色'), icon: 'moon', keywords: 'theme light dark 主题 浅色 深色', run: function () { toggleTheme(); } },
      ];
      return items.concat(extra);
    };
  }

  /* 设置页追加「外观」卡片（不重写原 render，只在尾部注入） */
  if (VIEWS.settings && typeof VIEWS.settings.render === 'function') {
    const baseSettingsRender = VIEWS.settings.render;
    VIEWS.settings.render = async function (ctx) {
      await baseSettingsRender.call(this, ctx);
      if (!ctx.alive()) return;
      const main = ctx.root;
      if (!main || document.getElementById('v3-appearance')) return;
      const cardHtml = card({ title: '外观与主题', icon: 'moon' },
        '<div class="setting-row"><div class="setting-main"><div class="setting-title">主题</div>' +
          '<div class="setting-desc">深色为默认控制台主题；浅色适合亮环境，偏好保存在本机浏览器。</div></div>' +
          '<div class="seg" id="v3-appearance">' +
            '<button type="button" data-v3-theme-set="dark"' + (currentTheme() === 'dark' ? ' class="active"' : '') + '>深色</button>' +
            '<button type="button" data-v3-theme-set="light"' + (currentTheme() === 'light' ? ' class="active"' : '') + '>浅色</button>' +
          '</div></div>' +
        '<div class="setting-row"><div class="setting-main"><div class="setting-title">当前主题</div>' +
          '<div class="setting-desc">与登录页独立；图表颜色固定深色配色，浅色下仍可读。</div></div>' +
          '<span class="tag tag-info" data-v3-theme-label>' + (currentTheme() === 'light' ? '浅色' : '深色') + '</span></div>' +
        '<div class="setting-row"><div class="setting-main"><div class="setting-title">页面渲染断言</div>' +
          '<div class="setting-desc">每个页面渲染后会断言至少 1 个真实元素（表格行 / 图表 / 空状态）；异常会自动兜底而不是白屏。</div></div>' +
          '<button class="btn btn-sm btn-ghost" id="v3-assert-view" type="button">' + icon('shield') + '查看断言</button></div>');
      main.insertAdjacentHTML('beforeend', '<div class="mt-16">' + cardHtml + '</div>');
      main.addEventListener('click', function (event) {
        const button = event.target.closest('[data-v3-theme-set]');
        if (button) {
          applyTheme(button.getAttribute('data-v3-theme-set'));
          main.querySelectorAll('[data-v3-theme-set]').forEach(function (node) {
            node.classList.toggle('active', node === button);
          });
          return;
        }
        if (event.target.closest('#v3-assert-view')) {
          const rows = assertSummary();
          const body = rows.length
            ? rows.map(function (row) {
              return '<div class="stat-line"><span class="dot" data-tone="' + (row.ok ? 'ok' : 'bad') + '"></span>' +
                '<span>' + esc(row.viewId) + '</span><span class="spacer"></span>' +
                '<span class="muted-2 text-xs">' + esc(fmtTime(row.at, { noSeconds: true })) + '</span>' +
                '<span class="tag ' + (row.ok ? 'tag-ok' : 'tag-bad') + '">' + (row.ok ? '已断言' : '未断言') + '</span></div>';
            }).join('')
            : emptyState({ icon: 'shield', title: '本次会话还没有页面断言记录', hint: '打开几个页面后回来看，或点「接口自检」运行全量检查。' });
          openJsonDrawer('页面渲染断言', rows.length ? rows : { notes: '还没有断言记录' });
        }
      });
    };
  }

  /* ── 8. 页面：配置编辑器（config.toml） ─────────────────────────────── */

  function lineNumbers(text) {
    const lines = String(text == null ? '' : text).split('\n').length;
    const out = [];
    for (let i = 1; i <= lines; i += 1) out.push(i);
    return out.join('\n');
  }

  function bindEditor(area, gutter, onSave) {
    if (!area || !gutter) return;
    const sync = function () {
      gutter.textContent = lineNumbers(area.value);
      gutter.scrollTop = area.scrollTop;
    };
    area.addEventListener('input', sync);
    area.addEventListener('scroll', function () { gutter.scrollTop = area.scrollTop; });
    area.addEventListener('keydown', function (event) {
      if (event.key === 'Tab') {
        event.preventDefault();
        const start = area.selectionStart;
        const end = area.selectionEnd;
        const value = area.value;
        area.value = value.slice(0, start) + '  ' + value.slice(end);
        area.selectionStart = area.selectionEnd = start + 2;
        sync();
        return;
      }
      if ((event.ctrlKey || event.metaKey) && (event.key === 's' || event.key === 'S')) {
        event.preventDefault();
        if (typeof onSave === 'function') onSave();
      }
    });
    sync();
  }

  defineView({
    id: 'config',
    name: '配置编辑器',
    icon: 'gear',
    skeleton: 'default',
    async render(ctx) {
      const result = await tryApi(['/api/v2/config/toml'], { timeout: 20000 });
      if (!ctx.alive()) return;
      if (!result.ok) {
        renderApiFailure(ctx, result, {
          title: '配置编辑器',
          icon: 'gear',
          paths: ['GET /api/v2/config/toml', 'POST /api/v2/config/toml'],
          hint: '配置编辑依赖 v3 后端接口（读取原文 + sha256 / 保存前备份 + TOML 校验）。就绪前可先用旧版后台 /classic 的配置页。',
          extraHtml: '<div class="notice mt-16">' + icon('info') + '<div>保存行为：后端先备份到 <span class="mono">/runtime/config.toml.bak-admin-&lt;时间&gt;</span>，用 tomllib 校验语法，写后回读 sha256；前端二次确认并展示返回的备份路径与生效提示。</div></div>',
        });
        return;
      }
      const data = result.data || {};
      const content = textOf(data.content != null ? data.content : (data.toml != null ? data.toml : (data.text != null ? data.text : (data.raw_text || ''))));
      const sha = textOf(data.sha256 || data.sha || '');
      const bytes = numOrNull(data.bytes != null ? data.bytes : (data.size != null ? data.size : content.length)) || 0;
      const lineCount = numOrNull(data.lines) || (content ? content.split('\n').length : 0);
      const filePath = textOf(data.path || data.file || 'config.toml');
      const valid = data.valid !== false;
      const parseError = textOf(data.parse_error || '');
      const restartHint = textOf(data.restart_required_hint || '');
      const configBackups = Array.isArray(data.backups) ? data.backups : [];
      const topSections = Array.isArray(data.top_sections) ? data.top_sections : [];
      const chatProviders = Array.isArray(data.chat_providers) ? data.chat_providers : [];

      ctx.root.innerHTML = pageHead({
        title: '配置编辑器',
        icon: 'gear',
        sub: '直接编辑 ' + filePath + ' · 保存前二次确认，后端备份 + TOML 语法校验 + sha256 回读；脏数据不会落盘',
        actions: [
          '<button class="btn btn-soft btn-sm" id="config-save" type="button">' + icon('save') + '保存 (Ctrl+S)</button>',
          '<button class="btn btn-ghost btn-sm" data-retry="1" type="button">' + icon('refresh') + '重新读取</button>',
          '<a class="btn btn-ghost btn-sm" href="#/selftest" id="config-selftest">' + icon('shield') + '接口自检</a>',
        ],
      }) +
      kpiGrid([
        { label: '文件', icon: 'gear', valueHtml: '<span style="font-size:15px" class="mono">' + esc(filePath) + '</span>', hint: '行数 ' + fmtNum(lineCount) + ' · ' + fmtBytes(bytes) + (data.mtime ? ' · 修改于 ' + esc(fmtTime(data.mtime)) : '') },
        { label: 'TOML 校验', icon: valid ? 'check' : 'alert', valueHtml: valid ? '<span class="tag tag-ok">语法通过</span>' : '<span class="tag tag-bad">语法错误</span>', hint: valid ? (chatProviders.length ? 'chat provider: ' + esc(chatProviders.join(' / ')) : 'tomllib 解析成功') : esc(parseError.slice(0, 80)) },
        { label: '结构', icon: 'list', valueHtml: fmtNum(topSections.length) + '<small>段</small>', hint: topSections.slice(0, 6).map(function (item) { return esc(item); }).join(' / ') || '未返回 top_sections' },
        { label: 'sha256', icon: 'shield', valueHtml: '<span class="mono" style="font-size:12px">' + esc(sha ? sha.slice(0, 16) + '…' : '—') + '</span>', hint: sha ? '保存后回读会刷新' : '接口未返回 sha256' },
      ]) +
      (!valid ? notice('bad', '当前 config.toml <b>TOML 语法校验失败</b>：' + esc(parseError) + '<br>后端仍可读取原文；保存时后端会拒绝写入不合法的内容。') : '') +
      (restartHint ? notice('info', esc(restartHint)) : '') +
      card({ title: 'config.toml', icon: 'terminal', flush: true, headActions: [
        '<span class="muted text-xs" id="config-dirty">未修改</span>',
        '<button class="btn btn-sm btn-ghost" id="config-reload" type="button">' + icon('refresh') + '放弃修改并重载</button>',
      ] },
        '<div class="v3-editor" id="config-editor">' +
          '<div class="v3-editor-gutter" id="config-gutter">' + esc(lineNumbers(content)) + '</div>' +
          '<textarea class="v3-editor-area" id="config-textarea" spellcheck="false" wrap="off" aria-label="config.toml 内容">' + esc(content) + '</textarea>' +
        '</div>') +
      '<div id="config-result" class="mt-16"></div>' +
      '<div class="grid grid-2 mt-16">' +
        card({ title: '保存策略', icon: 'shield' },
          '<div class="v3-check-list">' +
            '<div class="v3-check"><span class="dot" data-tone="ok"></span><span>保存前把当前文件备份到 <span class="mono">/runtime/config.toml.bak-admin-&lt;时间&gt;</span>' + (configBackups.length ? '（当前已有 ' + configBackups.length + ' 份备份）' : '') + '</span></div>' +
            '<div class="v3-check"><span class="dot" data-tone="ok"></span><span>用 Python tomllib 解析校验语法，失败则不写入</span></div>' +
            '<div class="v3-check"><span class="dot" data-tone="ok"></span><span>写入后回读文件并返回 sha256，前端展示新旧 sha 对比</span></div>' +
            '<div class="v3-check"><span class="dot" data-tone="warn"></span><span>模型/密钥类修改可能需要重启 bot 才生效（保存结果会提示）</span></div>' +
          '</div>' +
          (configBackups.length
            ? '<div class="section-title mt-16">' + icon('database') + '最近备份</div><div class="mt-8">' + configBackups.slice(0, 5).map(function (item) {
              return '<div class="stat-line"><span class="mono text-xs">' + esc(textOf(item.file || item.path)) + '</span><span class="spacer"></span><span class="muted-2 text-xs">' + (item.size != null ? fmtBytes(item.size) + ' · ' : '') + esc(item.mtime_text || fmtTime(item.mtime) || '') + '</span></div>';
            }).join('') + '</div>'
            : '')) +
        card({ title: '接口', icon: 'link' },
          '<div class="v3-jsonfallback">' +
            codeTip(['GET /api/v2/config/toml', 'POST /api/v2/config/toml {content}']) +
            '<div class="muted-2 text-xs mt-8">读取失败时不会覆盖文件；保存接口返回 <span class="mono">ok:false</span> 时错误原因会显示在上方结果区。</div>' +
          '</div>') +
      '</div>';

      const area = document.getElementById('config-textarea');
      const gutter = document.getElementById('config-gutter');
      const dirtyNode = document.getElementById('config-dirty');
      const resultNode = document.getElementById('config-result');
      const original = content;
      const markDirty = function () {
        const dirty = area.value !== original;
        if (dirtyNode) {
          dirtyNode.textContent = dirty ? '有未保存修改' : '未修改';
          dirtyNode.className = dirty ? 'tag tag-warn' : 'muted text-xs';
        }
      };
      bindEditor(area, gutter, function () { saveConfig(); });
      area.addEventListener('input', markDirty);

      let saving = false;
      async function saveConfig() {
        if (saving) return;
        const nextContent = area.value;
        if (nextContent === original) { toast('内容没有变化，无需保存', 'info'); return; }
        const ok = await confirmAction({
          title: '保存 config.toml？',
          detail: '将把编辑器中的内容写回 ' + filePath + '（' + nextContent.split('\n').length + ' 行 / ' + fmtBytes(nextContent.length) + '）。后端会先备份、做 TOML 语法校验，失败不会落盘。',
          confirmLabel: '备份并保存',
          danger: true,
        });
        if (!ok) return;
        saving = true;
        const saveBtn = document.getElementById('config-save');
        if (saveBtn) { saveBtn.disabled = true; saveBtn.innerHTML = icon('clock') + '保存中…'; }
        if (resultNode) resultNode.innerHTML = '<div class="sk sk-line w60" style="height:14px"></div>';
        try {
          const response = await apiPost('/api/v2/config/toml', { content: nextContent }, { timeout: 30000 });
          const saved = unwrap(response) || {};
          const savedSha = textOf(saved.sha256 || saved.sha || '');
          if (resultNode) {
            resultNode.innerHTML = notice('ok', '配置已保存' +
              (savedSha ? '，新 sha256 <span class="mono">' + esc(savedSha.slice(0, 24)) + '…</span>' : '') +
              (saved.backup ? '<br>备份：<span class="mono">' + esc(saved.backup) + '</span>' : (saved.backup_path ? '<br>备份：<span class="mono">' + esc(saved.backup_path) + '</span>' : '')) +
              (saved.restart_required ? '<br><b>需要重启 bot 才能完全生效</b>' : (saved.effective != null ? '<br>生效状态：' + esc(saved.effective) : '')) +
              (saved.message ? '<br>' + esc(saved.message) : '')) +
              '<div class="row mt-8"><button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(saved).slice(0, 6000)) + '" data-json-title="保存结果" type="button">' + icon('eye') + '查看完整返回</button></div>';
          }
          toast('config.toml 已保存', 'ok');
          if (savedSha) {
            const shaNode = ctx.root.querySelector('.kpi:nth-child(4) .kpi-value');
            if (shaNode) shaNode.innerHTML = '<span class="mono" style="font-size:12px">' + esc(savedSha.slice(0, 16)) + '…</span>';
          }
          if (saved.valid === false) toast('后端提示保存后的 TOML 校验未通过，请检查', 'warn', { timeout: 6000 });
        } catch (err) {
          if (resultNode) {
            resultNode.innerHTML = errorState(err, 'config-save-retry') +
              notice('warn', '保存失败时后端不会写入半成品文件；修正后可直接再次保存，或点「放弃修改并重载」恢复磁盘版本。');
            const retry = document.getElementById('config-save-retry');
            if (retry) retry.addEventListener('click', saveConfig);
          }
          toast('保存失败：' + err.message, 'bad');
        } finally {
          saving = false;
          if (saveBtn) { saveBtn.disabled = false; saveBtn.innerHTML = icon('save') + '保存 (Ctrl+S)'; }
        }
      }

      const saveBtn = document.getElementById('config-save');
      if (saveBtn) saveBtn.addEventListener('click', saveConfig);
      const reloadBtn = document.getElementById('config-reload');
      if (reloadBtn) reloadBtn.addEventListener('click', function () { route(); });
      guardAssert(ctx, 'config');
    },
  });

  /* ── 9. 页面：记忆与人设文件 ────────────────────────────────────────── */

  function normalizeMemoryTree(payload) {
    const out = [];
    const seen = {};
    function walk(node, parentPath) {
      if (node == null) return;
      if (Array.isArray(node)) { node.forEach(function (child) { walk(child, parentPath); }); return; }
      if (typeof node === 'string') {
        const path = node.replace(/^\/+/, '');
        if (!seen[path]) { seen[path] = true; out.push({ path: path, name: path.split('/').pop(), isDir: false, size: null, mtime: null }); }
        return;
      }
      if (typeof node !== 'object') return;
      const name = textOf(node.name || node.filename || (node.path ? String(node.path).split('/').pop() : ''));
      const rawPath = textOf(node.path || node.rel_path || node.file);
      const path = (rawPath || (parentPath ? parentPath + '/' + name : name)).replace(/^\/+/, '');
      const children = node.children || node.files || node.entries;
      const isDir = node.is_dir === true || node.isDir === true || node.type === 'dir' || node.type === 'directory' || (Array.isArray(children) && children.length > 0);
      if (path && !seen[path]) {
        seen[path] = true;
        out.push({
          path: path,
          name: name || path.split('/').pop(),
          isDir: isDir,
          size: numOrNull(node.size != null ? node.size : node.bytes),
          mtime: node.mtime != null ? node.mtime : (node.modified_at != null ? node.modified_at : node.mtime_ms),
          depth: path.split('/').length - 1,
        });
      }
      if (Array.isArray(children)) children.forEach(function (child) { walk(child, path); });
    }
    walk(payload && (payload.tree || payload.entries || payload.files || payload.items || payload), '');
    return out;
  }

  defineView({
    id: 'memory',
    name: '记忆与人设',
    icon: 'book',
    skeleton: 'default',
    crumb(route) { return route.segments[1] ? decodeSegment(route.segments[1]) : null; },
    async render(ctx) {
      const treeRes = await tryApi(['/api/v2/memory/tree'], { timeout: 20000 });
      if (!ctx.alive()) return;
      if (!treeRes.ok) {
        renderApiFailure(ctx, treeRes, {
          title: '记忆与人设',
          icon: 'book',
          paths: ['GET /api/v2/memory/tree', 'GET/POST /api/v2/memory/file?path='],
          hint: '记忆文件接口（白名单 /runtime/memory/**，读写带备份与 256KB 上限）尚未就绪。当前可先到「拟人模式」查看人设范围与 persona 文件。',
          extraHtml: '<div class="row mt-16"><a class="btn btn-sm btn-ghost" href="#/humanlike">' + icon('spark') + '去拟人模式</a></div>',
        });
        return;
      }
      const entries = normalizeMemoryTree(treeRes.data || {});
      const fileEntries = entries.filter(function (item) { return !item.isDir; });
      const search = ctx.q('path');
      const keyword = ctx.q('q');

      ctx.root.innerHTML = pageHead({
        title: '记忆与人设',
        icon: 'book',
        sub: '浏览、编辑运行时记忆 / 人设文本文件 · ' + fileEntries.length + ' 个文件 · 保存后自动备份',
        actions: [
          '<button class="btn btn-ghost btn-sm" data-retry="1" type="button">' + icon('refresh') + '刷新目录</button>',
          '<a class="btn btn-ghost btn-sm" href="#/humanlike">' + icon('spark') + '拟人模式</a>',
        ],
      }) +
      kpiGrid([
        { label: '文件数', icon: 'book', valueHtml: fmtNum(fileEntries.length), tone: 'acc', hint: '来自 /runtime/memory 目录树' },
        { label: '目录数', icon: 'database', valueHtml: fmtNum(entries.filter(function (i) { return i.isDir; }).length) },
        { label: '总大小', icon: 'download', valueHtml: fmtBytes(fileEntries.reduce(function (sum, item) { return sum + (Number(item.size) || 0); }, 0)) },
        { label: '最近修改', icon: 'clock', valueHtml: '<span style="font-size:14px">' + esc(ago(Math.max.apply(null, [0].concat(fileEntries.map(function (i) { return toMs(i.mtime) || 0; }))))) + '</span>', hint: '按文件 mtime 取最新' },
      ]) +
      '<div class="grid v3-split mt-16">' +
        card({ title: '目录', icon: 'list', flush: true, headActions: ['<span class="muted text-xs">' + entries.length + ' 项</span>'] },
          '<div class="v3-filter"><input class="input input-sm" id="memory-q" placeholder="过滤文件路径…" value="' + escAttr(keyword) + '"></div>' +
          '<div class="v3-tree" id="memory-tree"></div>') +
        card({ title: '文件编辑器', icon: 'terminal', flush: true, headActions: [
          '<span class="muted text-xs mono" id="memory-path-label">' + esc(search || '未选择文件') + '</span>',
          '<button class="btn btn-sm btn-soft" id="memory-save" type="button">' + icon('save') + '保存</button>',
        ] },
          '<div id="memory-editor"></div>') +
      '</div>' +
      '<div id="memory-result" class="mt-16"></div>';

      const treeRoot = document.getElementById('memory-tree');
      const editorRoot = document.getElementById('memory-editor');
      const filterInput = document.getElementById('memory-q');
      let activePath = search || (fileEntries[0] && fileEntries[0].path) || '';
      let activeOriginal = '';
      let loadSeq = 0;

      function visibleEntries() {
        const kw = (filterInput ? filterInput.value : '').trim().toLowerCase();
        if (!kw) return entries;
        return entries.filter(function (item) { return item.path.toLowerCase().indexOf(kw) >= 0; });
      }

      function paintTree() {
        const list = visibleEntries();
        if (!list.length) {
          treeRoot.innerHTML = emptyState({ icon: 'search', title: '没有匹配的文件', hint: '换个关键词，或清空过滤' });
          return;
        }
        treeRoot.innerHTML = list.map(function (item) {
          const indent = Math.min(6, Math.max(0, (item.depth || 0))) * 14;
          if (item.isDir) {
            return '<div class="v3-tree-item dir" style="padding-left:' + (10 + indent) + 'px">' + icon('chevron-down') +
              '<span class="v3-tree-name">' + esc(item.name) + '</span></div>';
          }
          return '<button type="button" class="v3-tree-item file' + (item.path === activePath ? ' active' : '') + '" data-memory-path="' + escAttr(item.path) + '" style="padding-left:' + (10 + indent) + 'px">' +
            icon('book') + '<span class="v3-tree-name mono">' + esc(item.name) + '</span>' +
            '<span class="v3-tree-size">' + (item.size != null ? fmtBytes(item.size) : '') + '</span></button>';
        }).join('');
      }

      async function openFile(path) {
        activePath = path;
        paintTree();
        const label = document.getElementById('memory-path-label');
        if (label) label.textContent = path;
        editorRoot.innerHTML = '<div class="v3-editor-loading"><div class="sk sk-line w60"></div><div class="sk sk-line"></div></div>';
        const seq = ++loadSeq;
        const res = await tryApi(['/api/v2/memory/file' + qs({ path: path })], { timeout: 20000 });
        if (seq !== loadSeq || !ctx.alive()) return;
        if (!res.ok) {
          editorRoot.innerHTML = res.pending
            ? pendingState({ title: '读取文件接口未就绪', hint: 'GET /api/v2/memory/file?path=' + path, path: 'GET /api/v2/memory/file' })
            : errorState(res.error, 'memory-retry');
          const retry = document.getElementById('memory-retry');
          if (retry) retry.addEventListener('click', function () { openFile(path); });
          return;
        }
        const data = res.data || {};
        const content = textOf(data.content != null ? data.content : (data.text != null ? data.text : data.body));
        activeOriginal = content;
        editorRoot.innerHTML =
          '<div class="v3-editor" id="memory-editor-box">' +
            '<div class="v3-editor-gutter" id="memory-gutter">' + esc(lineNumbers(content)) + '</div>' +
            '<textarea class="v3-editor-area" id="memory-textarea" spellcheck="false" aria-label="记忆文件内容">' + esc(content) + '</textarea>' +
          '</div>' +
          '<div class="card-foot"><span class="muted-2 text-xs">' + fmtBytes(content.length) + ' · ' + (data.mtime ? '修改于 ' + esc(fmtTime(data.mtime)) : '修改时间未知') + (data.sha256 ? ' · sha ' + esc(String(data.sha256).slice(0, 12)) + '…' : '') + '</span>' +
          '<span class="spacer"></span><span class="muted-2 text-xs">Ctrl+S 保存</span></div>';
        const area = document.getElementById('memory-textarea');
        const gutter = document.getElementById('memory-gutter');
        bindEditor(area, gutter, function () { saveFile(); });
      }

      async function saveFile() {
        const area = document.getElementById('memory-textarea');
        if (!area || !activePath) { toast('请先选择左侧文件', 'warn'); return; }
        if (area.value === activeOriginal) { toast('内容没有变化，无需保存', 'info'); return; }
        const ok = await confirmAction({
          title: '保存记忆文件？',
          detail: '写入 ' + activePath + '（' + area.value.split('\n').length + ' 行 / ' + fmtBytes(area.value.length) + '）。后端路径白名单限制在 /runtime/memory/**，保存前会备份。',
          confirmLabel: '备份并保存',
          danger: true,
        });
        if (!ok) return;
        const saveBtn = document.getElementById('memory-save');
        const resultRoot = document.getElementById('memory-result');
        if (saveBtn) saveBtn.disabled = true;
        if (resultRoot) resultRoot.innerHTML = '<div class="sk sk-line w60" style="height:14px"></div>';
        try {
          const response = await apiPost('/api/v2/memory/file', { path: activePath, content: area.value }, { timeout: 25000 });
          const saved = unwrap(response) || {};
          activeOriginal = area.value;
          if (resultRoot) {
            resultRoot.innerHTML = notice('ok', '已保存 <span class="mono">' + esc(activePath) + '</span>' +
              (saved.backup ? '，备份：<span class="mono">' + esc(saved.backup) + '</span>' : '') +
              (saved.bytes != null ? ' · ' + fmtBytes(saved.bytes) : '') +
              (saved.restart_required ? '<br><b>需要重启 bot 生效</b>' : ''));
          }
          toast('文件已保存', 'ok');
        } catch (err) {
          if (resultRoot) resultRoot.innerHTML = errorState(err, 'memory-save-retry');
          const retry = document.getElementById('memory-save-retry');
          if (retry) retry.addEventListener('click', saveFile);
          toast('保存失败：' + err.message, 'bad');
        } finally {
          if (saveBtn) saveBtn.disabled = false;
        }
      }

      treeRoot.addEventListener('click', function (event) {
        const button = event.target.closest('[data-memory-path]');
        if (button) openFile(button.getAttribute('data-memory-path'));
      });
      if (filterInput) {
        filterInput.addEventListener('input', paintTree);
        filterInput.addEventListener('keydown', function (event) {
          if (event.key !== 'Enter') return;
          const list = visibleEntries().filter(function (item) { return !item.isDir; });
          if (list.length) openFile(list[0].path);
        });
      }
      const saveBtn = document.getElementById('memory-save');
      if (saveBtn) saveBtn.addEventListener('click', saveFile);

      paintTree();
      if (activePath) openFile(activePath);
      else editorRoot.innerHTML = emptyState({ icon: 'book', title: '记忆目录里还没有文件', hint: '到「拟人模式」页可以查看人设范围；记忆文件接口返回空数组时会出现此提示。' });
      guardAssert(ctx, 'memory');
    },
  });

  /* ── 10. 页面：模型切换 ─────────────────────────────────────────────── */

  function normalizeOptionList(value) {
    const out = [];
    function push(item) {
      if (item == null) return;
      if (typeof item === 'string') { out.push({ name: item, provider: item }); return; }
      if (typeof item !== 'object') return;
      const name = textOf(item.name || item.provider || item.model || item.id || item.value);
      if (!name) return;
      out.push(Object.assign({}, item, { name: name, provider: item.provider || name, model: item.model || item.name || name }));
    }
    if (Array.isArray(value)) value.forEach(push);
    else if (value && typeof value === 'object') {
      Object.keys(value).forEach(function (key) {
        const child = value[key];
        if (child && typeof child === 'object') push(Object.assign({ name: key }, child));
        else if (child === true) push(key);
      });
    }
    return out;
  }

  function normalizeModelsPayload(payload) {
    const data = payload || {};
    const chatNode = (data.chat && typeof data.chat === 'object') ? data.chat : (data.llm && typeof data.llm === 'object' ? data.llm : {});
    const imageNode = (data.image && typeof data.image === 'object') ? data.image : (data.image_model && typeof data.image_model === 'object' ? data.image_model : {});
    const chatOptions = normalizeOptionList(chatNode.options || chatNode.providers || chatNode.models || chatNode.items || data.providers || data.models || data.chat_providers || data.available || []);
    const imageOptions = normalizeOptionList(imageNode.options || imageNode.providers || imageNode.models || imageNode.items || data.image_providers || data.image_models || []);
    const current = textOf(
      chatNode.current || chatNode.model || chatNode.name ||
      data.current_model || data.current_provider || data.current_chat_model || data.current || data.model || ''
    );
    const effort = textOf(chatNode.effort || chatNode.reasoning_effort || data.current_effort || data.effort || data.reasoning_effort || '');
    const imageCurrent = textOf(imageNode.current || data.current_image_provider || data.current_image_model || '');
    const efforts = Array.isArray(data.efforts) ? data.efforts.filter(function (item) { return typeof item === 'string'; }) : [];
    return {
      raw: data,
      chat: { current: current, options: chatOptions },
      image: { current: imageCurrent, options: imageOptions },
      effort: effort,
      efforts: efforts,
      rpc: data.rpc || null,
      note: textOf(data.note || ''),
      providerCount: chatOptions.length,
    };
  }

  function optionCurrent(option, current) {
    if (option.current === true || option.active === true) return true;
    if (!current) return false;
    const name = textOf(option.name || option.provider || option.model);
    const model = textOf(option.model || option.name);
    return name === current || model === current ||
      current.indexOf(name) >= 0 || (model && current.indexOf(model) >= 0);
  }

  defineView({
    id: 'mswitch',
    name: '模型切换',
    icon: 'cpu',
    skeleton: 'default',
    async render(ctx) {
      const result = await tryApi(['/api/v2/models'], { timeout: 20000 });
      if (!ctx.alive()) return;
      let models = null;
      let fallback = false;
      let fallbackError = null;
      if (result.ok) {
        models = normalizeModelsPayload(result.data || {});
      } else if (result.pending) {
        const [chatList, imageList] = await Promise.all([
          tryApi(['/api/runtime/config/models'], { timeout: 15000 }),
          tryApi(['/api/runtime/config/image_models'], { timeout: 15000 }),
        ]);
        if (!ctx.alive()) return;
        if (chatList.ok || imageList.ok) {
          fallback = true;
          const chatData = chatList.ok ? (chatList.data || {}) : {};
          const imageData = imageList.ok ? (imageList.data || {}) : {};
          models = normalizeModelsPayload({
            chat: chatData,
            image: imageData,
            current_model: (chatData.current || chatData.model || (chatData.items && chatData.items[0] && (chatData.items[0].name || chatData.items[0].model)) || ''),
            image_current: (imageData.current || (imageData.items && imageData.items[0] && (imageData.items[0].name || imageData.items[0].model)) || ''),
          });
        } else {
          fallbackError = result;
        }
      } else {
        fallbackError = result;
      }
      if (!models) {
        renderApiFailure(ctx, fallbackError, {
          title: '模型切换',
          icon: 'cpu',
          paths: ['GET /api/v2/models', 'POST /api/v2/models/switch'],
          hint: '模型清单与切换接口（读取 config.toml 的 chat/image provider，切换支持 RPC 或写文件并提示是否需重启）尚未就绪。',
        });
        return;
      }

      const switchOptions = models.chat.options.length ? models.chat.options : [{ name: models.chat.current || '未识别', provider: '' }];
      const imageCurrent = models.image.current || '未标注';
      const effortOptions = models.efforts.length ? models.efforts : ['low', 'medium', 'high'];
      ctx.root.innerHTML = pageHead({
        title: '模型切换',
        icon: 'cpu',
        sub: '查看当前聊天 / 图片模型，切换聊天 provider 并选择 reasoning effort · 切换会二次确认并显示 via / 重启提示',
        actions: [
          '<button class="btn btn-ghost btn-sm" data-retry="1" type="button">' + icon('refresh') + '刷新</button>',
          '<a class="btn btn-ghost btn-sm" href="#/config">' + icon('gear') + '编辑 config.toml</a>',
        ],
      }) +
      (fallback ? notice('warn', '新接口 <span class="mono">/api/v2/models</span> 未就绪，当前展示的是旧通道 <span class="mono">/api/runtime/config/models</span> 的只读数据；切换按钮暂不可用，接口就绪后自动恢复。') : '') +
      (models.note ? notice('info', esc(models.note) + (models.rpc && models.rpc.ok ? '<br>RPC 通道：<span class="mono">' + esc(models.rpc.endpoint || 'config.model') + '</span> 可用' : (models.rpc ? '<br>RPC 不可用：' + esc(models.rpc.error || '未知原因') : ''))) : '') +
      kpiGrid([
        { label: '当前聊天模型', icon: 'cpu', valueHtml: '<span style="font-size:15px" class="mono">' + esc(models.chat.current || '未识别') + '</span>', tone: 'acc', hint: models.providerCount ? models.chat.options.length + ' 个候选 provider' : '来自 config.toml 解析' },
        { label: 'Reasoning effort', icon: 'zap', valueHtml: '<span style="font-size:16px">' + esc(models.effort || '未设置') + '</span>', hint: '切换时可指定 effort' },
        { label: '当前图片模型', icon: 'eye', valueHtml: '<span style="font-size:15px" class="mono">' + esc(imageCurrent) + '</span>', hint: '只读展示' },
        { label: '切换通道', icon: 'link', valueHtml: '<span class="tag ' + (result.ok ? 'tag-ok' : 'tag-warn') + '">' + (result.ok ? 'v3 接口' : '只读降级') + '</span>', hint: result.ok ? 'POST /api/v2/models/switch' : '旧通道不支持写入' },
      ]) +
      '<div class="grid grid-wide mt-16">' +
        card({ title: '聊天模型候选', icon: 'cpu', flush: true, headActions: ['<span class="muted text-xs">当前：<span class="mono">' + esc(models.chat.current || '未识别') + '</span></span>'] },
          '<div id="models-chat-table"></div>' +
          '<div class="card-foot"><span class="muted-2 text-xs">切换后如果经过写文件通道，需要重启 bot 才完全生效；返回里会带 <span class="mono">via</span> 与 <span class="mono">restart_required</span>。</span></div>') +
        card({ title: '切换参数', icon: 'wand' },
          '<div class="stack" style="gap:12px">' +
            '<div class="field"><label>目标 provider</label><select class="select" id="models-provider">' +
              switchOptions.map(function (option) {
                return '<option value="' + escAttr(option.name) + '">' + esc(option.name) + (option.model && option.model !== option.name ? ' · ' + esc(option.model) : '') + '</option>';
              }).join('') +
            '</select></div>' +
            '<div class="field"><label>Reasoning effort（可选）</label><select class="select" id="models-effort">' +
              ['<沿用当前>'].concat(effortOptions).map(function (value, index) {
                const text = index === 0 ? '' : value;
                return '<option value="' + text + '"' + ((index === 0 && !models.effort) || models.effort === value ? ' selected' : '') + '>' + esc(index === 0 ? '沿用当前（' + (models.effort || '未设置') + '）' : value) + '</option>';
              }).join('') +
            '</select></div>' +
            '<button class="btn btn-solid btn-sm" id="models-switch" type="button"' + (fallback ? ' disabled' : '') + '>' + icon('zap') + (fallback ? '接口未就绪，暂不可切换' : '切换模型') + '</button>' +
            '<div id="models-result"></div>' +
          '</div>') +
      '</div>' +
      '<div class="grid grid-2 mt-16">' +
        card({ title: '图片模型', icon: 'eye', flush: true },
          models.image.options.length
            ? '<div class="v3-list">' + models.image.options.map(function (option) {
              return '<div class="v3-list-item">' + icon('eye') +
                '<div class="v3-list-main"><div class="v3-list-title mono">' + esc(option.name) + '</div>' +
                '<div class="v3-list-sub">' + esc(option.model || option.description || option.note || '') + '</div></div>' +
                (optionCurrent(option, models.image.current) ? '<span class="tag tag-ok">当前</span>' : '<span class="tag tag-muted">候选</span>') + '</div>';
            }).join('') + '</div>'
            : '<div class="card-body"><div class="muted-2 text-xs">接口未返回图片 provider 列表。</div></div>') +
        card({ title: '原始返回', icon: 'database' },
          '<div class="v3-jsonfallback">' +
            '<div class="row"><button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(models.raw).slice(0, 8000)) + '" data-json-title="模型接口原始返回" type="button">' + icon('eye') + '查看 JSON</button>' +
            '<span class="muted-2 text-xs">接口：' + esc(result.path || '/api/v2/models') + '</span></div>' +
          '</div>') +
      '</div>';

      const tableRoot = document.getElementById('models-chat-table');
      if (tableRoot) {
        v3Table(tableRoot, {
          stateKey: 'models-chat',
          rows: models.chat.options,
          countLabel: function (count) { return count + ' 个 provider'; },
          columns: [
            { key: 'name', label: 'provider', mono: true, render: function (row) { return '<span class="mono">' + esc(row.name) + '</span>'; } },
            { key: 'model', label: '模型', render: function (row) { return '<span class="cell-truncate">' + esc(row.model || '—') + '</span>'; } },
            { key: 'note', label: '备注', hideSm: true, render: function (row) { return '<span class="cell-truncate muted-2">' + esc(row.description || row.note || row.effort || '') + '</span>'; } },
            {
              key: 'current',
              label: '状态',
              width: '92px',
              render: function (row) {
                return optionCurrent(row, models.chat.current) ? '<span class="tag tag-ok">' + icon('check') + '当前</span>' : '<span class="tag tag-muted">候选</span>';
              },
            },
            {
              key: 'action',
              label: '',
              width: '92px',
              render: function (row) {
                return optionCurrent(row, models.chat.current)
                  ? '<span class="muted-2 text-xs">—</span>'
                  : '<button class="btn btn-sm btn-ghost" data-switch-provider="' + escAttr(row.name) + '" type="button"' + (fallback ? ' disabled' : '') + '>切换</button>';
              },
            },
          ],
          empty: { icon: 'cpu', title: '没有候选 provider', hint: '接口返回了空列表，请检查 config.toml 的 [llm] 配置' },
        });
      }

      async function doSwitch(provider) {
        if (!provider) { toast('请选择目标 provider', 'warn'); return; }
        if (provider === models.chat.current) { toast('已经是当前模型，无需切换', 'info'); return; }
        const effortSelect = document.getElementById('models-effort');
        const effort = effortSelect ? effortSelect.value : '';
        const ok = await confirmAction({
          title: '切换聊天模型？',
          detail: '目标 provider：' + provider + (effort ? ' · effort=' + effort : '（沿用当前 effort）') + '。切换可能通过 RPC 即时生效，也可能写 config.toml 后需要重启 bot；结果会显示 via 与 restart_required。',
          confirmLabel: '确认切换',
          danger: true,
        });
        if (!ok) return;
        const resultBox = document.getElementById('models-result');
        if (resultBox) resultBox.innerHTML = '<div class="sk sk-line w60" style="height:14px"></div>';
        try {
          const body = { provider: provider };
          if (effort) body.effort = effort;
          const response = await apiPost('/api/v2/models/switch', body, { timeout: 30000 });
          const data = unwrap(response) || {};
          if (resultBox) {
            resultBox.innerHTML = notice('ok', '切换请求已提交' +
              ' · via=<b>' + esc(data.via || 'unknown') + '</b>' +
              ' · restart_required=<b>' + esc(data.restart_required == null ? 'unknown' : String(data.restart_required)) + '</b>' +
              (data.effective != null ? '<br>生效状态：' + esc(data.effective) : '') +
              (data.message ? '<br>' + esc(data.message) : '') +
              (data.restart_required ? '<br>请到「部署与运维」或「运维监控 → 回滚」页重启 fm-cosmobot 后生效。' : '')) +
              '<div class="row mt-8"><button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(data).slice(0, 6000)) + '" data-json-title="切换返回" type="button">' + icon('eye') + '完整返回</button>' +
              '<button class="btn btn-sm btn-ghost" data-retry="1" type="button">' + icon('refresh') + '刷新当前模型</button></div>';
          }
          toast('切换请求已提交（via=' + (data.via || '?') + '）', 'ok');
        } catch (err) {
          if (resultBox) resultBox.innerHTML = errorState(err) +
            '<div class="muted-2 text-xs mt-8">若返回 403，说明当前登录账号无写权限（token 登录为只读账号）；请用管理员账号登录后再切换。</div>';
          toast('切换失败：' + err.message, 'bad');
        }
      }

      if (tableRoot) {
        tableRoot.addEventListener('click', function (event) {
          const button = event.target.closest('[data-switch-provider]');
          if (button) doSwitch(button.getAttribute('data-switch-provider'));
        });
      }
      const switchBtn = document.getElementById('models-switch');
      if (switchBtn) switchBtn.addEventListener('click', function () {
        const select = document.getElementById('models-provider');
        doSwitch(select ? select.value : '');
      });
      guardAssert(ctx, 'mswitch');
    },
  });

  /* ── 11. 页面：群能力矩阵 ───────────────────────────────────────────── */

  const CAP_EXTRA_LABELS = {
    agent: 'Agent 对话', ai_contest: 'AI 赛文', contest: '赛文投稿', library: '跟打文库',
    poke: '拍一拍', recall: '撤回处理', repeat: '复读', score_archive: '成绩归档', scores: '成绩识别',
    active_hours: '活跃时段', cross_group_visibility: '跨群可见性', daily_archive: '每日归档',
    group_reply: '群回复', group_trigger_mode: '触发方式', private_context_access: '私聊上下文',
    reply_style: '回复风格',
  };
  const CAP_ORDER = ['agent', 'ai_contest', 'contest', 'library', 'poke', 'recall', 'repeat', 'score_archive', 'scores'];

  function capLabel(key) {
    return (typeof CAPABILITY_LABELS !== 'undefined' && CAPABILITY_LABELS[key]) || CAP_EXTRA_LABELS[key] || key;
  }

  function extractCapabilities(payload) {
    const data = payload || {};
    const raw = data.capabilities || data.features || data.feature_map || (data.data && (data.data.capabilities || data.data.features)) || null;
    if (!raw || typeof raw !== 'object') return null;
    const out = {};
    Object.keys(raw).forEach(function (key) {
      const value = raw[key];
      if (typeof value === 'boolean') out[key] = value;
    });
    return Object.keys(out).length ? out : null;
  }

  defineView({
    id: 'capabilities',
    name: '群能力矩阵',
    icon: 'shield',
    skeleton: 'table',
    async render(ctx) {
      const root = ctx.root;
      let groups = [];
      let groupsError = null;
      try { groups = await getGroups(true); } catch (err) { groupsError = err; }
      if (!ctx.alive()) return;
      if (groupsError) {
        renderApiFailure(ctx, { ok: false, pending: isPendingError(groupsError), error: groupsError, path: '/api/v2/groups' }, {
          title: '群能力矩阵',
          icon: 'shield',
          paths: ['GET /api/v2/groups', 'GET /api/v2/group/capability?id=', 'POST /api/v2/group/capability'],
          hint: '群列表接口不可用，无法构建能力矩阵。',
        });
        return;
      }

      root.innerHTML = pageHead({
        title: '群能力矩阵',
        icon: 'shield',
        sub: '群 × 能力开关总览 · 勾选后统一保存（每格单独写回，失败会逐个提示）· 数据源 /api/v2/group/capability（降级 domain /group-capability）',
        actions: [
          '<button class="btn btn-soft btn-sm" id="cap-save" type="button">' + icon('save') + '保存修改 (0)</button>',
          '<button class="btn btn-ghost btn-sm" id="cap-reset" type="button">' + icon('refresh') + '重置未保存修改</button>',
          '<button class="btn btn-ghost btn-sm" data-retry="1" type="button">' + icon('refresh') + '重新加载</button>',
        ],
      }) +
      '<div id="cap-status"></div>' +
      '<div class="filters mt-12">' +
        '<div class="field grow"><input class="input input-sm" id="cap-q" placeholder="搜索群名 / 群号…" value="' + escAttr(ctx.q('q')) + '"></div>' +
        '<label class="check"><input type="checkbox" id="cap-only-enabled">只看有能力的群</label>' +
        '<span class="muted text-xs" id="cap-count"></span>' +
      '</div>' +
      '<div class="card mt-12" id="cap-card"><div class="card-body" id="cap-matrix">' +
        '<div class="sk sk-line w60"></div><div class="sk sk-line"></div><div class="sk sk-line"></div>' +
      '</div></div>' +
      '<div id="cap-result" class="mt-16"></div>' +
      '<div class="notice mt-16">' + icon('info') + '<div>矩阵数据按群并行读取能力和属性；某个群读取失败该行会标记为「—」，不影响其它行。保存时只提交改动过的格子。</div></div>';

      const matrixRoot = document.getElementById('cap-matrix');
      const statusRoot = document.getElementById('cap-status');
      const countNode = document.getElementById('cap-count');
      const matrix = {};
      const features = CAP_ORDER.slice();
      let changes = {};
      let loading = true;

      const keywordInput = document.getElementById('cap-q');
      const onlyEnabled = document.getElementById('cap-only-enabled');

      function groupLabel(group) {
        return group.name || group.display_name || group.group_id;
      }

      function visibleGroups() {
        const kw = (keywordInput ? keywordInput.value : '').trim().toLowerCase();
        const only = onlyEnabled ? onlyEnabled.checked : false;
        return groups.filter(function (group) {
          const id = textOf(group.group_id);
          if (only) {
            const state = matrix[id];
            if (!state || !state.caps || !Object.keys(state.caps).some(function (key) { return state.caps[key]; })) return false;
          }
          if (!kw) return true;
          return id.toLowerCase().indexOf(kw) >= 0 || String(groupLabel(group)).toLowerCase().indexOf(kw) >= 0;
        });
      }

      function effectiveValue(groupId, feature) {
        if (changes[groupId] && Object.prototype.hasOwnProperty.call(changes[groupId], feature)) return changes[groupId][feature];
        const state = matrix[groupId];
        if (state && state.caps && Object.prototype.hasOwnProperty.call(state.caps, feature)) return state.caps[feature];
        return null;
      }

      function changeCount() {
        return Object.keys(changes).reduce(function (sum, gid) { return sum + Object.keys(changes[gid]).length; }, 0);
      }

      function paint() {
        const list = visibleGroups();
        if (countNode) countNode.textContent = list.length + ' / ' + groups.length + ' 个群';
        const saveBtn = document.getElementById('cap-save');
        if (saveBtn) saveBtn.innerHTML = icon('save') + '保存修改 (' + changeCount() + ')';
        if (loading) return;
        if (!list.length) {
          matrixRoot.innerHTML = emptyState({ icon: 'search', title: '没有匹配的群', hint: '清空搜索或取消「只看有能力的群」' });
          return;
        }
        const head = '<th class="v3-cap-group">群</th>' + features.map(function (feature) {
          return '<th class="v3-cap-col" title="' + escAttr(capLabel(feature)) + '">' +
            '<span>' + esc(capLabel(feature)) + '</span>' +
            '<span class="v3-cap-bulk"><button type="button" data-cap-bulk="' + escAttr(feature) + '" data-cap-on="1" title="全部开启">开</button>' +
            '<button type="button" data-cap-bulk="' + escAttr(feature) + '" data-cap-on="0" title="全部关闭">关</button></span></th>';
        }).join('') + '<th class="v3-cap-rowbulk">整行</th>';
        const body = list.map(function (group) {
          const id = textOf(group.group_id);
          const state = matrix[id] || {};
          const cells = features.map(function (feature) {
            const value = effectiveValue(id, feature);
            const changed = changes[id] && Object.prototype.hasOwnProperty.call(changes[id], feature);
            const unknown = value == null;
            return '<td class="v3-cap-cell' + (changed ? ' changed' : '') + '">' +
              (unknown
                ? '<span class="muted-2" title="' + escAttr(state.error || (state.pending ? '接口未就绪' : '该群未返回此能力')) + '">—</span>'
                : '<label class="v3-cap-check"><input type="checkbox" data-cap-group="' + escAttr(id) + '" data-cap-feature="' + escAttr(feature) + '"' + (value ? ' checked' : '') + '><span></span></label>') +
              '</td>';
          }).join('');
          return '<tr data-cap-row="' + escAttr(id) + '">' +
            '<td class="v3-cap-group"><div class="v3-cap-name">' + esc(groupLabel(group)) + '</div>' +
            '<div class="v3-cap-id mono">' + esc(id) + '</div>' +
            (state.error ? '<span class="tag tag-warn" title="' + escAttr(state.error) + '">读取失败</span>' : (state.pending ? '<span class="tag tag-muted">未就绪</span>' : '')) +
            '</td>' + cells +
            '<td class="v3-cap-rowbulk">' +
              '<button class="btn btn-sm btn-ghost" type="button" data-cap-row-set="' + escAttr(id) + '" data-cap-on="1">全开</button>' +
              '<button class="btn btn-sm btn-ghost" type="button" data-cap-row-set="' + escAttr(id) + '" data-cap-on="0">全关</button>' +
            '</td></tr>';
        }).join('');
        matrixRoot.innerHTML = '<div class="v3-matrix-wrap"><table class="v3-matrix"><thead><tr>' + head + '</tr></thead><tbody>' + body + '</tbody></table></div>';
      }

      async function loadCapabilities() {
        loading = true;
        const ids = groups.map(function (group) { return textOf(group.group_id); });
        const queue = ids.slice();
        let pendingCount = 0;
        let failedCount = 0;
        async function worker() {
          while (queue.length) {
            const gid = queue.shift();
            const res = await tryApi(['/api/v2/group/capability' + qs({ id: gid })], { timeout: 15000 });
            let caps = res.ok ? extractCapabilities(res.data) : null;
            let usedDomain = false;
            if (!caps) {
              const domainRes = await tryApi(['/api/domain/group-capability' + qs({ group_id: gid })], { timeout: 15000 });
              if (domainRes.ok) {
                caps = extractCapabilities(domainRes.data);
                usedDomain = true;
              } else if (!res.ok && (res.pending || domainRes.pending)) {
                pendingCount += 1;
                matrix[gid] = { caps: null, pending: true, error: (domainRes.error && domainRes.error.message) || (res.error && res.error.message) };
                continue;
              } else {
                failedCount += 1;
                matrix[gid] = { caps: null, error: (domainRes.error && domainRes.error.message) || (res.error && res.error.message) || '能力读取失败' };
                continue;
              }
            }
            if (caps) {
              matrix[gid] = { caps: caps, viaDomain: usedDomain };
              Object.keys(caps).forEach(function (key) { if (features.indexOf(key) < 0) features.push(key); });
            }
          }
        }
        const workers = [];
        for (let i = 0; i < 4; i += 1) workers.push(worker());
        await Promise.all(workers);
        if (!ctx.alive()) return;
        loading = false;
        const readOk = Object.keys(matrix).filter(function (gid) { return matrix[gid].caps; }).length;
        if (statusRoot) {
          statusRoot.innerHTML = (readOk ? '' : notice('warn', '能力接口尚未返回任何可用数据（<span class="mono">/api/v2/group/capability</span> 未就绪时页面仍保留群列表与重试入口）。')) +
            ((pendingCount || failedCount)
              ? notice('info', '本次读取：成功 ' + readOk + ' 个群' + (pendingCount ? ' · 未就绪/无数据 ' + pendingCount + ' 个' : '') + (failedCount ? ' · 失败 ' + failedCount + ' 个' : '') + '。可在「接口自检」里查看后端状态。')
              : '');
        }
        paint();
      }

      async function saveChanges() {
        const count = changeCount();
        if (!count) { toast('没有需要保存的修改', 'info'); return; }
        const ok = await confirmAction({
          title: '保存能力矩阵修改？',
          detail: '共 ' + count + ' 处修改，涉及 ' + Object.keys(changes).length + ' 个群。会逐格写回并即时生效（部分能力只影响之后的处理）。',
          confirmLabel: '保存 ' + count + ' 处',
          danger: true,
        });
        if (!ok) return;
        const saveBtn = document.getElementById('cap-save');
        if (saveBtn) saveBtn.disabled = true;
        const gids = Object.keys(changes);
        const okRows = [];
        const failRows = [];
        const pendingRows = [];
        for (let i = 0; i < gids.length; i += 1) {
          const gid = gids[i];
          const featurePatch = changes[gid];
          const patchKeys = Object.keys(featurePatch);
          let groupDone = true;
          let useDomainBatch = false;
          for (let j = 0; j < patchKeys.length; j += 1) {
            const feature = patchKeys[j];
            const enabled = !!featurePatch[feature];
            try {
              await apiPost('/api/v2/group/capability', { group_id: gid, feature: feature, enabled: enabled }, { timeout: 20000 });
            } catch (err) {
              if (isPendingError(err)) { useDomainBatch = true; break; }
              groupDone = false;
              failRows.push({ gid: gid, feature: feature, error: err.message });
            }
          }
          if (useDomainBatch) {
            const baseCaps = matrix[gid] && matrix[gid].caps;
            if (!baseCaps) {
              failRows.push({ gid: gid, feature: '*', error: '该群能力未成功读取，无法安全合并保存（请刷新后重试）' });
              groupDone = false;
              continue;
            }
            try {
              const current = Object.assign({}, baseCaps);
              patchKeys.forEach(function (key) { current[key] = !!featurePatch[key]; });
              await apiPost('/api/domain/group-capability', { group_id: gid, capabilities: current }, { timeout: 20000 });
              Object.assign(baseCaps, current);
              okRows.push(gid);
              delete changes[gid];
              continue;
            } catch (err2) {
              if (isPendingError(err2)) pendingRows.push(gid);
              else failRows.push({ gid: gid, feature: '*', error: err2.message });
              groupDone = false;
            }
          }
          if (groupDone && !useDomainBatch) {
            if (matrix[gid] && matrix[gid].caps) {
              patchKeys.forEach(function (key) { matrix[gid].caps[key] = !!featurePatch[key]; });
            }
            okRows.push(gid);
            delete changes[gid];
          }
        }
        if (saveBtn) saveBtn.disabled = false;
        paint();
        const resultRoot = document.getElementById('cap-result');
        const summary = notice(okRows.length ? 'ok' : 'warn', '保存完成：成功 ' + okRows.length + ' 个群' +
          (failRows.length ? ' · 失败 ' + failRows.length + ' 格' : '') +
          (pendingRows.length ? ' · 未就绪 ' + pendingRows.length + ' 个群（接口 404）' : '') +
          (failRows.length ? '<br>' + failRows.slice(0, 6).map(function (row) { return esc(row.gid + ' / ' + row.feature + '：' + row.error); }).join('<br>') : ''));
        if (resultRoot) resultRoot.innerHTML = summary;
        else if (statusRoot) statusRoot.insertAdjacentHTML('afterbegin', summary);
        toast('能力修改已保存（成功 ' + okRows.length + ' 个群）', okRows.length ? 'ok' : 'warn');
      }

      matrixRoot.addEventListener('change', function (event) {
        const input = event.target.closest('[data-cap-group][data-cap-feature]');
        if (!input) return;
        const gid = input.getAttribute('data-cap-group');
        const feature = input.getAttribute('data-cap-feature');
        if (!changes[gid]) changes[gid] = {};
        changes[gid][feature] = input.checked;
        const cell = input.closest('td');
        if (cell) cell.classList.add('changed');
        const saveBtn = document.getElementById('cap-save');
        if (saveBtn) saveBtn.innerHTML = icon('save') + '保存修改 (' + changeCount() + ')';
      });
      matrixRoot.addEventListener('click', function (event) {
        const colBulk = event.target.closest('[data-cap-bulk]');
        if (colBulk) {
          const feature = colBulk.getAttribute('data-cap-bulk');
          const enabled = colBulk.getAttribute('data-cap-on') === '1';
          visibleGroups().forEach(function (group) {
            const gid = textOf(group.group_id);
            if (!changes[gid]) changes[gid] = {};
            changes[gid][feature] = enabled;
          });
          paint();
          return;
        }
        const rowBulk = event.target.closest('[data-cap-row-set]');
        if (rowBulk) {
          const gid = rowBulk.getAttribute('data-cap-row-set');
          const enabled = rowBulk.getAttribute('data-cap-on') === '1';
          if (!changes[gid]) changes[gid] = {};
          features.forEach(function (feature) {
            const state = matrix[gid];
            if (state && state.caps && !Object.prototype.hasOwnProperty.call(state.caps, feature)) return;
            changes[gid][feature] = enabled;
          });
          paint();
        }
      });
      const saveBtn = document.getElementById('cap-save');
      if (saveBtn) saveBtn.addEventListener('click', saveChanges);
      const resetBtn = document.getElementById('cap-reset');
      if (resetBtn) resetBtn.addEventListener('click', function () { changes = {}; paint(); toast('已重置未保存的修改', 'info'); });
      if (keywordInput) keywordInput.addEventListener('input', paint);
      if (onlyEnabled) onlyEnabled.addEventListener('change', paint);

      loadCapabilities();
      guardAssert(ctx, 'capabilities');
    },
  });

  /* ── 12. 页面：运维监控（指标 / 镜像 / 日志 / 回滚 / 定时任务） ─────── */

  const OPS_TABS = [
    { id: 'metrics', label: '指标', icon: 'activity' },
    { id: 'images', label: '镜像', icon: 'database' },
    { id: 'logs', label: '日志', icon: 'terminal' },
    { id: 'rollback', label: '回滚', icon: 'refresh' },
    { id: 'cron', label: '定时任务', icon: 'clock' },
  ];

  function normalizeMetrics(payload) {
    const data = payload || {};
    const host = data.host || data;
    const mem = host.mem || host.memory || {};
    const diskMap = host.disk || {};
    const diskKey = Object.prototype.hasOwnProperty.call(diskMap, '/') ? '/' : (Object.keys(diskMap)[0] || '');
    const disk = diskMap && typeof diskMap === 'object' && diskKey ? (diskMap[diskKey] || {}) : (host.disk || {});
    const load = host.load || host.loadavg || {};
    const load1 = numOrNull(load['1m'] != null ? load['1m'] : (load.load1 != null ? load.load1 : (Array.isArray(load) ? load[0] : host.load1)));
    const load5 = numOrNull(load['5m'] != null ? load['5m'] : (load.load5 != null ? load.load5 : (Array.isArray(load) ? load[1] : host.load5)));
    const load15 = numOrNull(load['15m'] != null ? load['15m'] : (load.load15 != null ? load.load15 : (Array.isArray(load) ? load[2] : host.load15)));
    const memTotal = numOrNull(mem.total != null ? mem.total : (mem.total_bytes != null ? mem.total_bytes : (mem.total_kb != null ? Number(mem.total_kb) * 1024 : null)));
    const memUsed = numOrNull(mem.used != null ? mem.used : (mem.used_bytes != null ? mem.used_bytes : (mem.used_kb != null ? Number(mem.used_kb) * 1024 : null)));
    const memPercent = percent(
      mem.used_percent != null ? mem.used_percent
        : (mem.percent != null ? mem.percent
          : (mem.usage_percent != null ? mem.usage_percent : (memUsed != null && memTotal ? memUsed / memTotal : null)))
    );
    const diskTotal = numOrNull(disk.total != null ? disk.total : (disk.total_bytes != null ? disk.total_bytes : null));
    const diskUsed = numOrNull(disk.used != null ? disk.used : (disk.used_bytes != null ? disk.used_bytes : null));
    const diskPercent = percent(
      disk.percent != null ? disk.percent
        : (disk.used_percent != null ? disk.used_percent
          : (disk.usage_percent != null ? disk.usage_percent : (diskUsed != null && diskTotal ? diskUsed / diskTotal : null)))
    );
    const cores = numOrNull(host.cores || host.cpu_count || (host.cpu && host.cpu.cores));
    const docker = data.docker || {};
    let containers = pickArray(docker.containers || [], ['containers', 'items', 'rows', 'list']);
    if (!containers.length && docker.container && typeof docker.container === 'object') {
      containers = [Object.assign({}, docker.container, docker.stats || {})];
    }
    const loadRatio = cores && load1 != null ? load1 / cores : null;
    return {
      raw: data,
      load1: load1, load5: load5, load15: load15,
      processes: textOf(load.processes || ''),
      memTotal: memTotal, memUsed: memUsed, memPercent: memPercent,
      memHuman: textOf(mem.used_human || ''),
      memTotalHuman: textOf(mem.total_human || ''),
      diskKey: diskKey,
      diskTotal: diskTotal, diskUsed: diskUsed, diskPercent: diskPercent,
      diskTotalHuman: textOf(disk.total_human || ''),
      diskFreeHuman: textOf(disk.free_human || ''),
      uptime: host.uptime_text || host.uptime || host.uptime_seconds,
      cores: cores,
      hostName: textOf(host.hostname || host.host || ''),
      containers: containers,
      dockerStats: docker.stats || null,
      collectedAt: host.now_text || data.collected_at || data.checked_at || null,
      duration: data.duration_ms != null ? data.duration_ms : data.elapsed_ms,
      agent: data.agent || null,
      loadRatio: loadRatio,
    };
  }

  function normalizeImageList(payload) {
    const data = payload || {};
    const list = Array.isArray(data) ? data : pickArray(data.images || data.items || data, ['images', 'items', 'rows', 'list']);
    return list.filter(function (item) { return item && typeof item === 'object'; }).map(function (item) {
      const image = textOf(item.image || item.name || (item.repository && item.tag ? item.repository + ':' + item.tag : (item.repository || item.id || '')));
      return Object.assign({}, item, {
        image: image,
        id: textOf(item.id || item.image_id || ''),
        size: item.size != null ? item.size : (item.size_text || ''),
        created: item.created || item.created_at || item.created_since || item.CreatedSince || '',
      });
    });
  }

  function imageIdText(row) {
    const id = textOf(row.id || '');
    return id.replace(/^sha256:/, '').slice(0, 12);
  }

  function isRollbackImage(row) {
    return /^fm-cosmobot:runtime-/.test(textOf(row.image || ''));
  }

  defineView({
    id: 'ops',
    name: '运维监控',
    icon: 'activity',
    skeleton: 'default',
    crumb(route) {
      const tab = OPS_TABS.find(function (item) { return item.id === route.segments[1]; });
      return tab ? tab.label : null;
    },
    async render(ctx) {
      const root = ctx.root;
      const tab = OPS_TABS.some(function (item) { return item.id === ctx.segments[1]; }) ? ctx.segments[1] : 'metrics';
      root.innerHTML = pageHead({
        title: '运维监控',
        icon: 'activity',
        sub: '宿主机与容器指标、镜像清单、日志查看器、镜像回滚与定时任务（经 host-ops-agent 代理，只读为主）',
        actions: [
          '<button class="btn btn-ghost btn-sm" data-retry="1" type="button">' + icon('refresh') + '刷新当前页</button>',
          '<a class="btn btn-ghost btn-sm" href="#/deploy">' + icon('server') + '部署面板</a>',
          '<a class="btn btn-ghost btn-sm" href="#/selftest">' + icon('shield') + '接口自检</a>',
        ],
      }) +
      tabsHtml(OPS_TABS.map(function (item) { return { id: item.id, label: item.label, icon: item.icon }; }), tab) +
      '<div id="ops-content">' +
        '<div class="sk sk-title"></div><div class="sk sk-block" style="height:120px"></div><div class="sk sk-block"></div>' +
      '</div>';

      const content = document.getElementById('ops-content');
      const tabsRoot = root.querySelector('.tabs');
      if (tabsRoot) {
        tabsRoot.addEventListener('click', function (event) {
          const button = event.target.closest('[data-tab]');
          if (button) go(hashFor('ops', [button.getAttribute('data-tab')], {}));
        });
      }

      if (tab === 'metrics') await renderOpsMetrics(ctx, content);
      else if (tab === 'images') await renderOpsImages(ctx, content);
      else if (tab === 'logs') await renderOpsLogs(ctx, content);
      else if (tab === 'rollback') await renderOpsRollback(ctx, content);
      else await renderOpsCron(ctx, content);
      guardAssert(ctx, 'ops');
    },
  });

  async function renderOpsMetrics(ctx, content) {
    const result = await opsTry(['/api/v2/ops/metrics'], ctx, { timeout: 25000 });
    if (!ctx.alive()) return;
    if (!result.ok) {
      content.innerHTML = (result.pending
        ? pendingState({ title: '运维指标接口尚未就绪', hint: 'GET /api/v2/ops/metrics 需要宿主机 host-ops-agent 的 metrics 能力（load / mem / disk / uptime + docker stats）。', path: 'GET /api/v2/ops/metrics' })
        : errorState(result.error)) +
        (result.error && /15\.0s|未完成|timeout|超时/i.test(result.error.message || '') ? notice('warn', '宿主机代理处理超时：接口会返回 request_id，可稍等 1–2 秒后刷新，或到「部署与运维」查看代理心跳。') : '') +
        '<div class="notice mt-16">' + icon('info') + '<div>部署面板（<a href="#/deploy">#/deploy</a>）已有宿主代理的镜像 / 容器快照，可作为降级查看入口。</div></div>';
      return;
    }
    const metrics = normalizeMetrics(result.data || {});
    const loadRatio = metrics.loadRatio;
    const memHint = metrics.memHuman && metrics.memTotalHuman
      ? metrics.memHuman + ' / ' + metrics.memTotalHuman
      : (fmtBytes(metrics.memUsed) + ' / ' + fmtBytes(metrics.memTotal));
    const diskHint = metrics.diskTotalHuman
      ? (metrics.diskFreeHuman ? '空闲 ' + metrics.diskFreeHuman + ' · ' : '') + '挂载 ' + (metrics.diskKey || '/')
      : (fmtBytes(metrics.diskUsed) + ' / ' + fmtBytes(metrics.diskTotal));
    content.innerHTML =
      kpiGrid([
        { label: '负载 1m', icon: 'activity', valueHtml: fmtNum(metrics.load1, 2), hint: '5m ' + fmtNum(metrics.load5, 2) + ' · 15m ' + fmtNum(metrics.load15, 2) + (metrics.cores ? ' · ' + metrics.cores + ' 核' : '') },
        { label: '内存占用', icon: 'cpu', valueHtml: fmtPercent(metrics.memPercent), tone: (percent(metrics.memPercent) || 0) > 90 ? 'bad' : 'acc', hint: memHint },
        { label: '磁盘占用', icon: 'database', valueHtml: fmtPercent(metrics.diskPercent), tone: (percent(metrics.diskPercent) || 0) > 90 ? 'bad' : '', hint: diskHint },
        { label: '容器数', icon: 'server', valueHtml: fmtNum(metrics.containers.length), hint: metrics.dockerStats ? 'CPU ' + metrics.dockerStats.cpu_percent + ' · 内存 ' + metrics.dockerStats.mem_usage : 'docker stats 摘要' },
      ]) +
      '<div class="grid grid-wide mt-16">' +
        card({ title: '主机指标', icon: 'server', headActions: [
          '<span class="muted-2 text-xs">采集 ' + esc(ago(metrics.collectedAt) || metrics.collectedAt || '—') + '</span>',
          '<button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(metrics.raw).slice(0, 8000)) + '" data-json-title="指标原始响应" type="button">' + icon('eye') + 'JSON</button>',
        ] },
          '<div class="v3-list">' +
            '<div class="v3-list-item"><span class="v3-meter-label">内存</span>' + barHtml(metrics.memPercent) +
              '<span class="v3-meter-value mono">' + esc(fmtPercent(metrics.memPercent)) + '</span></div>' +
            '<div class="v3-list-item"><span class="v3-meter-label">磁盘</span>' + barHtml(metrics.diskPercent) +
              '<span class="v3-meter-value mono">' + esc(fmtPercent(metrics.diskPercent)) + '</span></div>' +
            (loadRatio != null ? '<div class="v3-list-item"><span class="v3-meter-label">负载/核</span>' + barHtml(loadRatio * 100) +
              '<span class="v3-meter-value mono">' + esc((loadRatio * 100).toFixed(0)) + '%</span></div>' : '') +
          '</div>' +
          '<div class="kv-grid mt-12">' +
            '<div class="kv-item"><div class="k">主机</div><div class="v mono">' + esc(metrics.hostName || '—') + '</div></div>' +
            '<div class="kv-item"><div class="k">运行时长</div><div class="v">' + esc(typeof metrics.uptime === 'number' ? fmtDuration(metrics.uptime * 1000) : (metrics.uptime || '—')) + '</div></div>' +
            '<div class="kv-item"><div class="k">CPU 核数 / 进程</div><div class="v mono">' + esc(metrics.cores == null ? '—' : metrics.cores) + (metrics.processes ? ' / ' + esc(metrics.processes) : '') + '</div></div>' +
            '<div class="kv-item"><div class="k">采集耗时</div><div class="v mono">' + (metrics.duration != null ? fmtNum(metrics.duration) + ' ms' : '—') + '</div></div>' +
          '</div>') +
        card({ title: '容器资源', icon: 'activity', flush: true },
          '<div id="ops-containers"></div>') +
      '</div>';

    const tableRoot = document.getElementById('ops-containers');
    if (tableRoot) {
      v3Table(tableRoot, {
        stateKey: 'ops-containers',
        rows: metrics.containers,
        countLabel: function (count) { return count + ' 个容器'; },
        columns: [
          { key: 'name', label: '容器', render: function (row) {
            const status = textOf(row.status || '');
            return '<span class="mono">' + esc(textOf(row.name || row.Name || row.container)) + '</span>' +
              (status ? ' <span class="tag ' + (row.running === false ? 'tag-bad' : row.health === 'healthy' ? 'tag-ok' : 'tag-muted') + '">' + esc(status) + (row.health ? ' / ' + esc(row.health) : '') + '</span>' : '');
          } },
          { key: 'image', label: '镜像', hideSm: true, render: function (row) { return '<span class="mono cell-truncate">' + esc(textOf(row.image || '—')) + '</span>'; } },
          { key: 'cpu', label: 'CPU', width: '92px', num: true, render: function (row) { return '<span class="mono">' + esc(textOf(row.cpu_percent || row.cpu || row.CPUPerc || '—')) + '</span>'; } },
          { key: 'mem', label: '内存', width: '160px', render: function (row) { return '<span class="mono">' + esc(textOf(row.mem_usage || row.mem || row.MemUsage || '—')) + '</span>'; } },
          { key: 'net', label: '网络 I/O', width: '150px', hideSm: true, render: function (row) { return '<span class="mono">' + esc(textOf(row.net_io || row.net || row.NetIO || '—')) + '</span>'; } },
          { key: 'pids', label: 'PIDs', width: '76px', num: true, render: function (row) { return '<span class="mono">' + esc(textOf(row.pids || row.PIDs || '—')) + '</span>'; } },
        ],
        empty: { icon: 'server', title: '没有容器资源数据', hint: '宿主机代理可能未返回 docker stats 摘要' },
      });
    }
  }

  async function renderOpsImages(ctx, content) {
    const result = await opsTry(['/api/v2/ops/images'], ctx, { timeout: 30000 });
    let rows = [];
    let source = 'ops/images';
    let pending = false;
    let fallbackUsed = false;
    let warmingNote = '';
    let warming = false;
    if (result.ok) {
      const data = result.data || {};
      rows = normalizeImageList(data);
      warming = data.warming === true;
      warmingNote = textOf(data.note || (warming ? '宿主机镜像缓存首次采集约 20 秒，稍后刷新即可。' : ''));
      if (!rows.length && (warming || data.live === false)) {
        const fallback = await tryApi(['/api/v2/deploy/status'], { timeout: 30000 });
        if (!ctx.alive()) return;
        if (fallback.ok) {
          const deployData = fallback.data || {};
          const fallbackRows = normalizeImageList(deployData.images || {});
          if (fallbackRows.length) {
            rows = fallbackRows;
            fallbackUsed = true;
            source = 'deploy/status';
          }
        }
      }
    } else if (result.pending) {
      const fallback = await tryApi(['/api/v2/deploy/status'], { timeout: 30000 });
      if (!ctx.alive()) return;
      if (fallback.ok) {
        const data = fallback.data || {};
        rows = normalizeImageList(data.images || data || {});
        fallbackUsed = true;
        source = 'deploy/status';
        warmingNote = textOf(data.note || '');
      } else {
        pending = true;
      }
    }
    if (!rows.length && pending) {
      content.innerHTML = pendingState({ title: '镜像接口尚未就绪', hint: 'GET /api/v2/ops/images 需要宿主机代理的 images_detail 能力；旧 deploy/status 也没有可用快照。', path: 'GET /api/v2/ops/images' });
      return;
    }
    content.innerHTML =
      (fallbackUsed ? fallbackNotice('/api/v2/ops/images', '/api/v2/deploy/status') : '') +
      (warmingNote && !rows.length ? notice('warn', '宿主机镜像缓存采集中：' + esc(warmingNote) + '<div class="row mt-8"><button class="btn btn-sm btn-ghost" data-retry="1" type="button">' + icon('refresh') + '稍后刷新</button></div>') : '') +
      kpiGrid([
        { label: '镜像总数', icon: 'database', valueHtml: fmtNum(rows.length), tone: 'acc', hint: warming ? '缓存采集中' : source },
        { label: '可回滚镜像', icon: 'refresh', valueHtml: fmtNum(rows.filter(isRollbackImage).length), hint: '前缀 fm-cosmobot:runtime-' },
        { label: '数据源', icon: 'link', valueHtml: '<span class="tag ' + (fallbackUsed ? 'tag-warn' : 'tag-ok') + '">' + esc(source) + '</span>' },
        { label: '原始响应', icon: 'database', valueHtml: '<button class="btn btn-sm btn-ghost" id="ops-images-json" type="button">' + icon('eye') + '查看</button>' },
      ]) +
      '<div class="card mt-16"><div class="card-head"><h3>' + icon('database') + '镜像清单<span class="section-sub">' + rows.length + ' 个 · 点「回滚」进入回滚确认</span></h3></div>' +
      '<div id="ops-images-table"></div></div>' +
      '<div id="ops-images-result" class="mt-16"></div>';

    const tableRoot = document.getElementById('ops-images-table');
    if (tableRoot) {
      v3Table(tableRoot, {
        stateKey: 'ops-images',
        rows: rows,
        countLabel: function (count) { return count + ' 个镜像'; },
        columns: [
          { key: 'image', label: '镜像', render: function (row) { return '<span class="mono cell-truncate">' + esc(row.image || '—') + '</span>'; } },
          { key: 'id', label: 'ID', width: '110px', mono: true, render: function (row) { return '<span class="mono">' + esc(imageIdText(row) || '—') + '</span>'; } },
          { key: 'size', label: '大小', width: '94px', num: true, render: function (row) { return '<span class="mono">' + esc(textOf(row.size || '—')) + '</span>'; } },
          { key: 'created', label: '创建时间', width: '170px', hideSm: true, render: function (row) { return '<span class="mono">' + esc(textOf(row.created || '—')) + '</span>'; } },
          { key: 'action', label: '', width: '84px', render: function (row) {
            if (!isRollbackImage(row)) return '<span class="muted-2 text-xs">受保护</span>';
            return '<button class="btn btn-sm btn-ghost" data-rollback-image="' + escAttr(row.image) + '" type="button">回滚</button>';
          } },
        ],
        empty: { icon: 'database', title: '没有镜像记录', hint: fallbackUsed ? 'deploy/status 快照为空，可到部署面板点刷新快照' : '宿主机代理可能离线' },
      });
      tableRoot.addEventListener('click', function (event) {
        const button = event.target.closest('[data-rollback-image]');
        if (!button) return;
        go('#/ops/rollback?image=' + encodeURIComponent(button.getAttribute('data-rollback-image')));
      });
    }
    const jsonBtn = document.getElementById('ops-images-json');
    if (jsonBtn) jsonBtn.addEventListener('click', function () {
      openJsonDrawer('镜像接口原始返回', result.ok ? result.data : { error: result.error && result.error.message, source: source });
    });
  }

  function normalizeLogLines(payload) {
    if (payload == null) return [];
    if (typeof payload === 'string') return payload.split(/\r?\n/);
    if (Array.isArray(payload)) {
      const out = [];
      payload.forEach(function (item) {
        if (item == null) return;
        if (typeof item === 'string') out.push(item);
        else if (typeof item === 'object') out.push(textOf(item.line || item.text || item.message || item.msg || JSON.stringify(item)));
      });
      return out;
    }
    if (typeof payload !== 'object') return [String(payload)];
    const keys = ['lines', 'logs', 'entries', 'items', 'tail', 'rows'];
    for (let i = 0; i < keys.length; i += 1) {
      if (Array.isArray(payload[keys[i]])) return normalizeLogLines(payload[keys[i]]);
    }
    if (typeof payload.text === 'string') return payload.text.split(/\r?\n/);
    if (typeof payload.log === 'string') return payload.log.split(/\r?\n/);
    if (typeof payload.content === 'string') return payload.content.split(/\r?\n/);
    if (typeof payload.tail === 'string') return payload.tail.split(/\r?\n/);
    if (payload.data != null) return normalizeLogLines(payload.data);
    return [];
  }

  /* 宿主机代理异步任务：接口超时会返回 request_id，可轮询 /api/v2/ops/result */
  async function pollOpsResult(requestId, ctx, tries) {
    const max = tries || 14;
    for (let i = 0; i < max; i += 1) {
      await sleep(1400);
      if (!ctx.alive()) return null;
      try {
        const response = await apiGet('/api/v2/ops/result' + qs({ request_id: requestId }), { timeout: 15000 });
        const data = unwrap(response) || {};
        if (data.done) return data;
        if (data.agent_online === false && i > 2) return null;
      } catch (err) {
        if (err && err.status === 401) throw err;
        return null;
      }
    }
    return null;
  }

  /* 宿主机代理接口统一封装：504 带 request_id 时轮询 /api/v2/ops/result 直到完成 */
  async function opsTry(apiPaths, ctx, opts) {
    const first = await tryApi(apiPaths, opts || {});
    if (first.ok || !first.error) return first;
    const message = first.error.message || '';
    const match = /request_id=([0-9a-zA-Z-]+)/.exec(message);
    if (!match) return first;
    const done = await pollOpsResult(match[1], ctx);
    if (!done) return first;
    const payload = done.result != null ? done.result : done;
    return {
      ok: true,
      pending: false,
      data: unwrap(payload),
      raw: payload,
      path: '/api/v2/ops/result?request_id=' + match[1],
      viaPoll: true,
    };
  }

  async function renderOpsLogs(ctx, content) {
    const tailValue = clamp(Number(ctx.q('tail', 200)) || 200, 20, 2000);
    content.innerHTML =
      '<div class="card"><div class="tbl-toolbar">' +
        '<div class="field" style="flex:0 0 auto"><label class="field-label">行数</label><select class="select select-sm" id="ops-log-tail">' +
          [100, 200, 500, 1000, 2000].map(function (value) { return '<option value="' + value + '"' + (value === tailValue ? ' selected' : '') + '>最近 ' + value + ' 行</option>'; }).join('') +
        '</select></div>' +
        '<div class="field grow" style="flex:1 1 220px"><input class="input input-sm" id="ops-log-filter" placeholder="过滤日志关键词（仅显示匹配行）"></div>' +
        '<button class="btn btn-sm btn-ghost" id="ops-log-refresh" type="button">' + icon('refresh') + '刷新</button>' +
        '<label class="switch"><input type="checkbox" id="ops-log-auto"><span>自动刷新 5s</span></label>' +
        '<button class="btn btn-sm btn-ghost" id="ops-log-copy" type="button">' + icon('copy') + '复制</button>' +
        '<button class="btn btn-sm btn-ghost" id="ops-log-sse" type="button">' + icon('zap') + '尝试实时流</button>' +
      '</div>' +
      '<div class="card-body flush"><div class="v3-log" id="ops-log-view"><div class="sk sk-line w60"></div><div class="sk sk-line"></div></div></div>' +
      '<div class="card-foot"><span class="muted-2 text-xs" id="ops-log-meta">数据源：GET /api/v2/ops/logs?tail=</span><span class="spacer"></span><span class="muted-2 text-xs" id="ops-log-stat"></span></div></div>' +
      '<div class="notice mt-16">' + icon('info') + '<div>日志来自宿主机代理 <span class="mono">docker logs fm-cosmobot --tail N</span>；「尝试实时流」会建立 <span class="mono">/api/v2/logs/stream</span> SSE 连接，不支持时自动回退轮询。</div></div>';

    const view = document.getElementById('ops-log-view');
    const meta = document.getElementById('ops-log-meta');
    const stat = document.getElementById('ops-log-stat');
    const tailSelect = document.getElementById('ops-log-tail');
    const filterInput = document.getElementById('ops-log-filter');
    const autoInput = document.getElementById('ops-log-auto');
    let allLines = [];
    let timer = 0;
    let eventSource = null;
    let requestSeq = 0;
    let loadingLogs = false;

    function renderLines(lines) {
      if (!lines.length) {
        const keyword = filterInput ? filterInput.value.trim() : '';
        view.innerHTML = keyword
          ? emptyState({ icon: 'search', title: '没有匹配的日志行', hint: '关键词：' + keyword })
          : emptyState({ icon: 'terminal', title: '没有日志内容', hint: '接口返回为空；机器人可能刚重启或日志文件为空' });
        return;
      }
      view.innerHTML = lines.map(function (line) {
        const tone = /error|exception|traceback|failed|失败/i.test(line) ? ' bad' : (/warn|警告/i.test(line) ? ' warn' : '');
        return '<div class="v3-log-line' + tone + '">' + esc(line) + '</div>';
      }).join('');
    }

    function paint() {
      const keyword = filterInput ? filterInput.value.trim() : '';
      const lines = keyword ? allLines.filter(function (line) { return line.indexOf(keyword) >= 0; }) : allLines;
      renderLines(lines);
      if (stat) stat.textContent = lines.length + ' 行' + (keyword ? ' / 共 ' + allLines.length + ' 行' : '');
      view.scrollTop = view.scrollHeight;
    }

    async function loadLogs(silent) {
      if (loadingLogs) return;
      loadingLogs = true;
      const seq = ++requestSeq;
      try {
        const result = await opsTry(['/api/v2/ops/logs' + qs({ tail: tailSelect ? tailSelect.value : tailValue })], ctx, { timeout: 25000 });
        if (seq !== requestSeq || !ctx.alive()) return;
        if (!result.ok) {
          if (stat) stat.textContent = '';
          const message = (result.error && result.error.message) || '';
          const requestMatch = /request_id=([0-9a-zA-Z-]+)/.exec(message);
          view.innerHTML = result.pending
            ? pendingState({ title: '日志接口尚未就绪', hint: 'GET /api/v2/ops/logs?tail= 需要宿主代理的 logs 能力。', path: 'GET /api/v2/ops/logs' })
            : errorState(result.error, 'ops-log-retry') +
              (requestMatch ? notice('warn', '宿主机代理任务排队超时（request_id ' + esc(requestMatch[1]) + '）。点「刷新」重试，或稍后再回来查看。') : '');
          const retry = document.getElementById('ops-log-retry');
          if (retry) retry.addEventListener('click', function () { loadLogs(); });
          if (meta) meta.textContent = '接口：GET /api/v2/ops/logs?tail=' + (tailSelect ? tailSelect.value : tailValue);
          return;
        }
        allLines = normalizeLogLines(result.data);
        if (meta) meta.textContent = '接口：' + (result.path || '/api/v2/ops/logs') + ' · 更新 ' + fmtTime(Date.now());
        if (silent === true) {
          const keyword = filterInput ? filterInput.value.trim() : '';
          const lines = keyword ? allLines.filter(function (line) { return line.indexOf(keyword) >= 0; }) : allLines;
          const atBottom = Math.abs(view.scrollHeight - view.scrollTop - view.clientHeight) < 30;
          renderLines(lines);
          if (stat) stat.textContent = lines.length + ' 行';
          if (atBottom) view.scrollTop = view.scrollHeight;
        } else {
          paint();
        }
      } finally {
        loadingLogs = false;
      }
    }

    function stopAuto() {
      if (timer) { clearInterval(timer); timer = 0; }
    }
    function scheduleAuto() {
      stopAuto();
      if (!autoInput || !autoInput.checked) return;
      timer = setInterval(function () {
        if (!ctx.alive() || document.hidden) return;
        loadLogs(true);
      }, 5000);
    }
    ctx.cleanup(function () {
      stopAuto();
      if (eventSource) { try { eventSource.close(); } catch (err) { /* ignore */ } }
    });

    if (tailSelect) tailSelect.addEventListener('change', function () {
      go(hashFor('ops', ['logs'], new URLSearchParams({ tail: tailSelect.value })));
    });
    if (filterInput) filterInput.addEventListener('input', debounce(paint, 220));
    const refreshBtn = document.getElementById('ops-log-refresh');
    if (refreshBtn) refreshBtn.addEventListener('click', function () { loadLogs(); });
    if (autoInput) autoInput.addEventListener('change', function () {
      scheduleAuto();
      toast(autoInput.checked ? '已开启日志自动刷新（5 秒）' : '已停止日志自动刷新', 'info', { timeout: 1800 });
    });
    const copyBtn = document.getElementById('ops-log-copy');
    if (copyBtn) copyBtn.addEventListener('click', function () {
      copyText(allLines.join('\n')).then(function () { toast('已复制 ' + allLines.length + ' 行日志', 'ok'); })
        .catch(function () { toast('复制失败', 'bad'); });
    });
    const sseBtn = document.getElementById('ops-log-sse');
    if (sseBtn) sseBtn.addEventListener('click', function () {
      if (eventSource) { try { eventSource.close(); } catch (err) { /* ignore */ } eventSource = null; sseBtn.disabled = false; sseBtn.innerHTML = icon('zap') + '尝试实时流'; return; }
      if (typeof EventSource === 'undefined') { toast('浏览器不支持 EventSource', 'warn'); return; }
      if (meta) meta.textContent = '正在连接 /api/v2/logs/stream（SSE）…';
      sseBtn.disabled = true;
      sseBtn.innerHTML = icon('clock') + '连接中…';
      const source = new EventSource('/api/v2/logs/stream');
      eventSource = source;
      source.onopen = function () {
        if (meta) meta.textContent = 'SSE 已连接：/api/v2/logs/stream';
        sseBtn.innerHTML = icon('pause') + '停止实时流';
      };
      source.onmessage = function (event) {
        const line = textOf(event.data);
        if (!line || line.length > 4000) return;
        allLines.push(line);
        if (allLines.length > 4000) allLines = allLines.slice(-3000);
        paint();
      };
      source.onerror = function () {
        try { source.close(); } catch (err) { /* ignore */ }
        eventSource = null;
        sseBtn.disabled = false;
        sseBtn.innerHTML = icon('zap') + '尝试实时流';
        if (meta) meta.textContent = 'SSE 不可用（接口未就绪或代理不支持流式），已回退轮询模式';
        toast('实时流不可用，已回退轮询', 'warn');
      };
    });

    loadLogs();
  }

  async function renderOpsRollback(ctx, content) {
    const [imagesRes, deployRes] = await Promise.all([
      opsTry(['/api/v2/ops/images'], ctx, { timeout: 30000 }),
      tryApi(['/api/v2/deploy/status'], { timeout: 30000 }),
    ]);
    if (!ctx.alive()) return;
    let images = [];
    if (imagesRes.ok) images = normalizeImageList(imagesRes.data || {});
    else if (deployRes.ok) {
      const data = deployRes.data || {};
      images = normalizeImageList(data.images || {});
    }
    const running = deployRes.ok ? ((deployRes.data || {}).running || {}) : {};
    const runningImage = textOf(running.image || '');
    const rollbackImages = images.filter(isRollbackImage);
    const presetImage = ctx.q('image');
    const selected = presetImage || runningImage || (rollbackImages[0] && rollbackImages[0].image) || '';
    const customPreset = presetImage && !rollbackImages.some(function (row) { return row.image === presetImage; }) ? presetImage : '';

    content.innerHTML =
      (imagesRes.ok ? '' : fallbackNotice('/api/v2/ops/images', '/api/v2/deploy/status')) +
      kpiGrid([
        { label: '当前运行镜像', icon: 'server', valueHtml: '<span class="mono" style="font-size:12.5px">' + esc(runningImage || '未知') + '</span>', hint: running.name ? esc(running.name) + ' · ' + esc(running.status || '') : '来自 deploy/status' },
        { label: '可回滚镜像', icon: 'database', valueHtml: fmtNum(rollbackImages.length), hint: 'fm-cosmobot:runtime-*' },
        { label: '回滚通道', icon: 'shield', valueHtml: '<span class="tag ' + (rollbackImages.length ? 'tag-warn' : 'tag-muted') + '">' + (rollbackImages.length ? 'POST /api/v2/ops/rollback' : '接口未就绪') + '</span>' },
        { label: '保护策略', icon: 'lock', valueHtml: '<span style="font-size:13px">严格白名单</span>', hint: '仅允许存在且前缀匹配的镜像' },
      ]) +
      '<div class="grid grid-wide mt-16">' +
        card({ title: '镜像回滚', icon: 'refresh', className: 'danger' },
          '<div class="notice warn">' + icon('alert') + '<div>回滚会用选定镜像重建 fm-cosmobot 容器（停止 → 用原 run 参数 recreate）。期间机器人离线约 10–60 秒。<b>这是危险操作</b>，需要输入确认词。</div></div>' +
          '<div class="stack mt-12" style="gap:10px">' +
            '<div class="field"><label>选择镜像</label><select class="select" id="rollback-select">' +
              (rollbackImages.length
                ? rollbackImages.map(function (row) { return '<option value="' + escAttr(row.image) + '"' + (row.image === selected ? ' selected' : '') + '>' + esc(row.image) + ' · ' + esc(textOf(row.size || '')) + '</option>'; }).join('')
                : '<option value="">（没有可回滚镜像，接口未就绪或代理离线）</option>') +
            '</select></div>' +
            '<div class="field"><label>或输入镜像名（必须已存在且前缀正确）</label><input class="input mono" id="rollback-image-input" placeholder="fm-cosmobot:runtime-20261113-002403" value="' + escAttr(customPreset) + '"></div>' +
            '<div class="row"><button class="btn btn-danger-solid btn-sm" id="rollback-run" type="button"' + (rollbackImages.length ? '' : ' disabled') + '>' + icon('refresh') + '执行回滚</button>' +
            '<span class="muted-2 text-xs">会调用 POST /api/v2/ops/rollback {image}</span></div>' +
            '<div id="rollback-result"></div>' +
          '</div>') +
        card({ title: '镜像快照', icon: 'database', flush: true },
          '<div id="rollback-images"></div>') +
      '</div>';

    const tableRoot = document.getElementById('rollback-images');
    if (tableRoot) {
      v3Table(tableRoot, {
        stateKey: 'ops-rollback-images',
        rows: images,
        countLabel: function (count) { return count + ' 个镜像'; },
        columns: [
          { key: 'image', label: '镜像', render: function (row) {
            const isCurrent = row.image === runningImage;
            return '<span class="mono cell-truncate">' + esc(row.image) + '</span>' + (isCurrent ? ' <span class="tag tag-ok">运行中</span>' : '');
          } },
          { key: 'size', label: '大小', width: '94px', num: true, render: function (row) { return '<span class="mono">' + esc(textOf(row.size || '—')) + '</span>'; } },
          { key: 'created', label: '创建', width: '160px', hideSm: true, render: function (row) { return '<span class="mono">' + esc(textOf(row.created || '—')) + '</span>'; } },
        ],
        empty: { icon: 'database', title: '没有镜像记录', hint: '接口未就绪或宿主机代理离线' },
      });
    }

    const runBtn = document.getElementById('rollback-run');
    if (runBtn) runBtn.addEventListener('click', async function () {
      const select = document.getElementById('rollback-select');
      const custom = document.getElementById('rollback-image-input');
      const customValue = custom ? custom.value.trim() : '';
      const image = customValue || (select ? select.value : '');
      const resultBox = document.getElementById('rollback-result');
      if (!image) { toast('请选择或输入镜像', 'warn'); return; }
      if (!isRollbackImage({ image: image })) { toast('镜像必须匹配 fm-cosmobot:runtime-*', 'bad'); return; }
      const ok = await confirmAction({
        title: '回滚 fm-cosmobot？',
        detail: '目标镜像：' + image + '。容器会被重建，机器人离线约 10–60 秒，期间群消息不会回复。',
        confirmLabel: '执行回滚',
        danger: true,
        requireText: 'ROLLBACK',
      });
      if (!ok) return;
      runBtn.disabled = true;
      resultBox.innerHTML = '<div class="sk sk-line w60" style="height:14px"></div>';
      try {
        const response = await apiPost('/api/v2/ops/rollback', { image: image }, { timeout: 60000 });
        const data = unwrap(response) || {};
        const protectedMsg = data.protected || data.requires_manual || data.manual;
        resultBox.innerHTML = (protectedMsg
          ? notice('warn', '后端已拒绝自动回滚：<b>' + esc(data.message || data.note || '受保护操作，需要人工执行') + '</b><br>请按运维流程人工处理，或用旧版后台 / 服务器命令行操作。')
          : notice('ok', '回滚请求已执行' + (data.container ? ' · 容器 ' + esc(data.container) : '') +
            (data.image ? ' · 镜像 ' + esc(data.image) : '') +
            (data.message ? '<br>' + esc(data.message) : ''))) +
          '<div class="row mt-8"><button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(data).slice(0, 6000)) + '" data-json-title="回滚返回" type="button">' + icon('eye') + '完整返回</button>' +
          '<a class="btn btn-sm btn-ghost" href="#/deploy">' + icon('server') + '去部署面板看状态</a></div>';
        toast(protectedMsg ? '回滚被保护策略拒绝' : '回滚请求已提交', protectedMsg ? 'warn' : 'ok');
      } catch (err) {
        resultBox.innerHTML = errorState(err) +
          '<div class="muted-2 text-xs mt-8">404/501 表示回滚接口尚未就绪；403 表示当前登录账号只读。此操作无法在只读会话里执行。</div>';
        toast('回滚失败：' + err.message, 'bad');
      } finally {
        runBtn.disabled = false;
      }
    });
  }

  async function renderOpsCron(ctx, content) {
    const result = await opsTry(['/api/v2/ops/cron'], ctx, { timeout: 20000 });
    if (!ctx.alive()) return;
    if (!result.ok) {
      content.innerHTML = (result.pending
        ? pendingState({ title: '定时任务接口尚未就绪', hint: 'GET /api/v2/ops/cron 通过宿主机代理只读 crontab -l；就绪前可在服务器执行 crontab -l 查看。', path: 'GET /api/v2/ops/cron' })
        : errorState(result.error));
      return;
    }
    const data = result.data || {};
    let text = '';
    if (typeof data === 'string') text = data;
    else if (typeof data.crontab === 'string') text = data.crontab;
    else if (typeof data.text === 'string') text = data.text;
    else if (Array.isArray(data.lines)) text = data.lines.join('\n');
    else if (Array.isArray(data.cron)) text = data.cron.map(function (line) { return typeof line === 'string' ? line : JSON.stringify(line); }).join('\n');
    else if (Array.isArray(data)) text = data.map(function (line) { return typeof line === 'string' ? line : JSON.stringify(line); }).join('\n');
    else text = JSON.stringify(data, null, 2);
    const lines = text ? text.split(/\r?\n/) : [];
    const active = lines.filter(function (line) { return line.trim() && line.trim().charAt(0) !== '#'; });
    content.innerHTML = kpiGrid([
      { label: '任务行数', icon: 'clock', valueHtml: fmtNum(lines.length), tone: 'acc' },
      { label: '生效任务', icon: 'play', valueHtml: fmtNum(active.length), hint: '非注释行' },
      { label: '只读', icon: 'lock', valueHtml: '<span class="tag tag-ok">只读展示</span>', hint: '不支持从后台改写 crontab' },
      { label: '来源', icon: 'link', valueHtml: '<span class="tag tag-info mono">' + esc(result.path || '/api/v2/ops/cron') + '</span>' },
    ]) +
    '<div class="card mt-16"><div class="card-head"><h3>' + icon('clock') + 'crontab<span class="section-sub">宿主机 root crontab -l</span></h3>' +
      '<div class="card-head-actions"><button class="btn btn-sm btn-ghost" id="ops-cron-copy" type="button">' + icon('copy') + '复制</button>' +
      '<button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(data).slice(0, 8000)) + '" data-json-title="cron 原始返回" type="button">' + icon('eye') + 'JSON</button></div></div>' +
      '<div class="card-body flush"><pre class="v3-cron mono">' + esc(text || '（空 crontab）') + '</pre></div></div>' +
    '<div class="notice mt-16">' + icon('info') + '<div>定时任务页为只读；修改 crontab 需要服务器 root 权限，请走运维流程或在服务器执行 <span class="mono">crontab -e</span>。</div></div>';
    const copyBtn = document.getElementById('ops-cron-copy');
    if (copyBtn) copyBtn.addEventListener('click', function () {
      copyText(text).then(function () { toast('已复制 crontab', 'ok'); }).catch(function () { toast('复制失败', 'bad'); });
    });
  }

  /* ── 13. 页面：备份与导出 ───────────────────────────────────────────── */

  function normalizeBackupRows(payload) {
    const list = pickArray(payload, ['backups', 'items', 'files', 'rows', 'list', 'backup']);
    return list.map(function (item) {
      if (typeof item === 'string') return { name: item.split('/').pop(), path: item, db: /domain/i.test(item) ? 'domain' : 'bot' };
      if (!item || typeof item !== 'object') return null;
      const path = textOf(item.path || item.file || item.name || '');
      return {
        name: item.name || path.split('/').pop(),
        path: path,
        size: item.size != null ? item.size : item.bytes,
        mtime: item.mtime != null ? item.mtime : (item.modified_at != null ? item.modified_at : item.created_at),
        db: textOf(item.db || item.type || (item.kind || (/domain/i.test(path) ? 'domain' : 'bot'))),
        raw: item,
      };
    }).filter(Boolean);
  }

  async function doExport(path, filename) {
    const button = document.querySelector('[data-export-path="' + path + '"]');
    if (button) { button.disabled = true; button.innerHTML = icon('clock') + '导出中…'; }
    try {
      const name = await downloadFile(path, filename);
      toast('已开始下载：' + name, 'ok', { timeout: 5000 });
    } catch (err) {
      toast('导出失败：' + err.message, 'bad', { timeout: 8000 });
    } finally {
      if (button) {
        button.disabled = false;
        button.innerHTML = button.getAttribute('data-export-label') || (icon('download') + '导出');
      }
    }
  }

  defineView({
    id: 'backup',
    name: '备份与导出',
    icon: 'download',
    skeleton: 'table',
    async render(ctx) {
      const listRes = await tryApi(['/api/v2/backup/list'], { timeout: 20000 });
      if (!ctx.alive()) return;
      const backups = listRes.ok ? normalizeBackupRows(listRes.data || {}) : [];
      const totalSize = backups.reduce(function (sum, row) { return sum + (Number(row.size) || 0); }, 0);
      const latest = backups.slice().sort(function (a, b) { return (toMs(b.mtime) || 0) - (toMs(a.mtime) || 0); })[0];
      const backupMeta = listRes.ok ? (listRes.data || {}) : {};
      const freeHuman = textOf(backupMeta.free_human || (backupMeta.free_bytes != null ? fmtBytes(backupMeta.free_bytes) : ''));
      const backupDir = textOf(backupMeta.dir || '/opt/fm-admin/data/backups');
      const backupNote = textOf(backupMeta.note || '');
      const days = clamp(Number(ctx.q('days', 7)) || 7, 1, 365);

      const backupCard = listRes.ok
        ? card({ title: '备份文件', icon: 'database', flush: true, headActions: [
            '<span class="muted text-xs">' + backups.length + ' 个文件</span>',
            '<button class="btn btn-sm btn-ghost" data-retry="1" type="button">' + icon('refresh') + '刷新</button>',
          ] },
          '<div class="tbl-toolbar">' +
            '<div class="field" style="flex:0 0 auto"><label class="field-label">新建备份</label><select class="select select-sm" id="backup-db">' +
              [['bot', 'bot（机器人库）'], ['domain', 'domain（业务库）']].map(function (pair) {
                return '<option value="' + pair[0] + '">' + pair[1] + '</option>';
              }).join('') +
            '</select></div>' +
            '<button class="btn btn-soft btn-sm" id="backup-create" type="button">' + icon('save') + '立即备份</button>' +
            '<span class="muted-2 text-xs">备份目录：<span class="mono">' + esc(backupDir) + '</span></span>' +
          '</div>' +
          '<div id="backup-table"></div>' +
          '<div id="backup-result" class="card-body"></div>')
        : (listRes.pending
          ? pendingState({ title: '备份列表接口尚未就绪', hint: 'GET /api/v2/backup/list 会列出 /opt/fm-admin/data/backups/*.sqlite3；就绪前仍可尝试创建备份。', path: 'GET /api/v2/backup/list' })
          : errorState(listRes.error)) + codeTip(['GET /api/v2/backup/list']);

      ctx.root.innerHTML = pageHead({
        title: '备份与导出',
        icon: 'download',
        sub: 'SQLite 备份列表 / 一键备份 + 聊天、审计 CSV 导出（导出走 v2 接口，带筛选参数）',
        actions: [
          '<button class="btn btn-ghost btn-sm" data-retry="1" type="button">' + icon('refresh') + '刷新</button>',
          '<a class="btn btn-ghost btn-sm" href="#/browse">' + icon('database') + '数据浏览器</a>',
        ],
      }) +
      kpiGrid([
        { label: '备份文件', icon: 'database', valueHtml: fmtNum(backups.length), tone: 'acc', hint: listRes.ok ? '目录 ' + esc(backupDir) : '接口未就绪' },
        { label: '总大小', icon: 'save', valueHtml: fmtBytes(totalSize), hint: backups.length ? '平均 ' + fmtBytes(totalSize / backups.length) : '还没有备份文件' },
        { label: '磁盘剩余', icon: 'server', valueHtml: esc(freeHuman || '—'), hint: textOf(backupMeta.disk_total_human || '') ? '总容量 ' + esc(backupMeta.disk_total_human) : '来自宿主机 df' },
        { label: '最新备份', icon: 'clock', valueHtml: '<span style="font-size:14px">' + esc(latest ? ago(latest.mtime) : '—') + '</span>', hint: latest ? esc(latest.name) : '点「立即备份」生成第一份' },
      ]) +
      (backupNote ? notice('info', esc(backupNote)) : '') +
      backupCard +
      '<div class="grid grid-2 mt-16">' +
        card({ title: '导出聊天记录 CSV', icon: 'download' },
          '<div class="filters">' +
            '<div class="field"><label>天数</label><select class="select select-sm" id="export-chat-days">' +
              [1, 3, 7, 14, 30, 90].map(function (value) { return '<option value="' + value + '"' + (value === days ? ' selected' : '') + '>最近 ' + value + ' 天</option>'; }).join('') +
            '</select></div>' +
            '<div class="field grow"><label>群 ID（可选）</label><input class="input input-sm mono" id="export-chat-chat" placeholder="留空 = 全部群" value="' + escAttr(ctx.q('chat')) + '"></div>' +
          '</div>' +
          '<div class="row mt-12">' +
            '<button class="btn btn-soft btn-sm" data-export-path="/api/v2/export/chat" data-export-label="' + escAttr(icon('download') + '导出聊天 CSV') + '" id="export-chat" type="button">' + icon('download') + '导出聊天 CSV</button>' +
            '<span class="muted-2 text-xs">GET /api/v2/export/chat?days=&amp;chat=</span>' +
          '</div>') +
        card({ title: '导出审计日志 CSV', icon: 'shield' },
          '<div class="filters">' +
            '<div class="field"><label>天数</label><select class="select select-sm" id="export-audit-days">' +
              [1, 7, 30, 90, 365].map(function (value) { return '<option value="' + value + '"' + (value === days ? ' selected' : '') + '>最近 ' + value + ' 天</option>'; }).join('') +
            '</select></div>' +
          '</div>' +
          '<div class="row mt-12">' +
            '<button class="btn btn-soft btn-sm" data-export-path="/api/v2/export/audit" data-export-label="' + escAttr(icon('shield') + '导出审计 CSV') + '" id="export-audit" type="button">' + icon('shield') + '导出审计 CSV</button>' +
            '<span class="muted-2 text-xs">GET /api/v2/export/audit?days=</span>' +
          '</div>') +
      '</div>' +
      '<div class="notice mt-16">' + icon('info') + '<div>导出接口若返回 404/501 会弹出明确错误；如果返回 JSON 错误体而不是 CSV，页面会显示后端原因而不会静默失败。</div></div>';

      if (backups.length) {
        const tableRoot = document.getElementById('backup-table');
        if (tableRoot) {
          v3Table(tableRoot, {
            stateKey: 'backup-list',
            rows: backups,
            countLabel: function (count) { return count + ' 个备份'; },
            columns: [
              { key: 'name', label: '文件名', mono: true, render: function (row) { return '<span class="mono cell-truncate">' + esc(row.name) + '</span>'; } },
              { key: 'db', label: '库', width: '92px', render: function (row) { return '<span class="tag ' + (row.db === 'domain' ? 'tag-info' : 'tag-muted') + '">' + esc(row.db || 'bot') + '</span>'; } },
              { key: 'size', label: '大小', width: '100px', num: true, render: function (row) { return '<span class="mono">' + esc(fmtBytes(row.size)) + '</span>'; } },
              { key: 'mtime', label: '时间', width: '168px', render: function (row) { return '<span class="mono">' + esc(fmtTime(row.mtime) || '—') + '</span>'; } },
              { key: 'path', label: '路径', hideSm: true, render: function (row) { return '<span class="mono cell-truncate muted-2">' + esc(row.path) + '</span>'; } },
            ],
            empty: { icon: 'database', title: '还没有备份文件', hint: '点上方「立即备份」创建第一份' },
          });
        }
      }

      const createBtn = document.getElementById('backup-create');
      if (createBtn) createBtn.addEventListener('click', async function () {
        const dbSelect = document.getElementById('backup-db');
        const db = dbSelect ? dbSelect.value : 'bot';
        const ok = await confirmAction({
          title: '创建 ' + db + ' 库备份？',
          detail: '会调用 POST /api/v2/backup/create {db:"' + db + '"}，把 sqlite3 备份写到 /opt/fm-admin/data/backups/。该操作只读数据库、不修改线上数据。',
          confirmLabel: '开始备份',
        });
        if (!ok) return;
        createBtn.disabled = true;
        createBtn.innerHTML = icon('clock') + '备份中…';
        const resultRoot = document.getElementById('backup-result');
        if (resultRoot) resultRoot.innerHTML = '<div class="sk sk-line w60" style="height:14px"></div>';
        try {
          const response = await apiPost('/api/v2/backup/create', { db: db }, { timeout: 60000 });
          const data = unwrap(response) || {};
          if (resultRoot) {
            resultRoot.innerHTML = notice('ok', '备份完成' +
              (data.path || data.file ? '<br>路径：<span class="mono">' + esc(data.path || data.file) + '</span>' : '') +
              (data.size != null ? ' · ' + fmtBytes(data.size) : '') +
              (data.elapsed_ms != null ? ' · 耗时 ' + fmtNum(data.elapsed_ms) + ' ms' : '')) +
              '<div class="row mt-8"><button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(data).slice(0, 6000)) + '" data-json-title="备份返回" type="button">' + icon('eye') + '完整返回</button>' +
              '<button class="btn btn-sm btn-ghost" data-retry="1" type="button">' + icon('refresh') + '刷新列表</button></div>';
          }
          toast('备份已创建', 'ok');
          setTimeout(function () { if (ctx.alive()) route(); }, 900);
        } catch (err) {
          if (resultRoot) resultRoot.innerHTML = errorState(err) + '<div class="muted-2 text-xs mt-8">404/501 表示备份接口尚未就绪；403 表示当前账号只读。</div>';
          toast('备份失败：' + err.message, 'bad');
        } finally {
          createBtn.disabled = false;
          createBtn.innerHTML = icon('save') + '立即备份';
        }
      });

      const exportChatBtn = document.getElementById('export-chat');
      if (exportChatBtn) exportChatBtn.addEventListener('click', function () {
        const daysEl = document.getElementById('export-chat-days');
        const chatEl = document.getElementById('export-chat-chat');
        const query = { days: daysEl ? daysEl.value : days };
        const chat = chatEl ? chatEl.value.trim() : '';
        if (chat) query.chat = chat;
        doExport('/api/v2/export/chat' + qs(query), 'fm-chat-export-' + (query.days) + 'd.csv');
      });
      const exportAuditBtn = document.getElementById('export-audit');
      if (exportAuditBtn) exportAuditBtn.addEventListener('click', function () {
        const daysEl = document.getElementById('export-audit-days');
        doExport('/api/v2/export/audit' + qs({ days: daysEl ? daysEl.value : days }), 'fm-audit-export-' + (daysEl ? daysEl.value : days) + 'd.csv');
      });
      guardAssert(ctx, 'backup');
    },
  });

  /* ── 14. 页面：接口自检 ─────────────────────────────────────────────── */

  const SELFTEST_LOCAL_CHECKS = [
    ['/api/health', '后台健康', 'core'],
    ['/api/auth/me', '会话鉴权', 'core'],
    ['/api/v2/overview', 'v2 总览', 'core'],
    ['/api/v2/groups', '群列表', 'core'],
    ['/api/v2/config/toml', '配置编辑器接口', 'v3'],
    ['/api/v2/memory/tree', '记忆文件接口', 'v3'],
    ['/api/v2/models', '模型切换接口', 'v3'],
    ['/api/v2/ops/metrics', '运维指标', 'v3'],
    ['/api/v2/ops/images', '镜像清单', 'v3'],
    ['/api/v2/ops/logs?tail=5', '容器日志', 'v3'],
    ['/api/v2/ops/cron', '定时任务', 'v3'],
    ['/api/v2/backup/list', '备份列表', 'v3'],
    ['/api/v2/sessions', '会话列表', 'v3'],
    ['/api/v2/tokens', '令牌列表', 'v3'],
    ['/api/v2/selftest', '自检接口自身', 'v3'],
    ['/api/domain/health', 'Domain 健康', 'domain'],
    ['/api/domain/stats', 'Domain 统计', 'domain'],
    ['/api/domain/novel/stats', '番茄统计', 'domain'],
    ['/api/domain/contests/list', '赛文清单', 'domain'],
    ['/api/domain/library/list?limit=1', '文库列表', 'domain'],
    ['/api/domain/typing/summary', '打字汇总', 'domain'],
    ['/api/domain/reports/daily', '群日报', 'domain'],
  ];

  function normalizeSelfTestRows(payload) {
    const list = pickArray(payload, ['checks', 'items', 'results', 'rows', 'tests', 'data']);
    return list.map(function (item, index) {
      if (!item || typeof item !== 'object') return null;
      const name = textOf(item.name || item.title || item.label || item.path || ('check ' + (index + 1)));
      const ok = item.ok !== false && item.passed !== false && item.status !== 'fail' && item.success !== false;
      const httpStatus = item.http_status != null ? Number(item.http_status)
        : (item.status_code != null ? Number(item.status_code) : null);
      /* 后端 selftest 的 level：ok / warn / fail；warn 多为只读令牌的预期 403，不能标红 */
      const level = textOf(item.level || (ok ? 'ok' : (httpStatus === 403 ? 'warn' : 'fail'))) || (ok ? 'ok' : 'fail');
      const required = item.required !== false;
      const adminOnly = httpStatus === 403 && /sessions|tokens|auth/i.test(name + ' ' + textOf(item.path || ''));
      return {
        name: name,
        path: textOf(item.path || item.endpoint || item.url || ''),
        ok: ok,
        ms: item.ms != null ? item.ms : (item.elapsed_ms != null ? item.elapsed_ms : item.duration),
        error: textOf(item.error || item.reason || item.message || ''),
        group: textOf(item.group || item.kind || ''),
        level: level,
        required: required,
        httpStatus: httpStatus,
        adminOnly: adminOnly,
      };
    }).filter(Boolean);
  }

  async function runLocalSelfTest() {
    const rows = [];
    const queue = SELFTEST_LOCAL_CHECKS.slice();
    async function worker() {
      while (queue.length) {
        const item = queue.shift();
        const path = item[0];
        const started = (performance && performance.now) ? performance.now() : Date.now();
        try {
          await apiGet(path, { timeout: 12000 });
          rows.push({ name: item[1], path: path, ok: true, ms: Math.round(((performance && performance.now) ? performance.now() : Date.now()) - started), group: item[2] });
        } catch (err) {
          if (err && err.status === 401) throw err;
          const httpStatus = Number(err && err.status) || null;
          const adminOnly = httpStatus === 403 && /\/api\/v2\/(sessions|tokens)|\/api\/auth\/tokens/.test(path);
          rows.push({
            name: item[1],
            path: path,
            ok: false,
            pending: isPendingError(err),
            ms: Math.round(((performance && performance.now) ? performance.now() : Date.now()) - started),
            error: err && err.message ? err.message : String(err),
            group: item[2],
            level: adminOnly ? 'warn' : 'fail',
            httpStatus: httpStatus,
            adminOnly: adminOnly,
          });
        }
      }
    }
    const workers = [];
    for (let i = 0; i < 4; i += 1) workers.push(worker());
    await Promise.all(workers);
    rows.sort(function (a, b) { return String(a.path).localeCompare(String(b.path)); });
    return rows;
  }

  defineView({
    id: 'selftest',
    name: '接口自检',
    icon: 'shield',
    skeleton: 'default',
    async render(ctx) {
      const root = ctx.root;
      root.innerHTML = pageHead({
        title: '接口自检',
        icon: 'shield',
        sub: '逐项探测后台 v2 / 新增 v3 / Domain 接口，绿=通过、黄=只读预期/未就绪、红=真实失败；「全部重测」重新跑一遍',
        actions: [
          '<button class="btn btn-soft btn-sm" id="selftest-run" type="button">' + icon('play') + '全部重测</button>',
          '<button class="btn btn-ghost btn-sm" id="selftest-copy" type="button">' + icon('copy') + '复制结果</button>',
          '<a class="btn btn-ghost btn-sm" href="#/browse">' + icon('database') + '接口调试器</a>',
        ],
      }) +
      '<div id="selftest-progress" class="run-progress" hidden>' +
        '<div class="rp-head"><span class="rp-title mono" id="selftest-progress-label">' + icon('loader', 'spin') + ' 正在运行接口自检…</span>' +
        '<span class="rp-time mono" id="selftest-elapsed">0s</span></div>' +
        '<div class="progress-track"><div class="progress-fill indeterminate"></div></div>' +
        '<div class="rp-hint muted-2 text-xs">接口自检会依次探测约 92 个接口，通常 <b>2–10 秒</b>（首次冷启动略慢）；期间不用反复刷新。</div>' +
      '</div>' +
      '<div id="selftest-source"></div>' +
      '<div id="selftest-summary"></div>' +
      '<div class="filters mt-12">' +
        '<label class="check"><input type="checkbox" id="selftest-only-bad">只看未通过 / 未就绪</label>' +
        '<div class="field grow"><input class="input input-sm" id="selftest-q" placeholder="过滤检查项 / 路径…"></div>' +
        '<span class="muted text-xs" id="selftest-count"></span>' +
      '</div>' +
      '<div class="card mt-12"><div class="card-body" id="selftest-list"></div></div>' +
      '<div class="grid grid-4 mt-16" id="selftest-groups"></div>' +
      '<div class="notice mt-16">' + icon('info') + '<div>说明：<b>绿色=通过</b>，<b>黄色=只读账号的预期权限（403，需管理员）或接口未就绪</b>，<b>红色=真实失败</b>。后端 <span class="mono">/api/v2/selftest</span> 返回矩阵时以后端结果为准，未就绪时自动回退前端逐项探测。</div></div>';

      const progressRoot = document.getElementById('selftest-progress');
      const progressLabel = document.getElementById('selftest-progress-label');
      const elapsedNode = document.getElementById('selftest-elapsed');
      const listRoot = document.getElementById('selftest-list');
      const sourceRoot = document.getElementById('selftest-source');
      if (listRoot) listRoot.innerHTML = '<div class="sk sk-line w60"></div><div class="sk sk-line"></div><div class="sk sk-line"></div><div class="sk sk-line"></div>';
      let progressTimer = 0;
      let progressStartedAt = 0;
      function startProgress(label) {
        if (!progressRoot) return;
        progressRoot.hidden = false;
        progressStartedAt = Date.now();
        if (progressLabel && label) progressLabel.innerHTML = icon('loader', 'spin') + ' ' + esc(label);
        if (progressTimer) clearInterval(progressTimer);
        progressTimer = setInterval(function () {
          if (elapsedNode) elapsedNode.textContent = Math.round((Date.now() - progressStartedAt) / 1000) + 's';
        }, 500);
      }
      function stopProgress() {
        if (progressRoot) progressRoot.hidden = true;
        if (progressTimer) clearInterval(progressTimer);
        progressTimer = 0;
        if (elapsedNode) elapsedNode.textContent = '0s';
      }
      ctx.cleanup(stopProgress);

      startProgress('正在运行后端自检（约 2–10 秒，首次冷启动略慢）…');
      const serverRes = await tryApi(['/api/v2/selftest'], { timeout: 90000 });
      stopProgress();
      if (!ctx.alive()) return;
      let viaServer = false;
      let rows = [];
      let serverError = null;
      let serverSummary = null;
      if (serverRes.ok) {
        rows = normalizeSelfTestRows(serverRes.data || {});
        serverSummary = serverRes.data && serverRes.data.summary ? serverRes.data.summary : null;
        viaServer = rows.length > 0;
        if (!rows.length) serverError = '接口返回了空结果';
      } else {
        serverError = serverRes.error;
      }
      const serverPending = !viaServer && serverRes.pending;
      if (sourceRoot) {
        if (serverPending) {
          sourceRoot.innerHTML = notice('warn', '后端 <span class="mono">/api/v2/selftest</span> 尚未就绪（' + esc(serverError && serverError.message ? serverError.message : 'HTTP 404') + '），当前已自动切换为<b>前端逐项探测</b>：同样逐项请求真实接口并显示耗时与原因。');
        } else if (viaServer) {
          let summaryText = '';
          if (serverSummary) {
            summaryText = ' · 后端摘要：通过 ' + fmtNum(serverSummary.passed) + ' / ' + fmtNum(serverSummary.total);
            if (serverSummary.failed_optional) summaryText += '，需管理员 ' + fmtNum(serverSummary.failed_optional);
            summaryText += '，失败 ' + fmtNum(serverSummary.failed_required || 0);
            if (serverSummary.wall_ms) summaryText += ' · 全量 ' + (Number(serverSummary.wall_ms) / 1000).toFixed(1) + 's';
          }
          sourceRoot.innerHTML = notice('ok', '数据源：后端 <span class="mono">/api/v2/selftest</span> 自检矩阵（' + rows.length + ' 项）' + summaryText);
        } else {
          sourceRoot.innerHTML = notice('bad', '自检接口失败：' + esc(serverError && serverError.message ? serverError.message : '未知错误') + '，已回退前端探测。');
        }
      }
      const summaryRoot = document.getElementById('selftest-summary');
      const countNode = document.getElementById('selftest-count');
      const badCheck = document.getElementById('selftest-only-bad');
      const filterInput = document.getElementById('selftest-q');
      let currentRows = rows;
      let running = false;

      function paint() {
        const kw = (filterInput ? filterInput.value : '').trim().toLowerCase();
        const onlyBad = badCheck ? badCheck.checked : false;
        const visible = currentRows.filter(function (row) {
          if (onlyBad && row.ok) return false;
          if (!kw) return true;
          return (row.name + ' ' + row.path + ' ' + (row.error || '')).toLowerCase().indexOf(kw) >= 0;
        });
        const pass = currentRows.filter(function (row) { return row.ok; }).length;
        const needAdmin = currentRows.filter(function (row) {
          return !row.ok && (row.adminOnly || row.httpStatus === 403 || row.level === 'warn');
        }).length;
        const pending = currentRows.filter(function (row) {
          return !row.ok && !row.adminOnly && row.httpStatus !== 403 && row.level !== 'warn' && row.pending;
        }).length;
        const fail = currentRows.length - pass - needAdmin - pending;
        const slowest = currentRows.slice().sort(function (a, b) { return (Number(b.ms) || 0) - (Number(a.ms) || 0); })[0];
        if (summaryRoot) {
          summaryRoot.innerHTML = kpiGrid([
            { label: '检查项', icon: 'shield', valueHtml: fmtNum(currentRows.length), hint: viaServer ? '后端自检矩阵' : '前端逐项探测' },
            { label: '通过', icon: 'check-circle', valueHtml: fmtNum(pass), tone: 'ok', hint: pass === currentRows.length && currentRows.length ? '全部通过' : '' },
            { label: '需管理员', icon: 'lock', valueHtml: fmtNum(needAdmin), tone: needAdmin ? 'warn' : '', hint: '只读账号的预期 403（sessions / tokens）' },
            { label: '未就绪', icon: 'clock', valueHtml: fmtNum(pending), tone: pending ? 'warn' : '', hint: '404 / 501 / 超时' },
            { label: '失败', icon: 'alert', valueHtml: fmtNum(fail), tone: fail ? 'bad' : 'ok', hint: fail ? '红色仅代表真实失败' : (slowest ? '最慢 ' + esc(slowest.name) + ' ' + fmtNum(slowest.ms) + ' ms' : '无真实失败') },
          ], { cols: 5 });
        }
        if (countNode) countNode.textContent = visible.length + ' / ' + currentRows.length + ' 项';
        if (!visible.length) {
          listRoot.innerHTML = emptyState({ icon: 'shield', title: currentRows.length ? '没有匹配的检查项' : '还没有自检结果', hint: currentRows.length ? '调整过滤条件' : '点「全部重测」开始' });
          return;
        }
        listRoot.innerHTML = visible.map(function (row) {
          const adminOnly = !row.ok && (row.adminOnly || row.httpStatus === 403 || row.level === 'warn');
          const pendingRow = !row.ok && !adminOnly && row.pending;
          const tone = row.ok ? 'ok' : adminOnly ? 'warn' : pendingRow ? 'warn' : 'bad';
          const statusText = row.ok ? '通过' : adminOnly ? '需管理员' : pendingRow ? '未就绪' : '失败';
          const detail = !row.ok && !adminOnly && row.error ? '<div class="v3-check-error mono">' + esc(row.error) + '</div>'
            : (adminOnly ? '<div class="v3-check-hint text-xs">只读账号无权访问管理员接口（HTTP 403），属预期；使用管理员令牌即为绿色。</div>' : '');
          return '<div class="v3-check-list-item">' +
            '<div class="v3-check-row">' +
              '<span class="dot" data-tone="' + tone + '"></span>' +
              '<span class="v3-check-name" title="' + escAttr(row.name) + '">' + esc(row.name) + '</span>' +
              '<span class="v3-check-path mono" title="' + escAttr(row.path) + '">' + esc(row.path) + '</span>' +
              '<span class="v3-check-flex"></span>' +
              (row.httpStatus && !row.ok ? '<span class="muted-2 text-xs mono">HTTP ' + esc(row.httpStatus) + '</span>' : '') +
              (row.ms != null ? '<span class="muted-2 text-xs mono">' + fmtNum(row.ms) + ' ms</span>' : '') +
              '<span class="tag tag-' + tone + '">' + statusText + '</span>' +
            '</div>' + detail +
          '</div>';
        }).join('');
      }

      async function runAll() {
        if (running) return;
        running = true;
        const btn = document.getElementById('selftest-run');
        if (btn) { btn.disabled = true; btn.innerHTML = icon('clock') + '检测中…'; }
        listRoot.innerHTML = '<div class="sk sk-line w60"></div><div class="sk sk-line"></div><div class="sk sk-line"></div><div class="sk sk-line"></div>';
        startProgress(viaServer ? '正在重新运行后端自检（约 2–10 秒，首次冷启动略慢）…' : '正在逐项探测接口…');
        try {
          if (viaServer) {
            const res = await tryApi(['/api/v2/selftest'], { timeout: 90000 });
            if (res.ok) {
              currentRows = normalizeSelfTestRows(res.data || {});
              if (sourceRoot) sourceRoot.innerHTML = notice('ok', '数据源：后端 <span class="mono">/api/v2/selftest</span> 自检矩阵（' + currentRows.length + ' 项）');
            } else currentRows = await runLocalSelfTest();
          } else {
            currentRows = await runLocalSelfTest();
          }
          stopProgress();
          if (!ctx.alive()) return;
          paint();
          const pass = currentRows.filter(function (row) { return row.ok; }).length;
          toast('自检完成：' + pass + ' / ' + currentRows.length + ' 通过', pass === currentRows.length ? 'ok' : 'warn');
        } catch (err) {
          stopProgress();
          if (!ctx.alive()) return;
          listRoot.innerHTML = errorState(err, 'selftest-retry');
          const retry = document.getElementById('selftest-retry');
          if (retry) retry.addEventListener('click', runAll);
        } finally {
          running = false;
          const runBtn = document.getElementById('selftest-run');
          if (runBtn) { runBtn.disabled = false; runBtn.innerHTML = icon('play') + '全部重测'; }
        }
      }

      if (viaServer && currentRows.length) paint();
      else setTimeout(runAll, 30);

      const runBtn = document.getElementById('selftest-run');
      if (runBtn) runBtn.addEventListener('click', runAll);
      const copyBtn = document.getElementById('selftest-copy');
      if (copyBtn) copyBtn.addEventListener('click', function () {
        const text = currentRows.map(function (row) {
          return (row.ok ? '[OK] ' : row.pending ? '[PENDING] ' : '[FAIL] ') + row.name + ' ' + row.path + ' ' + (row.ms != null ? row.ms + 'ms' : '') + (row.error ? ' :: ' + row.error : '');
        }).join('\n');
        copyText(text).then(function () { toast('自检结果已复制', 'ok'); }).catch(function () { toast('复制失败', 'bad'); });
      });
      if (badCheck) badCheck.addEventListener('change', paint);
      if (filterInput) filterInput.addEventListener('input', debounce(paint, 200));
      if (!viaServer && currentRows.length === 0) {
        listRoot.innerHTML = '<div class="muted-2 text-xs">正在检测接口…</div>';
      }
      guardAssert(ctx, 'selftest');
    },
  });

  /* ── 15. 页面：会话与令牌 ───────────────────────────────────────────── */

  function normalizeSessionRows(payload) {
    const list = pickArray(payload, ['sessions', 'items', 'rows', 'list']);
    return list.filter(function (item) { return item && typeof item === 'object'; }).map(function (item) {
      return {
        id: textOf(item.id || item.sid || item.session_id),
        username: textOf(item.username || item.user || item.name),
        created_at: item.created_at || item.created || item.start_time,
        last_seen: item.last_seen || item.last_active || item.updated_at || item.last_seen_at,
        ip: textOf(item.ip || item.remote_addr || item.client_ip || ''),
        ua: textOf(item.user_agent || item.ua || ''),
        current: item.current === true,
        raw: item,
      };
    });
  }

  function normalizeTokenRows(payload) {
    const list = pickArray(payload, ['tokens', 'items', 'rows', 'list']);
    return list.filter(function (item) { return item && typeof item === 'object'; }).map(function (item) {
      return {
        id: textOf(item.id || item.token_id || ''),
        name: textOf(item.name || item.label || item.username || ''),
        prefix: textOf(item.prefix || item.value || item.token || item.masked || ''),
        created_at: item.created_at || item.created,
        last_used: item.last_used || item.last_used_at || item.used_at,
        expires_at: item.expires_at || item.expire_at,
        enabled: item.enabled !== false && item.revoked !== true,
        raw: item,
      };
    });
  }

  defineView({
    id: 'sessions',
    name: '会话与令牌',
    icon: 'key',
    skeleton: 'default',
    async render(ctx) {
      const [sessionsRes, tokensRes, meRes] = await Promise.all([
        tryApi(['/api/v2/sessions'], { timeout: 20000 }),
        tryApi(['/api/v2/tokens'], { timeout: 20000 }),
        apiGet('/api/auth/me').catch(function () { return null; }),
      ]);
      if (!ctx.alive()) return;
      const me = meRes || APP.user || {};
      let sessions = sessionsRes.ok ? normalizeSessionRows(sessionsRes.data || {}) : [];
      let tokens = tokensRes.ok ? normalizeTokenRows(tokensRes.data || {}) : [];
      let tokensFallback = false;
      let sessionsFallback = false;
      if (!tokensRes.ok) {
        const oldTokens = await apiGet('/api/auth/tokens').catch(function (err) { return { __error: err }; });
        if (oldTokens && !oldTokens.__error) {
          tokens = normalizeTokenRows({ items: oldTokens.items || [] });
          tokensFallback = true;
        }
      }
      if (!sessionsRes.ok) {
        sessionsFallback = true;
        sessions = [{
          id: 'current-session',
          username: textOf(me.username || '当前会话'),
          created_at: me.created_at || me.login_at || null,
          last_seen: Date.now(),
          ip: textOf(me.ip || ''),
          ua: typeof navigator !== 'undefined' ? navigator.userAgent : '',
          current: true,
        }];
      }
      if (!ctx.alive()) return;
      const sessionsForbidden = !sessionsRes.ok && sessionsRes.error && sessionsRes.error.status === 403;
      const tokensForbidden = !tokensRes.ok && tokensRes.error && tokensRes.error.status === 403;

      ctx.root.innerHTML = pageHead({
        title: '会话与令牌',
        icon: 'key',
        sub: '服务端登录会话（/api/v2/sessions）与 API 令牌（/api/v2/tokens，脱敏）· 支持吊销',
        actions: [
          '<button class="btn btn-ghost btn-sm" data-retry="1" type="button">' + icon('refresh') + '刷新</button>',
          '<a class="btn btn-ghost btn-sm" href="#/settings">' + icon('gear') + '账号设置（旧令牌入口）</a>',
        ],
      }) +
      (sessionsForbidden || tokensForbidden
        ? notice('warn', '当前登录账号是<b>只读账号</b>（HTTP 403）：v2 会话 / 令牌接口要求管理员权限，页面只展示可用部分。用 <span class="mono">admin</span> 密码登录后可查看完整列表并进行吊销。')
        : '') +
      (sessionsFallback && !sessionsForbidden ? notice('warn', '会话列表接口 <span class="mono">/api/v2/sessions</span> 未就绪，当前只显示本机会话信息。') : '') +
      (tokensFallback && !tokensForbidden ? notice('warn', '令牌接口 <span class="mono">/api/v2/tokens</span> 未就绪，已降级读取 <span class="mono">/api/auth/tokens</span>（需要管理员权限；当前返回可能受限）。') : '') +
      kpiGrid([
        { label: '登录会话', icon: 'user', valueHtml: fmtNum(sessions.length), tone: 'acc', hint: sessionsFallback ? '接口未就绪，仅本机' : '来自服务端内存会话表' },
        { label: 'API 令牌', icon: 'key', valueHtml: fmtNum(tokens.length), hint: tokens.filter(function (row) { return row.enabled; }).length + ' 个有效' },
        { label: '当前账号', icon: 'shield', valueHtml: '<span style="font-size:15px">' + esc(me.username || 'admin') + '</span>', hint: me.is_admin ? '管理员' : '只读令牌账号' },
        { label: '数据源', icon: 'link', valueHtml: '<span class="tag ' + (sessionsRes.ok && tokensRes.ok ? 'tag-ok' : 'tag-warn') + '">' + (sessionsRes.ok || tokensRes.ok ? 'v2 接口' : '降级') + '</span>' },
      ]) +
      '<div class="grid grid-2 mt-16">' +
        card({ title: '登录会话', icon: 'user', flush: true },
          '<div id="sessions-table"></div>' +
          (sessionsRes.ok ? '' : '<div class="card-foot"><span class="muted-2 text-xs">接口：GET /api/v2/sessions · ' + esc(sessionsRes.error ? sessionsRes.error.message : '未就绪') + '</span></div>')) +
        card({ title: 'API 令牌（脱敏）', icon: 'key', flush: true },
          '<div id="tokens-table"></div>' +
          '<div id="tokens-result" class="card-body"></div>') +
      '</div>' +
      '<div class="notice mt-16">' + icon('alert') + '<div>吊销令牌是<b>立即失效</b>的危险操作，需要输入 <span class="mono">REVOKE</span> 确认；吊销后使用该令牌的脚本会立刻收到 401。</div></div>';

      const sessionsTable = document.getElementById('sessions-table');
      if (sessionsTable) {
        v3Table(sessionsTable, {
          stateKey: 'sessions-list',
          rows: sessions,
          countLabel: function (count) { return count + ' 个会话'; },
          columns: [
            { key: 'username', label: '账号', render: function (row) { return esc(row.username || '未知') + (row.current ? ' <span class="tag tag-ok">当前</span>' : ''); } },
            { key: 'created_at', label: '建立时间', width: '150px', hideSm: true, render: function (row) { return '<span class="mono">' + esc(fmtTime(row.created_at) || '—') + '</span>'; } },
            { key: 'last_seen', label: '最近活跃', width: '150px', render: function (row) { return '<span class="mono">' + esc(row.last_seen ? (ago(row.last_seen) || fmtTime(row.last_seen)) : '—') + '</span>'; } },
            { key: 'ip', label: 'IP / 来源', width: '130px', hideSm: true, render: function (row) { return '<span class="mono">' + esc(row.ip || '—') + '</span>'; } },
            { key: 'id', label: '会话 ID', hideSm: true, render: function (row) { return '<span class="mono cell-truncate">' + esc(row.id || '—') + '</span>'; } },
          ],
          empty: { icon: 'user', title: '没有会话记录', hint: '接口可能尚未就绪' },
        });
      }

      const tokensTable = document.getElementById('tokens-table');
      if (tokensTable) {
        v3Table(tokensTable, {
          stateKey: 'tokens-list',
          rows: tokens,
          countLabel: function (count) { return count + ' 个令牌'; },
          columns: [
            { key: 'name', label: '名称', render: function (row) { return esc(row.name || '未命名'); } },
            { key: 'prefix', label: '令牌（脱敏）', mono: true, hideSm: true, render: function (row) { return '<span class="mono">' + esc(row.prefix || '—') + '</span>'; } },
            { key: 'created_at', label: '创建', width: '140px', hideSm: true, render: function (row) { return '<span class="mono">' + esc(fmtTime(row.created_at) || '—') + '</span>'; } },
            { key: 'last_used', label: '最近使用', width: '140px', hideSm: true, render: function (row) { return row.last_used ? '<span class="mono">' + esc(ago(row.last_used) || fmtTime(row.last_used)) + '</span>' : '<span class="muted-2">从未</span>'; } },
            { key: 'enabled', label: '状态', width: '86px', render: function (row) { return row.enabled ? '<span class="tag tag-ok">有效</span>' : '<span class="tag tag-muted">已吊销</span>'; } },
            { key: 'action', label: '', width: '84px', render: function (row) {
              if (!row.enabled) return '<span class="muted-2 text-xs">—</span>';
              return '<button class="btn btn-sm btn-danger" data-revoke-token="' + escAttr(row.id) + '" type="button">吊销</button>';
            } },
          ],
          empty: { icon: 'key', title: '还没有令牌', hint: tokensFallback ? '旧接口需要管理员权限，当前账号可能看不到' : '令牌可在设置页创建' },
        });
        tokensTable.addEventListener('click', async function (event) {
          const button = event.target.closest('[data-revoke-token]');
          if (!button) return;
          const id = button.getAttribute('data-revoke-token');
          const token = tokens.find(function (row) { return row.id === id; }) || {};
          const resultRoot = document.getElementById('tokens-result');
          const ok = await confirmAction({
            title: '吊销令牌？',
            detail: '令牌「' + (token.name || id) + '」将立即失效，使用它的脚本会收到 401。请输入 REVOKE 确认。',
            confirmLabel: '吊销令牌',
            danger: true,
            requireText: 'REVOKE',
          });
          if (!ok) return;
          try {
            await apiPost('/api/v2/tokens/revoke', { id: id }, { timeout: 20000 });
            if (resultRoot) resultRoot.innerHTML = notice('ok', '令牌已吊销：<span class="mono">' + esc(token.name || id) + '</span>');
            toast('令牌已吊销', 'ok');
            setTimeout(function () { if (ctx.alive()) route(); }, 700);
          } catch (err) {
            if (isPendingError(err)) {
              const legacy = await apiDelete('/api/auth/tokens/' + encodeURIComponent(id)).catch(function (legacyErr) { return { __error: legacyErr }; });
              if (legacy && !legacy.__error) {
                if (resultRoot) resultRoot.innerHTML = notice('ok', 'v2 吊销接口未就绪，已用旧接口 <span class="mono">DELETE /api/auth/tokens/:id</span> 吊销成功。');
                toast('令牌已吊销（旧接口）', 'ok');
                setTimeout(function () { if (ctx.alive()) route(); }, 700);
                return;
              }
              if (resultRoot) resultRoot.innerHTML = pendingState({ title: '吊销接口尚未就绪', hint: 'POST /api/v2/tokens/revoke 与旧接口都不可用；请到设置页管理令牌。', path: 'POST /api/v2/tokens/revoke' });
            } else {
              if (resultRoot) resultRoot.innerHTML = errorState(err) + '<div class="muted-2 text-xs mt-8">403 表示当前账号只读，需要管理员账号才能吊销令牌。</div>';
            }
            toast('吊销失败：' + err.message, 'bad');
          }
        });
      }
      guardAssert(ctx, 'sessions');
    },
  });

  /* ── 16. 页面：番茄阅读 ─────────────────────────────────────────────── */

  function chartOptionBars(labels, values, name) {
    return chartBase({
      tooltip: { trigger: 'axis', axisPointer: { type: 'shadow' } },
      grid: { left: 8, right: 16, top: 16, bottom: 4, containLabel: true },
      xAxis: { type: 'value' },
      yAxis: { type: 'category', data: labels, inverse: true, axisLabel: { color: '#66748a', fontSize: 10.5, width: 150, overflow: 'truncate' } },
      series: [{ name: name || '数量', type: 'bar', data: values, barMaxWidth: 14, itemStyle: { borderRadius: [0, 4, 4, 0], color: '#5b8cff' } }],
    });
  }

  function chartOptionPie(items) {
    const data = (items || []).map(function (item) { return { name: item.label, value: Number(item.value) || 0 }; });
    return chartBase({
      tooltip: { trigger: 'item', formatter: '{b}: {c} ({d}%)' },
      legend: { orient: 'vertical', right: 4, top: 'middle', textStyle: { color: '#8b98ac', fontSize: 11 } },
      series: [{
        type: 'pie', radius: ['44%', '68%'], center: ['38%', '52%'],
        avoidLabelOverlap: true, itemStyle: { borderColor: 'transparent', borderWidth: 2, borderRadius: 4 },
        label: { show: false }, emphasis: { label: { show: true, fontSize: 12, color: '#e9eef7' } },
        data: data,
      }],
    });
  }

  defineView({
    id: 'novel',
    name: '番茄阅读',
    icon: 'book',
    skeleton: 'default',
    async render(ctx) {
      const statsRes = await tryDomain(['novel/stats'], { timeout: 25000 });
      if (!ctx.alive()) return;
      if (!statsRes.ok) {
        const legacy = await tryApi(['/api/v2/novels'], { timeout: 20000 });
        if (!ctx.alive()) return;
        if (legacy.ok && legacy.data && legacy.data.ok !== false) {
          const books = Number(legacy.data.books || 0);
          const items = pickArray(legacy.data, ['items', 'books_list', 'list']);
          ctx.root.innerHTML = pageHead({ title: '番茄阅读', icon: 'book', sub: 'domain /novel/stats 未就绪，已降级使用 /api/v2/novels 汇总' }) +
            fallbackNotice('/api/domain/novel/stats', '/api/v2/novels') +
            kpiGrid([
              { label: '书籍数', icon: 'book', valueHtml: fmtNum(books), tone: 'acc' },
              { label: '章节数', icon: 'list', valueHtml: fmtNum(legacy.data.chapters) },
              { label: '阅读进度', icon: 'chart', valueHtml: fmtNum(legacy.data.progress) },
              { label: '数据源', icon: 'link', valueHtml: '<span class="tag tag-warn">v2 降级</span>' },
            ]) +
            '<div class="card mt-16"><div class="card-body">' +
              (items.length ? '<div class="v3-list">' + items.map(function (item) {
                return '<div class="v3-list-item">' + icon('book') + '<div class="v3-list-main"><div class="v3-list-title">' + esc(textOf(item.title || item.book_id)) + '</div>' +
                  '<div class="v3-list-sub">' + esc(textOf(item.author || '')) + '</div></div></div>';
              }).join('') + '</div>' : emptyState({ icon: 'book', title: '没有书籍明细', hint: 'v2 接口只返回汇总计数' })) +
            '</div></div>';
          guardAssert(ctx, 'novel');
          return;
        }
        renderApiFailure(ctx, statsRes, {
          title: '番茄阅读',
          icon: 'book',
          paths: ['GET /api/domain/novel/stats', 'GET /api/domain/novel/book?book_id='],
          hint: 'Domain 新增的番茄书架统计接口尚未就绪（task-3 由 domain 线补齐）；就绪后自动展示书目、字数图表与每章明细。',
        });
        return;
      }
      const data = statsRes.data || {};
      const items = pickArray(data, ['items', 'books_list', 'list', 'books']);
      const booksCount = numOrNull(data.books) != null ? numOrNull(data.books) : items.length;
      const chapters = numOrNull(data.chapters);
      const progress = numOrNull(data.progress);
      const topItems = items.slice().sort(function (a, b) {
        return (Number(b.word_count) || 0) - (Number(a.word_count) || 0);
      }).slice(0, 12);

      ctx.root.innerHTML = pageHead({
        title: '番茄阅读',
        icon: 'book',
        sub: 'Domain 番茄书架统计 · 书目 / 章节 / 阅读进度 / 字数图表，点行看每章明细',
        actions: [
          '<button class="btn btn-ghost btn-sm" data-retry="1" type="button">' + icon('refresh') + '刷新</button>',
          '<a class="btn btn-ghost btn-sm" href="#/business/novel">' + icon('book') + '旧业务数据页</a>',
        ],
      }) +
      kpiGrid([
        { label: '书籍数', icon: 'book', valueHtml: fmtNum(booksCount), tone: 'acc', hint: '来自 novel_books' },
        { label: '章节数', icon: 'list', valueHtml: fmtNum(chapters), hint: '来自 novel_chapters' },
        { label: '阅读进度记录', icon: 'trending', valueHtml: fmtNum(progress), hint: '来自 novel_progress' },
        { label: 'Top 书目总字数', icon: 'chart', valueHtml: fmtNum(topItems.reduce(function (sum, row) { return sum + (Number(row.word_count) || 0); }, 0)) + '<small>字</small>', hint: '前 ' + topItems.length + ' 本合计' },
      ]) +
      '<div class="grid grid-wide mt-16">' +
        card({ title: '书目字数 Top ' + topItems.length, icon: 'chart' },
          '<div class="chart chart-lg" id="novel-chart"></div>') +
        card({ title: '接口与说明', icon: 'link' },
          '<div class="v3-jsonfallback">' + codeTip(['GET /api/domain/novel/stats', 'GET /api/domain/novel/book?book_id=']) +
          '<div class="muted-2 text-xs mt-8">点击下方表格行会请求 <span class="mono">/novel/book?book_id=</span> 展示章节与进度明细；接口若未就绪会在抽屉里显示原因。</div></div>') +
      '</div>' +
      '<div class="card mt-16"><div class="card-head"><h3>' + icon('book') + '书目<span class="section-sub">' + items.length + ' 本</span></h3></div>' +
      '<div id="novel-table"></div></div>';

      const chartEl = document.getElementById('novel-chart');
      if (chartEl) {
        if (topItems.length) {
          mountChart(chartEl, chartOptionBars(topItems.map(function (row) { return textOf(row.title || row.book_id); }),
            topItems.map(function (row) { return Number(row.word_count) || 0; }), '字数'), topItems.map(function (row) {
            return { label: textOf(row.title || row.book_id), value: Number(row.word_count) || 0 };
          }));
        } else {
          chartNoData(chartEl, '接口没有返回书目明细（items）');
        }
      }

      const tableRoot = document.getElementById('novel-table');
      if (tableRoot) {
        v3Table(tableRoot, {
          stateKey: 'novel-books',
          rows: items,
          countLabel: function (count) { return count + ' 本书'; },
          columns: [
            { key: 'title', label: '书名', render: function (row) { return '<span class="cell-truncate">' + esc(textOf(row.title || row.book_id)) + '</span>'; } },
            { key: 'author', label: '作者', width: '130px', hideSm: true, render: function (row) { return esc(textOf(row.author || '—')); } },
            { key: 'total_chapters', label: '章节', width: '86px', num: true, render: function (row) { return fmtNum(row.total_chapters != null ? row.total_chapters : row.chapters); } },
            { key: 'word_count', label: '字数', width: '100px', num: true, render: function (row) { return '<span class="mono">' + fmtNum(row.word_count) + '</span>'; } },
            { key: 'book_id', label: 'book_id', hideSm: true, render: function (row) { return '<span class="mono cell-truncate">' + esc(textOf(row.book_id)) + '</span>'; } },
          ],
          empty: { icon: 'book', title: '没有书目数据', hint: '接口返回了空 items；可到旧业务数据页查看汇总' },
        });
        tableRoot.addEventListener('click', async function (event) {
          const tr = event.target.closest('tr.dt-row');
          if (!tr) return;
          const row = items[Number(tr.getAttribute('data-row'))];
          if (!row) return;
          const bookId = textOf(row.book_id || row.id || row.title);
          Drawer.open({
            title: '番茄书目 · ' + esc(textOf(row.title || bookId)),
            sub: esc(textOf(row.author || '')) + ' · ' + esc(bookId),
            body: '<div class="muted-2 text-xs">正在读取 /api/domain/novel/book?book_id=' + esc(bookId) + ' …</div>',
            wide: true,
            raw: row,
          });
          const bookRes = await tryDomain(['novel/book' + qs({ book_id: bookId })], { timeout: 20000 });
          if (!Drawer.isOpen()) return;
          const bodyRoot = document.getElementById('drawer-body');
          if (!bodyRoot) return;
          if (!bookRes.ok) {
            bodyRoot.innerHTML = (bookRes.pending
              ? pendingState({ title: '书目详情接口尚未就绪', hint: 'GET /api/domain/novel/book?book_id=' + bookId, path: 'GET /api/domain/novel/book' })
              : errorState(bookRes.error));
            return;
          }
          const bookData = bookRes.data || {};
          const book = bookData.book || {};
          const chaptersList = pickArray(bookData, ['chapters', 'items', 'list']);
          const progressList = pickArray(bookData, ['progress', 'progresses', 'readers', 'list_progress']);
          bodyRoot.innerHTML =
            '<div class="kv-grid">' +
              '<div class="kv-item"><div class="k">书名</div><div class="v">' + esc(textOf(book.title || row.title || bookId)) + '</div></div>' +
              '<div class="kv-item"><div class="k">作者</div><div class="v">' + esc(textOf(book.author || row.author || '—')) + '</div></div>' +
              '<div class="kv-item"><div class="k">章节数</div><div class="v mono">' + fmtNum(book.total_chapters != null ? book.total_chapters : chaptersList.length) + '</div></div>' +
              '<div class="kv-item"><div class="k">字数</div><div class="v mono">' + fmtNum(book.word_count != null ? book.word_count : row.word_count) + '</div></div>' +
            '</div>' +
            '<div class="section-title mt-16">' + icon('list') + '章节' + '<span class="section-sub">' + chaptersList.length + ' 条</span></div>' +
            '<div id="novel-chapters" class="mt-8"></div>' +
            '<div class="section-title mt-16">' + icon('trending') + '阅读进度' + '<span class="section-sub">' + progressList.length + ' 条</span></div>' +
            '<div id="novel-progress" class="mt-8"></div>';
          const chapterRoot = document.getElementById('novel-chapters');
          if (chapterRoot) {
            renderTable(chapterRoot, {
              columns: [
                { key: 'chapter_index', label: '章', width: '60px', num: true, render: function (item) { return fmtNum(item.chapter_index != null ? item.chapter_index : item.index); } },
                { key: 'title', label: '标题', render: function (item) { return '<span class="cell-truncate">' + esc(textOf(item.title || '—')) + '</span>'; } },
                { key: 'word_count', label: '字数', width: '90px', num: true, render: function (item) { return fmtNum(item.word_count); } },
              ],
              rows: chaptersList,
              empty: { icon: 'list', title: '没有章节数据', hint: '接口返回的 chapters 为空' },
            });
          }
          const progressRoot = document.getElementById('novel-progress');
          if (progressRoot) {
            renderTable(progressRoot, {
              columns: [
                { key: 'user', label: '用户', render: function (item) { return esc(textOf(item.user || item.username || item.user_id || '—')); } },
                { key: 'chapter', label: '读到', width: '120px', render: function (item) { return '<span class="mono">' + esc(textOf(item.chapter_title || item.chapter_index || item.chapter || '—')) + '</span>'; } },
                { key: 'updated_at', label: '更新时间', width: '160px', render: function (item) { return '<span class="mono">' + esc(fmtTime(item.updated_at || item.updated || item.last_read_at) || '—') + '</span>'; } },
              ],
              rows: progressList,
              empty: { icon: 'members', title: '没有阅读进度', hint: '接口返回的 progress 为空' },
            });
          }
          Drawer.raw = bookData;
        });
      }
      guardAssert(ctx, 'novel');
    },
  });

  /* ── 17. 页面：赛文图表 ─────────────────────────────────────────────── */

  function normalizeContestTypes(payload) {
    const data = payload || {};
    const source = data.contests || data.labels || data.items || data.types || data.contest_labels || data;
    const out = [];
    if (Array.isArray(source)) {
      source.forEach(function (item) {
        if (typeof item === 'string') { out.push({ type: item, label: item, count: null, users: null, scope: '' }); return; }
        if (!item || typeof item !== 'object') return;
        out.push({
          type: textOf(item.key || item.type || item.id || item.contest),
          label: textOf(item.name || item.label || item.title || item.key || item.type || item.id),
          count: numOrNull(firstDefined(item, ['scores', 'count', 'total', 'texts', 'n'])),
          users: numOrNull(firstDefined(item, ['users', 'user_count', 'participants'])),
          scope: textOf(item.scope || ''),
          groups: item.groups,
        });
      });
    } else if (source && typeof source === 'object') {
      Object.keys(source).forEach(function (key) {
        const value = source[key];
        if (value && typeof value === 'object') {
          out.push({ type: key, label: textOf(value.name || value.label || key), count: numOrNull(firstDefined(value, ['scores', 'count', 'total'])), users: numOrNull(value.users), scope: textOf(value.scope || '') });
        } else if (typeof value === 'number') {
          out.push({ type: key, label: key, count: value, users: null, scope: '' });
        }
      });
    }
    return out.filter(function (item) { return item.type || item.label; });
  }

  function normalizeChartPoints(payload) {
    const rows = pickArray(payload, ['points', 'series', 'rows', 'data', 'trend', 'items']);
    return rows.map(function (row) {
      if (!row || typeof row !== 'object') return null;
      const label = firstDefined(row, ['date', 'day', 'label', 'ts', 'time', 'bucket', 'x']);
      const speed = numOrNull(firstDefined(row, ['avg_speed', 'speed', 'avg', 'cpm', 'avg_cpm', 'average_speed']));
      const best = numOrNull(firstDefined(row, ['best_speed', 'max_speed', 'top_speed']));
      const accuracy = numOrNull(firstDefined(row, ['avg_accuracy', 'accuracy', 'key_accuracy', 'correct_rate', 'keyacc', 'acc']));
      const count = numOrNull(firstDefined(row, ['count', 'total', 'scores', 'records', 'n', 'participants', 'users']));
      return {
        label: label == null ? '' : String(label),
        speed: speed,
        best: best,
        accuracy: accuracy,
        count: count,
        raw: row,
      };
    }).filter(function (row) { return row && (row.speed != null || row.accuracy != null || row.count != null || row.best != null); });
  }

  function normalizeContestRanking(payload) {
    const rows = pickArray(payload, ['rows', 'ranking', 'items', 'leaderboard']);
    return rows.filter(function (row) { return row && typeof row === 'object'; }).map(function (row) {
      return {
        rank: numOrNull(row.rank),
        user_name: textOf(firstDefined(row, ['user_name', 'user', 'username', 'name', 'sender_name'])),
        best_speed: numOrNull(firstDefined(row, ['best_speed', 'speed', 'max_speed'])),
        avg_accuracy: numOrNull(firstDefined(row, ['avg_accuracy', 'accuracy', 'key_accuracy'])),
        records: numOrNull(firstDefined(row, ['records', 'count', 'scores'])),
        raw: row,
      };
    });
  }

  defineView({
    id: 'contests',
    name: '赛文图表',
    icon: 'chart',
    skeleton: 'default',
    async render(ctx) {
      const days = clamp(Number(ctx.q('days', 30)) || 30, 1, 365);
      const typeRes = await tryDomain(['contests/list'], { timeout: 20000 });
      if (!ctx.alive()) return;
      let legacyFallback = false;
      let types = typeRes.ok ? normalizeContestTypes(typeRes.data || {}) : [];
      let legacyItems = [];
      if (!typeRes.ok) {
        const legacy = await tryDomain(['contests'], { timeout: 20000 });
        if (!ctx.alive()) return;
        if (legacy.ok) {
          legacyFallback = true;
          legacyItems = pickArray(legacy.data, ['items', 'contests', 'rows', 'list']);
        } else {
          renderApiFailure(ctx, typeRes, {
            title: '赛文图表',
            icon: 'chart',
            paths: ['GET /api/domain/contests/list', 'GET /api/domain/contest/chart-data'],
            hint: 'Domain 赛文清单与图表数据接口尚未就绪；赛文旧接口（/contest/search）当前也不可用。',
          });
          return;
        }
      }
      const globalType = types.find(function (item) { return item.scope === 'global'; });
      const contestType = ctx.q('contest') || (globalType && globalType.type) || (types[0] && types[0].type) || '';
      const chartPath = 'contest/chart-data' + qs({ contest: contestType, days: days });
      const chartRes = contestType ? await tryDomain([chartPath], { timeout: 25000 }) : { ok: false, pending: true, error: new ApiError('没有可用的赛文类型', { status: 404 }) };
      if (!ctx.alive()) return;
      const chartData = chartRes.ok ? (chartRes.data || {}) : {};
      const points = chartRes.ok ? normalizeChartPoints(chartData) : [];
      const ranking = chartRes.ok ? normalizeContestRanking(chartData) : [];
      const pieItems = types.filter(function (item) { return item.count != null; }).map(function (item) {
        return { label: item.label || item.type, value: item.count };
      });
      const totalTexts = types.reduce(function (sum, item) { return sum + (Number(item.count) || 0); }, 0);
      const totalUsers = types.reduce(function (sum, item) { return sum + (Number(item.users) || 0); }, 0);

      ctx.root.innerHTML = pageHead({
        title: '赛文图表',
        icon: 'chart',
        sub: 'Domain 赛文类型 / 成绩曲线（均速、键准、条数）· 数据源 /contests/list 与 /contest/chart-data',
        actions: [
          '<button class="btn btn-ghost btn-sm" data-retry="1" type="button">' + icon('refresh') + '刷新</button>',
          '<a class="btn btn-ghost btn-sm" href="#/business/contest">' + icon('list') + '旧赛文列表</a>',
        ],
      }) +
      (legacyFallback ? fallbackNotice('/api/domain/contests/list', '/api/domain/contests') : '') +
      kpiGrid([
        { label: '赛文类型', icon: 'list', valueHtml: fmtNum(types.length), tone: 'acc', hint: types.slice(0, 3).map(function (item) { return item.label; }).join(' / ') },
        { label: '稿件总量', icon: 'book', valueHtml: fmtNum(totalTexts), hint: totalUsers ? '覆盖 ' + fmtNum(totalUsers) + ' 人次' : (legacyFallback ? '旧接口明细 ' + legacyItems.length + ' 条' : '来自 contests/list') },
        { label: '图表数据点', icon: 'chart', valueHtml: fmtNum(points.length), hint: '类型 ' + (contestType || '—') + ' · 最近 ' + days + ' 天 · 榜单 ' + ranking.length + ' 人' },
        { label: '图表接口', icon: 'link', valueHtml: '<span class="tag ' + (chartRes.ok ? 'tag-ok' : 'tag-warn') + '">' + (chartRes.ok ? '就绪' : chartRes.pending ? '未就绪' : '失败') + '</span>' },
      ]) +
      '<div class="filters mt-12">' +
        '<div class="field"><label>赛文类型</label><select class="select select-sm" id="contest-type">' +
          (types.length
            ? types.map(function (item) { return '<option value="' + escAttr(item.type || item.label) + '"' + ((item.type || item.label) === contestType ? ' selected' : '') + '>' + esc(item.label || item.type) + (item.count != null ? '（' + fmtNum(item.count) + '）' : '') + '</option>'; }).join('')
            : '<option value="">（接口未返回类型）</option>') +
        '</select></div>' +
        '<div class="field"><label>时间范围</label><select class="select select-sm" id="contest-days">' +
          [7, 14, 30, 90].map(function (value) { return '<option value="' + value + '"' + (value === days ? ' selected' : '') + '>最近 ' + value + ' 天</option>'; }).join('') +
        '</select></div>' +
        '<button class="btn btn-sm btn-ghost" id="contest-apply" type="button">' + icon('filter') + '应用</button>' +
      '</div>' +
      '<div class="grid grid-2 mt-12">' +
        card({ title: '类型分布', icon: 'chart' }, '<div class="chart" id="contest-pie"></div>') +
        card({ title: '成绩曲线 · ' + esc(contestType || '—'), icon: 'trending', headActions: ['<span class="muted-2 text-xs">最近 ' + days + ' 天</span>'] },
          '<div class="chart chart-lg" id="contest-line"></div>') +
      '</div>' +
      '<div class="grid grid-2 mt-16">' +
        card({ title: '成绩榜单 · ' + esc(contestType || '—'), icon: 'list', flush: true, headActions: ['<span class="muted-2 text-xs">' + ranking.length + ' 条 · ' + esc(chartData.rank_source || '') + '</span>'] },
          '<div id="contest-ranking"></div>') +
        card({ title: '赛文类型', icon: 'list', flush: true, headActions: ['<span class="muted-2 text-xs">' + types.length + ' 类</span>'] },
          '<div id="contest-table"></div>') +
      '</div>';

      const pieEl = document.getElementById('contest-pie');
      if (pieEl) {
        if (pieItems.length) mountChart(pieEl, chartOptionPie(pieItems), pieItems);
        else chartNoData(pieEl, '接口没有返回带数量的赛文类型');
      }
      const lineEl = document.getElementById('contest-line');
      if (lineEl) {
        if (points.length) {
          const labels = points.map(function (row) { return row.label; });
          const series = [];
          if (points.some(function (row) { return row.speed != null; })) {
            series.push({ name: '均速', type: 'line', smooth: true, symbolSize: 5, data: points.map(function (row) { return row.speed; }), areaStyle: { opacity: 0.08 } });
          }
          if (points.some(function (row) { return row.best != null; })) {
            series.push({ name: '最高均速', type: 'line', smooth: true, symbolSize: 4, lineStyle: { width: 1, type: 'dashed' }, data: points.map(function (row) { return row.best; }) });
          }
          if (points.some(function (row) { return row.accuracy != null; })) {
            series.push({ name: '键准 %', type: 'line', smooth: true, symbolSize: 5, yAxisIndex: 1, data: points.map(function (row) { return row.accuracy; }) });
          }
          if (points.some(function (row) { return row.count != null; })) {
            series.push({ name: '参与人数', type: 'bar', yAxisIndex: 1, barMaxWidth: 10, itemStyle: { opacity: 0.35, borderRadius: [3, 3, 0, 0] }, data: points.map(function (row) { return row.count; }) });
          }
          if (!series.length) series.push({ name: '条数', type: 'bar', data: points.map(function (row) { return row.count; }) });
          mountChart(lineEl, chartBase({
            tooltip: { trigger: 'axis' },
            legend: { data: series.map(function (item) { return item.name; }) },
            xAxis: { type: 'category', data: labels },
            yAxis: [{ type: 'value', name: '字/分' }, { type: 'value', name: '键准 / 人数', max: 100 }],
            series: series,
          }), points.map(function (row) { return { label: row.label, value: row.speed != null ? row.speed : (row.count || 0) }; }));
        } else if (chartRes.ok) {
          chartNoData(lineEl, '该类型在所选时间范围（' + days + ' 天）内没有曲线数据点；可把时间范围切到 90 天，或换一个类型。');
        } else if (chartRes.pending) {
          chartNoData(lineEl, '图表数据接口未就绪：GET /api/domain/contest/chart-data');
        } else {
          chartNoData(lineEl, '图表接口失败：' + (chartRes.error ? chartRes.error.message : '未知错误'));
        }
      }
      const rankingRoot = document.getElementById('contest-ranking');
      if (rankingRoot) {
        v3Table(rankingRoot, {
          stateKey: 'contest-ranking',
          rows: ranking,
          defaultSort: 'best_speed',
          countLabel: function (count) { return count + ' 人'; },
          columns: [
            { key: 'rank', label: '#', width: '52px', num: true, render: function (row) { return '<span class="mono">' + fmtNum(row.rank) + '</span>'; } },
            { key: 'user_name', label: '用户', render: function (row) { return esc(row.user_name || '—'); } },
            { key: 'best_speed', label: '最高均速', width: '104px', num: true, render: function (row) { return row.best_speed == null ? '—' : '<b>' + fmtNum(row.best_speed, 1) + '</b>'; } },
            { key: 'avg_accuracy', label: '键准', width: '82px', num: true, render: function (row) { return row.avg_accuracy == null ? '—' : fmtNum(row.avg_accuracy, 1) + '%'; } },
            { key: 'records', label: '记录数', width: '88px', num: true, hideSm: true, render: function (row) { return fmtNum(row.records); } },
          ],
          empty: { icon: 'list', title: '该类型没有榜单数据', hint: chartRes.ok ? '接口返回 rows 为空；换一个类型或时间范围试试' : '图表接口未就绪' },
        });
      }
      const tableRoot = document.getElementById('contest-table');
      if (tableRoot) {
        v3Table(tableRoot, {
          stateKey: 'contest-types',
          rows: types,
          countLabel: function (count) { return count + ' 类'; },
          columns: [
            { key: 'label', label: '赛文类型', render: function (row) { return esc(row.label || row.type); } },
            { key: 'type', label: 'type', mono: true, hideSm: true, render: function (row) { return '<span class="mono">' + esc(row.type || '—') + '</span>'; } },
            { key: 'count', label: '稿件数', width: '110px', num: true, render: function (row) { return fmtNum(row.count); } },
            { key: 'action', label: '', width: '96px', render: function (row) {
              return '<button class="btn btn-sm btn-ghost" data-contest-type="' + escAttr(row.type || row.label) + '" type="button">看曲线</button>';
            } },
          ],
          empty: { icon: 'chart', title: '没有赛文类型数据' },
        });
        tableRoot.addEventListener('click', function (event) {
          const button = event.target.closest('[data-contest-type]');
          if (!button) return;
          const params = new URLSearchParams({ contest: button.getAttribute('data-contest-type'), days: String(days) });
          go(hashFor('contests', [], params));
        });
      }
      const typeSelect = document.getElementById('contest-type');
      const daysSelect = document.getElementById('contest-days');
      const applyBtn = document.getElementById('contest-apply');
      if (applyBtn) applyBtn.addEventListener('click', function () {
        const params = new URLSearchParams();
        if (typeSelect && typeSelect.value) params.set('contest', typeSelect.value);
        if (daysSelect) params.set('days', daysSelect.value);
        go(hashFor('contests', [], params));
      });
      guardAssert(ctx, 'contests');
    },
  });

  /* ── 18. 页面：文库浏览 ─────────────────────────────────────────────── */

  const LIBRARY_LIMIT = 50;

  defineView({
    id: 'library',
    name: '文库浏览',
    icon: 'list',
    skeleton: 'table',
    async render(ctx) {
      const keyword = ctx.q('q');
      const difficulty = ctx.q('difficulty');
      const groupId = ctx.q('group_id');
      const offset = clamp(Number(ctx.q('offset', 0)) || 0, 0, 1000000);
      const res = await tryDomain(['library/list' + qs({ q: keyword, difficulty: difficulty, group_id: groupId, limit: LIBRARY_LIMIT, offset: offset })], { timeout: 30000 });
      if (!ctx.alive()) return;
      let rows = [];
      let total = 0;
      let fallbackUsed = false;
      let fallbackTotal = 0;
      if (res.ok) {
        const data = res.data || {};
        rows = pickArray(data, ['items', 'rows', 'texts', 'list']);
        total = numOrNull(data.total) != null ? numOrNull(data.total) : rows.length;
      } else if (res.pending) {
        const legacy = await tryDomain(['library'], { timeout: 30000 });
        if (!ctx.alive()) return;
        if (legacy.ok) {
          fallbackUsed = true;
          const all = pickArray(legacy.data, ['items', 'rows', 'texts', 'list']);
          const kw = keyword.toLowerCase();
          const filtered = all.filter(function (row) {
            if (!kw) return true;
            return (textOf(row.title) + ' ' + textOf(row.relative_path) + ' ' + textOf(row.category)).toLowerCase().indexOf(kw) >= 0;
          });
          fallbackTotal = filtered.length;
          rows = filtered.slice(offset, offset + LIBRARY_LIMIT);
          total = filtered.length;
        } else {
          renderApiFailure(ctx, res, {
            title: '文库浏览',
            icon: 'list',
            paths: ['GET /api/domain/library/list?q=&limit=&offset=', 'GET /api/domain/library/text?id='],
            hint: 'Domain 新增的文库分页接口尚未就绪；旧 /library/search 代理当前也不可用。',
          });
          return;
        }
      } else {
        renderApiFailure(ctx, res, {
          title: '文库浏览',
          icon: 'list',
          paths: ['GET /api/domain/library/list', 'GET /api/domain/library/text'],
        });
        return;
      }
      const pageCount = Math.max(1, Math.ceil((total || 0) / LIBRARY_LIMIT));
      const currentPage = Math.floor(offset / LIBRARY_LIMIT) + 1;
      const avgChars = rows.length ? Math.round(rows.reduce(function (sum, row) { return sum + (Number(row.char_count || row.chars || row.length) || 0); }, 0) / rows.length) : 0;

      ctx.root.innerHTML = pageHead({
        title: '文库浏览',
        icon: 'list',
        sub: '跟打文库分页浏览 · 搜索 / 难度 / 来源群筛选，点行读取正文（前 2000 字）',
        actions: [
          '<button class="btn btn-ghost btn-sm" id="library-refresh" type="button">' + icon('refresh') + '刷新</button>',
          '<a class="btn btn-ghost btn-sm" href="#/business/library">' + icon('book') + '旧文库页</a>',
        ],
      }) +
      (fallbackUsed ? fallbackNotice('/api/domain/library/list', '/api/domain/library') : '') +
      kpiGrid([
        { label: '命中条目', icon: 'list', valueHtml: fmtNum(total), tone: 'acc', hint: keyword ? '关键词：' + keyword : '全库分页' },
        { label: '当前页', icon: 'list', valueHtml: fmtNum(currentPage) + '<small>/ ' + fmtNum(pageCount) + '</small>', hint: '每页 ' + LIBRARY_LIMIT + ' 条' },
        { label: '本页平均字数', icon: 'chart', valueHtml: fmtNum(avgChars), hint: '按 char_count 平均' },
        { label: '数据源', icon: 'link', valueHtml: '<span class="tag ' + (fallbackUsed ? 'tag-warn' : 'tag-ok') + '">' + (fallbackUsed ? 'domain 旧接口' : 'library/list') + '</span>' },
      ]) +
      '<div class="card mt-16"><div class="tbl-toolbar">' +
        '<div class="field grow"><input class="input input-sm" id="library-q" placeholder="搜索标题 / 路径 / 分类…" value="' + escAttr(keyword) + '"></div>' +
        '<div class="field" style="flex:0 1 140px"><input class="input input-sm" id="library-difficulty" placeholder="难度（可选）" value="' + escAttr(difficulty) + '"></div>' +
        '<button class="btn btn-sm btn-ghost" id="library-apply" type="button">' + icon('filter') + '筛选</button>' +
        '<span class="muted text-xs">' + rows.length + ' / ' + fmtNum(total) + ' 条</span>' +
      '</div>' +
      '<div id="library-table"></div>' +
      '<div class="card-body">' + pagerBar({ offset: offset, limit: LIBRARY_LIMIT, total: total, unit: '篇' }) + '</div></div>' +
      '<div class="notice mt-16">' + icon('info') + '<div>点击任意行会请求 <span class="mono">/api/domain/library/text?id=</span> 打开正文抽屉；接口未就绪时抽屉内会显示原因而不是空白。</div></div>';

      const tableRoot = document.getElementById('library-table');
      if (tableRoot) {
        v3Table(tableRoot, {
          stateKey: 'library-list',
          rows: rows,
          countLabel: function (count) { return count + ' 条'; },
          columns: [
            { key: 'title', label: '标题', render: function (row) { return '<span class="cell-truncate">' + esc(textOf(row.title || row.name || row.text_id || row.id)) + '</span>'; } },
            { key: 'char_count', label: '字数', width: '82px', num: true, render: function (row) { return fmtNum(row.char_count != null ? row.char_count : (row.chars != null ? row.chars : row.length)); } },
            { key: 'difficulty', label: '难度', width: '84px', hideSm: true, render: function (row) { return '<span class="tag tag-muted">' + esc(textOf(row.difficulty || '—')) + '</span>'; } },
            { key: 'category', label: '分类', width: '110px', hideSm: true, render: function (row) { return '<span class="tag tag-info">' + esc(textOf(row.category || row.primary_genre || '—')) + '</span>'; } },
            { key: 'source', label: '来源', width: '150px', hideSm: true, render: function (row) { return '<span class="mono cell-truncate">' + esc(textOf(row.source || row.relative_path || row.group_id || '—')) + '</span>'; } },
            { key: 'added_at', label: '入库时间', width: '160px', hideSm: true, render: function (row) { return '<span class="mono">' + esc(fmtTime(row.added_at || row.created_at || row.inserted_at) || '—') + '</span>'; } },
            { key: 'id', label: 'ID', width: '84px', hideSm: true, render: function (row) { return '<span class="mono">' + esc(textOf(row.id != null ? row.id : row.text_id)) + '</span>'; } },
          ],
          empty: { icon: 'book', title: keyword ? '没有匹配的文库文本' : '文库为空', hint: keyword ? '换个关键词试试' : '接口返回空 items' },
        });
        tableRoot.addEventListener('click', async function (event) {
          const tr = event.target.closest('tr.dt-row');
          if (!tr) return;
          const row = rows[Number(tr.getAttribute('data-row'))];
          if (!row) return;
          const id = textOf(row.id != null ? row.id : (row.text_id != null ? row.text_id : (row.title || '')));
          Drawer.open({
            title: '文库正文 · ' + esc(textOf(row.title || row.name || id)),
            sub: esc(textOf(row.category || row.difficulty || '')) + ' · ' + fmtNum(row.char_count != null ? row.char_count : row.chars) + ' 字',
            body: '<div class="muted-2 text-xs">正在读取 /api/domain/library/text?id=' + esc(id) + ' …</div>',
            wide: true,
            raw: row,
          });
          const textRes = await tryDomain(['library/text' + qs({ id: id })], { timeout: 20000 });
          if (!Drawer.isOpen()) return;
          const bodyRoot = document.getElementById('drawer-body');
          if (!bodyRoot) return;
          if (!textRes.ok) {
            bodyRoot.innerHTML = (textRes.pending
              ? pendingState({ title: '正文接口尚未就绪', hint: 'GET /api/domain/library/text?id=' + id, path: 'GET /api/domain/library/text' })
              : errorState(textRes.error));
            return;
          }
          const data = textRes.data || {};
          const item = data.item || data.text_info || data.meta || {};
          const text = textOf(item.content || item.content_preview || data.content || data.text || data.body || '');
          const meta = item;
          bodyRoot.innerHTML =
            '<div class="kv-grid">' +
              '<div class="kv-item"><div class="k">标题</div><div class="v">' + esc(textOf(meta.title || row.title || '—')) + '</div></div>' +
              '<div class="kv-item"><div class="k">分类 / 难度</div><div class="v">' + esc(textOf(meta.category || row.category || '—')) + ' / ' + esc(textOf(meta.difficulty || row.difficulty || '—')) + '</div></div>' +
              '<div class="kv-item"><div class="k">字数</div><div class="v mono">' + fmtNum(meta.char_count != null ? meta.char_count : (text.length || row.char_count || 0)) + (meta.content_truncated ? '（已截断）' : '') + '</div></div>' +
              '<div class="kv-item"><div class="k">来源</div><div class="v mono">' + esc(textOf(meta.source || meta.relative_path || row.source || row.group_id || '—')) + '</div></div>' +
              '<div class="kv-item"><div class="k">题材 / 形式</div><div class="v">' + esc(textOf(meta.primary_genre || '—')) + ' / ' + esc(textOf(meta.form || '—')) + '</div></div>' +
            '</div>' +
            '<div class="section-title mt-16">' + icon('book') + '正文<span class="section-sub">最多展示 2000 字</span></div>' +
            '<div class="v3-text mt-8">' + esc(truncate(text, 2000)) + '</div>';
          Drawer.raw = data;
        });
      }
      const applyFilter = function () {
        const q = document.getElementById('library-q');
        const diff = document.getElementById('library-difficulty');
        const params = new URLSearchParams();
        if (q && q.value.trim()) params.set('q', q.value.trim());
        if (diff && diff.value.trim()) params.set('difficulty', diff.value.trim());
        go(hashFor('library', [], params));
      };
      const applyBtn = document.getElementById('library-apply');
      if (applyBtn) applyBtn.addEventListener('click', applyFilter);
      const qInput = document.getElementById('library-q');
      if (qInput) qInput.addEventListener('keydown', function (event) { if (event.key === 'Enter') applyFilter(); });
      const refreshBtn = document.getElementById('library-refresh');
      if (refreshBtn) refreshBtn.addEventListener('click', function () { route(); });
      function gotoOffset(next) {
        const params = new URLSearchParams();
        if (keyword) params.set('q', keyword);
        if (difficulty) params.set('difficulty', difficulty);
        if (groupId) params.set('group_id', groupId);
        if (next) params.set('offset', String(Math.max(0, next)));
        go(hashFor('library', [], params));
      }
      ctx.root.addEventListener('click', function (event) {
        const btn = event.target.closest('[data-page]');
        if (!btn) return;
        const action = btn.getAttribute('data-page');
        if (action === 'prev') gotoOffset(offset - LIBRARY_LIMIT);
        else if (action === 'next') gotoOffset(offset + LIBRARY_LIMIT);
      });
      const jumpInput = document.querySelector('[data-page="jump"]', ctx.root);
      if (jumpInput) {
        jumpInput.addEventListener('change', function () {
          const page = clamp(Number(jumpInput.value) || 1, 1, pageCount);
          gotoOffset((page - 1) * LIBRARY_LIMIT);
        });
        jumpInput.addEventListener('keydown', function (event) {
          if (event.key !== 'Enter') return;
          event.preventDefault();
          jumpInput.dispatchEvent(new Event('change'));
        });
      }
      guardAssert(ctx, 'library');
    },
  });

  /* ── 19. 页面：打字排行 ─────────────────────────────────────────────── */

  function normalizeTypingSummary(payload) {
    const data = payload || {};
    const root = data.summary || data;
    const daily = (Array.isArray(root.daily) ? root.daily : (Array.isArray(data.daily) ? data.daily : [])).map(function (row) {
      if (!row || typeof row !== 'object') return null;
      return {
        label: textOf(firstDefined(row, ['date', 'day', 'label'])),
        speed: numOrNull(firstDefined(row, ['avg_speed', 'speed', 'avg'])),
        users: numOrNull(firstDefined(row, ['users', 'people', 'participants'])),
        records: numOrNull(firstDefined(row, ['records', 'count', 'scores'])),
      };
    }).filter(Boolean);
    return {
      raw: data,
      people: numOrNull(firstDefined(root, ['users', 'people', 'participants', 'user_count', 'members'])),
      total: numOrNull(firstDefined(root, ['records', 'total', 'count', 'scores', 'total_records', 'entries'])),
      avgSpeed: numOrNull(firstDefined(root, ['avg_speed', 'speed', 'avg', 'speed_mean', 'average_speed', 'cpm'])),
      avgAccuracy: numOrNull(firstDefined(root, ['avg_accuracy', 'accuracy', 'key_accuracy', 'correct_rate', 'acc'])),
      activeDays: numOrNull(firstDefined(root, ['active_days', 'days', 'activity_days', 'day_count'])),
      bestSpeed: numOrNull(root.best_speed),
      chars: numOrNull(root.chars),
      daily: daily,
      range: root.range || root.days || null,
    };
  }

  function normalizeTypingRows(payload) {
    const list = pickArray(payload, ['items', 'rows', 'leaderboard', 'ranking', 'list', 'users']);
    return list.filter(function (item) { return item && typeof item === 'object'; }).map(function (item) {
      return {
        user: textOf(firstDefined(item, ['user_name', 'user', 'username', 'name', 'sender_name', 'nickname', 'user_id', 'sender_id']) || '未知'),
        rank: numOrNull(item.rank),
        avg_speed: numOrNull(firstDefined(item, ['avg_speed', 'speed', 'avg', 'average_speed', 'cpm'])),
        accuracy: numOrNull(firstDefined(item, ['avg_accuracy', 'accuracy', 'key_accuracy', 'correct_rate', 'acc', 'key_acc'])),
        count: numOrNull(firstDefined(item, ['records', 'count', 'scores', 'total', 'n', 'entries'])),
        days: numOrNull(firstDefined(item, ['active_days', 'days', 'day_count'])),
        best: numOrNull(firstDefined(item, ['best_speed', 'best', 'max_speed', 'top_speed'])),
        raw: item,
      };
    });
  }

  defineView({
    id: 'typing',
    name: '打字排行',
    icon: 'trending',
    skeleton: 'default',
    async render(ctx) {
      let groups = [];
      try { groups = await getGroups(); } catch (err) { groups = []; }
      if (!ctx.alive()) return;
      const days = clamp(Number(ctx.q('days', 30)) || 30, 1, 365);
      const groupId = ctx.q('group_id');
      const [summaryRes, boardRes] = await Promise.all([
        tryDomain(['typing/summary' + qs({ group_id: groupId, days: days })], { timeout: 25000 }),
        tryDomain(['typing/leaderboard' + qs({ group_id: groupId, days: days, limit: 50 })], { timeout: 25000 }),
      ]);
      if (!ctx.alive()) return;
      if (!summaryRes.ok && !boardRes.ok) {
        renderApiFailure(ctx, summaryRes.ok ? boardRes : summaryRes, {
          title: '打字排行',
          icon: 'trending',
          paths: ['GET /api/domain/typing/summary?group_id=&days=', 'GET /api/domain/typing/leaderboard?group_id=&days=&limit='],
          hint: 'Domain 打字成绩统计接口（task-3）尚未就绪，就绪后自动展示人数 / 均速 / 键准排行与柱状图。',
        });
        return;
      }
      const summary = summaryRes.ok ? normalizeTypingSummary(summaryRes.data || {}) : { raw: {} };
      const rows = boardRes.ok ? normalizeTypingRows(boardRes.data || {}) : [];
      const topRows = rows.slice(0, 12);

      ctx.root.innerHTML = pageHead({
        title: '打字排行',
        icon: 'trending',
        sub: '群打字成绩排行 · 均速 / 键准 / 条数 / 活跃天数 · 数据源 /typing/leaderboard 与 /typing/summary',
        actions: [
          '<button class="btn btn-ghost btn-sm" data-retry="1" type="button">' + icon('refresh') + '刷新</button>',
          '<a class="btn btn-ghost btn-sm" href="#/business/scores">' + icon('chart') + '旧成绩页</a>',
        ],
      }) +
      (!summaryRes.ok ? fallbackNotice('/api/domain/typing/summary', '/api/domain/typing/leaderboard') : '') +
      (!boardRes.ok ? fallbackNotice('/api/domain/typing/leaderboard', '/api/domain/typing/summary') : '') +
      kpiGrid([
        { label: '参与人数', icon: 'members', valueHtml: fmtNum(summary.people), tone: 'acc', hint: '最近 ' + days + ' 天' },
        { label: '成绩条数', icon: 'list', valueHtml: fmtNum(summary.total) },
        { label: '均速均值', icon: 'trending', valueHtml: fmtNum(summary.avgSpeed, 1) + '<small>字/分</small>' },
        { label: '平均键准', icon: 'check', valueHtml: summary.avgAccuracy == null ? '—' : fmtNum(summary.avgAccuracy, 1) + '<small>%</small>' + (summary.activeDays != null ? ' · 活跃 ' + fmtNum(summary.activeDays) + ' 天' : '') },
      ]) +
      '<div class="filters mt-12">' +
        '<div class="field"><label>群筛选</label><select class="select select-sm" id="typing-group"><option value="">全部群</option>' +
          groups.map(function (group) {
            const id = textOf(group.group_id);
            return '<option value="' + escAttr(id) + '"' + (id === groupId ? ' selected' : '') + '>' + esc(groupLabel(group)) + '</option>';
          }).join('') +
        '</select></div>' +
        '<div class="field"><label>时间范围</label><select class="select select-sm" id="typing-days">' +
          [7, 14, 30, 90].map(function (value) { return '<option value="' + value + '"' + (value === days ? ' selected' : '') + '>最近 ' + value + ' 天</option>'; }).join('') +
        '</select></div>' +
        '<button class="btn btn-sm btn-ghost" id="typing-apply" type="button">' + icon('filter') + '应用</button>' +
      '</div>' +
      '<div class="grid grid-2 mt-12">' +
        card({ title: '均速 Top ' + topRows.length, icon: 'chart' }, '<div class="chart chart-lg" id="typing-chart"></div>') +
        card({ title: '汇总', icon: 'activity' }, '<div class="v3-list" id="typing-summary"></div>') +
      '</div>' +
      (summary.daily.length ? '<div class="card mt-16"><div class="card-head"><h3>' + icon('trending') + '均速趋势<span class="section-sub">' + summary.daily.length + ' 天 · 来自 /typing/summary.daily</span></h3></div><div class="card-body"><div class="chart" id="typing-trend"></div></div></div>' : '') +
      '<div class="card mt-16"><div class="card-head"><h3>' + icon('list') + '排行榜<span class="section-sub">' + rows.length + ' 人</span></h3></div>' +
      '<div id="typing-table"></div></div>';

      const chartEl = document.getElementById('typing-chart');
      if (chartEl) {
        if (topRows.length) {
          mountChart(chartEl, chartOptionBars(topRows.map(function (row) { return row.user; }),
            topRows.map(function (row) { return row.avg_speed == null ? 0 : row.avg_speed; }), '均速'), topRows.map(function (row) {
            return { label: row.user, value: row.avg_speed || 0 };
          }));
        } else {
          chartNoData(chartEl, '接口没有返回排行数据');
        }
      }
      const trendEl = document.getElementById('typing-trend');
      if (trendEl && summary.daily.length) {
        mountChart(trendEl, chartBase({
          tooltip: { trigger: 'axis' },
          legend: { data: ['均速', '参与人数'] },
          xAxis: { type: 'category', data: summary.daily.map(function (row) { return row.label; }) },
          yAxis: [{ type: 'value', name: '字/分' }, { type: 'value', name: '人数' }],
          series: [
            { name: '均速', type: 'line', smooth: true, symbolSize: 4, areaStyle: { opacity: 0.1 }, data: summary.daily.map(function (row) { return row.speed; }) },
            { name: '参与人数', type: 'bar', yAxisIndex: 1, barMaxWidth: 10, itemStyle: { opacity: 0.32, borderRadius: [3, 3, 0, 0] }, data: summary.daily.map(function (row) { return row.users; }) },
          ],
        }), summary.daily.map(function (row) { return { label: row.label, value: row.speed }; }));
      }
      const summaryRoot = document.getElementById('typing-summary');
      if (summaryRoot) {
        const items = [
          ['参与人数', fmtNum(summary.people)],
          ['成绩条数', fmtNum(summary.total)],
          ['均速均值', summary.avgSpeed == null ? '—' : fmtNum(summary.avgSpeed, 1) + ' 字/分'],
          ['最高均速', summary.bestSpeed == null ? '—' : fmtNum(summary.bestSpeed, 1) + ' 字/分'],
          ['平均键准', summary.avgAccuracy == null ? '—' : fmtNum(summary.avgAccuracy, 1) + '%'],
          ['活跃天数', fmtNum(summary.activeDays)],
          ['总字数', summary.chars == null ? '—' : fmtNum(summary.chars)],
          ['数据范围', days + ' 天'],
        ];
        summaryRoot.innerHTML = items.map(function (pair) {
          return '<div class="v3-list-item"><span class="v3-meter-label">' + esc(pair[0]) + '</span><span class="spacer"></span><b>' + esc(pair[1]) + '</b></div>';
        }).join('') +
          '<div class="v3-list-item"><span class="muted-2 text-xs">原始 JSON</span><span class="spacer"></span>' +
          '<button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(summaryRes.ok ? summaryRes.data : { error: 'summary 未就绪' }).slice(0, 6000)) + '" data-json-title="打字汇总原始返回" type="button">' + icon('eye') + '查看</button></div>';
      }
      const tableRoot = document.getElementById('typing-table');
      if (tableRoot) {
        v3Table(tableRoot, {
          stateKey: 'typing-board',
          rows: rows,
          defaultSort: 'avg_speed',
          countLabel: function (count) { return count + ' 人'; },
          columns: [
            { key: 'rank', label: '#', width: '52px', num: true, render: function (row) { return row.rank == null ? '<span class="muted-2">—</span>' : '<span class="mono">' + fmtNum(row.rank) + '</span>'; } },
            { key: 'user', label: '用户', render: function (row) { return esc(row.user); } },
            { key: 'avg_speed', label: '均速', width: '100px', num: true, render: function (row) { return row.avg_speed == null ? '—' : '<b>' + fmtNum(row.avg_speed, 1) + '</b> <span class="muted-2 text-xs">字/分</span>'; } },
            { key: 'accuracy', label: '键准', width: '90px', num: true, render: function (row) { return row.accuracy == null ? '—' : fmtNum(row.accuracy, 1) + '%'; } },
            { key: 'count', label: '条数', width: '84px', num: true, render: function (row) { return fmtNum(row.count); } },
            { key: 'days', label: '活跃天数', width: '96px', num: true, hideSm: true, render: function (row) { return fmtNum(row.days); } },
            { key: 'best', label: '最高均速', width: '110px', num: true, hideSm: true, render: function (row) { return fmtNum(row.best, 1); } },
          ],
          empty: { icon: 'trending', title: '没有排行数据', hint: '接口返回空 items；可能所选时间范围内没有成绩' },
        });
      }
      const applyBtn = document.getElementById('typing-apply');
      if (applyBtn) applyBtn.addEventListener('click', function () {
        const groupSel = document.getElementById('typing-group');
        const daysSel = document.getElementById('typing-days');
        const params = new URLSearchParams();
        if (groupSel && groupSel.value) params.set('group_id', groupSel.value);
        if (daysSel) params.set('days', daysSel.value);
        go(hashFor('typing', [], params));
      });
      guardAssert(ctx, 'typing');
    },
  });

  /* ── 20. 页面：群日报 ──────────────────────────────────────────────── */

  function todayText() {
    const now = new Date();
    return now.getFullYear() + '-' + pad(now.getMonth() + 1) + '-' + pad(now.getDate());
  }

  function normalizeDailyReport(payload) {
    const data = payload || {};
    const messages = (data.messages && typeof data.messages === 'object') ? data.messages : {};
    const typing = (data.typing && typeof data.typing === 'object') ? data.typing : {};
    const contest = (data.contest && typeof data.contest === 'object') ? data.contest : {};
    const aiContest = (data.ai_contest && typeof data.ai_contest === 'object') ? data.ai_contest : {};
    const library = (data.library && typeof data.library === 'object') ? data.library : {};
    const byHour = Array.isArray(messages.by_hour) ? messages.by_hour : [];
    const hourly = byHour.map(function (value, index) {
      if (value && typeof value === 'object') {
        return { label: textOf(firstDefined(value, ['hour', 'label', 'bucket'])) || pad(index), count: Number(firstDefined(value, ['count', 'messages', 'total', 'value'])) || 0 };
      }
      return { label: pad(index), count: Number(value) || 0 };
    });
    const typingRows = (Array.isArray(typing.top) ? typing.top : []).filter(function (row) { return row && typeof row === 'object'; }).map(function (row) {
      return {
        rank: numOrNull(row.rank),
        user: textOf(firstDefined(row, ['user_name', 'user', 'username', 'name']) || '—'),
        avg_speed: numOrNull(firstDefined(row, ['avg_speed', 'speed'])),
        best_speed: numOrNull(firstDefined(row, ['best_speed', 'speed'])),
        accuracy: numOrNull(firstDefined(row, ['avg_accuracy', 'accuracy', 'key_accuracy'])),
        records: numOrNull(firstDefined(row, ['records', 'count', 'scores'])),
        raw: row,
      };
    });
    const contestRows = (Array.isArray(contest.items) ? contest.items : []).filter(function (row) { return row && typeof row === 'object'; }).map(function (row) {
      return {
        key: textOf(row.key || row.type || ''),
        name: textOf(row.name || row.label || row.key || ''),
        scores: numOrNull(row.scores != null ? row.scores : row.count),
        users: numOrNull(row.users),
        best_speed: numOrNull(row.best_speed),
        raw: row,
      };
    });
    const byGroup = (Array.isArray(messages.by_group) ? messages.by_group : []).filter(function (row) { return row && typeof row === 'object'; }).map(function (row) {
      return {
        group_id: textOf(row.group_id || ''),
        group_name: textOf(row.group_name || row.name || row.group_id || ''),
        messages: numOrNull(row.messages != null ? row.messages : row.count),
        users: numOrNull(row.users != null ? row.users : row.user_count),
        raw: row,
      };
    });
    const topSenders = (Array.isArray(messages.top_senders) ? messages.top_senders : []).filter(function (row) { return row && typeof row === 'object'; }).map(function (row) {
      return {
        sender_id: textOf(row.sender_id || row.user_id || ''),
        sender_name: textOf(row.sender_name || row.user_name || row.name || row.sender_id || ''),
        messages: numOrNull(row.messages != null ? row.messages : row.count),
        raw: row,
      };
    });
    return {
      raw: data,
      messages: numOrNull(messages.total != null ? messages.total : messages.count),
      activeUsers: numOrNull(messages.active_users != null ? messages.active_users : messages.users),
      librarySent: numOrNull(library.sent),
      libraryRecalls: numOrNull(library.recalls),
      typingRows: typingRows,
      typingRecords: numOrNull(typing.records),
      typingUsers: numOrNull(typing.users),
      contestRows: contestRows,
      contestScores: numOrNull(contest.scores),
      contestUsers: numOrNull(contest.users),
      aiContestScores: numOrNull(aiContest.scores),
      aiContestUsers: numOrNull(aiContest.users),
      aiContestBest: numOrNull(aiContest.best_speed),
      fallbackTyping: pickArray(data, ['top_typing', 'typing_scores', 'top_scores']),
      fallbackContests: pickArray(data, ['contests', 'contest_texts', 'contest_records']),
      hourly: hourly,
      byGroup: byGroup,
      topSenders: topSenders,
      digest: data.digest,
      groupName: textOf(data.group_name || data.group || ''),
      date: textOf(data.date || ''),
    };
  }

  function isBinaryPayload(data) {
    if (!data || typeof data !== 'object') return false;
    if (typeof data.raw === 'string') {
      const text = data.raw.trim();
      return text.indexOf('PNG') >= 0 || text.indexOf('\u0089PNG') >= 0 || text.charCodeAt(0) === 0x89;
    }
    return false;
  }

  defineView({
    id: 'reports',
    name: '群日报',
    icon: 'message',
    skeleton: 'default',
    async render(ctx) {
      let groups = [];
      try { groups = await getGroups(); } catch (err) { groups = []; }
      if (!ctx.alive()) return;
      const groupId = ctx.q('group_id');
      const date = ctx.q('date') || todayText();
      const res = await tryDomain(['reports/daily' + qs({ group_id: groupId, date: date })], { timeout: 30000 });
      if (!ctx.alive()) return;
      if (!res.ok) {
        renderApiFailure(ctx, res, {
          title: '群日报',
          icon: 'message',
          paths: ['GET /api/domain/reports/daily?group_id=&date='],
          hint: 'Domain 群日报接口（task-3）尚未就绪。注意：admin 代理把 <span class="mono">/api/domain/reports/*</span> 走二进制通道（Content-Type 为图片），但 JSON 正文仍然能被前端解析；若后端返回 PNG 则前端会提示无法解析。',
        });
        return;
      }
      const report = normalizeDailyReport(res.data || {});
      if (isBinaryPayload(res.data)) {
        ctx.root.innerHTML = pageHead({ title: '群日报', icon: 'message' }) +
          notice('warn', '接口返回的是二进制内容（PNG 图片），无法作为报表数据解析。') +
          notice('info', '原因：<span class="mono">/api/domain/reports/*</span> 在 admin 里被映射到 <span class="mono">fetch_domain_binary</span> 并强制 Content-Type: image/png。建议后端为 <span class="mono">/reports/daily</span> 增加 JSON 代理，或前端改用 domain 直连。');
        guardAssert(ctx, 'reports');
        return;
      }
      const hourlyPoints = report.hourly.filter(function (row) { return row && row.count > 0; });
      const totalHourly = report.hourly.reduce(function (sum, row) { return sum + (Number(row.count) || 0); }, 0);
      const typingRows = report.typingRows;
      const contestRows = report.contestRows;

      ctx.root.innerHTML = pageHead({
        title: '群日报',
        icon: 'message',
        sub: '按群 / 日期查看当日消息量、分时分布、群分布、活跃发言者、打字成绩与赛文参与 · 数据源 /api/domain/reports/daily',
        actions: [
          '<button class="btn btn-ghost btn-sm" data-retry="1" type="button">' + icon('refresh') + '刷新</button>',
          '<button class="btn btn-ghost btn-sm" data-json-drawer="' + escAttr(jsonSum(report.raw).slice(0, 9000)) + '" data-json-title="日报原始返回" type="button">' + icon('eye') + '原始 JSON</button>',
        ],
      }) +
      '<div class="filters mt-12">' +
        '<div class="field"><label>群</label><select class="select select-sm" id="report-group"><option value="">全部群</option>' +
          groups.map(function (group) {
            const id = textOf(group.group_id);
            return '<option value="' + escAttr(id) + '"' + (id === groupId ? ' selected' : '') + '>' + esc(groupLabel(group)) + '</option>';
          }).join('') +
        '</select></div>' +
        '<div class="field"><label>日期</label><input class="input input-sm" id="report-date" type="date" value="' + escAttr(date) + '"></div>' +
        '<button class="btn btn-sm btn-ghost" id="report-apply" type="button">' + icon('filter') + '查看</button>' +
        '<span class="muted-2 text-xs">' + esc(report.date || date) + (report.groupName ? ' · ' + esc(report.groupName) : '') + ' · 消息合计 ' + fmtNum(totalHourly) + '</span>' +
      '</div>' +
      kpiGrid([
        { label: '消息量', icon: 'message', valueHtml: fmtNum(report.messages), tone: 'acc', hint: '当日总消息（含机器人）' },
        { label: '活跃人数', icon: 'members', valueHtml: fmtNum(report.activeUsers), hint: '当日在群发言人数' },
        { label: '打字成绩', icon: 'trending', valueHtml: fmtNum(report.typingRecords) + '<small>条</small>', hint: (report.typingUsers == null ? '—' : fmtNum(report.typingUsers) + ' 人') + ' · Top ' + typingRows.length + ' 展示' },
        { label: '赛文成绩', icon: 'book', valueHtml: fmtNum(report.contestScores) + '<small>条</small>', hint: '参与 ' + fmtNum(report.contestUsers) + ' 人 · AI 赛文 ' + fmtNum(report.aiContestScores) + ' 条' },
      ]) +
      (hourlyPoints.length
        ? '<div class="card mt-16"><div class="card-head"><h3>' + icon('chart') + '分时消息量<span class="section-sub">按小时（0–23）</span></h3></div><div class="card-body"><div class="chart" id="report-chart"></div></div></div>'
        : '') +
      '<div class="grid grid-2 mt-16">' +
        card({ title: '群消息分布', icon: 'groups', flush: true, headActions: ['<span class="muted-2 text-xs">' + report.byGroup.length + ' 个群</span>'] }, '<div id="report-groups"></div>') +
        card({ title: '活跃发言者', icon: 'members', flush: true, headActions: ['<span class="muted-2 text-xs">' + report.topSenders.length + ' 人</span>'] }, '<div id="report-senders"></div>') +
      '</div>' +
      '<div class="grid grid-2 mt-16">' +
        card({ title: 'Top 打字成绩', icon: 'trending', flush: true, headActions: ['<span class="muted-2 text-xs">' + esc(report.date || date) + '</span>'] }, '<div id="report-typing"></div>') +
        card({ title: '赛文成绩', icon: 'book', flush: true, headActions: ['<span class="muted-2 text-xs">' + contestRows.length + ' 类</span>'] }, '<div id="report-contests"></div>') +
      '</div>' +
      (report.librarySent != null || report.libraryRecalls != null
        ? '<div class="notice mt-16">' + icon('book') + '<div>文库：当日发送 <b>' + fmtNum(report.librarySent) + '</b> 条 · 撤回 <b>' + fmtNum(report.libraryRecalls) + '</b> 条</div></div>'
        : '') +
      '<div class="notice mt-16">' + icon('info') + '<div>日报由 Domain 汇总当日数据；没数据时显示空状态。日期按服务器所在时区（Asia/Shanghai）计算。该接口经 admin 二进制代理返回，JSON 正文可正常解析。</div></div>';

      if (hourlyPoints.length) {
        const chartEl = document.getElementById('report-chart');
        mountChart(chartEl, chartBase({
          tooltip: { trigger: 'axis' },
          xAxis: { type: 'category', data: report.hourly.map(function (row) { return row.label + ':00'; }) },
          yAxis: { type: 'value' },
          series: [{ name: '消息量', type: 'bar', barMaxWidth: 14, itemStyle: { borderRadius: [3, 3, 0, 0] }, data: report.hourly.map(function (row) { return row.count; }) }],
        }), hourlyPoints.map(function (row) { return { label: row.label + ':00', value: row.count }; }));
      }
      const groupsRoot = document.getElementById('report-groups');
      if (groupsRoot) {
        renderTable(groupsRoot, {
          columns: [
            { key: 'group_name', label: '群', render: function (row) { return '<span class="cell-truncate">' + esc(row.group_name) + '</span>'; } },
            { key: 'messages', label: '消息', width: '84px', num: true, render: function (row) { return fmtNum(row.messages); } },
            { key: 'users', label: '发言人数', width: '92px', num: true, render: function (row) { return fmtNum(row.users); } },
          ],
          rows: report.byGroup,
          onRow: function (row) { openJsonDrawer('群分布详情', row.raw); },
          empty: { icon: 'groups', title: '当日没有群消息', hint: '接口返回的 messages.by_group 为空' },
        });
      }
      const sendersRoot = document.getElementById('report-senders');
      if (sendersRoot) {
        renderTable(sendersRoot, {
          columns: [
            { key: 'sender_name', label: '成员', render: function (row) { return esc(row.sender_name); } },
            { key: 'messages', label: '消息', width: '84px', num: true, render: function (row) { return fmtNum(row.messages); } },
            { key: 'sender_id', label: 'ID', width: '120px', hideSm: true, render: function (row) { return '<span class="mono">' + esc(row.sender_id) + '</span>'; } },
          ],
          rows: report.topSenders,
          onRow: function (row) { openJsonDrawer('发言者详情', row.raw); },
          empty: { icon: 'members', title: '当日没有发言者', hint: '接口返回的 messages.top_senders 为空' },
        });
      }
      const typingRoot = document.getElementById('report-typing');
      if (typingRoot) {
        renderTable(typingRoot, {
          columns: [
            { key: 'rank', label: '#', width: '46px', num: true, render: function (row) { return '<span class="mono">' + fmtNum(row.rank) + '</span>'; } },
            { key: 'user', label: '用户', render: function (row) { return esc(row.user); } },
            { key: 'avg_speed', label: '均速', width: '88px', num: true, render: function (row) { return row.avg_speed == null ? '—' : fmtNum(row.avg_speed, 1); } },
            { key: 'best_speed', label: '最高', width: '84px', num: true, hideSm: true, render: function (row) { return row.best_speed == null ? '—' : fmtNum(row.best_speed, 1); } },
            { key: 'accuracy', label: '键准', width: '78px', num: true, render: function (row) { return row.accuracy == null ? '—' : fmtNum(row.accuracy, 1) + '%'; } },
            { key: 'records', label: '条数', width: '74px', num: true, hideSm: true, render: function (row) { return fmtNum(row.records); } },
          ],
          rows: typingRows,
          onRow: function (row) { openJsonDrawer('打字成绩详情', row.raw); },
          empty: { icon: 'trending', title: '当日没有打字成绩', hint: '接口返回的 typing.top 为空' },
        });
      }
      const contestRoot = document.getElementById('report-contests');
      if (contestRoot) {
        renderTable(contestRoot, {
          columns: [
            { key: 'name', label: '赛文类型', render: function (row) { return esc(row.name || row.key); } },
            { key: 'scores', label: '成绩数', width: '84px', num: true, render: function (row) { return fmtNum(row.scores); } },
            { key: 'users', label: '人数', width: '74px', num: true, render: function (row) { return fmtNum(row.users); } },
            { key: 'best_speed', label: '最高均速', width: '96px', num: true, hideSm: true, render: function (row) { return row.best_speed == null ? '—' : fmtNum(row.best_speed, 1); } },
          ],
          rows: contestRows,
          onRow: function (row) { openJsonDrawer('赛文成绩详情', row.raw); },
          empty: { icon: 'book', title: '当日没有赛文记录', hint: '接口返回的 contest.items 为空' },
        });
      }
      const applyBtn = document.getElementById('report-apply');
      if (applyBtn) applyBtn.addEventListener('click', function () {
        const groupSel = document.getElementById('report-group');
        const dateInput = document.getElementById('report-date');
        const params = new URLSearchParams();
        if (groupSel && groupSel.value) params.set('group_id', groupSel.value);
        if (dateInput && dateInput.value) params.set('date', dateInput.value);
        go(hashFor('reports', [], params));
      });
      guardAssert(ctx, 'reports');
    },
  });

  /* ── 21. 启动挂钩 / 对外自检入口 ───────────────────────────────────── */

  function bootstrapV3() {
    /* v4：主题按钮由 console.js 的顶栏统一提供（#theme-toggle），这里不再注入 */
    updateThemeButtons();
    V3.pages = Object.keys(VIEWS);
    V3.navSections = EXTRA_SECTIONS.map(function (section) { return section.label; });
    V3.ready = true;
    const added = EXTRA_SECTIONS.reduce(function (sum, section) { return sum + section.items.length; }, 0);
    console.info('[FM Console] v3 扩展已加载：' + added + ' 个新页面 / 共 ' + V3.pages.length + ' 个视图 / 主题=' + currentTheme());
  }

  V3.auditPages = function () {
    return {
      version: V3.version,
      pages: V3.pages || Object.keys(VIEWS),
      assertions: assertSummary(),
      theme: currentTheme(),
    };
  };
  V3.runLocalSelfTest = runLocalSelfTest;
  V3.toggleTheme = toggleTheme;
  V3.applyTheme = applyTheme;

  if (document.readyState === 'loading') {
    window.addEventListener('DOMContentLoaded', bootstrapV3);
  } else {
    bootstrapV3();
  }
})();
