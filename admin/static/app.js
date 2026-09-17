/* ============================================================================
   FM Control Center · 入口
   外壳、路由、快捷键。视图在 views-*.js，零件在 core.js。
   ========================================================================= */

import {
  $, $$, h, initTheme, toggleTheme, toast, parseRoute, navigate,
  api, setUnauthorizedHandler, skeleton, errorState, invalidate,
  initParticles, countUpIn,
} from './core.js';

import { renderLibrary, renderScores } from './views-business.js';
import { renderContests, renderGroups, renderMessages } from './views-contests-groups.js';
import { renderDashboard, renderSettings } from './views-system.js';
import { renderBotTraffic } from './views-bot-traffic.js';

/* ── 视图表 ───────────────────────────────────────────────────────────── */
/* 变化记录：
   原先的「媒体与资源 / 任务与并发 / 审计日志」已撤 —— 前两个的数据不可靠
   或没有产出，第三个没人会翻；有用的信号并进了仪表盘。

   「会话管理」也撤了：那是一套 ACP 会话，库里只有 15 行测试数据
   （deployment-smoke / skill-check / probe-ok…），日志里零活动。
   而机器人自己的回复存在 cosmobot.sqlite3 的 chat_log 里（3086 条），
   后台此前完全没有入口。所以那个导航位置换成「机器人发言」。 */
const VIEWS = {
  dashboard: { title: '仪表盘',     group: '概览', render: renderDashboard },
  messages:  { title: '消息与归档', group: '对话', render: renderMessages },
  'bot-traffic': { title: '机器人发言', group: '对话', render: renderBotTraffic },
  library:   { title: '跟打文库',   group: '业务', render: renderLibrary },
  scores:    { title: '成绩中心',   group: '业务', render: renderScores },
  contests:  { title: '赛文赛事',   group: '业务', render: renderContests },
  groups:    { title: '群与房间',   group: '业务', render: renderGroups },
  settings:  { title: '设置',       group: '系统', render: renderSettings },
};

let current = '';
let ticket = 0;

/* ── 渲染 ─────────────────────────────────────────────────────────────── */
async function paint(route = parseRoute()) {
  const view = VIEWS[route.name] || VIEWS.dashboard;
  const name = VIEWS[route.name] ? route.name : 'dashboard';
  const mine = ++ticket;
  current = name;

  $$('#nav .nav-item').forEach((b) => {
    const on = b.dataset.view === name;
    b.setAttribute('aria-current', on ? 'page' : 'false');
  });

  document.title = `${view.title} · FM Control Center`;
  $('#page-group').textContent = view.group;
  $('#page-title').textContent = view.title;

  const page = $('#page');
  /* 通知上一个视图收摊：释放图表实例、摘掉全局监听 */
  page.firstElementChild?.dispatchEvent(new CustomEvent('view:leave'));
  page.replaceChildren(skeleton(5));

  try {
    const node = await view.render(route);
    if (mine !== ticket) return;
    page.replaceChildren(node);
    /* 回到页面顶部。这里必须用 scrollTo(0,0)：
       scrollIntoView 会尽量少滚，而浏览器恢复的上一次滚动位置往往落在
       长页面的中段，结果新页面停在半截、内容被吸顶栏压住，看起来像"字显示不全"。
       block:'start' 也会因为不知道顶栏有多高而把标题滚到栏下面。 */
    window.scrollTo(0, 0);
    /* 页面挂上去之后再启动数字滚动，否则 rAF 跑在游离节点上 */
    countUpIn(page);
  } catch (err) {
    if (mine !== ticket) return;
    page.replaceChildren(errorState(err.message, () => paint(route)));
  }
}

/* ── 侧栏：桌面折叠 + 移动抽屉 ────────────────────────────────────────── */
const RAIL_KEY = 'fm-admin-rail';

function setRail(mode) {
  const app = $('#app');
  app.dataset.rail = mode;
  try { localStorage.setItem(RAIL_KEY, mode); } catch { /* ignore */ }
}

function initRail() {
  const app = $('#app');

  let mode = 'full';
  try { mode = localStorage.getItem(RAIL_KEY) || 'full'; } catch { /* ignore */ }
  app.dataset.rail = mode;

  $('#rail-toggle')?.addEventListener('click', () =>
    setRail(app.dataset.rail === 'mini' ? 'full' : 'mini'));

  $('#bar-burger')?.addEventListener('click', () => app.toggleAttribute('data-drawer'));

  $('#nav')?.addEventListener('click', (e) => {
    const link = e.target.closest('.nav-item');
    if (!link) return;
    app.removeAttribute('data-drawer');
    if (link.dataset.view === current) return;
    invalidate();
    navigate(link.dataset.view);
  });

  /* 点空白处关抽屉 */
  document.addEventListener('click', (e) => {
    if (!app.hasAttribute('data-drawer')) return;
    if (e.target.closest('.rail') || e.target.closest('#bar-burger')) return;
    app.removeAttribute('data-drawer');
  });
}

/* ── 顶栏 ─────────────────────────────────────────────────────────────── */
function initBar() {
  const clock = $('#clock');
  const tick = () => {
    if (clock) clock.textContent = new Date().toLocaleTimeString('zh-CN', { hour12: false });
  };
  tick();
  setInterval(tick, 1000);

  $('#theme')?.addEventListener('click', () => { toggleTheme(); paint(); });

  $('#reload')?.addEventListener('click', async (e) => {
    const btn = e.currentTarget;
    btn.setAttribute('data-busy', '');
    invalidate();
    await paint();
    btn.removeAttribute('data-busy');
    toast('已刷新', 'ok', 1400);
  });

  /* 顶部搜索：回车直接带着关键词去文库 */
  const find = $('#find');
  find?.addEventListener('keydown', (e) => {
    if (e.key !== 'Enter') return;
    const q = find.value.trim();
    if (!q) { navigate('library'); return; }
    invalidate();
    navigate('library', { q });
  });

  document.addEventListener('keydown', (e) => {
    const typing = /^(INPUT|TEXTAREA|SELECT)$/.test(document.activeElement?.tagName || '');
    if (e.key === '\\' && !typing) {
      e.preventDefault();
      setRail($('#app').dataset.rail === 'mini' ? 'full' : 'mini');
      return;
    }
    if (e.key === '/' && !typing) { e.preventDefault(); find?.focus(); return; }
    if ((e.ctrlKey || e.metaKey) && e.key.toLowerCase() === 'k') {
      e.preventDefault();
      openPalette();
    }
  });
}

/* ── 命令面板（⌘K / Ctrl+K） ──────────────────────────────────────────── */
function openPalette() {
  if ($('.cmdk')) return;
  const entries = Object.entries(VIEWS);

  const input = h('input', { placeholder: '跳到某个页面，或输入关键词搜索文库…', 'aria-label': '命令面板' });
  const list = h('div', { class: 'cmdk-list' });
  let rows = [];

  function draw(query) {
    const q = query.trim().toLowerCase();
    const hit = entries.filter(([, v]) => !q || v.title.toLowerCase().includes(q) || v.group.includes(q));
    rows = q && !hit.length
      ? [{ label: `在文库中搜索「${query.trim()}」`, hint: '回车', run: () => navigate('library', { q: query.trim() }) }]
      : hit.map(([key, v]) => ({ label: v.title, hint: v.group, run: () => navigate(key) }));
    list.replaceChildren(...rows.map((r, i) =>
      h('div', { class: 'cmdk-row', 'data-on': i === 0 ? '' : null, onclick: () => { close(); r.run(); } },
        h('span', {}, r.label), h('small', {}, r.hint))));
  }

  let cursor = 0;
  const move = (delta) => {
    const nodes = $$('.cmdk-row', list);
    if (!nodes.length) return;
    cursor = (cursor + delta + nodes.length) % nodes.length;
    nodes.forEach((n, i) => n.toggleAttribute('data-on', i === cursor));
    nodes[cursor].scrollIntoView({ block: 'nearest' });
  };

  const box = h('div', { class: 'cmdk-box' },
    input,
    list,
    h('div', { class: 'card-head', style: 'border-bottom:0;border-top:1px solid var(--border)' },
      h('p', { style: 'font-size:var(--t-sm)' }, '↑↓ 选择 · 回车打开 · Esc 关闭')));

  const veil = h('div', { class: 'cmdk' }, box);
  function close() {
    document.removeEventListener('keydown', onKey);
    veil.remove();
  }
  function onKey(e) {
    if (e.key === 'Escape') { e.preventDefault(); close(); }
    else if (e.key === 'ArrowDown') { e.preventDefault(); cursor = Math.min(cursor + 1, rows.length - 1); move(0); }
    else if (e.key === 'ArrowUp') { e.preventDefault(); cursor = Math.max(cursor - 1, 0); move(0); }
    else if (e.key === 'Enter') { e.preventDefault(); const r = rows[cursor]; if (r) { close(); r.run(); } }
  }

  input.addEventListener('input', () => { cursor = 0; draw(input.value); });
  veil.addEventListener('click', (e) => { if (e.target === veil) close(); });
  document.addEventListener('keydown', onKey);

  draw('');
  document.body.append(veil);
  input.focus();
}

/* ── 服务状态灯 ───────────────────────────────────────────────────────── */
async function initLamp() {
  const lamp = $('#lamp');
  const text = $('#lamp-text');
  try {
    const health = await api('/api/health');
    const ok = health?.ok !== false;
    lamp.dataset.s = ok ? 'ok' : 'warn';
    text.textContent = ok ? '服务正常' : '服务异常';
  } catch {
    lamp.dataset.s = 'down';
    text.textContent = '无法连接';
  }
}

/* ── 登录（无会话时整页切到登录页） ───────────────────────────────────── */
function showLogin() {
  location.replace('/login');
}

/* ── 启动 ─────────────────────────────────────────────────────────────── */
async function boot() {
  initTheme();
  initParticles();
  initRail();
  initBar();
  setUnauthorizedHandler(() => showLogin());

  try {
    await api('/api/auth/me');
  } catch (err) {
    return showLogin();
  }

  initLamp();
  window.addEventListener('hashchange', () => paint());
  /* 视图里改完会写配置的动作（例如设置页保存人格）要走这条路重绘整页。
     原因：paint() 是在视图渲染**之前**发的那批请求，所以写入成功之后
     页面上的「配置版本」表还是旧的 —— 用户会点到上一次改动。
     这里先清缓存再重绘，保证拿到的是写入之后的数据。 */
  window.addEventListener('fm:reload-view', () => {
    invalidate();
    paint();
  });
  paint();
}

boot();
