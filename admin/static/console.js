/* ══════════════════════════════════════════════════════════════════════════
 * FM Console v4 · Core
 * 设计系统：design-system.css（tokens / 原语）+ console.css（shell / 页面）
 * 结构：工具 → API → Store → 主题 → UI 原语 → 表格 → 图表 → 快捷键 →
 *       命令面板 → 路由 → 外壳绑定 → 启动
 * 兼容：保留 v2/v3 页面使用的公共 API（页面文件在 console.js 之后加载）。
 * ════════════════════════════════════════════════════════════════════════ */

/* ── 0. 元信息 ─────────────────────────────────────────────────────────── */
const APP = {
  title: 'FM Console',
  version: '4.1.0',
  build: 'v4.1-r2-20261115',
  routeToken: 0,
  autoRefresh: false,
  user: null,
  lastLatency: 0,
  bootAt: Date.now(),
  navFilter: '',
  navPinned: false,
  env: { ok: null, latency: 0, at: 0 },
};

/* ── 1. 基础工具 ───────────────────────────────────────────────────────── */
const $ = (selector, root) => (root || document).querySelector(selector);
const $$ = (selector, root) => Array.prototype.slice.call((root || document).querySelectorAll(selector));

const ESCAPE_MAP = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' };
const esc = (value) => String(value == null ? '' : value).replace(/[&<>"']/g, (ch) => ESCAPE_MAP[ch]);
const escAttr = esc;

function icon(name, extraClass) {
  return '<svg class="ico' + (extraClass ? ' ' + extraClass : '') + '" aria-hidden="true"><use href="#i-' + escAttr(name || 'info') + '"/></svg>';
}

function clsx() {
  const out = [];
  Array.prototype.forEach.call(arguments, (arg) => {
    if (!arg) return;
    if (typeof arg === 'string') { out.push(arg); return; }
    if (Array.isArray(arg)) { arg.forEach((item) => { if (item) out.push(item); }); return; }
    if (typeof arg === 'object') {
      Object.keys(arg).forEach((key) => { if (arg[key]) out.push(key); });
    }
  });
  return out.join(' ');
}

const pad = (n) => (n < 10 ? '0' + n : '' + n);

function toMs(value) {
  if (value == null || value === '') return null;
  if (typeof value === 'number') return value > 1e12 ? value : value * 1000;
  if (value instanceof Date) return value.getTime();
  const text = String(value).trim();
  if (/^\d+(\.\d+)?$/.test(text)) {
    const num = Number(text);
    return num > 1e12 ? num : num * 1000;
  }
  const normalized = text.replace(' ', 'T');
  const parsed = Date.parse(normalized);
  if (!isNaN(parsed)) return parsed;
  const fallback = Date.parse(text);
  return isNaN(fallback) ? null : fallback;
}

function fmtTime(value, opts) {
  const ms = toMs(value);
  if (ms == null) return '';
  const d = new Date(ms);
  const o = opts || {};
  if (o.timeOnly) return pad(d.getHours()) + ':' + pad(d.getMinutes()) + ':' + pad(d.getSeconds());
  if (o.dateOnly) return d.getFullYear() + '-' + pad(d.getMonth() + 1) + '-' + pad(d.getDate());
  return d.getFullYear() + '-' + pad(d.getMonth() + 1) + '-' + pad(d.getDate()) + ' ' + pad(d.getHours()) + ':' + pad(d.getMinutes()) + ':' + pad(d.getSeconds());
}

function ago(value) {
  const ms = toMs(value);
  if (ms == null) return '';
  const diff = Date.now() - ms;
  if (diff < 0) {
    const future = -diff;
    if (future < 60000) return Math.round(future / 1000) + ' 秒后';
    if (future < 3600000) return Math.round(future / 60000) + ' 分钟后';
    if (future < 86400000) return Math.round(future / 3600000) + ' 小时后';
    return Math.round(future / 86400000) + ' 天后';
  }
  if (diff < 10000) return '刚刚';
  if (diff < 60000) return Math.round(diff / 1000) + ' 秒前';
  if (diff < 3600000) return Math.round(diff / 60000) + ' 分钟前';
  if (diff < 86400000) return Math.round(diff / 3600000) + ' 小时前';
  if (diff < 2592000000) return Math.round(diff / 86400000) + ' 天前';
  return Math.round(diff / 2592000000) + ' 个月前';
}

function fmtNum(value, digits) {
  if (value == null || value === '') return '—';
  const num = Number(value);
  if (!isFinite(num)) return String(value);
  if (digits != null) return num.toFixed(digits).replace(/\.0+$/, '');
  const abs = Math.abs(num);
  if (abs >= 1e8) return (num / 1e8).toFixed(2).replace(/\.?0+$/, '') + ' 亿';
  if (abs >= 1e4) return (num / 1e4).toFixed(abs >= 1e6 ? 0 : 1).replace(/\.0$/, '') + ' 万';
  return num.toLocaleString('zh-CN', { maximumFractionDigits: 2 });
}

function fmtPct(part, total, digits) {
  const p = Number(part);
  const t = Number(total);
  if (!isFinite(p) || !isFinite(t) || t === 0) return '—';
  return ((p / t) * 100).toFixed(digits == null ? 1 : digits) + '%';
}

function fmtBytes(value) {
  const num = Number(value);
  if (!isFinite(num) || num <= 0) return '—';
  const units = ['B', 'KB', 'MB', 'GB', 'TB'];
  let index = 0;
  let size = num;
  while (size >= 1024 && index < units.length - 1) { size /= 1024; index += 1; }
  return size.toFixed(size >= 100 || index === 0 ? 0 : 1) + ' ' + units[index];
}

function fmtDuration(ms) {
  const num = Number(ms);
  if (!isFinite(num)) return '—';
  if (num < 1000) return Math.round(num) + ' ms';
  const seconds = num / 1000;
  if (seconds < 60) return seconds.toFixed(seconds < 10 ? 2 : 1) + ' s';
  const minutes = Math.floor(seconds / 60);
  if (minutes < 60) return minutes + ' 分 ' + Math.round(seconds % 60) + ' 秒';
  const hours = Math.floor(minutes / 60);
  if (hours < 24) return hours + ' 小时 ' + (minutes % 60) + ' 分';
  return Math.floor(hours / 24) + ' 天 ' + (hours % 24) + ' 小时';
}

function truncate(text, max) {
  const value = String(text == null ? '' : text);
  const limit = max || 120;
  return value.length > limit ? value.slice(0, limit - 1) + '…' : value;
}

function prettyCell(value) {
  if (value == null || value === '') return '—';
  if (typeof value === 'object') return truncate(jsonSum(value), 160);
  if (typeof value === 'boolean') return value ? '是' : '否';
  return truncate(String(value), 300);
}

/* v2/v3 页面共用的数组/字段探测工具（原来定义在 console.js 页面段，现提升为公共 API） */
function pickArray(value, keys) {
  if (Array.isArray(value)) return value;
  if (!value || typeof value !== 'object') return [];
  const list = keys || ['items', 'rows', 'data', 'list', 'results', 'records', 'entries'];
  for (let i = 0; i < list.length; i += 1) {
    if (Array.isArray(value[list[i]])) return value[list[i]];
  }
  return [];
}

function firstDefined(row, keys) {
  if (!row || typeof row !== 'object') return undefined;
  for (let i = 0; i < (keys || []).length; i += 1) {
    const value = row[keys[i]];
    if (value !== undefined && value !== null && value !== '') return value;
  }
  return undefined;
}

function debounce(fn, wait) {
  let timer = 0;
  return function debounced() {
    const args = arguments;
    const context = this;
    clearTimeout(timer);
    timer = setTimeout(() => fn.apply(context, args), wait == null ? 220 : wait);
  };
}

function sleep(ms) { return new Promise((resolve) => setTimeout(resolve, ms)); }

function once(fn) {
  let called = false;
  return function wrapped() {
    if (called) return undefined;
    called = true;
    return fn.apply(this, arguments);
  };
}

function safeLocalGet(key, fallback) {
  try {
    const value = localStorage.getItem(key);
    return value == null ? fallback : value;
  } catch (err) { return fallback; }
}

function safeLocalSet(key, value) {
  try { localStorage.setItem(key, String(value)); } catch (err) { /* 隐私模式 */ }
}

function copyText(text) {
  const value = String(text == null ? '' : text);
  if (navigator.clipboard && navigator.clipboard.writeText) {
    return navigator.clipboard.writeText(value).catch(() => fallbackCopy(value));
  }
  return Promise.resolve(fallbackCopy(value));
}

function fallbackCopy(value) {
  const area = document.createElement('textarea');
  area.value = value;
  area.style.position = 'fixed';
  area.style.opacity = '0';
  document.body.appendChild(area);
  area.select();
  try { document.execCommand('copy'); } catch (err) { /* ignore */ }
  document.body.removeChild(area);
}

function go(hash) {
  const target = hash || '#/dashboard';
  if (location.hash === target) { route(); return; }
  location.hash = target;
}

function hrefOf(hash) { return hash; }

function jsonSum(value) {
  try { return JSON.stringify(value, null, 2) || ''; } catch (err) { return String(value); }
}

/* JSON 高亮 */
const JSON_TOKEN_RE = /("(?:\\.|[^"\\])*")(\s*:)?|\b(true|false|null)\b|(-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)/g;
function highlightJson(text) {
  return esc(text).replace(JSON_TOKEN_RE, (match, str, colon, bool, num) => {
    if (str) return '<span class="jv-' + (colon ? 'k' : 's') + '">' + str + '</span>' + (colon || '');
    if (bool) return '<span class="jv-b">' + bool + '</span>';
    if (num) return '<span class="jv-n">' + num + '</span>';
    return match;
  });
}

function jsonView(value, opts) {
  const options = opts || {};
  let text;
  try { text = JSON.stringify(value, null, 2); } catch (err) { text = String(value); }
  if (text == null) text = 'null';
  const id = options.id || 'jv-' + Math.random().toString(36).slice(2, 8);
  return '<div class="code-wrap" id="' + escAttr(id) + '">' +
    '<button class="code-copy code-tip" type="button" data-copy="' + escAttr(text.slice(0, 20000)) + '">复制</button>' +
    '<pre class="mono">' + highlightJson(text.slice(0, 40000)) + (text.length > 40000 ? '\n…（已截断）' : '') + '</pre></div>';
}

function codeBlock(text, opts) {
  const options = opts || {};
  const raw = String(text == null ? '' : text);
  return '<div class="code-wrap">' +
    '<button class="code-copy code-tip" type="button" data-copy="' + escAttr(raw.slice(0, 20000)) + '">复制</button>' +
    '<pre class="mono' + (options.wrap ? ' cell-wrap' : '') + '">' + esc(options.raw ? raw : raw) + '</pre></div>';
}

/* ── 2. API 层 ─────────────────────────────────────────────────────────── */
class ApiError extends Error {
  constructor(message, info) {
    super(message);
    const data = info || {};
    this.name = 'ApiError';
    this.status = data.status || 0;
    this.data = data.data || null;
    this.path = data.path || '';
    this.timeout = !!data.timeout;
  }
}

const API = {
  timeout: 22000,
  redirected: false,
  cache: new Map(),
};

function updateNetBadge(online, latencyMs) {
  APP.env.ok = !!online;
  if (online && latencyMs) APP.lastLatency = latencyMs;
  APP.env.at = Date.now();
  const pill = $('#env-pill');
  const dot = $('#env-pill-dot');
  const text = $('#env-text');
  if (pill) pill.setAttribute('data-tone', online ? 'ok' : 'bad');
  if (dot) dot.setAttribute('data-tone', online ? 'ok' : 'bad');
  if (text) text.textContent = online ? '在线' : '连接异常';
  const cardDot = $('#env-dot');
  if (cardDot) cardDot.setAttribute('data-tone', online ? 'ok' : 'bad');
  const footerLatency = $('#footer-latency');
  if (footerLatency) footerLatency.textContent = online && APP.lastLatency ? 'API ' + APP.lastLatency + ' ms' : '';
}

/* 环境心跳：用 /api/health 的往返时间做「环境延迟」，而不是最后一次慢接口耗时 */
async function pingEnv() {
  const started = (window.performance && performance.now) ? performance.now() : Date.now();
  try {
    await apiGet('/api/health', { timeout: 8000 });
    const ms = Math.round(((window.performance && performance.now) ? performance.now() : Date.now()) - started);
    APP.env.latency = ms;
    const latency = $('#env-latency');
    if (latency) latency.textContent = ms + 'ms';
    const sidebarLatency = $('#sidebar-latency');
    if (sidebarLatency) sidebarLatency.textContent = ms + ' ms';
    updateNetBadge(true, APP.lastLatency);
  } catch (err) {
    updateNetBadge(false, 0);
  }
}

function setSidebarStatus(tone, label) {
  const box = $('#sidebar-status-text');
  const dot = $('#env-dot');
  if (dot && tone) dot.setAttribute('data-tone', tone === 'ok' ? 'ok' : tone === 'bad' ? 'bad' : 'muted');
  if (box) box.textContent = label;
}

function redirectToLogin() {
  if (API.redirected) return;
  API.redirected = true;
  toast('会话已过期，正在跳转登录页…', 'warn', { timeout: 2400 });
  const next = '/console' + (location.hash || '');
  setTimeout(() => { location.replace('/login?next=' + encodeURIComponent(next)); }, 500);
}

/* ── 只读账号（is_admin=false）权限控制 ───────────────────────────────── */
const READONLY_WRITE_ALLOWLIST = ['/api/logout', '/api/rpc'];
const READONLY_TOAST_INTERVAL = 2500;
let lastReadonlyToastAt = 0;

function isAdmin() {
  return !!(APP.user && APP.user.is_admin === true);
}

function isReadonly() {
  return !!(APP.user && APP.user.is_admin === false);
}

function isWriteMethod(method) {
  return /^(POST|PUT|PATCH|DELETE)$/i.test(String(method || 'GET'));
}

function isReadonlyBlocked(path, method) {
  if (!isReadonly() || !isWriteMethod(method)) return false;
  const target = String(path || '');
  return !READONLY_WRITE_ALLOWLIST.some((prefix) => target.indexOf(prefix) === 0);
}

function toastReadonly(message) {
  const now = Date.now();
  if (now - lastReadonlyToastAt < READONLY_TOAST_INTERVAL) return;
  lastReadonlyToastAt = now;
  toast(message || '只读账号无权修改，请使用管理员令牌登录', 'warn', { title: '权限不足', timeout: 3200 });
}

async function apiFetch(path, opts) {
  const options = opts || {};
  const method = options.method || 'GET';
  const cacheKey = options.cache ? method + ' ' + path : '';
  if (cacheKey) {
    const hit = API.cache.get(cacheKey);
    if (hit && Date.now() - hit.at < options.cache) return hit.data;
  }
  if (isReadonlyBlocked(path, method)) {
    toastReadonly('只读账号无权修改（需要管理员），请求未发送');
    throw new ApiError('只读账号无权修改（需要管理员）', { status: 403, path: path, readonly: true });
  }
  const controller = typeof AbortController !== 'undefined' ? new AbortController() : null;
  const timeout = options.timeout || API.timeout;
  const timer = controller ? setTimeout(() => controller.abort(), timeout) : 0;
  const headers = Object.assign({ Accept: 'application/json' }, options.headers || {});
  let body;
  if (options.body !== undefined && options.body !== null) {
    headers['Content-Type'] = headers['Content-Type'] || 'application/json';
    body = typeof options.body === 'string' ? options.body : JSON.stringify(options.body);
  }
  const started = (window.performance && performance.now) ? performance.now() : Date.now();
  try {
    const response = await fetch(path, {
      method: method,
      credentials: 'same-origin',
      cache: 'no-store',
      headers: headers,
      body: body,
      signal: controller ? controller.signal : undefined,
    });
    const elapsed = Math.round(((window.performance && performance.now) ? performance.now() : Date.now()) - started);
    const text = await response.text();
    let data = null;
    if (text) {
      try { data = JSON.parse(text); } catch (err) { data = { raw: text }; }
    }
    if (response.status === 401) {
      redirectToLogin();
      throw new ApiError('未登录或会话已过期', { status: 401, data: data, path: path });
    }
    if (response.status === 403) {
      const write = isWriteMethod(method);
      if (write) toastReadonly('只读账号无权修改，请使用管理员令牌登录');
      updateNetBadge(true, elapsed);
      throw new ApiError(
        write ? '只读账号无权修改（需要管理员）' : '需要管理员权限（当前为只读账号，该接口仅管理员可用）',
        { status: 403, data: data, path: path, readonly: true },
      );
    }
    if (!response.ok) {
      const message = (data && (data.error || data.message)) || ('HTTP ' + response.status);
      updateNetBadge(true, elapsed);
      throw new ApiError(message, { status: response.status, data: data, path: path });
    }
    updateNetBadge(true, elapsed);
    if (cacheKey) API.cache.set(cacheKey, { at: Date.now(), data: data });
    return data;
  } catch (err) {
    if (err instanceof ApiError) throw err;
    if (err && err.name === 'AbortError') {
      updateNetBadge(false);
      throw new ApiError('请求超时（' + Math.round(timeout / 1000) + 's）：' + path, { status: 0, path: path, timeout: true });
    }
    updateNetBadge(false);
    throw new ApiError('网络请求失败：' + (err && err.message ? err.message : '未知错误'), { status: 0, path: path });
  } finally {
    if (timer) clearTimeout(timer);
  }
}

const apiGet = (path, opts) => apiFetch(path, Object.assign({ method: 'GET' }, opts || {}));
const apiPost = (path, body, opts) => apiFetch(path, Object.assign({ method: 'POST', body: body === undefined ? {} : body }, opts || {}));
const apiPut = (path, body, opts) => apiFetch(path, Object.assign({ method: 'PUT', body: body === undefined ? {} : body }, opts || {}));
const apiDelete = (path, opts) => apiFetch(path, Object.assign({ method: 'DELETE' }, opts || {}));

function unwrap(response) {
  if (response && typeof response === 'object' && !Array.isArray(response)) {
    if (response.ok === false && response.error) throw new ApiError(response.error, { data: response });
    if (Object.prototype.hasOwnProperty.call(response, 'data')) return response.data;
    if (Object.prototype.hasOwnProperty.call(response, 'items')) return response.items;
  }
  return response;
}

function qs(params) {
  const search = new URLSearchParams();
  Object.keys(params || {}).forEach((key) => {
    const value = params[key];
    if (value === undefined || value === null || value === '') return;
    search.set(key, String(value));
  });
  const text = search.toString();
  return text ? '?' + text : '';
}

/* 探测新接口：404/501/超时 视为「未就绪」，其它错误原样返回 */
async function probeApi(paths, opts) {
  const options = opts || {};
  const list = Array.isArray(paths) ? paths : [paths];
  const attempts = [];
  for (let i = 0; i < list.length; i += 1) {
    try {
      const response = await apiFetch(list[i], options);
      return { ok: true, data: unwrap(response), raw: response, path: list[i], attempts: attempts };
    } catch (err) {
      if (err.status === 401) throw err;
      attempts.push({ path: list[i], error: err });
      if (err.status !== 404 && err.status !== 501 && err.status !== 0 && err.status !== 502 && err.status !== 503) break;
    }
  }
  const lastError = attempts.length ? attempts[attempts.length - 1].error : null;
  const pending = !lastError || lastError.status === 404 || lastError.status === 501 || lastError.status === 503 || lastError.status === 0;
  /* 展示用错误：跳过「缺少参数 / 未知接口」这类降级噪声，优先取第一次有业务含义的错误（如 404 没有该成员） */
  const meaningful = attempts.filter((item) => {
    const message = String((item.error && item.error.message) || '');
    return !/缺少|未知的 v2 接口|not found|Not Found/i.test(message);
  });
  const displayError = pending
    ? ((attempts[0] && attempts[0].error) || lastError)
    : ((meaningful[0] && meaningful[0].error) || lastError);
  return { ok: false, pending: pending, error: displayError, attempts: attempts, path: list[list.length - 1] };
}

/* ── 3. 全局状态 ──────────────────────────────────────────────────────── */
const Store = {
  groups: null,
  groupsAt: 0,
  humanlike: null,
  humanlikeAt: 0,
  me: null,
  actions: null,
  triggers: null,
  triggersAt: 0,
  overview: null,
  overviewAt: 0,
  health: null,
  healthAt: 0,
};

async function getGroups(force) {
  const now = Date.now();
  if (!force && Store.groups && now - Store.groupsAt < 30000) return Store.groups;
  const data = unwrap(await apiGet('/api/v2/groups', { cache: 20000 }));
  Store.groups = (data && data.groups) || [];
  Store.groupsAt = now;
  return Store.groups;
}

async function getHumanlike(force) {
  const now = Date.now();
  if (!force && Store.humanlike && now - Store.humanlikeAt < 15000) return Store.humanlike;
  const data = unwrap(await apiGet('/api/v2/humanlike', { cache: 10000 }));
  Store.humanlike = data || { scopes: [], proactive: [], files: [] };
  Store.humanlikeAt = now;
  return Store.humanlike;
}

async function getTriggerScopes(force) {
  const now = Date.now();
  if (!force && Store.triggers && now - Store.triggersAt < 20000) return Store.triggers;
  const response = await apiPost('/api/rpc', { method: 'config.trigger', params: { action: 'list' } });
  const data = response && response.ok !== undefined ? unwrap(response) : response;
  Store.triggers = Array.isArray(data) ? data : [];
  Store.triggersAt = now;
  return Store.triggers;
}

function groupName(groups, id) {
  const list = groups || Store.groups || [];
  const found = list.find((item) => String(item.group_id) === String(id));
  return (found && (found.name || found.display_name)) || String(id);
}

/* ── 4. 主题 ───────────────────────────────────────────────────────────── */
const Theme = {
  key: 'fm-console-theme',
  listeners: [],
  current() {
    return document.documentElement.getAttribute('data-theme') === 'light' ? 'light' : 'dark';
  },
  set(mode, opts) {
    const value = mode === 'light' ? 'light' : 'dark';
    const options = opts || {};
    document.documentElement.setAttribute('data-theme', value);
    const meta = document.querySelector('meta[name="theme-color"]');
    if (meta) meta.setAttribute('content', value === 'light' ? '#f2f5fa' : '#06080e');
    safeLocalSet(this.key, value);
    this.listeners.forEach((fn) => { try { fn(value); } catch (err) { /* ignore */ } });
    if (!options.silent) toast('已切换到' + (value === 'light' ? '浅色' : '深色') + '主题', 'ok', { timeout: 1600 });
  },
  toggle(silent) { this.set(this.current() === 'light' ? 'dark' : 'light', { silent: silent }); },
  toggleSilent() { this.set(this.current() === 'light' ? 'dark' : 'light', { silent: true }); },
  onChange(fn) { if (typeof fn === 'function') this.listeners.push(fn); },
  init() {
    const saved = safeLocalGet(this.key, null);
    if (saved === 'light' || saved === 'dark') {
      document.documentElement.setAttribute('data-theme', saved);
    }
    const observer = new MutationObserver(() => {
      this.listeners.forEach((fn) => { try { fn(this.current()); } catch (err) { /* ignore */ } });
    });
    observer.observe(document.documentElement, { attributes: true, attributeFilter: ['data-theme'] });
    this.onChange(() => { refreshChartTheme(); });
    window.FM = window.FM || {};
    window.FM.theme = this;
  },
};

/* ── 5. UI 原语 ───────────────────────────────────────────────────────── */
const TOAST_ICONS = { info: 'info', ok: 'check-circle', warn: 'alert', bad: 'x-circle' };

function toast(message, kind, opts) {
  const options = opts || {};
  const type = TOAST_ICONS[kind] ? kind : 'info';
  const root = $('#toasts');
  if (!root) return { close() {} };
  const node = document.createElement('div');
  node.className = 'toast ' + type;
  node.setAttribute('role', type === 'bad' ? 'alert' : 'status');
  const title = options.title ? '<div class="toast-title">' + esc(options.title) + '</div>' : '';
  node.innerHTML = icon(TOAST_ICONS[type]) +
    '<div class="toast-main">' + title + '<div class="toast-msg">' + esc(message) + '</div></div>' +
    '<button class="toast-close" type="button" aria-label="关闭">' + icon('close') + '</button>';
  root.appendChild(node);
  const timeout = options.timeout || (type === 'bad' ? 6200 : 3600);
  let closed = false;
  function close() {
    if (closed) return;
    closed = true;
    node.classList.add('leaving');
    setTimeout(() => node.remove(), 200);
  }
  node.querySelector('.toast-close').addEventListener('click', close);
  if (timeout > 0) setTimeout(close, timeout);
  while (root.children.length > 5) root.removeChild(root.firstChild);
  return { close: close, el: node };
}

function openModal(config) {
  const cfg = config || {};
  const root = $('#modal-root');
  const overlay = document.createElement('div');
  overlay.className = 'overlay';
  overlay.innerHTML =
    '<div class="modal" role="dialog" aria-modal="true" aria-label="' + escAttr(cfg.ariaLabel || cfg.title || '对话框') + '">' +
      '<div class="modal-head">' +
        '<div class="modal-icon ' + (cfg.danger ? 'danger' : '') + '">' + icon(cfg.icon || (cfg.danger ? 'alert' : 'info')) + '</div>' +
        '<div style="flex:1;min-width:0">' +
          '<div class="modal-title">' + esc(cfg.title || '确认') + '</div>' +
          (cfg.desc ? '<div class="modal-desc">' + esc(cfg.desc) + '</div>' : '') +
        '</div>' +
      '</div>' +
      '<div class="modal-body" id="modal-body"></div>' +
      '<div class="modal-foot" id="modal-foot"></div>' +
    '</div>';
  root.appendChild(overlay);
  const body = $('#modal-body', overlay);
  const foot = $('#modal-foot', overlay);
  if (cfg.bodyHtml) body.innerHTML = cfg.bodyHtml;
  else body.remove();

  let closed = false;
  const api = { el: overlay, body: body, foot: foot, close: null };
  api.close = function close(result) {
    if (closed) return;
    closed = true;
    document.removeEventListener('keydown', onKey);
    overlay.remove();
    if (cfg.onClose) cfg.onClose(result);
  };

  function onKey(event) {
    if (event.key === 'Escape') { event.stopPropagation(); api.close(null); }
  }
  document.addEventListener('keydown', onKey);
  overlay.addEventListener('mousedown', (event) => {
    if (event.target === overlay && cfg.dismissible !== false) api.close(null);
  });

  (cfg.buttons || [{ label: '关闭' }]).forEach((button) => {
    const el = document.createElement('button');
    el.type = 'button';
    el.className = 'btn ' + (button.variant || 'btn-ghost');
    if (button.id) el.id = button.id;
    el.innerHTML = (button.icon ? icon(button.icon) : '') + '<span>' + esc(button.label) + '</span>';
    if (button.disabled) el.disabled = true;
    el.addEventListener('click', () => {
      if (button.onClick) button.onClick(api, el);
      else api.close(button.value);
    });
    if (button.note) {
      const note = document.createElement('span');
      note.className = 'danger-note';
      note.textContent = button.note;
      foot.appendChild(note);
    }
    foot.appendChild(el);
  });

  if (cfg.onMount) cfg.onMount(api);
  return api;
}

function confirmAction(config) {
  const cfg = config || {};
  return new Promise((resolve) => {
    let settled = false;
    let outcome = false;
    const finish = (value) => { if (!settled) { settled = true; resolve(!!value); } };
    const needText = cfg.requireText || '';
    const bodyHtml = needText
      ? '<div class="field"><label class="field-label">输入 <code class="inline">' + esc(needText) + '</code> 以继续</label>' +
        '<input class="input" id="confirm-text" autocomplete="off" placeholder="' + escAttr(needText) + '"></div>'
      : (cfg.extraHtml || '');
    const modal = openModal({
      title: cfg.title || '确认操作',
      desc: cfg.detail || '',
      danger: !!cfg.danger,
      icon: cfg.danger ? 'alert' : 'info',
      bodyHtml: bodyHtml,
      dismissible: true,
      onClose: () => finish(outcome),
      buttons: [
        { label: cfg.cancelLabel || '取消', variant: 'btn-ghost', onClick: (m) => { outcome = false; m.close(); } },
        {
          label: cfg.confirmLabel || '确认',
          variant: cfg.danger ? 'btn-danger-solid' : 'btn-solid',
          onClick: (m, btn) => {
            if (needText) {
              const input = $('#confirm-text', m.el);
              if (!input || input.value.trim() !== needText) {
                if (input) {
                  input.focus();
                  input.classList.add('input-error');
                  setTimeout(() => input.classList.remove('input-error'), 900);
                }
                toast('输入不匹配，请按提示输入', 'warn');
                return;
              }
            }
            if (btn) btn.disabled = true;
            outcome = true;
            m.close();
          },
        },
      ],
    });
    if (needText) {
      const input = $('#confirm-text', modal.el);
      if (input) setTimeout(() => input.focus(), 30);
    } else if (cfg.autoConfirm !== false) {
      const first = modal.foot.querySelector('.btn-solid,.btn-danger-solid');
      if (first) setTimeout(() => first.focus(), 30);
    }
  });
}

/* 抽屉 */
const Drawer = {
  raw: null,
  open(config) {
    const cfg = config || {};
    const drawer = $('#drawer');
    if (!drawer) return;
    const eyebrow = $('#drawer-eyebrow');
    if (eyebrow) eyebrow.innerHTML = cfg.eyebrow ? esc(cfg.eyebrow) : '';
    $('#drawer-title').innerHTML = cfg.title || '详情';
    $('#drawer-sub').innerHTML = cfg.sub || '';
    $('#drawer-body').innerHTML = cfg.body || '';
    const foot = $('#drawer-foot');
    if (cfg.foot) { foot.innerHTML = cfg.foot; foot.hidden = false; } else { foot.innerHTML = ''; foot.hidden = true; }
    Drawer.raw = cfg.raw === undefined ? null : cfg.raw;
    const copyBtn = $('#drawer-copy');
    if (copyBtn) copyBtn.style.display = Drawer.raw == null ? 'none' : '';
    drawer.classList.toggle('wide', !!cfg.wide);
    drawer.hidden = false;
    document.body.style.overflow = 'hidden';
    if (cfg.onMount) cfg.onMount($('#drawer-body'), drawer);
    const closeBtn = drawer.querySelector('[data-drawer-close]');
    if (closeBtn) closeBtn.focus({ preventScroll: true });
    const bodyEl = $('#drawer-body');
    if (bodyEl) bodyEl.scrollTop = 0;
  },
  close() {
    const drawer = $('#drawer');
    if (!drawer || drawer.hidden) return;
    drawer.hidden = true;
    document.body.style.overflow = '';
    $('#drawer-body').innerHTML = '';
    Drawer.raw = null;
  },
  isOpen() {
    const drawer = $('#drawer');
    return !!drawer && !drawer.hidden;
  },
  json(title, value, sub) {
    Drawer.open({
      title: title,
      sub: sub || '',
      eyebrow: 'JSON',
      body: jsonView(value),
      raw: value,
      wide: true,
    });
  },
};

function openJsonDrawer(title, value, sub) { Drawer.json(title, value, sub); }
function rowJsonDrawer(title, row, sub) {
  return function open() { openJsonDrawer(title, row, sub); };
}

/* 全局委托：复制 / 抽屉关闭 / 跳转 / JSON 抽屉 */
document.addEventListener('click', (event) => {
  const copyBtn = event.target.closest('[data-copy], #drawer-copy');
  if (copyBtn) {
    let text = copyBtn.getAttribute('data-copy');
    if (text == null && copyBtn.id === 'drawer-copy') text = Drawer.raw == null ? '' : JSON.stringify(Drawer.raw, null, 2);
    if (text == null) return;
    copyText(text).then(() => toast('已复制到剪贴板', 'ok', { timeout: 1500 })).catch(() => toast('复制失败', 'bad'));
    return;
  }
  const closeBtn = event.target.closest('[data-drawer-close]');
  if (closeBtn) { Drawer.close(); return; }
  const jump = event.target.closest('[data-goto]');
  if (jump) { event.preventDefault(); go(jump.getAttribute('data-goto')); return; }
  const jsonBtn = event.target.closest('[data-json-drawer]');
  if (jsonBtn) {
    event.preventDefault();
    const raw = jsonBtn.getAttribute('data-json-drawer');
    try { openJsonDrawer(jsonBtn.getAttribute('data-json-title') || '原始响应', JSON.parse(raw)); }
    catch (err) { openJsonDrawer('原始响应', raw); }
  }
});

/* ── 6. 通用渲染件 ────────────────────────────────────────────────────── */
function pageHead(config) {
  const cfg = config || {};
  return '<div class="page-head">' +
    '<div class="page-head-main">' +
      (cfg.eyebrow ? '<div class="page-eyebrow">' + (cfg.eyebrowIcon ? icon(cfg.eyebrowIcon) : '') + esc(cfg.eyebrow) + '</div>' : '') +
      '<h1 class="page-title">' + (cfg.icon ? icon(cfg.icon) : '') + esc(cfg.title || '') + '</h1>' +
      (cfg.sub ? '<div class="page-sub">' + (cfg.subHtml ? cfg.sub : esc(cfg.sub)) + '</div>' : '') +
    '</div>' +
    (cfg.actions && cfg.actions.length ? '<div class="page-actions">' + cfg.actions.join('') + '</div>' : '') +
    '</div>';
}

function card(config, bodyHtml) {
  const cfg = config || {};
  const head = (cfg.title || cfg.headActions || cfg.sub)
    ? '<div class="card-head"><h3>' + (cfg.icon ? icon(cfg.icon) : '') + esc(cfg.title || '') +
      (cfg.sub ? '<span class="section-sub">' + esc(cfg.sub) + '</span>' : '') + '</h3>' +
      (cfg.headActions ? '<div class="card-head-actions">' + cfg.headActions.join('') + '</div>' : '') + '</div>'
    : '';
  const body = bodyHtml == null || bodyHtml === '' ? '' : '<div class="card-body' + (cfg.flush ? ' flush' : '') + '">' + bodyHtml + '</div>';
  return '<section class="card ' + (cfg.className || '') + '">' + head + body + (cfg.foot || '') + '</section>';
}

function kpiGrid(items, opts) {
  const options = opts || {};
  const cards = (items || []).map((item) => {
    const tone = item.tone ? ' tone-' + item.tone : '';
    const inner =
      '<div class="kpi-top">' + (item.icon ? icon(item.icon) : '') + '<span>' + esc(item.label) + '</span>' +
      (item.badge || '') + '</div>' +
      '<div class="kpi-value">' + (item.valueHtml == null ? esc(item.value == null ? '-' : item.value) : item.valueHtml) +
      (item.unit ? '<small>' + esc(item.unit) + '</small>' : '') + '</div>' +
      (item.hint ? '<div class="kpi-hint">' + item.hint + '</div>' : '');
    const className = 'kpi' + tone + (item.clickable ? ' clickable' : '');
    if (item.href) return '<a class="' + className + '" href="' + escAttr(item.href) + '">' + inner + '</a>';
    return '<div class="' + className + '">' + inner + '</div>';
  });
  return '<div class="kpis' + (options.cols ? ' cols-' + options.cols : '') + '">' + cards.join('') + '</div>';
}

function statusTag(ok, okText, badText) {
  return ok
    ? '<span class="tag tag-ok">' + icon('check') + esc(okText || '正常') + '</span>'
    : '<span class="tag tag-bad">' + icon('alert') + esc(badText || '异常') + '</span>';
}

function emptyState(config) {
  const cfg = config || {};
  return '<div class="empty">' +
    '<div class="empty-ico">' + icon(cfg.icon || 'inbox') + '</div>' +
    '<b>' + esc(cfg.title || '暂无数据') + '</b>' +
    (cfg.hint ? '<p>' + esc(cfg.hint) + '</p>' : '') +
    (cfg.actions && cfg.actions.length ? '<div class="empty-actions">' + cfg.actions.join('') + '</div>' : '') +
    '</div>';
}

function errorState(err, retryId) {
  const message = err && err.message ? err.message : String(err || '未知错误');
  const status = err && err.status ? 'HTTP ' + err.status + ' · ' : '';
  return '<div class="error-box">' + icon('alert') +
    '<div style="flex:1;min-width:0"><b>加载失败</b><p>' + esc(status + message) + '</p>' +
    '<div class="err-actions">' +
    (retryId ? '<button class="btn btn-sm btn-ghost" id="' + escAttr(retryId) + '" type="button">' + icon('refresh') + '重试</button>' : '') +
    '<button class="btn btn-sm btn-ghost" data-goto="#/browse" type="button">' + icon('database') + '数据浏览器调试</button>' +
    '</div></div></div>';
}

function notice(kind, html) {
  const iconName = kind === 'warn' ? 'alert' : kind === 'bad' ? 'x-circle' : kind === 'ok' ? 'check-circle' : 'info';
  return '<div class="notice ' + (kind || '') + '">' + icon(iconName) + '<div style="flex:1;min-width:0">' + html + '</div></div>';
}

function pendingState(config) {
  const cfg = config || {};
  return '<div class="empty">' +
    '<div class="empty-ico">' + icon('clock') + '</div>' +
    '<b>' + esc(cfg.title || '接口尚未就绪') + '</b>' +
    '<p>' + esc(cfg.hint || '后端接口还在接入中，页面已按占位渲染，接入后无需改前端即可生效。') + '</p>' +
    (cfg.path ? '<p class="mono muted-2">' + esc(cfg.path) + '</p>' : '') +
    '<div class="empty-actions">' +
    '<button class="btn btn-sm btn-ghost" data-retry="1" type="button">' + icon('refresh') + '重试</button>' +
    (cfg.raw ? '<button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(cfg.raw).slice(0, 4000)) + '" type="button">' + icon('eye') + '查看原始响应</button>' : '') +
    '</div></div>';
}

/* ── 7. 数据表 ────────────────────────────────────────────────────────── */
function renderTable(container, config) {
  const cfg = config || {};
  const columns = cfg.columns || [];
  const rows = cfg.rows || [];
  if (!container) return;
  if (!rows.length) {
    container.innerHTML = emptyState(cfg.empty || {});
    return;
  }
  const expandable = typeof cfg.detail === 'function';
  const showIndex = !!cfg.index || expandable;
  const stateKey = cfg.stateKey || '';
  const savedState = stateKey ? readTableState(stateKey) : null;
  const hiddenCols = new Set((savedState && savedState.hidden) || []);
  const sortState = savedState && savedState.sort ? savedState.sort : null;

  const visible = columns.filter((col) => !hiddenCols.has(String(col.key || col.label)));
  const sorter = (cfg.sorters || {});
  let currentSort = sortState;
  const applySort = (list) => {
    if (!currentSort) return list;
    const key = currentSort.key;
    const dir = currentSort.dir === 'desc' ? -1 : 1;
    const getter = sorter[key] || ((row) => row[key]);
    return list.slice().sort((a, b) => {
      const va = getter(a);
      const vb = getter(b);
      if (va == null && vb == null) return 0;
      if (va == null) return 1;
      if (vb == null) return -1;
      if (typeof va === 'number' && typeof vb === 'number') return (va - vb) * dir;
      return String(va).localeCompare(String(vb), 'zh-CN') * dir;
    });
  };
  let sorted = applySort(rows);

  const draw = () => {
    sorted = applySort(rows);
    const head = [];
    if (showIndex) head.push('<th style="width:34px"></th>');
    visible.forEach((col) => {
      const isSorted = currentSort && String(currentSort.key) === String(col.key);
      const sortBtn = cfg.sortable !== false
        ? '<span class="th-sort" role="button" tabindex="0" aria-label="按 ' + escAttr(String(col.label)) + ' 排序" data-sort-key="' + escAttr(String(col.key)) + '">' + esc(col.label) + (isSorted ? icon(currentSort.dir === 'desc' ? 'arrow-down' : 'arrow-up') : '<span style="width:0"></span>') + '</span>'
        : esc(col.label);
      const style = col.width ? ' style="width:' + escAttr(col.width) + '"' : '';
      const ariaSort = isSorted ? (currentSort.dir === 'desc' ? 'descending' : 'ascending') : 'none';
      head.push('<th' + style + ' aria-sort="' + ariaSort + '" class="' + clsx({ num: col.num, mono: col.mono, 'hide-sm': col.hideSm, sorted: isSorted }) + '">' + sortBtn + '</th>');
    });
    const colCount = visible.length + (showIndex ? 1 : 0);
    const body = sorted.map((row, index) => {
      const cells = [];
      if (showIndex) {
        cells.push('<td class="nowrap">' + (expandable ? '<span class="row-caret">' + icon('chevron-right') + '</span>' : '<span class="idx">' + (index + 1) + '</span>') + '</td>');
      }
      visible.forEach((col) => {
        let cell;
        if (typeof col.render === 'function') cell = col.render(row, index);
        else cell = '<span class="cell-truncate">' + prettyCell(row[col.key]) + '</span>';
        cells.push('<td class="' + clsx({ num: col.num, mono: col.mono, 'hide-sm': col.hideSm }) + '">' + cell + '</td>');
      });
      const rowId = cfg.rowId ? cfg.rowId(row, index) : index;
      const clickable = expandable || typeof cfg.onRow === 'function';
      const main = '<tr class="dt-row' + (clickable ? ' clickable' : '') + '" data-row="' + index + '" data-row-id="' + escAttr(rowId) + '" tabindex="0"' +
        (expandable ? ' aria-expanded="false"' : '') + '>' + cells.join('') + '</tr>';
      if (!expandable) return main;
      return main + '<tr class="dt-detail" data-detail="' + index + '" hidden><td colspan="' + colCount + '">' +
        '<div class="dt-detail-body">' + (cfg.detail(row, index) || jsonView(row)) + '</div></td></tr>';
    }).join('');
    const stickyFirst = visible.length >= 8;
    container.innerHTML = '<div class="table-wrap' + (cfg.flush ? ' flush' : '') + (stickyFirst ? ' sticky-first' : '') + '"><table class="table"><thead><tr>' + head.join('') +
      '</tr></thead><tbody>' + body + '</tbody></table></div>';

    container.querySelectorAll('.th-sort').forEach((node) => {
      node.addEventListener('keydown', (event) => {
        if (event.key !== 'Enter' && event.key !== ' ') return;
        event.preventDefault();
        node.click();
      });
    });

    const toggleRow = (tr) => {
      const index = Number(tr.getAttribute('data-row'));
      const detail = container.querySelector('tr.dt-detail[data-detail="' + index + '"]');
      if (!detail) return;
      const open = detail.hidden;
      detail.hidden = !open;
      tr.classList.toggle('open', open);
      tr.setAttribute('aria-expanded', open ? 'true' : 'false');
      if (open && cfg.onExpand) cfg.onExpand(sorted[index], detail, index);
    };

    if (cfg.onRow) {
      container.onclick = (event) => {
        const tr = event.target.closest('tr.dt-row');
        if (!tr || !container.contains(tr)) return;
        if (event.target.closest('a,button,input,select,textarea')) return;
        const index = Number(tr.getAttribute('data-row'));
        cfg.onRow(sorted[index], tr, index);
      };
    }
    if (expandable) {
      container.onclick = (event) => {
        const sortHead = event.target.closest('.th-sort');
        if (sortHead) {
          const key = sortHead.getAttribute('data-sort-key');
          if (currentSort && String(currentSort.key) === String(key)) {
            currentSort = currentSort.dir === 'asc' ? { key: key, dir: 'desc' } : null;
          } else {
            currentSort = { key: key, dir: 'asc' };
          }
          if (stateKey) writeTableState(stateKey, { hidden: Array.from(hiddenCols), sort: currentSort });
          draw();
          return;
        }
        const tr = event.target.closest('tr.dt-row');
        if (!tr || !container.contains(tr)) return;
        if (event.target.closest('a,button,input,select,textarea')) return;
        const index = Number(tr.getAttribute('data-row'));
        toggleRow(tr);
        if (cfg.onRow) cfg.onRow(sorted[index], tr, index);
      };
      container.onkeydown = (event) => {
        if (event.key !== 'Enter' && event.key !== ' ') return;
        const tr = event.target.closest('tr.dt-row');
        if (!tr) return;
        event.preventDefault();
        toggleRow(tr);
      };
    } else {
      container.onclick = (event) => {
        const sortHead = event.target.closest('.th-sort');
        if (sortHead) {
          const key = sortHead.getAttribute('data-sort-key');
          if (currentSort && String(currentSort.key) === String(key)) {
            currentSort = currentSort.dir === 'asc' ? { key: key, dir: 'desc' } : null;
          } else {
            currentSort = { key: key, dir: 'asc' };
          }
          if (stateKey) writeTableState(stateKey, { hidden: Array.from(hiddenCols), sort: currentSort });
          draw();
          return;
        }
        if (!cfg.onRow) return;
        const tr = event.target.closest('tr.dt-row');
        if (!tr || !container.contains(tr)) return;
        if (event.target.closest('a,button,input,select,textarea')) return;
        const index = Number(tr.getAttribute('data-row'));
        cfg.onRow(sorted[index], tr, index);
      };
    }
  };
  draw();
  return {
    redraw: draw,
    setHidden(nextHidden) {
      hiddenCols.clear();
      (nextHidden || []).forEach((key) => hiddenCols.add(String(key)));
      if (stateKey) writeTableState(stateKey, { hidden: Array.from(hiddenCols), sort: currentSort });
      draw();
    },
  };
}

function readTableState(key) {
  try {
    const raw = safeLocalGet('fm-console-table-' + key, '');
    return raw ? JSON.parse(raw) : null;
  } catch (err) { return null; }
}

function writeTableState(key, value) {
  safeLocalSet('fm-console-table-' + key, JSON.stringify(value));
}

function pagerBar(config) {
  const cfg = config || {};
  const offset = Number(cfg.offset || 0);
  const limit = Number(cfg.limit || 50);
  const total = Number(cfg.total || 0);
  const from = total === 0 ? 0 : offset + 1;
  const to = Math.min(offset + limit, total);
  const hasPrev = offset > 0;
  const hasNext = offset + limit < total;
  return '<div class="pager">' +
    '<span class="pager-info">' + fmtNum(from) + '–' + fmtNum(to) + ' / ' + fmtNum(total) + ' ' + esc(cfg.unit || '行') + '</span>' +
    '<span class="pager-btns">' +
      '<button class="btn btn-sm btn-ghost" data-page="prev" type="button"' + (hasPrev ? '' : ' disabled') + '>' + icon('arrow-left') + '上一页</button>' +
      (cfg.jump === false ? '' : '<input class="input input-sm mono" data-page="jump" type="number" min="1" value="' + Math.floor(offset / limit + 1) + '" aria-label="页码">') +
      '<button class="btn btn-sm btn-ghost" data-page="next" type="button"' + (hasNext ? '' : ' disabled') + '>下一页' + icon('chevron-right') + '</button>' +
    '</span></div>';
}

function tabsHtml(items, activeId) {
  return '<div class="tabs" role="tablist">' + (items || []).map((item) =>
    '<button class="tab ' + (item.id === activeId ? 'active' : '') + '" data-tab="' + escAttr(item.id) + '" role="tab" aria-selected="' + (item.id === activeId) + '" type="button">' +
    (item.icon ? icon(item.icon) : '') + esc(item.label) +
    (item.count != null ? '<span class="badge-count">' + fmtNum(item.count) + '</span>' : '') +
    '</button>').join('') + '</div>';
}

function skeletonPage(kind) {
  if (kind === 'table') {
    return '<div class="sk sk-title"></div>' +
      '<div class="sk-row"><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div></div>' +
      '<div class="sk sk-block" style="height:110px"></div>' +
      '<div class="sk-table">' + Array(6).fill('<div class="sk sk-line"></div>').join('') + '</div>';
  }
  if (kind === 'detail') {
    return '<div class="sk sk-title"></div><div class="sk-row"><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div></div>' +
      '<div class="sk sk-block"></div><div class="sk sk-block" style="height:150px"></div>';
  }
  if (kind === 'chart') {
    return '<div class="sk sk-title"></div><div class="sk-row"><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div></div>' +
      '<div class="sk sk-block" style="height:300px"></div>';
  }
  return '<div class="sk sk-title"></div><div class="sk sk-block"></div><div class="sk sk-block" style="height:150px"></div>';
}

function showViewError(root, err, retry) {
  root.innerHTML = pageHead({ title: '加载失败', icon: 'alert' }) + errorState(err, 'view-retry');
  const btn = $('#view-retry', root);
  if (btn) btn.addEventListener('click', () => { if (retry) retry(); else route(); });
}

/* ── 8. 图表 ──────────────────────────────────────────────────────────── */
const CHARTS = [];
let CHART_COLORS = ['#7c9dff', '#b18cff', '#3ddc97', '#fbbf4e', '#fb7185', '#56c8f5', '#f472b6', '#a3e635', '#fb923c', '#22d3ee'];
let chartLoader = null;

function ensureCharts() {
  if (window.echarts) return Promise.resolve(true);
  if (chartLoader) return chartLoader;
  chartLoader = new Promise((resolve) => {
    const script = document.createElement('script');
    script.src = '/static/echarts.min.js';
    script.async = true;
    script.onload = () => { resolve(!!window.echarts); applyChartTheme(); };
    script.onerror = () => resolve(false);
    document.head.appendChild(script);
  });
  return chartLoader;
}

function chartFont() { return getComputedStyle(document.body).fontFamily || 'sans-serif'; }

function cssVar(name, fallback) {
  try {
    const value = getComputedStyle(document.documentElement).getPropertyValue(name).trim();
    return value || fallback;
  } catch (err) { return fallback; }
}

function applyChartTheme() {
  CHART_COLORS = [
    cssVar('--brand-1', '#7c9dff'),
    cssVar('--purple', '#b18cff'),
    cssVar('--ok', '#3ddc97'),
    cssVar('--warn', '#fbbf4e'),
    cssVar('--bad', '#fb7185'),
    cssVar('--info', '#56c8f5'),
    '#f472b6', '#a3e635', '#fb923c', '#22d3ee',
  ];
}

function refreshChartTheme() {
  applyChartTheme();
  CHARTS.forEach((chart) => {
    try {
      const option = chart.getOption();
      chart.setOption(chartBase({}), { replace: false, notMerge: false });
      void option;
    } catch (err) { /* ignore */ }
  });
}

function chartBase(overrides) {
  const text2 = cssVar('--text-2', '#a9b4c6');
  const text3 = cssVar('--text-3', '#748094');
  const line = cssVar('--line-2', 'rgba(255,255,255,.11)');
  const surface = cssVar('--surface-1', '#0e131d');
  const text1 = cssVar('--text-1', '#e9eef8');
  const base = {
    backgroundColor: 'transparent',
    color: CHART_COLORS,
    textStyle: { color: text3, fontFamily: chartFont(), fontSize: 11 },
    animationDuration: 520,
    animationEasing: 'cubicOut',
    tooltip: {
      trigger: 'axis',
      backgroundColor: surface,
      borderColor: line,
      borderWidth: 1,
      padding: [9, 12],
      textStyle: { color: text1, fontSize: 11.5, fontFamily: chartFont() },
      axisPointer: { type: 'line', lineStyle: { color: line, type: 'dashed' } },
      extraCssText: 'box-shadow:0 18px 44px -22px rgba(0,0,0,.85);border-radius:10px;backdrop-filter:blur(6px);',
    },
    grid: { left: 6, right: 14, top: 26, bottom: 4, containLabel: true },
    legend: {
      top: 0, right: 0, itemWidth: 8, itemHeight: 8, itemGap: 14,
      textStyle: { color: text2, fontSize: 11 }, icon: 'roundRect',
    },
    xAxis: {
      type: 'category',
      axisLine: { lineStyle: { color: line } },
      axisTick: { show: false },
      axisLabel: { color: text3, fontSize: 10.5, hideOverlap: true },
      splitLine: { show: false },
    },
    yAxis: {
      type: 'value',
      axisLine: { show: false },
      axisTick: { show: false },
      axisLabel: { color: text3, fontSize: 10.5 },
      splitLine: { lineStyle: { color: line, type: 'dashed', opacity: .55 } },
    },
  };
  const patch = overrides || {};
  const merged = Object.assign({}, base, patch);
  ['grid', 'tooltip', 'legend', 'xAxis', 'yAxis'].forEach((key) => {
    if (!patch[key]) return;
    if (Array.isArray(patch[key])) { merged[key] = patch[key]; return; }
    merged[key] = Object.assign({}, base[key] || {}, patch[key]);
  });
  return merged;
}

function chartFallbackHtml(items, unitLabel) {
  const rows = (items || []).slice(0, 12);
  if (!rows.length) return '<div class="empty" style="padding:20px"><b>暂无图表数据</b></div>';
  const max = Math.max.apply(null, rows.map((r) => Number(r.value) || 0)) || 1;
  return '<div class="chart-fallback">' + rows.map((row) =>
    '<div class="cf-row"><span class="cf-label" title="' + escAttr(row.label) + '">' + esc(row.label) + '</span>' +
    '<span class="cf-bar" style="width:' + Math.max(2, Math.round((Number(row.value) || 0) / max * 62)) + '%"></span>' +
    '<span class="cf-value">' + fmtNum(row.value) + esc(unitLabel || '') + '</span></div>').join('') +
    '<div class="muted-2 text-xs">（图表组件未能加载，已降级为条形列表）</div></div>';
}

function drawChart(el, option, fallbackItems) {
  if (!window.echarts) {
    el.style.height = 'auto';
    el.innerHTML = chartFallbackHtml(fallbackItems || []);
    return null;
  }
  try {
    const existing = window.echarts.getInstanceByDom(el);
    if (existing) existing.dispose();
    const chart = window.echarts.init(el, null, { renderer: 'canvas' });
    chart.setOption(option || {});
    CHARTS.push(chart);
    requestAnimationFrame(() => { try { chart.resize(); } catch (err) { /* ignore */ } });
    return chart;
  } catch (err) {
    el.style.height = 'auto';
    el.innerHTML = chartFallbackHtml(fallbackItems || []) +
      '<div class="muted-2 text-xs" style="padding:0 12px 10px">图表渲染失败：' + esc(err.message || err) + '</div>';
    return null;
  }
}

/* 挂载 ECharts（首次调用时懒加载 1MB 的 echarts.min.js） */
function mountChart(container, option, fallbackItems) {
  if (!container) return null;
  const el = typeof container === 'string' ? $(container) : container;
  if (!el) return null;
  if (window.echarts) return drawChart(el, option, fallbackItems);
  if (!el.querySelector('.load-inline')) {
    el.innerHTML = '<div class="load-inline">' + icon('loader', 'spin') + ' 图表组件加载中…</div>';
  }
  ensureCharts().then((ok) => {
    if (!ok) { el.innerHTML = chartFallbackHtml(fallbackItems || []); return; }
    if (!el.isConnected) return;
    drawChart(el, option, fallbackItems);
  });
  return null;
}

function chartNoData(container, text) {
  const el = typeof container === 'string' ? $(container) : container;
  if (!el) return;
  el.innerHTML = '<div class="empty" style="padding:26px 16px"><div class="empty-ico">' + icon('chart') + '</div><b>暂无数据</b>' +
    (text ? '<p>' + esc(text) + '</p>' : '') + '</div>';
}

function disposeCharts() {
  while (CHARTS.length) {
    const chart = CHARTS.pop();
    try { chart.dispose(); } catch (err) { /* ignore */ }
  }
}

/* 导出图表 PNG */
function exportChartImage(target, filename) {
  const el = typeof target === 'string' ? $(target) : target;
  if (!el) { toast('找不到图表容器', 'warn'); return; }
  const chart = window.echarts && window.echarts.getInstanceByDom(el);
  if (!chart) { toast('图表尚未就绪，无法导出', 'warn'); return; }
  try {
    const url = chart.getDataURL({ type: 'png', pixelRatio: 2, backgroundColor: cssVar('--surface-1', '#0e131d') });
    const link = document.createElement('a');
    link.href = url;
    link.download = (filename || 'fm-chart') + '-' + fmtTime(Date.now(), { dateOnly: true }) + '.png';
    document.body.appendChild(link);
    link.click();
    link.remove();
    toast('图表已导出 PNG', 'ok', { timeout: 1800 });
  } catch (err) {
    toast('导出失败：' + (err.message || err), 'bad');
  }
}

/* 表格导出 CSV */
function exportCsv(rows, columns, filename) {
  const cols = columns || Object.keys((rows && rows[0]) || {});
  const lines = [cols.join(',')];
  (rows || []).forEach((row) => {
    lines.push(cols.map((key) => {
      const value = typeof key === 'function' ? key(row) : row[key];
      const text = value == null ? '' : String(value).replace(/"/g, '""');
      return /[",\n]/.test(text) ? '"' + text + '"' : text;
    }).join(','));
  });
  const blob = new Blob(['\ufeff' + lines.join('\n')], { type: 'text/csv;charset=utf-8' });
  const url = URL.createObjectURL(blob);
  const link = document.createElement('a');
  link.href = url;
  link.download = (filename || 'fm-export') + '.csv';
  document.body.appendChild(link);
  link.click();
  link.remove();
  setTimeout(() => URL.revokeObjectURL(url), 2000);
  toast('已导出 CSV（' + (rows || []).length + ' 行）', 'ok');
}

let chartResizeTimer = 0;
window.addEventListener('resize', () => {
  clearTimeout(chartResizeTimer);
  chartResizeTimer = setTimeout(() => {
    CHARTS.forEach((chart) => { try { chart.resize(); } catch (err) { /* ignore */ } });
  }, 160);
});

/* ── 9. 导航 / 路由 ───────────────────────────────────────────────────── */
const VIEWS = {};

const NAV = [
  { label: '总览', items: [{ id: 'dashboard', name: '仪表盘', icon: 'dash', keys: 'dashboard home 首页 总览 overview' }] },
  {
    label: '分析',
    items: [
      { id: 'analytics-msg', name: '消息分析', icon: 'analytics', keys: 'analytics message 消息 趋势 热力 24x7' },
      { id: 'analytics-groups', name: '群分析', icon: 'groups', keys: 'analytics group 群 活跃 排行' },
      { id: 'analytics-senders', name: '成员活跃', icon: 'members', keys: 'analytics sender 成员 活跃 发言' },
      { id: 'analytics-latency', name: '响应延迟', icon: 'gauge', keys: 'latency 延迟 p50 p90 p99 响应' },
    ],
  },
  {
    label: '社区',
    items: [
      { id: 'groups', name: '群与房间', icon: 'groups', badge: 'groups', keys: 'group 群 房间 trigger 触发' },
      { id: 'members', name: '成员', icon: 'members', keys: 'member 成员 用户 档案' },
      { id: 'messages', name: '消息搜索', icon: 'message', keys: 'message 消息 搜索 chat' },
      { id: 'reports', name: '群日报', icon: 'calendar', keys: 'report daily 日报 每天' },
    ],
  },
  {
    label: '内容',
    items: [
      { id: 'content', name: '内容中心', icon: 'book', keys: 'content 内容 文库 赛文 番茄 打字 统一 搜索' },
      { id: 'library', name: '文库浏览', icon: 'list', keys: 'library 文库 跟打' },
      { id: 'contests', name: '赛文图表', icon: 'chart', keys: 'contest 赛文 图表 曲线' },
      { id: 'novel', name: '番茄阅读', icon: 'file', keys: 'novel 番茄 阅读 小说' },
      { id: 'typing', name: '打字排行', icon: 'trending', keys: 'typing 打字 排行 均速 键准' },
      { id: 'business', name: '业务数据', icon: 'database', keys: 'business 业务 成绩 数据' },
    ],
  },
  {
    label: '机器人',
    items: [
      { id: 'humanlike', name: '拟人模式', icon: 'spark', badge: 'humanlike', keys: 'humanlike 拟人 主动 发言 persona' },
      { id: 'actions', name: '操作台', icon: 'wand', keys: 'action 操作 动作 发送 重启' },
      { id: 'tools', name: '工具与审计', icon: 'wrench', keys: 'tool 工具 审计 调用' },
      { id: 'models', name: '模型与配置', icon: 'cpu', keys: 'model 模型 配置 参数' },
      { id: 'mswitch', name: '模型切换', icon: 'zap', keys: 'switch 切换 模型 provider effort' },
      { id: 'capabilities', name: '群能力矩阵', icon: 'shield', keys: 'capability 能力 矩阵 开关' },
    ],
  },
  {
    label: '运维',
    items: [
      { id: 'ops', name: '运维监控', icon: 'activity', keys: 'ops 运维 监控 metrics 镜像 回滚 cron 日志' },
      { id: 'deploy', name: '部署与运维', icon: 'server', keys: 'deploy 部署 重启 镜像' },
      { id: 'qq', name: 'QQ 登录', icon: 'message', keys: 'qq QQ 登录 snowluma 二维码 扫码 onebot 掉线 上线' },
      { id: 'logs', name: '日志', icon: 'terminal', keys: 'log 日志 错误 error' },
      { id: 'backup', name: '备份与导出', icon: 'download', keys: 'backup 备份 导出 csv' },
      { id: 'selftest', name: '接口自检', icon: 'shield-check', keys: 'selftest 自检 接口 健康' },
      { id: 'sessions', name: '会话与令牌', icon: 'key', keys: 'session token 会话 令牌 登录' },
    ],
  },
  {
    label: '系统',
    items: [
      { id: 'config', name: '配置编辑器', icon: 'gear', keys: 'config toml 配置 编辑器' },
      { id: 'memory', name: '记忆与人设', icon: 'book', keys: 'memory persona 记忆 人设 文件' },
      { id: 'browse', name: '数据浏览器', icon: 'database', keys: 'browse 数据 浏览器 sqlite 表' },
      { id: 'settings', name: '设置', icon: 'user', keys: 'settings 设置 账号 密码 主题' },
    ],
  },
];

const NAV_INDEX = {};
NAV.forEach((section) => {
  section.items.forEach((item) => {
    item.section = section.label;
    NAV_INDEX[item.id] = item;
  });
});

function mergeNavIndex() {
  Object.keys(VIEWS).forEach((id) => {
    if (!NAV_INDEX[id]) {
      NAV_INDEX[id] = { id: id, name: VIEWS[id].name || id, icon: VIEWS[id].icon || 'file', section: '' };
    }
  });
}

const CLEANUPS = [];
function registerCleanup(fn) { if (typeof fn === 'function') CLEANUPS.push(fn); }
function runCleanups() {
  while (CLEANUPS.length) {
    const fn = CLEANUPS.pop();
    try { fn(); } catch (err) { /* 清理失败不影响路由 */ }
  }
}

function decodeSegment(text) {
  try { return decodeURIComponent(text); } catch (err) { return text; }
}

function parseRoute() {
  const raw = (location.hash || '').replace(/^#\/?/, '');
  const pieces = raw.split('?');
  const segments = (pieces[0] || '').split('/').filter(Boolean).map(decodeSegment);
  let query;
  try { query = new URLSearchParams(pieces[1] || ''); } catch (err) { query = new URLSearchParams(); }
  return { segments: segments, query: query, viewId: segments[0] || 'dashboard' };
}

function hashFor(viewId, segments, query) {
  let hash = '#/' + viewId;
  (segments || []).forEach((segment) => { hash += '/' + encodeURIComponent(segment); });
  const params = query instanceof URLSearchParams ? query : new URLSearchParams(query || {});
  const text = params.toString();
  return text ? hash + '?' + text : hash;
}

function refreshNavBadges() {
  const groupsBadge = $('[data-nav-badge="groups"]');
  if (groupsBadge) groupsBadge.textContent = Store.groups ? String(Store.groups.length) : '';
  const humanBadge = $('[data-nav-badge="humanlike"]');
  if (humanBadge) humanBadge.textContent = Store.humanlike && Store.humanlike.scopes ? String(Store.humanlike.scopes.length) : '';
}

function navMatches(item, filter) {
  if (!filter) return true;
  const haystack = [item.name, item.id, item.section, item.keys].join(' ').toLowerCase();
  return filter.split(/\s+/).filter(Boolean).every((token) => haystack.indexOf(token.toLowerCase()) >= 0);
}

function renderNav(activeId) {
  const root = $('#nav');
  if (!root) return;
  const filter = APP.navFilter || '';
  const html = NAV.map((section) => {
    const items = section.items.filter((item) => navMatches(item, filter));
    if (!items.length) return '';
    return '<div class="nav-group">' +
      '<div class="nav-label">' + esc(section.label) + '</div>' +
      items.map((item) =>
        '<a class="nav-item' + (item.id === activeId ? ' active' : '') + '" href="' + escAttr('#/' + item.id) + '" data-nav="' + escAttr(item.id) + '" title="' + escAttr(item.name) + '">' +
          icon(item.icon) + '<span>' + esc(item.name) + '</span>' +
          (item.badge ? '<span class="nav-badge" data-nav-badge="' + item.badge + '"></span>' : '') +
        '</a>').join('') +
      '</div>';
  }).join('');
  root.innerHTML = html || '<div class="nav-empty">没有匹配的页面</div>';
  refreshNavBadges();
}

function renderCrumbs(route) {
  const root = $('#crumbs');
  if (!root) return;
  const view = VIEWS[route.viewId] || VIEWS.dashboard;
  const meta = NAV_INDEX[view.id] || NAV_INDEX.dashboard;
  const parts = [];
  parts.push('<a href="#/dashboard" aria-label="仪表盘" title="仪表盘">' + icon('dash') + '</a>');
  if (meta.section && meta.section !== '总览') {
    parts.push('<span class="sep">' + icon('chevron-right') + '</span>');
    parts.push('<span class="crumb-hide">' + esc(meta.section) + '</span>');
  }
  const paramCrumb = view.crumb ? view.crumb(route) : null;
  if (paramCrumb) {
    parts.push('<span class="sep">' + icon('chevron-right') + '</span>');
    parts.push('<a href="#/' + view.id + '">' + esc(view.name) + '</a>');
    parts.push('<span class="sep">' + icon('chevron-right') + '</span>');
    parts.push('<span class="current mono">' + esc(paramCrumb) + '</span>');
  } else {
    parts.push('<span class="sep">' + icon('chevron-right') + '</span>');
    parts.push('<span class="current crumb-page">' + (VIEWS[view.id] && VIEWS[view.id].icon ? icon(VIEWS[view.id].icon) : '') + esc(view.name) + '</span>');
  }
  root.innerHTML = parts.join('');
}

function setDocumentTitle(view, route) {
  const param = view.crumb ? view.crumb(route) : null;
  document.title = (view.name || 'Console') + (param ? ' · ' + param : '') + ' · ' + APP.title;
}

/* ── 只读账号 UI 门禁：写操作按钮置灰 + tooltip + 顶部提示 ─────────────── */
const ADMIN_WRITE_RE = /(保存|写入|提交|发送|回滚|重启|删除|移除|创建|新建|吊销|撤销|更新密码|生成令牌|立即备份|建备份|清空人设|写入上限|切换模型|保存修改|保存配置|保存并应用|执行回滚)/;
const ADMIN_SAFE_RE = /(刷新|重试|搜索|筛选|详情|查看|展开|收起|导出|复制|返回|上一页|下一页|跳转|取消|JSON|全选|反选|过滤|应用筛选|清空筛选|清除本地|重置未保存|打开|主题|深色|浅色)/;
const ADMIN_GATE_ATTR = 'data-admin-gate';

function adminGateText(el) {
  const text = (el.innerText || el.textContent || '').replace(/\s+/g, ' ').trim();
  return text || el.getAttribute('title') || el.getAttribute('aria-label') || '';
}

function shouldGateAsWrite(el) {
  if (el.hasAttribute('data-admin-safe')) return false;
  if (el.hasAttribute(ADMIN_GATE_ATTR)) return true;
  /* 页签 / 导航 / 命令面板里的按钮只负责跳转，不当作写操作 */
  if (el.hasAttribute('data-tab') || el.closest('.tabs, .nav, .palette-box, .user-menu')) return false;
  const text = adminGateText(el);
  if (ADMIN_WRITE_RE.test(text)) return true;
  if (el.classList.contains('switch')) {
    const id = el.id || '';
    if (!/pref-auto|auto-refresh|theme|nav/i.test(id)) return true;
  }
  return false;
}

function clearAdminGates(root) {
  (root || document).querySelectorAll('[' + ADMIN_GATE_ATTR + ']').forEach((el) => {
    el.removeAttribute(ADMIN_GATE_ATTR);
    el.removeAttribute('aria-disabled');
    el.classList.remove('is-admin-gated');
    if (el.tagName === 'BUTTON') el.disabled = false;
    if (el.dataset.adminOldTitle !== undefined) {
      if (el.dataset.adminOldTitle) el.setAttribute('title', el.dataset.adminOldTitle);
      else el.removeAttribute('title');
      delete el.dataset.adminOldTitle;
    }
    el.removeAttribute('tabindex');
  });
}

function applyAdminGating(root) {
  const scope = root || document;
  if (!isReadonly()) { clearAdminGates(scope); return 0; }
  let count = 0;
  scope.querySelectorAll('button, a.btn, [role="button"], .switch').forEach((el) => {
    if (!shouldGateAsWrite(el)) return;
    const text = adminGateText(el);
    if (ADMIN_SAFE_RE.test(text) && !ADMIN_WRITE_RE.test(text)) return;
    if (!el.hasAttribute(ADMIN_GATE_ATTR)) {
      if (el.dataset.adminOldTitle === undefined) el.dataset.adminOldTitle = el.getAttribute('title') || '';
      el.setAttribute(ADMIN_GATE_ATTR, '1');
      el.setAttribute('aria-disabled', 'true');
      el.classList.add('is-admin-gated');
      el.setAttribute('title', '需要管理员权限（当前为只读账号）');
      if (el.tagName === 'BUTTON') el.disabled = true;
      el.setAttribute('data-admin-old-tabindex', el.getAttribute('tabindex') || '');
    }
    count += 1;
  });
  return count;
}

function maybeReadonlyBanner() {
  if (!isReadonly()) return;
  const main = $('#main');
  if (!main || main.querySelector('.admin-readonly-banner')) return;
  if (!main.querySelector('[' + ADMIN_GATE_ATTR + ']')) return;
  const banner = document.createElement('div');
  banner.className = 'notice warn admin-readonly-banner';
  banner.innerHTML = icon('lock') + '<div><b>只读账号：</b>本页的写操作按钮已全部禁用（需要管理员权限）。如确需修改，请让管理员创建管理员令牌后重新登录。</div>';
  main.insertBefore(banner, main.firstChild);
}

let adminGatingTimer = 0;
let adminGatingObserver = null;
function scheduleAdminGating() {
  clearTimeout(adminGatingTimer);
  adminGatingTimer = setTimeout(() => {
    const gated = applyAdminGating(document);
    if (gated > 0) maybeReadonlyBanner();
  }, 120);
}

function initAdminGating() {
  if (adminGatingObserver) return;
  const targets = [$('#main'), $('#drawer'), $('#modal-root')].filter(Boolean);
  if (!targets.length) return;
  adminGatingObserver = new MutationObserver(() => scheduleAdminGating());
  targets.forEach((node) => adminGatingObserver.observe(node, { childList: true, subtree: true }));
}

/* ── 未知路由：显式 404 页面（不再静默回退仪表盘） ─────────────────────── */
function renderNotFound(ctx, viewId) {
  const root = ctx.root;
  document.title = '页面不存在 · ' + APP.title;
  root.innerHTML = pageHead({
    eyebrow: '404',
    title: '页面不存在',
    icon: 'alert',
    sub: '找不到 #/' + viewId + ' 对应的页面，可能链接已过期或拼写有误。',
    actions: ['<a class="btn btn-sm btn-solid" href="#/dashboard">' + icon('dash') + '返回仪表盘</a>',
      '<button class="btn btn-sm btn-ghost" id="nf-search" type="button">' + icon('search') + '搜索页面</button>'],
  }) + emptyState({
    icon: 'search',
    title: '没有这个页面',
    hint: '用 Ctrl+K 可以搜索页面 / 群 / 成员 / 消息，或返回仪表盘继续操作。',
    actions: ['<button class="btn btn-sm btn-ghost" id="nf-palette" type="button">' + icon('command') + '打开命令面板</button>',
      '<a class="btn btn-sm btn-ghost" href="#/dashboard">' + icon('dash') + '回到仪表盘</a>'],
  });
  const paletteBtn = $('#nf-palette', root) || $('#nf-search', root);
  if (paletteBtn) paletteBtn.addEventListener('click', () => Palette.openWith(viewId));
}

function makeCtx(route) {
  const token = ++APP.routeToken;
  const root = $('#main');
  const ctx = {
    route: route,
    segments: route.segments,
    query: route.query,
    root: root,
    token: token,
    alive() { return APP.routeToken === token; },
    cleanup(fn) { registerCleanup(fn); },
    q(name, fallback) {
      const value = route.query.get(name);
      return value == null || value === '' ? (fallback == null ? '' : fallback) : value;
    },
    n(name, fallback) {
      const value = Number(route.query.get(name));
      return isFinite(value) && value > 0 ? value : fallback;
    },
    setQuery(patch, opts) {
      const options = opts || {};
      const params = new URLSearchParams(route.query.toString());
      Object.keys(patch || {}).forEach((key) => {
        const value = patch[key];
        if (value === undefined || value === null || value === '') params.delete(key);
        else params.set(key, String(value));
      });
      route.query = params;
      const target = hashFor(route.viewId, route.segments.slice(1), params);
      if (options.replace && location.hash !== target) {
        history.replaceState(null, '', target);
      }
      return target;
    },
    every(ms, fn) {
      const id = setInterval(() => {
        if (!APP.autoRefresh || document.hidden) return;
        if (!ctx.alive()) return;
        try { fn(); } catch (err) { /* 轮询失败不打扰 */ }
      }, ms);
      registerCleanup(() => clearInterval(id));
      return id;
    },
  };
  return ctx;
}

let lastRouteKey = '';
let lastRouteAt = 0;
const RootScroller = { restore: 0 };

async function route() {
  const parsed = parseRoute();
  const knownView = Object.prototype.hasOwnProperty.call(VIEWS, parsed.viewId) ? VIEWS[parsed.viewId] : null;
  const view = knownView || VIEWS.dashboard;
  const normalized = parsed.viewId === view.id ? parsed : { segments: [view.id], query: parsed.query, viewId: view.id };
  const routeKey = (location.hash || '#/dashboard');
  const sameRouteRefresh = routeKey === lastRouteKey && Date.now() - lastRouteAt < 25000;
  const prevScroll = window.scrollY || 0;
  lastRouteKey = routeKey;
  lastRouteAt = Date.now();

  runCleanups();
  disposeCharts();
  Drawer.close();
  Palette.close();
  document.body.classList.remove('nav-open');
  const navBackdrop = $('#nav-backdrop');
  if (navBackdrop) navBackdrop.hidden = true;

  if (!knownView) {
    const ctx = makeCtx({ segments: parsed.segments, query: parsed.query, viewId: parsed.viewId });
    renderNav('');
    const crumbs = $('#crumbs');
    if (crumbs) {
      crumbs.innerHTML = '<a href="#/dashboard" aria-label="仪表盘" title="仪表盘">' + icon('dash') + '</a>' +
        '<span class="sep">' + icon('chevron-right') + '</span>' +
        '<span class="current">页面不存在</span>';
    }
    updateAutoRefreshButton();
    renderNotFound(ctx, parsed.viewId);
    scheduleAdminGating();
    return;
  }

  renderNav(view.id);
  renderCrumbs(normalized);
  setDocumentTitle(view, normalized);
  updateAutoRefreshButton();

  const ctx = makeCtx(normalized);
  const root = $('#main');
  root.innerHTML = skeletonPage(view.skeleton || 'default');
  RootScroller.restore = sameRouteRefresh ? prevScroll : 0;
  if (!sameRouteRefresh) window.scrollTo({ top: 0 });
  if (view.render) {
    try {
      await view.render(ctx);
    } catch (err) {
      if (!ctx.alive()) return;
      if (err instanceof ApiError && err.status === 401) return;
      console.error('[console] 视图渲染失败', view.id, err);
      showViewError(root, err, () => route());
    }
  } else {
    root.innerHTML = pageHead({ title: view.name || view.id, icon: view.icon }) + pendingState({ title: '页面还没实现', hint: '该视图尚未注册 render 方法。', path: '#/' + view.id });
  }
  if (ctx.alive() && sameRouteRefresh) window.scrollTo({ top: RootScroller.restore });
  if (ctx.alive()) {
    /* 只读账号：把本页写按钮置灰，并在写页顶部显示只读提示 */
    const gated = applyAdminGating(root);
    if (gated > 0) maybeReadonlyBanner();
    scheduleAdminGating();
  }
}

/* ── 10. 命令面板 ─────────────────────────────────────────────────────── */
const COMMANDS = [
  { id: 'theme', title: '切换深色 / 浅色主题', icon: 'moon', keys: 'theme dark light 主题 深色 浅色', run: () => Theme.toggle() },
  { id: 'collapse', title: '收起 / 展开左侧导航', icon: 'panel-left', keys: 'nav sidebar collapse 导航 收起', run: () => toggleNavCollapse() },
  { id: 'refresh', title: '刷新当前页面', icon: 'refresh', keys: 'refresh reload 刷新', run: () => route() },
  { id: 'shortcuts', title: '查看快捷键', icon: 'command', keys: 'shortcut key 快捷键 help', run: () => openShortcuts() },
  { id: 'logout', title: '退出登录', icon: 'logout', keys: 'logout 退出 登录', run: () => doLogout() },
];

const Palette = {
  open: false,
  items: [],
  filtered: [],
  activeIndex: 0,
  scopeIndex: 0,
  scopes: ['全部', '页面', '群', '成员', '消息', '内容', '命令'],
  searchToken: 0,
  lastQuery: '',

  init() {
    const input = $('#palette-input');
    if (!input) return;
    input.addEventListener('input', () => this.onInput(input.value));
    input.addEventListener('keydown', (event) => this.onKey(event));
    const closeBtn = $('#palette-close');
    if (closeBtn) closeBtn.addEventListener('click', () => this.close());
    const overlay = $('#palette');
    if (overlay) overlay.addEventListener('mousedown', (event) => { if (event.target === overlay) this.close(); });
    this.renderStatic('');
  },

  staticItems() {
    const items = [];
    NAV.forEach((section) => {
      section.items.forEach((item) => {
        items.push({
          group: '页面 · ' + section.label,
          title: item.name,
          sub: '#/' + item.id,
          icon: item.icon,
          keywords: [item.name, item.id, item.section, item.keys].join(' '),
          run: () => go('#/' + item.id),
          scope: '页面',
        });
      });
    });
    COMMANDS.forEach((command) => {
      items.push({
        group: '命令',
        title: command.title,
        sub: '快捷命令',
        icon: command.icon,
        keywords: command.keys,
        run: command.run,
        scope: '命令',
      });
    });
    return items;
  },

  openWith(query) {
    const overlay = $('#palette');
    if (!overlay) return;
    overlay.hidden = false;
    this.open = true;
    this.scopeIndex = 0;
    const input = $('#palette-input');
    if (input) {
      input.value = query || '';
      input.focus();
      input.select();
    }
    this.renderStatic(query || '');
    this.updateScopeLabel();
  },

  close() {
    const overlay = $('#palette');
    if (overlay) overlay.hidden = true;
    this.open = false;
  },

  updateScopeLabel() {
    const label = $('#palette-scope');
    if (label) label.textContent = this.scopes[this.scopeIndex] || '全部';
  },

  scopeFilter(items) {
    const scope = this.scopes[this.scopeIndex];
    if (!scope || scope === '全部') return items;
    const infer = (item) => {
      if (item.scope) return item.scope;
      const group = String(item.group || '');
      if (group.indexOf('页面') === 0) return '页面';
      if (group.indexOf('命令') === 0) return '命令';
      if (group.indexOf('群') === 0) return '群';
      if (group.indexOf('成员') === 0) return '成员';
      if (group.indexOf('消息') === 0) return '消息';
      if (group.indexOf('内容') === 0) return '内容';
      return '';
    };
    if (scope === '内容') return items.filter((item) => infer(item) === '内容' || /内容|文库|赛文|番茄|打字/.test(String(item.group || '')));
    return items.filter((item) => infer(item) === scope);
  },

  renderStatic(query) {
    const q = (query || '').trim().toLowerCase();
    /* 去重：pages-v3/v4 会往 staticItems 追加页面项，避免同一页面出现两次 */
    const seen = new Set();
    const base = this.staticItems().filter((item) => {
      const key = String(item.title || '') + '|' + String(item.sub || '');
      if (seen.has(key)) return false;
      seen.add(key);
      return true;
    });
    const filtered = base.filter((item) => !q || (item.keywords + ' ' + item.title).toLowerCase().indexOf(q) >= 0);
    this.items = filtered;
    this.filtered = this.scopeFilter(filtered);
    this.activeIndex = 0;
    this.paint();
    const tip = $('#palette-tip');
    if (tip) tip.textContent = q ? '搜索中… 同时检索群 / 成员 / 消息 / 文库 / 赛文' : '支持跨库搜索，输入 ≥1 个字符开始';
  },

  paint(extraGroups) {
    const root = $('#palette-list');
    if (!root) return;
    const groups = new Map();
    const add = (group, item) => {
      if (!groups.has(group)) groups.set(group, []);
      groups.get(group).push(item);
    };
    if (extraGroups) {
      const scope = this.scopes[this.scopeIndex];
      const allow = (groupScope) => !scope || scope === '全部' || scope === groupScope;
      if (allow('群')) (extraGroups.groups || []).forEach((item) => add('群', item));
      if (allow('成员')) (extraGroups.members || []).forEach((item) => add('成员', item));
      if (allow('消息')) (extraGroups.messages || []).forEach((item) => add('消息', item));
      if (allow('内容')) (extraGroups.content || []).forEach((item) => add('内容', item));
    }
    const list = [];
    groups.forEach((items, group) => {
      list.push({ group: group, items: items });
    });
    if (this.filtered.length) list.unshift({ group: '页面与命令', items: this.filtered.slice(0, 12) });
    this.activeIndex = Math.min(this.activeIndex, Math.max(0, list.reduce((sum, g) => sum + g.items.length, 0) - 1));
    if (!list.length) {
      root.innerHTML = '<div class="palette-empty">没有匹配结果<br><span class="text-xs muted-2">试试页面名、群号、成员昵称或消息关键词</span></div>';
      return;
    }
    let index = 0;
    root.innerHTML = list.map((group) =>
      '<div class="palette-group"><div class="palette-group-label">' + esc(group.group) + '</div>' +
      group.items.map((item) => {
        const active = index === this.activeIndex;
        const html = '<button class="palette-item' + (active ? ' active' : '') + '" data-palette-index="' + index + '" type="button">' +
          icon(item.icon || 'file') +
          '<span class="pi-main"><span class="pi-title">' + esc(item.title) + '</span>' +
          (item.sub ? '<span class="pi-sub">' + esc(item.sub) + '</span>' : '') + '</span>' +
          (item.extra ? '<span class="pi-extra">' + esc(item.extra) + '</span>' : '') +
          '</button>';
        index += 1;
        return html;
      }).join('') + '</div>').join('');
    this.flat = [];
    list.forEach((group) => group.items.forEach((item) => this.flat.push(item)));
    root.scrollTop = 0;
  },

  onInput(value) {
    this.renderStatic(value);
    this.updateScopeLabel();
    const q = (value || '').trim();
    const tip = $('#palette-tip');
    if (!q) return;
    const token = ++this.searchToken;
    if (tip) tip.textContent = '搜索中…';
    this.searchRemote(q, token);
  },

  async searchRemote(q, token) {
    const results = { groups: [], members: [], messages: [], content: [] };
    const settled = await Promise.allSettled([
      (async () => {
        const groups = Store.groups || await getGroups().catch(() => []);
        results.groups = (groups || []).filter((g) => (String(g.name || '') + ' ' + String(g.group_id)).toLowerCase().indexOf(q.toLowerCase()) >= 0)
          .slice(0, 6).map((g) => ({ group: '群', title: g.name || g.group_id, sub: g.group_id + ' · ' + fmtNum(g.messages) + ' 条消息', icon: 'groups', extra: '群', run: () => go('#/groups/' + encodeURIComponent(g.group_id)) }));
      })(),
      (async () => {
        const data = unwrap(await apiGet('/api/v2/members?q=' + encodeURIComponent(q), { timeout: 9000 }));
        const matches = (data && (data.matches || data.members)) || [];
        results.members = matches.slice(0, 5).map((m) => ({
          group: '成员', title: (m.aliases && m.aliases[0]) || m.user_id || '未命名', sub: 'ID ' + (m.user_id || '—') + (m.messages != null ? ' · ' + fmtNum(m.messages) + ' 条' : ''), icon: 'members', extra: '成员',
          run: () => go('#/members/' + encodeURIComponent(m.user_id || '')),
        }));
      })(),
      (async () => {
        const data = unwrap(await apiGet('/api/v2/messages' + qs({ q: q, limit: 5, days: 30 }), { timeout: 9000 }));
        const rows = (data && (data.rows || data.items)) || [];
        results.messages = rows.slice(0, 4).map((m) => ({
          group: '消息', title: truncate(m.body_text || m.text || '（空消息）', 46), sub: (m.sender_username || m.sender_name || m.sender_id || '') + ' · ' + fmtTime(m.recorded_at || m.occurred_at), icon: 'message', extra: '消息',
          run: () => openJsonDrawer('消息详情', m),
        }));
      })(),
      (async () => {
        const data = unwrap(await apiGet('/api/domain/library' + qs({ q: q, limit: 4 }), { timeout: 9000 }).catch(() => null));
        const rows = Array.isArray(data) ? data : ((data && data.items) || []);
        results.content = rows.slice(0, 4).map((row) => ({
          group: '内容 · 文库', title: row.title || row.text_id || '（无标题）', sub: fmtNum(row.char_count) + ' 字 · ' + (row.difficulty || '难度未知'), icon: 'book', extra: '文库',
          run: () => go('#/content?q=' + encodeURIComponent(row.title || '')),
        }));
      })(),
    ]);
    void settled;
    if (token !== this.searchToken || !this.open) return;
    this.paint(results);
    const count = results.groups.length + results.members.length + results.messages.length + results.content.length;
    const tip = $('#palette-tip');
    if (tip) tip.textContent = count ? '找到 ' + count + ' 条跨库结果 · ↑↓ 选择 · ↵ 打开' : '没有跨库结果，试试其它关键词';
  },

  onKey(event) {
    if (event.key === 'ArrowDown') {
      event.preventDefault();
      this.move(1);
    } else if (event.key === 'ArrowUp') {
      event.preventDefault();
      this.move(-1);
    } else if (event.key === 'Tab') {
      event.preventDefault();
      this.scopeIndex = (this.scopeIndex + (event.shiftKey ? -1 : 1) + this.scopes.length) % this.scopes.length;
      this.updateScopeLabel();
      const q = ($('#palette-input') || {}).value || '';
      this.renderStatic(q);
      if (q.trim()) this.searchRemote(q.trim(), ++this.searchToken);
    } else if (event.key === 'Enter') {
      event.preventDefault();
      const item = this.flat && this.flat[this.activeIndex];
      if (item && typeof item.run === 'function') {
        this.close();
        setTimeout(() => { try { item.run(); } catch (err) { toast(String(err.message || err), 'bad'); } }, 20);
      }
    } else if (event.key === 'Escape') {
      event.preventDefault();
      this.close();
    }
  },

  move(delta) {
    const total = (this.flat || []).length;
    if (!total) return;
    this.activeIndex = (this.activeIndex + delta + total) % total;
    const nodes = $$('#palette-list .palette-item');
    nodes.forEach((node) => node.classList.remove('active'));
    const target = nodes[this.activeIndex];
    if (target) {
      target.classList.add('active');
      target.scrollIntoView({ block: 'nearest' });
    }
  },
};

/* 点击面板项 */
document.addEventListener('click', (event) => {
  const item = event.target.closest('#palette-list .palette-item');
  if (!item) return;
  const index = Number(item.getAttribute('data-palette-index'));
  Palette.activeIndex = index;
  const entry = (Palette.flat || [])[index];
  if (entry && typeof entry.run === 'function') {
    Palette.close();
    setTimeout(() => { try { entry.run(); } catch (err) { toast(String(err.message || err), 'bad'); } }, 20);
  }
});

/* ── 11. 快捷键 / 帮助 / 全局绑定 ─────────────────────────────────────── */
const SHORTCUTS = [
  ['Ctrl / ⌘ + K', '打开全局搜索与命令面板'],
  ['/', '聚焦搜索（命令面板）'],
  ['g 然后 d / a / g / c / o / s / l / m', '快速跳转：仪表盘 / 分析 / 群 / 内容 / 运维 / 设置 / 日志 / 成员'],
  ['t', '切换深色 / 浅色主题'],
  ['r', '刷新当前页面'],
  ['Ctrl / ⌘ + B', '收起 / 展开左侧导航'],
  ['Esc', '关闭抽屉 / 弹窗 / 搜索面板'],
  ['?', '打开快捷键帮助'],
];

function openShortcuts() {
  openModal({
    title: '键盘快捷键',
    icon: 'command',
    bodyHtml: '<div class="kv-grid" style="grid-template-columns:1fr">' + SHORTCUTS.map((row) =>
      '<div class="stat-line"><span>' + esc(row[1]) + '</span><span class="row">' + row[0].split(' / ').map((k) => '<kbd class="kbd">' + esc(k) + '</kbd>').join('') + '</span></div>').join('') + '</div>',
    buttons: [{ label: '知道了', variant: 'btn-solid' }],
  });
}

/* 图表在容器宽度变化（侧栏折叠/窗口 resize）后必须重新 resize（任务检查项 5） */
function resizeChartsSoon() {
  clearTimeout(APP.__resizeTimer || 0);
  APP.__resizeTimer = setTimeout(() => {
    CHARTS.forEach((chart) => { try { chart.resize(); } catch (err) { /* ignore */ } });
  }, 320);
}

/* 视口自适应导航：>1024 展开；901–1024 自动收起为图标栏；≤900 交给抽屉。
   用户手动折叠/展开后不再自动覆盖（APP.navPinned） */
function syncResponsiveNav() {
  if (APP.navPinned) return;
  const width = window.innerWidth;
  const root = document.documentElement;
  if (width > 1024) root.setAttribute('data-nav', 'expanded');
  else if (width > 900) root.setAttribute('data-nav', 'collapsed');
  else root.setAttribute('data-nav', 'expanded');
  resizeChartsSoon();
}

function toggleNavCollapse() {
  APP.navPinned = true;
  const collapsed = document.documentElement.getAttribute('data-nav') === 'collapsed';
  document.documentElement.setAttribute('data-nav', collapsed ? 'expanded' : 'collapsed');
  safeLocalSet('fm-console-nav', collapsed ? 'expanded' : 'collapsed');
  resizeChartsSoon();
}

function openNavDrawer() {
  document.body.classList.add('nav-open');
  const backdrop = $('#nav-backdrop');
  if (backdrop) backdrop.hidden = false;
}

function closeNavDrawer() {
  document.body.classList.remove('nav-open');
  const backdrop = $('#nav-backdrop');
  if (backdrop) backdrop.hidden = true;
}

function doLogout() {
  confirmAction({ title: '退出登录', detail: '将清除当前会话并回到登录页。', confirmLabel: '退出', danger: true, icon: 'logout' })
    .then((ok) => {
      if (!ok) return;
      apiPost('/api/logout', {}).catch(() => {}).then(() => {
        location.replace('/login');
      });
    });
}

function toggleAutoRefresh() {
  APP.autoRefresh = !APP.autoRefresh;
  updateAutoRefreshButton();
  if (APP.autoRefresh) {
    route();
    toast('已开启自动刷新（10 秒）', 'ok', { timeout: 2000 });
  } else {
    toast('已关闭自动刷新', 'info', { timeout: 1600 });
  }
}

function updateAutoRefreshButton() {
  const btn = $('#auto-refresh');
  const text = $('#auto-refresh-text');
  if (btn) btn.classList.toggle('on', !!APP.autoRefresh);
  if (text) text.textContent = APP.autoRefresh ? '自动刷新中' : '自动刷新';
}

let footerTimer = 0;
function startFooterClock() {
  const paint = () => {
    const time = $('#footer-time');
    if (time) {
      const now = new Date();
      time.textContent = now.getFullYear() + '-' + pad(now.getMonth() + 1) + '-' + pad(now.getDate()) + ' ' + pad(now.getHours()) + ':' + pad(now.getMinutes()) + ':' + pad(now.getSeconds());
    }
  };
  paint();
  if (!footerTimer) footerTimer = setInterval(paint, 1000);
}

async function loadIdentity() {
  try {
    const me = await apiGet('/api/auth/me', { cache: 60000 });
    APP.user = me;
    Store.me = me;
    const name = (me && me.username) || 'admin';
    const initial = name.slice(0, 1).toUpperCase();
    const nameNode = $('#user-name');
    if (nameNode) nameNode.textContent = name;
    const avatar = $('#user-avatar');
    if (avatar) avatar.textContent = initial;
    const avatarLg = $('#user-avatar-lg');
    if (avatarLg) avatarLg.textContent = initial;
    const menuName = $('#user-menu-name');
    if (menuName) menuName.textContent = name;
    const menuRole = $('#user-menu-role');
    if (menuRole) menuRole.textContent = me && me.is_admin ? '管理员' : '只读令牌';
    const admin = !!(me && me.is_admin === true);
    document.body.setAttribute('data-role', admin ? 'admin' : 'readonly');
    if (!admin) document.body.setAttribute('data-readonly', '1');
    else document.body.removeAttribute('data-readonly');
    const chip = $('#user-chip');
    if (chip) chip.setAttribute('title', admin ? '当前账号：管理员' : '当前账号：只读（写操作已禁用）');
    const roleTag = $('.user-menu-head small');
    if (roleTag) roleTag.setAttribute('title', admin ? '管理员令牌：可读写' : '只读令牌：所有写操作已禁用');
    setSidebarStatus('ok', '已登录 · ' + name + (admin ? '' : '（只读）'));
    scheduleAdminGating();
  } catch (err) {
    if (err instanceof ApiError && err.status === 401) return;
    setSidebarStatus('bad', '身份校验失败');
  }
}

function bindGlobalUi() {
  const navOpen = $('#nav-open');
  const navClose = $('#nav-close');
  const collapse = $('#nav-collapse');
  const backdrop = $('#nav-backdrop');
  if (navOpen) navOpen.addEventListener('click', openNavDrawer);
  if (navClose) navClose.addEventListener('click', closeNavDrawer);
  if (collapse) collapse.addEventListener('click', toggleNavCollapse);
  if (backdrop) backdrop.addEventListener('click', closeNavDrawer);

  const navFilter = $('#nav-filter');
  const navFilterClear = $('#nav-filter-clear');
  if (navFilter) {
    navFilter.addEventListener('input', () => {
      APP.navFilter = navFilter.value.trim();
      if (navFilterClear) navFilterClear.hidden = !APP.navFilter;
      renderNav((parseRoute() || {}).viewId || 'dashboard');
    });
    navFilter.addEventListener('keydown', (event) => {
      if (event.key === 'Escape') { navFilter.value = ''; APP.navFilter = ''; renderNav(parseRoute().viewId); navFilter.blur(); }
    });
  }
  if (navFilterClear) navFilterClear.addEventListener('click', () => {
    if (navFilter) navFilter.value = '';
    APP.navFilter = '';
    navFilterClear.hidden = true;
    renderNav(parseRoute().viewId);
  });

  const themeToggle = $('#theme-toggle');
  if (themeToggle) themeToggle.addEventListener('click', () => Theme.toggle());

  const refresh = $('#refresh');
  if (refresh) refresh.addEventListener('click', () => {
    refresh.classList.add('spin');
    route();
    setTimeout(() => refresh.classList.remove('spin'), 700);
  });
  const auto = $('#auto-refresh');
  if (auto) auto.addEventListener('click', toggleAutoRefresh);
  const paletteOpen = $('#palette-open');
  if (paletteOpen) paletteOpen.addEventListener('click', () => Palette.openWith(''));
  const logout = $('#sidebar-logout');
  if (logout) logout.addEventListener('click', doLogout);
  const userLogout = $('#user-logout');
  if (userLogout) userLogout.addEventListener('click', doLogout);
  const envCard = $('#env-card');
  if (envCard) envCard.addEventListener('click', openEnvDrawer);
  const menuShortcuts = $('#menu-shortcuts');
  if (menuShortcuts) menuShortcuts.addEventListener('click', openShortcuts);

  const userChip = $('#user-chip');
  const userMenu = $('#user-menu');
  if (userChip && userMenu) {
    userChip.addEventListener('click', (event) => {
      event.stopPropagation();
      const willOpen = userMenu.hidden;
      userMenu.hidden = !willOpen;
      userChip.setAttribute('aria-expanded', willOpen ? 'true' : 'false');
    });
    document.addEventListener('click', (event) => {
      if (!userMenu.hidden && !event.target.closest('.user-menu-wrap')) {
        userMenu.hidden = true;
        userChip.setAttribute('aria-expanded', 'false');
      }
    });
  }

  document.addEventListener('click', (event) => {
    if (event.target.closest('.nav-item[data-nav]') && window.innerWidth <= 900) closeNavDrawer();
    if (event.target.closest('[data-retry]')) route();
    const tabBtn = event.target.closest('[data-tab]');
    if (tabBtn && !event.target.closest('[data-tab-keep]')) {
      const view = VIEWS[parseRoute().viewId];
      if (view && view.onTab) view.onTab(tabBtn.getAttribute('data-tab'));
    }
    const exportBtn = event.target.closest('[data-chart-export]');
    if (exportBtn) {
      event.preventDefault();
      exportChartImage(exportBtn.getAttribute('data-chart-export'), exportBtn.getAttribute('data-chart-name') || 'fm-chart');
    }
  });

  document.addEventListener('keydown', onGlobalKey);
  window.addEventListener('resize', debounce(syncResponsiveNav, 180));
  window.addEventListener('hashchange', () => route());
  window.addEventListener('online', () => { updateNetBadge(true, 0); toast('网络已恢复', 'ok', { timeout: 2000 }); });
  window.addEventListener('offline', () => { updateNetBadge(false); toast('网络已断开，数据可能不是最新', 'bad', { timeout: 5000 }); });
  window.addEventListener('unhandledrejection', (event) => {
    const reason = event.reason;
    if (reason instanceof ApiError) {
      if (reason.status === 401) return;
      toast(reason.message, 'bad');
      return;
    }
    console.error('[console] 未处理的 Promise 异常', reason);
  });
  window.addEventListener('error', (event) => {
    if (!event || !event.message) return;
    if (String(event.message).indexOf('ResizeObserver') >= 0) return;
    console.error('[console] 运行时错误', event.message);
  });
}

let pendingGoto = 0;
function onGlobalKey(event) {
  const tag = (event.target && event.target.tagName ? event.target.tagName : '').toLowerCase();
  const typing = tag === 'input' || tag === 'textarea' || tag === 'select' || (event.target && event.target.isContentEditable);
  const meta = event.ctrlKey || event.metaKey;

  if (meta && (event.key === 'k' || event.key === 'K')) {
    event.preventDefault();
    if (Palette.open) Palette.close(); else Palette.openWith('');
    return;
  }
  if (meta && (event.key === 'b' || event.key === 'B')) {
    event.preventDefault();
    toggleNavCollapse();
    return;
  }
  if (event.key === 'Escape') {
    if (Palette.open) { Palette.close(); return; }
    if (Drawer.isOpen()) { Drawer.close(); return; }
    if (document.body.classList.contains('nav-open')) { closeNavDrawer(); return; }
  }
  if (typing) return;

  if (event.key === '/') {
    event.preventDefault();
    Palette.openWith('');
    return;
  }
  if (event.key === '?') {
    event.preventDefault();
    openShortcuts();
    return;
  }
  if ((event.key === 'r' || event.key === 'R') && !meta) {
    if (Drawer.isOpen() || Palette.open) return;
    event.preventDefault();
    route();
    return;
  }
  if ((event.key === 't' || event.key === 'T') && !meta) {
    event.preventDefault();
    Theme.toggle();
    return;
  }
  if (!meta && !event.altKey) {
    const key = String(event.key || '').toLowerCase();
    const targets = {
      d: '#/dashboard', a: '#/analytics-msg', g: '#/groups', c: '#/content',
      o: '#/ops', s: '#/settings', l: '#/logs', m: '#/members', t: '#/tools',
      u: '#/analytics-senders', h: '#/humanlike', b: '#/browse', q: '#/qq',
    };
    if (key === 'g') {
      pendingGoto = Date.now();
      return;
    }
    if (Date.now() - pendingGoto < 1000 && targets[key]) {
      pendingGoto = 0;
      event.preventDefault();
      go(targets[key]);
      return;
    }
  }
}

/* 环境状态抽屉 */
async function openEnvDrawer() {
  Drawer.open({
    title: '环境与连接状态',
    eyebrow: 'ENV',
    sub: '登录身份、接口延迟、后端组件健康度',
    body: '<div class="load-inline">' + icon('loader', 'spin') + ' 正在收集状态…</div>',
    onMount: async (bodyEl) => {
      const results = await Promise.allSettled([
        apiGet('/api/auth/me', { cache: 60000 }),
        apiGet('/api/observability/overview', { timeout: 12000 }),
        apiGet('/api/errors/stats', { timeout: 12000 }),
      ]);
      if (!bodyEl.isConnected) return;
      const me = results[0].status === 'fulfilled' ? results[0].value : null;
      const obs = results[1].status === 'fulfilled' ? results[1].value : null;
      const errors = results[2].status === 'fulfilled' ? results[2].value : null;
      const obsData = obs || {};
      const errorData = (errors && errors.data) || {};
      const rows = [
        ['当前账号', me ? me.username + (me.is_admin ? '（管理员）' : '（只读令牌）') : '—', 'user'],
        ['浏览器时区', Intl.DateTimeFormat().resolvedOptions().timeZone || '—', 'globe'],
        ['RPC 通道', obsData.rpc && obsData.rpc.ok ? '正常' : '异常', 'link'],
        ['Domain 服务', obsData.domain && obsData.domain.ok ? '正常' : '异常', 'database'],
        ['配置同步', obsData.config_sync && obsData.config_sync.applied ? '已应用' : (obsData.config_sync && obsData.config_sync.ok ? '已保存待应用' : '不一致'), 'save'],
        ['最近错误', (errorData.items && errorData.items.length) || 0, 'alert'],
        ['页面加载耗时', Math.round(performance.now()) + ' ms', 'clock'],
      ];
      bodyEl.innerHTML = '<div class="kv-grid" style="grid-template-columns:repeat(auto-fit,minmax(210px,1fr))">' + rows.map((row) =>
        '<div class="kv-item"><div class="k">' + icon(row[2]) + esc(row[0]) + '</div><div class="v">' + esc(row[1]) + '</div></div>').join('') + '</div>' +
        '<div class="row mt-16">' +
        '<button class="btn btn-sm btn-ghost" data-goto="#/selftest" type="button">' + icon('shield-check') + '接口自检</button>' +
        '<button class="btn btn-sm btn-ghost" data-goto="#/ops/metrics" type="button">' + icon('activity') + '运维监控</button>' +
        '<button class="btn btn-sm btn-ghost" data-goto="#/browse" type="button">' + icon('database') + '数据浏览器</button>' +
        '</div>';
    },
  });
}

function refreshEnvHealth() {
  pingEnv();
  const timer = setInterval(() => { if (!document.hidden) pingEnv(); }, 30000);
  window.addEventListener('beforeunload', () => clearInterval(timer));
}

/* 表格 URL 状态同步辅助（列表页可分享） */
function syncQuery(ctx, patch, extraSegments) {
  const params = new URLSearchParams(ctx.query.toString());
  Object.keys(patch || {}).forEach((key) => {
    const value = patch[key];
    if (value === undefined || value === null || value === '') params.delete(key);
    else params.set(key, String(value));
  });
  const text = params.toString();
  const segments = extraSegments != null ? extraSegments : (ctx.segments || []).slice(1);
  return '#/' + ctx.route.viewId + (segments.length ? '/' + segments.map(encodeURIComponent).join('/') : '') + (text ? '?' + text : '');
}

/* 通用：时间单元格 */
function timeCell(value) {
  const text = fmtTime(value);
  if (!text) return '<span class="muted-2">—</span>';
  return '<span class="mono" title="' + escAttr(String(value)) + '">' + esc(text) + '</span>';
}
function agoCell(value) {
  const text = ago(value);
  if (!text) return '<span class="muted-2">—</span>';
  return '<span title="' + escAttr(fmtTime(value)) + '">' + esc(text) + '</span>';
}

/* ── 12. 启动 ─────────────────────────────────────────────────────────── */
async function boot() {
  Theme.init();
  bindGlobalUi();
  syncResponsiveNav();
  Palette.init();
  startFooterClock();
  updateAutoRefreshButton();
  renderNav('dashboard');
  /* 先渲染一次骨架，再拉身份与数据，保证首屏 < 1.5s 有内容 */
  await loadIdentity();
  mergeNavIndex();
  initAdminGating();
  await route();
  applyAdminGating(document);
  maybeReadonlyBanner();
  getGroups().then(refreshNavBadges).catch(() => {});
  getHumanlike().then(refreshNavBadges).catch(() => {});
  refreshEnvHealth();
  console.info('[FM Console ' + APP.version + '] boot 完成 ' + Math.round(performance.now()) + 'ms');
  window.FM = window.FM || {};
  window.FM.app = {
    APP: APP, VIEWS: VIEWS, NAV: NAV, route: route, go: go, theme: Theme,
    isAdmin: isAdmin, isReadonly: isReadonly, applyAdminGating: applyAdminGating,
    toastReadonly: toastReadonly, syncResponsiveNav: syncResponsiveNav, resizeCharts: resizeChartsSoon,
  };
}

if (document.readyState === 'loading') {
  window.addEventListener('DOMContentLoaded', () => { boot(); });
} else {
  boot();
}
