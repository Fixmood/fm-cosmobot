'use strict';
/* ============================================================================
 * FM Console v2 — 纯原生 JS 控制台（零构建、零依赖，图表用本地 ECharts）
 *
 * 结构：
 *   1. 常量与工具       2. 接口层（fetch / 401 / 降级探测）
 *   3. 全局状态         4. UI 原语（toast / 弹窗 / 抽屉 / 命令面板）
 *   5. 通用渲染件       6. 图表封装
 *   7. 页面视图（13 个） 8. 路由与启动
 *
 * 约定：
 *   - 所有动态文本一律走 esc()，禁止直接拼接未转义数据；
 *   - 视图 render(ctx) 自己负责先渲染骨架、再填数据；
 *   - 图表在路由切换时统一 dispose，避免内存泄漏。
 * ========================================================================== */

/* ── 1. 常量与工具 ─────────────────────────────────────────────────────── */

const APP = {
  version: '2.0.0',
  title: 'FM Console',
  autoRefresh: false,
  autoRefreshMs: 10000,
  lastLatency: 0,
  user: null,
  routeToken: 0,
};
try { APP.autoRefresh = localStorage.getItem('fm-console-auto-refresh') === '1'; } catch (e) { /* 隐私模式 */ }

const $ = (sel, root) => (root || document).querySelector(sel);
const $$ = (sel, root) => Array.from((root || document).querySelectorAll(sel));

const ESCAPE_MAP = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' };
const esc = (value) => String(value == null ? '' : value).replace(/[&<>"']/g, (ch) => ESCAPE_MAP[ch]);
const escAttr = esc;

function icon(name, extraClass) {
  return '<svg class="ico ' + (extraClass || '') + '" aria-hidden="true"><use href="#i-' + esc(name) + '"/></svg>';
}

function clsx() {
  const out = [];
  for (let i = 0; i < arguments.length; i += 1) {
    const value = arguments[i];
    if (!value) continue;
    if (typeof value === 'string') out.push(value);
    else if (Array.isArray(value)) out.push(clsx.apply(null, value));
    else if (typeof value === 'object') Object.keys(value).forEach((k) => { if (value[k]) out.push(k); });
  }
  return out.join(' ');
}

const pad = (n) => (n < 10 ? '0' + n : '' + n);

function toMs(value) {
  if (value == null || value === '') return null;
  if (typeof value === 'number' && isFinite(value)) {
    if (value > 1e16) return value / 1e6;
    if (value > 1e14) return value / 1e3;
    if (value > 1e12) return value;
    if (value > 1e9) return value * 1000;
    return null;
  }
  const raw = String(value).trim();
  if (/^-?\d+(\.\d+)?$/.test(raw)) return toMs(Number(raw));
  let text = raw.replace(/\.(\d{3})\d+/, '.$1');
  if (/[+-]\d{4}$/.test(text)) text = text.slice(0, -5) + text.slice(-5, -2) + ':' + text.slice(-2);
  if (!/[T ]\d{2}:\d{2}/.test(text)) return null;
  const date = new Date(text.indexOf('T') >= 0 ? text : text.replace(' ', 'T'));
  const ms = date.getTime();
  return isNaN(ms) ? null : ms;
}

function fmtTime(value, opts) {
  const options = opts || {};
  const ms = toMs(value);
  if (ms == null) return value == null || value === '' ? '' : String(value);
  const d = new Date(ms);
  const datePart = d.getFullYear() + '-' + pad(d.getMonth() + 1) + '-' + pad(d.getDate());
  if (options.dateOnly) return datePart;
  const timePart = pad(d.getHours()) + ':' + pad(d.getMinutes()) + (options.noSeconds ? '' : ':' + pad(d.getSeconds()));
  return options.timeOnly ? timePart : datePart + ' ' + timePart;
}

function ago(value) {
  const ms = toMs(value);
  if (ms == null) return '';
  const diff = Date.now() - ms;
  const future = diff < 0;
  const sec = Math.abs(diff) / 1000;
  const suffix = future ? '后' : '前';
  if (sec < 45) return '刚刚';
  if (sec < 3600) return Math.round(sec / 60) + ' 分钟' + suffix;
  if (sec < 86400) return Math.round(sec / 3600) + ' 小时' + suffix;
  if (sec < 86400 * 30) return Math.round(sec / 86400) + ' 天' + suffix;
  return fmtTime(value, { dateOnly: true });
}

function fmtNum(value, digits) {
  if (value == null || value === '' || (typeof value === 'number' && !isFinite(value))) return '-';
  const num = typeof value === 'number' ? value : Number(value);
  if (!isFinite(num)) return String(value);
  const fixed = digits == null ? num : Number(num.toFixed(digits));
  return fixed.toLocaleString('zh-CN', digits == null ? {} : { minimumFractionDigits: digits, maximumFractionDigits: digits });
}

function fmtPct(part, total, digits) {
  const p = Number(part);
  const t = Number(total);
  if (!isFinite(p) || !isFinite(t) || t <= 0) return '-';
  return (p / t * 100).toFixed(digits == null ? 1 : digits) + '%';
}

function fmtBytes(value) {
  const n = Number(value);
  if (!isFinite(n) || n < 0) return '-';
  if (n < 1024) return n + ' B';
  if (n < 1024 * 1024) return (n / 1024).toFixed(1) + ' KB';
  if (n < 1024 * 1024 * 1024) return (n / 1024 / 1024).toFixed(1) + ' MB';
  return (n / 1024 / 1024 / 1024).toFixed(2) + ' GB';
}

function fmtDuration(ms) {
  const num = Number(ms);
  if (!isFinite(num) || num < 0) return '-';
  if (num < 1000) return Math.round(num) + ' ms';
  if (num < 60000) return (num / 1000).toFixed(1) + ' s';
  if (num < 3600000) return Math.round(num / 60000) + ' 分钟';
  return (num / 3600000).toFixed(1) + ' 小时';
}

function truncate(text, max) {
  const value = String(text == null ? '' : text);
  const limit = max || 120;
  return value.length <= limit ? value : value.slice(0, limit) + '…';
}

function prettyCell(value) {
  if (value == null) return '<span class="muted-2">—</span>';
  if (typeof value === 'object') return esc(truncate(JSON.stringify(value), 160));
  return esc(truncate(String(value), 200)) || '<span class="muted-2">—</span>';
}

function debounce(fn, wait) {
  let timer = 0;
  return function debounced() {
    const args = arguments;
    const self = this;
    clearTimeout(timer);
    timer = setTimeout(() => fn.apply(self, args), wait);
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

function safeLocalGet(key) {
  try { return localStorage.getItem(key); } catch (e) { return null; }
}
function safeLocalSet(key, value) {
  try { localStorage.setItem(key, value); } catch (e) { /* ignore */ }
}

function copyText(text) {
  const value = String(text == null ? '' : text);
  if (navigator.clipboard && window.isSecureContext) {
    return navigator.clipboard.writeText(value).catch(() => fallbackCopy(value));
  }
  return fallbackCopy(value);
}
function fallbackCopy(value) {
  return new Promise((resolve, reject) => {
    try {
      const area = document.createElement('textarea');
      area.value = value;
      area.setAttribute('readonly', '');
      area.style.position = 'fixed';
      area.style.left = '-9999px';
      document.body.appendChild(area);
      area.select();
      document.execCommand('copy');
      document.body.removeChild(area);
      resolve();
    } catch (err) { reject(err); }
  });
}

function go(hash) {
  if (location.hash === hash) route();
  else location.hash = hash;
}

function hrefOf(hash) { return hash; }

function jsonSum(value) {
  try { return JSON.stringify(value); } catch (e) { return String(value); }
}

/* 轻量 JSON 高亮：先分词再转义，避免二次转义漏洞 */
const JSON_TOKEN_RE = /("(?:\\.|[^"\\])*")(\s*:)?|\b(true|false|null)\b|(-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)/g;

function highlightJson(text) {
  let out = '';
  let last = 0;
  let match;
  JSON_TOKEN_RE.lastIndex = 0;
  while ((match = JSON_TOKEN_RE.exec(text)) !== null) {
    out += esc(text.slice(last, match.index));
    if (match[1] !== undefined) {
      const isKey = !!match[2];
      out += '<span class="' + (isKey ? 'jv-k' : 'jv-s') + '">' + esc(match[1]) + '</span>';
      if (isKey) out += esc(match[2]);
    } else if (match[3] !== undefined) {
      out += '<span class="' + (match[3] === 'null' ? 'jv-z' : 'jv-b') + '">' + match[3] + '</span>';
    } else {
      out += '<span class="jv-n">' + esc(match[4]) + '</span>';
    }
    last = match.index + match[0].length;
  }
  out += esc(text.slice(last));
  return out;
}

function jsonView(value, opts) {
  const options = opts || {};
  let text;
  try {
    text = typeof value === 'string' ? value : JSON.stringify(value, null, 2);
  } catch (err) {
    text = String(value);
  }
  if (text == null) text = '';
  const limit = options.limit || 80000;
  if (text.length > limit) text = text.slice(0, limit) + '\n… 已截断（共 ' + text.length + ' 字符）';
  return '<pre class="json' + (options.small ? ' json-sm' : '') + '">' + highlightJson(text) + '</pre>';
}

function codeBlock(text, opts) {
  const options = opts || {};
  const value = String(text == null ? '' : text);
  const title = options.title ? '<div class="row spread mb-8"><span class="muted text-xs">' + esc(options.title) + '</span></div>' : '';
  return title + '<div class="code-wrap"><pre>' + esc(value) + '</pre>' +
    (options.copy === false ? '' : '<button class="btn btn-ghost btn-sm code-copy" data-copy-text="' + escAttr(value.slice(0, 4000)) + '" type="button">' + icon('copy') + '复制</button>') +
    '</div>';
}

/* 通用剪贴板/复制按钮委托 */
document.addEventListener('click', (event) => {
  const btn = event.target.closest('[data-copy-text],[data-copy-json]');
  if (!btn) return;
  event.preventDefault();
  let text = btn.getAttribute('data-copy-text');
  if (text == null) {
    const raw = btn.getAttribute('data-copy-json');
    try { text = JSON.stringify(JSON.parse(raw), null, 2); } catch (err) { text = raw; }
  }
  copyText(text).then(() => toast('已复制到剪贴板', 'ok', { timeout: 1800 }))
    .catch(() => toast('复制失败，请手动选择复制', 'bad'));
});

/* ── 2. 接口层 ────────────────────────────────────────────────────────── */

class ApiError extends Error {
  constructor(message, meta) {
    super(message || '请求失败');
    this.name = 'ApiError';
    const info = meta || {};
    this.status = info.status || 0;
    this.data = info.data || null;
    this.path = info.path || '';
    this.timeout = !!info.timeout;
  }
}

const API = {
  timeout: 22000,
  redirected: false,
};

function updateNetBadge(online, latencyMs) {
  const badge = $('#net-badge');
  const text = $('#net-text');
  const dot = badge && badge.querySelector('.dot');
  if (!badge) return;
  if (online) {
    if (dot) dot.setAttribute('data-tone', 'ok');
    if (text) text.textContent = latencyMs ? latencyMs + ' ms' : '在线';
    APP.lastLatency = latencyMs || APP.lastLatency;
    const footerLatency = $('#footer-latency');
    if (footerLatency && APP.lastLatency) footerLatency.textContent = 'API ' + APP.lastLatency + ' ms';
  } else {
    if (dot) dot.setAttribute('data-tone', 'bad');
    if (text) text.textContent = '连接异常';
  }
  setSidebarStatus(online ? 'ok' : 'bad', online ? '已连接' : '连接异常');
}

function setSidebarStatus(tone, label) {
  const box = $('#sidebar-status');
  if (!box) return;
  const dot = box.querySelector('.dot');
  if (dot) dot.setAttribute('data-tone', tone);
  const text = $('#sidebar-status-text');
  if (text) text.textContent = label;
}

function redirectToLogin() {
  if (API.redirected) return;
  API.redirected = true;
  toast('会话已过期，正在跳转登录页…', 'warn', { timeout: 2000 });
  const next = '/console' + (location.hash || '');
  setTimeout(() => { location.replace('/login?next=' + encodeURIComponent(next)); }, 400);
}

async function apiFetch(path, opts) {
  const options = opts || {};
  const method = options.method || 'GET';
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
    if (!response.ok) {
      const message = (data && (data.error || data.message)) || ('HTTP ' + response.status);
      updateNetBadge(true, elapsed);
      throw new ApiError(message, { status: response.status, data: data, path: path });
    }
    updateNetBadge(true, elapsed);
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

/* v2 接口统一形状 {ok, data}；旧接口有的直接平铺，这里统一 unwrap */
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

/* 探测新接口：全部 404/501 时返回 pending（等后端接入），其它错误原样返回 */
async function probeApi(paths, opts) {
  const options = opts || {};
  const list = Array.isArray(paths) ? paths : [paths];
  let lastError = null;
  for (let i = 0; i < list.length; i += 1) {
    try {
      const response = await apiFetch(list[i], options);
      return { ok: true, data: unwrap(response), raw: response, path: list[i] };
    } catch (err) {
      if (err.status === 401) throw err;
      lastError = err;
      if (err.status !== 404 && err.status !== 501 && err.status !== 0) {
        return { ok: false, pending: false, error: err, path: list[i] };
      }
    }
  }
  const pending = !lastError || lastError.status === 404 || lastError.status === 501;
  return { ok: false, pending: pending, error: lastError, path: list[list.length - 1] };
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
};

async function getGroups(force) {
  const now = Date.now();
  if (!force && Store.groups && now - Store.groupsAt < 30000) return Store.groups;
  const data = unwrap(await apiGet('/api/v2/groups'));
  Store.groups = (data && data.groups) || [];
  Store.groupsAt = now;
  return Store.groups;
}

async function getHumanlike(force) {
  const now = Date.now();
  if (!force && Store.humanlike && now - Store.humanlikeAt < 15000) return Store.humanlike;
  const data = unwrap(await apiGet('/api/v2/humanlike'));
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

/* ── 4. UI 原语 ───────────────────────────────────────────────────────── */

const TOAST_ICONS = { info: 'info', ok: 'check', warn: 'alert', bad: 'alert' };

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

/* 通用模态：返回 { close }；内容由调用方给 */
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
  const api = { el: overlay, body: body, foot: foot };
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

/* 危险操作确认：可要求输入指定文本 */
function confirmAction(config) {
  const cfg = config || {};
  return new Promise((resolve) => {
    let settled = false;
    let outcome = false;
    const finish = (value) => { if (!settled) { settled = true; resolve(!!value); } };
    const needText = cfg.requireText || '';
    const bodyHtml = needText
      ? '<div class="field"><label>输入 <code class="inline">' + esc(needText) + '</code> 以继续</label>' +
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
    $('#drawer-title').innerHTML = cfg.title || '详情';
    $('#drawer-sub').innerHTML = cfg.sub || '';
    $('#drawer-body').innerHTML = cfg.body || '';
    const foot = $('#drawer-foot');
    if (cfg.foot) { foot.innerHTML = cfg.foot; foot.hidden = false; } else { foot.innerHTML = ''; foot.hidden = true; }
    Drawer.raw = cfg.raw === undefined ? null : cfg.raw;
    $('#drawer-copy').style.display = Drawer.raw == null ? 'none' : '';
    drawer.classList.toggle('wide', !!cfg.wide);
    drawer.hidden = false;
    document.body.style.overflow = 'hidden';
    if (cfg.onMount) cfg.onMount($('#drawer-body'), drawer);
    const closeBtn = drawer.querySelector('[data-drawer-close]');
    if (closeBtn) closeBtn.focus({ preventScroll: true });
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
      body: jsonView(value),
      raw: value,
      wide: true,
    });
  },
};

function openJsonDrawer(title, value, sub) { Drawer.json(title, value, sub); }

/* 复制当前抽屉 JSON */
document.addEventListener('click', (event) => {
  if (!event.target.closest('#drawer-copy')) return;
  if (Drawer.raw == null) return;
  copyText(JSON.stringify(Drawer.raw, null, 2)).then(() => toast('已复制 JSON', 'ok', { timeout: 1600 }))
    .catch(() => toast('复制失败', 'bad'));
});

document.addEventListener('click', (event) => {
  if (!event.target.closest('[data-drawer-close]')) return;
  Drawer.close();
});

/* ── 通用渲染件 ───────────────────────────────────────────────────────── */

function pageHead(config) {
  const cfg = config || {};
  return '<div class="page-head">' +
    '<div class="page-head-main">' +
      '<h1 class="page-title">' + (cfg.icon ? icon(cfg.icon) : '') + esc(cfg.title || '') + '</h1>' +
      (cfg.sub ? '<div class="page-sub">' + (cfg.subHtml ? cfg.sub : esc(cfg.sub)) + '</div>' : '') +
    '</div>' +
    (cfg.actions && cfg.actions.length ? '<div class="page-actions">' + cfg.actions.join('') + '</div>' : '') +
    '</div>';
}

function card(config, bodyHtml) {
  const cfg = config || {};
  const head = (cfg.title || cfg.headActions)
    ? '<div class="card-head"><h3>' + (cfg.icon ? icon(cfg.icon) : '') + esc(cfg.title || '') +
      (cfg.sub ? '<span class="section-sub">' + esc(cfg.sub) + '</span>' : '') + '</h3>' +
      (cfg.headActions ? '<div class="card-head-actions">' + cfg.headActions.join('') + '</div>' : '') + '</div>'
    : '';
  const body = bodyHtml == null || bodyHtml === '' ? '' : '<div class="card-body' + (cfg.flush ? ' flush' : '') + '">' + bodyHtml + '</div>';
  return '<section class="card ' + (cfg.className || '') + '">' + head + body + (cfg.foot || '') + '</section>';
}

function kpiGrid(items) {
  const cards = (items || []).map((item) => {
    const inner =
      '<div class="kpi-top">' + (item.icon ? icon(item.icon) : '') + '<span>' + esc(item.label) + '</span>' +
      (item.badge || '') + '</div>' +
      '<div class="kpi-value ' + (item.tone ? '' : '') + '">' + (item.valueHtml == null ? esc(item.value == null ? '-' : item.value) : item.valueHtml) +
      (item.unit ? '<small>' + esc(item.unit) + '</small>' : '') + '</div>' +
      (item.hint ? '<div class="kpi-hint">' + item.hint + '</div>' : '');
    const className = 'kpi' + (item.tone ? ' tone-' + item.tone : '');
    if (item.href) return '<a class="' + className + '" href="' + escAttr(item.href) + '">' + inner + '</a>';
    return '<div class="' + className + '">' + inner + '</div>';
  });
  return '<div class="kpis">' + cards.join('') + '</div>';
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
  const iconName = kind === 'warn' ? 'alert' : kind === 'bad' ? 'alert' : kind === 'ok' ? 'check' : 'info';
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

/* 数据表：支持行点击展开（详情行由 detail 回调渲染） */
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
  const colCount = columns.length + (expandable || cfg.index ? 1 : 0);
  const head = [];
  if (expandable || cfg.index) head.push('<th style="width:34px"></th>');
  columns.forEach((col) => {
    const style = col.width ? ' style="width:' + escAttr(col.width) + '"' : '';
    head.push('<th' + style + ' class="' + clsx({ num: col.num, mono: col.mono, 'hide-sm': col.hideSm }) + '">' + esc(col.label) + '</th>');
  });
  const body = rows.map((row, index) => {
    const cells = [];
    if (expandable || cfg.index) {
      cells.push('<td class="nowrap">' + (expandable ? '<span class="row-caret">' + icon('chevron-right') + '</span>' : '<span class="idx">' + (index + 1) + '</span>') + '</td>');
    }
    columns.forEach((col) => {
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
  container.innerHTML = '<div class="table-wrap"><table class="table"><thead><tr>' + head.join('') +
    '</tr></thead><tbody>' + body + '</tbody></table></div>';

  const toggleRow = (tr) => {
    const index = Number(tr.getAttribute('data-row'));
    const detail = container.querySelector('tr.dt-detail[data-detail="' + index + '"]');
    if (!detail) return;
    const open = detail.hidden;
    detail.hidden = !open;
    tr.classList.toggle('open', open);
    tr.setAttribute('aria-expanded', open ? 'true' : 'false');
    if (open && cfg.onExpand) cfg.onExpand(rows[index], detail, index);
  };

  if (cfg.onRow) {
    container.addEventListener('click', (event) => {
      const tr = event.target.closest('tr.dt-row');
      if (!tr || !container.contains(tr)) return;
      if (event.target.closest('a,button,input,select,textarea')) return;
      const row = rows[Number(tr.getAttribute('data-row'))];
      cfg.onRow(row, tr, Number(tr.getAttribute('data-row')));
    });
  }
  if (expandable) {
    container.addEventListener('click', (event) => {
      const tr = event.target.closest('tr.dt-row');
      if (!tr || !container.contains(tr)) return;
      if (event.target.closest('a,button,input,select,textarea')) return;
      toggleRow(tr);
    });
    container.addEventListener('keydown', (event) => {
      if (event.key !== 'Enter' && event.key !== ' ') return;
      const tr = event.target.closest('tr.dt-row');
      if (!tr) return;
      event.preventDefault();
      toggleRow(tr);
    });
  }
}

/* 单行详情抽屉入口（表格数据行 → JSON） */
function rowJsonDrawer(title, row, sub) {
  return function open() { openJsonDrawer(title, row, sub); };
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
  return '<div class="sk sk-title"></div><div class="sk sk-block"></div><div class="sk sk-block" style="height:150px"></div>';
}

/* 统一错误渲染（带重试） */
function showViewError(root, err, retry) {
  root.innerHTML = pageHead({ title: '加载失败', icon: 'alert' }) + errorState(err, 'view-retry');
  const btn = $('#view-retry', root);
  if (btn) btn.addEventListener('click', () => { if (retry) retry(); else route(); });
}

/* ── 6. 图表封装 ───────────────────────────────────────────────────────── */

const CHARTS = [];
const CHART_COLORS = ['#5b8cff', '#8b5cf6', '#34d399', '#fbbf24', '#f87171', '#38bdf8', '#f472b6', '#a3e635', '#fb923c', '#22d3ee'];

function disposeCharts() {
  while (CHARTS.length) {
    const chart = CHARTS.pop();
    try { chart.dispose(); } catch (err) { /* ignore */ }
  }
}

function chartFont() { return getComputedStyle(document.body).fontFamily || 'sans-serif'; }

function chartBase(overrides) {
  const base = {
    backgroundColor: 'transparent',
    color: CHART_COLORS,
    textStyle: { color: '#8b98ac', fontFamily: chartFont(), fontSize: 11 },
    tooltip: {
      trigger: 'axis',
      backgroundColor: 'rgba(15,19,26,.96)',
      borderColor: '#2e3949',
      borderWidth: 1,
      textStyle: { color: '#e9eef7', fontSize: 11.5 },
      axisPointer: { type: 'line', lineStyle: { color: '#3a4658' } },
      extraCssText: 'box-shadow:0 12px 32px -8px rgba(0,0,0,.65);border-radius:8px;',
    },
    grid: { left: 8, right: 14, top: 22, bottom: 6, containLabel: true },
    legend: {
      top: 0, right: 0, itemWidth: 9, itemHeight: 9, itemGap: 12,
      textStyle: { color: '#8b98ac', fontSize: 11 }, icon: 'roundRect',
    },
    xAxis: {
      type: 'category',
      axisLine: { lineStyle: { color: '#222b38' } },
      axisTick: { show: false },
      axisLabel: { color: '#66748a', fontSize: 10.5, hideOverlap: true },
      splitLine: { show: false },
    },
    yAxis: {
      type: 'value',
      axisLine: { show: false },
      axisTick: { show: false },
      axisLabel: { color: '#66748a', fontSize: 10.5 },
      splitLine: { lineStyle: { color: 'rgba(34,43,56,.65)', type: 'dashed' } },
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
    '<div class="muted-2 text-xs">（ECharts 未能加载，已降级为条形列表）</div></div>';
}

/* 挂载 ECharts；容器需已在文档中且有宽度 */
function mountChart(container, option, fallbackItems) {
  if (!container) return null;
  const el = typeof container === 'string' ? $(container) : container;
  if (!el) return null;
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

function chartNoData(container, text) {
  const el = typeof container === 'string' ? $(container) : container;
  if (!el) return;
  el.innerHTML = '<div class="empty" style="padding:26px 16px"><div class="empty-ico">' + icon('chart') + '</div><b>暂无数据</b>' +
    (text ? '<p>' + esc(text) + '</p>' : '') + '</div>';
}

let chartResizeTimer = 0;
window.addEventListener('resize', () => {
  clearTimeout(chartResizeTimer);
  chartResizeTimer = setTimeout(() => {
    CHARTS.forEach((chart) => { try { chart.resize(); } catch (err) { /* ignore */ } });
  }, 160);
});


/* ── 7. 导航 / 路由 / 命令面板 / 启动 ───────────────────────────────────── */

const VIEWS = {};

const NAV = [
  {
    label: '总览',
    items: [{ id: 'dashboard', name: '仪表盘', icon: 'dash' }],
  },
  {
    label: '社交流量',
    items: [
      { id: 'groups', name: '群与房间', icon: 'groups' },
      { id: 'members', name: '成员', icon: 'members' },
      { id: 'messages', name: '消息搜索', icon: 'search' },
    ],
  },
  {
    label: '机器人',
    items: [
      { id: 'humanlike', name: '拟人模式', icon: 'spark' },
      { id: 'actions', name: '操作台', icon: 'wand' },
      { id: 'models', name: '模型与配置', icon: 'cpu' },
      { id: 'tools', name: '工具与审计', icon: 'wrench' },
    ],
  },
  {
    label: '数据',
    items: [
      { id: 'business', name: '业务数据', icon: 'book' },
      { id: 'browse', name: '数据浏览器', icon: 'database' },
    ],
  },
  {
    label: '系统',
    items: [
      { id: 'logs', name: '日志', icon: 'terminal' },
      { id: 'deploy', name: '部署与运维', icon: 'server' },
      { id: 'settings', name: '设置', icon: 'gear' },
    ],
  },
];

const NAV_INDEX = {};
NAV.forEach((section) => {
  section.items.forEach((item) => { NAV_INDEX[item.id] = Object.assign({ section: section.label }, item); });
});

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

function renderNav(activeId) {
  const root = $('#nav');
  if (!root) return;
  root.innerHTML = NAV.map((section) =>
    '<div class="nav-group">' +
      '<div class="nav-label">' + esc(section.label) + '</div>' +
      section.items.map((item) =>
        '<a class="nav-item' + (item.id === activeId ? ' active' : '') + '" href="' + escAttr('#/' + item.id) + '" data-nav="' + escAttr(item.id) + '">' +
          icon(item.icon) + '<span>' + esc(item.name) + '</span>' +
          (item.id === 'groups' || item.id === 'humanlike' ? '<span class="nav-badge" data-nav-badge="' + item.id + '"></span>' : '') +
        '</a>').join('') +
    '</div>').join('');
  refreshNavBadges();
}

function renderCrumbs(route) {
  const root = $('#crumbs');
  if (!root) return;
  const view = VIEWS[route.viewId] || VIEWS.dashboard;
  const meta = NAV_INDEX[view.id] || NAV_INDEX.dashboard;
  const parts = [];
  parts.push('<a href="#/dashboard" aria-label="仪表盘">' + icon('dash') + '</a>');
  if (meta.section && meta.section !== '总览') parts.push('<span class="crumb-hide">' + esc(meta.section) + '</span><span class="sep">' + icon('chevron-right') + '</span>');
  const paramCrumb = view.crumb ? view.crumb(route) : null;
  if (paramCrumb) {
    parts.push('<a href="#/' + view.id + '">' + esc(view.name) + '</a>');
    parts.push('<span class="sep">' + icon('chevron-right') + '</span>');
    parts.push('<span class="current mono">' + esc(paramCrumb) + '</span>');
  } else {
    parts.push('<span class="current">' + esc(view.name) + '</span>');
  }
  root.innerHTML = parts.join('');
}

function setDocumentTitle(view, route) {
  const param = view.crumb ? view.crumb(route) : null;
  document.title = (view.name || 'Console') + (param ? ' · ' + param : '') + ' · ' + APP.title;
}

function makeCtx(route) {
  const token = ++APP.routeToken;
  const root = $('#main');
  return {
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
    every(ms, fn) {
      const id = setInterval(() => {
        if (!APP.autoRefresh || document.hidden) return;
        if (!this.alive()) return;
        try { fn(); } catch (err) { /* 轮询失败不打扰 */ }
      }, ms);
      registerCleanup(() => clearInterval(id));
      return id;
    },
  };
}

let lastRouteKey = '';
let lastRouteAt = 0;

async function route() {
  const parsed = parseRoute();
  const view = VIEWS[parsed.viewId] || VIEWS.dashboard;
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

  renderNav(view.id);
  renderCrumbs(normalized);
  setDocumentTitle(view, normalized);
  updateAutoRefreshButton();

  const ctx = makeCtx(normalized);
  const root = $('#main');
  root.innerHTML = skeletonPage(view.skeleton || 'default');
  RootScroller.restore = sameRouteRefresh ? prevScroll : 0;
  if (!sameRouteRefresh) window.scrollTo({ top: 0 });
  try {
    await view.render(ctx);
  } catch (err) {
    if (!ctx.alive()) return;
    if (err instanceof ApiError && err.status === 401) return;
    console.error('[console] 视图渲染失败', view.id, err);
    showViewError(root, err, () => route());
  }
  if (ctx.alive() && sameRouteRefresh) window.scrollTo({ top: RootScroller.restore });
}

const RootScroller = { restore: 0 };

/* ── 命令面板 ─────────────────────────────────────────────────────────── */

const Palette = {
  open: false,
  items: [],
  active: 0,
  seq: 0,
  query: '',
  loading: false,

  staticItems() {
    const items = [];
    NAV.forEach((section) => {
      section.items.forEach((item) => {
        items.push({
          group: '页面 · ' + section.label,
          title: item.name,
          sub: '#/' + item.id,
          icon: item.icon,
          keywords: item.id,
          run: () => go('#/' + item.id),
        });
        if (item.id === 'business') {
          [['文库', 'library'], ['赛文', 'contest'], ['成绩', 'scores'], ['番茄', 'novel']].forEach((pair) => {
            items.push({ group: '页面 · 数据', title: '业务数据 · ' + pair[0], sub: '#/business/' + pair[1], icon: 'book', keywords: pair[1], run: () => go('#/business/' + pair[1]) });
          });
        }
        if (item.id === 'groups') {
          items.push({ group: '页面 · 社交流量', title: '群与房间 · 全部会话', sub: '#/groups', icon: 'groups', keywords: 'group list', run: () => go('#/groups') });
        }
      });
    });
    items.push(
      { group: '快捷操作', title: '刷新当前页面', sub: 'r', icon: 'refresh', keywords: 'refresh reload', run: () => route() },
      { group: '快捷操作', title: '打开旧版后台', sub: '/classic', icon: 'external', keywords: 'classic legacy', run: () => window.open('/classic', '_blank', 'noopener') },
      { group: '快捷操作', title: '打开状态页 /now', sub: '/now', icon: 'activity', keywords: 'now status', run: () => window.open('/now', '_blank', 'noopener') },
      { group: '快捷操作', title: '接口调试器', sub: '#/browse', icon: 'database', keywords: 'api debug raw', run: () => go('#/browse') },
      { group: '快捷操作', title: '退出登录', sub: 'logout', icon: 'logout', keywords: 'logout signout', run: () => doLogout() },
    );
    return items;
  },

  init() {
    const input = $('#palette-input');
    if (!input) return;
    input.addEventListener('input', () => {
      const value = input.value.trim();
      Palette.query = value;
      Palette.active = 0;
      Palette.renderStatic();
      if (value.length >= 1) Palette.searchRemote(value);
    });
    input.addEventListener('keydown', (event) => {
      if (event.key === 'ArrowDown') { event.preventDefault(); Palette.move(1); }
      else if (event.key === 'ArrowUp') { event.preventDefault(); Palette.move(-1); }
      else if (event.key === 'Enter') { event.preventDefault(); Palette.choose(Palette.active); }
      else if (event.key === 'Escape') { event.preventDefault(); Palette.close(); }
    });
    $('#palette-close').addEventListener('click', () => Palette.close());
    $('#palette').addEventListener('mousedown', (event) => {
      if (event.target.id === 'palette') Palette.close();
    });
  },

  openWith(query) {
    const overlay = $('#palette');
    if (!overlay) return;
    overlay.hidden = false;
    Palette.open = true;
    document.body.style.overflow = 'hidden';
    const input = $('#palette-input');
    input.value = query || '';
    Palette.query = query || '';
    Palette.active = 0;
    Palette.loading = false;
    Palette.renderStatic();
    setTimeout(() => input.focus(), 20);
    if ((query || '').length >= 1) Palette.searchRemote(query);
  },

  close() {
    const overlay = $('#palette');
    if (!overlay || overlay.hidden) return;
    overlay.hidden = true;
    Palette.open = false;
    Palette.seq += 1;
    document.body.style.overflow = Drawer.isOpen() ? 'hidden' : '';
  },

  renderStatic() {
    const keyword = Palette.query.trim().toLowerCase();
    const all = Palette.staticItems();
    const filtered = keyword
      ? all.filter((item) => (item.title + ' ' + item.sub + ' ' + (item.keywords || '')).toLowerCase().indexOf(keyword) >= 0)
      : all.slice(0, 9);
    Palette.items = filtered;
    Palette.renderList(Palette.items, Palette.loading);
  },

  async searchRemote(keyword) {
    const seq = ++Palette.seq;
    Palette.loading = true;
    Palette.renderList(Palette.items, true);
    let data = null;
    try {
      data = unwrap(await apiGet('/api/search' + qs({ q: keyword }), { timeout: 12000 }));
    } catch (err) {
      if (seq === Palette.seq) { Palette.loading = false; Palette.renderList(Palette.items, false); }
      return;
    }
    if (seq !== Palette.seq || !Palette.open) return;
    Palette.loading = false;
    const remote = [];
    const push = (group, iconName, title, sub, run, extra) => {
      if (remote.length >= 24) return;
      remote.push({ group: group, icon: iconName, title: title, sub: sub, run: run, extra: extra });
    };
    ((data && data.groups) || []).forEach((group) => {
      push('群', 'groups', group.display_name || group.group_name || group.group_id, String(group.group_id || ''), () => go('#/groups/' + encodeURIComponent(group.group_id)));
    });
    ((data && data.users) || []).forEach((user) => {
      const name = user.sender_name || user.sender_id;
      push('成员', 'members', name, 'QQ ' + (user.sender_id || ''), () => go('#/members?' + qs({ q: name }).slice(1)));
    });
    ((data && data.messages) || []).forEach((message) => {
      push('消息', 'message', truncate(message.text || '(无文本)', 46), (message.sender_name || '') + ' · ' + (message.chat_id || ''), () => {
        openJsonDrawer('消息详情', message);
      });
    });
    ((data && data.library) || []).forEach((row) => {
      push('文库', 'book', row.title || row.text_id, row.category || '', () => go('#/business/library?' + qs({ q: row.title || '' }).slice(1)));
    });
    ((data && data.contests) || []).forEach((row) => {
      push('赛文', 'book', row.title || row.text_id, row.source_group || '', () => go('#/business/contest?' + qs({ q: row.title || '' }).slice(1)));
    });
    ((data && data.scores) || []).forEach((row) => {
      push('成绩', 'chart', (row.sender_name || row.segment_id || '成绩') + ' · ' + (row.speed != null ? row.speed + ' 字/分' : ''), row.group_name || '', () => {
        go('#/business/scores?' + qs({ q: row.sender_name || '' }).slice(1));
      });
    });
    if (!remote.length) {
      push('搜索', 'search', '在消息搜索里查找「' + keyword + '」', '回车打开消息搜索页', () => go('#/messages?' + qs({ q: keyword }).slice(1)));
    }
    Palette.items = Palette.items.concat(remote);
    Palette.active = 0;
    Palette.renderList(Palette.items, false);
  },

  renderList(items, loading) {
    const root = $('#palette-list');
    if (!root) return;
    if (!items.length && !loading) {
      root.innerHTML = '<div class="palette-empty">没有匹配项<br><span class="text-xs">试试群号、成员昵称、消息关键词</span></div>';
      return;
    }
    let lastGroup = '';
    const html = [];
    items.forEach((item, index) => {
      if (item.group !== lastGroup) {
        lastGroup = item.group;
        html.push('<div class="palette-group">' + esc(item.group) + '</div>');
      }
      html.push('<div class="palette-item' + (index === Palette.active ? ' active' : '') + '" data-palette-index="' + index + '">' +
        icon(item.icon || 'search') +
        '<div class="pi-main"><div class="pi-title">' + esc(item.title) + '</div>' +
        (item.sub ? '<div class="pi-sub">' + esc(item.sub) + '</div>' : '') + '</div>' +
        (item.extra ? '<span class="pi-extra">' + esc(item.extra) + '</span>' : '') +
        '</div>');
    });
    if (loading) {
      html.push('<div class="palette-item" style="cursor:default"><span class="palette-spinner"></span><span class="muted">正在跨库搜索…</span></div>');
    }
    root.innerHTML = html.join('');
    root.querySelectorAll('[data-palette-index]').forEach((node) => {
      node.addEventListener('click', () => Palette.choose(Number(node.getAttribute('data-palette-index'))));
    });
    const active = root.querySelector('.palette-item.active');
    if (active && active.scrollIntoView) active.scrollIntoView({ block: 'nearest' });
  },

  move(delta) {
    if (!Palette.items.length) return;
    Palette.active = (Palette.active + delta + Palette.items.length) % Palette.items.length;
    Palette.renderList(Palette.items, Palette.loading);
  },

  choose(index) {
    const item = Palette.items[index];
    if (!item) return;
    Palette.close();
    try { item.run(); } catch (err) { toast('跳转失败：' + (err.message || err), 'bad'); }
  },
};

/* ── 退出 / 自动刷新 / 全局按键 ───────────────────────────────────────── */

async function doLogout() {
  const ok = await confirmAction({
    title: '退出登录？',
    detail: '退出后需要重新输入账号密码；服务端会话会立即失效。',
    confirmLabel: '退出登录',
    danger: true,
  });
  if (!ok) return;
  try { await apiPost('/api/logout', {}); } catch (err) { /* 会话可能已过期 */ }
  location.replace('/login');
}

function updateAutoRefreshButton() {
  const btn = $('#auto-refresh');
  const text = $('#auto-refresh-text');
  if (!btn) return;
  btn.classList.toggle('on', APP.autoRefresh);
  btn.setAttribute('aria-pressed', APP.autoRefresh ? 'true' : 'false');
  if (text) text.textContent = APP.autoRefresh ? '自动刷新 · 开' : '自动刷新';
}

function toggleAutoRefresh() {
  APP.autoRefresh = !APP.autoRefresh;
  safeLocalSet('fm-console-auto-refresh', APP.autoRefresh ? '1' : '0');
  updateAutoRefreshButton();
  toast(APP.autoRefresh ? '已开启自动刷新（10 秒，标签页隐藏时暂停）' : '已关闭自动刷新', APP.autoRefresh ? 'ok' : 'info');
}

function bindGlobalUi() {
  const navOpen = $('#nav-open');
  const navClose = $('#nav-close');
  const backdrop = $('#nav-backdrop');
  const closeNav = () => document.body.classList.remove('nav-open');
  if (navOpen) navOpen.addEventListener('click', () => {
    document.body.classList.add('nav-open');
    if (backdrop) backdrop.hidden = false;
    $('#nav') && $('#nav').setAttribute('data-open', '1');
  });
  const doCloseNav = () => { closeNav(); if (backdrop) backdrop.hidden = true; };
  if (navClose) navClose.addEventListener('click', doCloseNav);
  if (backdrop) backdrop.addEventListener('click', doCloseNav);
  document.addEventListener('click', (event) => {
    const jump = event.target.closest('[data-goto]');
    if (jump) { event.preventDefault(); go(jump.getAttribute('data-goto')); }
    const jsonButton = event.target.closest('[data-json-drawer]');
    if (jsonButton) {
      event.preventDefault();
      const raw = jsonButton.getAttribute('data-json-drawer');
      try { openJsonDrawer(jsonButton.getAttribute('data-json-title') || '原始响应', JSON.parse(raw)); }
      catch (err) { openJsonDrawer('原始响应', raw); }
    }
    const navItem = event.target.closest('[data-nav]');
    if (navItem && window.innerWidth <= 900) doCloseNav();
    if (event.target.closest('[data-retry]')) route();
  });

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
  const userChip = $('#user-chip');
  if (userChip) userChip.addEventListener('click', (event) => { event.preventDefault(); go('#/settings'); });

  document.addEventListener('keydown', (event) => {
    const tag = (event.target && event.target.tagName ? event.target.tagName : '').toLowerCase();
    const typing = tag === 'input' || tag === 'textarea' || tag === 'select' || (event.target && event.target.isContentEditable);
    const metaK = (event.ctrlKey || event.metaKey) && (event.key === 'k' || event.key === 'K');
    if (metaK) {
      event.preventDefault();
      if (Palette.open) Palette.close(); else Palette.openWith('');
      return;
    }
    if (event.key === 'Escape') {
      if (Palette.open) { Palette.close(); return; }
      if (Drawer.isOpen()) { Drawer.close(); return; }
    }
    if (typing) return;
    if (event.key === '/' && !event.ctrlKey && !event.metaKey && !event.altKey) {
      event.preventDefault();
      Palette.openWith('');
      return;
    }
    if ((event.key === 'r' || event.key === 'R') && !event.ctrlKey && !event.metaKey) {
      if (Drawer.isOpen() || Palette.open) return;
      event.preventDefault();
      route();
    }
  });

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
    if (event.message.indexOf('ResizeObserver') >= 0) return;
    console.error('[console] 运行时错误', event.message);
  });
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
    const me = await apiGet('/api/auth/me');
    APP.user = me;
    const nameNode = $('#user-name');
    if (nameNode) nameNode.textContent = (me && me.username) || 'admin';
    setSidebarStatus('ok', '已登录 · ' + ((me && me.username) || 'admin'));
  } catch (err) {
    if (err instanceof ApiError && err.status === 401) return;
    setSidebarStatus('bad', '身份校验失败');
    const nameNode = $('#user-name');
    if (nameNode) nameNode.textContent = 'admin';
  }
}

async function boot() {
  bindGlobalUi();
  Palette.init();
  startFooterClock();
  updateAutoRefreshButton();
  renderNav('dashboard');
  await loadIdentity();
  await route();
  getGroups().then(refreshNavBadges).catch(() => {});
  getHumanlike().then(refreshNavBadges).catch(() => {});
}

window.addEventListener('DOMContentLoaded', () => { boot(); });


/* ══════════════════════════════════════════════════════════════════════════
 * 7.1 仪表盘
 * 数据源：/api/v2/overview、/api/observability/overview、/api/health/score、
 *         /api/v2/groups、/api/v2/charts/messages(新)、/api/v2/charts/tools(新)、
 *         /api/v2/top/groups(新)、/api/v2/messages(降级采样)
 * ════════════════════════════════════════════════════════════════════════ */

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

function pickArray(value, keys) {
  if (Array.isArray(value)) return value;
  if (!value || typeof value !== 'object') return [];
  const names = keys || ['rows', 'items', 'data', 'list', 'results', 'days', 'series', 'trend', 'points'];
  for (let i = 0; i < names.length; i += 1) {
    const candidate = value[names[i]];
    if (Array.isArray(candidate)) return candidate;
  }
  return [];
}

function firstDefined(row, keys) {
  for (let i = 0; i < keys.length; i += 1) {
    const value = row[keys[i]];
    if (value !== undefined && value !== null && value !== '') return value;
  }
  return null;
}

/* 把后端返回的各种形状归一成 [{label, count, bot}]，供折线/柱状图使用 */
function normalizeDailySeries(payload) {
  const rows = pickArray(payload);
  const out = [];
  rows.forEach((row) => {
    if (row == null) return;
    if (typeof row === 'number') { out.push({ label: '', count: row, bot: null }); return; }
    if (Array.isArray(row)) {
      out.push({ label: row[0] == null ? '' : String(row[0]), count: Number(row[1] || 0), bot: row[2] == null ? null : Number(row[2]) });
      return;
    }
    if (typeof row !== 'object') return;
    const labelValue = firstDefined(row, ['date', 'day', 'label', 'ts', 'time', 'bucket', 'hour']);
    const count = Number(firstDefined(row, ['count', 'total', 'messages', 'value', 'n', 'cnt']) || 0);
    const botValue = firstDefined(row, ['bot', 'bot_count', 'bot_messages', 'botCount']);
    out.push({
      label: labelValue == null ? '' : String(labelValue),
      count: count,
      bot: botValue == null ? null : Number(botValue),
    });
  });
  return out;
}

function minutesAgo(ms) { return Date.now() - ms; }

VIEWS.dashboard = {
  id: 'dashboard',
  name: '仪表盘',
  icon: 'dash',
  skeleton: 'default',
  async render(ctx) {
    const root = ctx.root;
    const [overviewRes, obsRes, scoreRes, groupsRes, chartsMsgRes, chartsToolsRes, topGroupsRes] = await Promise.all([
      apiGet('/api/v2/overview').catch((err) => ({ __error: err })),
      apiGet('/api/observability/overview').catch((err) => ({ __error: err })),
      apiGet('/api/health/score').catch(() => null),
      apiGet('/api/v2/groups').catch(() => null),
      probeApi(['/api/v2/charts/messages' + qs({ days: 14 }), '/api/v2/charts/messages']),
      probeApi(['/api/v2/charts/tools' + qs({ days: 14 }), '/api/v2/tools/stats' + qs({ days: 14 })]),
      probeApi(['/api/v2/top/groups' + qs({ limit: 8 })]),
    ]);
    if (!ctx.alive()) return;

    const overview = overviewRes && overviewRes.__error ? null : unwrap(overviewRes);
    const obs = obsRes && obsRes.__error ? null : obsRes;
    const score = scoreRes ? (scoreRes.data || scoreRes) : null;
    const groups = groupsRes ? (unwrap(groupsRes).groups || []) : [];
    Store.groups = groups.length ? groups : Store.groups;
    Store.groupsAt = groups.length ? Date.now() : Store.groupsAt;

    const chat = (overview && overview.chat) || {};
    const humanlike = (overview && overview.humanlike) || {};
    const tools = (overview && overview.tools) || {};
    const stats = (overview && overview.stats) || {};
    const config = (overview && overview.config) || { base: {}, chat_models: {}, image_models: {} };
    const health = (overview && overview.health) || {};
    const tasks = (obs && obs.tasks) || {};
    const errors = (obs && obs.recent_errors) || [];
    const configSync = (obs && obs.config_sync) || {};
    const domainOk = !!(obs && obs.domain && obs.domain.ok);
    const rpcOk = !!(obs && obs.rpc && obs.rpc.ok);
    const healthScore = score ? Number(score.score || 0) : null;
    const issues = (score && score.issues) || [];

    const msgRows = normalizeDailySeries(chartsMsgRes.ok ? chartsMsgRes.data : null);
    const toolRows = normalizeDailySeries(chartsToolsRes.ok ? chartsToolsRes.data : null);
    const topGroups = (function () {
      if (topGroupsRes.ok) {
        const rows = pickArray(topGroupsRes.data);
        if (rows.length) {
          return {
            pending: false,
            rows: rows.map((row) => ({
              label: String(firstDefined(row, ['name', 'display_name', 'group_name', 'group_id', 'id']) || ''),
              value: Number(firstDefined(row, ['messages', 'count', 'total', 'value', 'n']) || 0),
            })).filter((row) => row.label),
          };
        }
      }
      const fallback = groups.slice().sort((a, b) => Number(b.messages || 0) - Number(a.messages || 0)).slice(0, 8)
        .map((group) => ({ label: group.name || group.group_id, value: Number(group.messages || 0) }));
      return { pending: topGroupsRes.pending, rows: fallback, fallback: true };
    })();

    const toolsStats = tools.rows || [];
    const failures = toolsStats.reduce((sum, row) => sum + Number(row.failures || 0), 0);
    const nowMs = Date.now();
    const lastAt = chat.last_at;
    const recentErrors24h = errors.filter((item) => {
      const ms = toMs(item.at);
      return ms != null && nowMs - ms <= 86400000;
    }).length;

    const scoreTone = healthScore == null ? 'muted' : healthScore >= 90 ? 'ok' : healthScore >= 70 ? 'warn' : 'bad';
    const msgChartTitle = msgRows.length ? '消息量趋势 · 近 14 天' : '消息量趋势 · 采样（最近 200 条 / 按小时）';
    const scoreRing = (function () {
      if (healthScore == null) return '<div class="health-ring"><div><div class="ring-num muted">—</div><div class="ring-meta">健康分不可用</div></div></div>';
      const color = healthScore >= 90 ? 'var(--ok)' : healthScore >= 70 ? 'var(--warn)' : 'var(--bad)';
      const dash = Math.max(0, Math.min(100, healthScore));
      return '<div class="health-ring">' +
        '<svg width="74" height="74" viewBox="0 0 74 74" aria-hidden="true" style="flex:0 0 auto">' +
          '<circle cx="37" cy="37" r="30" fill="none" stroke="#1f2836" stroke-width="8"/>' +
          '<circle cx="37" cy="37" r="30" fill="none" stroke="' + color + '" stroke-width="8" stroke-linecap="round" ' +
            'stroke-dasharray="' + (dash * 1.885) + ' 188.5" transform="rotate(-90 37 37)"/>' +
          '<text x="37" y="43" text-anchor="middle" font-size="19" font-weight="700" fill="#e9eef7">' + healthScore + '</text>' +
        '</svg>' +
        '<div><div class="ring-meta" style="font-size:12px;color:#c5cfdd">系统健康分</div>' +
        '<div class="ring-meta">' + (issues.length ? issues.length + ' 项待处理' : '各组件均正常') + '</div>' +
        '<div class="ring-meta text-xs">最近 1 小时错误 ' + fmtNum(recentErrors24h) + ' 条</div></div></div>';
    })();

    root.innerHTML = pageHead({
      title: '仪表盘',
      icon: 'dash',
      sub: '机器人运行状态、消息量、工具调用与数据规模一屏总览 · 更新时间 ' + fmtTime(Date.now()),
      actions: [
        '<a class="btn btn-ghost btn-sm" href="#/groups">' + icon('groups') + '群与房间</a>',
        '<a class="btn btn-ghost btn-sm" href="#/logs">' + icon('terminal') + '日志</a>',
        '<button class="btn btn-ghost btn-sm" id="dash-refresh" type="button">' + icon('refresh') + '刷新</button>',
      ],
    }) +
    kpiGrid([
      { label: '消息总数', icon: 'message', valueHtml: fmtNum(chat.messages), tone: 'acc', hint: '最近一条 ' + (lastAt ? esc(ago(lastAt)) : '无记录') },
      { label: '机器人消息', icon: 'robot', valueHtml: fmtNum(chat.bot_messages), hint: '占比 ' + fmtPct(chat.bot_messages, chat.messages) },
      { label: '发言人数', icon: 'members', valueHtml: fmtNum(chat.senders), hint: '去重后的人类成员' },
      { label: '拟人会话', icon: 'spark', valueHtml: fmtNum(humanlike.scope_count), hint: humanlike.scope_count ? '已开启拟人模式' : '未开启任何会话' },
      { label: '工具调用 · 7d', icon: 'wrench', valueHtml: fmtNum(tools.total_calls), hint: '失败 ' + fmtNum(failures) + ' 次' },
      { label: 'FM Domain', icon: 'globe', valueHtml: statusTag(domainOk, '在线', '离线'), hint: health && health.ok ? '健康检查通过' : '健康检查未通过' },
      { label: 'Cosmobot RPC', icon: 'cpu', valueHtml: statusTag(rpcOk, '可连', '不可用'), hint: (obs && obs.rpc && obs.rpc.reason) ? esc(truncate(obs.rpc.reason, 48)) : '配置快照读取正常' },
      { label: '配置同步', icon: 'save', valueHtml: configSync.applied ? '<span class="tag tag-ok">已生效</span>' : '<span class="tag tag-warn">待确认</span>', hint: configSync.reason ? esc(truncate(configSync.reason, 46)) : '后台与运行时一致' },
    ]) +
    '<div class="grid grid-wide">' +
      card({ title: '服务健康', icon: 'shield', headActions: ['<a class="btn btn-ghost btn-sm" href="#/deploy">' + icon('server') + '运维</a>'] },
        scoreRing +
        '<div class="grid grid-2 mt-12" style="gap:8px">' +
          [['FM Domain', domainOk, 'domain'], ['Cosmobot RPC', rpcOk, 'rpc'], ['配置同步', !!configSync.applied, 'config'], ['任务处理', !!(tasks && tasks.ok), 'tasks']].map((row) =>
            '<div class="cap ' + (row[1] ? '' : 'off') + '">' + icon(row[2] === 'tasks' ? 'activity' : 'server') + '<span>' + row[0] + '</span><span class="dot" data-tone="' + (row[1] ? 'ok' : 'bad') + '"></span></div>').join('') +
        '</div>' +
        (issues.length
          ? '<div class="stack mt-12" style="gap:7px">' + issues.map((issue) =>
            '<div class="notice warn">' + icon('alert') + '<div><b>' + esc(issue.issue || '') + '</b><div class="muted text-xs mt-4">' + esc(issue.suggestion || '') + '</div></div></div>').join('') + '</div>'
          : '<div class="notice ok mt-12">' + icon('check') + '<div>没有待处理问题。' + (configSync.saved ? '配置版本已保存并与运行时一致。' : '') + '</div></div>') +
        '<div class="kv-grid mt-12">' +
          '<div class="kv-item"><div class="k">聊天模型</div><div class="v mono">' + esc((overview && overview.chat_model) || '-') + '</div></div>' +
          '<div class="kv-item"><div class="k">思考档位</div><div class="v mono">' + esc((overview && overview.reasoning_effort) || '-') + '</div></div>' +
          '<div class="kv-item"><div class="k">最大轮数</div><div class="v mono">' + esc((config.base && config.base.agent_max_turns) || '-') + '</div></div>' +
          '<div class="kv-item"><div class="k">上下文上限</div><div class="v mono">' + esc((config.base && config.base.recent_chat_context_max_chars) || '-') + '</div></div>' +
          '<div class="kv-item"><div class="k">任务处理中</div><div class="v">' + fmtNum(tasks.running) + ' 个运行中 · ' + fmtNum(tasks.stuck) + ' 个疑似卡住</div></div>' +
          '<div class="kv-item"><div class="k">配置版本</div><div class="v">' + (configSync.saved ? '已保存 · ' + esc(configSync.last_version_id || '') : '暂无变更版本') + '</div></div>' +
        '</div>') +
      card({ title: '数据规模', icon: 'database', headActions: ['<a class="btn btn-ghost btn-sm" href="#/business">' + icon('book') + '业务数据</a>'] },
        '<div class="stack" style="gap:2px">' +
          [['群 / 房间', stats.groups, 'groups'], ['跟打文库', stats.library_texts, 'book'], ['群赛文', stats.contest_texts, 'book'],
           ['成绩记录', stats.score_records, 'chart'], ['AI 赛文', stats.ai_contest_texts, 'spark'], ['AI 赛文成绩', stats.ai_contest_scores, 'chart'],
           ['竞赛成绩', stats.competition_scores, 'chart'], ['最近消息', stats.recent_messages, 'message']].map((row) =>
            '<div class="stat-line"><span class="muted">' + esc(row[0]) + '</span><span class="spacer"></span><b>' + fmtNum(row[1]) + '</b></div>').join('') +
        '</div>' +
        '<div class="notice mt-12">' + icon('info') + '<div>数据来自 FM Domain（' + esc(fmtTime(obs && obs.generated_at)) + ' 采集），统计口径以 domain 为准。</div></div>') +
    '</div>' +
    '<div class="grid grid-2 mt-16">' +
      card({ title: msgChartTitle, icon: 'chart', headActions: [chartsMsgRes.pending ? '<span class="tag tag-warn">接口待接入</span>' : (msgRows.length ? '' : '<span class="tag tag-warn">降级采样</span>')] },
        '<div class="chart" id="chart-messages"></div>' +
        (chartsMsgRes.pending ? '<div class="card-note">' + icon('clock') + ' 后端 /api/v2/charts/messages 尚未就绪，正在用「最近消息采样」估算趋势；接口接入后自动切换为全量统计。</div>' : '') +
        (msgRows.length ? '' : '<div class="card-note">' + icon('info') + ' 暂无按天统计数据。</div>')) +
      card({ title: '工具调用趋势 · 近 14 天', icon: 'wrench', headActions: [chartsToolsRes.pending ? '<span class="tag tag-warn">接口待接入</span>' : ''] },
        '<div class="chart" id="chart-tools"></div>' +
        (chartsToolsRes.pending ? '<div class="card-note">' + icon('clock') + ' 后端 /api/v2/charts/tools 尚未就绪，已降级为「近 14 天工具调用总量」条形图。</div>' : '')) +
    '</div>' +
    '<div class="grid grid-2 mt-16">' +
      card({ title: '消息量 Top 群', icon: 'groups', headActions: [topGroups.fallback ? '<span class="tag tag-warn">来自群列表</span>' : ''] },
        '<div class="chart chart-sm" id="chart-top-groups"></div>') +
      card({ title: '最近错误', icon: 'alert', headActions: ['<a class="btn btn-ghost btn-sm" href="#/logs">' + icon('terminal') + '全部日志</a>'] },
        '<div id="dash-errors"></div>') +
    '</div>' +
    '<div class="grid grid-4 mt-16">' +
      ['群与房间|groups|查看每个群的活跃度、触发方式与拟人开关|#/groups',
       '拟人模式|spark|勾选开启拟人模式的会话，编辑主动发言上限|#/humanlike',
       '操作台|wand|以 FM 身份发消息、重启机器人、危险操作二次确认|#/actions',
       '数据浏览器|database|表数据浏览 + 任意 /api 接口调试|#/browse'].map((item) => {
        const parts = item.split('|');
        return '<a class="card" href="' + escAttr(parts[3]) + '" style="text-decoration:none"><div class="card-body">' +
          '<div class="section-title">' + icon(parts[1]) + esc(parts[0]) + '</div>' +
          '<div class="muted text-xs mt-8">' + esc(parts[2]) + '</div></div></a>';
      }).join('') +
    '</div>';

    /* 刷新按钮 */
    const refreshBtn = $('#dash-refresh', root);
    if (refreshBtn) refreshBtn.addEventListener('click', () => route());

    /* 消息量趋势 */
    const msgChartEl = $('#chart-messages', root);
    if (msgRows.length) {
      const labels = msgRows.map((row) => row.label ? row.label.slice(5) : '');
      const series = [{ name: '消息总量', type: 'line', smooth: true, symbol: 'circle', symbolSize: 5, data: msgRows.map((row) => row.count),
        lineStyle: { width: 2.2, color: '#5b8cff' }, itemStyle: { color: '#5b8cff' },
        areaStyle: { color: { type: 'linear', x: 0, y: 0, x2: 0, y2: 1, colorStops: [{ offset: 0, color: 'rgba(91,140,255,.32)' }, { offset: 1, color: 'rgba(91,140,255,0)' }] } } }];
      if (msgRows.some((row) => row.bot != null)) {
        series.push({ name: '机器人消息', type: 'line', smooth: true, symbol: 'none', data: msgRows.map((row) => row.bot || 0),
          lineStyle: { width: 1.8, color: '#8b5cf6', type: 'dashed' }, itemStyle: { color: '#8b5cf6' } });
      }
      mountChart(msgChartEl, chartBase({
        xAxis: { data: labels },
        yAxis: { type: 'value' },
        series: series,
        grid: { top: 30 },
      }), msgRows.map((row) => ({ label: row.label, value: row.count })));
    } else {
      /* 降级：最近 200 条消息按小时采样（覆盖最近 24 小时） */
      let sample = null;
      try { sample = unwrap(await apiGet('/api/v2/messages' + qs({ days: 14, limit: 200 }))); } catch (err) { sample = null; }
      if (!ctx.alive()) return;
      const sampleRows = (sample && sample.rows) || [];
      if (sampleRows.length) {
        const nowHour = Math.floor(Date.now() / 3600000) * 3600000;
        const since = nowHour - 23 * 3600000;
        const buckets = {};
        for (let t = since; t <= nowHour; t += 3600000) {
          const date = new Date(t);
          buckets[pad(date.getHours()) + ':00'] = 0;
        }
        let inWindow = 0;
        sampleRows.forEach((row) => {
          const ms = toMs(row.recorded_at);
          if (ms == null || ms < since) return;
          const date = new Date(ms);
          const key = pad(date.getHours()) + ':00';
          if (buckets[key] != null) { buckets[key] += 1; inWindow += 1; }
        });
        const labels = Object.keys(buckets);
        mountChart(msgChartEl, chartBase({
          xAxis: { data: labels },
          yAxis: { type: 'value', name: '条' },
          series: [{
            name: '采样消息数', type: 'line', smooth: true, symbol: 'circle', symbolSize: 4,
            data: labels.map((key) => buckets[key]),
            lineStyle: { width: 2, color: '#fbbf24' }, itemStyle: { color: '#fbbf24' },
            areaStyle: { color: { type: 'linear', x: 0, y: 0, x2: 0, y2: 1, colorStops: [{ offset: 0, color: 'rgba(251,191,36,.24)' }, { offset: 1, color: 'rgba(251,191,36,0)' }] } },
          }],
          grid: { top: 30 },
        }), labels.map((key) => ({ label: key, value: buckets[key] })));
        const note = document.createElement('div');
        note.className = 'card-note';
        note.innerHTML = icon('info') + ' 降级视图：最近 ' + sampleRows.length + ' 条消息中有 ' + inWindow +
          ' 条落在最近 24 小时，按小时聚合展示走势；/api/v2/charts/messages 接入后自动切换为近 14 天全量统计。';
        msgChartEl.parentNode.insertBefore(note, msgChartEl.nextSibling);
      } else {
        chartNoData(msgChartEl, '暂无消息数据');
      }
    }

    /* 工具调用趋势 */
    const toolsChartEl = $('#chart-tools', root);
    if (toolRows.length && chartsToolsRes.path && chartsToolsRes.path.indexOf('charts/tools') >= 0) {
      const labels = toolRows.map((row) => row.label ? row.label.slice(5) : '');
      mountChart(toolsChartEl, chartBase({
        xAxis: { data: labels },
        series: [{ name: '工具调用', type: 'bar', data: toolRows.map((row) => row.count),
          itemStyle: { color: { type: 'linear', x: 0, y: 0, x2: 0, y2: 1, colorStops: [{ offset: 0, color: '#5b8cff' }, { offset: 1, color: '#3b5fc0' }] }, borderRadius: [4, 4, 0, 0] }, barMaxWidth: 22 }],
        grid: { top: 26 },
      }), toolRows.map((row) => ({ label: row.label, value: row.count })));
    } else if (toolsStats.length) {
      const top = toolsStats.slice(0, 10).map((row) => ({ label: row.tool, value: Number(row.calls || 0) }));
      mountChart(toolsChartEl, chartBase({
        xAxis: { type: 'category', data: top.map((row) => row.label), axisLabel: { color: '#66748a', fontSize: 10, rotate: top.length > 6 ? 24 : 0 } },
        yAxis: { type: 'value' },
        series: [{ name: '近 14 天调用', type: 'bar', data: top.map((row) => row.value), barMaxWidth: 22,
          itemStyle: { color: { type: 'linear', x: 0, y: 0, x2: 0, y2: 1, colorStops: [{ offset: 0, color: '#8b5cf6' }, { offset: 1, color: '#5b8cff' }] }, borderRadius: [4, 4, 0, 0] } }],
        grid: { top: 26, bottom: top.length > 6 ? 34 : 6 },
      }), top);
    } else {
      chartNoData(toolsChartEl, '近 14 天没有工具调用记录');
    }

    /* Top 群 */
    const topGroupsEl = $('#chart-top-groups', root);
    if (topGroups.rows.length) {
      const sorted = topGroups.rows.slice().sort((a, b) => a.value - b.value);
      mountChart(topGroupsEl, chartBase({
        grid: { left: 8, right: 46, top: 8, bottom: 4, containLabel: true },
        tooltip: { trigger: 'axis', axisPointer: { type: 'shadow' } },
        xAxis: { type: 'value', splitLine: { show: false } },
        yAxis: { type: 'category', data: sorted.map((row) => truncate(row.label, 16)), axisLabel: { color: '#c5cfdd', fontSize: 11 }, axisLine: { show: false } },
        series: [{
          type: 'bar', data: sorted.map((row) => row.value), barMaxWidth: 16,
          itemStyle: { color: { type: 'linear', x: 0, y: 0, x2: 1, y2: 0, colorStops: [{ offset: 0, color: '#3b5fc0' }, { offset: 1, color: '#5b8cff' }] }, borderRadius: [0, 4, 4, 0] },
          label: { show: true, position: 'right', color: '#8b98ac', fontSize: 10, formatter: (params) => fmtNum(params.value) },
        }],
      }), sorted.map((row) => ({ label: row.label, value: row.value })));
    } else {
      chartNoData(topGroupsEl, '暂无群消息统计');
    }

    /* 最近错误 */
    const errorsRoot = $('#dash-errors', root);
    if (errorsRoot) {
      const rows = errors.slice(0, 8).map((item) => ({
        at: item.at,
        component: item.component || 'system',
        error: item.error || item.message || '',
        path: item.path || '',
        raw: item,
      }));
      renderTable(errorsRoot, {
        columns: [
          { key: 'at', label: '时间', width: '128px', render: (row) => timeCell(row.at) },
          { key: 'component', label: '组件', width: '92px', render: (row) => '<span class="tag tag-muted mono">' + esc(row.component) + '</span>' },
          { key: 'error', label: '错误', render: (row) => '<span class="cell-wrap">' + esc(truncate(row.error, 140)) + '</span>' },
        ],
        rows: rows,
        detail: (row) => jsonView(row.raw),
        empty: { icon: 'check', title: '最近没有错误', hint: '中控记录的运行时错误会出现在这里' },
      });
    }

    ctx.every(APP.autoRefreshMs, () => route());
  },
};

/* ══════════════════════════════════════════════════════════════════════════
 * 7.2 群与房间：列表 / 详情（触发方式 + 能力开关 + 拟人开关）
 * 数据源：/api/v2/groups、/api/v2/group?id=、/api/domain/group-capability、
 *         /api/domain/group-state、RPC config.trigger(status) 读、
 *         PUT /api/runtime/config/trigger 写、POST /api/v2/humanlike 写
 * ════════════════════════════════════════════════════════════════════════ */

const TRIGGER_MODES = [
  { value: 'prefix', label: '前缀（fm 空格）', desc: '「fm 你好」这类前缀触发' },
  { value: 'mention', label: '@ 机器人', desc: '被 @ 时响应' },
  { value: 'reply', label: '回复机器人', desc: '引用回复机器人消息时响应' },
  { value: 'name', label: '叫名字', desc: '消息里出现机器人名字时响应' },
  { value: 'keyword', label: '触发词', desc: '命中关键词列表时响应' },
];

const CAPABILITY_LABELS = {
  agent: 'Agent 对话', ai_contest: 'AI 赛文', contest: '赛文投稿', library: '跟打文库',
  poke: '拍一拍', recall: '撤回处理', repeat: '复读', score_archive: '成绩归档', scores: '成绩识别',
};

VIEWS.groups = {
  id: 'groups',
  name: '群与房间',
  icon: 'groups',
  skeleton: 'table',
  crumb(route) { return route.segments[1] ? decodeSegment(route.segments[1]) : null; },
  async render(ctx) {
    if (ctx.segments[1]) await renderGroupDetail(ctx, ctx.segments[1]);
    else await renderGroupsList(ctx);
  },
};

async function renderGroupsList(ctx) {
  const root = ctx.root;
  let groups = [];
  let loadError = null;
  try { groups = await getGroups(true); } catch (err) { loadError = err; }
  if (!ctx.alive()) return;
  if (loadError) { showViewError(root, loadError, () => route()); return; }
  const keyword = ctx.q('q').toLowerCase();
  const filter = ctx.q('filter', 'all');
  const sort = ctx.q('sort', 'messages');

  const totalMessages = groups.reduce((sum, group) => sum + Number(group.messages || 0), 0);
  const humanlikeCount = groups.filter((group) => group.humanlike).length;

  const filtered = groups.filter((group) => {
    if (filter === 'humanlike' && !group.humanlike) return false;
    if (filter === 'plain' && group.humanlike) return false;
    if (!keyword) return true;
    return String(group.group_id || '').toLowerCase().indexOf(keyword) >= 0 ||
      String(group.name || '').toLowerCase().indexOf(keyword) >= 0;
  }).sort((a, b) => {
    if (sort === 'name') return String(a.name || '').localeCompare(String(b.name || ''), 'zh-Hans-CN');
    if (sort === 'id') return String(a.group_id || '').localeCompare(String(b.group_id || ''));
    return Number(b.messages || 0) - Number(a.messages || 0);
  });

  root.innerHTML = pageHead({
    title: '群与房间',
    icon: 'groups',
    sub: '共 ' + groups.length + ' 个会话 · 合计 ' + fmtNum(totalMessages) + ' 条消息 · ' + humanlikeCount + ' 个已开启拟人模式',
    actions: [
      '<button class="btn btn-ghost btn-sm" id="groups-refresh" type="button">' + icon('refresh') + '刷新</button>',
      '<a class="btn btn-ghost btn-sm" href="#/humanlike">' + icon('spark') + '拟人模式</a>',
    ],
  }) +
  kpiGrid([
    { label: '会话总数', icon: 'groups', valueHtml: fmtNum(groups.length), tone: 'acc' },
    { label: '有消息的群', icon: 'message', valueHtml: fmtNum(groups.filter((group) => Number(group.messages || 0) > 0).length) },
    { label: '拟人模式开启', icon: 'spark', valueHtml: fmtNum(humanlikeCount), hint: fmtPct(humanlikeCount, groups.length) + ' 的会话' },
    { label: '消息总量', icon: 'chart', valueHtml: fmtNum(totalMessages) },
  ]) +
  card({ title: '会话列表', icon: 'list', flush: true, headActions: [
      '<button class="btn btn-ghost btn-sm" id="groups-clear" type="button">清空筛选</button>',
    ] },
    '<div class="tbl-toolbar">' +
      '<div class="field grow"><input class="input input-sm" id="groups-q" placeholder="搜索群名 / 群号…" value="' + escAttr(ctx.q('q')) + '"></div>' +
      '<select class="select select-sm" id="groups-filter" aria-label="拟人筛选">' +
        ['all:全部会话', 'humanlike:仅拟人开启', 'plain:仅未开启'].map((pair) => {
          const parts = pair.split(':');
          return '<option value="' + parts[0] + '"' + (filter === parts[0] ? ' selected' : '') + '>' + parts[1] + '</option>';
        }).join('') +
      '</select>' +
      '<select class="select select-sm" id="groups-sort" aria-label="排序">' +
        [['messages', '按消息数'], ['name', '按名称'], ['id', '按群号']].map((pair) =>
          '<option value="' + pair[0] + '"' + (sort === pair[0] ? ' selected' : '') + '>' + pair[1] + '</option>').join('') +
      '</select>' +
      '<span class="muted text-xs">' + filtered.length + ' / ' + groups.length + '</span>' +
    '</div>' +
    '<div id="groups-table"></div>') +
  '<div class="notice mt-16">' + icon('info') + '<div>点任意一行进入详情：触发方式、能力开关、拟人开关、最近消息与活跃成员，都能在详情页直接改。</div></div>';

  renderTable($('#groups-table', root), {
    columns: [
      { key: 'group_id', label: '群号', width: '132px', mono: true, render: (row) => '<span class="mono">' + esc(row.group_id) + '</span>' },
      { key: 'name', label: '名称', render: (row) => '<span class="cell-truncate">' + esc(row.name || row.group_id) + '</span>' },
      { key: 'status', label: '状态', width: '96px', render: (row) => '<span class="tag tag-muted">' + esc(row.status || 'unknown') + '</span>' },
      { key: 'messages', label: '消息数', width: '110px', num: true, render: (row) => '<span class="mono">' + fmtNum(row.messages) + '</span>' },
      { key: 'share', label: '占比', width: '120px', hideSm: true, render: (row) => '<div class="row tight"><div class="bar-track" style="flex:0 0 58px"><div class="bar-fill" style="width:' + Math.round(Number(row.messages || 0) / Math.max(1, Number(groups[0] && groups[0].messages || 1)) * 100) + '%"></div></div><span class="muted text-xs">' + fmtPct(row.messages, totalMessages) + '</span></div>' },
      { key: 'humanlike', label: '拟人', width: '78px', render: (row) => row.humanlike ? '<span class="tag tag-ok">' + icon('check') + '开</span>' : '<span class="tag tag-muted">关</span>' },
      { key: 'features', label: '能力标记', width: '110px', hideSm: true, render: (row) => {
        const count = row.features && typeof row.features === 'object' ? Object.keys(row.features).length : 0;
        return count ? '<span class="tag tag-info">' + count + ' 项</span>' : '<span class="muted-2">—</span>';
      } },
    ],
    rows: filtered,
    onRow: (row) => { go('#/groups/' + encodeURIComponent(row.group_id)); },
    empty: {
      icon: 'search',
      title: '没有匹配的会话',
      hint: keyword ? '换个关键词试试，或清空筛选' : '当前没有任何群记录',
      actions: ['<button class="btn btn-sm btn-ghost" id="groups-empty-clear" type="button">清空筛选</button>'],
    },
  });

  const search = function () {
    const query = new URLSearchParams();
    const q = $('#groups-q', root).value.trim();
    if (q) query.set('q', q);
    if (filter !== 'all') query.set('filter', filter);
    if (sort !== 'messages') query.set('sort', sort);
    location.hash = hashFor('groups', [], query);
  };
  const input = $('#groups-q', root);
  if (input) input.addEventListener('keydown', (event) => { if (event.key === 'Enter') search(); });
  const filterEl = $('#groups-filter', root);
  if (filterEl) filterEl.addEventListener('change', search);
  const sortEl = $('#groups-sort', root);
  if (sortEl) sortEl.addEventListener('change', search);
  const clearBtn = $('#groups-clear', root);
  if (clearBtn) clearBtn.addEventListener('click', () => go('#/groups'));
  const refreshBtn = $('#groups-refresh', root);
  if (refreshBtn) refreshBtn.addEventListener('click', () => route());
}

async function renderGroupDetail(ctx, gid) {
  const root = ctx.root;
  const detailRes = await apiGet('/api/v2/group' + qs({ id: gid })).then(unwrap).catch((err) => ({ __error: err }));
  if (!ctx.alive()) return;
  if (detailRes && detailRes.__error) { showViewError(root, detailRes.__error, () => route()); return; }
  const detail = detailRes || {};
  const groups = await getGroups().catch(() => []);
  const name = groupName(groups, gid);
  const capsRes = await apiGet('/api/domain/group-capability' + qs({ group_id: gid })).catch(() => null);
  const stateRes = await apiGet('/api/domain/group-state' + qs({ group_id: gid })).catch(() => null);
  if (!ctx.alive()) return;
  const capabilities = (capsRes && capsRes.ok !== false && unwrap(capsRes) && unwrap(capsRes).capabilities) || null;
  const domainState = (stateRes && unwrap(stateRes)) || detail.domain_state || {};
  const online = domainState && (domainState.online === true || (domainState.updated_at && minutesAgo(toMs(domainState.updated_at) || 0) < 600000));

  let trigger = null;
  let triggerError = null;
  try {
    trigger = unwrap(await apiGet('/api/v2/triggers' + qs({ group_id: gid }), { timeout: 15000 }));
  } catch (err) {
    try {
      const response = await apiPost('/api/rpc', { method: 'config.trigger', params: { action: 'status', scope: 'qq:' + gid } }, { timeout: 12000 });
      trigger = unwrap(response);
      triggerError = null;
    } catch (err2) { triggerError = err2; }
  }
  if (!ctx.alive()) return;

  const recent = detail.recent || [];
  const senders = detail.top_senders || [];
  const lastMessage = recent.length ? recent[0].recorded_at : null;

  root.innerHTML = pageHead({
    title: name === String(gid) ? '群 ' + gid : name,
    icon: 'groups',
    subHtml: true,
    sub: '群号 <span class="mono">' + esc(gid) + '</span> · ' +
      (detail.humanlike ? '<span class="tag tag-ok">' + icon('check') + '拟人开启</span>' : '<span class="tag tag-muted">拟人关闭</span>') +
      ' · ' + (online ? '<span class="tag tag-ok">在线</span>' : '<span class="tag tag-warn">离线/未知</span>'),
    actions: [
      '<button class="btn btn-ghost btn-sm" data-goto="#/groups" type="button">' + icon('arrow-left') + '返回列表</button>',
      '<button class="btn btn-ghost btn-sm" data-copy-text="' + escAttr(gid) + '" type="button">' + icon('copy') + '复制群号</button>',
      '<a class="btn btn-ghost btn-sm" href="#/messages?' + escAttr(qs({ chat: gid, days: 7, limit: 100 }).slice(1)) + '">' + icon('search') + '搜本群消息</a>',
      '<button class="btn btn-ghost btn-sm" id="group-refresh" type="button">' + icon('refresh') + '刷新</button>',
    ],
  }) +
  kpiGrid([
    { label: '消息总数', icon: 'message', valueHtml: fmtNum(detail.messages), tone: 'acc' },
    { label: '活跃成员', icon: 'members', valueHtml: fmtNum(senders.length), hint: '最近 30 条内的发言人' },
    { label: '最近消息', icon: 'clock', valueHtml: lastMessage ? '<span style="font-size:15px">' + esc(ago(lastMessage)) + '</span>' : '—', hint: lastMessage ? esc(fmtTime(lastMessage)) : '没有记录' },
    { label: '拟人模式', icon: 'spark', valueHtml: detail.humanlike ? '<span class="tag tag-ok">已开启</span>' : '<span class="tag tag-muted">未开启</span>', hint: detail.humanlike ? 'scope: qq:group:' + gid : '可在下方一键开启' },
  ]) +
  '<div class="grid grid-2">' +
    card({ title: '触发方式', icon: 'zap', headActions: [
        (trigger && (trigger.config || trigger.saved)) ? '<span class="tag tag-ok">' + icon('check') + '已配置</span>' : '<span class="tag tag-muted">未配置</span>',
        trigger && trigger.source ? '<span class="tag tag-muted mono">' + esc(truncate(trigger.source, 28)) + '</span>' : '',
      ] },
      '<div id="trigger-box">' + renderTriggerForm(gid, trigger, triggerError) + '</div>' +
      '<div class="card-note">触发方式写入机器人运行时配置（scope: <code class="inline">qq:' + esc(gid) + '</code>），会生成配置版本，可在旧后台回滚。至少选择一种触发方式。</div>') +
    card({ title: '能力开关', icon: 'cpu', headActions: [capabilities ? '<span class="tag tag-info">FDomain</span>' : '<span class="tag tag-warn">不可用</span>'] },
      capabilities
        ? '<div class="cap-grid" id="cap-grid">' + Object.keys(capabilities).sort().map((key) =>
            '<label class="cap ' + (capabilities[key] ? '' : 'off') + '"><span>' + esc(CAPABILITY_LABELS[key] || key) + '</span><input type="checkbox" class="cap-toggle" data-cap="' + escAttr(key) + '" ' + (capabilities[key] ? 'checked' : '') + ' style="margin-left:auto"></label>').join('') + '</div>' +
          '<div class="row mt-12"><button class="btn btn-sm btn-solid" id="cap-save" type="button">' + icon('save') + '保存能力开关</button><span class="muted text-xs">改动会写入 FM Domain 的群配置表</span></div>'
        : notice('warn', 'FM Domain 的群能力接口不可用（/api/domain/group-capability），暂时只能查看。')) +
  '</div>' +
  '<div class="grid grid-2 mt-16">' +
    card({ title: '拟人模式', icon: 'spark' },
      '<div class="row spread">' +
        '<div><div class="setting-title">在本群开启拟人模式</div><div class="setting-desc">写入 /runtime/humanlike/scopes.txt 与 proactive.txt，下一条消息生效</div></div>' +
        '<button class="switch" id="group-humanlike" type="button" aria-pressed="' + (!!detail.humanlike) + '"><input type="checkbox" ' + (detail.humanlike ? 'checked' : '') + ' tabindex="-1"><span>' + (detail.humanlike ? '已开启' : '已关闭') + '</span></button>' +
      '</div>' +
      '<div class="card-note">开启/关闭是「全量覆盖」语义：前端会先读取当前全部 scope，再增删本群后整体写回。</div>') +
    card({ title: 'domain 状态', icon: 'globe', headActions: ['<button class="btn btn-ghost btn-sm" data-json-drawer="' + escAttr(jsonSum(domainState).slice(0, 3000)) + '" data-json-title="domain 群状态" type="button">' + icon('eye') + 'JSON</button>'] },
      '<div class="kv-grid">' +
        '<div class="kv-item"><div class="k">group_id</div><div class="v mono">' + esc(domainState.group_id || gid) + '</div></div>' +
        '<div class="kv-item"><div class="k">online</div><div class="v">' + (domainState.online === true ? '是' : domainState.online === false ? '否' : '未知') + '</div></div>' +
        '<div class="kv-item"><div class="k">updated_at</div><div class="v mono">' + esc(fmtTime(domainState.updated_at) || '—') + '</div></div>' +
        '<div class="kv-item"><div class="k">status</div><div class="v">' + esc(detail.domain_state && detail.domain_state.status || '—') + '</div></div>' +
      '</div>') +
  '</div>' +
  '<div class="grid grid-wide mt-16">' +
    card({ title: '最近消息', icon: 'message', flush: true },
      '<div class="transcript" style="padding:12px 14px">' + (recent.length ? recent.map((message) =>
        '<div class="msg ' + (message.is_bot ? 'bot' : '') + '">' +
          '<div class="msg-meta">' + esc(fmtTime(message.recorded_at, { timeOnly: true })) + '<br>' + esc(ago(message.recorded_at)) + '</div>' +
          '<div class="msg-body">' + (message.is_bot ? '<span class="tag tag-acc">BOT</span> ' : '') +
            '<span class="msg-name">' + esc(message.sender_username || '未知') + '</span>' +
            esc(truncate(message.body_text || '(无文本)', 300)) + '</div>' +
        '</div>').join('') : emptyState({ icon: 'message', title: '还没有消息记录' })) + '</div>') +
    card({ title: '活跃成员', icon: 'members', flush: true },
      '<div id="group-senders"></div>') +
  '</div>';

  /* 触发方式表单 */
  const triggerBox = $('#trigger-box', root);
  if (triggerBox) {
    const saveBtn = $('#trigger-save', triggerBox);
    if (saveBtn) saveBtn.addEventListener('click', async () => {
      const modes = $$('.trigger-mode:checked', triggerBox).map((input) => input.value);
      const keywordText = ($('#trigger-keywords', triggerBox) || {}).value || '';
      const keywords = keywordText.split(/[\s,，]+/).map((item) => item.trim()).filter(Boolean);
      if (!modes.length) { toast('至少选择一种触发方式', 'warn'); return; }
      const ok = await confirmAction({
        title: '保存本群触发方式？',
        detail: 'scope: qq:' + gid + '\n触发方式：' + modes.join(' / ') + (keywords.length ? '\n触发词：' + keywords.join(' ') : ''),
        confirmLabel: '保存并生效',
      });
      if (!ok) return;
      saveBtn.disabled = true;
      try {
        const response = await apiPost('/api/v2/triggers/set', { group_id: gid, modes: modes, keywords: keywords }, { timeout: 20000 });
        const result = unwrap(response);
        Store.triggers = null;
        const fallback = !!(result && result.fallback);
        const applied = result ? result.applied : null;
        const message = fallback
          ? '触发配置已写入配置文件（RPC 不可用：' + truncate((result && result.fallback_reason) || '未知原因', 40) + '）'
          : applied === false ? '已保存，但机器人尚未确认生效' : '触发方式已保存并生效';
        toast(message, fallback || applied === false ? 'warn' : 'ok');
        route();
      } catch (err) {
        toast('保存失败：' + err.message, 'bad');
      } finally {
        saveBtn.disabled = false;
      }
    });
    const clearBtn = $('#trigger-clear', triggerBox);
    if (clearBtn) clearBtn.addEventListener('click', async () => {
      const ok = await confirmAction({
        title: '清除本群触发方式？',
        detail: '清除后本群回落到「群聊默认」触发配置。该操作会写入机器人运行时配置（或 trigger-config.json 兜底）。',
        confirmLabel: '清除',
        danger: true,
      });
      if (!ok) return;
      try {
        await apiPost('/api/v2/triggers/set', { group_id: gid, modes: [], keywords: [] }, { timeout: 20000 });
        Store.triggers = null;
        toast('已清除本群触发配置', 'ok');
        route();
      } catch (err) { toast('清除失败：' + err.message, 'bad'); }
    });
  }

  /* 能力开关 */
  const capSave = $('#cap-save', root);
  if (capSave) capSave.addEventListener('click', async () => {
    const next = {};
    $$('.cap-toggle', root).forEach((input) => { next[input.getAttribute('data-cap')] = input.checked; });
    const changed = Object.keys(next).filter((key) => !!capabilities[key] !== next[key]);
    if (!changed.length) { toast('能力开关没有变化', 'info'); return; }
    const ok = await confirmAction({
      title: '保存能力开关？',
      detail: '将修改：' + changed.map((key) => (CAPABILITY_LABELS[key] || key) + ' → ' + (next[key] ? '开启' : '关闭')).join('、'),
      confirmLabel: '写入 FM Domain',
    });
    if (!ok) return;
    capSave.disabled = true;
    try {
      const response = await apiPost('/api/domain/group-capability', { group_id: gid, capabilities: next });
      const payload = unwrap(response);
      if (payload && payload.capabilities) {
        toast('能力开关已保存', 'ok');
        route();
      } else {
        toast('接口已返回，但未见 capabilities 字段', 'warn');
      }
    } catch (err) {
      toast('保存失败：' + err.message, 'bad');
    } finally {
      capSave.disabled = false;
    }
  });

  /* 拟人开关 */
  const humanBtn = $('#group-humanlike', root);
  if (humanBtn) humanBtn.addEventListener('click', async () => {
    const scope = 'qq:group:' + gid;
    const enable = !detail.humanlike;
    const ok = await confirmAction({
      title: enable ? '开启本群拟人模式？' : '关闭本群拟人模式？',
      detail: '会话 scope：' + scope + '\n' + (enable ? '开启后 FM 在本群将按拟人模式与主动发言配置运行。' : '关闭后本群不再使用拟人模式，主动发言配置会被移除。'),
      confirmLabel: enable ? '开启' : '关闭',
      danger: !enable,
    });
    if (!ok) return;
    humanBtn.disabled = true;
    try {
      const state = await getHumanlike(true);
      const scopes = (state.scopes || []).filter((item) => item !== scope);
      if (enable) scopes.unshift(scope);
      await apiPost('/api/v2/humanlike', { scopes: scopes });
      Store.humanlike = null;
      toast(enable ? '已开启拟人模式' : '已关闭拟人模式', 'ok');
      route();
    } catch (err) {
      toast('保存失败：' + err.message, 'bad');
      humanBtn.disabled = false;
    }
  });

  /* 活跃成员 */
  renderTable($('#group-senders', root), {
    columns: [
      { key: 'sender_username', label: '成员', render: (row) => '<span class="cell-truncate">' + esc(row.sender_username || '未知') + '</span>' },
      { key: 'n', label: '消息', width: '80px', num: true, render: (row) => '<span class="mono">' + fmtNum(row.n) + '</span>' },
      { key: 'bar', label: '', width: '90px', hideSm: true, render: (row) => '<div class="bar-track"><div class="bar-fill" style="width:' + Math.round(Number(row.n || 0) / Math.max(1, Number(senders[0] && senders[0].n || 1)) * 100) + '%"></div></div>' },
    ],
    rows: senders,
    empty: { icon: 'members', title: '暂无活跃成员统计' },
  });

  const refreshBtn = $('#group-refresh', root);
  if (refreshBtn) refreshBtn.addEventListener('click', () => route());
}

function renderTriggerForm(gid, trigger, triggerError) {
  if (triggerError) {
    return notice('warn', '读取触发配置失败：' + esc(triggerError.message) +
      '<div class="mt-8"><button class="btn btn-sm btn-ghost" data-goto="#/browse" type="button">去接口调试器</button></div>');
  }
  const config = (trigger && trigger.config) || null;
  const activeModes = new Set(((config && config.modes) || []).map((mode) => String(mode).toLowerCase()));
  const keywords = ((config && config.keywords) || []).join(' ');
  const source = (trigger && trigger.source) || '';
  const modesCatalog = (trigger && Array.isArray(trigger.modes_catalog) && trigger.modes_catalog.length)
    ? trigger.modes_catalog
    : TRIGGER_MODES.map((mode) => ({ id: mode.value, label: mode.label }));
  const totalConfigured = trigger && trigger.total_configured != null ? trigger.total_configured : null;
  return '<div class="stack" style="gap:10px">' +
    '<div class="chips" style="gap:10px 12px">' + modesCatalog.map((mode) => {
      const value = String(mode.id || mode.value || '');
      const label = String(mode.label || value);
      return '<label class="check" title="' + escAttr(label) + '"><input type="checkbox" class="trigger-mode" value="' + escAttr(value) + '" ' +
        (activeModes.has(value.toLowerCase()) ? 'checked' : '') + '>' + esc(label) + '</label>';
    }).join('') + '</div>' +
    '<div class="field"><label>额外触发词（空格分隔）</label>' +
      '<input class="input" id="trigger-keywords" value="' + escAttr(keywords) + '" placeholder="例如：*fix 赞我"></div>' +
    '<div class="row">' +
      '<button class="btn btn-sm btn-solid" id="trigger-save" type="button">' + icon('save') + '保存触发方式</button>' +
      '<button class="btn btn-sm btn-ghost" id="trigger-clear" type="button">' + icon('trash') + '清除</button>' +
      '<span class="muted text-xs">' + (config
        ? '当前：' + esc(((config.modes || []).join(' / ') || '（无模式）')) + (config.keywords && config.keywords.length ? ' · 触发词 ' + config.keywords.length + ' 个' : '')
        : '当前未单独配置（使用群聊默认）') + '</span>' +
    '</div>' +
    '<div class="card-note">' + (source ? '配置来源：<code class="inline">' + esc(source) + '</code> · ' : '') +
      (totalConfigured != null ? '共有 ' + fmtNum(totalConfigured) + ' 个会话单独配置了触发方式 · ' : '') +
      '写入优先走机器人 RPC（生成运行时配置）；RPC 不可用时后端会原子写 trigger-config.json 兜底，并返回 fallback 标记。</div>' +
  '</div>';
}
/* ══════════════════════════════════════════════════════════════════════════
 * 7.3 成员：列表 / 详情（曾用昵称、分群活跃、最近消息）
 * 数据源：/api/v2/members/all、/api/v2/members/profile(新，降级 /api/v2/members)、
 *         /api/v2/messages（按 sender 采样过滤）
 * ════════════════════════════════════════════════════════════════════════ */

VIEWS.members = {
  id: 'members',
  name: '成员',
  icon: 'members',
  skeleton: 'table',
  crumb(route) { return route.segments[1] ? decodeSegment(route.segments[1]) : null; },
  async render(ctx) {
    if (ctx.segments[1]) await renderMemberDetail(ctx, ctx.segments[1]);
    else await renderMembersList(ctx);
  },
};

async function renderMembersList(ctx) {
  const root = ctx.root;
  let members = [];
  let total = 0;
  let loadError = null;
  try {
    const data = unwrap(await apiGet('/api/v2/members/all' + qs({ limit: 2000 })));
    members = (data && data.members) || [];
    total = Number(data && data.count || members.length);
  } catch (err) { loadError = err; }
  if (!ctx.alive()) return;
  if (loadError) { showViewError(root, loadError, () => route()); return; }

  const keyword = ctx.q('q').toLowerCase();
  const sort = ctx.q('sort', 'messages');
  const pageSize = 50;
  const page = Math.max(1, Number(ctx.q('page', '1')) || 1);

  let filtered = members.filter((member) => {
    if (!keyword) return true;
    if (String(member.user_id || '').toLowerCase().indexOf(keyword) >= 0) return true;
    return (member.aliases || []).some((alias) => String(alias || '').toLowerCase().indexOf(keyword) >= 0);
  });
  filtered = filtered.sort((a, b) => {
    if (sort === 'last') return String(b.last_at || '').localeCompare(String(a.last_at || ''));
    if (sort === 'aliases') return (b.aliases || []).length - (a.aliases || []).length;
    return Number(b.messages || 0) - Number(a.messages || 0);
  });
  const totalPages = Math.max(1, Math.ceil(filtered.length / pageSize));
  const currentPage = Math.min(page, totalPages);
  const pageRows = filtered.slice((currentPage - 1) * pageSize, currentPage * pageSize);
  const totalMessages = members.reduce((sum, member) => sum + Number(member.messages || 0), 0);
  const topMember = members[0];

  const setPage = (next) => {
    const query = new URLSearchParams();
    if (keyword) query.set('q', ctx.q('q'));
    if (sort !== 'messages') query.set('sort', sort);
    if (next > 1) query.set('page', String(next));
    go(hashFor('members', [], query));
  };

  root.innerHTML = pageHead({
    title: '成员',
    icon: 'members',
    sub: '共 ' + fmtNum(total) + ' 位人类成员 · 合计 ' + fmtNum(totalMessages) + ' 条消息 · 按消息数排序（Top 2000）',
    actions: ['<a class="btn btn-ghost btn-sm" href="#/messages">' + icon('search') + '消息搜索</a>',
      '<button class="btn btn-ghost btn-sm" id="members-refresh" type="button">' + icon('refresh') + '刷新</button>'],
  }) +
  kpiGrid([
    { label: '成员数', icon: 'members', valueHtml: fmtNum(total), tone: 'acc' },
    { label: '消息总量', icon: 'message', valueHtml: fmtNum(totalMessages) },
    { label: '平均发言', icon: 'chart', valueHtml: fmtNum(members.length ? Math.round(totalMessages / members.length) : 0), hint: '条 / 人' },
    { label: '最活跃成员', icon: 'zap', valueHtml: '<span style="font-size:14px">' + esc(truncate(topMember ? ((topMember.aliases && topMember.aliases[0]) || topMember.user_id) : '—', 18)) + '</span>', hint: topMember ? fmtNum(topMember.messages) + ' 条消息' : '' },
  ]) +
  card({ title: '成员列表', icon: 'list', flush: true }, 
    '<div class="tbl-toolbar">' +
      '<div class="field grow"><input class="input input-sm" id="members-q" placeholder="搜索 QQ 号 / 曾用昵称…" value="' + escAttr(ctx.q('q')) + '"></div>' +
      '<select class="select select-sm" id="members-sort" aria-label="排序">' +
        [['messages', '按消息数'], ['last', '按最近活跃'], ['aliases', '按曾用昵称数']].map((pair) =>
          '<option value="' + pair[0] + '"' + (sort === pair[0] ? ' selected' : '') + '>' + pair[1] + '</option>').join('') +
      '</select>' +
      '<span class="muted text-xs">' + filtered.length + ' 条结果 · 第 ' + currentPage + '/' + totalPages + ' 页</span>' +
    '</div>' +
    '<div id="members-table"></div>' +
    (filtered.length > pageSize ? pagerBar({ offset: (currentPage - 1) * pageSize, limit: pageSize, total: filtered.length, unit: '位成员', jump: false }) : '')) +
  '<div class="notice mt-16">' + icon('info') + '<div>点行进入成员档案：曾用昵称、分群活跃、最近消息，以及接口 /api/v2/members/profile 返回的完整档案。</div></div>';

  const tableRoot = $('#members-table', root);
  renderTable(tableRoot, {
    columns: [
      { key: 'user_id', label: 'QQ', width: '128px', mono: true, render: (row) => '<span class="mono">' + esc(row.user_id) + '</span>' },
      { key: 'aliases', label: '曾用昵称', render: (row) => {
        const aliases = row.aliases || [];
        if (!aliases.length) return '<span class="muted-2">—</span>';
        return '<span class="cell-truncate">' + aliases.map((alias) => '<span class="tag">' + esc(alias) + '</span>').join(' ') + '</span>';
      } },
      { key: 'messages', label: '消息数', width: '104px', num: true, render: (row) => '<span class="mono">' + fmtNum(row.messages) + '</span>' },
      { key: 'last_at', label: '最近活跃', width: '150px', render: (row) => agoCell(row.last_at) },
      { key: 'aliasCount', label: '昵称数', width: '76px', num: true, hideSm: true, render: (row) => fmtNum((row.aliases || []).length) },
    ],
    rows: pageRows,
    onRow: (row) => { go('#/members/' + encodeURIComponent(row.user_id)); },
    empty: { icon: 'search', title: '没有匹配成员', hint: '换个昵称片段或 QQ 号再试' },
  });
  const searchInput = $('#members-q', root);
  const applySearch = () => {
    const query = new URLSearchParams();
    const q = searchInput.value.trim();
    if (q) query.set('q', q);
    if (sort !== 'messages') query.set('sort', sort);
    go(hashFor('members', [], query));
  };
  if (searchInput) searchInput.addEventListener('keydown', (event) => { if (event.key === 'Enter') applySearch(); });
  const sortEl = $('#members-sort', root);
  if (sortEl) sortEl.addEventListener('change', () => {
    const query = new URLSearchParams();
    const q = ctx.q('q');
    if (q) query.set('q', q);
    if (sortEl.value !== 'messages') query.set('sort', sortEl.value);
    go(hashFor('members', [], query));
  });
  const refreshBtn = $('#members-refresh', root);
  if (refreshBtn) refreshBtn.addEventListener('click', () => route());
  root.addEventListener('click', (event) => {
    const btn = event.target.closest('[data-page]');
    if (!btn) return;
    const action = btn.getAttribute('data-page');
    if (action === 'prev') setPage(Math.max(1, currentPage - 1));
    if (action === 'next') setPage(Math.min(totalPages, currentPage + 1));
    if (action === 'jump') {
      const value = Number(btn.value);
      if (value >= 1 && value <= totalPages) setPage(value);
    }
  });
}

async function renderMemberDetail(ctx, uid) {
  const root = ctx.root;
  const profileRes = await probeApi([
    '/api/v2/members/profile' + qs({ id: uid }),
    '/api/v2/members/profile' + qs({ user_id: uid }),
  ]);
  if (!ctx.alive()) return;
  const profile = profileRes.ok ? profileRes.data : null;
  const loadError = !profileRes.ok && !profileRes.pending ? profileRes.error : null;
  let fallback = null;
  if (!profile) {
    try {
      const data = unwrap(await apiGet('/api/v2/members' + qs({ q: uid })));
      const matches = (data && data.matches) || [];
      fallback = matches.find((item) => String(item.user_id) === String(uid)) || null;
    } catch (err) { fallback = null; }
  }
  if (!ctx.alive()) return;
  const groups = await getGroups().catch(() => []);

  const aliases = (profile && profile.aliases) || (fallback && fallback.aliases) || [];
  const messageCount = (profile && profile.messages) || (fallback && fallback.messages) || 0;
  const lastSeen = (profile && profile.last_seen) || (fallback && fallback.last_seen) || '';
  const firstSeen = (profile && profile.first_seen) || '';
  const perGroup = (profile && profile.per_group) || [];
  const recent = (profile && profile.recent) || [];
  const displayName = aliases.length ? aliases[0] : uid;
  const knownGroupIds = new Set(groups.map((group) => String(group.group_id)));
  const totalGroupMessages = perGroup.reduce((sum, row) => sum + Number(row.messages || 0), 0);

  root.innerHTML = pageHead({
    title: '成员 ' + displayName,
    icon: 'members',
    subHtml: true,
    sub: 'QQ <span class="mono">' + esc(uid) + '</span> · ' +
      (aliases.length ? aliases.length + ' 个曾用昵称' : '暂无昵称记录') +
      (firstSeen ? ' · 首次出现 ' + esc(fmtTime(firstSeen, { dateOnly: true })) : ''),
    actions: [
      '<button class="btn btn-ghost btn-sm" data-goto="#/members" type="button">' + icon('arrow-left') + '返回列表</button>',
      '<button class="btn btn-ghost btn-sm" data-copy-text="' + escAttr(uid) + '" type="button">' + icon('copy') + '复制 QQ</button>',
      '<a class="btn btn-ghost btn-sm" href="' + escAttr('#/messages?' + qs({ days: 30, limit: 100 }).slice(1)) + '">' + icon('search') + '消息搜索</a>',
      '<button class="btn btn-ghost btn-sm" id="member-refresh" type="button">' + icon('refresh') + '刷新</button>',
    ],
  }) +
  (loadError ? '<div class="mb-16">' + errorState(loadError, 'member-retry') + '</div>' : '') +
  kpiGrid([
    { label: '发言总数', icon: 'message', valueHtml: fmtNum(messageCount), tone: 'acc', hint: perGroup.length ? '覆盖 ' + perGroup.length + ' 个会话' : '' },
    { label: '最近活跃', icon: 'clock', valueHtml: lastSeen ? '<span style="font-size:15px">' + esc(ago(lastSeen)) + '</span>' : '—', hint: lastSeen ? esc(fmtTime(lastSeen)) : '' },
    { label: '首次出现', icon: 'activity', valueHtml: firstSeen ? '<span style="font-size:15px">' + esc(fmtTime(firstSeen, { dateOnly: true })) + '</span>' : '—' },
    { label: '曾用昵称', icon: 'members', valueHtml: fmtNum(aliases.length) },
  ]) +
  '<div class="grid grid-wide">' +
    card({ title: '成员档案', icon: 'user', headActions: [
        profile ? '<span class="tag tag-ok">' + icon('check') + '/api/v2/members/profile</span>'
          : profileRes.pending ? '<span class="tag tag-warn">profile 接口待接入</span>' : '<span class="tag tag-bad">档案不可用</span>',
        '<button class="btn btn-ghost btn-sm" data-json-drawer="' + escAttr(jsonSum(profile || fallback || { user_id: uid }).slice(0, 6000)) + '" data-json-title="成员档案" type="button">' + icon('eye') + 'JSON</button>',
      ] },
      '<div class="kv-grid">' +
        '<div class="kv-item"><div class="k">QQ</div><div class="v mono">' + esc(uid) + '</div></div>' +
        '<div class="kv-item"><div class="k">消息数</div><div class="v">' + fmtNum(messageCount) + '</div></div>' +
        '<div class="kv-item"><div class="k">最近活跃</div><div class="v mono">' + esc(fmtTime(lastSeen) || '—') + '</div></div>' +
        '<div class="kv-item"><div class="k">首次出现</div><div class="v mono">' + esc(fmtTime(firstSeen) || '—') + '</div></div>' +
        '<div class="kv-item"><div class="k">会话数</div><div class="v">' + fmtNum(perGroup.length) + '</div></div>' +
        '<div class="kv-item"><div class="k">群内消息合计</div><div class="v">' + fmtNum(totalGroupMessages) + '</div></div>' +
      '</div>' +
      '<div class="section-title mt-16">' + icon('members') + '曾用昵称<span class="section-sub">' + aliases.length + ' 个 · 按出现次数排序</span></div>' +
      '<div class="chips mt-8">' + (aliases.length ? aliases.map((alias) => '<span class="tag">' + esc(alias) + '</span>').join('') : '<span class="muted-2">暂无记录</span>') + '</div>' +
      (!profile && profileRes.pending ? '<div class="notice mt-12">' + icon('clock') + '<div>后端 <code class="inline">/api/v2/members/profile?id=</code> 尚未就绪，当前展示成员搜索接口的聚合结果。</div></div>' : '')) +
    card({ title: '按群活跃', icon: 'groups', headActions: ['<span class="muted text-xs">全部历史</span>'] },
      '<div id="member-groups"></div>' +
      '<div class="card-note">来自 chat_log 全量聚合（不是采样），按消息数排序。</div>') +
  '</div>' +
  '<div class="grid grid-wide mt-16">' +
    card({ title: '最近消息', icon: 'message', flush: true, headActions: ['<span class="muted text-xs">最近 20 条</span>'] }, '<div id="member-recent"></div>') +
    card({ title: '快捷操作', icon: 'zap' },
      '<div class="stack" style="gap:9px">' +
        '<button class="btn btn-ghost w-full" id="member-copy-json" type="button">' + icon('copy') + '复制成员档案 JSON</button>' +
        '<button class="btn btn-ghost w-full" id="member-copy-id" type="button">' + icon('copy') + '复制 QQ 号</button>' +
        '<div class="notice">' + icon('info') + '<div>成员数据只读：后台没有修改成员资料的接口。昵称变化会随聊天记录自然累积。</div></div>' +
      '</div>') +
  '</div>';

  renderTable($('#member-groups', root), {
    columns: [
      { key: 'name', label: '会话', width: '46%', render: (row) => {
        const gid = String(row.chat_id || '');
        const label = esc(row.name || gid);
        const link = knownGroupIds.has(gid) ? '<a href="' + escAttr('#/groups/' + encodeURIComponent(gid)) + '">' + label + '</a>' : label;
        return '<span class="cell-truncate">' + link + ' <span class="tag tag-muted mono">' + esc(row.kind || 'chat') + '</span></span>';
      } },
      { key: 'messages', label: '消息数', width: '132px', num: true, render: (row) => '<div class="row tight" style="flex-wrap:nowrap;justify-content:flex-end"><span class="mono">' + fmtNum(row.messages) + '</span>' +
        '<span class="bar-track" style="flex:0 0 40px"><span class="bar-fill" style="width:' + Math.max(2, Math.round(Number(row.messages || 0) / Math.max(1, Number(perGroup[0] && perGroup[0].messages || 1)) * 100)) + '%"></span></span></div>' },
      { key: 'last_at', label: '最近', width: '132px', render: (row) => agoCell(row.last_at) },
    ],
    rows: perGroup,
    onRow: (row) => openJsonDrawer('会话活跃明细', row, row.name || ''),
    empty: { icon: 'groups', title: '没有会话记录', hint: '该成员可能只在私聊或未归档的会话中出现' },
  });

  renderTable($('#member-recent', root), {
    columns: [
      { key: 'at', label: '时间', width: '146px', render: (row) => timeCell(row.at) },
      { key: 'name', label: '会话', width: '140px', hideSm: true, render: (row) => '<span class="cell-truncate">' + esc(row.name || row.chat_id || '') + '</span>' },
      { key: 'text', label: '内容', render: (row) => '<span class="cell-wrap">' + (row.text ? esc(truncate(row.text, 220)) : '<span class="muted-2">（图片 / 无文本）</span>') +
        (Number(row.images || 0) > 0 ? ' <span class="tag tag-info">' + icon('eye') + esc(row.images) + ' 图</span>' : '') + '</span>' },
    ],
    rows: recent,
    detail: (row) => jsonView(row),
    empty: { icon: 'message', title: '没有最近消息' },
  });

  const copyJson = $('#member-copy-json', root);
  if (copyJson) copyJson.addEventListener('click', () => {
    copyText(JSON.stringify(profile || fallback || { user_id: uid }, null, 2))
      .then(() => toast('已复制成员档案 JSON', 'ok', { timeout: 1600 }))
      .catch(() => toast('复制失败', 'bad'));
  });
  const copyId = $('#member-copy-id', root);
  if (copyId) copyId.addEventListener('click', () => {
    copyText(uid).then(() => toast('已复制 QQ 号', 'ok', { timeout: 1600 })).catch(() => toast('复制失败', 'bad'));
  });
  const retryBtn = $('#member-retry', root);
  if (retryBtn) retryBtn.addEventListener('click', () => route());
  const refreshBtn = $('#member-refresh', root);
  if (refreshBtn) refreshBtn.addEventListener('click', () => route());
}
/* ══════════════════════════════════════════════════════════════════════════
 * 7.4 消息搜索
 * 数据源：/api/v2/messages?q=&chat=&days=&limit=（服务端最多 200 条）
 * ════════════════════════════════════════════════════════════════════════ */

VIEWS.messages = {
  id: 'messages',
  name: '消息搜索',
  icon: 'search',
  skeleton: 'table',
  async render(ctx) {
    const root = ctx.root;
    const groups = await getGroups().catch(() => []);
    if (!ctx.alive()) return;
    const keyword = ctx.q('q');
    const chat = ctx.q('chat');
    const days = ctx.q('days', '7');
    const limit = ctx.q('limit', '100');
    const botOnly = ctx.q('bot') === '1';

    let data = null;
    let loadError = null;
    const started = Date.now();
    try {
      data = unwrap(await apiGet('/api/v2/messages' + qs({ q: keyword, chat: chat, days: days, limit: limit })));
    } catch (err) { loadError = err; }
    if (!ctx.alive()) return;
    const rows = ((data && data.rows) || []).filter((row) => !botOnly || row.is_bot);
    const elapsed = Date.now() - started;

    const formQuery = new URLSearchParams();
    if (keyword) formQuery.set('q', keyword);
    if (chat) formQuery.set('chat', chat);
    if (days !== '7') formQuery.set('days', days);
    if (limit !== '100') formQuery.set('limit', limit);
    if (botOnly) formQuery.set('bot', '1');

    root.innerHTML = pageHead({
      title: '消息搜索',
      icon: 'search',
      sub: '全量聊天记录（含机器人消息）· 服务端单次最多返回 200 条 · 本次用时 ' + elapsed + ' ms',
      actions: ['<a class="btn btn-ghost btn-sm" href="#/groups">' + icon('groups') + '群列表</a>',
        '<button class="btn btn-ghost btn-sm" id="messages-refresh" type="button">' + icon('refresh') + '刷新</button>'],
    }) +
    card({ title: '筛选条件', icon: 'filter' },
      '<div class="filters">' +
        '<div class="field grow" style="flex:2 1 260px"><label>关键词</label><input class="input input-sm" id="msg-q" placeholder="内容包含…（留空则按时间倒序）" value="' + escAttr(keyword) + '"></div>' +
        '<div class="field" style="flex:1 1 180px"><label>群 / 会话</label><select class="select select-sm" id="msg-chat"><option value="">全部会话</option>' +
          groups.map((group) => '<option value="' + escAttr(group.group_id) + '"' + (String(group.group_id) === String(chat) ? ' selected' : '') + '>' +
            esc(truncate(group.name || group.group_id, 22)) + ' · ' + esc(group.group_id) + '</option>').join('') +
        '</select></div>' +
        '<div class="field" style="flex:0 1 120px"><label>最近</label><select class="select select-sm" id="msg-days">' +
          [['1', '1 天'], ['3', '3 天'], ['7', '7 天'], ['14', '14 天'], ['30', '30 天'], ['90', '90 天'], ['0', '全部']].map((pair) =>
            '<option value="' + pair[0] + '"' + (String(days) === pair[0] ? ' selected' : '') + '>' + pair[1] + '</option>').join('') +
        '</select></div>' +
        '<div class="field" style="flex:0 1 120px"><label>条数</label><select class="select select-sm" id="msg-limit">' +
          [['50', '50'], ['100', '100'], ['200', '200']].map((pair) =>
            '<option value="' + pair[0] + '"' + (String(limit) === pair[0] ? ' selected' : '') + '>' + pair[1] + ' 条</option>').join('') +
        '</select></div>' +
        '<label class="check" style="margin-bottom:5px"><input type="checkbox" id="msg-bot"' + (botOnly ? ' checked' : '') + '>仅机器人消息</label>' +
        '<div class="filter-actions" style="margin-bottom:0">' +
          '<button class="btn btn-sm btn-solid" id="msg-search" type="button">' + icon('search') + '搜索</button>' +
          '<button class="btn btn-sm btn-ghost" id="msg-reset" type="button">重置</button>' +
        '</div>' +
      '</div>') +
    card({ title: '结果', icon: 'message', flush: true, headActions: [
        '<span class="muted text-xs">' + rows.length + ' 条' + (data && data.count && data.count > rows.length ? '（已按条件过滤）' : '') + '</span>',
      ] },
      loadError ? errorState(loadError, 'msg-retry')
        : (rows.length ? '<div id="msg-table"></div>' : emptyState({
            icon: 'search',
            title: keyword ? '没有命中「' + keyword + '」' : '没有消息',
            hint: '试试缩短关键词、放宽天数，或清空群筛选',
            actions: ['<button class="btn btn-sm btn-ghost" id="msg-empty-reset" type="button">重置筛选</button>'],
          })));

    if (!loadError && rows.length) {
      renderTable($('#msg-table', root), {
        columns: [
          { key: 'recorded_at', label: '时间', width: '150px', render: (row) => timeCell(row.recorded_at) },
          { key: 'chat_id', label: '群', width: '112px', hideSm: true, render: (row) => '<a class="mono" href="' + escAttr('#/groups/' + encodeURIComponent(row.chat_id)) + '">' + esc(row.chat_id) + '</a>' },
          { key: 'sender_username', label: '发送者', width: '150px', render: (row) => '<span class="cell-truncate">' + esc(row.sender_username || row.sender_id || '未知') + '</span>' + (row.is_bot ? ' <span class="tag tag-acc">BOT</span>' : '') },
          { key: 'body_text', label: '内容', render: (row) => {
            const text = row.body_text ? esc(truncate(row.body_text, 160)) : '<span class="muted-2">（无文本）</span>';
            const image = row.image_urls ? ' <span class="tag tag-info">' + icon('eye') + '图片</span>' : '';
            return '<span class="cell-wrap">' + text + image + '</span>';
          } },
        ],
        rows: rows,
        onRow: (row) => openJsonDrawer('消息详情', row, '群 ' + row.chat_id + ' · ' + (row.sender_username || row.sender_id || '')),
      });
    }
    if (loadError) {
      const retry = $('#msg-retry', root);
      if (retry) retry.addEventListener('click', () => route());
    }
    const search = () => {
      const query = new URLSearchParams();
      const q = $('#msg-q', root).value.trim();
      const chatValue = $('#msg-chat', root).value;
      if (q) query.set('q', q);
      if (chatValue) query.set('chat', chatValue);
      query.set('days', $('#msg-days', root).value);
      query.set('limit', $('#msg-limit', root).value);
      if ($('#msg-bot', root).checked) query.set('bot', '1');
      go(hashFor('messages', [], query));
    };
    const qInput = $('#msg-q', root);
    if (qInput) qInput.addEventListener('keydown', (event) => { if (event.key === 'Enter') search(); });
    ['#msg-chat', '#msg-days', '#msg-limit', '#msg-bot'].forEach((selector) => {
      const el = $(selector, root);
      if (el) el.addEventListener('change', search);
    });
    const searchBtn = $('#msg-search', root);
    if (searchBtn) searchBtn.addEventListener('click', search);
    const resetBtn = $('#msg-reset', root);
    if (resetBtn) resetBtn.addEventListener('click', () => go('#/messages'));
    const emptyReset = $('#msg-empty-reset', root);
    if (emptyReset) emptyReset.addEventListener('click', () => go('#/messages'));
    const refreshBtn = $('#messages-refresh', root);
    if (refreshBtn) refreshBtn.addEventListener('click', () => route());
  },
};


/* ══════════════════════════════════════════════════════════════════════════
 * 7.5 拟人模式：scope 勾选保存 / 主动发言配置 / persona 文件
 * 数据源：/api/v2/humanlike（读写）、/api/v2/actions/run（proactive_set）、
 *         RPC config.persona(status/get) 读人格、/api/v2/groups
 * ════════════════════════════════════════════════════════════════════════ */

const PERSONA_SCOPES = [
  { value: 'private_default', label: '私聊默认人格' },
  { value: 'group_default', label: '群聊默认人格' },
];

VIEWS.humanlike = {
  id: 'humanlike',
  name: '拟人模式',
  icon: 'spark',
  skeleton: 'table',
  async render(ctx) {
    const root = ctx.root;
    const [stateRes, groupsRes, triggerRes, personaProbe] = await Promise.all([
      getHumanlike(true).catch((err) => ({ __error: err })),
      getGroups().catch(() => []),
      probeApi(['/api/v2/triggers']),
      probeApi(['/api/v2/persona' + qs({ scope: 'group_default' })]),
    ]);
    if (!ctx.alive()) return;
    if (stateRes && stateRes.__error) { showViewError(root, stateRes.__error, () => route()); return; }
    const state = stateRes || { scopes: [], proactive: [], files: [] };
    const groups = groupsRes || [];
    const scopes = (state.scopes || []).slice();
    const scopeSet = new Set(scopes);
    const groupIds = new Set(groups.map((group) => String(group.group_id)));
    const customScopes = scopes.filter((scope) => !groupIds.has(String(String(scope).replace('qq:group:', ''))));
    const triggerData = triggerRes.ok ? triggerRes.data : null;
    const triggerGroups = (triggerData && triggerData.groups) || [];
    const personaScopes = (personaProbe.ok && Array.isArray(personaProbe.data && personaProbe.data.scopes) && personaProbe.data.scopes.length)
      ? personaProbe.data.scopes
      : [{ scope: 'group_default', label: '默认群人设', needs_id: false },
         { scope: 'private_default', label: '默认私聊人设', needs_id: false },
         { scope: 'group', label: '指定群', needs_id: true },
         { scope: 'private', label: '指定用户私聊', needs_id: true },
         { scope: 'member', label: '成员风格', needs_id: true }];

    const proactive = (state.proactive || []).map((row) => ({
      scope: row.scope || '',
      date: row.date || '',
      limit: row.limit || '',
      ts: row.ts || '',
    })).filter((row) => row.scope);

    root.innerHTML = pageHead({
      title: '拟人模式',
      icon: 'spark',
      sub: '会话开关（全量覆盖）· 主动发言上限 · 人设内容读写 · 触发方式总览；保存后写入 /runtime，下一条消息生效',
      actions: [
        '<a class="btn btn-ghost btn-sm" href="#/actions">' + icon('wand') + '操作台</a>',
        '<a class="btn btn-ghost btn-sm" href="#/groups">' + icon('groups') + '群与房间</a>',
        '<button class="btn btn-ghost btn-sm" id="hl-refresh" type="button">' + icon('refresh') + '刷新</button>',
      ],
    }) +
    kpiGrid([
      { label: '已开启会话', icon: 'spark', valueHtml: fmtNum(scopes.length), tone: 'acc', hint: scopes.length ? '拟人模式生效中' : '当前没有开启任何会话' },
      { label: '主动发言配置', icon: 'zap', valueHtml: fmtNum(proactive.length), hint: '按会话记录每日上限' },
      { label: '触发方式已配置', icon: 'command', valueHtml: fmtNum(triggerData ? triggerData.count : (triggerProbeCount(triggerRes))), hint: '机器人群级触发配置' },
      { label: 'humanlike 文件', icon: 'database', valueHtml: fmtNum((state.files || []).length), hint: 'scopes.txt / proactive.txt' },
    ]) +
    '<div class="grid grid-narrow mt-16">' +
      card({ title: '会话开关', icon: 'groups', headActions: [
          '<button class="btn btn-ghost btn-sm" id="hl-all" type="button">全选</button>',
          '<button class="btn btn-ghost btn-sm" id="hl-none" type="button">清空</button>',
          '<button class="btn btn-ghost btn-sm" id="hl-invert" type="button">反选</button>',
        ] },
        '<div class="row mb-12"><div class="field grow"><input class="input input-sm" id="hl-filter" placeholder="搜索群名 / 群号…"></div></div>' +
        '<div id="hl-group-list" style="max-height:430px;overflow:auto;border:1px solid var(--line-soft);border-radius:var(--r-sm)">' +
          (groups.length ? groups.map((group) => {
            const scope = 'qq:group:' + group.group_id;
            return '<label class="list-row" style="cursor:pointer" data-hl-scope="' + escAttr(scope) + '" data-hl-search="' + escAttr((group.name || '') + ' ' + group.group_id) + '">' +
              '<input type="checkbox" class="hl-pick" value="' + escAttr(scope) + '" ' + (scopeSet.has(scope) ? 'checked' : '') + '>' +
              '<div class="lr-main"><div class="lr-title">' + esc(group.name || group.group_id) + '</div><div class="lr-sub mono">' + esc(group.group_id) + ' · ' + fmtNum(group.messages) + ' 条消息</div></div>' +
              (group.humanlike ? '<span class="tag tag-ok">开</span>' : '') +
            '</label>';
          }).join('') : emptyState({ icon: 'groups', title: '没有群列表', hint: '无法读取 /api/v2/groups' })) +
        '</div>' +
        (customScopes.length ? '<div class="mt-12"><div class="field-label">自定义 scope（不在群列表）</div><div class="chips mt-8" id="hl-custom">' +
          customScopes.map((scope) => '<span class="chip-scope removable" data-remove-scope="' + escAttr(scope) + '">' + esc(scope) + icon('close') + '</span>').join('') + '</div></div>' : '') +
        '<div class="row mt-12">' +
          '<div class="field grow"><input class="input input-sm" id="hl-new-scope" placeholder="添加 scope，例如 qq:private:123456"></div>' +
          '<button class="btn btn-sm btn-ghost" id="hl-add" type="button">' + icon('plus') + '添加</button>' +
        '</div>' +
        '<div class="row mt-12">' +
          '<button class="btn btn-solid" id="hl-save" type="button">' + icon('save') + '保存拟人模式</button>' +
          '<span class="muted text-xs" id="hl-dirty">修改未保存</span>' +
        '</div>' +
        '<div class="card-note">' + icon('alert') + ' 这是「全量覆盖」接口：保存时会把 <b>全部</b> scope 整体写回，未勾选的会话会被关闭。保存前会显示影响面并二次确认。</div>') +
      '<div class="stack">' +
        card({ title: '主动发言配置', icon: 'zap', flush: true },
          '<div class="tbl-toolbar"><span class="muted text-xs">每条记录 = 某会话当天主动发言的已用/上限</span></div>' +
          '<div id="hl-proactive"></div>' +
          '<div class="card-foot">' + icon('info') + ' 修改上限走 <code class="inline">proactive_set</code>，只影响指定 scope。</div>') +
        card({ title: '触发方式总览', icon: 'command', flush: true, headActions: [triggerRes.ok ? '<span class="tag tag-ok">' + esc(triggerData && triggerData.source || 'v2') + '</span>' : '<span class="tag tag-warn">不可用</span>'] },
          triggerRes.ok
            ? (triggerGroups.length
                ? '<div class="list-rows" style="max-height:220px;overflow:auto">' + triggerGroups.slice(0, 20).map((item) =>
                    '<div class="list-row" data-json-drawer="' + escAttr(jsonSum(item).slice(0, 1500)) + '" data-json-title="触发配置 · ' + escAttr(item.group_id || item.scope) + '">' +
                      '<div class="lr-main"><div class="lr-title mono">' + esc(item.group_id || item.scope) + '</div>' +
                      '<div class="lr-sub">' + (item.modes || []).map((mode) => '<span class="tag tag-muted">' + esc(mode) + '</span>').join(' ') + '</div></div>' +
                      '<span class="lr-extra">' + fmtNum((item.keywords || []).length) + ' 词</span></div>').join('') + '</div>'
                : emptyState({ icon: 'command', title: '没有单独配置触发方式的群', hint: '全部使用群聊默认触发配置' }))
            : pendingState({ title: '触发配置接口不可用', hint: (triggerRes.error && triggerRes.error.message) || 'RPC 与配置文件都读不到触发配置', path: '/api/v2/triggers' })) +
      '</div>' +
    '</div>' +
    '<div class="grid grid-2 mt-16">' +
      card({ title: '人设内容（可读写）', icon: 'robot', headActions: [
          personaProbe.ok ? '<span class="tag tag-ok">/api/v2/persona</span>' : '<span class="tag tag-warn">接口待接入</span>',
          '<button class="btn btn-ghost btn-sm" id="persona-edit-legacy" type="button">' + icon('external') + '旧后台</button>',
        ] },
        '<div class="row mb-12">' +
          '<select class="select select-sm" id="persona-scope" style="flex:1 1 200px">' +
            personaScopes.map((item) => '<option value="' + escAttr(item.scope) + '" data-needs-id="' + (item.needs_id ? '1' : '0') + '">' + esc(item.label || item.scope) + (item.needs_id ? '（需 ID）' : '') + '</option>').join('') +
          '</select>' +
          '<input class="input input-sm" id="persona-id" list="persona-group-ids" placeholder="群号 / QQ 号" style="flex:0 1 150px;display:none">' +
          '<datalist id="persona-group-ids">' + groups.map((group) => '<option value="' + escAttr(group.group_id) + '">' + esc(group.name || '') + '</option>').join('') + '</datalist>' +
          '<button class="btn btn-sm btn-ghost" id="persona-load" type="button">' + icon('eye') + '读取</button>' +
        '</div>' +
        '<div id="persona-meta" class="row mb-8 muted text-xs"></div>' +
        '<textarea class="textarea mono" id="persona-text" placeholder="读取后在这里编辑人设内容…" style="min-height:220px;max-height:360px"></textarea>' +
        '<div class="row mt-12">' +
          '<span class="muted text-xs" id="persona-count">0 字符</span>' +
          '<span class="spacer"></span>' +
          '<button class="btn btn-sm btn-danger" id="persona-clear" type="button">' + icon('trash') + '清空人设</button>' +
          '<button class="btn btn-sm btn-solid" id="persona-save" type="button">' + icon('save') + '保存人设</button>' +
        '</div>' +
        '<div class="card-note">人设写入会立即影响机器人说话风格（下一条消息生效）。默认群人设对所有群生效，单群人设/成员风格会覆盖默认值。</div>') +
      card({ title: 'humanlike 文件', icon: 'database', headActions: ['<button class="btn btn-ghost btn-sm" data-json-drawer="' + escAttr(jsonSum(state).slice(0, 4000)) + '" data-json-title="humanlike 状态" type="button">' + icon('eye') + 'JSON</button>'] },
        '<div class="chips">' + ((state.files || []).length ? state.files.map((file) => '<span class="tag tag-muted mono">' + esc(file) + '</span>').join('') : '<span class="muted-2">目录为空或不可读</span>') + '</div>' +
        '<div class="section-title mt-16">' + icon('spark') + '已开启 scope</div>' +
        '<div class="chips mt-8">' + (scopes.length ? scopes.map((scope) => '<span class="chip-scope">' + esc(scope) + '</span>').join('') : '<span class="muted-2">无</span>') + '</div>' +
        '<div class="card-note">scopes.txt 与 proactive.txt 在同一目录；文件内容可通过下方人设编辑器或旧后台查看。</div>') +
    '</div>';

    /* ── 会话开关 ── */
    const listRoot = $('#hl-group-list', root);
    const collect = () => {
      const picked = $$('.hl-pick:checked', listRoot).map((input) => input.value);
      $$('[data-remove-scope]', root).forEach((chip) => {
        if (!chip.classList.contains('removed')) picked.push(chip.getAttribute('data-remove-scope'));
      });
      return picked;
    };
    const markDirty = () => {
      const note = $('#hl-dirty', root);
      if (note) note.textContent = '有未保存修改';
    };
    if (listRoot) listRoot.addEventListener('change', markDirty);
    const filterInput = $('#hl-filter', root);
    if (filterInput) filterInput.addEventListener('input', () => {
      const keyword = filterInput.value.trim().toLowerCase();
      $$('[data-hl-search]', listRoot).forEach((label) => {
        const text = label.getAttribute('data-hl-search').toLowerCase();
        label.style.display = !keyword || text.indexOf(keyword) >= 0 ? '' : 'none';
      });
    });
    const setAll = (value) => { $$('.hl-pick', listRoot).forEach((input) => { input.checked = value; }); markDirty(); };
    const allBtn = $('#hl-all', root);
    if (allBtn) allBtn.addEventListener('click', () => setAll(true));
    const noneBtn = $('#hl-none', root);
    if (noneBtn) noneBtn.addEventListener('click', () => setAll(false));
    const invertBtn = $('#hl-invert', root);
    if (invertBtn) invertBtn.addEventListener('click', () => { $$('.hl-pick', listRoot).forEach((input) => { input.checked = !input.checked; }); markDirty(); });
    $$('[data-remove-scope]', root).forEach((chip) => {
      chip.addEventListener('click', () => {
        chip.classList.toggle('removed');
        chip.style.opacity = chip.classList.contains('removed') ? '.4' : '1';
        chip.style.textDecoration = chip.classList.contains('removed') ? 'line-through' : '';
        markDirty();
      });
    });
    const addBtn = $('#hl-add', root);
    if (addBtn) addBtn.addEventListener('click', () => {
      const input = $('#hl-new-scope', root);
      const value = input.value.trim();
      if (!/^qq:(group|private):\d+$/.test(value)) { toast('scope 格式应为 qq:group:<群号> 或 qq:private:<QQ号>', 'warn'); return; }
      if (collect().indexOf(value) >= 0) { toast('该 scope 已在列表中', 'warn'); return; }
      const chips = $('#hl-custom', root);
      if (chips) {
        const chip = document.createElement('span');
        chip.className = 'chip-scope removable';
        chip.setAttribute('data-remove-scope', value);
        chip.innerHTML = esc(value) + icon('close');
        chip.addEventListener('click', () => { chip.classList.toggle('removed'); markDirty(); });
        chips.appendChild(chip);
      }
      input.value = '';
      markDirty();
    });
    const saveBtn = $('#hl-save', root);
    if (saveBtn) saveBtn.addEventListener('click', async () => {
      const picked = collect();
      const before = new Set(scopes);
      const after = new Set(picked);
      const added = picked.filter((scope) => !before.has(scope));
      const removed = scopes.filter((scope) => !after.has(scope));
      if (!added.length && !removed.length) { toast('没有需要保存的变更', 'info'); return; }
      const ok = await confirmAction({
        title: '保存拟人模式？',
        detail: '开启 ' + picked.length + ' 个会话（新增 ' + added.length + '，移除 ' + removed.length + '）\n' +
          (added.length ? '新增：' + added.slice(0, 6).join('、') + (added.length > 6 ? '…' : '') + '\n' : '') +
          (removed.length ? '移除：' + removed.slice(0, 6).join('、') + (removed.length > 6 ? '…' : '') : ''),
        confirmLabel: '保存并生效',
      });
      if (!ok) return;
      saveBtn.disabled = true;
      try {
        await apiPost('/api/v2/humanlike', { scopes: picked });
        Store.humanlike = null;
        toast('拟人模式已保存：' + picked.length + ' 个会话', 'ok');
        route();
      } catch (err) {
        toast('保存失败：' + err.message, 'bad');
        saveBtn.disabled = false;
      }
    });

    /* ── 主动发言配置 ── */
    const proactiveRoot = $('#hl-proactive', root);
    if (proactiveRoot) {
      renderTable(proactiveRoot, {
        columns: [
          { key: 'scope', label: '会话 scope', mono: true, render: (row) => '<span class="cell-truncate">' + esc(row.scope) + '</span>' },
          { key: 'date', label: '日期', width: '102px', render: (row) => '<span class="mono">' + esc(row.date || '—') + '</span>' },
          { key: 'limit', label: '上限', width: '184px', render: (row) =>
            '<div class="row tight" style="flex-wrap:nowrap"><input class="input input-sm" style="width:62px" data-proactive-limit="' + escAttr(row.scope) + '" value="' + escAttr(row.limit || '20') + '">' +
            '<button class="btn btn-sm btn-ghost" data-proactive-save="' + escAttr(row.scope) + '" type="button">保存</button></div>' },
          { key: 'ts', label: '更新时间', width: '146px', hideSm: true, render: (row) => timeCell(row.ts) },
        ],
        rows: proactive,
        empty: { icon: 'zap', title: '还没有主动发言配置', hint: '在操作台为某个会话设置每日上限后会出现在这里' },
      });
      proactiveRoot.addEventListener('click', async (event) => {
        const btn = event.target.closest('[data-proactive-save]');
        if (!btn) return;
        const scope = btn.getAttribute('data-proactive-save');
        const input = proactiveRoot.querySelector('[data-proactive-limit="' + scope + '"]');
        const limit = String(Number(input && input.value || 0));
        if (!limit || Number(limit) < 0) { toast('上限必须是非负整数', 'warn'); return; }
        const ok = await confirmAction({ title: '修改主动发言上限？', detail: scope + ' → 每日上限 ' + limit, confirmLabel: '保存' });
        if (!ok) return;
        btn.disabled = true;
        try {
          await apiPost('/api/v2/actions/run', { name: 'proactive_set', scope: scope, limit: limit });
          Store.humanlike = null;
          toast('已更新 ' + scope + ' 的上限为 ' + limit, 'ok');
          route();
        } catch (err) {
          toast('保存失败：' + err.message, 'bad');
          btn.disabled = false;
        }
      });
    }

    /* ── 人设读写 ── */
    const scopeSelect = $('#persona-scope', root);
    const idInput = $('#persona-id', root);
    const syncIdInput = () => {
      if (!scopeSelect || !idInput) return;
      const option = scopeSelect.options[scopeSelect.selectedIndex];
      const needsId = option && option.getAttribute('data-needs-id') === '1';
      idInput.style.display = needsId ? '' : 'none';
      if (needsId && !idInput.placeholder.includes('群号')) idInput.placeholder = '群号 / QQ 号';
    };
    if (scopeSelect) scopeSelect.addEventListener('change', syncIdInput);
    syncIdInput();

    const personaState = { data: null };
    const personaText = $('#persona-text', root);
    const personaMeta = $('#persona-meta', root);
    const personaCount = $('#persona-count', root);
    const updateCount = () => { if (personaCount && personaText) personaCount.textContent = fmtNum(personaText.value.length) + ' 字符'; };
    if (personaText) personaText.addEventListener('input', updateCount);

    const loadPersona = async () => {
      if (!scopeSelect || !personaText) return;
      const scope = scopeSelect.value;
      const ident = scopeSelect.options[scopeSelect.selectedIndex].getAttribute('data-needs-id') === '1' ? (idInput.value.trim() || '') : '';
      if (scopeSelect.options[scopeSelect.selectedIndex].getAttribute('data-needs-id') === '1' && !ident) {
        toast('该 scope 需要填写群号 / QQ 号', 'warn');
        return;
      }
      personaMeta.innerHTML = '<span class="palette-spinner"></span> 正在读取人设…';
      try {
        const data = unwrap(await apiGet('/api/v2/persona' + qs({ scope: scope, id: ident }), { timeout: 15000 }));
        personaState.data = data;
        personaText.value = (data && data.content) || '';
        updateCount();
        personaMeta.innerHTML =
          '<span class="tag tag-muted mono">' + esc((data && data.scope) || scope) + '</span>' +
          '<span class="tag ' + (data && data.exists ? 'tag-ok' : 'tag-muted') + '">' + (data && data.exists ? '已配置' : '未配置') + '</span>' +
          (data && data.saved ? '<span class="tag tag-info">saved</span>' : '') +
          (data && data.applied === false ? '<span class="tag tag-warn">未生效</span>' : '') +
          '<span class="muted text-xs">来源 ' + esc((data && data.source) || '-') + ' · ' + fmtNum(data && data.length) + ' 字符</span>' +
          (data && data.note ? '<span class="tag tag-warn">' + esc(truncate(data.note, 40)) + '</span>' : '');
      } catch (err) {
        personaState.data = null;
        personaMeta.innerHTML = '';
        personaText.value = '';
        updateCount();
        const box = $('#persona-meta', root);
        if (box) box.innerHTML = '<span class="tag tag-bad">读取失败</span><span class="muted text-xs">' + esc(err.message) + '</span>';
      }
    };
    const loadBtn = $('#persona-load', root);
    if (loadBtn) loadBtn.addEventListener('click', loadPersona);
    const savePersona = $('#persona-save', root);
    if (savePersona) savePersona.addEventListener('click', async () => {
      if (!scopeSelect || !personaText) return;
      const scope = scopeSelect.value;
      const option = scopeSelect.options[scopeSelect.selectedIndex];
      const ident = option.getAttribute('data-needs-id') === '1' ? (idInput.value.trim() || '') : '';
      const text = personaText.value;
      if (option.getAttribute('data-needs-id') === '1' && !ident) { toast('该 scope 需要填写群号 / QQ 号', 'warn'); return; }
      if (!text.trim()) { toast('人设内容不能为空；如需清空请用「清空人设」', 'warn'); return; }
      const length = text.length;
      const ok = await confirmAction({
        title: '保存人设内容？',
        detail: 'scope：' + scope + (ident ? ' / ' + ident : '') + '\n' + length + ' 字符\n保存后机器人下一条消息就会使用新人设。',
        confirmLabel: '保存并生效',
      });
      if (!ok) return;
      savePersona.disabled = true;
      try {
        const response = await apiPost('/api/v2/persona', { scope: scope, id: ident, text: text, action: 'set' }, { timeout: 20000 });
        const data = unwrap(response);
        toast((data && data.applied === false) ? '已保存，但机器人尚未确认生效' : '人设已保存', (data && data.applied === false) ? 'warn' : 'ok');
        loadPersona();
      } catch (err) {
        toast('保存失败：' + err.message, 'bad');
      } finally { savePersona.disabled = false; }
    });
    const clearPersona = $('#persona-clear', root);
    if (clearPersona) clearPersona.addEventListener('click', async () => {
      if (!scopeSelect) return;
      const scope = scopeSelect.value;
      const option = scopeSelect.options[scopeSelect.selectedIndex];
      const ident = option.getAttribute('data-needs-id') === '1' ? (idInput.value.trim() || '') : '';
      const ok = await confirmAction({
        title: '清空人设内容？',
        detail: 'scope：' + scope + (ident ? ' / ' + ident : '') + '\n清空后回落到上级默认人设（如有）。该操作会写运行时配置。',
        confirmLabel: '清空',
        danger: true,
        requireText: 'CLEAR',
      });
      if (!ok) return;
      clearPersona.disabled = true;
      try {
        await apiPost('/api/v2/persona', { scope: scope, id: ident, action: 'clear' }, { timeout: 20000 });
        toast('人设已清空', 'ok');
        loadPersona();
      } catch (err) {
        toast('清空失败：' + err.message, 'bad');
      } finally { clearPersona.disabled = false; }
    });
    const legacyEdit = $('#persona-edit-legacy', root);
    if (legacyEdit) legacyEdit.addEventListener('click', () => window.open('/classic', '_blank', 'noopener'));

    if (personaProbe.ok) loadPersona();

    const refreshBtn = $('#hl-refresh', root);
    if (refreshBtn) refreshBtn.addEventListener('click', () => route());
  },
};

function triggerProbeCount(triggerRes) {
  const data = triggerRes && triggerRes.ok ? triggerRes.data : null;
  return (data && data.count) || 0;
}
/* ══════════════════════════════════════════════════════════════════════════
 * 7.6 操作台：写操作 + 二次确认 + 结果回显
 * 数据源：/api/v2/actions（目录）、/api/v2/actions/run（执行）、
 *         /api/v2/humanlike、/api/system/restart/{cosmobot,domain}
 * ════════════════════════════════════════════════════════════════════════ */

VIEWS.actions = {
  id: 'actions',
  name: '操作台',
  icon: 'wand',
  skeleton: 'table',
  async render(ctx) {
    const root = ctx.root;
    const [catalogRes, stateRes, groupsRes] = await Promise.all([
      apiGet('/api/v2/actions').then(unwrap).catch(() => null),
      getHumanlike(true).catch(() => ({ scopes: [], proactive: [] })),
      getGroups().catch(() => []),
    ]);
    if (!ctx.alive()) return;
    const state = stateRes || { scopes: [], proactive: [] };
    const groups = groupsRes || [];
    const scopes = state.scopes || [];
    const scopeSet = new Set(scopes);
    const catalogs = (catalogRes && catalogRes.actions) || [
      { name: 'humanlike_set', label: '设置拟人模式会话', params: 'scopes: [qq:group:<id>]', desc: '全量覆盖拟人会话列表' },
      { name: 'proactive_set', label: '设置主动发言上限', params: 'scope, limit', desc: '修改单会话每日上限' },
      { name: 'send_message', label: '以 FM 身份发消息', params: 'chat_id, text', desc: '调用机器人 chat.send' },
    ];

    root.innerHTML = pageHead({
      title: '操作台',
      icon: 'wand',
      sub: '所有写操作都会先展示影响面、二次确认，再执行并回显结果；破坏性操作需要输入确认词',
      actions: ['<a class="btn btn-ghost btn-sm" href="#/humanlike">' + icon('spark') + '拟人模式</a>',
        '<a class="btn btn-ghost btn-sm" href="/classic" target="_blank" rel="noopener">' + icon('external') + '旧版后台</a>'],
    }) +
    card({ title: '可用动作（后端白名单）', icon: 'shield' },
      '<div class="grid grid-3">' + catalogs.map((action) =>
        '<div class="cap" style="align-items:flex-start;flex-direction:column;gap:4px">' +
          '<div class="row" style="gap:6px"><span class="tag tag-acc mono">' + esc(action.name) + '</span></div>' +
          '<div style="color:var(--fg);font-size:12px">' + esc(action.label || action.name) + '</div>' +
          '<div class="muted-2 text-xs mono">' + esc(action.params || '') + '</div>' +
          (action.desc ? '<div class="muted text-xs">' + esc(action.desc) + '</div>' : '') +
        '</div>').join('') + '</div>' +
      '<div class="card-note">动作清单来自 <code class="inline">/api/v2/actions</code>，执行统一走 <code class="inline">/api/v2/actions/run</code>（POST）。</div>') +
    '<div class="grid grid-2 mt-16">' +
      card({ title: '① 拟人批量设置', icon: 'spark' },
        '<div class="notice mb-12">' + icon('alert') + '<div>全量覆盖：勾选的会话开启拟人模式，未勾选的关闭。当前已开启 <b>' + scopes.length + '</b> 个。</div></div>' +
        '<div id="act-hl-list" style="max-height:280px;overflow:auto;border:1px solid var(--line-soft);border-radius:var(--r-sm)">' +
          groups.map((group) => {
            const scope = 'qq:group:' + group.group_id;
            return '<label class="list-row" style="cursor:pointer"><input type="checkbox" class="act-hl-pick" value="' + escAttr(scope) + '" ' + (scopeSet.has(scope) ? 'checked' : '') + '>' +
              '<div class="lr-main"><div class="lr-title">' + esc(group.name || group.group_id) + '</div><div class="lr-sub mono">' + esc(group.group_id) + '</div></div></label>';
          }).join('') +
        '</div>' +
        '<div class="row mt-12"><button class="btn btn-sm btn-solid" id="act-hl-save" type="button">' + icon('save') + '保存拟人设置</button>' +
        '<span class="muted text-xs">保存前会二次确认影响面</span></div>') +
      card({ title: '② 主动发言上限', icon: 'zap' },
        '<div class="stack" style="gap:10px">' +
          '<div class="field"><label>会话 scope</label><input class="input" id="act-pro-scope" list="scope-list" placeholder="qq:group:776227233" value="' + escAttr(scopes[0] || '') + '">' +
            '<datalist id="scope-list">' + scopes.map((scope) => '<option value="' + escAttr(scope) + '"></option>').join('') +
            groups.map((group) => '<option value="' + escAttr('qq:group:' + group.group_id) + '">' + esc(group.name || '') + '</option>').join('') + '</datalist></div>' +
          '<div class="field" style="max-width:180px"><label>每日上限</label><input class="input" id="act-pro-limit" type="number" min="0" step="1" value="20"></div>' +
          '<div class="row"><button class="btn btn-sm btn-solid" id="act-pro-save" type="button">' + icon('save') + '写入上限</button>' +
          '<button class="btn btn-sm btn-ghost" id="act-pro-current" type="button">查看当前配置</button></div>' +
        '</div>' +
        '<div id="act-pro-out" class="mt-12"></div>') +
    '</div>' +
    '<div class="grid grid-2 mt-16">' +
      card({ title: '③ 以 FM 身份发消息', icon: 'send' },
        '<div class="stack" style="gap:10px">' +
          '<div class="field"><label>目标会话（群号 / QQ 号）</label><input class="input" id="act-chat-id" list="group-id-list" placeholder="776227233">' +
            '<datalist id="group-id-list">' + groups.map((group) => '<option value="' + escAttr(group.group_id) + '">' + esc(group.name || '') + '</option>').join('') + '</datalist></div>' +
          '<div class="field"><label>消息内容</label><textarea class="textarea" id="act-chat-text" placeholder="要发送的文本…"></textarea></div>' +
          '<div class="row"><button class="btn btn-sm btn-solid" id="act-send" type="button">' + icon('send') + '发送消息</button>' +
          '<span class="muted text-xs">会真实发到群里，请谨慎</span></div>' +
        '</div>' +
        '<div id="act-send-out" class="mt-12"></div>') +
      card({ title: '④ 服务重启', icon: 'server', className: 'danger' },
        '<div class="notice warn mb-12">' + icon('alert') + '<div>重启会造成短暂离线。机器人重启期间群消息不会被回复；domain 重启会导致后台数据接口暂时 503。</div></div>' +
        '<div class="stack" style="gap:9px">' +
          '<div class="row spread"><div><div class="setting-title">fm-cosmobot（机器人本体）</div><div class="setting-desc">QQ/MATRIX 连接会重连</div></div>' +
            '<button class="btn btn-sm btn-danger" id="act-restart-bot" type="button">重启机器人</button></div>' +
          '<div class="row spread"><div><div class="setting-title">fm-domain（数据服务）</div><div class="setting-desc">文库/赛文/成绩接口短暂不可用</div></div>' +
            '<button class="btn btn-sm btn-danger" id="act-restart-domain" type="button">重启 Domain</button></div>' +
          '<div class="row spread"><div><div class="setting-title">fm-admin（后台本身）</div><div class="setting-desc">重启后当前会话会失效，需要重新登录</div></div>' +
            '<button class="btn btn-sm btn-ghost" id="act-restart-admin" type="button">重启后台</button></div>' +
        '</div>' +
        '<div id="act-restart-out" class="mt-12"></div>') +
    '</div>' +
    '<div class="card mt-16"><div class="card-head"><h3>' + icon('clock') + '⑤ 定时消息<span class="section-sub">只读 · 可取消</span></h3>' +
      '<div class="card-head-actions"><button class="btn btn-ghost btn-sm" id="act-sched-refresh" type="button">' + icon('refresh') + '刷新</button></div>' +
    '</div><div class="card-body"><div id="act-scheduled"></div>' +
      '<div class="card-note">数据来自 /api/v2/scheduled（scheduled_messages 表）；取消操作走 /api/v2/scheduled/cancel，立即生效。</div></div></div>' +
    '<div class="card mt-16"><div class="card-head"><h3>' + icon('terminal') + '最近一次操作结果</h3></div>' +
      '<div class="card-body"><div id="act-result">' + emptyState({ icon: 'play', title: '还没有执行操作', hint: '执行任意动作后，这里会显示接口返回的完整 JSON' }) + '</div></div></div>';

    const showResult = (title, value) => {
      const box = $('#act-result', root);
      if (!box) return;
      box.innerHTML = '<div class="row mb-8"><span class="tag tag-ok">' + esc(title) + '</span><span class="spacer"></span>' +
        '<button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(value).slice(0, 4000)) + '" data-json-title="' + escAttr(title) + '" type="button">' + icon('external') + '抽屉查看</button></div>' + jsonView(value);
    };
    const showActionResult = (outSelector, value) => {
      const box = $(outSelector, root);
      if (!box) return;
      box.innerHTML = jsonView(value);
    };

    /* ① 拟人批量 */
    const hlSave = $('#act-hl-save', root);
    if (hlSave) hlSave.addEventListener('click', async () => {
      const picked = $$('.act-hl-pick:checked', root).map((input) => input.value);
      const ok = await confirmAction({
        title: '保存拟人模式设置？',
        detail: '将把拟人模式设置为 ' + picked.length + ' 个会话（当前 ' + scopes.length + ' 个）。\n未勾选的会话会被关闭拟人模式。',
        confirmLabel: '保存',
      });
      if (!ok) return;
      hlSave.disabled = true;
      try {
        const response = await apiPost('/api/v2/humanlike', { scopes: picked });
        Store.humanlike = null;
        toast('已保存 ' + picked.length + ' 个会话', 'ok');
        showResult('humanlike_set', unwrap(response));
      } catch (err) {
        toast('保存失败：' + err.message, 'bad');
        showResult('humanlike_set（失败）', { error: err.message, status: err.status });
      } finally { hlSave.disabled = false; }
    });

    /* ② 主动发言上限 */
    const proSave = $('#act-pro-save', root);
    if (proSave) proSave.addEventListener('click', async () => {
      const scope = $('#act-pro-scope', root).value.trim();
      const limit = String(Number($('#act-pro-limit', root).value));
      if (!/^qq:(group|private):\d+$/.test(scope)) { toast('scope 格式应为 qq:group:<群号> 或 qq:private:<QQ号>', 'warn'); return; }
      if (!isFinite(Number(limit)) || Number(limit) < 0) { toast('上限必须是非负整数', 'warn'); return; }
      const ok = await confirmAction({ title: '写入主动发言上限？', detail: scope + ' → ' + limit + ' 条 / 天', confirmLabel: '写入' });
      if (!ok) return;
      proSave.disabled = true;
      try {
        const response = await apiPost('/api/v2/actions/run', { name: 'proactive_set', scope: scope, limit: limit });
        Store.humanlike = null;
        toast('已写入 ' + scope, 'ok');
        showResult('proactive_set', unwrap(response));
        showActionResult('#act-pro-out', { ok: true, scope: scope, limit: limit });
      } catch (err) {
        toast('写入失败：' + err.message, 'bad');
        showActionResult('#act-pro-out', { ok: false, error: err.message, status: err.status });
      } finally { proSave.disabled = false; }
    });
    const proCurrent = $('#act-pro-current', root);
    if (proCurrent) proCurrent.addEventListener('click', async () => {
      const box = $('#act-pro-out', root);
      box.innerHTML = '<div class="sk sk-line w60" style="height:16px"></div>';
      try {
        const fresh = await getHumanlike(true);
        box.innerHTML = jsonView({ scopes: fresh.scopes || [], proactive: fresh.proactive || [] });
      } catch (err) { box.innerHTML = errorState(err); }
    });

    /* ③ 发消息 */
    const sendBtn = $('#act-send', root);
    if (sendBtn) sendBtn.addEventListener('click', async () => {
      const chatId = $('#act-chat-id', root).value.trim();
      const text = $('#act-chat-text', root).value.trim();
      if (!chatId || !text) { toast('目标会话和消息内容都不能为空', 'warn'); return; }
      const ok = await confirmAction({
        title: '确认以 FM 身份发送消息？',
        detail: '目标：' + chatId + '\n内容：' + truncate(text, 200),
        confirmLabel: '发送',
        danger: true,
      });
      if (!ok) return;
      sendBtn.disabled = true;
      sendBtn.innerHTML = icon('clock') + '发送中…';
      try {
        const response = await apiPost('/api/v2/actions/run', { name: 'send_message', chat_id: chatId, text: text }, { timeout: 30000 });
        const result = unwrap(response);
        toast('消息已提交发送', 'ok');
        showResult('send_message', result);
        showActionResult('#act-send-out', result);
        $('#act-chat-text', root).value = '';
      } catch (err) {
        toast('发送失败：' + err.message, 'bad');
        showActionResult('#act-send-out', { ok: false, error: err.message, status: err.status });
      } finally {
        sendBtn.disabled = false;
        sendBtn.innerHTML = icon('send') + '发送消息';
      }
    });

    /* ④ 重启 */
    const restart = async (service, label) => {
      const ok = await confirmAction({
        title: '重启 ' + label + '？',
        detail: '服务会短暂离线，期间相关功能不可用。请输入 RESTART 以确认。',
        confirmLabel: '重启',
        danger: true,
        requireText: 'RESTART',
      });
      if (!ok) return;
      const out = $('#act-restart-out', root);
      out.innerHTML = '<div class="sk sk-line w60" style="height:16px"></div>';
      try {
        const response = await apiPost('/api/system/restart/' + service, {});
        out.innerHTML = notice('ok', '已发起重启：' + esc(label) + '（服务约 5–20 秒后恢复）');
        showResult('restart ' + service, unwrap(response));
      } catch (err) {
        out.innerHTML = errorState(err);
        showResult('restart ' + service + '（失败）', { error: err.message, status: err.status });
      }
    };
    const botBtn = $('#act-restart-bot', root);
    if (botBtn) botBtn.addEventListener('click', () => restart('cosmobot', 'fm-cosmobot'));
    const domainBtn = $('#act-restart-domain', root);
    if (domainBtn) domainBtn.addEventListener('click', () => restart('domain', 'fm-domain'));
    const adminBtn = $('#act-restart-admin', root);
    if (adminBtn) adminBtn.addEventListener('click', async () => {
      const ok = await confirmAction({
        title: '重启后台本身？',
        detail: 'fm-admin 重启后当前登录会话会失效，页面会跳回登录页。请输入 RESTART 确认。',
        confirmLabel: '重启后台',
        danger: true,
        requireText: 'RESTART',
      });
      if (!ok) return;
      try { await apiPost('/api/system/restart/admin', {}); } catch (err) { /* 连接会断，属正常 */ }
      toast('后台正在重启，即将跳转登录页…', 'warn', { timeout: 4000 });
      setTimeout(() => location.replace('/login'), 2500);
    });

    /* ⑤ 定时消息：只读列表 + 取消 */
    const schedRoot = $('#act-scheduled', root);
    const loadScheduled = async () => {
      if (!schedRoot) return;
      schedRoot.innerHTML = '<div class="sk-table">' + Array(4).fill('<div class="sk sk-line"></div>').join('') + '</div>';
      const scheduled = await probeApi(['/api/v2/scheduled' + qs({ limit: 100 })]);
      if (!ctx.alive()) return;
      if (!scheduled.ok) {
        schedRoot.innerHTML = scheduled.pending
          ? pendingState({ title: '定时消息接口尚未就绪', hint: '后端 /api/v2/scheduled 读取 scheduled_messages 表；接入后这里会显示待发消息并可取消。', path: '/api/v2/scheduled' })
          : errorState(scheduled.error, 'sched-retry');
        const retry = $('#sched-retry', root);
        if (retry) retry.addEventListener('click', loadScheduled);
        return;
      }
      const data = scheduled.data || {};
      const rows = (data.rows || []).map((row) => Object.assign({ __id: row.id }, row));
      schedRoot.innerHTML = '<div class="tbl-toolbar"><span class="muted text-xs">共 ' + fmtNum(data.total) + ' 条定时消息 · 当前显示 ' + rows.length + ' 条</span></div><div id="sched-table"></div>';
      renderTable($('#sched-table', root), {
        columns: [
          { key: 'due_at', label: '计划发送', width: '150px', render: (row) => timeCell(row.due_at) },
          { key: 'remaining_seconds', label: '剩余', width: '96px', render: (row) => row.due ? '<span class="tag tag-info">' + esc(fmtDuration(Number(row.remaining_seconds) * 1000)) + '</span>' : '<span class="tag tag-muted">已过期</span>' },
          { key: 'chat_id', label: '会话', width: '130px', mono: true, hideSm: true, render: (row) => '<span class="mono">' + esc(row.chat_id || '-') + '</span>' },
          { key: 'sender_username', label: '创建人', width: '110px', render: (row) => esc(row.sender_username || row.sender_id || '-') },
          { key: 'preview', label: '内容预览', render: (row) => '<span class="cell-wrap">' + esc(truncate(row.preview || '', 150)) + '</span>' },
          { key: 'actions', label: '', width: '82px', render: (row) => '<button class="btn btn-sm btn-danger" data-sched-cancel="' + escAttr(row.id) + '" type="button">取消</button>' },
        ],
        rows: rows,
        detail: (row) => jsonView(row),
        empty: { icon: 'clock', title: '没有定时消息', hint: '待发送的定时消息会出现在这里' },
      });
      const tableRoot = $('#sched-table', root);
      if (tableRoot) tableRoot.addEventListener('click', async (event) => {
        const btn = event.target.closest('[data-sched-cancel]');
        if (!btn) return;
        const id = btn.getAttribute('data-sched-cancel');
        const row = rows.find((item) => String(item.id) === String(id)) || {};
        const ok = await confirmAction({
          title: '取消这条定时消息？',
          detail: '计划发送：' + (row.due_at || '') + '\n会话：' + (row.chat_id || '') + '\n内容：' + truncate(row.preview || '', 120) + '\n取消后不会补发。',
          confirmLabel: '取消发送',
          danger: true,
        });
        if (!ok) return;
        btn.disabled = true;
        try {
          await apiPost('/api/v2/scheduled/cancel', { id: Number(id) });
          toast('定时消息已取消', 'ok');
          loadScheduled();
        } catch (err) {
          toast('取消失败：' + err.message, 'bad');
          btn.disabled = false;
        }
      });
    };
    loadScheduled();
    const schedRefresh = $('#act-sched-refresh', root);
    if (schedRefresh) schedRefresh.addEventListener('click', () => loadScheduled());
  },
};

/* ══════════════════════════════════════════════════════════════════════════
 * 7.7 模型与配置（只读 + 旧后台入口）
 * 数据源：/api/v2/overview、/api/runtime/config/models、/api/runtime/config/image_models、
 *         /api/models/stats、/api/runtime/config/versions
 * ════════════════════════════════════════════════════════════════════════ */

VIEWS.models = {
  id: 'models',
  name: '模型与配置',
  icon: 'cpu',
  skeleton: 'table',
  async render(ctx) {
    const root = ctx.root;
    const [overviewRes, chatModelsRes, imageModelsRes, statsRes, versionsRes] = await Promise.all([
      apiGet('/api/v2/overview').catch(() => null),
      apiGet('/api/runtime/config/models').catch(() => null),
      apiGet('/api/runtime/config/image_models').catch(() => null),
      apiGet('/api/models/stats').catch(() => null),
      apiGet('/api/runtime/config/versions').catch(() => null),
    ]);
    if (!ctx.alive()) return;
    const overview = overviewRes ? unwrap(overviewRes) : null;
    const config = (overview && overview.config) || { base: {}, chat_models: {}, image_models: {}, ask: {} };
    const chatModels = (chatModelsRes && (chatModelsRes.items || chatModelsRes.data)) || [];
    const imageModels = (imageModelsRes && (imageModelsRes.items || imageModelsRes.data)) || [];
    const stats = statsRes ? (statsRes.data || statsRes) : { models: [], distribution: [] };
    const versions = (versionsRes && (versionsRes.items || [])) || [];
    const currentChat = (overview && overview.chat_model) || ((chatModels.find((item) => item.current) || {}).model) || '-';

    root.innerHTML = pageHead({
      title: '模型与配置',
      icon: 'cpu',
      sub: '当前运行模型、图片模型、请求参数与配置版本 · 本页只读，切换模型请到旧版后台',
      actions: ['<a class="btn btn-ghost btn-sm" href="/classic" target="_blank" rel="noopener">' + icon('external') + '旧后台模型页</a>',
        '<button class="btn btn-ghost btn-sm" id="models-refresh" type="button">' + icon('refresh') + '刷新</button>',
        '<button class="btn btn-ghost btn-sm" data-json-drawer="' + escAttr(jsonSum({ config: config, chatModels: chatModels, imageModels: imageModels }).slice(0, 6000)) + '" data-json-title="模型与配置原始数据" type="button">' + icon('eye') + '原始 JSON</button>'],
    }) +
    kpiGrid([
      { label: '聊天模型', icon: 'cpu', valueHtml: '<span style="font-size:15px">' + esc(currentChat) + '</span>', tone: 'acc', hint: '思考档位：' + ((overview && overview.reasoning_effort) || '-') },
      { label: '已配置聊天模型', icon: 'list', valueHtml: fmtNum(chatModels.length) },
      { label: '图片模型', icon: 'eye', valueHtml: fmtNum(imageModels.length), hint: '当前：' + esc(((imageModels.find((item) => item.current) || {}).model) || '-') },
      { label: '配置版本', icon: 'save', valueHtml: fmtNum(versions.length), hint: versions.length ? '最近 ' + esc(shortTime(versions[0] && (versions[0].at || versions[0].created_at))) : '暂无变更版本' },
    ]) +
    '<div class="grid grid-2 mt-16">' +
      card({ title: '聊天模型', icon: 'cpu', flush: true }, '<div id="chat-models"></div>') +
      card({ title: '图片模型', icon: 'eye', flush: true }, '<div id="image-models"></div>') +
    '</div>' +
    '<div class="grid grid-2 mt-16">' +
      card({ title: '运行参数', icon: 'gear' },
        '<div class="kv-grid">' +
          Object.keys(config.base || {}).sort().map((key) =>
            '<div class="kv-item"><div class="k mono">' + esc(key) + '</div><div class="v mono">' + esc(config.base[key] || '—') + '</div></div>').join('') +
        '</div>' +
        (Object.keys(config.ask || {}).length ? '<div class="section-title mt-16">' + icon('message') + 'Ask / 上下文</div>' +
          '<div class="kv-grid mt-8">' + Object.keys(config.ask).sort().map((key) =>
            '<div class="kv-item"><div class="k mono">' + esc(key.replace('handler.ask.', '')) + '</div><div class="v mono">' + esc(config.ask[key] || '—') + '</div></div>').join('') + '</div>' : '')) +
      card({ title: '模型调用统计', icon: 'chart' },
        '<div class="chart chart-sm" id="model-chart"></div><div id="model-table"></div>' +
        '<div class="card-note">统计来自机器人 audit.recent（最近 500 条事件），仅作近期观测。</div>') +
    '</div>' +
    '<div class="grid grid-2 mt-16">' +
      card({ title: '配置版本（只读）', icon: 'save', flush: true },
        '<div id="config-versions"></div>' +
        '<div class="card-foot">' + icon('info') + ' 回滚与编辑请到旧版后台「设置 → 配置版本」，这里只读展示最近变更。</div>') +
      card({ title: '模型切换入口', icon: 'external' },
        '<div class="stack" style="gap:9px">' +
          '<div class="notice">' + icon('info') + '<div>模型的新增 / 切换 / 测试连接都在旧版后台完成，中控只做只读展示，避免两条链路同时写配置。</div></div>' +
          '<a class="btn btn-soft w-full" href="/classic" target="_blank" rel="noopener">' + icon('external') + '打开旧版后台</a>' +
          '<a class="btn btn-ghost w-full" href="#/browse">' + icon('database') + '用数据浏览器直接调接口</a>' +
          '<div class="card-note">原始接口：<code class="inline">/api/runtime/config/models</code>、<code class="inline">/api/runtime/config/image_models</code>、<code class="inline">/api/runtime/config/versions</code>。</div>' +
        '</div>') +
    '</div>';

    renderTable($('#chat-models', root), {
      columns: [
        { key: 'model', label: '模型', render: (row) => '<span class="mono">' + esc(row.model || row.name || '-') + '</span>' },
        { key: 'provider', label: '提供方', render: (row) => '<span class="tag tag-muted">' + esc(row.provider || '-') + '</span>' },
        { key: 'current', label: '当前', width: '72px', render: (row) => row.current ? '<span class="tag tag-ok">使用中</span>' : '<span class="muted-2">—</span>' },
        { key: 'configured_default', label: '默认', width: '72px', hideSm: true, render: (row) => row.configured_default ? '<span class="tag tag-info">默认</span>' : '<span class="muted-2">—</span>' },
      ],
      rows: chatModels,
      detail: (row) => jsonView(row),
      empty: { icon: 'cpu', title: '没有聊天模型信息', hint: '机器人 RPC 可能不可用' },
    });
    renderTable($('#image-models', root), {
      columns: [
        { key: 'model', label: '模型', render: (row) => '<span class="mono">' + esc(row.model || row.name || '-') + '</span>' },
        { key: 'provider', label: '提供方', render: (row) => '<span class="tag tag-muted">' + esc(row.provider || '-') + '</span>' },
        { key: 'current', label: '当前', width: '72px', render: (row) => row.current ? '<span class="tag tag-ok">使用中</span>' : '<span class="muted-2">—</span>' },
        { key: 'can_generate', label: '可生成', width: '80px', hideSm: true, render: (row) => row.can_generate ? '<span class="tag tag-ok">是</span>' : '<span class="tag tag-muted">否</span>' },
        { key: 'api_key_configured', label: 'Key', width: '64px', hideSm: true, render: (row) => row.api_key_configured ? '<span class="tag tag-ok">已配</span>' : '<span class="tag tag-warn">缺失</span>' },
      ],
      rows: imageModels,
      detail: (row) => jsonView(row),
      empty: { icon: 'eye', title: '没有图片模型信息' },
    });

    renderTable($('#model-table', root), {
      columns: [
        { key: 'name', label: '模型', render: (row) => '<span class="mono">' + esc(row.name || '-') + '</span>' },
        { key: 'calls', label: '调用', width: '80px', num: true, render: (row) => fmtNum(row.calls) },
        { key: 'tokens', label: 'Tokens', width: '100px', num: true, hideSm: true, render: (row) => fmtNum(row.tokens) },
        { key: 'average_ms', label: '平均耗时', width: '96px', num: true, hideSm: true, render: (row) => fmtNum(row.average_ms) + ' ms' },
      ],
      rows: (stats.models || []),
      detail: (row) => jsonView(row),
      empty: { icon: 'chart', title: '暂无调用统计' },
    });
    const distribution = (stats.distribution || []).filter((item) => Number(item.value) > 0);
    const chartEl = $('#model-chart', root);
    if (distribution.length) {
      mountChart(chartEl, {
        backgroundColor: 'transparent',
        color: CHART_COLORS,
        textStyle: { color: '#8b98ac', fontFamily: chartFont(), fontSize: 11 },
        tooltip: {
          trigger: 'item', backgroundColor: 'rgba(15,19,26,.96)', borderColor: '#2e3949', borderWidth: 1,
          textStyle: { color: '#e9eef7', fontSize: 11.5 },
        },
        legend: { orient: 'vertical', right: 0, top: 'center', itemWidth: 9, itemHeight: 9, textStyle: { color: '#8b98ac', fontSize: 10.5 } },
        series: [{
          type: 'pie', radius: ['52%', '78%'], center: ['36%', '50%'],
          itemStyle: { borderColor: '#0f131a', borderWidth: 2 },
          label: { show: false },
          data: distribution.map((item) => ({ name: item.name, value: item.value })),
        }],
      }, distribution.map((item) => ({ label: item.name, value: item.value })));
    } else {
      chartNoData(chartEl, '没有可统计的 token 分布');
    }

    renderTable($('#config-versions', root), {
      columns: [
        { key: 'at', label: '时间', width: '150px', render: (row) => timeCell(row.at || row.created_at) },
        { key: 'kind', label: '类型', width: '92px', render: (row) => '<span class="tag tag-muted mono">' + esc(row.kind || '-') + '</span>' },
        { key: 'operation', label: '操作', width: '92px', render: (row) => '<span class="tag ' + (row.operation === 'rollback' ? 'tag-warn' : 'tag-info') + '">' + esc(row.operation || 'save') + '</span>' },
        { key: 'actor', label: '操作人', width: '96px', render: (row) => esc(row.actor || '-') },
        { key: 'id', label: '版本 ID', mono: true, hideSm: true, render: (row) => '<span class="mono">' + esc(truncate(row.id, 20)) + '</span>' },
      ],
      rows: versions.slice(0, 30),
      detail: (row) => jsonView({ id: row.id, kind: row.kind, operation: row.operation, actor: row.actor, at: row.at, changes: row.changes || row.request || null }),
      empty: { icon: 'save', title: '暂无配置版本', hint: '后台保存配置变更后会自动生成版本快照' },
    });

    const refreshBtn = $('#models-refresh', root);
    if (refreshBtn) refreshBtn.addEventListener('click', () => route());
  },
};

function shortTime(value) {
  const ms = toMs(value);
  return ms == null ? '' : fmtTime(ms, { noSeconds: true });
}

/* ══════════════════════════════════════════════════════════════════════════
 * 7.8 工具与审计：统计条形图 + 明细表（工具/状态/天数筛选）
 * 数据源：/api/v2/tools/stats?days=、/api/v2/tools/audit?days=&tool=&status=、
 *         新接口 /api/v2/tools/catalog
 * ════════════════════════════════════════════════════════════════════════ */

VIEWS.tools = {
  id: 'tools',
  name: '工具与审计',
  icon: 'wrench',
  skeleton: 'table',
  async render(ctx) {
    const root = ctx.root;
    const days = ctx.q('days', '7');
    const toolFilter = ctx.q('tool');
    const statusFilter = ctx.q('status');
    const statsRes = await apiGet('/api/v2/tools/stats' + qs({ days: days })).then(unwrap).catch((err) => ({ __error: err }));
    if (!ctx.alive()) return;
    if (statsRes && statsRes.__error) { showViewError(root, statsRes.__error, () => route()); return; }
    const statsRows = (statsRes && statsRes.rows) || [];
    const totalCalls = Number(statsRes && statsRes.total_calls || statsRows.reduce((sum, row) => sum + Number(row.calls || 0), 0));
    const totalFailures = statsRows.reduce((sum, row) => sum + Number(row.failures || 0), 0);
    const catalogRes = await probeApi(['/api/v2/tools/catalog']);
    const auditRes = await probeApi([
      '/api/v2/tools/audit' + qs({ days: days, tool: toolFilter, status: statusFilter, limit: 200 }),
      '/api/v2/tools/audit' + qs({ days: days, limit: 200 }),
    ]);
    if (!ctx.alive()) return;
    const auditRows = auditRes.ok ? (((auditRes.data && auditRes.data.rows) || []).map((row, index) => Object.assign({ __id: index }, row))) : [];
    const catalog = catalogRes.ok ? (pickArray(catalogRes.data) || []) : [];

    root.innerHTML = pageHead({
      title: '工具与审计',
      icon: 'wrench',
      sub: '近 ' + days + ' 天工具调用统计与调用明细 · 数据来自机器人 audit_log（tool_call_finished）',
      actions: ['<a class="btn btn-ghost btn-sm" href="#/logs">' + icon('terminal') + '运行日志</a>',
        '<button class="btn btn-ghost btn-sm" id="tools-refresh" type="button">' + icon('refresh') + '刷新</button>',
        '<button class="btn btn-ghost btn-sm" data-json-drawer="' + escAttr(jsonSum({ stats: statsRows, audit: auditRows }).slice(0, 6000)) + '" data-json-title="工具统计与明细" type="button">' + icon('eye') + '原始 JSON</button>'],
    }) +
    kpiGrid([
      { label: '工具种类', icon: 'wrench', valueHtml: fmtNum(statsRows.length), tone: 'acc' },
      { label: '总调用', icon: 'activity', valueHtml: fmtNum(totalCalls), hint: '近 ' + days + ' 天' },
      { label: '失败次数', icon: 'alert', valueHtml: fmtNum(totalFailures), tone: totalFailures ? 'bad' : 'ok' },
      { label: '失败率', icon: 'chart', valueHtml: fmtPct(totalFailures, totalCalls), tone: totalFailures ? 'warn' : 'ok' },
    ]) +
    '<div class="grid grid-wide mt-16">' +
      card({ title: '调用量 Top 工具', icon: 'chart', headActions: [
        '<select class="select select-sm" id="tools-days" aria-label="统计天数">' +
          ['1', '7', '14', '30', '90'].map((value) => '<option value="' + value + '"' + (days === value ? ' selected' : '') + '>近 ' + value + ' 天</option>').join('') +
        '</select>'] },
        '<div class="chart" id="tools-chart"></div>') +
      card({ title: '工具目录', icon: 'list', headActions: [catalogRes.pending ? '<span class="tag tag-warn">目录接口待接入</span>' : ''] },
        catalogRes.pending
          ? '<div class="notice">' + icon('clock') + '<div>后端 <code class="inline">/api/v2/tools/catalog</code> 尚未就绪，下面用近期调用统计推断工具清单。</div></div><div id="tools-catalog" class="mt-12"></div>'
          : '<div id="tools-catalog"></div>') +
    '</div>' +
    card({ title: '调用明细', icon: 'terminal', flush: true, className: 'mt-16' },
      '<div class="tbl-toolbar">' +
        '<div class="field" style="flex:0 1 200px"><select class="select select-sm" id="audit-tool"><option value="">全部工具</option>' +
          statsRows.map((row) => '<option value="' + escAttr(row.tool) + '"' + (row.tool === toolFilter ? ' selected' : '') + '>' + esc(row.tool) + '</option>').join('') +
        '</select></div>' +
        '<div class="field" style="flex:0 1 160px"><select class="select select-sm" id="audit-status"><option value="">全部状态</option>' +
          ['ok', 'error', 'failed', 'timeout'].map((value) => '<option value="' + value + '"' + (value === statusFilter ? ' selected' : '') + '>' + value + '</option>').join('') +
        '</select></div>' +
        '<button class="btn btn-sm btn-ghost" id="audit-filter" type="button">' + icon('filter') + '筛选</button>' +
        '<span class="muted text-xs">最多返回 200 条</span>' +
      '</div>' +
      '<div id="audit-table"></div>');

    /* 统计图 */
    const chartEl = $('#tools-chart', root);
    if (statsRows.length) {
      const top = statsRows.slice(0, 16).slice().reverse();
      mountChart(chartEl, chartBase({
        grid: { left: 8, right: 30, top: 26, bottom: 4, containLabel: true },
        tooltip: { trigger: 'axis', axisPointer: { type: 'shadow' } },
        legend: { data: ['成功', '失败'], top: 0, right: 0 },
        xAxis: { type: 'value', splitLine: { show: false } },
        yAxis: { type: 'category', data: top.map((row) => truncate(row.tool, 18)), axisLabel: { color: '#c5cfdd', fontSize: 10.5 }, axisLine: { show: false } },
        series: [
          { name: '成功', type: 'bar', stack: 'total', barMaxWidth: 13, data: top.map((row) => Math.max(0, Number(row.calls || 0) - Number(row.failures || 0))),
            itemStyle: { color: '#5b8cff', borderRadius: [0, 0, 0, 0] } },
          { name: '失败', type: 'bar', stack: 'total', barMaxWidth: 13, data: top.map((row) => Number(row.failures || 0)),
            itemStyle: { color: '#f87171', borderRadius: [0, 4, 4, 0] } },
        ],
      }), top.map((row) => ({ label: row.tool, value: row.calls })));
    } else {
      chartNoData(chartEl, '没有工具调用统计');
    }

    /* 工具目录 / 推断清单 */
    const catalogRoot = $('#tools-catalog', root);
    if (catalogRoot) {
      const entries = catalog.length
        ? catalog.map((item) => ({
            name: item.name || item.tool || 'unknown',
            description: item.description || item.desc || item.summary || '',
            calls: item.calls,
            failures: item.failures,
          }))
        : statsRows.map((row) => ({ name: row.tool, description: '', calls: row.calls, failures: row.failures, last: row.last }));
      catalogRoot.innerHTML = entries.length
        ? '<div class="list-rows" style="max-height:340px;overflow:auto">' + entries.slice(0, 40).map((entry) => {
            const stat = statsRows.find((row) => row.tool === entry.name) || {};
            return '<div class="list-row" data-json-drawer="' + escAttr(jsonSum(entry).slice(0, 1500)) + '" data-json-title="工具 · ' + escAttr(entry.name) + '">' +
              '<div class="lr-main"><div class="lr-title mono">' + esc(entry.name) + '</div>' +
              '<div class="lr-sub">' + esc(truncate(entry.description || '近 ' + days + ' 天调用 ' + fmtNum(stat.calls || entry.calls || 0) + ' 次' + (stat.failures ? ' · 失败 ' + stat.failures : ''), 70)) + '</div></div>' +
              (Number(stat.calls || entry.calls || 0) ? '<span class="lr-extra">' + fmtNum(stat.calls || entry.calls) + '</span>' : '') +
            '</div>';
          }).join('') + '</div>'
        : emptyState({ icon: 'wrench', title: '没有工具数据', hint: '近 ' + days + ' 天没有工具调用记录' });
    }

    /* 明细表 */
    const auditRoot = $('#audit-table', root);
    if (auditRes.ok) {
      renderTable(auditRoot, {
        columns: [
          { key: 'at', label: '时间', width: '150px', render: (row) => timeCell(row.at) },
          { key: 'tool', label: '工具', width: '180px', render: (row) => '<span class="mono">' + esc(row.tool || '-') + '</span>' },
          { key: 'status', label: '状态', width: '92px', render: (row) => '<span class="tag ' + (['ok', 'success'].indexOf(String(row.status)) >= 0 ? 'tag-ok' : 'tag-bad') + '">' + esc(row.status || 'unknown') + '</span>' },
          { key: 'result', label: '结果摘要', render: (row) => '<span class="cell-wrap">' + esc(truncate(row.result || '', 160)) + '</span>' },
        ],
        rows: auditRows,
        detail: (row) => jsonView(row),
        empty: { icon: 'terminal', title: '没有调用明细', hint: '换个工具 / 状态 / 天数再试' },
      });
    } else if (auditRes.pending) {
      auditRoot.innerHTML = pendingState({ title: '调用明细接口尚未就绪', hint: '统计接口可用，但 /api/v2/tools/audit 还没有返回数据；接入后这里会自动出现明细。', path: auditRes.path });
    } else {
      auditRoot.innerHTML = errorState(auditRes.error, 'audit-retry') +
        '<div class="notice mt-12">' + icon('info') + '<div>后端 <code class="inline">app.py</code> 的 <code class="inline">tools/audit</code> 分派把 <code class="inline">tool</code> 参数传给了 <code class="inline">days</code>（函数签名是 <code class="inline">tools_audit(days, tool, status, limit)</code>），所以接口固定 500；需要后端线改一行参数顺序。统计图与工具汇总不受影响。</div></div>';
      const retry = $('#audit-retry', root);
      if (retry) retry.addEventListener('click', () => route());
    }

    const daysEl = $('#tools-days', root);
    if (daysEl) daysEl.addEventListener('change', () => {
      const query = new URLSearchParams();
      query.set('days', daysEl.value);
      if (toolFilter) query.set('tool', toolFilter);
      if (statusFilter) query.set('status', statusFilter);
      go(hashFor('tools', [], query));
    });
    const auditFilter = $('#audit-filter', root);
    if (auditFilter) auditFilter.addEventListener('click', () => {
      const query = new URLSearchParams();
      query.set('days', days);
      const tool = $('#audit-tool', root).value;
      const status = $('#audit-status', root).value;
      if (tool) query.set('tool', tool);
      if (status) query.set('status', status);
      go(hashFor('tools', [], query));
    });
    const refreshBtn = $('#tools-refresh', root);
    if (refreshBtn) refreshBtn.addEventListener('click', () => route());
  },
};


/* ══════════════════════════════════════════════════════════════════════════
 * 7.9 业务数据：文库 / 赛文 / 成绩 / 番茄
 * 数据源：/api/domain/stats、/api/domain/library、/api/domain/contests、
 *         /api/domain/scores、/api/domain/scores/user/<name>（成员成绩趋势）、
 *         /api/v2/novels（番茄）
 * 注意：旧 /api/domain/* 代理会无脑拼 "?"，所以这些请求一律不带 query。
 * ════════════════════════════════════════════════════════════════════════ */

const BUSINESS_TABS = [
  { id: 'library', label: '跟打文库', icon: 'book' },
  { id: 'contest', label: '群赛文', icon: 'list' },
  { id: 'scores', label: '成绩', icon: 'chart' },
  { id: 'novel', label: '番茄小说', icon: 'book' },
];

VIEWS.business = {
  id: 'business',
  name: '业务数据',
  icon: 'book',
  skeleton: 'table',
  crumb(route) {
    const tab = route.segments[1];
    const found = BUSINESS_TABS.find((item) => item.id === tab);
    return found ? found.label : null;
  },
  async render(ctx) {
    const root = ctx.root;
    const tab = BUSINESS_TABS.some((item) => item.id === ctx.segments[1]) ? ctx.segments[1] : 'library';
    const keyword = ctx.q('q');
    const statsRes = await apiGet('/api/domain/stats').catch(() => null);
    if (!ctx.alive()) return;
    const stats = (statsRes && (statsRes.data || statsRes)) || {};

    let libraryData = null;
    let contestData = null;
    let scoresData = null;
    let novelData = null;
    let loadError = null;
    if (tab === 'library') {
      try { libraryData = (await apiGet('/api/domain/library')).data || []; } catch (err) { loadError = err; }
    } else if (tab === 'contest') {
      try { contestData = (await apiGet('/api/domain/contests')).data || []; } catch (err) { loadError = err; }
    } else if (tab === 'scores') {
      try { scoresData = (await apiGet('/api/domain/scores')).data || []; } catch (err) { loadError = err; }
    } else {
      novelData = await apiGet('/api/v2/novels').then(unwrap).catch((err) => ({ ok: false, error: err.message }));
    }
    if (!ctx.alive()) return;

    const counts = {
      library: Number(stats.library_texts || (libraryData ? libraryData.length : 0)),
      contest: Number(stats.contest_texts || (contestData ? contestData.length : 0)),
      scores: Number(stats.score_records || (scoresData ? scoresData.length : 0)),
      novel: novelData && novelData.ok ? Number(novelData.books || 0) : null,
    };

    root.innerHTML = pageHead({
      title: '业务数据',
      icon: 'book',
      sub: '跟打文库、群赛文、成绩与番茄小说 · 数据来自 FM Domain（含近期记录明细）',
      actions: ['<button class="btn btn-ghost btn-sm" id="biz-refresh" type="button">' + icon('refresh') + '刷新</button>',
        '<a class="btn btn-ghost btn-sm" href="#/browse">' + icon('database') + '数据浏览器</a>'],
    }) +
    kpiGrid([
      { label: '文库条目', icon: 'book', valueHtml: fmtNum(counts.library), tone: 'acc' },
      { label: '群赛文条目', icon: 'list', valueHtml: fmtNum(counts.contest) },
      { label: '成绩记录', icon: 'chart', valueHtml: fmtNum(counts.scores) },
      { label: 'AI 赛文成绩', icon: 'spark', valueHtml: fmtNum(stats.ai_contest_scores), hint: 'AI 赛文 ' + fmtNum(stats.ai_contest_texts) + ' 条' },
    ]) +
    tabsHtml(BUSINESS_TABS.map((item) => ({ id: item.id, label: item.label, icon: item.icon, count: counts[item.id] })), tab) +
    '<div id="biz-content"></div>';

    const content = $('#biz-content', root);
    const tabsRoot = $('.tabs', root);
    if (tabsRoot) tabsRoot.addEventListener('click', (event) => {
      const button = event.target.closest('[data-tab]');
      if (!button) return;
      go(hashFor('business', [button.getAttribute('data-tab')], {}));
    });

    if (loadError) {
      content.innerHTML = errorState(loadError, 'biz-retry');
      const retry = $('#biz-retry', root);
      if (retry) retry.addEventListener('click', () => route());
      return;
    }

    if (tab === 'library') {
      const rows = (libraryData || []).filter((row) => {
        if (!keyword) return true;
        const hay = String(row.title || '') + ' ' + String(row.relative_path || '') + ' ' + String(row.category || '');
        return hay.toLowerCase().indexOf(keyword.toLowerCase()) >= 0;
      });
      content.innerHTML = card({ title: '跟打文库', icon: 'book', flush: true, headActions: ['<span class="muted text-xs">接口返回最近 ' + (libraryData || []).length + ' 条</span>'] },
        '<div class="tbl-toolbar"><div class="field grow"><input class="input input-sm" id="biz-q" placeholder="搜索标题 / 路径 / 分类…" value="' + escAttr(keyword) + '"></div>' +
          '<span class="muted text-xs">' + rows.length + ' 条</span></div>' +
        '<div id="biz-table"></div>');
      renderTable($('#biz-table', root), {
        columns: [
          { key: 'title', label: '标题', render: (row) => '<span class="cell-truncate">' + esc(row.title || row.text_id) + '</span>' },
          { key: 'category', label: '分类', width: '120px', render: (row) => '<span class="tag tag-muted">' + esc(row.category || '-') + '</span>' },
          { key: 'char_count', label: '字数', width: '86px', num: true, render: (row) => fmtNum(row.char_count) },
          { key: 'relative_path', label: '路径', mono: true, hideSm: true, render: (row) => '<span class="cell-truncate mono">' + esc(row.relative_path || '') + '</span>' },
        ],
        rows: rows,
        onRow: (row) => openJsonDrawer('文库文本', row, row.title || ''),
        empty: { icon: 'book', title: keyword ? '没有匹配的文库文本' : '文库为空' },
      });
    } else if (tab === 'contest') {
      const rows = (contestData || []).filter((row) => {
        if (!keyword) return true;
        const hay = String(row.title || '') + ' ' + String(row.source_group || '') + ' ' + String(row.competition_date || '');
        return hay.toLowerCase().indexOf(keyword.toLowerCase()) >= 0;
      });
      content.innerHTML = card({ title: '群赛文', icon: 'list', flush: true, headActions: ['<span class="muted text-xs">接口返回最近 ' + (contestData || []).length + ' 条</span>'] },
        '<div class="tbl-toolbar"><div class="field grow"><input class="input input-sm" id="biz-q" placeholder="搜索标题 / 来源群 / 日期…" value="' + escAttr(keyword) + '"></div>' +
          '<span class="muted text-xs">' + rows.length + ' 条</span></div>' +
        '<div id="biz-table"></div>');
      renderTable($('#biz-table', root), {
        columns: [
          { key: 'title', label: '标题', render: (row) => '<span class="cell-truncate">' + esc(row.title || row.text_id) + '</span>' },
          { key: 'source_group', label: '来源群', width: '150px', render: (row) => '<span class="cell-truncate">' + esc(row.source_group || '-') + '</span>' },
          { key: 'competition_date', label: '比赛日期', width: '110px', mono: true, render: (row) => '<span class="mono">' + esc(row.competition_date || '-') + '</span>' },
          { key: 'char_count', label: '字数', width: '86px', num: true, render: (row) => fmtNum(row.char_count) },
        ],
        rows: rows,
        onRow: (row) => openJsonDrawer('赛文详情', row, row.title || ''),
        empty: { icon: 'list', title: keyword ? '没有匹配的赛文' : '赛文库为空' },
      });
    } else if (tab === 'scores') {
      const allScores = (scoresData || []).filter((row) => row && row.speed != null);
      const groupNames = {};
      allScores.forEach((row) => { groupNames[String(row.group_id || '')] = row.group_name || row.group_id || ''; });
      const groupFilter = ctx.q('group');
      const filtered = allScores.filter((row) => {
        if (groupFilter && String(row.group_id) !== String(groupFilter)) return false;
        if (!keyword) return true;
        const hay = String(row.sender_name || '') + ' ' + String(row.group_name || '') + ' ' + String(row.segment_id || '');
        return hay.toLowerCase().indexOf(keyword.toLowerCase()) >= 0;
      });
      content.innerHTML =
        card({ title: '成绩趋势 · 最近 ' + filtered.length + ' 条', icon: 'chart', headActions: ['<span class="muted text-xs">速度 / 键准</span>'] },
          '<div class="chart" id="score-chart"></div>' +
          '<div class="chart-toolbar mt-8" style="justify-content:flex-start">' +
            '<select class="select select-sm" id="score-group" style="max-width:220px"><option value="">全部群</option>' +
              Object.keys(groupNames).map((gid) => '<option value="' + escAttr(gid) + '"' + (String(gid) === String(groupFilter) ? ' selected' : '') + '>' + esc(truncate(groupNames[gid], 20)) + '</option>').join('') +
            '</select>' +
            '<span class="muted text-xs">按时间正序绘制，数值来自 /api/domain/scores（最近 50 条）</span>' +
          '</div>') +
        '<div class="grid grid-wide mt-16">' +
          card({ title: '成绩明细', icon: 'list', flush: true },
            '<div class="tbl-toolbar"><div class="field grow"><input class="input input-sm" id="biz-q" placeholder="搜索成员 / 群名…" value="' + escAttr(keyword) + '"></div>' +
              '<span class="muted text-xs">' + filtered.length + ' 条</span></div>' +
            '<div id="biz-table"></div>') +
          card({ title: '成员成绩查询', icon: 'search' },
            '<div class="row mb-12"><div class="field grow"><input class="input input-sm" id="score-user" placeholder="输入成员昵称，例如 leaf"></div>' +
            '<button class="btn btn-sm btn-solid" id="score-user-go" type="button">' + icon('search') + '查询</button></div>' +
            '<div id="score-user-out">' + emptyState({ icon: 'chart', title: '查询单个成员的成绩曲线', hint: '接口 /api/domain/scores/user/<昵称> 会聚合成标签、最好成绩与趋势' }) + '</div>') +
        '</div>';

      const chartEl = $('#score-chart', root);
      if (filtered.length) {
        const ordered = filtered.slice().sort((a, b) => Number(a.received_at || 0) - Number(b.received_at || 0));
        mountChart(chartEl, chartBase({
          legend: { data: ['速度', '键准'], top: 0, right: 0 },
          xAxis: { type: 'category', data: ordered.map((row) => fmtTime(row.received_at, { noSeconds: true })) },
          yAxis: [
            { type: 'value', axisLabel: { color: '#66748a', fontSize: 10.5, formatter: '{value}' }, splitLine: { lineStyle: { color: 'rgba(34,43,56,.65)', type: 'dashed' } } },
            { type: 'value', min: 0, max: 100, axisLabel: { color: '#34d399', fontSize: 10.5, formatter: '{value}%' }, splitLine: { show: false } },
          ],
          series: [
            { name: '速度', type: 'line', smooth: true, symbol: 'circle', symbolSize: 3.5, data: ordered.map((row) => Number(row.speed || 0)),
              lineStyle: { width: 2, color: '#5b8cff' }, itemStyle: { color: '#5b8cff' },
              areaStyle: { color: { type: 'linear', x: 0, y: 0, x2: 0, y2: 1, colorStops: [{ offset: 0, color: 'rgba(91,140,255,.28)' }, { offset: 1, color: 'rgba(91,140,255,0)' }] } } },
            { name: '键准', type: 'line', yAxisIndex: 1, smooth: true, symbol: 'none', data: ordered.map((row) => Number(row.accuracy || 0)),
              lineStyle: { width: 1.6, color: '#34d399', type: 'dashed' }, itemStyle: { color: '#34d399' } },
          ],
          grid: { top: 32 },
        }), ordered.map((row) => ({ label: row.sender_name || '', value: row.speed })));
      } else {
        chartNoData(chartEl, '没有可绘制的成绩');
      }

      renderTable($('#biz-table', root), {
        columns: [
          { key: 'received_at', label: '时间', width: '150px', render: (row) => timeCell(row.received_at) },
          { key: 'sender_name', label: '成员', width: '140px', render: (row) => '<span class="cell-truncate">' + esc(row.sender_name || '-') + '</span>' },
          { key: 'group_name', label: '群', width: '140px', hideSm: true, render: (row) => '<span class="cell-truncate">' + esc(row.group_name || row.group_id || '-') + '</span>' },
          { key: 'speed', label: '速度', width: '84px', num: true, render: (row) => '<span class="mono">' + fmtNum(row.speed, 2) + '</span>' },
          { key: 'keystroke', label: '击键', width: '76px', num: true, hideSm: true, render: (row) => fmtNum(row.keystroke, 2) },
          { key: 'accuracy', label: '键准', width: '80px', num: true, render: (row) => fmtNum(row.accuracy, 2) + '%' },
          { key: 'characters', label: '字数', width: '74px', num: true, hideSm: true, render: (row) => fmtNum(row.characters) },
        ],
        rows: filtered,
        onRow: (row) => openJsonDrawer('成绩详情', row, (row.sender_name || '') + ' · ' + fmtNum(row.speed, 2) + ' 字/分'),
        empty: { icon: 'chart', title: '没有匹配的成绩' },
      });

      const groupSel = $('#score-group', root);
      if (groupSel) groupSel.addEventListener('change', () => {
        const query = new URLSearchParams();
        if (groupSel.value) query.set('group', groupSel.value);
        if (keyword) query.set('q', keyword);
        go(hashFor('business', ['scores'], query));
      });

      const userBtn = $('#score-user-go', root);
      if (userBtn) userBtn.addEventListener('click', async () => {
        const name = $('#score-user', root).value.trim();
        const out = $('#score-user-out', root);
        if (!name) { toast('请输入成员昵称', 'warn'); return; }
        out.innerHTML = '<div class="sk sk-block" style="height:160px"></div>';
        try {
          const response = await apiGet('/api/domain/scores/user/' + encodeURIComponent(name));
          const detail = unwrap(response) || {};
          const trend = detail.trend || [];
          out.innerHTML =
            '<div class="row mb-8"><span class="tag tag-acc">' + esc(detail.username || name) + '</span>' +
              '<span class="muted text-xs">' + fmtNum(detail.attempts) + ' 次 · 最好 ' + fmtNum(detail.best_speed, 2) + ' 字/分 · 平均 ' + fmtNum(detail.average_speed, 2) + '</span>' +
              '<span class="spacer"></span><button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(detail).slice(0, 5000)) + '" data-json-title="成员成绩详情" type="button">' + icon('eye') + 'JSON</button></div>' +
            '<div class="chart chart-sm" id="score-user-chart"></div>' +
            '<div class="chips mt-8">' + ((detail.categories || []).map((cat) => '<span class="tag">' + esc(cat.name) + ' · ' + fmtNum(cat.attempts) + ' 次</span>').join('') || '<span class="muted-2">暂无分类统计</span>') + '</div>';
          const el = $('#score-user-chart', root);
          if (trend.length && el) {
            mountChart(el, chartBase({
              xAxis: { type: 'category', data: trend.map((row) => row.date || '') },
              yAxis: { type: 'value' },
              series: [{ name: '速度', type: 'line', smooth: true, data: trend.map((row) => row.speed), lineStyle: { color: '#5b8cff' }, itemStyle: { color: '#5b8cff' } }],
            }), trend.map((row) => ({ label: row.date, value: row.speed })));
          } else if (el) {
            chartNoData(el, '该成员没有可绘制的成绩趋势');
          }
        } catch (err) {
          out.innerHTML = errorState(err);
        }
      });
    } else {
      /* 番茄小说 */
      const novelOk = !!(novelData && novelData.ok);
      content.innerHTML = card({ title: '番茄小说', icon: 'book', headActions: [novelOk ? '<span class="tag tag-ok">已接入</span>' : '<span class="tag tag-warn">占位</span>'] },
        novelOk
          ? '<div class="grid grid-4 mb-16">' +
              '<div class="cap">' + icon('book') + '<span>书籍</span><b class="spacer" style="text-align:right">' + fmtNum(novelData.books) + '</b></div>' +
              '<div class="cap">' + icon('list') + '<span>章节</span><b class="spacer" style="text-align:right">' + fmtNum(novelData.chapters) + '</b></div>' +
              '<div class="cap">' + icon('activity') + '<span>进度记录</span><b class="spacer" style="text-align:right">' + fmtNum(novelData.progress) + '</b></div>' +
            '</div><div id="novel-table"></div>'
          : pendingState({
              title: '番茄书城接口尚未就绪',
              hint: '前端已接好 /api/v2/novels（后端会代理 FM Domain 的 POST /novel/stats）。当前 Domain 版本没有该路由，返回：' +
                (novelData && novelData.error ? novelData.error : '空响应'),
              path: '/api/v2/novels',
              raw: novelData,
            }) +
            '<div class="notice mt-12">' + icon('info') + '<div>这一页是占位实现：接口一旦返回 <code class="inline">ok: true</code>，书籍列表 / 章节 / 进度会自动渲染，不需要改前端。</div></div>');
      if (novelOk) {
        renderTable($('#novel-table', root), {
          columns: [
            { key: 'title', label: '书名', render: (row) => '<span class="cell-truncate">' + esc(row.title || '-') + '</span>' },
            { key: 'author', label: '作者', width: '140px' },
            { key: 'total_chapters', label: '章节', width: '80px', num: true, render: (row) => fmtNum(row.total_chapters) },
            { key: 'word_count', label: '字数', width: '100px', num: true, render: (row) => fmtNum(row.word_count) },
          ],
          rows: novelData.items || [],
          detail: (row) => jsonView(row),
          empty: { icon: 'book', title: '书架为空' },
        });
      }
    }

    const searchInput = $('#biz-q', root);
    if (searchInput) searchInput.addEventListener('keydown', (event) => {
      if (event.key !== 'Enter') return;
      const query = new URLSearchParams();
      const q = searchInput.value.trim();
      if (q) query.set('q', q);
      if (tab === 'scores' && ctx.q('group')) query.set('group', ctx.q('group'));
      go(hashFor('business', [tab], query));
    });
    const refreshBtn = $('#biz-refresh', root);
    if (refreshBtn) refreshBtn.addEventListener('click', () => route());
  },
};

/* ══════════════════════════════════════════════════════════════════════════
 * 7.10 数据浏览器：表 → 行 → JSON + 任意 /api 调试器
 * 数据源：/api/v2/browse/tables、/api/v2/browse/rows?table=&q=&limit=&offset=
 * ════════════════════════════════════════════════════════════════════════ */

const RAW_PRESETS = [
  '/api/v2/overview', '/api/v2/groups', '/api/v2/humanlike', '/api/v2/members/all?limit=20',
  '/api/v2/messages?days=7&limit=20', '/api/v2/tools/stats?days=7', '/api/v2/browse/tables',
  '/api/observability/overview', '/api/logs?limit=20', '/api/errors/stats', '/api/health/score',
  '/api/runtime/config/models', '/api/runtime/config/versions', '/api/domain/stats',
  '/api/domain/library', '/api/domain/scores', '/api/auth/me', '/api/v2/novels',
  '/api/v2/charts/messages', '/api/v2/deploy/status',
];

VIEWS.browse = {
  id: 'browse',
  name: '数据浏览器',
  icon: 'database',
  skeleton: 'table',
  crumb(route) { return route.segments[1] ? decodeSegment(route.segments[1]) : null; },
  async render(ctx) {
    const root = ctx.root;
    if (ctx.segments[1]) { await renderBrowseRows(ctx, ctx.segments[1]); return; }

    const tablesRes = await apiGet('/api/v2/browse/tables').then(unwrap).catch((err) => ({ __error: err }));
    if (!ctx.alive()) return;
    if (tablesRes && tablesRes.__error) { showViewError(root, tablesRes.__error, () => route()); return; }
    const tables = (tablesRes && tablesRes.tables) || [];

    root.innerHTML = pageHead({
      title: '数据浏览器',
      icon: 'database',
      sub: '本地 SQLite 表浏览 + 任意 /api 接口调试（GET / POST / PUT / DELETE），全部只读展示',
      actions: ['<button class="btn btn-ghost btn-sm" id="browse-refresh" type="button">' + icon('refresh') + '刷新表清单</button>'],
    }) +
    kpiGrid([
      { label: '可浏览表', icon: 'database', valueHtml: fmtNum(tables.length), tone: 'acc' },
      { label: '总行数', icon: 'list', valueHtml: fmtNum(tables.reduce((sum, table) => sum + Number(table.rows || 0), 0)) },
      { label: '最大表', icon: 'chart', valueHtml: '<span style="font-size:14px">' + esc((tables.slice().sort((a, b) => Number(b.rows) - Number(a.rows))[0] || {}).table || '—') + '</span>' },
      { label: '接口调试', icon: 'terminal', valueHtml: '<span style="font-size:14px">' + RAW_PRESETS.length + ' 个预设</span>', hint: '支持任意 /api 路径' },
    ]) +
    '<div class="grid grid-wide">' +
      card({ title: '表清单', icon: 'database', flush: true },
        '<div id="browse-tables"></div>' +
        '<div class="card-note">点表名进入行浏览；行可展开看完整 JSON。大表默认按 rowid 分页读取。</div>') +
      card({ title: '接口调试器', icon: 'terminal' },
        '<div class="stack" style="gap:10px">' +
          '<div class="row"><select class="select" id="raw-method" style="flex:0 0 104px">' +
            ['GET', 'POST', 'PUT', 'DELETE'].map((method) => '<option value="' + method + '">' + method + '</option>').join('') +
          '</select>' +
          '<input class="input mono" id="raw-path" list="raw-presets" value="/api/v2/overview" style="flex:1">' +
          '<datalist id="raw-presets">' + RAW_PRESETS.map((preset) => '<option value="' + escAttr(preset) + '"></option>').join('') + '</datalist></div>' +
          '<div class="field"><label>请求体（GET 可留空；JSON）</label><textarea class="textarea mono" id="raw-body" placeholder=\'{"name":"send_message","chat_id":"…","text":"…"}\'></textarea></div>' +
          '<div class="row"><button class="btn btn-sm btn-solid" id="raw-send" type="button">' + icon('send') + '发送请求</button>' +
            '<button class="btn btn-sm btn-ghost" id="raw-clear" type="button">清空</button>' +
            '<span class="muted text-xs">返回体会带 HTTP 状态与耗时</span></div>' +
          '<div id="raw-out"></div>' +
        '</div>') +
    '</div>';

    renderTable($('#browse-tables', root), {
      columns: [
        { key: 'label', label: '表', render: (row) => '<span class="mono" style="color:#c3d6ff">' + esc(row.table) + '</span><div class="muted-2 text-xs">' + esc(row.label || '') + '</div>' },
        { key: 'rows', label: '行数', width: '110px', num: true, render: (row) => '<span class="mono">' + fmtNum(row.rows) + '</span>' },
        { key: 'columns', label: '关键列', hideSm: true, render: (row) => (row.columns || []).length ? (row.columns || []).slice(0, 5).map((col) => '<span class="tag tag-muted mono">' + esc(col) + '</span>').join(' ') : '<span class="muted-2">自动探测</span>' },
      ],
      rows: tables,
      onRow: (row) => { go(hashFor('browse', [row.table], {})); },
      empty: { icon: 'database', title: '没有可浏览的表' },
    });

    /* 接口调试器 */
    const sendBtn = $('#raw-send', root);
    const send = async () => {
      const path = $('#raw-path', root).value.trim();
      const method = $('#raw-method', root).value;
      const rawBody = $('#raw-body', root).value.trim();
      const out = $('#raw-out', root);
      if (!path) { toast('请输入接口路径', 'warn'); return; }
      let body;
      if (rawBody) {
        try { body = JSON.parse(rawBody); }
        catch (err) { toast('请求体不是合法 JSON：' + err.message, 'bad'); return; }
      }
      out.innerHTML = '<div class="sk sk-line w60" style="height:18px"></div>';
      const started = performance.now();
      try {
        const response = await apiFetch(path, { method: method, body: body, timeout: 30000 });
        const elapsed = Math.round(performance.now() - started);
        const data = unwrap(response);
        out.innerHTML = '<div class="row mb-8"><span class="tag tag-ok">200 OK</span><span class="muted text-xs mono">' + elapsed + ' ms</span>' +
          '<span class="spacer"></span><button class="btn btn-sm btn-ghost" data-json-drawer="' + escAttr(jsonSum(data).slice(0, 8000)) + '" data-json-title="' + escAttr(path) + '" type="button">' + icon('external') + '抽屉查看</button></div>' + jsonView(data);
      } catch (err) {
        const elapsed = Math.round(performance.now() - started);
        out.innerHTML = '<div class="row mb-8"><span class="tag tag-bad">' + esc(err.status ? 'HTTP ' + err.status : '请求失败') + '</span><span class="muted text-xs mono">' + elapsed + ' ms</span></div>' +
          errorState(err) + (err.data ? '<div class="mt-8">' + jsonView(err.data) + '</div>' : '');
      }
    };
    if (sendBtn) sendBtn.addEventListener('click', send);
    const pathInput = $('#raw-path', root);
    if (pathInput) pathInput.addEventListener('keydown', (event) => { if (event.key === 'Enter') send(); });
    const clearBtn = $('#raw-clear', root);
    if (clearBtn) clearBtn.addEventListener('click', () => {
      $('#raw-path', root).value = '';
      $('#raw-body', root).value = '';
      $('#raw-out', root).innerHTML = '';
    });
    const refreshBtn = $('#browse-refresh', root);
    if (refreshBtn) refreshBtn.addEventListener('click', () => route());
  },
};

async function renderBrowseRows(ctx, table) {
  const root = ctx.root;
  const query = ctx.q('q');
  const limit = Number(ctx.q('limit', '50')) || 50;
  const offset = Number(ctx.q('offset', '0')) || 0;
  let data = null;
  let loadError = null;
  try {
    data = unwrap(await apiGet('/api/v2/browse/rows' + qs({ table: table, q: query, limit: limit, offset: offset })));
  } catch (err) { loadError = err; }
  if (!ctx.alive()) return;
  if (loadError) {
    root.innerHTML = pageHead({ title: '数据浏览器 · ' + table, icon: 'database', actions: ['<button class="btn btn-ghost btn-sm" data-goto="#/browse" type="button">' + icon('arrow-left') + '返回表清单</button>'] }) +
      errorState(loadError, 'browse-retry');
    const retry = $('#browse-retry', root);
    if (retry) retry.addEventListener('click', () => route());
    return;
  }
  const rows = (data && data.rows) || [];
  const total = Number(data && data.total || 0);
  const columns = rows.length ? Object.keys(rows[0]).filter((key) => key !== '__id') : [];
  const visibleColumns = columns.slice(0, 8);

  root.innerHTML = pageHead({
    title: '数据浏览器 · ' + table,
    icon: 'database',
    subHtml: true,
    sub: '表 <code class="inline">' + esc(table) + '</code> · 共 ' + fmtNum(total) + ' 行 · 当前 ' + (rows.length ? offset + 1 : 0) + '–' + (offset + rows.length) + ' 行',
    actions: ['<button class="btn btn-ghost btn-sm" data-goto="#/browse" type="button">' + icon('arrow-left') + '返回表清单</button>',
      '<button class="btn btn-ghost btn-sm" data-copy-text="' + escAttr('/api/v2/browse/rows?table=' + table + '&q=' + encodeURIComponent(query) + '&limit=' + limit + '&offset=' + offset) + '" type="button">' + icon('link') + '复制接口地址</button>',
      '<button class="btn btn-ghost btn-sm" id="rows-refresh" type="button">' + icon('refresh') + '刷新</button>'],
  }) +
  card({ title: '行浏览', icon: 'list', flush: true },
    '<div class="tbl-toolbar">' +
      '<div class="field grow"><input class="input input-sm" id="rows-q" placeholder="过滤（对 chat_log.body_text / audit_log.event_json / rowid 做 LIKE）" value="' + escAttr(query) + '"></div>' +
      '<select class="select select-sm" id="rows-limit" aria-label="每页行数">' +
        ['50', '100', '200', '300'].map((value) => '<option value="' + value + '"' + (String(limit) === value ? ' selected' : '') + '>' + value + ' 行</option>').join('') +
      '</select>' +
      '<button class="btn btn-sm btn-ghost" id="rows-filter" type="button">' + icon('filter') + '过滤</button>' +
      '<span class="muted text-xs">最多显示 8 列，展开行看完整字段</span>' +
    '</div>' +
    '<div id="rows-table"></div>' +
    pagerBar({ offset: offset, limit: limit, total: total, unit: '行' })) +
  '<div class="notice mt-16">' + icon('info') + '<div>表 <code class="inline">' + esc(table) + '</code> 共 ' + columns.length + ' 列：' + columns.map((col) => '<code class="inline">' + esc(col) + '</code>').join(' ') + '</div></div>';

  renderTable($('#rows-table', root), {
    columns: visibleColumns.map((col) => ({
      key: col,
      label: col,
      mono: col !== 'body_text',
      render: (row) => {
        const value = row[col];
        if (value == null) return '<span class="muted-2">—</span>';
        if (typeof value === 'object') return '<span class="cell-truncate">' + esc(JSON.stringify(value)) + '</span>';
        return '<span class="cell-truncate">' + esc(String(value)) + '</span>';
      },
    })),
    rows: rows,
    onRow: (row) => openJsonDrawer(table + ' 行详情', row, rows.indexOf(row) >= 0 ? '第 ' + rows.indexOf(row) + 1 + ' 行' : ''),
    empty: { icon: 'database', title: '没有行数据', hint: query ? '过滤条件没有命中任何行' : '这张表是空的' },
  });

  const setOffset = (next, nextLimit) => {
    const params = new URLSearchParams();
    if (query) params.set('q', query);
    if ((nextLimit || limit) !== 50) params.set('limit', String(nextLimit || limit));
    if (next) params.set('offset', String(Math.max(0, next)));
    go(hashFor('browse', [table], params));
  };
  const filterInput = $('#rows-q', root);
  const filterBtn = $('#rows-filter', root);
  const applyFilter = () => {
    const params = new URLSearchParams();
    const value = filterInput.value.trim();
    if (value) params.set('q', value);
    if (limit !== 50) params.set('limit', String(limit));
    go(hashFor('browse', [table], params));
  };
  if (filterInput) filterInput.addEventListener('keydown', (event) => { if (event.key === 'Enter') applyFilter(); });
  if (filterBtn) filterBtn.addEventListener('click', applyFilter);
  const limitEl = $('#rows-limit', root);
  if (limitEl) limitEl.addEventListener('change', () => setOffset(0, Number(limitEl.value)));
  root.addEventListener('click', (event) => {
    const btn = event.target.closest('[data-page]');
    if (!btn) return;
    const action = btn.getAttribute('data-page');
    if (action === 'prev') setOffset(Math.max(0, offset - limit));
    if (action === 'next') setOffset(offset + limit);
    if (action === 'jump') {
      const page = Math.max(1, Number(btn.value) || 1);
      setOffset((page - 1) * limit);
    }
  });
  const refreshBtn = $('#rows-refresh', root);
  if (refreshBtn) refreshBtn.addEventListener('click', () => route());
}

/* ══════════════════════════════════════════════════════════════════════════
 * 7.11 日志：运行日志 / 审计日志 / 错误统计（SSE 尝试 + 轮询降级）
 * 数据源：/api/logs/stream、/api/logs?limit=&action=&actor=、/api/errors/stats
 * ════════════════════════════════════════════════════════════════════════ */

const LOG_TABS = [
  { id: 'runtime', label: '运行日志', icon: 'terminal' },
  { id: 'audit', label: '审计日志', icon: 'shield' },
  { id: 'errors', label: '错误统计', icon: 'alert' },
];

VIEWS.logs = {
  id: 'logs',
  name: '日志',
  icon: 'terminal',
  skeleton: 'table',
  crumb(route) {
    const found = LOG_TABS.find((item) => item.id === route.segments[1]);
    return found ? found.label : null;
  },
  async render(ctx) {
    const root = ctx.root;
    const tab = LOG_TABS.some((item) => item.id === ctx.segments[1]) ? ctx.segments[1] : 'runtime';
    const [streamRes, errorsRes, auditRes] = await Promise.all([
      apiGet('/api/logs/stream').catch((err) => ({ __error: err })),
      apiGet('/api/errors/stats').catch((err) => ({ __error: err })),
      apiGet('/api/logs' + qs({ limit: 200 })).catch((err) => ({ __error: err })),
    ]);
    if (!ctx.alive()) return;
    const stream = streamRes && streamRes.__error ? null : (streamRes && (streamRes.data || streamRes));
    const runtimeItems = (stream && stream.items) || [];
    const errorsData = errorsRes && !errorsRes.__error ? (errorsRes.data || errorsRes) : { by_component: [], trend: [], items: [] };
    const auditItems = auditRes && !auditRes.__error ? (auditRes.items || []) : [];
    const errorItems = errorsData.items || [];

    root.innerHTML = pageHead({
      title: '日志',
      icon: 'terminal',
      sub: '运行事件、后台操作审计与错误统计 · 运行日志支持实时推送（SSE 不可用时自动降级为 5 秒轮询）',
      actions: ['<button class="btn btn-ghost btn-sm" id="logs-refresh" type="button">' + icon('refresh') + '刷新</button>',
        '<span id="logs-live-badge" class="tag tag-muted">实时：待连接</span>'],
    }) +
    kpiGrid([
      { label: '运行事件', icon: 'terminal', valueHtml: fmtNum(runtimeItems.length), tone: 'acc', hint: '来自 /api/logs/stream' },
      { label: '错误条目', icon: 'alert', valueHtml: fmtNum(errorItems.length), tone: errorItems.length ? 'warn' : 'ok' },
      { label: '审计操作', icon: 'shield', valueHtml: fmtNum(auditItems.length), hint: '后台写操作流水' },
      { label: '错误组件', icon: 'globe', valueHtml: fmtNum((errorsData.by_component || []).length) },
    ]) +
    tabsHtml(LOG_TABS.map((item) => ({ id: item.id, label: item.label, icon: item.icon })), tab) +
    '<div class="grid grid-wide">' +
      card({ title: tab === 'audit' ? '审计日志' : tab === 'errors' ? '错误列表' : '运行日志', icon: 'terminal', flush: true, headActions:
          tab === 'runtime' ? ['<label class="switch" title="每 5 秒自动拉取"><input type="checkbox" id="logs-auto" checked><span>自动刷新</span></label>'] : [] },
        '<div class="tbl-toolbar">' +
          (tab === 'audit'
            ? '<div class="field" style="flex:0 1 170px"><input class="input input-sm" id="log-action" placeholder="action 过滤…"></div>' +
              '<div class="field" style="flex:0 1 170px"><input class="input input-sm" id="log-actor" placeholder="actor 过滤…"></div>' +
              '<button class="btn btn-sm btn-ghost" id="log-filter" type="button">' + icon('filter') + '过滤</button>'
            : '<div class="field grow"><input class="input input-sm" id="log-keyword" placeholder="关键词 / 组件名 / 错误文本…"></div>' +
              '<select class="select select-sm" id="log-component"><option value="">全部组件</option></select>') +
          '<span class="muted text-xs" id="log-count"></span>' +
        '</div>' +
        '<div id="log-table"></div>') +
      card({ title: '错误趋势 · 近 7 天', icon: 'chart' },
        '<div class="chart chart-sm" id="error-trend"></div>' +
        '<div class="section-title mt-12">' + icon('alert') + '按组件分布</div>' +
        '<div class="mt-8" id="error-components"></div>') +
    '</div>';

    const tabsRoot = $('.tabs', root);
    if (tabsRoot) tabsRoot.addEventListener('click', (event) => {
      const button = event.target.closest('[data-tab]');
      if (!button) return;
      go(hashFor('logs', [button.getAttribute('data-tab')], {}));
    });

    /* 错误趋势图 */
    const trend = errorsData.trend || [];
    if (trend.length) {
      mountChart($('#error-trend', root), chartBase({
        xAxis: { type: 'category', data: trend.map((item) => String(item.date || '').slice(5)) },
        yAxis: { type: 'value' },
        series: [{ name: '错误数', type: 'bar', data: trend.map((item) => Number(item.count || 0)), barMaxWidth: 24,
          itemStyle: { color: '#f87171', borderRadius: [4, 4, 0, 0] } }],
        grid: { top: 12 },
      }), trend.map((item) => ({ label: item.date, value: item.count })));
    } else {
      chartNoData($('#error-trend', root));
    }
    const components = errorsData.by_component || [];
    const componentRoot = $('#error-components', root);
    if (componentRoot) {
      const max = Math.max.apply(null, components.map((item) => Number(item.count || 0)).concat([1]));
      componentRoot.innerHTML = components.length
        ? components.map((item) => '<div class="stat-line"><span class="muted mono">' + esc(item.name) + '</span><span class="spacer"></span>' +
            '<span class="bar-track" style="max-width:110px"><span class="bar-fill bad" style="width:' + Math.round(Number(item.count) / max * 100) + '%"></span></span>' +
            '<b>' + fmtNum(item.count) + '</b></div>').join('')
        : '<div class="muted-2 text-xs">没有错误组件记录</div>';
    }

    /* 渲染表格 */
    const tableRoot = $('#log-table', root);
    const countNode = $('#log-count', root);
    const logState = { rows: [] };
    let rows;
    if (tab === 'audit') {
      rows = auditItems.map((item, index) => Object.assign({ __id: index, at: item.at, actor: item.actor, action: item.action, collection: item.collection, item_id: item.item_id }, item));
      if (countNode) countNode.textContent = rows.length + ' 条';
      renderTable(tableRoot, {
        columns: [
          { key: 'at', label: '时间', width: '150px', render: (row) => timeCell(row.at) },
          { key: 'actor', label: '操作人', width: '110px', render: (row) => '<span class="tag tag-muted">' + esc(row.actor || 'admin') + '</span>' },
          { key: 'action', label: '动作', width: '120px', render: (row) => '<span class="mono">' + esc(row.action || '-') + '</span>' },
          { key: 'collection', label: '资源', width: '130px', render: (row) => '<span class="mono">' + esc(row.collection || '-') + '</span>' },
          { key: 'item_id', label: '对象', render: (row) => '<span class="cell-truncate mono">' + esc(row.item_id || '') + '</span>' },
        ],
        rows: rows,
        detail: (row) => jsonView(row),
        empty: { icon: 'shield', title: '没有审计记录' },
      });
    } else if (tab === 'errors') {
      rows = errorItems.map((item, index) => Object.assign({ __id: index }, item));
      if (countNode) countNode.textContent = rows.length + ' 条';
      renderTable(tableRoot, {
        columns: [
          { key: 'at', label: '时间', width: '150px', render: (row) => timeCell(row.at) },
          { key: 'component', label: '组件', width: '100px', render: (row) => '<span class="tag tag-bad">' + esc(row.component || '-') + '</span>' },
          { key: 'error', label: '错误', render: (row) => '<span class="cell-wrap">' + esc(truncate(row.error || '', 160)) + '</span>' },
          { key: 'path', label: '路径', width: '160px', hideSm: true, render: (row) => '<span class="cell-truncate mono">' + esc(row.path || '') + '</span>' },
        ],
        rows: rows,
        detail: (row) => jsonView(row),
        empty: { icon: 'check', title: '没有错误记录' },
      });
    } else {
      rows = runtimeItems.map((item, index) => Object.assign({ __id: index }, item));
      if (countNode) countNode.textContent = rows.length + ' 条';
      renderTable(tableRoot, {
        columns: [
          { key: 'at', label: '时间', width: '150px', render: (row) => timeCell(row.at) },
          { key: 'level', label: '级别', width: '86px', render: (row) => '<span class="tag ' + (String(row.level) === 'ERROR' ? 'tag-bad' : 'tag-warn') + '">' + esc(row.level || 'INFO') + '</span>' },
          { key: 'service', label: '服务', width: '96px', render: (row) => '<span class="tag tag-muted mono">' + esc(row.service || row.component || '-') + '</span>' },
          { key: 'error', label: '内容', render: (row) => '<span class="cell-wrap">' + esc(truncate(row.error || row.message || '', 180)) + '</span>' },
        ],
        rows: rows,
        detail: (row) => jsonView(row),
        empty: { icon: 'terminal', title: '没有运行日志', hint: '只有被中控记录的错误事件会出现在这里' },
      });
    }

    /* 关键词 / 组件过滤（前端过滤） */
    logState.rows = rows;
    const keywordInput = $('#log-keyword', root);
    const componentSelect = $('#log-component', root);
    if (componentSelect) {
      const names = Array.from(new Set(runtimeItems.map((item) => item.component || item.service || ''))).filter(Boolean);
      componentSelect.innerHTML = '<option value="">全部组件</option>' + names.map((name) => '<option value="' + escAttr(name) + '">' + esc(name) + '</option>').join('');
    }
    const applyClientFilter = () => {
      const keyword = keywordInput ? keywordInput.value.trim().toLowerCase() : '';
      const component = componentSelect ? componentSelect.value : '';
      $$('tr.dt-row', tableRoot).forEach((tr) => {
        const row = logState.rows[Number(tr.getAttribute('data-row'))] || {};
        const text = jsonSum(row).toLowerCase();
        const visible = (!keyword || text.indexOf(keyword) >= 0) && (!component || String(row.component || row.service) === component);
        tr.style.display = visible ? '' : 'none';
        const detail = tableRoot.querySelector('tr.dt-detail[data-detail="' + tr.getAttribute('data-row') + '"]');
        if (detail && !visible) detail.hidden = true;
      });
      if (countNode) countNode.textContent = logState.rows.filter((row) => {
        const text = jsonSum(row).toLowerCase();
        return (!keyword || text.indexOf(keyword) >= 0) && (!component || String(row.component || row.service) === component);
      }).length + ' / ' + logState.rows.length + ' 条';
    };
    if (keywordInput) keywordInput.addEventListener('input', debounce(applyClientFilter, 160));
    if (componentSelect) componentSelect.addEventListener('change', applyClientFilter);

    /* 审计过滤 */
    const logFilter = $('#log-filter', root);
    if (logFilter) logFilter.addEventListener('click', async () => {
      const action = $('#log-action', root).value.trim();
      const actor = $('#log-actor', root).value.trim();
      tableRoot.innerHTML = '<div class="sk sk-line w60" style="height:18px;margin:10px"></div>';
      try {
        const response = await apiGet('/api/logs' + qs({ limit: 200, action: action, actor: actor }));
        const items = (response && response.items) || [];
        if (countNode) countNode.textContent = items.length + ' 条';
        renderTable(tableRoot, {
          columns: [
            { key: 'at', label: '时间', width: '150px', render: (row) => timeCell(row.at) },
            { key: 'actor', label: '操作人', width: '110px', render: (row) => '<span class="tag tag-muted">' + esc(row.actor || 'admin') + '</span>' },
            { key: 'action', label: '动作', width: '120px', render: (row) => '<span class="mono">' + esc(row.action || '-') + '</span>' },
            { key: 'collection', label: '资源', width: '130px', render: (row) => '<span class="mono">' + esc(row.collection || '-') + '</span>' },
            { key: 'item_id', label: '对象', render: (row) => '<span class="cell-truncate mono">' + esc(row.item_id || '') + '</span>' },
          ],
          rows: items,
          detail: (row) => jsonView(row),
          empty: { icon: 'shield', title: '没有匹配的审计记录' },
        });
      } catch (err) {
        tableRoot.innerHTML = errorState(err);
      }
    });

    /* 实时：优先 SSE，失败降级轮询 */
    if (tab === 'runtime') {
      const badge = $('#logs-live-badge', root);
      const autoToggle = $('#logs-auto', root);
      let pollTimer = 0;
      let sse = null;
      let mode = 'idle';
      const setBadge = (kind, text) => { if (badge) { badge.className = 'tag ' + (kind === 'ok' ? 'tag-ok' : kind === 'warn' ? 'tag-warn' : 'tag-muted'); badge.textContent = '实时：' + text; } };
      const reloadStream = async () => {
        try {
          const response = await apiGet('/api/logs/stream', { timeout: 12000 });
          if (!ctx.alive()) return;
          const payload = response && (response.data || response);
          const items = (payload && payload.items) || [];
          const rows = items.map((item, index) => Object.assign({ __id: index }, item));
          logState.rows = rows;
          if (countNode) countNode.textContent = rows.length + ' 条';
          renderTable(tableRoot, {
            columns: [
              { key: 'at', label: '时间', width: '150px', render: (row) => timeCell(row.at) },
              { key: 'level', label: '级别', width: '86px', render: (row) => '<span class="tag tag-bad">' + esc(row.level || 'ERROR') + '</span>' },
              { key: 'service', label: '服务', width: '96px', render: (row) => '<span class="tag tag-muted mono">' + esc(row.service || '-') + '</span>' },
              { key: 'error', label: '内容', render: (row) => '<span class="cell-wrap">' + esc(truncate(row.error || row.message || '', 180)) + '</span>' },
            ],
            rows: rows,
            detail: (row) => jsonView(row),
            empty: { icon: 'terminal', title: '没有运行日志' },
          });
          setBadge(mode === 'polling' ? 'warn' : 'ok', mode === 'polling' ? '轮询 5s' : '已连接');
        } catch (err) {
          setBadge('warn', '拉取失败');
        }
      };
      const startPolling = () => {
        if (pollTimer) return;
        mode = 'polling';
        setBadge('warn', '轮询 5s');
        pollTimer = setInterval(() => {
          if (!ctx.alive()) return;
          if (document.hidden) return;
          if (autoToggle && !autoToggle.checked) return;
          reloadStream();
        }, 5000);
        ctx.cleanup(() => clearInterval(pollTimer));
      };
      const startLive = async () => {
        /* 先探测 SSE 是否可用（后端当前返回 JSON，避免 EventSource 报 MIME 错误） */
        try {
          const probe = await fetch('/api/logs/stream', {
            headers: { Accept: 'text/event-stream' },
            credentials: 'same-origin',
            cache: 'no-store',
          });
          const contentType = (probe.headers.get('content-type') || '').toLowerCase();
          try { if (probe.body && probe.body.cancel) probe.body.cancel(); } catch (err) { /* ignore */ }
          if (!probe.ok || contentType.indexOf('text/event-stream') < 0) { startPolling(); return; }
        } catch (err) {
          startPolling();
          return;
        }
        if (!ctx.alive()) return;
        if (typeof EventSource === 'undefined') { startPolling(); return; }
        try {
          sse = new EventSource('/api/logs/stream');
          sse.onopen = () => { mode = 'sse'; setBadge('ok', 'SSE'); };
          sse.onmessage = () => { reloadStream(); };
          sse.onerror = () => {
            if (sse) { try { sse.close(); } catch (err) { /* ignore */ } sse = null; }
            if (mode !== 'polling') startPolling();
          };
          ctx.cleanup(() => { if (sse) { try { sse.close(); } catch (err) { /* ignore */ } sse = null; } });
        } catch (err) { startPolling(); }
      };
      startLive();
      if (autoToggle) autoToggle.addEventListener('change', () => toast(autoToggle.checked ? '自动刷新已开启' : '自动刷新已暂停', 'info', { timeout: 1600 }));
    }

    const refreshBtn = $('#logs-refresh', root);
    if (refreshBtn) refreshBtn.addEventListener('click', () => route());
  },
};

/* ══════════════════════════════════════════════════════════════════════════
 * 7.12 部署与运维
 * 数据源：/api/v2/deploy/status（新，未就绪占位）、/api/health、
 *         /api/domain/health、/api/observability/overview、/api/domain/archive
 * ════════════════════════════════════════════════════════════════════════ */

VIEWS.deploy = {
  id: 'deploy',
  name: '部署与运维',
  icon: 'server',
  skeleton: 'default',
  async render(ctx) {
    const root = ctx.root;
    const [deployRes, healthRes, domainHealthRes, obsRes, archiveRes] = await Promise.all([
      probeApi(['/api/v2/deploy/status'], { timeout: 32000 }),
      apiGet('/api/health').catch(() => null),
      apiGet('/api/domain/health').catch(() => null),
      apiGet('/api/observability/overview').catch(() => null),
      apiGet('/api/domain/archive').catch(() => null),
    ]);
    if (!ctx.alive()) return;
    const health = healthRes || {};
    const domainHealth = domainHealthRes ? unwrap(domainHealthRes) : null;
    const obs = obsRes || {};
    const archive = archiveRes && archiveRes.ok ? unwrap(archiveRes) : null;
    const deploy = deployRes.ok ? deployRes.data : null;
    const tasks = (obs && obs.tasks) || {};
    const configSync = (obs && obs.config_sync) || {};
    const agent = (deploy && deploy.agent) || {};
    const running = (deploy && deploy.running) || {};
    const images = (deploy && deploy.images) || [];
    const missing = (deploy && deploy.missing) || [];
    const agentOnline = !!(deploy && deploy.agent_online);
    const imageSearch = ctx.q('image');
    const filteredImages = images.filter((image) => {
      if (!imageSearch) return true;
      const hay = String(image.image || '') + ' ' + String(image.id || '') + ' ' + String(image.created || '');
      return hay.toLowerCase().indexOf(imageSearch.toLowerCase()) >= 0;
    });

    const deployKpis = deployRes.ok
      ? [
          { label: '宿主机代理', icon: 'server', valueHtml: statusTag(agentOnline, '在线', '离线'), hint: agent.pid ? 'pid ' + agent.pid + ' · v' + esc(agent.version || '-') : '心跳超时' },
          { label: '镜像数', icon: 'database', valueHtml: fmtNum(images.length), hint: '宿主 docker images' },
          { label: '运行中容器', icon: 'activity', valueHtml: running.name ? '<span style="font-size:15px">' + esc(running.status || 'unknown') + '</span>' : '—', hint: running.name ? esc(truncate(running.image || '', 34)) : '未采集到容器状态' },
          { label: '采集耗时', icon: 'clock', valueHtml: fmtNum(deploy.elapsed_ms) + '<small>ms</small>', hint: '快照 ' + esc(ago(deploy.checked_at) || '刚刚') + (deploy.sampling ? ' · 首采中' : '') },
        ]
      : [
          { label: '后台服务', icon: 'server', valueHtml: statusTag(!!health.ok, '在线', '离线'), hint: esc(health.service || '-') },
          { label: 'FM Domain', icon: 'globe', valueHtml: statusTag(!!(domainHealth && (domainHealth.ok || domainHealth.status === 'ok')), '在线', '离线') },
          { label: 'Cosmobot RPC', icon: 'cpu', valueHtml: statusTag(!!(obs.rpc && obs.rpc.ok), '可连', '不可用') },
          { label: '部署接口', icon: 'external', valueHtml: deployRes.pending ? '<span class="tag tag-warn">待接入</span>' : '<span class="tag tag-bad">异常</span>' },
        ];

    root.innerHTML = pageHead({
      title: '部署与运维',
      icon: 'server',
      sub: '宿主机运维代理（host-ops-agent）提供的容器 / 镜像 / 重启能力 · 镜像枚举约 20 秒，后台线程缓存快照',
      actions: [
        '<button class="btn btn-ghost btn-sm" id="deploy-refresh" type="button">' + icon('refresh') + '刷新快照</button>',
        '<a class="btn btn-ghost btn-sm" href="#/actions">' + icon('wand') + '操作台</a>',
      ],
    }) +
    kpiGrid(deployKpis) +
    '<div class="grid grid-wide mt-16">' +
      card({ title: '宿主机运维代理', icon: 'server', headActions: [
          deployRes.ok ? '<span class="tag tag-ok">' + esc(deploy.source || 'host-ops-agent') + '</span>' : '<span class="tag tag-muted">未接入</span>',
          '<button class="btn btn-ghost btn-sm" data-json-drawer="' + escAttr(jsonSum({ agent: agent, requests: deploy && deploy.requests, note: deploy && deploy.note, missing: missing }).slice(0, 5000)) + '" data-json-title="运维代理状态" type="button">' + icon('eye') + 'JSON</button>',
        ] },
        deployRes.ok
          ? '<div class="kv-grid">' +
              '<div class="kv-item"><div class="k">在线</div><div class="v">' + (agentOnline ? '<span class="tag tag-ok">是</span>' : '<span class="tag tag-bad">否</span>') + '</div></div>' +
              '<div class="kv-item"><div class="k">版本 / PID</div><div class="v mono">' + esc(agent.version || '-') + ' / ' + esc(agent.pid || '-') + '</div></div>' +
              '<div class="kv-item"><div class="k">最近扫描</div><div class="v mono">' + esc(agent.last_scan_text || fmtTime(agent.last_scan) || '-') + '</div></div>' +
              '<div class="kv-item"><div class="k">心跳年龄</div><div class="v">' + (agent.age_seconds != null ? fmtNum(agent.age_seconds, 1) + ' 秒' : '—') + '</div></div>' +
              '<div class="kv-item"><div class="k">扫描次数</div><div class="v">' + fmtNum(agent.scans) + '</div></div>' +
              '<div class="kv-item"><div class="k">已处理请求</div><div class="v">' + fmtNum(agent.processed_total) + '</div></div>' +
              '<div class="kv-item"><div class="k">允许操作</div><div class="v">' + (agent.allowed_ops || []).map((op) => '<span class="tag tag-info mono">' + esc(op) + '</span>').join(' ') + '</div></div>' +
              '<div class="kv-item"><div class="k">队列目录</div><div class="v mono break">' + esc(agent.queue || '-') + '</div></div>' +
            '</div>' +
            (deploy.note ? '<div class="notice warn mt-12">' + icon('alert') + '<div>' + esc(deploy.note) + '</div></div>' : '') +
            (missing.length ? '<div class="notice warn mt-12">' + icon('alert') + '<div>本次快照缺少：' + missing.map((item) => esc(item)).join('、') + '，可稍后点「刷新快照」重试。</div></div>' : '')
          : pendingState({
              title: '部署状态接口尚未就绪',
              hint: '后端 /api/v2/deploy/status 通过宿主机 ops-agent 队列读取 docker 状态。当前接口不可用或运维代理离线。',
              path: '/api/v2/deploy/status',
              raw: deployRes.error && deployRes.error.data,
            })) +
      card({ title: '运行容器', icon: 'activity' },
        running.name
          ? '<div class="kv-grid">' +
              '<div class="kv-item"><div class="k">名称</div><div class="v mono">' + esc(running.name) + '</div></div>' +
              '<div class="kv-item"><div class="k">状态</div><div class="v">' + (running.running ? '<span class="tag tag-ok">running</span>' : '<span class="tag tag-bad">' + esc(running.status || 'stopped') + '</span>') + (running.health ? ' <span class="tag tag-info">' + esc(running.health) + '</span>' : '') + '</div></div>' +
              '<div class="kv-item"><div class="k">镜像</div><div class="v mono break">' + esc(truncate(running.image || '', 60)) + '</div></div>' +
              '<div class="kv-item"><div class="k">启动时间</div><div class="v mono">' + esc(fmtTime(running.started_at) || '-') + '</div></div>' +
              '<div class="kv-item"><div class="k">重启次数</div><div class="v">' + fmtNum(running.restart_count) + '</div></div>' +
              '<div class="kv-item"><div class="k">平台</div><div class="v">' + esc(running.platform || '-') + '</div></div>' +
            '</div>' +
            '<div class="notice mt-12">' + icon('info') + '<div>容器状态来自 docker inspect 快照；重启机器人请用下方「部署操作」或 <a href="#/actions">操作台</a>。</div></div>'
          : '<div id="running-empty"></div><div class="notice mt-8">' + icon('alert') + '<div>没有拿到容器运行状态（快照采集中或代理离线）。</div></div>') +
    '</div>' +
    '<div class="grid grid-2 mt-16">' +
      card({ title: '部署操作', icon: 'wand', className: 'danger', headActions: deployRes.ok ? [] : ['<span class="tag tag-warn">接口未就绪</span>'] },
        deployRes.ok
          ? '<div class="stack" style="gap:10px">' +
              '<div class="notice warn">' + icon('alert') + '<div>这些操作通过宿主机代理队列执行：入队后立即返回 request_id，页面会轮询结果。重启机器人会断开 QQ/MATRIX 连接并重连。</div></div>' +
              '<div class="row">' +
                '<button class="btn btn-sm btn-ghost" id="deploy-act-list" type="button">' + icon('list') + '重新采集镜像 / 容器</button>' +
                '<button class="btn btn-sm btn-danger" id="deploy-act-restart" type="button">' + icon('refresh') + '重启 fm-cosmobot</button>' +
              '</div>' +
              '<div id="deploy-act-out"></div>' +
            '</div>'
          : pendingState({ title: '部署动作接口尚未就绪', hint: '后端 /api/v2/deploy/act 提供 list / restart 两种动作。为安全起见，接口就绪前只保留 /api/system/restart/* 的旧通道。', path: '/api/v2/deploy/act' }) +
            '<div class="row mt-12"><a class="btn btn-sm btn-ghost" href="#/actions">' + icon('wand') + '去操作台重启服务</a></div>') +
      card({ title: '服务自检', icon: 'shield', headActions: ['<button class="btn btn-ghost btn-sm" id="deploy-check" type="button">' + icon('play') + '重新自检</button>'] },
        '<div id="deploy-checks"></div>' +
        '<div class="section-title mt-16">' + icon('database') + '采集与归档</div>' +
        '<div class="mt-8">' + (archive
          ? '<div class="kv-grid">' +
              Object.keys(archive).filter((key) => typeof archive[key] !== 'object').map((key) =>
                '<div class="kv-item"><div class="k mono">' + esc(key) + '</div><div class="v mono">' + esc(archive[key] == null ? '—' : (/at$|_at$/.test(key) ? (fmtTime(archive[key]) || archive[key]) : archive[key])) + '</div></div>').join('') +
            '</div>' +
            (archive.sources ? Object.keys(archive.sources).map((name) => {
              const item = archive.sources[name] || {};
              return '<div class="stat-line"><span class="muted">' + esc(name) + '</span><span class="spacer"></span><b>' + fmtNum(item.count) + '</b><span class="muted-2 text-xs">最近 ' + esc(ago(item.latest_at) || '—') + '</span></div>';
            }).join('') : '')
          : '<div class="muted-2 text-xs">归档状态不可用</div>') + '</div>') +
    '</div>' +
    '<div class="card mt-16">' +
      '<div class="card-head"><h3>' + icon('database') + '镜像列表<span class="section-sub">' + images.length + ' 个 · 按创建时间倒序</span></h3>' +
        '<div class="card-head-actions">' +
          '<input class="input input-sm" id="image-filter" placeholder="过滤镜像名 / ID…" value="' + escAttr(imageSearch) + '" style="width:190px">' +
          '<button class="btn btn-sm btn-ghost" id="image-filter-go" type="button">' + icon('filter') + '过滤</button>' +
        '</div>' +
      '</div>' +
      '<div id="images-table"></div>' +
    '</div>' +
    '<div class="grid grid-4 mt-16">' +
      [['任务处理', tasks.running != null ? fmtNum(tasks.running) + ' 运行中' : '—', tasks.stuck ? fmtNum(tasks.stuck) + ' 疑似卡住' : '无卡住任务'],
       ['配置同步', configSync.applied ? '已生效' : '待确认', configSync.last_version_id || '无版本'],
       ['最近错误', fmtNum(((obs && obs.recent_errors) || []).length) + ' 条', '来自 observability'],
       ['快照年龄', deploy && deploy.stale_seconds != null ? fmtNum(deploy.stale_seconds, 1) + ' 秒' : '—', '宿主代理缓存']].map((item) =>
        '<div class="card"><div class="card-body"><div class="section-title">' + esc(item[0]) + '</div>' +
          '<div class="kpi-value" style="font-size:18px;margin-top:6px">' + esc(item[1]) + '</div>' +
          '<div class="muted-2 text-xs mt-4">' + esc(item[2]) + '</div></div></div>').join('') +
    '</div>';

    /* 镜像表 */
    const imageRoot = $('#images-table', root);
    if (imageRoot) {
      renderTable(imageRoot, {
        columns: [
          { key: 'image', label: '镜像', render: (row) => '<span class="mono cell-truncate">' + esc(row.image || row.repository + ':' + row.tag) + '</span>' },
          { key: 'id', label: 'ID', width: '108px', mono: true, render: (row) => '<span class="mono">' + esc(truncate(row.id || '', 14)) + '</span>' },
          { key: 'size', label: '大小', width: '86px', num: true, render: (row) => '<span class="mono">' + esc(row.size || '-') + '</span>' },
          { key: 'created', label: '创建时间', width: '160px', hideSm: true, render: (row) => '<span class="mono">' + esc(row.created || '-') + '</span>' },
          { key: 'digest', label: 'Digest', mono: true, hideSm: true, render: (row) => '<span class="mono">' + esc(truncate(row.digest || '-', 22)) + '</span>' },
        ],
        rows: filteredImages.slice(0, 40),
        detail: (row) => jsonView(row),
        empty: { icon: 'database', title: imageSearch ? '没有匹配的镜像' : '没有镜像记录', hint: deployRes.ok ? '点击「刷新快照」重新采集' : '部署状态接口未就绪' },
      });
      if (filteredImages.length > 40) {
        const note = document.createElement('div');
        note.className = 'card-foot';
        note.innerHTML = icon('info') + ' 共 ' + fmtNum(filteredImages.length) + ' 个镜像，表格只显示前 40 个；可用过滤框缩小范围。';
        imageRoot.parentNode.appendChild(note);
      }
    }
    const imageFilter = $('#image-filter', root);
    const applyImageFilter = () => {
      const params = new URLSearchParams();
      const value = imageFilter.value.trim();
      if (value) params.set('image', value);
      go(hashFor('deploy', [], params));
    };
    if (imageFilter) imageFilter.addEventListener('keydown', (event) => { if (event.key === 'Enter') applyImageFilter(); });
    const imageFilterGo = $('#image-filter-go', root);
    if (imageFilterGo) imageFilterGo.addEventListener('click', applyImageFilter);

    /* 自检 */
    const checks = [
      { name: '后台 /api/health', ok: !!health.ok, detail: health.service || '' },
      { name: 'FM Domain /health', ok: !!(domainHealth && (domainHealth.ok || domainHealth.status === 'ok')), detail: domainHealth ? JSON.stringify(domainHealth).slice(0, 120) : '无响应' },
      { name: 'Cosmobot RPC', ok: !!(obs.rpc && obs.rpc.ok), detail: (obs.rpc && obs.rpc.reason) || '配置快照读取正常' },
      { name: 'Domain 统计', ok: !!(obs.domain && obs.domain.ok), detail: (obs.domain && obs.domain.reason) || '统计接口正常' },
      { name: '配置同步', ok: !!configSync.applied, detail: configSync.reason || '配置与运行时一致' },
      { name: '运维代理', ok: agentOnline, detail: agentOnline ? '心跳 ' + fmtNum(agent.age_seconds, 1) + ' 秒前' : '心跳超时或接口未就绪' },
    ];
    const checksRoot = $('#deploy-checks', root);
    if (checksRoot) {
      checksRoot.innerHTML = checks.map((check) =>
        '<div class="stat-line"><span class="dot" data-tone="' + (check.ok ? 'ok' : 'bad') + '"></span>' +
        '<span>' + esc(check.name) + '</span><span class="spacer"></span>' +
        '<span class="muted-2 text-xs" style="max-width:240px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap">' + esc(check.detail) + '</span>' +
        (check.ok ? '<span class="tag tag-ok">正常</span>' : '<span class="tag tag-warn">异常/未就绪</span>') + '</div>').join('');
    }

    /* 动作：采集 / 重启 + 轮询结果 */
    const actOut = $('#deploy-act-out', root);
    const pollRequest = async (requestId, label) => {
      if (!actOut) return;
      const started = Date.now();
      for (let i = 0; i < 40; i += 1) {
        if (!ctx.alive()) return;
        await sleep(1200);
        try {
          const data = unwrap(await apiGet('/api/v2/deploy/status' + qs({ request_id: requestId }), { timeout: 15000 }));
          if (data && data.done) {
            actOut.innerHTML = notice('ok', '请求已完成：' + esc(label) + '（request_id ' + esc(requestId) + '）') + jsonView(data.result);
            return;
          }
        } catch (err) {
          actOut.innerHTML = notice('warn', '轮询结果失败：' + esc(err.message) + '（可稍后刷新页面查看）');
          return;
        }
      }
      actOut.innerHTML = notice('warn', '请求 ' + esc(label) + ' 已入队，但 48 秒内未拿到代理结果（request_id ' + esc(requestId) + '）。宿主机代理可能繁忙。');
    };
    const runAct = async (action, label, confirmDetail, requireText) => {
      const ok = await confirmAction({
        title: label,
        detail: confirmDetail,
        confirmLabel: '执行',
        danger: action === 'restart',
        requireText: requireText || '',
      });
      if (!ok) return;
      if (actOut) actOut.innerHTML = '<div class="sk sk-line w60" style="height:16px"></div>';
      try {
        const response = await apiPost('/api/v2/deploy/act', { action: action, target: 'bot' }, { timeout: 20000 });
        const data = unwrap(response);
        const requestId = data && (data.request_id || (data.requests && Object.values(data.requests)[0]));
        if (actOut) actOut.innerHTML = notice('info', '已入队：' + esc(jsonSum(data.requests || {})) + '（request_id ' + esc(requestId || '-') + '）') + jsonView(data);
        if (requestId) pollRequest(requestId, label);
      } catch (err) {
        if (actOut) actOut.innerHTML = errorState(err);
      }
    };
    const listBtn = $('#deploy-act-list', root);
    if (listBtn) listBtn.addEventListener('click', () => runAct(
      'list', '重新采集镜像与容器状态？',
      '会向宿主机代理入队 images / inspect 两个任务；镜像枚举在 208 个 30GB 镜像的机器上约需 17 秒。',
    ));
    const restartBtn = $('#deploy-act-restart', root);
    if (restartBtn) restartBtn.addEventListener('click', () => runAct(
      'restart', '重启 fm-cosmobot？',
      '机器人会重启并重新连接 QQ/MATRIX（约 10–60 秒）。期间群消息不会回复。请输入 RESTART 确认。',
      'RESTART',
    ));

    const refreshBtn = $('#deploy-refresh', root);
    if (refreshBtn) refreshBtn.addEventListener('click', async () => {
      refreshBtn.disabled = true;
      refreshBtn.innerHTML = icon('clock') + '采集中（最长约 26s）…';
      toast('已请求强制刷新宿主机快照（镜像枚举约需 20 秒）', 'info', { timeout: 4000 });
      try {
        await apiGet('/api/v2/deploy/status' + qs({ force: 1 }), { timeout: 32000 });
        toast('快照已更新', 'ok');
      } catch (err) {
        toast('刷新失败：' + err.message, 'bad');
      }
      route();
    });
    const checkBtn = $('#deploy-check', root);
    if (checkBtn) checkBtn.addEventListener('click', () => { checkBtn.disabled = true; route(); });
    const retry = $('#deploy-retry', root);
    if (retry) retry.addEventListener('click', () => route());
  },
};
/* 通用数据渲染：标量进 KV，数组进表格（占位容器由 mountGenericTables 填充） */
function renderGenericData(data) {
  if (data == null) return emptyState({ icon: 'server', title: '接口返回空' });
  if (typeof data !== 'object') return '<pre>' + esc(String(data)) + '</pre>';
  const scalars = {};
  const arrays = {};
  Object.keys(data).forEach((key) => {
    if (Array.isArray(data[key])) arrays[key] = data[key];
    else if (data[key] && typeof data[key] === 'object') scalars[key] = JSON.stringify(data[key]);
    else scalars[key] = data[key];
  });
  let html = '';
  const scalarKeys = Object.keys(scalars);
  if (scalarKeys.length) {
    html += '<div class="kv-grid">' + scalarKeys.map((key) =>
      '<div class="kv-item"><div class="k mono">' + esc(key) + '</div><div class="v mono">' + esc(scalars[key] == null ? '—' : String(scalars[key]).slice(0, 200)) + '</div></div>').join('') + '</div>';
  }
  Object.keys(arrays).forEach((key) => {
    const rows = arrays[key];
    html += '<div class="section-title mt-16">' + icon('list') + esc(key) + '<span class="section-sub">' + rows.length + ' 条</span></div>' +
      '<div class="mt-8" data-generic="' + escAttr(key) + '"></div>';
  });
  return html || '<pre>' + esc(JSON.stringify(data, null, 2)) + '</pre>';
}

function mountGenericTables(root, data) {
  if (!root || !data || typeof data !== 'object') return;
  $$('[data-generic]', root).forEach((holder) => {
    const key = holder.getAttribute('data-generic');
    const rows = Array.isArray(data[key]) ? data[key] : [];
    if (!rows.length) { holder.innerHTML = '<div class="muted-2 text-xs">空数组</div>'; return; }
    const columns = Object.keys(rows[0]).slice(0, 7).map((col) => ({
      key: col,
      label: col,
      mono: true,
      render: (row) => {
        const value = row[col];
        if (value == null) return '<span class="muted-2">—</span>';
        if (typeof value === 'object') return '<span class="cell-truncate">' + esc(JSON.stringify(value)) + '</span>';
        return '<span class="cell-truncate">' + esc(String(value)) + '</span>';
      },
    }));
    renderTable(holder, {
      columns: columns,
      rows: rows,
      onRow: (row) => openJsonDrawer(key + ' 条目', row),
      empty: { icon: 'list', title: '空数组' },
    });
  });
}

/* ══════════════════════════════════════════════════════════════════════════
 * 7.13 设置：账号 / 改密 / 访问令牌 / 界面偏好 / 自检
 * 数据源：/api/auth/me、/api/auth/password、/api/auth/tokens
 * ════════════════════════════════════════════════════════════════════════ */

let lastCreatedToken = null;

VIEWS.settings = {
  id: 'settings',
  name: '设置',
  icon: 'gear',
  skeleton: 'default',
  async render(ctx) {
    const root = ctx.root;    const [meRes, tokensRes] = await Promise.all([
      apiGet('/api/auth/me').catch((err) => ({ __error: err })),
      apiGet('/api/auth/tokens').catch(() => null),
    ]);
    if (!ctx.alive()) return;
    const me = meRes && !meRes.__error ? meRes : (APP.user || { username: 'admin', is_admin: true });
    const tokens = (tokensRes && (tokensRes.items || [])) || [];

    root.innerHTML = pageHead({
      title: '设置',
      icon: 'gear',
      sub: '账号、密码、访问令牌与界面偏好 · 会话保存在服务端内存，后台重启后需重新登录',
      actions: ['<button class="btn btn-ghost btn-sm" id="settings-refresh" type="button">' + icon('refresh') + '刷新</button>',
        '<button class="btn btn-danger btn-sm" id="settings-logout" type="button">' + icon('logout') + '退出登录</button>'],
    }) +
    kpiGrid([
      { label: '当前账号', icon: 'user', valueHtml: '<span style="font-size:16px">' + esc(me.username || 'admin') + '</span>', tone: 'acc' },
      { label: '权限', icon: 'shield', valueHtml: me.is_admin ? '<span class="tag tag-ok">管理员</span>' : '<span class="tag tag-warn">只读令牌</span>' },
      { label: '访问令牌', icon: 'key', valueHtml: fmtNum(tokens.length), hint: tokens.filter((item) => item.enabled).length + ' 个有效' },
      { label: '自动刷新', icon: 'clock', valueHtml: APP.autoRefresh ? '<span class="tag tag-ok">已开启</span>' : '<span class="tag tag-muted">已关闭</span>', hint: '10 秒间隔 / 页面隐藏时暂停' },
    ]) +
    '<div class="grid grid-2 mt-16">' +
      card({ title: '修改密码', icon: 'lock' },
        '<div class="stack" style="gap:10px;max-width:420px">' +
          '<div class="field"><label>当前密码</label><input class="input" id="pw-old" type="password" autocomplete="current-password"></div>' +
          '<div class="field"><label>新密码（至少 8 位）</label><input class="input" id="pw-new" type="password" autocomplete="new-password"></div>' +
          '<div class="field"><label>确认新密码</label><input class="input" id="pw-confirm" type="password" autocomplete="new-password"></div>' +
          '<div class="row"><button class="btn btn-sm btn-solid" id="pw-save" type="button">' + icon('save') + '更新密码</button>' +
          '<span class="muted text-xs">修改后当前会话仍然有效，下次登录使用新密码</span></div>' +
        '</div>') +
      card({ title: '界面偏好', icon: 'gear' },
        '<div class="setting-row"><div class="setting-main"><div class="setting-title">自动刷新</div><div class="setting-desc">适合仪表盘 / 日志页；页面隐藏时自动暂停</div></div>' +
          '<button class="switch" id="pref-auto" type="button"><input type="checkbox" tabindex="-1" ' + (APP.autoRefresh ? 'checked' : '') + '><span>' + (APP.autoRefresh ? '开' : '关') + '</span></button></div>' +
        '<div class="setting-row"><div class="setting-main"><div class="setting-title">主题</div><div class="setting-desc">控制台固定深色主题（与登录页独立），保证数据可读性</div></div><span class="tag tag-muted">深色</span></div>' +
        '<div class="setting-row"><div class="setting-main"><div class="setting-title">本地缓存</div><div class="setting-desc">清除本机保存的偏好设置（不影响服务端配置）</div></div>' +
          '<button class="btn btn-sm btn-ghost" id="pref-reset" type="button">' + icon('trash') + '清除</button></div>' +
        '<div class="setting-row"><div class="setting-main"><div class="setting-title">键盘快捷键</div><div class="setting-desc">Ctrl/⌘ + K 快速跳转 · / 聚焦搜索 · r 刷新 · Esc 关闭弹层</div></div></div>' +
        '<div class="setting-row"><div class="setting-main"><div class="setting-title">旧版后台</div><div class="setting-desc">模型切换、配置回滚、复杂表单仍在旧后台</div></div>' +
          '<a class="btn btn-sm btn-ghost" href="/classic" target="_blank" rel="noopener">' + icon('external') + '打开</a></div>') +
    '</div>' +
    '<div class="grid grid-wide mt-16">' +
      card({ title: '访问令牌', icon: 'key', flush: true },
        '<div class="tbl-toolbar">' +
          '<div class="field grow"><input class="input input-sm" id="token-name" placeholder="令牌名称，例如 脚本只读"></div>' +
          '<div class="field" style="flex:0 1 130px"><select class="select select-sm" id="token-expire">' +
            [['0', '永不过期'], ['1', '1 天'], ['7', '7 天'], ['30', '30 天'], ['90', '90 天']].map((pair) => '<option value="' + pair[0] + '">' + pair[1] + '</option>').join('') +
          '</select></div>' +
          '<button class="btn btn-sm btn-soft" id="token-create" type="button">' + icon('plus') + '生成令牌</button>' +
        '</div>' +
        '<div id="token-table"></div>' +
        '<div id="token-new" class="card-body" style="display:none"></div>') +
      card({ title: '接口自检', icon: 'activity' },
        '<div class="stack" style="gap:8px">' +
          '<button class="btn btn-sm btn-ghost" id="self-check" type="button">' + icon('play') + '运行自检（8 个关键接口）</button>' +
          '<div id="self-check-out" class="mt-8">' + emptyState({ icon: 'activity', title: '尚未运行自检', hint: '会依次探测 health / overview / domain / deploy 等接口' }) + '</div>' +
        '</div>' +
        '<div class="card-note">自检结果仅在本页展示，不会修改任何配置。</div>') +
    '</div>';

    /* 改密 */
    const pwSave = $('#pw-save', root);
    if (pwSave) pwSave.addEventListener('click', async () => {
      const oldPassword = $('#pw-old', root).value;
      const newPassword = $('#pw-new', root).value;
      const confirmPassword = $('#pw-confirm', root).value;
      if (!oldPassword) { toast('请输入当前密码', 'warn'); return; }
      if (newPassword.length < 8) { toast('新密码至少 8 位', 'warn'); return; }
      if (newPassword !== confirmPassword) { toast('两次输入的新密码不一致', 'warn'); return; }
      const ok = await confirmAction({ title: '确认修改密码？', detail: '修改后请使用新密码登录，当前会话保持有效。', confirmLabel: '修改密码' });
      if (!ok) return;
      pwSave.disabled = true;
      try {
        await apiPost('/api/auth/password', { old_password: oldPassword, new_password: newPassword, confirm_password: confirmPassword });
        toast('密码已更新', 'ok');
        ['#pw-old', '#pw-new', '#pw-confirm'].forEach((selector) => { const el = $(selector, root); if (el) el.value = ''; });
      } catch (err) {
        toast('修改失败：' + err.message, 'bad');
      } finally { pwSave.disabled = false; }
    });

    /* 偏好 */
    const prefAuto = $('#pref-auto', root);
    if (prefAuto) prefAuto.addEventListener('click', () => { toggleAutoRefresh(); prefAuto.querySelector('input').checked = APP.autoRefresh; prefAuto.querySelector('span').textContent = APP.autoRefresh ? '开' : '关'; });
    const prefReset = $('#pref-reset', root);
    if (prefReset) prefReset.addEventListener('click', () => {
      try { localStorage.removeItem('fm-console-auto-refresh'); localStorage.removeItem('fm-admin-theme'); } catch (err) { /* ignore */ }
      toast('本地缓存已清除，刷新页面后生效', 'ok');
    });
    const logoutBtn = $('#settings-logout', root);
    if (logoutBtn) logoutBtn.addEventListener('click', doLogout);

    /* 令牌 */
    const tokenTable = $('#token-table', root);
    if (tokenTable) {
      renderTable(tokenTable, {
        columns: [
          { key: 'name', label: '名称', render: (row) => esc(row.name || '-') },
          { key: 'value', label: '令牌', mono: true, render: (row) => '<span class="mono">' + esc(row.value || '—') + '</span>' },
          { key: 'created_at', label: '创建时间', width: '132px', hideSm: true, render: (row) => '<span class="mono">' + esc(fmtTime(row.created_at, { noSeconds: true }) || '—') + '</span>' },
          { key: 'expires_at', label: '过期', width: '132px', render: (row) => row.expires_at ? '<span class="mono">' + esc(fmtTime(row.expires_at, { noSeconds: true })) + '</span>' : '<span class="muted-2">永不</span>' },
          { key: 'enabled', label: '状态', width: '86px', render: (row) => row.enabled ? '<span class="tag tag-ok">有效</span>' : '<span class="tag tag-muted">已吊销</span>' },
          { key: 'actions', label: '', width: '82px', render: (row) => row.enabled
              ? '<button class="btn btn-sm btn-danger" data-revoke="' + escAttr(row.id) + '" type="button">吊销</button>'
              : '<span class="muted-2">—</span>' },
        ],
        rows: tokens,
        detail: (row) => jsonView(row),
        empty: { icon: 'key', title: '还没有访问令牌', hint: '令牌适合脚本 / 外部工具调用后台接口' },
      });
      const tokenNewBox = $('#token-new', root);
      if (lastCreatedToken && tokenNewBox) {
        tokenNewBox.style.display = '';
        tokenNewBox.innerHTML = notice('ok', '令牌已生成，<b>只显示这一次</b>，请立即复制保存（刷新后不再显示明文）。') +
          '<div class="copy-field mt-8"><span>' + esc(lastCreatedToken) + '</span>' +
          '<button class="btn btn-sm btn-soft" data-copy-text="' + escAttr(lastCreatedToken) + '" type="button">' + icon('copy') + '复制</button>' +
          '<button class="btn btn-sm btn-ghost" id="token-dismiss" type="button">我已保存</button></div>';
        const dismiss = $('#token-dismiss', root);
        if (dismiss) dismiss.addEventListener('click', () => {
          lastCreatedToken = null;
          tokenNewBox.style.display = 'none';
        });
      }
      tokenTable.addEventListener('click', async (event) => {
        const btn = event.target.closest('[data-revoke]');
        if (!btn) return;
        const id = btn.getAttribute('data-revoke');
        const token = tokens.find((item) => String(item.id) === String(id)) || {};
        const ok = await confirmAction({ title: '吊销令牌？', detail: '令牌「' + (token.name || '') + '」将立即失效，使用它的脚本会收到 401。', confirmLabel: '吊销', danger: true });
        if (!ok) return;
        try {
          await apiDelete('/api/auth/tokens/' + encodeURIComponent(id));
          toast('令牌已吊销', 'ok');
          route();
        } catch (err) { toast('吊销失败：' + err.message, 'bad'); }
      });
    }
    const tokenCreate = $('#token-create', root);
    if (tokenCreate) tokenCreate.addEventListener('click', async () => {
      const name = $('#token-name', root).value.trim();
      const expireDays = Number($('#token-expire', root).value);
      if (!name) { toast('请输入令牌名称', 'warn'); return; }
      const payload = { name: name };
      if (expireDays > 0) payload.expires_at = Math.floor(Date.now() / 1000) + expireDays * 86400;
      tokenCreate.disabled = true;
      try {
        const response = await apiPost('/api/auth/tokens', payload);
        const token = response && (response.token || (response.data && response.data.token));
        if (token) {
          lastCreatedToken = token;
          toast('令牌已生成，请立即复制保存（只显示一次）', 'ok', { timeout: 6000 });
          route();
          return;
        }
        toast('令牌已生成，但接口没有返回明文令牌', 'warn');
        route();
      } catch (err) {
        toast('生成失败：' + err.message, 'bad');
      } finally { tokenCreate.disabled = false; }
    });

    /* 自检 */
    const selfCheck = $('#self-check', root);
    if (selfCheck) selfCheck.addEventListener('click', async () => {
      const out = $('#self-check-out', root);
      const endpoints = [
        ['/api/health', '后台健康'],
        ['/api/auth/me', '会话鉴权'],
        ['/api/v2/overview', 'v2 总览'],
        ['/api/observability/overview', '可观测性'],
        ['/api/v2/groups', '群列表'],
        ['/api/domain/stats', 'Domain 统计'],
        ['/api/v2/browse/tables', '数据浏览器'],
        ['/api/v2/deploy/status', '部署状态（新）'],
      ];
      out.innerHTML = '<div class="sk sk-line w60" style="height:14px"></div>';
      const results = [];
      for (let i = 0; i < endpoints.length; i += 1) {
        const started = performance.now();
        try {
          await apiGet(endpoints[i][0], { timeout: 12000 });
          results.push({ name: endpoints[i][1], path: endpoints[i][0], ok: true, ms: Math.round(performance.now() - started) });
        } catch (err) {
          results.push({ name: endpoints[i][1], path: endpoints[i][0], ok: false, ms: Math.round(performance.now() - started), error: err.message, pending: err.status === 404 });
        }
      }
      if (!ctx.alive()) return;
      out.innerHTML = results.map((item) =>
        '<div class="stat-line"><span class="dot" data-tone="' + (item.ok ? 'ok' : item.pending ? 'warn' : 'bad') + '"></span>' +
        '<span>' + esc(item.name) + '</span><span class="muted-2 text-xs mono" style="margin-left:6px">' + esc(item.path) + '</span>' +
        '<span class="spacer"></span><span class="muted-2 text-xs">' + item.ms + ' ms</span>' +
        '<span class="tag ' + (item.ok ? 'tag-ok' : item.pending ? 'tag-warn' : 'tag-bad') + '">' + (item.ok ? '通' : item.pending ? '未就绪' : '失败') + '</span></div>' +
        (!item.ok && item.error ? '<div class="muted-2 text-xs" style="padding-left:16px">' + esc(item.error) + '</div>' : '')).join('');
    });

    const refreshBtn = $('#settings-refresh', root);
    if (refreshBtn) refreshBtn.addEventListener('click', () => route());
  },
};
