/* ============================================================================
   FM Control Center · 核心层
   ────────────────────────────────────────────────────────────────────────────
   分四段：工具函数 → 请求与缓存 → 路由 → UI 零件。
   视图模块只依赖这里，这里不依赖任何视图。
   ========================================================================= */

/* ══ 1. 工具 ═══════════════════════════════════════════════════════════ */
export const $  = (sel, root = document) => root.querySelector(sel);
export const $$ = (sel, root = document) => [...root.querySelectorAll(sel)];

/* 元素工厂：h('div', {class:'x', onclick:fn}, child, child…) */
export function h(tag, attrs = {}, ...kids) {
  const el = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (v == null || v === false) continue;
    if (k === 'class') el.className = v;
    else if (k === 'html') el.innerHTML = v;
    else if (k === 'style') el.setAttribute('style', v);
    else if (k === 'text') el.textContent = v;
    else if (k.startsWith('on')) el.addEventListener(k.slice(2), v);
    else el.setAttribute(k, v === true ? '' : String(v));
  }
  append(el, kids);
  return el;
}

function append(el, kids) {
  for (const kid of kids.flat(4)) {
    if (kid == null || kid === false || kid === '') continue;
    el.append(kid instanceof Node ? kid : document.createTextNode(String(kid)));
  }
}

/* 内联 SVG（图标表见 ICONS） */
export const svg = (d, cls = '') =>
  h('svg', { viewBox: '0 0 24 24', class: cls, 'aria-hidden': 'true', html: `<path d="${d}"/>` });

export const ICONS = {
  refresh: 'M12 5V2L8 6l4 4V7a5 5 0 11-5 5H5a7 7 0 107-7z',
  search:  'M10 2a8 8 0 105.3 14l4.4 4.4 1.4-1.4-4.4-4.4A8 8 0 0010 2zm0 2a6 6 0 110 12 6 6 0 010-12z',
  close:   'M6.4 5L5 6.4 10.6 12 5 17.6 6.4 19 12 13.4 17.6 19 19 17.6 13.4 12 19 6.4 17.6 5 12 10.6z',
  inbox:   'M4 5h16v14H4zm2 2v7h3l1 2h4l1-2h3V7z',
  alert:   'M12 2l10 18H2zm-1 6v6h2V8zm0 8v2h2v-2z',
};

export const esc = (s) => String(s ?? '').replace(/[&<>"']/g,
  (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));

/* ── 格式化 ───────────────────────────────────────────────────────────── */
/* 后端字段名不完全一致，取不到就显示破折号，绝不让 NaN / undefined 冒到界面上 */
export function num(n) {
  if (n == null || n === '') return '—';
  const v = Number(n);
  return Number.isFinite(v) ? v.toLocaleString('zh-CN') : '—';
}

export function pct(n, digits = 1) {
  if (n == null || n === '') return '—';
  const v = Number(n);
  return Number.isFinite(v) ? `${v.toFixed(digits)}%` : '—';
}

const UNITS = ['B', 'KB', 'MB', 'GB', 'TB'];
export function bytes(n) {
  let v = Number(n ?? 0), i = 0;
  if (!Number.isFinite(v) || v < 0) return '—';
  while (v >= 1024 && i < UNITS.length - 1) { v /= 1024; i += 1; }
  return `${v.toFixed(v < 10 && i > 0 ? 1 : 0)} ${UNITS[i]}`;
}

/* 后端时间戳混用秒 / 毫秒 / ISO，统一换算 */
function toMillis(ts) {
  if (ts == null || ts === '') return null;
  if (typeof ts === 'number') return ts < 1e12 ? ts * 1000 : ts;
  const parsed = Date.parse(ts);
  return Number.isFinite(parsed) ? parsed : null;
}

export function ago(ts) {
  const ms = toMillis(ts);
  if (ms == null) return '—';
  const s = Math.max(0, (Date.now() - ms) / 1000);
  if (s < 60) return `${Math.floor(s)} 秒前`;
  if (s < 3600) return `${Math.floor(s / 60)} 分钟前`;
  if (s < 86400) return `${Math.floor(s / 3600)} 小时前`;
  if (s < 2592000) return `${Math.floor(s / 86400)} 天前`;
  return new Date(ms).toLocaleDateString('zh-CN');
}

export function clock(ts, { seconds = true } = {}) {
  const ms = toMillis(ts);
  if (ms == null) return '—';
  return new Date(ms).toLocaleString('zh-CN', {
    hour12: false, month: '2-digit', day: '2-digit',
    hour: '2-digit', minute: '2-digit',
    ...(seconds ? { second: '2-digit' } : {}),
  });
}

export const day = (ts) => {
  const ms = toMillis(ts);
  return ms == null ? '—' : new Date(ms).toLocaleDateString('zh-CN');
};

export const duration = (ms) => {
  const v = Number(ms ?? 0);
  if (!Number.isFinite(v) || v < 0) return '—';
  if (v < 1000) return `${Math.round(v)} ms`;
  if (v < 60000) return `${(v / 1000).toFixed(1)} s`;
  return `${Math.floor(v / 60000)}m ${Math.round((v % 60000) / 1000)}s`;
};

export const elapsed = (from, to) =>
  (from == null || to == null ? '—' : duration(new Date(to) - new Date(from)));

/* ══ 2. 数据解包 ═══════════════════════════════════════════════════════ */
/* 后端信封：{ok,data} / {ok,items,total} / {ok,data:{…}} / 裸值 */
const WRAPPER = ['ok', 'data', 'items', 'total', 'generated_at'];

export function unwrap(payload) {
  let node = payload;
  for (let i = 0; i < 4; i += 1) {
    if (!node || typeof node !== 'object' || Array.isArray(node)) break;
    const business = Object.keys(node).filter((k) => !WRAPPER.includes(k));
    if (Array.isArray(node.items) && business.length === 0) { node = node.items; continue; }
    if (node.data !== undefined && business.length === 0) { node = node.data; continue; }
    break;
  }
  return node;
}

/* 取数组，不管它藏在哪一层 */
export function asList(payload, ...keys) {
  if (Array.isArray(payload)) return payload;
  if (!payload || typeof payload !== 'object') return [];
  for (const key of [...keys, 'items', 'data', 'rows', 'list']) {
    if (Array.isArray(payload[key])) return payload[key];
  }
  return [];
}

/* 嵌套对象压平成 k/v 行 */
export function flatten(value, prefix = '', depth = 0, out = []) {
  if (value == null) return out;
  if (typeof value !== 'object') { out.push({ k: prefix, v: String(value) }); return out; }
  if (Array.isArray(value)) { out.push({ k: prefix, v: `${value.length} 项` }); return out; }
  for (const [key, v] of Object.entries(value)) {
    const label = prefix ? `${prefix}.${key}` : key;
    if (v && typeof v === 'object' && !Array.isArray(v) && depth < 1) flatten(v, label, depth + 1, out);
    else if (Array.isArray(v)) out.push({ k: label, v: `${v.length} 项` });
    else out.push({ k: label, v: v && typeof v === 'object' ? '…' : String(v) });
  }
  return out;
}

/* 群名：后端常把群号塞进 group_name，所以优先取有意义的那个 */
export function groupName(row, keys = ['group_name', 'display_name', 'group_id']) {
  const values = keys.map((k) => String(row?.[k] ?? '').trim()).filter(Boolean);
  const named = values.find((v) => !/^\d+$/.test(v));
  return named || values[0] || '—';
}

/* 两行单元格：上行主信息，下行灰色小字 */
export const stack = (top, bottom) => h('div', { style: 'line-height:1.45' },
  h('div', {}, top),
  bottom ? h('div', { style: 'font-size:var(--t-sm);color:var(--fg-subtle)' }, bottom) : null);

/* 布尔值的人话 */
export const yesno = (v, on = '开启', off = '关闭') => (v ? on : off);

/* ══ 3. 请求 ═══════════════════════════════════════════════════════════ */
let onUnauthorized = null;
export const setUnauthorizedHandler = (fn) => { onUnauthorized = fn; };

export async function api(path, options = {}) {
  const init = { credentials: 'same-origin', headers: {}, ...options };
  if (init.body !== undefined && typeof init.body !== 'string') {
    init.headers['Content-Type'] = 'application/json';
    init.body = JSON.stringify(init.body);
  }

  let res;
  try {
    res = await fetch(path, init);
  } catch (err) {
    throw new Error(`网络不可达：${err.message}`);
  }

  if (res.status === 401) {
    if (onUnauthorized) onUnauthorized();
    throw new Error('未登录或登录已过期');
  }

  const text = await res.text();
  let data = null;
  if (text) { try { data = JSON.parse(text); } catch { data = { raw: text }; } }

  if (!res.ok || (data && data.ok === false)) {
    const message = (data && (data.error || data.message || data.reason)) || `HTTP ${res.status}`;
    const error = new Error(String(message));
    error.payload = data;
    error.status = res.status;
    throw error;
  }
  return data;
}

/* 业务数据统一走 domain 代理 */
export const domain = (path, query = {}) => {
  const qs = new URLSearchParams(
    Object.entries(query).filter(([, v]) => v !== '' && v != null)).toString();
  return api(`/api/domain/${path}${qs ? `?${qs}` : ''}`);
};

/* RPC 转发（后端白名单校验） */
export const rpc = (method, params = {}) =>
  api('/api/rpc', { method: 'POST', body: { method, params } });

/* 取 RPC 结果里的 data 段 */
export const rpcData = async (method, params) => unwrap((await rpc(method, params))?.data ?? {});

/* 带 TTL 的缓存，避免来回切页时重复请求 */
const cacheStore = new Map();
export async function cached(key, loader, ttlMs = 20000) {
  const hit = cacheStore.get(key);
  if (hit && Date.now() - hit.at < ttlMs) return hit.value;
  const value = await loader();
  cacheStore.set(key, { at: Date.now(), value });
  return value;
}
export const invalidate = (prefix = '') => {
  for (const key of [...cacheStore.keys()]) if (key.startsWith(prefix)) cacheStore.delete(key);
};

/* 出错也别炸整页：拿一个带 _err 的占位回去 */
export const soft = (promise, fallback = {}) =>
  promise.catch((e) => ({ ...fallback, _err: e.message }));

/* ══ 4. 主题 ═══════════════════════════════════════════════════════════ */
const THEME_KEY = 'fm-admin-theme';

export function applyTheme(theme) {
  document.documentElement.dataset.theme = theme;
  try { localStorage.setItem(THEME_KEY, theme); } catch { /* 隐私模式 */ }
}

export const currentTheme = () => document.documentElement.dataset.theme || 'light';

export function initTheme() {
  let saved = null;
  try { saved = localStorage.getItem(THEME_KEY); } catch { /* ignore */ }
  /* 没设过就默认暗色：这一版的视觉是照着暗色霓虹设计的，
     跟随系统反而常常落到亮色那套上，效果差一截。 */
  applyTheme(saved || 'dark');
}

export function toggleTheme() { applyTheme(currentTheme() === 'dark' ? 'light' : 'dark'); }

/* ══ 5. 路由 ═══════════════════════════════════════════════════════════ */
/* #/library?q=xxx&page=2 —— 参数进地址栏，可分享可回退 */
export function parseRoute() {
  const raw = location.hash.replace(/^#\/?/, '') || 'dashboard';
  const [path, queryString] = raw.split('?');
  const segments = path.split('/').filter(Boolean);
  return {
    name: segments[0] || 'dashboard',
    arg: segments[1] || '',
    query: Object.fromEntries(new URLSearchParams(queryString || '')),
  };
}

export function navigate(name, query = {}, { replace = false } = {}) {
  const qs = new URLSearchParams(
    Object.entries(query).filter(([, v]) => v !== '' && v != null)).toString();
  const hash = `#/${name}${qs ? `?${qs}` : ''}`;
  if (replace) location.replace(hash); else location.hash = hash;
}

export function patchRoute(query = {}) {
  const route = parseRoute();
  navigate(route.name, { ...route.query, ...query }, { replace: true });
}

/* ══ 6. 提示与浮层 ═════════════════════════════════════════════════════ */
export function toast(message, kind = 'info', ms = 3600) {
  const host = $('#toasts');
  if (!host) return;
  const el = h('div', { class: 'toast', 'data-kind': kind }, String(message));
  host.append(el);
  setTimeout(() => {
    el.style.transition = 'opacity .18s, transform .18s';
    el.style.opacity = '0';
    el.style.transform = 'translateY(6px)';
    setTimeout(() => el.remove(), 220);
  }, ms);
}

export function confirmAction({ title, detail, confirmLabel = '确认执行', cancelLabel = '取消', danger = true }) {
  return new Promise((resolve) => {
    let done = false;
    const finish = (value) => {
      if (done) return;
      done = true;
      document.removeEventListener('keydown', onKey);
      veil.remove();
      resolve(value);
    };
    const onKey = (e) => { if (e.key === 'Escape') finish(false); };

    const veil = h('div', { class: 'scrim' },
      h('div', { class: 'modal', role: 'dialog', 'aria-modal': 'true' },
        h('h3', {}, title),
        detail ? h('p', {}, detail) : null,
        h('div', { class: 'modal-foot' },
          h('button', { class: 'btn', type: 'button', onclick: () => finish(false) }, cancelLabel),
          h('button', {
            class: 'btn', type: 'button', 'data-kind': danger ? 'danger' : 'solid',
            onclick: () => finish(true),
          }, confirmLabel))));

    veil.addEventListener('click', (e) => { if (e.target === veil) finish(false); });
    document.addEventListener('keydown', onKey);
    document.body.append(veil);
    veil.querySelector('button[data-kind]')?.focus();
  });
}

/* ══ 7. UI 零件（类名见 styles.css） ═══════════════════════════════════ */

/* 空态 */
export const blank = (title, hint) => h('div', { class: 'void' },
  svg(ICONS.inbox),
  h('b', {}, title),
  hint ? h('span', {}, hint) : null);

/* 骨架屏 */
export const skeleton = (lines = 4) => h('div', { style: 'padding:4px 0' },
  Array.from({ length: lines }, (_, i) =>
    h('div', { class: 'ph ph-line', style: `width:${92 - i * 11}%` })));

/* 出错 */
export const errorState = (message, retry) => h('div', { class: 'void' },
  svg(ICONS.alert),
  h('b', {}, '加载失败'),
  h('span', {}, message || '请稍后重试'),
  retry ? h('button', { class: 'btn', style: 'margin-top:12px', onclick: retry },
    svg(ICONS.refresh), '重试') : null);

/* 提示条 */
export const alert = (kind, text) => h('div', { class: 'alert', 'data-kind': kind }, String(text));

/* 分节标题 + 内容 */
export const section = (title, hint, ...body) => h('section', { class: 'group' },
  title || hint ? h('div', { class: 'group-head' },
    title ? h('h2', {}, title) : null,
    hint ? h('p', {}, hint) : null) : null,
  ...body);

/* 栅格：cols(4, a, b, c, d) */
export const cols = (n, ...children) => h('div', { class: 'rows', 'data-c': String(n) }, ...children);

/* 卡片；body 里若带表格（.slide）就自动去掉内边距，让表格贴边 */
export const card = (title, ...body) => {
  const flat = body.flat(4).filter(Boolean);
  const flush = flat.some((b) => b instanceof Element && b.classList.contains('slide'));
  return h('div', { class: 'card' },
    title ? h('div', { class: 'card-head' }, h('h3', {}, title)) : null,
    h('div', { class: 'card-body', 'data-flush': flush ? '' : null },
      ...body));
};

/* 指标块：左侧一条主色竖条 + 大号数字，tone 决定这条竖条的颜色 */
export const stat = (key, value, note, tone) => h('div', { class: 'card', 'data-k': tone || null },
  h('div', { class: 'stat' },
    h('span', { class: 'stat-key' }, key),
    h('strong', { class: 'stat-val' }, value == null || value === '' ? '—' : String(value)),
    note ? h('span', { class: 'stat-note' },
      tone ? h('span', { class: 'pill', 'data-kind': tone }, note) : String(note)) : null));

/* 会滚动的指标块：value 必须是纯数字，前后缀单独传。
   挂载后还要调一次 countUpIn()，滚动才会开始。 */
export const statCount = (key, value, { note, tone, decimals = 0, prefix = '', suffix = '', blank = '—' } = {}) => {
  const n = Number(value);
  const hasNumber = value != null && value !== '' && Number.isFinite(n);
  return h('div', { class: 'card', 'data-k': tone || null },
    h('div', { class: 'stat' },
      h('span', { class: 'stat-key' }, key),
      h('strong', {
        class: 'stat-val',
        'data-count': hasNumber ? String(n) : null,
        'data-count-decimals': String(decimals),
        'data-count-prefix': prefix,
        'data-count-suffix': suffix,
      }, hasNumber ? `${prefix}0${suffix}` : blank),
      note ? h('span', { class: 'stat-note' },
        tone ? h('span', { class: 'pill', 'data-kind': tone }, note) : String(note)) : null));
};

/* 状态胶囊 */
export const chip = (text, tone) => h('span', { class: 'pill', 'data-kind': tone || null }, String(text));

/* 段落切换 */
export const seg = (options, value, onPick) => h('div', { class: 'tabs', role: 'tablist' },
  options.map((o) => h('button', {
    type: 'button', role: 'tab', 'aria-selected': String(o.value === value),
    onclick: () => onPick(o.value),
  }, o.label)));

/* 开关 */
export const toggle = (on, onFlip) => h('button', {
  class: 'toggle', type: 'button', role: 'switch', 'aria-checked': String(!!on),
  onclick: () => onFlip(!on),
});

/* 表单行：标签 + 控件 + 可选说明。用于设置页那些可写的配置。 */
export const field = (label, control, hint) => h('label', { class: 'field' },
  h('span', { style: 'font-size:var(--t-base);font-weight:600' }, label),
  control,
  hint ? h('small', {}, hint) : null);

/* 下拉框 */
export const select = (options, value, onPick, { label } = {}) => h('select', {
  class: 'sel', 'aria-label': label || '选择',
  onchange: (e) => onPick?.(e.target.value),
}, options.map((o) => h('option', {
  value: o.value, selected: String(o.value) === String(value),
}, o.label)));

/* 多行输入 */
export const textarea = (value, attrs = {}) => h('textarea', {
  class: 'ta', spellcheck: 'false', ...attrs,
}, value == null ? '' : String(value));

/* 单行输入 */
export const input = (value, attrs = {}) => h('input', {
  class: 'in', value: value == null ? '' : String(value), ...attrs,
});

/* 一排动作按钮 */
export const actions = (...children) => h('div', {
  style: 'display:flex;flex-wrap:wrap;gap:8px;align-items:center;margin-top:12px',
}, ...children);

/* 带 loading 与错误处理的按钮：onClick 里抛错会变成 toast，不会静默 */
export function taskButton(labelText, onClick, { kind, icon } = {}) {
  const btn = h('button', {
    class: 'btn', type: 'button',
    'data-kind': kind || null,
    onclick: async () => {
      if (btn.hasAttribute('data-busy')) return;
      btn.setAttribute('data-busy', '');
      try {
        await onClick();
      } catch (err) {
        toast(err.message || String(err), 'bad');
      } finally {
        btn.removeAttribute('data-busy');
      }
    },
  }, icon ? svg(ICONS[icon]) : null, labelText);
  return btn;
}

export function toneFor(value) {
  const s = String(value ?? '').toLowerCase();
  if (['ok', 'healthy', 'success', 'running', 'active', 'true', 'enabled', 'online', '完成'].includes(s)) return 'ok';
  if (['warn', 'warning', 'degraded', 'starting', 'idle', 'pending', 'observed', '运行中', '进行中'].includes(s)) return 'warn';
  if (['error', 'failed', 'down', 'unhealthy', 'false', 'stopped', 'offline', '失败', '已撤销'].includes(s)) return 'bad';
  return undefined;
}

/* 表格：列 = {label, key?, render?, num?, mono?, wrap?, width?} */
export function table(columns, rows, { empty: emptyText = '没有数据', emptyHint } = {}) {
  if (!rows || !rows.length) return blank(emptyText, emptyHint);
  return h('div', { class: 'slide' }, h('table', { class: 'tbl' },
    h('thead', {}, h('tr', {}, columns.map((c) =>
      h('th', { class: c.num ? 'num' : '', style: c.width ? `width:${c.width}` : null }, c.label)))),
    h('tbody', {}, rows.map((row) => h('tr', {},
      columns.map((c) => h('td', {
        class: [c.num ? 'num' : '', c.mono ? 'mono' : '', c.wrap ? 'wrap' : ''].filter(Boolean).join(' '),
      }, c.render ? c.render(row) : String(row[c.key] ?? '—'))))))));
}

/* 对象 → 两列表格 */
export const kvTable = (value, { empty: emptyText = '无数据' } = {}) => {
  const rows = flatten(value);
  return rows.length
    ? table([{ label: '项', key: 'k', wrap: true }, { label: '值', key: 'v', wrap: true }], rows)
    : blank(emptyText);
};

/* 工具栏：输入框 / 下拉 / 按钮 / 右推 */
export function filters(controls) {
  const bar = h('div', { class: 'bar-tools' });
  for (const c of controls) {
    if (!c) continue;
    if (c.push) { bar.append(h('span', { style: 'margin-left:auto' })); continue; }
    if (c.node) { bar.append(c.node); continue; }
    if (c.select) {
      bar.append(h('select', {
        class: 'sel', 'aria-label': c.label || '筛选',
        onchange: (e) => c.onchange?.(e.target.value),
      }, c.select.map((o) => h('option', { value: o.value, selected: o.value === c.value }, o.label))));
      continue;
    }
    if (c.input) {
      bar.append(h('input', {
        class: 'in', type: c.type || 'search', value: c.value ?? '',
        placeholder: c.placeholder || '', 'aria-label': c.placeholder || '搜索',
        onkeydown: (e) => { if (e.key === 'Enter') c.onenter?.(e.target.value); },
        oninput: c.oninput,
      }));
      continue;
    }
    if (c.button) {
      bar.append(h('button', {
        class: 'btn', type: 'button',
        'data-kind': c.kind || (c.primary ? 'solid' : null),
        onclick: c.onclick,
      }, c.icon ? svg(ICONS[c.icon]) : null, c.button));
    }
  }
  return bar;
}

/* 分页 */
export const paginate = (rows, page, size) => {
  const total = rows.length;
  const pages = Math.max(1, Math.ceil(total / size));
  const current = Math.min(Math.max(1, Number(page) || 1), pages);
  return { rows: rows.slice((current - 1) * size, current * size), total, pages, current };
};

export function pager({ total, pages, current }, onGo) {
  if (pages <= 1) return h('div', { class: 'stat-note', style: 'padding:12px 0' }, `共 ${num(total)} 条`);
  const btn = (label, page, disabled) => h('button', {
    class: 'btn', type: 'button', disabled: disabled || undefined,
    onclick: () => !disabled && onGo(page),
  }, label);
  return h('div', { class: 'bar-tools', style: 'margin:12px 0 0' },
    btn('上一页', current - 1, current <= 1),
    h('span', { class: 'stat-note' }, `第 ${current} / ${pages} 页 · 共 ${num(total)} 条`),
    btn('下一页', current + 1, current >= pages));
}

/* ══ 8. 花哨零件 ═══════════════════════════════════════════════════════ */

/* ── 数字滚动：从 0 数到目标值，带缓出 ───────────────────────────────── */
export function animateNumber(el, to, { decimals = 0, ms = 900, prefix = '', suffix = '' } = {}) {
  const target = Number(String(to).replace(/[^\d.-]/g, ''));
  if (!Number.isFinite(target)) { el.textContent = to == null ? '—' : String(to); return; }
  if (matchMedia('(prefers-reduced-motion: reduce)').matches) {
    el.textContent = `${prefix}${target.toLocaleString('zh-CN', { minimumFractionDigits: decimals, maximumFractionDigits: decimals })}${suffix}`;
    return;
  }
  const started = performance.now();
  const tick = (now) => {
    const p = Math.min(1, (now - started) / ms);
    const eased = 1 - (1 - p) ** 3;              /* easeOutCubic */
    const value = target * eased;
    el.textContent = `${prefix}${value.toLocaleString('zh-CN', {
      minimumFractionDigits: decimals, maximumFractionDigits: decimals,
    })}${suffix}`;
    if (p < 1) requestAnimationFrame(tick);
  };
  requestAnimationFrame(tick);
}

/* ── 迷你趋势线：表格里塞一根小折线，末端带一个发光点 ───────────────── */
const SVG_NS = 'http://www.w3.org/2000/svg';
const svgEl = (tag, attrs = {}) => {
  const el = document.createElementNS(SVG_NS, tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (v == null || v === false) continue;
    el.setAttribute(k, String(v));
  }
  return el;
};

export function sparkline(values, { w = 96, h: height = 26, tone } = {}) {
  const data = (Array.isArray(values) ? values : [])
    .map((v) => Number(v))
    .filter((v) => Number.isFinite(v));
  const host = h('div', { class: 'spark', style: `width:${w}px;height:${height}px` });
  if (data.length < 2) return host;

  const id = `sp${(sparkline._n = (sparkline._n || 0) + 1)}`;
  const color = tone || 'var(--c2)';
  const max = Math.max(...data);
  const min = Math.min(...data);
  const span = max - min || 1;
  const stepX = w / (data.length - 1);
  const pad = 3;
  const pts = data.map((v, i) => [i * stepX, height - pad - ((v - min) / span) * (height - pad * 2)]);
  const line = pts.map(([x, y], i) => `${i ? 'L' : 'M'}${x.toFixed(1)} ${y.toFixed(1)}`).join(' ');
  const area = `${line} L${w} ${height} L0 ${height} Z`;
  const last = pts[pts.length - 1];
  const rising = data[data.length - 1] >= data[0];

  const svg = svgEl('svg', { viewBox: `0 0 ${w} ${height}`, preserveAspectRatio: 'none', 'aria-hidden': 'true' });
  const defs = svgEl('defs');
  const grad = svgEl('linearGradient', { id, x1: 0, y1: 0, x2: 0, y2: 1 });
  grad.append(
    svgEl('stop', { offset: '0%', 'stop-color': color, 'stop-opacity': '.38' }),
    svgEl('stop', { offset: '100%', 'stop-color': color, 'stop-opacity': '0' }),
  );
  defs.append(grad);
  svg.append(
    defs,
    svgEl('path', { d: area, fill: `url(#${id})` }),
    svgEl('path', {
      d: line, fill: 'none', stroke: color, 'stroke-width': 1.6,
      'stroke-linecap': 'round', 'stroke-linejoin': 'round',
      style: `filter:drop-shadow(0 0 5px ${color})`,
    }),
    svgEl('circle', {
      cx: last[0].toFixed(1), cy: last[1].toFixed(1), r: 2.4,
      fill: rising ? 'var(--c4)' : 'var(--bad)',
      style: 'filter:drop-shadow(0 0 6px currentColor)',
    }),
  );
  host.append(svg);
  host.title = `最低 ${min} · 最高 ${max} · 最新 ${data[data.length - 1]}`;
  return host;
}

/* ── 进度环：百分比用环表示，比数字直观 ─────────────────────────────── */
export function ring(value, { size = 92, thickness = 8, label, tone } = {}) {
  const pctValue = Math.max(0, Math.min(100, Number(value) || 0));
  const r = (size - thickness) / 2;
  const c = 2 * Math.PI * r;
  const id = `rg${(ring._n = (ring._n || 0) + 1)}`;

  const svg = svgEl('svg', { viewBox: `0 0 ${size} ${size}`, 'aria-hidden': 'true' });
  const defs = svgEl('defs');
  const grad = svgEl('linearGradient', { id, x1: 0, y1: 0, x2: 1, y2: 1 });
  grad.append(
    svgEl('stop', { offset: '0%', 'stop-color': 'var(--c2)' }),
    svgEl('stop', { offset: '55%', 'stop-color': 'var(--c1)' }),
    svgEl('stop', { offset: '100%', 'stop-color': 'var(--c3)' }),
  );
  defs.append(grad);

  const track = svgEl('circle', {
    cx: size / 2, cy: size / 2, r, fill: 'none',
    stroke: 'var(--border)', 'stroke-width': thickness,
  });
  const arc = svgEl('circle', {
    class: 'ring-arc',
    cx: size / 2, cy: size / 2, r, fill: 'none',
    stroke: `url(#${id})`, 'stroke-width': thickness, 'stroke-linecap': 'round',
    'stroke-dasharray': c, 'stroke-dashoffset': c,
    transform: `rotate(-90 ${size / 2} ${size / 2})`,
  });
  svg.append(defs, track, arc);

  const numEl = h('b', { 'data-ring-value': '' }, '0');
  const mid = h('div', { class: 'ring-mid' }, numEl, label ? h('span', {}, label) : null);
  const host = h('div', { class: 'ring', style: `width:${size}px;height:${size}px` }, svg, mid);

  requestAnimationFrame(() => { arc.style.strokeDashoffset = String(c * (1 - pctValue / 100)); });
  animateNumber(numEl, pctValue, { decimals: pctValue % 1 ? 1 : 0, ms: 1000, suffix: '%' });
  host.style.setProperty('--ring-color', tone || 'var(--c2)');
  return host;
}

/* ── 电平条：一排竖条既当占比条又当心电图 ───────────────────────────── */
export function meter(value, { bars = 12, tone = 'var(--c2)' } = {}) {
  const ratio = Math.max(0, Math.min(1, Number(value) || 0));
  const lit = Math.round(ratio * bars);
  return h('div', { class: 'meter' },
    Array.from({ length: bars }, (_, i) => h('i', {
      style: `background:${i < lit ? tone : 'var(--border)'};`
        + `box-shadow:${i < lit ? `0 0 8px -1px ${tone}` : 'none'};`
        + `animation-delay:${i * 55}ms`,
    })));
}

/* ── 热力图：星期 × 小时的活跃度，格子按值上色 ───────────────────────── */
export function heatmap(cells, { rows = 7, cols = 24, rowLabels = ['日', '一', '二', '三', '四', '五', '六'] } = {}) {
  const list = Array.isArray(cells) ? cells : [];
  if (!list.length) return blank('暂无活跃度数据', '需要积累一段时间的记录');
  const max = Math.max(...list.map((c) => Number(c.count) || 0)) || 1;

  const grid = h('div', { class: 'heat' });
  /* 行是星期，列是小时 */
  for (let r = 0; r < rows; r += 1) {
    grid.append(h('span', { class: 'heat-row-label' }, rowLabels[r] || String(r)));
    for (let c = 0; c < cols; c += 1) {
      const hit = list.find((x) => Number(x.day) === r && Number(x.hour) === c);
      const v = hit ? Number(hit.count) || 0 : 0;
      const t = v / max;
      const cell = h('i', {
        class: 'heat-cell',
        title: `周${rowLabels[r] || r} ${String(c).padStart(2, '0')}:00 · ${v} 条`,
        style: v
          ? `background:linear-gradient(135deg, var(--c2), var(--c1));opacity:${(0.22 + t * 0.78).toFixed(2)};`
            + `box-shadow:0 0 ${(2 + t * 12).toFixed(0)}px -1px var(--c2);`
            + `animation-delay:${(r * cols + c) * 1.6}ms`
          : null,
      });
      grid.append(cell);
    }
  }
  const axis = h('div', { class: 'heat-axis' },
    h('span', { class: 'heat-row-label' }, ''),
    ...Array.from({ length: cols }, (_, c) =>
      h('span', {}, c % 4 === 0 ? String(c).padStart(2, '0') : '')));
  return h('div', { class: 'heat-wrap' }, grid, axis,
    h('div', { class: 'heat-legend' },
      h('span', {}, '少'),
      ...Array.from({ length: 5 }, (_, i) => h('i', {
        style: `background:linear-gradient(135deg, var(--c2), var(--c1));opacity:${(0.22 + (i / 4) * 0.78).toFixed(2)}`,
      })),
      h('span', {}, '多')));
}

/* ── 活跃条：把一行数字变成会跳的电平条 ─────────────────────────────── */
export function activityBars(values, { h: hh = 30, tone = 'var(--c2)' } = {}) {
  const data = (Array.isArray(values) ? values : []).map((v) => Number(v) || 0);
  const max = Math.max(...data, 1);
  return h('div', { class: 'act', style: `height:${hh}px` },
    data.map((v, i) => h('i', {
      style: `height:${Math.max(8, (v / max) * 100)}%;background:${tone};`
        + `animation-delay:${i * 30}ms`,
    })));
}

/* ── 背景粒子：一层漂浮的光点，连近处的点 ───────────────────────────── */
export function initParticles() {
  if (matchMedia('(prefers-reduced-motion: reduce)').matches) return () => {};
  const canvas = document.createElement('canvas');
  canvas.id = 'fx';
  document.body.prepend(canvas);
  const ctx = canvas.getContext('2d');
  let raf = 0, w = 0, hgt = 0, dots = [];
  const pointer = { x: -9999, y: -9999 };

  const resize = () => {
    const dpr = Math.min(2, window.devicePixelRatio || 1);
    w = canvas.clientWidth;
    hgt = canvas.clientHeight;
    canvas.width = Math.floor(w * dpr);
    canvas.height = Math.floor(hgt * dpr);
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    /* 控制数量：面积越大越多，但封顶，免得笔记本风扇起飞 */
    const count = Math.min(90, Math.round((w * hgt) / 22000));
    dots = Array.from({ length: count }, () => ({
      x: Math.random() * w,
      y: Math.random() * hgt,
      vx: (Math.random() - 0.5) * 0.22,
      vy: (Math.random() - 0.5) * 0.22,
      r: Math.random() * 1.6 + 0.6,
      a: Math.random() * 0.5 + 0.2,
      hue: Math.random() < 0.5 ? 186 : 258,
    }));
  };

  const frame = () => {
    ctx.clearRect(0, 0, w, hgt);
    const dark = document.documentElement.dataset.theme === 'dark';
    for (const d of dots) {
      d.x += d.vx; d.y += d.vy;
      if (d.x < 0 || d.x > w) d.vx *= -1;
      if (d.y < 0 || d.y > hgt) d.vy *= -1;

      /* 靠近指针的点被轻轻推开，鼠标扫过会有反应 */
      const dx = d.x - pointer.x;
      const dy = d.y - pointer.y;
      const dist = Math.hypot(dx, dy);
      if (dist < 130) {
        d.x += (dx / dist) * 0.7;
        d.y += (dy / dist) * 0.7;
      }

      ctx.beginPath();
      ctx.arc(d.x, d.y, d.r, 0, Math.PI * 2);
      ctx.fillStyle = `hsl(${d.hue} 100% ${dark ? 68 : 46}% / ${dark ? d.a : d.a * 0.6})`;
      ctx.fill();
    }

    /* 近处的点连线，距离越近越亮 */
    for (let i = 0; i < dots.length; i += 1) {
      for (let j = i + 1; j < dots.length; j += 1) {
        const dx = dots[i].x - dots[j].x;
        const dy = dots[i].y - dots[j].y;
        const dist2 = dx * dx + dy * dy;
        if (dist2 < 15000) {
          const alpha = (1 - dist2 / 15000) * (dark ? 0.22 : 0.1);
          ctx.strokeStyle = `hsl(200 100% ${dark ? 70 : 45}% / ${alpha})`;
          ctx.lineWidth = 0.7;
          ctx.beginPath();
          ctx.moveTo(dots[i].x, dots[i].y);
          ctx.lineTo(dots[j].x, dots[j].y);
          ctx.stroke();
        }
      }
    }
    raf = requestAnimationFrame(frame);
  };

  const onMove = (e) => { pointer.x = e.clientX; pointer.y = e.clientY; };
  const onLeave = () => { pointer.x = -9999; pointer.y = -9999; };
  const onVisibility = () => {
    cancelAnimationFrame(raf);
    if (!document.hidden) raf = requestAnimationFrame(frame);
  };

  resize();
  window.addEventListener('resize', resize);
  window.addEventListener('pointermove', onMove, { passive: true });
  window.addEventListener('pointerleave', onLeave);
  document.addEventListener('visibilitychange', onVisibility);
  raf = requestAnimationFrame(frame);

  return () => {
    cancelAnimationFrame(raf);
    window.removeEventListener('resize', resize);
    window.removeEventListener('pointermove', onMove);
    window.removeEventListener('pointerleave', onLeave);
    document.removeEventListener('visibilitychange', onVisibility);
    canvas.remove();
  };
}

/* ── 数字计数：交给视图在挂载后调用，把 stat 里的字符串换成滚动数字 ──── */
export function countUpIn(root) {
  $$('[data-count]', root).forEach((el) => {
    const raw = el.dataset.count;
    const decimals = Number(el.dataset.countDecimals || 0);
    const prefix = el.dataset.countPrefix || '';
    const suffix = el.dataset.countSuffix || '';
    animateNumber(el, raw, { decimals, prefix, suffix, ms: 950 });
  });
}

/* ══ 9. 图表（ECharts 可选，缺库则降级） ═══════════════════════════════ */
let echartsPromise = null;
function loadEcharts() {
  if (echartsPromise) return echartsPromise;
  echartsPromise = new Promise((resolve) => {
    if (window.echarts) return resolve(true);
    const s = document.createElement('script');
    s.src = '/static/echarts.min.js';
    s.onload = () => resolve(!!window.echarts);
    s.onerror = () => resolve(false);
    document.head.append(s);
  });
  return echartsPromise;
}

export async function chart(option, { tall = false, height } = {}) {
  const host = h('div', { class: 'plot', 'data-tall': tall ? '' : null, style: height ? `height:${height}` : null });
  const ok = await loadEcharts();
  if (!ok) return blank('图表库未安装', '把 echarts.min.js 放进 static/ 即可启用');
  /* 先画进 host 再返回，避免首屏拿到占位块之后图表才补上 */
  const inst = window.echarts.init(host, currentTheme() === 'dark' ? 'dark' : null);
  inst.setOption(option);
  const ro = new ResizeObserver(() => inst.resize());
  ro.observe(host);
  host._dispose = () => { ro.disconnect(); inst.dispose(); };
  return host;
}

export function chartColors() {
  const css = getComputedStyle(document.documentElement);
  const pick = (name, fallback) => (css.getPropertyValue(name) || fallback).trim() || fallback;
  return {
    c1: pick('--c1', '#8b5cf6'),
    c2: pick('--c2', '#22d3ee'),
    c3: pick('--c3', '#ec4899'),
    c4: pick('--c4', '#10b981'),
    c5: pick('--c5', '#f59e0b'),
    ok: pick('--c4', '#10b981'),
    warn: pick('--c5', '#f59e0b'),
    bad: pick('--bad', '#f43f5e'),
    text: pick('--fg-muted', '#94a3b8'),
    grid: pick('--border', 'rgba(148,163,184,.2)'),
    surface: pick('--surface-solid', '#111827'),
    fg: pick('--fg', '#f8fafc'),
    dark: document.documentElement.dataset.theme === 'dark',
  };
}

/* Canvas 只认带 alpha 通道的颜色。令牌里是 `hsl(190 92% 42%)` 或 `hsl(190 92% 42% / 55%)`，
   直接在后面拼 "66" 会得到非法值，所以按格式分别处理。 */
export function alpha(color, a) {
  const c = String(color || '').trim();
  const n = Math.max(0, Math.min(1, a));
  const hsl = c.match(/^hsla?\(([^)]+)\)$/i);
  if (hsl) {
    const parts = hsl[1].split('/')[0].trim();
    return `hsl(${parts} / ${Math.round(n * 100)}%)`;
  }
  const hex = c.match(/^#([0-9a-f]{3,8})$/i);
  if (hex) {
    let h = hex[1];
    if (h.length === 3) h = h.split('').map((ch) => ch + ch).join('');
    const r = parseInt(h.slice(0, 2), 16);
    const g = parseInt(h.slice(2, 4), 16);
    const b = parseInt(h.slice(4, 6), 16);
    return `rgba(${r}, ${g}, ${b}, ${n})`;
  }
  const rgb = c.match(/^rgba?\(([^)]+)\)$/i);
  if (rgb) {
    const [r, g, b] = rgb[1].split(',').map((v) => parseFloat(v));
    return `rgba(${r || 0}, ${g || 0}, ${b || 0}, ${n})`;
  }
  return c;
}

/* 折线/柱状图的霓虹配色：渐变填充 + 发光描边 */
export function timeSeriesOption({ labels, series, yName = '' }) {
  const c = chartColors();
  const palette = [c.c2, c.c1, c.c3, c.c4, c.c5];
  const glow = (color) => alpha(color, 0.4);

  return {
    color: palette,
    grid: { left: 6, right: 16, top: 30, bottom: 6, containLabel: true },
    tooltip: {
      trigger: 'axis',
      backgroundColor: c.dark ? 'rgba(17,20,32,.94)' : 'rgba(255,255,255,.97)',
      borderColor: glow(c.c2),
      borderWidth: 1,
      textStyle: { color: c.fg, fontSize: 12 },
      extraCssText: `backdrop-filter:blur(10px);border-radius:12px;padding:9px 12px;box-shadow:0 20px 50px -18px ${glow(c.c1)};`,
    },
    legend: series.length > 1
      ? { top: 0, icon: 'roundRect', itemWidth: 10, itemHeight: 4, textStyle: { color: c.text, fontSize: 11 } }
      : undefined,
    xAxis: {
      type: 'category', data: labels, boundaryGap: false,
      axisLine: { lineStyle: { color: c.grid } },
      axisTick: { show: false },
      axisLabel: { color: c.text, fontSize: 11, margin: 10 },
    },
    yAxis: {
      type: 'value', name: yName || undefined,
      nameTextStyle: { color: c.text, fontSize: 11 },
      splitLine: { lineStyle: { color: c.grid, type: [3, 5] } },
      axisLabel: { color: c.text, fontSize: 11 },
    },
    series: series.map((s, i) => {
      const color = palette[i % palette.length];
      const isBar = s.type === 'bar';
      return {
        name: s.name,
        type: s.type || 'line',
        smooth: 0.4,
        data: s.data,
        showSymbol: false,
        symbolSize: 7,
        barMaxWidth: 38,
        /* 折线：上方渐变消隐的填充 + 发光描边 */
        areaStyle: s.area && !isBar ? {
          opacity: 1,
          color: {
            type: 'linear', x: 0, y: 0, x2: 0, y2: 1,
            colorStops: [
              { offset: 0, color: alpha(color, 0.45) },
              { offset: 1, color: alpha(color, 0) },
            ],
          },
        } : undefined,
        itemStyle: isBar ? {
          borderRadius: [5, 5, 0, 0],
          color: {
            type: 'linear', x: 0, y: 0, x2: 0, y2: 1,
            colorStops: [
              { offset: 0, color },
              { offset: 1, color: alpha(color, 0.35) },
            ],
          },
        } : { color },
        lineStyle: isBar ? undefined : {
          width: 2.5,
          color,
          shadowColor: glow(color),
          shadowBlur: 14,
          shadowOffsetY: 3,
        },
      };
    }),
  };
}
