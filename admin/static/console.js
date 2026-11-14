'use strict';
/* FM Console —— 一次到位的全功能后台（纯原生 JS，无构建步骤）。
   数据来源：/api/v2/*（我新加）+ 旧 /api/*（复用）+ /api/domain/* 代理。 */

const $ = (sel, root = document) => root.querySelector(sel);
const esc = (v) => String(v == null ? '' : v).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
const fmt = (v) => v == null || v === '' ? '-' : String(v);
const num = (v) => typeof v === 'number' ? v.toLocaleString('zh-CN') : fmt(v);

async function api(path, opts) {
  const res = await fetch(path, Object.assign({ credentials: 'same-origin', headers: { Accept: 'application/json' } }, opts || {}));
  if (res.status === 401) { location.href = '/login'; throw new Error('未登录'); }
  const text = await res.text();
  let data; try { data = JSON.parse(text); } catch (e) { data = { raw: text }; }
  if (!res.ok) throw new Error(data.error || data.message || ('HTTP ' + res.status));
  return data;
}
const get = (p) => api(p);
const num0 = (v, d = 0) => typeof v === 'number' ? v : (Number(v) || d);

function table(headers, rows, opts) {
  opts = opts || {};
  if (!rows || !rows.length) return '<div class="muted">没有数据</div>';
  const head = headers.map(h => `<th>${esc(h[0])}</th>`).join('');
  const body = rows.map((row, i) => {
    const tds = headers.map(h => `<td>${h[2] ? h[2](row) : esc(fmt(row[h[1]]))}</td>`).join('');
    return `<tr data-i="${i}">${tds}</tr>`;
  }).join('');
  const box = document.createElement('div');
  box.innerHTML = `<table><thead><tr>${head}</tr></thead><tbody>${body}</tbody></table>`;
  if (opts.onRow) box.querySelectorAll('tbody tr').forEach(tr => tr.onclick = () => opts.onRow(rows[Number(tr.dataset.i)], tr));
  return box.innerHTML;
}
const card = (title, inner) => `<section class="card"><h2>${esc(title)}</h2>${inner}</section>`;
const stat = (k, v, cls) => `<div class="stat"><div class="k">${esc(k)}</div><div class="v ${cls || ''}">${v}</div></div>`;
const stats = (items) => `<div class="grid">${items.map(i => stat(i[0], i[1], i[2])).join('')}</div>`;
const pre = (obj) => `<pre>${esc(JSON.stringify(obj, null, 1))}</pre>`;
const input = (id, ph, val) => `<input id="${id}" placeholder="${esc(ph)}" value="${esc(val || '')}"/>`;
const btn = (label, id, cls) => `<button class="btn ${cls || ''}" id="${id||''}">${esc(label)}</button>`;
const fail = (e) => `<section class="card err">加载失败：${esc(e.message || e)}</section>`;

/* ── 视图表 ───────────────────────────────────────────── */
const VIEWS = {};

VIEWS.overview = { name: '总览', async render(main) {
  const [ov, obs, errs] = await Promise.all([get('/api/v2/overview'), get('/api/observability/overview').catch(() => null), get('/api/errors/stats').catch(() => null)]);
  const d = ov.data;
  main.innerHTML = `<h1>总览</h1><div class="sub">机器人配置 · 消息量 · 工具调用 · 服务健康</div>` +
    stats([
      ['聊天模型', esc(d.chat_model || '-')], ['思考档位', esc(d.reasoning_effort || '-')],
      ['最大轮数', esc(d.config.base.agent_max_turns || '-')], ['上下文', esc(d.config.base.recent_chat_context_max_chars || '-') + ' 字符'],
      ['消息总数', num0(d.chat.messages)], ['机器人消息', num0(d.chat.bot_messages)],
      ['发言人数', num0(d.chat.senders)], ['最近一条', esc(d.chat.last_at || '-')],
      ['拟人会话', num0(d.humanlike.scope_count)], ['domain', d.health && (d.health.ok ? '<span class="tag ok">正常</span>' : '<span class="tag bad">异常</span>')],
    ]) +
    card('工具调用（近 7 天）', table([['工具', 'tool'], ['调用', 'calls'], ['失败', 'failures'], ['最近', 'last']], d.tools.rows.slice(0, 15))) +
    card('拟人模式已开启', d.humanlike.scopes.map(s => `<span class="tag">${esc(s)}</span>`).join(' ') || '<span class="muted">无</span>') +
    card('配置原文', pre(d.config));
  },
};

VIEWS.groups = { name: '群与房间', async render(main) {
  const r = await get('/api/v2/groups');
  main.innerHTML = `<h1>群与房间</h1><div class="sub">点任意一行看详情；拟人模式可在详情里开关</div>` +
    card('全部会话', table([['群号', 'group_id'], ['名称', 'name'], ['状态', 'status'],
      ['消息', 'messages', (r0) => num0(r0.messages)],
      ['拟人', 'humanlike', (r0) => r0.humanlike ? '<span class="tag ok">开</span>' : '<span class="tag">关</span>']],
      r.data.groups, { onRow: (g) => showGroup(main, g.group_id) }));
  },
};

async function showGroup(main, gid) {
  const r = await get('/api/v2/group?id=' + encodeURIComponent(gid));
  const d = r.data;
  main.innerHTML = `<h1>群 <span class="muted">${esc(gid)}</span></h1><div class="sub"><a href="#groups">← 返回群列表</a></div>` +
    stats([['消息数', num0(d.messages)], ['拟人模式', d.humanlike ? '<span class="tag ok">开</span>' : '<span class="tag">关</span>']]) +
    card('拟人模式开关', `<div class="row">${btn(d.humanlike ? '关闭拟人模式' : '开启拟人模式', 'hl-toggle', 'ghost')}<span class="muted">改动会写 /runtime/humanlike/*.txt，下一条消息生效</span></div>`) +
    card('最近消息', table([['时间', 'recorded_at', (r0) => esc(String(r0.recorded_at || '').slice(0, 19))],
      ['发送者', 'sender_username'], ['机器人', 'is_bot', (r0) => r0.is_bot ? '是' : ''],
      ['内容', 'body_text', (r0) => esc(String(r0.body_text || '').slice(0, 120))]], d.recent)) +
    card('活跃成员', table([['成员', 'sender_username'], ['消息', 'n', (r0) => num0(r0.n)]], d.top_senders)) +
    card('domain 状态', pre(d.domain_state));
  $('#hl-toggle').onclick = async () => {
    const cur = await get('/api/v2/humanlike');
    const scope = 'qq:group:' + gid;
    let scopes = cur.data.scopes.filter(s => s !== scope);
    if (!d.humanlike) scopes.unshift(scope);
    await api('/api/v2/humanlike', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ scopes }) });
    showGroup(main, gid);
  };
}

VIEWS.members = { name: '成员反查', async render(main) {
  main.innerHTML = `<h1>成员反查</h1><div class="sub">按昵称片段或 QQ 号查，返回该账号全部曾用昵称</div>` +
    card('查询', `<div class="row">${input('m-q', '昵称片段 / QQ 号', '')}${btn('查询', 'm-go')}</div><div id="m-out" class="muted">输入后点查询</div>`);
  const run = async () => {
    const q = $('#m-q').value.trim(); if (!q) return;
    $('#m-out').innerHTML = '查询中…';
    const r = await get('/api/v2/members?q=' + encodeURIComponent(q));
    const ms = r.data.matches || [];
    $('#m-out').innerHTML = ms.length ? ms.map(m => `<div class="card"><div><b>QQ ${esc(m.user_id)}</b> · ${num0(m.messages)} 条 · 最近 ${esc(m.last_seen || '-')}</div><div style="margin-top:6px">${m.aliases.map(a => `<span class="tag">${esc(a)}</span>`).join(' ')}</div></div>`).join('') : '<div class="muted">没有匹配的成员</div>';
  };
  $('#m-go').onclick = run; $('#m-q').onkeydown = (e) => { if (e.key === 'Enter') run(); };
  },
};

VIEWS.messages = { name: '消息搜索', async render(main) {
  main.innerHTML = `<h1>消息搜索</h1><div class="sub">全量聊天记录（含机器人）</div>` +
    card('筛选', `<div class="row">${input('s-q', '关键词', '')}${input('s-chat', '群号（可空）', '')}${input('s-days', '最近天数', '7')}${btn('搜索', 's-go')}</div><div id="s-out" class="muted">输入后点搜索</div>`);
  const run = async () => {
    $('#s-out').innerHTML = '搜索中…';
    const p = new URLSearchParams({ q: $('#s-q').value.trim(), chat: $('#s-chat').value.trim(), days: $('#s-days').value.trim() || '0', limit: '100' });
    const r = await get('/api/v2/messages?' + p.toString());
    $('#s-out').innerHTML = r.data.rows.length ? table([['时间', 'recorded_at', (x) => esc(String(x.recorded_at || '').slice(0, 19))],
      ['群', 'chat_id'], ['发送者', 'sender_username'], ['BOT', 'is_bot', (x) => x.is_bot ? '是' : ''],
      ['内容', 'body_text', (x) => esc(String(x.body_text || '').slice(0, 150))]], r.data.rows) : '<div class="muted">没有命中</div>';
  };
  $('#s-go').onclick = run; $('#s-q').onkeydown = (e) => { if (e.key === 'Enter') run(); };
  },
};

VIEWS.humanlike = { name: '拟人模式', async render(main) {
  const r = await get('/api/v2/humanlike');
  const d = r.data;
  main.innerHTML = `<h1>拟人模式</h1><div class="sub">scopes 每行一个会话；保存后写入 bot 读取的文件</div>` +
    card('已开启会话（可编辑）', `<textarea id="hl-scopes">${esc(d.scopes.join('\n'))}</textarea><div class="row" style="margin-top:8px">${btn('保存', 'hl-save')}<span class="muted">格式：qq:group:群号 / qq:private:QQ号</span></div>`) +
    card('主动发言配置（只读）', table([['会话', 'scope'], ['日期', 'date'], ['上限', 'limit'], ['时间戳', 'ts']], d.proactive)) +
    card('humanlike 文件', pre(d.files));
  $('#hl-save').onclick = async () => {
    const scopes = $('#hl-scopes').value.split('\n').map(s => s.trim()).filter(Boolean);
    try { await api('/api/v2/humanlike', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ scopes }) }); alert('已保存：' + scopes.length + ' 个会话'); VIEWS.humanlike.render(main); }
    catch (e) { alert('保存失败：' + e.message); }
  };
  },
};

VIEWS.models = { name: '模型与配置', async render(main) {
  const [models, ov] = await Promise.all([get('/api/runtime/config/models').catch(() => null), get('/api/v2/overview')]);
  main.innerHTML = `<h1>模型与配置</h1><div class="sub">当前模型 · 图片模型 · 请求参数</div>` +
    card('聊天模型', pre(ov.data.config.chat_models)) +
    card('图片模型', pre(ov.data.config.image_models)) +
    card('运行参数', pre(ov.data.config.base)) +
    card('models 接口', models ? pre(models) : '<div class="muted">无数据</div>');
  },
};

VIEWS.tools = { name: '工具与审计', async render(main) {
  main.innerHTML = `<h1>工具与审计</h1><div class="sub">工具调用统计（近 7 天）+ 明细</div><div id="t-out" class="muted">加载中…</div>`;
  const [st, au] = await Promise.all([get('/api/v2/tools/stats?days=7'), get('/api/v2/tools/audit?days=7&limit=200').catch(() => null)]);
  $('#t-out').innerHTML = stats([['工具数', st.data.rows.length], ['总调用', num0(st.data.total_calls)]]) +
    card('调用统计', table([['工具', 'tool'], ['调用', 'calls'], ['失败', 'failures', (x) => x.failures ? `<span class="err">${x.failures}</span>` : '0'], ['最近', 'last']], st.data.rows)) +
    card('最近调用明细', au ? table([['时间', 'at'], ['工具', 'tool'], ['状态', 'status', (x) => x.status === 'ok' ? '<span class="tag ok">ok</span>' : `<span class="tag bad">${esc(x.status)}</span>`], ['结果', 'result', (x) => esc(String(x.result || '').slice(0, 90))]], au.data.rows) : '<div class="muted">无明细</div>');
  },
};

VIEWS.business = { name: '业务数据', async render(main) {
  main.innerHTML = `<h1>业务数据</h1><div class="sub">文库 / 赛文 / 成绩 / 番茄</div><div id="b-out" class="loading">加载中…</div>`;
  const [lib, con, sco, nov] = await Promise.all([
    get('/api/domain/library?limit=30').catch(e => null),
    get('/api/domain/contests?limit=30').catch(e => null),
    get('/api/domain/scores?limit=30').catch(e => null),
    get('/api/v2/novels').catch(e => null),
  ]);
  const items = (x) => { const d = x && (x.data || x); if (!d) return []; if (Array.isArray(d)) return d; const arr = d.items || d.data || d.rows || d.results; return Array.isArray(arr) ? arr : []; };
  const generic = (rows) => rows.length ? table(Object.keys(rows[0]).slice(0, 6).map(k => [k, k, (r) => esc(String(r[k] == null ? '' : (typeof r[k] === 'object' ? JSON.stringify(r[k]) : r[k])).slice(0, 60))]), rows.slice(0, 30)) : '<div class="muted">无数据</div>';
  $('#b-out').innerHTML = stats([['文库', items(lib).length], ['赛文', items(con).length], ['成绩', items(sco).length], ['番茄书', num0(nov && nov.data && nov.data.books)]]) +
    card('跟打文库（前 30）', generic(items(lib))) +
    card('赛文（前 30）', generic(items(con))) +
    card('成绩（前 30）', generic(items(sco))) +
    card('番茄小说', nov && nov.data ? stats([['书', num0(nov.data.books)], ['章节', num0(nov.data.chapters)], ['进度记录', num0(nov.data.progress)]]) + table([['书名', 'title'], ['作者', 'author'], ['章节', 'total_chapters'], ['字数', 'word_count']], (nov.data.items || []).slice(0, 40)) : '<div class="muted">domain 返回空</div>');
  },
};

VIEWS.collections = { name: '配置集合', async render(main) {
  const names = ['triggers', 'personas', 'models', 'groups'];
  main.innerHTML = `<h1>配置集合</h1><div class="sub">触发规则 · 人设 · 模型 · 群配置（只读预览，编辑请在旧版后台）</div><div id="c-out" class="loading">加载中…</div>`;
  const parts = [];
  for (const n of names) {
    try { const r = await get('/api/collections/' + n); parts.push(card(n, `<pre>${esc(JSON.stringify(r, null, 1)).slice(0, 3000)}</pre>`)); }
    catch (e) { parts.push(card(n, `<div class="err">${esc(e.message)}</div>`)); }
  }
  $('#c-out').innerHTML = parts.join('');
  },
};

VIEWS.logs = { name: '日志与错误', async render(main) {
  const [logs, errs] = await Promise.all([get('/api/logs').catch(e => null), get('/api/errors/stats').catch(e => null)]);
  main.innerHTML = `<h1>日志与错误</h1><div class="sub">后台日志 / 错误统计</div>` +
    card('日志', logs ? `<pre>${esc(JSON.stringify(logs, null, 1)).slice(0, 8000)}</pre>` : '<div class="muted">无数据</div>') +
    card('错误统计', errs ? pre(errs) : '<div class="muted">无数据</div>');
  },
};

VIEWS.ops = { name: '运维', async render(main) {
  const [ov, obs, conc, media] = await Promise.all([
    get('/api/overview').catch(e => null), get('/api/observability/overview').catch(e => null),
    get('/api/runtime/concurrency').catch(e => null), get('/api/runtime/media').catch(e => null)]);
  main.innerHTML = `<h1>运维</h1><div class="sub">总览 / 可观测性 / 并发 / 媒体</div>` +
    card('overview', ov ? pre(ov) : '<div class="muted">无数据</div>') +
    card('observability', obs ? pre(obs) : '<div class="muted">无数据</div>') +
    card('并发', conc ? pre(conc) : '<div class="muted">无数据</div>') +
    card('媒体', media ? pre(media) : '<div class="muted">无数据</div>');
  },
};

/* ── 导航与路由 ───────────────────────────────────────── */
const NAV = [
  ['概览', ['overview']],
  ['会话', ['groups', 'members', 'messages']],
  ['机器人', ['humanlike', 'models', 'tools']],
  ['业务', ['business', 'collections']],
  ['系统', ['logs', 'ops']],
];

function drawNav(active) {
  const root = $('#navlist');
  root.innerHTML = NAV.map(([cap, keys]) => `<div class="navcap">${cap}</div>` + keys.map(k =>
    `<button data-view="${k}" class="${k === active ? 'active' : ''}">${esc(VIEWS[k].name)}</button>`).join('')).join('');
  root.querySelectorAll('button').forEach(b => b.onclick = () => { location.hash = '#/' + b.dataset.view; });
}

async function route() {
  const key = (location.hash || '#/overview').replace(/^#\//, '') || 'overview';
  const view = VIEWS[key] || VIEWS.overview;
  drawNav(key in VIEWS ? key : 'overview');
  const main = $('#main');
  main.innerHTML = '<div class="loading">加载中…</div>';
  try { await view.render(main); }
  catch (e) { main.innerHTML = `<h1>${esc(view.name)}</h1>` + fail(e); }
}

$('#logout').onclick = async (e) => {
  e.preventDefault();
  try { await api('/api/logout', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: '{}' }); } catch (err) { }
  location.href = '/login';
};

window.addEventListener('hashchange', route);
route();
