/* ══════════════════════════════════════════════════════════════════════════
 * pages-prefix.js · 「回复前缀」页（#/prefix）
 * 把「回复前缀开关」做成可视化 UI：当前状态 / 恢复默认 / 自定义 / 关闭，
 * 保存后提示「需重启生效」，并可直接触发「立即生效（重启 FM）」+ 恢复轮询。
 *
 * 后端契约（未就绪时占位 + 原因 + 重试，不白屏）：
 *   GET  /api/v2/prefix          → {ok,data:{prefix,enabled,raw,file_mtime,default_prefix}}
 *   POST /api/v2/prefix          → {action:"set"|"off"|"reset", text?} → {..., applied, note}
 *   POST /api/v2/prefix/apply    → 重启 bot 使其生效（管理员）
 * ════════════════════════════════════════════════════════════════════════ */
(function () {
  'use strict';

  const PREFIX_PATH = '/api/v2/prefix';
  const APPLY_PATH = '/api/v2/prefix/apply';
  const SAMPLE_QUESTION = '今天群里谁最活跃？';
  const MAX_LEN = 40;
  const APPLY_LIMIT_MS = 90000;

  const DEFAULT_PREFIX_FALLBACK = '😻 FM：';

  function normalize(raw) {
    const d = raw && typeof raw === 'object' ? raw : {};
    const defaultPrefix = String(d.default_prefix != null ? d.default_prefix : DEFAULT_PREFIX_FALLBACK) || DEFAULT_PREFIX_FALLBACK;
    const rawPrefix = d.prefix != null ? String(d.prefix) : (d.raw != null ? String(d.raw) : '');
    const noneText = rawPrefix.trim() === '<none>' || rawPrefix === '';
    const enabled = d.enabled === false || noneText ? false : true;
    const prefix = enabled ? rawPrefix : '';
    const mtime = d.file_mtime != null ? d.file_mtime : (d.file_mtime_text || null);
    return {
      raw: d,
      defaultPrefix: defaultPrefix,
      prefix: prefix,
      enabled: enabled,
      mode: !enabled ? 'off' : (prefix === defaultPrefix ? 'default' : 'custom'),
      fileMtime: mtime,
      note: d.note || '',
      applied: d.applied,
    };
  }

  function isWriteBlocked() {
    try { return typeof isReadonly === 'function' && isReadonly(); } catch (err) { return false; }
  }

  function sampleText(prefix) {
    return (prefix || '') + SAMPLE_QUESTION;
  }

  VIEWS.prefix = {
    id: 'prefix',
    name: '回复前缀',
    icon: 'message',
    skeleton: 'default',
    render: async (ctx) => {
      const root = ctx.root;
      const state = {
        data: null,          // normalize() 结果
        pending: false,
        error: null,
        loading: false,
        mode: 'default',
        customText: '',
        savedNeedApply: false,
        savedNote: '',
        saving: false,
        applying: false,
        applyStartedAt: 0,
        logs: [],
        readonly: isWriteBlocked(),
      };

      root.innerHTML = pageHead({
        eyebrow: 'BOT',
        title: '回复前缀',
        icon: 'message',
        sub: '控制 FM 每条回复开头的前缀文本：恢复默认 / 自定义 / 关闭。保存到配置后需重启 FM 生效，本页可直接触发重启并等待恢复。',
        actions: [
          '<button class="btn btn-sm btn-ghost" id="prefix-refresh" type="button">' + icon('refresh') + '刷新状态</button>',
          '<a class="btn btn-sm btn-ghost" href="#/humanlike" type="button">' + icon('spark') + '拟人模式</a>',
        ],
      }) +
      '<div id="prefix-alert"></div>' +
      '<div id="prefix-readonly"></div>' +
      '<div id="prefix-status" class="prefix-status"></div>' +
      '<div class="prefix-layout">' +
        '<section class="card"><div class="card-head"><h3>' + icon('gear') + '前缀设置</h3>' +
          '<div class="card-head-actions"><span id="prefix-current-tag"></span></div></div>' +
          '<div class="card-body">' +
            '<div class="prefix-modes" id="prefix-modes" role="radiogroup" aria-label="前缀模式">' +
              '<button class="prefix-mode" data-mode="default" type="button" role="radio" aria-checked="false">' +
                '<span class="prefix-mode-dot"></span><span class="prefix-mode-main">' +
                '<span class="prefix-mode-title">恢复默认</span>' +
                '<span class="prefix-mode-desc">使用默认前缀「<span class="mono">' + esc(DEFAULT_PREFIX_FALLBACK) + '</span>」</span></span></button>' +
              '<button class="prefix-mode" data-mode="custom" type="button" role="radio" aria-checked="false">' +
                '<span class="prefix-mode-dot"></span><span class="prefix-mode-main">' +
                '<span class="prefix-mode-title">自定义</span>' +
                '<span class="prefix-mode-desc">自己输入前缀（≤' + MAX_LEN + ' 字），实时预览效果</span></span></button>' +
              '<button class="prefix-mode" data-mode="off" type="button" role="radio" aria-checked="false">' +
                '<span class="prefix-mode-dot"></span><span class="prefix-mode-main">' +
                '<span class="prefix-mode-title">关闭</span>' +
                '<span class="prefix-mode-desc">回复不带任何前缀（等于 &lt;none&gt;）</span></span></button>' +
            '</div>' +
            '<div class="prefix-field" id="prefix-custom-field" hidden>' +
              '<div class="prefix-field-head"><label for="prefix-input">自定义前缀（≤' + MAX_LEN + ' 字）</label><span class="prefix-counter" id="prefix-counter">0/' + MAX_LEN + '</span></div>' +
              '<div class="prefix-input-wrap"><input class="input" id="prefix-input" type="text" maxlength="' + MAX_LEN + '" placeholder="例如：😻 小 FM：" autocomplete="off">' +
                '<button class="prefix-clear" id="prefix-clear" type="button">清空</button></div>' +
              '<div class="field-hint">前缀会原样拼在每条回复最前面；建议以分隔符结尾（如「：」或空格），避免和正文粘连。</div>' +
            '</div>' +
            '<div class="prefix-preview-card">' +
              '<div class="prefix-preview-label">' + icon('eye') + '实时预览</div>' +
              '<div class="prefix-bubble-row">' +
                '<span class="prefix-avatar">FM</span>' +
                '<div class="prefix-bubble"><span id="prefix-preview-text"></span>' +
                  '<div class="prefix-bubble-meta">示例：群里提问时的实际回复开头</div></div>' +
              '</div>' +
              '<div class="prefix-preview-hint" id="prefix-preview-hint"></div>' +
            '</div>' +
            '<div class="prefix-actions">' +
              '<button class="btn btn-sm btn-solid" id="prefix-save" type="button">' + icon('save') + '保存前缀</button>' +
              '<button class="btn btn-sm btn-ghost" id="prefix-reload" type="button">' + icon('refresh') + '重新读取</button>' +
              '<span class="spacer"></span>' +
              '<span class="muted text-xs" id="prefix-dirty-hint"></span>' +
            '</div>' +
            '<div id="prefix-apply-box" hidden></div>' +
          '</div></section>' +
        '<div class="prefix-side">' +
          '<section class="card"><div class="card-head"><h3>' + icon('clock') + '生效方式</h3></div>' +
            '<div class="card-body">' +
              '<div class="prefix-detail-list">' +
                '<div class="prefix-log-row"><span class="muted-2">1</span><span class="prefix-log-text">选择模式并保存 → 写回配置</span></div>' +
                '<div class="prefix-log-row"><span class="muted-2">2</span><span class="prefix-log-text">点「立即生效」→ 重启 FM 机器人</span></div>' +
                '<div class="prefix-log-row"><span class="muted-2">3</span><span class="prefix-log-text">本页自动轮询状态，恢复后显示耗时</span></div>' +
              '</div>' +
              '<div class="card-note">' + icon('info') + '<div>前缀保存在机器人配置中（GET <span class="mono">/api/v2/prefix</span> 返回 <span class="mono">file_mtime</span>）；保存后 bot 进程内状态需要重启才会使用新值。</div></div>' +
              '<div class="row mt-12"><button class="btn btn-sm btn-ghost" id="prefix-json" type="button">' + icon('code') + '查看原始 JSON</button>' +
                '<a class="btn btn-sm btn-ghost" href="#/selftest">' + icon('shield-check') + '接口自检</a></div>' +
            '</div></section>' +
          '<section class="card"><div class="card-head"><h3>' + icon('terminal') + '操作记录</h3></div>' +
            '<div class="card-body"><div class="prefix-log" id="prefix-log">' +
              '<div class="prefix-log-row"><span class="prefix-log-time">—</span><span class="prefix-log-text">本页操作会记录在这里</span></div>' +
            '</div></div></section>' +
        '</div>' +
      '</div>';

      const alertRoot = $('#prefix-alert', root);
      const readonlyRoot = $('#prefix-readonly', root);
      const statusRoot = $('#prefix-status', root);
      const modesRoot = $('#prefix-modes', root);
      const customField = $('#prefix-custom-field', root);
      const input = $('#prefix-input', root);
      const counter = $('#prefix-counter', root);
      const previewText = $('#prefix-preview-text', root);
      const previewHint = $('#prefix-preview-hint', root);
      const saveBtn = $('#prefix-save', root);
      const reloadBtn = $('#prefix-reload', root);
      const applyBox = $('#prefix-apply-box', root);
      const logRoot = $('#prefix-log', root);
      const currentTag = $('#prefix-current-tag', root);
      const dirtyHint = $('#prefix-dirty-hint', root);

      /* ── 操作日志 ─────────────────────────────────────────────────── */
      function addLog(text, tone) {
        const time = new Date();
        const pad = (n) => (n < 10 ? '0' + n : '' + n);
        state.logs.unshift({ at: pad(time.getHours()) + ':' + pad(time.getMinutes()) + ':' + pad(time.getSeconds()), text: text, tone: tone || 'info' });
        state.logs = state.logs.slice(0, 8);
        if (!logRoot) return;
        logRoot.innerHTML = state.logs.map((row) =>
          '<div class="prefix-log-row" data-tone="' + escAttr(row.tone) + '"><span class="prefix-log-time">' + esc(row.at) + '</span><span class="prefix-log-text">' + esc(row.text) + '</span></div>').join('');
      }

      function canWrite() { return !state.readonly; }

      /* ── 状态卡 ───────────────────────────────────────────────────── */
      function paintStatus() {
        if (!statusRoot) return;
        const d = state.data;
        if (state.pending || state.error || !d) {
          statusRoot.innerHTML = kpiGrid([
            { label: '当前前缀', icon: 'message', valueHtml: '—', hint: state.pending ? '接口未就绪' : '读取失败' },
            { label: '状态', icon: 'check-circle', valueHtml: '—', hint: '启用 / 关闭未知' },
            { label: '文件修改时间', icon: 'clock', valueHtml: '—' },
            { label: '默认前缀', icon: 'sparkles', valueHtml: '<span class="mono">' + esc(DEFAULT_PREFIX_FALLBACK) + '</span>', hint: '恢复默认时使用' },
          ], { cols: 4 });
          if (currentTag) currentTag.innerHTML = '';
          return;
        }
        const prefixText = d.enabled ? d.prefix : '';
        statusRoot.innerHTML = kpiGrid([
          {
            label: '当前前缀', icon: 'message', tone: d.enabled ? 'acc' : '',
            valueHtml: d.enabled
              ? '<span class="mono" style="font-size:15px">' + esc(truncateText(prefixText, 24)) + '</span>'
              : '<span class="tag tag-muted">不带前缀</span>',
            hint: d.enabled ? '已启用：回复开头拼接此文本' : '当前关闭（&lt;none&gt;）',
          },
          {
            label: '状态', icon: d.enabled ? 'check-circle' : 'x-circle', tone: d.enabled ? 'ok' : 'muted',
            valueHtml: d.enabled ? '已启用' : '已关闭',
            hint: d.enabled ? '每条回复带前缀' : '回复不带前缀',
          },
          {
            label: '文件修改时间', icon: 'clock',
            valueHtml: d.fileMtime ? '<span style="font-size:14px">' + esc(ago(d.fileMtime)) + '</span>' : '—',
            hint: d.fileMtime ? esc(fmtTime(d.fileMtime)) : '接口未返回 file_mtime',
          },
          {
            label: '默认前缀', icon: 'sparkles',
            valueHtml: '<span class="mono" style="font-size:15px">' + esc((state.data && state.data.defaultPrefix) || DEFAULT_PREFIX_FALLBACK) + '</span>',
            hint: '「恢复默认」会使用这个值',
          },
        ], { cols: 4 });
        if (currentTag) {
          currentTag.innerHTML = d.enabled
            ? '<span class="tag tag-ok">' + icon('check') + '前缀已启用</span>'
            : '<span class="tag tag-muted">' + icon('minus') + '不使用前缀</span>';
        }
      }

      function truncateText(text, max) {
        const value = String(text == null ? '' : text);
        return value.length > max ? value.slice(0, max - 1) + '…' : value;
      }

      /* ── 顶部提示 / 只读提示 ─────────────────────────────────────── */
      function paintAlert() {
        if (!alertRoot) return;
        if (state.pending) {
          alertRoot.innerHTML = notice('warn', '<div><div class="prefix-alert-title">回复前缀接口尚未就绪</div>' +
            '<div class="prefix-alert-desc">后端 <code class="inline">' + esc(PREFIX_PATH) + '</code> 还没有返回数据（HTTP 404/501）。页面已按占位渲染，接口上线后会自动恢复；也可以点「刷新状态」重试。</div></div>' +
            '<div class="row mt-12"><button class="btn btn-sm btn-ghost" id="prefix-alert-retry" type="button">' + icon('refresh') + '重试</button>' +
            '<a class="btn btn-sm btn-ghost" href="#/selftest" type="button">' + icon('shield-check') + '接口自检</a></div>');
        } else if (state.error) {
          alertRoot.innerHTML = notice('bad', '<div><div class="prefix-alert-title">回复前缀读取失败</div>' +
            '<div class="prefix-alert-desc">' + esc(state.error.message || '未知错误') + '（' + esc(PREFIX_PATH) + '）。可以点「刷新状态」重试。</div></div>' +
            '<div class="row mt-12"><button class="btn btn-sm btn-ghost" id="prefix-alert-retry" type="button">' + icon('refresh') + '重试</button></div>');
        } else {
          alertRoot.innerHTML = '';
        }
        const retry = $('#prefix-alert-retry', alertRoot);
        if (retry) retry.addEventListener('click', (event) => { event.stopPropagation(); load(true); });
      }

      function paintReadonly() {
        if (!readonlyRoot) return;
        if (!state.readonly) { readonlyRoot.innerHTML = ''; return; }
        readonlyRoot.innerHTML = '<div class="prefix-readonly-note">' + notice('warn', '<b>只读账号</b>：前缀设置控件已全部禁用（需要管理员权限）。可以查看当前配置，但不能保存或重启。') + '</div>';
      }

      /* ── 模式 / 预览 ─────────────────────────────────────────────── */
      function currentPreviewPrefix() {
        const d = state.data;
        const defaultPrefix = (d && d.defaultPrefix) || DEFAULT_PREFIX_FALLBACK;
        if (state.mode === 'off') return '';
        if (state.mode === 'default') return defaultPrefix;
        return state.customText;
      }

      function paintModes() {
        if (!modesRoot) return;
        Array.prototype.forEach.call(modesRoot.querySelectorAll('.prefix-mode'), (btn) => {
          const active = btn.getAttribute('data-mode') === state.mode;
          btn.classList.toggle('active', active);
          btn.setAttribute('aria-checked', active ? 'true' : 'false');
          btn.disabled = !canWrite();
          if (!canWrite()) btn.setAttribute('title', '需要管理员权限（当前为只读账号）');
          else btn.removeAttribute('title');
        });
        if (customField) customField.hidden = state.mode !== 'custom';
      }

      function paintPreview() {
        const d = state.data;
        const prefix = currentPreviewPrefix();
        const sample = sampleText(prefix);
        if (previewText) {
          if (state.mode === 'off') {
            previewText.innerHTML = '<span class="prefix-token off">（不带前缀）</span>' + esc(SAMPLE_QUESTION);
          } else {
            previewText.innerHTML = '<span class="prefix-token">' + esc(prefix || '（空）') + '</span>' + esc(SAMPLE_QUESTION);
          }
        }
        if (previewHint) {
          if (state.mode === 'off') previewHint.textContent = '关闭后机器人直接回复正文：' + SAMPLE_QUESTION;
          else if (state.mode === 'default') previewHint.textContent = '恢复默认：使用「' + ((d && d.defaultPrefix) || DEFAULT_PREFIX_FALLBACK) + '」作为前缀。';
          else previewHint.textContent = prefix ? '自定义前缀会原样拼接：' + sample : '请输入自定义前缀（保存前会校验非空）。';
        }
        if (counter) {
          const len = (state.customText || '').length;
          counter.textContent = len + '/' + MAX_LEN;
          counter.classList.toggle('over', len > MAX_LEN);
        }
        if (dirtyHint) {
          if (state.saving) dirtyHint.textContent = '保存中…';
          else if (state.applying) dirtyHint.textContent = '正在重启并等待恢复…';
          else if (state.savedNeedApply) dirtyHint.textContent = '已保存，等待重启生效';
          else if (d && d.enabled) dirtyHint.textContent = '当前生效值：' + truncateText(d.prefix, 24);
          else dirtyHint.textContent = '当前：不带前缀';
        }
      }

      function paintApplyBox() {
        if (!applyBox) return;
        if (!state.savedNeedApply && !state.applying) { applyBox.hidden = true; applyBox.innerHTML = ''; return; }
        applyBox.hidden = false;
        const elapsed = state.applying ? Math.max(0, Math.round((Date.now() - state.applyStartedAt) / 1000)) : 0;
        applyBox.innerHTML = '<div class="prefix-apply-box">' +
          '<div class="prefix-apply-title">' + icon(state.applying ? 'loader' : 'alert') + (state.applying ? '正在重启 FM 并等待恢复…' : '已写入配置，需要重启 FM 生效') + '</div>' +
          '<div class="prefix-apply-desc">' + (state.applying
            ? '已提交重启请求，当前已等待 ' + elapsed + 's（最多 ' + (APPLY_LIMIT_MS / 1000) + 's）。恢复后这里会显示耗时。'
            : esc(state.savedNote || '保存只写回配置文件，bot 进程内状态需要重启后才会使用新前缀。')) + '</div>' +
          (state.applying
            ? '<div class="run-progress" style="margin:10px 0 0"><div class="rp-head"><span class="rp-title mono">' + icon('loader', 'spin') + ' 重启中 / 等待恢复</span><span class="rp-time mono">' + elapsed + 's</span></div><div class="progress-track"><div class="progress-fill indeterminate"></div></div></div>'
            : '<div class="prefix-apply-actions"><button class="btn btn-sm btn-solid" id="prefix-apply" type="button"' + (canWrite() ? '' : ' disabled title="需要管理员权限"') + '>' + icon('play') + '立即生效（重启 FM）</button>' +
              '<button class="btn btn-sm btn-ghost" id="prefix-apply-later" type="button">稍后手动重启</button></div>') +
          '</div>';
        const applyBtn = $('#prefix-apply', applyBox);
        if (applyBtn) applyBtn.addEventListener('click', () => applyNow());
        const later = $('#prefix-apply-later', applyBox);
        if (later) later.addEventListener('click', () => { state.savedNeedApply = false; paintApplyBox(); });
      }

      function paintAll() {
        paintStatus();
        paintAlert();
        paintReadonly();
        paintModes();
        paintPreview();
        paintApplyBox();
        if (saveBtn) saveBtn.disabled = !canWrite() || state.saving || state.applying;
        if (reloadBtn) reloadBtn.disabled = state.saving || state.applying;
        if (input) input.disabled = !canWrite() || state.mode !== 'custom';
      }

      /* ── 数据加载 ─────────────────────────────────────────────────── */
      async function load(manual) {
        if (state.loading) return;
        state.loading = true;
        try {
          const res = await probeApi([PREFIX_PATH], { timeout: 15000 });
          if (!ctx.alive()) return;
          if (res.ok) {
            state.data = normalize(res.data);
            state.pending = false;
            state.error = null;
            state.mode = state.data.mode;
            if (state.data.mode === 'custom') state.customText = state.data.prefix;
          } else {
            state.data = null;
            state.pending = !!res.pending;
            state.error = res.error || new Error('prefix 接口不可用');
            if (manual) toast(state.pending ? '回复前缀接口尚未就绪（404/501）' : '读取回复前缀失败：' + state.error.message, state.pending ? 'warn' : 'bad');
          }
          paintAll();
        } catch (err) {
          if (!ctx.alive()) return;
          state.data = null;
          state.pending = false;
          state.error = err;
          paintAll();
          if (manual) toast('刷新失败：' + (err.message || err), 'bad');
        } finally {
          state.loading = false;
        }
      }

      /* ── 保存 ─────────────────────────────────────────────────────── */
      async function savePrefix() {
        if (!canWrite()) { toast('只读账号无权修改（需要管理员）', 'warn'); return; }
        if (state.saving || state.applying) return;
        const payload = {};
        if (state.mode === 'default') payload.action = 'reset';
        else if (state.mode === 'off') payload.action = 'off';
        else {
          const text = String(state.customText || '').trim();
          if (!text) {
            toast('自定义前缀不能为空，请先输入文本', 'warn', { timeout: 3000 });
            if (input) input.focus();
            return;
          }
          if (text.length > MAX_LEN) {
            toast('自定义前缀最长 ' + MAX_LEN + ' 字', 'warn', { timeout: 3000 });
            return;
          }
          payload.action = 'set';
          payload.text = text;
        }
        state.saving = true;
        paintAll();
        addLog('提交保存：' + (payload.action === 'set' ? '自定义「' + payload.text + '」' : payload.action === 'reset' ? '恢复默认' : '关闭前缀'));
        try {
          const res = await apiPost(PREFIX_PATH, payload, { timeout: 20000 });
          const data = unwrap(res) || {};
          const merged = Object.assign({}, state.data ? state.data.raw : {}, data);
          state.data = normalize(merged);
          state.mode = state.data.mode;
          if (state.data.mode === 'custom') state.customText = state.data.prefix;
          const applied = data.applied === true;
          state.savedNeedApply = !applied;
          state.savedNote = data.note || '';
          if (applied) {
            addLog('保存成功，已生效', 'ok');
            toast('回复前缀已保存并生效', 'ok', { timeout: 2600 });
          } else {
            addLog('保存成功，写入配置；需重启 FM 生效' + (data.note ? '（' + data.note + '）' : ''), 'warn');
            toast('已写入，需重启 FM 生效', 'ok', { timeout: 3600 });
          }
        } catch (err) {
          addLog('保存失败：' + (err.message || err), 'bad');
          toast('保存失败：' + (err.message || err), 'bad', { timeout: 5000 });
        } finally {
          state.saving = false;
          paintAll();
        }
      }

      /* ── 立即生效（重启 FM + 轮询恢复） ─────────────────────────── */
      async function applyNow() {
        if (!canWrite()) { toast('只读账号无权重启（需要管理员）', 'warn'); return; }
        if (state.applying) return;
        state.applying = true;
        state.applyStartedAt = Date.now();
        state.savedNeedApply = true;
        addLog('提交「立即生效」：重启 FM 机器人');
        paintAll();
        try {
          const res = await apiPost(APPLY_PATH, {}, { timeout: 30000 });
          const data = unwrap(res) || {};
          addLog('重启请求已受理' + (data.note ? '：' + data.note : data.request_id ? '：' + data.request_id : ''), 'info');
        } catch (err) {
          state.applying = false;
          addLog('重启提交失败：' + (err.message || err), 'bad');
          toast('重启失败：' + (err.message || err), 'bad', { timeout: 5000 });
          paintAll();
          return;
        }
        let recovered = false;
        let lastNote = '';
        while (Date.now() - state.applyStartedAt < APPLY_LIMIT_MS && ctx.alive()) {
          await sleep(2000);
          if (!ctx.alive()) return;
          const waited = Math.round((Date.now() - state.applyStartedAt) / 1000);
          paintApplyBox();
          const health = await probeApi(['/api/v2/qq/status', '/api/health'], { timeout: 10000 });
          if (health.ok) {
            const hd = health.data || {};
            const botOk = hd.logged_in === true
              || (hd.bot && (hd.bot.connected_to_onebot === true || /run|connected/i.test(String(hd.bot.state || ''))))
              || (hd.snowluma && /run/i.test(String(hd.snowluma.state || '')));
            if (botOk || health.path === '/api/health') { recovered = true; lastNote = health.path; break; }
          }
        }
        state.applying = false;
        const secs = Math.max(1, Math.round((Date.now() - state.applyStartedAt) / 1000));
        if (recovered) {
          state.savedNeedApply = false;
          addLog('已生效：重启完成并恢复（' + secs + 's，探测 ' + lastNote + '）', 'ok');
          toast('回复前缀已生效（重启 FM 用时 ' + secs + 's）', 'ok', { timeout: 4200 });
          const latest = await probeApi([PREFIX_PATH], { timeout: 12000 });
          if (latest.ok && ctx.alive()) {
            state.data = normalize(latest.data);
            state.mode = state.data.mode;
            if (state.data.mode === 'custom') state.customText = state.data.prefix;
          }
        } else {
          addLog((APPLY_LIMIT_MS / 1000) + 's 内未确认恢复，请稍后刷新状态或到运维页查看', 'warn');
          toast('等待超时：可稍后点「刷新状态」确认是否已生效', 'warn', { timeout: 6000 });
        }
        paintAll();
      }

      /* ── 事件绑定 ─────────────────────────────────────────────────── */
      if (modesRoot) {
        modesRoot.addEventListener('click', (event) => {
          const btn = event.target.closest('.prefix-mode');
          if (!btn || !canWrite()) return;
          state.mode = btn.getAttribute('data-mode');
          if (state.mode === 'custom') {
            if (input && !state.customText && state.data && state.data.mode === 'custom') state.customText = state.data.prefix;
            setTimeout(() => { if (input) input.focus(); }, 30);
          }
          paintAll();
        });
      }
      if (input) {
        input.addEventListener('input', () => {
          state.customText = input.value.slice(0, MAX_LEN);
          paintPreview();
        });
      }
      const clearBtn = $('#prefix-clear', root);
      if (clearBtn) clearBtn.addEventListener('click', () => {
        state.customText = '';
        if (input) { input.value = ''; input.focus(); }
        paintPreview();
      });
      if (saveBtn) saveBtn.addEventListener('click', () => savePrefix());
      if (reloadBtn) reloadBtn.addEventListener('click', () => load(true));
      const topRefresh = $('#prefix-refresh', root);
      if (topRefresh) topRefresh.addEventListener('click', () => load(true));
      const jsonBtn = $('#prefix-json', root);
      if (jsonBtn) jsonBtn.addEventListener('click', () => openJsonDrawer('回复前缀原始响应', state.data ? state.data.raw : (state.error ? { error: state.error.message } : {}), PREFIX_PATH));

      /* 首次加载 + 骨架 */
      if (statusRoot) statusRoot.innerHTML = '<div class="sk-row"><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div><div class="sk sk-card"></div></div>';
      paintReadonly();
      paintModes();
      await load(false);

      /* 页面守卫 */
      if (ctx.alive() && !$('#main .card, #main .kpi, #main .empty, #main .error-box')) {
        console.warn('[prefix] page guard: no real element');
      }
    },
  };
})();
