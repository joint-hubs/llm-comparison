/* =============================================================================
 * FOC-1266 — Fenix model dashboard: client-side logic + UI wiring.
 *
 * Pure, deterministic logic (quality, cache-aware cost, ranking, filters,
 * selection bounds, renderers) is defined first and exported for tests
 * (tests/test-dashboard.R runs this file under node). The DOM wiring at the
 * bottom only runs in a browser. No server; the only fetch is the local
 * payload. Routing keys are role-definition provenance ONLY and are never
 * resolved to any model (spec §7).
 *
 * Units: prices and costs are USD per 1M tokens. Percentiles are 0-100.
 * ========================================================================== */

'use strict';

const MAX_COMPARE = 4;
const INVALID_WEIGHTS_MSG = 'set at least one metric weight > 0';
const BANNER_TEXT = 'ranking rests mostly on E5 estimates; treat as indicative.';
const ENVELOPE_LABEL = 'Q envelope (heuristic)';
const CACHE_SUB_FLAG = 'cache-price substituted';
const NA_COST_LABEL = 'n/a';

/* ---- small helpers ------------------------------------------------------ */

function clampWeight(v) {
  const n = Number(v);
  if (!isFinite(n)) return 0;
  return n < 0 ? 0 : n;
}

function esc(s) {
  return String(s).replace(/[&<>"']/g, c => ({
    '&': '&'+'amp;', '<': '&'+'lt;', '>': '&'+'gt;', '"': '&'+'quot;', "'": '&#39;'
  }[c]));
}

function isInvalidWeights(weights) {
  let sum = 0;
  for (const k of Object.keys(weights || {})) sum += Number(weights[k]) || 0;
  return sum <= 0;
}

// accepts both {cacheRead: ...} (UI state) and {cache_read: ...} (registry)
function shareValue(shares, name) {
  if (!shares) return 0;
  if (name === 'cacheRead' && shares.cacheRead !== undefined) return Number(shares.cacheRead) || 0;
  if (name === 'cacheRead' && shares.cache_read !== undefined) return Number(shares.cache_read) || 0;
  if (shares[name] !== undefined) return Number(shares[name]) || 0;
  return 0;
}

function cmpStr(a, b) {
  return a < b ? -1 : a > b ? 1 : 0;   // code-unit order, deterministic
}

function cmpId(a, b) {
  return cmpStr(a.model.model_id, b.model.model_id);
}

/* ---- quality (weighted mean of percentiles) ------------------------------ */

function computeQuality(weights, model) {
  let tw = 0, q = 0, ql = 0, qh = 0, cov = 0;
  const met = (model && model.metrics) || {};
  for (const m of Object.keys(weights || {})) {
    const w = Number(weights[m]) || 0;
    if (w <= 0) continue;
    const cell = met[m];
    if (!cell) continue;
    tw += w; q += w * cell.pct;
    ql += w * cell.pct_low; qh += w * cell.pct_high;
    if (cell.provenance === 'measured') cov += w;
  }
  if (tw <= 0) return null;
  return { q: q / tw, qLow: ql / tw, qHigh: qh / tw, coverage: cov / tw };
}

function isLowCoverage(qobj) {
  return !qobj || qobj.coverage === 0;
}

/* ---- cache-aware cost ---------------------------------------------------- */

function computeCost(shares, prices) {
  const c = Math.max(0, shareValue(shares, 'cacheRead'));
  const i = Math.max(0, shareValue(shares, 'input'));
  const o = Math.max(0, shareValue(shares, 'output'));
  const sum = c + i + o;
  if (sum <= 0) return { cost: null, cacheSubstituted: false };
  const P = prices || {};
  const pin = P.input, pout = P.output;
  // Missing/negative (sentinel -1e6) input or output price -> n/a.
  // DEFENSE-IN-DEPTH: every scored model carries sane prices (the build
  // fails loud otherwise); this guard stays as a labelled branch only.
  if (pin === null || pin === undefined || pin < 0 ||
      pout === null || pout === undefined || pout < 0) {
    return { cost: null, cacheSubstituted: false };
  }
  let pc = P.cache_read;
  let cacheSub = false;
  if (c > 0 && (pc === null || pc === undefined)) {
    pc = pin;                    // conservative: no cache discount assumed
    cacheSub = true;
  }
  const cost = (c * (pc || 0) + i * pin + o * pout) / sum;
  return { cost, cacheSubstituted: cacheSub };
}

/* ---- ranking (deterministic total order) --------------------------------- */

function buildRows(models, weights, shares) {
  if (isInvalidWeights(weights)) return null;
  return models.map(m => {
    const q = computeQuality(weights, m);
    const cst = computeCost(shares, m.prices);
    return {
      model: m, q: q.q, qLow: q.qLow, qHigh: q.qHigh,
      coverage: q.coverage, cost: cst.cost, cacheSubstituted: cst.cacheSubstituted
    };
  });
}

function cmpCost(a, b) {
  if (a.cost === null && b.cost === null) return 0;
  if (a.cost === null) return 1;      // n/a cost always sorts LAST
  if (b.cost === null) return -1;
  return a.cost - b.cost;
}

function sortRows(rows, key) {
  const r = rows.slice();
  const byKey = {
    quality: (a, b) => (b.q - a.q) || cmpCost(a, b) || cmpId(a, b),
    cost:    (a, b) => cmpCost(a, b) || cmpId(a, b),
    coverage:(a, b) => (b.coverage - a.coverage) || cmpId(a, b),
    name:    (a, b) => cmpId(a, b),
    provider:(a, b) => cmpStr(a.model.provider, b.model.provider) || cmpId(a, b)
  };
  return r.sort(byKey[key] || byKey.quality);
}

/* ---- estimate-dominated banner (top N=10 of the active sort) -------------- */

function estimateDominated(sortedRows, N = 10) {
  const top = (sortedRows || []).slice(0, Math.min(N, sortedRows.length));
  if (top.length === 0) return false;
  return top.every(r => r.coverage < 0.50);
}

/* ---- filters -------------------------------------------------------------- */

function emptyFilters() {
  return { providers: [], maxCost: null, minCoverage: null, search: '' };
}

function applyFilters(rows, f) {
  const provs = (f && f.providers) || [];
  const hasProv = provs.length > 0;
  const maxCost = f && f.maxCost !== null && f.maxCost !== undefined && f.maxCost !== ''
    ? Number(f.maxCost) : null;
  const minCov = f && f.minCoverage !== null && f.minCoverage !== undefined && f.minCoverage !== ''
    ? Number(f.minCoverage) : null;
  const needle = f && f.search ? String(f.search).toLowerCase() : '';
  return rows.filter(r => {
    if (hasProv && !provs.includes(r.model.provider)) return false;
    if (maxCost !== null && (r.cost === null || r.cost > maxCost)) return false;
    if (minCov !== null && r.coverage < minCov) return false;
    if (needle) {
      const hay = (r.model.model_id + ' ' + (r.model.name || '')).toLowerCase();
      if (!hay.includes(needle)) return false;
    }
    return true;
  });
}

/* ---- per-role state (registry defaults; edits never leak across roles) ----- */

function newRoleState(payload, roleId) {
  const role = payload.roles.find(r => r.id === roleId);
  if (!role) throw new Error('unknown role: ' + roleId);
  return {
    roleId,
    weights: Object.assign({}, role.weights),
    shares: {
      cacheRead: role.default_shares.cache_read,
      input: role.default_shares.input,
      output: role.default_shares.output
    }
  };
}

function resetWeights(state, payload) {
  const role = payload.roles.find(r => r.id === state.roleId);
  Object.assign(state.weights, role.weights);
  return state;
}

function resetShares(state, payload) {
  const role = payload.roles.find(r => r.id === state.roleId);
  state.shares.cacheRead = role.default_shares.cache_read;
  state.shares.input = role.default_shares.input;
  state.shares.output = role.default_shares.output;
  return state;
}

/* ---- selection (side-by-side, 2..4, exclusive toggles) --------------------- */

function toggleSelection(sel, id) {
  if (sel.has(id)) sel.delete(id); else sel.add(id);
  return sel;
}

function canSelectMore(sel) {
  return sel.size < MAX_COMPARE;
}

/* ---- renderers (pure string builders) -------------------------------------- */

function fmtCost(c) {
  if (c === null || c === undefined) return NA_COST_LABEL;
  return '$' + c.toFixed(3);
}

function fmtPctRange(q) {
  return q.q.toFixed(1) + ' <span class="range">[' + q.qLow.toFixed(1) +
         ' – ' + q.qHigh.toFixed(1) + ']</span>';
}

function renderEmptyState() {
  return '<div class="empty-state">' +
    '<p>Brak modeli spełniających aktywne filtry.</p>' +
    '<button type="button" data-action="clear-filters">Wyczyść filtry</button>' +
    '</div>';
}

function renderComparison(models, role, metrics, metricLabels, sharesIn) {
  if (!models || models.length < 2) return null;
  const shares = sharesIn || role.default_shares;
  const active = (metrics || []).filter(m => (role.weights[m] || 0) > 0);
  const cS = shareValue(shares, 'cacheRead');
  const iS = shareValue(shares, 'input');
  const oS = shareValue(shares, 'output');
  const rows = models.map(m => ({
    model: m,
    q: computeQuality(role.weights, m),
    cost: computeCost(shares, m.prices)
  }));
  const head = '<tr><th scope="row">Model</th>' +
    rows.map(r => '<th scope="col">' + esc(r.model.name) +
      '<br><span class="mono">' + esc(r.model.model_id) + '</span></th>').join('') + '</tr>';
  const qRow = '<tr><th scope="row">' + ENVELOPE_LABEL + '</th>' +
    rows.map(r => '<td class="num">' + fmtPctRange(r.q) + '</td>').join('') + '</tr>';
  const cRow = '<tr><th scope="row">Cache-aware cost (USD/1M tok.)</th>' +
    rows.map(r => '<td class="num">' + fmtCost(r.cost.cost) +
      (r.cost.cacheSubstituted ? ' <span class="flag" title="' + CACHE_SUB_FLAG + '">†</span>' : '') +
      '<br><span class="range">' +
      (100 * cS).toFixed(0) + '% cache / ' + (100 * iS).toFixed(0) +
      '% input / ' + (100 * oS).toFixed(0) + '% output</span></td>').join('') + '</tr>';
  const provRow = '<tr><th scope="row">Dostawca</th>' +
    rows.map(r => '<td>' + esc(r.model.provider) + '</td>').join('') + '</tr>';
  const covRow = '<tr><th scope="row">Measured-weight coverage</th>' +
    rows.map(r => '<td class="num">' + (100 * r.q.coverage).toFixed(0) + '%</td>').join('') + '</tr>';
  const priceRow = '<tr><th scope="row">Ceny (USD/1M tok.)</th>' +
    rows.map(r => '<td class="mono">in ' + (r.model.prices.input ?? '—') +
      ' / out ' + (r.model.prices.output ?? '—') +
      ' / cache ' + (r.model.prices.cache_read ?? '—') + '</td>').join('') + '</tr>';
  const metricRows = active.map(m => {
    const label = (metricLabels && metricLabels[m]) || m;
    return '<tr><th scope="row">' + esc(label) + ' <span class="mono">(' + esc(m) + ')</span></th>' +
      rows.map(r => {
        const cell = r.model.metrics[m];
        const badge = cell.provenance === 'measured'
          ? '<span class="badge badge-measured">measured</span>'
          : '<span class="badge badge-estimated">estimated</span>';
        return '<td class="num">' + cell.pct.toFixed(1) +
          (cell.provenance === 'measured' ? '' :
            ' <span class="range">[' + cell.pct_low.toFixed(1) + ' – ' + cell.pct_high.toFixed(1) + ']</span>') +
          ' ' + badge + '</td>';
      }).join('') + '</tr>';
  }).join('');
  return '<h3>Porównanie modeli (' + rows.length + ' z ' + MAX_COMPARE + ')</h3>' +
    '<div class="table-wrap"><table class="data-table cmp-table">' +
    '<tbody>' + head + qRow + cRow + provRow + covRow + priceRow + metricRows + '</tbody>' +
    '</table></div>' +
    '<p class="hint">' + esc(ENVELOPE_LABEL) + ' — heurystyka, nie przedział ufności. ' +
    '† = ' + CACHE_SUB_FLAG + '.</p>';
}

function renderUnscored(unscored, meta) {
  const total = unscored.length;
  const noPrice = meta.n_unscored_no_price;
  const note = '<p>Modele bez danych benchmarkowych: <strong>' + total +
    '</strong> — wykluczone z rankingu, ponieważ <strong>nie mają danych benchmarkowych</strong>' +
    ' (nie z powodu cen). Bez danych cenowych: <strong>' + noPrice + '</strong>.</p>';
  const rows = unscored.map(u => {
    const ok = u.price_status === 'available';
    const cells = ok
      ? '<td class="mono num">' + (u.prices.input ?? '—') + '</td>' +
        '<td class="mono num">' + (u.prices.output ?? '—') + '</td>' +
        '<td class="mono num">' + (u.prices.cache_read ?? '—') + '</td>' +
        '<td>dostępne' + (u.prices.input === 0 || u.prices.output === 0 ? ' (0 = darmowy)' : '') + '</td>'
      : '<td class="mono">—</td><td class="mono">—</td><td class="mono">—</td>' +
        '<td><span class="badge badge-na">no price data</span></td>';
    return '<tr><td>' + esc(u.name) + '<br><span class="mono">' + esc(u.model_id) +
      '</span></td>' + cells + '</tr>';
  }).join('');
  return '<h3>Modele bez danych benchmarkowych (' + total + ')</h3>' + note +
    '<div class="table-wrap"><table class="data-table unscored-table">' +
    '<thead><tr><th>Model</th><th>Input USD/1M</th><th>Output USD/1M</th>' +
    '<th>Cache USD/1M</th><th>price_status</th></tr></thead>' +
    '<tbody>' + rows + '</tbody></table></div>';
}

/* ---- exports (node tests) --------------------------------------------------- */

const Dash = {
  MAX_COMPARE, INVALID_WEIGHTS_MSG, BANNER_TEXT, ENVELOPE_LABEL,
  clampWeight, isInvalidWeights,
  computeQuality, isLowCoverage,
  computeCost, buildRows, sortRows, estimateDominated,
  emptyFilters, applyFilters,
  newRoleState, resetWeights, resetShares,
  toggleSelection, canSelectMore,
  renderEmptyState, renderComparison, renderUnscored
};

if (typeof module !== 'undefined' && module.exports) {
  module.exports = Dash;
}

/* =============================================================================
 * Browser wiring — never runs under node.
 * ========================================================================== */

if (typeof window !== 'undefined' && typeof document !== 'undefined') {
  window.Dash = Dash;

  const el = id => document.getElementById(id);

  function numberInput(id, val, label, step) {
    return '<input type="number" id="' + id + '" value="' + val + '" min="0" ' +
      'step="' + (step || '5') + '" aria-label="' + esc(label) + '">';
  }

  function boot(payload) {
    const state = {
      payload,
      roleId: payload.roles[0].id,
      weights: null, shares: null,
      sortKey: 'quality',
      filters: Dash.emptyFilters(),
      selection: new Set(),
      showBaseline: false
    };
    const roleOf = id => payload.roles.find(r => r.id === id);

    function loadRoleDefaults() {
      const st = Dash.newRoleState(payload, state.roleId);
      state.weights = st.weights;
      state.shares = st.shares;
    }

    function activeRows() {
      const built = Dash.buildRows(payload.models, state.weights, state.shares);
      if (!built) return [];
      return Dash.sortRows(Dash.applyFilters(built, state.filters), state.sortKey);
    }

    /* ---- role panel ------------------------------------------------------ */
    function renderRolePanel() {
      const role = roleOf(state.roleId);
      const options = payload.roles.map(r =>
        '<option value="' + esc(r.id) + '"' + (r.id === state.roleId ? ' selected' : '') + '>' +
        esc(r.squad) + ' · ' + esc(r.id) + '</option>').join('');
      const wInputs = payload.meta.metrics.map(m => {
        const lbl = payload.meta.metric_labels[m] || m;
        return '<label class="w-cell" title="' + esc(lbl) + '"><span class="mono">' +
          esc(m) + '</span>' + numberInput('w-' + m, state.weights[m], 'waga ' + m + ' (' + lbl + ')', 5) +
          '</label>';
      }).join('');
      el('role-panel').innerHTML =
        '<label for="role-select" class="block-label">Rola (model-routed squad.role)</label>' +
        '<select id="role-select" aria-label="Wybór roli">' + options + '</select>' +
        '<p class="responsibility"><strong>Odpowiedzialność:</strong> ' + esc(role.responsibility) + '</p>' +
        '<p class="rationale"><strong>Dlaczego te wagi:</strong> ' + esc(role.weight_rationale) + '</p>' +
        '<p class="rationale"><strong>Uzasadnienie udziałów:</strong> ' + esc(role.share_rationale) + '</p>' +
        '<p class="rationale mono">routing keys (provenance of the role definition only): ' +
        (role.routing_keys.length ? esc(role.routing_keys.join(', ')) : '— (session model)') + '</p>' +
        '<fieldset><legend>Wagi metryk (0–30, skala 5-pkt; w rejestrze suma = 100)</legend>' +
        '<div class="w-grid">' + wInputs + '</div>' +
        '<p class="running-sum">Suma wag: <strong id="w-sum"></strong></p>' +
        '<button type="button" data-action="reset-weights">Reset wag (rejestr)</button></fieldset>' +
        '<fieldset><legend>Default token shares (cache-read / uncached input / output)</legend>' +
        '<div class="s-grid">' +
        '<label class="w-cell"><span class="mono">cacheRead</span>' + numberInput('sh-c', state.shares.cacheRead, 'udział cache-read', 0.05) + '</label>' +
        '<label class="w-cell"><span class="mono">input</span>' + numberInput('sh-i', state.shares.input, 'udział uncached input', 0.05) + '</label>' +
        '<label class="w-cell"><span class="mono">output</span>' + numberInput('sh-o', state.shares.output, 'udział output', 0.05) + '</label>' +
        '</div>' +
        '<p class="running-sum">Suma udziałów: <strong id="sh-sum"></strong></p>' +
        '<button type="button" data-action="reset-shares">Reset udziałów (rejestr)</button></fieldset>';
      updateSums();
      el('role-select').addEventListener('change', ev => {
        state.roleId = ev.target.value;
        loadRoleDefaults();
        renderAll();
      });
      payload.meta.metrics.forEach(m => {
        el('w-' + m).addEventListener('input', ev => {
          state.weights[m] = Dash.clampWeight(Number(ev.target.value) || 0);
          updateSums();
          renderData();          // keep focus in the weight input
        });
      });
      [['sh-c', 'cacheRead'], ['sh-i', 'input'], ['sh-o', 'output']].forEach(pair => {
        el(pair[0]).addEventListener('input', ev => {
          const v = Number(ev.target.value);
          state.shares[pair[1]] = isFinite(v) && v >= 0 ? v : 0;
          updateSums();
          renderData();
        });
      });
      el('role-panel').addEventListener('click', ev => {
        const t = ev.target;
        if (t && t.getAttribute) {
          const a = t.getAttribute('data-action');
          if (a === 'reset-weights') { Dash.resetWeights(state, payload); renderAll(); }
          if (a === 'reset-shares') { Dash.resetShares(state, payload); renderAll(); }
        }
      });
    }

    function updateSums() {
      const wSum = Object.values(state.weights).reduce((a, b) => a + (Number(b) || 0), 0);
      const sSum = (Number(state.shares.cacheRead) || 0) + (Number(state.shares.input) || 0) +
                   (Number(state.shares.output) || 0);
      const wEl = el('w-sum');
      if (wEl) wEl.innerHTML = wSum + (wSum !== 100 ? ' <span class="warn">— rejestr: 100; ranking normalizuje przez Σw</span>' : '');
      const sEl = el('sh-sum');
      if (sEl) sEl.innerHTML = sSum.toFixed(2) +
        (Math.abs(sSum - 1) > 1e-9 ? ' <span class="warn">— różni się od 1; obliczenia normalizowane</span>' : '');
    }

    /* ---- banner ------------------------------------------------------------ */
    function renderBanner(rows) {
      const show = Dash.estimateDominated(rows);
      el('banner').hidden = !show;
      if (show) el('banner').textContent = Dash.BANNER_TEXT;
    }

    /* ---- filters ------------------------------------------------------------ */
    function renderFilters() {
      const providers = [...new Set(payload.models.map(m => m.provider))].sort();
      const boxes = providers.map(p =>
        '<label class="prov"><input type="checkbox" value="' + esc(p) + '"' +
        (state.filters.providers.includes(p) ? ' checked' : '') + '> ' + esc(p) + '</label>').join('');
      el('filters').innerHTML =
        '<div class="f-row"><span class="block-label">Dostawcy:</span><div class="prov-grid">' +
        boxes + '</div></div>' +
        '<div class="f-row">' +
        '<label class="block-label" for="f-cost">Max koszt USD/1M:</label>' +
        '<input type="number" id="f-cost" min="0" step="0.1" value="' +
        (state.filters.maxCost === null ? '' : state.filters.maxCost) + '">' +
        '<label class="block-label" for="f-cov">Min coverage:</label>' +
        '<input type="number" id="f-cov" min="0" max="1" step="0.1" value="' +
        (state.filters.minCoverage === null ? '' : state.filters.minCoverage) + '">' +
        '<label class="block-label" for="f-search">Szukaj:</label>' +
        '<input type="search" id="f-search" value="' + esc(state.filters.search) + '">' +
        '<button type="button" data-action="clear-filters">Wyczyść filtry</button>' +
        '<label class="prov"><input type="checkbox" id="f-baseline"' +
        (state.showBaseline ? ' checked' : '') + '> kolumna blend_cost (0.75/0.25) — opcjonalne porównanie</label>' +
        '</div>';
      el('filters').addEventListener('change', ev => {
        const t = ev.target;
        if (t.type === 'checkbox' && t.value) {          // a provider box
          state.filters.providers = [...el('filters').querySelectorAll('.prov input[type=checkbox]:checked')]
            .filter(i => i.value).map(i => i.value);
          renderData();
        }
        if (t.id === 'f-baseline') {
          state.showBaseline = t.checked;
          renderData();
        }
      });
      el('filters').addEventListener('input', ev => {
        const t = ev.target;
        if (t.id === 'f-cost') {
          state.filters.maxCost = t.value === '' ? null : Number(t.value);
          renderData();
        } else if (t.id === 'f-cov') {
          state.filters.minCoverage = t.value === '' ? null : Number(t.value);
          renderData();
        } else if (t.id === 'f-search') {
          state.filters.search = t.value;
          renderData();
        }
      });
      el('filters').addEventListener('click', ev => {
        const t = ev.target;
        if (t && t.getAttribute && t.getAttribute('data-action') === 'clear-filters') {
          state.filters = Dash.emptyFilters();
          renderAll();
        }
      });
    }

    /* ---- ranked table -------------------------------------------------------- */
    function renderTable(rows) {
      const wrap = el('ranking');
      if (Dash.isInvalidWeights(state.weights)) {
        wrap.innerHTML = '<p class="warn">' + Dash.INVALID_WEIGHTS_MSG + '</p>';
        return;
      }
      if (rows.length === 0) {
        wrap.innerHTML = Dash.renderEmptyState();
        wrap.querySelector('[data-action="clear-filters"]')
          .addEventListener('click', () => { state.filters = Dash.emptyFilters(); renderAll(); });
        return;
      }
      const sortBtn = (key, label, dir) =>
        '<button type="button" class="sort-btn" data-sort="' + key + '">' + label +
        (state.sortKey === key ? ' <span class="mono">' + dir + '</span>' : '') + '</button>';
      const head = '<tr><th scope="col">' + sortBtn('name', 'Model', '↑') + '</th>' +
        '<th scope="col">' + sortBtn('provider', 'Dostawca', '↑') + '</th>' +
        '<th scope="col">' + sortBtn('quality', ENVELOPE_LABEL, '↓') + '</th>' +
        '<th scope="col">' + sortBtn('cost', 'Koszt cache-aware (USD/1M)', '↑') + '</th>' +
        '<th scope="col">' + sortBtn('coverage', 'Coverage (pomiary)', '↓') + '</th>' +
        '<th scope="col">Provenance</th>' + (state.showBaseline ? '<th scope="col">blend_cost 0.75/0.25</th>' : '') +
        '<th scope="col">Porównanie</th></tr>';
      const body = rows.map(r => {
        const id = r.model.model_id;
        const selected = state.selection.has(id);
        const dis = !selected && !Dash.canSelectMore(state.selection);
        const nMeas = payload.meta.metrics
          .filter(m => r.model.metrics[m].provenance === 'measured').length;
        return '<tr>' +
          '<td>' + esc(r.model.name) + '<br><span class="mono">' + esc(id) + '</span></td>' +
          '<td>' + esc(r.model.provider) + '</td>' +
          '<td class="num">' + fmtPctRange(r) + '</td>' +
          '<td class="num">' + fmtCost(r.cost) +
          (r.cacheSubstituted ? ' <span class="flag" title="' + CACHE_SUB_FLAG + '">†</span>' : '') + '</td>' +
          '<td class="num">' + (100 * r.coverage).toFixed(0) + '%' +
          (r.coverage === 0 ? ' <span class="flag" title="brak pomiarów dla metryk roli">○</span>' : '') + '</td>' +
          '<td><span class="badge badge-' + (nMeas >= 7 ? 'measured' : 'estimated') + '">' +
          nMeas + '/14 pomiar</span></td>' +
          (state.showBaseline ? '<td class="num">$' + Number(r.model.blend_cost).toFixed(3) + '</td>' : '') +
          '<td><button type="button" class="cmp-btn" data-cmp="' + esc(id) + '"' +
          (dis ? ' disabled' : '') + (selected ? ' data-on="1"' : '') + '>' +
          (selected ? '✓ w porównaniu' : 'Porównaj') + '</button></td>' +
          '</tr>';
      }).join('');
      wrap.innerHTML = '<p class="hint">Wyników: ' + rows.length + ' z ' + payload.models.length +
        ' modeli ze scoringiem. Ranking = jawne sortowanie po widocznej kolumnie. ' +
        '† = ' + CACHE_SUB_FLAG + '. ○ = brak pomiarów dla metryk roli. ' +
        'Koszt = c·P_cacheRead + i·P_input + o·P_output (USD/1M tok.).</p>' +
        '<div class="table-wrap"><table class="data-table rank-table"><thead>' + head +
        '</thead><tbody>' + body + '</tbody></table></div>' +
        (state.selection.size >= MAX_COMPARE ?
          '<p class="warn">Maksymalnie ' + MAX_COMPARE + ' modele w porównaniu — odznacz jeden, aby dodać kolejny.</p>' : '');
      wrap.querySelectorAll('.sort-btn').forEach(b =>
        b.addEventListener('click', () => { state.sortKey = b.getAttribute('data-sort'); renderData(); }));
      wrap.querySelectorAll('.cmp-btn').forEach(b =>
        b.addEventListener('click', () => {
          Dash.toggleSelection(state.selection, b.getAttribute('data-cmp'));
          renderData();
        }));
    }

    /* ---- comparison section --------------------------------------------------- */
    function renderComparisonSec() {
      const models = payload.models.filter(m => state.selection.has(m.model_id));
      const host = el('comparison');
      if (models.length < 2) {
        host.innerHTML = models.length === 1
          ? '<p class="hint">Wybierz drugi model, aby porównać (2–' + MAX_COMPARE + ').</p>'
          : '';
        return;
      }
      host.innerHTML = Dash.renderComparison(models, roleOf(state.roleId),
        payload.meta.metrics, payload.meta.metric_labels, state.shares);
    }

    function renderUnscoredSec() {
      el('unscored').innerHTML = Dash.renderUnscored(payload.unscored, payload.meta);
    }

    /* ---- render orchestration ---------------------------------------------------
     * renderData(): everything that depends on weights/shares/filters/sort —
     * keeps the role panel and filter inputs (and their focus) untouched.
     * renderAll(): full re-render (role switch, resets, cleared filters). */
    function renderData() {
      const rows = activeRows();
      renderBanner(rows);
      renderTable(rows);
      renderComparisonSec();
    }

    function renderAll() {
      renderRolePanel();
      renderFilters();
      renderData();
      renderUnscoredSec();
      el('data-as-of').textContent = 'Dane z dnia ' + payload.meta.generated_utc +
        ' · źródło: ' + payload.meta.source_rds +
        ' · modeli ze scoringiem: ' + payload.meta.n_models +
        ' / bez benchmarków: ' + payload.meta.n_unscored;
    }

    loadRoleDefaults();
    renderAll();
  }

  document.addEventListener('DOMContentLoaded', () => {
    fetch('data/model-dashboard.json')
      .then(r => { if (!r.ok) throw new Error('HTTP ' + r.status); return r.json(); })
      .then(p => boot(p))
      .catch(e => {
        const n = el('js-error');
        if (n) {
          n.hidden = false;
          n.textContent = 'Nie udało się wczytać data/model-dashboard.json (' + e.message + '). ' +
            'Otwórz stronę przez serwer HTTP (np. python -m http.server w katalogu docs/) ' +
            'lub na GitHub Pages.';
        }
      });
  });
}
