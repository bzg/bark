// bark-index.js — Client-side filtering, sorting, URL state, theme toggle.
// Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
// SPDX-License-Identifier: MPL-2.0
//
// Expects a global `barkConfig` object with:
//   .types    — array of report type strings

var allTypes = barkConfig.types;
var activeTypes = {};
allTypes.forEach(function(t) { activeTypes[t] = true; });
var onlyOpen    = true;
var onlyAcked   = false;
var onlyOwned   = false;

function getSearchInput() { return document.getElementById('si'); }

function setSearch(val) {
  getSearchInput().value = val;
  filterRows();
  pushURL();
}

function resetFilters() {
  onlyOpen = false;
  onlyAcked = false;
  onlyOwned = false;
  allTypes.forEach(function(t) { activeTypes[t] = true; });
  document.getElementById('btn-open').classList.add('outline');
  document.getElementById('btn-acked').classList.add('outline');
  document.getElementById('btn-owned').classList.add('outline');
  document.querySelectorAll('.filters button[data-type]').forEach(function(btn) {
    btn.classList.remove('outline');
  });
}

function showRelated(val) {
  resetFilters();
  setSearch(val);
}

function localDate(d) {
  var y = d.getFullYear();
  var m = String(d.getMonth() + 1).padStart(2, '0');
  var day = String(d.getDate()).padStart(2, '0');
  return y + '-' + m + '-' + day;
}

function resolveDate(s) {
  if (!s) return '';
  var m = s.match(/^(\d+)d$/);
  if (m) {
    var d = new Date();
    d.setDate(d.getDate() - parseInt(m[1]));
    return localDate(d);
  }
  if (/^\d{4}-\d{2}-\d{2}$/.test(s)) return s;
  return '';
}

/* Wildcard-aware field match: '*' matches any non-empty value */
function matchField(fieldVal, terms) {
  if (terms.length === 1 && terms[0] === '*') return fieldVal !== '';
  return terms.some(function(t) { return fieldVal.indexOf(t) !== -1; });
}

/* Extract the value after the first ':' in a search token */
function extractValue(part, lowered) {
  return part.substring(lowered.indexOf(':') + 1);
}

/* Check whether lowered starts with any of the given prefixes */
function startsWithAny(lowered, prefixes) {
  return prefixes.some(function(pfx) { return lowered.indexOf(pfx) === 0; });
}

/*
 * Field map: each entry maps search prefixes to a result key.
 * parseClause uses this to turn "from:alice" into result.froms = ["alice"].
 * matchClause uses the dataAttr to look up the corresponding data-* attribute.
 */
var fieldMap = [
  {prefixes: ['message-id:', 'mid:', 'm:'], key: 'mids',      dataAttr: 'mid'},
  {prefixes: ['from:', 'f:'],               key: 'froms',     dataAttr: 'from'},
  {prefixes: ['subject:', 's:'],            key: 'subjects',  dataAttr: 'subject'},
  {prefixes: ['topic:', 't:'],              key: 'topics',    dataAttr: 'topic'},
  {prefixes: ['acked:', 'a:'],              key: 'acked',     dataAttr: 'acked'},
  {prefixes: ['owned:', 'o:'],              key: 'owned',     dataAttr: 'owned'},
  {prefixes: ['closed:', 'c:'],             key: 'closed',    dataAttr: 'closedby'},
  {prefixes: ['urgent:', 'u:'],             key: 'urgent',    dataAttr: 'urgent'},
  {prefixes: ['important:', 'i:'],          key: 'important', dataAttr: 'important'}
];

/* Parse a single AND-clause (no | in it) */
function parseClause(q) {
  var result = { text: '', mids: [], froms: [], subjects: [],
                 acked: [], owned: [], closed: [], topics: [],
                 urgent: [], important: [],
                 dateFrom: '', dateTo: '', minPriority: null };
  var parts = q.trim().split(/\s+/).filter(Boolean);
  for (var i = 0; i < parts.length; i++) {
    var p  = parts[i];
    var lp = p.toLowerCase();
    // Try each field mapping
    var matched = false;
    for (var j = 0; j < fieldMap.length; j++) {
      if (startsWithAny(lp, fieldMap[j].prefixes)) {
        result[fieldMap[j].key] = extractValue(p, lp).toLowerCase().split(',').filter(Boolean);
        matched = true;
        break;
      }
    }
    if (matched) continue;
    // Special cases: priority and date (not simple field lookups)
    if (lp.indexOf('priority:') === 0 || lp.indexOf('p:') === 0) {
      var n = parseInt(extractValue(p, lp), 10);
      if (!isNaN(n)) result.minPriority = n;
    } else if (lp.indexOf('date:') === 0 || lp.indexOf('d:') === 0) {
      var pfxLen = lp.indexOf('date:') === 0 ? 5 : 2;
      var range = p.substring(pfxLen).split('..');
      result.dateFrom = resolveDate(range[0] || '');
      result.dateTo   = resolveDate(range[1] || '') || localDate(new Date());
    } else {
      result.text += (result.text ? ' ' : '') + p;
    }
  }
  return result;
}

function matchClause(tr, q) {
  var d = tr.dataset;

  if (!activeTypes[d.type]) return false;
  if (onlyOpen && d.closed === 'true') return false;
  if (onlyAcked  && d.acked  === '')     return false;
  if (onlyOwned  && d.owned  === '')     return false;

  // Check all field-mapped filters
  for (var j = 0; j < fieldMap.length; j++) {
    var f = fieldMap[j];
    if (q[f.key].length > 0) {
      var val = (d[f.dataAttr] || '').toLowerCase();
      if (!matchField(val, q[f.key])) return false;
    }
  }

  if (q.minPriority !== null) {
    if (parseInt(d.priority || '0', 10) < q.minPriority) return false;
  }
  if (q.dateFrom && d.date < q.dateFrom) return false;
  if (q.dateTo   && d.date > q.dateTo)   return false;
  if (q.text && d.search.indexOf(q.text.toLowerCase()) === -1) return false;

  return true;
}

/* Split on ' | ' (pipe with surrounding spaces), evaluate OR of clauses */
function matchRow(tr, raw) {
  var clauses = raw.split(/\s*\|\s*/);
  return clauses.some(function(c) { return matchClause(tr, parseClause(c)); });
}

/* ── Pure display: no side effects ─────────────────────────── */

function restripe() {
  var i = 0;
  document.querySelectorAll('tbody tr').forEach(function(tr) {
    if (!tr.classList.contains('hidden')) {
      tr.classList.toggle('stripe', i++ % 2 === 1);
    } else {
      tr.classList.remove('stripe');
    }
  });
}

function filterRows() {
  var raw  = getSearchInput().value;
  var rows = document.querySelectorAll('tbody tr');
  var visible = 0;
  rows.forEach(function(tr) {
    var show = matchRow(tr, raw);
    tr.classList.toggle('hidden', !show);
    if (show) visible++;
  });
  document.getElementById('status').textContent = visible + '/' + rows.length + ' reports';
  restripe();
}

/* ── URL ↔ state (no history decision here) ───────────────── */

function buildURL() {
  var params = new URLSearchParams();
  var q = getSearchInput().value;
  if (q) params.set('q', q);
  var active = allTypes.filter(function(t) { return activeTypes[t]; });
  if (active.length !== allTypes.length) params.set('types', active.join(','));
  if (!onlyOpen)   params.set('open', '0');
  if (onlyAcked)   params.set('acked', '1');
  if (onlyOwned)   params.set('owned', '1');
  var sortKeys = Object.keys(sortState);
  if (sortKeys.length > 0) {
    params.set('sort', sortKeys[0]);
    params.set('dir', sortState[sortKeys[0]]);
  }
  var qs = params.toString();
  return location.pathname + (qs ? '?' + qs : '');
}

function pushURL()    { history.pushState(null, '', buildURL()); }
function replaceURL() { history.replaceState(null, '', buildURL()); }

/* ── Button handlers: mutate → display → one pushState ────── */

function toggleType(type, btn) {
  activeTypes[type] = !activeTypes[type];
  btn.classList.toggle('outline');
  filterRows();
  pushURL();
}

function syncToolbarButtons() {
  document.querySelectorAll('.filters button[data-type]').forEach(function(btn) {
    btn.classList.toggle('outline', !activeTypes[btn.getAttribute('data-type')]);
  });
}

function isolateType(type) {
  var active = allTypes.filter(function(t) { return activeTypes[t]; });
  if (active.length === 1 && active[0] === type) {
    allTypes.forEach(function(t) { activeTypes[t] = true; });
  } else {
    allTypes.forEach(function(t) { activeTypes[t] = (t === type); });
  }
  syncToolbarButtons();
  filterRows();
  pushURL();
}

function toggleAcked(btn) {
  onlyAcked = !onlyAcked;
  btn.classList.toggle('outline');
  filterRows();
  pushURL();
}

function toggleOwned(btn) {
  onlyOwned = !onlyOwned;
  btn.classList.toggle('outline');
  filterRows();
  pushURL();
}

function toggleOpen(btn) {
  onlyOpen = !onlyOpen;
  btn.classList.toggle('outline');
  filterRows();
  pushURL();
}

/* ── Search input: no history push, just replaceState ─────── */

function onSearchInput() {
  filterRows();
  replaceURL();
}

/* ── Sort: pure DOM reorder + one pushState when user-initiated */

var sortState = {};

function doSort(colIdx, key, dir) {
  var tbody = document.querySelector('tbody');
  var rows  = Array.from(tbody.querySelectorAll('tr'));
  document.querySelectorAll('th[data-sort]').forEach(function(th) {
    th.classList.remove('asc', 'desc');
  });
  document.querySelector('th[data-sort="' + key + '"]').classList.add(dir);
  var isDate = /^\d{4}-\d{2}-\d{2}$/;
  rows.sort(function(a, b) {
    var ac = a.children[colIdx], bc = b.children[colIdx];
    var av = (ac.getAttribute('data-value') || ac.textContent).trim().toLowerCase();
    var bv = (bc.getAttribute('data-value') || bc.textContent).trim().toLowerCase();
    if (isDate.test(av) && isDate.test(bv))
      return dir === 'asc' ? av.localeCompare(bv) : bv.localeCompare(av);
    var an = parseFloat(av), bn = parseFloat(bv);
    var aNaN = isNaN(an) || av === '', bNaN = isNaN(bn) || bv === '';
    if (aNaN !== bNaN)
      // Push empty/NaN values to the end regardless of sort direction
      return aNaN ? 1 : -1;
    if (!aNaN && !bNaN) return dir === 'asc' ? an - bn : bn - an;
    return dir === 'asc' ? av.localeCompare(bv) : bv.localeCompare(av);
  });
  rows.forEach(function(r) { tbody.appendChild(r); });
  restripe();
}

/* Called from onclick on <th> — user-initiated, pushes history */
function sortTable(colIdx, key) {
  var dir = sortState[key] === 'asc' ? 'desc' : 'asc';
  sortState = {};
  sortState[key] = dir;
  doSort(colIdx, key, dir);
  pushURL();
}

/* ── Restore from URL: reads URL → sets state → displays.     */
/*    Touches NO history (the URL is already correct).          */

function restoreFromURL() {
  var params = new URLSearchParams(location.search);

  getSearchInput().value = params.get('q') || '';

  if (params.has('types')) {
    var allowed = params.get('types').split(',');
    allTypes.forEach(function(t) { activeTypes[t] = allowed.indexOf(t) !== -1; });
  } else {
    allTypes.forEach(function(t) { activeTypes[t] = true; });
  }
  document.querySelectorAll('.filters button[data-type]').forEach(function(btn) {
    btn.classList.toggle('outline', !activeTypes[btn.dataset.type]);
  });

  onlyOpen = params.get('open') !== '0';
  document.getElementById('btn-open').classList.toggle('outline', !onlyOpen);

  onlyAcked = params.get('acked') === '1';
  document.getElementById('btn-acked').classList.toggle('outline', !onlyAcked);

  onlyOwned = params.get('owned') === '1';
  document.getElementById('btn-owned').classList.toggle('outline', !onlyOwned);

  document.querySelectorAll('th[data-sort]').forEach(function(th) {
    th.classList.remove('asc', 'desc');
  });
  sortState = {};
  if (params.has('sort') && params.has('dir')) {
    var key = params.get('sort');
    var dir = params.get('dir');
    var th  = document.querySelector('th[data-sort="' + key + '"]');
    if (th && (dir === 'asc' || dir === 'desc')) {
      sortState[key] = dir;
      doSort(Array.from(th.parentNode.children).indexOf(th), key, dir);
    }
  }

  filterRows();
}

restoreFromURL();

window.addEventListener('popstate', function() { restoreFromURL(); });

/* Compute "Due" column: convert deadline dates to days-until-deadline */
(function() {
  var today = new Date();
  today.setHours(0,0,0,0);
  var todayMs = today.getTime();
  var msPerDay = 86400000;
  document.querySelectorAll('.due-cell').forEach(function(td) {
    var dl = td.getAttribute('data-value');
    if (!dl) return;
    var parts = dl.split('-');
    if (parts.length !== 3) return;
    var deadlineMs = new Date(+parts[0], +parts[1]-1, +parts[2]).getTime();
    var days = Math.round((deadlineMs - todayMs) / msPerDay);
    td.setAttribute('data-value', String(days));
    td.textContent = 'In ' + days + ' d.';
    td.title = dl;
    td.style.textAlign = 'center';
    if (days < 0) td.style.color = 'var(--pico-del-color, #c0392b)';
    else if (days <= 3) td.style.color = 'var(--pico-ins-color, #b8860b)';
  });
})();

