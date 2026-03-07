// bark-index.js — Client-side filtering, sorting, URL state, theme toggle.
//
// Expects a global `barkConfig` object with:
//   .types    — array of report type strings
//   .allOpen  — boolean, true if pre-filtered to open reports only

var allTypes = barkConfig.types;
var activeTypes = {};
allTypes.forEach(function(t) { activeTypes[t] = true; });
var showClosed  = false;
var onlyAcked   = false;
var onlyOwned   = false;

function getSearchInput() { return document.getElementById('si'); }

function setSearch(val) {
  getSearchInput().value = val;
  filterRows();
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

/* Parse a single AND-clause (no | in it) */
function parseClause(q) {
  var result = { text: '', mids: [], froms: [], subjects: [],
                 ackedBy: [], ownedBy: [], closedBy: [],
                 dateFrom: '', dateTo: '', minPriority: null };
  var parts = q.trim().split(/\s+/).filter(Boolean);
  for (var i = 0; i < parts.length; i++) {
    var p  = parts[i];
    var lp = p.toLowerCase();
    if (lp.indexOf('message-id:') === 0 || lp.indexOf('m:') === 0) {
      var v = p.substring(lp.indexOf(':') + 1);
      result.mids = v.toLowerCase().split(',').filter(Boolean);
    } else if (lp.indexOf('from:') === 0 || lp.indexOf('f:') === 0) {
      var v = p.substring(lp.indexOf(':') + 1);
      result.froms = v.toLowerCase().split(',').filter(Boolean);
    } else if (lp.indexOf('s:') === 0) {
      result.subjects = p.substring(2).toLowerCase().split(',').filter(Boolean);
    } else if (lp.indexOf('acked:') === 0 || lp.indexOf('a:') === 0) {
      var v = p.substring(lp.indexOf(':') + 1);
      result.ackedBy = v.toLowerCase().split(',').filter(Boolean);
    } else if (lp.indexOf('owned:') === 0 || lp.indexOf('o:') === 0) {
      var v = p.substring(lp.indexOf(':') + 1);
      result.ownedBy = v.toLowerCase().split(',').filter(Boolean);
    } else if (lp.indexOf('closed-by:') === 0 || lp.indexOf('c:') === 0) {
      var v = p.substring(lp.indexOf(':') + 1);
      result.closedBy = v.toLowerCase().split(',').filter(Boolean);
    } else if (lp.indexOf('priority:') === 0 || lp.indexOf('p:') === 0) {
      var v = p.substring(lp.indexOf(':') + 1);
      var n = parseInt(v, 10);
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
  if (showClosed && d.closed !== 'true') return false;
  if (!showClosed && d.closed === 'true') return false;
  if (onlyAcked  && d.acked  === '')     return false;
  if (onlyOwned  && d.owned  === '')     return false;

  if (q.mids.length     > 0 && !matchField((d.mid || '').toLowerCase(), q.mids))     return false;
  if (q.froms.length    > 0 && !matchField(d.from,                      q.froms))    return false;
  if (q.subjects.length > 0 && !matchField(d.subject,                   q.subjects)) return false;
  if (q.ackedBy.length  > 0 && !matchField(d.acked,                     q.ackedBy))  return false;
  if (q.ownedBy.length  > 0 && !matchField(d.owned,                     q.ownedBy))  return false;
  if (q.closedBy.length > 0 && !matchField(d.closedby,                  q.closedBy)) return false;

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
  updateURL();
}

function toggleType(type, btn) {
  activeTypes[type] = !activeTypes[type];
  btn.classList.toggle('outline');
  filterRows();
}

function toggleAcked(btn) {
  onlyAcked = !onlyAcked;
  btn.classList.toggle('outline');
  filterRows();
}

function toggleOwned(btn) {
  onlyOwned = !onlyOwned;
  btn.classList.toggle('outline');
  filterRows();
}

function toggleClosed() {
  showClosed = document.getElementById('show-closed').checked;
  filterRows();
}

function updateURL() {
  var params = new URLSearchParams();
  var q = getSearchInput().value;
  if (q) params.set('q', q);
  var active = allTypes.filter(function(t) { return activeTypes[t]; });
  if (active.length !== allTypes.length) params.set('types', active.join(','));
  if (showClosed)  params.set('closed', '1');
  if (onlyAcked)   params.set('acked', '1');
  if (onlyOwned)   params.set('owned', '1');
  var qs = params.toString();
  history.replaceState(null, '', location.pathname + (qs ? '?' + qs : ''));
}

function restoreFromURL() {
  var params = new URLSearchParams(location.search);
  if (params.has('q')) getSearchInput().value = params.get('q');
  if (params.has('types')) {
    var allowed = params.get('types').split(',');
    allTypes.forEach(function(t) { activeTypes[t] = allowed.indexOf(t) !== -1; });
    document.querySelectorAll('.filters button[data-type]').forEach(function(btn) {
      btn.classList.toggle('outline', !activeTypes[btn.dataset.type]);
    });
  }
  if (params.get('closed') === '1') {
    showClosed = true;
    document.getElementById('show-closed').checked = true;
  }
  if (params.get('acked') === '1') {
    onlyAcked = true;
    document.getElementById('btn-acked').classList.remove('outline');
  }
  if (params.get('owned') === '1') {
    onlyOwned = true;
    document.getElementById('btn-owned').classList.remove('outline');
  }
  filterRows();
}

var sortState = {};
function sortTable(colIdx, key) {
  var tbody = document.querySelector('tbody');
  var rows  = Array.from(tbody.querySelectorAll('tr'));
  var dir   = sortState[key] === 'asc' ? 'desc' : 'asc';
  sortState = {};
  sortState[key] = dir;
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
    if (!isNaN(an) && !isNaN(bn)) return dir === 'asc' ? an - bn : bn - an;
    return dir === 'asc' ? av.localeCompare(bv) : bv.localeCompare(av);
  });
  rows.forEach(function(r) { tbody.appendChild(r); });
}

restoreFromURL();
