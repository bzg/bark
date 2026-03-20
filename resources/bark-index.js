// bark-index.js — Client-side filtering, sorting, URL state, lazy-load closed.
// Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
// SPDX-License-Identifier: MPL-2.0
//
// Expects a global `barkConfig` object with:
//   .types          — array of report type strings
//   .total          — total report count (open + closed)
//   .openCount      — open report count
//   .closedCount    — closed report count
//   .closedJsonUrl  — URL of all-closed.json (lazy-loaded)

var allTypes = barkConfig.types;
var activeTypes = {};
allTypes.forEach(function(t) { activeTypes[t] = true; });
var onlyOpen    = true;
var onlyAcked   = false;
var onlyOwned   = false;

var closedLoaded = false;
var closedLoading = false;

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

function matchField(fieldVal, terms) {
  if (terms.length === 1 && terms[0] === '*') return fieldVal !== '';
  return terms.some(function(t) { return fieldVal.indexOf(t) !== -1; });
}

function extractValue(part, lowered) {
  return part.substring(lowered.indexOf(':') + 1);
}

function startsWithAny(lowered, prefixes) {
  return prefixes.some(function(pfx) { return lowered.indexOf(pfx) === 0; });
}

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

function parseClause(q) {
  var result = { text: '', mids: [], froms: [], subjects: [],
                 acked: [], owned: [], closed: [], topics: [],
                 urgent: [], important: [],
                 dateFrom: '', dateTo: '', minPriority: null };
  var parts = q.trim().split(/\s+/).filter(Boolean);
  for (var i = 0; i < parts.length; i++) {
    var p  = parts[i];
    var lp = p.toLowerCase();
    var matched = false;
    for (var j = 0; j < fieldMap.length; j++) {
      if (startsWithAny(lp, fieldMap[j].prefixes)) {
        result[fieldMap[j].key] = extractValue(p, lp).toLowerCase().split(',').filter(Boolean);
        matched = true;
        break;
      }
    }
    if (matched) continue;
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
  if (onlyAcked  && d.acked  === '') return false;
  if (onlyOwned  && d.owned  === '') return false;
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

function matchRow(tr, raw) {
  var clauses = raw.split(/\s*\|\s*/);
  return clauses.some(function(c) { return matchClause(tr, parseClause(c)); });
}

/* ── Display ───────────────────────────────────────────────── */

/* Post-restripe hooks — called after stripe classes change */
var _restripeHooks = [];

function restripe() {
  var i = 0;
  document.querySelectorAll('tbody tr').forEach(function(tr) {
    if (!tr.classList.contains('hidden')) {
      tr.classList.toggle('stripe', i++ % 2 === 1);
    } else {
      tr.classList.remove('stripe');
    }
  });
  _restripeHooks.forEach(function(fn) { fn(); });
}

function updateStatus() {
  var rows = document.querySelectorAll('tbody tr');
  var visible = 0;
  rows.forEach(function(tr) {
    if (!tr.classList.contains('hidden')) visible++;
  });
  var base = onlyOpen ? barkConfig.openCount : barkConfig.total;
  document.getElementById('status').textContent = visible + '/' + base + ' reports';
}

function filterRows() {
  var raw  = getSearchInput().value;
  document.querySelectorAll('tbody tr').forEach(function(tr) {
    tr.classList.toggle('hidden', !matchRow(tr, raw));
  });
  updateStatus();
  restripe();
}

/* ── Lazy-load closed reports ──────────────────────────────── */

function escHtml(s) {
  return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
}
function escAttr(s) {
  return String(s).replace(/&/g,'&amp;').replace(/"/g,'&quot;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
}

function parseIsoDate(dateRaw) {
  var ds = String(dateRaw || '').trim();
  if (ds.length >= 10 && /^\d{4}-\d{2}-\d{2}/.test(ds)) return ds.substring(0, 10);
  var monthMap = {Jan:'01',Feb:'02',Mar:'03',Apr:'04',May:'05',Jun:'06',
                  Jul:'07',Aug:'08',Sep:'09',Oct:'10',Nov:'11',Dec:'12'};
  var dm = ds.match(/^\w+ (\w+) (\d+) .* (\d{4})$/);
  if (dm && monthMap[dm[1]]) return dm[3] + '-' + monthMap[dm[1]] + '-' + String(dm[2]).padStart(2, '0');
  return '';
}

var _typeLabels = {bug:'bug',announcement:'ann',request:'req',patch:'patch',release:'rel',change:'chg'};

function buildRowElement(r) {
  var type = r.type || '', subject = r.subject || '', from = r.from || '';
  var fromName = r['from-name'] || '', dateRaw = r['date-raw'] || r.date || '';
  var flags = r.flags || '---', priority = r.priority || 0, replies = r.replies || 0;
  var archivedAt = r['archived-at'] || '', messageId = r['message-id'] || '';
  var acked = r.acked || '', owned = r.owned || '', closed = r.closed || '';
  var urgent = r.urgent || '', important = r.important || '';
  var deadline = r.deadline || '', topic = r.topic || '';
  var closeReason = r['close-reason'] || '', role = r.role || '';

  var isoDate = parseIsoDate(dateRaw);
  var closed_b = flags.length >= 3 && flags[2] === 'C';
  var author = fromName || from;
  var flagA = acked ? 'A' : '-', flagO = owned ? 'O' : '-';
  var flagC = closeReason === 'canceled' ? 'C' : closeReason === 'expired' ? 'E' : closed_b ? 'R' : '-';
  var flagsStr = flagA + flagO + flagC;
  var flagsScore = (acked ? 1 : 0) + (owned ? 2 : 0) + (closed_b ? 0 : 4);
  var label = _typeLabels[type] || type;

  var subjectHtml = closeReason === 'canceled' ? '<em><s>' + escHtml(subject) + '</s></em>'
                  : closed_b ? '<em>' + escHtml(subject) + '</em>' : escHtml(subject);
  if (archivedAt) subjectHtml = '<a href="' + escAttr(archivedAt) + '">' + subjectHtml + '</a>';

  var priLabel = priority === 3 ? 'A' : priority === 2 ? 'B' : priority === 1 ? 'C' : ' ';
  var isMaint = role === 'maintainer' || role === 'admin';
  var authorHtml = isMaint ? '<strong>' + escHtml(author) + '</strong>' : escHtml(author);

  var tr = document.createElement('tr');
  tr.dataset.type = type;
  tr.dataset.closed = String(closed_b);
  tr.dataset.mid = messageId;
  tr.dataset.from = from.toLowerCase();
  tr.dataset.subject = subject.toLowerCase();
  tr.dataset.date = isoDate;
  tr.dataset.source = r.source || '';
  tr.dataset.acked = acked.toLowerCase();
  tr.dataset.owned = owned.toLowerCase();
  tr.dataset.closedby = closed.toLowerCase();
  tr.dataset.urgent = urgent.toLowerCase();
  tr.dataset.important = important.toLowerCase();
  tr.dataset.priority = String(priority);
  tr.dataset.deadline = deadline;
  tr.dataset.topic = (topic || '').toLowerCase();
  tr.dataset.search = (subject + ' ' + from + ' ' + author + ' ' + isoDate + ' ' + topic).toLowerCase();

  tr.innerHTML =
    '<td><mark data-type="' + escAttr(type) + '" style="cursor:pointer" onclick="isolateType(\'' + escAttr(type) + '\')">' + escHtml(label) + '</mark></td>' +
    '<td data-value="' + priority + '" style="text-align:center">' + priLabel + '</td>' +
    '<td data-value="' + escAttr(deadline) + '" class="due-cell"></td>' +
    '<td data-value="' + flagsScore + '" title="' + escAttr(flagsStr) + '" style="text-align:center;font-family:monospace;font-size:0.8rem;letter-spacing:0.1em">' + flagsStr + '</td>' +
    '<td>' + subjectHtml + '</td>' +
    '<td class="secondary" title="' + escAttr(from) + '">' + authorHtml + '</td>' +
    '<td data-value="' + escAttr(isoDate) + '"><small>' + escHtml(isoDate || '') + '</small></td>' +
    '<td style="text-align:center">' + replies + '</td>';

  return tr;
}

function computeDueCells(container) {
  var today = new Date();
  today.setHours(0,0,0,0);
  var todayMs = today.getTime();
  var msPerDay = 86400000;
  container.querySelectorAll('.due-cell').forEach(function(td) {
    var dl = td.getAttribute('data-value');
    if (!dl) return;
    var parts = dl.split('-');
    if (parts.length !== 3) return;
    var deadlineMs = new Date(+parts[0], +parts[1]-1, +parts[2]).getTime();
    var days = Math.round((deadlineMs - todayMs) / msPerDay);
    td.setAttribute('data-value', String(days));
    td.textContent = days < 0 ? Math.abs(days) + 'd. ago' : 'In ' + days + ' d.';
    td.title = dl;
    td.style.textAlign = 'center';
    if (days < 0) td.style.color = 'var(--pico-del-color, #c0392b)';
    else if (days <= 3) td.style.color = 'var(--pico-ins-color, #b8860b)';
  });
}

function loadClosedReports(callback) {
  if (closedLoaded) { if (callback) callback(); return; }
  if (closedLoading) return;
  closedLoading = true;
  document.getElementById('status').textContent = 'Loading closed reports…';
  fetch(barkConfig.closedJsonUrl)
    .then(function(resp) { return resp.json(); })
    .then(function(data) {
      if (closedLoaded) return; // guard against double resolution
      var reports = data.reports || [];
      var tbody = document.querySelector('tbody');
      var fragment = document.createDocumentFragment();
      reports.forEach(function(r) { fragment.appendChild(buildRowElement(r)); });
      tbody.appendChild(fragment);
      computeDueCells(tbody);
      reports.forEach(function(r) {
        if (r.type && allTypes.indexOf(r.type) === -1) {
          allTypes.push(r.type);
          activeTypes[r.type] = true;
        }
      });
      closedLoaded = true;
      closedLoading = false;
      if (callback) callback();
    })
    .catch(function(err) {
      closedLoading = false;
      console.error('Failed to load closed reports:', err);
      document.getElementById('status').textContent = 'Failed to load closed reports.';
    });
}

/* ── URL ↔ state ───────────────────────────────────────────── */

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

/* ── Button handlers ───────────────────────────────────────── */

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
  if (!onlyOpen && !closedLoaded) {
    loadClosedReports(function() { filterRows(); pushURL(); });
  } else {
    filterRows();
    pushURL();
  }
}

function onSearchInput() {
  filterRows();
  replaceURL();
}

/* ── Sort ──────────────────────────────────────────────────── */

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
    if (aNaN !== bNaN) return aNaN ? 1 : -1;
    if (!aNaN && !bNaN) return dir === 'asc' ? an - bn : bn - an;
    return dir === 'asc' ? av.localeCompare(bv) : bv.localeCompare(av);
  });
  rows.forEach(function(r) { tbody.appendChild(r); });
  restripe();
}

function sortTable(colIdx, key) {
  var dir = sortState[key] === 'asc' ? 'desc' : 'asc';
  sortState = {};
  sortState[key] = dir;
  doSort(colIdx, key, dir);
  pushURL();
}

/* ── Restore from URL ──────────────────────────────────────── */

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

  if (!onlyOpen && !closedLoaded) {
    loadClosedReports(function() { filterRows(); });
  } else {
    filterRows();
  }
}

restoreFromURL();
window.addEventListener('popstate', function() { restoreFromURL(); });

/* Compute "Due" column for server-rendered rows */
computeDueCells(document);

/* ── Subject fold/unfold ───────────────────────────────────── */
(function() {
  var style = document.createElement('style');
  style.textContent =
    'td:nth-child(5) { position: relative; white-space: nowrap; overflow: hidden; max-width: 740px; }' +
    'td:nth-child(5).expanded { white-space: normal; overflow: visible; }' +
    '.unfold { position: absolute; right: 0; top: 50%; transform: translateY(-50%);' +
    '  cursor: pointer; color: var(--pico-primary); font-weight: 700; font-size: 1em;' +
    '  padding: 0.1em 0.4em 0.1em 0.6em; user-select: none; z-index: 1; }';
  document.head.appendChild(style);

  function isTruncated(td) {
    return td.scrollWidth > td.clientWidth + 1;
  }

  function setupToggles(container) {
    container.querySelectorAll('td:nth-child(5)').forEach(function(td) {
      if (td.querySelector('.unfold') || td.classList.contains('expanded')) return;
      var toggle = document.createElement('span');
      toggle.className = 'unfold';
      toggle.textContent = '\u2026';
      toggle.style.display = 'none';
      toggle.onmousedown = function(e) {
        e.preventDefault();
        e.stopPropagation();
      };
      toggle.onclick = function(e) {
        e.preventDefault();
        e.stopPropagation();
        td.classList.add('expanded');
        toggle.remove();
      };
      td.appendChild(toggle);
    });
  }

  function showTogglesIfNeeded() {
    document.querySelectorAll('td:nth-child(5) .unfold').forEach(function(toggle) {
      var td = toggle.parentElement;
      if (isTruncated(td)) {
        toggle.style.display = '';
        toggle.style.backgroundColor = getComputedStyle(td).backgroundColor;
      } else {
        toggle.style.display = 'none';
      }
    });
  }

  setupToggles(document);
  requestAnimationFrame(showTogglesIfNeeded);
  window.addEventListener('resize', showTogglesIfNeeded);
  _restripeHooks.push(function() {
    requestAnimationFrame(showTogglesIfNeeded);
  });

  var tbody = document.querySelector('tbody');
  if (window.MutationObserver) {
    new MutationObserver(function() {
      setupToggles(tbody);
      requestAnimationFrame(showTogglesIfNeeded);
    }).observe(tbody, { childList: true });
  }
})();

