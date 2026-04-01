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
//   .pageSize       — reports per page (0 = no pagination)

var allTypes = barkConfig.types;
var activeTypes = {};
allTypes.forEach(function(t) { activeTypes[t] = true; });
var onlyOpen    = true;
var onlyAcked   = false;
var onlyOwned   = false;
var onlyAwaiting = false;

var closedLoaded = false;
var closedLoading = false;

var pageSize    = barkConfig.pageSize || 0; // 0 = show all (no pagination)
var currentPage = 1;

function getSearchInput() { return document.getElementById('si'); }

function setSearch(val) {
  getSearchInput().value = val;
  currentPage = 1;
  filterRows();
  pushURL();
}

function resetFilters() {
  onlyOpen = false;
  onlyAcked = false;
  onlyOwned = false;
  onlyAwaiting = false;
  allTypes.forEach(function(t) { activeTypes[t] = true; });
  document.getElementById('btn-open').classList.add('outline');
  document.getElementById('btn-acked').classList.add('outline');
  document.getElementById('btn-owned').classList.add('outline');
  document.getElementById('btn-awaiting').classList.add('outline');
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
  var m;
  m = s.match(/^(\d+)d$/);
  if (m) { var d = new Date(); d.setDate(d.getDate() - parseInt(m[1])); return localDate(d); }
  m = s.match(/^(\d+)w$/);
  if (m) { var d = new Date(); d.setDate(d.getDate() - parseInt(m[1]) * 7); return localDate(d); }
  m = s.match(/^(\d+)m$/);
  if (m) { var d = new Date(); d.setMonth(d.getMonth() - parseInt(m[1])); return localDate(d); }
  if (/^\d{4}-\d{2}-\d{2}$/.test(s)) return s;
  return '';
}

function resolveFutureDate(s) {
  if (!s) return '';
  var m;
  m = s.match(/^(\d+)d$/);
  if (m) { var d = new Date(); d.setDate(d.getDate() + parseInt(m[1])); return localDate(d); }
  m = s.match(/^(\d+)w$/);
  if (m) { var d = new Date(); d.setDate(d.getDate() + parseInt(m[1]) * 7); return localDate(d); }
  m = s.match(/^(\d+)m$/);
  if (m) { var d = new Date(); d.setMonth(d.getMonth() + parseInt(m[1])); return localDate(d); }
  if (/^\d{4}-\d{2}-\d{2}$/.test(s)) return s;
  return '';
}

function isDuration(s) { return /^\d+[dwm]$/.test(s); }

function parseFutureDateRange(val) {
  var hasDots = val.indexOf('..') !== -1;
  var parts = val.split('..');
  if (!hasDots && isDuration(parts[0])) {
    // D:2m → from today to today + duration
    return { from: localDate(new Date()), to: resolveFutureDate(parts[0]) };
  }
  if (!hasDots) {
    // D:2026-09-01 → exact date match
    var exact = resolveFutureDate(parts[0]);
    return { from: exact, to: exact };
  }
  // D:2026-01-01..2026-06-30 or D:2026-01-01..
  return {
    from: resolveFutureDate(parts[0] || ''),
    to:   resolveFutureDate(parts[1] || '') || localDate(new Date())
  };
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
                 dateFrom: '', dateTo: '',
                 deadlineFrom: '', deadlineTo: '',
                 expiredFrom: '', expiredTo: '',
                 minPriority: null };
  var parts = q.trim().split(/\s+/).filter(Boolean);
  for (var i = 0; i < parts.length; i++) {
    var p  = parts[i];
    // D: (uppercase) is deadline shortcut — check before lowercasing
    if (p.indexOf('D:') === 0) {
      var dr = parseFutureDateRange(p.substring(2));
      result.deadlineFrom = dr.from;
      result.deadlineTo   = dr.to;
      continue;
    }
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
    } else if (lp.indexOf('deadline:') === 0) {
      var dr = parseFutureDateRange(p.substring(9));
      result.deadlineFrom = dr.from;
      result.deadlineTo   = dr.to;
    } else if (lp.indexOf('expired:') === 0 || lp.indexOf('e:') === 0) {
      var pfxLen = lp.indexOf('expired:') === 0 ? 8 : 2;
      var er = parseFutureDateRange(p.substring(pfxLen));
      result.expiredFrom = er.from;
      result.expiredTo   = er.to;
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
  if (onlyAwaiting && d.awaiting !== 'true') return false;
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
  if (q.deadlineFrom || q.deadlineTo) {
    if (!d.deadline) return false;
    if (q.deadlineFrom && d.deadline < q.deadlineFrom) return false;
    if (q.deadlineTo   && d.deadline > q.deadlineTo)   return false;
  }
  if (q.expiredFrom || q.expiredTo) {
    if (!d.expired) return false;
    if (q.expiredFrom && d.expired < q.expiredFrom) return false;
    if (q.expiredTo   && d.expired > q.expiredTo)   return false;
  }
  if (q.text && d.search.indexOf(q.text.toLowerCase()) === -1) return false;
  return true;
}

function matchRow(tr, clauses) {
  return clauses.some(function(c) { return matchClause(tr, c); });
}

/* ── Display ───────────────────────────────────────────────── */

/* Post-restripe hooks — called after stripe classes change */
var _restripeHooks = [];
var _cachedRows = null;

function getCachedRows() {
  if (!_cachedRows) _cachedRows = Array.from(document.querySelectorAll('tbody tr'));
  return _cachedRows;
}

function invalidateRowCache() { _cachedRows = null; }

function restripe() {
  var rows = getCachedRows();
  var i = 0;
  for (var k = 0; k < rows.length; k++) {
    var tr = rows[k];
    if (!tr.classList.contains('hidden')) {
      tr.classList.toggle('stripe', i++ % 2 === 1);
    } else {
      tr.classList.remove('stripe');
    }
  }
  _restripeHooks.forEach(function(fn) { fn(); });
}

// _visibleRows holds the filtered (and sorted) rows for pagination.
var _visibleRows = [];

function filterRows() {
  var raw = getSearchInput().value;
  var clauses = raw.split(/\s*\|\s*/).map(parseClause);
  var rows = getCachedRows();
  _visibleRows = [];
  for (var k = 0; k < rows.length; k++) {
    var tr = rows[k];
    if (matchRow(tr, clauses)) {
      _visibleRows.push(tr);
    }
  }
  paginate();
}

function paginate() {
  var rows = getCachedRows();
  var total = _visibleRows.length;
  var visibleSet;

  if (pageSize > 0 && total > pageSize) {
    var totalPages = Math.ceil(total / pageSize);
    if (currentPage > totalPages) currentPage = totalPages;
    if (currentPage < 1) currentPage = 1;
    var start = (currentPage - 1) * pageSize;
    var end = Math.min(start + pageSize, total);
    visibleSet = new Set(_visibleRows.slice(start, end));
    renderPagination(currentPage, totalPages, total);
  } else {
    visibleSet = new Set(_visibleRows);
    renderPagination(0, 0, total);
  }

  var i = 0;
  for (var k = 0; k < rows.length; k++) {
    var tr = rows[k];
    var show = visibleSet.has(tr);
    tr.classList.toggle('hidden', !show);
    if (show) {
      tr.classList.toggle('stripe', i++ % 2 === 1);
    } else {
      tr.classList.remove('stripe');
    }
  }
  document.getElementById('status').textContent =
    total + '/' + barkConfig.total + ' reports';
  _restripeHooks.forEach(function(fn) { fn(); });
}

function renderPagination(page, totalPages, totalVisible) {
  var el = document.getElementById('pagination');
  if (!el) return;
  if (totalPages <= 1) { el.innerHTML = ''; return; }

  var html = '<nav aria-label="Pagination" style="display:flex;align-items:center;' +
    'justify-content:center;gap:0.3rem;margin-top:0.8rem;font-size:0.85rem">';
  html += '<button ' + (page <= 1 ? 'disabled ' : '') +
    'onclick="goToPage(' + (page - 1) + ')" ' +
    'style="padding:0.2rem 0.5rem;margin:0">&lsaquo;</button>';

  // Show at most 7 page buttons with ellipsis
  var pages = compactPageRange(page, totalPages, 7);
  for (var i = 0; i < pages.length; i++) {
    var p = pages[i];
    if (p === '...') {
      html += '<span style="padding:0 0.2rem">\u2026</span>';
    } else {
      html += '<button onclick="goToPage(' + p + ')" ' +
        (p === page ? 'class="outline" aria-current="page" ' : '') +
        'style="padding:0.2rem 0.5rem;margin:0;' +
        (p === page ? 'font-weight:700;border-width:2px' : '') + '">' + p + '</button>';
    }
  }

  html += '<button ' + (page >= totalPages ? 'disabled ' : '') +
    'onclick="goToPage(' + (page + 1) + ')" ' +
    'style="padding:0.2rem 0.5rem;margin:0">&rsaquo;</button>';
  html += '</nav>';
  el.innerHTML = html;
}

function compactPageRange(current, total, maxButtons) {
  if (total <= maxButtons) {
    var r = [];
    for (var i = 1; i <= total; i++) r.push(i);
    return r;
  }
  var pages = [1];
  var left = Math.max(2, current - 1);
  var right = Math.min(total - 1, current + 1);
  if (left > 2) pages.push('...');
  for (var i = left; i <= right; i++) pages.push(i);
  if (right < total - 1) pages.push('...');
  pages.push(total);
  return pages;
}

function goToPage(p) {
  currentPage = p;
  paginate();
  pushURL();
  // Scroll to top of table
  var tbl = document.querySelector('figure');
  if (tbl) tbl.scrollIntoView({behavior: 'smooth', block: 'start'});
}

/* ── Lazy-load closed reports ──────────────────────────────── */

function abbreviateName(name) {
  var parts = name.trim().split(/\s+/);
  if (parts.length < 2) return name;
  return parts[0] + ' ' + parts[1].substring(0, 2) + '.';
}

function emailLocalPart(addr) {
  var at = addr.indexOf('@');
  return at > 0 ? addr.substring(0, at) : addr;
}

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
  var expiredDate = r['expired-date'] || '';
  var supersededBy = r['superseded-by'] || null;
  var awaitingFlag = r.awaiting || false;
  var lastActivity = r['last-activity'] || '';

  var isoDate = parseIsoDate(dateRaw);
  var closed_b = flags.length >= 3 && flags[2] !== '-';
  var author = fromName ? abbreviateName(fromName) : emailLocalPart(from);
  var flagA = acked ? 'A' : '-', flagO = owned ? 'O' : '-';
  var flagC = closeReason === 'canceled' ? 'C' : closeReason === 'expired' ? 'E' : closeReason === 'superseded' ? 'S' : closed_b ? 'R' : '-';
  var flagsStr = flagA + flagO + flagC;
  var flagsScore = (acked ? 1 : 0) + (owned ? 2 : 0) + (closed_b ? 0 : 4);
  var label = _typeLabels[type] || type;

  var subjectHtml = (closeReason === 'canceled' || closeReason === 'superseded')
                  ? '<em><s>' + escHtml(subject) + '</s></em>'
                  : closed_b ? '<em>' + escHtml(subject) + '</em>' : escHtml(subject);
  var _srcType = barkConfig.sourceType || '';
  if (archivedAt && _srcType !== 'alias' && _srcType !== 'mailbox') {
    var titleAttr = supersededBy ? ' title="Superseded by: ' + escAttr(supersededBy.subject || 'another report') + '"' : '';
    subjectHtml = '<a href="' + escAttr(archivedAt) + '"' + titleAttr + '>' + subjectHtml + '</a>';
  }

  var patchHtml = '';
  if (r.patches && r.patches.length > 0) {
    var n = r.patches.length;
    var href = n === 1
      ? 'patches/' + r.patches[0].file
      : 'patches/' + r.patches[0].file.replace(/\/[^/]+$/, '/');
    var plabel = n === 1 ? '1 patch file' : n + ' patch files';
    patchHtml = '<a href="' + escAttr(href) + '" title="' + escAttr(plabel) +
      '" aria-label="' + escAttr(plabel) + '" style="font-size:0.75rem">\uD83E\uDE79 </a>';
  }

  var relatedHtml = '';
  if (r.related && r.related.length > 0) {
    var mids = r.related.map(function(x) { return x['message-id']; }).filter(Boolean).join(',');
    if (mids) {
      relatedHtml = '<a class="secondary" href="#" onclick="showRelated(\'m:' + escAttr(mids) +
        '\'); return false;" title="Filter related reports" style="font-size:0.75rem">\u21B3' +
        r.related.length + ' </a>';
    }
  }

  var eventsHtml = '';
  if (r.events && r.events.length > 0) {
    var en = r.events.length;
    var ehref = en === 1
      ? 'events/' + r.events[0].file
      : 'events/' + r.events[0].file.replace(/\/[^/]+$/, '/');
    var elabel = en === 1 ? '1 event file' : en + ' event files';
    eventsHtml = '<a href="' + escAttr(ehref) + '" title="' + escAttr(elabel) +
      '" aria-label="' + escAttr(elabel) + '" style="font-size:0.75rem">\uD83D\uDCC5 </a>';
  }

  var textsHtml = '';
  if (r.texts && r.texts.length > 0) {
    var tn = r.texts.length;
    var thref = tn === 1
      ? 'text/' + r.texts[0].file
      : 'text/' + r.texts[0].file.replace(/\/[^/]+$/, '/');
    var tlabel = tn === 1 ? '1 text file' : tn + ' text files';
    textsHtml = '<a href="' + escAttr(thref) + '" title="' + escAttr(tlabel) +
      '" aria-label="' + escAttr(tlabel) + '" style="font-size:0.75rem">\uD83D\uDCC4 </a>';
  }

  var votesHtml = '';
  if (r.votes) {
    var vs = r.votes.split('/'), vscore = parseInt(vs[0] || '0', 10);
    var vcls = vscore > 0 ? 'vote-pos' : vscore < 0 ? 'vote-neg' : 'vote-zero';
    votesHtml = '<span class="vote-badge ' + vcls + '">' + escHtml(r.votes) + '</span>';
  }

  var priLabel = priority === 3 ? 'A' : priority === 2 ? 'B' : priority === 1 ? 'C' : ' ';
  var isMaint = role === 'maintainer' || role === 'admin';
  var authorInner = isMaint ? '<strong>' + escHtml(author) + '</strong>' : escHtml(author);
  var authorHtml = '<a href="javascript:void(0)" onclick="setSearch(\'f:' + escAttr(from) + '\')" title="' + escAttr(from) + '">' + authorInner + '</a>';

  var ownerAddr = owned || '';
  var ownerHtml = ownerAddr
    ? '<a href="javascript:void(0)" onclick="setSearch(\'o:' + escAttr(ownerAddr) + '\')" title="' + escAttr(ownerAddr) + '">' + escHtml(emailLocalPart(ownerAddr)) + '</a>'
    : '';

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
  tr.dataset.expired = expiredDate;
  tr.dataset.topic = (topic || '').toLowerCase();
  tr.dataset.awaiting = String(!!awaitingFlag);
  tr.dataset.lastActivity = lastActivity;
  tr.dataset.search = (subject + ' ' + from + ' ' + author + ' ' + ownerAddr + ' ' + isoDate + ' ' + topic).toLowerCase();

  tr.innerHTML =
    '<td title="Filter by type"><mark data-type="' + escAttr(type) + '" style="cursor:pointer" onclick="isolateType(\'' + escAttr(type) + '\')">' + escHtml(label) + '</mark></td>' +
    '<td data-value="' + priority + '" style="text-align:center">' + priLabel + '</td>' +
    '<td data-value="' + escAttr(deadline) + '" class="due-cell" title="Filter"></td>' +
    '<td data-value="' + flagsScore + '" title="' + escAttr(flagsStr) + '" style="text-align:center;font-family:monospace;font-size:0.8rem;letter-spacing:0.1em">' + flagsStr + '</td>' +
    '<td>' + patchHtml + eventsHtml + textsHtml + relatedHtml + votesHtml + (awaitingFlag ? '<span title="Awaiting reply" style="font-size:0.75rem">\u231A </span>' : '') + subjectHtml + '</td>' +
    '<td class="secondary">' + authorHtml + '</td>' +
    '<td class="secondary" data-value="' + escAttr(ownerAddr) + '" title="' + escAttr(ownerAddr) + '">' + ownerHtml + '</td>' +
    '<td data-value="' + escAttr(isoDate) + '" title="Filter"><small>' + (isoDate ? '<a href="javascript:void(0)" onclick="setSearch(\'d:' + escAttr(isoDate) + '..\')">' + escHtml(isoDate) + '</a>' : '') + '</small></td>' +
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
    var label = days < 0 ? Math.abs(days) + 'd. ago' : 'In ' + days + ' d.';
    td.innerHTML = '<a href="javascript:void(0)" onclick="setSearch(\'D:' + dl + '\')" title="' + dl + '">' + label + '</a>';
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
      invalidateRowCache();
      updateStatusButtons();
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
  if (!onlyOpen)    params.set('open', '0');
  if (onlyAcked)    params.set('acked', '1');
  if (onlyOwned)    params.set('owned', '1');
  if (onlyAwaiting) params.set('awaiting', '1');
  var sortKeys = Object.keys(sortState);
  if (sortKeys.length > 0) {
    params.set('sort', sortKeys[0]);
    params.set('dir', sortState[sortKeys[0]]);
  }
  if (pageSize > 0 && currentPage > 1) params.set('page', String(currentPage));
  var qs = params.toString();
  return location.pathname + (qs ? '?' + qs : '');
}

function pushURL()    { history.pushState(null, '', buildURL()); }
function replaceURL() { history.replaceState(null, '', buildURL()); }

/* ── Button handlers ───────────────────────────────────────── */

function toggleType(type, btn) {
  activeTypes[type] = !activeTypes[type];
  btn.classList.toggle('outline');
  currentPage = 1;
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
  currentPage = 1;
  filterRows();
  pushURL();
}

function toggleAcked(btn) {
  onlyAcked = !onlyAcked;
  btn.classList.toggle('outline');
  currentPage = 1;
  filterRows();
  pushURL();
}

function toggleOwned(btn) {
  onlyOwned = !onlyOwned;
  btn.classList.toggle('outline');
  currentPage = 1;
  filterRows();
  pushURL();
}

function toggleAwaiting(btn) {
  onlyAwaiting = !onlyAwaiting;
  btn.classList.toggle('outline');
  currentPage = 1;
  filterRows();
  pushURL();
}

function toggleOpen(btn) {
  onlyOpen = !onlyOpen;
  btn.classList.toggle('outline');
  currentPage = 1;
  if (!onlyOpen && !closedLoaded) {
    loadClosedReports(function() { filterRows(); pushURL(); });
  } else {
    filterRows();
    pushURL();
  }
}

var _filterTimer;
function onSearchInput() {
  clearTimeout(_filterTimer);
  currentPage = 1;
  _filterTimer = setTimeout(function() { filterRows(); replaceURL(); }, 120);
}

/* ── Sort ──────────────────────────────────────────────────── */

var sortState = {};

function doSort(colIdx, key, dir) {
  var tbody = document.querySelector('tbody');
  var parent = tbody.parentNode;
  var rows  = Array.from(tbody.querySelectorAll('tr'));
  document.querySelectorAll('th[data-sort]').forEach(function(th) {
    th.classList.remove('asc', 'desc');
  });
  document.querySelector('th[data-sort="' + key + '"]').classList.add(dir);
  var isDate = /^\d{4}-\d{2}-\d{2}$/;
  rows.sort(function(a, b) {
    var ac = a.children[colIdx], bc = b.children[colIdx];
    var av, bv;
    if (key === 'subject') {
      av = a.dataset.lastActivity || '';
      bv = b.dataset.lastActivity || '';
    } else {
      av = (ac.getAttribute('data-value') || ac.textContent).trim().toLowerCase();
      bv = (bc.getAttribute('data-value') || bc.textContent).trim().toLowerCase();
    }
    if (isDate.test(av) && isDate.test(bv))
      return dir === 'asc' ? av.localeCompare(bv) : bv.localeCompare(av);
    var an = parseFloat(av), bn = parseFloat(bv);
    var aNaN = isNaN(an) || av === '', bNaN = isNaN(bn) || bv === '';
    if (aNaN !== bNaN) return aNaN ? 1 : -1;
    if (!aNaN && !bNaN) return dir === 'asc' ? an - bn : bn - an;
    return dir === 'asc' ? av.localeCompare(bv) : bv.localeCompare(av);
  });
  parent.removeChild(tbody);
  for (var k = 0; k < rows.length; k++) tbody.appendChild(rows[k]);
  parent.appendChild(tbody);
  invalidateRowCache();
  restripe();
}

function sortTable(colIdx, key) {
  var dir = sortState[key] === 'asc' ? 'desc' : 'asc';
  sortState = {};
  sortState[key] = dir;
  doSort(colIdx, key, dir);
  currentPage = 1;
  filterRows();
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

  onlyAwaiting = params.get('awaiting') === '1';
  document.getElementById('btn-awaiting').classList.toggle('outline', !onlyAwaiting);

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

  currentPage = params.has('page') ? parseInt(params.get('page'), 10) || 1 : 1;

  if (!onlyOpen && !closedLoaded) {
    loadClosedReports(function() { filterRows(); });
  } else {
    filterRows();
  }
}

/* ── Conditionally hide status buttons with no matching reports ── */

function updateStatusButtons() {
  var rows = getCachedRows();
  var hasAcked = false, hasOwned = false, hasAwaiting = false;
  for (var i = 0; i < rows.length; i++) {
    var d = rows[i].dataset;
    if (!hasAcked   && d.acked   !== '') hasAcked = true;
    if (!hasOwned   && d.owned   !== '') hasOwned = true;
    if (!hasAwaiting && d.awaiting === 'true') hasAwaiting = true;
    if (hasAcked && hasOwned && hasAwaiting) break;
  }
  document.getElementById('btn-acked').style.display   = hasAcked   ? '' : 'none';
  document.getElementById('btn-owned').style.display    = hasOwned   ? '' : 'none';
  document.getElementById('btn-awaiting').style.display = hasAwaiting ? '' : 'none';
}

restoreFromURL();
updateStatusButtons();
window.addEventListener('popstate', function() { restoreFromURL(); });

document.addEventListener('keydown', function(e) {
  if (e.key === '/' && !e.ctrlKey && !e.metaKey && !e.altKey) {
    var tag = (e.target.tagName || '').toLowerCase();
    if (tag === 'input' || tag === 'textarea' || tag === 'select') return;
    e.preventDefault();
    getSearchInput().focus();
  }
});

/* Compute "Due" column for server-rendered rows */
computeDueCells(document);

/* ── Subject fold/unfold ───────────────────────────────────── */
(function() {
  var style = document.createElement('style');
  style.textContent =
    'td:nth-child(5) { position: relative; white-space: nowrap; overflow: hidden; max-width: 740px; }' +
    'td:nth-child(5).expanded { white-space: normal; overflow: visible; }' +
    '.unfold { position: absolute; right: 0; top: 50%; transform: translateY(-50%);' +
    '  cursor: pointer; font-weight: 700; font-size: 1em;' +
    '  padding: 0.1em 0.4em 0.1em 0.6em; user-select: none; z-index: 1;' +
    '  background-color: inherit; }';
  document.head.appendChild(style);

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
    var toggles = document.querySelectorAll('td:nth-child(5) .unfold');
    // Pass 1: collect visible candidates, hide hidden rows immediately
    var items = [];
    for (var i = 0; i < toggles.length; i++) {
      var toggle = toggles[i];
      var td = toggle.parentElement;
      if (td.closest('tr').classList.contains('hidden')) {
        toggle.style.display = 'none';
      } else if (td.textContent.length < 75) {
        // Short subjects never overflow 740px — skip geometry check
        toggle.style.display = 'none';
      } else {
        items.push({toggle: toggle, td: td});
      }
    }
    // Pass 2: batch geometry reads (single reflow)
    var truncated = new Array(items.length);
    for (var i = 0; i < items.length; i++) {
      truncated[i] = items[i].td.scrollWidth > items[i].td.clientWidth + 1;
    }
    // Pass 3: batch writes
    for (var i = 0; i < items.length; i++) {
      items[i].toggle.style.display = truncated[i] ? '' : 'none';
    }
  }

  setupToggles(document);
  requestAnimationFrame(showTogglesIfNeeded);
  window.addEventListener('resize', showTogglesIfNeeded);
  _restripeHooks.push(function() {
    // Double-rAF: let the browser paint visibility changes first,
    // then measure geometry on already-laid-out rows
    requestAnimationFrame(function() {
      requestAnimationFrame(showTogglesIfNeeded);
    });
  });

  var tbody = document.querySelector('tbody');
  if (window.MutationObserver) {
    new MutationObserver(function() {
      setupToggles(tbody);
      requestAnimationFrame(showTogglesIfNeeded);
    }).observe(tbody, { childList: true });
  }
})();

