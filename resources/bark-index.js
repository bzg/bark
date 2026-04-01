// bark-index.js — Client-side filtering, sorting, URL state, lazy-load closed.
// Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
// SPDX-License-Identifier: MPL-2.0
//
// Expects global objects:
//   barkConfig — .types, .total, .openCount, .closedCount, .closedJsonUrl, .pageSize, .sourceType
//   barkData   — array of report objects (open reports, embedded in page)

var allTypes = barkConfig.types;
var activeTypes = {};
allTypes.forEach(function(t) { activeTypes[t] = true; });
var onlyOpen    = true;
var onlyAcked   = false;
var onlyOwned   = false;
var onlyAwaiting = false;

var closedLoaded = false;
var closedLoading = false;

var pageSize    = barkConfig.pageSize !== undefined ? barkConfig.pageSize : 50;
var currentPage = 1;

// Pre-compute today's timestamp for due-date calculations
var _today = new Date();
_today.setHours(0,0,0,0);
var _todayMs = _today.getTime();

// ── Data model ──────────────────────────────────────────────
// All data lives in JS arrays; DOM is only used for rendering the current page.
var _allReports = [];       // prepared report objects
var _filteredReports = [];  // filtered + sorted subset

function getSearchInput() { return document.getElementById('si'); }

function setSearch(val) {
  getSearchInput().value = val;
  currentPage = 1;
  filterReports();
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
    return { from: localDate(new Date()), to: resolveFutureDate(parts[0]) };
  }
  if (!hasDots) {
    var exact = resolveFutureDate(parts[0]);
    return { from: exact, to: exact };
  }
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
  // Pre-lowercase text for matching (avoids repeated toLowerCase in tight loop)
  if (result.text) result.text = result.text.toLowerCase();
  return result;
}

/* ── Report preparation ──────────────────────────────────────── */

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

function prepareReport(r) {
  var type = r.type || '';
  var subject = r.subject || '';
  var from = r.from || '';
  var fromName = r['from-name'] || '';
  var dateRaw = r['date-raw'] || r.date || '';
  var flags = r.flags || '---';
  var priority = r.priority || 0;
  var acked = r.acked || '';
  var owned = r.owned || '';
  var closed = r.closed || '';
  var urgent = r.urgent || '';
  var important = r.important || '';
  var deadline = r.deadline || '';
  var topic = r.topic || '';
  var closeReason = r['close-reason'] || '';
  var expiredDate = r['expired-date'] || '';
  var awaiting = r.awaiting || false;
  var lastActivity = r['last-activity'] || '';
  var messageId = r['message-id'] || '';
  var source = r.source || '';
  var replies = r.replies || 0;

  var isoDate = parseIsoDate(dateRaw);
  var closedBool = flags.length >= 3 && flags[2] !== '-';
  var author = fromName ? abbreviateName(fromName) : emailLocalPart(from);
  var flagsScore = (acked ? 1 : 0) + (owned ? 2 : 0) + (closedBool ? 0 : 4);

  var dueDays = null;
  if (deadline) {
    var parts = deadline.split('-');
    if (parts.length === 3) {
      var deadlineMs = new Date(+parts[0], +parts[1]-1, +parts[2]).getTime();
      dueDays = Math.round((deadlineMs - _todayMs) / 86400000);
    }
  }

  return {
    raw: r,
    // Filter/sort index (lowercase for string comparisons)
    type: type,
    closed: closedBool,
    mid: messageId,
    from: from.toLowerCase(),
    subject: subject.toLowerCase(),
    date: isoDate,
    source: source,
    acked: acked.toLowerCase(),
    owned: owned.toLowerCase(),
    closedby: closed.toLowerCase(),
    urgent: urgent.toLowerCase(),
    important: important.toLowerCase(),
    priority: priority,
    deadline: deadline,
    expired: expiredDate,
    topic: (topic || '').toLowerCase(),
    awaiting: !!awaiting,
    lastActivity: lastActivity,
    search: (subject + ' ' + from + ' ' + author + ' ' + owned + ' ' + isoDate + ' ' + topic).toLowerCase(),
    // Render helpers (pre-computed once)
    isoDate: isoDate,
    author: author,
    flagsScore: flagsScore,
    dueDays: dueDays,
    replies: replies
  };
}

/* ── Matching (operates on prepared report objects, not DOM) ── */

function matchReport(rpt, q) {
  if (!activeTypes[rpt.type]) return false;
  if (onlyOpen && rpt.closed) return false;
  if (onlyAcked  && rpt.acked === '') return false;
  if (onlyOwned  && rpt.owned === '') return false;
  if (onlyAwaiting && !rpt.awaiting) return false;
  for (var j = 0; j < fieldMap.length; j++) {
    var f = fieldMap[j];
    if (q[f.key].length > 0) {
      var val = rpt[f.dataAttr] || '';
      if (!matchField(val, q[f.key])) return false;
    }
  }
  if (q.minPriority !== null && rpt.priority < q.minPriority) return false;
  if (q.dateFrom && rpt.date < q.dateFrom) return false;
  if (q.dateTo   && rpt.date > q.dateTo)   return false;
  if (q.deadlineFrom || q.deadlineTo) {
    if (!rpt.deadline) return false;
    if (q.deadlineFrom && rpt.deadline < q.deadlineFrom) return false;
    if (q.deadlineTo   && rpt.deadline > q.deadlineTo)   return false;
  }
  if (q.expiredFrom || q.expiredTo) {
    if (!rpt.expired) return false;
    if (q.expiredFrom && rpt.expired < q.expiredFrom) return false;
    if (q.expiredTo   && rpt.expired > q.expiredTo)   return false;
  }
  if (q.text && rpt.search.indexOf(q.text) === -1) return false;
  return true;
}

function matchReportAny(rpt, clauses) {
  return clauses.some(function(c) { return matchReport(rpt, c); });
}

/* ── Filtering & Sorting (in-memory, no DOM access) ──────────── */

function filterReports() {
  console.time('bark:filter');
  var raw = getSearchInput().value;
  var clauses = raw.split(/\s*\|\s*/).map(parseClause);
  _filteredReports = [];
  for (var i = 0; i < _allReports.length; i++) {
    if (matchReportAny(_allReports[i], clauses)) {
      _filteredReports.push(_allReports[i]);
    }
  }
  // Re-apply current sort if active
  var sortKeys = Object.keys(sortState);
  if (sortKeys.length > 0) {
    sortReports(sortKeys[0], sortState[sortKeys[0]]);
  }
  console.timeEnd('bark:filter');
  console.time('bark:render');
  renderPage();
  console.timeEnd('bark:render');
}

function getSortValue(rpt, key) {
  switch(key) {
    case 'type':     return _typeLabels[rpt.type] || rpt.type;
    case 'priority': return rpt.priority;
    case 'due':      return rpt.dueDays !== null ? rpt.dueDays : 99999;
    case 'flags':    return rpt.flagsScore;
    case 'subject':  return rpt.lastActivity || '';
    case 'from':     return rpt.from;
    case 'owner':    return rpt.owned;
    case 'date':     return rpt.date;
    case 'replies':  return rpt.replies;
    default:         return '';
  }
}

function sortReports(key, dir) {
  _filteredReports.sort(function(a, b) {
    var av = getSortValue(a, key);
    var bv = getSortValue(b, key);
    if (typeof av === 'number' && typeof bv === 'number') {
      return dir === 'asc' ? av - bv : bv - av;
    }
    av = String(av); bv = String(bv);
    if (av === '' && bv !== '') return 1;
    if (av !== '' && bv === '') return -1;
    return dir === 'asc' ? av.localeCompare(bv) : bv.localeCompare(av);
  });
}

/* ── Rendering (builds only the current page's DOM nodes) ────── */

function buildRowElement(rpt) {
  var r = rpt.raw;
  var type = rpt.type;
  var subject = r.subject || '';
  var from = r.from || '';
  var priority = rpt.priority;
  var acked = r.acked || '';
  var owned = r.owned || '';
  var closeReason = r['close-reason'] || '';
  var role = r.role || '';
  var archivedAt = r['archived-at'] || '';
  var supersededBy = r['superseded-by'] || null;
  var awaitingFlag = rpt.awaiting;
  var expiry = r.expiry || '';
  var isoDate = rpt.isoDate;
  var author = rpt.author;

  var closedBool = rpt.closed;
  var flagA = acked ? 'A' : '-', flagO = owned ? 'O' : '-';
  var flagC = closeReason === 'canceled' ? 'C' : closeReason === 'expired' ? 'E' : closeReason === 'superseded' ? 'S' : closedBool ? 'R' : '-';
  var flagsStr = flagA + flagO + flagC;
  var flagsTitle = [flagA === 'A' ? 'Acked' : '', flagO === 'O' ? 'Owned' : '',
                    flagC === 'C' ? 'Canceled' : flagC === 'E' ? 'Expired' : flagC === 'S' ? 'Superseded' : flagC === 'R' ? 'Resolved' : '']
                   .filter(Boolean).join(', ');
  var label = _typeLabels[type] || type;

  var subjectHtml = (closeReason === 'canceled' || closeReason === 'superseded')
                  ? '<em><s>' + escHtml(subject) + '</s></em>'
                  : closedBool ? '<em>' + escHtml(subject) + '</em>' : escHtml(subject);
  var _srcType = barkConfig.sourceType || '';
  if (archivedAt && _srcType !== 'alias' && _srcType !== 'mailbox') {
    var titleAttr = supersededBy ? ' title="Superseded by: ' + escAttr(supersededBy.subject || 'another report') + '"' : '';
    subjectHtml = '<a href="' + escAttr(archivedAt) + '"' + titleAttr + ' target="_blank">' + subjectHtml + '</a>';
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

  // Due cell (pre-computed, no post-render DOM walk needed)
  var dueHtml = '';
  var dueStyle = '';
  if (rpt.dueDays !== null) {
    var dl = rpt.deadline;
    var days = rpt.dueDays;
    var dueLabel = days < 0 ? Math.abs(days) + 'd. ago' : 'In ' + days + ' d.';
    if (days < 0) dueStyle = 'color:var(--pico-del-color, #c0392b);';
    else if (days <= 3) dueStyle = 'color:var(--pico-ins-color, #b8860b);';
    dueHtml = '<a href="javascript:void(0)" onclick="setSearch(\'D:' + dl + '\')" title="' + dl + '">' + dueLabel + '</a>';
  }

  var priLabel = priority === 3 ? 'A' : priority === 2 ? 'B' : priority === 1 ? 'C' : ' ';
  var isMaint = role === 'maintainer' || role === 'admin';
  var authorInner = isMaint ? '<strong>' + escHtml(author) + '</strong>' : escHtml(author);
  var authorHtml = '<a href="javascript:void(0)" onclick="setSearch(\'f:' + escAttr(from) + '\')" title="' + escAttr(from) + '">' + authorInner + '</a>';

  var ownerAddr = owned || '';
  var ownerHtml = ownerAddr
    ? '<a href="javascript:void(0)" onclick="setSearch(\'o:' + escAttr(ownerAddr) + '\')" title="' + escAttr(ownerAddr) + '">' + escHtml(emailLocalPart(ownerAddr)) + '</a>'
    : '';

  // Date cell with expiry handling
  var dateHtml = '';
  if (isoDate) {
    var dateLink = '<a href="javascript:void(0)" onclick="setSearch(\'d:' + escAttr(isoDate) + '..\')">' + escHtml(isoDate) + '</a>';
    if (expiry) {
      dateHtml = '<small title="Expires on ' + escAttr(expiry) + '"><em>' + dateLink + '</em></small>';
    } else {
      dateHtml = '<small>' + dateLink + '</small>';
    }
  }

  var tr = document.createElement('tr');
  tr.innerHTML =
    '<td title="Filter by type"><mark data-type="' + escAttr(type) + '" style="cursor:pointer" onclick="isolateType(\'' + escAttr(type) + '\')">' + escHtml(label) + '</mark></td>' +
    '<td style="text-align:center">' + priLabel + '</td>' +
    '<td style="text-align:center;' + dueStyle + '">' + dueHtml + '</td>' +
    '<td title="' + escAttr(flagsTitle) + '" style="text-align:center;font-family:monospace;font-size:0.8rem;letter-spacing:0.1em">' + flagsStr + '</td>' +
    '<td>' + patchHtml + eventsHtml + textsHtml + relatedHtml + votesHtml + (awaitingFlag ? '<span title="Awaiting reply" style="font-size:0.75rem">\u231A </span>' : '') + subjectHtml + '</td>' +
    '<td class="secondary">' + authorHtml + '</td>' +
    '<td class="secondary" title="' + escAttr(ownerAddr) + '">' + ownerHtml + '</td>' +
    '<td title="Filter">' + dateHtml + '</td>' +
    '<td style="text-align:center">' + rpt.replies + '</td>';

  return tr;
}

function renderPage() {
  var total = _filteredReports.length;
  var start, end;

  if (pageSize > 0 && total > pageSize) {
    var totalPages = Math.ceil(total / pageSize);
    if (currentPage > totalPages) currentPage = totalPages;
    if (currentPage < 1) currentPage = 1;
    start = (currentPage - 1) * pageSize;
    end = Math.min(start + pageSize, total);
    renderPagination(currentPage, totalPages, total);
  } else {
    start = 0;
    end = total;
    renderPagination(0, 0, total);
  }

  var tbody = document.querySelector('tbody');
  var fragment = document.createDocumentFragment();
  for (var i = start; i < end; i++) {
    var tr = buildRowElement(_filteredReports[i]);
    tr.classList.toggle('stripe', (i - start) % 2 === 1);
    fragment.appendChild(tr);
  }
  tbody.innerHTML = '';
  tbody.appendChild(fragment);

  document.getElementById('status').textContent =
    total + '/' + barkConfig.total + ' reports';

  // Setup subject toggles for rendered rows only (not all 2000+)
  _setupToggles(tbody);
  requestAnimationFrame(_showTogglesIfNeeded);
}

/* ── Pagination ──────────────────────────────────────────────── */

function renderPagination(page, totalPages, totalVisible) {
  var el = document.getElementById('pagination');
  if (!el) return;
  if (totalPages <= 1) { el.innerHTML = ''; return; }

  var html = '<nav aria-label="Pagination" style="display:flex;align-items:center;' +
    'justify-content:center;gap:0.3rem;margin-top:0.8rem;font-size:0.85rem">';
  html += '<button ' + (page <= 1 ? 'disabled ' : '') +
    'onclick="goToPage(' + (page - 1) + ')" ' +
    'style="padding:0.2rem 0.5rem;margin:0">&lsaquo;</button>';

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
  renderPage();
  pushURL();
  var tbl = document.querySelector('figure');
  if (tbl) tbl.scrollIntoView({behavior: 'smooth', block: 'start'});
}

/* ── Lazy-load closed reports ────────────────────────────────── */

function loadClosedReports(callback) {
  if (closedLoaded) { if (callback) callback(); return; }
  if (closedLoading) return;
  closedLoading = true;
  document.getElementById('status').textContent = 'Loading closed reports\u2026';
  fetch(barkConfig.closedJsonUrl)
    .then(function(resp) { return resp.json(); })
    .then(function(data) {
      if (closedLoaded) return;
      var reports = data.reports || [];
      for (var i = 0; i < reports.length; i++) {
        var rpt = prepareReport(reports[i]);
        _allReports.push(rpt);
        if (rpt.type && allTypes.indexOf(rpt.type) === -1) {
          allTypes.push(rpt.type);
          activeTypes[rpt.type] = true;
        }
      }
      closedLoaded = true;
      closedLoading = false;
      updateStatusButtons();
      if (callback) callback();
    })
    .catch(function(err) {
      closedLoading = false;
      console.error('Failed to load closed reports:', err);
      document.getElementById('status').textContent = 'Failed to load closed reports.';
    });
}

/* ── URL ↔ state ─────────────────────────────────────────────── */

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

/* ── Button handlers ─────────────────────────────────────────── */

function toggleType(type, btn) {
  activeTypes[type] = !activeTypes[type];
  btn.classList.toggle('outline');
  currentPage = 1;
  filterReports();
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
  filterReports();
  pushURL();
}

function toggleAcked(btn) {
  onlyAcked = !onlyAcked;
  btn.classList.toggle('outline');
  currentPage = 1;
  filterReports();
  pushURL();
}

function toggleOwned(btn) {
  onlyOwned = !onlyOwned;
  btn.classList.toggle('outline');
  currentPage = 1;
  filterReports();
  pushURL();
}

function toggleAwaiting(btn) {
  onlyAwaiting = !onlyAwaiting;
  btn.classList.toggle('outline');
  currentPage = 1;
  filterReports();
  pushURL();
}

function toggleOpen(btn) {
  onlyOpen = !onlyOpen;
  btn.classList.toggle('outline');
  currentPage = 1;
  if (!onlyOpen && !closedLoaded) {
    loadClosedReports(function() { filterReports(); pushURL(); });
  } else {
    filterReports();
    pushURL();
  }
}

var _filterTimer;
function onSearchInput() {
  clearTimeout(_filterTimer);
  currentPage = 1;
  _filterTimer = setTimeout(function() { filterReports(); replaceURL(); }, 120);
}

/* ── Sort ────────────────────────────────────────────────────── */

var sortState = {};

function sortTable(colIdx, key) {
  var dir = sortState[key] === 'asc' ? 'desc' : 'asc';
  sortState = {};
  sortState[key] = dir;
  document.querySelectorAll('th[data-sort]').forEach(function(th) {
    th.classList.remove('asc', 'desc');
  });
  document.querySelector('th[data-sort="' + key + '"]').classList.add(dir);
  sortReports(key, dir);
  currentPage = 1;
  renderPage();
  pushURL();
}

/* ── Status buttons ──────────────────────────────────────────── */

function updateStatusButtons() {
  var hasAcked = false, hasOwned = false, hasAwaiting = false;
  for (var i = 0; i < _allReports.length; i++) {
    var rpt = _allReports[i];
    if (!hasAcked   && rpt.acked   !== '') hasAcked = true;
    if (!hasOwned   && rpt.owned   !== '') hasOwned = true;
    if (!hasAwaiting && rpt.awaiting) hasAwaiting = true;
    if (hasAcked && hasOwned && hasAwaiting) break;
  }
  document.getElementById('btn-acked').style.display   = hasAcked   ? '' : 'none';
  document.getElementById('btn-owned').style.display    = hasOwned   ? '' : 'none';
  document.getElementById('btn-awaiting').style.display = hasAwaiting ? '' : 'none';
}

/* ── Restore from URL ────────────────────────────────────────── */

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
      th.classList.add(dir);
    }
  }

  currentPage = params.has('page') ? parseInt(params.get('page'), 10) || 1 : 1;

  if (!onlyOpen && !closedLoaded) {
    loadClosedReports(function() { filterReports(); });
  } else {
    filterReports();
  }
}

/* ── Subject fold/unfold ─────────────────────────────────────── */
var _setupToggles, _showTogglesIfNeeded;
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

  _setupToggles = function(container) {
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
  };

  _showTogglesIfNeeded = function() {
    var toggles = document.querySelectorAll('td:nth-child(5) .unfold');
    // Only processes rendered rows (current page), not all 2000+
    var items = [];
    for (var i = 0; i < toggles.length; i++) {
      var toggle = toggles[i];
      var td = toggle.parentElement;
      if (td.textContent.length < 75) {
        toggle.style.display = 'none';
      } else {
        items.push({toggle: toggle, td: td});
      }
    }
    // Batch geometry reads (single reflow)
    var truncated = new Array(items.length);
    for (var i = 0; i < items.length; i++) {
      truncated[i] = items[i].td.scrollWidth > items[i].td.clientWidth + 1;
    }
    // Batch writes
    for (var i = 0; i < items.length; i++) {
      items[i].toggle.style.display = truncated[i] ? '' : 'none';
    }
  };

  window.addEventListener('resize', function() {
    requestAnimationFrame(_showTogglesIfNeeded);
  });
})();

/* ── Initialize ──────────────────────────────────────────────── */

// Prepare all embedded report data into indexed objects
console.time('bark:prepare');
for (var _i = 0; _i < barkData.length; _i++) {
  _allReports.push(prepareReport(barkData[_i]));
}
console.timeEnd('bark:prepare');

console.time('bark:initial-render');
restoreFromURL();
console.timeEnd('bark:initial-render');
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
