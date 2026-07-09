// bone-search.js -- The report query engine behind the index search box.
// Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
// SPDX-License-Identifier: MPL-2.0
//
// Port of gnaw.el's search: the query compiles once into predicates
// (one per token), `|' separates OR clauses, spaces separate AND
// tokens, double quotes protect both.  Kept DOM-free so the
// conformance tests (test/bone-search-conformance.js) run it under
// node; the HTML shell (scripts/bone-index.clj) inlines it right
// before bone-index.js, which consumes the same globals.
//
// Syntax, kept in sync with gnaw.el and docs-tpl.org:
//   clause  = token token ...        (space = AND, `|' between clauses = OR)
//   token   = [-]key:value | [-]bare-word | "quoted subject phrase"
//   value   = alt,alt,...            (comma = OR; a "…" or /re/ value
//                                     stays whole and may contain commas)
//   keys    = from/f subject/s similar topic/t/T source/S type
//             priority/p mid/m acked/a owned/o closed/c urgent/u
//             important/i flags/F att/attributes/A date/d deadline/D
//             expired/e
// A bare word (or unknown key) searches the subject.  An empty value
// matches nothing, whatever the key.  type:, flags: and att: are
// closed sets compared literally: no *, "…" or /re/ there.

/* ── Date helpers ────────────────────────────────────────────── */

function localDate(d) {
  var y = d.getFullYear();
  var m = String(d.getMonth() + 1).padStart(2, '0');
  var day = String(d.getDate()).padStart(2, '0');
  return y + '-' + m + '-' + day;
}

function resolveRelativeDate(s, sign) {
  if (!s) return '';
  var m = s.match(/^(\d+)([dwm])$/);
  if (m) {
    var n = parseInt(m[1]) * sign;
    var d = new Date();
    if (m[2] === 'd') d.setDate(d.getDate() + n);
    else if (m[2] === 'w') d.setDate(d.getDate() + n * 7);
    else d.setMonth(d.getMonth() + n);
    return localDate(d);
  }
  if (/^\d{4}-\d{2}-\d{2}$/.test(s)) return s;
  return '';
}

function resolveDate(s)       { return resolveRelativeDate(s, -1); }
function resolveFutureDate(s) { return resolveRelativeDate(s,  1); }

function isDuration(s) { return /^\d+[dwm]$/.test(s); }
function isYmd(s)      { return /^\d{4}-\d{2}-\d{2}$/.test(s); }

function parseIsoDate(dateRaw) {
  var ds = String(dateRaw || '').trim();
  if (ds.length >= 10 && /^\d{4}-\d{2}-\d{2}/.test(ds)) return ds.substring(0, 10);
  var monthMap = {Jan:'01',Feb:'02',Mar:'03',Apr:'04',May:'05',Jun:'06',
                  Jul:'07',Aug:'08',Sep:'09',Oct:'10',Nov:'11',Dec:'12'};
  var dm = ds.match(/^\w+ (\w+) (\d+) .* (\d{4})$/);
  if (dm && monthMap[dm[1]]) return dm[3] + '-' + monthMap[dm[1]] + '-' + String(dm[2]).padStart(2, '0');
  return '';
}

/* ── Tokenizer ───────────────────────────────────────────────── */

// Split RAW into clauses of tokens: `|' = OR, space = AND, both losing
// their meaning between double quotes, so subject:"a b" stays one
// token.  An unbalanced quote swallows the rest of the query into the
// current token; empty clauses are dropped.  Port of gnaw--query-parse.
function parseQuery(raw) {
  var clauses = [], tokens = [], tok = '', inQuote = false;
  for (var i = 0; i < raw.length; i++) {
    var ch = raw[i];
    if (ch === '"') { inQuote = !inQuote; tok += ch; }
    else if (!inQuote && (ch === ' ' || ch === '\t')) {
      if (tok) { tokens.push(tok); tok = ''; }
    } else if (!inQuote && ch === '|') {
      if (tok) { tokens.push(tok); tok = ''; }
      if (tokens.length) clauses.push(tokens);
      tokens = [];
    } else tok += ch;
  }
  if (tok) tokens.push(tok);
  if (tokens.length) clauses.push(tokens);
  return clauses;
}

/* ── Value forms ─────────────────────────────────────────────── */

// The inner text of a "quoted" (or /slashed/) value, or null.
function quotedValue(v) {
  return v.length >= 2 && v[0] === '"' && v[v.length - 1] === '"'
    ? v.substring(1, v.length - 1) : null;
}
function regexpValue(v) {
  return v.length >= 2 && v[0] === '/' && v[v.length - 1] === '/'
    ? v.substring(1, v.length - 1) : null;
}

// Split VAL into its comma (OR) alternatives; a quoted or regexp VAL
// stays whole and may thus contain commas.  Port of gnaw--query-vals.
function queryVals(val) {
  if (quotedValue(val) !== null || regexpValue(val) !== null) return [val];
  return val.split(',').filter(Boolean);
}

/* ── Matchers ────────────────────────────────────────────────── */
// Every matcher takes a lowercased field string ('' when unset); the
// prepared report fields are lowercased once at load time.

// Port of gnaw--query-text-matcher: NEEDLE `*' matches any non-empty
// field; /re/ is a case-insensitive regexp (JS dialect), invalid = no
// match; "…" is literal, `*' and slashes included; anything else is a
// case-insensitive substring.  An empty NEEDLE matches nothing.
function textMatcher(needle) {
  if (needle === '*') return function(s) { return s !== ''; };
  var lit = quotedValue(needle);
  var re  = lit === null ? regexpValue(needle) : null;
  if ((lit !== null ? lit : re !== null ? re : needle) === '')
    return function() { return false; };
  if (re !== null) {
    var rx;
    try { rx = new RegExp(re, 'i'); }
    catch (e) { return function() { return false; }; }
    return function(s) { return rx.test(s); };
  }
  var sub = (lit !== null ? lit : needle).toLowerCase();
  return function(s) { return s.indexOf(sub) !== -1; };
}

// Port of gnaw--query-actor-matcher: `*' or `true' (any case) matches
// any set actor; other values match as textMatcher does, but never an
// unset one.
function actorMatcher(needle) {
  if (needle === '*' || needle.toLowerCase() === 'true')
    return function(s) { return s !== ''; };
  var m = textMatcher(needle);
  return function(s) { return s !== '' && m(s); };
}

// Port of gnaw--query-field-matcher: compile VAL's comma alternatives
// through MAKE-MATCHER and try each against the fields read by GETTERS.
function fieldMatcher(val, makeMatcher, getters) {
  var ms = queryVals(val).map(makeMatcher);
  return function(rpt) {
    return ms.some(function(m) {
      return getters.some(function(g) { return m(g(rpt)); });
    });
  };
}

// Port of gnaw--query-glyph-matcher: every character of an alternative
// must be present in the report's glyph string (comma is OR).
function glyphMatcher(val, getGlyphs) {
  var alts = queryVals(val).filter(function(v) { return v !== ''; });
  return function(rpt) {
    var s = getGlyphs(rpt);
    return alts.some(function(v) {
      for (var i = 0; i < v.length; i++)
        if (s.indexOf(v[i]) === -1) return false;
      return true;
    });
  };
}

// Port of gnaw--query-flag-matcher: `*' and `true' require the mark,
// `false' its absence (both any case); any other value matches
// nothing.  The exported urgent/important fields hold who set the
// mark, so set = non-empty.
function flagBitMatcher(val, getField) {
  var v = val.toLowerCase();
  var want = (v === '*' || v === 'true');
  if (!want && v !== 'false') return function() { return false; };
  return function(rpt) { return (getField(rpt) !== '') === want; };
}

// One bound of a date range: '' = open, a duration resolves relative
// to today (forward or backward per key), undefined = invalid.
function queryBound(s, forward) {
  if (!s) return null;
  if (isDuration(s)) return forward ? resolveFutureDate(s) : resolveDate(s);
  if (isYmd(s)) return s;
  return undefined;
}

// Port of gnaw--query-date-matcher: A..B compares both bounds (open
// ends stay open), a lone duration is a window ending (date:) or
// starting (deadline:, expired:) today, a lone date is that exact day,
// anything else matches nothing.  A report without the field never
// matches.  ISO strings compare lexicographically.
function dateMatcher(val, getField, forward) {
  var lo = null, hi = null, none = false;
  if (val.indexOf('..') !== -1) {
    var parts = val.split('..');
    var a = queryBound(parts[0], forward);
    var b = queryBound(parts[1] || '', forward);
    if (a === undefined || b === undefined) none = true;
    else if (a !== null && b !== null) { lo = a < b ? a : b; hi = a < b ? b : a; }
    else { lo = a; hi = b; }
  } else if (isDuration(val)) {
    if (forward) { lo = localDate(new Date()); hi = resolveFutureDate(val); }
    else { lo = resolveDate(val); hi = localDate(new Date()); }
  } else if (isYmd(val)) { lo = val; hi = val; }
  else none = true;
  if (none) return function() { return false; };
  return function(rpt) {
    var d = getField(rpt);
    return d !== '' && (lo === null || d >= lo) && (hi === null || d <= hi);
  };
}

// Port of gnaw--subject-words: downcased words of four letters or
// more, deduped, bracketed tags like [PATCH v2 1/3] dropped first, a
// hyphenated name like org-element--cache counting as one word.
// Returns a set-like object, precomputed per report at load time.
function subjectWords(subject) {
  var s = String(subject || '').toLowerCase().replace(/\[[^\]]*\]/g, ' ');
  // No prototype: 'constructor' in a query must not count as a word.
  var words = Object.create(null);
  s.split(/[^\p{L}\p{N}-]+/u).forEach(function(w) {
    w = w.replace(/^-+|-+$/g, '');
    if (w.length >= 4) words[w] = true;
  });
  return words;
}

// Port of gnaw--query-similar-matcher: a subject matches when it
// shares at least three of VAL's `+'-joined words -- all of them when
// VAL has fewer than three.
function similarMatcher(val) {
  var words = val.toLowerCase().split('+').filter(Boolean);
  var need = Math.min(3, words.length);
  return function(rpt) {
    if (need === 0) return false;
    var n = 0;
    for (var i = 0; i < words.length; i++)
      if (rpt.subjectWords[words[i]] && ++n >= need) return true;
    return false;
  };
}

// The bare-word (and unknown-key) search: the whole token matched on
// the subject as textMatcher does, commas included.
function subjectMatcher(needle) {
  var m = textMatcher(needle);
  return function(rpt) { return m(rpt.subject); };
}

/* ── Token compilation ───────────────────────────────────────── */

// Compile an unquoted, unnegated token into a predicate on a prepared
// report.  Port of gnaw--query-compile-key; keys are case-sensitive
// (T: topic, D: deadline, F: flags, A: att, S: source).
function compileKey(token) {
  var i = token.indexOf(':');
  var key = i !== -1 ? token.substring(0, i) : null;
  var val = i !== -1 ? token.substring(i + 1) : null;
  switch (key) {
    case 'from': case 'f':
      return fieldMatcher(val, textMatcher,
        [function(r) { return r.from; }, function(r) { return r.fromName; }]);
    case 'subject': case 's':
      return fieldMatcher(val, textMatcher,
        [function(r) { return r.subject; }]);
    case 'topic': case 't': case 'T':
      // Topics are whitespace-free tokens; match any of the report's.
      return (function() {
        var ms = queryVals(val).map(textMatcher);
        return function(rpt) {
          return ms.some(function(m) { return rpt.topics.some(m); });
        };
      })();
    case 'message-id': case 'mid': case 'm':
      return fieldMatcher(val, textMatcher,
        [function(r) { return r.mid; }]);
    case 'acked': case 'a':
      return fieldMatcher(val, actorMatcher,
        [function(r) { return r.acked; }]);
    case 'owned': case 'o':
      return fieldMatcher(val, actorMatcher,
        [function(r) { return r.owned; }, function(r) { return r.ownedName; }]);
    case 'closed': case 'c':
      // The exact value true (any case) never reaches here:
      // compileQuery keeps it as the include-closed trigger, matching
      // everything (unlike gnaw.el, where reports of every state
      // always sit in the list).
      return fieldMatcher(val, actorMatcher,
        [function(r) { return r.closedby; }]);
    case 'source': case 'S':
      // A single character matches the source name exactly (in gnaw.el
      // it also matches the source letter, a config concept the web
      // page does not have); a longer value matches as topic: does.
      return fieldMatcher(val, function(v) {
        if (v.length !== 1 || v === '*') return textMatcher(v);
        var d = v.toLowerCase();
        return function(s) { return s === d; };
      }, [function(r) { return r.source; }]);
    case 'type':
      // A closed set compared whole: no *, regexp or quotes here, and
      // type:* matches nothing.
      return (function() {
        var vals = queryVals(val).map(function(v) { return v.toLowerCase(); });
        return function(rpt) {
          return vals.indexOf(String(rpt.type).toLowerCase()) !== -1;
        };
      })();
    case 'priority': case 'p':
      return (function() {
        var vals = queryVals(val);
        return function(rpt) {
          return vals.indexOf(String(rpt.priority)) !== -1;
        };
      })();
    case 'urgent': case 'u':
      return flagBitMatcher(val, function(r) { return r.urgent; });
    case 'important': case 'i':
      return flagBitMatcher(val, function(r) { return r.important; });
    // Glyph matches, mirroring the Flags and Att columns:
    // flags:AO = acked and owned; flags:S = superseded;
    // att:~+ = related and a single patch.
    case 'flags': case 'F':
      return glyphMatcher(val.toUpperCase(), function(r) { return r.flags; });
    case 'att': case 'attributes': case 'A':
      // downcase: x is the only cased glyph, and the neighbor flags:
      // alphabet is uppercase.
      return glyphMatcher(val.toLowerCase(), function(r) { return r.att; });
    case 'similar':
      return similarMatcher(val);
    case 'date': case 'd':
      return dateMatcher(val, function(r) { return r.date; }, false);
    case 'deadline': case 'D':
      return dateMatcher(val, function(r) { return r.deadline; }, true);
    case 'expired': case 'e':
      return dateMatcher(val, function(r) { return r.expired; }, true);
    default:
      return subjectMatcher(token);
  }
}

// Compile a token, handling `-' (the negation covers the whole token,
// comma alternatives included) and fully quoted tokens (a literal
// subject search: a colon inside the quotes is not a key separator).
// Port of gnaw--query-compile-token.
function compileToken(token) {
  if (token.length > 1 && token[0] === '-') {
    var inner = compileToken(token.substring(1));
    return function(rpt) { return !inner(rpt); };
  }
  if (quotedValue(token) !== null) return subjectMatcher(token);
  return compileKey(token);
}

// Compile RAW into clauses of {preds, includeClosed}.  closed:true /
// c:true (values compare case-insensitively, like everywhere else)
// bypasses the Open filter without touching the button: the marker
// lives in the query, so clearing it restores the previous state.
// Any other token reading the closed state -- closed:<who>, flags:
// naming a close reason (C R E S) -- also raises includeClosed, so
// the closed reports get loaded: an Open-only answer to them would
// lie.  Keys stay case-sensitive (S: and s: differ).
function compileQuery(raw) {
  return parseQuery(raw).map(function(tokens) {
    var clause = { preds: [], includeClosed: false };
    tokens.forEach(function(tok) {
      var t = tok[0] === '-' ? tok.substring(1) : tok;
      var i = t.indexOf(':');
      var key = i === -1 ? null : t.substring(0, i);
      var val = i === -1 ? '' : t.substring(i + 1);
      if (key === 'closed' || key === 'c') {
        clause.includeClosed = true;
        // The unnegated include-closed trigger matches everything.
        if (t === tok && val.toLowerCase() === 'true') return;
      } else if ((key === 'flags' || key === 'F') && /[cres]/i.test(val)) {
        clause.includeClosed = true;
      }
      clause.preds.push(compileToken(tok));
    });
    return clause;
  });
}

// A report matches CLAUSES when some clause's predicates all hold; an
// empty query (no clause) matches everything.  This is the matching
// semantics itself: the page's matchReport and the conformance tests
// must both go through it.
function clauseMatches(rpt, clause) {
  return clause.preds.every(function(p) { return p(rpt); });
}

function matchClauses(rpt, clauses) {
  return clauses.length === 0 || clauses.some(function(c) {
    return clauseMatches(rpt, c);
  });
}

function clausesIncludeClosed(clauses) {
  return clauses.some(function(c) { return c.includeClosed; });
}

/* ── Report search fields ────────────────────────────────────── */

var _relationKinds = ['resolves', 'resolved-by', 'supersedes',
                      'superseded-by', 'duplicates', 'duplicated-by',
                      'related-to'];

// The fields the compiled predicates read, extracted from a raw
// reports.json entry and lowercased once.  bone-index.js merges them
// into its prepared reports; the conformance tests build them directly.
// att mirrors the gnaw.el Att column, sans spaces: awaiting (.),
// related (~), then one attachment glyph -- + one patch, x several,
// @ calendar events, # plain-text files.
function searchFields(r) {
  var subject = r.subject || '';
  var patches = r.patches || [];
  var related = _relationKinds.some(function(k) {
    return r[k] && r[k].length;
  });
  return {
    type: r.type || '',
    subject: subject.toLowerCase(),
    subjectWords: subjectWords(subject),
    from: (r.from || '').toLowerCase(),
    fromName: (r['from-name'] || '').toLowerCase(),
    mid: (r['message-id'] || '').toLowerCase(),
    topics: (r.topic || '').toLowerCase().split(/\s+/).filter(Boolean),
    source: (r.source || '').toLowerCase(),
    acked: (r.acked || '').toLowerCase(),
    owned: (r.owned || '').toLowerCase(),
    ownedName: (r['owned-name'] || '').toLowerCase(),
    closedby: (r.closed || '').toLowerCase(),
    urgent: (r.urgent || '').toLowerCase(),
    important: (r.important || '').toLowerCase(),
    priority: r.priority || 0,
    flags: (r.flags || '').toUpperCase(),
    att: (r.awaiting ? '.' : '') + (related ? '~' : '') +
         (patches.length > 1 ? 'x' : patches.length === 1 ? '+' :
          (r.events || []).length ? '@' : (r.texts || []).length ? '#' : ''),
    date: parseIsoDate(r['date-raw'] || r.date || ''),
    deadline: r.deadline || '',
    // expired: filters on the upcoming expiry (like gnaw.el), not on
    // the past expired-date of already-expired reports.
    expired: r.expiry || ''
  };
}

// Under node (the conformance tests), export the engine; in the
// browser everything above is already global.
if (typeof module !== 'undefined' && module.exports) {
  module.exports = {
    parseQuery: parseQuery,
    compileQuery: compileQuery,
    clauseMatches: clauseMatches,
    matchClauses: matchClauses,
    clausesIncludeClosed: clausesIncludeClosed,
    searchFields: searchFields,
    subjectWords: subjectWords,
    localDate: localDate
  };
}
