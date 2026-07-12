// bone-search-conformance.js -- Conformance cases for the query engine.
// Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
// SPDX-License-Identifier: MPL-2.0
//
// Run with `bb test-search` (or `node test/bone-search-conformance.js`).
// Each case maps a query to the message-ids it must match against the
// fixture reports below.  The fixtures go through searchFields, the
// exact preparation the index page applies to reports.json entries, so
// these cases pin the whole engine, tokenizer included.  They are
// meant to be reusable as an ERT fixture by gnaw.el one day, keeping
// both implementations of the shared syntax convergent.

var engine = require('../resources/bone-search.js');

function iso(daysFromToday) {
  var d = new Date();
  d.setDate(d.getDate() + daysFromToday);
  return engine.localDate(d);
}

// A mini reports.json: two sources, mixed types, flags, topics,
// relations, patches, deadlines relative to today so duration
// filters stay stable over time.
var rawReports = [
  {'message-id': '<crash@org>', type: 'bug', subject: '[BUG] Agenda crash on export',
   from: 'alice@example.org', 'from-name': 'Alice Doe', date: iso(-2),
   flags: 'AO-', priority: 3, acked: 'bob@example.org', owned: 'bob@example.org',
   'owned-name': 'Bob Maintainer', urgent: 'bob@example.org',
   important: 'bob@example.org', topic: 'agenda export', source: 'Org mode ML',
   awaiting: true},
  {'message-id': '<patch@org>', type: 'patch',
   subject: '[PATCH v2] org-element--cache: Fix agenda export crash',
   from: 'carol@example.org', 'from-name': 'Carol Lispian', date: iso(-10),
   flags: 'A--', priority: 1, acked: 'bob@example.org',
   important: 'bob@example.org', topic: 'agenda', source: 'Org mode ML',
   expiry: iso(5), patches: [{file: '0001.patch'}],
   'related-to': [{'message-id': '<crash@org>'}]},
  {'message-id': '<req@emacs>', type: 'request', subject: 'Re: preamble shuffling in LaTeX',
   from: 'dave@example.net', 'from-name': 'Dave Requester', date: iso(-40),
   flags: '---', priority: 0, topic: 'latex', source: 'Emacs devel',
   deadline: iso(20),
   events: [{file: 'meet.ics'}]},
  {'message-id': '<done@emacs>', type: 'bug', subject: 'LaTeX preamble shuffling crash',
   from: 'alice@example.org', 'from-name': 'Alice Doe', date: iso(-60),
   flags: 'A-R', priority: 2, acked: 'erin@example.net',
   closed: 'erin@example.net', 'close-reason': 'resolved',
   urgent: 'erin@example.net', topic: 'latex', source: 'Emacs devel',
   patches: [{file: '0001.patch'}, {file: '0002.patch'}]}
];

var reports = rawReports.map(function(r) {
  return engine.searchFields(r);
});

// query -> expected matching mids, in fixture order.  The engine sees
// every report (the Open toggle is page chrome, outside the engine).
// An optional third element asserts the clauses' includeClosed flag:
// true for any closed:/flags:C,R,E,S token (the page then loads the
// closed reports), with closed:true / c:true as the match-all trigger.
var cases = [
  // Bare words search the subject only, each word one AND token.
  ['crash',                    ['<crash@org>', '<patch@org>', '<done@emacs>']],
  ['agenda crash',             ['<crash@org>', '<patch@org>']],
  ['AGENDA',                   ['<crash@org>', '<patch@org>']],
  // Quoted phrase: one token, spaces kept; colon inside is no key.
  ['"preamble shuffling"',     ['<req@emacs>', '<done@emacs>']],
  ['"shuffling in"',           ['<req@emacs>']],
  // Clauses: | is OR, comma inside a value is OR, space is AND.
  ['latex | agenda',           ['<crash@org>', '<patch@org>', '<req@emacs>', '<done@emacs>']],
  ['T:latex s:crash',          ['<done@emacs>']],
  ['topic:latex,agenda',       ['<crash@org>', '<patch@org>', '<req@emacs>', '<done@emacs>']],
  // Negation covers the whole token, alternatives included.
  ['-topic:latex,agenda',      []],
  ['-type:patch crash',        ['<crash@org>', '<done@emacs>']],
  // Text fields: substring, *, /regexp/, "literal".
  ['from:alice',               ['<crash@org>', '<done@emacs>']],
  ['f:doe',                    ['<crash@org>', '<done@emacs>']],
  ['subject:/v[0-9]+/',        ['<patch@org>']],
  ['s:/^re:/',                 ['<req@emacs>']],
  ['s:"latex preamble"',       ['<done@emacs>']],
  ['s:/[/',                    []],          // invalid regexp: no match
  ['mid:crash@org',            ['<crash@org>']],
  ['message-id:crash@org',     ['<crash@org>']], // the pre-port long alias
  // Person fields: * = anyone, never an unset actor; keyword values
  // compare case-insensitively.
  ['acked:*',                  ['<crash@org>', '<patch@org>', '<done@emacs>']],
  ['acked:True',               ['<crash@org>', '<patch@org>', '<done@emacs>']],
  ['owned:bob',                ['<crash@org>']],
  ['o:maintainer',             ['<crash@org>']], // owned-name matches too
  ['closed:*',                 ['<done@emacs>'], true],
  ['closed:erin',              ['<done@emacs>'], true],
  ['-closed:*',                ['<crash@org>', '<patch@org>', '<req@emacs>'], true],
  // The include-closed trigger matches everything, whatever the case.
  ['closed:true',              ['<crash@org>', '<patch@org>', '<req@emacs>', '<done@emacs>'], true],
  ['c:True',                   ['<crash@org>', '<patch@org>', '<req@emacs>', '<done@emacs>'], true],
  ['c:true mid:crash',         ['<crash@org>'], true],
  // Priority is exact; urgent/important are set-or-not marks.
  ['priority:3',               ['<crash@org>']],
  ['p:1,2',                    ['<patch@org>', '<done@emacs>']],
  ['p:0',                      ['<req@emacs>']],
  ['urgent:*',                 ['<crash@org>', '<done@emacs>']],
  ['urgent:TRUE',              ['<crash@org>', '<done@emacs>']],
  ['important:False',          ['<req@emacs>', '<done@emacs>']],
  ['u:whoever',                []],          // only */true/false mean anything
  // Closed sets, compared whole: no *, quotes or regexps.
  ['type:patch',               ['<patch@org>']],
  ['t:patch',                  ['<patch@org>']],
  ['type:bug,request',         ['<crash@org>', '<req@emacs>', '<done@emacs>']],
  ['type:*',                   []],
  ['flags:AO',                 ['<crash@org>'], false],
  ['flags:R',                  ['<done@emacs>'], true],
  ['F:c,r',                    ['<done@emacs>'], true],
  ['F:a',                      ['<crash@org>', '<patch@org>', '<done@emacs>'], false],
  // Att glyphs: . awaiting, ~ related, + one patch, x several,
  // @ events, # texts.
  ['att:.',                    ['<crash@org>']],
  ['att:~+',                   ['<patch@org>']],
  ['A:x',                      ['<done@emacs>']],
  ['A:@',                      ['<req@emacs>']],
  // similar: at least 3 of the +-joined words, tags like [BUG] dropped.
  ['similar:agenda+crash+export',    ['<crash@org>', '<patch@org>']],
  ['similar:latex+preamble+shuffling', ['<req@emacs>', '<done@emacs>']],
  ['similar:agenda+crash',     ['<crash@org>', '<patch@org>']], // 2 words: all required
  // Object.prototype names must not count as shared subject words.
  ['similar:agenda+export+constructor', []],
  // source: one char = exact name (gnaw.el also tries its local
  // letters); longer = substring like topic:.
  ['source:org',               ['<crash@org>', '<patch@org>']],
  ['S:emacs',                  ['<req@emacs>', '<done@emacs>']],
  ['source:*',                 ['<crash@org>', '<patch@org>', '<req@emacs>', '<done@emacs>']],
  ['S:x',                      []],          // no one-char source name
  // Dates: a lone date is that exact day, durations window on today,
  // open bounds stay open.
  ['date:' + iso(-2),          ['<crash@org>']],
  ['date:3d',                  ['<crash@org>']],
  ['d:15d',                    ['<crash@org>', '<patch@org>']],
  ['d:' + iso(-45) + '..',     ['<crash@org>', '<patch@org>', '<req@emacs>']],
  ['d:..' + iso(-45),          ['<done@emacs>']],
  ['d:garbage',                []],
  ['d:1-1-1..' + iso(0),       []],         // invalid bound: no match, not open
  ['deadline:2m',              ['<req@emacs>']],
  ['D:' + iso(19) + '..' + iso(21), ['<req@emacs>']],
  ['expired:10d',              ['<patch@org>']],
  ['expired:1d',               []],
  // Empty values match nothing, whatever the key.
  ['s:',                       []],
  ['type:',                    []],
  ['source:',                  []],
  ['d:',                       []],
  // Unknown keys search the subject as bare words do.
  ['nosuchkey:crash',          []],
  // Trailing | drops the empty clause instead of matching everything.
  ['crash |',                  ['<crash@org>', '<patch@org>', '<done@emacs>']]
];

var failures = 0;

// One loop, going through the engine's own matching helpers
// (matchClauses, clausesIncludeClosed) -- the same ones matchReport
// uses in the page, so the suite cannot drift from it.
function runCase(query, expected, wantClosed) {
  var clauses = engine.compileQuery(query);
  var got = [];
  reports.forEach(function(rpt) {
    if (engine.matchClauses(rpt, clauses)) got.push(rpt.mid);
  });
  if (JSON.stringify(got) !== JSON.stringify(expected)) {
    failures++;
    console.error('FAIL ' + JSON.stringify(query));
    console.error('  expected ' + JSON.stringify(expected));
    console.error('  got      ' + JSON.stringify(got));
  }
  if (wantClosed !== undefined) {
    var inc = engine.clausesIncludeClosed(clauses);
    if (inc !== wantClosed) {
      failures++;
      console.error('FAIL ' + JSON.stringify(query) +
                    ' includeClosed: expected ' + wantClosed + ', got ' + inc);
    }
  }
}

cases.forEach(function(c) { runCase(c[0], c[1], c[2]); });

if (failures > 0) {
  console.error(failures + ' failure(s) out of ' + cases.length + ' cases');
  process.exit(1);
}
console.log(cases.length + ' search conformance cases passed');
