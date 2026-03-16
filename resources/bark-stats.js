// bark-stats.js — Vega-Lite chart rendering with theme support.
// Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
// SPDX-License-Identifier: MPL-2.0
//
// Expects:
//   barkSpecs — object populated by inline <script> tags (id -> vega-lite spec)
//   toggleTheme() from bark-theme.js (extended here to re-render charts)

var barkSpecs = {};

function barkTheme() {
  return document.documentElement.getAttribute('data-theme') === 'dark' ? 'dark' : 'excel';
}

function barkRenderAll() {
  Object.entries(barkSpecs).forEach(function(kv) {
    vegaEmbed('#' + kv[0], kv[1], {actions: false, renderer: 'svg', theme: barkTheme()});
  });
}

// Wait for DOMContentLoaded so bark-theme.js has defined toggleTheme.
// Then wrap it to also re-render charts on theme change.
document.addEventListener('DOMContentLoaded', function() {
  barkRenderAll();
  if (typeof toggleTheme === 'function') {
    var origToggle = toggleTheme;
    toggleTheme = function() { origToggle(); barkRenderAll(); };
  }
});
