// bark-stats.js — Vega-Lite chart rendering with theme support.
// Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
// SPDX-License-Identifier: MPL-2.0
//
// Expects:
//   barkSpecs — object populated by inline <script> tags (id -> vega-lite spec)
//   toggleTheme() from bark-theme.js (extended here to re-render charts)

var barkSpecs = {};

function barkConfig() {
  var s = getComputedStyle(document.documentElement);
  var v = function(k) { return s.getPropertyValue(k).trim(); };
  var dark = document.documentElement.getAttribute('data-theme') === 'dark';
  var fg   = v('--pico-color') || (dark ? '#c8c8c8' : '#444');
  var muted = v('--pico-muted-border-color') || (dark ? '#555' : '#ddd');
  var c1 = v('--bark-chart-1');
  var c2 = v('--bark-chart-2');
  var c3 = v('--bark-chart-3');
  var c4 = v('--bark-chart-4');
  var c5 = v('--bark-chart-5');
  var palette = [c1, c2, c3, c4, c5].filter(Boolean);
  // Fall back to Vega defaults if no CSS vars found
  if (palette.length === 0) palette = undefined;
  return {
    background: 'transparent',
    title:  { color: fg },
    axis:   { labelColor: fg, titleColor: fg, gridColor: muted, domainColor: muted, tickColor: muted },
    legend: { labelColor: fg, titleColor: fg },
    view:   { stroke: muted },
    range:  palette ? { category: palette } : {},
    bar:    palette ? { fill: palette[0] } : {},
    line:   palette ? { stroke: palette[0] } : {},
    arc:    {},
    point:  palette ? { fill: palette[0] } : {}
  };
}

function barkRenderAll() {
  var cfg = barkConfig();
  Object.entries(barkSpecs).forEach(function(kv) {
    vegaEmbed('#' + kv[0], kv[1], {actions: false, renderer: 'svg', config: cfg});
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
