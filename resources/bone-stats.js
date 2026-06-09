// bone-stats.js -- Vega-Lite chart rendering with theme support.
// Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
// SPDX-License-Identifier: MPL-2.0
//
// data.html is a static shell: on load we fetch stats.json (its location
// discovered via reports/meta.json) and build the KPI cards and charts
// client-side from its `kpis` and `charts` fields.
//   toggleTheme() from bone-theme.js (extended here to re-render charts)

var boneSpecs = {};

function boneConfig() {
  var s = getComputedStyle(document.documentElement);
  var v = function(k) { return s.getPropertyValue(k).trim(); };
  var dark = document.documentElement.getAttribute('data-theme') === 'dark';
  var fg   = v('--pico-color') || (dark ? '#c8c8c8' : '#444');
  var muted = v('--pico-muted-border-color') || (dark ? '#555' : '#ddd');
  var c1 = v('--bone-chart-1');
  var c2 = v('--bone-chart-2');
  var c3 = v('--bone-chart-3');
  var c4 = v('--bone-chart-4');
  var c5 = v('--bone-chart-5');
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

function boneRenderAll() {
  var cfg = boneConfig();
  Object.entries(boneSpecs).forEach(function(kv) {
    vegaEmbed('#' + kv[0], kv[1], {actions: false, renderer: 'svg', config: cfg});
  });
}

// Vega's default tooltip handler is mouse-driven; on touch devices a tap
// triggers mouseover but no mouseout, so the tooltip sticks (and worse,
// stays on top while the user scrolls). Force-hide it on scroll or on any
// tap outside a chart.
function boneHideTooltip() {
  var el = document.getElementById('vg-tooltip-element');
  if (el) el.classList.remove('visible');
}

// ── Build the page from the fetched stats payload ───────────────
function boneKpiEl(k) {
  var div = document.createElement('div');
  div.className = 'kpi';
  [['kpi-v', k.v], ['kpi-l', k.l], ['kpi-s', k.s]].forEach(function(pair) {
    if (pair[0] === 'kpi-s' && !k.s) return;
    var el = document.createElement('div');
    el.className = pair[0];
    el.textContent = pair[1];
    div.appendChild(el);
  });
  return div;
}

function boneBuildStats(stats) {
  var kpiArea = document.getElementById('kpi-area');
  if (kpiArea && stats.kpis) {
    kpiArea.textContent = '';
    stats.kpis.forEach(function(k) { kpiArea.appendChild(boneKpiEl(k)); });
  }
  var grid = document.getElementById('chart-grid');
  boneSpecs = {};
  if (grid && stats.charts) {
    grid.textContent = '';
    stats.charts.forEach(function(c) {
      var box = document.createElement('div'); box.className = 'box';
      var chart = document.createElement('div'); chart.className = 'chart'; chart.id = c.id;
      box.appendChild(chart);
      grid.appendChild(box);
      boneSpecs[c.id] = c.spec;
    });
  }
  var gen = document.getElementById('generated-at');
  if (gen && stats['generated-at']) gen.textContent = 'Generated ' + stats['generated-at'];
  boneRenderAll();
}

// Discover the stats file via meta.json (the per-source manifest), then
// fetch it. Falls back to stats.json if the manifest omits the list.
function boneLoadStats() {
  fetch('reports/meta.json')
    .then(function(r) { return r.json(); })
    .then(function(meta) {
      var files = (meta && meta['stats-files']) || ['stats.json'];
      return fetch('reports/' + files[0]).then(function(r) { return r.json(); });
    })
    .then(boneBuildStats)
    .catch(function(err) { console.error('Failed to load stats:', err); });
}

// Wait for DOMContentLoaded so bone-theme.js has defined toggleTheme.
// Then wrap it to also re-render charts on theme change.
document.addEventListener('DOMContentLoaded', function() {
  if (typeof toggleTheme === 'function') {
    var origToggle = toggleTheme;
    toggleTheme = function() { origToggle(); boneRenderAll(); };
  }
  window.addEventListener('scroll', boneHideTooltip, {passive: true});
  document.addEventListener('touchstart', function(e) {
    if (!e.target.closest || !e.target.closest('.chart')) boneHideTooltip();
  }, {passive: true});
  boneLoadStats();
});
