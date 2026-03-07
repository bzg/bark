// bark-stats.js — Vega-Lite chart rendering with theme support.
//
// Expects:
//   barkSpecs — object populated by inline <script> tags (id → vega-lite spec)
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

// Patch toggleTheme to also re-render charts
var _origToggleTheme = typeof toggleTheme === 'function' ? toggleTheme : null;
function toggleTheme() {
  if (_origToggleTheme) _origToggleTheme();
  barkRenderAll();
}

document.addEventListener('DOMContentLoaded', barkRenderAll);
