// bark-theme.js — Shared dark/light theme toggle.
// Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
// SPDX-License-Identifier: MPL-2.0

(function() {
  var html = document.documentElement;
  if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
    html.setAttribute('data-theme', 'dark');
    document.addEventListener('DOMContentLoaded', function() {
      var icon = document.getElementById('theme-icon');
      if (icon) icon.textContent = '☀️';
    });
  }
})();

function toggleTheme() {
  var html = document.documentElement;
  var next = html.getAttribute('data-theme') === 'dark' ? 'light' : 'dark';
  html.setAttribute('data-theme', next);
  document.getElementById('theme-icon').textContent = next === 'dark' ? '☀️' : '🌙';
}
