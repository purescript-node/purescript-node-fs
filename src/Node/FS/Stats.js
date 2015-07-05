/* global require */
/* global exports */
"use strict";

// module Node.FS.Stats

exports.showStatsObj = require('util').inspect;

exports.statsMethod = function (m, s) {
  return s[m]();
}
