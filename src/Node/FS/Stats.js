"use strict";

export var showStatsObj = require("util").inspect;

export function statsMethod(m, s) {
  return s[m]();
}
