"use strict";

export const showStatsObj = require("util").inspect;

export function statsMethod(m, s) {
  return s[m]();
}
