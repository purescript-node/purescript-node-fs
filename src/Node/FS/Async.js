/* global require */
/* global exports */
"use strict";

// module Node.FS.Async

var fs = require('fs');
exports.fs = fs

exports.handleCallbackImpl = function (left, right, f) {
  return function (err, value) {
    if (err) {
      f(left(err))();
    } else {
      f(right(value))();
    }
  };
};
