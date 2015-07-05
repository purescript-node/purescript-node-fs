/* global require */
/* global exports */
"use strict";

// module Node.FS.Async

exports.fs = require('fs');

exports.handleCallbackImpl = function (left, right, f) {
  return function (err, value) {
    if (err) {
      f(left(err))();
    } else {
      f(right(value))();
    }
  };
};
