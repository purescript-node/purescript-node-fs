/* global require */
/* global exports */
"use strict";

// module Node.FS.Async

var fs = require('fs');
exports.fs = fs

exports.create = fs.open;
exports.readSeq = fs.read;
exports.writeSeq = fs.write;

exports.readSeq = function(fd, buffer, offset, length, callback) {
  fs.read(fd, buffer, offset, length, null, callback);
}

exports.writeSeq = function(fd, buffer, offset, length, callback) {
  fs.write(fd, buffer, offset, length, null, callback);
}


exports.handleCallbackImpl = function (left, right, f) {
  return function (err, value) {
    if (err) {
      f(left(err))();
    } else {
      f(right(value))();
    }
  };
};
