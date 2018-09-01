"use strict";

exports.createReadStreamImpl = function (Left) {
  return function (Right) {
    return function (path) {
      return function (options) {
        return function () {
          try {
            return Right(require("fs").createReadStream(path, options));
          } catch (e) {
            return Left(e);
          }
        };
      };
    };
  };
};
