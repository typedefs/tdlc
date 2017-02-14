'use strict';

var crypto = require('crypto');

exports.sha256 = function(buf) {
  var hash = crypto.createHash('sha256');
  hash.update(buf);
  return hash.digest();
};
