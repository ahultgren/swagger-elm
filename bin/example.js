'use strict';

var log = console.log.bind(console);
var outputRegex = /^[^:]*: "(.*)"$/;
console.log = function (output) {
  log(output.match(outputRegex)[1]);
};

require('../dist/elm.js');
