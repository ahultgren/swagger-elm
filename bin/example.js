'use strict';

var log = console.log.bind(console);
var outputRegex = /^[^:]*: "(.*)"$/;
console.log = function (output) {
  var match = output.match(outputRegex);

  if(match) {
    log(match[1]);
  }
  else {
    console.error(output);
  }
};

require('../dist/elm.js');
