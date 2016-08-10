'use strict';

var log = console.log.bind(console);
var outputRegex = /^result: "(.*)"$/;
console.log = function (output) {
  var match = output.match(outputRegex);

  if(match) {
    log(match[1].replace(/\\"/g, '"'));
  }
  else {
    console.error(output);
  }
};

require('../dist/elm.js');
