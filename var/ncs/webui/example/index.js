window.onerror = function (errorMsg, url, lineNumber) {
  window.alert('Error: ' +
               errorMsg +
               ' Script: ' +
               url +
               ' Line: ' +
               lineNumber);
};

requirejs.config({
  paths: {
    jquery: 'https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery',
    lodash: 'https://cdnjs.cloudflare.com/ajax/libs/lodash.js/3.3.1/lodash',
    bluebird: 'https://cdnjs.cloudflare.com/ajax/libs/bluebird/2.9.12/bluebird'
  }
});

require(['example']);
