/*jshint devel:true*/
// !!!
// The following code is purely for example purposes.
// The code has inline comments for a better understanding.
// Your mileage might vary.
// !!!

define([
  'lodash',
  'bluebird',
  './JsonRpc',
  './Comet'
], function(
  _,
  Promise,
  JsonRpc,
  Comet
) {
  'use strict';

  // CHANGE AT LEAST THESE
  // IN ORDER TO MAKE THIS EXAMPLE WORK IN YOUR SETUP
  var jsonrpcUrl = '/jsonrpc', // 'http://localhost:8008/jsonrpc';
      path = '/dhcp:dhcp/max-lease-time',
      value = Math.floor(Math.random() * 800) + 7200;

  var log,
      jsonRpc,
      comet,
      funs = {},
      ths = {
        read: undefined,
        webui: undefined
      };

  // UTILITY
  log = function(msg) {
    document.body.innerHTML =
      '<pre>' +
      msg +
      '</pre>' +
      document.body.innerHTML;
  };

  // SETUP
  jsonRpc = new JsonRpc({
    url: jsonrpcUrl,
    onError: function(method, params, deferred, reply) {
      var error = reply.error,
          msg = [method,
                 params,
                 reply.id,
                 error.code,
                 error.type,
                 error.message
                ];

      if (method === 'comet') {
        return;
      }

      window.alert('JsonRpc error: ' + msg);
    }
  });

  comet = new Comet({
    jsonRpc: jsonRpc,
    onError: function(reply) {
      var error = reply.error,
          msg = [reply.id, error.code, error.type, error.message];

      window.alert('Comet error: ' + msg);
    }
  });

  // CALLS FOR A COMMON SCENARIO
  funs.login = function() {
    log('Logging in as admin:admin...');
    return jsonRpc.call('login', {
      user: 'admin',
      passwd: 'admin'
    }).done(function() {
      log('Logged in.');
    });
  };

  funs.getSystemSetting = function() {
    log('Getting system settings...');
    return jsonRpc.call('get_system_setting').done(function(result) {
      log(JSON.stringify(result, null, 1));
    });
  };

  funs.newReadTrans = function() {
    log('Create a new read-only transaction...');
    return jsonRpc.call('new_read_trans', {
      db: 'running'
    }).done(function(result) {
      ths.read = result.th;
      log('Read-only transaction with th (transaction handle) id: ' +
          result.th + '.');
    });
  };

  funs.newWebuiTrans = function() {
    log('Create a new webui (read-write) transaction...');
    return jsonRpc.call('new_webui_trans', {
      conf_mode: 'private',
      db: 'candidate'
    }).done(function(result) {
      ths.webui = result.th;
      log('webui (read-write) transaction with th (transaction handle) id: ' +
          result.th + '.');
    });
  };

  funs.getValue = function(args /*{th, path}*/) {
    log('Get value for ' + args.path + ' in ' + args.th + ' transaction...');
    if (typeof args.th === 'string') {
      args.th = ths[args.th];
    }
    return jsonRpc.call('get_value', {
      th: args.th,
      path: path
    }).done(function(result) {
      log(path + ' is now set to: ' + result.value + '.');
    });
  };

  funs.setValue = function(args /*{th, path, value}*/) {
    log('Set value for ' + args.path +
        ' to ' + args.value +
        ' in ' + args.th + ' transaction...');
    if (typeof args.th === 'string') {
      args.th = ths[args.th];
    }
    return jsonRpc.call('set_value', {
      th: args.th,
      path: path,
      value: args.value
    }).done(function(result) {
      log(path + ' is now set to: ' + result.value + '.');
    });
  };

  funs.validate = function(args /*{th}*/) {
    log('Validating changes in ' + args.th + ' transaction...');
    if (typeof args.th === 'string') {
      args.th = ths[args.th];
    }
    return jsonRpc.call('validate_commit', {
      th: args.th
    }).done(function() {
      log('Validated.');
    });
  };

  funs.commit = function(args /*{th}*/) {
    log('Commiting changes in ' + args.th + ' transaction...');
    if (typeof args.th === 'string') {
      args.th = ths[args.th];
    }
    return jsonRpc.call('commit', {
      th: args.th
    }).done(function() {
      log('Commited.');
    });
  };

  funs.subscribeChanges = function(args /*{path, handle}*/) {
    log('Subcribing to changes of ' + args.path +
        ' (with handle ' + args.handle + ')...');
    return jsonRpc.call('subscribe_changes', {
      comet_id: comet.id,
      path: args.path,
      handle: args.handle,
    }).done(function(result) {
      log('Subscribed with handle id ' + result.handle + '.');
    });
  };

  // RUN
  Promise.resolve([
    funs.login,
    funs.getSystemSetting,
    funs.newReadTrans,
    function() {
      return funs.getValue({th: 'read', path: path});
    },
    function() {
      var handle = comet.id + '-max-lease-time';
      comet.on(handle, function(msg) {
        log('>>> Notification >>>\n' +
            JSON.stringify(msg, null, 2) +
            '\n<<< Notification <<<');
      });
      return funs.subscribeChanges({th: 'read', path: path, handle: handle});
    },
    funs.newWebuiTrans,
    function() {
      return funs.setValue({th: 'webui', path: path, value: value.toString()});
    },
    function() {
      return funs.getValue({th: 'webui', path: path});
    },
    function() {
      return funs.validate({th: 'webui'});
    },
    function() {
      return funs.commit({th: 'webui'});
    },
    function() {
      return new Promise(function(resolve) {
        log('Waiting 2.5 seconds before one last call to get_value');
        window.setTimeout(function() {
          resolve();
        }, 2500);
      });
    },
    function() {
      return funs.getValue({th: 'read', path: path});
    },
  ]).each(function(fn){
    return fn().then(function() {
      log('--------------------------------------------------');
    });
  });
});


// Local Variables:
// mode: js
// js-indent-level: 2
// End:
