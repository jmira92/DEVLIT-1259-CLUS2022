/*jshint devel:true*/
// !!!
// The following code is purely for example purposes.
// The code has inline comments for a better understanding.
// Your mileage might vary.
// !!!

define([
  'jquery',
  'lodash'
], function(
  $,
  _
) {
  'use strict';

  var JsonRpc;

  JsonRpc = function(params) {
    $.extend(this, {
      // API

      // Call a JsonRpc method with params
      call: undefined,

      // API (OPTIONAL)

      // Decide what to do when there is no active session
      onNoSession: undefined,
      // Decide what to do when the request errors
      onError: undefined,
      // Set an id to start using in requests
      id: 0,
      // Set another url for the JSON-RPC API
      url: '/jsonrpc',


      // INTERNAL

      makeOnCallDone: undefined,
      makeOnCallFail: undefined
    }, params || {});

    _.bindAll(this, [
      'call',
      'onNoSession',
      'onError',
      'makeOnCallDone',
      'makeOnCallFail'
    ]);
  };

  JsonRpc.prototype = {
    call: function(method, params, timeout) {
      var deferred = $.Deferred();

      // Easier to associate request/response logs
      // when the id is unique to each request
      this.id = this.id + 1;

      $.ajax({
        // HTTP method is always POST for the JSON-RPC API
        type: 'POST',
        // Let's show <method> rather than just "jsonrpc"
        // in the browsers' Developer Tools - Network tab - Name column
        url: this.url + '/' + method,
        // Content-Type is mandatory
        // and is always "application/json" for the JSON-RPC API
        contentType: 'application/json',
        // Optionally set a timeout for the request
        timeout: timeout,
        // Request payload
        data: JSON.stringify({
          jsonrpc: '2.0',
          id: this.id,
          method: method,
          params: params
        }),
        dataType: 'json',
        // Just in case you are doing cross domain requests
        // NOTE: make sure you are setting CORS headers similarly to
        // --
        // Access-Control-Allow-Origin: http://server1.com, http://server2.com
        // Access-Control-Allow-Credentials: true
        // Access-Control-Allow-Headers: Origin, Content-Type, Accept
        // Access-Control-Request-Method: POST
        // --
        // if you want to allow JSON-RPC calls from server1.com and server2.com
        crossDomain: true,
        xhrFields: {
          withCredentials: true
        }
      })
      // When done, or on failure,
      // call a function that has access to both
      // the request and the response information
      .done(this.makeOnCallDone(method, params, deferred))
      .fail(this.makeOnCallFail(method, params, deferred));
      return deferred.promise();
    },

    makeOnCallDone: function(method, params, deferred) {
      var me = this;

      return function(reply/*, status, xhr*/) {
        if (reply.error) {
          return me.onError(method, params, deferred, reply);
        }
        deferred.resolve(reply.result);
      };
    },

    onNoSession: function() {
      // It is common practice that when missing a session identifier
      // or when the session crashes or it times out due to inactivity
      // the user is taken back to the login page
      _.defer(function() {
        window.location.href = 'login.html';
      });
    },

    onError: function(method, params, deferred, reply) {
      if (reply.error.type === 'session.missing_sessionid' ||
          reply.error.type === 'session.invalid_sessionid') {
        this.onNoSession();
      }

      deferred.reject(reply.error);
    },

    makeOnCallFail: function(method, params, deferred) {
      return function(xhr, status, errorMessage) {
        var error;

        error = $.extend(new Error(errorMessage), {
          type: 'ajax.response.error',
          detail: JSON.stringify({method: method, params: params})
        });

        deferred.reject(error);
      };
    }
  };

  return JsonRpc;
});

// Local Variables:
// mode: js
// js-indent-level: 2
// End:
