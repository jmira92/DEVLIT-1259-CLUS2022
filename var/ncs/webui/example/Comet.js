/*jshint devel:true*/
// !!!
// The following code is purely for example purposes.
// The code has inline comments for a better understanding.
// Your mileage might vary.
// !!!

define([
  'jquery',
  'lodash',
  './JsonRpc'
], function(
  $,
  _,
  JsonRpc
) {
  'use strict';

  var Comet;

  Comet = function(params) {
    $.extend(this, {
      // API

      // Add a callback for a notification handle
      on: undefined,
      // Remove a specific callback or all callbacks for a notification handle
      off: undefined,
      // Stop all comet notifications
      stop: undefined,

      // API (OPTIONAL)

      // Decide what to do when the comet errors
      onError: undefined,
      // Optionally set a different id for this comet channel
      id: 'main-1.' + String(Math.random()).substring(2),
      // Optionally give an existing JsonRpc client
      jsonRpc: new JsonRpc(),
      // Optionally wait 1 second in between polling the comet channel
      sleep: 1 * 1000,

      // INTERNAL

      handlers: [],
      polling: false,
      poll: undefined,
      onPollDone: undefined,
      onPollFail: undefined
    }, params || {});

    _.bindAll(this, [
      'on',
      'off',
      'stop',
      'onError',
      'poll',
      'onPollDone',
      'onPollFail'
    ]);
  };

  Comet.prototype = {
    on: function(handle, callback) {
      if (!callback) {
        throw new Error('Missing a callback for handle ' + handle);
      }

      // Add a handler made of handle id and a callback function
      this.handlers.push({handle: handle, callback: callback});

      // Start polling
      _.defer(this.poll);
    },

    off: function(handle, callback) {
      if (!handle) {
        throw new Error('Missing a handle');
      }

      // Remove all handlers matching the handle,
      // and optionally also the callback function
      _.remove(this.handlers, {handle: handle, callback: callback});

      // If there are no more handlers matching the handle,
      // then unsubscribe from notifications, in order to releave
      // the server and the network from redundancy
      if (!_.find(this.handlers, {handle: handle}).length) {
        this.jsonRpc.call('unsubscribe', {handle: handle});
      }
    },

    stop: function() {
      var me = this,
          deferred = $.Deferred(),
          deferreds = [];

      if (this.polling) {
        // Unsubcribe from all known notifications, in order to releave
        // the server and the network from redundancy
        _.each(this.handlers, function(handler) {
          deferreds.push(me.jsonRpc('unsubscribe', {
            handle: handler.handle
          }));
        });

        $.when.apply($, deferreds).done(function() {
          deferred.resolve();
        }).fail(function(err) {
          deferred.reject(err);
        }).always(function() {
          me.polling = false;
          me.handlers = [];
        });
      } else {
        deferred.resolve();
      }

      return deferred.promise();
    },

    poll: function() {
      var me = this;

      if (this.polling) {
        return;
      }

      this.polling = true;

      this.jsonRpc.call('comet', {
        comet_id: this.id
      }).done(function(notifications) {
        me.onPollDone(notifications);
      }).fail(function(err) {
        me.onPollFail(err);
      }).always(function() {
        me.polling = false;
      });
    },

    onPollDone: function(notifications) {
      var me = this;

      // Polling has stopped meanwhile
      if (!this.polling) {
        return;
      }

      _.each(notifications, function(notification) {
        var handle = notification.handle,
            message = notification.message,
            handlers = _.where(me.handlers, {handle: handle});

        // If we received a notification that we cannot handle,
        // then unsubcribe from it, in order to releave
        // the server and the network from redundancy
        if (!handlers.length) {
          return this.jsonRpc.call('unsubscribe', {handle: handle});
        }

        _.each(handlers, function(handler) {
          _.defer(function() {handler.callback(message);});
        });
      });

      _.defer(this.poll);
    },

    onPollFail: function(error) {
      switch (error.type) {
        case 'comet.duplicated_channel':
        this.onError(error);
        break;

        default:
        this.onError(error);
        _.wait(this.poll, this.sleep);
      }
    },

    onError: function(reply) {
      var error = reply.error,
          msg = [reply.id, error.code, error.type, error.message].join(' ');

      console.error('Comet error: ' + msg);
    }
  };

  return Comet;
});

// Local Variables:
// mode: js
// js-indent-level: 2
// End:
