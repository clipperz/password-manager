/*

Copyright 2008-2015 Clipperz Srl

This file is part of Clipperz, the online password manager.
For further information about its features and functionalities please
refer to http://www.clipperz.com.

* Clipperz is free software: you can redistribute it and/or modify it
  under the terms of the GNU Affero General Public License as published
  by the Free Software Foundation, either version 3 of the License, or 
  (at your option) any later version.

* Clipperz is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of 
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Clipperz. If not, see http://www.gnu.org/licenses/.

*/

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.NotificationCenter) == 'undefined') { Clipperz.NotificationCenter = {}; }


//#############################################################################

Clipperz.NotificationCenterEvent = function(args) {
	args = args || {};
//	MochiKit.Base.bindMethods(this);

	this._source = args.source || null;
	this._event = args.event || null;
	this._parameters = args.parameters || null;
	this._isSynchronous = args.isSynchronous || false;
	
	return this;
}

//=============================================================================

Clipperz.NotificationCenterEvent.prototype = MochiKit.Base.update(null, {

	//-------------------------------------------------------------------------

	'toString': function() {
		return "Clipperz.NotificationCenterEvent";
		//return "Clipperz.NotificationCenterEvent {source: " + this.source() + ", event: " + this.event() + ", parameters: " + this.parameters() + "}";
	},

	//-------------------------------------------------------------------------

	'source': function() {
		return this._source;
	},
	
	'setSource': function(aValue) {
		this._source = aValue;
	},

	//-------------------------------------------------------------------------

	'event': function() {
		return this._event;
	},
	
	'setEvent': function(aValue) {
		this._event = aValue;
	},
	
	//-------------------------------------------------------------------------

	'parameters': function() {
		return this._parameters;
	},
	
	'setParameters': function(aValue) {
		this._parameters = aValue;
	},
	
	//-------------------------------------------------------------------------

	'isSynchronous': function() {
		return this._isSynchronous;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});


//#############################################################################
//#############################################################################

Clipperz.NotificationCenter = function(args) {
	args = args || {};
//	MochiKit.Base.bindMethods(this);

	this._listeners = {};
	this._useSynchronousListenerInvocation = args.useSynchronousListenerInvocation || false;
	this._timeoutDelay = args.timeoutDelay || 0.1;

	return this;
}

//=============================================================================

Clipperz.NotificationCenter.prototype = MochiKit.Base.update(null, {

	//-------------------------------------------------------------------------

	'toString': function() {
		return "Clipperz.NotificationCenter";
	},

	//-------------------------------------------------------------------------

	'useSynchronousListenerInvocation': function() {
		return this._useSynchronousListenerInvocation;
	},
	
	'setUseSynchronousListenerInvocation': function(aValue) {
		this._useSynchronousListenerInvocation = aValue;
	},

	//-------------------------------------------------------------------------

	'timeoutDelay': function() {
		return this._timeoutDelay;
	},
	
	'setTimeoutDelay': function(aValue) {
		this._timeoutDelay = aValue;
	},

	//-------------------------------------------------------------------------

	'listeners': function() {
		return this._listeners;
	},

	//-------------------------------------------------------------------------

	'register': function(aSource, anEvent, aListener, aMethod) {
		var	eventListeners;
		var	listenerInfo;
		var	eventKey;

		if (anEvent != null) {
			eventKey = anEvent;
		} else {
			eventKey = '_notificationCenter_matchAnyEvent_key_';
		}
		
		eventListeners = this.listeners()[eventKey];
		
		if (eventListeners == null) {
			eventListeners = [];
			this.listeners()[eventKey] = eventListeners;
		}

		listenerInfo = {};
		if (aSource != null) {
			listenerInfo['source'] = aSource;
		} else {
			listenerInfo['source'] = 'any';
		}
		
		listenerInfo['listener'] = aListener;
		listenerInfo['method'] = aMethod;

		eventListeners.push(listenerInfo);

		return listenerInfo;
	},

	//-------------------------------------------------------------------------

	'removeListenerInfoFromListeners': function(aListener, someListeners) {
		var	listenerIndex;
		var	i,c;
		
		if (someListeners != null) {
			listenerIndex = -1;
			c = someListeners.length;
			for (i=0; i<c; i++) {
				var	listenerInfo;
			
				listenerInfo = someListeners[i];
				if (listenerInfo['listener'] === aListener) {
					listenerIndex = i;
				}
			}

			if (listenerIndex != -1) {
				Clipperz.Base.removeObjectAtIndexFromArray(listenerIndex, someListeners);
			}
		}
	},
	
	//-------------------------------------------------------------------------

	'unregister': function(aListener, anEvent) {
		if (anEvent == null) {
			var	allListenerList;
			var	i, c;
			
//			allListenerList = Clipperz.Base.values(this.listeners());
			allListenerList = MochiKit.Base.values(this.listeners());
			c = allListenerList.length;
			for (i=0; i<c; i++) {
				this.removeListenerInfoFromListeners(aListener, allListenerList[i]);
			}
		} else {
			this.removeListenerInfoFromListeners(aListener, this.listeners()[anEvent]);
		}
	},
	
	//-------------------------------------------------------------------------

	'asysnchronousListenerNotification': function(anEventInfo, aMethod, aListener) {
		MochiKit.Async.callLater(this.timeoutDelay(), MochiKit.Base.partial(MochiKit.Base.methodcaller(aMethod, anEventInfo), aListener));
//		setTimeout(MochiKit.Base.partial(MochiKit.Base.methodcaller(aMethod, anEventInfo), aListener), this.timeoutDelay());
	},

	//-------------------------------------------------------------------------

	'processListenerInfo': function(anEventInfo, aListenerInfo) {
		var	shouldInvokeListenerMethod;

		if (aListenerInfo['source'] == 'any') {
			shouldInvokeListenerMethod = true;
		} else {
			if (aListenerInfo['source'] === anEventInfo.source()) {
				shouldInvokeListenerMethod = true;
			} else {
				shouldInvokeListenerMethod = false;
			}
		}

		if (shouldInvokeListenerMethod) {
			if (this.useSynchronousListenerInvocation() || anEventInfo.isSynchronous()) {
//MochiKit.Logging.logDebug("syncrhronous listener invocation");
				try {
//					MochiKit.Base.map(MochiKit.Base.methodcaller(aListenerInfo['method'], anEventInfo), [aListenerInfo['listener']]);
//console.log("notification: ", aListenerInfo['listener'], aListenerInfo['method'], anEventInfo);
					MochiKit.Base.method(aListenerInfo['listener'], aListenerInfo['method'], anEventInfo)();
				} catch(exception) {
					MochiKit.Logging.logError('NotificationCenter ERROR: unable to invoke method \'' + aListenerInfo['method'] + '\' on object ' + aListenerInfo['listener']);
				}
			} else {
				var asyncMethod;
				
//MochiKit.Logging.logDebug("asyncrhronous listener invocation");
				asyncMethod = MochiKit.Base.bind(this.asysnchronousListenerNotification, this)
				MochiKit.Base.map(MochiKit.Base.partial(asyncMethod, anEventInfo, aListenerInfo['method']), [aListenerInfo['listener']]);
			}
		}
	},

	//-------------------------------------------------------------------------

	'notify': function(aSource, anEvent, someEventParameters, isSynchronous) {
		var	eventInfo;
		var processInfoMethod;

//MochiKit.Logging.logDebug(">>> NotificationCenter.notify");
		eventInfo = new Clipperz.NotificationCenterEvent({source:aSource, event:anEvent, parameters:someEventParameters, isSynchronous:isSynchronous});
//MochiKit.Logging.logDebug("--- NotificationCenter.notify - 1");
		processInfoMethod = MochiKit.Base.bind(this.processListenerInfo, this);
//MochiKit.Logging.logDebug("--- NotificationCenter.notify - 2");

		MochiKit.Base.map(MochiKit.Base.partial(processInfoMethod, eventInfo), this.listeners()[anEvent] || []);
//MochiKit.Logging.logDebug("--- NotificationCenter.notify - 3");
		MochiKit.Base.map(MochiKit.Base.partial(processInfoMethod, eventInfo), this.listeners()['_notificationCenter_matchAnyEvent_key_'] || []);
//MochiKit.Logging.logDebug("<<< NotificationCenter.notify");
	},

	//-------------------------------------------------------------------------

	'deferredNotification': function(aSource, anEvent, someEventParameters, aDeferredResult) {
		
		this.notify(aSource, anEvent, someEventParameters, true);
		
		return aDeferredResult;
//		return MochiKit.Async.wait(1, aDeferredResult);
	},
	
	//-------------------------------------------------------------------------

	'resetStatus': function() {
		this._listeners = {};
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});

//#############################################################################

Clipperz.NotificationCenter.defaultNotificationCenter = new Clipperz.NotificationCenter();

Clipperz.NotificationCenter.notify = MochiKit.Base.method(Clipperz.NotificationCenter.defaultNotificationCenter, 'notify');
Clipperz.NotificationCenter.register = MochiKit.Base.method(Clipperz.NotificationCenter.defaultNotificationCenter, 'register');
Clipperz.NotificationCenter.unregister = MochiKit.Base.method(Clipperz.NotificationCenter.defaultNotificationCenter, 'unregister');
Clipperz.NotificationCenter.deferredNotification = MochiKit.Base.method(Clipperz.NotificationCenter.defaultNotificationCenter, 'deferredNotification');
/*
_clipperz_notificationCenter_defaultNotificationCenter = null;

Clipperz.NotificationCenter.defaultNotificationCenter = function() {
	if (_clipperz_notificationCenter_defaultNotificationCenter == null) {
		_clipperz_notificationCenter_defaultNotificationCenter = new Clipperz.NotificationCenter();
	}

	return _clipperz_notificationCenter_defaultNotificationCenter;
};
*/
