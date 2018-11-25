/*

Copyright 2008-2018 Clipperz Srl

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
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }

//=============================================================================

Clipperz.PM.Proxy = function(args) {
	args = args || {};

	this._shouldPayTolls = args.shouldPayTolls || false;

	this._tolls = {
		'CONNECT':	[],
		'REGISTER':	[],
		'MESSAGE':	[]
	};

	if (args.isDefault === true) {
		Clipperz.PM.Proxy.defaultProxy = this;
	}

	return this;
}

Clipperz.PM.Proxy.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "Clipperz.PM.Proxy";
	},

	//=========================================================================

	'shouldPayTolls': function() {
		return this._shouldPayTolls;
	},

	//-------------------------------------------------------------------------

	'tolls': function() {
		return this._tolls;
	},

	//-------------------------------------------------------------------------

	'payToll': function(aRequestType, someParameters) {
		var	deferredResult;

		if (this.shouldPayTolls()) {
			deferredResult = new Clipperz.Async.Deferred("Proxy.payToll", {trace:false});

			if (this.tolls()[aRequestType].length == 0) {
				deferredResult.addMethod(this, 'sendMessage', 'knock', {requestType:aRequestType});
				deferredResult.addMethod(this, 'setTollCallback');
			}
			deferredResult.addMethod(this.tolls()[aRequestType], 'pop');
			deferredResult.addCallback(MochiKit.Base.methodcaller('deferredPay'));
			deferredResult.addCallback(function(aToll) {
				var result;

				result = {
					parameters: someParameters,
					toll: aToll
				}
		
				return result;
			});
		
			deferredResult.callback();
		} else {
			deferredResult = MochiKit.Async.succeed({parameters:someParameters});
		}

		return deferredResult;
	},

	'justPayToll': function(aRequestType) {
		var	deferredResult;

		if (this.shouldPayTolls()) {
			deferredResult = new Clipperz.Async.Deferred("Proxy.justPayToll", {trace:false});

			if (this.tolls()[aRequestType].length == 0) {
				deferredResult.addMethod(this, 'sendMessage', 'knock', {requestType:aRequestType});
				deferredResult.addMethod(this, 'setTollCallback');
			}
			deferredResult.addMethod(this.tolls()[aRequestType], 'pop');
			deferredResult.addCallback(MochiKit.Base.methodcaller('deferredPay'));
		
			deferredResult.callback();
		} else {
			deferredResult = MochiKit.Async.succeed(null);
		}

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'addToll': function(aToll) {
		this.tolls()[aToll.requestType()].push(aToll);
	},

	//=========================================================================

	'setTollCallback': function(someParameters) {
		if (typeof(someParameters['toll']) != 'undefined') {
			this.addToll(new Clipperz.PM.Toll(someParameters['toll']));
		}

		return someParameters['result'];	// <- This is what will be returned by the message call
	},

	//=========================================================================

	'registration': function (someParameters) {
		return this.processMessage('registration', someParameters, 'REGISTER');
	},

	'handshake': function (someParameters) {
		return this.processMessage('handshake', someParameters, 'CONNECT');
	},

	'message': function (someParameters, someOptionalParameters) {
		return this.processMessage('message', someParameters, 'MESSAGE', someOptionalParameters);
	},

	'logout': function (someParameters) {
		return this.processMessage('logout', someParameters, 'MESSAGE');
	},

	//=========================================================================

	'type': function () {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'typeDescription': function () {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'features': function (someFeatures) {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	//=========================================================================

	'processMessage': function (aFunctionName, someParameters, aRequestType, someOptionalParameters) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("Proxy.processMessage", {trace:false});
		deferredResult.addMethod(this, 'payToll', aRequestType);
		// deferredResult.addMethod(this, 'sendMessage', aFunctionName);
		deferredResult.addCallback(MochiKit.Base.bind(function(aResult){
			return this.sendMessage(aFunctionName, aResult, someOptionalParameters);
		}, this));
		deferredResult.addMethod(this, 'setTollCallback');
		deferredResult.callback(someParameters);
		
		return deferredResult;		
	},

	'processAttachmentMessage': function(aMessageCallback) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("Proxy.uploadAttachment", {trace:false});
		deferredResult.addMethod(this, 'justPayToll', 'MESSAGE');
		deferredResult.addCallback(aMessageCallback);
		deferredResult.addErrback(MochiKit.Base.method(this, 'handleError'));
		deferredResult.addMethod(this, 'setTollCallback');
		deferredResult.callback();
		
		return deferredResult;		
	},

	'uploadAttachment': function(someArguments, aProgressCallback, aSharedSecret) {
		var messageCallback = MochiKit.Base.method(this, '_uploadAttachment', someArguments, aProgressCallback, aSharedSecret);

		return this.processAttachmentMessage(messageCallback);
	},

	'downloadAttachment': function(someArguments, aProgressCallback, aSharedSecret) {
		var messageCallback = MochiKit.Base.method(this, '_downloadAttachment', someArguments, aProgressCallback, aSharedSecret);

		return this.processAttachmentMessage(messageCallback);
	},

	//=========================================================================

	'_uploadAttachment': function (someArguments, aProgressCallback, aSharedSecret) {
		throw Clipperz.Base.exception.AbstractMethod;		
	},

	'_sendMessage': function (aFunctionName, aVersion, someParameters, someOptionalParameters) {
		throw Clipperz.Base.exception.AbstractMethod;		
	},

	'sendMessage': function (aFunctionName, someParameters, someOptionalParameters) {
		var deferredResult;

//console.log("PROXY.sendMessage", aFunctionName, someParameters);
//	TODO: read actual application version for a property set at build time
		deferredResult = new Clipperz.Async.Deferred("Proxy.sendMessage", {trace:false});
		deferredResult.addMethod(this, '_sendMessage', aFunctionName, 'fake-app-version', someParameters, someOptionalParameters);
		deferredResult.addErrback(MochiKit.Base.method(this, 'handleError'));
		deferredResult.callback();
		
		return deferredResult;		
	},

	//-------------------------------------------------------------------------

	'handleError': function (anError) {
		if (anError['message'] == 'Wrong application version') {
			anError['isPermanent'] = true;
		}
		return anError;
	},

	//=========================================================================

	'isReadOnly': function () {
		return false;
	},

	'canRegisterNewUsers': function () {
		return true;
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
	
});
