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

Clipperz.PM.Proxy.JSON = function(args) {
	Clipperz.PM.Proxy.JSON.superclass.constructor.call(this, args);

	this._url = args.url || Clipperz.Base.exception.raise('MandatoryParameter');
	
	return this;
}

Clipperz.Base.extend(Clipperz.PM.Proxy.JSON, Clipperz.PM.Proxy, {

	'toString': function() {
		return "Clipperz.PM.Proxy.JSON";
	},

	//=========================================================================

	'url': function () {
		return this._url;
	},

	//=========================================================================

	'type': function () {
		return 'ONLINE';
	},

	'typeDescription': function () {
		return 'Online service';
	},

	'features': function (someFeatures) {
		return someFeatures;
	},

	//=========================================================================
	
	'_sendMessage': function(aFunctionName, aVersion, someParameters, someOptionalParameters) {
		var	deferredResult;
		var parameters;

		parameters = {
			method: aFunctionName,
			version: aVersion,
			parameters: Clipperz.Base.serializeJSON(someParameters)
		};

		someOptionalParameters = someOptionalParameters || {};

//console.log("PROXY.JSON._sendMessage", parameters);
		deferredResult = new Clipperz.Async.Deferred("Proxy.JSON._sendMessage", {trace:false});
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'remoteRequestSent');
		deferredResult.addCallback(Clipperz.Async.doXHR, this.url(), {
//		deferredResult.addCallback(MochiKit.Async.doXHR, this.url(), {
			method:'POST',
			sendContent:MochiKit.Base.queryString(parameters),
			headers:{"Content-Type":"application/x-www-form-urlencoded"},
			// downloadProgress:someOptionalParameters['downloadProgressCallback'] || null,
			// uploadProgress:someOptionalParameters['uploadProgressCallback'] || null,
		});
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'remoteRequestReceived');
		deferredResult.addCallback(MochiKit.Base.itemgetter('responseText'));
		deferredResult.addCallback(Clipperz.Base.evalJSON);
		deferredResult.addCallback(function (someValues) {
			if (someValues['result'] == 'EXCEPTION') {
				throw someValues['message'];
			}
			
			return someValues;
		})
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'_uploadAttachment': function(someArguments, aProgressCallback, aSharedSecret, aToll) {
		var formData;
		var deferredResult;

		var parameters = {
			'toll': aToll,
			'parameters': {
				'message': 'uploadAttachment',
				'srpSharedSecret': aSharedSecret,
				'parameters': {
					'attachmentReference': someArguments['attachmentReference'],
					'recordReference':     someArguments['recordReference'],
					'version':             someArguments['version']
				}
			}
		}

		var blob = new Blob([someArguments['arrayBufferData']]);

		formData = new FormData();
		formData.append('method', 'message');
		formData.append('version', 'fake-app-version');		// Not implemented anywhere yet
		formData.append('parameters', JSON.stringify(parameters));
		formData.append('data', blob);

		deferredResult = new Clipperz.Async.Deferred("Proxy.JSON._sendMessage", {trace:false});
//deferredResult.addCallback(function(){console.log("About to send request");});
		deferredResult.addCallback(Clipperz.Async.doXHR, this.url(), {
			'method':'POST',
			'sendContent':formData,
			// headers:{"Content-Type":"application/x-www-form-urlencoded"},
			'uploadProgress': aProgressCallback || null,
		});
//deferredResult.addCallback(function(something){console.log("Done sending request"); return something;});
		deferredResult.addCallback(function (someValues) {
			if (someValues['result'] == 'EXCEPTION') {
				throw someValues['message'];
			}
			
			return someValues;
		});

		deferredResult.callback();

		return deferredResult;
	},

	'_downloadAttachment': function(someArguments,  aProgressCallback, aSharedSecret, aToll) {
		var	deferredResult;
		var parameters;

		var innerParameters = {
			'toll': aToll,
			'parameters': {
				'message': 'downloadAttachment',
				'srpSharedSecret': aSharedSecret,
				'parameters': {
					'reference': someArguments['reference']
				}
			}
		}

		parameters = {
			method: 'message',
			version: 'fake-app-version',
			parameters: Clipperz.Base.serializeJSON(innerParameters)
		};

		deferredResult = new Clipperz.Async.Deferred("Proxy.JSON._downloadAttachment", {trace:false});
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'remoteRequestSent');
//deferredResult.addCallback(function(){console.log("About to send request");});
		deferredResult.addCallback(Clipperz.Async.doXHR, this.url(), {
			method:'POST',
			responseType:'arraybuffer',
			sendContent:MochiKit.Base.queryString(parameters),
			headers:{"Content-Type":"application/x-www-form-urlencoded"},
			downloadProgress:aProgressCallback || null,
		});
deferredResult.addCallback(
	function(something){
		// console.log("Done sending request", something);
		return something;
});
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'remoteRequestReceived');
		deferredResult.addCallback(MochiKit.Base.itemgetter('response'));
		deferredResult.addCallback(function (anArrayBuffer) {
			var result = (anArrayBuffer.byteLength > 0) ? anArrayBuffer : false;
			
var DEBUG = 'PUT_A_BREAKPOINT_HERE';
			
			return {
				result: anArrayBuffer,
			};
		})
		// deferredResult.addErrback(function(something) {
		// 	return something;
		// });
		deferredResult.callback();

		return deferredResult;
	},


	//=========================================================================
	__syntaxFix__: "syntax fix"
	
});
