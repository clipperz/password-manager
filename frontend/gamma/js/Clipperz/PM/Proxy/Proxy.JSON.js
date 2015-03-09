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
	
	'sendMessage': function(aFunctionName, someParameters) {
		var	deferredResult;
		var parameters;
		
		parameters = {
			method: aFunctionName,
//			version: someParameters['version'],
//			message: someParameters['message'],
			parameters: Clipperz.Base.serializeJSON(someParameters)
		};

		deferredResult = new Clipperz.Async.Deferred("Proxy.JSON.sendMessage", {trace:false});
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'remoteRequestSent');
		deferredResult.addCallback(MochiKit.Async.doXHR, this.url(), {
			method:'POST',
			sendContent:MochiKit.Base.queryString(parameters),
			headers:{"Content-Type":"application/x-www-form-urlencoded"}
		});
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'remoteRequestReceived');
//		deferredResult.addCallback(MochiKit.Async.evalJSONRequest);
		deferredResult.addCallback(MochiKit.Base.itemgetter('responseText'));
		deferredResult.addCallback(Clipperz.Base.evalJSON);
		deferredResult.addCallback(function (someValues) {
			if (someValues['result'] == 'EXCEPTION') {
				throw someValues['message'];
			}
			
			return someValues;
		})
//			return MochiKit.Base.evalJSON(req.responseText);
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
	
});
