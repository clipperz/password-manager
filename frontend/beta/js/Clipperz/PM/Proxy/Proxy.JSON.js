/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz's Javascript Crypto Library.
Javascript Crypto Library provides web developers with an extensive
and efficient set of cryptographic functions. The library aims to
obtain maximum execution speed while preserving modularity and
reusability.
For further information about its features and functionalities please
refer to http://www.clipperz.com

* Javascript Crypto Library is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Javascript Crypto Library is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Javascript Crypto Library.  If not, see
  <http://www.gnu.org/licenses/>.

*/

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }

//=============================================================================

Clipperz.PM.Proxy.JSON = function(args) {
	Clipperz.PM.Proxy.JSON.superclass.constructor.call(this, args);

	this._url = args.url || Clipperz.Base.exception.raise('MandatoryParameter');
	
	return this;
}

YAHOO.extendX(Clipperz.PM.Proxy.JSON, Clipperz.PM.Proxy, {

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

		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(function (aValue) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'remoteRequestSent');
			return aValue;
		});
		deferredResult.addCallback(MochiKit.Async.doXHR, this.url(), {
			method:'POST',
			sendContent:MochiKit.Base.queryString(parameters),
			headers:{"Content-Type":"application/x-www-form-urlencoded"}
		});
		deferredResult.addCallback(function (aValue) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'remoteRequestReceived');
			return aValue;
		});
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
