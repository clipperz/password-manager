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

Clipperz.PM.Proxy.PHP = function(args) {
	Clipperz.PM.Proxy.PHP.superclass.constructor.call(this, args);
/*
	this._tolls = {
		'CONNECT':	[],
		'REGISTER':	[],
		'MESSAGE':	[]
	};
*/	
	return this;
}

YAHOO.extendX(Clipperz.PM.Proxy.PHP, Clipperz.PM.Proxy, {

	'toString': function() {
		return "Clipperz.PM.Proxy.PHP - " + this.args();
	},

	//=========================================================================
/*
	'tolls': function() {
		return this._tolls;
	},
*/
	//-------------------------------------------------------------------------
/*
	'payToll': function(aRequestType, someParameters) {
		var	deferredResult;

//MochiKit.Logging.logDebug(">>> Proxy.DWR.payToll: " + aRequestType);
		if (this.tolls()[aRequestType].length > 0) {
			deferredResult = MochiKit.Async.succeed(this.tolls()[aRequestType].pop());
		} else {
//MochiKit.Logging.logDebug("### " + aRequestType + " toll NOT immediately available; request queued.");
			deferredResult = new MochiKit.Async.Deferred();
			deferredResult.addCallback(function(someParameters) {
				return new Clipperz.PM.Toll(someParameters['toll']);
			})
			com_clipperz_pm_Proxy.knock(Clipperz.Base.serializeJSON({requestType:aRequestType}), {
				callback:MochiKit.Base.method(deferredResult, 'callback'),
				errorHandler:MochiKit.Base.method(deferredResult, 'errback')
			});
		}
		
		deferredResult.addCallback(function(aToll) {
			return aToll.deferredPay();
		});
		deferredResult.addCallback(function(someParameters, aToll) {
			var result;
			
			result = {
				parameters: someParameters,
				toll: aToll
			}
			
			return result;
		}, someParameters);
		
		return deferredResult;
	},
*/
	//-------------------------------------------------------------------------
/*
	'addToll': function(aToll) {
		this.tolls()[aToll.requestType()].push(aToll);
	},
*/
	//=========================================================================
/*
	'setTollCallback': function(someParameters) {
//MochiKit.Logging.logDebug(">>> Proxy.DWR.setTollCallback");
//MochiKit.Logging.logDebug("--- Proxy.DWR.setTollCallback - " + Clipperz.Base.serializeJSON(someParameters));
		if (typeof(someParameters['toll']) != 'undefined') {
			this.addToll(new Clipperz.PM.Toll(someParameters['toll']));
		}
		return someParameters['result'];
	},
*/
	//=========================================================================

	'registration': function(someParameters) {
		return this.sendMessage('registration', someParameters, 'REGISTER');
	},

	//-------------------------------------------------------------------------

	'handshake': function(someParameters) {
/*
		_s = "e8a2162f29aeaabb729f5625e9740edbf0cd80ac77c6b19ab951ed6c88443b8c";
		_v = new Clipperz.Crypto.BigInt("955e2db0f7844aca372f5799e5f7e51b5866718493096908bd66abcf1d068108", 16);
		_b = new Clipperz.Crypto.BigInt("5761e6c84d22ea3c5649de01702d60f674ccfe79238540eb34c61cd020230c53", 16);

		_B = _v.add(Clipperz.Crypto.SRP.g().powerModule(_b, Clipperz.Crypto.SRP.n()));
		_u = new Clipperz.Crypto.BigInt(Clipperz.PM.Crypto.encryptingFunctions.versions[someParameters.version].hash(new Clipperz.ByteArray(_B.asString(10))).toHexString(), 16);
		_A = new Clipperz.Crypto.BigInt("3b3567ec33d73673552e960872eb154d091a2488915941038aef759236a27e64", 16);
		_S = (_A.multiply(_v.powerModule(_u, Clipperz.Crypto.SRP.n()))).powerModule(_b, Clipperz.Crypto.SRP.n());
		_K = Clipperz.PM.Crypto.encryptingFunctions.versions[someParameters.version].hash(new Clipperz.ByteArray(_S.asString(10))).toHexString().slice(2);
		_M1 = Clipperz.PM.Crypto.encryptingFunctions.versions[someParameters.version].hash(new Clipperz.ByteArray(_A.asString(10) + _B.asString(10) + _K)).toHexString().slice(2);
		_M2 = Clipperz.PM.Crypto.encryptingFunctions.versions[someParameters.version].hash(new Clipperz.ByteArray(_A.asString(10) + _M1 + _K)).toHexString().slice(2);
		
//		MochiKit.Logging.logDebug("b = " + _b.asString(16));
//		MochiKit.Logging.logDebug("v = " + _v.asString(16));
		MochiKit.Logging.logDebug("B = " + _B.asString(16));
		MochiKit.Logging.logDebug("u = " + _u.asString(16));
		MochiKit.Logging.logDebug("S = " + _S.asString(16));
		MochiKit.Logging.logDebug("K = " + _K);
		MochiKit.Logging.logDebug("M1 = " + _M1);
		MochiKit.Logging.logDebug("M2 = " + _M2);
//		MochiKit.Logging.logDebug("someParameters.version: " + someParameters.version);
*/		
		return this.sendMessage('handshake', someParameters, 'CONNECT');
	},

	//-------------------------------------------------------------------------

	'message': function(someParameters) {
		return this.sendMessage('message', someParameters, 'MESSAGE');
	},

	//-------------------------------------------------------------------------

	'logout': function(someParameters) {
//MochiKit.Logging.logDebug("=== Proxy.DWR.logout");
		return this.sendMessage('logout', someParameters, 'MESSAGE');
	},

	//=========================================================================

	'sendMessage': function(aFunctionName, someParameters, aRequestType) {
/*
		var	deferredResult;
		var proxy;
		
//MochiKit.Logging.logDebug(">>> Proxy.DWR.sendMessage - " + aFunctionName + " - " + aRequestType);
		proxy = this;
		
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("x.1 Proxy.DWR.sendMessage - 1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(proxy, 'payToll'), aRequestType);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("x.2 Proxy.DWR.sendMessage - 2: " + Clipperz.Base.serializeJSON(res)); return res;});
		deferredResult.addCallback(MochiKit.Base.method(proxy, 'sendRemoteMessage'), aFunctionName);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("x.3 Proxy.DWR.sendMessage - 3: " + res); return res;});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("x.3 Proxy.DWR.sendMessage - 3: " + Clipperz.Base.serializeJSON(res)); return res;});
		deferredResult.callback(someParameters);
		
//MochiKit.Logging.logDebug("<<< Proxy.DWR.sendMessage");
		return deferredResult;		
*/

//		return this.sendRemoteMessage(aFunctionName, someParameters);


		var	deferredResult;
		var proxy;

		proxy = this;

		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(MochiKit.Base.method(proxy, 'sendRemoteMessage'), aFunctionName);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("x.3 Proxy.PHP.sendMessage - 3: " + res); return res;});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("x.3 Proxy.PHP.sendMessage - 3.1: " + Clipperz.Base.serializeJSON(res)); return res;});

		deferredResult.callback(someParameters);

		return deferredResult;
	},

	//=========================================================================
	
	'sendRemoteMessage': function(aFunctionName, someParameters) {
/*
		var	deferredResult;

//MochiKit.Logging.logDebug(">>> Proxy.DWR.sendRemoteMessage('" + aFunctionName + "', " + Clipperz.Base.serializeJSON(someParameters) + ") - " + this);
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Proxy.DWR.sendRemoteMessage - 1: " + res); return res;});
//		deferredResult.addCallback(MochiKit.Base.method(this, 'setTollCallback'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Proxy.DWR.sendRemoteMessage - 2: " + res); return res;});
		
		com_clipperz_pm_Proxy[aFunctionName](Clipperz.Base.serializeJSON(someParameters), {
			callback:MochiKit.Base.method(deferredResult, 'callback'),
			errorHandler:MochiKit.Base.method(deferredResult, 'errback')
		});
//MochiKit.Logging.logDebug("<<< Proxy.PHP.sendRemoteMessage - result: " + deferredResult);

		return deferredResult;
*/

		var	deferredResult;
		var parameters;
		
//MochiKit.Logging.logDebug(">>> Proxy.PHP.sendRemoteMessage('" + aFunctionName + "', " + Clipperz.Base.serializeJSON(someParameters) + ") - " + this);
		parameters = {};
		parameters['method'] = aFunctionName;
//		parameters['version'] = someParameters['version'];
//		parameters['message'] = someParameters['message'];
		parameters['parameters'] = Clipperz.Base.serializeJSON(someParameters);
//MochiKit.Logging.logDebug("--- Proxy.PHP.sendRemoteMessage('" + Clipperz.Base.serializeJSON(parameters) + ") - " + this);
		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(MochiKit.Async.doXHR, "./php/index.php", {
			method:'POST',
			sendContent:MochiKit.Base.queryString(parameters),
			headers:{"Content-Type":"application/x-www-form-urlencoded"}
		});
//deferredResult.addCallback(function(res) {MochiKit.Logging.logDebug("Proxy.PHP.response - 2: " + res.responseText); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("Proxy.PHP.response - ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Async.evalJSONRequest);
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'isReadOnly': function() {
		return false;
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
	
});

//=============================================================================

//Clipperz.PM.Proxy.defaultProxy = new Clipperz.PM.Proxy.PHP("Proxy.PHP - async test");
