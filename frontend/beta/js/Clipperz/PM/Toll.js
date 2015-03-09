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

Clipperz.PM.Toll = function(args) {
	this._requestType = args.requestType;
	this._targetValue = args.targetValue;
	this._cost = args.cost;
	this._toll = null;

	return this;
}

Clipperz.PM.Toll.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "Clipperz.PM.Toll (" + this.requestType() + ": " + this.cost() + " - " + ((this.toll() == null)? 'UNPAID' : 'PAID') + ")";
	},

	//-------------------------------------------------------------------------

	'requestType': function() {
		return this._requestType;
	},

	//-------------------------------------------------------------------------

	'targetValue': function() {
		return this._targetValue;
	},

	//-------------------------------------------------------------------------

	'cost': function() {
		return this._cost;
	},
	
	//-------------------------------------------------------------------------

	'toll': function() {
		return this._toll;
	},

	//=========================================================================
	
	'prefixMatchingBits': function(aValue1, aValue2) {
		var	result;
		var i,c;
		
		result = 0;

		c = Math.min(aValue1.length(), aValue2.length());
		i = 0;
		while (i<c && (aValue1.byteAtIndex(i) == aValue2.byteAtIndex(i))) {
			result += 8;
			i++;
		}

		if (i<c) {
			var	xorValue;
			
			xorValue = (aValue1.byteAtIndex(i) ^ aValue2.byteAtIndex(i));
			
			if (xorValue >= 128) {
				result += 0;
			} else if (xorValue >= 64) {
				result += 1;
			} else if (xorValue >= 32) {
				result += 2;
			} else if (xorValue >= 16) {
				result += 3;
			} else if (xorValue >= 8) {
				result += 4;
			} else if (xorValue >= 4) {
				result += 5;
			} else if (xorValue >= 2) {
				result += 6;
			} else if (xorValue >= 1) {
				result += 7;
			}
		}

		return result;
	},

	//=========================================================================

	'pay': function() {
		var	result;
		var	targetData;
		var	targetMatchSize;
		var prefixMatchingBits;
		var	payment;
		var i;
		
//MochiKit.Logging.logDebug(">>> Toll.pay");
		if (this.toll() == null) {
			i = 0;
//MochiKit.Logging.logDebug("--- Proxy.payToll - 1");
			targetData = new Clipperz.ByteArray("0x" + this.targetValue());
//MochiKit.Logging.logDebug("--- Proxy.payToll - 2");
			targetMatchSize = this.cost();
//MochiKit.Logging.logDebug("--- Proxy.payToll - 3");
		
			payment = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32);
//MochiKit.Logging.logDebug("--- Proxy.payToll - 4");
		
			do {
				var	paymentData;

//MochiKit.Logging.logDebug("--- Proxy.payToll - 5");
				//payment = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32);
				payment.increment();
//MochiKit.Logging.logDebug("--- Proxy.payToll - 6");
				paymentData = Clipperz.Crypto.SHA.sha256(payment);
//MochiKit.Logging.logDebug("--- Proxy.payToll - 7");
				prefixMatchingBits = this.prefixMatchingBits(targetData, paymentData);
//MochiKit.Logging.logDebug("--- Proxy.payToll - 8");
				i++;
//MochiKit.Logging.logDebug("--- Proxy.payToll - 9");
			} while (prefixMatchingBits < targetMatchSize);
//MochiKit.Logging.logDebug("--- Proxy.payToll - 10");

			this._toll = payment.toHexString().substring(2)
		}
//MochiKit.Logging.logDebug("<<< Toll.pay");

		return this;
	},
	
	//-------------------------------------------------------------------------

	'deferredPay': function() {
		var	deferredResult;
		var	toll;
		
//MochiKit.Logging.logDebug(">>> Toll.deferredPay");
		toll = this;
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("y.1 - Proxy.deferredPayToll - 1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(toll, 'pay'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("y.2 - Proxy.deferredPayToll - 2: " + res); return res;});
		deferredResult.addCallback(function(aToll) {
			var result;
		
			result = {
				targetValue:aToll.targetValue(),
				toll:aToll.toll()
			};
		
			return result;
		});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("y.3 - Proxy.deferredPayToll - 3: " + res); return res;});
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< Toll.deferredPay");
		
		return deferredResult;
	},
	
	//=========================================================================
	__syntaxFix__: "syntax fix"
	
});

