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
	args = args || {};

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

	//-------------------------------------------------------------------------
/*
	'__pay': function() {
		var	result;
		var	targetData;
		var	targetMatchSize;
		var prefixMatchingBits;
		var	payment;
		var i;
		
		if (this.toll() == null) {
			i = 0;
			targetData = new Clipperz.ByteArray("0x" + this.targetValue());
			targetMatchSize = this.cost();
		
			payment = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32);
		
			do {
				var	paymentData;

				//payment = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32);
				payment.increment();
				paymentData = Clipperz.Crypto.SHA.sha256(payment);
//				prefixMatchingBits = this.prefixMatchingBits(targetData, paymentData);
				prefixMatchingBits = Clipperz.ByteArray.prefixMatchingBits(targetData, paymentData);
				i++;
			} while (prefixMatchingBits < targetMatchSize);

			this._toll = payment.toHexString().substring(2)
		}

		return this;
	},
*/	
	//-------------------------------------------------------------------------

	'innerDeferredPay': function (aTargetValue, aCost, aPayment) {
		var deferredResult;
		var result;
		var payment;
		var i;

		result = null;
		payment = aPayment;
		i = 0;

		while ((result == null) && (i < Clipperz.PM.Toll.numberOfCloseLoopIterations)) {
			if (Clipperz.ByteArray.prefixMatchingBits(aTargetValue, Clipperz.Crypto.SHA.sha256(payment)) > aCost) {
				result = payment;
			} else {
				payment.increment();
			}

			i ++;
		}
		
		if (result == null) {
			deferredResult = MochiKit.Async.callLater(Clipperz.PM.Toll.pauseBetweenEachCloseLoop, MochiKit.Base.method(this, 'innerDeferredPay', aTargetValue, aCost, aPayment));
		} else {
			deferredResult = MochiKit.Async.succeed(result);
		}
		
		return deferredResult;
	},

	'deferredPay': function () {
		var	deferredResult;
		var	toll;
		
		toll = this;
		deferredResult = new Clipperz.Async.Deferred("Toll.deferredPay");
//deferredResult.addLog("--->>> deferredPay - " + this.cost());
		deferredResult.addMethod(Clipperz.Crypto.PRNG.defaultRandomGenerator(), 'getRandomBytes', 32);
		deferredResult.addMethod(toll, 'innerDeferredPay', new Clipperz.ByteArray("0x" + this.targetValue()), this.cost());
		deferredResult.addCallback(MochiKit.Base.bind(function(aPayment) {
			var result;
		
			result = {
				targetValue: this.targetValue(),
				toll: aPayment.toHexString().substr(2)
			};
		
			return result;
		}, this));
//deferredResult.addLog("<<<--- deferredPay - " + this.cost());
		deferredResult.callback();
		
		return deferredResult;
	},
	
	//=========================================================================
	__syntaxFix__: "syntax fix"
	
});


Clipperz.PM.Toll.validate = function(aTargetValue, aToll, aCost) {
	var result;
	var	tollValue;
	var targetValue;
	var hashedTollValue;
	var payedToll;
	
	tollValue = new Clipperz.ByteArray("0x" + aToll);
	targetValue = new Clipperz.ByteArray("0x" + aTargetValue);
	hashedTollValue = Clipperz.Crypto.SHA.sha256(tollValue);
	
	payedToll = Clipperz.ByteArray.prefixMatchingBits(targetValue, hashedTollValue);
	
	if (payedToll < aCost) {
		result = false;
	} else {
		result = true;
	}
	
	return result;
};

Clipperz.PM.Toll.numberOfCloseLoopIterations = 50;
Clipperz.PM.Toll.pauseBetweenEachCloseLoop = 0.5;