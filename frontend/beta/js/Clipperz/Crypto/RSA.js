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

try { if (typeof(Clipperz.Crypto.BigInt) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.Crypto.RSA depends on Clipperz.Crypto.BigInt!";
}  

if (typeof(Clipperz.Crypto.RSA) == 'undefined') { Clipperz.Crypto.RSA = {}; }

Clipperz.Crypto.RSA.VERSION = "0.1";
Clipperz.Crypto.RSA.NAME = "Clipperz.RSA";

//#############################################################################

MochiKit.Base.update(Clipperz.Crypto.RSA, {

	//-------------------------------------------------------------------------

	'publicKeyWithValues': function (e, d, n) {
		var	result;
		
		result = {};

		if (e.isBigInt) {
			result.e = e;
		} else {
			result.e = new Clipperz.Crypto.BigInt(e, 16);
		}
		
		if (d.isBigInt) {
			result.d = d;
		} else {
			result.d = new Clipperz.Crypto.BigInt(d, 16);
		}
		
		if (n.isBigInt) {
			result.n = n;
		} else {
			result.n = new Clipperz.Crypto.BigInt(n, 16);
		}
		
		return result;
	},

	'privateKeyWithValues': function(e, d, n) {
		return Clipperz.Crypto.RSA.publicKeyWithValues(e, d, n);
	},

	//-----------------------------------------------------------------------------

	'encryptUsingPublicKey': function (aKey, aMessage) {
		var	messageValue;
		var	result;
		
		messageValue = new Clipperz.Crypto.BigInt(aMessage, 16);
		result = messageValue.powerModule(aKey.e, aKey.n);
		
		return result.asString(16);
	},

	//.............................................................................

	'decryptUsingPublicKey': function (aKey, aMessage) {
		return Clipperz.Crypto.RSA.encryptUsingPublicKey(aKey, aMessage);
	},

	//-----------------------------------------------------------------------------

	'encryptUsingPrivateKey': function (aKey, aMessage) {
		var	messageValue;
		var	result;
		
		messageValue = new Clipperz.Crypto.BigInt(aMessage, 16);
		result = messageValue.powerModule(aKey.d, aKey.n);
		
		return result.asString(16);
	},

	//.............................................................................

	'decryptUsingPrivateKey': function (aKey, aMessage) {
		return Clipperz.Crypto.RSA.encryptUsingPrivateKey(aKey, aMessage);
	},

	//-----------------------------------------------------------------------------
	
	'generatePublicKey': function(aNumberOfBits) {
		var	result;
		var	e;
		var	d;
		var	n;
		
		e = new Clipperz.Crypto.BigInt("10001", 16);

		{
			var p, q;
			var	phi;

			do {
				p = Clipperz.Crypto.BigInt.randomPrime(aNumberOfBits);
			} while (p.module(e).equals(1));
			
			do {
				q = Clipperz.Crypto.BigInt.randomPrime(aNumberOfBits);
			} while ((q.equals(p)) || (q.module(e).equals(1)));

			n = p.multiply(q);
			phi = (p.subtract(1).multiply(q.subtract(1)));
			d = e.powerModule(-1, phi);
		}
		
		result = Clipperz.Crypto.RSA.publicKeyWithValues(e, d, n);
		
		return result;
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
	
	//-------------------------------------------------------------------------

});

//#############################################################################

