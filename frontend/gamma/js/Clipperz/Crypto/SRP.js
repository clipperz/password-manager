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

try { if (typeof(Clipperz.ByteArray) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.Crypto.PRNG depends on Clipperz.ByteArray!";
}  

try { if (typeof(Clipperz.Crypto.BigInt) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.Crypto.SRP depends on Clipperz.Crypto.BigInt!";
}  

try { if (typeof(Clipperz.Crypto.PRNG) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.Crypto.SRP depends on Clipperz.Crypto.PRNG!";
}  

if (typeof(Clipperz.Crypto.SRP) == 'undefined') { Clipperz.Crypto.SRP = {}; }

Clipperz.Crypto.SRP.VERSION = "0.1";
Clipperz.Crypto.SRP.NAME = "Clipperz.Crypto.SRP";

//#############################################################################

MochiKit.Base.update(Clipperz.Crypto.SRP, {

	'_n': null,
	'_g': null,
	//-------------------------------------------------------------------------

	'n': function() {
		if (Clipperz.Crypto.SRP._n == null) {
		 	Clipperz.Crypto.SRP._n = new Clipperz.Crypto.BigInt("115b8b692e0e045692cf280b436735c77a5a9e8a9e7ed56c965f87db5b2a2ece3", 16);
		}
		
		return Clipperz.Crypto.SRP._n;
	},

	//-------------------------------------------------------------------------

	'g': function() {
		if (Clipperz.Crypto.SRP._g == null) {
			Clipperz.Crypto.SRP._g = new Clipperz.Crypto.BigInt(2);	//	eventually 5 (as suggested on the Diffi-Helmann documentation)
		}
		
		return Clipperz.Crypto.SRP._g;
	},

	//-----------------------------------------------------------------------------

	'exception': {
		'InvalidValue': new MochiKit.Base.NamedError("Clipperz.Crypto.SRP.exception.InvalidValue") 
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});

//#############################################################################
//
//		S R P   C o n n e c t i o n     version 1.0
//
//=============================================================================
Clipperz.Crypto.SRP.Connection = function (args) {
	args = args || {};

	this._C = args.C;
	this._P = args.P;
	this.hash = args.hash;

	this._a = null;
	this._A = null;
	
	this._s = null;
	this._B = null;

	this._x = null;
	
	this._u = null;
	this._K = null;
	this._M1 = null;
	this._M2 = null;
	
	this._sessionKey = null;

	return this;
}

Clipperz.Crypto.SRP.Connection.prototype = MochiKit.Base.update(null, {

	'toString': function () {
		return "Clipperz.Crypto.SRP.Connection (username: " + this.username() + "). Status: " + this.statusDescription();
	},

	//-------------------------------------------------------------------------

	'C': function () {
		return this._C;
	},

	//-------------------------------------------------------------------------

	'P': function () {
		return this._P;
	},

	//-------------------------------------------------------------------------

	'a': function () {
		if (this._a == null) {
			this._a = new Clipperz.Crypto.BigInt(Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32).toHexString().substring(2), 16);
//			this._a = new Clipperz.Crypto.BigInt("37532428169486597638072888476611365392249575518156687476805936694442691012367", 10);
//MochiKit.Logging.logDebug("SRP a: " + this._a);
		}
		
		return this._a;
	},

	//-------------------------------------------------------------------------

	'A': function () {
		if (this._A == null) {
			//	Warning: this value should be strictly greater than zero: how should we perform this check?
			this._A = Clipperz.Crypto.SRP.g().powerModule(this.a(), Clipperz.Crypto.SRP.n());
			
			if (this._A.equals(0)) {
MochiKit.Logging.logError("Clipperz.Crypto.SRP.Connection: trying to set 'A' to 0.");
				throw Clipperz.Crypto.SRP.exception.InvalidValue;
			}
//MochiKit.Logging.logDebug("SRP A: " + this._A);
		}
		
		return this._A;
	},

	//-------------------------------------------------------------------------

	's': function () {
		return this._s;
//MochiKit.Logging.logDebug("SRP s: " + this._S);
	},

	'set_s': function(aValue) {
		this._s = aValue;
	},
	
	//-------------------------------------------------------------------------

	'B': function () {
		return this._B;
	},

	'set_B': function(aValue) {
		//	Warning: this value should be strictly greater than zero: how should we perform this check?
		if (! aValue.equals(0)) {
			this._B = aValue;
//MochiKit.Logging.logDebug("SRP B: " + this._B);
		} else {
MochiKit.Logging.logError("Clipperz.Crypto.SRP.Connection: trying to set 'B' to 0.");
			throw Clipperz.Crypto.SRP.exception.InvalidValue;
		}
	},
	
	//-------------------------------------------------------------------------

	'x': function () {
		if (this._x == null) {
			this._x = new Clipperz.Crypto.BigInt(this.stringHash(this.s().asString(16, 64) + this.P()), 16);
//MochiKit.Logging.logDebug("SRP x: " + this._x);
		}
		
		return this._x;
	},

	//-------------------------------------------------------------------------

	'u': function () {
		if (this._u == null) {
			this._u = new Clipperz.Crypto.BigInt(this.stringHash(this.B().asString()), 16);
//MochiKit.Logging.logDebug("SRP u: " + this._u);
		}
		
		return this._u;
	},

	//-------------------------------------------------------------------------

	'S': function () {
		if (this._S == null) {
			var bigint;
			var	srp;

			bigint = Clipperz.Crypto.BigInt;
			srp = 	 Clipperz.Crypto.SRP;

			this._S =	bigint.powerModule(
								bigint.subtract(this.B(), bigint.powerModule(srp.g(), this.x(), srp.n())),
								bigint.add(this.a(), bigint.multiply(this.u(), this.x())),
								srp.n()
						)
//MochiKit.Logging.logDebug("SRP S: " + this._S);
		}
		
		return this._S;
	},

	//-------------------------------------------------------------------------

	'K': function () {
		if (this._K == null) {
			this._K = this.stringHash(this.S().asString());
//MochiKit.Logging.logDebug("SRP K: " + this._K);
		}
		
		return this._K;
	},

	//-------------------------------------------------------------------------

	'M1': function () {
		if (this._M1 == null) {
			this._M1 = this.stringHash(this.A().asString(10) + this.B().asString(10) + this.K());
//MochiKit.Logging.logDebug("SRP M1: " + this._M1);
		}
		
		return this._M1;
	},

	//-------------------------------------------------------------------------

	'M2': function () {
		if (this._M2 == null) {
			this._M2 = this.stringHash(this.A().asString(10) + this.M1() + this.K());
//MochiKit.Logging.logDebug("SRP M2: " + this._M2);
		}
		
		return this._M2;
	},

	//=========================================================================

	'serverSideCredentialsWithSalt': function(aSalt) {
		var result;
		var s, x, v;
		
		s = aSalt;
		x = this.stringHash(s + this.P());
		v = Clipperz.Crypto.SRP.g().powerModule(new Clipperz.Crypto.BigInt(x, 16), Clipperz.Crypto.SRP.n());

		result = {};
		result['C'] = this.C();
		result['s'] = s;
		result['v'] = v.asString(16);
		
		return result;
	},
	
	'serverSideCredentials': function() {
		var result;
		var s;
		
		s = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32).toHexString().substring(2);

		result = this.serverSideCredentialsWithSalt(s);
		
		return result;
	},
	
	//=========================================================================
/*
	'computeServerSide_S': function(b) {
		var result;
		var v;
		var bigint;
		var	srp;

		bigint = Clipperz.Crypto.BigInt;
		srp = 	 Clipperz.Crypto.SRP;

		v = new Clipperz.Crypto.BigInt(srpConnection.serverSideCredentialsWithSalt(this.s().asString(16, 64)).v, 16);
//		_S =  (this.A().multiply(this.v().modPow(this.u(), this.n()))).modPow(this.b(), this.n());
		result = bigint.powerModule(
					bigint.multiply(
						this.A(),
						bigint.powerModule(v, this.u(), srp.n())
					), new Clipperz.Crypto.BigInt(b, 10), srp.n()
				);

		return result;
	},
*/
	//=========================================================================

	'stringHash': function(aValue) {
		var	result;

		result = this.hash(new Clipperz.ByteArray(aValue)).toHexString().substring(2);
		
		return result;
	},
	
	//=========================================================================
	__syntaxFix__: "syntax fix"
	
});

//#############################################################################
