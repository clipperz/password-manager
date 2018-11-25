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
if (typeof(Clipperz.PM.PIN) == 'undefined') { Clipperz.PM.PIN = {}; }

MochiKit.Base.update(Clipperz.PM.PIN, {

	//-------------------------------------------------------------------------

	'__repr__': function () {
		return "[" + this.NAME + " " + this.VERSION + "]";
	},

	//-------------------------------------------------------------------------

	'toString': function () {
		return this.__repr__();
	},

	'ENCRYPTED_PASSPHRASE_LENGTH': 1024,
	'DEFAULT_PIN_LENGTH': 5,
	'ALLOWED_RETRY': 3,

	'LS_USERNAME':   'clipperz.pin.username',
	'LS_PASSPHRASE':   'clipperz.pin.passphrase',
	'LS_FAILURE_COUNT': 'clipperz.pin.failureCount',

	//-------------------------------------------------------------------------

	'isSet': function () {
		return (localStorage[this.LS_USERNAME] && localStorage[this.LS_PASSPHRASE]);
	},

	//-------------------------------------------------------------------------

	'recordFailedAttempt': function () {
		var	failureCount;
		var	result;

		failureCount = localStorage[this.LS_FAILURE_COUNT];

		if (failureCount == null) {
			failureCount = 0
		}

		failureCount ++;

		if (failureCount < this.ALLOWED_RETRY) {
			localStorage[this.LS_FAILURE_COUNT] = failureCount;
			result = failureCount;
		} else {
			this.disablePin();
			result = -1;
		}

		return result;
	},

	'resetFailedAttemptCount': function () {
		localStorage.removeItem(this.LS_FAILURE_COUNT);
	},

	'failureCount': function () {
		return localStorage[this.LS_FAILURE_COUNT];
	},

	//-------------------------------------------------------------------------

	'deriveKeyFromPin': function (aPIN) {
		return Clipperz.Crypto.SHA.sha256(new Clipperz.ByteArray(aPIN));
	},

	'credentialsWithPIN': function(aPIN) {
		return {
			'username': localStorage[this.LS_USERNAME],
			'passphrase': this.decryptPassphraseWithPin(aPIN, localStorage[this.LS_PASSPHRASE]),
		}
	},

	'encryptPassphraseWithPin': function(aPIN, aPassphrase) {
		var byteArrayPassphrase = new Clipperz.ByteArray(aPassphrase);
		var randomBytesLength = this.ENCRYPTED_PASSPHRASE_LENGTH-byteArrayPassphrase.length()-1;
		var randomBytes = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(randomBytesLength);
		var derivedKey = this.deriveKeyFromPin(aPIN);

		byteArrayPassphrase.appendByte(0);
		byteArrayPassphrase.appendBytes(randomBytes.arrayValues());

		return Clipperz.Crypto.AES_2.encrypt(derivedKey, byteArrayPassphrase).toBase64String();
	},

	'decryptPassphraseWithPin': function(aPIN, anEncryptedPassphrase) {
		var byteArrayEncryptedPassphrase = (new Clipperz.ByteArray()).appendBase64String(anEncryptedPassphrase);
		var derivedKey = this.deriveKeyFromPin(aPIN);
		var byteArrayPassphrase = Clipperz.Crypto.AES_2.decrypt(derivedKey, byteArrayEncryptedPassphrase);
		var arrayPassphrase = byteArrayPassphrase.arrayValues();
		var slicedArrayPassphrase = arrayPassphrase.slice(0, arrayPassphrase.indexOf(0));

		return new Clipperz.ByteArray(slicedArrayPassphrase).asString();
	},

	'updatePin': function(aUser, aPIN) {
		return Clipperz.Async.callbacks("Clipperz.PM.PIN", [
			MochiKit.Base.method(aUser, 'username'),
			MochiKit.Base.method(localStorage, 'setItem', this.LS_USERNAME),
			MochiKit.Base.method(aUser, 'getPassphrase'),
			MochiKit.Base.method(this, 'encryptPassphraseWithPin', aPIN),
			MochiKit.Base.method(localStorage, 'setItem', this.LS_PASSPHRASE),
			MochiKit.Base.method(localStorage, 'setItem', this.LS_FAILURE_COUNT, 0),
		], {trace:false});
	},

	'disablePin': function () {
		localStorage.removeItem(this.LS_USERNAME);
		localStorage.removeItem(this.LS_PASSPHRASE);
		localStorage.removeItem(this.LS_FAILURE_COUNT);
	},

	'isLocalStorageSupported': function() {
		var result;

		var test = 'test';
		try {
		    localStorage.setItem(test, test);
		    localStorage.removeItem(test);
		    result = true;
		} catch(e) {
		    result = false;
		}

		return result;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});

