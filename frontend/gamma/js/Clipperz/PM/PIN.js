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

	'CREDENTIALS':   'CLIPPERZ.CREDENTIALS',
	'FAILURE_COUNT': 'CLIPPERZ.FAILED_LOGIN_COUNT',
	'ALLOWED_RETRY': 3,

	//-------------------------------------------------------------------------

	'isSet': function () {
		return (this.storedCredentials() != null);
	},

	'storedCredentials': function () {
		return localStorage[this.CREDENTIALS];
	},

	//-------------------------------------------------------------------------

	'recordFailedAttempt': function () {
		var	failureCount;
		var	result;

		failureCount = localStorage[this.FAILURE_COUNT];

		if (failureCount == null) {
			failureCount = 0
		}

		failureCount ++;

		if (failureCount < this.ALLOWED_RETRY) {
			localStorage[this.FAILURE_COUNT] = failureCount;
			result = failureCount;
		} else {
			this.removeLocalCredentials();
			result = -1;
		}

		return result;
	},

	'resetFailedAttemptCount': function () {
		localStorage.removeItem(this.FAILURE_COUNT);
	},

	'failureCount': function () {
		return localStorage[this.FAILURE_COUNT];
	},

	//-------------------------------------------------------------------------

	'deriveKeyFromPin': function (aPIN) {
		return Clipperz.Crypto.SHA.sha256(new Clipperz.ByteArray(aPIN));
	},

	'credentialsWithPIN': function (aPIN) {
		var	byteArrayValue;
		var decryptedValue;
		var	result;

		byteArrayValue = (new Clipperz.ByteArray()).appendBase64String(localStorage[this.CREDENTIALS]);
		decryptedValue = Clipperz.Crypto.AES.decrypt(this.deriveKeyFromPin(aPIN), byteArrayValue).asString();
		try {
			result = Clipperz.Base.evalJSON(decryptedValue);
		} catch (error) {
			result = {'username':'fakeusername', 'passphrase':'fakepassphrase'};
		}

		return result;
	},

	'setCredentialsWithPIN': function (aPIN, someCredentials) {
		var	encodedValue;
		var	byteArrayValue;
		var encryptedValue;

		encodedValue = Clipperz.Base.serializeJSON(someCredentials);
		byteArrayValue = new Clipperz.ByteArray(encodedValue);
		encryptedValue = Clipperz.Crypto.AES.encrypt(this.deriveKeyFromPin(aPIN), byteArrayValue).toBase64String();

		localStorage[this.CREDENTIALS] = encryptedValue;
	},

	'removeLocalCredentials': function () {
		localStorage.removeItem(this.CREDENTIALS);
		localStorage.removeItem(this.FAILURE_COUNT);
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});

