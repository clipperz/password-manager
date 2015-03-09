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
if (typeof(Clipperz.PM.DataModel) == 'undefined') { Clipperz.PM.DataModel = {}; }


//#############################################################################

Clipperz.PM.DataModel.OneTimePassword = function(args) {
	args = args || {};

//console.log("new OneTimePassword", args);
//MochiKit.Logging.logDebug("---");
	this._user = args['user'];
	this._password = args['password'];
	this._passwordValue = Clipperz.PM.DataModel.OneTimePassword.normalizedOneTimePassword(args['password']);
	this._reference = args['reference'] || Clipperz.PM.Crypto.randomKey();
	this._creationDate =	args['created']		? Clipperz.PM.Date.parseDateWithUTCFormat(args['created'])		: new Date();
	this._usageDate =		args['used']		? Clipperz.PM.Date.parseDateWithUTCFormat(args['used'])			: null;

	this._status =			args['status'] || 'ACTIVE';
	this._connectionInfo = null;
	
	this._key = null;
	this._keyChecksum = null;

	return this;
}

Clipperz.PM.DataModel.OneTimePassword.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "Clipperz.PM.DataModel.OneTimePassword";
	},

	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},

	//-------------------------------------------------------------------------

	'password': function() {
		return this._password;
	},

	//-------------------------------------------------------------------------

	'passwordValue': function() {
		return this._passwordValue;
	},

	//-------------------------------------------------------------------------

	'creationDate': function() {
		return this._creationDate;
	},

	//-------------------------------------------------------------------------

	'reference': function() {
		return this._reference;
	},

	//-------------------------------------------------------------------------

	'key': function() {
		if (this._key == null) {
			this._key = Clipperz.PM.DataModel.OneTimePassword.computeKeyWithUsernameAndPassword(this.user().username(), this.passwordValue());
		}
		
		return this._key;
	},
	
	//-------------------------------------------------------------------------

	'keyChecksum': function() {
		if (this._keyChecksum == null) {
			this._keyChecksum = Clipperz.PM.DataModel.OneTimePassword.computeKeyChecksumWithUsernameAndPassword(this.user().username(), this.passwordValue());
		}
		
		return this._keyChecksum;
	},

	//-------------------------------------------------------------------------

	'status': function() {
		return this._status;
	},
	
	'setStatus': function(aValue) {
		this._status = aValue;
	},
	
	//-------------------------------------------------------------------------

	'serializedData': function() {
		var result;
		
		result = {
			'password': 	this.password(),
			'created':		this.creationDate()		? Clipperz.PM.Date.formatDateWithUTCFormat(this.creationDate())		: null,
			'used':			this.usageDate()		? Clipperz.PM.Date.formatDateWithUTCFormat(this.usageDate())		: null,
			'status':	 	this.status()
		};
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'packedPassphrase': function() {
		var result;
		var packedPassphrase;
		var encodedPassphrase;
		var	prefixPadding;
		var suffixPadding;
		var getRandomBytes;
		
		getRandomBytes = MochiKit.Base.method(Clipperz.Crypto.PRNG.defaultRandomGenerator(), 'getRandomBytes');
		
		encodedPassphrase = new Clipperz.ByteArray(this.user().passphrase()).toBase64String();
//MochiKit.Logging.logDebug("--- encodedPassphrase.length: " + encodedPassphrase.length);
		prefixPadding = getRandomBytes(getRandomBytes(1).byteAtIndex(0)).toBase64String();
//MochiKit.Logging.logDebug("--- prefixPadding.length: " + prefixPadding.length);
		suffixPadding = getRandomBytes((500 - prefixPadding.length - encodedPassphrase.length) * 6 / 8).toBase64String();
//MochiKit.Logging.logDebug("--- suffixPadding.length: " + suffixPadding.length);
//MochiKit.Logging.logDebug("--- total.length: " + (prefixPadding.length + encodedPassphrase.length + suffixPadding.length));
	
		packedPassphrase = {
			'prefix': prefixPadding,
			'passphrase': encodedPassphrase,
			'suffix': suffixPadding
		};
		
//		result = Clipperz.Base.serializeJSON(packedPassphrase);
		result = packedPassphrase;
//MochiKit.Logging.logDebug("===== OTP packedPassprase: [" + result.length + "]" + result);
//MochiKit.Logging.logDebug("<<< OneTimePassword.packedPassphrase");

		return result;
	},
	
	//-------------------------------------------------------------------------

	'encryptedPackedPassphrase': function() {
		return Clipperz.PM.Crypto.deferredEncryptWithCurrentVersion(this.passwordValue(), this.packedPassphrase())
	},

	//-------------------------------------------------------------------------

	'encryptedData': function() {
		var deferredResult;
		var	result;

//MochiKit.Logging.logDebug(">>> OneTimePassword.encryptedData");
//MochiKit.Logging.logDebug("--- OneTimePassword.encryptedData - id: " + this.reference());
		result = {
			'reference': this.reference(),
			'key': this.key(),
			'keyChecksum': this.keyChecksum(),
			'data': "",
			'version': Clipperz.PM.Crypto.encryptingFunctions.currentVersion
		}
//MochiKit.Logging.logDebug("--- OneTimePassword.encryptedData - 2: " + Clipperz.Base.serializeJSON(result));
		deferredResult = new MochiKit.Async.Deferred();
//MochiKit.Logging.logDebug("--- OneTimePassword.encryptedData - 3");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OneTimePassword.encryptedData - 1: " + res); return res;});
//#		deferredResult.addCallback(Clipperz.PM.Crypto.deferredEncryptWithCurrentVersion, this.passwordValue(), this.packedPassphrase());
		deferredResult.addCallback(MochiKit.Base.method(this, 'encryptedPackedPassphrase'));
//MochiKit.Logging.logDebug("--- OneTimePassword.encryptedData - 4");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OneTimePassword.encryptedData - 2: [" + res.length + "]" + res); return res;});
		deferredResult.addCallback(function(aResult, res) {
			aResult['data'] = res;
			return aResult;
		}, result);
//MochiKit.Logging.logDebug("--- OneTimePassword.encryptedData - 5");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OneTimePassword.encryptedData - 3: " + Clipperz.Base.serializeJSON(res)); return res;});
		deferredResult.callback();
//MochiKit.Logging.logDebug("--- OneTimePassword.encryptedData - 6");
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'saveChanges': function() {
		var deferredResult;
		var	result;

//MochiKit.Logging.logDebug(">>> OneTimePassword.saveChanges");
		result = {};
		deferredResult = new MochiKit.Async.Deferred();

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveOTP_encryptUserData');
		deferredResult.addCallback(MochiKit.Base.method(this.user(), 'encryptedData'));
		deferredResult.addCallback(function(aResult, res) {
			aResult['user'] = res;
			return aResult;
		}, result);

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveOTP_encryptOTPData');
		deferredResult.addCallback(MochiKit.Base.method(this, 'encryptedData'));
		deferredResult.addCallback(function(aResult, res) {
			aResult['oneTimePassword'] = res;
			return aResult;
		}, result);

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveOTP_sendingData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OneTimePassword.saveChanges - 1: " + Clipperz.Base.serializeJSON(res)); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this.user().connection(), 'message'), 'addNewOneTimePassword');

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveOTP_updatingInterface');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OneTimePassword.saveChanges - 2: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'notify', 'OTPUpdated');
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'oneTimePassword_saveChanges_done', null);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OneTimePassword.saveChanges - 2: " + res); return res;});
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< OneTimePassword.saveChanges");
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'usageDate': function() {
		return this._usageDate;
	},

	'setUsageDate': function(aValue) {
		this._usageDate = aValue;
	},

	//-------------------------------------------------------------------------

	'connectionInfo': function() {
		return this._connectionInfo;
	},

	'setConnectionInfo': function(aValue) {
		this._connectionInfo = aValue;
	},
	
	//-------------------------------------------------------------------------

	'isExpired': function() {
		return (this.usageDate() != null);
	},

	//-------------------------------------------------------------------------

	'updateStatusWithValues': function(someValues) {
		var result;

		result = false;
		
		if (someValues['status'] != this.status()) {
			result = true;
		}
		
		this.setStatus(someValues['status']);
		this.setUsageDate(Clipperz.PM.Date.parseDateWithUTCFormat(someValues['requestDate']));
		this.setConnectionInfo(someValues['connection']);

		return result;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

//=============================================================================

Clipperz.PM.DataModel.OneTimePassword.computeKeyWithUsernameAndPassword = function(anUsername, aPassword) {
	return Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aPassword)).toHexString().substring(2);
}

Clipperz.PM.DataModel.OneTimePassword.computeKeyChecksumWithUsernameAndPassword = function(anUsername, aPassword) {
	return Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(anUsername + aPassword)).toHexString().substring(2);
}

//=============================================================================

Clipperz.PM.DataModel.OneTimePassword.normalizedOneTimePassword = function(aPassword) {
	var	result;
	
	if (aPassword.replace(/[\s\-]/g, '').length == 32) {
		try {
			var passwordByteArray;
			
			passwordByteArray = new Clipperz.ByteArray();
			passwordByteArray.appendBase32String(aPassword);
			
			result = passwordByteArray.toBase64String();
		} catch(exception) {
			result = aPassword;
		}
	} else {
		result = aPassword;
	}
	
	return result;
}

//=============================================================================
