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
if (typeof(Clipperz.PM.DataModel) == 'undefined') { Clipperz.PM.DataModel = {}; }


//#############################################################################

Clipperz.PM.DataModel.OneTimePassword = function(args) {
	args = args || {};

	this._username		= args['username'];
	this._passphraseCallback = args['passphraseCallback'];
	this._reference		= args['reference']	|| Clipperz.PM.Crypto.randomKey();
	this._password		= args['password'];
	this._passwordValue = Clipperz.PM.DataModel.OneTimePassword.normalizedOneTimePassword(args['password']);
	this._label			= args['label'] || "";
	this._usageDate		= args['usageDate'] || null; // Usage date is stored when the client is sure that the otp was used

	return this;
}

Clipperz.PM.DataModel.OneTimePassword.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "Clipperz.PM.DataModel.OneTimePassword";
	},

	'username': function() {
		return this._username;
	},

	'passphraseCallback': function () {
		return this._passphraseCallback;
	},

	'setPassphraseCallback': function(aPassphraseCallback) {
		this._passphraseCallback = aPassphraseCallback;
	},

	'password': function() {
		return this._password;
	},

	'label': function() {
		return this._label;
	},

	'usageDate': function() {
		return this._usageDate;
	},

	'setUsageDate': function(aDate) {
		this._usageDate = aDate;
	},
	
	//-------------------------------------------------------------------------

	'passwordValue': function() {
		return this._passwordValue;
	},

	//-------------------------------------------------------------------------

	'reference': function() {
		return this._reference;
	},

	//-------------------------------------------------------------------------

	'key': function() {
		return Clipperz.PM.DataModel.OneTimePassword.computeKeyWithPassword(this.passwordValue());
	},

	//-------------------------------------------------------------------------

	'keyChecksum': function() {
		return Clipperz.PM.DataModel.OneTimePassword.computeKeyChecksumWithUsernameAndPassword(this.username(), this.passwordValue());
	},
	
	//-------------------------------------------------------------------------

	'packedPassphrase': function(aPassphrase) {
		var result;
		var packedPassphrase;
		var encodedPassphrase;
		var	prefixPadding;
		var suffixPadding;
		var getRandomBytes;
		
		getRandomBytes = MochiKit.Base.method(Clipperz.Crypto.PRNG.defaultRandomGenerator(), 'getRandomBytes');
		
		encodedPassphrase = new Clipperz.ByteArray(aPassphrase).toBase64String();
		prefixPadding = getRandomBytes(getRandomBytes(1).byteAtIndex(0)).toBase64String();
		suffixPadding = getRandomBytes((500 - prefixPadding.length - encodedPassphrase.length) * 6 / 8).toBase64String();

		packedPassphrase = {
			'prefix': prefixPadding,
			'passphrase': encodedPassphrase,
			'suffix': suffixPadding
		};
		
		result = packedPassphrase;

		return result;
	},
	
	//-------------------------------------------------------------------------

	'encryptedPackedPassphrase': function(aPassphrase) {
		return Clipperz.PM.Crypto.deferredEncrypt({
			'key':		this.passwordValue(),
			'value':	this.packedPassphrase(aPassphrase),
			'version':	Clipperz.PM.Crypto.encryptingFunctions.currentVersion
		})
	},

	//-------------------------------------------------------------------------

	'encryptedData': function() {
		var deferredResult;
		var	result;

		result = {
			'reference': this.reference(),
			'key': this.key(),
			'keyChecksum': this.keyChecksum(),
			'data': "",
			'version': Clipperz.PM.Crypto.encryptingFunctions.currentVersion
		}
		deferredResult = new Clipperz.Async.Deferred("OneTimePassword.encryptedData", {trace: false});
		deferredResult.addCallback(this.passphraseCallback());
		deferredResult.addCallback(MochiKit.Base.method(this, 'encryptedPackedPassphrase'));
		deferredResult.addCallback(function(aResult, res) {
			aResult['data'] = res;
			return aResult;
		}, result);

		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

//#############################################################################

Clipperz.PM.DataModel.OneTimePassword.computeKeyWithPassword = function(aPassword) {
	return Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aPassword)).toHexString().substring(2);
}

Clipperz.PM.DataModel.OneTimePassword.computeKeyChecksumWithUsernameAndPassword = function(anUsername, aPassword) {
	return Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(anUsername + aPassword)).toHexString().substring(2);
}

//=============================================================================

Clipperz.PM.DataModel.OneTimePassword.isValidOneTimePasswordValue = function(aPassword) {
	var result;
	
//	"yaxx k7ww - f8y6 tqz5 - 58b6 th44 - 9cwv q0fg"
	if (aPassword.replace(/[^a-zA-Z0-9]/g, '').length == 32) {
		try {
			var passwordByteArray;
			
			passwordByteArray = new Clipperz.ByteArray();
			passwordByteArray.appendBase32String(aPassword);
			
			result = true;
		} catch(exception) {
			result = false;
		}
	} else {
		result = false;
	}

	return result;
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
};

//#############################################################################

Clipperz.PM.DataModel.OneTimePassword.generateRandomBase32OTPValue = function() {
	var randomValue;
	var	result;

	randomValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(160/8);
	result = randomValue.toBase32String();
	result = result.replace(/.{4}\B/g, '$&' + ' ');
	result = result.replace(/(.{4} ){2}/g, '$&' + '- ');

	return result;
};

//#############################################################################

Clipperz.PM.DataModel.OneTimePassword.createNewOneTimePassword = function(aUsername, aPassphraseCallback) {
	var result;
	var password;
	
	password = Clipperz.PM.DataModel.OneTimePassword.generateRandomBase32OTPValue();
	result = new Clipperz.PM.DataModel.OneTimePassword({
		'username':	aUsername,
		'passphraseCallback': aPassphraseCallback,
		'password': password,
		'label': ""
	});

	return result;
};