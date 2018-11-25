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

try { if (typeof(Clipperz.PM.DataModel.User) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.PM.DataModel.User.Header.OneTimePasswords depends on Clipperz.PM.DataModel.User!";
}  
if (typeof(Clipperz.PM.DataModel.User.Header) == 'undefined') { Clipperz.PM.DataModel.User.Header = {}; }

//-----------------------------------------------------------------------------

Clipperz.PM.DataModel.User.Header.OneTimePasswords = function(args) {
	Clipperz.PM.DataModel.User.Header.OneTimePasswords.superclass.constructor.apply(this, arguments);

	// TODO: there are still method calls around passing these values: should be cleared...
	this._connection = args['connection'];
	this._username = args['username'];
	this._passphraseCallback = args['retrieveKeyFunction'];

	this._oneTimePasswords = null;
	this._oneTimePasswordsDetails = null;
	
	return this;
}

//-----------------------------------------------------------------------------

Clipperz.Base.extend(Clipperz.PM.DataModel.User.Header.OneTimePasswords, Clipperz.PM.DataModel.EncryptedRemoteObject, {

	'toString': function() {
		return "Clipperz.PM.DataModel.User.Header.OneTimePasswords";
	},

	//-------------------------------------------------------------------------

	'connection': function() {
		return this._connection;
	},

	'username': function() {
		return this._username;
	},

	'passphraseCallback': function() {
		return this._passphraseCallback;
	},

	//=========================================================================

	'oneTimePasswords': function () {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("User.Header.OneTimePasswords.oneTimePasswords", {trace:false});

		// TODO: change with transient state
		// Also, OTPs created here don't store username, making it impossible to generate the key checksum (shouldn't be used anywhere, but probably the design should be changed)
		if (this._oneTimePasswords == null) {
			deferredResult.addMethod(this, 'values');
			deferredResult.addCallback(MochiKit.Base.bind(function (someData) {
				var	otpKey;
				
				this._oneTimePasswords = {};

				for (otpKey in someData) {
					var otp;
					var otpParameters;
					
					otpParameters = Clipperz.Base.deepClone(someData[otpKey]);
					otpParameters['reference'] = otpKey;
					otpParameters['username'] = this.username();
					otpParameters['passphraseCallback'] = this.passphraseCallback();
					otpParameters['usageDate'] = someData[otpKey]['usageDate'] || null;

					otp = new Clipperz.PM.DataModel.OneTimePassword(otpParameters);
					this._oneTimePasswords[otpKey] = otp;
				}

				return this._oneTimePasswords;

			}, this));
			deferredResult.callback();
		} else {
			deferredResult = MochiKit.Async.succeed(this._oneTimePasswords);
		}
		
		return deferredResult;
	},

	'oneTimePasswordsDetails': function() {
		if (this._oneTimePasswordsDetails) {
			return MochiKit.Async.succeed(this._oneTimePasswordsDetails);
		} else {
			return Clipperz.Async.callbacks("User.oneTimePasswordsDetails", [
				MochiKit.Base.method(this.connection(), 'message', 'getOneTimePasswordsDetails'),
				MochiKit.Base.bind(function(someData) {
					this._oneTimePasswordsDetails = someData;

					return someData;
				}, this)
			], {trace:false});
		}
	},

	//=========================================================================

	'getReferenceFromKey': function(aKey) {
		return Clipperz.Async.callbacks("User.Header.OneTimePasswords.getReferenceFromKey", [
			MochiKit.Base.method(this, 'values'),
			function(someValues) {
				var result;
				var normalizedOTP;
				var i;

				result = null;
				for (i in someValues) {
					normalizedOTP = Clipperz.PM.DataModel.OneTimePassword.normalizedOneTimePassword(someValues[i]['password']);

					if (Clipperz.PM.DataModel.OneTimePassword.computeKeyWithPassword(normalizedOTP) == aKey) {
						result = i;
					}
				}

				return result;
			}
		], {trace:false});
	},

	//=========================================================================

	'createNewOTP': function (aUsername, aPassphraseCallback) {
		var newOneTimePassword;

		newOneTimePassword = Clipperz.PM.DataModel.OneTimePassword.createNewOneTimePassword(aUsername, aPassphraseCallback);

		// TODO: this is deferred --> change everything to deferred
		// TODO: TestData include 'created' and 'status': check if status is necessary
		this.setValue(newOneTimePassword.reference(), {
			'password': newOneTimePassword.password(),
			'label': newOneTimePassword.label()
			// 'status': newOneTimePassword.status()
		});

		this._oneTimePasswords = null;
		this._oneTimePasswordsDetails = null;

		return newOneTimePassword;
	},

	//.........................................................................

	'deleteOTPs': function (aList) {
		this._oneTimePasswords = null;
		this._oneTimePasswordsDetails = null;

		return Clipperz.Async.callbacks("User.Header.OneTimePasswords.deleteOTPs", [
			MochiKit.Base.method(this, 'values'),
			MochiKit.Base.keys,
			MochiKit.Base.bind(function(someKeys) {
				var result;
				
				result = [];
				MochiKit.Base.map(MochiKit.Base.bind(function(aList, aKey) {
					if (aList.indexOf(aKey) >= 0) {
						this.removeValue(aKey);
					} else {
						result.push(aKey);
					}
				}, this, aList), someKeys);

				return result;	// Return a list of references of the remaining OTPs, needed for the 'updateOneTimePasswords' message
								// Maybe this logic should be moved to another method
			}, this),
		], {trace:false});
	},

	//.........................................................................

	'changeOTPLabel': function (aReference, aLabel) {
		this._oneTimePasswords = null;

		return Clipperz.Async.callbacks("User.Header.OneTimePasswords.changeOTPLabel", [
			MochiKit.Base.method(this, 'getValue', aReference),
			function(aValue) {
				aValue['label'] = aLabel;
				return aValue;
			},
			MochiKit.Base.method(this, 'setValue', aReference)
		], {trace:false});
	},

	//.........................................................................

	'markOTPAsUsed': function(aKey) {
		var reference;

		this._oneTimePasswords = null;

		return Clipperz.Async.callbacks("User.Header.OneTimePasswords.markOTPAsUsed", [
			MochiKit.Base.method(this, 'getReferenceFromKey', aKey),
			function(aReference) {
				reference = aReference;
				return aReference;
			},
			MochiKit.Base.method(this, 'getValue'),
			MochiKit.Base.bind(function(aValue) {
				if (aValue) {
					aValue['usageDate'] = new Date().toString();
					this.setValue(reference, aValue);
				}
			}, this)
		], {trace:false});
	},

	//=========================================================================

	'getEncryptedOTPData': function(aPassphraseCallback) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("User.Header.OneTimePasswords.getEncryptedOTPData", {trace:false});

		deferredResult.collectResults({
			'oneTimePasswords': MochiKit.Base.method(this, 'oneTimePasswords'),
			'oneTimePasswordsDetails': MochiKit.Base.method(this, 'oneTimePasswordsDetails')
		});
		deferredResult.addCallback(function (someData) {
			var result;
			var otpFilteredList;
			var i;

			var otpList = MochiKit.Base.values(someData['oneTimePasswords']);

			otpFilteredList = MochiKit.Base.filter(function (aOTP) {
				return (someData['oneTimePasswordsDetails'][aOTP.reference()]
					&&	someData['oneTimePasswordsDetails'][aOTP.reference()]['status'] == 'ACTIVE'
					&&	! someData['oneTimePasswords'][aOTP.reference()].usageDate()
				);
			}, otpList);
			
			result = MochiKit.Base.map(function (aOTP) {
				aOTP.setPassphraseCallback(aPassphraseCallback);
				return aOTP.encryptedData();
			}, otpFilteredList);

			return result;
		});
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addCallback(function (someData) {
			var result;
			var i;
			
			result = {};
			for (i in someData) {
				result[someData[i].reference] = someData[i];
			}

			return result;
		});

		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});

//-----------------------------------------------------------------------------

