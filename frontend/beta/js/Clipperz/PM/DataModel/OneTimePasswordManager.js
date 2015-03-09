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

Clipperz.PM.DataModel.OneTimePasswordManager = function(anUser, args) {
	args = args || {};

	this._user = anUser;
	this._oneTimePasswords = {};

	this.updateWithData(args);

	Clipperz.NotificationCenter.notify(null, 'oneTimePasswordAdded', null, true);

	return this;
}

Clipperz.PM.DataModel.OneTimePasswordManager.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "Clipperz.PM.DataModel.OneTimePasswordManager";
	},

	//-------------------------------------------------------------------------

	'updateWithData': function(someValues) {
		var	otpReference;

//console.log("OneTimePasswordManager.updateWithData", someValues);
//MochiKit.Logging.logDebug("OneTimePasswordManager.updateWithData: " + Clipperz.Base.serializeJSON(someValues));
		for (otpReference in someValues) {
			var otp;
			var otpConfiguration;
		
			otpConfiguration = someValues[otpReference];
			otpConfiguration['user'] = this.user();
			otpConfiguration['reference'] = otpReference;
			otp = new Clipperz.PM.DataModel.OneTimePassword(otpConfiguration);
			this._oneTimePasswords[otpReference] = otp;
		}

		return this;
	},

	//-------------------------------------------------------------------------
	
	'updateWithServerData': function(someValues) {
		var deferredResult;
		var	oneTimePasswordReference;
		var wereChangesApplied;
		
//MochiKit.Logging.logDebug(">>> OneTimePasswordManager.updateWithServerData");
		deferredResult = new MochiKit.Async.Deferred();
		wereChangesApplied = false;
	
		for (oneTimePasswordReference in someValues) {
			var oneTimePassword;
		
			oneTimePassword = this.oneTimePasswordWithReference(oneTimePasswordReference);
			if (oneTimePassword != null) {
				var oneTimePasswordHasBeenUpdated;
				
				oneTimePasswordHasBeenUpdated = oneTimePassword.updateStatusWithValues(someValues[oneTimePasswordReference]);
				wereChangesApplied = oneTimePasswordHasBeenUpdated || wereChangesApplied;
			} else {

			}
		}

		if (wereChangesApplied == true) {
			this.user().header().markSectionAsUpdated('oneTimePasswords');
		}

		for (oneTimePasswordReference in this.oneTimePasswords()) {
			if (typeof(someValues[oneTimePasswordReference]) == 'undefind') {
				deferredResult.addCallback(MochiKit.Base.method(this.oneTimePasswordWithReference(oneTimePasswordReference), 'saveChanges'));
			}
		}
		
		deferredResult.addCallback(MochiKit.Async.succeed, this);
		
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< OneTimePasswordManager.updateWithServerData");

		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},
	
	//-------------------------------------------------------------------------

	'addOneTimePassword': function(aOneTimePassword, isBatchUpdate) {
		this.oneTimePasswords()[aOneTimePassword.reference()] = aOneTimePassword;
		
		if (isBatchUpdate != true) {
			Clipperz.NotificationCenter.notify(aOneTimePassword, 'oneTimePasswordAdded');
			Clipperz.NotificationCenter.notify(this.user(), 'updatedSection', 'oneTimePasswords', true);
		}
	},

	//-------------------------------------------------------------------------

	'archiveOneTimePassword': function(aOneTimePasswordReference) {
		var deferredResult;
		
//MochiKit.Logging.logDebug(">>> OneTimePasswordManager.archiveOneTimePassword");
//MochiKit.Logging.logDebug("--- OneTimePasswordManager.archiveOneTimePassword - 0 otp.reference: " + aOneTimePasswordReference);
		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(MochiKit.Base.method(this.user(), 'loadOneTimePasswords'));
		deferredResult.addCallback(MochiKit.Base.bind(function(aOneTimePasswordReference) {
			var oneTimePassword;
			
//MochiKit.Logging.logDebug("--- OneTimePasswordManager.archiveOneTimePassword - 1 serializedData: " + Clipperz.Base.serializeJSON(this.serializedData()));
			oneTimePassword = this.oneTimePasswords()[aOneTimePasswordReference];
			
			if (oneTimePassword != null) {
				oneTimePassword.setUsageDate(new Date());

//				while (this.usedOneTimePasswords().length > 10) {
//					var referenceOfOneTimePasswordToRemove;
//					
//					referenceOfOneTimePasswordToRemove = this.usedOneTimePasswords()[0];
//					delete this.oneTimePasswords()[referenceOfOneTimePasswordToRemove];
//					this.usedOneTimePasswords().shift();
//				}

				Clipperz.NotificationCenter.notify(this.user(), 'updatedSection', 'oneTimePasswords', true);
			} else {
				MochiKit.Logging.logError("### OneTimePasswordManager.archiveOneTimePassword - the used OneTimePassword has not been found on the index-card. :-(");
			}

//MochiKit.Logging.logDebug("--- OneTimePasswordManager.archiveOneTimePassword - 2 serializedData: " + Clipperz.Base.serializeJSON(this.serializedData()));
		}, this), aOneTimePasswordReference);
		deferredResult.addCallback(MochiKit.Base.method(this, 'saveChanges'));
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< OneTimePasswordManager.archiveOneTimePassword");

		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	'serializedData': function() {
		var result;
		var	key;
		
		result = {};

		for (key in this.oneTimePasswords()) {
			result[key] = this.oneTimePasswords()[key].serializedData();
		}
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'oneTimePasswords': function() {
		return this._oneTimePasswords;
	},

	//-------------------------------------------------------------------------

	'oneTimePasswordWithReference': function(aOneTimePasswordReference) {
		return this.oneTimePasswords()[aOneTimePasswordReference];
	},
	
	//-------------------------------------------------------------------------

	'deleteOneTimePasswordWithReference': function(aOneTimePasswordReference) {
		delete(this.oneTimePasswords()[aOneTimePasswordReference]);
		Clipperz.NotificationCenter.notify(this.user(), 'updatedSection', 'oneTimePasswords', true);
	},
	
	//-------------------------------------------------------------------------

	'encryptedData': function() {
		var deferredResult;
		var oneTimePasswordReferences;
		var result;
		var i, c;
		
		result = {};
		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(MochiKit.Async.succeed);

		oneTimePasswordReferences = MochiKit.Base.keys(this.oneTimePasswords());
		c = oneTimePasswordReferences.length;
		for (i=0; i<c; i++) {
			var currentOneTimePassword;
			
			currentOneTimePassword = this.oneTimePasswords()[oneTimePasswordReferences[i]];
			deferredResult.addCallback(MochiKit.Base.method(currentOneTimePassword, 'encryptedPackedPassphrase'));
			deferredResult.addCallback(function(aResult, aOneTimePasswordReference, anEncryptedPackedPassphrase) {
				aResult[aOneTimePasswordReference] = anEncryptedPackedPassphrase;
				return aResult;
			}, result, oneTimePasswordReferences[i]);
		}
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OneTimePasswordManager.encryptedData: " + res); return res;});
		
		deferredResult.callback(result);
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------
	
	'saveChanges': function() {
		var deferredResult;
		var	result;

//MochiKit.Logging.logDebug(">>> OneTimePasswordManager.saveChanges");
		result = {};
		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveOTP_encryptUserData');
		deferredResult.addCallback(MochiKit.Base.method(this.user(), 'encryptedData'));
		deferredResult.addCallback(function(aResult, res) {
			aResult['user'] = res;
			return aResult;
		}, result);

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveOTP_encryptOTPData');
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			res['oneTimePasswords'] = MochiKit.Base.keys(this.oneTimePasswords());
			return res;
		}, this));

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveOTP_sendingData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OneTimePasswordManager.saveChanges - 1: " + Clipperz.Base.serializeJSON(res)); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this.user().connection(), 'message'), 'updateOneTimePasswords');

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveOTP_updatingInterface');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OneTimePasswordManager.saveChanges - 2: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'notify', 'OTPUpdated');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OneTimePasswordManager.saveChanges - 3: " + res); return res;});
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< OneTimePasswordManager.saveChanges");
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

