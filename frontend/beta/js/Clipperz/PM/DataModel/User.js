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

Clipperz.PM.DataModel.User = function(args) {
//MochiKit.Logging.logDebug(">>> new User");
	args = args || {};

	this._username = args.username || null;
	this._passphrase = args.passphrase || null;

	this._connection = null;
	this._connectionVersion = 'current';

	this._header = null;
	this._statistics = null;
	this._lock = 'new lock';

	this._preferences = null;
	this._records = {};
	this._directLoginReferences = {};
	this._oneTimePasswordManager = null;
	
	this._isLoadingUserDetails = false;
	this._loadingUserDetailsPendingQueue = [];
	
	this._maxNumberOfRecords = Number.MAX_VALUE;

	this._shouldDownloadOfflineCopy = false;
	
	this._loginInfo = null;
	this._loginHistory = null;
	
	this._serverData = null;
//MochiKit.Logging.logDebug("<<< new User");

	return this;
}

Clipperz.PM.DataModel.User.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "Clipperz.PM.DataModel.User - " + this.username();
	},

	//-------------------------------------------------------------------------

	'username': function() {
		return this._username;
	},

	'setUsername': function(aValue) {
		this._username = aValue;
	},
	
	//-------------------------------------------------------------------------

	'passphrase': function() {
		return this._passphrase;
	},

	'setPassphrase': function(aValue) {
		this._passphrase = aValue;
	},
	
	//-------------------------------------------------------------------------

	'maxNumberOfRecords': function() {
		return this._maxNumberOfRecords;
	},
	
	'setMaxNumberOfRecords': function(aValue) {
		this._maxNumberOfRecords = aValue;
	},

	//-------------------------------------------------------------------------

	'errorHandler': function(anErrorString, anException) {
MochiKit.Logging.logError("- User.errorHandler: " + anErrorString + " (" + anException + ")");
	},

	//-------------------------------------------------------------------------

	'connectionVersion': function() {
		return this._connectionVersion;
	},
	
	'setConnectionVersion': function(aValue) {
		this._connectionVersion = aValue;
	},

	//-------------------------------------------------------------------------
	
	'connection': function() {
		if ((this._connection == null) && (this.connectionVersion() != null) ){
			this._connection = new Clipperz.PM.Crypto.communicationProtocol.versions[this.connectionVersion()]({user:this});
		}
				
		return this._connection;
	},
	
	'resetConnection': function(aValue) {
		this._connection = null;
	},

	//=========================================================================

	'register': function(anInvitationCode) {
		var	deferredResult;
		var prng;
		
//MochiKit.Logging.logError(">>> User.register: " + this);
		prng = Clipperz.Crypto.PRNG.defaultRandomGenerator();
		
		deferredResult = new MochiKit.Async.Deferred()
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.register - 1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(prng, 'deferredEntropyCollection'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.register - 2: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this.header(), 'updateAllSections'), anInvitationCode);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.register - 2.1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'register'), anInvitationCode);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.register - 3: " + res); return res;});
		deferredResult.callback();
//MochiKit.Logging.logError("<<< User.register");
		
		return deferredResult;
	},

	//=========================================================================

	'connect': function(aValue) {
		var	deferredResult;
		var prng;
		
		prng = Clipperz.Crypto.PRNG.defaultRandomGenerator();
		
//MochiKit.Logging.logDebug(">>> User.connect");
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.2.1 - User.connect - 1: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(prng, 'deferredEntropyCollection'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.2.2 - User.connect - 2: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'login'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.2.3 - User.connect - 3: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});

//	TODO:	add an addErrback call here to manage a wrong login. Any error after this point is due to some other causes.
//			possibly the same exact 'handleConnectionFallback use at the end of this same method.
			
		if (this.connectionVersion() != 'current') {
			var	currentConnection;
		
			currentVersionConnection = new Clipperz.PM.Crypto.communicationProtocol.versions['current']({user:this});

//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.2.4 - User.connect - 4: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'connection_upgrading');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.2.5 - User.connect - 5: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'message'), 'upgradeUserCredentials', currentVersionConnection.serverSideUserCredentials());
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.2.6 - User.connect - 6: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		}

//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.2.7 - User.connect - 7: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'userConnected', null);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.2.8 - User.connect - 8: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addErrback(MochiKit.Base.method(this, 'handleConnectionFallback'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.2.9 - User.connect - 9: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});

		deferredResult.callback(aValue);
//MochiKit.Logging.logDebug("<<< User.connect");
		
		return deferredResult;
	},

	//.........................................................................

	'handleConnectionFallback': function(aValue) {
		var result;
//MochiKit.Logging.logDebug(">>> User.handleConnectionFallback");
		if (aValue instanceof MochiKit.Async.CancelledError) {
//MochiKit.Logging.logDebug("--- User.handleConnectionFallback - operation cancelled");
			result = aValue;
		} else {
			
//MochiKit.Logging.logDebug("--- User.handleConnectionFallback - an ERROR has occurred - " + aValue);
			this.resetConnection();
			this.setConnectionVersion(Clipperz.PM.Crypto.communicationProtocol.fallbackVersions[this.connectionVersion()]);

			if (this.connectionVersion() != null) {
				result = new MochiKit.Async.Deferred();
				
				result.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'connection_tryOlderSchema');
				result.addCallback(MochiKit.Base.method(this, 'connect'));
				result.callback();
			} else {
				result = MochiKit.Async.fail(Clipperz.PM.DataModel.User.exception.LoginFailed);
			}
		}
//MochiKit.Logging.logDebug("<<< User.handleConnectionFallback");
		return result;
	},

	//=========================================================================

	'header': function() {
		if (this._header == null) {
			this._header = new Clipperz.PM.DataModel.Header({user:this});
		}
		return this._header;
	},

	//-------------------------------------------------------------------------
	
	'statistics': function() {
		if (this._statistics == null) {
			this._statistics = new Clipperz.PM.DataModel.Statistics({user:this});
		}
		return this._statistics;
	},

	//-------------------------------------------------------------------------

	'records': function() {
		return this._records;
	},

	//.........................................................................
	
	'addRecord': function(aValue, isBatchUpdate) {
		this.records()[aValue.reference()] = aValue;
		
		if (isBatchUpdate != true) {
			Clipperz.NotificationCenter.notify(aValue, 'recordAdded', null, true);
			Clipperz.NotificationCenter.notify(this, 'updatedSection', 'records', true);
		}
	},

	//-----------------------------------------------------------------------------

	'addNewRecord': function() {
		var	record;
		
//MochiKit.Logging.logDebug(">>> User.addNewRecord");
		record = new Clipperz.PM.DataModel.Record({user:this});
		this.addRecord(record);
		Clipperz.NotificationCenter.notify(this, 'updatedSection', 'records', true);
//MochiKit.Logging.logDebug("<<< User.addNewRecord");

		return record;
	},

	//-------------------------------------------------------------------------

	'saveRecords': function(someRecords /*, aMethodName*/) {
		var deferredResult;
//		var methodName;
		var	result;
		var i,c;

//console.log("User.saveRecords - someRecords", someRecords);
//		methodName = aMethodName || 'addNewRecords';
		
		Clipperz.NotificationCenter.notify(this, 'updatedSection', 'records', true);
//MochiKit.Logging.logDebug(">>> User.saveRecords");
//MochiKit.Logging.logDebug(">>> [" + (new Date()).valueOf() + "] User.saveRecords");
/*
MochiKit.Logging.logDebug("--- User.saveRecords - 1");
		MochiKit.Iter.forEach(someRecords, function(aRecord) {
			if (aRecord.headerNotes() != null) {
				aRecord.setNotes(aRecord.headerNotes());
			}
			aRecord.syncDirectLoginReferenceValues();
			aRecord.currentVersion().createNewVersion();
			aRecord.updateKey();
		});
MochiKit.Logging.logDebug("--- User.saveRecords - 2");
*/

		result = {
			'records': {
//				'deleted': [],
				'updated': []
			}
		};
		
		deferredResult = new MochiKit.Async.Deferred();
		c = someRecords.length;
		for (i=0; i<c; i++) {
			deferredResult.addCallback(function(aRecord) {
				if (aRecord.headerNotes() != null) {
					aRecord.setNotes(aRecord.headerNotes());
				}
				aRecord.syncDirectLoginReferenceValues();
				aRecord.currentVersion().createNewVersion();
				aRecord.updateKey();
			}, someRecords[i]);
			deferredResult.addCallback(MochiKit.Async.wait, 0.1);
		}
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 1 " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveCard_collectRecordInfo');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 2 " + res); return res;});

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveCard_encryptUserData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 3 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'encryptedData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 4 " + res); return res;});
		deferredResult.addCallback(function(aResult, res) {
			aResult['user'] = res;
			return aResult;
		}, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 5 " + res); return res;});

		c = someRecords.length;
		for (i=0; i<c; i++) {
			var recordData;
			
			recordData = {};
			
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 6.1 " + res); return res;});
			deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveCard_encryptRecordData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 6.2 " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(someRecords[i], 'encryptedData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 6.3 " + res); return res;});
			deferredResult.addCallback(function(aResult, res) {
				aResult['record'] = res;
				return aResult;
			}, recordData);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 6.4 " + res); return res;});

			deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', {} /*'saveCard_encryptRecordVersions'*/);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 6.5 " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(someRecords[i].currentVersion(), 'encryptedData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 6.6 " + res); return res;});
			deferredResult.addCallback(function(aResult, res) {
				aResult['currentRecordVersion'] = res;
				return aResult;
			}, recordData);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 6.7 " + res); return res;});

			deferredResult.addCallback(function(aResult, res) {
				aResult['records']['updated'].push(res);
				return aResult;
			}, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 6.8 " + res); return res;});
		}

//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 7 " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveCard_sendingData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 8 " + res); return res;});
//		deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'message'), methodName);
		deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'message'), 'saveChanges');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 9 " + res); return res;});

		for (i=0; i<c; i++) {
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 9.1 " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(someRecords[i], 'takeSnapshotOfCurrentData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 9.2 " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(someRecords[i], 'setIsBrandNew'), false);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 9.3 " + res); return res;});
		}
		
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 10 " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'notify', 'recordUpdated');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 11 " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'notify', 'directLoginUpdated');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.saveRecords - 12 " + res); return res;});
		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'removeRecord': function(aRecord) {
//MochiKit.Logging.logDebug(">>> User.removeRecord");
		delete this.records()[aRecord.reference()];
//MochiKit.Logging.logDebug("--- User.removeRecord - 1");
		Clipperz.NotificationCenter.notify(aRecord, 'recordRemoved', null, false);
		Clipperz.NotificationCenter.notify(this, 'updatedSection', 'records', true);
//MochiKit.Logging.logDebug("<<< User.removeRecord");
	},

	//-------------------------------------------------------------------------

	'deleteRecordsAction': function(someRecords) {
		var	deferredResult;
		var parameters;
		
//MochiKit.Logging.logDebug(">>> User.deleteRecordsAction - someRecords.length: " + someRecords.length);
		parameters = {};
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.deleteRecordsAction - 1 " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'deleteRecord_collectData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.deleteRecordsAction - 2 " + res); return res;});
		deferredResult.addCallback(function(someParameters, someRecords) {
			var recordReferences;

			recordReferences = MochiKit.Base.map(function(aRecord) {
				var result;
				
				result = aRecord.reference();
				aRecord.remove();

				return result;
			}, someRecords);
//			someParameters.recordReferences = recordReferences;
			someParameters['records'] = { 'deleted': recordReferences};
			
			return someParameters;
		}, parameters);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.deleteRecordsAction - 3 " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'deleteRecord_encryptData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.deleteRecordsAction - 4 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'encryptedData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.deleteRecordsAction - 5 " + res); return res;});
		deferredResult.addCallback(function(someParameters, anUserEncryptedData) {
			someParameters.user = anUserEncryptedData;
			return someParameters;
		}, parameters);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.deleteRecordsAction - 6 " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'deleteRecord_sendingData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.deleteRecords parameters: " + Clipperz.Base.serializeJSON(res)); return res;});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.deleteRecordsAction - 7 " + res); return res;});
//		deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'message'), 'deleteRecords');
		deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'message'), 'saveChanges');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.deleteRecordsAction - 8 " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'deleteRecord_updatingInterface');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.deleteRecordsAction - 9 " + res); return res;});
		deferredResult.callback(someRecords);
//MochiKit.Logging.logDebug("<<< User.deleteRecordsAction");
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'resetAllLocalData': function() {
		this.resetConnection();

		this.setUsername("");
		this.setPassphrase("");

		this._header = null;
		this._statistics = null;
		this._preferences = null;
		this._records = {};
		this._directLoginReferences = {};
	},
	
	//-------------------------------------------------------------------------

	'deleteAccountAction': function() {
		var deferredResult;
		
//MochiKit.Logging.logDebug(">>> user.deleteAccountAction - " + this);
		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'message'), 'deleteUser');
		deferredResult.addCallback(MochiKit.Base.method(this, 'resetAllLocalData'));
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< user.deleteAccountAction - " + this);

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'encryptedData': function() {
		var deferredResult;
		var	result;

		result = {};
		
		deferredResult = new MochiKit.Async.Deferred();

//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.encryptedData - 0: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this.header(), 'encryptedData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.encryptedData - 1: " + res); return res;});
		deferredResult.addCallback(function(aResult, aValue) {
			aResult['header'] = aValue;
		}, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.encryptedData - 2: " + res); return res;});

		deferredResult.addCallback(MochiKit.Base.method(this.statistics(), 'encryptedData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.encryptedData - 3: " + res); return res;});
		deferredResult.addCallback(function(aResult, aValue) {
			aResult['statistics'] = aValue;
		}, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.encryptedData - 4: " + res); return res;});

		deferredResult.addCallback(MochiKit.Base.bind(function(aResult, aValue) {
			aResult['version'] = Clipperz.PM.Crypto.encryptingFunctions.currentVersion;
			aResult['lock'] = this.lock();

			return aResult;
		}, this), result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.encryptedData - 5: " + res); return res;});
		deferredResult.callback();
		
		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	'preferences': function() {
		if (this._preferences == null) {
			this._preferences = new Clipperz.PM.DataModel.UserPreferences({user:this});
		}
		
		return this._preferences;
	},
/*
	'setPreferences': function(aValue) {
		this._preferences = aValue;

		if (this._preferences.preferredLanguage() != null) {
			Clipperz.PM.Strings.Languages.setSelectedLanguage(this._preferences.preferredLanguage());
		} else {
//MochiKit.Logging.logDebug("### keepping the browser selected language: " + Clipperz.PM.Strings.selectedLanguage);
		}
	},
*/
	//-------------------------------------------------------------------------

	'oneTimePasswordManager': function() {
		if (this._oneTimePasswordManager == null) {
			this._oneTimePasswordManager = new Clipperz.PM.DataModel.OneTimePasswordManager(this, null);
		}
		
		return this._oneTimePasswordManager;
	},

	//-------------------------------------------------------------------------

	'directLoginReferences': function() {
		return this._directLoginReferences;
	},
	
	'addDirectLoginReference': function(aDirectLoginReference, isBatchUpdate) {
//MochiKit.Logging.logDebug(">>> User.addDirectLoginReference");
		this.directLoginReferences()[aDirectLoginReference.reference()] = aDirectLoginReference;
		
		if (isBatchUpdate != true) {
			Clipperz.NotificationCenter.notify(aDirectLoginReference, 'directLoginAdded');
			Clipperz.NotificationCenter.notify(this, 'updatedSection', 'directLogins', true);
		}
	},

	'removeDirectLoginReference': function(aDirectLoginReference) {
		delete this.directLoginReferences()[aDirectLoginReference.reference()];
		Clipperz.NotificationCenter.notify(aDirectLoginReference, 'directLoginRemoved');
		Clipperz.NotificationCenter.notify(this, 'updatedSection', 'directLogins', true);
	},

	//.........................................................................
	
	'addDirectLogin': function(aDirectLogin) {
		var	newDirectLoginReference;
		
		newDirectLoginReference = new Clipperz.PM.DataModel.DirectLoginReference({user:this, directLogin:aDirectLogin})
		this.addDirectLoginReference(newDirectLoginReference);
	},

	'synchronizeDirectLogin': function(aDirectLogin) {
		var directLoginReference;
		
		directLoginReference = this.directLoginReferences()[aDirectLogin.reference()];
		if (typeof(directLoginReference) != 'undefined') {
			directLoginReference.synchronizeValues(aDirectLogin);
		} else {
			this.addDirectLogin(aDirectLogin);
		}
	},
	
	'removeDirectLogin': function(aDirectLogin) {
		this.removeDirectLoginReference(aDirectLogin);
	},
	
	//-------------------------------------------------------------------------
	
	'changeCredentials': function(aUsername, aPassphrase) {
		var	deferredResult;
		var result;
		
		result = {};
		
		deferredResult = new MochiKit.Async.Deferred();

		deferredResult.addCallback(MochiKit.Base.method(this.header(), 'loadAllSections'));
		deferredResult.addCallback(MochiKit.Base.method(this.header(), 'updateAllSections'));

		deferredResult.addCallback(MochiKit.Base.bind(function(aUsername, aPssphrase) {
			this.setUsername(aUsername);
			this.setPassphrase(aPassphrase);
		}, this), aUsername, aPassphrase)
		
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.changeCredentials - 1: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'changeCredentials_encryptingData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.changeCredentials - 2: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'encryptedData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.changeCredentials - 3: " + res); return res;});
		deferredResult.addCallback(function(aResult, anEncryptedData) {
			aResult['user'] = anEncryptedData;

			return aResult;
		}, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.changeCredentials - 4: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'changeCredentials_creatingNewCredentials');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.changeCredentials - 5: " + res); return res;});
		deferredResult.addCallback(function(aResult, anUser) {
			var	newConnection;
			
			newConnection = new Clipperz.PM.Crypto.communicationProtocol.versions[Clipperz.PM.Crypto.communicationProtocol.currentVersion]({user:anUser})
			aResult['credentials'] = newConnection.serverSideUserCredentials();

			return aResult;
		}, result, this);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.changeCredentials - 6: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this.oneTimePasswordManager(), 'encryptedData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.changeCredentials - 7: " + res); return res;});
		deferredResult.addCallback(function(aResult, anEncryptedData) {
			aResult['oneTimePasswords'] = anEncryptedData;

			return aResult;
		}, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.changeCredentials - 8: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'changeCredentials_sendingCredentials');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.changeCredentials - 9: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'message'), 'upgradeUserCredentials');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.changeCredentials - 10: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'changeCredentials_done');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.changeCredentials - 11: " + res); return res;});
		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'doLogout': function() {
		var deferredResult;
		
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.doLogout - 1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'logout'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.doLogout - 2: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'resetAllLocalData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.doLogout - 3: " + res); return res;});

		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'lock': function() {
		this.setPassphrase("")
		this.connection().logout();
		this.connection().resetSrpConnection();
	},

	'unlockWithPassphrase': function(aValue) {
		var	deferredResult;
//		var	connection;
		
//		connection = this.connection();
		
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.unlockWithPassphrase 1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'setPassphrase'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.unlockWithPassphrase 2: " + res); return res;});
//		deferredResult.addCallback(MochiKit.Base.method(connection, 'message'), 'echo', {'echo':"echo"});
		deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'reestablishConnection'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.unlockWithPassphrase 3: " + res); return res;});
//		deferredResult.addErrback(MochiKit.Base.method(this, 'setPassphrase', ""));
		deferredResult.addErrback(MochiKit.Base.bind(function(anError) {
			this.setPassphrase("");
			this.connection().resetSrpConnection();
			
			return anError;
		}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("User.unlockWithPassphrase 4: " + res); return res;});
		deferredResult.callback(aValue);
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------
	//-------------------------------------------------------------------------
	//-------------------------------------------------------------------------
	//-------------------------------------------------------------------------
	//-------------------------------------------------------------------------
	//-------------------------------------------------------------------------
	//-------------------------------------------------------------------------
	//-------------------------------------------------------------------------
	//-------------------------------------------------------------------------
	//-------------------------------------------------------------------------
	//-------------------------------------------------------------------------
	//-------------------------------------------------------------------------

	'serverData': function() {
		return this._serverData;
	},
	
	'setServerData': function(aValue) {
//MochiKit.Logging.logDebug(">>> User.setServerData");
		this._serverData = aValue;

		if (typeof(aValue.maxNumberOfRecords) != 'undefined') {
			this.setMaxNumberOfRecords(aValue.maxNumberOfRecords);
		}
//MochiKit.Logging.logDebug("<<< User.setServerData");
	},

	//-------------------------------------------------------------------------

	'isLoadingUserDetails': function() {
		return this._isLoadingUserDetails;
	},
	
	'setIsLoadingUserDetails': function(aValue) {
		this._isLoadingUserDetails = aValue;
	},
	
	//-------------------------------------------------------------------------

	'loadingUserDetailsPendingQueue': function() {
		return this._loadingUserDetailsPendingQueue;
	},
	
	'flushLoadingUserDetailsPendingQueue': function() {
		var queue;

//MochiKit.Logging.logDebug(">>> User.flushLoadingUserDetailsPendingQueue");
		queue = this.loadingUserDetailsPendingQueue();
		
		while(queue.length > 0) {
//MochiKit.Logging.logDebug("--- User.flushLoadingUserDetailsPendingQueue - pop");
			queue.pop().callback();
		}
//MochiKit.Logging.logDebug("<<< User.flushLoadingUserDetailsPendingQueue");
	},
	
	//-------------------------------------------------------------------------

	'getUserDetails': function() {
		var deferredResult;

//MochiKit.Logging.logDebug(">>> User.getUserDetails");
		deferredResult = new MochiKit.Async.Deferred();
		if ((this.serverData() == null) && (this.isLoadingUserDetails() == false)) {
			deferredResult.addCallback(MochiKit.Base.method(this, 'setIsLoadingUserDetails', true));
			deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'message'), 'getUserDetails');
			deferredResult.addCallback(MochiKit.Base.method(this, 'setServerData'));
			deferredResult.addCallback(MochiKit.Base.method(this, 'flushLoadingUserDetailsPendingQueue'));
			deferredResult.addCallback(MochiKit.Base.method(this, 'setIsLoadingUserDetails', false));
		}

		deferredResult.addCallback(MochiKit.Base.method(this, 'serverData'));

		if (this.isLoadingUserDetails() == false) {
			deferredResult.callback();
		} else {
			this.loadingUserDetailsPendingQueue().push(deferredResult);
		}
//MochiKit.Logging.logDebug("<<< User.getUserDetails");

		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	'loadRecords': function() {
		return this.header().loadRecords();
	},
	
	'loadDirectLogins': function() {
		return this.header().loadDirectLogins();
	},
	
	'loadPreferences': function() {
		return this.header().loadPreferences();
	},

	'loadOneTimePasswords': function() {
		return this.header().loadOneTimePasswords();
	},

	//-------------------------------------------------------------------------

	'loadLoginHistory': function() {
		var deferredResult;
		
		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'message'), 'getLoginHistory');
		deferredResult.addCallback(function(aResult) {
			return aResult['result'];
		});
		deferredResult.addCallback(MochiKit.Base.method(this, 'setLoginHistory'));
		deferredResult.addCallback(MochiKit.Base.method(this, 'loginHistory'));
		deferredResult.callback();
		
		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	'shouldDownloadOfflineCopy': function() {
		return this._shouldDownloadOfflineCopy;
	},

	'setShouldDownloadOfflineCopy': function(aValue) {
		this._shouldDownloadOfflineCopy = aValue;
	},

	//-------------------------------------------------------------------------

	'loginInfo': function() {
		return this._loginInfo;
	},
	
	'setLoginInfo': function(aValue) {
		this._loginInfo = aValue;
//MochiKit.Logging.logDebug("### LoginInfo: " + Clipperz.Base.serializeJSON(aValue));
	},

	//-------------------------------------------------------------------------

	'loginHistory': function() {
		return this._loginHistory;
	},
	
	'setLoginHistory': function(aValue) {
		this._loginHistory = aValue;
	},
/*
	'loginInfoWithOneTimePasswordReference': function(aOneTimePasswordReference) {
		var result;
		var i,c;

		result = null;
		c = this.loginHistory().length;
		for (i=0; (i<c) && (result == null); i++) {
			var currentLoginInfo;
			
			currentLoginInfo = this.loginHistory()[i];
			if (currentLoginInfo['oneTimePasswordReference'] == aOneTimePasswordReference) {
				result = currentLoginInfo;
			}
		}
		
		return result;
	},
*/	
	//-------------------------------------------------------------------------

	'lock': function() {
		return this._lock;
	},
	
	'setLock': function(aValue) {
//MochiKit.Logging.logDebug("=== User.setLock: " + aValue);
		this._lock = aValue;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});


Clipperz.PM.DataModel.User.exception = {
	LoginFailed: new MochiKit.Base.NamedError("Clipperz.PM.DataModel.User.exception.LoginFailed") 
};
	
