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

Clipperz.PM.DataModel.Record = function(args) {
	args = args || {};

	this._user = args['user'] || null;
	this._reference = args['reference'] || Clipperz.PM.Crypto.randomKey();
	this._version = args['version'] || Clipperz.PM.Crypto.encryptingFunctions.currentVersion;
	this._key = args['key'] || Clipperz.PM.Crypto.randomKey();

	this.setLabel(args['label'] || Clipperz.PM.Strings['newRecordTitleLabel']);
	
	this.setHeaderNotes(args['headerNotes'] || null);
	this.setNotes(args['notes'] || args['headerNotes'] || "");
//MochiKit.Logging.logDebug("--- new Record ('" + this._label + "')- _headerNotes: '" + this._headerNotes + "'");
//MochiKit.Logging.logDebug("--- new Record ('" + this._label + "')- _notes: '" + this._notes + "'");
//	this._notes = args.notes || "";
	
	this._versions = {};
	this._directLogins = {};
	this._removedDirectLogins = [];
	
	this.setIsBrandNew(args['reference'] == null);

	this.setShouldLoadData(this.isBrandNew() ? false: true);
	this.setShouldDecryptData(this.isBrandNew() ? false: true);
	this.setShouldProcessData(this.isBrandNew() ? false: true);

	this.setCurrentVersion(this.isBrandNew() ? new Clipperz.PM.DataModel.RecordVersion(this, null): null);
	this.setCurrentVersionKey(null);

	this._serverData = null;
	this._decryptedData = null;
	this._cachedData = null;
	
	return this;
}

Clipperz.PM.DataModel.Record.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "Record (" + this.label() + ")";
	},

	//-------------------------------------------------------------------------

	'isBrandNew': function() {
		return this._isBrandNew;
	},
	
	'setIsBrandNew': function(aValue) {
		this._isBrandNew = aValue;
	},

	//-------------------------------------------------------------------------
/*
	'shouldRunTheRecordCreationWizard': function() {
		return (this.isBrandNew() && (MochiKit.Base.keys(this.currentVersion().fields()).length == 0));
	},
*/	
	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},
	
	//-------------------------------------------------------------------------

	'reference': function() {
		return this._reference;
	},
	
	//-------------------------------------------------------------------------

	'key': function() {
		return this._key;
	},
	
	'updateKey': function() {
		this._key = Clipperz.PM.Crypto.randomKey();
	},
	
	//-------------------------------------------------------------------------
	
	'label': function() {
		return this._label;
	},

	'setLabel': function(aValue) {
		this._label = aValue;
	},
	
	'lowerCaseLabel': function() {
		return this.label().toLowerCase();
	},
	
	//-------------------------------------------------------------------------

	'versions': function() {
		return this._versions;
	},
	
	//-------------------------------------------------------------------------

	'currentVersion': function() {
		return this._currentVersion;
	},
	
	'setCurrentVersion': function(aValue) {
		this._currentVersion = aValue;
	},
	
	//-------------------------------------------------------------------------

	'currentVersionKey': function() {
		return this._currentVersionKey;
	},

	'setCurrentVersionKey': function(aValue) {
		this._currentVersionKey = aValue;
	},
	
	//-------------------------------------------------------------------------

	'deferredData': function() {
		var	deferredResult;

//MochiKit.Logging.logDebug(">>> [" + (new Date()).valueOf() + "] Record.deferredData - this: " + this);
		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(MochiKit.Base.method(this, 'loadData'));
		deferredResult.addCallback(MochiKit.Base.method(this, 'decryptData'));
		deferredResult.addCallback(MochiKit.Base.method(this, 'processData'));
		deferredResult.addCallback(function(aRecord) {
			return aRecord.currentVersion().deferredData();
		});
		deferredResult.addCallback(MochiKit.Base.method(this, 'takeSnapshotOfCurrentData'));
		deferredResult.addCallback(MochiKit.Async.succeed, this);
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< [" + (new Date()).valueOf() + "] Record.deferredData");
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'exportedData': function() {
		var result;
		
		result = {};
		result['label'] = this.label();
		result['data'] = this.serializedData();
		result['currentVersion'] = this.currentVersion().serializedData();
		result['currentVersion']['reference'] = this.currentVersion().reference();
//		result['versions'] = MochiKit.Base.map(MochiKit.Base.methodcaller("serializedData"), MochiKit.Base.values(this.versions()));
		
		return Clipperz.Base.serializeJSON(result);
	},
	
	//-------------------------------------------------------------------------

	'shouldLoadData': function() {
		return this._shouldLoadData;
	},

	'setShouldLoadData': function(aValue) {
		this._shouldLoadData = aValue;
	},

	//-------------------------------------------------------------------------

	'shouldDecryptData': function() {
		return this._shouldDecryptData;
	},

	'setShouldDecryptData': function(aValue) {
		this._shouldDecryptData = aValue;
	},

	//-------------------------------------------------------------------------

	'shouldProcessData': function() {
		return this._shouldProcessData;
	},
	
	'setShouldProcessData': function(aValue) {
		this._shouldProcessData = aValue;
	},

	//-------------------------------------------------------------------------
	
	'loadData': function() {
		var result;
		
//MochiKit.Logging.logDebug(">>> [" + (new Date()).valueOf() + "] Record.loadData - this: " + this);
		if (this.shouldLoadData()) {
			var deferredResult;
		
			deferredResult = new MochiKit.Async.Deferred();
			deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'notify', 'loadingRecordData');
			deferredResult.addCallback(MochiKit.Base.method(this.user().connection(), 'message'), 'getRecordDetail', {reference: this.reference()});
			deferredResult.addCallback(MochiKit.Base.method(this,'setServerData'));
			deferredResult.callback();
			result = deferredResult;
		} else {
			result = MochiKit.Async.succeed(this.serverData());
		}
//MochiKit.Logging.logDebug("<<< [" + (new Date()).valueOf() + "] Record.loadData");

		return result;
	},

	//-------------------------------------------------------------------------
	
	'decryptData': function(anEncryptedData) {
		var result;
		
//MochiKit.Logging.logDebug(">>> [" + (new Date()).valueOf() + "] Record.decryptData - this: " + this + " (" + anEncryptedData + ")");
		if (this.shouldDecryptData()) {
			var deferredResult;
		
			deferredResult = new MochiKit.Async.Deferred();
			deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'notify', 'decryptingRecordData');
			deferredResult.addCallback(Clipperz.PM.Crypto.deferredDecrypt, this.key(), anEncryptedData['data'], anEncryptedData['version']);
			deferredResult.addCallback(function(anEncryptedData, someDecryptedValues) {
				var	result;

				result = anEncryptedData;
				result['data'] = someDecryptedValues;
			
				return result;
			}, anEncryptedData);
			deferredResult.addCallback(MochiKit.Base.method(this, 'setDecryptedData'));
			deferredResult.callback();

			result = deferredResult;
		} else {
			result = MochiKit.Async.succeed(this.decryptedData());
		}
//MochiKit.Logging.logDebug("<<< [" + (new Date()).valueOf() + "] Record.decryptData");

		return result;
	},

	//-------------------------------------------------------------------------

	'processData': function(someValues) {
//MochiKit.Logging.logDebug(">>> [" + (new Date()).valueOf() + "] Record.processData");
//MochiKit.Logging.logDebug("--- Record.processData: " + Clipperz.Base.serializeJSON(someValues));
		if (this.shouldProcessData()) {
			var currentVersionParameters;

			this.processDataToExtractLegacyValues(someValues['data']);

			if (typeof(someValues['data']['notes']) != 'undefined') {
				this.setNotes(someValues['data']['notes']);
			}

			if (someValues['data']['currentVersionKey'] != null) {
				this.setCurrentVersionKey(someValues['data']['currentVersionKey']);
			} else {
				this.setCurrentVersionKey(this.key());
			}

//			community edition doesn't currently pass version information
			if (someValues['versions'] == null) {
				currentVersionParameters = someValues['currentVersion'];
			} else {
				currentVersionParameters = someValues['versions'][someValues['currentVersion']];
			}

//-			currentVersionParameters = someValues['currentVersion'];
//			currentVersionParameters = someValues['versions'][someValues['currentVersion']];

			currentVersionParameters['key'] = this.currentVersionKey();
			this.setCurrentVersion(new Clipperz.PM.DataModel.RecordVersion(this, currentVersionParameters));
			
			if (someValues['data']['directLogins'] != null) {
				var	directLoginReference;
			
				for (directLoginReference in someValues['data']['directLogins']) {
					var directLogin;
					var directLoginParameters;
				
					directLoginParameters = someValues['data']['directLogins'][directLoginReference];
					directLoginParameters.record = this;
					directLoginParameters.reference = directLoginReference;
				
					directLogin = new Clipperz.PM.DataModel.DirectLogin(directLoginParameters);
					this.addDirectLogin(directLogin, true);
				}
			}
			this.setShouldProcessData(false);
		}
		
		Clipperz.NotificationCenter.notify(this, 'recordDataReady');
//MochiKit.Logging.logDebug("<<< [" + (new Date()).valueOf() + "] Record.processData");
//MochiKit.Logging.logDebug("<<< Record.processData");
		
		return this;
	},

	//-------------------------------------------------------------------------

	'processDataToExtractLegacyValues': function(someValues) {
//MochiKit.Logging.logDebug(">>> Record.processDataToExtractLegacyValues");
		if (someValues['data'] != null) {
			this.setNotes(someValues['data']);
		}

		if (
				(typeof(someValues['loginFormData']) != "undefined")
			&&	(typeof(someValues['loginBindings'] != "undefined"))
			&&	(someValues['loginFormData'] != "")
			&&	(someValues['loginBindings'] != "")
		) {
			var	directLogin;

			directLogin = new Clipperz.PM.DataModel.DirectLogin({
				record:this,
				label:this.label() + Clipperz.PM.Strings['newDirectLoginLabelSuffix'],
				reference:Clipperz.Crypto.SHA.sha256(new Clipperz.ByteArray(this.label() +
																			someValues['loginFormData'] +
																			someValues['loginBindings'])).toHexString().substring(2),
				formData:Clipperz.Base.evalJSON(someValues['loginFormData']),
				legacyBindingData:Clipperz.Base.evalJSON(someValues['loginBindings']),
				bookmarkletVersion:'0.1'
			});
			this.addDirectLogin(directLogin, true);
		}
//MochiKit.Logging.logDebug("<<< Record.processDataToExtractLegacyValues");
	},
	
	//-------------------------------------------------------------------------

	'getReadyBeforeUpdatingVersionValues': function() {
	},

	//-------------------------------------------------------------------------
	
	'addNewField': function() {
		var	newField;

//MochiKit.Logging.logDebug(">>> Record.addNewField - " + this);
		this.getReadyBeforeUpdatingVersionValues();
		newField = this.currentVersion().addNewField();
		Clipperz.NotificationCenter.notify(this, 'recordUpdated');
//MochiKit.Logging.logDebug("<<< Record.addNewField");
		
		return newField;
	},

	//-------------------------------------------------------------------------

	'removeField': function(aField) {
		this.getReadyBeforeUpdatingVersionValues();
		this.currentVersion().removeField(aField);
		Clipperz.NotificationCenter.notify(this, 'recordUpdated');
	},

	'removeEmptyFields': function() {
		MochiKit.Iter.forEach(MochiKit.Base.values(this.currentVersion().fields()), MochiKit.Base.bind(function(aField) {
			if (aField.isEmpty()) {
				this.removeField(aField);
//				this.currentVersion().removeField(aField);
			}
		}, this));
	},
	
	//-------------------------------------------------------------------------

	'notes': function() {
		return this._notes;
	},
	
	'setNotes': function(aValue) {
		this._notes = aValue;
		this.setHeaderNotes(null);
	},

	//-------------------------------------------------------------------------

	'headerNotes': function() {
		return this._headerNotes;
	},
	
	'setHeaderNotes': function(aValue) {
		this._headerNotes = aValue;
	},
	
	//-------------------------------------------------------------------------

	'remove': function() {
//MochiKit.Logging.logDebug(">>> Record.remove - " + this);
		MochiKit.Iter.forEach(MochiKit.Base.values(this.directLogins()), MochiKit.Base.method(this, 'removeDirectLogin'));

		this.syncDirectLoginReferenceValues();
		this.user().removeRecord(this);
//MochiKit.Logging.logDebug("<<< Record.remove");
	},
	
	//-------------------------------------------------------------------------

	'directLogins': function() {
		return this._directLogins;
	},
	
	'addDirectLogin': function(aDirectLogin, shouldUpdateUser) {
		this.directLogins()[aDirectLogin.reference()] = aDirectLogin;
		if (shouldUpdateUser == true) {
			this.user().addDirectLogin(aDirectLogin);
		}
	},

	'removeDirectLogin': function(aDirectLogin) {
		this.removedDirectLogins().push(aDirectLogin);
		delete this.directLogins()[aDirectLogin.reference()];
//		this.user().removeDirectLogin(aDirectLogin);
	},

	'resetDirectLogins': function() {
		this._directLogins = {};
	},

	'removedDirectLogins': function() {
		return this._removedDirectLogins;
	},

	'resetRemovedDirectLogins': function() {
		this._removedDirectLogins = [];
	},
	
	//-------------------------------------------------------------------------

	'serverData': function() {
		return this._serverData;
	},
	
	'setServerData': function(aValue) {
		this._serverData = aValue;
		this.setShouldLoadData(false);
		return aValue;
	},

	//-------------------------------------------------------------------------

	'decryptedData': function() {
		return this._decryptedData;
	},
	
	'setDecryptedData': function(aValue) {
		this._decryptedData = aValue;
		this.setShouldDecryptData(false);
		return aValue;
	},

	//-------------------------------------------------------------------------

	'cachedData': function() {
		return this._cachedData;
	},
	
	'setCachedData': function(aValue) {
//MochiKit.Logging.logDebug(">>> Record.setCachedData");
//MochiKit.Logging.logDebug("--- Record.setCachedData - aValue: " + Clipperz.Base.serializeJSON(aValue));
		this._cachedData = aValue;
		this.setShouldProcessData(false);
//MochiKit.Logging.logDebug("<<< Record.setCachedData");

		return aValue;
	},

	//-------------------------------------------------------------------------

	'hasPendingChanges': function() {
		var result;

//MochiKit.Logging.logDebug(">>> [" + (new Date()).valueOf() + "] Record.hasPendingChanges");
//MochiKit.Logging.logDebug(">>> Record.hasPendingChanges - cachedData: " + this.cachedData());
//MochiKit.Logging.logDebug(">>> Record.hasPendingChanges - cachedData: " + Clipperz.Base.serializeJSON(this.cachedData()));
//MochiKit.Logging.logDebug(">>> Record.hasPendingChanges - currentSnapshot: " + this.currentDataSnapshot());
//MochiKit.Logging.logDebug(">>> Record.hasPendingChanges - currentSnapshot: " + Clipperz.Base.serializeJSON(this.currentDataSnapshot()));
//console.log(">>> Record.hasPendingChanges - cachedData: %o", this.cachedData());
//console.log(">>> Record.hasPendingChanges - currentSnapshot: %o", this.currentDataSnapshot());
		result = (MochiKit.Base.compare(this.cachedData(), this.currentDataSnapshot()) != 0);
//MochiKit.Logging.logDebug("<<< Record.hasPendingChanges - " + result);

		if ((result == false) && this.isBrandNew() && (this.label() != Clipperz.PM.Strings['newRecordTitleLabel'])) {
			result = true;
		}
//MochiKit.Logging.logDebug("<<< [" + (new Date()).valueOf() + "] Record.hasPendingChanges");
		
		return result;
	},

	//-------------------------------------------------------------------------

	'currentDataSnapshot': function() {
		var	result;
		
//MochiKit.Logging.logDebug(">>> [" + (new Date()).valueOf() + "] Record.currentDataSnapshot");
		result = {
			'label': this.label(),
			'data': this.serializedData(),
			'currentVersion': this.currentVersion().currentDataSnapshot()
		};
		
//		result['data']['data'] = this.notes();
		result = Clipperz.Base.serializeJSON(result);
		
//MochiKit.Logging.logDebug("<<< [" + (new Date()).valueOf() + "] Record.currentDataSnapshot");
//MochiKit.Logging.logDebug("<<< Record.currentDataSnapshot");

		return result;
	},

	//.........................................................................

	'takeSnapshotOfCurrentData': function() {
		this.setCachedData(this.currentDataSnapshot());
	},

	//-------------------------------------------------------------------------

	'headerData': function() {
		var result;
		
		result = {
			'label':		this.label(),
			'key':			this.key()
		};

		if (this.headerNotes() != null) {
			result['headerNotes'] = this.headerNotes();
		}
		
		return result;
	},

	//-------------------------------------------------------------------------
	
	'serializedData': function() {
		var result;
		var directLoginReference;

		result = {};
		result['currentVersionKey'] = this.currentVersion().key();
		
		result['directLogins'] = {};
		for (directLoginReference in this.directLogins()) {
			result['directLogins'][directLoginReference] = this.directLogins()[directLoginReference].serializedData();
		}
		result['notes'] = this.notes();
		
		return result;
	},

	//-------------------------------------------------------------------------

	'encryptedData': function() {
		var deferredResult;
		var	result;
		
//MochiKit.Logging.logDebug(">>> [" + (new Date()).valueOf() + "] Record.encryptedData");
		result = {}
//MochiKit.Logging.logDebug("--- Record.encryptedData - 1");
		deferredResult = new MochiKit.Async.Deferred();
//MochiKit.Logging.logDebug("--- Record.encryptedData - 2");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Record.encryptedData - 1: " + res); return res;});
		deferredResult.addCallback(function(aResult, aRecord) {
			aResult['reference'] = aRecord.reference();
			return aResult;
		}, result, this);
//MochiKit.Logging.logDebug("--- Record.encryptedData - 3");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Record.encryptedData - 2: " + res); return res;});
		deferredResult.addCallback(Clipperz.PM.Crypto.deferredEncryptWithCurrentVersion, this.key(), this.serializedData());
//MochiKit.Logging.logDebug("--- Record.encryptedData - 4");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Record.encryptedData - 3: " + res); return res;});
		deferredResult.addCallback(function(aResult, res) {
			aResult['data'] = res;
			return aResult;
		}, result);
//MochiKit.Logging.logDebug("--- Record.encryptedData - 5");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Record.encryptedData - 4: " + res); return res;});
		deferredResult.addCallback(function(aResult) {
			aResult['version'] = Clipperz.PM.Crypto.encryptingFunctions.currentVersion;
			return aResult;
		}, result);
//MochiKit.Logging.logDebug("--- Record.encryptedData - 6");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Record.encryptedData - 5: " + res); return res;});
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< [" + (new Date()).valueOf() + "] Record.encryptedData");
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'syncDirectLoginReferenceValues': function() {
//MochiKit.Logging.logDebug(">>> Record.syncDirectLoginReferenceValues");
		MochiKit.Iter.forEach(MochiKit.Base.values(this.directLogins()), function(aDirectLogin) {
			aDirectLogin.record().user().synchronizeDirectLogin(aDirectLogin);
		});

		MochiKit.Iter.forEach(this.removedDirectLogins(), function(aDirectLogin) {
			aDirectLogin.record().user().removeDirectLogin(aDirectLogin);
		});
		
		this.resetRemovedDirectLogins();
//MochiKit.Logging.logDebug("<<< Record.syncDirectLoginReferenceValues");
	},
	
	//-------------------------------------------------------------------------

	'saveChanges': function() {
		var result;
		
//		if (this.isBrandNew() == false) {
//			result = this.user().saveRecords([this], 'updateData');
//		} else {
//			result = this.user().saveRecords([this], 'addNewRecords');
//		}

		result = this.user().saveRecords([this]);
		
		return result;
	},
	
/*
	'saveChanges': function() {
		var deferredResult;
		var	result;

		Clipperz.NotificationCenter.notify(this.user(), 'updatedSection', 'records', true);
//MochiKit.Logging.logDebug(">>> Record.saveChanges");
//MochiKit.Logging.logDebug(">>> [" + (new Date()).valueOf() + "] Record.saveChanges");
		if (this.headerNotes() != null) {
			this.setNotes(this.headerNotes());
		}
		this.syncDirectLoginReferenceValues();
		this.currentVersion().createNewVersion();

		result = {'records': [{}]};
		
		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveCard_collectRecordInfo');
		deferredResult.addCallback(MochiKit.Base.method(this, 'updateKey'));

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveCard_encryptUserData');
		deferredResult.addCallback(MochiKit.Base.method(this.user(), 'encryptedData'));
		deferredResult.addCallback(function(aResult, res) {
			aResult['user'] = res;
			return aResult;
		}, result);

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveCard_encryptRecordData');
		deferredResult.addCallback(MochiKit.Base.method(this, 'encryptedData'));
		deferredResult.addCallback(function(aResult, res) {
//#			aResult['record'] = res;
			aResult['records'][0]['record'] = res;
			return aResult;
		}, result);

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveCard_encryptRecordVersions');
		deferredResult.addCallback(MochiKit.Base.method(this.currentVersion(), 'encryptedData'));
		deferredResult.addCallback(function(aResult, res) {
//			aResult['currentRecordVersion'] = res;
			aResult['records'][0]['currentRecordVersion'] = res;
			return aResult;
		}, result);

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveCard_sendingData');
		if (this.isBrandNew() == false) {
			deferredResult.addCallback(MochiKit.Base.method(this.user().connection(), 'message'), 'updateData');
		} else {
//#			deferredResult.addCallback(MochiKit.Base.method(this.user().connection(), 'message'), 'addNewRecord');
			deferredResult.addCallback(MochiKit.Base.method(this.user().connection(), 'message'), 'addNewRecords');
		}

		deferredResult.addCallback(MochiKit.Base.method(this, 'takeSnapshotOfCurrentData'));
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'saveCard_updatingInterface');
		deferredResult.addCallback(MochiKit.Base.method(this, 'setIsBrandNew'), false);
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'notify', 'recordUpdated');
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'notify', 'directLoginUpdated');
		deferredResult.callback();
		
		return deferredResult;
	},
*/	
	//-------------------------------------------------------------------------

	'cancelChanges': function() {
//MochiKit.Logging.logDebug(">>> Record.cancelChanges");
//MochiKit.Logging.logDebug("--- Record.cancelChanges - cachedData: " + Clipperz.Base.serializeJSON(this.cachedData()));
		if (this.isBrandNew()) {
			this.user().removeRecord(this);
		} else {
			this.restoreValuesFromSnapshot(this.cachedData());
		}
//MochiKit.Logging.logDebug("<<< Record.cancelChanges");
	},
	
	//-------------------------------------------------------------------------

	'restoreValuesFromSnapshot': function(someSnapshotData) {
		var	snapshotData;
		
//MochiKit.Logging.logDebug(">>> [" + (new Date()).valueOf() + "] Record.restoreValuesFromSnapshot");
		snapshotData = Clipperz.Base.evalJSON(someSnapshotData);
//MochiKit.Logging.logDebug("--- Record.restoreValuesFromSnapshot - someSnapshotData (1): " + Clipperz.Base.serializeJSON(someSnapshotData));
		this.setLabel(snapshotData['label']);
		this.resetDirectLogins();
		this.setShouldProcessData(true);
		this.processData(snapshotData);
//MochiKit.Logging.logDebug("--- Record.restoreValuesFromSnapshot - snapshotData: (2)" + Clipperz.Base.serializeJSON(snapshotData));
		
		this.resetRemovedDirectLogins();
		
		{
			var currentSnapshot;
			var	comparisonResult;
			
			currentSnapshot = this.currentDataSnapshot();
//MochiKit.Logging.logDebug("--- Record.restoreValuesFromSnapshot - 1");
//console.log("snapshot data: %o", someSnapshotData.currentVersion);
//console.log("current data: %o", currentSnapshot.currentVersion);
//MochiKit.Logging.logDebug("--- Record.restoreValuesFromSnapshot - someSnapshotData: " + Clipperz.Base.serializeJSON(someSnapshotData.currentVersion));
//MochiKit.Logging.logDebug("--- Record.restoreValuesFromSnapshot - currentSnapshot: " + Clipperz.Base.serializeJSON(currentSnapshot.currentVersion));
			comparisonResult = MochiKit.Base.compare(someSnapshotData.currentVersion, currentSnapshot.currentVersion);
//MochiKit.Logging.logDebug("--- Record.restoreValuesFromSnapshot - " + comparisonResult);
		}
//MochiKit.Logging.logDebug("<<< [" + (new Date()).valueOf() + "] Record.restoreValuesFromSnapshot");
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});


