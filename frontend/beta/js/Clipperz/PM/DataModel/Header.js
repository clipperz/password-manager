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

Clipperz.PM.DataModel.Header = function(args) {
	args = args || {};

	this._user = args.user;

	this._serverData = null;
	this._serverDataVersion = null;
	this._jsonEvaledServerData = null;
	
	this._decryptedLegacyServerData = null;
	this._isDecryptingLegacyServerData = false;
	this._decryptingLegacyServerDataPendingQueue = [];

	this.resetUpdatedSections();
	
	this._shouldLoadSections = {};

	Clipperz.NotificationCenter.register(this.user(), 'updatedSection', this, 'updatedSectionHandler');
	
	return this;
}

Clipperz.PM.DataModel.Header.prototype = MochiKit.Base.update(null, {

	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
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
	
	'updatedSections': function() {
		return this._updatedSections;
	},

	'markSectionAsUpdated': function(aSectionName) {
		this.updatedSections().push(aSectionName);
	},
	
	'resetUpdatedSections': function() {
		this._updatedSections = []
	},
	
	'hasSectionBeenUpdated': function(aSectionName) {
		return	(this.updatedSections().join().indexOf(aSectionName) != -1)
			||	(this.serverDataVersion() != Clipperz.PM.Crypto.encryptingFunctions.currentVersion);
	},
	
	'cachedServerDataSection': function(aSectionName) {
		return (this.hasSectionBeenUpdated(aSectionName)) ? {} : this.jsonEvaledServerData()[aSectionName];
	},
	
	'updateAllSections': function() {
		this.resetUpdatedSections();
		this.markSectionAsUpdated('records');
		this.markSectionAsUpdated('directLogins');
		this.markSectionAsUpdated('preferences');
		this.markSectionAsUpdated('oneTimePasswords');
		
		return MochiKit.Async.succeed(this);
	},
	
	'updatedSectionHandler': function(anEvent) {
		this.markSectionAsUpdated(anEvent.parameters());
	},
	
	//-------------------------------------------------------------------------
	
	'getObjectKeyIndex': function(anObject) {
		var result;
		var	itemReference;
		var index;
		
		result = {};
		index = 0;

		for (itemReference in anObject) {
			result[itemReference] = index.toString();
			index ++;
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'serializedDataWithRecordAndDirectLoginIndexes': function(aRecordIndexes, aDirectLoginIndexs) {
		var result;
		var records;
		var recordReference;

//MochiKit.Logging.logDebug(">>> Header.serializedData");
		result = {
			'records': {},
			'directLogins': {}
		};

		records = this.user().records();
		for (recordReference in records) {
			result['records'][aRecordIndexes[recordReference]] = this.user().records()[recordReference].headerData();
		}
		
		for (directLoginReference in this.user().directLoginReferences()) {
			var currentDirectLogin;
			var	directLoginData;
			
			currentDirectLogin = this.user().directLoginReferences()[directLoginReference];
			if (aRecordIndexes[currentDirectLogin.recordReference()] != null) {
				directLoginData = {
//					reference:  currentDirectLogin.reference(),
					record:		aRecordIndexes[currentDirectLogin.recordReference()].toString(),
					label:		currentDirectLogin.label(),
					favicon:	currentDirectLogin.favicon() || ""
				}

				result['directLogins'][aDirectLoginIndexs[directLoginReference]] = directLoginData;
			}
			
		}
//MochiKit.Logging.logDebug("<<< Header.serializedData - result: " + Clipperz.Base.serializeJSON(result));
//MochiKit.Logging.logDebug("<<< Header.serializedData");

		return result;
	},

	//-------------------------------------------------------------------------
	
	'encryptedData': function() {
		var deferredResult;
		var recordIndex;
		var directLoginIndex;
		var	serializedData;
		var result;

//MochiKit.Logging.logDebug(">>> [" + (new Date()).valueOf() + "] Header.encryptedData");
//MochiKit.Logging.logDebug("### Header.encryptedData - " + Clipperz.Base.serializeJSON(this.updatedSections()));
		result = {
			'records': this.cachedServerDataSection('records'),
			'directLogins': this.cachedServerDataSection('directLogins'),
			'preferences': this.cachedServerDataSection('preferences'),
			'oneTimePasswords': this.cachedServerDataSection('oneTimePasswords'),
			'version': '0.1'
		};

		if (this.hasSectionBeenUpdated('records')) {
			recordIndex = this.getObjectKeyIndex(this.user().records());
			result['records']['index'] = recordIndex;
		} else {
			recordIndex = result['records']['index'];
		}
		
		if (this.hasSectionBeenUpdated('directLogins')) {
			directLoginIndex = this.getObjectKeyIndex(this.user().directLoginReferences());
			result['directLogins']['index'] = directLoginIndex;
		} else {
			directLoginIndex = result['directLogins']['index'];
		}
		
		if (this.hasSectionBeenUpdated('records') || this.hasSectionBeenUpdated('directLogins')) {
			serializedData = this.serializedDataWithRecordAndDirectLoginIndexes(recordIndex, directLoginIndex);
		}
		
		deferredResult = new MochiKit.Async.Deferred();

//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 1: " + res); return res;});
		if (this.hasSectionBeenUpdated('records')) {
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 1.1: " + res); return res;});
			deferredResult.addCallback(function(anHeader, aResult, aSerializedData, aValue) {
				return Clipperz.PM.Crypto.deferredEncryptWithCurrentVersion(anHeader.user().passphrase(), aSerializedData['records']);
			}, this, result, serializedData);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 1.2: " + res); return res;});
			deferredResult.addCallback(function(anHeader, aResult, aValue) {
				aResult['records']['data'] = aValue;
			}, this, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 1.3: " + res); return res;});
		}
		
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 2: " + res); return res;});
		if (this.hasSectionBeenUpdated('directLogins')) {
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 2.1: " + res); return res;});
			deferredResult.addCallback(function(anHeader, aResult, aSerializedData, aValue) {
				return Clipperz.PM.Crypto.deferredEncryptWithCurrentVersion(anHeader.user().passphrase(), aSerializedData['directLogins']);
			}, this, result, serializedData);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 2.2: " + res); return res;});
			deferredResult.addCallback(function(anHeader, aResult, aValue) {
				aResult['directLogins']['data'] = aValue;
			}, this, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 2.3: " + res); return res;});
		}
		
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 3: " + res); return res;});
		if (this.hasSectionBeenUpdated('preferences')) {
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 3.1: " + res); return res;});
			deferredResult.addCallback(function(anHeader, aResult, aValue) {
				return Clipperz.PM.Crypto.deferredEncryptWithCurrentVersion(anHeader.user().passphrase(), anHeader.user().preferences().serializedData());
			}, this, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 3.2: " + res); return res;});
			deferredResult.addCallback(function(anHeader, aResult, aValue) {
				aResult['preferences']['data'] = aValue;
			}, this, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 3.3: " + res); return res;});
		}

//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 4: " + res); return res;});
		if (this.hasSectionBeenUpdated('oneTimePasswords')) {
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 4.1: " + res); return res;});
//			deferredResult.addCallback(MochiKit.Base.method(this, 'loadOneTimePasswords'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 4.2: " + res); return res;});
			deferredResult.addCallback(function(anHeader, aResult, aValue) {
				return Clipperz.PM.Crypto.deferredEncryptWithCurrentVersion(anHeader.user().passphrase(), anHeader.user().oneTimePasswordManager().serializedData());
			}, this, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 4.3: " + res); return res;});
			deferredResult.addCallback(function(anHeader, aResult, aValue) {
				aResult['oneTimePasswords']['data'] = aValue;
			}, this, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 4.4: " + res); return res;});
		}
		
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 5: " + res); return res;});
		deferredResult.addCallback(function(anHeader, aResult, aValue) {
			var serverData;
			
			serverData = Clipperz.Base.serializeJSON(aResult);
			anHeader.setServerData(serverData);
			
			return serverData;
		}, this, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.encryptedData - 6: " + res); return res;});

		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< [" + (new Date()).valueOf() + "] Header.encryptedData");

		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	'serverData': function() {
		return this._serverData;
	},
	
	'setServerData': function(aValue) {
//MochiKit.Logging.logDebug(">>> Header.setServerData");
//MochiKit.Logging.logDebug("[start]=============================================");
//MochiKit.Logging.logDebug("SERVER_DATA: " + aValue);
//MochiKit.Logging.logDebug("[end]===============================================");
		this._serverData = aValue;
//MochiKit.Logging.logDebug("--- Header.setServerData - 1");
		this.resetUpdatedSections();
//MochiKit.Logging.logDebug("--- Header.setServerData - 2");
		this.resetJsonEvaledServerData();
//MochiKit.Logging.logDebug("<<< Header.setServerData");
	},

	'jsonEvaledServerData': function() {
		if (this._jsonEvaledServerData == null) {
			this._jsonEvaledServerData = Clipperz.Base.evalJSON(this.serverData());
		}
		
		return this._jsonEvaledServerData;
	},
	
	'resetJsonEvaledServerData': function() {
		this._jsonEvaledServerData = null;
	},
	
	//-------------------------------------------------------------------------

	'serverDataVersion': function() {
		return this._serverDataVersion;
	},
	
	'setServerDataVersion': function(aValue) {
		this._serverDataVersion = aValue;
	},

	//-------------------------------------------------------------------------

	'decryptedLegacyServerData': function() {
		var deferredResult;

//MochiKit.Logging.logDebug(">>> Header.decryptedLegacyServerData");
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.decryptedLegacyServerData 1: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'updateAllSections'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.decryptedLegacyServerData 2: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		if (this._decryptedLegacyServerData == null) {
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.decryptedLegacyServerData 3: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'connection_decryptingUserData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.decryptedLegacyServerData 4: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(Clipperz.PM.Crypto.deferredDecrypt, this.user().passphrase(), this.serverData(), this.serverDataVersion());
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.decryptedLegacyServerData 5: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(function(anHeader, aValue) {
				anHeader._decryptedLegacyServerData = aValue;
			}, this);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.decryptedLegacyServerData 6: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		};
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.decryptedLegacyServerData 7: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		
		deferredResult.addCallback(function(anHeader) {
			return anHeader._decryptedLegacyServerData;
		}, this);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.decryptedLegacyServerData 8: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< Header.decryptedLegacyServerData");
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'serverDataFormat': function() {
		var result;
		
//MochiKit.Logging.logDebug(">>> Header.serverDataFormat");
		if (this.serverData().charAt(0) == '{') {
			var	serverData;
			
			serverData = Clipperz.Base.evalJSON(this.serverData());
			result = serverData['version'];
		} else {
			result = 'LEGACY';
		}
//MochiKit.Logging.logDebug("<<< Header.serverDataFormat");
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'extractHeaderDataFromUserDetails': function(someUserDetails) {
		if (this.serverData() == null) {
			this.setServerData(someUserDetails['header']);
			this.setServerDataVersion(someUserDetails['version'])
		}
	},

	//-------------------------------------------------------------------------

	'extractDataWithKey': function(aKey) {
		var deferredResult;
		
//MochiKit.Logging.logDebug(">>> Header.extractDataWithKey");
		deferredResult = new MochiKit.Async.Deferred();
		
		switch (this.serverDataFormat()) {
			case 'LEGACY':
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.extractDataWithKey 1: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
				deferredResult.addCallback(MochiKit.Base.method(this, 'decryptedLegacyServerData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.extractDataWithKey 2: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
				deferredResult.addCallback(function(someDecryptedValues) {
					return someDecryptedValues[aKey] || {};
				})
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.extractDataWithKey 3: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
				break;
			case '0.1':
				var data;

//#				data = Clipperz.Base.evalJSON(this.serverData());
				data = this.jsonEvaledServerData();
				if (typeof(data[aKey]) != 'undefined') {
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.extractDataWithKey 4: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
					deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'connection_decryptingUserData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.extractDataWithKey 5: "/* + res*/); return res;});
//deferredResult.addBoth(function(res) {console.log("aKey: " + aKey); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
					deferredResult.addCallback(Clipperz.PM.Crypto.deferredDecrypt, this.user().passphrase(), data[aKey]['data'], this.serverDataVersion());
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.extractDataWithKey 6: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
					deferredResult.addCallback(function(/*anHeader,*/ aKey, aData, aRecordIndex, aValue) {
						var result;
//MochiKit.Logging.logDebug(">>> [start] ===============================================");
//MochiKit.Logging.logDebug("--- extractDataWithKey - 0 [" + aKey + "]: " + Clipperz.Base.serializeJSON(aValue));
//MochiKit.Logging.logDebug("<<< [end] =================================================");
						if (aKey == 'records') {
							var recordKey;
				
							result = {};
							for (recordKey in aData['index']) {
								result[recordKey] = aValue[aData['index'][recordKey]];
							}
						} else if (aKey == 'directLogins') {
							var	recordKeyReversedIndex;
							var recordKey;
							var directLoginKey;
				
							result = {};
							recordKeyReversedIndex = {};
	
							for (recordKey in aRecordIndex) {
								recordKeyReversedIndex[aRecordIndex[recordKey]] = recordKey;
							}

//MochiKit.Logging.logDebug("--- extractDataWithKey - 1 - aData['index']: " + Clipperz.Base.serializeJSON(aData['index']));
							for (directLoginKey in aData['index']) {
try {
								if ((aData['index'][directLoginKey] != null) && (aValue[aData['index'][directLoginKey]] != null)) {
									result[directLoginKey] = aValue[aData['index'][directLoginKey]];
									result[directLoginKey]['record'] = recordKeyReversedIndex[result[directLoginKey]['record']];
								}
} catch(exception) {
	//	result[directLoginKey] has no properties
	MochiKit.Logging.logDebug("[Header 391] EXCEPTION: " + exception);
	throw exception;
}
							}
//MochiKit.Logging.logDebug("--- extractDataWithKey - 2");
						} else {
							result = aValue;
						}
			
						return result;
					}, /*this,*/ aKey, data[aKey], data['records']['index']);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.extractDataWithKey 6: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
				} else {
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.extractDataWithKey 7: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
					deferredResult.addCallback(MochiKit.Async.succeed, {});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.extractDataWithKey 8: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
				}
 				break;
		}
		
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.extractDataWithKey 9: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< Header.extractDataWithKey");
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'processRecordData': function(someRecordData) {
		var records;
		var	recordReference;

//console.log("HeaderRecordData parameters", someRecordData);
//MochiKit.Logging.logDebug(">>> Header.processRecordData");
		records = someRecordData;
//MochiKit.Logging.logDebug("--- Header.processRecordData - 1");
		if (records != null) {
//MochiKit.Logging.logDebug("--- Header.processRecordData - records: " + Clipperz.Base.serializeJSON(records));
			for (recordReference in records) {
				var newRecord;
				var parameters;
	
//MochiKit.Logging.logDebug("--- Header.processRecordData - 2 - recordReference: " + recordReference);
				if (recordReference != "stacktrace") {
					parameters = records[recordReference];	//.slice();
//MochiKit.Logging.logDebug("--- Header.processRecordData - 3");
					if (typeof(parameters['notes']) != 'undefined') {
//MochiKit.Logging.logDebug("--- Header.processRecordData - 4");
						if (parameters['notes'] != "") {
//MochiKit.Logging.logDebug("--- Header.processRecordData - 5");
							parameters['headerNotes'] = parameters['notes'];
//MochiKit.Logging.logDebug("--- Header.processRecordData - 6");
						}
//MochiKit.Logging.logDebug("--- Header.processRecordData - 7");
						delete parameters['notes'];
//MochiKit.Logging.logDebug("--- Header.processRecordData - 8");
					}
//MochiKit.Logging.logDebug("--- Header.processRecordData - 9");
					parameters['reference'] = recordReference;
//MochiKit.Logging.logDebug("--- Header.processRecordData - 10");
					parameters['user'] = this.user();
//MochiKit.Logging.logDebug("--- Header.processRecordData - 11");
	
					newRecord = new Clipperz.PM.DataModel.Record(parameters);
//MochiKit.Logging.logDebug("--- Header.processRecordData - 12");
					this.user().addRecord(newRecord, true);
//MochiKit.Logging.logDebug("--- Header.processRecordData - 13");
				}
			}

//MochiKit.Logging.logDebug("--- Header.processRecordData - 14");
			Clipperz.NotificationCenter.notify(null, 'recordAdded', null, true);
//MochiKit.Logging.logDebug("--- Header.processRecordData - 15");
		}
//MochiKit.Logging.logDebug("<<< Header.processRecordData");
		
		return this.user().records();
	},
	
	//-------------------------------------------------------------------------

	'processDirectLoginData': function(someDirectLoginData) {
		var directLogins;
		var directLoginReference;
		
//MochiKit.Logging.logDebug(">>> Header.processDirectLoginData");
		directLogins = someDirectLoginData;
		if (directLogins != null) {
			for (directLoginReference in directLogins) {
				var directLoginReference;
				var parameters;
	
				parameters = directLogins[directLoginReference];	//.slice();
				parameters.user = this.user();
				parameters.reference = directLoginReference;
				directLoginReference = new Clipperz.PM.DataModel.DirectLoginReference(parameters);
				if (directLoginReference.record() != null) {
					this.user().addDirectLoginReference(directLoginReference, true);
				}
			}
		}

		Clipperz.NotificationCenter.notify(null, 'directLoginAdded', null, true);
//MochiKit.Logging.logDebug("<<< Header.processDirectLoginData");
		
		return this.user().directLoginReferences();
	},

	//-------------------------------------------------------------------------

	'shouldLoadSections': function() {
		return this._shouldLoadSections;
	},
	
	'shouldLoadSection': function(aSectionName) {
		var result;

		if (typeof(this.shouldLoadSections()[aSectionName]) != 'undefined') {
			result = this.shouldLoadSections()[aSectionName];
		} else {
			result = true;
		}
		
		return result;
	},
	
	'setShouldLoadSection': function(aSectionName, aValue) {
		this.shouldLoadSections()[aSectionName] = aValue;
	},
	
	//-------------------------------------------------------------------------
	
	'loadRecords': function() {
		var deferredResult;

		if (this.shouldLoadSection('records') == true) {
			this.setShouldLoadSection('records', false);

			deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadRecords 1: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this.user(), 'getUserDetails'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadRecords 2: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this, 'extractHeaderDataFromUserDetails'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadRecords 3: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this, 'extractDataWithKey', 'records'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadRecords 4: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this, 'processRecordData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadRecords 5: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.callback();
		} else {
			deferredResult = MochiKit.Async.succeed(this.user().records());
		}
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------
	
	'loadDirectLogins': function() {
		var deferredResult;

		if (this.shouldLoadSection('directLogins') == true) {
			this.setShouldLoadSection('directLogins', false);

			deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadDirectLogins - 1: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this.user(), 'getUserDetails'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadDirectLogins - 2: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this, 'extractHeaderDataFromUserDetails'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadDirectLogins - 3: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this, 'extractDataWithKey', 'directLogins'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadDirectLogins - 4: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this, 'processDirectLoginData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadDirectLogins - 5: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.callback();
		} else {
			deferredResult = MochiKit.Async.succeed(this.user().directLoginReferences());
		}
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'loadPreferences': function() {
		var deferredResult;

		if (this.shouldLoadSection('preferences') == true) {
			this.setShouldLoadSection('preferences', false);

			deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadPreferences - 1: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this.user(), 'getUserDetails'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadPreferences - 2: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this, 'extractHeaderDataFromUserDetails'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadPreferences - 3: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this, 'extractDataWithKey', 'preferences'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadPreferences - 4: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this.user().preferences(), 'updateWithData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadPreferences - 5: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.callback();
		} else {
			deferredResult = MochiKit.Async.succeed(this.user().preferences());
		}
		
		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	'loadOneTimePasswords': function() {
		var deferredResult;
		
		if (this.shouldLoadSection('oneTimePasswords') == true) {
			this.setShouldLoadSection('oneTimePasswords', false);

			deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadOneTimePasswords - 1: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this.user(), 'getUserDetails'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadOneTimePasswords - 2: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this, 'extractHeaderDataFromUserDetails'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadOneTimePasswords - 3: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this, 'extractDataWithKey', 'oneTimePasswords'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadOneTimePasswords - 4: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this.user().oneTimePasswordManager(), 'updateWithData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadOneTimePasswords - 5: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this.user().connection(), 'message'), 'getOneTimePasswordsDetails', {});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadOneTimePasswords - 6: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this.user().oneTimePasswordManager(), 'updateWithServerData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadOneTimePasswords - 7: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
			deferredResult.callback();
		} else {
			deferredResult = MochiKit.Async.succeed(this.user().oneTimePasswordManager());
		}
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'loadAllSections': function() {
		var deferredResult;

		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadAllSections - 1: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'loadRecords'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadAllSections - 2: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'loadDirectLogins'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadAllSections - 3: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'loadPreferences'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadAllSections - 4: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'loadOneTimePasswords'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Header.loadAllSections - 5: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.callback();
		
		return deferredResult;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});

