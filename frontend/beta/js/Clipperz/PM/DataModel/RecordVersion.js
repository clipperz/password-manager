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

Clipperz.PM.DataModel.RecordVersion = function(aRecord, args) {
	args = args || {};

	this._record = aRecord;

	this._reference = args.reference || Clipperz.PM.Crypto.randomKey();
	this._version = args.version || Clipperz.PM.Crypto.encryptingFunctions.currentVersion;
	this._key = args.key || Clipperz.PM.Crypto.randomKey();;

	this._previousVersion = args.previousVersion || null;
	this._previousVersionKey = args.previousVersionKey || null;

	this.setIsBrandNew(args.reference == null);

	if (this.isBrandNew()) {
		this._fields = {};

		this.setShouldLoadData(false);
		this.setShouldDecryptData(false);
		this.setShouldProcessData(false);
	} else {
		if (typeof(args.fields) != 'undefined') {
			this.processFieldData(args.fields);
			
			this.setShouldLoadData(false);
			this.setShouldDecryptData(false);
			this.setShouldProcessData(false);
		} else {
			 if (typeof(args.data) != 'undefined') {
				this.setShouldLoadData(false);
			} else {
				this.setShouldLoadData(true);
			}
			this.setShouldDecryptData(true);
			this.setShouldProcessData(true);
		}
	}

	this._serverData = args.data;
	this._decryptedData = null;

	return this;
}

Clipperz.PM.DataModel.RecordVersion.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "RecordVersion";
	},

	//-------------------------------------------------------------------------

	'record': function() {
		return this._record;
	},
	
	//-------------------------------------------------------------------------

	'reference': function() {
		return this._reference;
	},
	
	'setReference': function(aValue) {
		this._reference = aValue;
	},
	
	//-------------------------------------------------------------------------

	'key': function() {
//MochiKit.Logging.logDebug(">>> RecordVersion.key");
//MochiKit.Logging.logDebug("--- RecordVersion.key - " + this._key);
		return this._key;
	},
	
	'setKey': function(aValue) {
		this._key = aValue;
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
//MochiKit.Logging.logDebug(">>> RecordVersion.decryptedData: " + (this._decryptedData ? Clipperz.Base.serializeJSON(aValue) : "null"));
		return this._decryptedData;
	},
	
	'setDecryptedData': function(aValue) {
//MochiKit.Logging.logDebug(">>> RecordVersion.setDecryptedData: " + Clipperz.Base.serializeJSON(aValue));
		this._decryptedData = aValue;
		this.setShouldDecryptData(false);
		return aValue;
	},
	
	//-------------------------------------------------------------------------

	'version': function() {
		return this._version;
	},

	//-------------------------------------------------------------------------

	'isBrandNew': function() {
		return this._isBrandNew;
	},

	'setIsBrandNew': function(aValue) {
		this._isBrandNew = aValue;
	},
	
	//-------------------------------------------------------------------------

	'fields': function() {
		return this._fields;
	},

	'addField': function(aField) {
		this.fields()[aField.key()] = aField;
	},

	'addNewField': function() {
		var	newRecordField;
		
		newRecordField = new Clipperz.PM.DataModel.RecordField({recordVersion:this});
		this.addField(newRecordField);
		
		return newRecordField;
	},
	
	'fieldWithName': function(aValue) {
		var	result;
		var fieldValues;
		var i,c;
		
		result = null;
		fieldValues = MochiKit.Base.values(this.fields());
		c = fieldValues.length;
		for (i=0; (i<c) && (result == null); i++) {
			var	currentField;
			
			currentField = fieldValues[i];
			if (currentField.label() == aValue) {
				result = currentField;
			}
		}

		return result;
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

	'deferredData': function() {
		var deferredResult;
		
//MochiKit.Logging.logDebug(">>> RecordVersion.deferredData - this: " + this);
		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(MochiKit.Base.method(this, 'loadData'));
		deferredResult.addCallback(MochiKit.Base.method(this, 'decryptData'));
		deferredResult.addCallback(MochiKit.Base.method(this, 'processData'));
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< RecordVersion.deferredData");
		
		return deferredResult;
	},
	
	//-------------------------------------------------------------------------
	
	'loadData': function() {
		var result;
		
//MochiKit.Logging.logDebug(">>> RecordVersion.loadData - this: " + this);
		if (this.shouldLoadData()) {
			var deferredResult;

			alert("ERROR: this should have not happened yet!");
//MochiKit.Logging.logDebug("--- RecordVersion.loadData - 1");
			deferredResult = new MochiKit.Async.Deferred();
//MochiKit.Logging.logDebug("--- RecordVersion.loadData - 2");
			deferredResult.addCallback(MochiKit.Base.method(this, 'notify'), 'loadingRecordVersionData');
//MochiKit.Logging.logDebug("--- RecordVersion.loadData - 3");
			deferredResult.addCallback(MochiKit.Base.method(this.user().connection(), 'message'), 'getRecordVersionDetail', {reference: this.reference()});
//MochiKit.Logging.logDebug("--- RecordVersion.loadData - 4");
			deferredResult.addCallback(MochiKit.Base.method(this, 'setServerData'));
//MochiKit.Logging.logDebug("--- RecordVersion.loadData - 5");
			deferredResult.callback();
//MochiKit.Logging.logDebug("--- RecordVersion.loadData - 6");
			result = deferredResult;
//MochiKit.Logging.logDebug("--- RecordVersion.loadData - 7");
		} else {
//MochiKit.Logging.logDebug("--- RecordVersion.loadData - 8");
			result = MochiKit.Async.succeed(this.serverData());
//MochiKit.Logging.logDebug("--- RecordVersion.loadData - 9");
		}
//MochiKit.Logging.logDebug("<<< RecordVersion.loadData");

		return result;
	},

	//-------------------------------------------------------------------------

	'decryptData': function(anEncryptedData) {
		var result;
		
//MochiKit.Logging.logDebug(">>> RecordVersion.decryptData - this: " + this + " (" + anEncryptedData + ")");
		if (this.shouldDecryptData()) {
			var deferredResult;
			
//MochiKit.Logging.logDebug("--- RecordVersion.decryptData - 1");
			deferredResult = new MochiKit.Async.Deferred();
//MochiKit.Logging.logDebug("--- RecordVersion.decryptData - 2");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.decryptData 1: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this, 'notify'), 'decryptingRecordVersionData');
//MochiKit.Logging.logDebug("--- RecordVersion.decryptData - 3");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.decryptData 2: " + res); return res;});
			deferredResult.addCallback(Clipperz.PM.Crypto.deferredDecrypt, this.key(), anEncryptedData, this.version());
//MochiKit.Logging.logDebug("--- RecordVersion.decryptData - 4");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.decryptData 3: " + res); return res;});
//MochiKit.Logging.logDebug("--- RecordVersion.decryptData - 5");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.decryptData 4: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this, 'setDecryptedData'));
//MochiKit.Logging.logDebug("--- RecordVersion.decryptData - 6");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.decryptData 5: " + res); return res;});
			deferredResult.callback();
//MochiKit.Logging.logDebug("--- RecordVersion.decryptData - 7");
			result = deferredResult;
//MochiKit.Logging.logDebug("--- RecordVersion.decryptData - 8");
		} else {
//MochiKit.Logging.logDebug("--- RecordVersion.decryptData - 9");
			result = MochiKit.Async.succeed(this.decryptedData());
//MochiKit.Logging.logDebug("--- RecordVersion.decryptData - 10");
		}
//MochiKit.Logging.logDebug("<<< RecordVersion.decryptData");

		return result;
	},

	//-------------------------------------------------------------------------

	'processFieldData': function(someValues) {
		var fieldValues;
		
		this._fields = {};

		if (typeof(someValues) == 'undefined') {
			fieldValues = {};
		} else {
			fieldValues = someValues;
		}

		if (fieldValues.constructor == Array) {
			var i, c;
			c = fieldValues.length;
			for (i=0; i<c; i++) {
				var newRecordField;
				var currentFieldValues;
		
				currentFieldValues = fieldValues[i];
				currentFieldValues['recordVersion'] = this;
				newRecordField = new Clipperz.PM.DataModel.RecordField(currentFieldValues);
				this._fields[newRecordField.key()] = newRecordField;
			}
			
		} else {
			var fieldKey;
			
			for (fieldKey in fieldValues) {
				var newRecordField;
				var currentFieldValues;
		
				currentFieldValues = fieldValues[fieldKey];
				currentFieldValues['key'] = fieldKey;
				currentFieldValues['recordVersion'] = this;
				newRecordField = new Clipperz.PM.DataModel.RecordField(currentFieldValues);
				this._fields[fieldKey] = newRecordField;
			}
		}
		
	},
	
	'processData': function(someValues) {
		if (this.shouldProcessData()) {
			this.processFieldData(someValues.fields);
			this.setShouldProcessData(false);
		}
		
		this.notify('recordVersionDataReady');
		
		return this;
	},

	//-------------------------------------------------------------------------

	'notify': function(aValue) {
		Clipperz.NotificationCenter.notify(this, aValue);
	},
	
	//-------------------------------------------------------------------------

	'removeField': function(aField) {
		delete this.fields()[aField.key()];
	},

	//-------------------------------------------------------------------------

	'previousVersion': function() {
		return this._previousVersion;
	},

	'setPreviousVersion': function(aValue) {
		this._previousVersion = aValue;
	},
	
	//-------------------------------------------------------------------------
	
	'previousVersionKey': function() {
		return this._previousVersionKey;
	},
	
	'setPreviousVersionKey': function(aValue) {
		this._previousVersionKey = aValue;
	},
	
	//-------------------------------------------------------------------------

	'serializedData': function() {
		var result;
		var	fieldKey;

//MochiKit.Logging.logDebug(">>> RecordVersion.serializedData");
		result = {
			fields: {}
		};
//MochiKit.Logging.logDebug("--- RecordVersion.serializedData - 1");
		
		for (fieldKey in this.fields()) {
//MochiKit.Logging.logDebug("--- RecordVersion.serializedData - 2");
			result.fields[fieldKey] = this.fields()[fieldKey].serializeData();
//MochiKit.Logging.logDebug("--- RecordVersion.serializedData - 3");
		}
//MochiKit.Logging.logDebug("--- RecordVersion.serializedData - 4");
//MochiKit.Logging.logDebug("<<< RecordVersion.serializedData: " + Clipperz.Base.serializeJSON(result));

		return result;
	},

	'currentDataSnapshot': function() {
		var result;

		result = this.serializedData();
		result['version'] = this.version();
		result['reference'] = this.reference();
		result['previousVersionKey'] = this.previousVersionKey();
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'encryptedData': function() {
		var deferredResult;
		var result;
		
//MochiKit.Logging.logDebug(">>> RecordVersion.encryptedData - " + this);
		result = {};
		deferredResult = new MochiKit.Async.Deferred();
//MochiKit.Logging.logDebug("--- RecordVersion.encryptedData - 1");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.encryptedData - 1: " + res); return res;});
		deferredResult.addCallback(function(aResult, aRecordVersion) {
			aResult['reference'] = aRecordVersion.reference();
			aResult['recordReference'] = aRecordVersion.record().reference();	// TODO - this seems to be completely useless
			return aResult;
		}, result, this);
//MochiKit.Logging.logDebug("--- RecordVersion.encryptedData - 2");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.encryptedData - 2: " + res); return res;});
		deferredResult.addCallback(Clipperz.PM.Crypto.deferredEncryptWithCurrentVersion, this.key(), this.serializedData());
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.encryptedData - 3: " + res); return res;});
		deferredResult.addCallback(function(aResult, res) {
			aResult['data'] = res;
			return aResult;
		}, result);
//MochiKit.Logging.logDebug("--- RecordVersion.encryptedData - 3");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.encryptedData - 4: " + res); return res;});
		deferredResult.addCallback(function(aResult) {
			aResult['version'] = Clipperz.PM.Crypto.encryptingFunctions.currentVersion;
			return aResult;
		}, result);
//MochiKit.Logging.logDebug("--- RecordVersion.encryptedData - 4");
		if (this.previousVersion() != null) {
//MochiKit.Logging.logDebug("--- RecordVersion.encryptedData - 5");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.encryptedData - 5: " + res); return res;});
			deferredResult.addCallback(function(aResult, aRecordVersion) {
				aResult['previousVersion'] = aRecordVersion.previousVersion();
				return aResult;
			}, result, this);
//MochiKit.Logging.logDebug("--- RecordVersion.encryptedData - 6");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.encryptedData - 6: " + res); return res;});
			deferredResult.addCallback(Clipperz.PM.Crypto.deferredEncryptWithCurrentVersion, this.key(), this.previousVersionKey());
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.encryptedData - 7: " + res); return res;});
			deferredResult.addCallback(function(aResult, res) {
				aResult['previousVersionKey'] = res;
				return aResult;
			}, result);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.encryptedData - 8: " + res); return res;});
//MochiKit.Logging.logDebug("--- RecordVersion.encryptedData - 7");
		} else {
//MochiKit.Logging.logDebug("--- RecordVersion.encryptedData - 8");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.encryptedData - 9: " + res); return res;});
			deferredResult.addCallback(function(aResult) {
				aResult['previousVersionKey'] = Clipperz.PM.Crypto.nullValue;
				return aResult;
			}, result);
//MochiKit.Logging.logDebug("--- RecordVersion.encryptedData - 9");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.encryptedData - 10: " + res); return res;});
		};
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RecordVersion.encryptedData - 11: " + res); return res;});
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< RecordVersion.encryptedData");
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'createNewVersion': function() {
		if (this.record().isBrandNew() == false) {
			this.setPreviousVersion(this.reference());
			this.setPreviousVersionKey(this.key());

			this.setReference(Clipperz.PM.Crypto.randomKey());
			this.setKey(Clipperz.PM.Crypto.randomKey());
		}
	},
	
	//-------------------------------------------------------------------------
/*
	'shouldLoadData': function() {
		return ((this.data() == null) && (this.isBrandNew() === false));
	},
	
	'loadData': function() {
//MochiKit.Logging.logDebug(">>> Record.loadData (" + this.label() + ")");
//		if (this.shouldLoadData()) {
//			this.user().connection().message(	'getRecordDetail',
//												{recordReference: this.reference()},
//												{	callback:MochiKit.Base.bind(this.loadDataCallback, this),
//													errorHandler:Clipperz.PM.defaultErrorHandler	});		
//		} else {
//			this.notify('loadDataDone');
//		}
	},

	'loadDataCallback': function() {
MochiKit.Logging.logDebug("RecordVersion.loadDataCallback: " + Clipperz.Base.serializeJSON(arguments));
	},
*/
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});

