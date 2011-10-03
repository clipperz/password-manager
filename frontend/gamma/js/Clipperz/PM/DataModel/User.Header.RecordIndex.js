/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz's Javascript Crypto Library.
Javascript Crypto Library provides web developers with an extensive
and efficient set of cryptographic functions. The library aims to
obtain maximum execution speed while preserving modularity and
reusability.
For further information about its features and functionalities please
refer to http://www.clipperz.com

* Javascript Crypto Library is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Javascript Crypto Library is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Javascript Crypto Library.  If not, see
  <http://www.gnu.org/licenses/>.

*/

try { if (typeof(Clipperz.PM.DataModel.User) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.PM.DataModel.User.Header.RecordIndex depends on Clipperz.PM.DataModel.User!";
}  

if (typeof(Clipperz.PM.DataModel.User.Header) == 'undefined') { Clipperz.PM.DataModel.User.Header = {}; }

Clipperz.PM.DataModel.User.Header.RecordIndex = function(args) {
	Clipperz.PM.DataModel.User.Header.RecordIndex.superclass.constructor.apply(this, arguments);

//console.log("NEW Clipperz.PM.DataModel.User.Header.RecordIndex", args);
	this._recordsData = new Clipperz.PM.DataModel.EncryptedRemoteObject({
		'name':	'recordsData',
		'retrieveKeyFunction': args.retrieveKeyFunction,
		'remoteData': {
			'data': args.recordsData['data'],
			'version': args.encryptedDataVersion,
			'recordsStats': args.recordsStats
		}//,
//		'encryptedDataKeypath':			'data',
//		'encryptedVersionKeypath':		'version'
	});

	this._directLoginsData = new Clipperz.PM.DataModel.EncryptedRemoteObject({
		'name':	'directLoginsData',
		'retrieveKeyFunction': args.retrieveKeyFunction,
		'remoteData': {
			'data': args.directLoginsData['data'],
			'version': args.encryptedDataVersion
		}//,
//		'encryptedDataKeypath':			'data',
//		'encryptedVersionKeypath':		'version'
	});

	this._lock = new MochiKit.Async.DeferredLock();
	this._transientState = null;

	this._retrieveRecordDetailFunction	= args.retrieveRecordDetailFunction	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._recordsIndex					= args.recordsData['index']			|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._directLoginsIndex				= args.directLoginsData['index']	|| Clipperz.Base.exception.raise('MandatoryParameter');

	this._records = null;

	return this;
}


Clipperz.Base.extend(Clipperz.PM.DataModel.User.Header.RecordIndex, Object, {

	'toString': function() {
		return "Clipperz.PM.DataModel.User.Header.RecordIndex";
	},

	//-------------------------------------------------------------------------

	'retrieveRecordDetailFunction': function () {
		return this._retrieveRecordDetailFunction;
	},

	//-------------------------------------------------------------------------

	'recordsIndex': function () {
		return this._recordsIndex;
	},

	'recordsData': function () {
		return this._recordsData;
	},
	
	//-------------------------------------------------------------------------

	'directLoginsIndex': function () {
		return this._directLoginsIndex;
	},

	'directLoginsData': function () {
		return this._directLoginsData;
	},

	//-------------------------------------------------------------------------

	'lock': function () {
		return this._lock;
	},

	//-------------------------------------------------------------------------

	'transientState': function () {
		if (this._transientState == null) {
			this._transientState = new Clipperz.KeyValueObjectStore(/*{'name':'User.Header.RecordIndex.transientState [1]'}*/);
		}
		
		return this._transientState;
	},

	'resetTransientState': function (isCommitting) {
//console.log("######## UserHeaderRecordIndex - resetTransientState", Clipperz.Base.deepClone(this._transientState));
		if (this._transientState != null) {
			this._transientState.removeAllData();
		}

		this._transientState = null;
	},

	//-------------------------------------------------------------------------

	'getRecordKey': function (aRecordReference) {
		return Clipperz.Async.callbacks("User.Header.RecordIndex.getRecordKey", [
			MochiKit.Base.method(this, 'getRecordIndexData', aRecordReference),
			MochiKit.Base.itemgetter('key')
		], {trace:false});
	},

	'setRecordKey': function (aRecordReference, aValue) {
		return this.updateRecordIndexData(aRecordReference, 'key', aValue);
	},

	//-------------------------------------------------------------------------
	
	'getRecordIndexData': function (aRecordReference) {
		return this.recordsData().getValue(this.recordsIndex()[aRecordReference]);
	},

	//.........................................................................

	'updateRecordIndexData': function (aRecordReference, aKey, aValue) {
		return this.recordsData().setValue(this.recordsIndex()[aRecordReference]+'.'+aKey, aValue);
	},

	//-------------------------------------------------------------------------

	'getDirectLoginIndexData': function (aDirectLoginReference) {
		return this.directLoginsData().getValue(this.directLoginsIndex()[aDirectLoginReference]);
	},

	'setDirectLoginIndexData': function (aDirectLoginReference, aKey, aValue) {
//console.log("UserHeaderRecordIndex.setDirectLoginIndexData", aDirectLoginReference, this.directLoginsIndex()[aDirectLoginReference], aKey);
//if (MochiKit.Base.isUndefinedOrNull(this.directLoginsIndex()[aDirectLoginReference])) {
//	throw "PIPPO";
//}
		return this.directLoginsData().setValue(this.directLoginsIndex()[aDirectLoginReference] + '.' + aKey, aValue);
	},

	'addDirectLoginIndexData': function (aDirectLoginReference) {
//console.log("UserHeaderRecordIndex.addDirectLoginIndexData", aDirectLoginReference, this.directLoginsIndex()[aDirectLoginReference]);
		return this.directLoginsData().setValue(this.directLoginsIndex()[aDirectLoginReference], {});
	},
	
	'removeDirectLoginIndexData': function (aDirectLoginReference) {
		return this.directLoginsData().removeValue(this.directLoginsIndex()[aDirectLoginReference])
	},

	//-------------------------------------------------------------------------

	'records': function () {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("User.Header.RecordIndex.records", {trace:false});
		deferredResult.acquireLock(this.lock());
		deferredResult.addCallback(MochiKit.Base.bind(function () {
			var innerDeferredResult;

			if (this._records == null) {
				innerDeferredResult = new Clipperz.Async.Deferred("User.Header.RecordIndex.records <inner deferred>", {trace:false});
				innerDeferredResult.collectResults({
					'records': [
//						MochiKit.Base.method(this.recordsData(), 'getObjectDataStore'),
//						MochiKit.Base.methodcaller('values')
						MochiKit.Base.method(this.recordsData(), 'values')
					],
					'recordsStats': [
						MochiKit.Base.method(this.recordsData(), 'getRemoteData'),
						MochiKit.Base.itemgetter('recordsStats')
					],
					'directLogins': [
//						MochiKit.Base.method(this.directLoginsData(), 'getObjectDataStore'),
//						MochiKit.Base.methodcaller('values')
						MochiKit.Base.method(this.directLoginsData(), 'values')
					]
				})
				innerDeferredResult.addCallback(MochiKit.Base.bind(function (someData) {
					var indexReference;
					var recordsInvertedIndex;
					var directLoginsInvertedIndex;
					
					recordsInvertedIndex		= Clipperz.PM.DataModel.User.Header.RecordIndex.invertIndex(this.recordsIndex());
					directLoginsInvertedIndex	= Clipperz.PM.DataModel.User.Header.RecordIndex.invertIndex(this.directLoginsIndex());

					this._records = {};

					for (indexReference in someData['records']) {
						var	record;
						var reference;
						var updateDate;

						reference = recordsInvertedIndex[indexReference];
						
						if (typeof(someData['recordsStats'][reference]) != 'undefined') {
							updateDate = someData['recordsStats'][reference]['updateDate'];
						
							record = new Clipperz.PM.DataModel.Record({
								'reference':					reference,
								'retrieveKeyFunction':			MochiKit.Base.method(this, 'getRecordKey'),
								'retrieveRemoteDataFunction':	this.retrieveRecordDetailFunction(),

								'retrieveIndexDataFunction':	MochiKit.Base.method(this, 'getRecordIndexData'),
								'updateIndexDataFunction':		MochiKit.Base.method(this, 'updateRecordIndexData'),
								'updateDate':					updateDate,

								'retrieveDirectLoginIndexDataFunction':	MochiKit.Base.method(this, 'getDirectLoginIndexData'),
								'setDirectLoginIndexDataFunction':		MochiKit.Base.method(this, 'setDirectLoginIndexData'),
								'removeDirectLoginIndexDataFunction':	MochiKit.Base.method(this, 'removeDirectLoginIndexData'),
							
								'createNewDirectLoginFunction':			MochiKit.Base.method(this, 'createNewDirectLogin')
							});
		
							this._records[reference] = record;
						} else {
Clipperz.log("SKIPPING record " + reference + " as there are no stas associated - " + Clipperz.Base.serializeJSON(someData['records'][reference]));
							//	# skip the record, as it seems it is not present in the DB
							//	updateDate = Clipperz.PM.Date.formatDateWithUTCFormat(new Date());
						}
					}

					for (indexReference in someData['directLogins']) {
//						var	directLogin;
						var reference;
						var record;

						reference = directLoginsInvertedIndex[indexReference];
						record = this._records[recordsInvertedIndex[someData['directLogins'][indexReference]['record']]];

						if (record != null) {
//							directLogin = new Clipperz.PM.DataModel.DirectLogin({
							new Clipperz.PM.DataModel.DirectLogin({
								'reference':					reference,
								'record':						record
							});
						} else {
Clipperz.log("WARNING: DIRECT LOGIN without a matching RECORD!!");
//console.log("direct login data", someData['directLogins']);
//console.log("current direct login data", someData['directLogins'][indexReference])
//console.log("reference", reference);
//console.log("record index", this.recordsIndex());
//console.log("record inverted index", recordsInvertedIndex);
						}
					}

					return this._records;
				}, this));
				innerDeferredResult.callback();
			} else {
				innerDeferredResult = MochiKit.Async.succeed(this._records);
			}
			
			return innerDeferredResult;
		}, this));
		deferredResult.releaseLock(this.lock());
		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'updateRecordIndexForNewRecord': function (aNewRecord) {
		var newRecordIndex;
		var recordReference;

		recordReference = aNewRecord.reference();
		newRecordIndex = (MochiKit.Base.listMax(MochiKit.Base.map(MochiKit.Base.partial(MochiKit.Base.operator.mul, 1), MochiKit.Base.values(this.recordsIndex()))) + 1) + '';
		this.recordsIndex()[recordReference] = newRecordIndex;
		
		this.transientState().setValue('newlyCreatedRecordsIndex'		+ '.' + recordReference, newRecordIndex);
		this.transientState().setValue('newlyCreatedRecordsReferences'	+ '.' + recordReference, aNewRecord);
	},

	//.........................................................................
	
	'createNewRecord': function () {
		var deferredResult;
		var newRecord;

//console.log("#### new Clipperz.PM.DataModel.Record [4]");
		newRecord = new Clipperz.PM.DataModel.Record({
			'retrieveKeyFunction':			MochiKit.Base.method(this, 'getRecordKey'),
			'retrieveRemoteDataFunction':	this.retrieveRecordDetailFunction(),

			'retrieveIndexDataFunction':	MochiKit.Base.method(this, 'getRecordIndexData'),
			'updateIndexDataFunction':		MochiKit.Base.method(this, 'updateRecordIndexData'),
			'updateDate':					Clipperz.PM.Date.formatDateWithUTCFormat(new Date()),

			'retrieveDirectLoginIndexDataFunction':	MochiKit.Base.method(this, 'getDirectLoginIndexData'),
			'setDirectLoginIndexDataFunction':		MochiKit.Base.method(this, 'setDirectLoginIndexData'),
			'removeDirectLoginIndexDataFunction':	MochiKit.Base.method(this, 'removeDirectLoginIndexData'),

			'createNewDirectLoginFunction':			MochiKit.Base.method(this, 'createNewDirectLogin')
		});

		this.transientState().setValue('newRecordsReferences' + '.' + newRecord.reference(), newRecord);
		this.updateRecordIndexForNewRecord(newRecord);

		deferredResult = Clipperz.Async.callbacks("User.Header.RecordIndex.createNewRecord", [
			MochiKit.Base.method(this, 'records'),
			MochiKit.Base.partial(Clipperz.Async.setItemOnObject, newRecord.reference(), newRecord),
			MochiKit.Base.method(this, 'setRecordKey', newRecord.reference(), Clipperz.PM.Crypto.randomKey()),
			MochiKit.Base.method(newRecord, 'setLabel', ''),
			MochiKit.Base.partial(MochiKit.Async.succeed, newRecord)
		], {trace:false});

		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'deleteRecord': function (aRecord) {
		var deferredResult;
		var recordReference;
		
		recordReference = aRecord.reference();

		deferredResult = new Clipperz.Async.Deferred("User.Header.RecordIndex.deleteRecord", {trace:false});

		deferredResult.addMethod(aRecord, 'directLogins');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.method(this, 'removeDirectLogin'));

		deferredResult.addMethod(this.recordsData(),  'removeValue', this.recordsIndex()[recordReference]);
		deferredResult.addCallback(MochiKit.Base.bind(function () {
			this.transientState().setValue('deleteRecordsIndex' + '.' + recordReference, this.recordsIndex()[recordReference]);
			delete this.recordsIndex()[recordReference];
		}, this));
		
		deferredResult.addMethod(this,  'records');
		deferredResult.addCallback(MochiKit.Base.itemgetter(recordReference));
		deferredResult.addMethod(this.transientState(), 'setValue', 'deleteRecordsReferences' + '.' + recordReference);

		deferredResult.addMethod(this,  'records');
		deferredResult.addCallback(MochiKit.Base.bind(function (someRecords) {
			delete someRecords[recordReference];
		}, this));
		deferredResult.callback();
		
		return deferredResult;
	},

	//=========================================================================

	'removeDirectLogin': function (aDirectLogin) {
		this.directLoginsData().removeValue(this.directLoginsIndex()[aDirectLogin.reference()]);
	},
	
	//-------------------------------------------------------------------------

	'createNewDirectLogin': function (aRecord) {
		var newDirectLogin;
		var	newDirectLoginIndexValue;

		newDirectLogin = new Clipperz.PM.DataModel.DirectLogin({record:aRecord});
		newDirectLoginIndexValue = MochiKit.Base.listMax(MochiKit.Base.map(function (aValue) { return aValue * 1; }, MochiKit.Base.values(this.directLoginsIndex()))) + 1;

		this.transientState().setValue('newDirectLoginReferences' + '.' + newDirectLogin.reference(), newDirectLogin);

//console.log("UserHeaderRecordIndex.createNewDirectLogin [1]", newDirectLogin.reference(), newDirectLoginIndexValue);
		this.directLoginsIndex()[newDirectLogin.reference()] = newDirectLoginIndexValue;
//console.log("UserHeaderRecordIndex.createNewDirectLogin [2]", newDirectLogin.reference(), this.directLoginsIndex()[newDirectLogin.reference()]);
		this.directLoginsData().setValue(this.directLoginsIndex()[newDirectLogin.reference()], {'record': this.recordsIndex()[aRecord.reference()]});
	
		return newDirectLogin;
	},

	//=========================================================================

	'deleteAllCleanTextData': function () {
		return Clipperz.Async.callbacks("User.Header.RecordIndex.deleteAllCleanTextData", [
//			MochiKit.Base.method(this, 'records'),
//			MochiKit.Base.values,
//			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('deleteAllCleanTextData')),

			MochiKit.Base.method(this, 'recordsData'),
			MochiKit.Base.methodcaller('deleteAllCleanTextData'),
			MochiKit.Base.method(this, 'directLoginsData'),
			MochiKit.Base.methodcaller('deleteAllCleanTextData')
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'hasAnyCleanTextData': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred({trace:false});
		deferredResult.collectResults({
			'recordsData': [
				MochiKit.Base.method(this, 'recordsData'),
				MochiKit.Base.methodcaller('hasAnyCleanTextData')
			],
			'directLoginsData':	[
				MochiKit.Base.method(this, 'directLoginsData'),
				MochiKit.Base.methodcaller('hasAnyCleanTextData')
			],
//			'records': [
//				MochiKit.Base.method(this, 'records'),
//				MochiKit.Base.values,
//				MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('hasAnyCleanTextData')),
//				Clipperz.Async.collectAll
//			]
		});
//deferredResult.addCallback(function (aValue) { console.log("USER.Header.RecordIndex.hasAnyCleanTextData", aValue); return aValue});

//		deferredResult.addCallback(MochiKit.Base.values);
//		deferredResult.addCallback(MochiKit.Base.flattenArguments);
//		deferredResult.addCallback(function(someValues) {
//			return MochiKit.Iter.some(someValues, MochiKit.Base.operator.identity);
//		});
		deferredResult.addCallback(Clipperz.Async.or);
		
		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'hasPendingChanges': function () {
		var	deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("User.Header.RecordIndex.hasPendingChanges", {trace:false});
		deferredResult.collectResults({
			'recordsData': [
				MochiKit.Base.method(this, 'recordsData'),
				MochiKit.Base.methodcaller('hasPendingChanges')
			],
			'directLoginsData': [
				MochiKit.Base.method(this, 'directLoginsData'),
				MochiKit.Base.methodcaller('hasPendingChanges')
			]
		});
//deferredResult.addCallback(function (aValue) { console.log("UserHeaderIndex.hasPendingResults", aValue); return aValue; });
		deferredResult.addCallback(Clipperz.Async.or);
//		deferredResult.addCallback(MochiKit.Base.values);
//		deferredResult.addCallback(MochiKit.Base.flattenArguments);
//		deferredResult.addCallback(function(someValues) {
//			return MochiKit.Iter.some(someValues, MochiKit.Base.operator.identity);
//		});
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'commitTransientState': function () {
		var deferredResult;

		deferredResut = Clipperz.Async.callbacks("User.Header.RecordIndex.commitTransientState", [
			MochiKit.Base.method(this, 'recordsData'),
			MochiKit.Base.methodcaller('commitTransientState'),

			MochiKit.Base.method(this, 'directLoginsData'),
			MochiKit.Base.methodcaller('commitTransientState'),
			
			MochiKit.Base.method(this, 'resetTransientState', true)
		], {trace:false});

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'revertChanges': function () {
		return Clipperz.Async.callbacks("User.Header.RecordIndex.revertChanges", [
			MochiKit.Base.method(this, 'recordsData'),
			MochiKit.Base.methodcaller('revertChanges'),

//			MochiKit.Base.method(this, 'directLoginsData'),
//			MochiKit.Base.methodcaller('revertChanges'),

			MochiKit.Base.method(this, 'records'),
			MochiKit.Base.bind(function (someRecords) {
				var	recordReference;
				
				for (recordReference in this.transientState().getValue('deleteRecordsReferences')) {
					this.recordsIndex()[recordReference] = this.transientState().getValue('deleteRecordsIndex' + '.' + recordReference);
					someRecords[recordReference] = this.transientState().getValue('deleteRecordsReferences' + '.' + recordReference);
				}

				for (recordReference in this.transientState().getValue('newRecordsReferences')) {
					delete this.recordsIndex()[recordReference];
					delete someRecords[recordReference];
				}
			}, this),

//			MochiKit.Base.method(this, 'directLogins'),
			MochiKit.Base.bind(function () {
				var	directLoginReference;

//	this.transientState().setValue('newDirectLoginReferences' + '.' + newDirectLogin.reference(), newDirectLogin);
//
//	this.directLoginsIndex()[newDirectLogin.reference()] = newDirectLoginIndexValue;
//	this.directLoginsData().setValue(this.directLoginsIndex()[newDirectLogin.reference()], {'record': this.recordsIndex()[aRecord.reference()]});

				
//				for (directLoginReference in this.transientState().getValue('deleteDirectLoginReferences')) {
//					someDirectLogins[directLoginReference] = this.transientState().getValue('deleteDirectLoginReferences' + '.' + recordReference);
//				}

				for (directLoginReference in this.transientState().getValue('newDirectLoginReferences')) {
//					this.directLoginsData().removeValue(this.directLoginsIndex()[directLoginReference]);
					delete this.directLoginsIndex()[directLoginReference];
				}
			}, this),

			MochiKit.Base.method(this, 'directLoginsData'),
			MochiKit.Base.methodcaller('revertChanges'),

			MochiKit.Base.method(this, 'resetTransientState', false)
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'prepareRemoteDataWithKey': function (aKey) {
//		"records": {
//			"index": {
//				"eeda70e0392261967bda71c3764da78989c45bbd2bb7be6b941b90f81d9b81b5": "0",
//				"13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551": "1",
//				...
//				"465a067a0bd2b470fa834de5397e38494de0c7707938262fae3427932e219744": "18",
//				"4fd1dc2ca860b7fb47cef10a84edb3270da05510b0a30a6b0b083898712d4b9e": "19"
//			},
//			"data": "n+AzGEEQXaSRSY4d ... BDypotrXgPo94uHfoXvGFzwCn8w="
//		},
//		"directLogins": {
//			"index": {
//				"61e87fdc4f1d9112e3b30c1f6812d095dcdb24f014c83319091eb6c9899ec348":"0",
//				"989593d4c48929f0c8f1581aa96969c622807e99619ed4732026e967530a68ad":"1",
//				...
//				"cb9ae0bba1957075ccdbfd3b3481704d62087687a2ac7c411a4f07d444bde0f7":"17",
//				"7e1d069b7fa57c03bd7bf48807520feb953157834503aaff8c9d493f37dea69d":"18"
//			},
//			"data":"5YG9KKU/OZ5guUgFlms6k1 ... ZG/5Fn0uN+LoAsNfHm+EE62x"
//		},
		
		var deferredResult;
		var result;

//console.log("recordsIndex", this.recordsIndex());
		result = {};

		deferredResult = new Clipperz.Async.Deferred("User.Header.RecordIndex.prepareRemoteDataWithKey", {trace:false});
		deferredResult.collectResults({
			'index':	MochiKit.Base.partial(MochiKit.Async.succeed, this.recordsIndex()),
			'data': [
				MochiKit.Base.method(this.recordsData(), 'prepareRemoteDataWithKey', aKey),
				MochiKit.Base.itemgetter('data')
			]
		});
		deferredResult.addCallback(Clipperz.Async.setItem, result, 'records');

		deferredResult.collectResults({
			'index':	MochiKit.Base.partial(MochiKit.Async.succeed, this.directLoginsIndex()),
			'data': [
				MochiKit.Base.method(this.directLoginsData(), 'prepareRemoteDataWithKey', aKey),
				MochiKit.Base.itemgetter('data')
			]
		});
		deferredResult.addCallback(Clipperz.Async.setItem, result, 'directLogins');
		
		deferredResult.addCallback(MochiKit.Async.succeed, result);

		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'updateRecordKeyAndPrepareRemoteData': function (aRecord) {
		var	newRecordKey;
		var deferredResult;
		
		newRecordKey = Clipperz.PM.Crypto.randomKey();

		deferredResult = new Clipperz.Async.Deferred("User.Header.RecordIndex.updateRecordKeyAndPrepareRemoteData", {trace:false});
		deferredResult.addCallback(MochiKit.Base.method(aRecord, 'prepareRemoteDataWithKey', newRecordKey));
		deferredResult.addCallbackPass(MochiKit.Base.method(this, 'setRecordKey', aRecord.reference(), newRecordKey));
		deferredResult.callback();

		return deferredResult;
	},

	//.........................................................................

	'removeNewRecordWithNoChanges': function (aRecord) {
		var deferredResult;
		var recordReference;
		
		recordReference = aRecord.reference();
		
		deferredResult = new Clipperz.Async.Deferred("User.Header.RecordIndex.removeNewRecordWithNoChanges", {trace:false});

		deferredResult.addMethod(this.recordsData(),  'removeValue', this.recordsIndex()[recordReference]);
		deferredResult.addCallback(MochiKit.Base.bind(function () {
			delete this.recordsIndex()[recordReference];
		}, this));
		
		deferredResult.addMethod(this,  'records');
		deferredResult.addCallback(MochiKit.Base.bind(function (someRecords) {
			delete someRecords[recordReference];
		}, this));
		deferredResult.callback();
		
		return deferredResult;
	},

	//.........................................................................

	'prepareRemoteDataForChangedRecords': function () {
		var	deferredResult;
		var	result;
		
		result = {};

		deferredResult = new Clipperz.Async.Deferred("User.Header.RecordIndex.prepareRemoteDataForChangedRecords", {trace:false});

		deferredResult.addMethod(this, 'records');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(Clipperz.Async.deferredFilter, MochiKit.Base.methodcaller('isBrandNewWithNoPendingChanges'));
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.method(this, 'removeNewRecordWithNoChanges'));

		deferredResult.addMethod(this, 'records');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(Clipperz.Async.deferredFilter, MochiKit.Base.methodcaller('hasPendingChanges'));
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.method(this, 'updateRecordKeyAndPrepareRemoteData'));
		deferredResult.addCallback(Clipperz.Async.collectAll);

		deferredResult.addCallback(Clipperz.Async.deferredIf("updated records != null", [
			MochiKit.Base.operator.identity
		], [
			MochiKit.Base.partial(MochiKit.Async.succeed, [])
		]));
		deferredResult.addCallback(Clipperz.Async.setItem, result, 'updated');
		
		deferredResult.addMethod(this.transientState(), 'getValue', 'deleteRecordsReferences');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(Clipperz.Async.deferredIf("deleted records != null", [
			MochiKit.Base.operator.identity
		], [
			MochiKit.Base.partial(MochiKit.Async.succeed, [])
		]));
		deferredResult.addCallback(Clipperz.Async.setItem, result, 'deleted');
		
		deferredResult.addCallback(MochiKit.Async.succeed, result);
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});



Clipperz.PM.DataModel.User.Header.RecordIndex.invertIndex = function (anIndex) {
	var result;
	var key;
	
	result = {};
	
	for (key in anIndex) {
		result[anIndex[key]] = key;
	}
	
	return result;
};