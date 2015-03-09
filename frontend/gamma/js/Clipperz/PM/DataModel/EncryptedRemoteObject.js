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

try { if (typeof(Clipperz.KeyValueObjectStore) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.PM.DataModel.EncryptedRemoteObject depends on Clipperz.KeyValueObjectStore!";
}  

if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.DataModel) == 'undefined') { Clipperz.PM.DataModel = {}; }

Clipperz.PM.DataModel.EncryptedRemoteObject = function(args) {
	args = args || {};

	this._name = args.name || null;
	this._reference = args.reference || Clipperz.PM.Crypto.randomKey();
	this._isBrandNew = ((args.reference == null) && (args.remoteData == null));

	if ((this._isBrandNew == false) && (args['retrieveKeyFunction'] == null)) {
		Clipperz.Base.exception.raise('MandatoryParameter');
	} else {
		this._retrieveKeyFunction = args['retrieveKeyFunction'];
	}

	this._retrieveRemoteDataFunction	= args.retrieveRemoteDataFunction	|| null;
	this._remoteData					= args.remoteData					|| null;
//	this._remoteData					= args.remoteData ? Clipperz.Base.deepClone(args.remoteData) : null;
	if ((!this._isBrandNew) && ((this._retrieveRemoteDataFunction == null) && (this._remoteData == null))) {
		Clipperz.Base.exception.raise('MandatoryParameter');
	}


	this._encryptedDataKeypath			= args.encryptedDataKeypath			|| 'data';		//Clipperz.Base.exception.raise('MandatoryParameter');
	this._encryptedVersionKeypath		= args.encryptedVersionKeypath		|| 'version';	//Clipperz.Base.exception.raise('MandatoryParameter');

	
	this._transientState = null;
	this._deferredLocks = {};

	if (this._isBrandNew == true) {
		this._objectDataStore = new Clipperz.KeyValueObjectStore(/*{'name':'EncryptedRemoteObject.objectDataStore [1]'}*/);
	} else {
		this._objectDataStore = null;
	}

	return this;
}

//
//	  Basic data workflow
//	=======================
//
//	getRemoteData
//		unpackRemoteData
//			getDecryptData [encryptedDataKeypath, encryptedVersionKeypath]
//				unpackData
//				
//				
//	??			packData
//	??		encryptDataWithKey
//	??	packRemoteData [encryptedDataKeypath (?), encryptedVersionKeypath (?)]
//

Clipperz.PM.DataModel.EncryptedRemoteObject.prototype = MochiKit.Base.update(null, {

	'toString': function () {
		return "Clipperz.PM.DataModel.EncryptedRemoteObject" + (this.name() != null ? " - " + this.name() : "");
	},

	//-------------------------------------------------------------------------

	'name': function () {
		return this._name;
	},

	//-------------------------------------------------------------------------

	'reference': function () {
		return this._reference;
	},

	'setReference': function (aValue) {
		this._reference = aValue;

		return this._reference;
	},

	//-------------------------------------------------------------------------

	'transientState': function () {
		if (this._transientState == null) {
			this._transientState = new Clipperz.KeyValueObjectStore(/*{'name':'EncryptedRemoteObject.transientState [2]'}*/);
		}
		
		return this._transientState;
	},

	'resetTransientState': function (isCommitting) {
		if (this._transientState != null) {
			this._transientState.removeAllData();
		}

		this._transientState = null;
	},

	//-------------------------------------------------------------------------

	'isBrandNew': function () {
		return this._isBrandNew;
	},

	//-------------------------------------------------------------------------

	'getKey': function () {
		var deferredResult;
		var deferredLock;

		deferredLock = this.getDeferredLockForKey('key');

		deferredResult = new Clipperz.Async.Deferred("EncryptedRemoteObject.getKey", {trace:false});
		deferredResult.acquireLock(deferredLock);
		deferredResult.addMethod(
			this.decryptedDataStore(),
			'deferredGetOrSet',
			'decryptionKey',
			MochiKit.Base.partial(this.retrieveKeyFunction(), this.reference())
		);
		deferredResult.releaseLock(deferredLock);
		deferredResult.callback();
		
		return deferredResult;
	},

	// . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

	'retrieveKeyFunction': function () {
		return this._retrieveKeyFunction;
	},

	'setRetrieveKeyFunction': function (aFunction) {
		this._retrieveKeyFunction = aFunction;
	},

	//-------------------------------------------------------------------------

	'hasLoadedRemoteData': function () {
		return (this._remoteData != null);
	},

	'getRemoteData': function () {
		var deferredResult;
		var	deferredLock;
		
		deferredLock = this.getDeferredLockForKey('remoteData');

		deferredResult = new Clipperz.Async.Deferred("EncryptedRemoteObjects.getRemoteData", {trace:false});
		deferredResult.acquireLock(deferredLock);
		deferredResult.addCallback(MochiKit.Base.bind(function () {
			var innerDeferredResult;
			
			if (this._remoteData != null) {
				innerDeferredResult = MochiKit.Async.succeed(this._remoteData);
			} else {
				innerDeferredResult = Clipperz.Async.callbacks("EncryptedRemoteObjects.getRemoteData <inner deferred>", [
					MochiKit.Base.partial(this.retrieveRemoteDataFunction(), this.reference()),
					MochiKit.Base.method(this, 'unpackRemoteData'),
					MochiKit.Base.bind(function (someData) {
						this._remoteData = someData;
						return this._remoteData;
					}, this)
				], {trace:false});
			}

			return innerDeferredResult;
		}, this))
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');
		deferredResult.releaseLock(deferredLock);
		
		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'unpackRemoteData': function (someData) {
		return MochiKit.Async.succeed(someData);
	},

	//.........................................................................

	'packRemoteData': function (someData) {
		var result;
		
		result = {
			'reference':	this.reference(),
			'data':			someData,
			'version':		Clipperz.PM.Crypto.encryptingFunctions.currentVersion
		};

		return MochiKit.Async.succeed(result);
	},

	//-------------------------------------------------------------------------

	'retrieveRemoteDataFunction': function () {
		return this._retrieveRemoteDataFunction;
	},
	
	'setRetrieveRemoteDataFunction': function (aFunction) {
		this._retrieveRemoteDataFunction = aFunction;
	},

	//-------------------------------------------------------------------------

	'decryptedDataStore': function () {
		if (this._decryptedDataStore == null) {
			this._decryptedDataStore = new Clipperz.KeyValueObjectStore(/*{'name':'EncryptedRemoteObject.decryptedDataStore [3]'}*/);
		};
		
		return this._decryptedDataStore;
	},

	//.........................................................................

	'getDecryptedData': function () {
		var deferredResult;
		var deferredLock;
		
		deferredLock = this.getDeferredLockForKey('decryptedData');

		deferredResult = new Clipperz.Async.Deferred("EncryptedRemoteObject.getDecryptedData", {trace:false});
		deferredResult.acquireLock(deferredLock);
		deferredResult.addMethod(this, 'decryptedDataStore');
		deferredResult.addCallback(MochiKit.Base.methodcaller('deferredGetOrSet', 'decryptedData', MochiKit.Base.bind(function () {
			var	innerDeferredResult;

			innerDeferredResult = new Clipperz.Async.Deferred("EncryptedRemoteObject.getDecryptedData <inner deferred>", {trace:false});

			innerDeferredResult.addMethod(this, 'getRemoteData');
			innerDeferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');
			innerDeferredResult.collectResults({
				'key':		MochiKit.Base.method(this, 'getKey'),
				'value':	MochiKit.Base.itemgetter(this._encryptedDataKeypath),
				'version':	MochiKit.Base.itemgetter(this._encryptedVersionKeypath)
			});

			innerDeferredResult.addCallback(Clipperz.PM.Crypto.deferredDecrypt);
			innerDeferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');
			innerDeferredResult.addMethod(this, 'unpackData');
			innerDeferredResult.callback();
		
			return innerDeferredResult;
		}, this)));
		deferredResult.releaseLock(deferredLock);
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');
		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'setValue': function(aKey, aValue) {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("EncryptedRemoteObject.setValue", {trace:false});
		deferredResult.addMethod(this, '_getObjectDataStore');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setValue', aKey, aValue));
		deferredResult.callback();
		
		return deferredResult;
	},

	//.........................................................................

	'getValue': function (aKey) {
		return Clipperz.Async.callbacks("EncryptedRemoteObject.getValue", [
			MochiKit.Base.method(this, '_getObjectDataStore'),
			MochiKit.Base.methodcaller('getValue', aKey)
		], {trace:false});
	},

	//.........................................................................

	'removeValue': function (aKey) {
		return Clipperz.Async.callbacks("EncryptedRemoteObject.removeValue", [
			MochiKit.Base.method(this, '_getObjectDataStore'),
			MochiKit.Base.methodcaller('removeValue', aKey)
		], {trace:false});
	},
	
	//.........................................................................

	'values': function () {
		return Clipperz.Async.callbacks("EncryptedRemoteObject.values", [
			MochiKit.Base.method(this, '_getObjectDataStore'),
			MochiKit.Base.methodcaller('values')
		], {trace:false});
	},

	'setValues': function (someValues) {
		return Clipperz.Async.callbacks("EncryptedRemoteObject.values", [
			MochiKit.Base.method(this, '_getObjectDataStore'),
			MochiKit.Base.methodcaller('setValues', someValues)
		], {trace:false});
	},
	
	//.........................................................................

	'_getObjectDataStore': function () {
		var deferredResult;
		var deferredLock;
		
		deferredLock = this.getDeferredLockForKey('objectDataStore');
		
		deferredResult = new Clipperz.Async.Deferred("EncryptedRemoteObject._getObjectDataStore", {trace:false});
		deferredResult.acquireLock(deferredLock);
		deferredResult.addCallback(MochiKit.Base.bind(function () {
			var innerDeferredResult;

			if (this._objectDataStore == null) {
				this._objectDataStore = new Clipperz.KeyValueObjectStore(/*{'name':'EncryptedRemoteObject.objectDataStore [4]'}*/);

				innerDeferredResult = new Clipperz.Async.Deferred("EncryptedRemoteObject._getObjectDataStore <inner deferred>", {trace:false});
				innerDeferredResult.addMethod(this, 'getDecryptedData');
				innerDeferredResult.addMethod(this._objectDataStore, 'initWithValues');
				innerDeferredResult.callback();
			} else {
				innerDeferredResult = MochiKit.Async.succeed(this._objectDataStore);
			}

			return innerDeferredResult;
		}, this));
		deferredResult.releaseLock(deferredLock);
		deferredResult.callback();
		
		return deferredResult;
	},
	
	'hasInitiatedObjectDataStore': function () {
		return (this._objectDataStore != null);
	},

	//-------------------------------------------------------------------------

	'getDeferredLockForKey': function (aKey) {
		var result;
		
		result = this._deferredLocks[aKey];
		
		if (typeof(result) == 'undefined') {
			result = new MochiKit.Async.DeferredLock();
			this._deferredLocks[aKey] = result;
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'unpackData': function (someData) {	//	++
		return someData;
	},
	
	'packData': function (someData) {	//	++
		return someData;
	},

	//-------------------------------------------------------------------------
	
	'hasPendingChanges': function () {
		var deferredResult;
		var tempObj = this;

		if (this.isBrandNew()) {
//			deferredResult = MochiKit.Async.succeed(true);
			deferredResult = this.hasPendingChangesWhenBrandNew();
		} else if (this.hasInitiatedObjectDataStore()) {
			deferredResult = new Clipperz.Async.Deferred("EncryptedRemoteObject.hasPendingChanges", {trace:false});
			deferredResult.collectResults({
				'decryptedData': [
					MochiKit.Base.method(this, 'getDecryptedData'),
					Clipperz.Base.serializeJSON
				],
				'objectData': [
					MochiKit.Base.method(this, '_getObjectDataStore'),
					MochiKit.Base.methodcaller('values'),
					Clipperz.Base.serializeJSON
				]
			});
			deferredResult.addCallback(function (someValues) {
				return (someValues['decryptedData'] != someValues['objectData']);
			});
			deferredResult.callback();
		} else {
			deferredResult = MochiKit.Async.succeed(false);
		}

		return deferredResult;
	},

	'hasPendingChangesWhenBrandNew': function () {
		return MochiKit.Async.succeed(true);
	},

	//-------------------------------------------------------------------------

	'commitTransientState': function () {
		var deferredResult;

//		if (this.transientState().getValue('__prepareRemoteData') == true) {
		if (this.transientState().getValue('packedRemoteData') != null) {
			deferredResult = Clipperz.Async.callbacks("EncryptedRemoteObject.commitTransientState - prepareRemoteData", [
				MochiKit.Base.bind(function (someData) {
					this._remoteData = this.transientState().getValue('packedRemoteData');
				}, this),

				MochiKit.Base.method(this, '_getObjectDataStore'),
				MochiKit.Base.methodcaller('values'),
				Clipperz.Base.deepClone,
				MochiKit.Base.method(this.decryptedDataStore(), 'setValue', 'decryptedData'),

				MochiKit.Base.method(this, 'resetTransientState', true)
			], {trace:false});

		} else {
			deferredResult = Clipperz.Async.callbacks("EncryptedRemoteObject.commitTransientState - NO prepareRemoteData", [
				MochiKit.Base.method(this, 'resetTransientState', true)
			], {trace:false});
		}
		
		this._isBrandNew = false;

		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	'revertChanges': function () {
		if (this.hasInitiatedObjectDataStore()) {
			this._objectDataStore.removeAllData();
			this._objectDataStore = null;
		}
		this.resetTransientState(false);

		return MochiKit.Async.succeed();
	},

	//-------------------------------------------------------------------------

	'deleteAllCleanTextData': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("EncryptedRemoteObject.deleteAllCleanTextData", {trace:false});

		deferredResult.addMethod(this, 'resetTransientState', false);

		deferredResult.acquireLock(this.getDeferredLockForKey('decryptedData'));
		deferredResult.addCallback(MochiKit.Base.bind(function () {
			if (this._decryptedDataStore != null) {
				this._decryptedDataStore.removeAllData();
			}
		}, this));
		deferredResult.releaseLock(this.getDeferredLockForKey('decryptedData'));
		
		deferredResult.acquireLock(this.getDeferredLockForKey('objectDataStore'));
		deferredResult.addCallback(MochiKit.Base.bind(function () {
			if (this._objectDataStore != null) {
				this._objectDataStore.removeAllData();
				this._objectDataStore = null;
			}
		}, this));
		deferredResult.releaseLock(this.getDeferredLockForKey('objectDataStore'));

		deferredResult.callback();
		
		return deferredResult;
	},

	//.........................................................................

	'hasAnyCleanTextData': function () {
		var result;
		
		result = false;
		
		result = result || (! this.decryptedDataStore().isEmpty());
		result = result || (! this.transientState().isEmpty());
		if (this.hasInitiatedObjectDataStore()) {
			result = result || (! this._objectDataStore.isEmpty());
		}

		return MochiKit.Async.succeed(result);
	},

	//-------------------------------------------------------------------------

	'prepareRemoteDataWithKey': function (aKey) {
		return Clipperz.Async.callbacks("EncryptedRemoteObject.prepareRemoteDataWithKey", [
//			MochiKit.Base.method(this.transientState(), 'setValue', '__prepareRemoteData', true),
			MochiKit.Base.method(this, '_getObjectDataStore'),
			MochiKit.Base.methodcaller('values'),
			MochiKit.Base.method(this, 'packData'),
			function (someData) {
				return Clipperz.PM.Crypto.deferredEncrypt({
					'key':		aKey,
					'value':	someData,
					'version':	Clipperz.PM.Crypto.encryptingFunctions.currentVersion
				})
			},
			MochiKit.Base.method(this, 'packRemoteData'),
			MochiKit.Base.method(this.transientState(), 'setValue', 'packedRemoteData'),
			function (someData) {
				MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'advanceProgress');
				return someData;
			}
		], {trace:false});
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
