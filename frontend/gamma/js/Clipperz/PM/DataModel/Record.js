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


Clipperz.PM.DataModel.Record = function(args) {
	Clipperz.PM.DataModel.Record.superclass.constructor.apply(this, arguments);

	this._updateDate				= (args.updateDate ? Clipperz.PM.Date.parse(args.updateDate) : Clipperz.Base.exception.raise('MandatoryParameter'));

	this._retrieveIndexDataFunction	= args.retrieveIndexDataFunction	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._updateIndexDataFunction	= args.updateIndexDataFunction		|| Clipperz.Base.exception.raise('MandatoryParameter');

	this._retrieveDirectLoginIndexDataFunction	= args.retrieveDirectLoginIndexDataFunction	|| null;
	this._setDirectLoginIndexDataFunction		= args.setDirectLoginIndexDataFunction		|| null;
	this._removeDirectLoginIndexDataFunction	= args.removeDirectLoginIndexDataFunction	|| null;

	this._createNewDirectLoginFunction			= args.createNewDirectLoginFunction			|| null;
	
	this._directLogins = {};

	this._versions = {};

	this._currentRecordVersion = null;
	if (this.isBrandNew()) {
		var newVersion;

		this.setNotes('');
		newVersion = new Clipperz.PM.DataModel.Record.Version({
			'retrieveKeyFunction':	MochiKit.Base.method(this, 'getVersionKey'),
			'getVersion':			MochiKit.Base.method(this, 'getVersion')

		});
		this._versions[newVersion.reference()] = newVersion;
		this._currentVersionReference = newVersion.reference();
//		this.setLabel('');
	}

	return this;
}


Clipperz.Base.extend(Clipperz.PM.DataModel.Record, Clipperz.PM.DataModel.EncryptedRemoteObject, {

	'toString': function() {
		return "Record (" + this.reference() + ")";
	},

	//-------------------------------------------------------------------------

	'reference': function () {
		return this._reference;
	},
	
	//=========================================================================

	'getIndexData': function () {
		return this._retrieveIndexDataFunction(this.reference());
	},

	//.........................................................................

	'getIndexDataForKey': function (aKey) {
		return Clipperz.Async.callbacks("Record.getIndexDataForKey", [
			MochiKit.Base.method(this, 'getIndexData'),
			MochiKit.Base.itemgetter(aKey)
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'setIndexDataForKey': function (aKey, aValue) {
//		return this._updateIndexDataFunction(this.reference(), aKey, aValue);
		
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("Record.setIndexDataForKey", {trace:false});
		deferredResult.addMethod(this, 'getIndexDataForKey', aKey);
		deferredResult.addCallback(MochiKit.Base.bind(function (aCurrentValue) {
			var result;
			var originalValue;
			
			originalValue = this.transientState().getValue('originalValues.indexData.' + aKey);
			if (originalValue == null) {
				originalValue = this.transientState().setValue('originalValues.indexData.' + aKey, aCurrentValue);
			}

			if (aCurrentValue != aValue) {
				if (originalValue != aValue) {
					this.transientState().setValue('hasPendingChanges.indexData.' + aKey, true);
				} else {
					this.transientState().setValue('hasPendingChanges.indexData.' + aKey, false);
				}

				result = this._updateIndexDataFunction(this.reference(), aKey, aValue);
			} else {
				result = MochiKit.Async.succeed(aValue);
			}

			return result;
		}, this));
		
		deferredResult.callback();
		
		return deferredResult;
	},

	//=========================================================================
/*
	'key': function () {
		return this.getIndexDataForKey('key');
	},
*/
	//=========================================================================

	'label': function () {
		return this.getIndexDataForKey('label');
	},

	//.........................................................................

	'setLabel': function (aValue) {
		return this.setIndexDataForKey('label', aValue);
	},

	//=========================================================================

	'headerNotes': function () {
		return this.getIndexDataForKey('notes');
	},

	//-------------------------------------------------------------------------

	'notes': function () {
		return Clipperz.Async.callbacks("Record.notes", [
			MochiKit.Base.method(this, 'headerNotes'),
			MochiKit.Base.bind(function (someHeaderNotes) {
				var result;

				if ((someHeaderNotes == null) || (typeof(someHeaderNotes) == 'undefined')) {
					result = this.getValue('notes');
				} else {
					result = MochiKit.Async.succeed(someHeaderNotes);
				}
				
				return result;
			}, this)
		], {trace:false});
	},

	//.........................................................................

	'setNotes': function (aValue) {
		return this.setValue('notes', aValue);
	},

	//=========================================================================

	'updateDate': function () {
		return MochiKit.Async.succeed(this._updateDate);
	},

	//=========================================================================

	'favicon': function () {
		var result;
		var directLogins;

		directLogins = MochiKit.Base.values(this.directLogins());
		if (directLogins.length > 0) {
			result = directLogins[0].favicon();
//		} else if (/* is there an URL to use for searching a favicon */){
		} else {
			result = null; //	MochiKit.Async.succeed(Clipperz.PM.Strings['defaultFaviconUrl']);
		}

		return result;
	},

	//-------------------------------------------------------------------------

	'searchableContent': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("Record.searchableContent", {trace:false});
		
		deferredResult.collectResults({
			'recordLabel': MochiKit.Base.method(this, 'label'),
			'directLoginLabels': [
				MochiKit.Base.method(this, 'directLoginReferences'),
				MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.itemgetter('label'))
			]
		})
		deferredResult.addCallback(function (someValues) {
			return someValues['recordLabel'] + ' ' + someValues['directLoginLabels'].join(' ');
		});
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'isMatching': function (aRegExp) {
		return Clipperz.Async.callbacks("deferredFilterFunction", [
			MochiKit.Base.method(this, 'searchableContent'),
			MochiKit.Base.method(aRegExp, 'test'),
			function (doesItMatch) {
				var result;
				
				if (doesItMatch) {
					result = MochiKit.Async.succeed('match');
				} else {
					result = MochiKit.Async.fail('miss');
				}
				
				return result;
			}
		], {trace:false});
	},

	//=========================================================================

	'directLogins': function () {
		return this._directLogins;
	},

	'addDirectLogin': function (aDirectLogin) {
		this._directLogins[aDirectLogin.reference()] = aDirectLogin;
	},

	'directLoginWithReference': function (aDirectLoginReference) {
		return this._directLogins[aDirectLoginReference];
	},

	'createNewDirectLoginFunction': function () {
		return this._createNewDirectLoginFunction;
	},

	'saveOriginalDirectLoginStatusToTransientState': function () {
		if (this.transientState().getValue('directLogins') == null) {
//			this.transientState().setValue('directLogins', this._directLogins)
			MochiKit.Iter.forEach(MochiKit.Base.keys(this._directLogins), MochiKit.Base.bind(function(aKey) {
				this.transientState().setValue('directLogins' + '.' + aKey, this._directLogins[aKey])
			}, this))
		}
	},
	
	'createNewDirectLogin': function () {
		this.saveOriginalDirectLoginStatusToTransientState();

		return this.createNewDirectLoginFunction()(this);
	},

	'removeDirectLogin': function(aDirectLogin) {
		this.saveOriginalDirectLoginStatusToTransientState();

		return Clipperz.Async.callbacks("Record.removeDirectLogin", [
			MochiKit.Base.method(this, 'removeValue', 'directLogins' + '.' + aDirectLogin.reference()),
			MochiKit.Base.bind(function () {
				delete this._directLogins[aDirectLogin.reference()]
			}, this)
		], {trace:false});
		
	},

	'directLoginReferences': function () {
		var result;

		result = Clipperz.Async.callbacks("Record.directLoginReferences", [
			MochiKit.Base.method(this, 'directLogins'),
			MochiKit.Base.values,
			function (someDirectLogins) {
				var result;
				var i,c;
				
				result = [];
				c = someDirectLogins.length;
				for (i=0; i<c; i++) {
					result.push(Clipperz.Async.collectResults("Record.directLoginReferences - collectResults", {
						'_rowObject': MochiKit.Async.succeed,
						'_reference': MochiKit.Base.methodcaller('reference'),
						'label': MochiKit.Base.methodcaller('label'),
						'favicon': MochiKit.Base.methodcaller('favicon')
					}, {trace:false})(someDirectLogins[i]));
				};
				
				return result;
			},
			Clipperz.Async.collectAll
		], {trace:false});
		
		return result;
	},

	//=========================================================================

	'unpackRemoteData': function (someData) {
		var result;

/*
		this._currentRecordVersion = new Clipperz.PM.DataModel.Record.Version({
			'reference':				someData['currentVersion']['reference'],
			'retrieveKeyFunction':		MochiKit.Base.method(this, 'getCurrentRecordVersionKey'),
			'remoteData':				someData['currentVersion'],
		});
*/
		var versionKey;

		for (versionKey in someData['versions']) {
			this._versions[versionKey] = new Clipperz.PM.DataModel.Record.Version({
				'reference':			versionKey,
				'retrieveKeyFunction':	MochiKit.Base.method(this, 'getVersionKey'),
				'remoteData':			someData['versions'][versionKey],
				'getVersion':			MochiKit.Base.method(this, 'getVersion')
			})
		}
		
//		this._currentVersionReference = someData['currentVersion']['reference'];
		this._currentVersionReference = someData['currentVersion'];

		result = Clipperz.PM.DataModel.Record.superclass.unpackRemoteData.apply(this, arguments);

		return result;
	},
	
	//-------------------------------------------------------------------------

	'unpackData': function (someData) {
		var result;

		result = Clipperz.PM.DataModel.Record.superclass.unpackData.apply(this, arguments);

		if (MochiKit.Base.isUndefinedOrNull(result['notes'])) {
			result['notes'] = ''
		}
				
		return result;
	},
	
	//-------------------------------------------------------------------------

	'prepareRemoteDataWithKey': function (aKey) {
		var deferredResult;
		var	newVersionKey;
		var result;

		newVersionKey = Clipperz.PM.Crypto.randomKey();
		result = {};

		deferredResult = new Clipperz.Async.Deferred("Record.prepareRemoteDataWithKey", {trace:false});
		deferredResult.addCallbackList([
			Clipperz.Async.collectResults("Record.prepareRemoteDataWithKey - collect results", {
				'isBrandNew': MochiKit.Base.method(this, 'isBrandNew'),
				'versionHasPendingChanges':	[
//					MochiKit.Base.method(this, 'getCurrentRecordVersion'),
//					MochiKit.Base.methodcaller('hasPendingChanges')
					MochiKit.Base.method(this, 'invokeCurrentRecordVersionMethod', 'hasPendingChanges')
				]
			}),
			Clipperz.Async.or,

			Clipperz.Async.deferredIf("Current Version has pending changes", [
				MochiKit.Base.method(this, 'createNewRecordVersion'),
				MochiKit.Base.methodcaller('prepareRemoteDataWithKey', newVersionKey),
				MochiKit.Base.partial(Clipperz.Async.setItem, result, 'currentRecordVersion'),
				MochiKit.Base.method(this, 'setCurrentRecordVersionKey', newVersionKey)
			], []),
		
			MochiKit.Base.bind(Clipperz.PM.DataModel.Record.superclass.prepareRemoteDataWithKey, this, aKey),
			MochiKit.Base.partial(Clipperz.Async.setItem, result, 'record'),

			MochiKit.Base.partial(MochiKit.Async.succeed, result)
		]);
		
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'fields': function () {
		return this.invokeCurrentRecordVersionMethod('fields');
	},

	'addField': function (someParameters) {
		return this.invokeCurrentRecordVersionMethod('addField', someParameters);
	},

	'removeField': function (someParameters) {
		return this.invokeCurrentRecordVersionMethod('removeField', someParameters);
	},

//	'sortFieldReference': function (someSortedFieldReferences) {
//		return this.invokeCurrentRecordVersionMethod('sortFieldReference', someSortedFieldReferences);
//	},

	'getFieldsValues': function () {
		return this.invokeCurrentRecordVersionMethod('getFieldsValues');
	},

	'fieldWithLabel': function (aLabel) {
		return Clipperz.Async.callbacks("Record.fieldWithLabel", [
			MochiKit.Base.method(this, 'fields'),
			MochiKit.Base.values,
			MochiKit.Base.partial(Clipperz.Async.deferredFilter, function (aField) {
				return Clipperz.Async.callbacks("Record.fieldWithLabel - check field label", [
					MochiKit.Base.methodcaller('label'),
					MochiKit.Base.partial(MochiKit.Base.operator.eq, aLabel)
				], {trace:false}, aField);
			}),
			function (someFilteredResults) {
				var result;

				switch (someFilteredResults.length) {
					case 0:
						result = null;
						break;
					case 1:
						result = someFilteredResults[0];
						break;
					default:
						WTF = TODO;
						break;
				}
				
				return result;
			}
		], {trace:false});
	},

	//=========================================================================

	'getVersion': function (aVersionReference) {
		return Clipperz.Async.callbacks("Record.getVersion", [
			MochiKit.Base.method(this, 'getVersions'),
			MochiKit.Base.itemgetter(aVersionReference)
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'getVersionKey': function (aVersionReference) {
		var	deferredResult;
		var transientStateKey;

		transientStateKey = 'versionKeys' + '.' + aVersionReference;
		if (this.transientState().getValue(transientStateKey) != null) {
			deferredResult = MochiKit.Async.succeed(this.transientState().getValue(transientStateKey));
		} else {
			deferredResult = Clipperz.Async.callbacks("Record.getVersionKey", [
				MochiKit.Base.method(this, 'getVersions'),
				MochiKit.Base.partial(MochiKit.Base.operator.eq, aVersionReference, this.currentVersionReference()),
				Clipperz.Async.deferredIf("getVersionKey for current version", [
					MochiKit.Base.method(this, 'getCurrentRecordVersionKey'),
					MochiKit.Base.method(this.transientState(), 'setValue', transientStateKey)
				],[
					MochiKit.Async.fail
				])
			], {trace:false});
		}
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'versions': function () {
		return this._versions;
	},

	'getVersions': function () {
		return Clipperz.Async.callbacks("Record.versions", [
			MochiKit.Base.method(this, 'getValue', 'fakeKey, just to trigger unpackRemoteData'),
			MochiKit.Base.bind(function () { return this._versions; }, this)
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'getCurrentRecordVersion': function () {
		return Clipperz.Async.callbacks("Record.getCurrentRecordVersion", [
//			MochiKit.Base.method(this, 'getValue', 'fakeKey, just to trigger unpackRemoteData'),
//			MochiKit.Base.bind(function () { return this._currentRecordVersion; }, this)

			MochiKit.Base.method(this, 'versions'),
			MochiKit.Base.itemgetter(this.currentVersionReference()),
			Clipperz.Async.deferredIf("The current version is available", [
				MochiKit.Async.succeed
			], [
				MochiKit.Base.method(this, 'getVersions'),
				MochiKit.Base.bind(function (someVersions) { return someVersions[this.currentVersionReference()]}, this)
			])
		], {trace:false});
	},
	
	'setCurrentRecordVersion': function (aRecordVersion) {
		this._currentVersionReference = aRecordVersion.reference();
	},

	//.........................................................................

	'currentVersionReference': function () {
		return this._currentVersionReference;
	},

	//-------------------------------------------------------------------------

	'createNewRecordVersion': function () {
		var deferredResult;
		
		if (this.isBrandNew()) {
			deferredResult = this.getCurrentRecordVersion();
		} else {
			var newVersion;
			
			newVersion = new Clipperz.PM.DataModel.Record.Version({
			//	'reference':			versionKey,
				'retrieveKeyFunction':	MochiKit.Base.method(this, 'getVersionKey'),
//				'remoteData':			{},
				'getVersion':			MochiKit.Base.method(this, 'getVersion')
			})
			this._versions[newVersion.reference()] = newVersion;

			deferredResult = Clipperz.Async.callbacks("Record.createNewRecordVersion", [
//				MochiKit.Base.method(this, 'getCurrentRecordVersion'),
//				MochiKit.Base.methodcaller('values'),
				MochiKit.Base.method(this, 'invokeCurrentRecordVersionMethod', 'values'),
				MochiKit.Base.method(newVersion, 'setValues'),

				Clipperz.Async.collectResults("Record.createNewRecordVersion [collect results]", {
					'reference':	MochiKit.Base.method(this, 'currentVersionReference'),
					'key': 			MochiKit.Base.method(this, 'getCurrentRecordVersionKey')
				}, {trace:false}),
				MochiKit.Base.method(newVersion, 'setPreviousVersionReferenceAndKey'),

//				MochiKit.Base.method(this, 'getCurrentRecordVersion'),
//				MochiKit.Base.method(this, 'revertChanges'),
				MochiKit.Base.method(this, 'invokeCurrentRecordVersionMethod', 'revertChanges'),

				MochiKit.Base.method(this, 'setCurrentRecordVersion', newVersion),
				MochiKit.Base.partial(MochiKit.Async.succeed, newVersion)
			], {trace:false});
		}
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'getCurrentRecordVersionKey': function () {
		return Clipperz.Async.callbacks("Record.getCurrentRecordVersionKey", [
			MochiKit.Base.method(this, 'getValue', 'currentVersionKey'),
			Clipperz.Async.deferredIf("currentVersionKey is NOT null", [
				MochiKit.Async.succeed
			], [
				MochiKit.Base.method(this, 'getKey')
			])
		], {trace:false});
	},

	'setCurrentRecordVersionKey': function (aValue) {
		//	TODO: triple check this method!
		return Clipperz.Async.callbacks("Record.setCurrentRecordVersionKey", [
			MochiKit.Base.method(this, 'setValue', 'currentVersionKey', aValue)
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'invokeCurrentRecordVersionMethod': function (aMethodName, someValues) {
		return Clipperz.Async.callbacks("Record.invokeCurrentRecordVersionMethod", [
			MochiKit.Base.method(this, 'getCurrentRecordVersion'),
			MochiKit.Base.methodcaller(aMethodName, someValues)
		], {trace:false});
	},


	'lazilyinvokeCurrentRecordVersionMethod': function (aMethodName, someValues, defaultResult) {
		return Clipperz.Async.callbacks("Record.lazilyinvokeCurrentRecordVersionMethod", [
			MochiKit.Base.method(this, 'currentVersionReference'),
			Clipperz.Async.deferredIf("versions has been loaded", [
				MochiKit.Base.method(this, 'getCurrentRecordVersion'),
				MochiKit.Base.methodcaller(aMethodName, someValues),
			], [
				MochiKit.Base.partial(MochiKit.Async.succeed, defaultResult),
			])
		], {trace:false});
	},

	//=========================================================================

	'hasPendingChanges': function () {
		var deferredResult;

		if (this.hasInitiatedObjectDataStore()) {
			deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.DataModel.Record.hasPendingChanges", {trace:false});
			deferredResult.collectResults({
				'super': MochiKit.Base.bind(Clipperz.PM.DataModel.Record.superclass.hasPendingChanges, this),
				'currentVersion': [
//					MochiKit.Base.method(this, 'getCurrentRecordVersion'),
//					MochiKit.Base.methodcaller('hasPendingChanges')
					MochiKit.Base.method(this, 'invokeCurrentRecordVersionMethod', 'hasPendingChanges')
				],
				'directLogins': [
					MochiKit.Base.method(this, 'directLogins'),
					MochiKit.Base.values,
					MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('hasPendingChanges')),
					Clipperz.Async.collectAll,
					Clipperz.Async.or
//					function(someValues) {
//						return MochiKit.Iter.some(someValues, MochiKit.Base.operator.identity);
//					}
				]
			});
			deferredResult.addCallback(MochiKit.Base.values);
			deferredResult.addCallback(MochiKit.Base.bind(function(someValues) {
				var result;
				result = MochiKit.Iter.some(someValues, MochiKit.Base.operator.identity);

				if ((result == false) && (this.isBrandNew() == false)) {
					result = MochiKit.Iter.some(MochiKit.Base.values(this.transientState().getValue('hasPendingChanges.indexData')), MochiKit.Base.operator.identity);
				}
		
				return result;
			}, this));

			deferredResult.callback();
		} else {
			deferredResult = Clipperz.Async.callbacks("Recrod.hasPendingChanges [hasInitiatedObjectDataStore == false]", [
				MochiKit.Base.method(this, 'directLogins'),
				MochiKit.Base.values,
				MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('hasPendingChanges')),
				Clipperz.Async.collectAll,
				Clipperz.Async.or
//				function(someValues) {
//					return MochiKit.Iter.some(someValues, MochiKit.Base.operator.identity);
//				}
			], {trace:false})
		}

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'hasPendingChangesWhenBrandNew': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.DataModel.Record.hasPendingChangesWhenBrandNew", {trace:false});
		deferredResult.collectResults({
			'label': [
				MochiKit.Base.method(this, 'label'),
				MochiKit.Base.partial(MochiKit.Base.operator.ne, '')
			],
			'notes': [
				MochiKit.Base.method(this, 'notes'),
				MochiKit.Base.partial(MochiKit.Base.operator.ne, '')
			]
		});
//		deferredResult.addCallback(MochiKit.Base.values);
//		deferredResult.addCallback(function(someValues) {
//			return MochiKit.Iter.some(someValues, MochiKit.Base.operator.identity);
//		});
		deferredResult.addCallback(Clipperz.Async.or);

		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'isBrandNewWithNoPendingChanges': function () {
		var	deferredResult;

		if (this.isBrandNew() == false) {
			deferredResult = MochiKit.Async.succeed(false);
		} else {
			deferredResult = Clipperz.Async.callbacks("Record.isBrandNewWithNoPendingChanges", [
				MochiKit.Base.method(this, 'hasPendingChanges'),
				MochiKit.Base.operator.lognot
			], {trace:false});
		}
		
		return deferredResult;
	},

	//=========================================================================

	'revertChanges': function () {
		var deferredResult;

		if (this.isBrandNew() == false) {
			deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.DataModel.Record.revertChanges", {trace:false});
			deferredResult.addMethod(this, 'hasPendingChanges');
			deferredResult.addIf([
//				MochiKit.Base.method(this, 'getCurrentRecordVersion'),
//				MochiKit.Base.methodcaller('revertChanges'),
				MochiKit.Base.method(this,'invokeCurrentRecordVersionMethod', 'revertChanges'),

				MochiKit.Base.method(this, 'directLogins'),
				MochiKit.Base.values,
				MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('revertChanges')),

				MochiKit.Base.bind(Clipperz.PM.DataModel.Record.superclass.revertChanges, this)
			], [
				MochiKit.Async.succeed
			]);
			deferredResult.callback();
		} else {
//			this.deleteAllCleanTextData();
			deferredResult = MochiKit.Async.succeed();
		}

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'resetTransientState': function (isCommitting) {
//		if ((isCommitting == false) && (this.transientState().getValue('directLogins') != null)) {
//			this._directLogins = this.transientState().getValue('directLogins');
//		}

		return Clipperz.Async.callbacks("Record.resetTransientState", [
//-			MochiKit.Base.method(this, 'getCurrentRecordVersion'),
//-			MochiKit.Base.methodcaller('resetTransientState'),
//			MochiKit.Base.method(this, 'invokeCurrentRecordVersionMethod', 'resetTransientState'),
			MochiKit.Base.method(this, 'lazilyinvokeCurrentRecordVersionMethod', 'resetTransientState'),

			MochiKit.Base.method(this, 'directLogins'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('resetTransientState')),

			MochiKit.Base.bind(function () {
				if ((isCommitting == false) && (this.transientState().getValue('directLogins') != null)) {
					this._directLogins = this.transientState().getValue('directLogins');
				}
			}, this),

			MochiKit.Base.bind(Clipperz.PM.DataModel.Record.superclass.resetTransientState, this, isCommitting)
		], {trace:false})
	},

	//-------------------------------------------------------------------------
	
	'commitTransientState': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.DataModel.Record.commitTransientState", {trace:false});
		deferredResult.addMethod(this, 'hasPendingChanges');
		deferredResult.addIf([
			MochiKit.Base.bind(Clipperz.PM.DataModel.Record.superclass.commitTransientState, this),
//			MochiKit.Base.method(this, 'getCurrentRecordVersion'),
//			MochiKit.Base.methodcaller('commitTransientState'),
			MochiKit.Base.method(this, 'invokeCurrentRecordVersionMethod', 'commitTransientState'),
			MochiKit.Base.method(this, 'directLogins'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('commitTransientState'))
		], [
			MochiKit.Async.succeed
		]);
		deferredResult.callback();
		
		return deferredResult;
	},

	//=========================================================================

	'retrieveDirectLoginIndexDataFunction': function () {
		return this._retrieveDirectLoginIndexDataFunction;
	},
	
	'setDirectLoginIndexDataFunction': function () {
		return this._setDirectLoginIndexDataFunction;
	},
	
	'removeDirectLoginIndexDataFunction': function () {
		return this._removeDirectLoginIndexDataFunction;
	},

	//=========================================================================

	'deleteAllCleanTextData': function () {
//		return Clipperz.PM.DataModel.Record.superclass.deleteAllCleanTextData.apply(this, arguments);

		return Clipperz.Async.callbacks("Record.deleteAllCleanTextData", [
			MochiKit.Base.method(this, 'versions'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('deleteAllCleanTextData')),

			MochiKit.Base.method(this, 'directLogins'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('deleteAllCleanTextData')),

			MochiKit.Base.bind(Clipperz.PM.DataModel.Record.superclass.deleteAllCleanTextData, this)
		], {trace:false});
	},

	'hasAnyCleanTextData': function () {
//		return Clipperz.PM.DataModel.Record.superclass.hasAnyCleanTextData.apply(this, arguments);

		return Clipperz.Async.callbacks("Record.hasAnyCleanTextData", [
			Clipperz.Async.collectResults("Record.hasAnyCleanTextData [collect results]", {
				'versions':	[
					MochiKit.Base.method(this, 'versions'),
					MochiKit.Base.values,
					MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('hasAnyCleanTextData')),
					Clipperz.Async.collectAll
				],
				'directLogins': [
					MochiKit.Base.method(this, 'directLogins'),
					MochiKit.Base.values,
					MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('hasAnyCleanTextData')),
					Clipperz.Async.collectAll
				],
				'super': [
					MochiKit.Base.bind(Clipperz.PM.DataModel.Record.superclass.hasAnyCleanTextData, this)
				]
			}, {trace:false}),
			Clipperz.Async.or
		])
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});


