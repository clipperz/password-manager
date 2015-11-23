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

"use strict";
Clipperz.Base.module('Clipperz.PM.DataModel');

//if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
//if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
//if (typeof(Clipperz.PM.DataModel) == 'undefined') { Clipperz.PM.DataModel = {}; }


Clipperz.PM.DataModel.Record = function(args) {
	Clipperz.PM.DataModel.Record.superclass.constructor.apply(this, arguments);

	this._updateDate				= (args.updateDate ? Clipperz.PM.Date.parse(args.updateDate) : Clipperz.Base.exception.raise('MandatoryParameter'));
	this._accessDate				= (args.accessDate ? Clipperz.PM.Date.parse(args.accessDate) : Clipperz.Base.exception.raise('MandatoryParameter'));

	this._retrieveIndexDataFunction	= args.retrieveIndexDataFunction	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._updateIndexDataFunction	= args.updateIndexDataFunction		|| Clipperz.Base.exception.raise('MandatoryParameter');

	this._retrieveDirectLoginIndexDataFunction	= args.retrieveDirectLoginIndexDataFunction	|| null;
	this._setDirectLoginIndexDataFunction		= args.setDirectLoginIndexDataFunction		|| null;
	this._removeDirectLoginIndexDataFunction	= args.removeDirectLoginIndexDataFunction	|| null;
	this._createNewDirectLoginFunction			= args.createNewDirectLoginFunction			|| null;

	this._retrieveAttachmentIndexDataFunction	= args.retrieveAttachmentIndexDataFunction	|| null;
	this._setAttachmentIndexDataFunction		= args.setAttachmentIndexDataFunction		|| null;
	this._removeAttachmentIndexDataFunction		= args.removeAttachmentIndexDataFunction	|| null;
	this._createNewAttachmentFunction			= args.createNewAttachmentFunction			|| null;
	
	this._attachmentServerStatus = {};

	this._tags = [];
	this._directLogins = {};
	this._attachments = {};
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

//	'reference': function () {
//		return this._reference;
//	},
	
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

	//----------------------------------------------------------------------------

	setAttachmentServerStatus: function(anObject) {
		this._attachmentServerStatus = anObject;
	},

	getAttachmentServerStatus: function() {
		return this._attachmentServerStatus;
	},

	//============================================================================
/*
	'key': function () {
		return this.getIndexDataForKey('key');
	},
*/
	//============================================================================

	'fullLabel': function () {
		return this.getIndexDataForKey('label');
	},
	
	'setFullLabel': function (aLabel, someTags) {
		var fullLabel = MochiKit.Base.extend([aLabel], MochiKit.Base.map(function (aTag) { return Clipperz.PM.DataModel.Record.tagChar + aTag; }, someTags)).join(' ');

		return this.setIndexDataForKey('label', fullLabel);
	},

	'updateTags': function (someTagInfo) {
		return Clipperz.Async.callbacks("Record.updateTags", [
			MochiKit.Base.method(this, 'label'),
			MochiKit.Base.bind(function (aLabel) {
				return this.setFullLabel(aLabel, MochiKit.Base.keys(someTagInfo));
			}, this)
		], {trace:false});
	},

	//............................................................................

	'extractLabelFromFullLabel': function (aValue) {
		return Clipperz.PM.DataModel.Record.extractLabelFromFullLabel(aValue);
	},

	'extractTagsFromFullLabel': function (aLabel) {
		return Clipperz.PM.DataModel.Record.extractTagsFromFullLabel(aLabel);
	},

	//............................................................................
	
	'label': function () {
		return Clipperz.Async.callbacks("Record.label", [
			MochiKit.Base.method(this, 'fullLabel'),
			MochiKit.Base.method(this, 'extractLabelFromFullLabel')
		], {trace:false});
	},

	'setLabel': function (aValue) {
//		var	tags;
		
		return Clipperz.Async.callbacks("Record.setLabel", [
			MochiKit.Base.method(this, 'tags'),
//			function (someValues) { console.log("TAGS", someValues); tags = someValues; },
//			MochiKit.Base.method(this, 'setIndexDataForKey', 'label', aValue),
//			MochiKit.Base.method(this, 'updateFullLabelWithTags', tags),
			MochiKit.Base.method(this, 'setFullLabel', aValue),
			MochiKit.Base.method(this, 'label'),
		], {trace:false});
		
//		return this.setIndexDataForKey('label', aValue);	//	[???]
	},

	//.........................................................................

	'tags': function () {
		return Clipperz.Async.callbacks("Record.label", [
			MochiKit.Base.method(this, 'fullLabel'),
			MochiKit.Base.method(this, 'extractTagsFromFullLabel'),
			MochiKit.Base.keys
		], {trace:false});
	},

	'addTag': function (aNewTag) {
//		var	tag = aNewTag.replace(/\s/g, Clipperz.PM.DataModel.Record.tagSpace);
//console.log("TAG", aNewTag, tag);
		return Clipperz.Async.callbacks("Record.addTag", [
			MochiKit.Base.method(this, 'fullLabel'),
			MochiKit.Base.method(this, 'extractTagsFromFullLabel'),
			function (someTags) { someTags[aNewTag] = true; return someTags; },
			MochiKit.Base.method(this, 'updateTags')
		], {trace:false});
	},

	'removeTag': function (aTag) {
//console.log("ADD TAG", aNewTag);
		return Clipperz.Async.callbacks("Record.removeTag", [
			MochiKit.Base.method(this, 'fullLabel'),
			MochiKit.Base.method(this, 'extractTagsFromFullLabel'),
			function (someTags) { delete someTags[aTag]; return someTags; },
			MochiKit.Base.method(this, 'updateTags')
		], {trace:false});
	},

	'toggleArchive': function () {
		return Clipperz.Async.callbacks("Record.toggleArchive", [
			MochiKit.Base.method(this, 'isArchived'),
			Clipperz.Async.deferredIf("Record is archived", [
				MochiKit.Base.method(this, 'removeTag', Clipperz.PM.DataModel.Record.archivedTag)
			], [
				MochiKit.Base.method(this, 'addTag', Clipperz.PM.DataModel.Record.archivedTag)
			]),
		], {trace:false});
	},

	'isArchived': function () {
		return Clipperz.Async.callbacks("Record.isArchived", [
			MochiKit.Base.method(this, 'tags'),
			function (someTags) { return MochiKit.Iter.some(someTags, MochiKit.Base.partial(MochiKit.Base.objEqual, Clipperz.PM.DataModel.Record.archivedTag))},
		], {trace:false});
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

	'accessDate': function () {
		return MochiKit.Async.succeed(this._accessDate);
	},

	//=========================================================================
	
	attachmentsCount: function() {
		return MochiKit.Base.keys(this.attachments()).length;
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
			'recordLabel': MochiKit.Base.method(this, 'fullLabel'),
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

	'content': function () {
		var deferredResult;
		var	result;

		result = {
			'fields': [],
			'directLogins': []
		};

		deferredResult = new Clipperz.Async.Deferred("Record.content", {trace:false});
		deferredResult.addMethod(this, 'reference');
		deferredResult.addCallback(function (aValue) { result['reference'] = aValue; });
		deferredResult.addMethod(this, 'label');
		deferredResult.addCallback(function (aValue) { result['title'] = aValue; });
		deferredResult.addMethod(this, 'notes');
		deferredResult.addCallback(function (aValue) { result['notes'] = aValue; });

		deferredResult.addMethod(this, 'fields');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.methodcaller('content'));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addCallback(MochiKit.Base.map, function (aValue) { result['fields'].push(aValue); });

		deferredResult.addMethod(this, 'directLogins');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.methodcaller('content'));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addCallback(MochiKit.Base.map, function (aValue) { result['directLogins'].push(aValue); });
		deferredResult.addCallback(function () { return result; });

		deferredResult.addMethod(this, 'attachments');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.methodcaller('content'));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addCallback(MochiKit.Base.map, function (aValue) { result['directLogins'].push(aValue); });
		deferredResult.addCallback(function () { return result; });

		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'directLogins': function () {
		return this._directLogins;
	},

	'bindDirectLogin': function (aDirectLogin) {
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
			this.transientState().setValue('directLogins', {});
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
	//	TODO: !!!!
	'attachments': function () {
		return this._attachments;
	},

	'bindAttachment': function (anAttachment) {
		this._attachments[anAttachment.reference()] = anAttachment;
	},

	'attachmentWithReference': function (anAttachmentReference) {
		return this._attachments[anAttachmentReference];
	},

	'createNewAttachmentFunction': function () {
		return this._createNewAttachmentFunction;
	},

	'saveOriginalAttachmentStatusToTransientState': function () {
		if (this.transientState().getValue('attachments') == null) {
// console.log("saving attachments to transient state: ts:", this.transientState());
			this.transientState().setValue('attachments', {});
			MochiKit.Iter.forEach(MochiKit.Base.keys(this._attachments), MochiKit.Base.bind(function(aKey) {
// console.log("saving attachment to transient state:", this._attachments[aKey]);
				this.transientState().setValue('attachments' + '.' + aKey, this._attachments[aKey])
			}, this))
		}
	},
	
	'createNewAttachment': function () {
		this.saveOriginalAttachmentStatusToTransientState();
		return this.createNewAttachmentFunction()(this);
	},

	'removeAttachment': function(anAttachment) {
		this.saveOriginalAttachmentStatusToTransientState();
		return Clipperz.Async.callbacks("Record.removeAttachment", [
			MochiKit.Base.method(this, 'removeValue', 'attachments' + '.' + anAttachment.reference()),
			MochiKit.Base.bind(function () {
				this._removeAttachmentIndexDataFunction(anAttachment.reference());
				delete this._attachments[anAttachment.reference()]
			}, this)
		], {trace:false});
		
	},

	'attachmentReferences': function () {
		var result;

		result = Clipperz.Async.callbacks("Record.attachmentReferences", [
			MochiKit.Base.method(this, 'attachments'),
			MochiKit.Base.values,
			function (someAttachments) {
				var result;
				var i,c;
				
				result = [];
				c = someAttachments.length;
				for (i=0; i<c; i++) {
					result.push(Clipperz.Async.collectResults("Record.attachmentReferences - collectResults", {
						'_rowObject': MochiKit.Async.succeed,
						'_reference': MochiKit.Base.methodcaller('reference'),
//						'label': MochiKit.Base.methodcaller('label'),
//						'favicon': MochiKit.Base.methodcaller('favicon')
					}, {trace:false})(someAttachments[i]));
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

		result['attachments'] = MochiKit.Base.keys(this.attachments());

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
//console.log("Warning: Record.fieldWithLabel('" + aLabel + "') is returning more than one result: " + someFilteredResults.length);
						result = someFilteredResults[0];
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

		if (typeof(aVersionReference) == 'undefined') {
			console.log("ERROR; getVersionKey aVersionReference is undefined");
		}

		transientStateKey = 'versionKeys' + '.' + aVersionReference;
		if (this.transientState().getValue(transientStateKey) != null) {
			deferredResult = MochiKit.Async.succeed(this.transientState().getValue(transientStateKey));
		} else {
			deferredResult = Clipperz.Async.callbacks("Record.getVersionKey", [
//MochiKit.Base.bind(function () {console.log("VERSION REFERENCE", aVersionReference, this.currentVersionReference()); }, this),
				MochiKit.Base.method(this, 'getVersions'),
//function (aValue) { console.log("VERSIONs", aValue); return aValue; },
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
			MochiKit.Base.methodcaller(aMethodName, someValues),
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
		var recordReference = this.reference();
//		var	self = this;

		deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.DataModel.Record.hasPendingChanges", {trace:false});
		deferredResult.collectResults({
			'super': [
				MochiKit.Base.bind(Clipperz.PM.DataModel.Record.superclass.hasPendingChanges, this),
			],	
			'currentVersion': [
				MochiKit.Base.method(this, 'hasInitiatedObjectDataStore'),
				Clipperz.Async.deferredIf("Record.hasPendingChanges - hasInitiatedObjectDataStore", [
					MochiKit.Base.method(this, 'invokeCurrentRecordVersionMethod', 'hasPendingChanges'),
				], [
					MochiKit.Base.partial(MochiKit.Async.succeed, false),
				]),
			],
			'directLogins': [
				MochiKit.Base.method(this, 'directLogins'),
				MochiKit.Base.values,
				MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('hasPendingChanges')),
				Clipperz.Async.collectAll,
				Clipperz.Async.or
			],
			'attachments': [
				MochiKit.Base.method(this, 'attachments'),
				MochiKit.Base.values,
				MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('hasPendingChanges')),
				Clipperz.Async.collectAll,
				Clipperz.Async.or
			]
		}, {trace:true});
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.bind(function (someValues) {
			return MochiKit.Iter.some(someValues, MochiKit.Base.operator.identity);
		}, this));

		deferredResult.callback();

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
// console.log("Revert changes: attachments in transient state", this.transientState().getValue('attachments'));

		var deferredResult;
//		var recordReference = this.reference();
		
		if (this.isBrandNew() == false) {
/* */
			deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.DataModel.Record.revertChanges", {trace:false});
			deferredResult.addMethod(this, 'hasPendingChanges');
//deferredResult.addCallback(function (aValue) { 
//	if (recordReference == 'd620764a656bfd4e1d3758500d5db72e460a0cf729d56ed1a7755b5725c50045') {
//		console.log("Record.revertChanges - hasPendingChanges", aValue);
//	}
//	return aValue;
//	return true;
//});
			deferredResult.addIf([
				MochiKit.Base.method(this, 'cancelRevertedAttachmentsUpload'),

				MochiKit.Base.method(this, 'invokeCurrentRecordVersionMethod', 'revertChanges'),
				
				MochiKit.Base.method(this, 'directLogins'),
				MochiKit.Base.values,
				MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('revertChanges')),
				
				MochiKit.Base.method(this, 'attachments'),
				MochiKit.Base.values,
				MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('revertChanges')),

				MochiKit.Base.bind(Clipperz.PM.DataModel.Record.superclass.revertChanges, this),
				MochiKit.Base.method(this, '_getObjectDataStore'),
			], [
				MochiKit.Async.succeed
			]);
			deferredResult.callback();
/* * /
			deferredResult = Clipperz.Async.callbacks("Clipperz.PM.DataModel.Record.revertChanges", [
				MochiKit.Base.method(this, 'directLogins'),
				MochiKit.Base.values,
				MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('revertChanges')),

				MochiKit.Base.method(this, 'invokeCurrentRecordVersionMethod', 'revertChanges'),
				MochiKit.Base.bind(Clipperz.PM.DataModel.Record.superclass.revertChanges, this),
			], {trace:false});
/ * */
		} else {
//			this.deleteAllCleanTextData();
			deferredResult = MochiKit.Async.succeed();
		}

		return deferredResult;
	},

	cancelRevertedAttachmentsUpload: function() {
		var reference;

		var transientStateAttachments = (this.transientState() ? this.transientState().values()['attachments'] : {}) || {};
		var savedReferences = MochiKit.Base.keys(transientStateAttachments);

		for (reference in this.attachments()) {
			if (savedReferences.indexOf(reference) < 0) {
				MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'cancelAttachment', this.attachments()[reference]);
			}
		}
	},

	//-------------------------------------------------------------------------

	'resetTransientState': function (isCommitting) {
//		if ((isCommitting == false) && (this.transientState().getValue('directLogins') != null)) {
//			this._directLogins = this.transientState().getValue('directLogins');
//		}

// console.log("Reset transient state: attachments:", this.transientState().getValue('directLogins'));

		return Clipperz.Async.callbacks("Record.resetTransientState", [
//-			MochiKit.Base.method(this, 'getCurrentRecordVersion'),
//-			MochiKit.Base.methodcaller('resetTransientState'),
//			MochiKit.Base.method(this, 'invokeCurrentRecordVersionMethod', 'resetTransientState'),
			MochiKit.Base.method(this, 'lazilyinvokeCurrentRecordVersionMethod', 'resetTransientState'),

			MochiKit.Base.method(this, 'directLogins'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('resetTransientState')),

			MochiKit.Base.method(this, 'attachments'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('resetTransientState')),

			MochiKit.Base.bind(function () {
				if (isCommitting == false) {
					if (this.transientState().getValue('directLogins') != null) {
						this._directLogins = this.transientState().getValue('directLogins');
					}
					if (this.transientState().getValue('attachments') != null) {
						this._attachments = this.transientState().getValue('attachments');
					}
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
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('commitTransientState')),

			MochiKit.Base.method(this, 'attachments'),
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

	'retrieveAttachmentIndexDataFunction': function () {
		return this._retrieveAttachmentIndexDataFunction;
	},
	
	'setAttachmentIndexDataFunction': function () {
		return this._setAttachmentIndexDataFunction;
	},
	
	'removeAttachmentIndexDataFunction': function () {
		return this._removeAttachmentIndexDataFunction;
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

			MochiKit.Base.method(this, 'attachments'),
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
				'attachments': [
					MochiKit.Base.method(this, 'attachments'),
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

	//-------------------------------------------------------------------------

	'moveFieldToPosition': function (aFieldReference, aPosition) {
		var	deferredResult;
		var	currentFieldValues;
		var	fromPosition;

		deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.DataModel.Record.moveFieldToPosition", {trace:false});
		deferredResult.addMethod(this, 'getFieldsValues');
		deferredResult.addCallback(function (someValues) {
			fromPosition = MochiKit.Base.keys(someValues).indexOf(aFieldReference);
			
			return ((fromPosition != -1) && (fromPosition!= aPosition));
		});
		deferredResult.addIf([
			MochiKit.Base.method(this, 'getFieldsValues'),
			function (someValues) { currentFieldValues = Clipperz.Base.deepClone(someValues); return currentFieldValues},
			MochiKit.Base.method(this, 'fields'), MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this, 'removeField')),
			Clipperz.Async.collectAll,

			function () {
				var	currentFieldKeys = MochiKit.Base.keys(currentFieldValues);
				currentFieldKeys.splice(aPosition, 0, currentFieldKeys.splice(fromPosition, 1)[0]);
				return currentFieldKeys;
			},
//function (aValue) { console.log("Sorted Keys", aValue); return aValue; },
			MochiKit.Base.partial(MochiKit.Base.map, function (aReference) { return currentFieldValues[aReference]; }),
//function (aValue) { console.log("Sorted Field values", aValue); return aValue; },
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this, 'addField')),
			Clipperz.Async.collectAll,
		], [
			MochiKit.Async.succeed
		]);
		deferredResult.callback();
		
		return deferredResult;
	},

	//=========================================================================

	'setUpWithRecord': function (aRecord) {
		return Clipperz.Async.callbacks("Record.setUpWithRecord", [
			MochiKit.Base.method(aRecord, 'label'),
			MochiKit.Base.bind(function (aLabel) {
				return this.setLabel(aLabel + " - copy");
			}, this),

			MochiKit.Base.method(aRecord, 'fullLabel'),
			MochiKit.Base.method(aRecord, 'extractTagsFromFullLabel'),
			MochiKit.Base.method(this, 'updateTags'),
			
			MochiKit.Base.method(aRecord, 'notes'),
			MochiKit.Base.method(this, 'setNotes'),

			MochiKit.Base.method(aRecord, 'getFieldsValues'), MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this, 'addField')),
			Clipperz.Async.collectAll,

			MochiKit.Base.method(aRecord, 'directLogins'), MochiKit.Base.values,
//function (aValue) { console.log("-> SETUP WITH RECORD: DirectLogin Values", aValue); return aValue; },
			//	TODO: possibly broken implementation of direct login cloning
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this, 'bindDirectLogin')),
//function (aValue) { console.log("-> DirectLogin Values", aValue); return aValue; },
//			Clipperz.Async.collectAll,

			// TODO: attachments

			MochiKit.Base.bind(function () { return this; }, this)
		], {trace:false});
	},
	
	'directLoginWithJsonData': function (someData) {
		var	result;

		result = new Clipperz.PM.DataModel.DirectLogin({'record': this});

		return result;
	},

	'setUpWithJSON': function(data, labelPostfix) {
		return Clipperz.Async.callbacks("Record.setUpWithJSON", [
			// TODO: proper tag handling
			MochiKit.Base.method(this,'setLabel', data['label'] + ((labelPostfix) ? labelPostfix : '')),
			MochiKit.Base.method(this,'setNotes', data['data']['notes']),
			// TODO: check whether fields' order is kept or not
			MochiKit.Base.partial(MochiKit.Base.values, data['currentVersion']['fields']),
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this, 'addField')),
			Clipperz.Async.collectAll,
			MochiKit.Base.partial(MochiKit.Async.succeed, this),
		], {trace:false});
	},

	//=========================================================================
	
	'exportDirectLogins': function() {
		var result;
		var directLoginsObject = this.directLogins();
		
		if (MochiKit.Base.keys(directLoginsObject).length == 0) {
			result = {};
		} else {
			var callbackObject = Object.keys(directLoginsObject).reduce(function(previous, current) {
				previous[current] = MochiKit.Base.method( directLoginsObject[current], 'serializedData' );
				return previous;
			}, {});
			
			result = Clipperz.Async.collectResults("Record.exportDirectLogins", callbackObject,{trace:false})();
		}
		
		return result;
	},

	'export': function() {
		var deferredResult;
		var label;
		var data;
		var currentVersion;
//		var directLogins;
		var currentVersionObject;

		data = {};
		currentVersion = {};
//		directLogins = {};
		deferredResult = new Clipperz.Async.Deferred('Record.export', {trace:false});
		deferredResult.addMethod(this, 'getCurrentRecordVersion');
		deferredResult.addCallback(function(recordVersionIn) { currentVersionObject = recordVersionIn; })
		deferredResult.addMethod(this, 'fullLabel');
		deferredResult.addMethod(this, function(labelIn) {label = labelIn});
		deferredResult.addMethod(this, 'exportDirectLogins');
		deferredResult.addCallback(function(directLoginsIn) { data['directLogins'] = directLoginsIn; });
		deferredResult.addCallback(function() { return currentVersionObject.getKey(); }),
//		deferredResult.addMethod(this,function(keyIn) { data['currentVersionKey'] = keyIn; });
		deferredResult.addMethod(this, 'notes');
		deferredResult.addMethod(this, function(notesIn) { data['notes'] = notesIn; });
//		deferredResult.addMethod(this, function() { currentVersion['reference'] = this.currentVersionReference(); });
		deferredResult.addCallback(function() { return currentVersionObject.exportFields(); }),
		deferredResult.addCallback(function(fieldsIn) { currentVersion['fields'] = fieldsIn; });
		deferredResult.addMethod(this, function() {
			return {
				'label': label,
				'data': data,
				'currentVersion': currentVersion
			};
		});
		
		deferredResult.callback();
		
		return deferredResult;
	},
	
	//=========================================================================
	__syntaxFix__: "syntax fix"
});

Clipperz.PM.DataModel.Record.defaultCardInfo = {
	'_rowObject':			MochiKit.Async.succeed,
	'_reference':			MochiKit.Base.methodcaller('reference'),
	'_searchableContent':	MochiKit.Base.methodcaller('searchableContent'),
	'_accessDate':			MochiKit.Base.methodcaller('accessDate'),
	'_isArchived':			MochiKit.Base.methodcaller('isArchived'),
	'_isBrandNew':			MochiKit.Base.methodcaller('isBrandNew'),
	'label':				MochiKit.Base.methodcaller('label'),
	'favicon':				MochiKit.Base.methodcaller('favicon'),
	'attachmentsCount':		MochiKit.Base.methodcaller('attachmentsCount'),
};
Clipperz.PM.DataModel.Record.defaultSearchField = '_searchableContent';

Clipperz.PM.DataModel.Record.tagChar =			'\uE009';
Clipperz.PM.DataModel.Record.specialTagChar =	'\uE010';
//Clipperz.PM.DataModel.Record.tagSpace =			'\uE011';
Clipperz.PM.DataModel.Record.specialTagsConstructor = function (aTag) {
	return Clipperz.PM.DataModel.Record.specialTagChar + aTag;	//.replace(/\s/g, Clipperz.PM.DataModel.Record.tagSpace);
}
Clipperz.PM.DataModel.Record.archivedTag = Clipperz.PM.DataModel.Record.specialTagsConstructor('ARCH');
Clipperz.PM.DataModel.Record.regExpForTag = function (aTag) {
	return new RegExp('\\' + Clipperz.PM.DataModel.Record.tagChar + aTag, 'g');
};
Clipperz.PM.DataModel.Record.regExpForNoTag = function () {
	return new RegExp('^((?!\\' + Clipperz.PM.DataModel.Record.tagChar + '[^' + Clipperz.PM.DataModel.Record.specialTagChar + ']).)*$', 'g');
}
Clipperz.PM.DataModel.Record.isSpecialTag = function (aTag) {
	return aTag.indexOf(Clipperz.PM.DataModel.Record.specialTagChar) == 0;
};
Clipperz.PM.DataModel.Record.isRegularTag = function (aTag) {
	return !Clipperz.PM.DataModel.Record.isSpecialTag(aTag);
};
Clipperz.PM.DataModel.Record.regExpForSearch = function (aSearch) {
	return new RegExp(aSearch.replace(/[^A-Za-z0-9]/g, '\\$&'), 'i');
};


// Slightly more readable: http://rubular.com/r/4O8xGEsLgw
Clipperz.PM.DataModel.Record.tagRegExp = new RegExp('\\' + Clipperz.PM.DataModel.Record.tagChar + '(' + Clipperz.PM.DataModel.Record.specialTagChar + '?[^'+Clipperz.PM.DataModel.Record.tagChar+']+?)\\s*(?='+Clipperz.PM.DataModel.Record.tagChar+'|$)', 'g');
Clipperz.PM.DataModel.Record.trimSpacesRegExp = new RegExp('^\\s+|\\s+$', 'g');

Clipperz.PM.DataModel.Record.extractLabelFromFullLabel = function (aValue) {
	var value;

	value = aValue;
	value = value.replace(Clipperz.PM.DataModel.Record.tagRegExp, '');
	value = value.replace(Clipperz.PM.DataModel.Record.trimSpacesRegExp, '');

	return value;
};

Clipperz.PM.DataModel.Record.extractTagsFromFullLabel = function (aLabel) {
	var	tagRegEx;
	var	result;
	var	match;
	
	result = {};
	tagRegEx = Clipperz.PM.DataModel.Record.tagRegExp;
	match = tagRegEx.exec(aLabel);
	while (match != null) {
		result[match[1]] = true;
		match = tagRegEx.exec(aLabel);
	}		
	
	return result;
};

Clipperz.PM.DataModel.Record.labelContainsTag = function (aLabel, aTag) {
	return MochiKit.Iter.some(
		MochiKit.Base.keys(Clipperz.PM.DataModel.Record.extractTagsFromFullLabel(aLabel)),
		MochiKit.Base.partial(MochiKit.Base.operator.eq, aTag)
	);
};

Clipperz.PM.DataModel.Record.labelContainsArchiveTag = function (aLabel) {
	return Clipperz.PM.DataModel.Record.labelContainsTag(aLabel, Clipperz.PM.DataModel.Record.archivedTag);
};