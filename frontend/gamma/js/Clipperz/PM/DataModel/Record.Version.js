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

try { if (typeof(Clipperz.PM.DataModel.Record) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.PM.DataModel.Record.Version depends on Clipperz.PM.DataModel.Record!";
}  

Clipperz.PM.DataModel.Record.Version = function(args) {
//console.log(">>> Record.new");
	Clipperz.PM.DataModel.Record.Version.superclass.constructor.apply(this, arguments);

	this._getVersionFunction = args.getVersion	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._fields = null;

	return this;
}


Clipperz.Base.extend(Clipperz.PM.DataModel.Record.Version, Clipperz.PM.DataModel.EncryptedRemoteObject, {

	'toString': function() {
		return "Record.Version (" + this.reference() + ")";
	},

	//-------------------------------------------------------------------------

	'reference': function () {
		return this._reference;
	},
	
	//-------------------------------------------------------------------------
/*
	'hasPendingChanges': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.DataModel.Record.Version.hasPendingChanges", {trace:false});
		deferredResult.addCallback(MochiKit.Base.bind(Clipperz.PM.DataModel.Record.Version.superclass.hasPendingChanges, this));
		deferredResult.callback();
		
		return deferredResult;
	},
*/
	//-------------------------------------------------------------------------


	'hasPendingChangesWhenBrandNew': function () {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.DataModel.Record.Version.hasPendingChangesWhenBrandNew", {trace:false});
		deferredResult.addMethod(this, 'fields');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.methodcaller('isEmpty'))
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addCallback(function(someValues) {
			return MochiKit.Iter.every(someValues, MochiKit.Base.operator.identity);
		});
		deferredResult.addCallback(MochiKit.Base.operator.lognot)
		deferredResult.callback();
		
		return deferredResult;
	},

	//=========================================================================

	'commitTransientState': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.DataModel.Record.Version.commitTransientState", {trace:false});
		deferredResult.addCallback(MochiKit.Base.bind(Clipperz.PM.DataModel.Record.Version.superclass.commitTransientState, this));
		deferredResult.callback();
		
		return deferredResult;
	},

	//=========================================================================

	'unpackData': function (someData) {	//	++
		var	result;
		
//console.log("Record.Version - UNPACK DATA", this, someData);
		result = someData;
		if ((someData['fields'] != null) && (someData['fields'] instanceof Array)) {
			var	fields;
			var i,c;
			
			fields = someData['fields'];
			delete someData['fields'];
			
			someData['fields'] = {};
			c = fields.length;
			for (i=0; i<c; i++) {
				someData['fields'][i] = fields[i];
			}
		}



		return result;
	},

	//=========================================================================

	'fields': function () {
		var	deferredResult;
		var deferredLock;
		
		deferredLock = this.getDeferredLockForKey('fields');
		
		deferredResult = new Clipperz.Async.Deferred("Record.Version.fields", {trace:false});
		deferredResult.acquireLock(deferredLock);
		deferredResult.addCallback(MochiKit.Base.bind(function () {
			var innerDeferredResult;

			if (this._fields == null) {
				innerDeferredResult = new Clipperz.Async.Deferred("Record.Version.fields <inner deferred>", {trace:false});
				innerDeferredResult.addMethod(this, 'getValue', 'fields');
				innerDeferredResult.addCallback(MochiKit.Base.bind(function (someObjectData) {
					var reference;

					this._fields = {};
					
					for (reference in someObjectData) {
						var	recordVersionField;
			
						recordVersionField = new Clipperz.PM.DataModel.Record.Version.Field({
							'recordVersion':	this,
							'reference':		reference
						});
			
						this._fields[reference] = recordVersionField;
					}

					return this._fields;
				}, this));
				innerDeferredResult.callback();
			} else {
				innerDeferredResult = MochiKit.Async.succeed(this._fields);
			}
			
			return innerDeferredResult;
		}, this));
		deferredResult.releaseLock(deferredLock);
		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'getFieldsValues': function () {
		return this.getValue('fields');
	},
	
	//-------------------------------------------------------------------------

	'addField': function (someParameters) {
		var	newField;
		
		newField = new Clipperz.PM.DataModel.Record.Version.Field({recordVersion:this});
		
		return Clipperz.Async.callbacks("Record.Version.addField", [
			MochiKit.Base.method(this, 'fields'),

			MochiKit.Base.method(this, '_getObjectDataStore'),
			MochiKit.Base.methodcaller('values'),
			Clipperz.Base.serializeJSON,

			MochiKit.Base.bind(function () { this._fields[newField.reference()] = newField; }, this),
			MochiKit.Base.method(newField,	'setLabel',		someParameters['label']),
			MochiKit.Base.method(newField,	'setValue',		someParameters['value']),
			MochiKit.Base.method(newField,	'setIsHidden',	someParameters['isHidden']),

			MochiKit.Base.method(this, '_getObjectDataStore'),
			MochiKit.Base.methodcaller('values'),
			Clipperz.Base.serializeJSON,

			MochiKit.Base.partial(MochiKit.Async.succeed, newField)
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'removeField': function (aField) {
		return Clipperz.Async.callbacks("Record.Version.removeField", [
			MochiKit.Base.method(this, 'fields'),
			MochiKit.Base.bind(function () { delete this._fields[aField.reference()]; }, this),
			MochiKit.Base.method(this, 'removeValue', 'fields' + '.' + aField.reference())
		], {trace:false});
	},

	//-------------------------------------------------------------------------
/*
	'sortFieldReference': function (someSortedFieldReferences) {



	},
*/
	//=========================================================================
/*
	'directLogins': function () {
		return MochiKit.Base.values(this._directLogins);
	},

	'addDirectLogin': function (aDirectLogin) {
		this._directLogins[aDirectLogin.reference()] = aDirectLogin;
	},
*/

	//=========================================================================
/*
	'updateValues': function (anotherVersion) {
		return Clipperz.Async.callbacks("Record.Version.updateValue", [
			MochiKit.Base.partial(MochiKit.Async.succeed, this)
		], {trace:false});
	},
*/
	//=========================================================================

	'setRemoteData': function (aValue) {
		this._remoteData = aValue;
		
		return aValue;
	},

	//=========================================================================

	'getVersionFunction': function () {
		return this._getVersionFunction;
	},
	
	'previousVersion': function () {
		return Clipperz.Async.callbacks("Record.Versions.previousVersion", [
			MochiKit.Base.method(this, 'previousVersionReference'),
			this.getVersionFunction()
		], {trace:false});
	},

	'previousVersionReference': function () {
		return this.getValue('previousVersionReference');
	},
	
	'previousVersionKey': function () {
//	TODO: this value i encrypted on its own. So it can not be saved in the main objectStore!!!
		return this.getValue('previousVersionKey');
	},

	//-------------------------------------------------------------------------

	'setPreviousVersionReferenceAndKey': function (aVersionObjectAndKey) {
//		this._previousVersion = anotherVersion;
		return Clipperz.Async.callbacks("Record.Version.setPreviousVersion", [
			MochiKit.Base.method(this, 'setValue', 'previousVersionReference',	aVersionObjectAndKey['reference']),
			MochiKit.Base.method(this, 'setValue', 'previousVersionKey',		aVersionObjectAndKey['key'])
		], {trace:false});
	},

	//=========================================================================

	'revertChanges': function () {
		this.setReference(this.transientState()['originalReference']);
		Clipperz.PM.DataModel.Record.Version.superclass.revertChanges.apply(this, arguments);
	},

	//-------------------------------------------------------------------------

	'prepareRemoteDataWithKey': function (aKey) {
		var deferredResult;
		var result;

		result = {};

//console.log("prepareRemoteDataWithKey", aKey);
		deferredResult = new Clipperz.Async.Deferred("Record.Version.prepareRemoteDataWithKey", {trace:false});
		if (this.isBrandNew() == false) {
			this.transientState()['originalReference'] = this.reference();

			deferredResult.collectResults({
				'key':	MochiKit.Base.partial(MochiKit.Async.succeed, aKey),
				'value': MochiKit.Base.method(this, 'getKey'),
				'version': MochiKit.Base.partial(MochiKit.Async.succeed, Clipperz.PM.Crypto.encryptingFunctions.currentVersion)
			});
			deferredResult.addCallback(Clipperz.PM.Crypto.deferredEncrypt);
			deferredResult.addCallback(Clipperz.Async.setItem, result, 'previousVersionKey');
		} else {
			deferredResult.addCallback(Clipperz.Async.setItem, result, 'previousVersionKey', Clipperz.PM.Crypto.nullValue);
		}
		deferredResult.addCallback(MochiKit.Base.bind(Clipperz.PM.DataModel.Record.superclass.prepareRemoteDataWithKey, this, aKey));
		deferredResult.addCallback(MochiKit.Base.update, result);
		deferredResult.addMethod(this, 'setRemoteData');
		
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================
/*
	'deleteAllCleanTextData': function () {
		return Clipperz.PM.DataModel.Record.Version.superclass.deleteAllCleanTextData.apply(this, arguments);
	},

	'hasAnyCleanTextData': function () {
		return Clipperz.PM.DataModel.Record.Version.superclass.hasAnyCleanTextData.apply(this, arguments);
	},
*/
	//=========================================================================
	__syntaxFix__: "syntax fix"
});


