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

try { if (typeof(Clipperz.PM.DataModel.User) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.PM.DataModel.User.Header.Legacy depends on Clipperz.PM.DataModel.User!";
}  

if (typeof(Clipperz.PM.DataModel.User.Header) == 'undefined') { Clipperz.PM.DataModel.User.Header = {}; }

Clipperz.PM.DataModel.User.Header.Legacy = function(args) {
//	args = args || {};
	Clipperz.PM.DataModel.User.Header.Legacy.superclass.constructor.apply(this, arguments);
	
	this._retrieveRecordDetailFunction = args.retrieveRecordDetailFunction	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._records = null;
//	this._directLogins = null;

	return this;
}


Clipperz.Base.extend(Clipperz.PM.DataModel.User.Header.Legacy, Clipperz.PM.DataModel.EncryptedRemoteObject, {

	'toString': function() {
		return "Clipperz.PM.DataModel.User.Header.Legacy";
	},

	//-------------------------------------------------------------------------

	'retrieveRecordDetailFunction': function () {
		return this._retrieveRecordDetailFunction;
	},

	//-------------------------------------------------------------------------

	'getRecordKey': function (aRecordReference) {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("User.Header.Legacy.getRecordKey", {trace:false});
		deferredResult.addMethod(this, 'getRecordIndexData');
		deferredResult.addCallback(MochiKit.Base.itemgetter('key'))
		deferredResult.callback();
		
		return deferredResult;
	},

	//=========================================================================
	
	'getRecordIndexData': function (aRecordReference) {
		return this.getValue('records.' + aRecordReference);
	},

	'updateRecordIndexData': function (aRecordReference, aKey, aValue) {
		return this.setValue('records.' + aRecordReference + "." + aKey, aValue);
	},

	//-------------------------------------------------------------------------

	'getDirectLoginIndexData': function (aDirectLoginReference) {
		return this.getValue('directLogins.' + aDirectLoginReference);
	},

	'setDirectLoginIndexData': function (aDirectLoginReference, aKey, aValue) {
		return this.setValue('directLogins.' + aDirectLoginReference + '.' + aKey, aValue);
	},
	
	'removeDirectLoginIndexData': function (aDirectLoginReference) {
		return this.removeValue('directLogins.' + aDirectLoginReference);
	},

	//=========================================================================

	'records': function () {
		var	deferredResult;
		var deferredLock;
		
		deferredLock = this.getDeferredLockForKey('records');
		
		deferredResult = new Clipperz.Async.Deferred("User.Header.Legacy.records", {trace:false});
		deferredResult.acquireLock(deferredLock);
		deferredResult.addCallback(MochiKit.Base.bind(function () {
			var innerDeferredResult;

			if (this._records == null) {
				innerDeferredResult = new Clipperz.Async.Deferred("User.Header.Legacy.records <inner deferred>", {trace:false});
				innerDeferredResult.collectResults({
					'header': [
//						MochiKit.Base.method(this, 'getObjectDataStore'),
//						MochiKit.Base.methodcaller('values')
						MochiKit.Base.method(this, 'values')
					],
					'recordsStats': [
						MochiKit.Base.method(this, 'getRemoteData'),
						MochiKit.Base.itemgetter('recordsStats')
					]
				});
				innerDeferredResult.addCallback(MochiKit.Base.bind(function (someObjectData) {
					var reference;

					this._records = {};
//					this._directLogins = {};
					
					for (reference in someObjectData['header']['records']) {
						var	record;
			
						record = new Clipperz.PM.DataModel.Record({
							'reference':					reference,
							'retrieveKeyFunction':			MochiKit.Base.method(this, 'getRecordKey'),
							'retrieveRemoteDataFunction':	this.retrieveRecordDetailFunction(),
//							'encryptedDataKeypath':			'data',
//							'encryptedVersionKeypath':		'version',

							'retrieveIndexDataFunction':	MochiKit.Base.method(this, 'getRecordIndexData'),
							'updateIndexDataFunction':		MochiKit.Base.method(this, 'updateRecordIndexData'),
							'updateDate':					someObjectData['recordsStats'][reference]['updateDate'],

							'retrieveDirectLoginIndexDataFunction':	MochiKit.Base.method(this, 'getDirectLoginIndexData'),
							'setDirectLoginIndexDataFunction':		MochiKit.Base.method(this, 'setDirectLoginIndexData'),
							'removeDirectLoginIndexDataFunction':	MochiKit.Base.method(this, 'removeDirectLoginIndexData')
						});
			
						this._records[reference] = record;
					}

					for (reference in someObjectData['header']['directLogins']) {
						var	directLogin;
						var record;
						
						record = this._records[someObjectData['header']['directLogins'][reference]['record']];
						if (record != null) {
							directLogin = new Clipperz.PM.DataModel.DirectLogin({
								'reference':					reference,
								'record':						record	//,
//								'retrieveIndexDataFunction':	MochiKit.Base.method(this, 'getDirectLoginIndexData'),
//								'setIndexDataFunction':			MochiKit.Base.method(this, 'setDirectLoginIndexData'),
//								'removeIndexDataFunction':		MochiKit.Base.method(this, 'removeDirectLoginIndexData')
							});
						} else {
Clipperz.log("WARNING: DIRECT LOGIN without a matching RECORD!!");
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
		deferredResult.releaseLock(deferredLock);
		deferredResult.callback();
		
		return deferredResult;
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});


