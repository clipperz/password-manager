/*

Copyright 2008-2018 Clipperz Srl

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


//#############################################################################

Clipperz.PM.DataModel.User = function (args) {
	args = args || {};

	Clipperz.PM.DataModel.User.superclass.constructor.apply(this, arguments);

	this._username = args.username || null;
	this._getPassphraseFunction = args.getPassphraseFunction || null;

	this._data = null;

	this._connection = null;
	this._connectionVersion = 'current';

	this._accountInfo = null;
	this._serverData = null;
//	this._serverLockValue = null;
	this._transientState = null;

	this._deferredLocks = {
		'passphrase':			new MochiKit.Async.DeferredLock(),
		'serverData':			new MochiKit.Async.DeferredLock(),
		'__syntaxFix__': 'syntax fix'
	};

	this._usedOTP = null;

	return this;
}

Clipperz.Base.extend(Clipperz.PM.DataModel.User, Object, {

	'toString': function () {
		return "Clipperz.PM.DataModel.User - " + this.username();
	},

	//-------------------------------------------------------------------------

	'username': function () {
		return this._username;
	},

	'setUsername': function (aValue) {
		this._username = aValue;
	},

	//-------------------------------------------------------------------------

	'setUsedOTP': function(aOTP) {
		this._usedOTP = aOTP;

		return aOTP;
	},

	'resetUsedOTP': function(aOTP) {
		this._usedOTP = null;
	},

	'markUsedOTP': function(aOTP) {
		var result;
		var oneTimePasswordKey;

		if (this._usedOTP) {
			oneTimePasswordKey = Clipperz.PM.DataModel.OneTimePassword.computeKeyWithPassword(
				Clipperz.PM.DataModel.OneTimePassword.normalizedOneTimePassword(this._usedOTP)
			);

			result = Clipperz.Async.callbacks("User.markUsedOTP", [ // NOTE: fired also when passphrase looks exactly like OTP
				MochiKit.Base.method(this, 'getHeaderIndex', 'oneTimePasswords'),
				MochiKit.Base.methodcaller('markOTPAsUsed', oneTimePasswordKey),
				MochiKit.Base.method(this,'saveChanges'), // Too 'heavy'?
				MochiKit.Base.method(this, 'resetUsedOTP')
			], {'trace': false});
		} else {
			result = MochiKit.Async.succeed();
		}

		return result;
	},

	//-------------------------------------------------------------------------

//	this.setSubscription(new Clipperz.PM.DataModel.User.Subscription(someServerData['subscription']));
	'accountInfo': function () {
		return this._accountInfo;
	},

	'setAccountInfo': function (aValue) {
		this._accountInfo = aValue;
	},

	//-------------------------------------------------------------------------

	'displayName': function() {
		return "" + this.username() + "";
	},
	
	//-------------------------------------------------------------------------

	'data': function () {
		if (this._data == null) {
			this._data = new Clipperz.KeyValueObjectStore(/*{'name':'User.data [1]'}*/);
		};
		
		return this._data;
	},

	//-------------------------------------------------------------------------
/*
	'serverLockValue': function () {
		return this._serverLockValue;
	},
	
	'setServerLockValue': function (aValue) {
		this._serverLockValue = aValue;
	},
*/
	//-------------------------------------------------------------------------

	'transientState': function () {
		if (this._transientState == null) {
			this._transientState = {}
		}
		
		return this._transientState;
	},

	'resetTransientState': function (isCommitting) {
		this._transientState = null;
	},

	//-------------------------------------------------------------------------

	'deferredLockForSection': function(aSectionName) {
		return this._deferredLocks[aSectionName];
	},

	//-------------------------------------------------------------------------

	'getPassphrase': function() {
		return this._getPassphraseFunction();
	},

	'getPassphraseFunction': function () {
		return this._getPassphraseFunction;
	},

	'setPassphraseFunction': function (aFunction) {
		this._getPassphraseFunction = aFunction;
	},

	//-------------------------------------------------------------------------

	'getCredentials': function () {
		return Clipperz.Async.collectResults("User; get username and passphrase", {
			'username': MochiKit.Base.method(this, 'username'),
			'password': MochiKit.Base.method(this, 'getPassphrase')
		}, {trace:false})();
	},

	//-------------------------------------------------------------------------

	'changePassphrase': function (aNewValueCallback) {
		return this.updateCredentials(this.username(), aNewValueCallback);
	},

	//.........................................................................

	'updateCredentials': function (aUsername, aPassphraseCallback) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("User.updateCredentials", {trace:false});
		deferredResult.addMethod(this.connection(), 'ping');
		deferredResult.collectResults({
			'newUsername': MochiKit.Base.partial(MochiKit.Async.succeed, aUsername),
			'newPassphrase': aPassphraseCallback,
			'user': MochiKit.Base.method(this, 'prepareRemoteDataWithKeyFunction', aPassphraseCallback),
			'oneTimePasswords': [
				MochiKit.Base.method(this, 'getHeaderIndex', 'oneTimePasswords'),
				MochiKit.Base.methodcaller('getEncryptedOTPData', aPassphraseCallback),
				function (otps) {
					var result;
					var otpRefs;
					var i, c;

					result = {};
					otpRefs = MochiKit.Base.keys(otps);
					c = otpRefs.length;
					for (i=0; i<c; i++) {
						result[otpRefs[i]] = {}
						result[otpRefs[i]]['data'] = otps[otpRefs[i]]['data'];
						result[otpRefs[i]]['version'] = otps[otpRefs[i]]['version'];
					}

					return result;
				}
			]
		});

		deferredResult.addMethod(this.connection(), 'updateCredentials');
		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'initialSetupWithNoData': function () {
		this._serverData = {
			'version': '0.1',
			'statistics': "",
			'header': {
				'data': null,
				'version': Clipperz.PM.Crypto.encryptingFunctions.currentVersion,

				'recordsIndex': new Clipperz.PM.DataModel.User.Header.RecordIndex({
					'retrieveKeyFunction':			MochiKit.Base.method(this, 'getPassphrase'),
					'recordsData':					{'data':null, 'index':{}},
					'recordsStats':					null,
					'directLoginsData':				{'data':null, 'index':{}},
					'attachmentsData':				{'data':null, 'index':{}},
					'encryptedDataVersion':			Clipperz.PM.Crypto.encryptingFunctions.currentVersion,
					'retrieveRecordDetailFunction':	MochiKit.Base.method(this, 'getRecordDetail')
				}),
				'preferences': new Clipperz.PM.DataModel.User.Header.Preferences({
					'name':	'preferences',
					'retrieveKeyFunction': MochiKit.Base.method(this, 'getPassphrase')
				}),
				'oneTimePasswords': new Clipperz.PM.DataModel.User.Header.OneTimePasswords({
					'connection': this.connection(),
					'name':	'oneTimePasswords',
					'username': this.username(),
					'passphraseCallback': MochiKit.Base.method(this, 'getPassphrase')
				}),
				'wallet': new Clipperz.PM.DataModel.User.Header.Wallet({
					'name':	'wallet',
					'retrieveKeyFunction': MochiKit.Base.method(this, 'getPassphrase')
				})
			}
		};
		
//		this._serverLockValue = Clipperz.PM.Crypto.randomKey();
	},

	//.........................................................................

	'registerAsNewAccount': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("User.registerAsNewAccount", {trace:false});
		deferredResult.addMethod(this, 'initialSetupWithNoData')
		deferredResult.addMethod(this, 'getPassphrase');
		deferredResult.addMethod(this, 'prepareRemoteDataWithKey');
		deferredResult.addMethod(this.connection(), 'register');
		deferredResult.callback();
		
		return deferredResult;
	},
	
	'deleteAccount': function() {
		var deferredResult;
		
		deferredResult = new MochiKit.Async.Deferred("User.deleteAccount", {trace:false});
		deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'message'), 'deleteUser');
		deferredResult.addCallback(MochiKit.Base.method(this, 'resetAllLocalData'));
		deferredResult.callback();

		return deferredResult;
	},
	
	'resetAllLocalData': function() {
		var deferredResult;
		
		deferredResult = new MochiKit.Async.Deferred("User.resetAllLocalData", {trace:false});
		deferredResult.addCallback(MochiKit.Base.method(this, 'deleteAllCleanTextData'));
		deferredResult.addCallback(MochiKit.Base.method(this, function() {
			this.resetConnection();
			this.setUsername("");
			this._getPassphraseFunction = function() { return ""; };
			this._serverData = null;
		}));
		
		deferredResult.callback();
		
		return deferredResult;	
	},

	//-------------------------------------------------------------------------

	'login': function () {
		var deferredResult;
		var oneTimePasswordReference;

		deferredResult = new Clipperz.Async.Deferred("User.login", {trace:false});
		deferredResult.addMethod(this, 'getPassphrase');
		deferredResult.addCallback(Clipperz.PM.DataModel.OneTimePassword.isValidOneTimePasswordValue);

		deferredResult.addCallback(Clipperz.Async.deferredIf("Is the passphrase an OTP", [
			MochiKit.Base.method(this,'getPassphrase'),
			MochiKit.Base.method(this,'setUsedOTP'),
			MochiKit.Base.method(this, 'getCredentials'),
			MochiKit.Base.method(this.connection(), 'redeemOneTimePassword'),
			function (aPassphrase) {
				return MochiKit.Base.partial(MochiKit.Async.succeed, aPassphrase);
			},
			MochiKit.Base.method(this, 'setPassphraseFunction')
		], []));

		deferredResult.addBoth(MochiKit.Base.method(this, 'loginWithPassphrase'));
		deferredResult.addBothPass(MochiKit.Base.method(this, 'resetUsedOTP'));
		
		deferredResult.callback();
		
		return deferredResult;
	},

	'loginWithPassphrase': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("User.loginWithPassphrase", {trace:false});

		deferredResult.addMethod(this, 'getPassphrase');
		deferredResult.addMethod(this.connection(), 'login', false);
		deferredResult.addMethod(this, 'setupAccountInfo');
		deferredResult.addMethod(this, 'markUsedOTP');
		deferredResult.addErrback (MochiKit.Base.method(this, 'handleConnectionFallback'));

		deferredResult.callback();

		return deferredResult;
	},

	//.........................................................................

	'handleConnectionFallback': function(aValue) {
		var result;

		if (aValue instanceof MochiKit.Async.CancelledError) {
			result = aValue;
		} else if ((aValue['isPermanent'] === true) || (Clipperz.PM.Connection.communicationProtocol.fallbackVersions[this.connectionVersion()] == null)) {
			result = Clipperz.Async.callbacks("User.handleConnectionFallback - failed", [
				MochiKit.Base.method(this.data(), 'removeValue', 'passphrase'),
				MochiKit.Base.method(this, 'setConnectionVersion', 'current'),
				MochiKit.Base.partial(MochiKit.Async.fail, aValue)
			], {trace:false});
		} else {
			this.setConnectionVersion(Clipperz.PM.Connection.communicationProtocol.fallbackVersions[this.connectionVersion()]);
			result = new Clipperz.Async.Deferred("User.handleConnectionFallback - retry");
			result.addMethod(this, 'loginWithPassphrase');
			result.callback();
		}

		return result;
	},

	//-------------------------------------------------------------------------

	'setupAccountInfo': function (aValue) {
		this.setAccountInfo(new Clipperz.PM.DataModel.User.AccountInfo(aValue['accountInfo']));
	},

	'updateAccountInfo': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("User.updateAccountInfo", {trace:false});

		deferredResult.addMethod(this.connection(), 'message', 'accountInfo');
//		deferredResult.addCallback(function (aValue) {
//console.log("ACCOUNT INFO", aValue);
//			return new Clipperz.PM.DataModel.User.AccountInfo(aValue);
//		})
		deferredResult.addMethod(this, 'setupAccountInfo');
		deferredResult.addErrback (MochiKit.Base.method(this, 'handleConnectionFallback'));

		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'lock': function () {
		return Clipperz.Async.callbacks("User.lock", [
			MochiKit.Base.method(this.connection(), 'logout'),
			MochiKit.Base.method(this, 'deleteAllCleanTextData'),
			MochiKit.Base.method(this, 'setPassphraseFunction', function() {throw("No passphrase set.")})
		], {trace:false});
	},

	'unlock': function (aPassphraseDelegate) {
		this.setPassphraseFunction(aPassphraseDelegate);
		return this.getPreferences();	//	TODO: make this more explicit.
	},

	//-------------------------------------------------------------------------

	'logout': function () {
		return Clipperz.Async.callbacks("User.logout", [
			MochiKit.Base.method(this, 'deleteAllCleanTextData'),
			MochiKit.Base.method(this.connection(), 'logout')
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'headerFormatVersion': function(anHeader) {
		var result;
		
		if (anHeader.charAt(0) == '{') {
			var	headerData;
			
			headerData = Clipperz.Base.evalJSON(anHeader);
			result = headerData['version'];
		} else {
			result = 'LEGACY';
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'unpackServerData': function (someServerData) {
		var unpackedData;
		var headerVersion;

		var	recordsIndex;
		var	preferences;
		var	oneTimePasswords;
		var	wallet;

		headerVersion = this.headerFormatVersion(someServerData['header']);
		switch (headerVersion) {
			case 'LEGACY':
				var	legacyHeader;
				
				legacyHeader = new Clipperz.PM.DataModel.User.Header.Legacy({
					'retrieveKeyFunction': MochiKit.Base.method(this, 'getPassphrase'),
					'remoteData': {
						'data': someServerData['header'],
						'version': someServerData['version'],
						'recordsStats': someServerData['recordsStats']
					},
//					'encryptedDataKeypath':			'data',
//					'encryptedVersionKeypath':		'version',
					'retrieveRecordDetailFunction':	MochiKit.Base.method(this, 'getRecordDetail')
				});

				recordsIndex		= legacyHeader;
				preferences			= legacyHeader;
				oneTimePasswords	= legacyHeader;
				wallet				= legacyHeader;	//	TODO: does this make any sense?
				break;
			case '0.1':
				var	headerData;

//console.log("Server data", someServerData);
				headerData = Clipperz.Base.evalJSON(someServerData['header']);

//console.log("headerData", headerData);
				recordsIndex = new Clipperz.PM.DataModel.User.Header.RecordIndex({
					'retrieveKeyFunction':			MochiKit.Base.method(this, 'getPassphrase'),
					'recordsData':					headerData['records'],
					'recordsStats':					someServerData['recordsStats'],
					'directLoginsData':				headerData['directLogins'],
					'attachmentsData':				headerData['attachments'] || {'data': null, 'index':{}},
					'encryptedDataVersion':			someServerData['version'],
					'retrieveRecordDetailFunction':	MochiKit.Base.method(this, 'getRecordDetail')
				});

				//	Still missing a test case that actually fais with the old version of the code, where the check for undefined was missing
				if (typeof(headerData['preferences']) != 'undefined') {
					preferences	= new Clipperz.PM.DataModel.User.Header.Preferences({
						'name':	'preferences',
						'retrieveKeyFunction': MochiKit.Base.method(this, 'getPassphrase'),
						'remoteData': {
							'data': headerData['preferences']['data'],
							'version': someServerData['version']
						}
					});
				} else {
					preferences	= new Clipperz.PM.DataModel.User.Header.Preferences({
						'name':	'preferences',
						'retrieveKeyFunction': MochiKit.Base.method(this, 'getPassphrase')
					});
				}

				if (typeof(headerData['oneTimePasswords']) != 'undefined') {
					oneTimePasswords = new Clipperz.PM.DataModel.User.Header.OneTimePasswords({
						'name':	'oneTimePasswords',
						'connection': this.connection(),
						'username': this.username(),
						'retrieveKeyFunction': MochiKit.Base.method(this, 'getPassphrase'),
						'remoteData': {
							'data': headerData['oneTimePasswords']['data'],
							'version': someServerData['version']
						}
					});
				} else {
					oneTimePasswords = new Clipperz.PM.DataModel.User.Header.OneTimePasswords({
						'name':	'OneTimePasswords',
						'connection': this.connection(),
						'username': this.username(),
						'retrieveKeyFunction': MochiKit.Base.method(this, 'getPassphrase')
					});
				}

				if (typeof(headerData['wallet']) != 'undefined') {
					wallet	= new Clipperz.PM.DataModel.User.Header.Wallet({
						'name':	'wallet',
						'retrieveKeyFunction': MochiKit.Base.method(this, 'getPassphrase'),
						'remoteData': {
							'data': headerData['wallet']['data'],
							'version': someServerData['version']
						}
					});
				} else {
					wallet	= new Clipperz.PM.DataModel.User.Header.Wallet({
						'name':	'wallet',
						'retrieveKeyFunction': MochiKit.Base.method(this, 'getPassphrase')
					});
				}
				
				break;
		}

		unpackedData = {
			'version': someServerData['version'],
			'statistics': someServerData['statistics'],
			'header': {
				'data': someServerData['header'],
				'version': headerVersion,
				
				'recordsIndex':		recordsIndex,
				'preferences':		preferences,
				'oneTimePasswords':	oneTimePasswords,
				'wallet':			wallet
			}
		};

		this._serverData = unpackedData;
		
		return this._serverData;
	},

	//-------------------------------------------------------------------------

	'getServerData': function() {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("User.getServerData", {trace:false});
		deferredResult.acquireLock(this.deferredLockForSection('serverData'));
		deferredResult.addCallback(MochiKit.Base.bind(function(aResult) {
			var innerDeferredResult;
		
			innerDeferredResult = new Clipperz.Async.Deferred("User.getUserDetails.innerDeferred", {trace:false});
			if (this._serverData == null) {
				innerDeferredResult.addCallbackPass(MochiKit.Signal.signal, this, 'loadingUserDetails');
				innerDeferredResult.addMethod(this.connection(), 'message', 'getUserDetails');
//				innerDeferredResult.addCallback(function (aValue) { console.log("USER DETAILS", aValue); return aValue; });
				innerDeferredResult.addMethod(this, 'unpackServerData');
//				innerDeferredResult.addCallback(function (aValue) { console.log("UNPACKED SERVER DATA", aValue); return aValue; });
				innerDeferredResult.addCallbackPass(MochiKit.Signal.signal, this, 'loadedUserDetails');
			}

			innerDeferredResult.addCallback(MochiKit.Base.bind(function () {
				return this._serverData;
			},this));
			innerDeferredResult.callback();

			return innerDeferredResult;
		}, this));
		deferredResult.releaseLock(this.deferredLockForSection('serverData'));
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'connectionVersion': function() {
		return this._connectionVersion;
	},
	
	'setConnectionVersion': function(aValue) {
		if (this._connectionVersion != aValue) {
			this.resetConnection();
		}
		this._connectionVersion = aValue;
	},

	//-------------------------------------------------------------------------

	'connection': function() {
		if ((this._connection == null) && (this.connectionVersion() != null) ){
			this._connection = new Clipperz.PM.Connection.communicationProtocol.versions[this.connectionVersion()]({
				getCredentialsFunction: MochiKit.Base.method(this, 'getCredentials')
			});
		}
				
		return this._connection;
	},

	'resetConnection': function(aValue) {
		if (this._connection != null) {
			this._connection.reset();
		}

		this._connection = null;
	},

	//=========================================================================
	
	'getHeaderIndex': function (aKey) {
		return Clipperz.Async.callbacks("User.getHeaderIndex", [
			MochiKit.Base.method(this, 'getServerData'),
//			function (aValue) { console.log("HEADER INDEX", aValue); return aValue; },
			MochiKit.Base.itemgetter('header'),
			MochiKit.Base.itemgetter(aKey)
		], {trace:false})
	},

	//=========================================================================

	'getTags': function (shouldIncludeArchivedCards) {
		var	archivedCardsFilter =	(shouldIncludeArchivedCards || false)
									?	MochiKit.Async.succeed
									:	MochiKit.Base.partial(MochiKit.Base.filter, function (someTags) {
											return someTags.indexOf(Clipperz.PM.DataModel.Record.archivedTag) == -1;
										});

		return Clipperz.Async.callbacks("User.getTags", [
			MochiKit.Base.method(this, 'getRecords'),
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller('tags')),
			Clipperz.Async.collectAll,
			archivedCardsFilter,
			MochiKit.Base.flattenArray,
			MochiKit.Iter.groupby,
			function (someGroups) {
				return  MochiKit.Iter.reduce(function(aCollector, aGroup) {
					var currentValue = aCollector[aGroup[0]] ? aCollector[aGroup[0]] : 0;
					aCollector[aGroup[0]] = MochiKit.Iter.list(aGroup[1]).length + currentValue;

					return aCollector;
				}, someGroups, {});
			}
		], {trace:false});
	},

	//=========================================================================

	'recordWithData': function (recordData) {
//console.log("recordWithData", recordData)
		return Clipperz.Async.callbacks("User.recordWithData", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'recordsIndex'),
			MochiKit.Base.methodcaller('records'),
			MochiKit.Base.itemgetter(recordData['reference']),
			MochiKit.Base.methodcaller('setRemoteData', recordData),
		], {trace:false});
		
	},

	//=========================================================================

	'getRecords': function () {
		return Clipperz.Async.callbacks("User.getRecords", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'recordsIndex'),
			MochiKit.Base.methodcaller('records'),
			MochiKit.Base.values
		], {trace:false});
	},

	'getRecordsLoadingAllData': function () {
		return Clipperz.Async.callbacks("User.getRecordsLoadingAllData", [
			MochiKit.Base.method(this.connection(), 'message', 'getAllRecordDetails'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this, 'recordWithData')),
			MochiKit.Base.method(this, 'getRecords'),
		], {trace:false});
	},

	'getRecordsInfo': function (someInfo) {
		return Clipperz.Async.callbacks("User.getRecordsInfo", [
			MochiKit.Base.method(this, 'getRecords'),
			MochiKit.Base.partial(MochiKit.Base.map, Clipperz.Async.collectResults("collectResults", someInfo, {trace:false})),
			Clipperz.Async.collectAll,
		], {trace:false});
	},

	'recordWithLabel': function (aLabel) {
		return Clipperz.Async.callbacks("User.recordWithLabel", [
			MochiKit.Base.method(this, 'getRecords'),
			MochiKit.Base.partial(Clipperz.Async.deferredFilter, function (aRecord) {
				return Clipperz.Async.callbacks("User.recordWithLabel - check record label", [
					MochiKit.Base.methodcaller('label'),
					MochiKit.Base.partial(MochiKit.Base.operator.eq, aLabel)
				], {trace:false}, aRecord);
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
Clipperz.log("Warning: User.recordWithLabel('" + aLabel + "') is returning more than one result: " + someFilteredResults.length);
						result = someFilteredResults[0];
						break;
				}
				
				return result;
			}
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'getRecord': function (aRecordReference) {
		return Clipperz.Async.callbacks("User.getRecord", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'recordsIndex'),
			MochiKit.Base.methodcaller('records'),
			MochiKit.Base.itemgetter(aRecordReference),
			
			Clipperz.Async.deferredIf("record != null", [
				MochiKit.Base.operator.identity
			], [
				function () { throw "Record does not exists"}
			])
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'getRecordDetail': function (aRecord) {
		return this.connection().message('getRecordDetail', {reference: aRecord.reference()});
	},

	//-------------------------------------------------------------------------

	'deleteRecord': function (aRecord) {
		return Clipperz.Async.callbacks("User.deleteRecord", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'recordsIndex'),
			MochiKit.Base.methodcaller('deleteRecord', aRecord)
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'createNewRecord': function () {
		return Clipperz.Async.callbacks("User.createNewRecord", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'recordsIndex'),
			MochiKit.Base.methodcaller('createNewRecord')
		], {trace:false});
	},

	//.........................................................................

	'createNewRecordFromJSON': function(someJSON) {
		var deferredResult;

		//	TODO: how do we handle attachments?
		deferredResult = new Clipperz.Async.Deferred("User.createNewRecordFromJSON", {trace:false});
		deferredResult.collectResults({
			'recordIndex': MochiKit.Base.method(this, 'getHeaderIndex', 'recordsIndex'),
			'newRecord': [
				MochiKit.Base.method(this, 'createNewRecord'),
				MochiKit.Base.methodcaller('setUpWithJSON', someJSON),
			]
		});
		deferredResult.addCallback(function (someInfo) {
			var	record = someInfo['newRecord'];
			var	recordIndex = someInfo['recordIndex'];
			
			return MochiKit.Base.map(function (aDirectLogin) {
				var	configuration = JSON.stringify({
					'page': {'title': aDirectLogin['label']},
					'form': aDirectLogin['formData'],
					'version': '0.2' // correct?
				});

				return Clipperz.Async.callbacks("User.createNewRecordFromJSON__inner", [
					//	TODO: check if we should invoke the create new direct login methon on Record instead of calling it directly on the index
					MochiKit.Base.method(recordIndex, 'createNewDirectLogin', record),
					MochiKit.Base.methodcaller('setLabel', aDirectLogin['label']),
					MochiKit.Base.methodcaller('setBookmarkletConfiguration', configuration),
					MochiKit.Base.methodcaller('setBindings', aDirectLogin['bindingData'], someJSON['currentVersion']['fields']),
				], {'trace': false});
			}, MochiKit.Base.values(someJSON.data.directLogins));
		});
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'cloneRecord': function (aRecord) {
		var	result;
		var	user = this;
		
		return Clipperz.Async.callbacks("User.cloneRecord", [
//			MochiKit.Base.method(this, 'hasPendingChanges'),
//			Clipperz.Async.deferredIf("User has pending changes", [
///				MochiKit.Async.fail
//			], [
//				MochiKit.Base.method(user, 'createNewRecord'),
//				MochiKit.Base.methodcaller('setUpWithRecord', aRecord),
//			])
			MochiKit.Base.method(user, 'createNewRecord'),
			MochiKit.Base.methodcaller('setUpWithRecord', aRecord),
		], {trace:false});
	},

	//=========================================================================

	'getDirectLogins': function() {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("User.getDirectLogins", {trace:false});
		deferredResult.addMethod(this, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.compose(MochiKit.Base.values, MochiKit.Base.methodcaller('directLogins')));
		deferredResult.addCallback(MochiKit.Base.flattenArray);
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'getOneTimePasswords': function () {
		return Clipperz.Async.callbacks("User.getOneTimePasswords", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'oneTimePasswords'),
			MochiKit.Base.methodcaller('oneTimePasswords'),
			MochiKit.Base.values
		], {trace:false});
	},

	'getOneTimePasswordsDetails': function() {
		return Clipperz.Async.callbacks("User.getOneTimePasswords", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'oneTimePasswords'),
			MochiKit.Base.methodcaller('oneTimePasswordsDetails', this.connection()),
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'createNewOTP': function () {
		var messageParameters;

		messageParameters = {};
		return Clipperz.Async.callbacks("User.createNewOTP", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'oneTimePasswords'),
			MochiKit.Base.methodcaller('createNewOTP', this.username(), MochiKit.Base.method(this, 'getPassphrase')),
			MochiKit.Base.methodcaller('encryptedData'),
			MochiKit.Base.partial(function(someParameters, someOTPEncryptedData) {
				someParameters['oneTimePassword'] = someOTPEncryptedData;
			}, messageParameters),
			MochiKit.Base.method(this, 'getPassphrase'),
			MochiKit.Base.method(this, 'prepareRemoteDataWithKey'),
			MochiKit.Base.partial(function(someParameters, someEncryptedRemoteData) {
				someParameters['user'] = someEncryptedRemoteData;
			}, messageParameters),
			MochiKit.Base.method(this.connection(), 'message', 'addNewOneTimePassword', messageParameters)
		], {trace:false});
	},

	'deleteOTPs': function (aList) {
		var messageParameters;

		messageParameters = {};
		return Clipperz.Async.callbacks("User.deleteOTPs", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'oneTimePasswords'),
			MochiKit.Base.methodcaller('deleteOTPs', aList),
			MochiKit.Base.partial(function(someParameters, aList) {
				someParameters['oneTimePasswords'] = aList
			}, messageParameters),
			MochiKit.Base.method(this, 'getPassphrase'),
			MochiKit.Base.method(this, 'prepareRemoteDataWithKey'),
			MochiKit.Base.partial(function(someParameters, someEncryptedRemoteData) {
				someParameters['user'] = someEncryptedRemoteData;
			}, messageParameters),
			MochiKit.Base.method(this.connection(), 'message', 'updateOneTimePasswords', messageParameters)			
		], {trace:false});
	},

	'changeOTPLabel': function (aReference, aLabel) {
		return Clipperz.Async.callbacks("User.changeOTPLabel", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'oneTimePasswords'),
			MochiKit.Base.methodcaller('changeOTPLabel', aReference, aLabel),
			MochiKit.Base.method(this,'saveChanges') // Too 'heavy'? Should be moved to MainController to prevent glitch in the UI? 
		], {trace:false});
	},

	//=========================================================================

	'invokeMethodNamedOnHeader': function (aMethodName, aValue) {
		return Clipperz.Async.collectResults("User.invokeMethodNamedOnHeader [" + aMethodName + "]", {
			'recordIndex': [
				MochiKit.Base.method(this, 'getHeaderIndex', 'recordsIndex'),
				MochiKit.Base.methodcaller(aMethodName, aValue)
			],
			'preferences': [
				MochiKit.Base.method(this, 'getHeaderIndex', 'preferences'),
				MochiKit.Base.methodcaller(aMethodName, aValue)
			],
			'oneTimePasswords': [
				MochiKit.Base.method(this, 'getHeaderIndex', 'oneTimePasswords'),
				MochiKit.Base.methodcaller(aMethodName, aValue)
			],
			'wallet': [
				MochiKit.Base.method(this, 'getHeaderIndex', 'wallet'),
				MochiKit.Base.methodcaller(aMethodName, aValue)
			]
//			'statistics': [
//				MochiKit.Base.method(this, 'getStatistics'),
//				MochiKit.Base.methodcaller(aMethodName, aValue)
//			]
		}, {trace:false})();
	},

	//-------------------------------------------------------------------------

	'invokeMethodNamedOnRecords': function (aMethodName, aValue) {
		return Clipperz.Async.callbacks("User.invokeMethodNamedOnRecords[" + aMethodName + "]", [
			MochiKit.Base.method(this, 'getRecords'),
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.methodcaller(aMethodName, aValue)),
			Clipperz.Async.collectAll
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	getPreferences: function() {
		return Clipperz.Async.callbacks("User.getPreferences", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'preferences'),
			MochiKit.Base.methodcaller('getPreferences')
		], {trace:false});
	},

	getPreference: function(aKey) {
		return Clipperz.Async.callbacks("User.getPreference", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'preferences'),
			MochiKit.Base.methodcaller('getPreference', aKey)
		], {trace:false});
	},

	setPreferences: function(anObject) {
		return Clipperz.Async.callbacks("User.setPreferences", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'preferences'),
			MochiKit.Base.methodcaller('setPreferences', anObject),
			MochiKit.Base.method(this, 'saveChanges')
		], {trace:false});
	},


	//-------------------------------------------------------------------------

	'uploadAttachment': function(aRecordReference, anAttachmentReference, someData) {
		// TODO: pass a callback to handle onProgress events (and modify Connection accordingly)
		this.connection().message('uploadAttachment', {
			'recordReference': aRecordReference,
			'attachmentReference': anAttachmentReference,
			'data': someData
		});
	},

	//=========================================================================

	getWallet: function () {
		return this.getHeaderIndex('wallet');
	},
	
	getUserMasterKey: function () {
//console.log("USER - GET USER MASTER KEY");
		return Clipperz.Async.callbacks("User.getPreferences", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'wallet'),
			MochiKit.Base.methodcaller('userMasterKey')
		], {trace:false});
	},

	createCertificateForRecord: function (aRecord) {
		var	cardCertificatePath = 'm/0';

		return Clipperz.Async.callbacks("User.createCertificateForRecord", [
			MochiKit.Base.method(this, 'getWallet'),
			MochiKit.Base.methodcaller('getNextID', cardCertificatePath),
			MochiKit.Base.method(aRecord, 'setID'),
			MochiKit.Base.method(aRecord, 'markAsCertified'),
			MochiKit.Base.method(this, 'getWallet'),
			MochiKit.Base.method(aRecord, 'computeCertificateInfo'),
			function (certificateInfo) { return {'certificateInfo': certificateInfo}; },
		], {trace:false});
	},

	//=========================================================================

	'hasPendingChanges': function () {
		var	deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("User.hasPendingChanges", {trace:false});
		deferredResult.collectResults({
			'header': [
				MochiKit.Base.method(this, 'invokeMethodNamedOnHeader', 'hasPendingChanges'),
				MochiKit.Base.values
			],
			'records': MochiKit.Base.method(this, 'invokeMethodNamedOnRecords', 'hasPendingChanges')
		});
		deferredResult.addCallback(Clipperz.Async.or);
		deferredResult.callback();
//		recordsIndex		= legacyHeader;
//		preferences			= legacyHeader;
//		oneTimePasswords	= legacyHeader;
		
		return deferredResult;
	},

	//=========================================================================

	'commitTransientState': function () {
		return Clipperz.Async.callbacks("User.commitTransientState", [
			MochiKit.Base.method(this, 'invokeMethodNamedOnHeader', 'commitTransientState'),
			MochiKit.Base.method(this, 'invokeMethodNamedOnRecords', 'commitTransientState'),

			MochiKit.Base.method(this, 'transientState'),
//			MochiKit.Base.itemgetter('lock'),
//			MochiKit.Base.method(this, 'setServerLockValue'),
			MochiKit.Base.method(this, 'resetTransientState', true)
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'revertChanges': function () {
		return Clipperz.Async.callbacks("User.revertChanges", [
			MochiKit.Base.method(this, 'invokeMethodNamedOnHeader', 'revertChanges'),
			MochiKit.Base.method(this, 'invokeMethodNamedOnRecords', 'revertChanges'),
			MochiKit.Base.method(this, 'resetTransientState', false),
		], {trace:false});
	},

	//=========================================================================

	'deleteAllCleanTextData': function () {
		return Clipperz.Async.callbacks("User.deleteAllCleanTextData", [
			MochiKit.Base.method(this, 'invokeMethodNamedOnRecords', 'deleteAllCleanTextData'),
			MochiKit.Base.method(this, 'invokeMethodNamedOnHeader',  'deleteAllCleanTextData'),

			MochiKit.Base.method(this.data(), 'removeAllData'),
			MochiKit.Base.method(this, 'resetTransientState', false)
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'hasAnyCleanTextData': function () {
		var	deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("User.hasAnyCleanTextData", {trace:false});
		deferredResult.collectResults({
			'header':	[
				MochiKit.Base.method(this, 'invokeMethodNamedOnHeader',  'hasAnyCleanTextData'),
				MochiKit.Base.values
			],
			'records':	MochiKit.Base.method(this, 'invokeMethodNamedOnRecords', 'hasAnyCleanTextData'),
			'data':		MochiKit.Base.bind(function () {
							return MochiKit.Async.succeed(! this.data().isEmpty());
						}, this),
			'transientState':	MochiKit.Base.bind(function () {
							return MochiKit.Async.succeed(MochiKit.Base.keys(this.transientState()).length != 0);
						}, this)
		});
		deferredResult.addCallback(Clipperz.Async.or);
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'prepareRemoteDataWithKey': function (aKey /*, aCurrentKey*/) {
		var deferredResult;
		var	result;

		result = {};
		deferredResult = new Clipperz.Async.Deferred("User.prepareRemoteDataWithKey", {trace:false});
		deferredResult.addMethod(this, 'invokeMethodNamedOnHeader', 'prepareRemoteDataWithKey', aKey /*, aCurrentKey*/);
		deferredResult.addCallback(MochiKit.Base.bind(function (aResult, someHeaderPackedData) {
			var header;

//console.log("USER - prepareRemoteDataWithKey", someHeaderPackedData);
			header = {};
			header['records']			= someHeaderPackedData['recordIndex']['records'];
			header['directLogins']		= someHeaderPackedData['recordIndex']['directLogins'];
			header['attachments']		= someHeaderPackedData['recordIndex']['attachments'];
			header['preferences']		= {'data': someHeaderPackedData['preferences']['data']};
			header['oneTimePasswords']	= {'data': someHeaderPackedData['oneTimePasswords']['data']};
			header['wallet']			= {'data': someHeaderPackedData['wallet']['data']};
			header['version']			= '0.1';

			aResult['header']		= Clipperz.Base.serializeJSON(header);
			aResult['statistics']	= this._serverData['statistics'];	//	"someHeaderPackedData['statistics']['data']";
			
			return aResult;
		}, this), result);
		deferredResult.addCallback(Clipperz.Async.setItem, result, 'version', Clipperz.PM.Crypto.encryptingFunctions.currentVersion);
//		deferredResult.addCallback(Clipperz.Async.setItem, result, 'lock', this.serverLockValue());
		deferredResult.callback();

		return deferredResult;
	},

	'prepareRemoteDataWithKeyFunction': function(aKeyFunction) {
		return new Clipperz.Async.callbacks("User.prepareRemoteDataWithKeyFunction", [
			aKeyFunction,
			MochiKit.Base.method(this, 'prepareRemoteDataWithKey')
		], {'trace': false})
	},

	//=========================================================================

	'saveChanges': function () {
		return this.saveChangesWithExtraParameters();
	},

	'saveChangesWithExtraParameters': function (extraParameters) {
		var	deferredResult;
		var messageParameters;
		
		messageParameters = {};
		if (typeof(extraParameters) != 'undefined') {
			messageParameters['extraParameters'] = extraParameters;
		}

		deferredResult = new Clipperz.Async.Deferred("User.saveChangaes", {trace:false});

		deferredResult.addMethod(this, 'getHeaderIndex', 'recordsIndex');
		deferredResult.addCallback(MochiKit.Base.methodcaller('prepareRemoteDataForChangedRecords'));
		deferredResult.addCallback(Clipperz.Async.setItem, messageParameters, 'records');

		deferredResult.addMethod(this, 'getPassphrase');
		deferredResult.addMethod(this, 'prepareRemoteDataWithKey');
		deferredResult.addCallback(Clipperz.Async.setItem, messageParameters, 'user');

		deferredResult.addCallback(MochiKit.Async.succeed, messageParameters);
		deferredResult.addMethod(this.connection(), 'message', 'saveChanges');
		deferredResult.addCallback(MochiKit.Base.update, this.transientState())

		deferredResult.addMethod(this, 'commitTransientState');
		deferredResult.addErrbackPass(MochiKit.Base.method(this, 'revertChanges'));
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});

//-----------------------------------------------------------------------------

Clipperz.PM.DataModel.User.registerNewAccount = function (anUsername, aPassphraseFunction) {
	var	deferredResult;
	var user;
	
	user = new Clipperz.PM.DataModel.User({'username':anUsername, 'getPassphraseFunction':aPassphraseFunction});

	deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.DataModel.User.registerNewAccount", {trace:false});
	deferredResult.addMethod(user, 'registerAsNewAccount');
	deferredResult.addMethod(user, 'login');
	deferredResult.addCallback(MochiKit.Async.succeed, user);
	deferredResult.callback();
	
	return deferredResult;
}

//-----------------------------------------------------------------------------

Clipperz.PM.DataModel.User.exception = {
	LoginFailed:				new MochiKit.Base.NamedError("Clipperz.PM.DataModel.User.exception.LoginFailed"),
	CredentialUpgradeFailed:	new MochiKit.Base.NamedError("Clipperz.PM.DataModel.User.exception.CredentialUpgradeFailed") 
};
	
//-----------------------------------------------------------------------------
