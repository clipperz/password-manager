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

Clipperz.PM.DataModel.User = function (args) {
	args = args || {};

	Clipperz.PM.DataModel.User.superclass.constructor.apply(this, arguments);

	this._username = args.username || null;
	this._getPassphraseFunction = args.getPassphraseFunction || null;

	this._data = null;

	this._connection = null;
	this._connectionVersion = 'current';

	this._serverData = null;
//	this._serverLockValue = null;
	this._transientState = null;

	this._deferredLocks = {
		'passphrase':			new MochiKit.Async.DeferredLock(),
		'serverData':			new MochiKit.Async.DeferredLock(),
//		'recordsIndex':			new MochiKit.Async.DeferredLock(),
//		'directLoginsIndex':	new MochiKit.Async.DeferredLock()
//		'preferences':			new MochiKit.Async.DeferredLock()
//		'oneTimePasswords':		new MochiKit.Async.DeferredLock()
		'__syntaxFix__': 'syntax fix'
	};

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
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("User.getPassphrase", {trace:false});
		deferredResult.acquireLock(this.deferredLockForSection('passphrase'));
		deferredResult.addMethod(this.data(), 'deferredGetOrSet', 'passphrase', this.getPassphraseFunction());
		deferredResult.releaseLock(this.deferredLockForSection('passphrase'));
		deferredResult.callback();

		return deferredResult;
	},

	'getPassphraseFunction': function () {
		return this._getPassphraseFunction;
	},

	//-------------------------------------------------------------------------

	'getCredentials': function () {
		return Clipperz.Async.collectResults("User; get username and passphrase", {
			'username': MochiKit.Base.method(this, 'username'),
			'password': MochiKit.Base.method(this, 'getPassphrase')
		}, {trace:false})();
	},

	//-------------------------------------------------------------------------

	'changePassphrase': function (aNewValue) {
		return this.updateCredentials(this.username(), aNewValue);
	},

	//.........................................................................

	'updateCredentials': function (aUsername, aPassphrase) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("User.updateCredentials", {trace:false});
//		deferredResult.addMethod(this, 'getPassphrase');
//		deferredResult.setValue('currentPassphrase');
		deferredResult.addMethod(this.connection(), 'ping');
		deferredResult.addMethod(this, 'setUsername', aUsername)
		deferredResult.acquireLock(this.deferredLockForSection('passphrase'));
		deferredResult.addMethod(this.data(), 'deferredGetOrSet', 'passphrase', aPassphrase);
		deferredResult.releaseLock(this.deferredLockForSection('passphrase'));
//		deferredResult.getValue('currentPassphrase');
		deferredResult.addMethod(this, 'prepareRemoteDataWithKey', aPassphrase);
		deferredResult.addMethod(this.connection(), 'updateCredentials', aUsername, aPassphrase);
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
					'encryptedDataVersion':			Clipperz.PM.Crypto.encryptingFunctions.currentVersion,
					'retrieveRecordDetailFunction':	MochiKit.Base.method(this, 'getRecordDetail')
				}),
				'preferences': new Clipperz.PM.DataModel.User.Header.Preferences({
					'name':	'preferences',
					'retrieveKeyFunction': MochiKit.Base.method(this, 'getPassphrase')
				}),
				'oneTimePasswords': new Clipperz.PM.DataModel.User.Header.OneTimePasswords({
					'name':	'preferences',
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
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'updateProgress', {'extraSteps':3});
		deferredResult.addMethod(this, 'initialSetupWithNoData')
		deferredResult.addMethod(this, 'getPassphrase');
		deferredResult.addMethod(this, 'prepareRemoteDataWithKey');
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');
		deferredResult.addMethod(this.connection(), 'register');
//		deferredResult.addCallback(MochiKit.Base.itemgetter('lock'));
//		deferredResult.addMethod(this, 'setServerLockValue');
		deferredResult.addCallbackPass(MochiKit.Signal.signal,	Clipperz.Signal.NotificationCenter, 'userSuccessfullyRegistered');

//		deferredResult.addErrback (MochiKit.Base.method(this, 'handleRegistrationFailure'));

		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'login': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("User.login", {trace:false});
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'updateProgress', {'extraSteps':3});
		deferredResult.addMethod(this, 'getPassphrase');
		deferredResult.addCallback(Clipperz.PM.DataModel.OneTimePassword.isValidOneTimePasswordValue);
		deferredResult.addCallback(Clipperz.Async.deferredIf("Is the passphrase an OTP", [
			MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'updateProgress', {'extraSteps':1}),
			MochiKit.Base.method(this, 'getCredentials'),
			MochiKit.Base.method(this.connection(), 'redeemOneTimePassword'),
			MochiKit.Base.method(this.data(), 'setValue', 'passphrase')
		], []));
		deferredResult.addErrback(MochiKit.Base.method(this, 'getPassphrase'));
		deferredResult.addMethod(this.connection(), 'login', false);
		deferredResult.addCallbackPass(MochiKit.Signal.signal,	Clipperz.Signal.NotificationCenter, 'userSuccessfullyLoggedIn');
		deferredResult.addErrback (MochiKit.Base.method(this, 'handleConnectionFallback'));

		deferredResult.callback();
		
		return deferredResult;
	},

	//.........................................................................

	'handleConnectionFallback': function(aValue) {
		var result;

		if (aValue instanceof MochiKit.Async.CancelledError) {
			result = aValue;
		} else {
			this.setConnectionVersion(Clipperz.PM.Connection.communicationProtocol.fallbackVersions[this.connectionVersion()]);

			if (this.connectionVersion() != null) {
				result = new Clipperz.Async.Deferred("User.handleConnectionFallback - retry");
				
				result.addMethod(this, 'login');
				result.callback();
			} else {
				result = Clipperz.Async.callbacks("User.handleConnectionFallback - failed", [
					MochiKit.Base.method(this.data(), 'removeValue', 'passphrase'),
					MochiKit.Base.method(this, 'setConnectionVersion', 'current'),
					MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'userLoginFailed'),
					MochiKit.Base.partial(MochiKit.Async.fail, Clipperz.PM.DataModel.User.exception.LoginFailed)
				], {trace:false});
			}
		}

		return result;
	},

	//-------------------------------------------------------------------------

	'lock': function () {
		return Clipperz.Async.callbacks("User.lock", [
			MochiKit.Base.method(this, 'deleteAllCleanTextData')
		], {trace:false});
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
		var preferences;
		var oneTimePasswords;

//		this.setServerLockValue(someServerData['lock']);
console.log("USER.unpackServerData", someServerData);
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
				break;
			case '0.1':
				var	headerData;
				
				headerData = Clipperz.Base.evalJSON(someServerData['header']);

				recordsIndex = new Clipperz.PM.DataModel.User.Header.RecordIndex({
					'retrieveKeyFunction':			MochiKit.Base.method(this, 'getPassphrase'),
					'recordsData':					headerData['records'],
					'recordsStats':					someServerData['recordsStats'],
					'directLoginsData':				headerData['directLogins'],
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
						'name':	'preferences',
						'retrieveKeyFunction': MochiKit.Base.method(this, 'getPassphrase'),
						'remoteData': {
							'data': headerData['oneTimePasswords']['data'],
							'version': someServerData['version']
						}
					});
				} else {
					oneTimePasswords = new Clipperz.PM.DataModel.User.Header.OneTimePasswords({
						'name':	'preferences',
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
				
				'recordsIndex': recordsIndex,
				'preferences': preferences,
				'oneTimePasswords': oneTimePasswords
			},
//			'accountInfo': new Clipperz.PM.DataModel.User.AccountInfo(someServerData['accountInfo']),
			'offlineCopyNeeded': someServerData['offlineCopyNeeded']
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
				innerDeferredResult.addMethod(this, 'unpackServerData');
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

	'resetAllData': function () {
		this.deleteAllCleanTextData();
		this._serverData = null;
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
			MochiKit.Base.itemgetter('header'),
			MochiKit.Base.itemgetter(aKey)
		], {trace:false})
	},

	'getCurrentAccountInfo': function () {
		return Clipperz.Async.callbacks("User.getHeaderIndex", [
			MochiKit.Base.method(this, 'getServerData'),
			MochiKit.Base.itemgetter('accountInfo')
		], {trace:false})
	},

	//=========================================================================

	'getRecords': function () {
		return Clipperz.Async.callbacks("User.getRecords", [
			MochiKit.Base.method(this, 'getHeaderIndex', 'recordsIndex'),
			MochiKit.Base.methodcaller('records'),
			MochiKit.Base.values
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
						WTF = TODO;
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

	'getRecordDetail': function (aRecordReference) {
		return this.connection().message('getRecordDetail', {reference: aRecordReference});
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
			]//,
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
			MochiKit.Base.method(this, 'resetTransientState', false)
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

			header = {};
			header['records']			= someHeaderPackedData['recordIndex']['records'];
			header['directLogins']		= someHeaderPackedData['recordIndex']['directLogins'];
			header['preferences']		= {'data': someHeaderPackedData['preferences']['data']};
			header['oneTimePasswords']	= {'data': someHeaderPackedData['oneTimePasswords']['data']};
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

	//=========================================================================

	'saveChanges': function () {
		var	deferredResult;
		var messageParameters;
		
		messageParameters = {};

		deferredResult = new Clipperz.Async.Deferred("User.saveChangaes", {trace:false});

		deferredResult.addMethod(this, 'getHeaderIndex', 'recordsIndex');
		deferredResult.addCallback(MochiKit.Base.methodcaller('prepareRemoteDataForChangedRecords'));
		deferredResult.addCallback(Clipperz.Async.setItem, messageParameters, 'records');
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');

		deferredResult.addMethod(this, 'getPassphrase');
		deferredResult.addMethod(this, 'prepareRemoteDataWithKey');
		deferredResult.addCallback(Clipperz.Async.setItem, messageParameters, 'user');
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');

		deferredResult.addCallback(MochiKit.Async.succeed, messageParameters);
		deferredResult.addMethod(this.connection(), 'message', 'saveChanges');
		deferredResult.addCallback(MochiKit.Base.update, this.transientState())
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');

		deferredResult.addMethod(this, 'commitTransientState');
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'userDataSuccessfullySaved');

		deferredResult.addErrbackPass(MochiKit.Base.method(this, 'revertChanges'));
		deferredResult.addErrbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'failureWhileSavingUserData');

		deferredResult.callback();

		return deferredResult;
	},

	'deleteAccount': function() {
		var deferredResult;
		
//MochiKit.Logging.logDebug(">>> user.deleteAccountAction - " + this);
		this.deleteAllCleanTextData();
		deferredResult = new Clipperz.Async.Deferred("User.deleteAccount", {trace:false});
		deferredResult.addCallback(MochiKit.Base.method(this.connection(), 'message'), 'deleteUser');
//		deferredResult.addCallback(MochiKit.Base.method(this, 'resetAllLocalData'));
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< user.deleteAccountAction - " + this);

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
