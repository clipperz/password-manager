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

Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();

var tests = {

    //-------------------------------------------------------------------------

	'simple_tests': function(someTestArgs) {
		var	deferredResult;
		var	encryptedRemoteObject;

		var	key;
		var version;
		var rawData;
		
		key = "just a random key";
		version = Clipperz.PM.Crypto.encryptingFunctions.currentVersion;
		rawData = "just a random text to encrypt";
		
		encryptedRemoteObject = new Clipperz.PM.DataModel.EncryptedRemoteObject({
			'retrieveKeyFunction':			MochiKit.Base.noop,
			'retrieveRemoteDataFunction':	function () { return  "--"},
			'encryptedDataKeypath':			'--',
			'encryptedVersionKeypath':		'--'
		});
		SimpleTest.ok(encryptedRemoteObject != null, "create an encryptedRemoteObject");
		
		deferredResult = Clipperz.Async.callbacks("EncryptedRemoteObject.test - simple_tests", [
			MochiKit.Base.method(encryptedRemoteObject, 'getRemoteData'),
			function(aResult) {
				SimpleTest.is(aResult, "--", "setting and getting serverData works");
			}
		], someTestArgs);

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'decrypt_test': function (someTestArgs) {
		var	deferredResult;
		var	encryptedRemoteObject;
		var	key;
		var version;
		var rawData;
		
		key = "just a random key";
		version = Clipperz.PM.Crypto.encryptingFunctions.currentVersion;
		rawData = "just a random text to encrypt";

		encryptedRemoteObject = new Clipperz.PM.DataModel.EncryptedRemoteObject({
			'retrieveKeyFunction':			MochiKit.Base.partial(MochiKit.Async.succeed, key),
			'retrieveRemoteDataFunction':	MochiKit.Base.partial(Clipperz.Async.callbacks, "EncryptedRemoteObject.test - decrypt_test <encrypt data>", [
				MochiKit.Base.partial(Clipperz.PM.Crypto.deferredEncrypt, {key:key, value:rawData, version:version}),
				function (someEncryptedData) {
					return {
						'data': someEncryptedData,
						'version': version
					}
				}
			], someTestArgs)//,
//			'encryptedDataKeypath':			'data',
//			'encryptedVersionKeypath':		'version'
		});

		deferredResult = new Clipperz.Async.Deferred("decrypt_test", someTestArgs);
		deferredResult.addMethod(encryptedRemoteObject, 'getDecryptedData');
		deferredResult.addCallback(function (aResult) {
			SimpleTest.is(aResult, rawData, "encrypt and decrypt works");
		})
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'getValue_test': function (someTestArgs) {
		var	deferredResult;
		var	encryptedRemoteObject;
		var	key;
		var version;
		var rawData;
		
		key = "just a random key";
		version = Clipperz.PM.Crypto.encryptingFunctions.currentVersion;
		rawData = {
			key1: 'value1',
			key2: 'value2'
		};

		encryptedRemoteObject = new Clipperz.PM.DataModel.EncryptedRemoteObject({
			'reference':					"testReference",
			'retrieveKeyFunction':			MochiKit.Base.partial(MochiKit.Async.succeed, key),
			'retrieveRemoteDataFunction':	MochiKit.Base.partial(Clipperz.Async.callbacks, "EncryptedRemoteObject.test - decrypt_test <encrypt data>", [
				MochiKit.Base.partial(Clipperz.PM.Crypto.deferredEncrypt, {key:key, value:rawData, version:version}),
				function (someEncryptedData) {
					return {
						'data': someEncryptedData,
						'version': version
					}
				}
			], someTestArgs)//,
//			'encryptedDataKeypath':			'data',
//			'encryptedVersionKeypath':		'version'
		});

		deferredResult = new Clipperz.Async.Deferred("decrypt_test", someTestArgs);

		deferredResult.addMethod(encryptedRemoteObject, 'getValue', 'key1');
		deferredResult.addTest('value1', "getting 'key1' works");
		deferredResult.addMethod(encryptedRemoteObject, 'hasAnyCleanTextData');
		deferredResult.addTest(true, "After accessing a value, hasAnyCleanTextData returns false");

		deferredResult.addMethod(encryptedRemoteObject, 'hasPendingChanges');
		deferredResult.addTest(false, "if nothing has changed, the object should return not pending changes");

		deferredResult.addMethod(encryptedRemoteObject, 'deleteAllCleanTextData');
		deferredResult.addMethod(encryptedRemoteObject, 'hasAnyCleanTextData');
		deferredResult.addTest(false, "After deleting all cleanTextData, hasAnyCleanTextData returns false");
		
		deferredResult.addMethod(encryptedRemoteObject, 'getValue', 'key1');
		deferredResult.addTest('value1', "getting 'key1' (also after a lock) works correctly");

		deferredResult.addMethod(encryptedRemoteObject, 'setValue', 'key1', 'new value1');
		deferredResult.addMethod(encryptedRemoteObject, 'getValue', 'key1');
		deferredResult.addTest('new value1', "after setting a new value, it is correctly returned");

		deferredResult.addMethod(encryptedRemoteObject, 'values');
		deferredResult.addCallback(function (someValues) {
			SimpleTest.is(someValues['key1'], 'new value1', "the value got straight from the objectStore is correct");
		});

		deferredResult.addMethod(encryptedRemoteObject, 'hasPendingChanges');
		deferredResult.addTest(true, "once a value has been changed, the object should return that there're some pending changes");

		deferredResult.addMethod(encryptedRemoteObject, 'revertChanges');
		deferredResult.addMethod(encryptedRemoteObject, 'hasPendingChanges');
		deferredResult.addTest(false, "reverting the changes should return the object to its initial state, without any peding changes");

		deferredResult.addMethod(encryptedRemoteObject, 'getValue', 'key1');
		deferredResult.addTest('value1', "also the value of the changed item has been restored");

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'encrypt_with_new_object_test': function (someTestArgs) {
		var	deferredResult;
		var	encryptedRemoteObject;
		var	key;
//		var version;
//		var rawData;
		
		key = "just a random key";
//		version = Clipperz.PM.Crypto.encryptingFunctions.currentVersion;
//		rawData = "just a random text to encrypt";

		encryptedRemoteObject = new Clipperz.PM.DataModel.EncryptedRemoteObject({
			'retrieveRemoteDataFunction':	MochiKit.Base.noop
		});

		encryptedRemoteObject.setValue('key1', "value1");
		encryptedRemoteObject.setValue('key2', "value2");
		encryptedRemoteObject.setValue('key3', "value3");

		deferredResult = new Clipperz.Async.Deferred("encrypt_with_new_object_test", someTestArgs);
		deferredResult.addMethod(encryptedRemoteObject, 'prepareRemoteDataWithKey', key);
		deferredResult.addCallback(MochiKit.Base.itemgetter('data'));
		deferredResult.collectResults({
			'key':		MochiKit.Base.partial(MochiKit.Async.succeed, key),
			'value':	MochiKit.Async.succeed,
			'version':	MochiKit.Base.partial(MochiKit.Async.succeed, Clipperz.PM.Crypto.encryptingFunctions.currentVersion)
		});
		deferredResult.addCallback(Clipperz.PM.Crypto.deferredDecrypt);
		deferredResult.addCallback(function (aResult) {
			SimpleTest.is(aResult['key1'], "value1", "encrypt and decrypt works for first element");
			SimpleTest.is(aResult['key2'], "value2", "encrypt and decrypt works for second element");
			SimpleTest.is(aResult['key3'], "value3", "encrypt and decrypt works for third element");
		})
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};

//#############################################################################

SimpleTest.runDeferredTests("Clipperz.PM.DataModel.EncryptedRemoteObject", tests, {trace:false});
