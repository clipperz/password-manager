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

// Clipperz.PM.DataModel.Record.tagChar = '#';	//	Simplify tests using a 'regular' char instead of an UTF-8 reserved one
Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();

var tests = {

    //-------------------------------------------------------------------------

	'recordUseOf_retrieveIndexDataFunction_and_getRemoteDataFunction_test': function (someTestArgs) {
		var deferredResult;
		var record;

//console.log("#### new Clipperz.PM.DataModel.Record [5]");
		record = new Clipperz.PM.DataModel.Record({
			'reference':					'<<< record reference >>>',
			'retrieveKeyFunction':			MochiKit.Base.noop,
			'retrieveRemoteDataFunction': 	function (aRecordReference) {
				SimpleTest.is(aRecordReference, '<<< record reference >>>', "Record correctly passes its record reference when asking for encrypted data");
				return MochiKit.Async.succeed({
					// fake server payload
					'data': "#### fake encrypted data ####",
					'version': "0.x",
					'currentVersion': {
						'reference': "<<< fake record version reference >>>",
						'data': "#### fake encrypted data ####",
						'version': "0.x"
					}
				});
			},
			'updateDate': "Thu, 10 May 2007 13:01:21 UTC",
			'accessDate': "Thu, 10 May 2007 13:01:21 UTC",
//			'encryptedDataKeypath':			'data',
//			'encryptedVersionKeypath':		'version',

			'retrieveIndexDataFunction':	function (aRecordReference) {
				SimpleTest.is(aRecordReference, '<<< record reference >>>', "Record correctly passes its record reference when asking for index data");
				return MochiKit.Async.succeed({
					key:	'<< key >>',
					label:	'<< label >>',
					notes:	'<< notes >>'
				});
			},
			'updateIndexDataFunction':		MochiKit.Base.noop

		});
		
		deferredResult = new Clipperz.Async.Deferred("recordUseOf_retrieveIndexDataFunction_and_getEncryptedDataFunction_test", someTestArgs);
		deferredResult.addMethod(record, 'label');
		deferredResult.addTest('<< label >>', "Record returns the right value for label");
		deferredResult.addMethod(record, 'notes');
		deferredResult.addTest('<< notes >>', "Record returns the right value for notes - even the legacy one, stored on the header");
		deferredResult.addMethod(record, 'getRemoteData');
		deferredResult.addCallback(Clipperz.Async.Test.isDeeply({ 'data': "#### fake encrypted data ####", 'version': "0.x", 'currentVersion': { 'reference': "<<< fake record version reference >>>", 'data': "#### fake encrypted data ####", 'version': "0.x" } }, "Record returns the expected encrypted data"));
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------
    'createNewRecord_test': function(someTestArgs) {
    	var deferredResult;
    	var proxy;
    	var user;

    	proxy =	new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
    	user =	new Clipperz.PM.DataModel.User({username:"1", getPassphraseFunction:MochiKit.Base.partial(MochiKit.Async.succeed, "1")});

    	deferredResult = new Clipperz.Async.Deferred("deleteOTP_test", someTestArgs);
    	deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['1/1_data']);

    	deferredResult.addMethod(user, 'login');
    	deferredResult.addMethod(user, 'createNewRecord');
    	deferredResult.setValue('newRecord');
    	deferredResult.addMethodcaller('setLabel', "A record");
    	deferredResult.getValue('newRecord');
    	deferredResult.addMethodcaller('addField', {'label':"Field 1", 'value':"Value 1", 'isHidden':false});
    	deferredResult.getValue('newRecord');
    	deferredResult.addMethodcaller('label');
    	deferredResult.addTest("A record", "Record returns the right value for label");

    	deferredResult.callback();

    	return deferredResult;
    },

    //-------------------------------------------------------------------------

	'createRecordWithoutAllRequiredParameters_test': function (someTestArgs) {
		var	record;
		
		try {
//console.log("#### new Clipperz.PM.DataModel.Record [6]");
			record = new Clipperz.PM.DataModel.Record({reference:'--'});
			SimpleTest.ok(false, "creating a record without all parameters should raise an exception");
		} catch(exception) {
//			SimpleTest.is(exception.name, "Clipperz.Base.exception.MandatoryParameter", "creating a record without all parameters raises an exception");
			SimpleTest.ok(/Clipperz\.Base\.exception\.MandatoryParameter.*/.test(exception.name), "creating a record without all parameters raises an exception");
		}
	},

    //-------------------------------------------------------------------------

	'recordFromOldData_version_0.1_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var recordID;

//console.log("#### new Clipperz.PM.DataModel.Record [7]");
/*
		record = new Clipperz.PM.DataModel.Record({
			'reference':	'05aad20ee399b11ddc923e601fcd1d096233634f2ad4c55db4f6435e5f9cc17a',
			'retrieveKeyFunction':			MochiKit.Base.partial(MochiKit.Async.succeed, 'e038652297f981d5ca917d88fa2c4c3251a12c0fa41bf7313a4d24a9738fe6c6'),
			'retrieveRemoteDataFunction':	MochiKit.Base.partial(MochiKit.Async.succeed, {
				'data': '4ec19a7093534e7dcf7c796b889283c6cec224b1895720ba3ff43ce091dc72c61fd5ea56def418ba3f15239f73228c6c8558585311f5e6673efe57482a1f9c9fe71e921576989eace671ec543685e3ad8f976bbfa4c2dbc629fab936c227d4fd4da3a1561ea79e553bae7b758ff91762572c1448a2d18bec797e12721238ef5ba18ddf1fba8ae773a8debe1040b3b158220aec6be9c7190687139f589a30d9c8887792fd7040e3c7cf3f9999fb9dde1f9f334d17c996996d538a7e374ac93135acafdaf5fce738a1702182897b63d2cb8e308b94156473cba63dcc557d17dcbdb55fcff63d9ba5edf68c42855052e34207d6fabe94fe024c3db616b45f494da42c62224d3897e320080072cc442d4212e7b1e8d5b3d9e3c25d48f4e7c37112ef4c6b2c0c8aff0bd3ce05694370e4378701463dde26c7c0322f8a9eb5a724106039b16b35050a9a9b5717b2eec803efa962b88b9655742f5e7b180ea567449671fb5a2ce563d8b47bc25705821938192eae420391c208182a788dd06fb6448b9858a4104a14efd7717671c65cd08fd979a4da7c01712bc5d4e949a10ef1ea65caf1f07cee34b063bab01bfb7a59047fef30c3059ea652f1c92b9e72aac515ac8851756703772e1fa05384ee7f0d5c7a3c',
				'version': '0.1',
				'currentVersion': {
					'reference': '05aad20ee399b11ddc923e601fcd1d096233634f2ad4c55db4f6435e5f9cc17a',
					'data': '4ec19a7093534e7dcf7c796b889283c6cec224b1895720ba3ff43ce091dc72c61fd5ea56def418ba3f15239f73228c6c8558585311f5e6673efe57482a1f9c9fe71e921576989eace671ec543685e3ad8f976bbfa4c2dbc629fab936c227d4fd4da3a1561ea79e553bae7b758ff91762572c1448a2d18bec797e12721238ef5ba18ddf1fba8ae773a8debe1040b3b158220aec6be9c7190687139f589a30d9c8887792fd7040e3c7cf3f9999fb9dde1f9f334d17c996996d538a7e374ac93135acafdaf5fce738a1702182897b63d2cb8e308b94156473cba63dcc557d17dcbdb55fcff63d9ba5edf68c42855052e34207d6fabe94fe024c3db616b45f494da42c62224d3897e320080072cc442d4212e7b1e8d5b3d9e3c25d48f4e7c37112ef4c6b2c0c8aff0bd3ce05694370e4378701463dde26c7c0322f8a9eb5a724106039b16b35050a9a9b5717b2eec803efa962b88b9655742f5e7b180ea567449671fb5a2ce563d8b47bc25705821938192eae420391c208182a788dd06fb6448b9858a4104a14efd7717671c65cd08fd979a4da7c01712bc5d4e949a10ef1ea65caf1f07cee34b063bab01bfb7a59047fef30c3059ea652f1c92b9e72aac515ac8851756703772e1fa05384ee7f0d5c7a3c',
					'version': '0.1'
				}
				
			}),

			'retrieveIndexDataFunction':	MochiKit.Base.partial(MochiKit.Async.succeed, {
//				'key':	 'e038652297f981d5ca917d88fa2c4c3251a12c0fa41bf7313a4d24a9738fe6c6',
				'label': '<< label >>',
				'notes': '<< notes >>'
			}),
			'updateIndexDataFunction':		MochiKit.Base.noop,
			'updateDate':					'Mon Oct 02 10:01:52 CEST 2006'
		});
*/
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
		recordID = "05aad20ee399b11ddc923e601fcd1d096233634f2ad4c55db4f6435e5f9cc17a";
		
		deferredResult = new Clipperz.Async.Deferred("recordFromOldData_version_0.1_test", someTestArgs);

		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_someExtraOldData']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasAnyCleanTextData');
		deferredResult.addTest(false, "When first loaded, the record has no clean text data");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('label');
		deferredResult.addTest("Card encoded with an old algorithm", "Record returns the right value for label");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('notes');
		deferredResult.addTest("", "Record returns the right value for notes - even the legacy one, stored on the header");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasAnyCleanTextData');
		deferredResult.addTest(true, "After reading some values, the record has some clean text data");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(function (someFields) {
			SimpleTest.is(MochiKit.Base.values(someFields).length, 6, "the card has 6 fields");
		});
		deferredResult.callback();
		
		return deferredResult;
		
	},

    //-------------------------------------------------------------------------

	'removeDirectLogin': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var user2;
		
		var recordID =		'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d';	//	YAHOO (4)
		var directLoginID =	'dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496';

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		user2 = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.removeDirectLogin", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(4, "The record initially has 4 direct logins");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(directLoginID));
		deferredResult.addMethodcaller('remove');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "removing a direct login to a record should result in pending changes on the record");

		deferredResult.addMethod(user, 'saveChanges');

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "after saving there should be not any pending changes");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(3, "after saving changes, the record should have only 3 direct logins");

		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user2, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(3, "also reloading all the data with a new user, the direct logins should always be 3");

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'removeDirectLoginAndRevertChanges': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		var recordID =		'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d';
		var directLoginID =	'dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496';
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.removeDirectLoginAndRevertChanges", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getDirectLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(22, "the user has 22 initially");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(4, "the selected record has 4 direct logins");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(directLoginID));
		deferredResult.addMethodcaller('remove');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "removing a direct login to a record should result in pending changes on the record");

		deferredResult.addMethod(user, 'revertChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "after reverting the changes, the user should not have pending changes");

		deferredResult.addMethod(user, 'getDirectLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(22, "after reverting the changes, the user should still have 22 direct logins");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(4, "after reverting the changes, the record should still have 4 direct logins");

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'addDirectLoginAndRevertChanges': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user, user2;
		var recordID =		'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d';	//	YAHOO (4)
		var directLoginReference;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		user2 = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.addDirectLoginAndRevertChanges", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.setValue('record');
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(4, "the record, when initially loaded, has 4 direct logins");

		deferredResult.getValue('record');
		deferredResult.addMethodcaller('createNewDirectLogin');
		deferredResult.setValue('directLogin');

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('setLabel', "New direct login");

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('setBookmarkletConfiguration', '{"page": {"title": "Parallels Account"}, "form": {"attributes": {"action": "https://www.parallels.com/account/", "method": "post"}, "inputs": [{"type": "text", "name": "Email", "value": ""}, {"type": "password", "name": "Password", "value": ""}]}, "version": "0.2.3"}');

		deferredResult.addMethod(user, 'revertChanges');
//deferredResult.addCallback(function () { console.log("###################################"); });
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "after reverting the changes, the user should NOT have pending changes");

		deferredResult.getValue('record');
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(false, "after reverting the changes, the record should NOT have pending changes");

		deferredResult.addMethod(user2, 'login');

		deferredResult.addMethod(user2, 'getRecord', recordID);
		deferredResult.setValue('record_2');
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(4, "the record, when reloaded from scratch, has still 4 direct logins");

		deferredResult.callback();

		return deferredResult;
	},
	
    //-------------------------------------------------------------------------

	'addDirectLoginAndSaveChanges': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user, user2;
		var recordID =		'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d';	//	YAHOO (4)
		var directLoginReference;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		user2 = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.addDirectLoginAndSaveChanges", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasAnyCleanTextData');
		deferredResult.addTest(false, "When first loaded, the record has no clean text data [2]");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.setValue('record');
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(4, "the selected record has 4 direct logins");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasAnyCleanTextData');
		deferredResult.addTest(false, "Still no clean text data is stored on the record, as all accessed data are stored on the index");

		deferredResult.addMethod(user, 'hasAnyCleanTextData');
		deferredResult.addTest(true, "the user has some clean text data");
	
		deferredResult.getValue('record');
		deferredResult.addMethodcaller('createNewDirectLogin');
		deferredResult.setValue('directLogin');

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('label');
		deferredResult.addTest(null, "The label of a initially created direct login is empty");

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('bookmarkletConfiguration');
		deferredResult.addTest('', "The bookmaraklet configuration of a initially created direct login is empty");

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('bindings');
		deferredResult.addCallback(Clipperz.Async.Test.isDeeply({}, "The bindings of a initially created direct login is empty"));

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('setLabel', "New direct login");

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('setBookmarkletConfiguration', directLoginConfigurations['Parallels']);

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('bindings');
		deferredResult.addCallback(MochiKit.Base.itemgetter('Email'));
		deferredResult.addMethodcaller('setFieldKey', '4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9');	//	"userID"

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('bindings');
		deferredResult.addCallback(MochiKit.Base.itemgetter('Password'));
		deferredResult.addMethodcaller('setFieldKey', 'ef2dee54322bf401540657d469e158a50e9228bc0a192a31d2e3ee56a77e565b');	//	"password"

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('favicon'),
		deferredResult.addTest('http://www.parallels.com/favicon.ico', "the original favicon is the expected one"),

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('reference');
		deferredResult.addCallback(function (aReference) {
			directLoginReference = aReference;
		});

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "after adding a new direct login, the user should have pending changes");

		deferredResult.getValue('record');
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "after adding a new direct login, the record should have pending changes");
		
		deferredResult.getValue('record');
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(5, "after adding a new direct login, the record has now 5 direct logins");


		deferredResult.addMethod(user, 'saveChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "after saving the changes, the user should NOT have pending changes");


		deferredResult.addMethod(user2, 'login');

		deferredResult.addMethod(user2, 'getRecord', recordID);
		deferredResult.setValue('record_2');
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(5, "the record, when reloaded from scratch, has still 5 direct logins");

		deferredResult.getValue('record_2');
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.itemgetter('4'));	//	TODO: accessing directLogins by index is not really nice
		deferredResult.setValue('directLogin_2');
		deferredResult.addMethodcaller('label');
		deferredResult.addTest('New direct login', "The label of the direct login has been correctly saved");

		deferredResult.getValue('directLogin_2');
		deferredResult.addMethodcaller('bindings');
		deferredResult.addCallback(MochiKit.Base.itemgetter('Email'));
		deferredResult.addMethodcaller('field');
		deferredResult.addMethodcaller('value');
		deferredResult.addTest('joe.clipperz', "The value bound to the direct login 'Email' field is correct");

		deferredResult.getValue('directLogin_2');
		deferredResult.addMethodcaller('bindings');
		deferredResult.addCallback(MochiKit.Base.itemgetter('Password'));
		deferredResult.addMethodcaller('field');
		deferredResult.addMethodcaller('value');
		deferredResult.addTest('enfvDG1RxAsl', "The value bound to the direct login 'Password' field is correct");

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'readDirectLoginAttributes': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		var recordID =		'2977aa5f99a9f6e5596c1bd7871e82d7328c3716c9ef8ba349ae65f10d97924e';
		var directLoginID =	'03251dc1cbc5398789e4c4b45c52cfac3fcd8c1a4f19a81fa68fc6feae31d55c';
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.readDirectLoginAttributes", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_with_multipleRecordVersions_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter('03251dc1cbc5398789e4c4b45c52cfac3fcd8c1a4f19a81fa68fc6feae31d55c'));
		deferredResult.setValue('directLogin');

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('inputs');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(13, "Amazon direct login has 13 inputs");

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('inputs');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.filter, MochiKit.Base.methodcaller('needsFormValue'));
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "Amazon direct login has 1 field needing a form value");

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('inputs');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.filter, MochiKit.Base.methodcaller('needsBinding'));
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(2, "Amazon direct login has 2 field needing a binding");


		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('bindings');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(2, "Amazon direct login has just two bindings");

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('bindings');
		deferredResult.addCallback(MochiKit.Base.itemgetter('email'));
		deferredResult.addMethodcaller('fieldKey');
		deferredResult.addTest('5e822c34aaf1a9fbc0b52585c1915f3a3758abd51923a4d35ae85373bbb839c2', "Amazon direct login 'email' binding points to the correct field");

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('bindings');
		deferredResult.addCallback(MochiKit.Base.itemgetter('password'));
		deferredResult.setValue('passwordBinding');
		deferredResult.addMethodcaller('fieldKey');
		deferredResult.addTest('01e4bb6dcf054f312c535de8160bcf50bdccd664bdc05721b10d4e69583765f7', "Amazon direct login 'password' binding points to the correct field");

		deferredResult.getValue('passwordBinding');
		deferredResult.addMethodcaller('field');
		deferredResult.addMethodcaller('label');
		deferredResult.addTest('password', "Amazon direct login 'password' binding points to the 'password' field");


		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('formValues');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "Amazon direct login has just one formValue");

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('formValues');
		deferredResult.addCallback(MochiKit.Base.itemgetter('action'));
		deferredResult.addMethodcaller('value');
		deferredResult.addTest('sign-in', "Amazon direct 'action' formValue is set to 'sign-in'");


		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'editDirectLoginLabel': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var oldLabel;
		var newLabel;

		var recordID =		'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d';
		var directLoginID =	'dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496';
		
		oldLabel = "Yahoo! Mail";
		newLabel = "YAHOO! Mail";

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.editDirectLoginLabel", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(directLoginID));
		deferredResult.addMethodcaller('label');
		deferredResult.addTest(oldLabel, "the current label of the direct login");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(directLoginID));
		deferredResult.addMethodcaller('setLabel', newLabel);

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "changing the label of a direct login should trigger some changes");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "changing the label of a direct login should trigger some changes also on the record itself");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(directLoginID));
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "changing the label of a direct login should trigger some changes also on the directLogin itself");

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'editDirectLoginFormValueAndRestoreChanges': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		var recordID =		'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d';
		var directLoginID =	'dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496';
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.editDirectLoginFormValueAndRestoreChanges", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(directLoginID));
		deferredResult.setValue('directLogin');
		deferredResult.addMethodcaller('formValues');
		deferredResult.addCallback(MochiKit.Base.itemgetter('.persistent'));
		deferredResult.addMethodcaller('value');
		deferredResult.addTest(null, "original formValue value matches");

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('formValues');
		deferredResult.addCallback(MochiKit.Base.itemgetter('.persistent'))
		deferredResult.addMethodcaller('setValue', 'y');

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('formValues');
		deferredResult.addCallback(MochiKit.Base.itemgetter('.persistent'))
		deferredResult.addMethodcaller('value');
		deferredResult.addTest('y', "the newly set value is retained");


		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "changing the label of a direct login should trigger some changes");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "changing the label of a direct login should trigger some changes also on the record itself");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(directLoginID));
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "changing the label of a direct login should trigger some changes also on the directLogin itself");

		deferredResult.addMethod(user, 'revertChanges');

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('formValues');
		deferredResult.addCallback(MochiKit.Base.itemgetter('.persistent'))
		deferredResult.addMethodcaller('value');
		deferredResult.addTest(null, "the old formValue value is correctly restored");

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'editDirectLoginConfigurationAndRevertChanges': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		var recordID =		'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d';
		var directLoginID =	'dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496';
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.editDirectLoginConfigurationAndRevertChanges", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(directLoginID));

		deferredResult.addCallback(function (aDirectLogin) {
//			var newBookmarkletConfiguration;

//			newBookmarkletConfiguration = '{\n  \"page\": {\n    \"title\": \"Yahoo! Mail\"\n  },\n  \"form\": {\n    \"attributes\": {\n      \"action\": \"https://login.yahoo.com/config/login?\",\n      \"method\": \"post\"\n    },\n    \"inputs\": [\n      {\n        \"type\": \"text\",\n        \"name\": \"login\",\n        \"value\": \"\"\n      },\n      {\n        \"type\": \"password\",\n        \"name\": \"passwd\",\n        \"value\": \"\"\n      },\n      {\n        \"type\": \"checkbox\",\n        \"name\": \".persistent\",\n        \"value\": \"y\"\n      }\n    ]\n  },\n  \"version\": \"0.2\"\n}';
			return Clipperz.Async.callbacks("Record.test.editDirectLoginConfiguration [inner call]", [
				MochiKit.Base.method(aDirectLogin, 'bookmarkletConfiguration'),
				Clipperz.Async.Test.is(directLoginConfigurations['Yahoo! Mail'], "the current bookmarkletConfiguration"),

				MochiKit.Base.method(aDirectLogin, 'favicon'),
				Clipperz.Async.Test.is('http://login.yahoo.com/favicon.ico', "the original favicon is the expected one"),

				MochiKit.Base.method(aDirectLogin, 'setBookmarkletConfiguration', directLoginConfigurations['Parallels']),

				MochiKit.Base.method(aDirectLogin, 'favicon'),
				Clipperz.Async.Test.is('http://login.yahoo.com/favicon.ico', "the original favicon is the expected one"),

				MochiKit.Base.method(aDirectLogin, 'hasPendingChanges'),
				Clipperz.Async.Test.ok("changing the configuration should trigger the pending changes on the direct login"),
				MochiKit.Base.method(aDirectLogin.record(), 'hasPendingChanges'),
				Clipperz.Async.Test.ok("changing the configuration should trigger the pending changes also on the record"),
				MochiKit.Base.method(user, 'hasPendingChanges'),
				Clipperz.Async.Test.ok("changing the configuration should trigger the pending changes also on the user"),

				MochiKit.Base.method(aDirectLogin, 'revertChanges'),
				
				MochiKit.Base.method(aDirectLogin, 'hasPendingChanges'),
				Clipperz.Async.Test.fail("reverting changes should reset pending changes on the direct login"),
				MochiKit.Base.method(aDirectLogin.record(), 'hasPendingChanges'),
				Clipperz.Async.Test.fail("reverting changes should reset pending changes also on the record"),
				MochiKit.Base.method(user, 'hasPendingChanges'),
				Clipperz.Async.Test.fail("reverting changes should reset pending changes also on the user"),

				MochiKit.Base.noop
			], someTestArgs);
		})

		deferredResult.callback();

		return deferredResult;
	},
	
    //-------------------------------------------------------------------------

	'editDirectLoginConfiguration_keepingTheSameStructure_AndSaveChanges': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var user2;
		var newBookmarkletConfiguration;

		var recordID =		'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d';
		var directLoginID =	'dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496';
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		user2 = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		newBookmarkletConfiguration = '{\n  \"page\": {\n    \"title\": \"Yahoo! Mail\"\n  },\n  \"form\": {\n    \"attributes\": {\n      \"action\": \"https://login.yahoo.com/config/login?\",\n      \"method\": \"post\"\n    },\n    \"inputs\": [\n      {\n        \"type\": \"text\",\n        \"name\": \"login\",\n        \"value\": \"\"\n      },\n      {\n        \"type\": \"password\",\n        \"name\": \"passwd\",\n        \"value\": \"\"\n      },\n      {\n        \"type\": \"checkbox\",\n        \"name\": \".persistent\",\n        \"value\": \"y\"\n      }\n    ]\n  },\n  \"version\": \"0.2\"\n}';

		deferredResult = new Clipperz.Async.Deferred("Record.test.editDirectLoginConfiguration_keepingTheSameStructure_AndSaveChanges", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(directLoginID));
		deferredResult.addCallback(function (aDirectLogin) {
			return Clipperz.Async.callbacks("Record.test.editDirectLoginConfiguration_keepingTheSameStructure_AndSaveChanges [inner call 1]", [
				MochiKit.Base.method(aDirectLogin, 'bookmarkletConfiguration'),
				Clipperz.Async.Test.is(directLoginConfigurations['Yahoo! Mail'], "the current bookmarkletConfiguration (1)"),

				MochiKit.Base.method(aDirectLogin, 'inputs'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(26, "The original direct login had 26 inputs"),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(2, "The original direct login had 2 inputs"),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.itemgetter('login'),
				MochiKit.Base.methodcaller('field'),
				MochiKit.Base.methodcaller('reference'),
				Clipperz.Async.Test.is('4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9', "the original 'login' direct login binding points to the correct field"),
				
				MochiKit.Base.method(aDirectLogin, 'formValues'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(1, "The original direct login had 1 form values"),

				MochiKit.Base.method(aDirectLogin, 'formValues'),
				MochiKit.Base.itemgetter('.persistent'),
				MochiKit.Base.methodcaller('type'),
				Clipperz.Async.Test.is('checkbox', "the original formValue has the expected type"),

				MochiKit.Base.method(aDirectLogin, 'formValues'),
				MochiKit.Base.itemgetter('.persistent'),
				MochiKit.Base.methodcaller('value'),
				Clipperz.Async.Test.is(null, "the original formValue is correct (1)"),

				MochiKit.Base.method(aDirectLogin, 'formValues'),
				MochiKit.Base.itemgetter('.persistent'),
				MochiKit.Base.methodcaller('setValue', 'y'),


				MochiKit.Base.method(aDirectLogin, 'setBookmarkletConfiguration', newBookmarkletConfiguration),
				MochiKit.Base.method(user, 'saveChanges'),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.itemgetter('login'),
				MochiKit.Base.methodcaller('field'),
				MochiKit.Base.methodcaller('reference'),
				Clipperz.Async.Test.is('4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9', "the 'login' binding is still valid after the new configuration is set"),
				
				MochiKit.Base.method(aDirectLogin, 'hasPendingChanges'),
				Clipperz.Async.Test.fail("reverting changes should reset pending changes on the direct login (2)"),
				MochiKit.Base.method(aDirectLogin.record(), 'hasPendingChanges'),
				Clipperz.Async.Test.fail("reverting changes should reset pending changes also on the record (2)"),
				MochiKit.Base.method(user, 'hasPendingChanges'),
				Clipperz.Async.Test.fail("reverting changes should reset pending changes also on the user (2)"),

				MochiKit.Base.noop
			], someTestArgs);
		})

		deferredResult.addMethod(user2, 'login');

		deferredResult.addMethod(user2, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(directLoginID));
		deferredResult.addCallback(function (aDirectLogin) {
			return Clipperz.Async.callbacks("Record.test.editDirectLoginConfiguration_keepingTheSameStructure_AndSaveChanges [inner call 2]", [
				MochiKit.Base.method(aDirectLogin, 'bookmarkletConfiguration'),
				Clipperz.Async.Test.is(newBookmarkletConfiguration, "the direct login has the new bookmarkletConfiguration even after being reloaded"),
				
				MochiKit.Base.method(aDirectLogin, 'inputs'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(3, "The new direct login has 3 inputs"),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(2, "The new direct login had 2 inputs"),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.itemgetter('login'),
				MochiKit.Base.methodcaller('field'),
				MochiKit.Base.methodcaller('reference'),
				Clipperz.Async.Test.is('4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9', "the new 'login' direct login binding still points to the correct field"),

				MochiKit.Base.method(aDirectLogin, 'formValues'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(1, "The new direct login had 1 form values (1)"),

				MochiKit.Base.method(aDirectLogin, 'formValues'),
				MochiKit.Base.itemgetter('.persistent'),
				MochiKit.Base.methodcaller('value'),
				Clipperz.Async.Test.is(null, "the formValue is still correctly set"),

				MochiKit.Base.noop
			], someTestArgs);
		});

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'editDirectLoginConfiguration_changingTheStructure_AndSaveChanges': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var user2;

		var recordID =		'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d';
		var directLoginID =	'dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496';
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		user2 = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.editDirectLoginConfiguration_changingTheStructure_AndSaveChanges", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(directLoginID));
		deferredResult.addCallback(function (aDirectLogin) {
			return Clipperz.Async.callbacks("Record.test.editDirectLoginConfiguration_changingTheStructure_AndSaveChanges [inner call 1]", [
				MochiKit.Base.method(aDirectLogin, 'bookmarkletConfiguration'),
				Clipperz.Async.Test.is(directLoginConfigurations['Yahoo! Mail'], "the current bookmarkletConfiguration (2)"),

				MochiKit.Base.method(aDirectLogin, 'inputs'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(26, "The original direct login had 26 inputs"),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(2, "The original direct login had 2 inputs"),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.itemgetter('login'),
				MochiKit.Base.methodcaller('field'),
				MochiKit.Base.methodcaller('reference'),
				Clipperz.Async.Test.is('4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9', "the original 'login' direct login binding points to the correct field"),
				
				MochiKit.Base.method(aDirectLogin, 'formValues'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(1, "The original direct login had 1 form values"),

				MochiKit.Base.method(aDirectLogin, 'formValues'),
				MochiKit.Base.itemgetter('.persistent'),
				MochiKit.Base.methodcaller('value'),
				Clipperz.Async.Test.is(null, "the original formValue is correct (2)"),


				MochiKit.Base.method(aDirectLogin, 'setBookmarkletConfiguration', directLoginConfigurations['Parallels']),
				MochiKit.Base.method(user, 'saveChanges'),

				MochiKit.Base.method(aDirectLogin, 'inputs'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(2, "The new direct login has 2 inputs"),

				MochiKit.Base.method(aDirectLogin, 'formValues'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(0, "The new direct login has no form values"),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(2, "The new direct login has 2 inputs"),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.itemgetter('login'),
				Clipperz.Async.Test.is(null, "the 'login' binding should not exist within the new configuration"),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.itemgetter('passwd'),
				Clipperz.Async.Test.is(null, "the 'passwd' binding should not exist within the new configuration"),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.itemgetter('Email'),
				Clipperz.Async.Test.ok("the 'Email' binding should exist within the new configuration"),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.itemgetter('Password'),
				Clipperz.Async.Test.ok("the 'Password' binding should exist within the new configuration"),


				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.itemgetter('Email'),
				MochiKit.Base.methodcaller('field'),
				Clipperz.Async.Test.is(null, "the 'Email' binding should not point to any field, yet"),

				
				MochiKit.Base.method(aDirectLogin, 'hasPendingChanges'),
				Clipperz.Async.Test.fail("reverting changes should reset pending changes on the direct login (2)"),
				MochiKit.Base.method(aDirectLogin.record(), 'hasPendingChanges'),
				Clipperz.Async.Test.fail("reverting changes should reset pending changes also on the record (2)"),
				MochiKit.Base.method(user, 'hasPendingChanges'),
				Clipperz.Async.Test.fail("reverting changes should reset pending changes also on the user (2)"),

				MochiKit.Base.noop
			], someTestArgs);
		})

		deferredResult.addMethod(user2, 'login');

		deferredResult.addMethod(user2, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(directLoginID));
		deferredResult.addCallback(function (aDirectLogin) {
			return Clipperz.Async.callbacks("Record.test.editDirectLoginConfiguration_changingTheStructure_AndSaveChanges [inner call 2]", [
				MochiKit.Base.method(aDirectLogin, 'bookmarkletConfiguration'),
				Clipperz.Base.evalJSON,
				MochiKit.Base.itemgetter('form'),
				Clipperz.Async.Test.isDeeply(Clipperz.Base.evalJSON(directLoginConfigurations['Parallels'])['form'], "the direct login has the new bookmarkletConfiguration even after being reloaded"),
				
				MochiKit.Base.method(aDirectLogin, 'inputs'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(2, "The new -reloaded- direct login has 2 inputs"),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(2, "The new direct login had 2 inputs"),

				MochiKit.Base.method(aDirectLogin, 'bindings'),
				MochiKit.Base.itemgetter('login'),
				Clipperz.Async.Test.is(null, "the 'login' binding should not exist within the new configuration"),

				MochiKit.Base.method(aDirectLogin, 'formValues'),
				MochiKit.Base.keys,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(0, "The new direct login has no form values (2)"),

				MochiKit.Base.noop
			], someTestArgs);
		});

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'editFieldValueAndRestoreIt': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		var recordID = 'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d';
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.editValueAndRestoreIt", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.itemgetter('4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9'));
		deferredResult.addMethodcaller('value');
		deferredResult.addTest('joe.clipperz', "the current field value is 'joe.clipperz'");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.itemgetter('4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9'));
		deferredResult.addMethodcaller('setValue', 'fake.clipperz');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "changing the value of a field should trigger pending changes");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.itemgetter('4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9'));
		deferredResult.addMethodcaller('setValue', 'joe.clipperz');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(false, "restoring the value of a field should revert pending changes");


		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'accessFieldValues': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		var recordID = 'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d';
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.accessFieldValues", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fieldWithLabel', 'userID');
		deferredResult.addMethodcaller('value');
		deferredResult.addTest('joe.clipperz', "the current field value is 'joe.clipperz'");


		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'editFieldValueAndSaveIt': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user, user2;

		var recordID =	'2977aa5f99a9f6e5596c1bd7871e82d7328c3716c9ef8ba349ae65f10d97924e';
		var passwordFieldID =	'01e4bb6dcf054f312c535de8160bcf50bdccd664bdc05721b10d4e69583765f7';
	
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		user2 = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.editValueAndRestoreIt", someTestArgs);
//		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_with_multipleRecordVersions_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getVersions');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(5, "the selected record has 5 versions");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.itemgetter(passwordFieldID));
		deferredResult.collectResults({
			'label': MochiKit.Base.methodcaller('label'),
			'value': MochiKit.Base.methodcaller('value')
		});
		deferredResult.addTest({'label': 'password', 'value': 'HRRd7ycaFVG6'},	"the current field label and value match", true);

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.itemgetter(passwordFieldID));
		deferredResult.addMethodcaller('setValue', '<<pippo>>');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "changing the value of a field should trigger pending changes");

		deferredResult.addMethod(user, 'saveChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "after saving, there should be no pending changes");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getVersions');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(6, "the selected record, after saving a new version, has now 6 versions");

		deferredResult.addMethod(user2, 'login');

		deferredResult.addMethod(user2, 'getRecord', recordID);
		deferredResult.addMethodcaller('getVersions');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(6, "the selected record - reloaded from the db - has 6 versions");

		deferredResult.addMethod(user2, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.itemgetter(passwordFieldID));
		deferredResult.collectResults({
			'label': MochiKit.Base.methodcaller('label'),
			'value': MochiKit.Base.methodcaller('value')
		});
		deferredResult.addTest({'label': 'password', 'value': '<<pippo>>'},	"the current field label and value match", true);
		
/*
deferredResult.addCallback(function (aValue) { console.log("FIELDS", aValue); return aValue});

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.itemgetter('4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9'));
		deferredResult.addMethodcaller('setValue', 'joe.clipperz');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(false, "restoring the value of a field should revert pending changes");
*/

		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'removeFieldAndRevertChanges': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user, user2;

		var recordID =	'2977aa5f99a9f6e5596c1bd7871e82d7328c3716c9ef8ba349ae65f10d97924e';
		var passwordFieldID =	'01e4bb6dcf054f312c535de8160bcf50bdccd664bdc05721b10d4e69583765f7';
	
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		user2 = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.removeFieldAndRevertChanges", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_with_multipleRecordVersions_data']);
//		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
//		deferredResult.addCallback(function (aValue) {console.log("FIELDS", aValue); return aValue; });
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(3, "the selected record has 3 fields");

		deferredResult.collectResults({
			'record':	MochiKit.Base.method(user, 'getRecord', recordID),
			'field': [
				MochiKit.Base.method(user, 'getRecord', recordID),
				MochiKit.Base.methodcaller('fields'),
				MochiKit.Base.itemgetter(passwordFieldID)
			]
		});
		deferredResult.addCallback(function (someInfo) {
			someInfo['record'].removeField(someInfo['field']);
		});

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(2, "the selected record has 2 fields left");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "the record reports pending changes");

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "the user reports pending changes too");

		deferredResult.addMethod(user, 'revertChanges');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(false, "the record no longer reports pending changes");

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "the user reports no pending changes");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(3, "the selected record should be back to have 3 fields");

/*
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.itemgetter(passwordFieldID));
		deferredResult.setValue('field');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.itemgetter(passwordFieldID));
		deferredResult.collectResults({
			'label': MochiKit.Base.methodcaller('label'),
			'value': MochiKit.Base.methodcaller('value')
		});
		deferredResult.addTest({'label': 'password', 'value': 'HRRd7ycaFVG6'},	"the current field label and value match", true);

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.itemgetter(passwordFieldID));
		deferredResult.addMethodcaller('setValue', '<<pippo>>');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "changing the value of a field should trigger pending changes");

		deferredResult.addMethod(user, 'saveChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "after saving, there should be no pending changes");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getVersions');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(6, "the selected record, after saving a new version, has now 6 versions");

		deferredResult.addMethod(user2, 'login');

		deferredResult.addMethod(user2, 'getRecord', recordID);
		deferredResult.addMethodcaller('getVersions');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(6, "the selected record - reloaded from the db - has 6 versions");

		deferredResult.addMethod(user2, 'getRecord', recordID);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.itemgetter(passwordFieldID));
		deferredResult.collectResults({
			'label': MochiKit.Base.methodcaller('label'),
			'value': MochiKit.Base.methodcaller('value')
		});
		deferredResult.addTest({'label': 'password', 'value': '<<pippo>>'},	"the current field label and value match", true);
*/		
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'editNotesAndRestoreIt': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		var recordID = 'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d';
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.editNotesAndRestoreIt", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('notes');
		deferredResult.addTest('', "the current note is the empty string even if nothing is set on the record");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('setNotes', '');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(false, "setting notes to an empty string should be the same has not having the notes altogether");

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'loadAllRecordVersions': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		var recordID = '2977aa5f99a9f6e5596c1bd7871e82d7328c3716c9ef8ba349ae65f10d97924e';
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.loadAllRecordVersions", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_with_multipleRecordVersions_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('label');
		deferredResult.addTest('Amazon.com', "the selected record is the expected one");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getVersions');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(5, "the 'Amazon' record has 5 versions");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getCurrentRecordVersion');
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(3, "the current version of 'Amazon' record has 3 fields");

		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'createDirectLoginAndDeleteItAfterward': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user, user2;
		var recordID =		'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d';	//	YAHOO (4)
		var directLoginReference;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		user2 = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.addDirectLoginAndRevertChanges", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.setValue('record');
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.keys);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(4, "the record, when initially loaded, has 4 direct logins");

		deferredResult.getValue('record');
		deferredResult.addMethodcaller('createNewDirectLogin');
		deferredResult.setValue('directLogin');

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('setLabel', "New direct login");

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('setBookmarkletConfiguration', '{"page": {"title": "Parallels Account"}, "form": {"attributes": {"action": "https://www.parallels.com/account/", "method": "post"}, "inputs": [{"type": "text", "name": "Email", "value": ""}, {"type": "password", "name": "Password", "value": ""}]}, "version": "0.2.3"}');

		deferredResult.getValue('directLogin');
		deferredResult.addMethodcaller('remove');

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "after reverting the changes, the user should NOT have pending changes");

		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'selectTags': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;

		var	filterRecordsWithRegExp = function (aUser, aRegExp) {
			var	innerDeferredResult;
			var	filterCards = function (aCardInfo) {
				aRegExp.lastIndex = 0;
				return aRegExp.test(aCardInfo[Clipperz.PM.DataModel.Record.defaultSearchField]);
			}

			innerDeferredResult = new Clipperz.Async.Deferred("Record.test.selectTags-filterRecordsWithTag", someTestArgs);
			innerDeferredResult.addMethod(aUser, 'getRecordsInfo', Clipperz.PM.DataModel.Record.defaultCardInfo, false);
			innerDeferredResult.addCallback(MochiKit.Base.filter, filterCards);
			innerDeferredResult.addMethodcaller('sort', Clipperz.Base.caseInsensitiveKeyComparator('label'));
			innerDeferredResult.callback();
	
			return innerDeferredResult;
		}

		var	filterRecordsWithTag = function (aUser, aTag) {
			return filterRecordsWithRegExp(aUser, Clipperz.PM.DataModel.Record.regExpForTag(aTag));
		}

		var	filterRecordsWithSearchTerm = function (aUser, aSearchTerm) {
			return filterRecordsWithRegExp(aUser, Clipperz.PM.DataModel.Record.regExpForSearch(aSearchTerm));
		}

		var tagChar = Clipperz.PM.DataModel.Record.tagChar;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'tag', getPassphraseFunction:function () { return 'tag';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.selectTags", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['tag/tag_accountWithTags']);

		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getTags');
		deferredResult.addCallback(SimpleTest.eq, {
			'Tag1': 3,
			'Tag2': 1,
			'Tag3': 2,
			'Tag4': 1
		});

		deferredResult.addMethod(user, 'getRecordsInfo', Clipperz.PM.DataModel.Record.defaultCardInfo);
		deferredResult.addCallback(MochiKit.Base.map, Clipperz.Base.itemgetter('_searchableContent'));
		deferredResult.addCallback(SimpleTest.eq, [
			'Card 1 '+tagChar+'Tag1 '+tagChar+'Tag2 ',
			'Card 2 '+tagChar+'Tag1 '+tagChar+'Tag3 ',
			'Card 3 '+tagChar+'Tag1 ',
			'Card 4 '+tagChar+'Tag3 ',
			'Card 5 '+tagChar+'Tag4 ',
			'Card 6 '
		]);

		deferredResult.addCallback(filterRecordsWithTag, user, 'Tag1');
		deferredResult.addCallback(MochiKit.Base.map, Clipperz.Base.itemgetter('label'));
		deferredResult.addCallback(SimpleTest.eq, ['Card 1', 'Card 2', 'Card 3']);

		deferredResult.addCallback(filterRecordsWithTag, user, 'Tag2');
		deferredResult.addCallback(MochiKit.Base.map, Clipperz.Base.itemgetter('label'));
		deferredResult.addCallback(SimpleTest.eq, ['Card 1']);

		deferredResult.addCallback(filterRecordsWithTag, user, 'Tag3');
		deferredResult.addCallback(MochiKit.Base.map, Clipperz.Base.itemgetter('label'));
		deferredResult.addCallback(SimpleTest.eq, ['Card 2', 'Card 4']);

		deferredResult.addCallback(filterRecordsWithTag, user, 'Tag4');
		deferredResult.addCallback(MochiKit.Base.map, Clipperz.Base.itemgetter('label'));
		deferredResult.addCallback(SimpleTest.eq, ['Card 5']);

		deferredResult.addCallback(filterRecordsWithTag, user, 'Tag5');
		deferredResult.addCallback(MochiKit.Base.map, Clipperz.Base.itemgetter('label'));
		deferredResult.addCallback(SimpleTest.eq, []);

		deferredResult.addCallback(filterRecordsWithSearchTerm, user, '3');
		deferredResult.addCallback(MochiKit.Base.map, Clipperz.Base.itemgetter('label'));
		deferredResult.addCallback(SimpleTest.eq, ['Card 2', 'Card 3', 'Card 4']);

		deferredResult.addCallback(filterRecordsWithSearchTerm, user, '3');
		deferredResult.addMethodcaller('sort', Clipperz.Base.reverseComparator(Clipperz.Base.caseInsensitiveKeyComparator('label')));
		deferredResult.addCallback(MochiKit.Base.map, Clipperz.Base.itemgetter('label'));
		deferredResult.addCallback(SimpleTest.eq, ['Card 4', 'Card 3', 'Card 2']);

		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'editTags_add': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;
		var user_2;
		var	recordID = '021c1512cd4eb8e05fd82d4b0d86cb6edcc7be0c06e32bc21ed74beafefdb33d';	//	fullLabel: "Card 1 #Tag1 #Tag2"

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'tag', getPassphraseFunction:function () { return 'tag';}});
		user_2  = new Clipperz.PM.DataModel.User({username:'tag', getPassphraseFunction:function () { return 'tag';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.removeDirectLogin", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['tag/tag_accountWithTags']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('tags');
		deferredResult.addCallback(SimpleTest.eq, ['Tag1', 'Tag2']);

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('label');
		deferredResult.addTest("Card 1", "expected label");
		
		deferredResult.addMethod(user, 'getRecord', recordID);
//deferredResult.addCallback(function (aValue) { console.log("VALUE", aValue); return aValue; });
		deferredResult.addMethodcaller('addTag', "TagX");
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('addTag', "A tag with spaces");
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('addTag', "ετικέτα");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('tags');
		deferredResult.addCallback(SimpleTest.eq, ['Tag1', 'Tag2', 'TagX', 'A tag with spaces', 'ετικέτα']);

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('setLabel', "Card 1 - edited");
		
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "user should have pending changes");
		deferredResult.addMethod(user, 'saveChanges');

		deferredResult.addMethod(user_2, 'login');
		deferredResult.addMethod(user_2, 'getRecord', recordID);
		deferredResult.addMethodcaller('tags');
		deferredResult.addCallback(SimpleTest.eq, ['Tag1', 'Tag2', 'TagX', 'A tag with spaces', 'ετικέτα']);

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'editTags_addSame': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;
		var	recordID = '021c1512cd4eb8e05fd82d4b0d86cb6edcc7be0c06e32bc21ed74beafefdb33d';	//	fullLabel: "Card 1 #Tag1 #Tag2"

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'tag', getPassphraseFunction:function () { return 'tag';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.removeDirectLogin", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['tag/tag_accountWithTags']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('tags');
		deferredResult.addCallback(SimpleTest.eq, ['Tag1', 'Tag2']);

		deferredResult.addMethod(user, 'getRecord', recordID);
//deferredResult.addCallback(function (aValue) { console.log("VALUE", aValue); return aValue; });
		deferredResult.addMethodcaller('addTag', "Tag1");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('tags');
		deferredResult.addCallback(SimpleTest.eq, ['Tag1', 'Tag2']);

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "user should have pending changes");
		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'editTags_addFirst': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;
		var user_2;
		var	recordID = 'b869b4b928e26b8c669d7e39da1df55406336b259edf19b032ee2e475347e8fa';	//	fullLabel: "Card 6"
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'tag', getPassphraseFunction:function () { return 'tag';}});
		user_2  = new Clipperz.PM.DataModel.User({username:'tag', getPassphraseFunction:function () { return 'tag';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.removeDirectLogin", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['tag/tag_accountWithTags']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('tags');
		deferredResult.addCallback(SimpleTest.eq, []);

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('addTag', "Tag1");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('tags');
		deferredResult.addCallback(SimpleTest.eq, ['Tag1']);

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "user should have pending changes");

		deferredResult.addMethod(user, 'getTags');
		deferredResult.addCallback(SimpleTest.eq, {
			'Tag1': 4,
			'Tag2': 1,
			'Tag3': 2,
			'Tag4': 1,
		});

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('addTag', "Tag5");

		deferredResult.addMethod(user, 'getTags');
		deferredResult.addCallback(SimpleTest.eq, {
			'Tag1': 4,
			'Tag2': 1,
			'Tag3': 2,
			'Tag4': 1,
			'Tag5': 1,
		});

		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'editTags_remove': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;
		var user_2;
		var	recordID = '021c1512cd4eb8e05fd82d4b0d86cb6edcc7be0c06e32bc21ed74beafefdb33d';	//	fullLabel: "Card 1 #Tag1 #Tag2"

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'tag', getPassphraseFunction:function () { return 'tag';}});
		user_2  = new Clipperz.PM.DataModel.User({username:'tag', getPassphraseFunction:function () { return 'tag';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.removeDirectLogin", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['tag/tag_accountWithTags']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('tags');
		deferredResult.addCallback(SimpleTest.eq, ['Tag1', 'Tag2']);

		deferredResult.addMethod(user, 'getRecord', recordID);
//deferredResult.addCallback(function (aValue) { console.log("VALUE", aValue); return aValue; });
		deferredResult.addMethodcaller('removeTag', "Tag1");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('tags');
		deferredResult.addCallback(SimpleTest.eq, ['Tag2']);

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "user should have pending changes");
		deferredResult.addMethod(user, 'saveChanges');

		deferredResult.addMethod(user_2, 'login');
		deferredResult.addMethod(user_2, 'getRecord', recordID);
		deferredResult.addMethodcaller('tags');
		deferredResult.addCallback(SimpleTest.eq, ['Tag2']);

		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'answerHasPendingChangesWithoutLoadingRemoteData': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;
		var	recordID = '021c1512cd4eb8e05fd82d4b0d86cb6edcc7be0c06e32bc21ed74beafefdb33d';	//	fullLabel: "Card 1 #Tag1 #Tag2"

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'tag', getPassphraseFunction:function () { return 'tag';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.answerHasPendingChangesWithoutLoadingRemoteData", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['tag/tag_accountWithTags']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasLoadedRemoteData');
		deferredResult.addTest(false, "Initially, the record should not have loaded remote data");

		deferredResult.addMethod(user, 'hasPendingChanges');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasLoadedRemoteData');
		deferredResult.addTest(false, "After chencking for pending changes, the record should have not loaded its remote data");

		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'changeFieldOrder': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;
		var user2;
		var recordID =		'd620764a656bfd4e1d3758500d5db72e460a0cf729d56ed1a7755b5725c50045'
//		var directLoginID =	'dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496';
		var originalFieldReference = '31d750b1944b65454a47ab10ad8b04ce99464c68b5f85bb015817ae7433b3940';

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		user2 = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.changeFieldOrder", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getFieldsValues');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
		deferredResult.addTest([
			"AAdvantage N.",
			"Password",
			"Web site",		// *
			"Call center",
			"Expire date"
		], "initial order of keys", true);

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fieldWithLabel', "Web site");
		deferredResult.addMethodcaller('reference');
		deferredResult.addTest(originalFieldReference, "Expected field reference");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('moveFieldToPosition', originalFieldReference, 0);

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "after changing the order of fields in a record, the user should report pending changes")

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "after changing the order of fields, also the record should report pending changes")

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getFieldsValues');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
//		deferredResult.addCallback(function (aValue) { console.log("[1] -> 0", aValue); return aValue; });
		deferredResult.addTest([
			"Web site",		// *
			"AAdvantage N.",
			"Password",
			"Call center",
			"Expire date"
		], "final order of keys [1]", true);

		deferredResult.addMethod(user, 'saveChanges');

		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getFieldsValues');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
//		deferredResult.addCallback(function (aValue) { console.log("[2] -> 0", aValue); return aValue; });
		deferredResult.addTest([
			"Web site",		// *
			"AAdvantage N.",
			"Password",
			"Call center",
			"Expire date"
		], "final order of keys [2]", true);

		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'changeFieldOrderToLast': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;
		var user2;
		var recordID =		'd620764a656bfd4e1d3758500d5db72e460a0cf729d56ed1a7755b5725c50045'
//		var directLoginID =	'dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496';
		var originalFieldReference = '31d750b1944b65454a47ab10ad8b04ce99464c68b5f85bb015817ae7433b3940';

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		user2 = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.changeFieldOrderToLast", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getFieldsValues');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
		deferredResult.addTest([
			"AAdvantage N.",
			"Password",
			"Web site",		// *
			"Call center",
			"Expire date",
		], "initial order of keys", true);

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('moveFieldToPosition', originalFieldReference, 0);
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getFieldsValues');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
//		deferredResult.addCallback(function (aValue) { console.log("[3] -> 0", aValue); return aValue; });
		deferredResult.addTest([
			"Web site",		// *
			"AAdvantage N.",
			"Password",
			"Call center",
			"Expire date",
		], "final order of keys [3]", true);
		deferredResult.addMethod(user, 'revertChanges');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('moveFieldToPosition', originalFieldReference, 1);
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getFieldsValues');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
//		deferredResult.addCallback(function (aValue) { console.log("[4] -> 1", aValue); return aValue; });
		deferredResult.addTest([
			"AAdvantage N.",
			"Web site",		// *
			"Password",
			"Call center",
			"Expire date",
		], "final order of keys [4]", true);
		deferredResult.addMethod(user, 'revertChanges');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('moveFieldToPosition', originalFieldReference, 2);
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getFieldsValues');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
//		deferredResult.addCallback(function (aValue) { console.log("[5] -> 2", aValue); return aValue; });
		deferredResult.addTest([
			"AAdvantage N.",
			"Password",
			"Web site",		// *
			"Call center",
			"Expire date",
		], "final order of keys [5]", true);
		deferredResult.addMethod(user, 'revertChanges');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('moveFieldToPosition', originalFieldReference, 3);
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getFieldsValues');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
//		deferredResult.addCallback(function (aValue) { console.log("[6] -> 3", aValue); return aValue; });
		deferredResult.addTest([
			"AAdvantage N.",
			"Password",
			"Call center",
			"Web site",		// *
			"Expire date",
		], "final order of keys [6]", true);
		deferredResult.addMethod(user, 'revertChanges');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('moveFieldToPosition', originalFieldReference, 4);
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getFieldsValues');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
//		deferredResult.addCallback(function (aValue) { console.log("[7] -> 4", aValue); return aValue; });
		deferredResult.addTest([
			"AAdvantage N.",
			"Password",
			"Call center",
			"Expire date",
			"Web site",		// *
		], "final order of keys [7]", true);
		deferredResult.addMethod(user, 'revertChanges');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('moveFieldToPosition', originalFieldReference, 10);
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getFieldsValues');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
//		deferredResult.addCallback(function (aValue) { console.log("[8] -> 10 -> 4", aValue); return aValue; });
		deferredResult.addTest([
			"AAdvantage N.",
			"Password",
			"Call center",
			"Expire date",
			"Web site",		// *
		], "final order of keys [8]", true);
		deferredResult.addMethod(user, 'revertChanges');

		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------
/*	NEW TEST
	'changeFieldOrderAndRevert': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;
		var recordID =		'327139a4d4cfbdb61c06b4cfa009f9cb05ef2f3e3703b6b071bcdb4213b2ca83'
		var originalFieldReference = 'bfd7624054e1eb6f1849082714f4016e300bce66645c7a7370276d82767cf125';

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'1', getPassphraseFunction:function () { return '1';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.changeFieldOrderAndRevert", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['1/1_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getCurrentRecordVersion');
		deferredResult.addMethodcaller('hasInitiatedObjectDataStore');
		deferredResult.addTest(false, "the current version of the record is not initiated if no data is accessed");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getCurrentRecordVersion');
		deferredResult.addMethodcaller('transientState');
		deferredResult.addMethodcaller('isEmpty');
		deferredResult.addTest(true, "the current version should initially have an empty Transient State");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('moveFieldToPosition', originalFieldReference, 0);

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getCurrentRecordVersion');
		deferredResult.addMethodcaller('transientState');
		deferredResult.addCallback(function (aValue) { console.log("TRANSIENT STATE", aValue); return aValue});
		deferredResult.addMethodcaller('isEmpty');
		deferredResult.addTest(false, "once a field has been moved, the current version should have a non empty Transient State");

/ *
		for (var i=0; i<2; i++) {
			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('moveFieldToPosition', originalFieldReference, 0);
			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('getFieldsValues');
			deferredResult.addCallback(MochiKit.Base.values);
			deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
	deferredResult.addCallback(function (aValue) { console.log("[changeFieldOrderAndRevert] -> 0", aValue); return aValue; });
			deferredResult.addTest([
				"Label 2",		// *
				"Label 1",
				"Label 3",
			], "final order of keys", true);

			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('getCurrentRecordVersion');
			deferredResult.addMethodcaller('hasInitiatedObjectDataStore');
			deferredResult.addTest(true, "once data is accessed, the version is initiated");

			deferredResult.addMethod(user, 'revertChanges');

			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('getCurrentRecordVersion');
			deferredResult.addMethodcaller('hasInitiatedObjectDataStore');
			deferredResult.addTest(false, "reverting changes, the current version object data store is de-initiated");


			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('getFieldsValues');
			deferredResult.addCallback(MochiKit.Base.values);
			deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
	deferredResult.addCallback(function (aValue) { console.log("[changeFieldOrderAndRevert] REVERTED", aValue); return aValue; });
			deferredResult.addTest([
				"Label 1",
				"Label 2",		// *
				"Label 3",
			], "initial order of keys", true);
		}
* /
		deferredResult.callback();

		return deferredResult;
	},
*/
	//-------------------------------------------------------------------------

	'deleteFieldAndRevert': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;
		var recordID =		'327139a4d4cfbdb61c06b4cfa009f9cb05ef2f3e3703b6b071bcdb4213b2ca83'
		var originalFieldReference = 'bfd7624054e1eb6f1849082714f4016e300bce66645c7a7370276d82767cf125';

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'1', getPassphraseFunction:function () { return '1';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.deleteFieldAndRevert", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['1/1_data']);
		deferredResult.addMethod(user, 'login');

		for (var i=0; i<2; i++) {
			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('removeField', { 'reference': function () { return originalFieldReference; } });

			deferredResult.addMethod(user, 'hasPendingChanges');
			deferredResult.addTest(true, "removing a field on a record trigger pending changes");

			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('getFieldsValues');
			deferredResult.addCallback(MochiKit.Base.values);
			deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
			deferredResult.addTest([
				"Label 1",
				"Label 3",
			], "Keys left", true);

			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('fields');
			deferredResult.addCallback(MochiKit.Base.values);
			deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
			deferredResult.addTest(2, "The record is left with two fields");

			deferredResult.addMethod(user, 'revertChanges');

			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('getFieldsValues');
			deferredResult.addCallback(MochiKit.Base.values);
			deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
			deferredResult.addTest([
				"Label 1",
				"Label 2",
				"Label 3",
			], "All fields restored", true);

			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('fields');
			deferredResult.addCallback(MochiKit.Base.values);
			deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
			deferredResult.addTest(3, "The record is left with two fields");

			deferredResult.addMethod(user, 'hasPendingChanges');
			deferredResult.addTest(false, "reverting changes discards not saved changes");
		}
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'addFieldAndRevert': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;
		var recordID =		'327139a4d4cfbdb61c06b4cfa009f9cb05ef2f3e3703b6b071bcdb4213b2ca83'
//		var originalFieldReference = 'bfd7624054e1eb6f1849082714f4016e300bce66645c7a7370276d82767cf125';

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'1', getPassphraseFunction:function () { return '1';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.addFieldAndRevert", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['1/1_data']);
		deferredResult.addMethod(user, 'login');

		for (var i=0; i<2; i++) {
			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('addField', { 'label':"Label 4", 'value':"Value 4", 'hidden':false });

			deferredResult.addMethod(user, 'hasPendingChanges');
			deferredResult.addTest(true, "addking a field on a record trigger pending changes");

			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('getFieldsValues');
			deferredResult.addCallback(MochiKit.Base.values);
			deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
			deferredResult.addTest([
				"Label 1",
				"Label 2",
				"Label 3",
				"Label 4",
			], "Keys left", true);

			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('fields');
			deferredResult.addCallback(MochiKit.Base.values);
			deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
			deferredResult.addTest(4, "The record is left with two fields");

			deferredResult.addMethod(user, 'revertChanges');

			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('getFieldsValues');
			deferredResult.addCallback(MochiKit.Base.values);
			deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
			deferredResult.addTest([
				"Label 1",
				"Label 2",
				"Label 3",
			], "All fields restored", true);

			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('fields');
			deferredResult.addCallback(MochiKit.Base.values);
			deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
			deferredResult.addTest(3, "The record is left with two fields");

			deferredResult.addMethod(user, 'hasPendingChanges');
			deferredResult.addTest(false, "reverting changes discards not saved changes");
		}
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'setNotesAndRevert': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;
		var recordID =		'327139a4d4cfbdb61c06b4cfa009f9cb05ef2f3e3703b6b071bcdb4213b2ca83'
//		var originalFieldReference = 'bfd7624054e1eb6f1849082714f4016e300bce66645c7a7370276d82767cf125';

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'1', getPassphraseFunction:function () { return '1';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.setNotesAndRevert", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['1/1_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('notes');
		deferredResult.addTest("", "The record has no notes");

		for (var i=0; i<2; i++) {
			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('setNotes', "New notes added to the record");

			deferredResult.addMethod(user, 'hasPendingChanges');
			deferredResult.addTest(true, "adding some notes to a record trigger pending changes");

			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('notes');
			deferredResult.addTest("New notes added to the record", "new notes value is stored");

			deferredResult.addMethod(user, 'revertChanges');

			deferredResult.addMethod(user, 'getRecord', recordID);
			deferredResult.addMethodcaller('notes');
			deferredResult.addTest("", "Restored original notes");

			deferredResult.addMethod(user, 'hasPendingChanges');
			deferredResult.addTest(false, "reverting changes discards not saved changes");
		}
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'changeFieldOrderSettingSamePosition': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;
		var user2;
		var recordID =		'd620764a656bfd4e1d3758500d5db72e460a0cf729d56ed1a7755b5725c50045'
//		var directLoginID =	'dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496';
		var originalFieldReference = '31d750b1944b65454a47ab10ad8b04ce99464c68b5f85bb015817ae7433b3940';

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		user2 = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.changeFieldOrderSettingSamePosition", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getFieldsValues');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
		deferredResult.addTest([
			"AAdvantage N.",
			"Password",
			"Web site",		// *
			"Call center",
			"Expire date"
		], "initial order of keys", true);

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('fieldWithLabel', "Web site");
		deferredResult.addMethodcaller('reference');
		deferredResult.addTest(originalFieldReference, "Expected field reference");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('moveFieldToPosition', originalFieldReference, 2);
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "changing the position of a field to its previous place should not trigger any changes");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(false, "also the record should not have any pending changes");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('getFieldsValues');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.itemgetter('label'));
//		deferredResult.addCallback(function (aValue) { console.log("[9]", aValue); return aValue; });
		deferredResult.addTest([
			"AAdvantage N.",
			"Password",
			"Web site",		// *
			"Call center",
			"Expire date"
		], "final order of keys [9]", true);

		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------
/*
	'sortFieldsThenCancelAndLoadSomeOtherRecord_test': function (someTestArgs) {
		var	deferredResult;
		var	proxy;
		var	user;
		var recordID_1 = 'd620764a656bfd4e1d3758500d5db72e460a0cf729d56ed1a7755b5725c50045';
		var recordID_2 = '6026370f3db3860d2c46a08e389a7e906dc14f98c8444b21be9a7e9f405a2728';
		
		var originalFieldReference = '31d750b1944b65454a47ab10ad8b04ce99464c68b5f85bb015817ae7433b3940';

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, isDefault:true, readOnly:false});
		user  = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("Record.test.sortFieldsThenCancelAndLoadSomeOtherRecord_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

//		deferredResult.addMethod(user, 'getRecord', recordID_2);
//		deferredResult.addMethodcaller('fields');
//deferredResult.addCallback(function () { console.log(">!> ========================="); });

		deferredResult.addMethod(user, 'getRecord', recordID_1);
		deferredResult.addMethodcaller('moveFieldToPosition', originalFieldReference, 0);
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "changing the position of a field to its previous place should not trigger any changes");

deferredResult.addCallback(function () { console.log(">>> ========================="); });

		deferredResult.addMethod(user, 'revertChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "changing the position of a field to its previous place should not trigger any changes");

deferredResult.addCallback(function () { console.log("<<< ========================="); });

		deferredResult.addMethod(user, 'getRecord', recordID_2);
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallbacks(
			MochiKit.Base.partial(SimpleTest.ok, true,	"Execution should complete without errors"),
			MochiKit.Base.partial(SimpleTest.ok, false,	"Execution should complete without errors")
		);

		deferredResult.callback();

		return deferredResult;
	},
*/
	//-------------------------------------------------------------------------

	'syntaxFix': MochiKit.Base.noop
};



//#############################################################################

SimpleTest.runDeferredTests("Clipperz.PM.DataModel.Record", tests, {trace:false});
