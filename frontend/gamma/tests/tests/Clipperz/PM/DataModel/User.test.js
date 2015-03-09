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

	'invertIndex_test': function (someTestArgs) {
		var testIndex;
		var invertedIndex;
		
		testIndex = {
			'key1': 'value1',
			'key2': 'value2',
			'key3': 'value3',
			'key4': 'value4',
			'key5': 'value5'
		};
		
		invertedIndex = Clipperz.PM.DataModel.User.Header.RecordIndex.invertIndex(testIndex);
		
		SimpleTest.is(MochiKit.Base.keys(invertedIndex).length, MochiKit.Base.keys(testIndex).length, "the inverted index has the same number of elements as the original index");
		SimpleTest.is(invertedIndex['value1'], 'key1', "the first element has been correctly inverted");
		SimpleTest.is(invertedIndex['value2'], 'key2', "the second element has been correctly inverted");
		SimpleTest.is(invertedIndex['value3'], 'key3', "the third element has been correctly inverted");
		SimpleTest.is(invertedIndex['value4'], 'key4', "the forth element has been correctly inverted");
		SimpleTest.is(invertedIndex['value5'], 'key5', "the fifth element has been correctly inverted");
		return MochiKit.Async.succeed('done');
	},

    //-------------------------------------------------------------------------

	'joe_clipperz_offline_copy_test': function(someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("joe_clipperz_offline_copy_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			is(someRecords.length,	20,	"joe_clipperz_offline_copy_test - joe has 20 records");
		});

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.methodcaller('label'));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addCallback(MochiKit.Base.methodcaller('sort'));
		deferredResult.addCallback(function (someSortedLabels) {
			SimpleTest.is(someSortedLabels.length, 20, "We got all the labels");
			SimpleTest.is(someSortedLabels[0],	"Amazon.com",				"The first label is correct");
			SimpleTest.is(someSortedLabels[1],	"American Airlines",		"The second label is correct");
			SimpleTest.is(someSortedLabels[2],	"Bloglines",				"The third label is correct");
			SimpleTest.is(someSortedLabels[3],	"Digg",						"The fourth label is correct");
			SimpleTest.is(someSortedLabels[4],	"Example Attack",			"The fifth label is correct");
			SimpleTest.is(someSortedLabels[5],	"Expedia.com",				"The sixth label is correct");
			SimpleTest.is(someSortedLabels[6],	"Google Account",			"The seventh label is correct");
			SimpleTest.is(someSortedLabels[7],	"Home burglar alarm",		"The eighth label is correct");
			SimpleTest.is(someSortedLabels[8],	"Jaiku",					"The ninth label is correct");
			SimpleTest.is(someSortedLabels[9],	"LinkedIn",					"The 10th label is correct");
			SimpleTest.is(someSortedLabels[10],	"Lufthansa",				"The 11th label is correct");
			SimpleTest.is(someSortedLabels[11],	"Microsoft Office CD Key",	"The 12th label is correct");
			SimpleTest.is(someSortedLabels[12],	"MyBlogLog",				"The 13th label is correct");
			SimpleTest.is(someSortedLabels[13],	"MySpace",					"The 14th label is correct");
			SimpleTest.is(someSortedLabels[14],	"SAP - Login",				"The 15th label is correct");
			SimpleTest.is(someSortedLabels[15],	"The New York Times",		"The 16th label is correct");
			SimpleTest.is(someSortedLabels[16],	"Web password",				"The 17th label is correct");
			SimpleTest.is(someSortedLabels[17],	"Web password",				"The 18th label is correct");
			SimpleTest.is(someSortedLabels[18],	"Yahoo! Account",			"The 19th label is correct");
			SimpleTest.is(someSortedLabels[19],	"del.icio.us",				"The 20th label is correct");
		});
		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'joe_clipperz_offline_copy_getDirectLogins_test': function(someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("joe_clipperz_offline_copy_getDirectLogins_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getDirectLogins');
		deferredResult.addCallback(function (someDirectLogins) {
			SimpleTest.is(someDirectLogins.length,	22,	"joe has 22 direct logins");
		});

		deferredResult.addMethod(user, 'getDirectLogins');
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.methodcaller('label'));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addCallback(MochiKit.Base.methodcaller('sort'));
		deferredResult.addCallback(function (someSortedLabels) {
			SimpleTest.is(someSortedLabels.length, 22, "We got all the labels");
			SimpleTest.is(someSortedLabels[0],	"Amazon.com",			"The first label is correct");
			SimpleTest.is(someSortedLabels[1],	"American Airlines",	"The second label is correct");
			SimpleTest.is(someSortedLabels[2],	"Bloglines",			"The third label is correct");
			SimpleTest.is(someSortedLabels[3],	"Digg",					"The fourth label is correct");
			SimpleTest.is(someSortedLabels[4],	"Example Attack",		"The 5th label is correct");
			SimpleTest.is(someSortedLabels[5],	"Expedia.com",			"The 6th label is correct");
			SimpleTest.is(someSortedLabels[6],	"Flickr",				"The 7th label is correct");
			SimpleTest.is(someSortedLabels[7],	"Google Account",		"The 8th label is correct");
			SimpleTest.is(someSortedLabels[8],	"Google Calendar",		"The 9th label is correct");
			SimpleTest.is(someSortedLabels[9],  "Google Docs",			"The 10th label is correct");
			SimpleTest.is(someSortedLabels[10],	"Google Mail",			"The 11th label is correct");
			SimpleTest.is(someSortedLabels[11],	"Jaiku",				"The 12th label is correct");
			SimpleTest.is(someSortedLabels[12],	"LinkedIn",				"The 13th label is correct");
			SimpleTest.is(someSortedLabels[13],	"Lufthansa",			"The 14th label is correct");
			SimpleTest.is(someSortedLabels[14],	"My Yahoo!",			"The 15th label is correct");
			SimpleTest.is(someSortedLabels[15],	"MyBlogLog",			"The 16th label is correct");
			SimpleTest.is(someSortedLabels[16],	"MySpace",				"The 17th label is correct");
			SimpleTest.is(someSortedLabels[17],	"SAP - Login",			"The 18th label is correct");
			SimpleTest.is(someSortedLabels[18],	"SAP - Login",			"The 19th label is correct");
			SimpleTest.is(someSortedLabels[19],	"The New York Times",	"The 20th label is correct");
			SimpleTest.is(someSortedLabels[20],	"Yahoo! Groups",		"The 21st label is correct");
			SimpleTest.is(someSortedLabels[21],	"Yahoo! Mail",			"The 22nd label is correct");
		});

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'getCredentials_test': function (someTestArgs) {
		var deferredResult;
		var user;
		
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("getCredentials_test", someTestArgs);
		deferredResult.addMethod(user, 'getCredentials');
		deferredResult.addCallback(function(aResult) {
			SimpleTest.is(aResult.username, 'joe',		"the username of the credentaials is correct");
			SimpleTest.is(aResult.password, 'clipperz', "the password of the credentaials is correct");
		})
		deferredResult.callback();
		
		return deferredResult;
	},
	
    //-------------------------------------------------------------------------

	'loginWithOfflineData_test': function (someTestArgs) {
		var deferredResult;
		var user;
		var proxy;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("loginWithOfflineData_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addCallback(function(aResult) {
			SimpleTest.is(aResult.result, 'done', "successfully logged in");
		})
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'getRecords_fromOfflineData_OLD_test': function (someTestArgs) {
		var deferredResult;
		var user;
		var proxy;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("getRecords_fromOfflineData_OLD_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['OLD_joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addCallback(function(aResult) {
			SimpleTest.is(aResult.result, 'done', "successfully logged in");
		});
		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(function(someRecords) {
			SimpleTest.is(someRecords.length, 15, "the OLD test user has just 15 records");
		});
		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.methodcaller('label'));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addCallback(MochiKit.Base.methodcaller('sort'));
		deferredResult.addCallback(function (someSortedLabels) {
			SimpleTest.is(someSortedLabels.length, 15, "We got all the labels");
			SimpleTest.is(someSortedLabels[0], "Amazon.com",		"The first label is correct");
			SimpleTest.is(someSortedLabels[1], "American Airlines",	"The second label is correct");
			SimpleTest.is(someSortedLabels[2], "Bloglines",			"The third label is correct");
			SimpleTest.is(someSortedLabels[3], "Digg",				"The fourth label is correct");
			SimpleTest.is(someSortedLabels[4], "Expedia.com",		"The fifth label is correct");
			SimpleTest.is(someSortedLabels[5], "Google Account",	"The sixth label is correct");
			SimpleTest.is(someSortedLabels[6], "Home burglar alarm","The seventh label is correct");
		});
/*
*/
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'getDirectLogins_fromOfflineData_OLD_test': function (someTestArgs) {
		var deferredResult;
		var user;
		var proxy;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("getRecords_fromOfflineData_OLD_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['OLD_joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addCallback(function(aResult) {
			SimpleTest.is(aResult.result, 'done', "successfully logged in");
		});
		deferredResult.addMethod(user, 'getDirectLogins');
		deferredResult.addCallback(function(someDirectLogins) {
			SimpleTest.is(someDirectLogins.length, 17, "the OLD test user has 17 direct logins");
		});
		deferredResult.addMethod(user, 'getDirectLogins');
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.methodcaller('label'));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addCallback(MochiKit.Base.methodcaller('sort'));
		deferredResult.addCallback(function (someSortedLabels) {
			SimpleTest.is(someSortedLabels.length, 17, "We got all the labels");
			SimpleTest.is(someSortedLabels[0],	"Amazon.com",			"The first label is correct");
			SimpleTest.is(someSortedLabels[1],	"American Airlines",	"The second label is correct");
			SimpleTest.is(someSortedLabels[2],	"Bloglines",			"The third label is correct");
			SimpleTest.is(someSortedLabels[3],	"Digg",					"The fourth label is correct");
			SimpleTest.is(someSortedLabels[4],	"Expedia.com",			"The fifth label is correct");
			SimpleTest.is(someSortedLabels[5],	"Flickr",				"The sixth label is correct");
			SimpleTest.is(someSortedLabels[6],	"Google Calendar",		"The seventh label is correct");
			SimpleTest.is(someSortedLabels[7],	"Google Docs",			"The 8th label is correct");
			SimpleTest.is(someSortedLabels[8],	"Google Mail",			"The 9th label is correct");
			SimpleTest.is(someSortedLabels[9],  "LinkedIn",				"The 10th label is correct");
			SimpleTest.is(someSortedLabels[10],	"Lufthansa",			"The 11th label is correct");
			SimpleTest.is(someSortedLabels[11],	"My Yahoo!",			"The 12th label is correct");
			SimpleTest.is(someSortedLabels[12],	"MyBlogLog",			"The 13th label is correct");
			SimpleTest.is(someSortedLabels[13],	"MySpace",				"The 14th label is correct");
			SimpleTest.is(someSortedLabels[14],	"The New York Times",	"The 15th label is correct");
			SimpleTest.is(someSortedLabels[15],	"Yahoo! Groups",		"The 16th label is correct");
			SimpleTest.is(someSortedLabels[16],	"Yahoo! Mail",			"The 17th label is correct");
		});

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'sortRecords_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var caseInsensitiveCompare;
		
		caseInsensitiveCompare = function (aValue, bValue) {
			return MochiKit.Base.compare(aValue.toLowerCase(), bValue.toLowerCase());
		};
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("sortRecords_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(Clipperz.Async.deferredSort, function (aRecord, bRecord) {
			return Clipperz.Async.deferredCompare(MochiKit.Base.compare, aRecord.label(), bRecord.label());
		});
		deferredResult.addCallback(MochiKit.Base.itemgetter('0'));
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Amazon.com", "Sorting the records themselves (by labels), the first one is 'Amazon.com'");

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(Clipperz.Async.deferredSort, function (aRecord, bRecord) {
			return Clipperz.Async.deferredCompare(MochiKit.Base.compare, aRecord.label(), bRecord.label());
		});
		deferredResult.addCallback(MochiKit.Base.itemgetter('5'));
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Expedia.com", "Sorting the records themselves (by labels), the sixth element is 'Expedia.com'");

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(Clipperz.Async.deferredSort, function (aRecord, bRecord) {
			return Clipperz.Async.deferredCompare(MochiKit.Base.compare, aRecord.label(), bRecord.label());
		});
		deferredResult.addCallback(MochiKit.Base.itemgetter('19'));
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("del.icio.us", "Sorting the records themselves (by labels), the 20th element is 'del.icio.us'");



		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(Clipperz.Async.deferredSort, function (aRecord, bRecord) {
			return Clipperz.Async.deferredCompare(caseInsensitiveCompare, aRecord.label(), bRecord.label());
		});
		deferredResult.addCallback(MochiKit.Base.itemgetter('3'));
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("del.icio.us", "Sorting with case insensitive mode (by labels), the fourth record is 'del.ico.us'");

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(Clipperz.Async.deferredSort, function (aRecord, bRecord) {
			return Clipperz.Async.deferredCompare(caseInsensitiveCompare, aRecord.label(), bRecord.label());
		});
		deferredResult.addCallback(MochiKit.Base.itemgetter('19'));
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Yahoo! Account", "Sorting with case insensitive mode (by labels), the 20th record is 'Yahoo! Account'");

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'accessToSingleRecord_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("sortRecords_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Amazon.com", "Sorting the records themselves (by labels), the first one is 'Amazon.com'");
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'accessToSingleRecordContent_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("sortRecords_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('fields'));
		deferredResult.addCallback(function (someRecordFields) {
			SimpleTest.is(MochiKit.Base.keys(someRecordFields).length, 2, "The number of fields of the Amazon.com record matches");
			return someRecordFields;
		});
		deferredResult.addCallback(MochiKit.Base.itemgetter('5e822c34aaf1a9fbc0b52585c1915f3a3758abd51923a4d35ae85373bbb839c2'));
		deferredResult.collectResults({
			'label':	MochiKit.Base.methodcaller('label'),
			'value':	MochiKit.Base.methodcaller('value')
		})
		deferredResult.addCallback(function (someValues) {
			SimpleTest.is(someValues['label'], 'email',				"the first field label matches");
			SimpleTest.is(someValues['value'], 'joe@clipperz.com',	"the first field value matches");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'modifyRecordLabelAndCheckForPendingChanges_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("modifyRecordAndCheckForPendingChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Amazon.com", "This is the record the test was expecting");

		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setLabel', "New label"));

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "setting a label on one of the user's record, trigger the 'hasPendingChanges' flag");

		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("New label", "once set the new label value, I can still get back its value");

		deferredResult.addMethod(user, 'revertChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "reverting changes should return to the original state");

		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Amazon.com", "The label of the record is restored to its initial value");

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'modifyRecordFieldsAndCheckForPendingChanges_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("modifyRecordFieldsAndCheckForPendingChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('fields'));
		deferredResult.addCallback(MochiKit.Base.itemgetter('5e822c34aaf1a9fbc0b52585c1915f3a3758abd51923a4d35ae85373bbb839c2'));
		deferredResult.addCallback(MochiKit.Base.methodcaller('value'));
		deferredResult.addTest("joe@clipperz.com", "The value of the record field is correct");

		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('fields'));
		deferredResult.addCallback(MochiKit.Base.itemgetter('5e822c34aaf1a9fbc0b52585c1915f3a3758abd51923a4d35ae85373bbb839c2'));
		deferredResult.addCallback(MochiKit.Base.methodcaller('setValue', 'joe@example.com'));

		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('fields'));
		deferredResult.addCallback(MochiKit.Base.itemgetter('5e822c34aaf1a9fbc0b52585c1915f3a3758abd51923a4d35ae85373bbb839c2'));
		deferredResult.addCallback(MochiKit.Base.methodcaller('value'));
		deferredResult.addTest("joe@example.com", "The record field correctly returns the newly updated value");

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "changing the value of a record's field trigger the 'hasPendingChanges' flag");

		deferredResult.addMethod(user, 'revertChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "reverting changes should return to the original state");

		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('fields'));
		deferredResult.addCallback(MochiKit.Base.itemgetter('5e822c34aaf1a9fbc0b52585c1915f3a3758abd51923a4d35ae85373bbb839c2'));
		deferredResult.addCallback(MochiKit.Base.methodcaller('value'));
		deferredResult.addTest("joe@clipperz.com", "The original value of the record field is returned after reverting the changes");

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'modifyRecordNoteAndCheckForPendingChanges_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("modifyRecordNoteAndCheckForPendingChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', '084e23120544603f0297368fd3891a3818e0fe13488e2f2c6079913c8d1bed8d');
		deferredResult.addCallback(MochiKit.Base.methodcaller('notes'));
		deferredResult.addTest("aproofofconcept\n<script>alert(\"nothing bad in here\");</script><script>alert(\"AAHHGGGH!\");</script>", "The value of the record note is correct");

		deferredResult.addMethod(user, 'getRecord', '084e23120544603f0297368fd3891a3818e0fe13488e2f2c6079913c8d1bed8d');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setNotes', "A new note text"));

		deferredResult.addMethod(user, 'getRecord', '084e23120544603f0297368fd3891a3818e0fe13488e2f2c6079913c8d1bed8d');
		deferredResult.addCallback(MochiKit.Base.methodcaller('notes'));
		deferredResult.addTest("A new note text", "The value of the record note has been updated");

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "changing the value of record's notes trigger the 'hasPendingChanges' flag");

		deferredResult.addMethod(user, 'getRecord', '084e23120544603f0297368fd3891a3818e0fe13488e2f2c6079913c8d1bed8d');
		deferredResult.addCallback(MochiKit.Base.methodcaller('hasPendingChanges'));
		deferredResult.addTest(true, "also the record should flat its pending changes on the note field");

		deferredResult.addMethod(user, 'revertChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "reverting changes should return to the original state");

		deferredResult.addMethod(user, 'getRecord', '084e23120544603f0297368fd3891a3818e0fe13488e2f2c6079913c8d1bed8d');
		deferredResult.addCallback(MochiKit.Base.methodcaller('notes'));
		deferredResult.addTest("aproofofconcept\n<script>alert(\"nothing bad in here\");</script><script>alert(\"AAHHGGGH!\");</script>", "The value of the record note is restored to its initial value");
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'loadUser_withoutPreferences_andTryToAccessThem_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("loadUser_withoutPreferences_andTryToAccessThem_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.collectResults({
			'preferences': [
				MochiKit.Base.method(user, 'getHeaderIndex', 'preferences'),
				MochiKit.Base.methodcaller('getDecryptedData')
			],
			'oneTimePasswords': [
				MochiKit.Base.method(user, 'getHeaderIndex', 'oneTimePasswords'),
				MochiKit.Base.methodcaller('getDecryptedData')
			]
		});
		deferredResult.addCallback(function (someValues) {
			SimpleTest.is(Clipperz.Base.serializeJSON(someValues['preferences']), '{}', "The preferences are empty");
		});

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'modifyRecordTitleAndCheckForPendingChanges_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("modifyRecordTitleAndCheckForPendingChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Card 1", "The value of the record note is correct");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setLabel', "Edited card 1"));

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Edited card 1", "The value of the record label has been updated");

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "changing the value of record's label trigger the 'hasPendingChanges' flag");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('hasPendingChanges'));
//		deferredResult.addTest(true, "also the record should flag its pending changes on the label - 1");
		deferredResult.addTest(false, "changing just the label (or any other attribute stored on the header) should not trigger the 'hasPendingChanges' flag");

		deferredResult.addMethod(user, 'revertChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "reverting changes should return to the original state");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Card 1", "The value of the record label is restored to its initial value");
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'modifyRecordTitleAndCheckForPendingChanges_take2_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("modifyRecordTitleAndCheckForPendingChanges_take2_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Card 1", "The value of the record note is correct");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setLabel', "Edited card 1"));

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "changing the value of record's label trigger the 'hasPendingChanges' flag");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('hasPendingChanges'));
//		deferredResult.addTest(true, "also the record should flag its pending changes on the label - 2");
		deferredResult.addTest(false, "only the label has changed, and this should not trigger the 'hasPendingChanges' flag on the record as it is stored in the header");

//		deferredResult.addCallback(Clipperz.log, "=======================================================");
		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setLabel', "Card 1"));

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "Setting the old value back should void all pending changes");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('hasPendingChanges'));
		deferredResult.addTest(false, "also the record should not flag any pending changes, as the original value has been restored");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Card 1", "The value of the record label is restored to its initial value");
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'modifyRecordNoteAndThanResetOriginalValue_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("modifyRecordNoteAndThanResetOriginalValue_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', '084e23120544603f0297368fd3891a3818e0fe13488e2f2c6079913c8d1bed8d');
		deferredResult.addCallback(MochiKit.Base.methodcaller('notes'));
		deferredResult.addTest("aproofofconcept\n<script>alert(\"nothing bad in here\");</script><script>alert(\"AAHHGGGH!\");</script>", "The value of the record note is correct");

		deferredResult.addMethod(user, 'getRecord', '084e23120544603f0297368fd3891a3818e0fe13488e2f2c6079913c8d1bed8d');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setNotes', "A new note text"));

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "changing the value of record's notes trigger the 'hasPendingChanges' flag");

		deferredResult.addMethod(user, 'getRecord', '084e23120544603f0297368fd3891a3818e0fe13488e2f2c6079913c8d1bed8d');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setNotes', "aproofofconcept\n<script>alert(\"nothing bad in here\");</script><script>alert(\"AAHHGGGH!\");</script>"));

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "setting the field to its original value basically reset the change, like 'revertChanges' would have done");
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'modifyRecordNoteAndCommitChanges_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var user2;
		var newNoteText;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
		user2 = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
		newNoteText = "A new note text";

		deferredResult = new Clipperz.Async.Deferred("modifyRecordAndCommitChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('notes'));
		deferredResult.addTest("", "This is the original value of the notes");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setNotes', newNoteText));

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "changing the value of record's notes trigger the 'hasPendingChanges' flag");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('hasPendingChanges'));
		deferredResult.addTest(true, "changing the value of record's notes trigger the 'hasPendingChanges' flag also on the record itself");

		deferredResult.addMethod(user, 'saveChanges');

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "saving changes should return the user to a state with not changes pending - 1");

		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user2, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('notes'));
		deferredResult.addTest(newNoteText, "It looks like the data edited was correctly stored and reloaded here");

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'multipleModificationToRecordTitleAndCommitChanges_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var user2;
		var user3;
		var newRecordTitle;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
		user2 = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
		user3 = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
		newRecordTitle = "A nice new title here";

		deferredResult = new Clipperz.Async.Deferred("multipleModificationToRecordTitleAndCommitChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');

		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Card 1", "This is the original value of the label");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setLabel', newRecordTitle));

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "changing the value of record's label trigger the 'hasPendingChanges' flag");

		deferredResult.addMethod(user, 'saveChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "saving changes should return the user to a state with not changes pending - 2");	//	FAIL

		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user2, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest(newRecordTitle, "It looks like the label edited was correctly stored and reloaded here");

		deferredResult.addMethod(user2, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setLabel', newRecordTitle + "-" + newRecordTitle));
		deferredResult.addMethod(user2, 'hasPendingChanges');
		deferredResult.addTest(true, "changing again the value of record's label trigger the 'hasPendingChanges' flag also on the new user");

		deferredResult.addMethod(user2, 'saveChanges');
		deferredResult.addMethod(user2, 'hasPendingChanges');
		deferredResult.addTest(false, "after committing the changes, the user has no pending changes");	//	FAIL

		deferredResult.addMethod(user3, 'login');
		deferredResult.addMethod(user3, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest(newRecordTitle + "-" + newRecordTitle, "It looks like the label edited was correctly stored and reloaded here");

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'saveChangesWithADeletedRecord_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("saveChangesWithADeletedRecord_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "This account has oly a single card");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Card 1", "This is the initial value of the label ...");

		deferredResult.addMethod(user, 'getHeaderIndex', 'recordsIndex');
		deferredResult.addCallback(MochiKit.Base.methodcaller('prepareRemoteDataForChangedRecords'));
		deferredResult.addCallback(function (aResult) {
			SimpleTest.ok(aResult['updated'] != null, "There updated key is not null");
			SimpleTest.isDeeply(aResult['updated'], [], "There are no updated cards");
			SimpleTest.ok(aResult['deleted'] != null, "There deleted key is not null");
			SimpleTest.isDeeply(aResult['deleted'], [], "There are no deleted references");
			return aResult;
		})
		deferredResult.addMethod(user, 'revertChanges');

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addMethod(user, 'deleteRecord');
		deferredResult.addMethod(user, 'getHeaderIndex', 'recordsIndex');
		deferredResult.addCallback(MochiKit.Base.methodcaller('prepareRemoteDataForChangedRecords'));
		deferredResult.addCallback(function (aResult) {
			SimpleTest.isDeeply(aResult['deleted'], ['8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13'], "The deleted record reference is correctly reported");
			return aResult;
		})
		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.shouldFail("accessing the deleted record reference should raise an exception");

		deferredResult.addMethod(user, 'getRecord', '0000000000000000000000000000000000000000000000000000000000000000');
		deferredResult.shouldFail("accessing a fake record reference should raise an exception");

		deferredResult.addMethod(user, 'saveChanges');
		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(0, "after deleting the only record, there should be no records bound to the user");

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'revertingChangesAfterDeletingACard_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("simpleSaveChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Card 1", "This is the initial value of the label ...");

		deferredResult.addMethod(user, 'getHeaderIndex', 'recordsIndex');
		deferredResult.addCallback(MochiKit.Base.methodcaller('prepareRemoteDataForChangedRecords'));
		deferredResult.addCallback(function (aResult) {
			SimpleTest.ok(aResult['updated'] != null, "There updated key is not null");
			SimpleTest.isDeeply(aResult['updated'], [], "There are no updated cards");
			SimpleTest.ok(aResult['deleted'] != null, "There deleted key is not null");
			SimpleTest.isDeeply(aResult['deleted'], [], "There are no deleted references");
			return aResult;
		})
		deferredResult.addMethod(user, 'revertChanges');

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addMethod(user, 'deleteRecord');
		deferredResult.addMethod(user, 'getHeaderIndex', 'recordsIndex');
		deferredResult.addCallback(MochiKit.Base.methodcaller('prepareRemoteDataForChangedRecords'));
		deferredResult.addCallback(function (aResult) {
			SimpleTest.isDeeply(aResult['deleted'], ['8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13'], "The deleted record reference is correctly reported");
			return aResult;
		})
		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.shouldFail("accessing the deleted record reference should raise an exception");

		deferredResult.addMethod(user, 'getRecord', '0000000000000000000000000000000000000000000000000000000000000000');
		deferredResult.shouldFail("accessing a fake record reference should raise an exception");

		deferredResult.addMethod(user, 'revertChanges');
		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(SimpleTest.ok, true, "after reverting all changes, the deleted card is restored");

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'lockUnlockAccountAfterDeletingACard_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var recordID;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		recordID = 'eeda70e0392261967bda71c3764da78989c45bbd2bb7be6b941b90f81d9b81b5';
//		recordID = '507f38b06d587d8889698ae1ebbba7ef8f0539b82550dd25779fd9ee88fc0c7a';

		deferredResult = new Clipperz.Async.Deferred("lockUnlockAccountAfterDeletingACard_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(20, "Initially the user has 20 cards");

		deferredResult.addMethod(user, 'getDirectLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(22, "Initially the user has 22 direct logins");

		deferredResult.addMethod(user, 'getHeaderIndex', 'recordsIndex');
		deferredResult.addMethodcaller('directLoginsData');
		deferredResult.addMethodcaller('values');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(22, "There should be also 22 direct login references");

		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(4, "The selected record has 4 direct logins");
		
		deferredResult.addMethod(user, 'getRecord', recordID);
		deferredResult.addMethod(user, 'deleteRecord');
		deferredResult.addMethod(user, 'saveChanges');

		deferredResult.addMethod(user, 'hasAnyCleanTextData');
		deferredResult.addTest(true, "after saving changes, hasAnyCleanTextData should be true");

		deferredResult.addMethod(user, 'deleteAllCleanTextData');

		deferredResult.addMethod(user, 'hasAnyCleanTextData');
		deferredResult.addTest(false, "after deleting all clean text data, hasAnyCleanTextData should be true");

		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest((20 - 1), "After deleting a card, only 19 are left");

		deferredResult.addMethod(user, 'getDirectLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest((22 - 4), "Initially the user has 18 direct logins");

		deferredResult.addMethod(user, 'getHeaderIndex', 'recordsIndex');
		deferredResult.addMethodcaller('directLoginsData');
		deferredResult.addMethodcaller('values');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest((22 - 4), "Once deleted the card, there should be just 18 direct login references left");

		deferredResult.addCallback(function () {
			SimpleTest.ok(true, "nothing wrong had happen 'till here");
		});

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'lockUnlockAccountAfterDeletingACard_2_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("lockUnlockAccountAfterDeletingACard_2_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(someRecords.length, 20, "Initially the user has 20 cards");
		});

		deferredResult.addMethod(user, 'getDirectLogins');
		deferredResult.addCallback(function (someDirectLogins) {
			SimpleTest.is(someDirectLogins.length, 22, "Initially the user has 42 direct logins");
		});

		deferredResult.addMethod(user, 'getHeaderIndex', 'recordsIndex');
		deferredResult.addMethodcaller('directLoginsData');
		deferredResult.addMethodcaller('values');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(function (someDirectLoginReferences) {
			SimpleTest.is(someDirectLoginReferences.length, 22, "There should be also 22 direct login references - 2");
		});

		deferredResult.addMethod(user, 'getRecord', '507f38b06d587d8889698ae1ebbba7ef8f0539b82550dd25779fd9ee88fc0c7a');
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(function (someDirectLogins) {
			SimpleTest.is(someDirectLogins.length, 1, "The selected record has 4 direct logins");
		});
		
		
		deferredResult.addMethod(user, 'getRecord', '507f38b06d587d8889698ae1ebbba7ef8f0539b82550dd25779fd9ee88fc0c7a');
		deferredResult.addMethod(user, 'deleteRecord');
		deferredResult.addMethod(user, 'saveChanges');
		deferredResult.addMethod(user, 'deleteAllCleanTextData');
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(someRecords.length, (20 -1), "After deleting a card, only 19 are left - 2");
		});

		deferredResult.addMethod(user, 'getDirectLogins');
		deferredResult.addCallback(function (someDirectLogins) {
			SimpleTest.is(someDirectLogins.length, (22 - 1), "Initially the user has 21 direct logins - 2");
		});

		deferredResult.addMethod(user, 'getHeaderIndex', 'recordsIndex');
		deferredResult.addMethodcaller('directLoginsData');
		deferredResult.addMethodcaller('values');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(function (someDirectLoginReferences) {
			SimpleTest.is(someDirectLoginReferences.length, (22 - 1), "Once deleted the card, there should be just 21 direct login references left");
		});

		deferredResult.addCallback(function () {
			SimpleTest.ok(true, "nothing wrong had happen 'till here - 2");
		});

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'simpleSaveChanges_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("simpleSaveChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('label'));
		deferredResult.addTest("Card 1", "This is the initial value of the label ...");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setLabel', "New label for Card 1"));

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setNotes', "Setting just the label would not trigger the update of the record data and nothing will end up in the 'updated' list"));

		deferredResult.addMethod(user, 'getHeaderIndex', 'recordsIndex');
		deferredResult.addCallback(MochiKit.Base.methodcaller('prepareRemoteDataForChangedRecords'));
		deferredResult.addCallback(function (aResult) {
			SimpleTest.is(aResult['updated'].length, 1, "The updadated record should be listed in the changes getting ready for commit");
			return aResult;
		})

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'saveChangesAndDataCaching_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var record_1;
		var record_2;
		
		record_1 = '062af892bcfba49ffcff05c56d99b7af2d508358e39c058c2e1fc83531436f80';
		record_2 = '084e23120544603f0297368fd3891a3818e0fe13488e2f2c6079913c8d1bed8d';

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("saveChangesAndDataCaching_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', record_1);
		deferredResult.addMethodcaller('hasLoadedRemoteData');
		deferredResult.addTest(false, "The card data should have not been loaded yet");

		deferredResult.addMethod(user, 'getRecord', record_2);
		deferredResult.addMethodcaller('hasLoadedRemoteData');
		deferredResult.addTest(false, "Also the other card data should have not been loaded yet");

		deferredResult.addMethod(user, 'getRecord', record_1);
		deferredResult.addMethodcaller('setLabel', "New title");

		deferredResult.addMethod(user, 'getRecord', record_1);
		deferredResult.addMethodcaller('hasLoadedRemoteData');
		deferredResult.addTest(false, "Changing just the label should not trigger the full loading of the card");

		deferredResult.addMethod(user, 'getRecord', record_1);
		deferredResult.addMethodcaller('setNotes', "New note text");

		deferredResult.addMethod(user, 'getRecord', record_1);
		deferredResult.addMethodcaller('hasLoadedRemoteData');
		deferredResult.addTest(true, "Changing the notes should trigger the loading of the card");

		deferredResult.addMethod(user, 'getRecord', record_2);
		deferredResult.addMethodcaller('hasLoadedRemoteData');
		deferredResult.addTest(false, "Changing record_1 should not trigger the loading of record_2");

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "changing the value of record's label trigger the 'hasPendingChanges' flag");

		deferredResult.addMethod(user, 'saveChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "saving changes should return the user to a state with not changes pending - 3");

		deferredResult.addMethod(user, 'getRecord', record_1);
		deferredResult.addMethodcaller('hasLoadedRemoteData');
		deferredResult.addTest(true, "After saving the changes, record_1 sould not have the remote data");

		deferredResult.addMethod(user, 'getRecord', record_2);
		deferredResult.addMethodcaller('hasLoadedRemoteData');
		deferredResult.addTest(false, "After saving, record_2 should still be NOT loaded");


		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'addNewRecordFieldAndSave_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("simpleSaveChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(function (someFields) {
			SimpleTest.is(MochiKit.Base.values(someFields).length, 3, "The record has initially 3 fields");
		});

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('addField', {'label':"New field label", 'value':"New field value", 'isHidden':false}));

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "adding a field should mark the record as having pending changes");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(function (someFields) {
			SimpleTest.is(MochiKit.Base.values(someFields).length, 4, "The record has now 4 fields");
		});

		deferredResult.addMethod(user, 'saveChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "saving changes should return the user to a state with not changes pending - 4");

		deferredResult.addMethod(user, 'hasAnyCleanTextData');
		deferredResult.addTest(true, "after saving changes, hasAnyCleanTextData should be true");

//deferredResult.addCallback(function (aValue) { console.log(">>> #################################################"); return aValue});
		deferredResult.addMethod(user, 'deleteAllCleanTextData');
//deferredResult.addCallback(function (aValue) { console.log("<<< #################################################"); return aValue});

		deferredResult.addMethod(user, 'hasAnyCleanTextData');
		deferredResult.addTest(false, "after deleting all clean text, hasAnyCleanTextData should be false");

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'deleteRecordFieldAndSave_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var user2;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
		user2 = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("simpleSaveChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(function (someFields) {
			SimpleTest.is(MochiKit.Base.values(someFields).length, 3, "The record has initially 3 fields");
		});

		deferredResult.collectResults({
			'record': MochiKit.Base.method(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13'),
			'field': [
				MochiKit.Base.method(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13'),
				MochiKit.Base.methodcaller('fields'),
				MochiKit.Base.values,
				MochiKit.Base.itemgetter('0')
			]
		})
		deferredResult.addCallback(function (someValues) {
			someValues['record'].removeField(someValues['field']);
		});

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "removing a field should mark the record as having pending changes");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(function (someFields) {
			SimpleTest.is(MochiKit.Base.values(someFields).length, 2, "The record has now 2 fields");
		});

		deferredResult.addMethod(user, 'saveChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "saving changes should return the user to a state with not changes pending - 5");

		//. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
		
		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user2, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(function (someFields) {
			SimpleTest.is(MochiKit.Base.values(someFields).length, 2, "Once saved, the record is left with just two fields");
		});

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'loadDirectLogin_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("loadDirectLogin_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('directLogins'));
		deferredResult.addCallback(function (someDirectLogins) {
			SimpleTest.is(1, MochiKit.Base.keys(someDirectLogins).length, "the Amazon.com card has just one direct login");
		});

		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('hasPendingChanges'));
		deferredResult.addTest(false, "initially the record does not have any pending changes");

		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('directLogins'));
		deferredResult.addCallback(MochiKit.Base.itemgetter('03251dc1cbc5398789e4c4b45c52cfac3fcd8c1a4f19a81fa68fc6feae31d55c'));
//		deferredResult.addCallback(MochiKit.Base.methodcaller('runDirectLogin', true));
		deferredResult.addCallback(Clipperz.PM.UI.Common.Controllers.DirectLoginRunner.testDirectLogin);
		
		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('getFieldsValues'));
		deferredResult.addCallback(function (someFieldsValues) {
			SimpleTest.is(MochiKit.Base.keys(someFieldsValues).length, 2, "the Amazon.com card has just two fields");
		});
		
		deferredResult.addMethod(user, 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');
		deferredResult.addCallback(MochiKit.Base.methodcaller('hasPendingChanges'));
		deferredResult.addTest(false, "accessing fields values should not trigger the 'hasPendingChanges' flag");
		
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'readingVeryOldCards_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("readingVeryOldCards_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_someExtraOldData']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', '05aad20ee399b11ddc923e601fcd1d096233634f2ad4c55db4f6435e5f9cc17a');
		deferredResult.addMethodcaller('label');
		deferredResult.addTest('Card encoded with an old algorithm', 'the label of the selected record is the expected one');

		deferredResult.addMethod(user, 'getRecord', '05aad20ee399b11ddc923e601fcd1d096233634f2ad4c55db4f6435e5f9cc17a');
		deferredResult.addCallback(MochiKit.Base.methodcaller('fields'));
		deferredResult.addCallback(function (someFields) {
			SimpleTest.is(6, MochiKit.Base.keys(someFields).length, "the 'Card encoded with an old algorithm' card has six fields");
		});

		deferredResult.addMethod(user, 'getRecord', '05aad20ee399b11ddc923e601fcd1d096233634f2ad4c55db4f6435e5f9cc17a');
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(false, "accessing the card fields should not trigger the hasPendingChanges flag");

		deferredResult.addMethod(user, 'getRecord', '05aad20ee399b11ddc923e601fcd1d096233634f2ad4c55db4f6435e5f9cc17a');
		deferredResult.addCallback(MochiKit.Base.methodcaller('addField', {'label':"New field label", 'value':"New field value", 'isHidden':false}));

		deferredResult.addMethod(user, 'getRecord', '05aad20ee399b11ddc923e601fcd1d096233634f2ad4c55db4f6435e5f9cc17a');
		deferredResult.addCallback(MochiKit.Base.methodcaller('fields'));
		deferredResult.addCallback(function (someFields) {
			SimpleTest.is(7, MochiKit.Base.keys(someFields).length, "adding a field shoult bring the total field count to 7");
		});

		deferredResult.addMethod(user, 'getRecord', '05aad20ee399b11ddc923e601fcd1d096233634f2ad4c55db4f6435e5f9cc17a');
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "adding a field should mark the record as having pending changes - 2");
		
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'addingNewEmptyRecordAndSaveChanges_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var newRecordReference;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
		user2 = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("addingNewEmptyRecordAndSaveChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_someExtraOldData']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(MochiKit.Base.keys(someRecords).length, 1, "The user has initially just one record");
		});

		deferredResult.addMethod(user, 'createNewRecord');
		deferredResult.addMethodcaller('reference');
		deferredResult.addCallback(function (aNewRecordReference) {
			newRecordReference = aNewRecordReference;
		})

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(MochiKit.Base.keys(someRecords).length, 2, "After having created a new record, the total should be updated accordingly");
		});

		deferredResult.addMethod(user, 'getRecord', '05aad20ee399b11ddc923e601fcd1d096233634f2ad4c55db4f6435e5f9cc17a');
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(false, "adding a new record should not trigger any changes on a sibling record");

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "adding a new record should trigger the 'has pending changes' flag on the user");

		deferredResult.addMethod(user, 'saveChanges');

		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user2, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(MochiKit.Base.keys(someRecords).length, 1, "Reloading the data, just one record is available, as a brand new record without any changes should not be saved");
		});

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'addNewRecordAndSaveChanges_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
		user2 = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("addNewRecordAndSaveChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_someExtraOldData']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(MochiKit.Base.keys(someRecords).length, 1, "The user has initially just one record");
		});

		deferredResult.addMethod(user, 'createNewRecord');
		deferredResult.addCallback(function (aNewRecord) {
			var innerDeferredResult;
			
			innerDeferredResult = new Clipperz.Async.Deferred("addNewRecordAndSaveChanges_test <internal>", {trace:false});

			innerDeferredResult.addMethod(aNewRecord, 'label');
			innerDeferredResult.addTest('', "The label of a brand new record should be the empty string");

			innerDeferredResult.addMethod(aNewRecord, 'setLabel', "New record label");
			innerDeferredResult.addMethod(aNewRecord, 'setNotes', "New record notes");
			innerDeferredResult.addMethod(aNewRecord, 'addField', {'label':"Label 1", 'value':"Value 1", 'isHidden':false});
			innerDeferredResult.addMethod(aNewRecord, 'addField', {'label':"Label 2", 'value':"Value 2", 'isHidden':false});
			innerDeferredResult.addMethod(aNewRecord, 'addField', {'label':"Label 3", 'value':"Value 3", 'isHidden':true});
			innerDeferredResult.addMethod(aNewRecord, 'addField', {'label':"Label 4", 'value':"Value 4", 'isHidden':false});
			innerDeferredResult.callback();
			
			return innerDeferredResult;
		})

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(MochiKit.Base.keys(someRecords).length, 2, "After having created a new record, the total should be updated accordingly");
		});

		deferredResult.addMethod(user, 'getRecord', '05aad20ee399b11ddc923e601fcd1d096233634f2ad4c55db4f6435e5f9cc17a');
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(false, "adding a new record should not trigger any changes on a sibling record");

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "adding a new record should trigger the 'has pending changes' flag on the user");

		deferredResult.addMethod(user, 'saveChanges');

		deferredResult.addCallback(function () {
			var recordData
			var recordVersionData;
			
			recordData = MochiKit.Base.values(proxy.dataStore().data()['users']['9a984e219b07f9b645ef35f4de938b4741abe2e0b4adc88b40e9367170c91cc8']['records'])[1];
			recordVersionData = MochiKit.Base.values(recordData['versions'])[0];

			SimpleTest.is(recordVersionData['previousVersionKey'], Clipperz.PM.Crypto.nullValue, "The previous version key on the first version of a newly created record is equal to Clipperz.PM.Crypto.nullValue");
		});

		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user2, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(someRecords.length, 2, "Reloading the data, two records are available.");
			return someRecords;
		});
		deferredResult.addCallback(MochiKit.Base.itemgetter('1'));
		deferredResult.collectResults({
			'label':	[
				MochiKit.Base.methodcaller('label'),
				Clipperz.Async.Test.is("New record label", "The label is correct")
			],
			'notes':	[
				MochiKit.Base.methodcaller('notes'),
				Clipperz.Async.Test.is("New record notes", "The note is correct")
			],
			'fields':	[
				MochiKit.Base.methodcaller('fields'),
				function (someFields) {
					SimpleTest.is(MochiKit.Base.values(someFields).length, 4, "The fields are 4, as expected");
					return someFields;
				}
			]
		})

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'addNewRecordAndTestNewRecordIndex_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("addNewRecordAndTestNewRecordIndex_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(MochiKit.Base.keys(someRecords).length, 20, "The user has initially 20 records");
		});

		deferredResult.addMethod(user, 'createNewRecord');
		deferredResult.addCallback(function (aNewRecord) {
			var innerDeferredResult;
			
			innerDeferredResult = new Clipperz.Async.Deferred("addNewRecordAndTestNewRecordIndex_test <internal>", {trace:false});

			innerDeferredResult.addMethod(user, 'getHeaderIndex', 'recordsIndex');
			innerDeferredResult.addMethodcaller('recordsIndex');
			innerDeferredResult.addCallback(MochiKit.Base.itemgetter(aNewRecord.reference()));
			innerDeferredResult.addTest(20, "The index of the new record should be 20");

			innerDeferredResult.callback();
			
			return innerDeferredResult;
		})

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'editRecordAndTestForChangesInPreferencesAndOTP_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var user_2;
		var originalPreferences;
		var originalOTPs;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		user_2 = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("editRecordAndTestForChangesInPreferencesAndOTP_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_with_preferences_and_OTPs_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getServerData');

		deferredResult.collectResults({
			'preferences': [
				MochiKit.Base.method(user, 'getHeaderIndex', 'preferences'),
				MochiKit.Base.methodcaller('getDecryptedData')
			],
			'oneTimePasswords': [
				MochiKit.Base.method(user, 'getHeaderIndex', 'oneTimePasswords'),
				MochiKit.Base.methodcaller('getDecryptedData')
			]
		});

		deferredResult.addCallback(function (someValues) {
//console.log("SOME VALUES", someValues);
			originalPreferences = Clipperz.Base.deepClone(someValues['preferences']);
			originalOTPs = Clipperz.Base.deepClone(someValues['oneTimePasswords']);

			SimpleTest.is(originalPreferences['preferredLanguage'], 'en-US', "Preference.language is ok");
			SimpleTest.is(originalPreferences['shouldShowDonationPanel'], false, "Preference.shouldShowDonationPanel is ok");

			SimpleTest.is(MochiKit.Base.keys(originalOTPs).length, 6, "the number of OTPs is as expected");
		});

		deferredResult.addMethod(user, 'getRecord', '35b30f9e923ce913365815d44cf344ce66cb71b636093b8ec55b8245d13df82b');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setLabel', "NEW LABEL"));
		deferredResult.addMethod(user, 'saveChanges');

		deferredResult.addMethod(user_2, 'login');
		deferredResult.addMethod(user_2, 'getServerData');

		deferredResult.collectResults({
			'preferences': [
				MochiKit.Base.method(user_2, 'getHeaderIndex', 'preferences'),
				MochiKit.Base.methodcaller('getDecryptedData')
			],
			'oneTimePasswords': [
				MochiKit.Base.method(user_2, 'getHeaderIndex', 'oneTimePasswords'),
				MochiKit.Base.methodcaller('getDecryptedData')
			]
		});

		deferredResult.addCallback(function (someValues) {
//console.log("SOME VALUES", someValues);
//			originalPreferences = Clipperz.Base.deepClone(someValues['preferences']);
//			originalOTPs = Clipperz.Base.deepClone(someValues['oneTimePasswords']);

			SimpleTest.is(someValues['preferences']['preferredLanguage'], originalPreferences['preferredLanguage'], "Preference.language is preserved");
			SimpleTest.is(someValues['preferences']['shouldShowDonationPanel'], originalPreferences['shouldShowDonationPanel'], "Preference.shouldShowDonationPanel is preserved");

			SimpleTest.is(MochiKit.Base.keys(someValues['oneTimePasswords']).length, MochiKit.Base.keys(originalOTPs).length, "the number of OTPs is preserved");
		});

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'addRecordAndSaveChangesMultipleTimes_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
		user2 = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("addRecordAndSaveChangesMultipleTimes_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_someExtraOldData']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "The user has one record stored in its account");

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "After loading records, the user should have no pending changes");

		deferredResult.addMethod(user, 'createNewRecord');
		deferredResult.addCallback(function (aNewRecord) {
			var innerDeferredResult;
			
			innerDeferredResult = new Clipperz.Async.Deferred("addRecordAndSaveChangesMultipleTimes_test <internal [1]>", {trace:false});

			innerDeferredResult.addMethod(aNewRecord, 'setLabel', "New record 1");
			innerDeferredResult.addMethod(aNewRecord, 'addField', {'label':'record number', 'value':"1", 'isHidden':false});
			innerDeferredResult.addMethod(aNewRecord, 'addField', {'label':'field count', 'value':"2", 'isHidden':false});
			innerDeferredResult.callback();
			
			return innerDeferredResult;
		})

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "Before saving, the user has pending changes");

		deferredResult.addMethod(user, 'saveChanges');
		deferredResult.addCallback(SimpleTest.ok, true, "Saving worked (apparently) fine");

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "After saving, the user has no pending changes");

		deferredResult.addMethod(user, 'createNewRecord');
		deferredResult.addCallback(function (aNewRecord) {
			var innerDeferredResult;
			
			innerDeferredResult = new Clipperz.Async.Deferred("addRecordAndSaveChangesMultipleTimes_test <internal [2]>", {trace:false});

			innerDeferredResult.addMethod(aNewRecord, 'setLabel', "New record 2");
			innerDeferredResult.addMethod(aNewRecord, 'addField', {'label':"record number", 'value':"2", 'isHidden':false});
			innerDeferredResult.callback();
			
			return innerDeferredResult;
		})
		deferredResult.addMethod(user, 'saveChanges');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(3, "After having created two new records, the total should be updated accordingly");


		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user2, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(3, "Reloading the data, three records are available");

		deferredResult.addMethod(user2, 'recordWithLabel', 'New record 1');
//deferredResult.addCallback(function (aValue) { console.log("RECORD with Label", aValue); return aValue; });
		deferredResult.collectResults({
			'label':	[
				MochiKit.Base.methodcaller('label'),
				MochiKit.Base.partial(Clipperz.Async.Test.is, 'New record label', "The label is correct")
			],
			'notes':	[
				MochiKit.Base.methodcaller('notes'),
				Clipperz.Async.Test.is('', "The note of the new created record is empty")
			],
			'fields':	[
				MochiKit.Base.methodcaller('fields'),
				MochiKit.Base.values,
				MochiKit.Base.itemgetter('length'),
				Clipperz.Async.Test.is(2, "The new record has just one field, as expected")
			],
			'fieldValues_1': [
				MochiKit.Base.methodcaller('fieldWithLabel', 'record number'),
				MochiKit.Base.methodcaller('value'),
				Clipperz.Async.Test.is('1', "The field value is as expected")
			],
			'fieldValues_2': [
				MochiKit.Base.methodcaller('fieldWithLabel', 'field count'),
				MochiKit.Base.methodcaller('value'),
				Clipperz.Async.Test.is('2', "Also the second field value is as expected")
			]
		})

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'addNewRecordAndRevertChanges_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user, user2;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
		user2 = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("addNewRecordAndRevertChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_someExtraOldData']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(MochiKit.Base.keys(someRecords).length, 1, "The user has initially just one record");
		});

		deferredResult.addMethod(user, 'createNewRecord');
		deferredResult.addCallback(function (aNewRecord) {
			return	Clipperz.Async.callbacks("addNewRecordAndRevertChanges_test <internal>", [
				MochiKit.Base.method(aNewRecord, 'setLabel', "New record label"),
				MochiKit.Base.method(aNewRecord, 'setNotes', "New record notes"),
				MochiKit.Base.method(aNewRecord, 'addField', {'label':"Label 1", 'value':"Value 1", 'isHidden':false}),
				MochiKit.Base.method(aNewRecord, 'addField', {'label':"Label 2", 'value':"Value 2", 'isHidden':false}),
				MochiKit.Base.method(aNewRecord, 'addField', {'label':"Label 3", 'value':"Value 3", 'isHidden':true}),
				MochiKit.Base.method(aNewRecord, 'addField', {'label':"Label 4", 'value':"Value 4", 'isHidden':false})
			], {trace:false});
		})

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(MochiKit.Base.keys(someRecords).length, 2, "After having created a new record, the total should be updated accordingly");
		});

		deferredResult.addMethod(user, 'getRecord', '05aad20ee399b11ddc923e601fcd1d096233634f2ad4c55db4f6435e5f9cc17a');
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(false, "adding a new record should not trigger any changes on a sibling record");

		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(true, "adding a new record should trigger the 'has pending changes' flag on the user");

		deferredResult.addMethod(user, 'revertChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "reverting changes shoud restore the previous state on the user");

		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user2, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(someRecords.length, 1, "Reloading the data, just one record is available.");
			return someRecords;
		});

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'logout_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("logout_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_with_preferences_and_OTPs_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(MochiKit.Base.keys(someRecords).length, 18, "The user has 18 records");
		});

		deferredResult.addMethod(user, 'logout');
		deferredResult.shouldSucceed("Logging out should not trigger an exception");

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'lock_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var returnPassword	= function () { return MochiKit.Async.succeed('clipperz'); };
		var failPassword	= function () { throw "Unexpected access to the password"; };
		var currentPasswordFunction = returnPassword;
		var passwordFunction = function () { return currentPasswordFunction(); };

		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:passwordFunction});

		deferredResult = new Clipperz.Async.Deferred("lock_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_with_preferences_and_OTPs_data']);

		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(MochiKit.Base.keys(someRecords).length, 18, "The user has 18 records");
		});

		deferredResult.addMethod(user, 'getDirectLogins');
		deferredResult.addCallback(function (someDirectLogins) {
			SimpleTest.is(MochiKit.Base.keys(someDirectLogins).length, 22, "The user has 22 direct logins");
		});

		deferredResult.addMethod(proxy, 'shouldNotReceiveAnyFurtherRequest');
		deferredResult.addCallback(function () { currentPasswordFunction = failPassword; });

		deferredResult.addMethod(user, 'lock');
		deferredResult.shouldSucceed("Locking out should not trigger an exception");

		deferredResult.addMethod(proxy, 'unexpectedRequests');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(0, "The proxy should have not received any extra request");
//deferredResult.addCallback(function (aValue) { console.log("PROXY.unexpectedRequests", Clipperz.Base.serializeJSON(proxy.unexpectedRequests())); return aValue; });
		deferredResult.addMethod(proxy, 'mayReceiveMoreRequests');
		deferredResult.addCallback(function () { currentPasswordFunction = returnPassword; });

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'registerNewUser_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user, user2;
		var username;
		var passphrase;
		
		username = "new";
		passphrase = "user";

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
//		user2 = new Clipperz.PM.DataModel.User({username:username, getPassphraseFunction:function () { return MochiKit.Async.succeed(passphrase);}});
		user2 = new Clipperz.PM.DataModel.User({username:username, getPassphraseFunction:function () { return passphrase;}});

		deferredResult = new Clipperz.Async.Deferred("registerNewUser_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_with_preferences_and_OTPs_data']);

		deferredResult.addCallback(Clipperz.PM.DataModel.User.registerNewAccount, username, function () { return MochiKit.Async.succeed(passphrase);});
		deferredResult.setValue('user');

		deferredResult.addMethodcaller('getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(MochiKit.Base.keys(someRecords).length, 0, "The newly created user has no records");
		});
		deferredResult.getValue('user');
		deferredResult.addMethodcaller('logout');

		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user2, 'getDirectLogins');
		deferredResult.addCallback(function (someDirectLogins) {
			SimpleTest.is(MochiKit.Base.keys(someDirectLogins).length, 0, "The user has no direct logins");
		});

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'registerNewUserAndAddARecord_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user, user2;
		var username;
		var passphrase;
		
		username = "new";
		passphrase = "user";

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user2 = new Clipperz.PM.DataModel.User({username:username, getPassphraseFunction:function () { return passphrase;}});
console.log("PROXY", proxy);
		deferredResult = new Clipperz.Async.Deferred("registerNewUserAndAddARecord_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_with_preferences_and_OTPs_data']);

		deferredResult.addCallback(Clipperz.PM.DataModel.User.registerNewAccount, username, function () { return MochiKit.Async.succeed(passphrase);});
		deferredResult.setValue('user');

		deferredResult.addMethodcaller('getRecords');
		deferredResult.addCallback(function (someRecords) {
			SimpleTest.is(MochiKit.Base.keys(someRecords).length, 0, "The newly created user has no records");
		});

		deferredResult.getValue('user');
		deferredResult.addMethodcaller('createNewRecord');
		deferredResult.addCallback(function (aNewRecord) {
			var innerDeferredResult;
			
			innerDeferredResult = new Clipperz.Async.Deferred("addRecordAndSaveChangesMultipleTimes_test <internal [1]>", {trace:false});

			innerDeferredResult.addMethod(aNewRecord, 'setLabel', "New record 1");
			innerDeferredResult.addMethod(aNewRecord, 'addField', {'label':'record number', 'value':"1", 'isHidden':false});
			innerDeferredResult.addMethod(aNewRecord, 'addField', {'label':'field count', 'value':"2", 'isHidden':false});
			innerDeferredResult.callback();
			
			return innerDeferredResult;
		})

		deferredResult.getValue('user');
		deferredResult.addMethodcaller('saveChanges');
		deferredResult.addCallback(SimpleTest.ok, true, "Saving worked (apparently) fine");


		deferredResult.getValue('user');
		deferredResult.addMethodcaller('logout');

		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user2, 'getRecords');
		deferredResult.addCallback(function (someDirectLogins) {
			SimpleTest.is(MochiKit.Base.keys(someDirectLogins).length, 1, "The user - even after a brand new login - has the newly created record");
		});

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'changePassphrase_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var user2;
		var newPassphrase;
		
		newPassphrase = 'zreppilc';
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		user2 = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return newPassphrase;}});

		deferredResult = new Clipperz.Async.Deferred("changePassphrase_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);

		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(20, "This account has oly a single card");

		deferredResult.addMethod(user, 'changePassphrase', newPassphrase);
		deferredResult.addMethod(user, 'logout');

		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user2, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(20, "This account has oly a single card");

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------
/*
	'rearrangeRecordFieldOrderAndSave_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

		deferredResult = new Clipperz.Async.Deferred("simpleSaveChanges_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(function (someFields) {
			var fields;
			
			fields = MochiKit.Base.values(someFields);
			SimpleTest.is(fields.length, 3, "The record has initially 3 fields");
			SimpleTest.is(fields[0].reference(), '6a84c414866dd6d266186f0255a595e9330fb34973c085a81a6e4906876c721b', "the first field is the expected one");
			SimpleTest.is(fields[1].reference(), 'fde88847cdbae6f7ee7e38aca1a242492888ff430a79c997bc6ba4afd0540ca2', "the second field is the expected one");
			SimpleTest.is(fields[2].reference(), 'bd4e3bb9d3497f63c4c3a507d4b80f489fdb57deb9d1b342a5e1cff65936a410', "the third field is the expected one");
		});

//		"6a84c414866dd6d266186f0255a595e9330fb34973c085a81a6e4906876c721b": {
//			"label":"Label 1","value":"Value 1","type":"TXT","hidden":false
//		},
//		"fde88847cdbae6f7ee7e38aca1a242492888ff430a79c997bc6ba4afd0540ca2": {
//			"label":"Label 2","value":"Value 2","type":"PWD","hidden":true
//		},
//		"bd4e3bb9d3497f63c4c3a507d4b80f489fdb57deb9d1b342a5e1cff65936a410": {
//			"label":"Label 3","value":"http://www.example.com","type":"URL","hidden":false
//		}
		
		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addCallback(MochiKit.Base.methodcaller('sortFieldReference', [
			'bd4e3bb9d3497f63c4c3a507d4b80f489fdb57deb9d1b342a5e1cff65936a410',
			'6a84c414866dd6d266186f0255a595e9330fb34973c085a81a6e4906876c721b',
			'bd4e3bb9d3497f63c4c3a507d4b80f489fdb57deb9d1b342a5e1cff65936a410'
		]));

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addMethodcaller('hasPendingChanges');
		deferredResult.addTest(true, "adding a field should mark the record as having pending changes");

		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(function (someFields) {
			SimpleTest.is(MochiKit.Base.values(someFields).length, 4, "The record has now 4 fields");
		});

		deferredResult.addMethod(user, 'saveChanges');
		deferredResult.addMethod(user, 'hasPendingChanges');
		deferredResult.addTest(false, "saving changes should return the user to a state with not changes pending - 4");

		deferredResult.callback();

		return deferredResult;
	},
*/
    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};



//#############################################################################

SimpleTest.runDeferredTests("Clipperz.PM.DataModel.User", tests, {trace:false});
