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

/*
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
		...
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(user, 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');	


		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});
		...
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'getRecord', '5cdac63b317f3942da38f3a3de3b7f0e5d6678200951c6216230295550f63fb4');	//	WEB PASSWORD (0)
		deferredResult.addMethod(user, 'getRecord', '36ec1a41118813ced3553534fa2607d781cba687768db305beed368a8e06e113');	//	DIGG  (1)
		deferredResult.addMethod(user, 'getRecord', 'c0ce9130ca365bb02418d4305ea1d29e49c3f0e96d44b9d3cb6b4b6843d25065');	//	SAP   (2)
		deferredResult.addMethod(user, 'getRecord', 'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d');	//	YAHOO (4)
			DirectLogin references
			- dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496	//	Yahoo! Mail
			- aa18149164302d5dbe7e2d3724565b9550e00887b49978559783b2e38c625584	//	(Yahoo! Groups)
			- 6f7bbc4e42ea462b5246e6f51c3f86056bec50601ce2de6067c8c1d26f21c07f	//	(Flickr)
			- a7b32e72502804bf2946a2a8856139cbbb759c5777e6b3e673db1fdf7e3bd06e	//	(My Yahoo!)
*/
    //-------------------------------------------------------------------------

	'editDirectLoginLabel_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("editDirectLoginLabel_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecord', 'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d');	//	YAHOO (4)
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter('dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496'));
		deferredResult.addCallback(function (aDirectLogin) {
			var innerDeferredResult;
			var updatedLabel;
			
			updatedLabel = "updated Yahoo! Mail label";
			
			innerDeferredResult = new Clipperz.Async.Deferred("editDirectLoginLabel_test <inner>", someTestArgs);
			innerDeferredResult.addMethod(aDirectLogin, 'label');
			innerDeferredResult.addTest("Yahoo! Mail", "The label of the selected direct login is correct.");
			innerDeferredResult.addMethod(aDirectLogin, 'setLabel', updatedLabel);
			
			innerDeferredResult.addMethod(aDirectLogin, 'label');
			innerDeferredResult.addTest(updatedLabel, "The DirectLogin returns the correct label even before committing data.");

			innerDeferredResult.addMethod(user, 'hasPendingChanges');
			innerDeferredResult.addTest(true, "When changing the label of a DirectLogin, the user should report pending changes.");

//innerDeferredResult.addCallback(function () { console.log("+_+_+_+_+_+_+_+_+_+_+_+ >>>")});
			innerDeferredResult.addMethod(user, 'saveChanges');
//innerDeferredResult.addCallback(function () { console.log("+_+_+_+_+_+_+_+_+_+_+_+ <<<")});
			innerDeferredResult.addMethod(user, 'hasPendingChanges');
			innerDeferredResult.addTest(false, "After saving changes there should be no pending changes left.");

			innerDeferredResult.addMethod(aDirectLogin, 'label');
			innerDeferredResult.addTest(updatedLabel, "The DirectLogin returns the correct label even after committing data.");
			
			innerDeferredResult.callback();
			
			return innerDeferredResult;
		})

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'editDirectLoginLabel_thanResetItToThePreviousValue_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("editDirectLoginLabel_thanResetItToThePreviousValue_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecord', 'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d');	//	YAHOO (4)
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter('dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496'));
		deferredResult.addCallback(function (aDirectLogin) {
			var innerDeferredResult;
			var originalLabel;
			var updatedLabel;
			
			originalLabel = "Yahoo! Mail";
			updatedLabel  = "updated Yahoo! Mail label";
			
			innerDeferredResult = new Clipperz.Async.Deferred("editDirectLoginLabel_thanResetItToThePreviousValue_test <inner>", someTestArgs);
			innerDeferredResult.addMethod(aDirectLogin, 'label');
			innerDeferredResult.addTest(originalLabel, "The label of the selected direct login is correct.");
			innerDeferredResult.addMethod(aDirectLogin, 'setLabel', updatedLabel);
			
			innerDeferredResult.addMethod(aDirectLogin, 'label');
			innerDeferredResult.addTest(updatedLabel, "The DirectLogin returns the correct label even before committing data.");

			innerDeferredResult.addMethod(user, 'hasPendingChanges');
			innerDeferredResult.addTest(true, "When changing the label of a DirectLogin, the user should report pending changes [2].");

			innerDeferredResult.addMethod(aDirectLogin, 'setLabel', originalLabel);

			innerDeferredResult.addMethod(aDirectLogin, 'hasPendingChanges');
			innerDeferredResult.addTest(false, "Restoring the original label, the directLogin should report no pending changes.");
			
			innerDeferredResult.addMethod(aDirectLogin.record(), 'hasPendingChanges');
			innerDeferredResult.addTest(false, "Restoring the original label, the record should report no pending changes.");

			innerDeferredResult.addMethod(user, 'hasPendingChanges');
			innerDeferredResult.addTest(false, "Restoring the original label, the user should report no pending changes.");
			
			innerDeferredResult.callback();
			
			return innerDeferredResult;
		})

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------
/*
	'bindingValues_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

		deferredResult = new Clipperz.Async.Deferred("DirectLogin.bindingValues_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecord', 'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d');	//	YAHOO (4)
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter('dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496'));
		deferredResult.addMethodcaller('bindingValues');
		deferredResult.addCallback(function (someBindingValues) {
			SimpleTest.is(MochiKit.Base.keys(someBindingValues).length, 2, "there should be 2 bindings for this direct login");
			SimpleTest.is(someBindingValues['login'], 'joe.clipperz', "the login field should be filled with 'joe.clipperz'");
			SimpleTest.is(someBindingValues['passwd'], 'enfvDG1RxAsl', "the passwd field should be filled with 'enfvDG1RxAsl'");
		})

		deferredResult.callback();

		return deferredResult;
	},
*/
    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};

//#############################################################################

SimpleTest.runDeferredTests("Clipperz.PM.DataModel.DirectLogin", tests, {trace:false});
