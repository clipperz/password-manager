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

Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();

var tests = {

    //-------------------------------------------------------------------------

	'isValidOneTimePasswordValue_test': function (someTestArgs) {
		var otp;
		var notOTP;

		otp = 'yaxx k7ww - f8y6 tqz5 - 58b6 th44 - 9cwv q0fg';
		SimpleTest.is(Clipperz.PM.DataModel.OneTimePassword.isValidOneTimePasswordValue(otp), true, "isValidOneTimePasswordValue [expect true] - test 1");

		notOTP = 'trustno1';
		SimpleTest.is(Clipperz.PM.DataModel.OneTimePassword.isValidOneTimePasswordValue(notOTP), false, "isValidOneTimePasswordValue [expect false] - test 1");

		return MochiKit.Async.succeed('done');
	},

    //-------------------------------------------------------------------------

	'loginUsingOtp_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user, user2;
		var username;
		var passphrase;
		
		username = "test";
		passphrase = "yaxx k7ww - f8y6 tqz5 - 58b6 th44 - 9cwv q0fg";	//	OTP

		proxy =	new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user =	new Clipperz.PM.DataModel.User({username:username, getPassphraseFunction:function () { return passphrase;}});
		user2 =	new Clipperz.PM.DataModel.User({username:username, getPassphraseFunction:function () { return passphrase;}});

		deferredResult = new Clipperz.Async.Deferred("loginUsingOtp_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_with_otps']);

		// TODO: Apparently these tests are skipped when they fail, not showing up in the results

		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "This account has one single card");

		deferredResult.addMethod(user, 'getOneTimePasswords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "This account has one OTP");

		deferredResult.addMethod(user, 'getOneTimePasswords');
		deferredResult.addCallback(MochiKit.Base.itemgetter(0));
		deferredResult.addCallback(MochiKit.Base.methodcaller('status'));
		deferredResult.addTest('USED', "The available OTP has been unsed to login, and should be marked accordingly");

		deferredResult.addMethod(user2, 'login');
		deferredResult.shouldFail("trying to login using the same OTP twice");

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'changePassphraseAndLoginUsingOtp_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var user2;
		var user3;
		var otp;
		
		otp = "yaxx k7ww - f8y6 tqz5 - 58b6 th44 - 9cwv q0fg";	//	OTP

		newPassphrase = 'tset';
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction: MochiKit.Base.partial(MochiKit.Async.succeed, 'test')});
		user2 = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction: MochiKit.Base.partial(MochiKit.Async.succeed, otp)});

		deferredResult = new Clipperz.Async.Deferred("changePassphraseAndLoginUsingOtp_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_with_otps']);

		deferredResult.addMethod(user, 'login');

		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "This account has only a single card");

		deferredResult.addMethod(user, 'changePassphrase', MochiKit.Base.partial(MochiKit.Async.succeed, newPassphrase));
		deferredResult.addMethod(user, 'logout');

		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user2, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "This account has oly a single card");
		deferredResult.addMethod(user2, 'logout');

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'loginUsingOtpAndWrongUsername_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user, user2;
		var username;
		var wrongUsername;
		var passphrase;
		
		username = "test";
		wrongUsername = "tset";
		passphrase = "yaxx k7ww - f8y6 tqz5 - 58b6 th44 - 9cwv q0fg";	//	OTP

		proxy =	new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user =	new Clipperz.PM.DataModel.User({username:wrongUsername, getPassphraseFunction:function () { return passphrase;}});
		user2 =	new Clipperz.PM.DataModel.User({username:username, getPassphraseFunction:function () { return passphrase;}});

		deferredResult = new Clipperz.Async.Deferred("loginUsingOtpAndWrongUsername_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_with_otps']);

		deferredResult.addMethod(user, 'login');
		deferredResult.shouldFail("login in using the wrong username with the OTP should fail");

		deferredResult.addMethod(user2, 'login');
		deferredResult.shouldFail("trying to reuse the same OTP should fail, even if now it is used with the correct username");

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'loginUserWithAPassphraseLookingExactlyLikeAnOTP_test': function (someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var username;
		var passphrase;
		
		username = "otp_user";
		passphrase = "yaxx k7ww - f8y6 tqz5 - 58b6 th44 - 9cwv q0fg";	//	passphrase

		proxy =	new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user =	new Clipperz.PM.DataModel.User({username:username, getPassphraseFunction:function () { return passphrase;}});

		deferredResult = new Clipperz.Async.Deferred("loginUserWithAPassphraseLookingExactlyLikeAnOTP_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['otp_user_test']);

		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "This account has one single card");

		deferredResult.callback();
		
		return deferredResult;
	},

	'loginWithANewOTP_test': function(someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var user2;
		var user3;
		var username;
		var passphrase;
		
		username = "1";
		passphrase = "1";

		proxy =	new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user =	new Clipperz.PM.DataModel.User({username:username, getPassphraseFunction:MochiKit.Base.partial(MochiKit.Async.succeed, passphrase)});
		user2 =	new Clipperz.PM.DataModel.User({username:username, getPassphraseFunction:function () { return "WILL_BE_CHANGED_WITH_OTP";}});
		user3 =	new Clipperz.PM.DataModel.User({username:username, getPassphraseFunction:function () { return "WILL_BE_CHANGED_WITH_OTP";}});

		deferredResult = new Clipperz.Async.Deferred("loginUserWithANewOTP_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['1/1_data']);

		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "This account has one single card");

		deferredResult.addMethod(user, 'getOneTimePasswords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(0, "There should be no OTPs initially");

		deferredResult.addMethod(user, 'createNewOTP');
		deferredResult.addMethod(user, 'getOneTimePasswords');
		deferredResult.setValue('otpList');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "There should be one single OTP now");

		deferredResult.getValue('otpList');
		deferredResult.addCallback(function(aList) {return MochiKit.Base.partial(MochiKit.Async.succeed, aList[0].password()); });
		deferredResult.addMethod(user2, 'setPassphraseFunction'),
		deferredResult.addMethod(user, 'logout');
		deferredResult.addMethod(user2, 'login');

		deferredResult.addMethod(user2, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "This account has one single card");

		deferredResult.getValue('otpList');
		deferredResult.addCallback(function(aList) {return MochiKit.Base.partial(MochiKit.Async.succeed, aList[0].password()); });
		deferredResult.addMethod(user3, 'setPassphraseFunction'),
		deferredResult.addMethod(user2, 'logout');
		deferredResult.addMethod(user3, 'login');
		deferredResult.shouldFail("Second login with the same OTP should fail");

		deferredResult.callback();

		return deferredResult;
	},

	'deleteOTP_test': function(someTestArgs) {
		var deferredResult;
		var proxy;
		var user;
		var user2;
		var username;
		var passphrase;
		
		username = "1";
		passphrase = "1";

		proxy =	new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});
		user =	new Clipperz.PM.DataModel.User({username:username, getPassphraseFunction:MochiKit.Base.partial(MochiKit.Async.succeed, passphrase)});
		user2 =	new Clipperz.PM.DataModel.User({username:username, getPassphraseFunction:function () { return "WILL_BE_CHANGED_WITH_OTP";}});

		deferredResult = new Clipperz.Async.Deferred("deleteOTP_test", someTestArgs);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['1/1_data']);

		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(user, 'getOneTimePasswords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(0, "There should be no OTPs initially");

		deferredResult.addMethod(user, 'createNewOTP');
		deferredResult.addMethod(user, 'createNewOTP');
		deferredResult.addMethod(user, 'createNewOTP');
		deferredResult.addMethod(user, 'createNewOTP');
		deferredResult.addMethod(user, 'createNewOTP');
		deferredResult.addMethod(user, 'createNewOTP');

		deferredResult.addMethod(user, 'getOneTimePasswords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(6, "There should be 6 OTPs now");

		deferredResult.addMethod(user, 'getOneTimePasswords');
		deferredResult.addCallback(function(aList) {return [aList[0].reference(), aList[1].reference()];});
		deferredResult.addMethod(user, 'deleteOTPs');
		deferredResult.addMethod(user, 'getOneTimePasswords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(4, "There should be 4 OTPs now");

		deferredResult.addMethod(user, 'getOneTimePasswords');
		deferredResult.addCallback(function(aList) {return [aList[0].reference(), aList[1].reference(), aList[2].reference()];});
		deferredResult.addMethod(user, 'deleteOTPs');
		deferredResult.addMethod(user, 'getOneTimePasswords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "There should be 1 OTP now");

		deferredResult.addMethod(user, 'getOneTimePasswords');
		deferredResult.addCallback(function(aList) {return MochiKit.Base.partial(MochiKit.Async.succeed, aList[0].password()); });
		deferredResult.addMethod(user2, 'setPassphraseFunction');
		deferredResult.addMethod(user, 'logout');
		deferredResult.addMethod(user2, 'login');
		deferredResult.addMethod(user2, 'getRecords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "Login with the remaining OTP should work");

		deferredResult.addMethod(user2, 'getOneTimePasswords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(1, "There should be 1 remaining OTP now");

		deferredResult.collectResults({
			'oneTimePasswords': MochiKit.Base.method(user2, 'getOneTimePasswords'),
			'oneTimePasswordsDetails': MochiKit.Base.method(user2, 'getOneTimePasswordsDetails')
		});
		deferredResult.addCallback(function(someData) {
			return someData['oneTimePasswordsDetails'][someData['oneTimePasswords'][0].reference()]['status'];
		});
		deferredResult.addTest('USED', "The remaining OTP should have 'USED' status");

		deferredResult.addMethod(user2, 'getOneTimePasswords');
		deferredResult.addCallback(function(aList) {return [aList[0].reference()];});
		deferredResult.addMethod(user2, 'deleteOTPs');
		deferredResult.addMethod(user2, 'getOneTimePasswords');
		deferredResult.addCallback(MochiKit.Base.itemgetter('length'));
		deferredResult.addTest(0, "There should be no OTPs left after the deletion of the last one");

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};



//#############################################################################

SimpleTest.runDeferredTests("Clipperz.PM.DataModel.OneTimePassword", tests, {trace:true});
