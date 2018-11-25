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

Clipperz.PM.RunTime = {};
MochiKit.DOM.addLoadEvent(function () {
	var deferredResult;
	var proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});

	Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();
	Clipperz.PM.Strings.Languages.initSetup();

	Clipperz.PM.RunTime.mainController = new Clipperz.PM.UI.MainController();
	Clipperz.PM.RunTime.mainController.run({'shouldShowRegistrationForm':false});

	deferredResult = new Clipperz.Async.Deferred("CardDetail_test.init", {trace:false});

/* * /
	deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
	deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'doLogin', {username:'test', passphrase:'test'});
	deferredResult.wait(1);
	deferredResult.addCallback(function () { console.log("SHOW RECORD");});
	deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'showRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
/ **/
	deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
	deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'doLogin', {username:'joe', passphrase:'clipperz'});
/**/

	deferredResult.callback();

	return deferredResult;
});
