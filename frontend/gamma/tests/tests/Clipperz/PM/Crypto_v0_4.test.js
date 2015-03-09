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

var tests = {
	
	'decryptDataEncryptedUsingPythonLibrary_test': function (someTestArgs) {
		var deferredResult;

		passphrase = 'trustno1';
		encryptedData = 'OucTxzBWmqm8jS7EUyIlWUWDPSFKvulL5iM4WwLPbNVIH7jtaK9pmzpm9w5ioVy2/tyebVwWr36t7QXSBOPwUPo2SlGmARCozA==';

		deferredResult = new Clipperz.Async.Deferred("decryptDataEncryptedUsingPythonLibrary_test", someTestArgs);
		deferredResult.addCallback(Clipperz.PM.Crypto.deferredDecrypt, {key:passphrase, value:encryptedData, version:'0.4'});
		deferredResult.addCallback(MochiKit.Base.itemgetter('message'));
		deferredResult.addTest("The quick brown fox jumps over the lazy dog", "expected value");
		
		deferredResult.callback();
		
		return deferredResult;
		
	},

    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
}

//=============================================================================

Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();
SimpleTest.runDeferredTests("Clipperz.PM.Crypto [0.4]", tests, {trace:false});
