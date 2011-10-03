/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz's Javascript Crypto Library.
Javascript Crypto Library provides web developers with an extensive
and efficient set of cryptographic functions. The library aims to
obtain maximum execution speed while preserving modularity and
reusability.
For further information about its features and functionalities please
refer to http://www.clipperz.com

* Javascript Crypto Library is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Javascript Crypto Library is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Javascript Crypto Library.  If not, see
  <http://www.gnu.org/licenses/>.

*/

//Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();

var tests = {

	'isPassphraseDelegateLockSetAfterInit': function (someTestArgs) {
		return Clipperz.Async.callbacks('isPassphraseDelegateLockSetAfterInit', [
			function () {
				var mainController;
				
				mainController = new Clipperz.PM.UI.Web.Controllers.MainController();
				SimpleTest.is(true, mainController._passphraseDelegateLock.locked, 'passphraseDelegate lock is locked after MainController initialization')		
		}]);
	},

    //-------------------------------------------------------------------------
	
	'getPassphraseWithUnsetDelegate_test': function (someTestArgs) {
		var deferredResult;
		var mainController;
		var delegateFunction;
		
		mainController = new Clipperz.PM.UI.Web.Controllers.MainController();
		delegateFunction = function () { return "clipperz";};
		
		MochiKit.Async.callLater(1, MochiKit.Base.method(mainController, 'setPassphraseDelegate', delegateFunction));

		return Clipperz.Async.callbacks("MainController.getPassphraseWithUnsetDelegate",[
			MochiKit.Base.method(mainController, 'getPassphrase'),
			function (aResult) {
				SimpleTest.is(aResult, 'clipperz', "the password provided by the delegate is correct");
			}
		], {trace:false});
	},
	
    //-------------------------------------------------------------------------
	
	'getPassphraseWithSetDelegate_test': function (someTestArgs) {
		var deferredResult;
		var mainController;
		var delegateFunction;
		
		mainController = new Clipperz.PM.UI.Web.Controllers.MainController();
		delegateFunction = function () {return "clipperz";};
		
		return Clipperz.Async.callbacks("MainController.getPassphraseWithSetDelegate_test",[
			MochiKit.Base.method(mainController, 'setPassphraseDelegate', delegateFunction),
			MochiKit.Base.method(mainController, 'getPassphrase'),
			function (aResult) {
				SimpleTest.is(aResult, 'clipperz', "the password provided by the delegate is correct");
			}
		], {trace:false});
	},
	
    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};



//#############################################################################

SimpleTest.runDeferredTests("Clipperz.PM.UI.Web.Controllers.MainController", tests, {trace:false});
