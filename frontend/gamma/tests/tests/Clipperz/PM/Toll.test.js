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
/*
	'simple_test': function() {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("simple_tests", {trace:false});
		deferredResult.addCallback(function() {
			var	tollTargetValue;
			var	toll;
			var price;
		
			tollTargetValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32);
			tollTargetValue = Clipperz.Crypto.SHA.sha256(tollTargetValue);
		
			price = 1;
			tollTargetValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32);
			toll = new Clipperz.PM.Toll({targetValue:tollTargetValue.toHexString(), cost:price});
			toll.pay();

			price = 5;
			tollTargetValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32);
			toll = new Clipperz.PM.Toll({targetValue:tollTargetValue.toHexString(), cost:price});
			toll.pay();

			price = 6;
			tollTargetValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32);
			toll = new Clipperz.PM.Toll({targetValue:tollTargetValue.toHexString(), cost:price});
			toll.pay();

			price = 7;
			tollTargetValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32);
			toll = new Clipperz.PM.Toll({targetValue:tollTargetValue.toHexString(), cost:price});
			toll.pay();

			price = 7;
			tollTargetValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32);
			toll = new Clipperz.PM.Toll({targetValue:tollTargetValue.toHexString(), cost:price});
			toll.pay();
		});
		
		deferredResult.callback();
		
		return deferredResult;
	},
*/
    //-------------------------------------------------------------------------

	'deferredPay_test': function() {
		var	deferredResult;
		var	tollTargetValue;
		var toll_1, toll_5, toll_6, toll_7, toll_10, toll_16;
		
//		tollTargetValue =  Clipperz.Crypto.SHA.sha256(Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32));



		deferredResult = new Clipperz.Async.Deferred("deferredPay_tests", {trace:false});

		tollTargetValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32).toHexString().substring(2);
		toll_1 = new Clipperz.PM.Toll({targetValue:tollTargetValue, cost:1});

		deferredResult.addMethod(toll_1, 'deferredPay');
		deferredResult.addCallback(function (aResult) {
			SimpleTest.ok(Clipperz.PM.Toll.validate(aResult.targetValue, aResult.toll, 1), "Payed a toll of cost 1");
		});
		deferredResult.addCallback(MochiKit.Async.wait, 1);

		tollTargetValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32).toHexString().substring(2);
		toll_5 = new Clipperz.PM.Toll({targetValue:tollTargetValue, cost:5});

		deferredResult.addMethod(toll_5, 'deferredPay');
		deferredResult.addCallback(function (aResult) {
			SimpleTest.ok(Clipperz.PM.Toll.validate(aResult.targetValue, aResult.toll, 5), "Payed a toll of cost 5");
		});
		deferredResult.addCallback(MochiKit.Async.wait, 1);


		tollTargetValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32).toHexString().substring(2);
		toll_6 = new Clipperz.PM.Toll({targetValue:tollTargetValue, cost:6});

		deferredResult.addMethod(toll_6, 'deferredPay');
		deferredResult.addCallback(function (aResult) {
			SimpleTest.ok(Clipperz.PM.Toll.validate(aResult.targetValue, aResult.toll, 6), "Payed a toll of cost 6");
		});
		deferredResult.addCallback(MochiKit.Async.wait, 1);


		tollTargetValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32).toHexString().substring(2);
		toll_7 = new Clipperz.PM.Toll({targetValue:tollTargetValue, cost:7});

		deferredResult.addMethod(toll_7, 'deferredPay');
		deferredResult.addCallback(function (aResult) {
			SimpleTest.ok(Clipperz.PM.Toll.validate(aResult.targetValue, aResult.toll, 7), "Payed a toll of cost 7");
		});
		deferredResult.addCallback(MochiKit.Async.wait, 1);


		tollTargetValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32).toHexString().substring(2);
		toll_10 = new Clipperz.PM.Toll({targetValue:tollTargetValue, cost:10});

		deferredResult.addMethod(toll_10, 'deferredPay');
		deferredResult.addCallback(function (aResult) {
			SimpleTest.ok(Clipperz.PM.Toll.validate(aResult.targetValue, aResult.toll, 10), "Payed a toll of cost 10");
		});
		deferredResult.addCallback(MochiKit.Async.wait, 1);

/*
		tollTargetValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32).toHexString().substring(2);
		toll_16 = new Clipperz.PM.Toll({targetValue:tollTargetValue, cost:16});

		deferredResult.addMethod(toll_16, 'deferredPay');
		deferredResult.addCallback(function (aResult) {
			SimpleTest.ok(Clipperz.PM.Toll.validate(aResult.targetValue, aResult.toll, 16), "Payed a toll of cost 16");
		});
		deferredResult.addCallback(MochiKit.Async.wait, 1);
*/

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};

//#############################################################################

runTests = function() {
	try {
		var deferredTests;
		var aTestName;

		deferredTests = new Clipperz.Async.Deferred("Clipperz.PM.Toll.test", {trace:false});
	
		aTestName = window.location.href.match(/#.*/);
		if (aTestName && (aTestName != '#')) {
			aTestName = aTestName[0].slice(1);
			if (aTestName in tests) {
	//Clipperz.log("single test execution, via fragment identifier", aTestName);
				deferredTests.addCallback(tests[aTestName]);
				deferredTests.addErrback(SimpleTest.ok, false, aTestName);
			} else {
				deferredTests.addBoth(is, aTestName, null, "Wrong test name selected to run");
			}
		} else {
			for (aTestName in tests) {
				deferredTests.addCallback(tests[aTestName]);
				deferredTests.addErrback(SimpleTest.ok, false, aTestName);
			}
			deferredTests.addBoth(is, true, true, "FINISH: completed the full stack of tests");
		}
		deferredTests.addBoth(SimpleTest.finish);
		deferredTests.callback();

		SimpleTest.waitForExplicitFinish();
	} catch (err) {
		var s = "test suite failure!\n";
		var o = {};
		var k = null;
		for (k in err) {
			// ensure unique keys?!
			if (!o[k]) {
				s +=  k + ": " + err[k] + "\n";
				o[k] = err[k];
			}
		}
		ok ( false, s );
	}
}();
