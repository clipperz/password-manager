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

	'simple_tests': function() {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("simple_tests", {trace:false});
		deferredResult.addCallback(function() {
			var proxy;
			
			proxy = new Clipperz.PM.Proxy();
			ok(proxy != null, "can create instances of the Proxy class");
			is(proxy.shouldPayTolls(), false, "proxy is set to NOT pay tolls by default");
		});
		
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'simpleTestsWithTolls_tests': function() {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("simple_tests", {trace:false});
		deferredResult.addCallback(function() {
			var proxy;
			
			proxy = new Clipperz.PM.Proxy({shouldPayTolls:true});
			is(proxy.shouldPayTolls(), true, "I can set Proxy to pays tolls");
		});
		
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};

//#############################################################################

runTests = function(aClassName) {
	try {
		var deferredTests;
		var aTestName;

		deferredTests = new Clipperz.Async.Deferred(aClassName + ".test", {trace:false});
	
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
}("Clipperz.PM.Proxy");
