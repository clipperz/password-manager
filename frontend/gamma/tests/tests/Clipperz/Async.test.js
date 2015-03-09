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

testObject = function (aLabel) {
	this._label = aLabel;
	this._sibling = null;
}

testObject.prototype = {
	'label': function () {
		return MochiKit.Async.succeed(this._label);
	},

	'shouldHit': function () {
		var filterRegExp;
		
		filterRegExp = new RegExp("hit", "i");

		return Clipperz.Async.callbacks("testObject.shouldHit", [
			MochiKit.Base.method(this, 'label'),
			MochiKit.Base.method(filterRegExp, 'test')
		]);
	},
	
	'sibling': function () {
		return this._sibling;
	},
	
	'setSibling': function (aSibling) {
		this._sibling = aSibling;
	}
	
}

var tests = {

    //-------------------------------------------------------------------------

	'succeedingForkedDeferrer_test': function (someTestArgs) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('Async.test succeedingForkedDeferred', someTestArgs);
		deferredResult.addCallback(Clipperz.Async.forkAndJoin("Async.test succeedingForkedDeferred",
			[
				MochiKit.Base.partial(MochiKit.Async.succeed, 3),
				MochiKit.Base.partial(MochiKit.Async.succeed, 2)
			], someTestArgs
		));
		deferredResult.addCallback(function (anAsyncResult) { 
			var sum;
			
			sum = MochiKit.Iter.reduce(MochiKit.Base.operator.add, anAsyncResult);
			SimpleTest.is(sum, 5, "the sum of all the returned results should be 5");
		});
		deferredResult.addErrback(function() {
			SimpleTest.ok(false, "forkAndJoin should succeed and execution path should NOT go through here (1)")
		})
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'succeedingForkedAndWaitDeferrer_test': function (someTestArgs) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('succeedingForkedAndWaitDeferrer_test', someTestArgs);
		deferredResult.addCallback(Clipperz.Async.forkAndJoin("Async.test succeedingForkedAndWaitDeferrer",
			[
				MochiKit.Base.partial(MochiKit.Async.callLater, 2, MochiKit.Async.succeed, 3),
				MochiKit.Base.partial(MochiKit.Async.callLater, 1, MochiKit.Async.succeed, 2),
				MochiKit.Base.partial(MochiKit.Async.callLater, 3, MochiKit.Async.succeed, 7)
			], someTestArgs
		));
		deferredResult.addCallback(function (anAsyncResult) { 
			var sum;
			
			sum = MochiKit.Iter.reduce(MochiKit.Base.operator.add, anAsyncResult);
			SimpleTest.is(sum, 12, "the sum of all the returned results should be 12");
		});
		deferredResult.addErrback(function() {
			SimpleTest.ok(false, "forkAndJoin should succeed and execution path should NOT go through here (2)")
		})
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'failingForkedDeferrer_test': function (someTestArgs) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('Async.test failingForkedDeferred', someTestArgs);
		deferredResult.addCallback(Clipperz.Async.forkAndJoin("Async.test failingForkedDeferred",
			[
				MochiKit.Async.succeed,
				MochiKit.Async.fail
			], someTestArgs
		));
		deferredResult.addCallback(function () { 
			SimpleTest.ok(false, "forkAndJoin should fail, and execution path should NOT go through here");
		});
		deferredResult.addErrback(function() {
			SimpleTest.ok(true, "forkAndJoin should fail and execution path should go through here")
		})
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'succeedingCollectResultsDeferrer_test': function (someTestArgs) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('Async.test succeedingCollectResultsDeferrer', someTestArgs);
		deferredResult.addCallback(Clipperz.Async.collectResults('Async.test succeedingCollectResultsDeferrer', {
			'first':	MochiKit.Base.partial(MochiKit.Async.callLater, 2, MochiKit.Async.succeed, 3),
			'second':	MochiKit.Base.partial(MochiKit.Async.callLater, 1, MochiKit.Async.succeed, 2),
			'third':	MochiKit.Base.partial(MochiKit.Async.callLater, 3, MochiKit.Async.succeed, 7)
		}, someTestArgs));
		deferredResult.addCallback(function (anAsyncResult) { 
			var sum;
			
			sum = MochiKit.Iter.reduce(MochiKit.Base.operator.add, MochiKit.Base.values(anAsyncResult));
			SimpleTest.is(sum, 12, "the sum of all the returned results should be 12");
			SimpleTest.is(anAsyncResult['first'], 3, "the result of the 'first' item is 3");
			SimpleTest.is(anAsyncResult['second'], 2, "the result of the 'second' item is 2");
			SimpleTest.is(anAsyncResult['third'], 7, "the result of the 'third' item is 7");
			SimpleTest.is(MochiKit.Base.keys(anAsyncResult).length, 3, "the result has exactly 3 values");
		});
		deferredResult.addErrback(function() {
			SimpleTest.ok(false, "collectResults should succeed and execution path should NOT go through here");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'collectResultsWithParameter_test': function (someTestArgs) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('collectResultsWithParameter_test', someTestArgs);
		deferredResult.collectResults({
			'add_3':	MochiKit.Base.partial(MochiKit.Base.operator.add, 3),
			'sub_8':	MochiKit.Base.partial(MochiKit.Base.operator.sub, 8),
			'mul_4':	MochiKit.Base.partial(MochiKit.Base.operator.mul, 4)
		});
		deferredResult.addCallback(function (anAsyncResult) { 
			SimpleTest.is(anAsyncResult['add_3'],  8, "adding 3 to the passed value (5) returns 8");
			SimpleTest.is(anAsyncResult['sub_8'],  3, "subtracting the passed value (5) to 8 returns 3");
			SimpleTest.is(anAsyncResult['mul_4'], 20, "multiplying the passed value (5) by 4 returns 20");
			SimpleTest.is(MochiKit.Base.keys(anAsyncResult).length, 3, "the result has exactly 3 values");
		});
		deferredResult.callback(5);
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'succeedingCollectResultsDeferrer_alternative_syntax_test': function (someTestArgs) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('succeedingCollectResultsDeferrer_alternative_syntax_test', someTestArgs);
		deferredResult.collectResults({
			'first':	MochiKit.Base.partial(MochiKit.Async.callLater, 2, MochiKit.Async.succeed, 3),
			'second':	MochiKit.Base.partial(MochiKit.Async.callLater, 1, MochiKit.Async.succeed, 2),
			'third':	MochiKit.Base.partial(MochiKit.Async.callLater, 3, MochiKit.Async.succeed, 7)
		});
		deferredResult.addCallback(function (anAsyncResult) { 
			var sum;
			
			sum = MochiKit.Iter.reduce(MochiKit.Base.operator.add, MochiKit.Base.values(anAsyncResult));
			SimpleTest.is(sum, 12, "the sum of all the returned results should be 12");
			SimpleTest.is(anAsyncResult['first'], 3, "the result of the 'first' item is 3");
			SimpleTest.is(anAsyncResult['second'], 2, "the result of the 'second' item is 2");
			SimpleTest.is(anAsyncResult['third'], 7, "the result of the 'third' item is 7");
			SimpleTest.is(MochiKit.Base.keys(anAsyncResult).length, 3, "the result has exactly 3 values");
		});
		deferredResult.addErrback(function() {
			SimpleTest.ok(false, "collectResults should succeed and execution path should NOT go through here");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'failingCollectResultsDeferrer_test': function (someTestArgs) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('Async.test failingCollectResultsDeferrer', someTestArgs);
		deferredResult.addCallback(Clipperz.Async.collectResults('Async.test failingCollectResultsDeferrer', {
			'succeed':	MochiKit.Base.partial(MochiKit.Async.succeed, 3),
			'fail':		MochiKit.Base.partial(MochiKit.Async.fail, 2)
		}, someTestArgs));
		deferredResult.addCallback(function() {
			SimpleTest.ok(false, "collectResults should fail and execution path should NOT go through here");
		});
		deferredResult.addErrback(function (anAsyncResult) { 
			var result;
			
			result = anAsyncResult['message'];
			SimpleTest.is(MochiKit.Base.keys(result).length, 2, "the result has exactly 2 values");
//console.log("anAsyncResult", anAsyncResult);
			SimpleTest.ok(!(result['succeed'] instanceof Error), "the successful value is actually successful");
			SimpleTest.ok(result['fail'] instanceof Error, "the failed value is actually failed");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'collectResults_withSimpleArrayListOfCalls_test': function (someTestArgs) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('collectResults_withSimpleArrayListOfCalls_test', someTestArgs);
		deferredResult.addCallback(Clipperz.Async.collectResults('collectResults_withSimpleArrayListOfCalls_test - collectResults', {
			'path1': [
				MochiKit.Base.partial(MochiKit.Async.succeed, 3)
			],
			'path2': MochiKit.Base.partial(MochiKit.Async.succeed, 2)
		}, someTestArgs));
		deferredResult.addCallback(function (anAsyncResult) { 
			SimpleTest.is(anAsyncResult['path1'], 3, "the result of the first path is 3");
			SimpleTest.is(anAsyncResult['path2'], 2, "the result of the second path is 2");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'collectResults_withArrayListOfCalls_test': function (someTestArgs) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('collectResults_withArrayListOfCalls_test', someTestArgs);
		deferredResult.addCallback(Clipperz.Async.collectResults('collectResults_withArrayListOfCalls_test - collectResults', {
			'path1': [
				MochiKit.Base.partial(MochiKit.Async.succeed, {key1:'value1', key2:'value2'}),
				Clipperz.Base.serializeJSON
			],
			'path2': [
				MochiKit.Base.partial(MochiKit.Async.succeed, {key3:'value3', key4:'value4'}),
				Clipperz.Base.serializeJSON
			]
		}, someTestArgs));
		deferredResult.addCallback(function (anAsyncResult) { 
			SimpleTest.is(anAsyncResult['path1'], "{\"key1\":\"value1\",\"key2\":\"value2\"}", "the result of the first path is correct");
			SimpleTest.is(anAsyncResult['path2'], "{\"key3\":\"value3\",\"key4\":\"value4\"}", "the result of the second path is correct");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'deferredSort_test': function (someTestArgs) {
		var deferredResult;
		var	testArray;

		testArray = [
			{key:'key2', label:function () { return MochiKit.Async.succeed('label2')} },
			{key:'key3', label:function () { return MochiKit.Async.succeed('label3')} },
			{key:'key7', label:function () { return MochiKit.Async.succeed('label7')} },
			{key:'key1', label:function () { return MochiKit.Async.succeed('label1')} },
			{key:'key5', label:function () { return MochiKit.Async.succeed('label5')} },
			{key:'key4', label:function () { return MochiKit.Async.succeed('label4')} },
			{key:'key9', label:function () { return MochiKit.Async.succeed('label9')} },
			{key:'key8', label:function () { return MochiKit.Async.succeed('label8')} },
			{key:'key6', label:function () { return MochiKit.Async.succeed('label6')} }
		]

		deferredResult = new Clipperz.Async.Deferred('deferredSort_test', someTestArgs);
		deferredResult.addCallback(Clipperz.Async.deferredSort, function (aObject, bObject) {
			var result;

			result = Clipperz.Async.deferredCompare(MochiKit.Base.compare, aObject.label(), bObject.label());
			
			return result;
		});
		deferredResult.addCallback(function (aSortedArray) {
			SimpleTest.is(aSortedArray.length, testArray.length, "The sorted array should have the same number of objects as the original one");
			SimpleTest.is(aSortedArray[0]['key'], 'key1', "[0] -> key1");
			SimpleTest.is(aSortedArray[1]['key'], 'key2', "[1] -> key2");
			SimpleTest.is(aSortedArray[2]['key'], 'key3', "[2] -> key3");
			SimpleTest.is(aSortedArray[3]['key'], 'key4', "[3] -> key4");
			SimpleTest.is(aSortedArray[4]['key'], 'key5', "[4] -> key5");
			SimpleTest.is(aSortedArray[5]['key'], 'key6', "[5] -> key6");
			SimpleTest.is(aSortedArray[6]['key'], 'key7', "[6] -> key7");
			SimpleTest.is(aSortedArray[7]['key'], 'key8', "[7] -> key8");
			SimpleTest.is(aSortedArray[8]['key'], 'key9', "[8] -> key9");
		});
		deferredResult.callback(testArray);
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'callbacks_test': function (someTestArgs) {
		return Clipperz.Async.callbacks("callbacks_test", [
			MochiKit.Base.partial(MochiKit.Base.operator.add, 5),
			Clipperz.Async.Test.is(15, "Clipperz.Async.callbacks seems to work")
		], someTestArgs, 10);
	},

    //-------------------------------------------------------------------------

	'deferredAcquireLockTest': function (someTestArgs) {
		var deferredLock;
		var deferredResult;
		var anotherDeferred;
		var deferredResults;
		
		deferredLock = new MochiKit.Async.DeferredLock();
		deferredResult = new Clipperz.Async.Deferred("acquireLockTest", {trace:false});
		deferredResult.acquireLock(deferredLock);
		deferredResult.addCallback(function () {
			SimpleTest.is(true, deferredLock.locked, 'DeferredLock is locked after Clipperz.Async.Deferred.acquireLock');
		});
		
		anotherDeferred = new Clipperz.Async.Deferred("acquireLockLockedTest", {trace:false});
		anotherDeferred.acquireLock(deferredLock);
		anotherDeferred.addCallback(function () {
			SimpleTest.ok(false, 'Did not wait on a locked DeferredLock')
		});
		anotherDeferred.addErrback(function (anError) {
			SimpleTest.ok(true, 'Did wait on a locked DeferredLock');				
		});
		
		deferredResults = new MochiKit.Async.DeferredList([deferredResult, anotherDeferred], false, false, false);
		MochiKit.Async.callLater(1, function () {
			anotherDeferred.cancel();
		});
		
		deferredResult.callback();
		anotherDeferred.callback();
		return deferredResults;
	},

    //-------------------------------------------------------------------------

	'deferredLockAcquireTest': function (someTestArgs) {
		var deferredLock;
		var defferedResult;
		var anotherDeferred;
		var deferredResults;
		
		deferredLock = new MochiKit.Async.DeferredLock();
		deferredResult = new Clipperz.Async.Deferred("acquireLockTest", {trace:false});
		deferredResult.addMethod(deferredLock, 'acquire');
		deferredResult.addCallback(function () {
			SimpleTest.is(true, deferredLock.locked, 'DeferredLock is locked after Clipperz.Async.Deferred.acquireLock');
		});
		
		anotherDeferred = new Clipperz.Async.Deferred("acquireLockLockedTest", {trace:false});
		anotherDeferred.addMethod(deferredLock, 'acquire');
		anotherDeferred.addCallback(function () {
			SimpleTest.ok(false, 'Did not wait on a locked DeferredLock')
		});
		anotherDeferred.addErrback(function (anError) {
			SimpleTest.ok(true, 'Did wait on a locked DeferredLock');				
		});
		

		deferredResults = new MochiKit.Async.DeferredList([deferredResult, anotherDeferred], false, false, false);
		MochiKit.Async.callLater(1, function () {
			anotherDeferred.cancel();
		});
		
		deferredResult.callback();
		anotherDeferred.callback()

		return deferredResults;
	},

    //-------------------------------------------------------------------------

	'deferredFilter_test': function (someTestArgs) {
		var deferredResult;
		var	testObjects;
		var filterRegExp;
		var deferredFilterFunction;

		testObjects = [
			{key: '1', label: MochiKit.Base.partial(MochiKit.Async.succeed, 'skip  1') },
			{key: '2', label: MochiKit.Base.partial(MochiKit.Async.succeed, 'hit   2') },
			{key: '3', label: MochiKit.Base.partial(MochiKit.Async.succeed, 'skip  3') },
			{key: '4', label: MochiKit.Base.partial(MochiKit.Async.succeed, 'hit   4') },
			{key: '5', label: MochiKit.Base.partial(MochiKit.Async.succeed, 'skip  5') },
			{key: '6', label: MochiKit.Base.partial(MochiKit.Async.succeed, 'skip  6') },
			{key: '7', label: MochiKit.Base.partial(MochiKit.Async.succeed, 'hit   7') },
			{key: '8', label: MochiKit.Base.partial(MochiKit.Async.succeed, 'skip  8') },
			{key: '9', label: MochiKit.Base.partial(MochiKit.Async.succeed, 'skip  9') },
			{key:'10', label: MochiKit.Base.partial(MochiKit.Async.succeed, 'hit  10') }
		]

		filterRegExp = new RegExp("hit", "i");

		deferredFilterFunction = function (aRegExp, anObject) {
			return Clipperz.Async.callbacks("deferredFilterFunction", [
				MochiKit.Base.method(anObject, 'label'),
				MochiKit.Base.method(aRegExp, 'test'),
				function (doesItMatch) {
					var result;
					
					if (doesItMatch) {
						result = MochiKit.Async.succeed('match');
					} else {
						result = MochiKit.Async.fail('miss');
					}
					
					return result;
				}
			], someTestArgs);
		};

		deferredResult = new Clipperz.Async.Deferred("deferredFilter_test", someTestArgs);
		deferredResult.addCallback(Clipperz.Async.deferredFilter, MochiKit.Base.partial(deferredFilterFunction, filterRegExp), testObjects);
		deferredResult.addCallback(function (aResult) {
			SimpleTest.is(aResult.length, 4, "There are 4 items matching the 'hit' regexp");
			SimpleTest.is(aResult[0]['key'],  '2', "The first item to match is the one with key:2");
			SimpleTest.is(aResult[1]['key'],  '4', "The first item to match is the one with key:4");
			SimpleTest.is(aResult[2]['key'],  '7', "The first item to match is the one with key:7");
			SimpleTest.is(aResult[3]['key'], '10', "The first item to match is the one with key:10");
		})
		deferredResult.callback();
		
		return deferredResult;
	},

     //-------------------------------------------------------------------------

	'deferredFilter_methodcaller_test': function (someTestArgs) {
		var deferredResult;
		var	testObjects;
		var filterRegExp;
		var deferredFilterFunction;

		testObjects = [
			new testObject('skip 1'),
			new testObject('hit  2'),
			new testObject('skip 3'),
			new testObject('hit  4'),
			new testObject('skip 5'),
			new testObject('skip 6'),
			new testObject('hit  7'),
			new testObject('skip 8'),
			new testObject('skip 9'),
			new testObject('hit 10')
		];

		deferredResult = new Clipperz.Async.Deferred("deferredFilter_methodcaller_test", someTestArgs);
//		deferredResult.addCallback(function () { return testObjects[0]; });
		deferredResult.addCallback(MochiKit.Async.succeed, testObjects[0]);
		deferredResult.addCallback(MochiKit.Base.methodcaller('shouldHit'));
		deferredResult.addTest(false, "the first element shoud return 'false' to the 'shouldHit' method");

		deferredResult.addCallback(MochiKit.Async.succeed, testObjects[1]);
		deferredResult.addCallback(MochiKit.Base.methodcaller('shouldHit'));
		deferredResult.addTest(true, "the second element shoud return 'true' to the 'shouldHit' method");

		deferredResult.addCallback(MochiKit.Async.succeed, testObjects);
		deferredResult.addCallback(Clipperz.Async.deferredFilter, MochiKit.Base.methodcaller('shouldHit'));
		deferredResult.addCallback(function (aResult) {
			SimpleTest.is(aResult.length, 4, "There are 4 items matching the 'hit' regexp");
			SimpleTest.is(aResult[0]._label, 'hit  2', "The first item to match is the one with key:2");
			SimpleTest.is(aResult[1]._label, 'hit  4', "The first item to match is the one with key:4");
			SimpleTest.is(aResult[2]._label, 'hit  7', "The first item to match is the one with key:7");
			SimpleTest.is(aResult[3]._label, 'hit 10', "The first item to match is the one with key:10");
		})
		deferredResult.callback();
		
		return deferredResult;
	},

   //-------------------------------------------------------------------------

	'setItem_test': function (someTestArgs) {
		var deferredResult;
		var result;

		result = {};
		deferredResult = new Clipperz.Async.Deferred("setItem_test", someTestArgs);
		deferredResult.addCallback(MochiKit.Async.succeed, "Value 1");
		deferredResult.addCallback(Clipperz.Async.setItem, result, 'key1');
		deferredResult.addCallback(MochiKit.Async.succeed, "Value 2");
		deferredResult.addCallback(Clipperz.Async.setItem, result, 'key2');

		deferredResult.addCallback(MochiKit.Base.itemgetter('key1'));
		deferredResult.addTest("Value 1", "the value for the 'key1' does match");

		deferredResult.addCallback(MochiKit.Async.succeed, result);
		deferredResult.addCallback(MochiKit.Base.itemgetter('key2'));
		deferredResult.addTest("Value 2", "the value for the 'key2' does match");
		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'setItemOnObject_test': function (someTestArgs) {
		var deferredResult;
		var result;

		result = {};

		deferredResult = new Clipperz.Async.Deferred("setItemOnObject_test", someTestArgs);
		deferredResult.addCallback(MochiKit.Async.succeed, result);
		deferredResult.addCallback(Clipperz.Async.setItemOnObject, 'key1', "Value 1");
		deferredResult.addCallback(Clipperz.Async.setItemOnObject, 'key2', "Value 2");

		deferredResult.addCallback(MochiKit.Base.itemgetter('key1'));
		deferredResult.addTest("Value 1", "the value for the 'key1' does match");

		deferredResult.addCallback(MochiKit.Async.succeed, result);
		deferredResult.addCallback(MochiKit.Base.itemgetter('key2'));
		deferredResult.addTest("Value 2", "the value for the 'key2' does match");
		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'setDeferredItemOnObject_test': function (someTestArgs) {
		var deferredResult;
		var result;

		result = {};

		deferredResult = new Clipperz.Async.Deferred("setDeferredItemOnObject_test", someTestArgs);
		deferredResult.addCallback(MochiKit.Async.succeed, result);
		deferredResult.addCallback(Clipperz.Async.setDeferredItemOnObject, 'key1', MochiKit.Base.partial(MochiKit.Async.succeed, "Value 1"));
		deferredResult.addCallback(Clipperz.Async.setDeferredItemOnObject, 'key2', MochiKit.Base.partial(MochiKit.Async.succeed, "Value 2"));

		deferredResult.addCallback(MochiKit.Base.itemgetter('key1'));
		deferredResult.addTest("Value 1", "the value for the 'key1' does match");

		deferredResult.addCallback(MochiKit.Async.succeed, result);
		deferredResult.addCallback(MochiKit.Base.itemgetter('key2'));
		deferredResult.addTest("Value 2", "the value for the 'key2' does match");
		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'addIf_test': function (someTestArgs) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("addIf_test", someTestArgs);

		deferredResult.addCallback(MochiKit.Async.succeed, true);
		deferredResult.addIf([
			MochiKit.Base.partial(SimpleTest.ok, true, "when a true value is passed, the 'then' branch is executed")
		], [
			MochiKit.Base.partial(SimpleTest.ok, false, "when a true value is passed, the 'else' branch should NOT be executed")
		]);

		deferredResult.addCallback(MochiKit.Async.succeed, false);
		deferredResult.addIf([
			MochiKit.Base.partial(SimpleTest.ok, false, "when a false value is passed, the 'then' branch should NOT be executed")
		], [
			MochiKit.Base.partial(SimpleTest.ok, true, "when a false value is passed, the 'else' branch is executed")
		]);

		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'addMethod_test': function (someTestArgs) {
		var deferredResult;
		var testObjectInstance;
		var testObjectInstance_2;
		var label;
		var	testObjectInstanceGetter;

		label = "Test - Label";
		testObjectInstance = new testObject(label);
		testObjectInstance_2 = new testObject(label + label);
		testObjectInstance.setSibling(testObjectInstance_2);

		testObjectInstanceGetter = function () {
			return testObjectInstance;
		}

		deferredResult = new Clipperz.Async.Deferred("addMethod_test", someTestArgs);
		deferredResult.addMethod(testObjectInstance, 'label');
		deferredResult.addTest(label, "the addMethod seems to work");

		deferredResult.addMethod(testObjectInstanceGetter(), 'label');
		deferredResult.addTest(label, "the addMethod seems to work");

		deferredResult.addMethod(testObjectInstance.sibling(), 'label');
		deferredResult.addTest(label+label, "the addMethod seems to work");
		deferredResult.callback();

		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'callbacksWithErrors_test': function (someTestArgs) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("callbacksWithErrors_test", someTestArgs);
		deferredResult.addCallback(MochiKit.Base.partial(Clipperz.Async.callbacks, "callbacksWithErrors_test - callbacks", [
			function () { return 10; },
			function (aValue) { pippo = pluto + aValue; },
			function (aValue) { SimpleTest.ok(false, "this code should never be executed"); return aValue; }
		], {trace:someTestArgs}));
		deferredResult.addCallback(SimpleTest.ok, false, "the inner code should raise an exception and exit through the error chain");
		deferredResult.addErrback(SimpleTest.ok, true, "the inner code should raise an exception and exit through the error chain");
		
		deferredResult.callback();
		
		return deferredResult;
	},
	
    //-------------------------------------------------------------------------

	'deferredVars_test': function (someTestArgs) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("deferredVars_test", someTestArgs);
		deferredResult.addCallback(MochiKit.Async.succeed, "test string");
		deferredResult.setValue('testKey');
		deferredResult.addCallback(MochiKit.Async.succeed, "another string");
		deferredResult.getValue('testKey');
		deferredResult.addTest("test string", "The right string has been fetched");
		
		deferredResult.callback();
		
		return deferredResult;
	},
	
    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};



//#############################################################################

SimpleTest.runDeferredTests("Clipperz.Async", tests, {trace:false});
