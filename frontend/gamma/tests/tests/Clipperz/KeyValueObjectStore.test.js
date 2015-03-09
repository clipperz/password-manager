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

    //-------------------------------------------------------------------------

	'simple_tests': function() {
		var	deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("simple_tests", {trace:false});
		deferredResult.addCallback(function() {
			var	objectStore;
	
			objectStore = new Clipperz.KeyValueObjectStore();
	
			ok(objectStore != null, "created an object store");
	
			objectStore.setValue('key', "value");
			is(objectStore.getValue('key'), "value", "can store and read a value to a simple key");
	
			objectStore.setValue('key', "overwritten value");
			is(objectStore.getValue('key'), "overwritten value", "using the same key overwrites the previous value");

			objectStore.setValue('record.keys', [1, 2, 3]);
			is(
				MochiKit.Base.compare(objectStore.getValue('record'), {'keys': [1,2,3]}),
				0,
				"getting a partial key returns the whole content associate with that key"
			);
			is(
				MochiKit.Base.compare(objectStore.getValue('record.keys'), [1,2,3]),
				0,
				"accessing data using a key.path return the matching content"
			);
			is(
				MochiKit.Base.compare(objectStore.getValue('record.keys.1'), 2),
				0,
				"accessing data using a key.path return the matching content, even inside an array"
			);

			is(
				objectStore.setValue('key', "value"),
				"value",
				"setting a value return the value itself, as a convenience to chain deferred methods"
			);

			is(
				objectStore.getValue('not_set_key'),
				null,
				"accessing a previously undefined key will return null"
			);
			is(
				objectStore.getValue('record.not_set_key'),
				null,
				"accessing a previously undefined key will return null, even if part of the path is defined"
			);
			is(
				objectStore.getValue('not_set_path.not_set_key'),
				null,
				"accessing a previously undefined key will return null, even if using a completely undefined path"
			);

			objectStore.removeAllData();
			is(
				objectStore.getValue('key'),
				null,
				"getting a value after a 'removeAllData' return no value"
			);
			
		});
		
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'simple_deferredGetOrSet_test': function () {
		var	deferredResult;
		var	objectStore;
		var testValue;
		
		objectStore = new Clipperz.KeyValueObjectStore();
		testValue = "nifty test value";
		
		deferredResult = new Clipperz.Async.Deferred("simple_deferredGetOrSet_test", {trace:false});
		deferredResult.addMethod(objectStore, 'setValue', 'key', testValue);
		deferredResult.addMethod(objectStore, 'deferredGetOrSet', 'key', function() {return testValue});
		deferredResult.addCallback(function(aResult) {
			SimpleTest.is(aResult, testValue, "deferredGetOrSet works when accessing data already present on the object store");
		})
		deferredResult.callback();
	},

    //-------------------------------------------------------------------------

	'deferredGetOrSet_test': function () {
		var	deferredResult;
		var	objectStore;
		var testValue;
		
		objectStore = new Clipperz.KeyValueObjectStore();
		testValue = "nifty test value";
		
		deferredResult = new Clipperz.Async.Deferred("deferredGetOrSet_test", {trace:false});
		deferredResult.addMethod(objectStore, 'setValue', 'key.path', testValue);
		deferredResult.addMethod(objectStore, 'deferredGetOrSet', 'key', function() {return {'path': testValue}; });
		deferredResult.addCallback(function(aResult) {
			SimpleTest.is(aResult['path'], testValue, "deferredGetOrSet works when accessing data already present on the object store");
		})
		deferredResult.callback();
	},

    //-------------------------------------------------------------------------

	'simple_deferredGetOrSet_withMissingValue_test': function () {
		var	deferredResult;
		var	objectStore;
		var testValue;
		
		objectStore = new Clipperz.KeyValueObjectStore();
		testValue = "nifty test value";
		
		deferredResult = new Clipperz.Async.Deferred("simple_deferredGetOrSet_withMissingValue_test", {trace:false});
		deferredResult.addMethod(objectStore, 'deferredGetOrSet', 'key', function() {return testValue});
		deferredResult.addCallback(function(aResult) {
			SimpleTest.is(aResult, testValue, "deferredGetOrSet works when accessing data already present on the object store");
		})
		deferredResult.callback();
	},

    //-------------------------------------------------------------------------

	'deferredGetOrSet_withMissingValue_test': function () {
		var	deferredResult;
		var	objectStore;
		var testValue;
		
		objectStore = new Clipperz.KeyValueObjectStore();
		testValue = "nifty test value";
		
		deferredResult = new Clipperz.Async.Deferred("deferredGetOrSet_withMissingValue_test", {trace:false});
		deferredResult.addMethod(objectStore, 'deferredGetOrSet', 'key', function() {return {'path': testValue}; });
		deferredResult.addCallback(function(aResult) {
			SimpleTest.is(aResult['path'], testValue, "deferredGetOrSet works when accessing data already present on the object store");
		})
		deferredResult.callback();
	},

    //-------------------------------------------------------------------------

	'deleteObjectKey': function () {
		var	someValues;
		var objectStore;
		
		someValues = {
			'key1': {
				'key1_1': "value 1.1",
				'key1_2': "value 1.2"
			},
			'key2': {
				'key2_1': {
					'key2.1.1': "value 2.1.1",
					'key2.1.2': "value 2.1.2"
				},
				'key2_2': "value 2.2"
			}
		}
		
		objectStore = new Clipperz.KeyValueObjectStore();
		objectStore.initWithValues(someValues);
		objectStore.removeValue('key2.key2_1');
		
		SimpleTest.is(objectStore.getValue('key1.key1_1'), "value 1.1", "The first element is still there");
		SimpleTest.is(objectStore.getValue('key1.key1_2'), "value 1.2", "The second element is still there");
		SimpleTest.is(objectStore.getValue('key2.key2_1'), null, "The deleted element is actually gone");
		SimpleTest.is(MochiKit.Base.keys(objectStore.getValue('key2')).length, 1, "Even the key is not stored anylonger");
		SimpleTest.is(objectStore.getValue('key2.key2_2'), "value 2.2", "The sibling of the deleted element is still there");
	},

    //-------------------------------------------------------------------------

	'accessDataUsingANumericKey': function () {
		var	someValues;
		var objectStore;
		
		someValues = {
			'1': "value 1",
			'2': "value 2"
		}
		
		objectStore = new Clipperz.KeyValueObjectStore();
		objectStore.initWithValues(someValues);
		
		SimpleTest.is(objectStore.getValue(1), "value 1", "The first element is accessed even using a numeric key");
		
		objectStore.setValue(3, "value 3");
		SimpleTest.is(objectStore.getValue('3'), "value 3", "I can set the value using a numeric key and get it with a string key");
	},

    //-------------------------------------------------------------------------

	'isEmpty_test': function () {
		var	someValues;
		var objectStore;
		
		someValues = {
			'1': "value 1",
			'2': "value 2"
		}
		
		objectStore = new Clipperz.KeyValueObjectStore();
		SimpleTest.is(objectStore.isEmpty(), true, "A newly initialized KeyValueStore is empty");

		objectStore.initWithValues(someValues);
		SimpleTest.is(objectStore.isEmpty(), false, "Once the KeyValueStore is initialized with some values, it is no logner empty");
		
		objectStore.removeAllData();
		SimpleTest.is(objectStore.isEmpty(), true, "A KeyValueStore is empty after invoking the 'removeAllData' method");
	},

    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};

//#############################################################################

SimpleTest.runDeferredTests("Clipperz.KeyValueObjectStore", tests, {trace:false});
