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


//=============================================================================

testEvalJSON = function(aString, shouldFail, aDescription) {
	var result;
	
	if (shouldFail == true) {
		try {
			result = Clipperz.Base.evalJSON(aString);
			is(true, false, aDescription + ": vulnerability not caught");
//console.log(result);
		} catch(exception) {
			is(true, true, aDescription + ": vulnerability correctly caught");
		}
	} else {
		try {
			result = Clipperz.Base.evalJSON(aString);
			is(true, true, aDescription + ": configuration correctly checked");
		} catch(exception) {
			is(true, false, aDescription + ": configuration wrongly caught as malicious");
//			console.log(exception);
		}
	}
	
	return result;
}

//=============================================================================

var tests = {

	'001_test': function () {
		var	stringToSplit;	
		var	splittedString;
	
		stringToSplit = "stringToSplit";
		splittedString = Clipperz.Base.splitStringAtFixedTokenSize(stringToSplit, 2);
		is(splittedString.length, 7);
		is(splittedString[0], 'st');
		is(splittedString[1], 'ri');
		is(splittedString[2], 'ng');
		is(splittedString[3], 'To');
		is(splittedString[4], 'Sp');
		is(splittedString[5], 'li');
		is(splittedString[6], 't', "test that should pass");

		stringToSplit = "stringToSplit";
		splittedString = Clipperz.Base.splitStringAtFixedTokenSize(stringToSplit, 20);
		is(splittedString.length, 1);
		is(splittedString[0], 'stringToSplit');

		stringToSplit = null;
		splittedString = Clipperz.Base.splitStringAtFixedTokenSize(stringToSplit, 20);
		is(splittedString.length, 0);
	},

    //-------------------------------------------------------------------------

	'002_test': function () {
		var anObject;
	
		anObject = "String";
		is(Clipperz.Base.objectType(anObject), 'string', "test on strings (1)");
		anObject = new String("String");
		is(Clipperz.Base.objectType(anObject), 'string', "test on strings (2)");

		anObject = 120;
		is(Clipperz.Base.objectType(anObject), 'number', "test on numbers (1)");
		anObject = new Number(120);
		is(Clipperz.Base.objectType(anObject), 'number', "test on numbers (2)");

		anObject = true;
		is(Clipperz.Base.objectType(anObject), 'boolean', "test on booleans (1)");
		anObject = new Boolean(true);
		is(Clipperz.Base.objectType(anObject), 'boolean', "test on booleans (2)");

		anObject = new Date;
		is(Clipperz.Base.objectType(anObject), 'date', "test on dates");

		anObject = new Error("test");
		is(Clipperz.Base.objectType(anObject), 'error', "test on errors");

		anObject = function() {};
		is(Clipperz.Base.objectType(anObject), 'function', "test on functions");

		anObject = new Object();
		is(Clipperz.Base.objectType(anObject), 'object', "test on objects");


		anObject = [1, 2, 3];
		is(Clipperz.Base.objectType(anObject), 'array', "test on arrays");
	},

	//-------------------------------------------------------------------------
	
	'003_test': function () {
		var original, clone;
	
		original = {
			a: "a",
			b: "b"
		};
	
		clone = Clipperz.Base.deepClone(original);
		is(MochiKit.Base.compare(original, clone), 0, "simple cloning return two equal objects");
	
		clone.c = "c";
		is(MochiKit.Base.compare(original, clone), -1, "changing an object leave the original object unchanged");

		original = {
			a: "a",
			b: "b",
			nested: {
				a1: "a1",
				b1: "b1"
			}
		};

		clone = Clipperz.Base.deepClone(original);
		is(MochiKit.Base.compare(original, clone), 0, "cloning of an object with nested values return two equal objects");
	
		clone.nested.c1 = "c1";
		is(MochiKit.Base.compare(original, clone), -1, "changing a nested value leave the original object unchanged");
	},

    //-------------------------------------------------------------------------

	'004_test': function () {
		var jsonString;
	
		jsonString = '{"page": {"title": "Example Attack"},"form": {  "attributes": {    "action": "javascript:opener.document.body.innerHTML = \'hacked!\';close();",    "style": "-moz-binding:url(\'http://ha.ckers.org/xssmoz.xml#xss\')",    "method": null  },  "inputs": [{"type": "text", "name": "username", "value": ""},             {"type": "password", "name": "password", "value": ""}]},"version": "0.2.3" }';
		testEvalJSON(jsonString, false, "");

	//	jsonString = '{"0":{"label":"<script>alert(\"Ciao Marco\")< /script>","key":"ebc9782019bf9aa757e9c4d716ab303e2050b60c4b9a06b18ab09a417e0ddf00"}, "1":{"label":"<iframe><script>alert(\\"Ciao ragazzi\\")< /script></iframe>", "key":"413cfb122a1601c50e0f9462978ba77a36fdcecb49dda7550ee129dc114ba328"}}';
		jsonString = new Clipperz.ByteArray().appendBase64String("eyIwIjp7ImxhYmVsIjoiPHNjcmlwdD5hbGVydChcIkNpYW8gTWFyY29cIik8L3NjcmlwdD4iLCAia2V5IjoiZWJjOTc4MjAxOWJmOWFhNzU3ZTljNGQ3MTZhYjMwM2UyMDUwYjYwYzRiOWEwNmIxOGFiMDlhNDE3ZTBkZGYwMCJ9LCAiMSI6eyJsYWJlbCI6IjxpZnJhbWU+PHNjcmlwdD5hbGVydChcIkNpYW8gcmFnYXp6aVwiKTwvc2NyaXB0PjwvaWZyYW1lPiIsICJrZXkiOiI0MTNjZmIxMjJhMTYwMWM1MGUwZjk0NjI5NzhiYTc3YTM2ZmRjZWNiNDlkZGE3NTUwZWUxMjlkYzExNGJhMzI4In19").asString();
		testEvalJSON(jsonString, false);

		jsonString = 'alert("foobar");';
		testEvalJSON(jsonString, true);

	//	jsonString = '<script>alert("foobar");< /script>';
		jsonString = new Clipperz.ByteArray().appendBase64String("PHNjcmlwdD5hbGVydCgiZm9vYmFyIik7PC9zY3JpcHQ+").asString();
		testEvalJSON(jsonString, true);

		jsonString = '{"xss": alert("XSS!")}';
		testEvalJSON(jsonString, true);

		jsonString = '{"inner": {"xss":  alert("XSS!")}}';
		testEvalJSON(jsonString, true);
	},

    //-------------------------------------------------------------------------

	'Clipperz.Base.itemgetter_test': function () {
		var	anObject;
		
		anObject = {
			key1: 'value1',
			key2: {
				key2_1: 'value2_1',
				key2_2: 'value2_2'
			},
			key3: {
				key3_1: {
					key3_1_1: 'value3_1_1',
					key3_1_2: 'value3_1_2'
				},
				key3_2: {
					key3_2_1: 'value3_2_1',
					key3_2_2: 'value3_2_2'
				}
			}
		};
		
		SimpleTest.is(Clipperz.Base.itemgetter('key1')(anObject), "value1", "Clipperz.Base.itemgetter works as MochiKit.Base.itemgetter");
		SimpleTest.is(Clipperz.Base.itemgetter('key2.key2_1')(anObject), "value2_1", "Clipperz.Base.itemgetter works also with keypaths");
		SimpleTest.is(Clipperz.Base.itemgetter('key3.key3_2.key3_2_2')(anObject), "value3_2_2", "Clipperz.Base.itemgetter works also with 'long' keypaths");
	},

    //-------------------------------------------------------------------------

	'Clipperz.Base.caseInsensitiveCompare_test': function () {
		var	comparator;
		var objects;

		comparator = Clipperz.Base.caseInsensitiveCompare;
		objects = [ 'ccc', 'aaa', 'bbb', 'eee', 'ddd'];
		SimpleTest.isDeeply(['aaa', 'bbb', 'ccc', 'ddd', 'eee'], objects.sort(comparator), "caseInsensitiveCompare works with all lowercase values");

		comparator = MochiKit.Base.compare;
		objects = [ 'ccc', 'AAA', 'bbb', 'EEE', 'ddd'];
		SimpleTest.isDeeply(['AAA', 'EEE', 'bbb', 'ccc', 'ddd'], objects.sort(comparator), "caseInsensitiveCompare works with all lowercase values");

		comparator = Clipperz.Base.caseInsensitiveCompare;
		objects = [ 'ccc', 'AAA', 'bbb', 'EEE', 'ddd'];
		SimpleTest.isDeeply(['AAA', 'bbb', 'ccc', 'ddd', 'EEE'], objects.sort(comparator), "caseInsensitiveCompare works with all lowercase values");
	},

    //-------------------------------------------------------------------------

	'Clipperz.Base.reverseComparator_test': function () {
		var	comparator;
		var objects;
		
		comparator = MochiKit.Base.compare;
		objects = [5, 3, 2, 4, 1];
		SimpleTest.isDeeply([1, 2, 3, 4, 5], objects.sort(comparator), "a regular comparator works fine");
		
		comparator = Clipperz.Base.reverseComparator(MochiKit.Base.compare);
		objects = [5, 3, 2, 4, 1];
		SimpleTest.isDeeply([5, 4, 3, 2 ,1], objects.sort(comparator), "a reversed comparator works fine");

		comparator = MochiKit.Base.keyComparator('label');
		objects = [ {label:"5"}, {label:"3"}, {label:"1"}, {label:"4"}, {label:"2"}];
		SimpleTest.isDeeply([ {label:"1"}, {label:"2"}, {label:"3"}, {label:"4"}, {label:"5"}], objects.sort(comparator), "a regular keyComparator works fine");
		
		comparator = Clipperz.Base.reverseComparator(MochiKit.Base.keyComparator('label'));
		objects = [ {label:"5"}, {label:"3"}, {label:"1"}, {label:"4"}, {label:"2"}];
		SimpleTest.isDeeply([ {label:"5"}, {label:"4"}, {label:"3"}, {label:"2"}, {label:"1"}], objects.sort(comparator), "a reversed keyComparator works fine");
	},

    //-------------------------------------------------------------------------

	'Clipperz.Base.map_test': function () {
		var objects;
		var	computedObjecs;
		
		objects = [5, 3, 2, 4, 1];
		computedObjecs = Clipperz.Base.map(function (aValue) { return aValue * 2;}, objects);
		SimpleTest.isDeeply(computedObjecs, [10, 6, 4, 8, 2], "the mapped values of the array do match");
		
		objects = {
			'five':		5,
			'three':	3,
			'two':		2,
			'four':		4,
			'one':		1
		};
		computedObjecs = Clipperz.Base.map(function (aValue) { return aValue * 2;}, objects);
		SimpleTest.isDeeply(computedObjecs, {
			'five':		10,
			'three':	6,
			'two':		4,
			'four':		8,
			'one':		2
		}, "the mapped values of the object do match");
	},

    //-------------------------------------------------------------------------

	'Clipperz.Base.isUrl_test': function () {
		var	urlTestCases;
		
		urlTestCases = [
			{url:'http://foo.com/blah_blah',						expectedResult:true},
			{url:'http://foo.com/blah_blah',						expectedResult:true},
			{url:'http://foo.com/blah_blah/',						expectedResult:true},
			{url:'http://foo.com/blah_blah_(wikipedia)',			expectedResult:true},
			{url:'http://foo.com/blah_blah.',						expectedResult:true},
			{url:'http://foo.com/blah_blah/.',						expectedResult:true},
			{url:'http://foo.com/blah_blah,',						expectedResult:true},
			{url:'http://✪df.ws/123',								expectedResult:true},
			{url:'http://➡.ws/䨹',									expectedResult:true},
			{url:'www.➡.ws/䨹',										expectedResult:true},
			{url:'http://www.example.com/wpstyle/?p=364.',			expectedResult:true},
			{url:'www.clipperz.com',								expectedResult:true},
			{url:'http://www.clipperz.com',							expectedResult:true},
			{url:'http://clipperz.com',								expectedResult:true},

			{url:'clipperz.com',									expectedResult:false},
//			{url:'www.clipperz',									expectedResult:false},
//			{url:'www.abc',											expectedResult:false},
			{url:'joe@clipperz.com',								expectedResult:false},
			{url:'<http://foo.com/blah_blah>',						expectedResult:false},
			{url:'<http://foo.com/blah_blah/>',						expectedResult:false},
			{}
		];
		
		MochiKit.Base.map(function (someValues) {
			if (typeof(someValues['url']) != 'undefined') {
				SimpleTest.is(Clipperz.Base.isUrl(someValues['url']), someValues['expectedResult'], "testing url '" + someValues['url'] + "' - expected result: " + someValues['expectedResult']);
			}
		}, urlTestCases);

/*
		//	RegExp and test strings courtesy of John Gruber: http://daringfireball.net/2009/11/liberal_regex_for_matching_urls
		SimpleTest.is(Clipperz.Base.isUrl('http://foo.com/blah_blah'),				true,	"url test +1");
		SimpleTest.is(Clipperz.Base.isUrl('http://foo.com/blah_blah/'),				true,	"url test +2");
		SimpleTest.is(Clipperz.Base.isUrl('http://foo.com/blah_blah_(wikipedia)'),	true,	"url test +3");
		SimpleTest.is(Clipperz.Base.isUrl('http://foo.com/blah_blah.'),				true,	"url test +4");
		SimpleTest.is(Clipperz.Base.isUrl('http://foo.com/blah_blah/.'),			true,	"url test +5");
		SimpleTest.is(Clipperz.Base.isUrl('<http://foo.com/blah_blah>'),			true,	"url test +6");
		SimpleTest.is(Clipperz.Base.isUrl('<http://foo.com/blah_blah/>'),			true,	"url test +7");
		SimpleTest.is(Clipperz.Base.isUrl('http://foo.com/blah_blah,'),				true,	"url test +8");
		SimpleTest.is(Clipperz.Base.isUrl('http://✪df.ws/123'),						true,	"url test +9");
		SimpleTest.is(Clipperz.Base.isUrl('http://➡.ws/䨹'),							true,	"url test +10");
		SimpleTest.is(Clipperz.Base.isUrl('www.➡.ws/䨹'),							true,	"url test +11");
		SimpleTest.is(Clipperz.Base.isUrl('http://www.example.com/wpstyle/?p=364.'),true,	"url test +12");
		SimpleTest.is(Clipperz.Base.isUrl('www.clipperz.com'),						true,	"url test +13");
		SimpleTest.is(Clipperz.Base.isUrl('http://www.clipperz.com'),				true,	"url test +14");

//		SimpleTest.is(Clipperz.Base.isUrl('http://userid@example.com'),					true,	"url test +13");	// FAIL
//		SimpleTest.is(Clipperz.Base.isUrl('http://userid@example.com:8080'),			true,	"url test +14");	// FAIL
//		SimpleTest.is(Clipperz.Base.isUrl('http://userid:password@example.com'),		true,	"url test +15");	// FAIL
//		SimpleTest.is(Clipperz.Base.isUrl('http://userid:password@example.com:8080'),	true,	"url test +16");	// FAIL


		SimpleTest.is(Clipperz.Base.isUrl('joe@clipperz.com'),			false,	"url test -1");
		SimpleTest.is(Clipperz.Base.isUrl('rdar://1234'),				false,	"url test -2");
		SimpleTest.is(Clipperz.Base.isUrl('rdar:/1234'),				false,	"url test -3");
		SimpleTest.is(Clipperz.Base.isUrl('http://example.com:8080 x-yojimbo-item://6303E4C1-xxxx-45A6-AB9D-3A908F59AE0E'),	false,	"url test -4");
		SimpleTest.is(Clipperz.Base.isUrl('message://%3c330e7f8409726r6a4ba78dkf1fd71420c1bf6ff@mail.gmail.com%3e'),		false,	"url test -5");
*/
	},

    //-------------------------------------------------------------------------

	'Clipperz.Base.isBitcoin_test': function () {
		var	bitcoinTestCases;
		
		bitcoinTestCases = [
			{url:'bitcoin:175tWpb8K1S7NmH4Zx6rewF9WQrcZv245W?amount=20.3&label=Luke-Jr',	expectedResult:true},
			{url:'bitcoin:175tWpb8K1S7NmH4Zx6rewF9WQrcZv245W',								expectedResult:true},

			{url:'http://www.clipperz.com',													expectedResult:false},
			{url:'http://clipperz.com',														expectedResult:false},
			{}
		];
		
		MochiKit.Base.map(function (someValues) {
			if (typeof(someValues['url']) != 'undefined') {
				SimpleTest.is(Clipperz.Base.isBitcoin(someValues['url']), someValues['expectedResult'], "testing url '" + someValues['url'] + "' - expected result: " + someValues['expectedResult']);
			}
		}, bitcoinTestCases);
	},
	
    //-------------------------------------------------------------------------

	'Clipperz.Base.isEmail_test': function () {
		var	emailTestCases;
		
		emailTestCases = [
			{email:'joe@clipperz.com',						expectedResult:true},

			{email:'http://foo.com/blah_blah',				expectedResult:false}
		];
		
		MochiKit.Base.map(function (someValues) {
			SimpleTest.is(Clipperz.Base.isEmail(someValues['email']), someValues['expectedResult'], "testing email '" + someValues['email'] + "' - expected result: " + someValues['expectedResult']);
		}, emailTestCases);
	},

    //-------------------------------------------------------------------------

    'syntaxFix': MochiKit.Base.noop
}

//=============================================================================

SimpleTest.runDeferredTests("Clipperz.Base", tests, {trace:false});
