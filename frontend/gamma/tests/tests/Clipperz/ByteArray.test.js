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

	'core_tests': function (someTestArgs) {
//		var	deferredResult;

//		deferredResult = new Clipperz.Async.Deferred("core_tests", someTestArgs);
//		deferredResult.addCallback(function() {
			var byteArray;

			byteArray = new Clipperz.ByteArray();

			try {
				byteArray.checkByteValue(512);
				is(false, true, "a value greater that a byte (0x200) should have raised an exception - NO Exception");
			} catch(e) {
//				is(	e.name,
//					"Clipperz.ByteArray.exception.InvalidValue",
//					"appending a value greater that a byte (0x200) should have raised an exception - EXCEPTION HANDLER")
				ok(	/Clipperz\.ByteArray\.exception\.InvalidValue.*/.test(e.name),
					"appending a value greater that a byte (0x200) should have raised an exception - EXCEPTION HANDLER")
			};

//		});
//		deferredResult.callback();
		
//		return deferredResult;
		
	},
	
    //-------------------------------------------------------------------------

	'basic_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("simple_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray;
			var byteArray2;
			var	byteArrayIterator;
			var	nextBlock;
	
			byteArray = new Clipperz.ByteArray();
			is(byteArray.length(), 0, "before adding any element the length is 0");
			byteArray.appendByte(10);
			is(byteArray.length(), 1, "adding a single byte the length == 1");
			is(byteArray.byteAtIndex(0), 10, "the first element is correct");

			byteArrayIterator = new Clipperz.ByteArrayIterator({byteArray:byteArray, blockSize:(8/8), finalPadding:true});
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock.constructor, Array, "ByteArrayIterator.nextBlock returns an array of byte values");
			is(nextBlock.length, 1, "as the block size is 8bit, the returned array has only one element");
			is(nextBlock[0], 10, "the element of the returned block is correct");
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock, null, "after the last element, the nextBlock returns null");

			byteArray = new Clipperz.ByteArray();
			byteArray.appendBytes(10, 20, 45, 38);
			is(byteArray.length(), 4, "Appending more bytes, returns the right length");
			is(byteArray.byteAtIndex(0), 10, "and all the elements are right [0]");
			is(byteArray.byteAtIndex(1), 20, "and all the elements are right [1]");
			is(byteArray.byteAtIndex(2), 45, "and all the elements are right [2]");
			is(byteArray.byteAtIndex(3), 38, "and all the elements are right [3]");

			byteArray2 = new Clipperz.ByteArray();
			byteArray2.appendBytes([10, 20, 45, 38]);
			is(byteArray.equals(byteArray2), true, "equals method tested with a byteArray created with the same values");

			byteArray2 = new Clipperz.ByteArray();
			byteArray2.appendBytes([20, 11,  3, 22]);
			is(byteArray.equals(byteArray2), false, "equals method tested with a byteArray created with different values");

			byteArrayIterator = new Clipperz.ByteArrayIterator({byteArray:byteArray, blockSize:(8/8), finalPadding:true});
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock.length, 1, "the size of the block returned by the byteArrayIterator match with the configured blockedSize");
			is(nextBlock[0], 10, "the values returned by nextBlock are right [1]");
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock[0], 20, "the values returned by nextBlock are right [2]");
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock[0], 45, "the values returned by nextBlock are right [3]");
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock[0], 38, "the values returned by nextBlock are right [4]");
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock, null, "after the last block the nextBlock method returns null");

			byteArrayIterator = new Clipperz.ByteArrayIterator({byteArray:byteArray, blockSize:(16/8), finalPadding:true});
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock.length, 2, "on the same data, using a different block size, returns the right length");
			is(nextBlock[0], 10, "and also the data are fine [1][0]");
			is(nextBlock[1], 20, "and also the data are fine [1][1]");
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock.length, 2, "also the second block size is right");
			is(nextBlock[0], 45, "and also the data are fine [2][0]");
			is(nextBlock[1], 38, "and also the data are fine [2][1]");
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock, null, "even with a bigger blockSize, at the end the returned value is null");

			byteArray = new Clipperz.ByteArray(11,22,33,44,55,66,77,88,99);
			is(byteArray.length(), 9, "");
			byteArrayIterator = new Clipperz.ByteArrayIterator({byteArray:byteArray, blockSize:(16/8), finalPadding:true});
			nextBlock = byteArrayIterator.nextBlock();
			nextBlock = byteArrayIterator.nextBlock();
			nextBlock = byteArrayIterator.nextBlock();
			nextBlock = byteArrayIterator.nextBlock();
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock.length, 2, "the last block of an odd byte array has always the same size");
			is(nextBlock[0], 99, "the last element is returned");
			is(nextBlock[1], 0, "and a 0 is return for the extra values");
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock, null, "even with odd blockSize, after returning all the values, null is returned");

			byteArrayIterator = new Clipperz.ByteArrayIterator({byteArray:byteArray, blockSize:(32/8), finalPadding:true});
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock[0], 11, "32 bit blockSize [1][0]");
			is(nextBlock[1], 22, "32 bit blockSize [1][1]");
			is(nextBlock[2], 33, "32 bit blockSize [1][2]");
			is(nextBlock[3], 44, "32 bit blockSize [1][3]");
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock[0], 55, "32 bit blockSize [2][0]");
			is(nextBlock[1], 66, "32 bit blockSize [2][1]");
			is(nextBlock[2], 77, "32 bit blockSize [2][2]");
			is(nextBlock[3], 88, "32 bit blockSize [2][3]");
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock.length, 4, "the last block of an odd byte array has always the same size");
			is(nextBlock[0], 99, "the last element is returned (2)");
			is(nextBlock[1], 0, "and a 0 is return for the extra values (2.1)");
			is(nextBlock[2], 0, "and a 0 is return for the extra values (2.2)");
			is(nextBlock[3], 0, "and a 0 is return for the extra values (2.3)");
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock, null, "even with odd blockSize, after returning all the values, null is returned (2)");

			byteArray = new Clipperz.ByteArray([11,22,33,44,55,66,77,88,99,100, 111, 122, 133, 144, 155, 166, 177]);
			byteArrayIterator = new Clipperz.ByteArrayIterator({byteArray:byteArray, blockSize:(128/8), finalPadding:true});
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock.length, 16, "using a blockSize of 128 bit, the nextBlock return a 16 elements array");

			byteArray = new Clipperz.ByteArray();
			try {
				byteArray.appendByte(512);
				is(false, true, "appending a value greater that a byte (0x200) should have raised an exception - NO Exception");
			} catch(e) {
//				is(	e.name,
//					"Clipperz.ByteArray.exception.InvalidValue",
//					"appending a value greater that a byte (0x200) should have raised an exception - EXCEPTION HANDLER")
				ok(	/Clipperz\.ByteArray\.exception\.InvalidValue.*/.test(e.name),
					"appending a value greater that a byte (0x200) should have raised an exception - EXCEPTION HANDLER")
			};

			try {
				byteArray.appendByte(256);
				is(false, true, "appending a value greater that a byte (0x100) should have raised an exception - NO Exception");
			} catch(e) {
//				is(	e.name,
//					"Clipperz.ByteArray.exception.InvalidValue",
//					"appending a value greater that a byte (0x100) should have raised an exception - EXCEPTION HANDLER")
				ok(	/Clipperz\.ByteArray\.exception\.InvalidValue.*/.test(e.name),
					"appending a value greater that a byte (0x100) should have raised an exception - EXCEPTION HANDLER")
			};

			byteArray.appendByte(255);
			is(byteArray.length(), 1, "appending a value that feets a single byte does not raise any exception");
	
			byteArray = new Clipperz.ByteArray(10, 20, 30, 40, 50);
			byteArray2 = new Clipperz.ByteArray("0x0a141e2832");
			is(byteArray.equals(byteArray2), true, "the hex constructor works fine");
	
			byteArray2 = byteArray.clone();
			is(byteArray.equals(byteArray2), true, "the clone method works fine");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'compare_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("compare_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray1;
			var byteArray2;
			var result;
			
			byteArray1 = new Clipperz.ByteArray("0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", 16);
			byteArray2 = new Clipperz.ByteArray("0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", 16);
			result = byteArray1.compare(byteArray2);
			is(result, 0, "equal compare");

			byteArray1 = new Clipperz.ByteArray("0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", 16);
			byteArray2 = new Clipperz.ByteArray("0x05", 16);
			result = byteArray1.compare(byteArray2);
			is(result, 1, "'OxXXXXXXX'.compare('0x05')");
			result = byteArray2.compare(byteArray1);
			is(result, -1, "'0x05'.compare('OxXXXXXXX')");

			byteArray1 = new Clipperz.ByteArray("0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", 16);
			byteArray2 = new Clipperz.ByteArray("0xff", 16);
			result = byteArray1.compare(byteArray2);
			is(result, 1, "'OxXXXXXXX'.compare('0xff')");
			result = byteArray2.compare(byteArray1);
			is(result, -1, "'0xff'.compare('OxXXXXXXX')");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'shiftLeft_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("shiftLeft_tests", someTestArgs);
		deferredResult.addCallback(function() {

		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'nextBlockArray_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("nextBlockArray_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray;
			var byteArrayIterator;
			var nextBlock;
			
			byteArray = new Clipperz.ByteArray("0x8cf53d90077df9a043bf8d10b470b144784411c93a4d504556834dae3ea4a5bb");
			byteArrayIterator = new Clipperz.ByteArrayIterator({byteArray:byteArray, blockSize:(128/8), finalPadding:false});
			nextBlock = byteArrayIterator.nextBlockArray();
			is(nextBlock.toHexString(), "0x8cf53d90077df9a043bf8d10b470b144", "nextBlockArray first iteration");
			nextBlock = byteArrayIterator.nextBlockArray();
			is(nextBlock.toHexString(), "0x784411c93a4d504556834dae3ea4a5bb", "nextBlockArray second iteration");

			byteArray = new Clipperz.ByteArray("0x8cf53d90077df9a043bf8d10b470b144784411c93a4d504556834dae3ea4a5bb");
			byteArrayIterator = new Clipperz.ByteArrayIterator({byteArray:byteArray, blockSize:(128/8), finalPadding:false});
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock.length, 16, "nextBlock first iteration length");
			nextBlock = byteArrayIterator.nextBlock();
			is(nextBlock.length, 16, "nextBlock second iteration length");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'xor_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("xor_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray;
			var byteArray2;

			byteArray = new Clipperz.ByteArray("0x55555555555555555555555555555555");
			byteArray2 = new Clipperz.ByteArray("0x00ff000000ff000000ff000000ff0000");
			is(byteArray.xorMergeWithBlock(byteArray2).toHexString(), "0x55aa555555aa555555aa555555aa5555", "the xorMergeWithBlock method works fine");

			byteArray  = new Clipperz.ByteArray("0x555555555555555555");
			byteArray2 = new Clipperz.ByteArray("0x00ff000000ff000000ff000000ff0000");
			is(byteArray.xorMergeWithBlock(byteArray2, 'left', 'truncate').toHexString(), "0x55aa555555aa555555", "the xorMergeWithBlock (left, truncate) method works fine");

			byteArray  = new Clipperz.ByteArray("0x555555555555555555");
			byteArray2 = new Clipperz.ByteArray("0x00ff000000ff000000ff000000ff0000");
			is(byteArray.xorMergeWithBlock(byteArray2, 'right', 'truncate').toHexString(), "0x5555aa555555aa5555", "the xorMergeWithBlock (right, truncate) method works fine");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'increment_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("increment_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray;
			var	number;
			var i,c;

			number = new Clipperz.Crypto.BigInt("55555555555555555555555555555555", 16);
			byteArray = new Clipperz.ByteArray("0x" + number.asString(16));
			c = 2048;
			for (i=0; i<c; i++) {
				byteArray.increment();
			}
			is(byteArray.toHexString(), "0x" + number.add(2048).asString(16), "Clipperz.ByteArray.increment works");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'split_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("split_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray;
			
			byteArray = new Clipperz.ByteArray("0xa1b2c3d4");
			is(byteArray.split(0, 2).toHexString(), "0xa1b2", "byteArray.split(0,2) works fine");
			is(byteArray.split(2).toHexString(), "0xc3d4", "byteArray.split(2) works fine");
			is(byteArray.split(1, 3).toHexString(), "0xb2c3", "byteArray.split(1,3) works fine");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'appendBlock_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("appendBlock_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray;
			var byteArray2;
			
			byteArray = new Clipperz.ByteArray("0xa1b2c3d4");
			byteArray2 = new Clipperz.ByteArray("0x1a2b3c4d");
			is(byteArray.appendBlock(byteArray2).toHexString(), "0xa1b2c3d41a2b3c4d", "the appendBlock method works fine");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'appendWords_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("appendWords_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray;
			
			byteArray = new Clipperz.ByteArray();
			byteArray.appendWords(0xa1b2c4d4, 0x1a2b3c4d);
			is(byteArray.toHexString(), "0xa1b2c4d41a2b3c4d", "test appendWords");

			byteArray = new Clipperz.ByteArray();
			byteArray.appendBigEndianWords(0xa1b2c4d4, 0x1a2b3c4d);
			is(byteArray.toHexString(), "0xd4c4b2a14d3c2b1a", "test appendBigEndianWords");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'setByteAtIndex_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("setByteAtIndex_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var	byteArray;

			byteArray = new Clipperz.ByteArray("0xa1b2c3d4e5f6");
			byteArray.setByteAtIndex(10, 0);
			is(byteArray.toHexString(), "0x0ab2c3d4e5f6", "setByteAtIndex( , 0) works fine");

			byteArray.setByteAtIndex(10, 5);
			is(byteArray.toHexString(), "0x0ab2c3d4e50a", "setByteAtIndex( , 5) works fine");

			byteArray.setByteAtIndex(10, 6);
			is(byteArray.toHexString(), "0x0ab2c3d4e50a0a", "setByteAtIndex( , 6) works fine");

			byteArray.setByteAtIndex(10, 10);
			is(byteArray.toHexString(), "0x0ab2c3d4e50a0a0000000a", "setByteAtIndex( , 10) works fine");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'string_encoding_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("string_encoding_tests", someTestArgs);
		deferredResult.addCallback(function() {
			is(Clipperz.ByteArray.unicodeToUtf8HexString(40), "28", "String encoding - One byte character - middle");
			is(Clipperz.ByteArray.unicodeToUtf8HexString(127), "7f", "String encoding - One byte character - top");

			is(Clipperz.ByteArray.unicodeToUtf8HexString(128), "c280", "String encoding - Two bytes character - bottom");
			is(Clipperz.ByteArray.unicodeToUtf8HexString(300), "c4ac", "String encoding - Two bytes character - middle");
			is(Clipperz.ByteArray.unicodeToUtf8HexString(2047), "dfbf", "String encoding - Two bytes character - top");

			is(Clipperz.ByteArray.unicodeToUtf8HexString(2048), "e0a080", "String encoding - Three bytes character - bottom");
			is(Clipperz.ByteArray.unicodeToUtf8HexString(8192), "e28080", "String encoding - Three bytes character - middle");
			is(Clipperz.ByteArray.unicodeToUtf8HexString(65535), "efbfbf", "String encoding - Three bytes character - top");

			is(Clipperz.ByteArray.unicodeToUtf8HexString(65536), "f0908080", "String encoding - Four bytes character - bottom");
			is(Clipperz.ByteArray.unicodeToUtf8HexString(1000000), "f3b48980", "String encoding - Four bytes character - middle");
			is(Clipperz.ByteArray.unicodeToUtf8HexString(2097151), "f7bfbfbf", "String encoding - Four bytes character - top");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'string_decoding_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("string_decoding_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray;
			var	byteArray2;
			var unicodeString;

			is(new Clipperz.ByteArray("0x28").asString().charCodeAt(0), 40, "String decoding - One byte character - middle");
			is(new Clipperz.ByteArray("0x7f").asString().charCodeAt(0), 127, "String decoding - One byte character - top");

			is(new Clipperz.ByteArray("0xc280").asString().charCodeAt(0), 128, "String decoding - Two bytes character - bottom");
			is(new Clipperz.ByteArray("0xc4ac").asString().charCodeAt(0), 300, "String decoding - Two bytes character - middle");
			is(new Clipperz.ByteArray("0xceb1").asString().charCodeAt(0), 945, "String decoding - Two bytes char: greek lowercase alpha - α");
			is(new Clipperz.ByteArray("0xd790").asString().charCodeAt(0), 1488, "String decoding - Two bytes char:  hebrew Aleph - א");
			is(new Clipperz.ByteArray("0xdfbf").asString().charCodeAt(0), 2047, "String decoding - Two bytes character - top");

			is(new Clipperz.ByteArray("0xe0a080").asString().charCodeAt(0), 2048, "String decoding - Three bytes character - bottom");
			is(new Clipperz.ByteArray("0xe0bc80").asString().charCodeAt(0), 3840, "String decoding - Three bytes char: tibetan syllable Om - ༀ");
			is(new Clipperz.ByteArray("0xe28080").asString().charCodeAt(0), 8192, "String decoding - Three bytes character - middle");
			is(new Clipperz.ByteArray("0xe4b899").asString().charCodeAt(0), 19993, "String decoding - Three bytes char: CJK  ideograph - 丙");
			is(new Clipperz.ByteArray("0xefbfbf").asString().charCodeAt(0), 65535, "String decoding - Three bytes character - top");

/*
			is(new Clipperz.ByteArray("0xf0908080").asString().charCodeAt(0), 65536, "String decoding - Four bytes character - bottom");
			is(new Clipperz.ByteArray("0xf0a08294").asString().charCodeAt(0), 131220, "String decoding - Four bytes char: CJK extended ideograph - ");
			is(new Clipperz.ByteArray("0xf3b48980").asString().charCodeAt(0), 1000000, "String decoding - Four bytes character - middle");
			is(new Clipperz.ByteArray("0xf7bfbfbf").asString().charCodeAt(0), 2097151, "String decoding - Four bytes character - top");

			is(String.fromCharCode(65535).charCodeAt(0), 65535, "test fromCharCode - charCodeAt (65535)");
			is(String.fromCharCode(65536).charCodeAt(0), 65536, "test fromCharCode - charCodeAt (65536)");
			is(String.fromCharCode(1000000).charCodeAt(0), 1000000, "test fromCharCode - charCodeAt (1000000)");
*/	
			//----------------------------------------------------------

			byteArray = new Clipperz.ByteArray("ABCD");
			byteArray2 = new Clipperz.ByteArray("0x41424344");
			is(byteArray.toHexString(), byteArray2.toHexString(), "the string constructor works fine (1 byte codes)");

			unicodeString = "una stringa un po' piu' semplice";
			byteArray = new Clipperz.ByteArray(unicodeString);
			is(byteArray.asString(), unicodeString, "");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'base64_encoding_decoding_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("base64_encoding_decoding_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray;

			byteArray = new Clipperz.ByteArray("\n");
			is(byteArray.toBase64String(), "Cg==", "base64 encoding test (1)");

			byteArray = new Clipperz.ByteArray("what");
			is(byteArray.toBase64String(), "d2hhdA==", "base64 encoding test (2)");

			byteArray = new Clipperz.ByteArray("what will print out");
			is(byteArray.toBase64String(), "d2hhdCB3aWxsIHByaW50IG91dA==", "base64 encoding test (3)");

			byteArray = new Clipperz.ByteArray("what  will   print     out      ");
			is(byteArray.toBase64String(), "d2hhdCAgd2lsbCAgIHByaW50ICAgICBvdXQgICAgICA=", "base64 encoding test (4)");

			byteArray = new Clipperz.ByteArray("what  will   print     out");
			is(byteArray.toBase64String(), "d2hhdCAgd2lsbCAgIHByaW50ICAgICBvdXQ=", "base64 encoding test (5)");

			byteArray = new Clipperz.ByteArray().appendBase64String("Cg==");
			is(byteArray.asString(), "\n", "base64 encoding test (1)");

			byteArray = new Clipperz.ByteArray().appendBase64String("d2hhdA==");
			is(byteArray.asString(), "what", "base64 encoding test (2)");

			byteArray = new Clipperz.ByteArray().appendBase64String("d2hhdCB3aWxsIHByaW50IG91dA==");
			is(byteArray.asString(), "what will print out", "base64 encoding test (3)");

			byteArray = new Clipperz.ByteArray().appendBase64String("d2hhdCAgd2lsbCAgIHByaW50ICAgICBvdXQgICAgICA=");
			is(byteArray.asString(), "what  will   print     out      ", "base64 encoding test (4)");
	
			byteArray = new Clipperz.ByteArray().appendBase64String("d2hhdCAgd2lsbCAgIHByaW50ICAgICBvdXQ=");
			is(byteArray.asString(), "what  will   print     out", "base64 deconding test (5)");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'base64_encoding_decoding_longText_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("base64_encoding_decoding_longText_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var longText;
			var byteArray;
			var base64encoding;
	
			longText = "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Curabitur interdum vulputate ligula. Aliquam erat volutpat. Fusce malesuada felis et purus. Vivamus convallis, metus id lacinia venenatis, est eros sagittis neque, at porttitor erat lorem a risus. Phasellus ultricies nunc et turpis. Ut nec sem ut quam ornare feugiat. Donec nibh. Quisque ullamcorper, neque eu convallis dictum, orci nisl dictum quam, ac elementum arcu sem varius sem. Suspendisse vitae dolor non magna suscipit semper. Suspendisse ac magna. Praesent et purus. Aenean ut justo. Curabitur dictum, nisl a consectetuer viverra, libero est auctor metus, et rhoncus risus diam in orci. Nam magna felis, lobortis eu, eleifend quis, tristique quis, mi. Praesent aliquet. Aenean mollis turpis sed dolor. Donec ac nunc. Nulla eu lacus in ipsum molestie rutrum. Nunc dignissim purus non nibh. Maecenas congue risus at neque sollicitudin eleifend.\n\nNunc purus orci, sagittis ut, vehicula non, auctor eu, orci. Fusce non nisl vel risus rutrum venenatis. Donec dapibus sodales eros. Lorem ipsum dolor sit amet, consectetuer adipiscing elit. In ultrices fringilla dui. Phasellus sit amet enim in tellus rutrum dapibus. Sed blandit malesuada turpis. Ut vel orci. Curabitur id eros. Sed sodales tempus urna. Praesent et neque. Sed ultricies. Praesent risus quam, elementum eget, tincidunt ac, condimentum sit amet, urna.\n\nLorem ipsum dolor sit amet, consectetuer adipiscing elit. Proin vel diam imperdiet felis luctus luctus. Nunc tincidunt nisl ac arcu. Sed id urna id magna nonummy varius. Mauris ligula. Vivamus purus eros, dignissim non, bibendum eget, dapibus sit amet, urna. Donec scelerisque dapibus odio. Aenean gravida ante at eros. Aenean volutpat vehicula urna. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Sed elementum nisi ac dui. Fusce consectetuer venenatis nisi. Etiam diam. Nunc urna tortor, feugiat sed, facilisis eu, lacinia ut, felis. Vivamus vel lacus in massa accumsan faucibus. Cras iaculis eros. Donec non risus eu mi aliquam facilisis. Quisque vitae erat. Vivamus sed felis at metus ullamcorper ornare. Aliquam orci.\n\nPellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Cras ultrices, tortor in tempus laoreet, orci magna auctor quam, ultricies consequat sem eros eu magna. Mauris tempus egestas libero. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos hymenaeos. Curabitur auctor molestie ante. Quisque laoreet urna a quam. Sed sagittis, nulla vitae bibendum tempus, metus ante bibendum urna, ut ullamcorper mi urna vitae neque. Praesent dolor. Maecenas sit amet tortor. Proin erat. Suspendisse ipsum. Nulla facilisi. Vestibulum nonummy nulla ac libero. Aliquam in mauris ut libero cursus ullamcorper. Nam ullamcorper auctor tortor.\n\nNulla et odio. Duis nec velit. Sed posuere neque vitae dolor. Phasellus diam massa, sollicitudin vel, pellentesque in, fringilla pretium, libero. In lacinia massa ut libero pharetra sodales. Aliquam erat volutpat. Suspendisse egestas, turpis at pretium placerat, sem dolor tristique felis, nec egestas lectus risus eu quam. Curabitur leo dolor, varius sed, tristique non, fermentum eget, elit. Vivamus a leo eu diam faucibus congue. Nam quam. Curabitur est velit, luctus quis, tempor et, mattis consectetuer, dolor. Ut accumsan dui nec mi. Donec enim. Mauris accumsan nisl. Praesent sit amet diam ut velit suscipit vehicula. Vestibulum augue diam, placerat sit amet, gravida volutpat, egestas nec, metus. Curabitur risus felis, tempus nec, blandit a, condimentum at, risus.";
	
			byteArray = new Clipperz.ByteArray(longText);
			base64encoding = byteArray.toBase64String();
			byteArray = new Clipperz.ByteArray().appendBase64String(base64encoding);
			is(byteArray.asString(), longText, "the base64 encoding/decoding works also with long texts");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'base32_encoding_decoding_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("base32_encoding_decoding_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray;

			byteArray = new Clipperz.ByteArray("\n");
			is(byteArray.toBase32String(), "18======", "base32 encoding test (1)");

			byteArray = new Clipperz.ByteArray().appendBase32String("18======");
			is(byteArray.toBase32String(), "18======", "base32 encoding test (2)");

			byteArray = new Clipperz.ByteArray().appendBase32String("lLiIo0O1");
			is(byteArray.toBase32String(), "11110001", "base32 encoding test (3)");

			byteArray = new Clipperz.ByteArray().appendBase32String("lLiI o0O1");
			is(byteArray.toBase32String(), "11110001", "base32 encoding test (3.1)");

			byteArray = new Clipperz.ByteArray("0xffffffffff");
			is(byteArray.toBase32String(), "zzzzzzzz", "base32 encoding test (4)");

			byteArray = new Clipperz.ByteArray().appendBase32String("cccccccc");
			is(byteArray.toBase32String(), "cccccccc", "base32 encoding test (5)");

			byteArray = new Clipperz.ByteArray().appendBase32String("bbbbbbbb");
			is(byteArray.toBase32String(), "bbbbbbbb", "base32 encoding test (6)");

			byteArray = new Clipperz.ByteArray().appendBase32String("aAbBcCdD");
			is(byteArray.toHexString(), "0x5296b631ad", "base32 encoding test (7 hex)");
			is(byteArray.toBase32String(), "aabbccdd", "base32 encoding test (7)");

			byteArray = new Clipperz.ByteArray().appendBase32String("lLiIo0O1-aAbBcCdD-abcdefge-ll11ooOO");
			is(byteArray.toBase32String(), "11110001aabbccddabcdefge11110000", "base32 encoding test (8)");

			byteArray = new Clipperz.ByteArray().appendBase32String("lLiI o0O1 - aAbB cCdD - abcd efge - ll11 ooOO");
			is(byteArray.toBase32String(), "11110001aabbccddabcdefge11110000", "base32 encoding test (8.1)");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'base32_encoding_decoding_longText_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("base32_encoding_decoding_longText_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var longText;
			var base32String;
			var byteArray;

			longText = "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Curabitur interdum vulputate ligula. Aliquam erat volutpat. Fusce malesuada felis et purus. Vivamus convallis, metus id lacinia venenatis, est eros sagittis neque, at porttitor erat lorem a risus. Phasellus ultricies nunc et turpis. Ut nec sem ut quam ornare feugiat. Donec nibh. Quisque ullamcorper, neque eu convallis dictum, orci nisl dictum quam, ac elementum arcu sem varius sem. Suspendisse vitae dolor non magna suscipit semper. Suspendisse ac magna. Praesent et purus. Aenean ut justo. Curabitur dictum, nisl a consectetuer viverra, libero est auctor metus, et rhoncus risus diam in orci. Nam magna felis, lobortis eu, eleifend quis, tristique quis, mi. Praesent aliquet. Aenean mollis turpis sed dolor. Donec ac nunc. Nulla eu lacus in ipsum molestie rutrum. Nunc dignissim purus non nibh. Maecenas congue risus at neque sollicitudin eleifend.\n\nNunc purus orci, sagittis ut, vehicula non, auctor eu, orci. Fusce non nisl vel risus rutrum venenatis. Donec dapibus sodales eros. Lorem ipsum dolor sit amet, consectetuer adipiscing elit. In ultrices fringilla dui. Phasellus sit amet enim in tellus rutrum dapibus. Sed blandit malesuada turpis. Ut vel orci. Curabitur id eros. Sed sodales tempus urna. Praesent et neque. Sed ultricies. Praesent risus quam, elementum eget, tincidunt ac, condimentum sit amet, urna.\n\nLorem ipsum dolor sit amet, consectetuer adipiscing elit. Proin vel diam imperdiet felis luctus luctus. Nunc tincidunt nisl ac arcu. Sed id urna id magna nonummy varius. Mauris ligula. Vivamus purus eros, dignissim non, bibendum eget, dapibus sit amet, urna. Donec scelerisque dapibus odio. Aenean gravida ante at eros. Aenean volutpat vehicula urna. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Sed elementum nisi ac dui. Fusce consectetuer venenatis nisi. Etiam diam. Nunc urna tortor, feugiat sed, facilisis eu, lacinia ut, felis. Vivamus vel lacus in massa accumsan faucibus. Cras iaculis eros. Donec non risus eu mi aliquam facilisis. Quisque vitae erat. Vivamus sed felis at metus ullamcorper ornare. Aliquam orci.\n\nPellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Cras ultrices, tortor in tempus laoreet, orci magna auctor quam, ultricies consequat sem eros eu magna. Mauris tempus egestas libero. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos hymenaeos. Curabitur auctor molestie ante. Quisque laoreet urna a quam. Sed sagittis, nulla vitae bibendum tempus, metus ante bibendum urna, ut ullamcorper mi urna vitae neque. Praesent dolor. Maecenas sit amet tortor. Proin erat. Suspendisse ipsum. Nulla facilisi. Vestibulum nonummy nulla ac libero. Aliquam in mauris ut libero cursus ullamcorper. Nam ullamcorper auctor tortor.\n\nNulla et odio. Duis nec velit. Sed posuere neque vitae dolor. Phasellus diam massa, sollicitudin vel, pellentesque in, fringilla pretium, libero. In lacinia massa ut libero pharetra sodales. Aliquam erat volutpat. Suspendisse egestas, turpis at pretium placerat, sem dolor tristique felis, nec egestas lectus risus eu quam. Curabitur leo dolor, varius sed, tristique non, fermentum eget, elit. Vivamus a leo eu diam faucibus congue. Nam quam. Curabitur est velit, luctus quis, tempor et, mattis consectetuer, dolor. Ut accumsan dui nec mi. Donec enim. Mauris accumsan nisl. Praesent sit amet diam ut velit suscipit vehicula. Vestibulum augue diam, placerat sit amet, gravida volutpat, egestas nec, metus. Curabitur risus felis, tempus nec, blandit a, condimentum at, risus.";
			byteArray = new Clipperz.ByteArray(longText);
			base32String = byteArray.toBase32String();
			byteArray = new Clipperz.ByteArray().appendBase32String(base32String);
			is(byteArray.asString(), longText, "base32 encoding test (9)");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'error_correction_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("error_correction_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var	oddInput;
			var byteArray;
	
			oddInput = "0x1aa22bb33cc";
			byteArray = new Clipperz.ByteArray(oddInput);
			is(byteArray.toHexString(), "0x01aa22bb33cc", "fix an input hex string with on odd string length");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'zero_values_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("zero_values_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray;

			byteArray = new Clipperz.ByteArray(0, 0, 0, 0);
			is(byteArray.length(), 4, "An array with 4 '0' values has length = 4");
			is(byteArray.byteAtIndex(0), 0, "The first element of a '0' filled array is '0'");

			byteArray = new Clipperz.ByteArray(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
			is(byteArray.length(), 16, "An array with 16 '0' values has length = 16");
			is(byteArray.byteAtIndex(0), 0, "The first element of a '0' filled array is '0'");
			is(byteArray.byteAtIndex(15), 0, "The last element of a '0' filled array is '0'");
			is(byteArray.toHexString(), "0x00000000000000000000000000000000", "HEX representation of an array full of '0'");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'array_values_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("array_values_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray;
			var arrayValues;
			
			byteArray = new Clipperz.ByteArray("0xa1b2c3d4e5f6");
			arrayValues = byteArray.arrayValues();
			is(arrayValues[0], parseInt("0xa1"), "the first value of arrayValues is 'a1'");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'string_contructor_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("string_contructor_tests", someTestArgs);
		deferredResult.addCallback(function() {
//			var	longAsciiText;
//			var longIsoLatin1Text;
//			var longUtf8Text;
			var stringConstructorTestFunction;
	
//			longAsciiText = "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Nullam ac ipsum. Morbi mauris. Aenean ac elit id metus lobortis elementum. Proin at quam ac ipsum pellentesque adipiscing. Aenean vestibulum, nisl eu suscipit iaculis, quam pede congue mi, sit amet dapibus metus neque eget dui. Suspendisse posuere diam ac sapien. Nulla lobortis dapibus leo. Quisque ornare tortor quis turpis. Aliquam erat volutpat. Ut faucibus lacinia magna. Nunc metus leo, volutpat quis, mollis ac, sagittis ut, turpis. Quisque purus. Mauris ante enim, vehicula eu, suscipit vitae, laoreet vel, nulla. Pellentesque pede leo, aliquam quis, vehicula eget, rhoncus nec, metus. Vestibulum tellus. Suspendisse blandit.  Pellentesque vel tellus. Maecenas arcu. Duis eget purus. Curabitur non pede nec odio cursus luctus. In non elit. Nullam eget nunc in nisl elementum commodo. Vivamus sollicitudin pede quis dui. Morbi commodo. Praesent a risus id urna hendrerit fermentum. Nunc ultricies tristique odio. Phasellus imperdiet, sapien eget viverra blandit, tortor risus blandit nisi, et sodales libero dolor quis nisl. Morbi vel enim. Nunc in quam. Vestibulum a magna.  Fusce auctor elit in augue. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Mauris tincidunt consectetuer leo. Etiam non turpis. Vestibulum eros. Praesent venenatis adipiscing augue. Pellentesque dapibus odio ac arcu rhoncus sagittis. Nullam vitae augue. Ut magna nulla, congue eu, porta in, egestas quis, ligula. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Curabitur malesuada neque non nulla. Nulla facilisi. Fusce viverra magna ut tellus. Sed rutrum pretium sapien. Vivamus dui. Cras fringilla. Nullam lorem.  Vestibulum varius, purus at imperdiet fermentum, metus diam ultricies lacus, vitae aliquam ipsum libero sit amet felis. Sed eget eros ac velit commodo sollicitudin. Morbi in metus in mi viverra lacinia. Sed ut urna. Suspendisse imperdiet tellus ac velit. Duis malesuada velit sit amet sapien. Vestibulum a sapien id libero accumsan luctus. Fusce iaculis. Donec pulvinar orci ut pede. Nam placerat sem ut sem.  Ut pretium bibendum nisl. Suspendisse potenti. Phasellus mollis neque in neque. Suspendisse augue magna, eleifend et, malesuada at, viverra in, nisl. Donec vel lectus in justo ultrices tristique. Aliquam erat volutpat. Morbi suscipit, risus ac volutpat mollis, risus metus hendrerit sapien, ac scelerisque est orci eu est. Quisque sit amet velit. Sed libero diam, semper id, eleifend ac, iaculis non, nulla. Donec varius tincidunt arcu. Sed quis metus eu erat adipiscing viverra. Donec odio. Aenean sagittis nisl sed purus. Fusce vel nibh quis felis accumsan bibendum. Etiam et risus ac tortor cursus pharetra. Maecenas tellus. Pellentesque nec felis id eros vehicula commodo. Aliquam interdum sagittis odio. Maecenas at lorem eget mi aliquet sagittis.  Mauris quis nibh in odio sodales lacinia. Proin augue mauris, placerat a, blandit vel, tincidunt eget, ante. Quisque turpis purus, placerat eget, tempor consectetuer, aliquet ac, enim. Etiam eleifend vestibulum mi. Vivamus gravida. Morbi dolor. In hac habitasse platea dictumst. Nulla commodo lectus faucibus lorem. Phasellus aliquet pede id metus hendrerit tempus. Fusce convallis pede ac neque tempor dignissim. Sed vitae lorem sit amet justo dapibus porta. Ut quam orci, pretium non, sagittis nec, condimentum id, dolor. Sed tempor.  Nunc porta rutrum leo. Nunc id sem. Sed nibh tortor, dapibus eget, feugiat a, pretium pretium, purus. Suspendisse suscipit lobortis sem. Praesent pharetra orci. Quisque molestie tristique quam. Maecenas nunc lorem, rhoncus non, venenatis sed, sodales at, felis. Quisque semper. Quisque malesuada est quis lacus. Nullam a justo. Aliquam pellentesque, ante ut congue molestie, nisl sapien posuere nisl, eu cursus nulla ligula vel nisl. Fusce commodo lacinia magna. Aenean rutrum vestibulum lorem. Pellentesque fermentum tristique ipsum. Nulla facilisi. Donec id mi eget ipsum commodo egestas. Mauris iaculis.  Nulla vulputate mi at nisl. In condimentum sodales tellus. Donec metus orci, mollis vel, accumsan ac, ornare ac, lacus. Pellentesque accumsan est et tellus. Nam mollis. Aenean accumsan eros sit amet tellus. Praesent eu libero. Sed tempus urna nec dolor. Nulla facilisi. Duis eleifend rhoncus neque. Curabitur consectetuer quam eu justo. Sed metus. Vivamus risus. Aliquam erat volutpat. Aliquam erat volutpat. Nunc semper urna. Praesent molestie libero a lacus. Nullam suscipit lobortis velit. Praesent rhoncus, felis ut interdum dapibus, ipsum lectus vestibulum nulla, in interdum risus dolor eget orci.  Nullam venenatis. Suspendisse laoreet, arcu a luctus consectetuer, libero ligula condimentum quam, eget elementum mauris tortor sed enim. Pellentesque leo. Nam interdum malesuada ante. Praesent fermentum nunc et dolor. Donec auctor volutpat odio. Pellentesque volutpat egestas ipsum. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Cras ac ligula eu justo dignissim accumsan. Nullam nisi. Fusce id sem. Fusce et urna. Pellentesque commodo pharetra lorem. Donec erat. Vestibulum elit arcu, commodo et, consequat eget, posuere eget, sem. Morbi sed nulla sed neque commodo commodo. Aliquam erat volutpat. Ut id turpis a enim malesuada vestibulum.  In arcu dui, dignissim vitae, blandit eu, egestas ac, arcu. In ultricies sapien vitae nisi. Proin rhoncus magna eget tortor. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. In dictum. Sed volutpat pharetra quam. Mauris eget eros. Fusce malesuada dolor id pede. Praesent nec justo sed nisi vehicula varius. In scelerisque convallis nulla. Pellentesque sodales massa vulputate urna. Duis metus urna, imperdiet ac, sodales vel, ullamcorper sed, pede. Vestibulum aliquam mollis metus.  Praesent tempus tristique elit. Maecenas tellus tortor, pretium id, mollis id, molestie non, turpis. Vivamus nibh magna, bibendum vitae, a.";
			is(longAsciiText.length, 6000, "tThe ascii string should be 6000 char long");

//			longIsoLatin1Text = "¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ¡¢£¤¥¦§¨©ª«¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùù";
			is(longIsoLatin1Text.length, 3000, "The Iso-Latin1 string should be 3000 char long");

//			longUtf8Text = "客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之所以能统称为汉语第客家方言简体字危及了对古代文学的研究共有找华语并不被中国政府很好的接受然而汉语之";
			is(longUtf8Text.length, 2000, "tThe UTF8 string should be 2000 char long");
	
			stringConstructorTestFunction = function(aTestString, aLabel) {
				var byteArray;
				var base64String;
				var stringConstructorStartTime, stringConstructorEndTime;
				var maxExpectedTime;
		
stringConstructorStartTime = new Date();		
				byteArray = new Clipperz.ByteArray(aTestString);
stringConstructorEndTime = new Date();
				maxExpectedTime = 30;
				is(byteArray.length(), 6000, "[" + aLabel + "] The reference text should be 6000 bytes long");
				is((stringConstructorEndTime - stringConstructorStartTime) < maxExpectedTime, true, "[" + aLabel + "] The construction of the byte array took " + (stringConstructorEndTime - stringConstructorStartTime) + " (expected < " + maxExpectedTime + ")");
//MochiKit.Logging.logDebug("[" + aLabel + "] The construction of the byte array took " + (stringConstructorEndTime - stringConstructorStartTime));

stringConstructorStartTime = new Date();		
				base64String = byteArray.toBase64String();
stringConstructorEndTime = new Date();
				maxExpectedTime = 30;
				is(base64String.length, 8000, "[" + aLabel + "] the base64 representation of 6000 bytes is 8000 chars long");
				is((stringConstructorEndTime - stringConstructorStartTime) < maxExpectedTime, true, "[" + aLabel + "] The generation of the base64 string took " + (stringConstructorEndTime - stringConstructorStartTime) + " (expected < " + maxExpectedTime + ")");
//MochiKit.Logging.logDebug("[" + aLabel + "] The generation of the base64 string took " + (stringConstructorEndTime - stringConstructorStartTime));

stringConstructorStartTime = new Date();		
				byteArray = new Clipperz.ByteArray();
				byteArray.appendBase64String(base64String);
stringConstructorEndTime = new Date();
				maxExpectedTime = 200;
				is(byteArray.length(), 6000, "[" + aLabel + "] a byte array build from a 8000 chars base64 string should contain 6000 bytes");
				is((stringConstructorEndTime - stringConstructorStartTime) < maxExpectedTime, true, "[" + aLabel + "] The construction of the byte array from a base64 string took " + (stringConstructorEndTime - stringConstructorStartTime) + " (expected < " + maxExpectedTime + ")");
//MochiKit.Logging.logDebug("[" + aLabel + "] The construction of the byte array from a base64 string took " + (stringConstructorEndTime - stringConstructorStartTime));
			}

			stringConstructorTestFunction(longAsciiText,	 "ASCII");
			stringConstructorTestFunction(longIsoLatin1Text, "ISO-Latin1");
			stringConstructorTestFunction(longUtf8Text,		 "UTF-8");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'bit_operations_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("bit_operations_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var byteArray;
			
			byteArray = new Clipperz.ByteArray(0, 255, 0, 255);
			is(byteArray.bitAtIndex(0), 1, "bitAtIndex - test 1");
			is(byteArray.bitAtIndex(7), 1, "bitAtIndex - test 2");
			is(byteArray.bitAtIndex(8), 0, "bitAtIndex - test 3");

			is(byteArray.bitBlockAtIndexWithSize(0, 4), 15, "bitBlockAtIndexWithSize - test 1");
			is(byteArray.bitBlockAtIndexWithSize(4, 4), 15, "bitBlockAtIndexWithSize - test 2");
			is(byteArray.bitBlockAtIndexWithSize(4, 6), 15, "bitBlockAtIndexWithSize - test 3");
			is(byteArray.bitBlockAtIndexWithSize(8, 4), 0, "bitBlockAtIndexWithSize - test 4");
			is(byteArray.bitBlockAtIndexWithSize(12, 4), 0, "bitBlockAtIndexWithSize - test 5");
			is(byteArray.bitBlockAtIndexWithSize(12, 5), 16, "bitBlockAtIndexWithSize - test 6");
			is(byteArray.bitBlockAtIndexWithSize(12, 6), 48, "bitBlockAtIndexWithSize - test 7");

		});
		deferredResult.callback();
		
		return deferredResult;
	},
	
	//-------------------------------------------------------------------------
	
	'prefixMatchingBits_tests': function (someTestArgs) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("simple_tests", someTestArgs);
		deferredResult.addCallback(function() {
			var	value1;
			var value2;
			var toll;
		
//			toll = new Clipperz.PM.Toll();

			value1 = new Clipperz.ByteArray("0x0000000000000000000000000000000000000000000000000000000000000000");
			value2 = new Clipperz.ByteArray("0x0000000000000000000000000000000000000000000000000000000000000000");
			is(Clipperz.ByteArray.prefixMatchingBits(value1, value2), 256);

			value1 = new Clipperz.ByteArray("0x0000000000000000000000000000000000000000000000000000000000000000");
			value2 = new Clipperz.ByteArray("0x0000000000000000000000000000000000000000000000000000000000000008");
			is(Clipperz.ByteArray.prefixMatchingBits(value1, value2), 252);

			value1 = new Clipperz.ByteArray("0x0000000000000000000000000000000000000000000000000000000000000000");
			value2 = new Clipperz.ByteArray("0x0000000000000000000000000000000080000000000000000000000000000000");
			is(Clipperz.ByteArray.prefixMatchingBits(value1, value2), 128);

			value1 = new Clipperz.ByteArray("0x0000000000000000000000000000000000000000000000000000000000000000");
			value2 = new Clipperz.ByteArray("0x8000000000000000000000000000000080000000000000000000000000000000");
			is(Clipperz.ByteArray.prefixMatchingBits(value1, value2), 0);

			value1 = new Clipperz.ByteArray("0x0000000000000000000000000000000000000000000000000000000000000000");
			value2 = new Clipperz.ByteArray("0x0100000000000000000000000000000000000000000000000000000000000000");
			is(Clipperz.ByteArray.prefixMatchingBits(value1, value2), 7);

			value1 = new Clipperz.ByteArray("0x0000000000000000000000000000000000000000000000000000000000000000");
			value2 = new Clipperz.ByteArray("0x0000000000000000000000000000000000000000000000000000000000000001");
			is(Clipperz.ByteArray.prefixMatchingBits(value1, value2), 255);

			value1 = new Clipperz.ByteArray("0x0000000000000000000000000000000000000000000000000000000000000000");
			value2 = new Clipperz.ByteArray("0x4000000000000000000000000000000000000000000000000000000000000000");
			is(Clipperz.ByteArray.prefixMatchingBits(value1, value2), 1);

			value1 = new Clipperz.ByteArray("0x0853120d776e04325d070c082e06390315505166740b07007f060f003e0c576f");
			value2 = new Clipperz.ByteArray("0x0853120d776e04325d070c082e06390315505166740b07007f060f003e0c576f");
			is(Clipperz.ByteArray.prefixMatchingBits(value1, value2), 256);

			value1 = new Clipperz.ByteArray("0x0a03580756011d000a620e3b60670508092f020a0a0c2500282d480307000046");
			value2 = new Clipperz.ByteArray("0x0a03580756011d000a620e3b60670508092f020a0a0c2500282d480307004d46");
			is(Clipperz.ByteArray.prefixMatchingBits(value1, value2), 241);

			value1 = new Clipperz.ByteArray("0x0a03580756011d000a620e3b60670508092f020a0a0c2500282d480307000046");
			value2 = new Clipperz.ByteArray("0x0a83580756011d000a620e3b60670508092f020a0a0c2500282d480307004d46");
			is(Clipperz.ByteArray.prefixMatchingBits(value1, value2), 8);
		});
		
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'binaryString_test': function (someTestArgs) {
		var	hexString;
		var binaryString;

		var testBinaryString = function (anHexString, aBinaryString) {
			var	byteArray;
			var byteArrayFromBinaryString;

			byteArray = new Clipperz.ByteArray(anHexString);
			byteArrayFromBinaryString = (new Clipperz.ByteArray()).appendBinaryString(aBinaryString);
			ok(byteArray.equals(byteArrayFromBinaryString), "the two byteArray have the same values");
			is(byteArray.toBinaryString(), aBinaryString, "the binaryString matches");
		};
		
		hexString = "0x888990";
		binaryString = "\x88\x89\x90";
		testBinaryString(hexString, binaryString);

		hexString = "0x000102030405060708090a0b0c0d0e0f";
		binaryString = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f";
		testBinaryString(hexString, binaryString);

		hexString = "0x101112131415161718191a1b1c1d1e1f";
		binaryString = "\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f";
		testBinaryString(hexString, binaryString);

		hexString = "0xd0d1d2d3d4d5d6d7d8d9dadbdcdddedf";
		binaryString = "\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf";
		testBinaryString(hexString, binaryString);

		hexString = "0xe0e1e2e3e4e5e6e7e8e9eaebecedeeef";
		binaryString = "\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef";
		testBinaryString(hexString, binaryString);

		hexString = "0xf0f1f2f3f4f5f6f7f8f9fafbfcfdfeff";
		binaryString = "\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff";
		testBinaryString(hexString, binaryString);
	},

    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};

//#############################################################################

SimpleTest.runDeferredTests("Clipperz.ByteArray", tests, {trace:false});
