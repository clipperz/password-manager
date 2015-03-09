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

try { if (typeof(Clipperz.ByteArray) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.Crypto.AES depends on Clipperz.ByteArray!";
}  

//	Dependency commented to avoid a circular reference
//try { if (typeof(Clipperz.Crypto.PRNG) == 'undefined') { throw ""; }} catch (e) {
//	throw "Clipperz.Crypto.AES depends on Clipperz.Crypto.PRNG!";
//}  

if (typeof(Clipperz.Crypto.AES) == 'undefined') { Clipperz.Crypto.AES = {}; }

//#############################################################################

Clipperz.Crypto.AES.DeferredExecutionContext = function(args) {
	args = args || {};

	this._key = args.key;
	this._message = args.message;
	this._result = args.message.clone();
	this._nonce = args.nonce;
	this._messageLength = this._message.length();
	
	this._messageArray = this._message.arrayValues();
	this._resultArray = this._result.arrayValues();
	this._nonceArray = this._nonce.arrayValues();
	
	this._executionStep = 0;
	
	return this;
}

Clipperz.Crypto.AES.DeferredExecutionContext.prototype = MochiKit.Base.update(null, {

	'key': function() {
		return this._key;
	},
	
	'message': function() {
		return this._message;
	},
	
	'messageLength': function() {
		return this._messageLength;
	},
	
	'result': function() {
		return new Clipperz.ByteArray(this.resultArray());
	},
	
	'nonce': function() {
		return this._nonce;
	},

	'messageArray': function() {
		return this._messageArray;
	},
	
	'resultArray': function() {
		return this._resultArray;
	},
	
	'nonceArray': function() {
		return this._nonceArray;
	},

	'elaborationChunkSize': function() {
		return Clipperz.Crypto.AES.DeferredExecution.chunkSize;
	},
	
	'executionStep': function() {
		return this._executionStep;
	},
	
	'setExecutionStep': function(aValue) {
		this._executionStep = aValue;
	},
	
	'pause': function(aValue) {
		return MochiKit.Async.wait(Clipperz.Crypto.AES.DeferredExecution.pauseTime, aValue);
	},
	
	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});

//#############################################################################

Clipperz.Crypto.AES.Key = function(args) {
	args = args || {};

	this._key = args.key;
	this._keySize = args.keySize || this.key().length();
	
	if (this.keySize() == 128/8) {
		this._b = 176;
		this._numberOfRounds = 10;
 	} else if (this.keySize() == 256/8) {
		this._b = 240;
		this._numberOfRounds = 14;
	} else {
		MochiKit.Logging.logError("AES unsupported key size: " + (this.keySize() * 8) + " bits");
		throw Clipperz.Crypto.AES.exception.UnsupportedKeySize;
	}
	
	this._stretchedKey = null;
	
	return this;
}

Clipperz.Crypto.AES.Key.prototype = MochiKit.Base.update(null, {

	'asString': function() {
		return "Clipperz.Crypto.AES.Key (" + this.key().toHexString() + ")";
	},
	
	//-----------------------------------------------------------------------------

	'key': function() {
		return this._key;
	},

	'keySize': function() {
		return this._keySize;
	},
	
	'b': function() {
		return this._b;
	},
	
	'numberOfRounds': function() {
		return this._numberOfRounds;
	},
	//=========================================================================

	'keyScheduleCore': function(aWord, aRoundConstantsIndex) {
		var	result;
		var sbox;

		sbox = Clipperz.Crypto.AES.sbox();

		result = [	sbox[aWord[1]] ^ Clipperz.Crypto.AES.roundConstants()[aRoundConstantsIndex],
					sbox[aWord[2]],
					sbox[aWord[3]],
					sbox[aWord[0]]	];

		return result;
	},

	//-----------------------------------------------------------------------------

	'xorWithPreviousStretchValues': function(aKey, aWord, aPreviousWordIndex) {
		var	result;
		var i,c;
		
		result = [];
		c = 4;
		for (i=0; i<c; i++) {
			result[i] = aWord[i] ^ aKey.byteAtIndex(aPreviousWordIndex + i);
		}
		
		return result;
	},

	//-----------------------------------------------------------------------------

	'sboxShakeup': function(aWord) {
		var result;
		var sbox;
		var i,c;
		
		result = [];
		sbox = Clipperz.Crypto.AES.sbox();
		c =4;
		for (i=0; i<c; i++) {
			result[i] = sbox[aWord[i]];
		}
		
		return result;
	},

	//-----------------------------------------------------------------------------

	'stretchKey': function(aKey) {
		var	currentWord;
		var	keyLength;
		var	previousStretchIndex;
		var i,c;
		
		keyLength = aKey.length();
		previousStretchIndex = keyLength - this.keySize();

		currentWord = [	aKey.byteAtIndex(keyLength - 4),
						aKey.byteAtIndex(keyLength - 3),
						aKey.byteAtIndex(keyLength - 2),
						aKey.byteAtIndex(keyLength - 1)	];
		currentWord = this.keyScheduleCore(currentWord, keyLength / this.keySize());

		if (this.keySize() == 256/8) {
			c = 8;
		} else if (this.keySize() == 128/8){
			c = 4;
		}
		
		for (i=0; i<c; i++) {
			if (i == 4) {
				//	fifth streatch word
				currentWord = this.sboxShakeup(currentWord);
			}

			currentWord = this.xorWithPreviousStretchValues(aKey, currentWord, previousStretchIndex + (i*4));
			aKey.appendBytes(currentWord);
		}

		return aKey;
	},

	//-----------------------------------------------------------------------------

	'stretchedKey': function() {
		if (this._stretchedKey == null) {
			var stretchedKey;
			
			stretchedKey = this.key().clone();

			while (stretchedKey.length() < this.keySize()) {
				stretchedKey.appendByte(0);
			}

			while (stretchedKey.length() < this.b()) {
				stretchedKey = this.stretchKey(stretchedKey);
			}
			
			this._stretchedKey = stretchedKey.split(0, this.b());
		}
		
		return this._stretchedKey;
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});

//#############################################################################

Clipperz.Crypto.AES.State = function(args) {
	args = args || {};

	this._data = args.block;
	this._key = args.key;
	
	return this;
}

Clipperz.Crypto.AES.State.prototype = MochiKit.Base.update(null, {

	'key': function() {
		return this._key;
	},
	
	//-----------------------------------------------------------------------------

	'data': function() {
		return this._data;
	},

	'setData': function(aValue) {
		this._data = aValue;
	},

	//=========================================================================

	'addRoundKey': function(aRoundNumber) {
	 	//	each byte of the state is combined with the round key; each round key is derived from the cipher key using a key schedule.
		var	data;
		var	stretchedKey;
		var	firstStretchedKeyIndex;
		var i,c;

		data = this.data();
		stretchedKey = this.key().stretchedKey();
		firstStretchedKeyIndex = aRoundNumber * (128/8);
		c = 128/8;
		for (i=0; i<c; i++) {
			data[i] = data[i] ^ stretchedKey.byteAtIndex(firstStretchedKeyIndex + i);
		}
	},
	
	//-----------------------------------------------------------------------------

	'subBytes': function() {
		//	 a non-linear substitution step where each byte is replaced with another according to a lookup table.
		var i,c;
		var	data;
		var sbox;
		
		data = this.data();
		sbox = Clipperz.Crypto.AES.sbox();
		
		c = 16;
		for (i=0; i<c; i++) {
			data[i] = sbox[data[i]];
		}
	},

	//-----------------------------------------------------------------------------

	'shiftRows': function() {
		//	a transposition step where each row of the state is shifted cyclically a certain number of steps.
		var	newValue;
		var	data;
		var	shiftMapping;
		var	i,c;
		
		newValue = new Array(16);
		data = this.data();
		shiftMapping = Clipperz.Crypto.AES.shiftRowMapping();
//		[0, 5, 10, 15, 4, 9, 14, 3, 8, 13, 2, 7, 12, 1, 6, 11];
		c = 16;
		for (i=0; i<c; i++) {
			newValue[i] = data[shiftMapping[i]];
		}
		for (i=0; i<c; i++) {
			data[i] = newValue[i];
		}
	},

	//-----------------------------------------------------------------------------
/*
	'mixColumnsWithValues': function(someValues) {
		var	result;
		var	a;
		var i,c;
		
		c = 4;
		result = [];
		a = [];
		for (i=0; i<c; i++) {
			a[i] = [];
			a[i][1] = someValues[i]
			if ((a[i][1] & 0x80) == 0x80) {
				a[i][2] = (a[i][1] << 1) ^ 0x11b;
			} else {
				a[i][2] = a[i][1] << 1;
			}
			
			a[i][3] = a[i][2] ^ a[i][1];
		}
	
		for (i=0; i<c; i++) {
			var	x;

			x = Clipperz.Crypto.AES.mixColumnsMatrix()[i];
			result[i] = a[0][x[0]] ^ a[1][x[1]] ^ a[2][x[2]] ^ a[3][x[3]];
		}

		return result;
	},
	
	'mixColumns': function() {
		//	a mixing operation which operates on the columns of the state, combining the four bytes in each column using a linear transformation.
		var data;
		var i, c;
		
		data = this.data();
		c = 4;
		for(i=0; i<c; i++) {
			var	blockIndex;
			var mixedValues;
			
			blockIndex = i * 4;
			mixedValues = this.mixColumnsWithValues([	data[blockIndex + 0],
														data[blockIndex + 1],
														data[blockIndex + 2],
														data[blockIndex + 3]]);
			data[blockIndex + 0] = mixedValues[0];
			data[blockIndex + 1] = mixedValues[1];
			data[blockIndex + 2] = mixedValues[2];
			data[blockIndex + 3] = mixedValues[3];
		}
	},
*/

	'mixColumns': function() {
		//	a mixing operation which operates on the columns of the state, combining the four bytes in each column using a linear transformation.
		var data;
		var i, c;
		var a_1;
		var a_2;

		a_1 = new Array(4);
		a_2 = new Array(4);
		
		data = this.data();
		c = 4;
		for(i=0; i<c; i++) {
			var	blockIndex;
			var ii, cc;
			
			blockIndex = i * 4;

			cc = 4;
			for (ii=0; ii<cc; ii++) {
				var value;
				
				value = data[blockIndex + ii];
				a_1[ii] = value;
				a_2[ii] = (value & 0x80) ? ((value << 1) ^ 0x011b) : (value << 1);
			}

			data[blockIndex + 0] = a_2[0] ^ a_1[1] ^ a_2[1] ^ a_1[2] ^ a_1[3];
			data[blockIndex + 1] = a_1[0] ^ a_2[1] ^ a_1[2] ^ a_2[2] ^ a_1[3];
			data[blockIndex + 2] = a_1[0] ^ a_1[1] ^ a_2[2] ^ a_1[3] ^ a_2[3];
			data[blockIndex + 3] = a_1[0] ^ a_2[0] ^ a_1[1] ^ a_1[2] ^ a_2[3];
		}
	},

	//=========================================================================
	
	'spinRound': function(aRoundNumber) {
		this.addRoundKey(aRoundNumber);
		this.subBytes();
		this.shiftRows();
		this.mixColumns();
	},

	'spinLastRound': function() {
		this.addRoundKey(this.key().numberOfRounds() - 1);
		this.subBytes();
		this.shiftRows();
		this.addRoundKey(this.key().numberOfRounds());
	},

	//=========================================================================

	'encrypt': function() {
		var	i,c;

		c = this.key().numberOfRounds() - 1;
		for (i=0; i<c; i++) {
			this.spinRound(i);
		}

		this.spinLastRound();
	},
	
	//=========================================================================
	__syntaxFix__: "syntax fix"
});

//#############################################################################

Clipperz.Crypto.AES.VERSION = "0.1";
Clipperz.Crypto.AES.NAME = "Clipperz.Crypto.AES";

MochiKit.Base.update(Clipperz.Crypto.AES, {

//	http://www.cs.eku.edu/faculty/styer/460/Encrypt/JS-AES.html
//	http://en.wikipedia.org/wiki/Advanced_Encryption_Standard
//	http://en.wikipedia.org/wiki/Rijndael_key_schedule 
//	http://en.wikipedia.org/wiki/Rijndael_S-box
		
	'__repr__': function () {
		return "[" + this.NAME + " " + this.VERSION + "]";
	},

	'toString': function () {
		return this.__repr__();
	},

	//=============================================================================
	
	'_sbox': null,
	'sbox': function() {
		if (Clipperz.Crypto.AES._sbox == null) {
			Clipperz.Crypto.AES._sbox = [
0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
			];
		}
		
		return Clipperz.Crypto.AES._sbox;
	},

	//-----------------------------------------------------------------------------
	//
	//		0	4	8	12				0	4	8	12
	//		1	5	9	13		=>		5	9	13	1
	//		2	6	10	14				10	14	2	6
	//		3	7	11	15				15	3	7	11
	//
	'_shiftRowMapping': null,
	'shiftRowMapping': function() {
		if (Clipperz.Crypto.AES._shiftRowMapping == null) {
			Clipperz.Crypto.AES._shiftRowMapping = [0, 5, 10, 15, 4, 9, 14, 3, 8, 13, 2, 7, 12, 1, 6, 11];
		}
		
		return Clipperz.Crypto.AES._shiftRowMapping;
	},

	//-----------------------------------------------------------------------------

	'_mixColumnsMatrix': null,
	'mixColumnsMatrix': function() {
		if (Clipperz.Crypto.AES._mixColumnsMatrix == null) {
			Clipperz.Crypto.AES._mixColumnsMatrix = [	[2, 3, 1 ,1],
														[1, 2, 3, 1],
														[1, 1, 2, 3],
														[3, 1, 1, 2]   ];
		}
		
		return Clipperz.Crypto.AES._mixColumnsMatrix;
	},

	'_roundConstants': null,
	'roundConstants': function() {
		if (Clipperz.Crypto.AES._roundConstants == null) {
			Clipperz.Crypto.AES._roundConstants = [ , 1, 2, 4, 8, 16, 32, 64, 128, 27, 54, 108, 216, 171, 77, 154];
//			Clipperz.Crypto.AES._roundConstants = [ , 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a];
		}
		
		return Clipperz.Crypto.AES._roundConstants;
	},
	
	//=============================================================================

	'incrementNonce': function(aNonce) {
//Clipperz.Profile.start("Clipperz.Crypto.AES.incrementNonce");
		var i;
		var done;
		
		done = false;
		i = aNonce.length - 1;
		
		while ((i>=0) && (done == false)) {
			var currentByteValue;
			
			currentByteValue = aNonce[i];
			
			if (currentByteValue == 0xff) {
				aNonce[i] = 0;
				if (i>= 0) {
					i --;
				} else {
					done = true;
				}
			} else {
				aNonce[i] = currentByteValue + 1;
				done = true;
			}
		}
//Clipperz.Profile.stop("Clipperz.Crypto.AES.incrementNonce");
	},
	
	//-----------------------------------------------------------------------------

	'encryptBlock': function(aKey, aBlock) {
		var	result;
		var	state;

		state = new Clipperz.Crypto.AES.State({block:aBlock, key:aKey});
//is(state.data(), 'before');
		state.encrypt();
		result = state.data(); 
		
		return result;
	},

	//-----------------------------------------------------------------------------

	'encryptBlocks': function(aKey, aMessage, aNonce) {
		var	result;
		var nonce;
		var self;
		var	messageIndex;
		var	messageLength;
		var blockSize;		
		
		self = Clipperz.Crypto.AES;
		blockSize = 128/8;
		messageLength = aMessage.length;
		nonce = aNonce;

		result = aMessage;
		messageIndex = 0;
		while (messageIndex < messageLength) {
			var encryptedBlock;
			var i,c;
			
			self.incrementNonce(nonce);
			encryptedBlock = self.encryptBlock(aKey, nonce);
			
			if ((messageLength - messageIndex) > blockSize) {
				c = blockSize;
			} else {
				c = messageLength - messageIndex;
			}
			
			for (i=0; i<c; i++) {
				result[messageIndex + i] = result[messageIndex + i] ^ encryptedBlock[i];
			}
			
			messageIndex += blockSize;
		}
		
		return result;
	},
	
	//-----------------------------------------------------------------------------

	'encrypt': function(aKey, someData, aNonce) {
		var result;
		var nonce;
		var	encryptedData;
		var key;

		key = new Clipperz.Crypto.AES.Key({key:aKey});
		nonce = aNonce ? aNonce.clone() : Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(128/8);

		encryptedData = Clipperz.Crypto.AES.encryptBlocks(key, someData.arrayValues(), nonce.arrayValues());

		result = nonce.appendBytes(encryptedData);
		
		return result;
	},

	//-----------------------------------------------------------------------------

	'decrypt': function(aKey, someData) {
		var result;
		var nonce;
		var encryptedData;
		var decryptedData;
		var	dataIterator;
		var key;

		key = new Clipperz.Crypto.AES.Key({key:aKey});

		encryptedData = someData.arrayValues();
		nonce = encryptedData.slice(0, (128/8));
		encryptedData = encryptedData.slice(128/8);
		decryptedData = Clipperz.Crypto.AES.encryptBlocks(key, encryptedData, nonce);

		result = new Clipperz.ByteArray(decryptedData);
		
		return result;
	},
	
	//=============================================================================

	'deferredEncryptExecutionChunk': function(anExecutionContext) {
		var	result;
		var nonce;
		var self;
		var	messageIndex;
		var	messageLength;
		var blockSize;	
		var executionLimit;

		self = Clipperz.Crypto.AES;
		blockSize = 128/8;
		messageLength = anExecutionContext.messageArray().length;
		nonce = anExecutionContext.nonceArray();
		result = anExecutionContext.resultArray();

		messageIndex = anExecutionContext.executionStep();
		executionLimit = messageIndex + anExecutionContext.elaborationChunkSize();
		executionLimit = Math.min(executionLimit, messageLength);
		
		while (messageIndex < executionLimit) {
			var encryptedBlock;
			var i,c;
			
			self.incrementNonce(nonce);
			encryptedBlock = self.encryptBlock(anExecutionContext.key(), nonce);
			
			if ((executionLimit - messageIndex) > blockSize) {
				c = blockSize;
			} else {
				c = executionLimit - messageIndex;
			}
			
			for (i=0; i<c; i++) {
				result[messageIndex + i] = result[messageIndex + i] ^ encryptedBlock[i];
			}
			
			messageIndex += blockSize;
		}
		anExecutionContext.setExecutionStep(messageIndex);
		
		return anExecutionContext;
	},

	//-----------------------------------------------------------------------------
	
	'deferredEncryptBlocks': function(anExecutionContext) {
		var	deferredResult;
		var	messageSize;
		var i,c;
		var now;
		
		messageSize = anExecutionContext.messageLength();		
		
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.Crypto.AES.deferredEncryptBlocks - START: " + res); return res;});
//		deferredResult.addCallback(MochiKit.Base.method(anExecutionContext, 'pause'));

		c = Math.ceil(messageSize / anExecutionContext.elaborationChunkSize());
		for (i=0; i<c; i++) {
//deferredResult.addBoth(function(res) {now = new Date(); return res;});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.Crypto.AES.deferredEncryptBlocks - : (" + i + ") - " + res); return res;});
			deferredResult.addCallback(Clipperz.Crypto.AES.deferredEncryptExecutionChunk);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("[" + (new Date() - now) + "]Clipperz.Crypto.AES.deferredEncryptBlocks"); return res;});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.Crypto.AES.deferredEncryptBlocks - : (" + i + ") -- " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(anExecutionContext, 'pause'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.Crypto.AES.deferredEncryptBlocks - : (" + i + ") --- " + res); return res;});
		}
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.Crypto.AES.deferredEncryptBlocks - END: " + res); return res;});
		
		deferredResult.callback(anExecutionContext);
		
		return deferredResult;
	},

	//-----------------------------------------------------------------------------
	
	'deferredEncrypt': function(aKey, someData, aNonce) {
		var deferredResult;
		var	executionContext;
		var result;
		var nonce;
		var key;

		key = new Clipperz.Crypto.AES.Key({key:aKey});
		nonce = aNonce ? aNonce.clone() : Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(128/8);

		executionContext = new Clipperz.Crypto.AES.DeferredExecutionContext({key:key, message:someData, nonce:nonce});

		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.Crypto.AES.deferredEncrypt - 1: " + res); return res;});
		deferredResult.addCallback(Clipperz.Crypto.AES.deferredEncryptBlocks);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.Crypto.AES.deferredEncrypt - 2: " + res); return res;});
		deferredResult.addCallback(function(anExecutionContext) {
			var result;
			
			result = anExecutionContext.nonce().clone();
			result.appendBytes(anExecutionContext.resultArray());

			return result;
		});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.Crypto.AES.deferredEncrypt - 3: " + res); return res;});
		deferredResult.callback(executionContext)

		return deferredResult;
	},

	//-----------------------------------------------------------------------------

	'deferredDecrypt': function(aKey, someData) {
		var deferredResult
		var nonce;
		var message;
		var key;

		key = new Clipperz.Crypto.AES.Key({key:aKey});
		nonce = someData.split(0, (128/8));
		message = someData.split(128/8);
		executionContext = new Clipperz.Crypto.AES.DeferredExecutionContext({key:key, message:message, nonce:nonce});

		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(Clipperz.Crypto.AES.deferredEncryptBlocks);
		deferredResult.addCallback(function(anExecutionContext) {
			return anExecutionContext.result();
		});
		deferredResult.callback(executionContext);

		return deferredResult;
	},
	
	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});

//#############################################################################

Clipperz.Crypto.AES.DeferredExecution = {
	'chunkSize': 4096,	//	1024	4096	8192	16384	32768;
	'pauseTime': 0.2
}

Clipperz.Crypto.AES.exception = {
	'UnsupportedKeySize':    new MochiKit.Base.NamedError("Clipperz.Crypto.AES.exception.UnsupportedKeySize") 
};
