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

try { if (typeof(Clipperz.ByteArray) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.Crypto.PRNG depends on Clipperz.ByteArray!";
}  

if (typeof(Clipperz.Crypto) == 'undefined') { Clipperz.Crypto = {}; }
if (typeof(Clipperz.Crypto.SHA) == 'undefined') { Clipperz.Crypto.SHA = {}; }

Clipperz.Crypto.SHA.VERSION = "0.3";
Clipperz.Crypto.SHA.NAME = "Clipperz.Crypto.SHA";

MochiKit.Base.update(Clipperz.Crypto.SHA, {

	'__repr__': function () {
		return "[" + this.NAME + " " + this.VERSION + "]";
	},

	'toString': function () {
		return this.__repr__();
	},

	//-----------------------------------------------------------------------------

	'rotateRight': function(aValue, aNumberOfBits) {
//Clipperz.Profile.start("Clipperz.Crypto.SHA.rotateRight");
		var result;
		
		result = (aValue >>> aNumberOfBits) | (aValue << (32 - aNumberOfBits));
		
//Clipperz.Profile.stop("Clipperz.Crypto.SHA.rotateRight");
		return result;
	},

	'shiftRight': function(aValue, aNumberOfBits) {
//Clipperz.Profile.start("Clipperz.Crypto.SHA.shiftRight");
		var result;
		
		result = aValue >>> aNumberOfBits;
		
//Clipperz.Profile.stop("Clipperz.Crypto.SHA.shiftRight");
		return result;
	},

	//-----------------------------------------------------------------------------

	'safeAdd': function() {
//Clipperz.Profile.start("Clipperz.Crypto.SHA.safeAdd");
		var	result;
		var	i, c;
		
		result = arguments[0];
		c = arguments.length;
		for (i=1; i<c; i++) {
			var	lowerBytesSum;

			lowerBytesSum = (result & 0xffff) + (arguments[i] & 0xffff);
			result = (((result >> 16) + (arguments[i] >> 16) + (lowerBytesSum >> 16)) << 16) | (lowerBytesSum & 0xffff);
		}
		
//Clipperz.Profile.stop("Clipperz.Crypto.SHA.safeAdd");
		return result;
	},
	
	//-----------------------------------------------------------------------------

	'sha256_array': function(aValue) {
//Clipperz.Profile.start("Clipperz.Crypto.SHA.sha256_array");
		var	result;
		var	message;
		var h0, h1, h2, h3, h4, h5, h6, h7;
		var	k;
		var	messageLength;
		var	messageLengthInBits;
		var	_i, _c;
		var charBits;
		var rotateRight;
		var shiftRight;
		var safeAdd;
		var	bytesPerBlock;
		var currentMessageIndex;

		bytesPerBlock = 512/8;
		rotateRight = Clipperz.Crypto.SHA.rotateRight;
		shiftRight = Clipperz.Crypto.SHA.shiftRight;
		safeAdd = Clipperz.Crypto.SHA.safeAdd;
		
		charBits = 8;
		
		h0 = 0x6a09e667;
		h1 = 0xbb67ae85;
		h2 = 0x3c6ef372;
		h3 = 0xa54ff53a;
		h4 = 0x510e527f;
		h5 = 0x9b05688c;
		h6 = 0x1f83d9ab;
		h7 = 0x5be0cd19;

		k = [	0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
				0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
				0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
				0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
				0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
				0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
				0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
				0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2];

		message = aValue;
		messageLength = message.length;

		//Pre-processing:
		message.push(0x80);	//	append a single "1" bit to  message

		_c = (512 - (((messageLength + 1) * charBits) % 512) - 64) / charBits;
		if (_c < 0) {
			_c = _c + (512 / charBits);
		}

		for (_i=0; _i<_c; _i++) {
			message.push(0x00);			//	append "0" bits until message length ≡ 448 ≡ -64 (mod 512)
		}

		messageLengthInBits = messageLength * charBits;
		message.push(0x00);	//	the 4 most high byte are alway 0 as message length is represented with a 32bit value;
		message.push(0x00);
		message.push(0x00);
		message.push(0x00);
		message.push((messageLengthInBits >> 24)	& 0xff);
		message.push((messageLengthInBits >> 16)	& 0xff);
		message.push((messageLengthInBits >> 8)		& 0xff);
		message.push( messageLengthInBits			& 0xff);

		currentMessageIndex = 0;
		while(currentMessageIndex < message.length) {
			var	w;
			var	a, b, c, d, e, f, g, h;

			w = Array(64);

			_c = 16;
			for (_i=0; _i<_c; _i++) {
				var _j;
				
				_j = currentMessageIndex + _i*4;
				w[_i] = (message[_j] << 24) | (message[_j + 1] << 16) | (message[_j + 2] << 8) | (message[_j + 3] << 0);
			}

			_c = 64;
			for (_i=16; _i<_c; _i++) {
				var	s0, s1;
				
				s0 = (rotateRight(w[_i-15], 7)) ^ (rotateRight(w[_i-15], 18)) ^ (shiftRight(w[_i-15], 3));
				s1 = (rotateRight(w[_i-2], 17)) ^ (rotateRight(w[_i-2], 19)) ^ (shiftRight(w[_i-2], 10));
				w[_i] = safeAdd(w[_i-16], s0, w[_i-7], s1);
			}

			a=h0; b=h1; c=h2; d=h3; e=h4; f=h5; g=h6; h=h7;

			_c = 64;
			for (_i=0; _i<_c; _i++) {
				var s0, s1, ch, maj, t1, t2;
				
				s0  = (rotateRight(a, 2)) ^ (rotateRight(a, 13)) ^ (rotateRight(a, 22));
				maj = (a & b) ^ (a & c) ^ (b & c);
				t2  = safeAdd(s0, maj);
				s1  = (rotateRight(e, 6)) ^ (rotateRight(e, 11)) ^ (rotateRight(e, 25));
				ch  = (e & f) ^ ((~e) & g);
				t1  = safeAdd(h, s1, ch, k[_i], w[_i]);

				h = g;
				g = f;
				f = e;
				e = safeAdd(d, t1);
				d = c;
				c = b;
				b = a;
				a = safeAdd(t1, t2);
			}

		    h0 = safeAdd(h0, a);
		    h1 = safeAdd(h1, b);
		    h2 = safeAdd(h2, c);
		    h3 = safeAdd(h3, d);
		    h4 = safeAdd(h4, e);
		    h5 = safeAdd(h5, f);
		    h6 = safeAdd(h6, g);
		    h7 = safeAdd(h7, h);
			
			currentMessageIndex += bytesPerBlock;
		}

		result = new Array(256/8);
		result[0]  = (h0 >> 24)	& 0xff;
		result[1]  = (h0 >> 16)	& 0xff;
		result[2]  = (h0 >> 8)	& 0xff;
		result[3]  =  h0		& 0xff;

		result[4]  = (h1 >> 24)	& 0xff;
		result[5]  = (h1 >> 16)	& 0xff;
		result[6]  = (h1 >> 8)	& 0xff;
		result[7]  =  h1		& 0xff;

		result[8]  = (h2 >> 24)	& 0xff;
		result[9]  = (h2 >> 16)	& 0xff;
		result[10] = (h2 >> 8)	& 0xff;
		result[11] =  h2		& 0xff;
		
		result[12] = (h3 >> 24)	& 0xff;
		result[13] = (h3 >> 16)	& 0xff;
		result[14] = (h3 >> 8)	& 0xff;
		result[15] =  h3		& 0xff;

		result[16] = (h4 >> 24)	& 0xff;
		result[17] = (h4 >> 16)	& 0xff;
		result[18] = (h4 >> 8)	& 0xff;
		result[19] =  h4		& 0xff;

		result[20] = (h5 >> 24)	& 0xff;
		result[21] = (h5 >> 16)	& 0xff;
		result[22] = (h5 >> 8)	& 0xff;
		result[23] =  h5	 	& 0xff;

		result[24] = (h6 >> 24)	& 0xff;
		result[25] = (h6 >> 16)	& 0xff;
		result[26] = (h6 >> 8)	& 0xff;
		result[27] =  h6		& 0xff;
			
		result[28] = (h7 >> 24)	& 0xff;
		result[29] = (h7 >> 16)	& 0xff;
		result[30] = (h7 >> 8)	& 0xff;
		result[31] =  h7		& 0xff;
		
//Clipperz.Profile.stop("Clipperz.Crypto.SHA.sha256_array");
		return result;
	},

	//-----------------------------------------------------------------------------

	'sha256': function(aValue) {
//Clipperz.Profile.start("Clipperz.Crypto.SHA.sha256");
		var result;
		var resultArray;
		var	valueArray;
		
		valueArray = aValue.arrayValues();
		resultArray = Clipperz.Crypto.SHA.sha256_array(valueArray);
		
		result = new Clipperz.ByteArray(resultArray);
		
//Clipperz.Profile.stop("Clipperz.Crypto.SHA.sha256");
		return result;
	},
	
	//-----------------------------------------------------------------------------

	'sha_d256': function(aValue) {
//Clipperz.Profile.start("Clipperz.Crypto.SHA.sha_d256");
		var result;
		var resultArray;
		var	valueArray;
		
		valueArray = aValue.arrayValues();
		resultArray = Clipperz.Crypto.SHA.sha256_array(valueArray);
		resultArray = Clipperz.Crypto.SHA.sha256_array(resultArray);
		
		result = new Clipperz.ByteArray(resultArray);
		
//Clipperz.Profile.stop("Clipperz.Crypto.SHA.sha256");
		return result;
	},
	
	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});
