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

/* jsCrypto
Core AES

Emily Stark (estark@stanford.edu)
Mike Hamburg (mhamburg@stanford.edu)
Dan Boneh (dabo@cs.stanford.edu)

Symmetric AES in Javascript using precomputed lookup tables for round transformations rather for speed improvements
and code size reduction. Provides authenticated encryption in OCB and CCM modes.
Parts of this code are based on the OpenSSL implementation of AES: http://www.openssl.org

Public domain, 2009.

*/


// CCM mode is the default
var CCM = 1, OCB = 2; 

/* aes object constructor. Takes as arguments:
- 16-byte key, or an array of 4 32-bit words
- Optionally specify a mode (aes.OCB or aes.CCM). Defaults to CCM
- Optionally specify a MAC tag length for integrity. Defaults to 16 bytes
*/
function aes(key, mode, Tlen) {
	// initialize objects for CCM and OCB modes
	this._CCM = new cipherCCM(this);
	this._OCB = new cipherOCB(this);
    
    this._decryptScheduled = false;
	
	if (mode) this._mode = mode;
	else this._mode = OCB;

	// AES round constants
	this._RCON = [
	    [0x00, 0x00, 0x00, 0x00],
	    [0x01, 0x00, 0x00, 0x00],
	    [0x02, 0x00, 0x00, 0x00],
	    [0x04, 0x00, 0x00, 0x00],
	    [0x08, 0x00, 0x00, 0x00],
	    [0x10, 0x00, 0x00, 0x00],
	    [0x20, 0x00, 0x00, 0x00],
	    [0x40, 0x00, 0x00, 0x00],
	    [0x80, 0x00, 0x00, 0x00],
	    [0x1b, 0x00, 0x00, 0x00],
	    [0x36, 0x00, 0x00, 0x00]
	];

	this._key_len = 16;
    if (key.length == 4) {
        this._key = [];
        aes.wordsToBytes(key, this._key);
    }
    else
        this._key = key;
	
	if (Tlen) this._Tlen = Tlen;
	else this._Tlen = 16; // tag length in bytes

	this._nr = 10;
	
	// initialize tables that will be precomputed
	this._SBOX = [];
	this._INV_SBOX = [];
	this._T = new Array(4);
	this._Tin = new Array(4);
	for (var i=0; i < 4; i++) {
		this._T[i] = [];
		this._Tin[i] = [];
	}
	
	this._precompute();	
	this.scheduleEncrypt();

	// initialize encryption and decryption buffers
	this._ctBuffer = [];
	this._ptBuffer = [];
}
	

//////////////////
// KEY SCHEDULING
//////////////////

aes.prototype.scheduleEncrypt = function () {
    this._decryptScheduled = false;

	this._w = new Array(this._nr+1);
	this._w[0] = new Array(4);
	for (var i=0; i < 4; i++)
		this._w[0][i] = this._key[i];
	var temp = new Array(4);
	for (var i=1; i < this._nr+1; i++) {
		temp[0] = this._w[i-1][3];
		this._w[i] = new Array(4);
		temp[0] = (this._T[2][(temp[0]>>>16)&0xff] & 0xff000000) ^ (this._T[3][(temp[0]>>>8)&0xff]&0x00ff0000) ^ (this._T[0][(temp[0]&0xff)]&0x0000ff00) ^ (this._T[1][(temp[0]>>>24)]&0x000000ff) ^ (this._RCON[i][0]<<24);
		this._w[i][0] = this._w[i-1][0] ^ temp[0];
		for (var j=1; j < 4; j++) {
			temp[j] = this._w[i][j-1];
			this._w[i][j] = this._w[i-1][j] ^ temp[j];
		}
	}
};

aes.prototype.scheduleDecrypt = function() {

	if (!this._w) this.scheduleEncrypt();
    this._decryptScheduled = true;
	var temp = [];
	var j = this._w.length-1;
	for (var i=0; i<j; i++) {
		temp[0] = this._w[i][0];
		temp[1] = this._w[i][1];
		temp[2] = this._w[i][2];
		temp[3] = this._w[i][3];
		this._w[i][0] = this._w[j][0];
		this._w[i][1] = this._w[j][1];
		this._w[i][2] = this._w[j][2];
		this._w[i][3] = this._w[j][3];
		this._w[j][0] = temp[0];
		this._w[j][1] = temp[1];
		this._w[j][2] = temp[2];
		this._w[j][3] = temp[3];
		j--;
	}

	var td0 = this._Tin[0], td1 = this._Tin[1], td2 = this._Tin[2], td3 = this._Tin[3], te1 = this._T[1];
	for (var i=1; i < this._w.length-1; i++) {
		this._w[i][0] = td0[te1[(this._w[i][0] >>> 24)       ] & 0xff] ^
			td1[te1[(this._w[i][0] >>> 16) & 0xff] & 0xff] ^
			td2[te1[(this._w[i][0] >>>  8) & 0xff] & 0xff] ^
			td3[te1[(this._w[i][0]      ) & 0xff] & 0xff];
		this._w[i][1] = td0[te1[(this._w[i][1] >>> 24)       ] & 0xff] ^
			td1[te1[(this._w[i][1] >>> 16) & 0xff] & 0xff] ^
			td2[te1[(this._w[i][1] >>>  8) & 0xff] & 0xff] ^
			td3[te1[(this._w[i][1]      ) & 0xff] & 0xff];
		this._w[i][2] = td0[te1[(this._w[i][2] >>> 24)       ] & 0xff] ^
			td1[te1[(this._w[i][2] >>> 16) & 0xff] & 0xff] ^
			td2[te1[(this._w[i][2] >>>  8) & 0xff] & 0xff] ^
			td3[te1[(this._w[i][2]      ) & 0xff] & 0xff];
		this._w[i][3] = td0[te1[(this._w[i][3] >>> 24)       ] & 0xff] ^
			td1[te1[(this._w[i][3] >>> 16) & 0xff] & 0xff] ^
			td2[te1[(this._w[i][3] >>>  8) & 0xff] & 0xff] ^
			td3[te1[(this._w[i][3]      ) & 0xff] & 0xff];
	}
};


/////////////////////////
// ENCRYPTION/DECRYPTION
/////////////////////////


/* Authenticated encryption on a multi-block message in OCB or CCM mode.
iv should be an array of 32-bit words - either 4 words for OCB mode or 1, 2, or 3 words for CCM.
Use a unique IV for every message encrypted.
The plaintext argument will be encrypted and MACed; adata will be sent in plaintext but MACed.
Plaintext and adata are strings.
ciphertext is an array of bytes. tag is an array of 32-bit words.
*/
aes.prototype.encrypt = function(iv, plaintext, ciphertext, adata, tag) {
	var plaintextBytes = [], adataBytes = [];
	aes.asciiToBytes(plaintext, plaintextBytes);
	aes.asciiToBytes(adata, adataBytes);
	
	this._iv = iv;
	if (this._mode == CCM)
		this._CCM.encrypt(plaintextBytes, ciphertext, adataBytes, tag);
	else if (this._mode == OCB) {
		this._OCB.encrypt(plaintextBytes, ciphertext, adataBytes, tag);
	}
    
    // prepend to the ciphertext the length of the iv (in bytes) and the iv
    var ivbytes=[];
    aes.wordsToBytes(iv, ivbytes);
	var ct = [iv.length*4].concat(ivbytes, ciphertext);
	for (var i=0; i < ct.length; i++) ciphertext[i] = ct[i];

	for (var i=0; i < ciphertext.length; i++)
		this._ctBuffer[this._ctBuffer.length] = ciphertext[i];
	
};

/* Authenticated decryption on a multi-block ciphertext in OCB or CCM mode.
ciphertext is an array of bytes. tag is an array of 32-bit words.
plaintext and adata are strings.
*/
aes.prototype.decrypt = function(ciphertext, adata, tag) {
    var ivlen = ciphertext[0];
    var ivbytes = ciphertext.slice(1, ivlen+1);
    var iv = [];
    aes.bytesToWords(ivbytes, iv);
    this._iv = iv;
    var ct = ciphertext.slice(ivlen+1);

	var valid = false;
	var plaintextBytes = [], adataBytes = [];
	aes.asciiToBytes(adata, adataBytes);
	if (this._mode == CCM)
		valid = this._CCM.decrypt(ct, plaintextBytes, adataBytes);
	else if (this._mode == OCB)
		valid = this._OCB.decrypt(ct, plaintextBytes, adataBytes, tag);
	if (valid) {
		var plaintext = aes.bytesToAscii(plaintextBytes);
		for (var i=0; i < plaintext.length; i++)
			this._ptBuffer[this._ptBuffer.length] = plaintext.charAt(i);
		return plaintext;
	}
	return "";
};

// MACs (but doesn't encrypt) data using CMAC (in CCM mode) or PMAC (in OCB mode)
aes.prototype.sign = function(data, tag) {
	if (this._mode == CCM)
		this._CCM.CMAC(data, "", tag, this._Tlen, false);
	else if (this._mode == OCB) {
		this._OCB.PMAC(data, tag);
	}
};

// Verifies a CMAC or PMAC tag
aes.prototype.verify = function(data, tag) {
	var validTag = [];
	if (this._mode == CCM)
		this._CCM.CMAC(data, "", validTag, this._Tlen, false);
	else if (this._mode == OCB) {
		this._OCB.PMAC(data, validTag);
	}
	if (validTag.length != tag.length) return false;
	for (var i=0; i < tag.length; i++) {
		if (tag[i] != validTag[i]) return false;
	}
	return true;
};

/* Encrypts a single block message in AES. Takes the plaintext, an array in which to dump
the ciphertext, and a boolean decrypt argument. If set to true, this function acts as
a decryption function.
block and ciphertext are both arrays of 4 32-bit words.
*/
aes.prototype.encryptBlock = function(block, ciphertext, decrypt) {
    if (!decrypt && this._decryptScheduled) this.scheduleEncrypt();

	// get key schedule
	var w = this._w;
	// load round transformation tables
	var te0, te1, te2, te3;
	if (decrypt) {
		te0 = this._Tin[0];
		te1 = this._Tin[1];
		te2 = this._Tin[2];
		te3 = this._Tin[3];
	} else {
		te0 = this._T[0];
		te1 = this._T[1];
		te2 = this._T[2];
		te3 = this._T[3];
	}
	
	// perform rounds
	var rk = w[0];
	var s0 = block[0] ^ rk[0];
	var s1 = block[1] ^ rk[1];
	var s2 = block[2] ^ rk[2];
	var s3 = block[3] ^ rk[3];
	var t0,t1,t2,t3;
	rk = w[1];
	var order = [];
	for (var round = 1; round < w.length-1; round++) {
		order = [s1, s2, s3, s0];
		if (decrypt) order = [s3, s0, s1, s2];
		t0 = te0[(s0>>>24)] ^ te1[(order[0]>>>16) & 0xff]^ te2[(s2>>>8)&0xff] ^ te3[order[2]&0xff] ^ rk[0];
		t1 = te0[(s1>>>24)] ^ te1[(order[1]>>>16) & 0xff]^ te2[(s3>>>8)&0xff] ^ te3[order[3]&0xff] ^ rk[1];
		t2 = te0[(s2>>>24)] ^ te1[(order[2]>>>16) & 0xff]^ te2[(s0>>>8)&0xff] ^ te3[order[0]&0xff] ^ rk[2];
		t3 = te0[(s3>>>24)] ^ te1[(order[3]>>>16) & 0xff]^ te2[(s1>>>8)&0xff] ^ te3[order[1]&0xff] ^ rk[3];
		s0 = t0;
		s1 = t1;
		s2 = t2;
		s3 = t3;
		rk = w[round+1];
	}
	if (decrypt) {
		s0 = ((this._INV_SBOX[(t0>>>24)])<<24) ^ ((this._INV_SBOX[(t3>>>16)&0xff])<<16) ^ ((this._INV_SBOX[(t2>>>8)&0xff])<<8) ^ (this._INV_SBOX[(t1)&0xff]) ^ rk[0];
		s1 = ((this._INV_SBOX[(t1>>>24)])<<24) ^ ((this._INV_SBOX[(t0>>>16)&0xff])<<16) ^ ((this._INV_SBOX[(t3>>>8)&0xff])<<8) ^ (this._INV_SBOX[(t2)&0xff]) ^ rk[1]
		s2 = ((this._INV_SBOX[(t2>>>24)])<<24) ^ ((this._INV_SBOX[(t1>>>16)&0xff])<<16) ^ ((this._INV_SBOX[(t0>>>8)&0xff])<<8) ^ (this._INV_SBOX[(t3)&0xff]) ^ rk[2];
		s3 = (this._INV_SBOX[(t3>>>24)]<<24) ^ (this._INV_SBOX[(t2>>>16)&0xff]<<16) ^ (this._INV_SBOX[(t1>>>8)&0xff]<<8) ^ (this._INV_SBOX[(t0)&0xff]) ^ rk[3];
	} else {
		s0 = (te2[t0>>>24]&0xff000000) ^ (te3[(t1>>>16)&0xff]&0x00ff0000) ^ (te0[(t2>>>8)&0xff]&0x0000ff00) ^ (te1[(t3)&0xff]&0x000000ff) ^ rk[0];
		s1 = (te2[t1>>>24]&0xff000000) ^ (te3[(t2>>>16)&0xff]&0x00ff0000) ^ (te0[(t3>>>8)&0xff]&0x0000ff00) ^ (te1[(t0)&0xff]&0x000000ff) ^ rk[1];
		s2 = (te2[t2>>>24]&0xff000000) ^ (te3[(t3>>>16)&0xff]&0x00ff0000) ^ (te0[(t0>>>8)&0xff]&0x0000ff00) ^ (te1[(t1)&0xff]&0x000000ff) ^ rk[2];
		s3 = (te2[t3>>>24]&0xff000000) ^ (te3[(t0>>>16)&0xff]&0x00ff0000) ^ (te0[(t1>>>8)&0xff]&0x0000ff00) ^ (te1[(t2)&0xff]&0x000000ff) ^ rk[3];
	}
	ciphertext[0] = s0;
	ciphertext[1] = s1;
	ciphertext[2] = s2;
	ciphertext[3] = s3;
};

// As above, block and plaintext are arrays of 4 32-bit words.
aes.prototype.decryptBlock = function(block, plaintext) {
    if (!this._decryptScheduled) this.scheduleDecrypt();

	this.encryptBlock(block, plaintext, true);
};


////////////////////
// HELPER FUNCTIONS
////////////////////

aes._hex = function(n) {
  var out = "",i,digits="0123456789ABCDEF";
  for (i=0; i<8; i++) {
    var digit = n&0xF;
    out = digits.substring(digit,digit+1) + out;
    n = n >>> 4;
  }
  return out;
}

aes._hexall = function(nn) {
  var out = "",i;
  for (i=0;i<nn.length;i++) {
    if (i%4 == 0) out+= "<br/>\n";
    else if (i) out += " ";
    out += aes._hex(nn[i]);
  }
  return out;
}

aes.bytesToAscii = function(bytes) {
	var ascii = "";
	var len = bytes.length;
	for (var i=0; i < len; i++) {
		ascii = ascii + String.fromCharCode(bytes[i]);
	}
	return ascii;
};

aes.asciiToBytes = function(ascii, bytes) {
	var len = ascii.length;
	for (var i=0; i < len; i++)
		bytes[i] = ascii.charCodeAt(i);
};

aes.wordsToBytes = function(words, bytes) {
	var bitmask = 1;
	for (var i=0; i < 7; i++) bitmask = (bitmask << 1) | 1;
	for (var i=0; i < words.length; i++) {
		var bstart = i*4;
		for (var j=0; j < 4; j++) {
			bytes[bstart+j] = (words[i] & (bitmask << (8*(3-j)))) >>> (8*(3-j));
		}
	}
};

aes.bytesToWords = function(bytes, words) {
    var paddedBytes = bytes.slice();
    while (paddedBytes.length % 4 != 0) paddedBytes.push(0);
	var num_words = Math.floor(paddedBytes.length/4);
	for (var j=0; j < num_words; j++)
		words[j] = ((paddedBytes[(j<<2)+3]) | (paddedBytes[(j<<2)+2] << 8) | (paddedBytes[(j<<2)+1] << 16) | (paddedBytes[j<<2] << 24));
};


///////////////////////////////////////
// KEY DERIVATION
//////////////////////////////////////

// password is a string, presumably a password entered by the user.
// salt is eight random bytes associated with each user
// This function returns an array of bytes of length 16
function generateKey(password, salt) {
	var pwBytes = [];
	aes.asciiToBytes(password, pwBytes);
	var pwWords = [], saltWords = [];
	aes.bytesToWords(pwBytes, pwWords);
	aes.bytesToWords(salt, saltWords);
	
	var iterations = 1000;
	
	var derivedKey = [];
	var blockIndex = 1;
	
	var xorHashes = function(h1, h2) {
		var xor = [];
		var i;
		for (i=0; i < h1.length; i++) xor.push(h1[i] ^ h2[i]);
        return xor;
	};
	
	while (derivedKey.length < 16) {
		var hashBytes = pwWords.concat(saltWords);
		hashBytes.push(blockIndex);
		var T = SHA256.hash_words_big_endian(hashBytes);
		var u = T;
		for (var i=2; i < iterations; i++) {
			var hash = SHA256.hash_words_big_endian(pwWords.concat(u));
			u = xorHashes(T, hash);
		}
		var block = [];
		aes.wordsToBytes(T, block);
		for (var i=0; i < block.length; i++) derivedKey.push(block[i]);
	}
	
	if (derivedKey.length > 16) derivedKey.length = 16;
	return derivedKey;
}

///////////////////////////////////////
// ROUND TRANSFORMATION PRECOMPUTATION
///////////////////////////////////////


// Precomputation code by Mike Hamburg

aes.prototype._precompute = function() {
  var x,xi,sx,tx,tisx,i;
  var d=[];

  /* compute double table */
  for (x=0;x<256;x++) {
    d[x]= x&128 ? x<<1 ^ 0x11b : x<<1;
    //d[x] = x<<1 ^ (x>>7)*0x11b; //but I think that's less clear.
  }

  /* Compute the round tables.
   * 
   * We'll need access to x and x^-1, which we'll get by walking
   * GF(2^8) as generated by (82,5).
   */
  for(x=xi=0;;) {
    // compute sx := sbox(x)
    sx = xi^ xi<<1 ^ xi<<2 ^ xi<<3 ^ xi<<4;
    sx = sx>>8 ^ sx&0xFF ^ 0x63;

    var dsx = d[sx], x2=d[x],x4=d[x2],x8=d[x4];

    // te(x) = rotations of (2,1,1,3) * sx
    tx   = dsx<<24 ^ sx<<16 ^ sx<<8 ^ sx^dsx;

    // similarly, td(sx) = (E,9,D,B) * x
    tisx = (x8^x4^x2) <<24 ^
           (x8^x    ) <<16 ^
           (x8^x4^x ) << 8 ^
           (x8^x2^x );

    // This can be done by multiplication instead but I think that's less clear
    // tisx = x8*0x1010101 ^ x4*0x1000100 ^ x2*0x1000001 ^ x*0x10101;
    // tx = dsx*0x1000001^sx*0x10101;

    // rotate and load
    for (i=0;i<4;i++) {
      this._T[i][x]  = tx;
      this._Tin[i][sx] = tisx;
      tx   =   tx<<24 | tx>>>8;
      tisx = tisx<<24 | tisx>>>8;
    }

    // te[4] is the sbox; td[4] is its inverse
    this._SBOX[ x] = sx;
    this._INV_SBOX[sx] =  x;
    

    // wonky iteration goes through 0
    if (x==5) {
      break;
    } else if (x) {
      x   = x2^d[d[d[x8^x2]]]; // x  *= 82 = 0b1010010
      xi ^= d[d[xi]];          // xi *= 5  = 0b101
    } else {
      x=xi=1;
    }
  }

  // We computed the arrays out of order.  On Firefox, this matters.
  // Compact them.
  for (i=0; i<4; i++) {
    this._T[i] = this._T[i].slice(0);
    this._Tin[i] = this._Tin[i].slice(0);
  }
  this._SBOX = this._SBOX.slice(0);
  this._INV_SBOX = this._INV_SBOX.slice(0);


};





/* jsCrypto
CCM mode

Emily Stark (estark@stanford.edu)
Mike Hamburg (mhamburg@stanford.edu)
Dan Boneh (dabo@cs.stanford.edu)

CCM mode for authenticated encryption of multiple 16-byte blocks. Uses AES as core cipher.

Public domain, 2009.

*/

// Constructor takes an aes object as its core cipher
function cipherCCM(cipher) {
	this._cipher = cipher;
}

/* Formats plaintext and adata for MACing and encryption.
adata and plaintext are arrays of bytes, B will be an array of arrays of 16 bytes
Tlen specifies the number of bytes in the tag.
Formatted according to the CCM specification.
 */
cipherCCM.prototype._formatInput = function(adata, plaintext, Tlen, B) {
	// compute B[0]
	var flags, nbytes=[];
	aes.wordsToBytes(this._cipher._iv, nbytes);
	if (adata) flags = 0x01<<6;
	else flags = 0x00<<6;
	flags = flags | (((Tlen-2)/2)<<3); // (t-2)/2
	var q = 15-this._cipher._iv.length*4;
	flags = flags | (q-1);
	B[0] = new Array(16);
	B[0][0] = flags;
	for (var i=1; i <= 15-q; i++) B[0][i] = nbytes[i-1];
	var Q = plaintext.length;
	
	// make some bitmasks
	var bitmask = 1;
	for (var i=0; i < 7; i++) bitmask = (bitmask<<1) | 1;
	for (var i=15; i > 15-q; i--) {
		B[0][i] = Q & bitmask;
		Q = Q>>>8;
	}

	// compute the blocks which identify adata
	if (adata) {
		var a = adata.length, Bind=1, BIind = 0, aind=0;
		B[1] = new Array(16);
		if (a < (2<<16 - 2<<8)) {
			B[1][0] = a>>>8;
			B[1][1] = a & bitmask;
			BIind = 2;
		} else if (a < (2<<32)) {
			B[1][0] = 0xff;
			B[1][1] = 0xfe;
			for (var i=5; i >= 0; i--) {
				B[1][2+i] = a & bitmask;
				a = a>>>8;
			}
			BIind=8;
		} else {
			B[1][0] = 0xff;
			B[1][0] = 0xff;
			for (i=9; i >= 0; i--) {
				B[1][2+i] = a & bitmask;
				a = a >>> 8;
			}
			BIind = 12;
		}
	}

	while (aind < adata.length) {
		B[Bind][BIind] = adata[aind];
		aind++;
		if (BIind == 15) {
			Bind++;
			BIind = 0;
			if (aind != adata.length) B[Bind] = new Array(16);
		} else BIind++;
	}
	if (BIind != 0) {
		while (BIind <= 15) {
			B[Bind][BIind] = 0x00;
			BIind++;
		}
	}
	
	Bind++;
	BIind=0;
	B[Bind] = new Array(16);

	// compute the payload blocks
	var pind = 0;
	while (pind < plaintext.length) {
		B[Bind][BIind] = plaintext[pind];
		pind++;
		if (BIind == 15) {
			Bind++;
			BIind = 0;
			if (pind != plaintext.length) B[Bind] = new Array(16);
		} else BIind++;
	}
	if (BIind != 0) {
		while (BIind <= 15) {
			B[Bind][BIind] = 0x00;
			BIind++;
		}
	}

};

/* Generate the blocks that will be used as counters.
ctr will be an array of m+1 arrays of 16 bytes. */
cipherCCM.prototype._generateCtrBlocks = function(m, ctr) {
	var nbytes = [];
	aes.wordsToBytes(this._cipher._iv, nbytes);
	var flags = 15 - (this._cipher._iv.length*4) - 1;
	var bitmask = 1;
	for (var i=0; i < 7; i++) bitmask = (bitmask<<1) | 1;
	for (var i=0; i <= m; i++) {
		ctr[i] = new Array(16);
		ctr[i][0] = flags;
		for (var j=0; j < nbytes.length; j++) {
			ctr[i][j+1] = nbytes[j];
		}
		for (var j=15; j > nbytes.length; j--) {
			ctr[i][j] = (i>>>(8*(15-j))) & bitmask;
		}
	}
		
};


/* CBC-MAC adata and plaintext, and store the tag in tag.
adata and plaintext are arrays of bytes
tag will be an array of Tlen/4 32-bit words
Tlen is an integer divisible by 4 that specifies the number of bytes in the tag.
*/
cipherCCM.prototype.CBCMAC = function(adata, plaintext, tag, Tlen, formatInput) {
	var B = [];
	if (formatInput)
		this._formatInput(adata,plaintext,Tlen,B);
	else {
		var Sind = 0, SIind = 0, aind = 0, alen = adata.length;
		B[0] = [];
		while (aind < alen) {
			B[Sind][SIind] = adata[aind];
			SIind++;
			if (SIind == 16) {
				SIind = 0;
				Sind++;
				if (aind != alen-1) B[Sind] = [];
			}
			aind++;
		}
	}
	var words = [];
	var Yprev = [], Y = [];
	aes.bytesToWords(B[0],words);
	this._cipher.encryptBlock(words, Y);
	var r = B.length, t = new Array(4);

	for (var i=1; i < r; i++) {
		for (var j=0; j < 4; j++) {
			var bstart = j*4;
			t[j] = Y[j] ^ ((B[i][bstart++]<<24) | (B[i][bstart++]<<16) | (B[i][bstart++]<<8) | (B[i][bstart++]));
			Yprev[j] = Y[j];
		}
		this._cipher.encryptBlock(t, Y);
	}
	for (var i=0; i < Tlen/4; i++)
		tag[i] = Y[i];
};


/* Provides authenticated encryption using CBCMAC and CTR-mode encryption on plaintext.
adata is MACed but not encrypted.
plaintext, adata, and tag are arrays of bytes
Tlen is the number of bytes in the tag
ciphertext will be an array of bytes. */
cipherCCM.prototype.encrypt = function(plaintext, ciphertext, adata, tag) {
	var Tlen = this._cipher._Tlen;
	this.CBCMAC(adata, plaintext, tag, Tlen, true);
	var ctr = [], m = Math.ceil(plaintext.length/16);
	this._generateCtrBlocks(m, ctr);
	var cblocks = [], S=[], t = new Array(4);
	for (var i=0; i <= m; i++) {
		S[i] = new Array(16);
		aes.bytesToWords(ctr[i], cblocks);
		this._cipher.encryptBlock(cblocks, t);
		aes.wordsToBytes(t, S[i]);
	}
	var Sind = 1, SIind = 0;
	for (var i=0; i < plaintext.length; i++) {
		ciphertext[i] = plaintext[i] ^ S[Sind][SIind];
		SIind++;
		if (SIind == 16) {
			Sind++;
			SIind = 0;
		}
	}
	var tbytes = [];
	aes.wordsToBytes(tag, tbytes);
	var cstart = plaintext.length;
	for (var i=0; i < Tlen; i++)
		ciphertext[cstart+i] = tbytes[i] ^ S[0][i];
};


/* Decrypt and verify the MAC on ciphertext and adata. The integrity of adata is verified, but isn't decrypted.
ciphertext, adata are arrays of bytes
plaintext will be an array of bytes
Returns true if tag is valid, false otherwise.
*/
cipherCCM.prototype.decrypt = function(ciphertext, plaintext, adata) {
	var Tlen = this._cipher._Tlen;
	if (ciphertext.length <= Tlen) return false;
	var ctr = [], tag = new Array(Tlen), m = Math.ceil(ciphertext.length/16);
	this._generateCtrBlocks(m, ctr);
	var S = [], t = new Array(4), cblocks=[];

	for (var i=0; i <= m; i++) {
		S[i] = new Array(16);
		aes.bytesToWords(ctr[i], cblocks);
		this._cipher.encryptBlock(cblocks, t);
		aes.wordsToBytes(t, S[i]);
	}

	var Sind = 1, SIind = 0;
	for (var i=0; i < (ciphertext.length-Tlen); i++) {
		plaintext[i] = ciphertext[i] ^ S[Sind][SIind];
		SIind++;
		if (SIind == 16) {
			SIind = 0;
			Sind++;
		}
	}
	
	for (var i=0; i < Tlen; i++)
		tag[i] = ciphertext[ciphertext.length-Tlen+i] ^ S[0][i];

	// verify integrity
	var validTag = [], vtbytes = [];
	this.CBCMAC(adata, plaintext, validTag, Tlen, true);
	aes.wordsToBytes(validTag, vtbytes);
	for (var i=0; i < Tlen; i++) {
		if (vtbytes[i] != tag[i])
			return false;
	}
	return true;
		
};

// Generate subkeys according to the CCM specification. */
cipherCCM.prototype._generateSubkeys = function(k1,k2) {
	var t = [0x00000000,0x00000000,0x00000000,0x00000000], t2 = new Array(4);
	this._cipher.encryptBlock(t, t2);
	for (var i=0; i < 3; i++)
		k1[i] = t2[i]<<1 | t2[i+1]>>>31;
	k1[3] = t2[3]<<1;
	if (t2[0]>>>31 != 0)
		k1[3] = k1[3] ^ 135;
	for (var i=0; i < 3; i++)
		k2[i] = k1[i]<<1 | k1[i+1]>>>31;
	k2[3] = k1[3]<<1;
	if (k1[0]>>>31 != 0)
		k2[3] = k2[3] ^ 135;	
};


/* CMAC used for integrity only (no encryption). */
cipherCCM.prototype.CMAC = function(adata, plaintext, tag, Tlen, formatInput) {
	var B = [], t = new Array(4); // will be an array of arrays of 16 bytes
	if (formatInput)
		this._formatInput(adata,plaintext,Tlen,B);
	else {
		var Sind = 0, SIind = 0, aind = 0, alen = adata.length;
		B[0] = [];
		while (aind < alen) {
			B[Sind][SIind] = adata[aind];
			SIind++;
			if (SIind == 16) {
				SIind = 0;
				Sind++;
				if (aind != alen-1) B[Sind] = [];
			}
			aind++;
		}
	}
	var k1 = new Array(4), k2 = new Array(4);
	this._generateSubkeys(k1,k2);
	var last = B.length-1, kbytes = [];
	if (alen % 16 == 0) {
		aes.wordsToBytes(k1, kbytes);
	} else {
		aes.wordsToBytes(k2, kbytes);
		B[last][B[last].length] = 1<<7;
		while (B[last].length % 16 != 0)
			B[last][B[last].length] = 0x00;
	}
	for (var i=0; i < 16; i++) B[last][i] = B[last][i] ^ kbytes[i];
	var C = [0x00000000,0x00000000,0x00000000,0x00000000], Cprev = new Array(4), words = new Array(4);
	for (var i=0; i < B.length; i++) {
		aes.bytesToWords(B[i], words);
		for (var j=0; j < 4; j++) {
			Cprev[j] = C[j];
			t[j] = C[j] ^ words[j];
		}
		this._cipher.encryptBlock(t, C);
	}
	var cbytes=[];
	aes.wordsToBytes(C, cbytes);
	for (var i=0; i < Tlen; i++)
		tag[i] = cbytes[i];	
		
};



/* jsCrypto
OCB mode

Emily Stark (estark@stanford.edu)
Mike Hamburg (mhamburg@stanford.edu)
Dan Boneh (dabo@cs.stanford.edu)

OCB mode for authenticated encryption of multiple 16-byte blocks. Uses AES as core cipher.

Public domain, 2009.
*/

/* Constructor takes an aes object as the core cipher. */
function cipherOCB(cipher) {
	this._cipher = cipher;
}


/* Provides integrity only, no encryption.
header is an array of bytes, tag will be an array of 4 32-bit words */
cipherOCB.prototype.PMAC = function(header, tag) {
	var carry, t = new Array(4), t2 = new Array(4), Checksum = [0x00000000,0x00000000,0x00000000,0x00000000];
	var Offset = new Array(4);
	this._cipher.encryptBlock(Checksum, Offset);
	this._times2(t, Offset);
	for (var i=0; i < 4; i++) Offset[i] = t[i] ^ Offset[i];
	this._times2(t, Offset);
	for (var i=0; i < 4; i++) Offset[i] = t[i] ^ Offset[i];

	// accumulate all but the last block
	var num_blocks = Math.floor((header.length-1)/16);
	for (var i=0; i < num_blocks; i++) {
		this._times2(Offset,Offset);
		var bstart = i*16; // start-of-block index
		for (var j=0; j < 4; j++)
			t[j] = Offset[j] ^ ((header[bstart+(j<<2)+3]) | (header[bstart+(j<<2)+2] << 8) | (header[bstart+(j<<2)+1] << 16) | (header[bstart+(j<<2)] << 24));
		this._cipher.encryptBlock(t, t2);
		for (var j=0; j < 4; j++) Checksum[j] = Checksum[j] ^ t2[j];		
	}

	// accumulate the last block
	this._times2(Offset,Offset);
	
	if (header.length%16 == 0) {
		var bstart = header.length-16;
		for (var j=0; j < 4; j++)
			Checksum[j] = Checksum[j] ^ ((header[bstart+(j<<2)+3]) | (header[bstart+(j<<2)+2] << 8) | (header[bstart+(j<<2)+1] << 16) | (header[bstart+(j<<2)] << 24));
		this._times2(t, Offset);
		for (var i=0; i < 4; i++) Offset[i] = Offset[i] ^ t[i];
	} else {
		var block_bytes = [], block = new Array(4), len = header.length, ind=0;
		for (var i=(header.length-(header.length%16)); i < len; i++) {
			block_bytes[ind] = header[i];
			ind++;
		}
		block_bytes[ind] = 0x80;
		ind++;
		while (block_bytes.length%16 != 0) {
			block_bytes[ind] = 0x00;
			ind++;
		}
		aes.bytesToWords(block_bytes,block);
		for (var j=0; j < 4; j++) {
			var bstart = 4*j;
			Checksum[j] = Checksum[j] ^ ((block_bytes[bstart++]<<24) | (block_bytes[bstart++]<<16) | (block_bytes[bstart++]<<8) | (block_bytes[bstart++]));
		}
		this._times2(t, Offset);
		for (var i=0; i < 4; i++) Offset[i] = Offset[i] ^ t[i];
		this._times2(t, Offset);
		for (var i=0; i < 4; i++) Offset[i] = Offset[i] ^ t[i];
	}

	// compute result
	for (var i=0; i < 4; i++) t[i] = Offset[i] ^ Checksum[i];
	this._cipher.encryptBlock(t, tag);
};


/* Encrypts and MACs plaintext, only MACS header.
plaintext, ciphertext and header are arrays of bytes. tag will be an array of 4 32-bit words. */
cipherOCB.prototype.encrypt = function(plaintext, ciphertext, header, tag) {
	var Checksum = [0x00000000,0x00000000,0x00000000,0x00000000];
	var t = [0x00000000,0x00000000,0x00000000,0x00000000], t2 = new Array(4);
	var Offset = new Array(4);
	this._cipher.encryptBlock(this._cipher._iv, Offset);
	var cbytes = [];

	// encrypt and accumulate all but last block
	var num_blocks = Math.floor((plaintext.length-1)/16), bstart=0, block = new Array(4);
	for (var i=0; i < num_blocks; i++) {
		this._times2(Offset,Offset);
		bstart = 16*i;
		for (var j=0; j < 4; j++)
			block[j] = ((plaintext[bstart+(j<<2)+3]) | (plaintext[bstart+(j<<2)+2] << 8) | (plaintext[bstart+(j<<2)+1] << 16) | (plaintext[bstart+(j<<2)] << 24));
		for (var j=0; j < 4; j++)
			t[j] = Offset[j] ^ block[j];
		this._cipher.encryptBlock(t,t2);
		for (var j=0; j < 4; j++) t[j] = Offset[j] ^ t2[j];
		aes.wordsToBytes(t, cbytes);
		for (var j=0; j < 16; j++) ciphertext[bstart+j] = cbytes[j];
		for (var j=0; j < 4; j++) Checksum[j] = Checksum[j] ^ block[j];
	}

	// encrypt and accumulate last block
	var num_bytes = plaintext.length%16;
	if ((num_bytes == 0) && (plaintext.length > 0)) num_bytes=16;
	this._times2(Offset,Offset);
	t = [0x00000000,0x00000000,0x00000000,0x00000000];
	t[3] = num_bytes*8;
	for (var i=0; i < 4; i++) t[i] = Offset[i] ^ t[i];
	var Pad = new Array(4);
	this._cipher.encryptBlock(t, Pad);
	var pad_bytes = new Array(16);
	aes.wordsToBytes(Pad, pad_bytes);
	var tempbytes = [];
	bstart = plaintext.length-num_bytes;
	for (var i=0; i < num_bytes; i++) {
		ciphertext[bstart+i] = plaintext[bstart+i] ^ pad_bytes[i];
		tempbytes[tempbytes.length] = plaintext[bstart+i];
	}
	for (var i=num_bytes; i < 16; i++)
		tempbytes[tempbytes.length] = pad_bytes[i];
	aes.bytesToWords(tempbytes, t);
	for (var i=0; i < 4; i++) Checksum[i] = Checksum[i] ^ t[i];

	// compute authentication tag
	this._times2(t,Offset);
	for (var i=0; i < 4; i++) {
		Offset[i] = t[i] ^ Offset[i];
		t[i] = Checksum[i] ^ Offset[i];
	}
	this._cipher.encryptBlock(t,t2);
	if (header.length > 0) {
		this.PMAC(header, t);
		for (var i=0; i < 4; i++) tag[i] = t[i] ^ t2[i];
	} else {
		for (var i=0; i < 4; i++) tag[i] = t2[i];
	}
};


/* Decrypts and verifies integrity of ciphertext, only verifies integrity of header.
ciphertext, plaintext, and header are arrays of bytes. tag is an array of 4 32-bit words.
Returns true if tag is valid, false otherwise. */
cipherOCB.prototype.decrypt = function(ciphertext, plaintext, header, tag) {
	var Offset = new Array(4), Checksum = [0x00000000,0x00000000,0x00000000,0x00000000];
	this._cipher.encryptBlock(this._cipher._iv, Offset);

	var t = new Array(4), t2 = new Array(4), block = new Array(4);

	// decrypt and accumulate first m-1 blocks
	var num_blocks = Math.floor((ciphertext.length-1)/16);
	var bstart = 0, pbytes = new Array(16);
	this._cipher.scheduleDecrypt();
	for (var i=0; i < num_blocks; i++) {
		this._times2(Offset,Offset);
		bstart = i*16;
		for (var j=0; j < 4; j++)
			t[j] = Offset[j] ^ ((ciphertext[bstart+(j<<2)+3]) | (ciphertext[bstart+(j<<2)+2] << 8) | (ciphertext[bstart+(j<<2)+1] << 16) | (ciphertext[bstart+(j<<2)] << 24));
		this._cipher.decryptBlock(t,t2);
		for (var j=0; j < 4; j++) {
			block[j] = Offset[j] ^ t2[j];
			Checksum[j] = block[j] ^ Checksum[j];
		}
		aes.wordsToBytes(block,pbytes);
		for (var j=0; j < 16; j++)
			plaintext[bstart+j] = pbytes[j];
	}

	// decrypt and accumulate final block
	var Pad = new Array(4), padbytes=[];
	this._cipher.scheduleEncrypt()
	this._times2(Offset,Offset);
	var num_bytes = ciphertext.length%16;
	if ((num_bytes == 0) && (ciphertext.length > 0)) num_bytes=16;
	t = [0x00000000,0x00000000,0x00000000,0x00000000];
	t[3] = num_bytes*8;
	for (var i=0; i < 4; i++) t[i] = t[i] ^ Offset[i]
	this._cipher.encryptBlock(t,Pad);
	aes.wordsToBytes(Pad, padbytes);
	bstart = ciphertext.length - num_bytes;
	for (var i=0; i < num_bytes; i++) {
		plaintext[bstart+i] = ciphertext[bstart+i] ^ padbytes[i];
		t[i] = plaintext[bstart+i];
	}
	for (var i = num_bytes; i < 16; i++)
		t[i] = padbytes[i];
	aes.bytesToWords(t,t2);
	for (var i=0; i < 4; i++) Checksum[i] = Checksum[i] ^ t2[i];

	// compute valid authentication tag
	this._times2(t, Offset);
	for (var i=0; i < 4; i++) {
		Offset[i] = Offset[i] ^ t[i];
		t[i] = Offset[i] ^ Checksum[i];
	}
	var validTag = new Array(4);
	this._cipher.encryptBlock(t,validTag);
	t = new Array(4);
	if (header.length > 0) {
		this.PMAC(header, t);
		for (var i=0; i < 4; i++) validTag[i] = validTag[i] ^ t[i];
	}
	// compute results
	for (var i=0; i < 4; i++) {
		if (aes._hex(tag[i]) != aes._hex(validTag[i])) {
			return false;
		}
	}
	return true;
};



cipherOCB.prototype._times2 = function(dst, src) {
	var carry = src[0]>>>31;
	for (var i=0; i < 3; i++)
		dst[i] = (src[i]<<1) | (src[i+1]>>>31);
	dst[3] = (src[3]<<1) ^ (carry * 0x87);
};








/* 
jsCrypto

sha256.js
Mike Hamburg, 2008.  Public domain.
 */


function SHA256() {
  if (!this.k[0])
    this.precompute();
  this.initialize();
}

SHA256.prototype = {
  /*
  init:[0x6a09e667,0xbb67ae85,0x3c6ef372,0xa54ff53a,0x510e527f,0x9b05688c,0x1f83d9ab,0x5be0cd19],

  k:[0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
     0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
     0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
     0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
     0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
     0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
     0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
     0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2],
  */

  init:[], k:[],

  precompute: function() {
    var p=2,i=0,j;

    function frac(x) { return (x-Math.floor(x)) * 4294967296 | 0 }

    outer: for (;i<64;p++) {
      for (j=2;j*j<=p;j++)
	if (p % j == 0)
	  continue outer;

      if (i<8) this.init[i] = frac(Math.pow(p,1/2));
      this.k[i] = frac(Math.pow(p,1/3));
      i++;
    }
  },

  initialize:function() {
    this.h = this.init.slice(0);
    this.word_buffer   = [];
    this.bit_buffer    = 0;
    this.bits_buffered = 0; 
    this.length        = 0;
    this.length_upper  = 0;
  },

  // one cycle of SHA256
  block:function(words) {
    var w=words.slice(0),i,h=this.h,tmp,k=this.k;

    var h0=h[0],h1=h[1],h2=h[2],h3=h[3],h4=h[4],h5=h[5],h6=h[6],h7=h[7];

    for (i=0;i<64;i++) {
      if (i<16) {
	tmp=w[i];
      } else {
        var a=w[(i+1)&15], b=w[(i+14)&15];
        tmp=w[i&15]=((a>>>7^a>>>18^a>>>3^a<<25^a<<14) + (b>>>17^b>>>19^b>>>10^b<<15^b<<13) + w[i&15] + w[(i+9)&15]) | 0;
      }
      
      tmp += h7 + (h4>>>6^h4>>>11^h4>>>25^h4<<26^h4<<21^h4<<7) + (h6 ^ h4&(h5^h6)) + k[i];
      
      h7=h6; h6=h5; h5=h4;
      h4 = h3 + tmp | 0;

      h3=h2; h2=h1; h1=h0;

      h0 = (tmp + ((h1&h2)^(h3&(h1^h2))) + (h1>>>2^h1>>>13^h1>>>22^h1<<30^h1<<19^h1<<10)) | 0;
    }

    h[0]+=h0; h[1]+=h1; h[2]+=h2; h[3]+=h3;
    h[4]+=h4; h[5]+=h5; h[6]+=h6; h[7]+=h7;
  },

  update_word_big_endian:function(word) {
    var bb;
    if ((bb = this.bits_buffered)) {
      this.word_buffer.push(word>>>(32-bb) ^ this.bit_buffer);
      this.bit_buffer = word << bb;
    } else {
      this.word_buffer.push(word);
    }
    this.length += 32;
    if (this.length == 0) this.length_upper ++; // mmhm..
    if (this.word_buffer.length == 16) {
      this.block(this.word_buffer);
      this.word_buffer = [];
    }
  },

  update_word_little_endian:function(word) {
    word = word >>> 16 ^ word << 16;
    word = ((word>>>8) & 0xFF00FF) ^ ((word<<8) & 0xFF00FF00);
    this.update_word_big_endian(word);
  },

  update_words_big_endian: function(words) { 
    for (var i=0; i<words.length; i++) this.update_word_big_endian(words[i]);
  },

  update_words_little_endian: function(words) { 
    for (var i=0; i<words.length; i++) this.update_word_little_endian(words[i]);
  },

  update_byte:function(byte) {
    this.bit_buffer ^= (byte & 0xff) << (24 - (this.bits_buffered));
    this.bits_buffered += 8;
    if (this.bits_buffered == 32) {
      this.bits_buffered = 0; 
      this.update_word_big_endian(this.bit_buffer);
      this.bit_buffer = 0;
    }
  },

  update_string:function(string) {
    throw "not yet implemented";
  },

  finalize:function() {
    var i, wb = this.word_buffer;

    wb.push(this.bit_buffer ^ (0x1 << (31 - this.bits_buffered)));
    for (i = (wb.length + 2) & 15; i<16; i++) {
      wb.push(0);
    }
    
    wb.push(this.length_upper);
    wb.push(this.length + this.bits_buffered);

    this.block(wb.slice(0,16));
    if (wb.length > 16) {
      this.block(wb.slice(0,16));
    }

    var h = this.h;
    this.initialize();
    return h;
  }
}

SHA256.hash_words_big_endian = function(words) {
  var s = new SHA256();
  for (var i=0; i<=words.length-16; i+=16) {
    s.block(words.slice(i,i+16));
  }
  s.length = i << 5; // so don't pass this function more than 128M words
  if (i<words.length)
    s.update_words_little_endian(words.slice(i));
  return s.finalize();
}

SHA256.hash_words_little_endian = function(words) {
  var w = words.slice(0);
  for (var i=0; i<w.length; i++) {
    w[i] = w[i] >>> 16 ^ w[i] << 16;
    w[i] = ((w[i]>>>8) & 0xFF00FF) ^ ((w[i]<<8) & 0xFF00FF00);    
  }
  return SHA256.hash_words_big_endian(w);
}







/* 
 
 jsCrypto
 
 * Random.js -- cryptographic random number generator
 * Mike Hamburg, 2008.  Public domain.
 *
 * This generator uses a modified version of Fortuna.  Fortuna has
 * excellent resilience to compromise, relies on a state file, and is
 * intended to run for a long time.  As such, it does not need an
 * entropy estimator.  Unfortunately, Fortuna's startup in low-entropy
 * conditions leaves much to be desired.
 *
 * This generator features the following modifications.  First, the
 * generator does not create the n-th entropy pool until it exhausts
 * the n-1-st.  This means that entropy doesn't get "stuck" in pools
 * 10-31, which will never be used on a typical webpage.  It also
 * means that the entropy will all go into a single pool until the
 * generator is seeded.
 *
 * Second, there is a very crude entropy estimator.  The primary goal
 * of this estimator is to prevent the generator from being used in
 * low-entropy situations.  Corresponding to this entropy estimator,
 * there is a "paranoia control".  This controls how many bits of
 * estimated entropy must be present before the generator is used.
 * The generator cannot have more than 256 bits of actual entropy in
 * the main pool; rather, the paranoia control is designed to deal
 * with the fact that the entropy estimator is probably horrible.
 *
 * Third, the "statefile" is optional and stored in a cookie.  As
 * such, it is not protected from multiple simultaneous usage, and so
 * is treated conservatively.
 */

Random = {
	/* public */
NOT_READY: 0,
READY: 1,
REQUIRES_RESEED: 2,
	
	/* generate one random word */
random_word: function(paranoia) {
    return this.random_words(1, paranoia)[0];
},
	
	/* generate nwords random words, and return them in an array */
random_words: function(nwords, paranoia) {
    var out = [], i, readiness = this.is_ready(paranoia);
	
    if (readiness == this.NOT_READY)
		throw("Random: generator isn't seeded!");    
	
    else if (readiness && this.REQUIRES_RESEED)
		this._reseed_from_pools(!(readiness & this.READY));
	
    for (i=0; i<nwords; i+= 4) {
		if ((i+1) % this._max_words_per_burst == 0)
			this._gate();
		
		var g = this._gen_4_words();
		out.push(g[0],g[1],g[2],g[3]);
    }
    this._gate();
	
    return out.slice(0,nwords);
},
	
set_default_paranoia: function(paranoia) {
    this._default_paranoia = paranoia;
},
	
	/* Add entropy to the pools.  Pass data as an array, number or
	 * string.  Pass estimated_entropy in bits.  Pass the source as a
	 * number or string.
	 */
add_entropy: function(data, estimated_entropy, source) {
    source = source || "user";
	
    var id = this._collector_ids[source] ||
	(this._collector_ids[source] = this._collector_id_next ++);
	
    var i, ty = 0;
	
    var t = new Date().valueOf();
	
    var robin = this._robins[source];
    if (robin == undefined) robin = this._robins[source] = 0;
    this._robins[source] = ( this._robins[source] + 1 ) % this._pools.length;
	
    switch(typeof(data)) {
			
		case "number":
			data=[data];
			ty=1;
			break;
			
		case "object":
			if (!estimated_entropy) {
				/* horrible entropy estimator */
				estimated_entropy = 0;
				for (i=0; i<data.length; i++) {
					var x = data[i];
					while (x>0) {
						estimated_entropy++;
						x = x >>> 1;
					}
				}
			}
			this._pools[robin].update_words_big_endian([id,this._event_id++,ty||2,estimated_entropy,t,data.length].concat(data));
			break;
			
		case "string":
			if (!estimated_entropy) {
				/* English text has just over 1 bit per character of entropy.
				 * But this might be HTML or something, and have far less
				 * entropy than English...  Oh well, let's just say one bit.
				 */
				estimated_entropy = data.length;
			}
			this._pools[robin].update_words_big_endian([id,this._event_id++,3,estimated_entropy,t,data.length])
			this._pools[robin].update_string(data);
			break;
			
		default:
			throw "add_entropy: must give an array, number or string"
    }
	
    var old_ready = this.is_ready();
	
    /* record the new strength */
    this._pool_entropy[robin] += estimated_entropy;
    this._pool_strength += estimated_entropy;
	
    /* fire off events */
    if (!old_ready && this.is_ready())
		this._fire_event("seeded", Math.max(this._strength, this._pool_strength));
	
    if (!old_ready)
		this._fire_event("progress", this.get_progress());
},
	
	/* is the generator ready? */
is_ready: function(paranoia) {
    var entropy_required = this._PARANOIA_LEVELS[ paranoia ? paranoia : this._default_paranoia ];
	
    if (this._strength >= entropy_required) {
		return (this._pool_entropy[0] > this._BITS_PER_RESEED && new Date.valueOf() > this._next_reseed) ?
		this.REQUIRES_RESEED | this.READY :
		this.READY;
    } else {
		return (this._pool_strength > entropy_required) ?
        this.REQUIRES_RESEED | this.NOT_READY :
        this.NOT_READY;
    }
},
	
	/* how close to ready is it? */
get_progress: function(paranoia) {
    var entropy_required = this._PARANOIA_LEVELS[ paranoia ? paranoia : this._default_paranoia ];
	
    if (this._strength >= entropy_required) {
		return 1.0;
    } else {
		return (this._pool_strength > entropy_required) ?
        1.0 :
        this._pool_strength / entropy_required;
    }
},
	
	/* start the built-in entropy collectors */
start_collectors: function() {
    if (this._collectors_started) return;
	
    if (window.addEventListener) {
		window.addEventListener("load", this._load_time_collector, false);
		window.addEventListener("mousemove", this._mouse_collector, false);
	} else if (document.attachEvent) {
		document.attachEvent("onload", this._load_time_collector);
		document.attachEvent("onmousemove", this._mouse_collector);
	}
    else throw("can't attach event");    
	
    this._collectors_started = true;
},
	
	/* stop the built-in entropy collectors */
stop_collectors: function() {
    if (!this._collectors_started) return;
	
    if (window.removeEventListener) {
		window.removeEventListener("load", this._load_time_collector);
		window.removeEventListener("mousemove", this._mouse_collector);
    } else if (window.detachEvent) {
		window.detachEvent("onload", this._load_time_collector);
		window.detachEvent("onmousemove", this._mouse_collector)
	}
    this._collectors_started = false;
},
	
use_cookie: function(all_cookies) {
    throw "TODO: implement use_cookie";
},
	
	/* add an event listener for progress or seeded-ness */
addEventListener: function(name, callback) {
    this._callbacks[name][this._callback_i++] = callback;
},
	
	/* remove an event listener for progress or seeded-ness */
removeEventListener: function(name, cb) {
    var i, j, cbs=this._callbacks[name], js_temp=[];
	
    /* I'm not sure if this is necessary; in C++, iterating over a
     * collection and modifying it at the same time is a no-no.
     */
	
    for (j in cbs)
		if (cbs.hasOwnProperty[j] && cbs[j] === cb)
			js_temp.push(j);
	
    for (i=0; i<js_temp.length; i++) {
		j = js[i];
		delete cbs[j];
    }
},
	
	/* private */
	_pools                   : [new SHA256()],
	_pool_entropy            : [0],
	_reseed_count            : 0,
	_robins                  : {},
	_event_id                : 0,
	
	_collector_ids           : {},
	_collector_id_next       : 0,
	
	_strength                : 0,
	_pool_strength           : 0,
	_next_reseed             : 0,
	_key                     : [0,0,0,0,0,0,0,0],
	_counter                 : [0,0,0,0],
	_cipher                  : undefined,
	_default_paranoia        : 6,
	
	/* event listener stuff */
	_collectors_started      : false,
	_callbacks               : {progress: {}, seeded: {}},
	_callback_i              : 0,
	
	/* constants */
	_MAX_WORDS_PER_BURST     : 65536,
	_PARANOIA_LEVELS         : [0,48,64,96,128,192,256,384,512,768,1024],
	_MILLISECONDS_PER_RESEED : 100,
	_BITS_PER_RESEED         : 80,
	
	/* generate 4 random words, no reseed, no gate */
_gen_4_words: function() {
	var words = [];
    for (var i=0; i<3; i++) if (++this._counter[i]) break;
	this._cipher.encryptBlock(this._counter, words);
	return words;
},
	
	/* rekey the AES instance with itself after a request, or every _MAX_WORDS_PER_BURST words */
_gate: function() {
    this._key = this._gen_4_words().concat(this._gen_4_words());
    this._cipher = new aes(this._key);
},
	
	/* reseed the generator with the given words */
_reseed: function(seedWords) {
    this._key = SHA256.hash_words_big_endian(this._key.concat(seedWords));
    this._cipher = new aes(this._key);
    for (var i=0; i<3; i++) if (++this._counter[i]) break;
},
	
	/* reseed the data from the entropy pools */
_reseed_from_pools: function(full) {
    var reseed_data = [], strength = 0;
	
    this._next_reseed = new Date().valueOf() + this._MILLISECONDS_PER_RESEED;
    
    for (i=0; i<this._pools.length; i++) {
		reseed_data = reseed_data.concat(this._pools[i].finalize());
		strength += this._pool_entropy[i];
		this._pool_entropy[i] = 0;
		
		if (!full && (this._reseed_count & (1<<i))) break;
    }
	
    /* if we used the last pool, push a new one onto the stack */
    if (this._reseed_count >= 1 << this._pools.length) {
		this._pools.push(new SHA256());
		this._pool_entropy.push(0);
    }
	
    /* how strong was this reseed? */
    this._pool_strength -= strength;
    if (strength > this._strength) this._strength = strength;
	
    this._reseed_count ++;
    this._reseed(reseed_data);
},
	
_mouse_collector: function(ev) {
    var x = ev.x || ev.clientX || ev.offsetX;
    var y = ev.y || ev.clientY || ev.offsetY;
    Random.add_entropy([x,y], 2, "mouse");
},
	
_load_time_collector: function(ev) {
	var d = new Date();
	Random.add_entropy(d, 2, "loadtime");
},
	
_fire_event: function(name, arg) {
    var j, cbs=Random._callbacks[name], cbs_temp=[];
	
    /* I'm not sure if this is necessary; in C++, iterating over a
     * collection and modifying it at the same time is a no-no.
     */
	
    for (j in cbs) {
		if (cbs.hasOwnProperty(j)) {
			cbs_temp.push(cbs[j]);
		}
    }
	
    for (j=0; j<cbs_temp.length; j++) {
		cbs_temp[j](arg);
    }
}
};

