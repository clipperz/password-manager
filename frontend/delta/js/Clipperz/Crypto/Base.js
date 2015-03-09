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

try { if (typeof(Clipperz.Base) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.Crypto.Base depends on Clipperz.Base!";
}  

if (typeof(Clipperz.Crypto) == 'undefined') { Clipperz.Crypto = {}; }
if (typeof(Clipperz.Crypto.Base) == 'undefined') { Clipperz.Crypto.Base = {}; }

Clipperz.Crypto.Base.VERSION = "0.1";
Clipperz.Crypto.Base.NAME = "Clipperz.Crypto.Base";

//#############################################################################
//	Downloaded on March 30, 2006 from http://anmar.eu.org/projects/jssha2/files/jssha2-0.3.zip (jsSha2/sha256.js)
//#############################################################################

/* A JavaScript implementation of the Secure Hash Algorithm, SHA-256
 * Version 0.3 Copyright Angel Marin 2003-2004 - http://anmar.eu.org/
 * Distributed under the BSD License
 * Some bits taken from Paul Johnston's SHA-1 implementation
 */
var chrsz = 8;  /* bits per input character. 8 - ASCII; 16 - Unicode  */
function safe_add (x, y) {
  var lsw = (x & 0xFFFF) + (y & 0xFFFF);
  var msw = (x >> 16) + (y >> 16) + (lsw >> 16);
  return (msw << 16) | (lsw & 0xFFFF);
}
function S (X, n) {return ( X >>> n ) | (X << (32 - n));}
function R (X, n) {return ( X >>> n );}
function Ch(x, y, z) {return ((x & y) ^ ((~x) & z));}
function Maj(x, y, z) {return ((x & y) ^ (x & z) ^ (y & z));}
function Sigma0256(x) {return (S(x, 2) ^ S(x, 13) ^ S(x, 22));}
function Sigma1256(x) {return (S(x, 6) ^ S(x, 11) ^ S(x, 25));}
function Gamma0256(x) {return (S(x, 7) ^ S(x, 18) ^ R(x, 3));}
function Gamma1256(x) {return (S(x, 17) ^ S(x, 19) ^ R(x, 10));}
function core_sha256 (m, l) {
    var K = new Array(0x428A2F98,0x71374491,0xB5C0FBCF,0xE9B5DBA5,0x3956C25B,0x59F111F1,0x923F82A4,0xAB1C5ED5,0xD807AA98,0x12835B01,0x243185BE,0x550C7DC3,0x72BE5D74,0x80DEB1FE,0x9BDC06A7,0xC19BF174,0xE49B69C1,0xEFBE4786,0xFC19DC6,0x240CA1CC,0x2DE92C6F,0x4A7484AA,0x5CB0A9DC,0x76F988DA,0x983E5152,0xA831C66D,0xB00327C8,0xBF597FC7,0xC6E00BF3,0xD5A79147,0x6CA6351,0x14292967,0x27B70A85,0x2E1B2138,0x4D2C6DFC,0x53380D13,0x650A7354,0x766A0ABB,0x81C2C92E,0x92722C85,0xA2BFE8A1,0xA81A664B,0xC24B8B70,0xC76C51A3,0xD192E819,0xD6990624,0xF40E3585,0x106AA070,0x19A4C116,0x1E376C08,0x2748774C,0x34B0BCB5,0x391C0CB3,0x4ED8AA4A,0x5B9CCA4F,0x682E6FF3,0x748F82EE,0x78A5636F,0x84C87814,0x8CC70208,0x90BEFFFA,0xA4506CEB,0xBEF9A3F7,0xC67178F2);
    var HASH = new Array(0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A, 0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19);
    var W = new Array(64);
    var a, b, c, d, e, f, g, h, i, j;
    var T1, T2;
    /* append padding */
    m[l >> 5] |= 0x80 << (24 - l % 32);
    m[((l + 64 >> 9) << 4) + 15] = l;
    for ( var i = 0; i<m.length; i+=16 ) {
        a = HASH[0]; b = HASH[1]; c = HASH[2]; d = HASH[3]; e = HASH[4]; f = HASH[5]; g = HASH[6]; h = HASH[7];
        for ( var j = 0; j<64; j++) {
            if (j < 16) W[j] = m[j + i];
            else W[j] = safe_add(safe_add(safe_add(Gamma1256(W[j - 2]), W[j - 7]), Gamma0256(W[j - 15])), W[j - 16]);
            T1 = safe_add(safe_add(safe_add(safe_add(h, Sigma1256(e)), Ch(e, f, g)), K[j]), W[j]);
            T2 = safe_add(Sigma0256(a), Maj(a, b, c));
            h = g; g = f; f = e; e = safe_add(d, T1); d = c; c = b; b = a; a = safe_add(T1, T2);
        }
        HASH[0] = safe_add(a, HASH[0]); HASH[1] = safe_add(b, HASH[1]); HASH[2] = safe_add(c, HASH[2]); HASH[3] = safe_add(d, HASH[3]); HASH[4] = safe_add(e, HASH[4]); HASH[5] = safe_add(f, HASH[5]); HASH[6] = safe_add(g, HASH[6]); HASH[7] = safe_add(h, HASH[7]);
    }
    return HASH;
}
function str2binb (str) {
  var bin = Array();
  var mask = (1 << chrsz) - 1;
  for(var i = 0; i < str.length * chrsz; i += chrsz)
    bin[i>>5] |= (str.charCodeAt(i / chrsz) & mask) << (24 - i%32);
  return bin;
}
function binb2hex (binarray) {
  var hexcase = 0; /* hex output format. 0 - lowercase; 1 - uppercase */
  var hex_tab = hexcase ? "0123456789ABCDEF" : "0123456789abcdef";
  var str = "";
  for (var i = 0; i < binarray.length * 4; i++) {
    str += hex_tab.charAt((binarray[i>>2] >> ((3 - i%4)*8+4)) & 0xF) + hex_tab.charAt((binarray[i>>2] >> ((3 - i%4)*8  )) & 0xF);
  }
  return str;
}
function hex_sha256(s){return binb2hex(core_sha256(str2binb(s),s.length * chrsz));}



//#############################################################################
//	Downloaded on March 30, 2006 from http://www.fourmilab.ch/javascrypt/javascrypt.zip (entropy.js)
//#############################################################################

    //  Entropy collection utilities

    /*	Start by declaring static storage and initialise
    	the entropy vector from the time we come through
    	here. */
	
    var entropyData = new Array();   	    // Collected entropy data
    var edlen = 0;  	    	    	    // Keyboard array data length
 
    addEntropyTime();	    	    	    // Start entropy collection with page load time
    ce();   	    	    	    	    // Roll milliseconds into initial entropy

    //	Add a byte to the entropy vector
    
    function addEntropyByte(b) {
    	entropyData[edlen++] = b;
    }
            
    /*	Capture entropy.  When the user presses a key or performs
	various other events for which we can request
	notification, add the time in 255ths of a second to the
	entropyData array.  The name of the function is short
	so it doesn't bloat the form object declarations in
	which it appears in various "onXXX" events.  */
    
    function ce() {
    	addEntropyByte(Math.floor((((new Date).getMilliseconds()) * 255) / 999));
    }
    
    //	Add a 32 bit quantity to the entropy vector
    
    function addEntropy32(w) {
    	var i;
	
	for (i = 0; i < 4; i++) {
	    addEntropyByte(w & 0xFF);
	    w >>= 8;
    	}
    }
    
    /*	Add the current time and date (milliseconds since the epoch,
    	truncated to 32 bits) to the entropy vector.  */
	
    function addEntropyTime() {
    	addEntropy32((new Date()).getTime());
    }

    /*  Start collection of entropy from mouse movements. The
	argument specifies the  number of entropy items to be
	obtained from mouse motion, after which mouse motion
	will be ignored.  Note that you can re-enable mouse
	motion collection at any time if not already underway.  */
	
    var mouseMotionCollect = 0;
    var oldMoveHandler;     	    // For saving and restoring mouse move handler in IE4
	
    function mouseMotionEntropy(maxsamp) {
    	if (mouseMotionCollect <= 0) {
	    mouseMotionCollect = maxsamp;
    	    if ((document.implementation.hasFeature("Events", "2.0")) &&
	    	document.addEventListener) {
    	    	//  Browser supports Document Object Model (DOM) 2 events
		document.addEventListener("mousemove", mouseMoveEntropy, false);
	    } else {
		if (document.attachEvent) {
	    	    //  Internet Explorer 5 and above event model
		    document.attachEvent("onmousemove", mouseMoveEntropy);
		} else {
		    //	Internet Explorer 4 event model
	    	    oldMoveHandler = document.onmousemove;
		    document.onmousemove = mouseMoveEntropy;
		}
	    }
//dump("Mouse enable", mouseMotionCollect);
	}
    }
    
    /*	Collect entropy from mouse motion events.  Note that
    	this is craftily coded to work with either DOM2 or Internet
	Explorer style events.  Note that we don't use every successive
	mouse movement event.  Instead, we XOR the three bytes collected
	from the mouse and use that to determine how many subsequent
	mouse movements we ignore before capturing the next one.  */
	
    var mouseEntropyTime = 0;	    // Delay counter for mouse entropy collection
	
    function mouseMoveEntropy(e) {
    	if (!e) {
	    e = window.event;	    // Internet Explorer event model
	}
	if (mouseMotionCollect > 0) {
	    if (mouseEntropyTime-- <= 0) {
	    	addEntropyByte(e.screenX & 0xFF);
	    	addEntropyByte(e.screenY & 0xFF);
	    	ce();
	    	mouseMotionCollect--;
	    	mouseEntropyTime = (entropyData[edlen - 3] ^ entropyData[edlen - 2] ^
		    	    	    entropyData[edlen - 1]) % 19;
//dump("Mouse Move", byteArrayToHex(entropyData.slice(-3)));
    	    }
	    if (mouseMotionCollect <= 0) {
	    	if (document.removeEventListener) {
		    document.removeEventListener("mousemove", mouseMoveEntropy, false);
		} else if (document.detachEvent) {
		    document.detachEvent("onmousemove", mouseMoveEntropy);
		} else {
		    document.onmousemove = oldMoveHandler;
		}
//dump("Spung!", 0);
	    }
	}
    }    
    
    /*	Compute a 32 byte key value from the entropy vector.
    	We compute the value by taking the MD5 sum of the even
	and odd bytes respectively of the entropy vector, then
	concatenating the two MD5 sums.  */
    
    function keyFromEntropy() {
	var i, k = new Array(32);
	
	if (edlen == 0) {
	    alert("Blooie!  Entropy vector void at call to keyFromEntropy.");
	}
//dump("Entropy bytes", edlen);

	md5_init();
	for (i = 0; i < edlen; i += 2) {
	    md5_update(entropyData[i]);
	}
	md5_finish();
    	for (i = 0; i < 16; i++) {
	    k[i] = digestBits[i];
	}

	md5_init();
	for (i = 1; i < edlen; i += 2) {
	    md5_update(entropyData[i]);
	}
	md5_finish();
    	for (i = 0; i < 16; i++) {
	    k[i + 16] = digestBits[i];
	}
	
//dump("keyFromEntropy", byteArrayToHex(k));
	return k;
    }

//#############################################################################
//	Downloaded on March 30, 2006 from http://www.fourmilab.ch/javascrypt/javascrypt.zip (aesprng.js)
//#############################################################################


    //  AES based pseudorandom number generator

    /*  Constructor.  Called with an array of 32 byte (0-255) values
	containing the initial seed.  */

    function AESprng(seed) {
	this.key = new Array();
	this.key = seed;
	this.itext = hexToByteArray("9F489613248148F9C27945C6AE62EECA3E3367BB14064E4E6DC67A9F28AB3BD1");
	this.nbytes = 0;    	    // Bytes left in buffer
	
	this.next = AESprng_next;
	this.nextbits = AESprng_nextbits;
	this.nextInt = AESprng_nextInt;
	this.round = AESprng_round;
	
	/*  Encrypt the initial text with the seed key
	    three times, feeding the output of the encryption
	    back into the key for the next round.  */
	
	bsb = blockSizeInBits;
	blockSizeInBits = 256;    
	var i, ct;
    	for (i = 0; i < 3; i++) {
	    this.key = rijndaelEncrypt(this.itext, this.key, "ECB");
	}
	
	/*  Now make between one and four additional
	    key-feedback rounds, with the number determined
	    by bits from the result of the first three
	    rounds.  */
	
	var n = 1 + (this.key[3] & 2) + (this.key[9] & 1);    
    	for (i = 0; i < n; i++) {
	    this.key = rijndaelEncrypt(this.itext, this.key, "ECB");
	}
    	blockSizeInBits = bsb;
    }
    
    function AESprng_round() {
	bsb = blockSizeInBits;
	blockSizeInBits = 256;    
    	this.key = rijndaelEncrypt(this.itext, this.key, "ECB");
	this.nbytes = 32;
    	blockSizeInBits = bsb;
    }
    
    //	Return next byte from the generator

    function AESprng_next() {
    	if (this.nbytes <= 0) {
	    this.round();
	}
	return(this.key[--this.nbytes]);
    }
    
    //	Return n bit integer value (up to maximum integer size)
    
    function AESprng_nextbits(n) {
    	var i, w = 0, nbytes = Math.floor((n + 7) / 8);

	for (i = 0; i < nbytes; i++) {
	    w = (w << 8) | this.next();
	}
	return w & ((1 << n) - 1);
    }

    //  Return integer between 0 and n inclusive
    
    function AESprng_nextInt(n) {
    	var p = 1, nb = 0;
	
	//  Determine smallest p,  2^p > n
	//  nb = log_2 p
	
	while (n >= p) {
	    p <<= 1;
	    nb++;
	}
	p--;
	
	/*  Generate values from 0 through n by first generating
	    values v from 0 to (2^p)-1, then discarding any results v > n.
	    For the rationale behind this (and why taking
	    values mod (n + 1) is biased toward smaller values, see
	    Ferguson and Schneier, "Practical Cryptography",
	    ISBN 0-471-22357-3, section 10.8).  */

	while (true) {
    	    var v = this.nextbits(nb) & p;
	    
	    if (v <= n) {
	    	return v;
	    }
	}
    }

//#############################################################################
//	Downloaded on March 30, 2006 from http://www.fourmilab.ch/javascrypt/javascrypt.zip (md5.js)
//#############################################################################

/*
 *  md5.jvs 1.0b 27/06/96
 *
 * Javascript implementation of the RSA Data Security, Inc. MD5
 * Message-Digest Algorithm.
 *
 * Copyright (c) 1996 Henri Torgemane. All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for any purposes and without
 * fee is hereby granted provided that this copyright notice
 * appears in all copies. 
 *
 * Of course, this soft is provided "as is" without express or implied
 * warranty of any kind.

    This version contains some trivial reformatting modifications
    by John Walker.

 */

function array(n) {
    for (i = 0; i < n; i++) {
        this[i] = 0;
    }
    this.length = n;
}

/* Some basic logical functions had to be rewritten because of a bug in
 * Javascript.. Just try to compute 0xffffffff >> 4 with it..
 * Of course, these functions are slower than the original would be, but
 * at least, they work!
 */

function integer(n) {
    return n % (0xffffffff + 1);
}

function shr(a, b) {
    a = integer(a);
    b = integer(b);
    if (a - 0x80000000 >= 0) {
        a = a % 0x80000000;
        a >>= b;
        a += 0x40000000 >> (b - 1);
    } else {
        a >>= b;
    }
    return a;
}

function shl1(a) {
    a = a % 0x80000000;
    if (a & 0x40000000 == 0x40000000) {
        a -= 0x40000000;  
        a *= 2;
        a += 0x80000000;
    } else {
        a *= 2;
    }
    return a;
}

function shl(a, b) {
    a = integer(a);
    b = integer(b);
    for (var i = 0; i < b; i++) {
        a = shl1(a);
    }
    return a;
}

function and(a, b) {
    a = integer(a);
    b = integer(b);
    var t1 = a - 0x80000000;
    var t2 = b - 0x80000000;
    if (t1 >= 0) {
        if (t2 >= 0) {
            return ((t1 & t2) + 0x80000000);
        } else {
            return (t1 & b);
        }
    } else {
        if (t2 >= 0) {
            return (a & t2);
        } else {
            return (a & b);  
        }
    }
}

function or(a, b) {
    a = integer(a);
    b = integer(b);
    var t1 = a - 0x80000000;
    var t2 = b - 0x80000000;
    if (t1 >= 0) {
        if (t2 >= 0) {
            return ((t1 | t2) + 0x80000000);
        } else {
            return ((t1 | b) + 0x80000000);
        }
    } else {
        if (t2 >= 0) {
            return ((a | t2) + 0x80000000);
        } else {
            return (a | b);  
        }
    }
}

function xor(a, b) {
    a = integer(a);
    b = integer(b);
    var t1 = a - 0x80000000;
    var t2 = b - 0x80000000;
    if (t1 >= 0) {
        if (t2 >= 0) {
            return (t1 ^ t2);
        } else {
            return ((t1 ^ b) + 0x80000000);
        }
    } else {
        if (t2 >= 0) {
            return ((a ^ t2) + 0x80000000);
        } else {
            return (a ^ b);  
        }
    }
}

function not(a) {
    a = integer(a);
    return 0xffffffff - a;
}

/* Here begin the real algorithm */

var state = new array(4); 
var count = new array(2);
    count[0] = 0;
    count[1] = 0;                     
var buffer = new array(64); 
var transformBuffer = new array(16); 
var digestBits = new array(16);

var S11 = 7;
var S12 = 12;
var S13 = 17;
var S14 = 22;
var S21 = 5;
var S22 = 9;
var S23 = 14;
var S24 = 20;
var S31 = 4;
var S32 = 11;
var S33 = 16;
var S34 = 23;
var S41 = 6;
var S42 = 10;
var S43 = 15;
var S44 = 21;

function F(x, y, z) {
    return or(and(x, y), and(not(x), z));
}

function G(x, y, z) {
    return or(and(x, z), and(y, not(z)));
}

function H(x, y, z) {
    return xor(xor(x, y), z);
}

function I(x, y, z) {
    return xor(y ,or(x , not(z)));
}

function rotateLeft(a, n) {
    return or(shl(a, n), (shr(a, (32 - n))));
}

function FF(a, b, c, d, x, s, ac) {
    a = a + F(b, c, d) + x + ac;
    a = rotateLeft(a, s);
    a = a + b;
    return a;
}

function GG(a, b, c, d, x, s, ac) {
    a = a + G(b, c, d) + x + ac;
    a = rotateLeft(a, s);
    a = a + b;
    return a;
}

function HH(a, b, c, d, x, s, ac) {
    a = a + H(b, c, d) + x + ac;
    a = rotateLeft(a, s);
    a = a + b;
    return a;
}

function II(a, b, c, d, x, s, ac) {
    a = a + I(b, c, d) + x + ac;
    a = rotateLeft(a, s);
    a = a + b;
    return a;
}

function transform(buf, offset) { 
    var a = 0, b = 0, c = 0, d = 0; 
    var x = transformBuffer;
    
    a = state[0];
    b = state[1];
    c = state[2];
    d = state[3];
    
    for (i = 0; i < 16; i++) {
        x[i] = and(buf[i * 4 + offset], 0xFF);
        for (j = 1; j < 4; j++) {
            x[i] += shl(and(buf[i * 4 + j + offset] ,0xFF), j * 8);
        }
    }

    /* Round 1 */
    a = FF( a, b, c, d, x[ 0], S11, 0xd76aa478); /* 1 */
    d = FF( d, a, b, c, x[ 1], S12, 0xe8c7b756); /* 2 */
    c = FF( c, d, a, b, x[ 2], S13, 0x242070db); /* 3 */
    b = FF( b, c, d, a, x[ 3], S14, 0xc1bdceee); /* 4 */
    a = FF( a, b, c, d, x[ 4], S11, 0xf57c0faf); /* 5 */
    d = FF( d, a, b, c, x[ 5], S12, 0x4787c62a); /* 6 */
    c = FF( c, d, a, b, x[ 6], S13, 0xa8304613); /* 7 */
    b = FF( b, c, d, a, x[ 7], S14, 0xfd469501); /* 8 */
    a = FF( a, b, c, d, x[ 8], S11, 0x698098d8); /* 9 */
    d = FF( d, a, b, c, x[ 9], S12, 0x8b44f7af); /* 10 */
    c = FF( c, d, a, b, x[10], S13, 0xffff5bb1); /* 11 */
    b = FF( b, c, d, a, x[11], S14, 0x895cd7be); /* 12 */
    a = FF( a, b, c, d, x[12], S11, 0x6b901122); /* 13 */
    d = FF( d, a, b, c, x[13], S12, 0xfd987193); /* 14 */
    c = FF( c, d, a, b, x[14], S13, 0xa679438e); /* 15 */
    b = FF( b, c, d, a, x[15], S14, 0x49b40821); /* 16 */

    /* Round 2 */
    a = GG( a, b, c, d, x[ 1], S21, 0xf61e2562); /* 17 */
    d = GG( d, a, b, c, x[ 6], S22, 0xc040b340); /* 18 */
    c = GG( c, d, a, b, x[11], S23, 0x265e5a51); /* 19 */
    b = GG( b, c, d, a, x[ 0], S24, 0xe9b6c7aa); /* 20 */
    a = GG( a, b, c, d, x[ 5], S21, 0xd62f105d); /* 21 */
    d = GG( d, a, b, c, x[10], S22,  0x2441453); /* 22 */
    c = GG( c, d, a, b, x[15], S23, 0xd8a1e681); /* 23 */
    b = GG( b, c, d, a, x[ 4], S24, 0xe7d3fbc8); /* 24 */
    a = GG( a, b, c, d, x[ 9], S21, 0x21e1cde6); /* 25 */
    d = GG( d, a, b, c, x[14], S22, 0xc33707d6); /* 26 */
    c = GG( c, d, a, b, x[ 3], S23, 0xf4d50d87); /* 27 */
    b = GG( b, c, d, a, x[ 8], S24, 0x455a14ed); /* 28 */
    a = GG( a, b, c, d, x[13], S21, 0xa9e3e905); /* 29 */
    d = GG( d, a, b, c, x[ 2], S22, 0xfcefa3f8); /* 30 */
    c = GG( c, d, a, b, x[ 7], S23, 0x676f02d9); /* 31 */
    b = GG( b, c, d, a, x[12], S24, 0x8d2a4c8a); /* 32 */

    /* Round 3 */
    a = HH( a, b, c, d, x[ 5], S31, 0xfffa3942); /* 33 */
    d = HH( d, a, b, c, x[ 8], S32, 0x8771f681); /* 34 */
    c = HH( c, d, a, b, x[11], S33, 0x6d9d6122); /* 35 */
    b = HH( b, c, d, a, x[14], S34, 0xfde5380c); /* 36 */
    a = HH( a, b, c, d, x[ 1], S31, 0xa4beea44); /* 37 */
    d = HH( d, a, b, c, x[ 4], S32, 0x4bdecfa9); /* 38 */
    c = HH( c, d, a, b, x[ 7], S33, 0xf6bb4b60); /* 39 */
    b = HH( b, c, d, a, x[10], S34, 0xbebfbc70); /* 40 */
    a = HH( a, b, c, d, x[13], S31, 0x289b7ec6); /* 41 */
    d = HH( d, a, b, c, x[ 0], S32, 0xeaa127fa); /* 42 */
    c = HH( c, d, a, b, x[ 3], S33, 0xd4ef3085); /* 43 */
    b = HH( b, c, d, a, x[ 6], S34,  0x4881d05); /* 44 */
    a = HH( a, b, c, d, x[ 9], S31, 0xd9d4d039); /* 45 */
    d = HH( d, a, b, c, x[12], S32, 0xe6db99e5); /* 46 */
    c = HH( c, d, a, b, x[15], S33, 0x1fa27cf8); /* 47 */
    b = HH( b, c, d, a, x[ 2], S34, 0xc4ac5665); /* 48 */

    /* Round 4 */
    a = II( a, b, c, d, x[ 0], S41, 0xf4292244); /* 49 */
    d = II( d, a, b, c, x[ 7], S42, 0x432aff97); /* 50 */
    c = II( c, d, a, b, x[14], S43, 0xab9423a7); /* 51 */
    b = II( b, c, d, a, x[ 5], S44, 0xfc93a039); /* 52 */
    a = II( a, b, c, d, x[12], S41, 0x655b59c3); /* 53 */
    d = II( d, a, b, c, x[ 3], S42, 0x8f0ccc92); /* 54 */
    c = II( c, d, a, b, x[10], S43, 0xffeff47d); /* 55 */
    b = II( b, c, d, a, x[ 1], S44, 0x85845dd1); /* 56 */
    a = II( a, b, c, d, x[ 8], S41, 0x6fa87e4f); /* 57 */
    d = II( d, a, b, c, x[15], S42, 0xfe2ce6e0); /* 58 */
    c = II( c, d, a, b, x[ 6], S43, 0xa3014314); /* 59 */
    b = II( b, c, d, a, x[13], S44, 0x4e0811a1); /* 60 */
    a = II( a, b, c, d, x[ 4], S41, 0xf7537e82); /* 61 */
    d = II( d, a, b, c, x[11], S42, 0xbd3af235); /* 62 */
    c = II( c, d, a, b, x[ 2], S43, 0x2ad7d2bb); /* 63 */
    b = II( b, c, d, a, x[ 9], S44, 0xeb86d391); /* 64 */

    state[0] += a;
    state[1] += b;
    state[2] += c;
    state[3] += d;

}

function md5_init() {
    count[0] = count[1] = 0;
    state[0] = 0x67452301;
    state[1] = 0xefcdab89;
    state[2] = 0x98badcfe;
    state[3] = 0x10325476;
    for (i = 0; i < digestBits.length; i++) {
        digestBits[i] = 0;
    }
}

function md5_update(b) { 
    var index, i;
    
    index = and(shr(count[0],3) , 0x3F);
    if (count[0] < 0xFFFFFFFF - 7) {
      count[0] += 8;
    } else {
      count[1]++;
      count[0] -= 0xFFFFFFFF + 1;
      count[0] += 8;
    }
    buffer[index] = and(b, 0xff);
    if (index  >= 63) {
        transform(buffer, 0);
    }
}

function md5_finish() {
    var bits = new array(8);
    var padding; 
    var i = 0, index = 0, padLen = 0;

    for (i = 0; i < 4; i++) {
        bits[i] = and(shr(count[0], (i * 8)), 0xFF);
    }
    for (i = 0; i < 4; i++) {
        bits[i + 4] = and(shr(count[1], (i * 8)), 0xFF);
    }
    index = and(shr(count[0], 3), 0x3F);
    padLen = (index < 56) ? (56 - index) : (120 - index);
    padding = new array(64); 
    padding[0] = 0x80;
    for (i = 0; i < padLen; i++) {
      md5_update(padding[i]);
    }
    for (i = 0; i < 8; i++) {
      md5_update(bits[i]);
    }

    for (i = 0; i < 4; i++) {
        for (j = 0; j < 4; j++) {
            digestBits[i * 4 + j] = and(shr(state[i], (j * 8)) , 0xFF);
        }
    } 
}

/* End of the MD5 algorithm */

//#############################################################################
//	Downloaded on March 30, 2006 from http://www.fourmilab.ch/javascrypt/javascrypt.zip (aes.js)
//#############################################################################


/* rijndael.js      Rijndael Reference Implementation

    This is a modified version of the software described below,
    produced in September 2003 by John Walker for use in the
    JavsScrypt browser-based encryption package.  The principal
    changes are replacing the original getRandomBytes function with
    one which calls our pseudorandom generator (which must
    be instantiated and seeded before the first call on getRandomBytes),
    and changing keySizeInBits to 256.  Some code not required by the
    JavsScrypt application has been commented out.  Please see
    http://www.fourmilab.ch/javascrypt/ for further information on
    JavaScrypt.
    
    The following is the original copyright and application
    information.

   Copyright (c) 2001 Fritz Schneider
 
 This software is provided as-is, without express or implied warranty.  
 Permission to use, copy, modify, distribute or sell this software, with or
 without fee, for any purpose and by any individual or organization, is hereby
 granted, provided that the above copyright notice and this paragraph appear 
 in all copies. Distribution as a part of an application or binary must
 include the above copyright notice in the documentation and/or other materials
 provided with the application or distribution.

   As the above disclaimer notes, you are free to use this code however you
   want. However, I would request that you send me an email 
   (fritz /at/ cs /dot/ ucsd /dot/ edu) to say hi if you find this code useful
   or instructional. Seeing that people are using the code acts as 
   encouragement for me to continue development. If you *really* want to thank
   me you can buy the book I wrote with Thomas Powell, _JavaScript:
   _The_Complete_Reference_ :)

   This code is an UNOPTIMIZED REFERENCE implementation of Rijndael. 
   If there is sufficient interest I can write an optimized (word-based, 
   table-driven) version, although you might want to consider using a 
   compiled language if speed is critical to your application. As it stands,
   one run of the monte carlo test (10,000 encryptions) can take up to 
   several minutes, depending upon your processor. You shouldn't expect more
   than a few kilobytes per second in throughput.

   Also note that there is very little error checking in these functions. 
   Doing proper error checking is always a good idea, but the ideal 
   implementation (using the instanceof operator and exceptions) requires
   IE5+/NS6+, and I've chosen to implement this code so that it is compatible
   with IE4/NS4. 

   And finally, because JavaScript doesn't have an explicit byte/char data 
   type (although JavaScript 2.0 most likely will), when I refer to "byte" 
   in this code I generally mean "32 bit integer with value in the interval 
   [0,255]" which I treat as a byte.

   See http://www-cse.ucsd.edu/~fritz/rijndael.html for more documentation
   of the (very simple) API provided by this code.

                                               Fritz Schneider
                                               fritz at cs.ucsd.edu
 
*/


// Rijndael parameters --  Valid values are 128, 192, or 256

var keySizeInBits = 256;
var blockSizeInBits = 128;

//
// Note: in the following code the two dimensional arrays are indexed as
//       you would probably expect, as array[row][column]. The state arrays
//       are 2d arrays of the form state[4][Nb].


// The number of rounds for the cipher, indexed by [Nk][Nb]
var roundsArray = [ ,,,,[,,,,10,, 12,, 14],, 
                        [,,,,12,, 12,, 14],, 
                        [,,,,14,, 14,, 14] ];

// The number of bytes to shift by in shiftRow, indexed by [Nb][row]
var shiftOffsets = [ ,,,,[,1, 2, 3],,[,1, 2, 3],,[,1, 3, 4] ];

// The round constants used in subkey expansion
var Rcon = [ 
0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 
0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 
0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 
0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 
0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91 ];

// Precomputed lookup table for the SBox
var SBox = [
 99, 124, 119, 123, 242, 107, 111, 197,  48,   1, 103,  43, 254, 215, 171, 
118, 202, 130, 201, 125, 250,  89,  71, 240, 173, 212, 162, 175, 156, 164, 
114, 192, 183, 253, 147,  38,  54,  63, 247, 204,  52, 165, 229, 241, 113, 
216,  49,  21,   4, 199,  35, 195,  24, 150,   5, 154,   7,  18, 128, 226, 
235,  39, 178, 117,   9, 131,  44,  26,  27, 110,  90, 160,  82,  59, 214, 
179,  41, 227,  47, 132,  83, 209,   0, 237,  32, 252, 177,  91, 106, 203, 
190,  57,  74,  76,  88, 207, 208, 239, 170, 251,  67,  77,  51, 133,  69, 
249,   2, 127,  80,  60, 159, 168,  81, 163,  64, 143, 146, 157,  56, 245, 
188, 182, 218,  33,  16, 255, 243, 210, 205,  12,  19, 236,  95, 151,  68,  
23,  196, 167, 126,  61, 100,  93,  25, 115,  96, 129,  79, 220,  34,  42, 
144, 136,  70, 238, 184,  20, 222,  94,  11, 219, 224,  50,  58,  10,  73,
  6,  36,  92, 194, 211, 172,  98, 145, 149, 228, 121, 231, 200,  55, 109, 
141, 213,  78, 169, 108,  86, 244, 234, 101, 122, 174,   8, 186, 120,  37,  
 46,  28, 166, 180, 198, 232, 221, 116,  31,  75, 189, 139, 138, 112,  62, 
181, 102,  72,   3, 246,  14,  97,  53,  87, 185, 134, 193,  29, 158, 225,
248, 152,  17, 105, 217, 142, 148, 155,  30, 135, 233, 206,  85,  40, 223,
140, 161, 137,  13, 191, 230,  66, 104,  65, 153,  45,  15, 176,  84, 187,  
 22 ];

// Precomputed lookup table for the inverse SBox
var SBoxInverse = [
 82,   9, 106, 213,  48,  54, 165,  56, 191,  64, 163, 158, 129, 243, 215, 
251, 124, 227,  57, 130, 155,  47, 255, 135,  52, 142,  67,  68, 196, 222, 
233, 203,  84, 123, 148,  50, 166, 194,  35,  61, 238,  76, 149,  11,  66, 
250, 195,  78,   8,  46, 161, 102,  40, 217,  36, 178, 118,  91, 162,  73, 
109, 139, 209,  37, 114, 248, 246, 100, 134, 104, 152,  22, 212, 164,  92, 
204,  93, 101, 182, 146, 108, 112,  72,  80, 253, 237, 185, 218,  94,  21,  
 70,  87, 167, 141, 157, 132, 144, 216, 171,   0, 140, 188, 211,  10, 247, 
228,  88,   5, 184, 179,  69,   6, 208,  44,  30, 143, 202,  63,  15,   2, 
193, 175, 189,   3,   1,  19, 138, 107,  58, 145,  17,  65,  79, 103, 220, 
234, 151, 242, 207, 206, 240, 180, 230, 115, 150, 172, 116,  34, 231, 173,
 53, 133, 226, 249,  55, 232,  28, 117, 223, 110,  71, 241,  26, 113,  29, 
 41, 197, 137, 111, 183,  98,  14, 170,  24, 190,  27, 252,  86,  62,  75, 
198, 210, 121,  32, 154, 219, 192, 254, 120, 205,  90, 244,  31, 221, 168,
 51, 136,   7, 199,  49, 177,  18,  16,  89,  39, 128, 236,  95,  96,  81,
127, 169,  25, 181,  74,  13,  45, 229, 122, 159, 147, 201, 156, 239, 160,
224,  59,  77, 174,  42, 245, 176, 200, 235, 187,  60, 131,  83, 153,  97, 
 23,  43,   4, 126, 186, 119, 214,  38, 225, 105,  20,  99,  85,  33,  12,
125 ];

// This method circularly shifts the array left by the number of elements
// given in its parameter. It returns the resulting array and is used for 
// the ShiftRow step. Note that shift() and push() could be used for a more 
// elegant solution, but they require IE5.5+, so I chose to do it manually. 

function cyclicShiftLeft(theArray, positions) {
  var temp = theArray.slice(0, positions);
  theArray = theArray.slice(positions).concat(temp);
  return theArray;
}

// Cipher parameters ... do not change these
var Nk = keySizeInBits / 32;                   
var Nb = blockSizeInBits / 32;
var Nr = roundsArray[Nk][Nb];

// Multiplies the element "poly" of GF(2^8) by x. See the Rijndael spec.

function xtime(poly) {
  poly <<= 1;
  return ((poly & 0x100) ? (poly ^ 0x11B) : (poly));
}

// Multiplies the two elements of GF(2^8) together and returns the result.
// See the Rijndael spec, but should be straightforward: for each power of
// the indeterminant that has a 1 coefficient in x, add y times that power
// to the result. x and y should be bytes representing elements of GF(2^8)

function mult_GF256(x, y) {
  var bit, result = 0;
  
  for (bit = 1; bit < 256; bit *= 2, y = xtime(y)) {
    if (x & bit) 
      result ^= y;
  }
  return result;
}

// Performs the substitution step of the cipher.  State is the 2d array of
// state information (see spec) and direction is string indicating whether
// we are performing the forward substitution ("encrypt") or inverse 
// substitution (anything else)

function byteSub(state, direction) {
  var S;
  if (direction == "encrypt")           // Point S to the SBox we're using
    S = SBox;
  else
    S = SBoxInverse;
  for (var i = 0; i < 4; i++)           // Substitute for every byte in state
    for (var j = 0; j < Nb; j++)
       state[i][j] = S[state[i][j]];
}

// Performs the row shifting step of the cipher.

function shiftRow(state, direction) {
  for (var i=1; i<4; i++)               // Row 0 never shifts
    if (direction == "encrypt")
       state[i] = cyclicShiftLeft(state[i], shiftOffsets[Nb][i]);
    else
       state[i] = cyclicShiftLeft(state[i], Nb - shiftOffsets[Nb][i]);

}

// Performs the column mixing step of the cipher. Most of these steps can
// be combined into table lookups on 32bit values (at least for encryption)
// to greatly increase the speed. 

function mixColumn(state, direction) {
  var b = [];                            // Result of matrix multiplications
  for (var j = 0; j < Nb; j++) {         // Go through each column...
    for (var i = 0; i < 4; i++) {        // and for each row in the column...
      if (direction == "encrypt")
        b[i] = mult_GF256(state[i][j], 2) ^          // perform mixing
               mult_GF256(state[(i+1)%4][j], 3) ^ 
               state[(i+2)%4][j] ^ 
               state[(i+3)%4][j];
      else 
        b[i] = mult_GF256(state[i][j], 0xE) ^ 
               mult_GF256(state[(i+1)%4][j], 0xB) ^
               mult_GF256(state[(i+2)%4][j], 0xD) ^
               mult_GF256(state[(i+3)%4][j], 9);
    }
    for (var i = 0; i < 4; i++)          // Place result back into column
      state[i][j] = b[i];
  }
}

// Adds the current round key to the state information. Straightforward.

function addRoundKey(state, roundKey) {
  for (var j = 0; j < Nb; j++) {                 // Step through columns...
    state[0][j] ^= (roundKey[j] & 0xFF);         // and XOR
    state[1][j] ^= ((roundKey[j]>>8) & 0xFF);
    state[2][j] ^= ((roundKey[j]>>16) & 0xFF);
    state[3][j] ^= ((roundKey[j]>>24) & 0xFF);
  }
}

// This function creates the expanded key from the input (128/192/256-bit)
// key. The parameter key is an array of bytes holding the value of the key.
// The returned value is an array whose elements are the 32-bit words that 
// make up the expanded key.

function keyExpansion(key) {
  var expandedKey = new Array();
  var temp;

  // in case the key size or parameters were changed...
  Nk = keySizeInBits / 32;                   
  Nb = blockSizeInBits / 32;
  Nr = roundsArray[Nk][Nb];

  for (var j=0; j < Nk; j++)     // Fill in input key first
    expandedKey[j] = 
      (key[4*j]) | (key[4*j+1]<<8) | (key[4*j+2]<<16) | (key[4*j+3]<<24);

  // Now walk down the rest of the array filling in expanded key bytes as
  // per Rijndael's spec
  for (j = Nk; j < Nb * (Nr + 1); j++) {    // For each word of expanded key
    temp = expandedKey[j - 1];
    if (j % Nk == 0) 
      temp = ( (SBox[(temp>>8) & 0xFF]) |
               (SBox[(temp>>16) & 0xFF]<<8) |
               (SBox[(temp>>24) & 0xFF]<<16) |
               (SBox[temp & 0xFF]<<24) ) ^ Rcon[Math.floor(j / Nk) - 1];
    else if (Nk > 6 && j % Nk == 4)
      temp = (SBox[(temp>>24) & 0xFF]<<24) |
             (SBox[(temp>>16) & 0xFF]<<16) |
             (SBox[(temp>>8) & 0xFF]<<8) |
             (SBox[temp & 0xFF]);
    expandedKey[j] = expandedKey[j-Nk] ^ temp;
  }
  return expandedKey;
}

// Rijndael's round functions... 

function Round(state, roundKey) {
  byteSub(state, "encrypt");
  shiftRow(state, "encrypt");
  mixColumn(state, "encrypt");
  addRoundKey(state, roundKey);
}

function InverseRound(state, roundKey) {
  addRoundKey(state, roundKey);
  mixColumn(state, "decrypt");
  shiftRow(state, "decrypt");
  byteSub(state, "decrypt");
}

function FinalRound(state, roundKey) {
  byteSub(state, "encrypt");
  shiftRow(state, "encrypt");
  addRoundKey(state, roundKey);
}

function InverseFinalRound(state, roundKey){
  addRoundKey(state, roundKey);
  shiftRow(state, "decrypt");
  byteSub(state, "decrypt");  
}

// encrypt is the basic encryption function. It takes parameters
// block, an array of bytes representing a plaintext block, and expandedKey,
// an array of words representing the expanded key previously returned by
// keyExpansion(). The ciphertext block is returned as an array of bytes.

function encrypt(block, expandedKey) {
  var i;  
  if (!block || block.length*8 != blockSizeInBits)
     return; 
  if (!expandedKey)
     return;

  block = packBytes(block);
  addRoundKey(block, expandedKey);
  for (i=1; i<Nr; i++) 
    Round(block, expandedKey.slice(Nb*i, Nb*(i+1)));
  FinalRound(block, expandedKey.slice(Nb*Nr)); 
  return unpackBytes(block);
}

// decrypt is the basic decryption function. It takes parameters
// block, an array of bytes representing a ciphertext block, and expandedKey,
// an array of words representing the expanded key previously returned by
// keyExpansion(). The decrypted block is returned as an array of bytes.

function decrypt(block, expandedKey) {
  var i;
  if (!block || block.length*8 != blockSizeInBits)
     return;
  if (!expandedKey)
     return;

  block = packBytes(block);
  InverseFinalRound(block, expandedKey.slice(Nb*Nr)); 
  for (i = Nr - 1; i>0; i--) 
    InverseRound(block, expandedKey.slice(Nb*i, Nb*(i+1)));
  addRoundKey(block, expandedKey);
  return unpackBytes(block);
}

/* !NEEDED
// This method takes a byte array (byteArray) and converts it to a string by
// applying String.fromCharCode() to each value and concatenating the result.
// The resulting string is returned. Note that this function SKIPS zero bytes
// under the assumption that they are padding added in formatPlaintext().
// Obviously, do not invoke this method on raw data that can contain zero
// bytes. It is really only appropriate for printable ASCII/Latin-1 
// values. Roll your own function for more robust functionality :)

function byteArrayToString(byteArray) {
  var result = "";
  for(var i=0; i<byteArray.length; i++)
    if (byteArray[i] != 0) 
      result += String.fromCharCode(byteArray[i]);
  return result;
}
*/

// This function takes an array of bytes (byteArray) and converts them
// to a hexadecimal string. Array element 0 is found at the beginning of 
// the resulting string, high nibble first. Consecutive elements follow
// similarly, for example [16, 255] --> "10ff". The function returns a 
// string.

function byteArrayToHex(byteArray) {
  var result = "";
  if (!byteArray)
    return;
  for (var i=0; i<byteArray.length; i++)
    result += ((byteArray[i]<16) ? "0" : "") + byteArray[i].toString(16);

  return result;
}

// This function converts a string containing hexadecimal digits to an 
// array of bytes. The resulting byte array is filled in the order the
// values occur in the string, for example "10FF" --> [16, 255]. This
// function returns an array. 

function hexToByteArray(hexString) {
  var byteArray = [];
  if (hexString.length % 2)             // must have even length
    return;
  if (hexString.indexOf("0x") == 0 || hexString.indexOf("0X") == 0)
    hexString = hexString.substring(2);
  for (var i = 0; i<hexString.length; i += 2) 
    byteArray[Math.floor(i/2)] = parseInt(hexString.slice(i, i+2), 16);
  return byteArray;
}

// This function packs an array of bytes into the four row form defined by
// Rijndael. It assumes the length of the array of bytes is divisible by
// four. Bytes are filled in according to the Rijndael spec (starting with
// column 0, row 0 to 3). This function returns a 2d array.

function packBytes(octets) {
  var state = new Array();
  if (!octets || octets.length % 4)
    return;

  state[0] = new Array();  state[1] = new Array(); 
  state[2] = new Array();  state[3] = new Array();
  for (var j=0; j<octets.length; j+= 4) {
     state[0][j/4] = octets[j];
     state[1][j/4] = octets[j+1];
     state[2][j/4] = octets[j+2];
     state[3][j/4] = octets[j+3];
  }
  return state;  
}

// This function unpacks an array of bytes from the four row format preferred
// by Rijndael into a single 1d array of bytes. It assumes the input "packed"
// is a packed array. Bytes are filled in according to the Rijndael spec. 
// This function returns a 1d array of bytes.

function unpackBytes(packed) {
  var result = new Array();
  for (var j=0; j<packed[0].length; j++) {
    result[result.length] = packed[0][j];
    result[result.length] = packed[1][j];
    result[result.length] = packed[2][j];
    result[result.length] = packed[3][j];
  }
  return result;
}

// This function takes a prospective plaintext (string or array of bytes)
// and pads it with pseudorandom bytes if its length is not a multiple of the block 
// size. If plaintext is a string, it is converted to an array of bytes
// in the process. The type checking can be made much nicer using the 
// instanceof operator, but this operator is not available until IE5.0 so I 
// chose to use the heuristic below. 

function formatPlaintext(plaintext) {
	var bpb = blockSizeInBits / 8;               // bytes per block
	var fillWithRandomBits;
	var i;

	// if primitive string or String instance
	if ((!((typeof plaintext == "object") &&
		((typeof (plaintext[0])) == "number"))) &&
		((typeof plaintext == "string") || plaintext.indexOf))
	{
		plaintext = plaintext.split("");
		// Unicode issues here (ignoring high byte)
		for (i=0; i<plaintext.length; i++) {
			plaintext[i] = plaintext[i].charCodeAt(0) & 0xFF;
		}
	} 

	i = plaintext.length % bpb;
	if (i > 0) {
//alert("adding " + (bpb - 1) + " bytes");
//		plaintext = plaintext.concat(getRandomBytes(bpb - i));
		{
			var	paddingBytes;
			var ii,cc;
			
			paddingBytes = new Array();
			cc = bpb - i;
			for (ii=0; ii<cc; ii++) {
				paddingBytes[ii] = cc;
			}

//is("cc", cc);
//is(getRandomBytes(bpb - i) + "", paddingBytes + "");
			plaintext = plaintext.concat(paddingBytes);
		}
	}
  
	return plaintext;
}

// Returns an array containing "howMany" random bytes.

function getRandomBytes(howMany) {
    var i, bytes = new Array();

//alert("getting some random bytes");
    for (i = 0; i < howMany; i++) {
    	bytes[i] = prng.nextInt(255);
    }
    return bytes;
}

// rijndaelEncrypt(plaintext, key, mode)
// Encrypts the plaintext using the given key and in the given mode. 
// The parameter "plaintext" can either be a string or an array of bytes. 
// The parameter "key" must be an array of key bytes. If you have a hex 
// string representing the key, invoke hexToByteArray() on it to convert it 
// to an array of bytes. The third parameter "mode" is a string indicating
// the encryption mode to use, either "ECB" or "CBC". If the parameter is
// omitted, ECB is assumed.
// 
// An array of bytes representing the cihpertext is returned. To convert 
// this array to hex, invoke byteArrayToHex() on it.

function rijndaelEncrypt(plaintext, key, mode) {
  var expandedKey, i, aBlock;
  var bpb = blockSizeInBits / 8;          // bytes per block
  var ct;                                 // ciphertext

  if (!plaintext || !key)
    return;
  if (key.length*8 != keySizeInBits)
    return; 
  if (mode == "CBC") {
    ct = getRandomBytes(bpb);             // get IV
//dump("IV", byteArrayToHex(ct));
  } else {
    mode = "ECB";
    ct = new Array();
  }

  // convert plaintext to byte array and pad with zeros if necessary. 
  plaintext = formatPlaintext(plaintext);

  expandedKey = keyExpansion(key);
  
  for (var block = 0; block < plaintext.length / bpb; block++) {
    aBlock = plaintext.slice(block * bpb, (block + 1) * bpb);
    if (mode == "CBC") {
      for (var i = 0; i < bpb; i++) {
        aBlock[i] ^= ct[(block * bpb) + i];
      }
    }
    ct = ct.concat(encrypt(aBlock, expandedKey));
  }

  return ct;
}

// rijndaelDecrypt(ciphertext, key, mode)
// Decrypts the using the given key and mode. The parameter "ciphertext" 
// must be an array of bytes. The parameter "key" must be an array of key 
// bytes. If you have a hex string representing the ciphertext or key, 
// invoke hexToByteArray() on it to convert it to an array of bytes. The
// parameter "mode" is a string, either "CBC" or "ECB".
// 
// An array of bytes representing the plaintext is returned. To convert 
// this array to a hex string, invoke byteArrayToHex() on it. To convert it 
// to a string of characters, you can use byteArrayToString().

function rijndaelDecrypt(ciphertext, key, mode) {
  var expandedKey;
  var bpb = blockSizeInBits / 8;          // bytes per block
  var pt = new Array();                   // plaintext array
  var aBlock;                             // a decrypted block
  var block;                              // current block number

  if (!ciphertext || !key || typeof ciphertext == "string")
    return;
  if (key.length*8 != keySizeInBits)
    return; 
  if (!mode) {
    mode = "ECB";                         // assume ECB if mode omitted
  }

  expandedKey = keyExpansion(key);
 
  // work backwards to accomodate CBC mode 
  for (block=(ciphertext.length / bpb)-1; block>0; block--) {
    aBlock = 
     decrypt(ciphertext.slice(block*bpb,(block+1)*bpb), expandedKey);
    if (mode == "CBC") 
      for (var i=0; i<bpb; i++) 
        pt[(block-1)*bpb + i] = aBlock[i] ^ ciphertext[(block-1)*bpb + i];
    else 
      pt = aBlock.concat(pt);
  }

  // do last block if ECB (skips the IV in CBC)
  if (mode == "ECB")
    pt = decrypt(ciphertext.slice(0, bpb), expandedKey).concat(pt);

  return pt;
}

//#############################################################################
//	Downloaded on March 30, 2006 from http://www.fourmilab.ch/javascrypt/javascrypt.zip (utf-8.js)
//#############################################################################


    /*	Encoding and decoding of Unicode character strings as
    	UTF-8 byte streams.  */
	
    //	UNICODE_TO_UTF8  --  Encode Unicode argument string as UTF-8 return value

    function unicode_to_utf8(s) {
	var utf8 = "";
	
	for (var n = 0; n < s.length; n++) {
            var c = s.charCodeAt(n);

            if (c <= 0x7F) {
	    	//  0x00 - 0x7F:  Emit as single byte, unchanged
        	utf8 += String.fromCharCode(c);
            } else if ((c >= 0x80) && (c <= 0x7FF)) {
	    	//  0x80 - 0x7FF:  Output as two byte code, 0xC0 in first byte
		//  	    	    	    	    	    0x80 in second byte
        	utf8 += String.fromCharCode((c >> 6) | 0xC0);
        	utf8 += String.fromCharCode((c & 0x3F) | 0x80);
            } else {
	    	// 0x800 - 0xFFFF:  Output as three bytes, 0xE0 in first byte
		//  	    	    	    	    	   0x80 in second byte
		//  	    	    	    	    	   0x80 in third byte
        	utf8 += String.fromCharCode((c >> 12) | 0xE0);
        	utf8 += String.fromCharCode(((c >> 6) & 0x3F) | 0x80);
        	utf8 += String.fromCharCode((c & 0x3F) | 0x80);
            }
	}
	return utf8;
    }

    //	UTF8_TO_UNICODE  --  Decode UTF-8 argument into Unicode string return value

    function utf8_to_unicode(utf8) {
	var s = "", i = 0, b1, b2, b2;

	while (i < utf8.length) {
            b1 = utf8.charCodeAt(i);
            if (b1 < 0x80) {	    // One byte code: 0x00 0x7F
        	s += String.fromCharCode(b1);
        	i++;
            } else if((b1 >= 0xC0) && (b1 < 0xE0)) {	// Two byte code: 0x80 - 0x7FF
        	b2 = utf8.charCodeAt(i + 1);
        	s += String.fromCharCode(((b1 & 0x1F) << 6) | (b2 & 0x3F));
        	i += 2;
            } else {	    	    // Three byte code: 0x800 - 0xFFFF
        	b2 = utf8.charCodeAt(i + 1);
		b3 = utf8.charCodeAt(i + 2);
        	s += String.fromCharCode(((b1 & 0xF) << 12) |
		    	    	    	 ((b2 & 0x3F) << 6) |
					 (b3 & 0x3F));
        	i += 3;
            }
	}
	return s;
    }

    /*	ENCODE_UTF8  --  Encode string as UTF8 only if it contains
			 a character of 0x9D (Unicode OPERATING
			 SYSTEM COMMAND) or a character greater
			 than 0xFF.  This permits all strings
			 consisting exclusively of 8 bit
			 graphic characters to be encoded as
			 themselves.  We choose 0x9D as the sentinel
			 character as opposed to one of the more
			 logical PRIVATE USE characters because 0x9D
			 is not overloaded by the regrettable
			 "Windows-1252" character set.  Now such characters
			 don't belong in JavaScript strings, but you never
			 know what somebody is going to paste into a
			 text box, so this choice keeps Windows-encoded
			 strings from bloating to UTF-8 encoding.  */
			 
    function encode_utf8(s) {
    	var i, necessary = false;
	
	for (i = 0; i < s.length; i++) {
	    if ((s.charCodeAt(i) == 0x9D) ||
	    	(s.charCodeAt(i) > 0xFF)) {
	    	necessary = true;
		break;
	    }
	}
	if (!necessary) {
	    return s;
	}
	return String.fromCharCode(0x9D) + unicode_to_utf8(s);
    }
    
    /*  DECODE_UTF8  --  Decode a string encoded with encode_utf8
			 above.  If the string begins with the
			 sentinel character 0x9D (OPERATING
			 SYSTEM COMMAND), then we decode the
			 balance as a UTF-8 stream.  Otherwise,
			 the string is output unchanged, as
			 it's guaranteed to contain only 8 bit
			 characters excluding 0x9D.  */
			 
    function decode_utf8(s) {
    	if ((s.length > 0) && (s.charCodeAt(0) == 0x9D)) {
	    return utf8_to_unicode(s.substring(1));
	}
	return s;
    }


//#############################################################################
//	Downloaded on April 26, 2006 from http://pajhome.org.uk/crypt/md5/md5.js
//#############################################################################

/*
 * A JavaScript implementation of the RSA Data Security, Inc. MD5 Message
 * Digest Algorithm, as defined in RFC 1321.
 * Version 2.1 Copyright (C) Paul Johnston 1999 - 2002.
 * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet
 * Distributed under the BSD License
 * See http://pajhome.org.uk/crypt/md5 for more info.
 */

/*
 * Configurable variables. You may need to tweak these to be compatible with
 * the server-side, but the defaults work in most cases.
 */
var hexcase = 0;  /* hex output format. 0 - lowercase; 1 - uppercase        */
var b64pad  = ""; /* base-64 pad character. "=" for strict RFC compliance   */
var chrsz   = 8;  /* bits per input character. 8 - ASCII; 16 - Unicode      */

/*
 * These are the functions you'll usually want to call
 * They take string arguments and return either hex or base-64 encoded strings
 */
function hex_md5(s){ return binl2hex(core_md5(str2binl(s), s.length * chrsz));}
function b64_md5(s){ return binl2b64(core_md5(str2binl(s), s.length * chrsz));}
function str_md5(s){ return binl2str(core_md5(str2binl(s), s.length * chrsz));}
function hex_hmac_md5(key, data) { return binl2hex(core_hmac_md5(key, data)); }
function b64_hmac_md5(key, data) { return binl2b64(core_hmac_md5(key, data)); }
function str_hmac_md5(key, data) { return binl2str(core_hmac_md5(key, data)); }

/*
 * Perform a simple self-test to see if the VM is working
 */
function md5_vm_test()
{
  return hex_md5("abc") == "900150983cd24fb0d6963f7d28e17f72";
}

/*
 * Calculate the MD5 of an array of little-endian words, and a bit length
 */
function core_md5(x, len)
{
  /* append padding */
  x[len >> 5] |= 0x80 << ((len) % 32);
  x[(((len + 64) >>> 9) << 4) + 14] = len;

  var a =  1732584193;
  var b = -271733879;
  var c = -1732584194;
  var d =  271733878;

  for(var i = 0; i < x.length; i += 16)
  {
    var olda = a;
    var oldb = b;
    var oldc = c;
    var oldd = d;

    a = md5_ff(a, b, c, d, x[i+ 0], 7 , -680876936);
    d = md5_ff(d, a, b, c, x[i+ 1], 12, -389564586);
    c = md5_ff(c, d, a, b, x[i+ 2], 17,  606105819);
    b = md5_ff(b, c, d, a, x[i+ 3], 22, -1044525330);
    a = md5_ff(a, b, c, d, x[i+ 4], 7 , -176418897);
    d = md5_ff(d, a, b, c, x[i+ 5], 12,  1200080426);
    c = md5_ff(c, d, a, b, x[i+ 6], 17, -1473231341);
    b = md5_ff(b, c, d, a, x[i+ 7], 22, -45705983);
    a = md5_ff(a, b, c, d, x[i+ 8], 7 ,  1770035416);
    d = md5_ff(d, a, b, c, x[i+ 9], 12, -1958414417);
    c = md5_ff(c, d, a, b, x[i+10], 17, -42063);
    b = md5_ff(b, c, d, a, x[i+11], 22, -1990404162);
    a = md5_ff(a, b, c, d, x[i+12], 7 ,  1804603682);
    d = md5_ff(d, a, b, c, x[i+13], 12, -40341101);
    c = md5_ff(c, d, a, b, x[i+14], 17, -1502002290);
    b = md5_ff(b, c, d, a, x[i+15], 22,  1236535329);

    a = md5_gg(a, b, c, d, x[i+ 1], 5 , -165796510);
    d = md5_gg(d, a, b, c, x[i+ 6], 9 , -1069501632);
    c = md5_gg(c, d, a, b, x[i+11], 14,  643717713);
    b = md5_gg(b, c, d, a, x[i+ 0], 20, -373897302);
    a = md5_gg(a, b, c, d, x[i+ 5], 5 , -701558691);
    d = md5_gg(d, a, b, c, x[i+10], 9 ,  38016083);
    c = md5_gg(c, d, a, b, x[i+15], 14, -660478335);
    b = md5_gg(b, c, d, a, x[i+ 4], 20, -405537848);
    a = md5_gg(a, b, c, d, x[i+ 9], 5 ,  568446438);
    d = md5_gg(d, a, b, c, x[i+14], 9 , -1019803690);
    c = md5_gg(c, d, a, b, x[i+ 3], 14, -187363961);
    b = md5_gg(b, c, d, a, x[i+ 8], 20,  1163531501);
    a = md5_gg(a, b, c, d, x[i+13], 5 , -1444681467);
    d = md5_gg(d, a, b, c, x[i+ 2], 9 , -51403784);
    c = md5_gg(c, d, a, b, x[i+ 7], 14,  1735328473);
    b = md5_gg(b, c, d, a, x[i+12], 20, -1926607734);

    a = md5_hh(a, b, c, d, x[i+ 5], 4 , -378558);
    d = md5_hh(d, a, b, c, x[i+ 8], 11, -2022574463);
    c = md5_hh(c, d, a, b, x[i+11], 16,  1839030562);
    b = md5_hh(b, c, d, a, x[i+14], 23, -35309556);
    a = md5_hh(a, b, c, d, x[i+ 1], 4 , -1530992060);
    d = md5_hh(d, a, b, c, x[i+ 4], 11,  1272893353);
    c = md5_hh(c, d, a, b, x[i+ 7], 16, -155497632);
    b = md5_hh(b, c, d, a, x[i+10], 23, -1094730640);
    a = md5_hh(a, b, c, d, x[i+13], 4 ,  681279174);
    d = md5_hh(d, a, b, c, x[i+ 0], 11, -358537222);
    c = md5_hh(c, d, a, b, x[i+ 3], 16, -722521979);
    b = md5_hh(b, c, d, a, x[i+ 6], 23,  76029189);
    a = md5_hh(a, b, c, d, x[i+ 9], 4 , -640364487);
    d = md5_hh(d, a, b, c, x[i+12], 11, -421815835);
    c = md5_hh(c, d, a, b, x[i+15], 16,  530742520);
    b = md5_hh(b, c, d, a, x[i+ 2], 23, -995338651);

    a = md5_ii(a, b, c, d, x[i+ 0], 6 , -198630844);
    d = md5_ii(d, a, b, c, x[i+ 7], 10,  1126891415);
    c = md5_ii(c, d, a, b, x[i+14], 15, -1416354905);
    b = md5_ii(b, c, d, a, x[i+ 5], 21, -57434055);
    a = md5_ii(a, b, c, d, x[i+12], 6 ,  1700485571);
    d = md5_ii(d, a, b, c, x[i+ 3], 10, -1894986606);
    c = md5_ii(c, d, a, b, x[i+10], 15, -1051523);
    b = md5_ii(b, c, d, a, x[i+ 1], 21, -2054922799);
    a = md5_ii(a, b, c, d, x[i+ 8], 6 ,  1873313359);
    d = md5_ii(d, a, b, c, x[i+15], 10, -30611744);
    c = md5_ii(c, d, a, b, x[i+ 6], 15, -1560198380);
    b = md5_ii(b, c, d, a, x[i+13], 21,  1309151649);
    a = md5_ii(a, b, c, d, x[i+ 4], 6 , -145523070);
    d = md5_ii(d, a, b, c, x[i+11], 10, -1120210379);
    c = md5_ii(c, d, a, b, x[i+ 2], 15,  718787259);
    b = md5_ii(b, c, d, a, x[i+ 9], 21, -343485551);

    a = safe_add(a, olda);
    b = safe_add(b, oldb);
    c = safe_add(c, oldc);
    d = safe_add(d, oldd);
  }
  return Array(a, b, c, d);

}

/*
 * These functions implement the four basic operations the algorithm uses.
 */
function md5_cmn(q, a, b, x, s, t)
{
  return safe_add(bit_rol(safe_add(safe_add(a, q), safe_add(x, t)), s),b);
}
function md5_ff(a, b, c, d, x, s, t)
{
  return md5_cmn((b & c) | ((~b) & d), a, b, x, s, t);
}
function md5_gg(a, b, c, d, x, s, t)
{
  return md5_cmn((b & d) | (c & (~d)), a, b, x, s, t);
}
function md5_hh(a, b, c, d, x, s, t)
{
  return md5_cmn(b ^ c ^ d, a, b, x, s, t);
}
function md5_ii(a, b, c, d, x, s, t)
{
  return md5_cmn(c ^ (b | (~d)), a, b, x, s, t);
}

/*
 * Calculate the HMAC-MD5, of a key and some data
 */
function core_hmac_md5(key, data)
{
  var bkey = str2binl(key);
  if(bkey.length > 16) bkey = core_md5(bkey, key.length * chrsz);

  var ipad = Array(16), opad = Array(16);
  for(var i = 0; i < 16; i++)
  {
    ipad[i] = bkey[i] ^ 0x36363636;
    opad[i] = bkey[i] ^ 0x5C5C5C5C;
  }

  var hash = core_md5(ipad.concat(str2binl(data)), 512 + data.length * chrsz);
  return core_md5(opad.concat(hash), 512 + 128);
}

/*
 * Add integers, wrapping at 2^32. This uses 16-bit operations internally
 * to work around bugs in some JS interpreters.
 */
function safe_add(x, y)
{
  var lsw = (x & 0xFFFF) + (y & 0xFFFF);
  var msw = (x >> 16) + (y >> 16) + (lsw >> 16);
  return (msw << 16) | (lsw & 0xFFFF);
}

/*
 * Bitwise rotate a 32-bit number to the left.
 */
function bit_rol(num, cnt)
{
  return (num << cnt) | (num >>> (32 - cnt));
}

/*
 * Convert a string to an array of little-endian words
 * If chrsz is ASCII, characters >255 have their hi-byte silently ignored.
 */
function str2binl(str)
{
  var bin = Array();
  var mask = (1 << chrsz) - 1;
  for(var i = 0; i < str.length * chrsz; i += chrsz)
    bin[i>>5] |= (str.charCodeAt(i / chrsz) & mask) << (i%32);
  return bin;
}

/*
 * Convert an array of little-endian words to a string
 */
function binl2str(bin)
{
  var str = "";
  var mask = (1 << chrsz) - 1;
  for(var i = 0; i < bin.length * 32; i += chrsz)
    str += String.fromCharCode((bin[i>>5] >>> (i % 32)) & mask);
  return str;
}

/*
 * Convert an array of little-endian words to a hex string.
 */
function binl2hex(binarray)
{
  var hex_tab = hexcase ? "0123456789ABCDEF" : "0123456789abcdef";
  var str = "";
  for(var i = 0; i < binarray.length * 4; i++)
  {
    str += hex_tab.charAt((binarray[i>>2] >> ((i%4)*8+4)) & 0xF) +
           hex_tab.charAt((binarray[i>>2] >> ((i%4)*8  )) & 0xF);
  }
  return str;
}

/*
 * Convert an array of little-endian words to a base-64 string
 */
function binl2b64(binarray)
{
  var tab = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  var str = "";
  for(var i = 0; i < binarray.length * 4; i += 3)
  {
    var triplet = (((binarray[i   >> 2] >> 8 * ( i   %4)) & 0xFF) << 16)
                | (((binarray[i+1 >> 2] >> 8 * ((i+1)%4)) & 0xFF) << 8 )
                |  ((binarray[i+2 >> 2] >> 8 * ((i+2)%4)) & 0xFF);
    for(var j = 0; j < 4; j++)
    {
      if(i * 8 + j * 6 > binarray.length * 32) str += b64pad;
      else str += tab.charAt((triplet >> 6*(3-j)) & 0x3F);
    }
  }
  return str;
}


//#############################################################################
//#############################################################################
//#############################################################################



MochiKit.Base.update(Clipperz.Crypto.Base, {

	'__repr__': function () {
		return "[" + this.NAME + " " + this.VERSION + "]";
	},

	'toString': function () {
		return this.__repr__();
	},

	//-----------------------------------------------------------------------------

	'encryptUsingSecretKey': function (aKey, aMessage) {
//Clipperz.Profile.start("Clipperz.Crypto.Base.encryptUsingSecretKey");
		var result;
		var plaintext;
		var	header;
		var	key;
		
		key = hexToByteArray(Clipperz.Crypto.Base.computeHashValue(aKey));

		addEntropyTime();
    	prng = new AESprng(keyFromEntropy());

		plaintext = encode_utf8(aMessage);

		header = Clipperz.Base.byteArrayToString(hexToByteArray(Clipperz.Crypto.Base.computeMD5HashValue(plaintext)));
		
		//  Add message length in bytes to header
		i = plaintext.length;
		header += String.fromCharCode(i >>> 24);
		header += String.fromCharCode(i >>> 16);
		header += String.fromCharCode(i >>> 8);
		header += String.fromCharCode(i & 0xFF);

    	//	The format of the actual message passed to rijndaelEncrypt
	    //	is:
	    //
	    //	    Bytes   	Content
		//		 0-15   	MD5 signature of plaintext
		//		16-19   	Length of plaintext, big-endian order
		//		20-end  	Plaintext
		//    
	    //	Note that this message will be padded with zero bytes
	    //	to an integral number of AES blocks (blockSizeInBits / 8).
	    //	This does not include the initial vector for CBC
	    //	encryption, which is added internally by rijndaelEncrypt.
		result = byteArrayToHex(rijndaelEncrypt(header + plaintext, key, "CBC"));

    	delete prng;

//Clipperz.Profile.stop("Clipperz.Crypto.Base.encryptUsingSecretKey");
		return result;
	},

	//.............................................................................

	'decryptUsingSecretKey': function (aKey, aMessage) {
//Clipperz.Profile.start("Clipperz.Crypto.Base.decryptUsingSecretKey");
		var	key;
		var decryptedText;
		var	textLength;
		var	header;
		var	headerDigest;
		var plaintext;
		var i;
		
		key = hexToByteArray(Clipperz.Crypto.Base.computeHashValue(aKey));

		decryptedText = rijndaelDecrypt(hexToByteArray(aMessage), key, "CBC");

		header = decryptedText.slice(0, 20);
		decryptedText = decryptedText.slice(20);

		headerDigest = byteArrayToHex(header.slice(0,16));
		textLength = (header[16] << 24) | (header[17] << 16) | (header[18] << 8) | header[19];

		if ((textLength < 0) || (textLength > decryptedText.length)) {
//			jslog.warning("Message (length " + decryptedText.length + ") truncated.  " + textLength + " characters expected.");
			//	Try to sauve qui peut by setting length to entire message
			textLength = decryptedText.length;
		}

		plainText = "";

		for (i=0; i<textLength; i++) {
			plainText += String.fromCharCode(decryptedText[i]);
		}

		if (Clipperz.Crypto.Base.computeMD5HashValue(plainText) != headerDigest) {
//			jslog.warning("Message corrupted.  Checksum of decrypted message does not match.");
			throw Clipperz.Crypto.Base.exception.CorruptedMessage;
//			throw new Error("Message corrupted.  Checksum of decrypted message does not match. Parsed result: " + decode_utf8(plainText));
		}

		//  That's it; plug plaintext into the result field

		result = decode_utf8(plainText);
		
//Clipperz.Profile.stop("Clipperz.Crypto.Base.decryptUsingSecretKey");
		return result;
	},

	//-----------------------------------------------------------------------------

	'computeHashValue': function (aMessage) {
//Clipperz.Profile.start("Clipperz.Crypto.Base.computeHashValue");
		var	result;

		result = hex_sha256(aMessage);
//Clipperz.Profile.stop("Clipperz.Crypto.Base.computeHashValue");

		return result;
	},

	//.........................................................................
	
	'computeMD5HashValue': function (aMessage) {
		var	result;
//Clipperz.Profile.start("Clipperz.Crypto.Base.computeMD5HashValue");
		result = hex_md5(aMessage);
//Clipperz.Profile.stop("Clipperz.Crypto.Base.computeMD5HashValue");

		return result;
	},

	//-----------------------------------------------------------------------------

	'generateRandomSeed': function () {
//Clipperz.Profile.start("Clipperz.Crypto.Base.generateRandomSeed");
		var	result;
		var seed;
		var prng;
		var charA;
		var i;

		addEntropyTime();

		seed = keyFromEntropy();
		prng = new AESprng(seed);

		result = "";
		charA = ("A").charCodeAt(0);

		for (i = 0; i < 64; i++) {
			result += String.fromCharCode(charA + prng.nextInt(25));
		}
		
		delete prng;
		
		result = Clipperz.Crypto.Base.computeHashValue(result);
		
//Clipperz.Profile.stop("Clipperz.Crypto.Base.generateRandomSeed");
		return result;
	},

	//-----------------------------------------------------------------------------

	'exception': {
		'CorruptedMessage': new MochiKit.Base.NamedError("Clipperz.Crypto.Base.exception.CorruptedMessage") 
	},

	//.........................................................................
	__syntaxFix__: "syntax fix"
});

