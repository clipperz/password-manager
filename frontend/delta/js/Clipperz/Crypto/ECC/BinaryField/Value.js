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

//try { if (typeof(Clipperz.ByteArray) == 'undefined') { throw ""; }} catch (e) {
//	throw "Clipperz.Crypto.ECC depends on Clipperz.ByteArray!";
//}  
if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.Crypto) == 'undefined') { Clipperz.Crypto = {}; }
if (typeof(Clipperz.Crypto.ECC) == 'undefined') { Clipperz.Crypto.ECC = {}; }
if (typeof(Clipperz.Crypto.ECC.BinaryField) == 'undefined') { Clipperz.Crypto.ECC.BinaryField = {}; }

Clipperz.Crypto.ECC.BinaryField.Value = function(aValue, aBase, aBitSize) {
	if (aValue.constructor == String) {
		var	value;
		var	stringLength;
		var numberOfWords;
		var	i,c;
	
		if (aBase != 16) {
			throw Clipperz.Crypto.ECC.BinaryField.Value.exception.UnsupportedBase;
		}

		value = aValue.replace(/ /g, '');
		stringLength = value.length;
		numberOfWords = Math.ceil(stringLength / 8);
		this._value = new Array(numberOfWords);
	
		c = numberOfWords;
		for (i=0; i<c; i++) {
			var	word;
		
			if (i < (c-1)) {
				word = parseInt(value.substr(stringLength-((i+1)*8), 8), 16);
			} else {
				word = parseInt(value.substr(0, stringLength-(i*8)), 16);
			}
			
			this._value[i] = word;
		}
	} else if (aValue.constructor == Array) {
		var itemsToCopy;

		itemsToCopy = aValue.length;
		while (aValue[itemsToCopy - 1] == 0) {
			itemsToCopy --;
		}

		this._value = aValue.slice(0, itemsToCopy);
	} else if (aValue.constructor == Number) {
		this._value = [aValue];
	} else {
//		throw Clipperz.Crypto.ECC.BinaryField.Value.exception.UnsupportedConstructorValueType;
	}
	
	this._bitSize == aBitSize || null;

	return this;
}

Clipperz.Crypto.ECC.BinaryField.Value.prototype = MochiKit.Base.update(null, {

	'value': function() {
		return this._value;
	},

	//-----------------------------------------------------------------------------
	
	'wordSize': function() {
		return this._value.length
	},

	//-----------------------------------------------------------------------------

	'clone': function() {
		return new Clipperz.Crypto.ECC.BinaryField.Value(this._value.slice(0), null, this._bitSize);
	},
	
	//-----------------------------------------------------------------------------

	'isZero': function() {
		return (this.compare(Clipperz.Crypto.ECC.BinaryField.Value.O) == 0);
	},

	//-----------------------------------------------------------------------------

	'asString': function(aBase) {
		var	result;
		var i,c;
		
		if (aBase != 16) {
			throw Clipperz.Crypto.ECC.BinaryField.Value.exception.UnsupportedBase;
		}
		
		result = "";
		c = this.wordSize();
		for (i=0; i<c; i++) {
			var	wordAsString;
			
//			wordAsString = ("00000000" + this.value()[i].toString(16));
			wordAsString = ("00000000" + this._value[i].toString(16));
			wordAsString = wordAsString.substring(wordAsString.length - 8);
			result = wordAsString + result;
		}
		
		result = result.replace(/^(00)*/, "");
		
		if (result == "") {
			result = "0";
		}
		
		return result;
	},
	
	//-----------------------------------------------------------------------------

	'shiftLeft': function(aNumberOfBitsToShift) {
		//	this method seems like it is never called. :-(
		return new Clipperz.Crypto.ECC.BinaryField.Value(Clipperz.Crypto.ECC.BinaryField.Value._shiftLeft(this._value, aNumberOfBitsToShift));
	},

	//-----------------------------------------------------------------------------

	'bitSize': function() {
		if (this._bitSize == null) {
			this._bitSize = Clipperz.Crypto.ECC.BinaryField.Value._bitSize(this._value);
		}

		return this._bitSize;
	},
	
	//-----------------------------------------------------------------------------

	'isBitSet': function(aBitPosition) {
		return Clipperz.Crypto.ECC.BinaryField.Value._isBitSet(this._value, aBitPosition);
	},
	
	//-----------------------------------------------------------------------------

	'xor': function(aValue) {
		return new Clipperz.Crypto.ECC.BinaryField.Value(Clipperz.Crypto.ECC.BinaryField.Value._xor(this._value, aValue._value));
	}, 

	//-----------------------------------------------------------------------------

	'compare': function(aValue) {
		return Clipperz.Crypto.ECC.BinaryField.Value._compare(this._value, aValue._value);
	},
	
	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

Clipperz.Crypto.ECC.BinaryField.Value.O = new Clipperz.Crypto.ECC.BinaryField.Value('0', 16);
Clipperz.Crypto.ECC.BinaryField.Value.I = new Clipperz.Crypto.ECC.BinaryField.Value('1', 16);

Clipperz.Crypto.ECC.BinaryField.Value._xor = function(a, b, aFirstItemOffset) {
	var result;
	var resultSize;
	var i,c;
	var firstItemOffset;
	
	firstItemOffset = aFirstItemOffset || 0;
	resultSize = Math.max((a.length - firstItemOffset), b.length) + firstItemOffset;

	result = new Array(resultSize);
	
	c = firstItemOffset;
	for (i=0; i<c; i++) {
		result[i] = a[i];
	}

	c = resultSize;
	for (i=firstItemOffset; i<c; i++) {
		result[i] = (((a[i] || 0) ^ (b[i - firstItemOffset] || 0)) >>> 0);
	}
	
	return result;
};

Clipperz.Crypto.ECC.BinaryField.Value._overwriteXor = function(a, b, aFirstItemOffset) {
	var i,c;
	var firstItemOffset;
	
	firstItemOffset = aFirstItemOffset || 0;

	c = Math.max((a.length - firstItemOffset), b.length) + firstItemOffset;
	for (i=firstItemOffset; i<c; i++) {
		a[i] = (((a[i] || 0) ^ (b[i - firstItemOffset] || 0)) >>> 0);
	}
};

Clipperz.Crypto.ECC.BinaryField.Value._shiftLeft = function(aWordArray, aNumberOfBitsToShift) {
	var numberOfWordsToShift;
	var	numberOfBitsToShift;
	var result;
	var	overflowValue;
	var nextOverflowValue;
	var	i,c;

	numberOfWordsToShift = Math.floor(aNumberOfBitsToShift / 32);
	numberOfBitsToShift = aNumberOfBitsToShift % 32;

	result = new Array(aWordArray.length + numberOfWordsToShift);
	
	c = numberOfWordsToShift;
	for (i=0; i<c; i++) {
		result[i] = 0;
	}
	
	overflowValue = 0;
	nextOverflowValue = 0;
	
	c = aWordArray.length;
	for (i=0; i<c; i++) {
		var	value;
		var	resultWord;

//		value = this.value()[i];
		value = aWordArray[i];

		if (numberOfBitsToShift > 0) {
			nextOverflowValue = (value >>> (32 - numberOfBitsToShift));
			value = value & (0xffffffff >>> numberOfBitsToShift);
			resultWord = (((value << numberOfBitsToShift) | overflowValue) >>> 0);
		} else {
			resultWord = value;
		}
		
		result[i+numberOfWordsToShift] = resultWord;
		overflowValue = nextOverflowValue;
	}

	if (overflowValue != 0) {
		result[aWordArray.length + numberOfWordsToShift] = overflowValue;
	}

	return result;
};

Clipperz.Crypto.ECC.BinaryField.Value._overwriteShiftLeft = function(aWordArray, aNumberOfBitsToShift) {
	var numberOfWordsToShift;
	var	numberOfBitsToShift;
	var result;
	var	overflowValue;
	var	i,c;

	numberOfWordsToShift = Math.floor(aNumberOfBitsToShift / 32);
	numberOfBitsToShift = aNumberOfBitsToShift % 32;

	result = new Array(aWordArray.length + numberOfWordsToShift);
	
	c = numberOfWordsToShift;
	for (i=0; i<c; i++) {
		result[i] = 0;
	}
	
	overflowValue = 0;
	nextOverflowValue = 0;
	
	c = aWordArray.length;
	for (i=0; i<c; i++) {
		var	value;
		var	resultWord;

//		value = this.value()[i];
		value = aWordArray[i];

		if (numberOfBitsToShift > 0) {
			var nextOverflowValue;
		
			nextOverflowValue = (value >>> (32 - numberOfBitsToShift));
			value = value & (0xffffffff >>> numberOfBitsToShift);
			resultWord = (((value << numberOfBitsToShift) | overflowValue) >>> 0);
		} else {
			resultWord = value;
		}
		
		result[i+numberOfWordsToShift] = resultWord;
		overflowValue = nextOverflowValue;
	}

	if (overflowValue != 0) {
		result[aWordArray.length + numberOfWordsToShift] = overflowValue;
	}

	return result;
};

Clipperz.Crypto.ECC.BinaryField.Value._bitSize = function(aWordArray) {
	var	result;
	var	notNullElements;
	var mostValuableWord;
	var matchingBitsInMostImportantWord;
	var mask;
	var i,c;

	notNullElements = aWordArray.length;
	
	if ((aWordArray.length == 1) && (aWordArray[0] == 0)) {
		result = 0;
	} else {
			notNullElements --;
		while((notNullElements > 0) && (aWordArray[notNullElements] == 0)) {
			notNullElements --;
		}
	
		result = notNullElements * 32;
		mostValuableWord = aWordArray[notNullElements];

		matchingBits = 32;
		mask = 0x80000000;
	
		while ((matchingBits > 0) && ((mostValuableWord & mask) == 0)) {
			matchingBits --;
			mask >>>= 1;
		}
	
		result += matchingBits;
	}
	
	return result;
};

Clipperz.Crypto.ECC.BinaryField.Value._isBitSet = function(aWordArray, aBitPosition) {
	var result;
	var	byteIndex;
	var bitIndexInSelectedByte;

	byteIndex = Math.floor(aBitPosition / 32);
	bitIndexInSelectedByte = aBitPosition % 32;
	
	if (byteIndex <= aWordArray.length) {
		result = ((aWordArray[byteIndex] & (1 << bitIndexInSelectedByte)) != 0);
	} else {
		result = false;
	}

	return result;
};

Clipperz.Crypto.ECC.BinaryField.Value._compare = function(a,b) {
	var	result;
	var i,c;
	
	result = MochiKit.Base.compare(a.length, b.length);

	c = a.length;
	for (i=0; (i<c) && (result==0); i++) {
		result = MochiKit.Base.compare(a[c-i-1], b[c-i-1]);
	}
	
	return result;
};


Clipperz.Crypto.ECC.BinaryField.Value['exception']= {
	'UnsupportedBase':					new MochiKit.Base.NamedError("Clipperz.Crypto.ECC.BinaryField.Value.exception.UnsupportedBase"),
	'UnsupportedConstructorValueType':	new MochiKit.Base.NamedError("Clipperz.Crypto.ECC.BinaryField.Value.exception.UnsupportedConstructorValueType")
};
