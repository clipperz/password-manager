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

/*
try { if (typeof(Clipperz.ByteArray) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.Crypto.ECC depends on Clipperz.ByteArray!";
}  

if (typeof(Clipperz.Crypto.ECC) == 'undefined') { Clipperz.Crypto.ECC = {}; }


//#############################################################################

Clipperz.Crypto.ECC.BinaryField = {};

//#############################################################################

Clipperz.Crypto.ECC.BinaryField.AbstractValue = function(aValue, aBase) {
	return this;
}

Clipperz.Crypto.ECC.BinaryField.AbstractValue.prototype = MochiKit.Base.update(null, {

	'asString': function(aBase) {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'isZero': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'shiftLeft': function(aNumberOfBitsToShift) {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'bitSize': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'isBitSet': function(aBitPosition) {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'xor': function(aValue) {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'compare': function(aValue) {
		throw Clipperz.Base.exception.AbstractMethod;
	},
	
	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

//*****************************************************************************
/ *
Clipperz.Crypto.ECC.BinaryField.BigIntValue = function(aValue, aBase) {
	this._value = new Clipperz.Crypto.BigInt(aValue, aBase);
	return this;
}

Clipperz.Crypto.ECC.BinaryField.BigIntValue.prototype = MochiKit.Base.update(new Clipperz.Crypto.ECC.BinaryField.AbstractValue(), {

	'value': function() {
		return this._value;
	},

	//-----------------------------------------------------------------------------
	
	'isZero': function() {
		return (this.value().compare(Clipperz.Crypto.ECC.BinaryField.BigIntValue.O) == 0);
	},

	//-----------------------------------------------------------------------------

	'asString': function(aBase) {
		return this.value().asString(aBase);
	},
	
	//-----------------------------------------------------------------------------

	'shiftLeft': function(aNumberOfBitsToShift) {
		return new Clipperz.Crypto.ECC.BinaryField.BigIntValue(this.value().shiftLeft(aNumberOfBitsToShift));
	},
	
	//-----------------------------------------------------------------------------

	'bitSize': function() {
		return this.value().bitSize();
	},
	
	//-----------------------------------------------------------------------------

	'isBitSet': function(aBitPosition) {
		return this.value().isBitSet(aBitPosition);
	},
	
	//-----------------------------------------------------------------------------

	'xor': function(aValue) {
		return new Clipperz.Crypto.ECC.BinaryField.BigIntValue(this.value().xor(aValue.value()));
	},
	
	//-----------------------------------------------------------------------------

	'compare': function(aValue) {
		return this.value().compare(aValue.value());
	},
	
	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

Clipperz.Crypto.ECC.BinaryField.BigIntValue.O =  new Clipperz.Crypto.BigInt(0);
Clipperz.Crypto.ECC.BinaryField.BigIntValue.I =  new Clipperz.Crypto.BigInt(1);
* /
//*****************************************************************************

Clipperz.Crypto.ECC.BinaryField.WordArrayValue = function(aValue, aBase) {
	if (aValue.constructor == String) {
		var	value;
		var	stringLength;
		var numberOfWords;
		var	i,c;
	
		if (aBase != 16) {
			throw Clipperz.Crypto.ECC.BinaryField.WordArrayValue.exception.UnsupportedBase;
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
//		throw Clipperz.Crypto.ECC.BinaryField.WordArrayValue.exception.UnsupportedConstructorValueType;
	}
	
	return this;
}

Clipperz.Crypto.ECC.BinaryField.WordArrayValue.prototype = MochiKit.Base.update(new Clipperz.Crypto.ECC.BinaryField.AbstractValue(), {

	'value': function() {
		return this._value;
	},

	//-----------------------------------------------------------------------------
	
	'wordSize': function() {
		return this._value.length
	},

	//-----------------------------------------------------------------------------

	'clone': function() {
		return new Clipperz.Crypto.ECC.BinaryField.WordArrayValue(this._value.slice(0));
	},
	
	//-----------------------------------------------------------------------------

	'isZero': function() {
		return (this.compare(Clipperz.Crypto.ECC.BinaryField.WordArrayValue.O) == 0);
	},

	//-----------------------------------------------------------------------------

	'asString': function(aBase) {
		var	result;
		var i,c;
		
		if (aBase != 16) {
			throw Clipperz.Crypto.ECC.BinaryField.WordArrayValue.exception.UnsupportedBase;
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
		
		result = result.replace(/^(00)* SPACEs THAT SHOULD BE REMOVED TO FIX THIS REGEX /, "");
		
		if (result == "") {
			result = "0";
		}
		
		return result;
	},
	
	//-----------------------------------------------------------------------------

	'shiftLeft': function(aNumberOfBitsToShift) {
		return new Clipperz.Crypto.ECC.BinaryField.WordArrayValue(Clipperz.Crypto.ECC.BinaryField.WordArrayValue.shiftLeft(this._value, aNumberOfBitsToShift));
	},

	//-----------------------------------------------------------------------------

	'bitSize': function() {
		return Clipperz.Crypto.ECC.BinaryField.WordArrayValue.bitSize(this._value);
	},
	
	//-----------------------------------------------------------------------------

	'isBitSet': function(aBitPosition) {
		return Clipperz.Crypto.ECC.BinaryField.WordArrayValue.isBitSet(this._value, aBitPosition);
	},
	
	//-----------------------------------------------------------------------------

	'xor': function(aValue) {
		return new Clipperz.Crypto.ECC.BinaryField.WordArrayValue(Clipperz.Crypto.ECC.BinaryField.WordArrayValue.xor(this._value, aValue._value));
	}, 

	//-----------------------------------------------------------------------------

	'compare': function(aValue) {
		return Clipperz.Crypto.ECC.BinaryField.WordArrayValue.compare(this._value, aValue._value);
	},
	
	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

Clipperz.Crypto.ECC.BinaryField.WordArrayValue.O = new Clipperz.Crypto.ECC.BinaryField.WordArrayValue('0', 16);
Clipperz.Crypto.ECC.BinaryField.WordArrayValue.I = new Clipperz.Crypto.ECC.BinaryField.WordArrayValue('1', 16);

Clipperz.Crypto.ECC.BinaryField.WordArrayValue.xor = function(a, b) {
	var result;
	var resultSize;
	var i,c;
	
	resultSize = Math.max(a.length, b.length);
	
	result = new Array(resultSize);
	c = resultSize;
	for (i=0; i<c; i++) {
//		resultValue[i] = (((this.value()[i] || 0) ^ (aValue.value()[i] || 0)) >>> 0);
		result[i] = (((a[i] || 0) ^ (b[i] || 0)) >>> 0);
	}
	
	return result;
};

Clipperz.Crypto.ECC.BinaryField.WordArrayValue.shiftLeft = function(aWordArray, aNumberOfBitsToShift) {
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

Clipperz.Crypto.ECC.BinaryField.WordArrayValue.bitSize = function(aWordArray) {
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
		while((aWordArray[notNullElements - 1] == 0) && (notNullElements > 0)) {
			notNullElements --;
		}
	
		result = (notNullElements - 1) * 32;
		mostValuableWord = aWordArray[notNullElements - 1];

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

Clipperz.Crypto.ECC.BinaryField.WordArrayValue.isBitSet = function(aWordArray, aBitPosition) {
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

Clipperz.Crypto.ECC.BinaryField.WordArrayValue.compare = function(a,b) {
	var	result;
	var i,c;
	
	result = MochiKit.Base.compare(a.length, b.length);

	c = a.length;
	for (i=0; (i<c) && (result==0); i++) {
//console.log("compare[" + c + " - " + i + " - 1] " + this.value()[c-i-1] + ", " + aValue.value()[c-i-1]);
//		result = MochiKit.Base.compare(this.value()[c-i-1], aValue.value()[c-i-1]);
		result = MochiKit.Base.compare(a[c-i-1], b[c-i-1]);
	}
	
	return result;
};


Clipperz.Crypto.ECC.BinaryField.WordArrayValue['exception']= {
	'UnsupportedBase':					new MochiKit.Base.NamedError("Clipperz.Crypto.ECC.BinaryField.WordArrayValue.exception.UnsupportedBase"),
	'UnsupportedConstructorValueType':	new MochiKit.Base.NamedError("Clipperz.Crypto.ECC.BinaryField.WordArrayValue.exception.UnsupportedConstructorValueType")
};

//*****************************************************************************

//Clipperz.Crypto.ECC.BinaryField.Value	=	Clipperz.Crypto.ECC.BinaryField.BigIntValue;
Clipperz.Crypto.ECC.BinaryField.Value	=	Clipperz.Crypto.ECC.BinaryField.WordArrayValue;

//#############################################################################

Clipperz.Crypto.ECC.BinaryField.Point = function(args) {
	args = args || {};
	this._x = args.x;
	this._y = args.y;
	
	return this;
}

Clipperz.Crypto.ECC.BinaryField.Point.prototype = MochiKit.Base.update(null, {

	'asString': function() {
		return "Clipperz.Crypto.ECC.BinaryField.Point (" + this.x() + ", " + this.y() + ")";
	},

	//-----------------------------------------------------------------------------

	'x': function() {
		return this._x;
	},
	
	'y': function() {
		return this._y;
	},

	//-----------------------------------------------------------------------------

	'isZero': function() {
		return (this.x().isZero() && this.y().isZero())
	},
	
	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

//#############################################################################

Clipperz.Crypto.ECC.BinaryField.FiniteField = function(args) {
	args = args || {};
	this._modulus = args.modulus;

	return this;
}

Clipperz.Crypto.ECC.BinaryField.FiniteField.prototype = MochiKit.Base.update(null, {

	'asString': function() {
		return "Clipperz.Crypto.ECC.BinaryField.FiniteField (" + this.modulus().asString() + ")";
	},

	//-----------------------------------------------------------------------------

	'modulus': function() {
		return this._modulus;
	},
	
	//-----------------------------------------------------------------------------

	'_module': function(aValue) {
		var	result;
		var modulusComparison;
//console.log(">>> binaryField.finiteField.(standard)module");
		
		modulusComparison = Clipperz.Crypto.ECC.BinaryField.WordArrayValue.compare(aValue, this.modulus()._value);

		if (modulusComparison < 0) {
			result = aValue;
		} else if (modulusComparison == 0) {
			result = [0];
		} else {
			var modulusBitSize;
			var resultBitSize;
			
			result = aValue;

			modulusBitSize = this.modulus().bitSize();
			resultBitSize = Clipperz.Crypto.ECC.BinaryField.WordArrayValue.bitSize(result);
			while (resultBitSize >= modulusBitSize) {
				result = Clipperz.Crypto.ECC.BinaryField.WordArrayValue.xor(result, Clipperz.Crypto.ECC.BinaryField.WordArrayValue.shiftLeft(this.modulus()._value, resultBitSize - modulusBitSize));
				resultBitSize = Clipperz.Crypto.ECC.BinaryField.WordArrayValue.bitSize(result);
			}
		}
//console.log("<<< binaryField.finiteField.(standard)module");
		
		return result;
	},
	
	'module': function(aValue) {
		return new Clipperz.Crypto.ECC.BinaryField.Value(this._module(aValue._value));
	},
	
	//-----------------------------------------------------------------------------

	'_add': function(a, b) {
		return Clipperz.Crypto.ECC.BinaryField.WordArrayValue.xor(a, b);
	},
	
	'add': function(a, b) {
		return new Clipperz.Crypto.ECC.BinaryField.Value(this._add(a._value, b._value));
	},
	
	//-----------------------------------------------------------------------------

	'negate': function(aValue) {
		return aValue.clone();
	},

	//-----------------------------------------------------------------------------
/ *
	'multiply': function(a, b) {
		var result;
		var valueToXor;
		var i,c;

		result = Clipperz.Crypto.ECC.BinaryField.Value.O;
		valueToXor = b;
		c = a.bitSize();
		for (i=0; i<c; i++) {
			if (a.isBitSet(i) === true) {
				result = result.xor(valueToXor);
			}
			valueToXor = valueToXor.shiftLeft(1);
		}
		result = this.module(result);

		return result;
	},
* /

	'_multiply': function(a, b) {
		var result;
		var valueToXor;
		var i,c;

		result = [0];
		valueToXor = b;
		c = Clipperz.Crypto.ECC.BinaryField.WordArrayValue.bitSize(a);
		for (i=0; i<c; i++) {
			if (Clipperz.Crypto.ECC.BinaryField.WordArrayValue.isBitSet(a, i) === true) {
				result = Clipperz.Crypto.ECC.BinaryField.WordArrayValue.xor(result, valueToXor);
			}
			valueToXor = Clipperz.Crypto.ECC.BinaryField.WordArrayValue.shiftLeft(valueToXor, 1);
		}
		result = this._module(result);

		return result;
	},

	'multiply': function(a, b) {
		return new Clipperz.Crypto.ECC.BinaryField.Value(this._multiply(a._value, b._value));
	},
	
	//-----------------------------------------------------------------------------
	//
	//	Guide to Elliptic Curve Cryptography
	//	Darrel Hankerson, Alfred Menezes, Scott Vanstone
	//	- Pag: 49, Alorithm 2.34
	//	
	//-----------------------------------------------------------------------------
/ *
	'square': function(aValue) {
		var	result;
		var	t;
		var i,c;
		
		result = [0];
		t = Math.max(a)
		c = 32;
		for (i=0; i<c; i++) {
			var ii, cc;
			
			cc = 
		}
		
		
		
		
		return result;
	},
* /	
	//-----------------------------------------------------------------------------

	'inverse': function(aValue) {
		var	result;
		var b, c;
		var u, v;
		
		b = Clipperz.Crypto.ECC.BinaryField.Value.I;
		c = Clipperz.Crypto.ECC.BinaryField.Value.O;
		u = this.module(aValue);
		v = this.modulus();
		
		while (u.bitSize() > 1) {
			var	bitDifferenceSize;
			
			bitDifferenceSize = u.bitSize() - v.bitSize();
			if (bitDifferenceSize < 0) {
				var swap;
				
				swap = u;
				u = v;
				v = swap;
				
				swap = c;
				c = b;
				b = swap;
				
				bitDifferenceSize = -bitDifferenceSize;
			}

			u = this.add(u, v.shiftLeft(bitDifferenceSize));
			b = this.add(b, c.shiftLeft(bitDifferenceSize))
		}

		result = this.module(b);
		
		return result;
	},

	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

//#############################################################################

Clipperz.Crypto.ECC.BinaryField.Curve = function(args) {
	args = args || {};

	this._modulus = args.modulus;
	
	this._a = args.a;
	this._b = args.b;
	this._G = args.G;
	this._r = args.r;
	this._h = args.h;

	this._finiteField = null;
	
	return this;
}

Clipperz.Crypto.ECC.BinaryField.Curve.prototype = MochiKit.Base.update(null, {

	'asString': function() {
		return "Clipperz.Crypto.ECC.BinaryField.Curve";
	},

	//-----------------------------------------------------------------------------

	'modulus': function() {
		return this._modulus;
	},
	
	'a': function() {
		return this._a;
	},
	
	'b': function() {
		return this._b;
	},
	
	'G': function() {
		return this._G;
	},
	
	'r': function() {
		return this._r;
	},
	
	'h': function() {
		return this._h;
	},
	
	//-----------------------------------------------------------------------------

	'finiteField': function() {
		if (this._finiteField == null) {
			this._finiteField = new Clipperz.Crypto.ECC.BinaryField.FiniteField({modulus:this.modulus()})
		}
		
		return this._finiteField;
	},

	//-----------------------------------------------------------------------------

	'negate': function(aPointA) {
		var result;

		result = new Clipperz.Crypto.ECC.Point({x:aPointA.x(), y:this.finiteField().add(aPointA.y(), aPointA.x())})
		
		return result;
	},
	
	//-----------------------------------------------------------------------------

	'add': function(aPointA, aPointB) {
		var result;

//console.log(">>> ECC.BinaryField.Curve.add");
		if (aPointA.isZero()) {
//console.log("--- pointA == zero");
			result = aPointB;
		} else if (aPointB.isZero()) {
//console.log("--- pointB == zero");
			result = aPointA;
		} else if (	(aPointA.x().compare(aPointB.x()) == 0) &&
					((aPointA.y().compare(aPointB.y()) != 0) || aPointB.x().isZero()))
		{
//console.log("compare A.x - B.x: ", aPointA.x().compare(aPointB.x()));
//console.log("compare A.y - B.y: ", (aPointA.y().compare(aPointB.y()) != 0));
//console.log("compare B.x.isZero(): ", aPointB.x().isZero());

//console.log("--- result = zero");
			result = new Clipperz.Crypto.ECC.BinaryField.Point({x:Clipperz.Crypto.ECC.BinaryField.Value.O, y:Clipperz.Crypto.ECC.BinaryField.Value.O});
		} else {
//console.log("--- result = ELSE");
			var	f2m;
			var x, y;
			var lambda;

			f2m = this.finiteField();
			
			if (aPointA.x().compare(aPointB.x()) != 0) {
//console.log(" a.x != b.x");
				lambda =	f2m.multiply(
								f2m.add(aPointA.y(), aPointB.y()),
								f2m.inverse(f2m.add(aPointA.x(), aPointB.x()))
							);
				x = f2m.add(this.a(), f2m.multiply(lambda, lambda));
				x = f2m.add(x, lambda);
				x = f2m.add(x, aPointA.x());
				x = f2m.add(x, aPointB.x());
			} else {
//console.log(" a.x == b.x");
				lambda = f2m.add(aPointB.x(), f2m.multiply(aPointB.y(), f2m.inverse(aPointB.x())));
//console.log(" lambda: " + lambda.asString(16));
				x = f2m.add(this.a(), f2m.multiply(lambda, lambda));
//console.log(" x (step 1): " + x.asString(16));
				x = f2m.add(x, lambda);
//console.log(" x (step 2): " + x.asString(16));
			}
			
			y = f2m.multiply(f2m.add(aPointB.x(), x), lambda);
//console.log(" y (step 1): " + y.asString(16));
			y = f2m.add(y, x);
//console.log(" y (step 2): " + y.asString(16));
			y = f2m.add(y, aPointB.y());
//console.log(" y (step 3): " + y.asString(16));

			result = new Clipperz.Crypto.ECC.BinaryField.Point({x:x, y:y})
		}
//console.log("<<< ECC.BinaryField.Curve.add");
		
		return result;
	},
	
	//-----------------------------------------------------------------------------

	'multiply': function(aValue, aPoint) {
		var result;

console.profile();
		result = new Clipperz.Crypto.ECC.BinaryField.Point({x:Clipperz.Crypto.ECC.BinaryField.Value.O, y:Clipperz.Crypto.ECC.BinaryField.Value.O});
		
		if (aValue.isZero() == false) {
			var k, Q;
			var i;
			var countIndex; countIndex = 0;
			
			if (aValue.compare(Clipperz.Crypto.ECC.BinaryField.WordArrayValue.O) > 0) {
				k = aValue;
				Q = aPoint;
			} else {
MochiKit.Logging.logError("The Clipperz.Crypto.ECC.BinaryFields.Value does not work with negative values!!!!");
				k = aValue.negate();
				Q = this.negate(aPoint);
			}

//console.log("k: " + k.toString(16));
//console.log("k.bitSize: " + k.bitSize());
			for (i=k.bitSize()-1; i>=0; i--) {
				result = this.add(result, result);
				if (k.isBitSet(i)) {
					result = this.add(result, Q);
				}
				
//				if (countIndex==100) {console.log("multiply.break"); break;} else countIndex++;
			}
		}
console.profileEnd();
		
		return result;
	},

	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

//#############################################################################

//#############################################################################
/ *
Clipperz.Crypto.ECC.Key = function(args) {
	args = args || {};

	return this;
}

Clipperz.Crypto.ECC.Key.prototype = MochiKit.Base.update(null, {

	'asString': function() {
		return "Clipperz.Crypto.ECC.Key";
	},
	
	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
* /
//#############################################################################


//#############################################################################

Clipperz.Crypto.ECC.StandardCurves = {};

MochiKit.Base.update(Clipperz.Crypto.ECC.StandardCurves, {
/ *
	'_K571': null,
	'K571': function() {
		if (Clipperz.Crypto.ECC.StandardCurves._K571 == null) {
			Clipperz.Crypto.ECC.StandardCurves._K571 = new Clipperz.Crypto.ECC.Curve.Koblitz({
				exadecimalForm: '80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425',
				a: new Clipperz.Crypto.BigInt(0),
				G: new Clipperz.Crypto.ECC.Point({
					x: new Clipperz.Crypto.BigInt('26eb7a859923fbc82189631f8103fe4ac9ca2970012d5d46024804801841ca44370958493b205e647da304db4ceb08cbbd1ba39494776fb988b47174dca88c7e2945283a01c8972', 16),
					y: new Clipperz.Crypto.BigInt('349dc807f4fbf374f4aeade3bca95314dd58cec9f307a54ffc61efc006d8a2c9d4979c0ac44aea74fbebbb9f772aedcb620b01a7ba7af1b320430c8591984f601cd4c143ef1c7a3', 16)
				}),
				n: new Clipperz.Crypto.BigInt('1932268761508629172347675945465993672149463664853217499328617625725759571144780212268133978522706711834706712800825351461273674974066617311929682421617092503555733685276673', 16),
				h: new Clipperz.Crypto.BigInt(4)
			});
		}
		
		return Clipperz.Crypto.ECC.StandardCurves._K571;
	},
* /
	//-----------------------------------------------------------------------------

	'_B571': null,
	'B571': function() {	//	f(z) = z^571 + z^10 + z^5 + z^2 + 1
		if (Clipperz.Crypto.ECC.StandardCurves._B571 == null) {
			Clipperz.Crypto.ECC.StandardCurves._B571 = new Clipperz.Crypto.ECC.BinaryField.Curve({
				modulus: new Clipperz.Crypto.ECC.BinaryField.Value('80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425', 16),
				a: new Clipperz.Crypto.ECC.BinaryField.Value('1', 16),
				b: new Clipperz.Crypto.ECC.BinaryField.Value('02f40e7e2221f295de297117b7f3d62f5c6a97ffcb8ceff1cd6ba8ce4a9a18ad84ffabbd8efa59332be7ad6756a66e294afd185a78ff12aa520e4de739baca0c7ffeff7f2955727a', 16),
				G: new Clipperz.Crypto.ECC.BinaryField.Point({
					x: new Clipperz.Crypto.ECC.BinaryField.Value('0303001d34b856296c16c0d40d3cd7750a93d1d2955fa80aa5f40fc8db7b2abdbde53950f4c0d293cdd711a35b67fb1499ae60038614f1394abfa3b4c850d927e1e7769c8eec2d19', 16),
					y: new Clipperz.Crypto.ECC.BinaryField.Value('037bf27342da639b6dccfffeb73d69d78c6c27a6009cbbca1980f8533921e8a684423e43bab08a576291af8f461bb2a8b3531d2f0485c19b16e2f1516e23dd3c1a4827af1b8ac15b', 16)
				}),
//				r: new Clipperz.Crypto.ECC.BinaryField.Value('3864537523017258344695351890931987344298927329706434998657235251451519142289560424536143999389415773083133881121926944486246872462816813070234528288303332411393191105285703', 10),
				r: new Clipperz.Crypto.ECC.BinaryField.Value('03ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe661ce18ff55987308059b186823851ec7dd9ca1161de93d5174d66e8382e9bb2fe84e47', 16),
				h: new Clipperz.Crypto.ECC.BinaryField.Value('2', 16)

//				S: new Clipperz.Crypto.ECC.BinaryField.Value('2aa058f73a0e33ab486b0f610410c53a7f132310', 10),
//				n: new Clipperz.Crypto.ECC.BinaryField.Value('03ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe661ce18ff55987308059b186823851ec7dd9ca1161de93d5174d66e8382e9bb2fe84e47', 16),
			});
			
			//-----------------------------------------------------------------------------
			//
			//	Guide to Elliptic Curve Cryptography
			//	Darrel Hankerson, Alfred Menezes, Scott Vanstone
			//	- Pag: 56, Alorithm 2.45 (with a typo!!!)
			//	
			//-----------------------------------------------------------------------------
			//
			//		http://www.milw0rm.com/papers/136
			//
			//		-------------------------------------------------------------------------
			//		  Polynomial Reduction Algorithm Modulo f571 
			//		-------------------------------------------------------------------------
			//		
			//		  Input:  Polynomial p(x) of degree 1140 or less, stored as 
			//		          an array of 2T machinewords.
			//		  Output: p(x) mod f571(x)
			//		
			//		  FOR i = T-1, ..., 0 DO
			//		      SET X  := P[i+T]
			//		      P[i]   := P[i]   ^ (X<<5) ^ (X<<7) ^ (X<<10) ^ (X<<15)
			//		      P[i+1] := P[i+1] ^ (X>>17) ^ (X>>22) ^ (X>>25) ^ (X>>27)
			//		
			//		  SET X  := P[T-1] >> 27
			//		  P[0]   := P[0] ^ X ^ (X<<2) ^ (X<<5) ^ (X<<10)
			//		  P[T-1] := P[T-1] & 0x07ffffff
			//		
			//		  RETURN P[T-1],...,P[0]
			//		
			//		-------------------------------------------------------------------------
			//		
			Clipperz.Crypto.ECC.StandardCurves._B571.finiteField().module = function(aValue) {
				var	result;
				var	C, T;
				var i;
		
//console.log(">>> binaryField.finiteField.(improved)module");
//				C = aValue.value().slice(0);
				C = aValue._value.slice(0);
				for (i=35; i>=18; i--) {
					T = C[i];
					C[i-18] = (((C[i-18] ^ (T<<5) ^ (T<<7) ^ (T<<10) ^ (T<<15)) & 0xffffffff) >>> 0);
					C[i-17] = ((C[i-17] ^ (T>>>27) ^ (T>>>25) ^ (T>>>22) ^ (T>>>17)) >>> 0);
				}
				T = (C[17] >>> 27);
				C[0] = ((C[0] ^ T ^ ((T<<2) ^ (T<<5) ^ (T<<10)) & 0xffffffff) >>> 0);
				C[17] = (C[17] & 0x07ffffff);

				for(i=18; i<=35; i++) {
					C[i] = 0;
				}
				
				result = new Clipperz.Crypto.ECC.BinaryField.WordArrayValue(C);
//console.log("<<< binaryField.finiteField.(improved)module");

				return result;
			};
		}
		
		return Clipperz.Crypto.ECC.StandardCurves._B571;
	},

	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

//#############################################################################
*/
