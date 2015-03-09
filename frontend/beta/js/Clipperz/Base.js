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

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.Base) == 'undefined') { Clipperz.Base = {}; }

Clipperz.Base.VERSION = "0.1";
Clipperz.Base.NAME = "Clipperz.Base";

MochiKit.Base.update(Clipperz.Base, {

	//-------------------------------------------------------------------------

	'__repr__': function () {
		return "[" + this.NAME + " " + this.VERSION + "]";
	},

	//-------------------------------------------------------------------------

	'toString': function () {
		return this.__repr__();
	},

	//-------------------------------------------------------------------------

	'trim': function (aValue) {
		return aValue.replace(/^\s+|\s+$/g, "");
	},

	//-------------------------------------------------------------------------

	'stringToByteArray': function (aValue) {
		var	result;
		var i, c;

		result = [];
		
		c = aValue.length;
		for (i=0; i<c; i++) {
			result[i] = aValue.charCodeAt(i);
		}
		
		return result;
	},
	
	//.........................................................................
	
	'byteArrayToString': function (anArrayOfBytes) {
		var	result;
		var i, c;

		result = "";

		c = anArrayOfBytes.length;
		for (i=0; i<c; i++) {
			result += String.fromCharCode(anArrayOfBytes[i]);
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'getValueForKeyInFormContent': function (aFormContent, aKey) {
		return aFormContent[1][MochiKit.Base.find(aFormContent[0], aKey)];
	},

	//-------------------------------------------------------------------------

	'indexOfObjectInArray': function(anObject, anArray) {
		var	result;
		var	i, c;
		
		result = -1;

		c = anArray.length;
		for (i=0; ((i<c) && (result < 0)); i++) {
			if (anArray[i] === anObject) {
				result = i;
			}
		}

		return result;
	},

	'removeObjectAtIndexFromArray': function(anIndex, anArray) {
		anArray.splice(anIndex, 1);
	},
	
	'removeObjectFromArray': function(anObject, anArray) {
		var	objectIndex;
		
		objectIndex = Clipperz.Base.indexOfObjectInArray(anObject, anArray);
		if (objectIndex > -1) {
			Clipperz.Base.removeObjectAtIndexFromArray(objectIndex, anArray);
		} else {
//			jslog.error("Trying to remove an object not present in the array");
			//	TODO: raise an exception
		}
	},
	
	'removeFromArray': function(anArray, anObject) {
		return Clipperz.Base.removeObjectFromArray(anObject, anArray);
	},

	//-------------------------------------------------------------------------

	'splitStringAtFixedTokenSize': function(aString, aTokenSize) {
		var result;
		var	stringToProcess;
		
		stringToProcess = aString;
		result = [];
		if (stringToProcess != null) {
			while (stringToProcess.length > aTokenSize) {
				result.push(stringToProcess.substring(0, aTokenSize));
				stringToProcess = stringToProcess.substring(aTokenSize);
			}
			
			result.push(stringToProcess);
		}
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'objectType': function(anObject) {
		var result;

		if (anObject == null) {
			result = null;
		} else {
			result = typeof(anObject);
			
			if (result == "object") {
				if (anObject instanceof Array) {
					result = 'array'
				} else if (anObject.constructor == Boolean) {
					result = 'boolean'
				} else if (anObject instanceof Date) {
					result = 'date'
				} else if (anObject instanceof Error) {
					result = 'error'
				} else if (anObject instanceof Function) {
					result = 'function'
				} else if (anObject.constructor == Number) {
					result = 'number'
				} else if (anObject.constructor == String) {
					result = 'string'
				} else if (anObject instanceof Object) {
					result = 'object'
				} else {
					throw Clipperz.Base.exception.UnknownType;
				}
			}
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'escapeHTML': function(aValue) {
		var result;

		result = aValue;
		result = result.replace(/</g, "&lt;");
		result = result.replace(/>/g, "&gt;");
		
		return result;
	},

	//-------------------------------------------------------------------------

	'deepClone': function(anObject) {
		var result;
		
		result = Clipperz.Base.evalJSON(Clipperz.Base.serializeJSON(anObject));
		
		return result;
	},

	//-------------------------------------------------------------------------

	'evalJSON': function(aString) {
/*
		var result;
		
		//	check for XSS injection
		if (/<script>/.test(aString)) {
			throw "error";
		}

		if (/<iframe>/.test(aString)) {
			throw "error";
		}

		result = MochiKit.Base.evalJSON(aString);
		
		return result;
*/

//		return MochiKit.Base.evalJSON(aString);
		return JSON2.parse(aString);
	},
	
	'serializeJSON': function(anObject) {
//		return MochiKit.Base.serializeJSON(anObject);
		return JSON2.stringify(anObject);
	},

	//-------------------------------------------------------------------------

	'sanitizeString': function(aValue) {
		var result;
		
		if (Clipperz.Base.objectType(aValue) == 'string') {
			result = aValue;
			result = result.replace(/</img,"&lt;");
			result = result.replace(/>/img,"&gt;");
		} else {
			result = aValue;
		}
		
		return result;
	},

	'javascriptInjectionPattern': new RegExp("javascript:\/\/\"", "g"),
	
	'sanitizeUrl': function(aValue) {
		var	result;
		
		if ((aValue != null) && this.javascriptInjectionPattern.test(aValue)) {
			result = aValue.replace(this.javascriptInjectionPattern, '');
			console.log("sanitized url", aValue, result);
		} else {
			result = aValue;
		}

		return result;
	},

	'sanitizeFavicon': function(aValue) {
		var	result;
		
		if ((aValue != null) && this.javascriptInjectionPattern.test(aValue)) {
			result = aValue.replace(this.javascriptInjectionPattern, '');
			console.log("sanitized favicon", aValue, result);
		} else {
			result = aValue;
		}

		return result;
	},

	//-------------------------------------------------------------------------

	'exception': {
		'AbstractMethod': 		new MochiKit.Base.NamedError("Clipperz.Base.exception.AbstractMethod"),
		'UnknownType':    		new MochiKit.Base.NamedError("Clipperz.Base.exception.UnknownType"),
		'VulnerabilityIssue':	new MochiKit.Base.NamedError("Clipperz.Base.exception.VulnerabilityIssue") 
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});



MochiKit.Base.registerComparator('Object dummy comparator',
	function(a, b) {
		return ((a.constructor == Object) && (b.constructor == Object));
	},
	function(a, b) {
		var result;
		var aKeys;
		var bKeys;
		
//MochiKit.Logging.logDebug(">>> comparator");
//MochiKit.Logging.logDebug("- a: " + Clipperz.Base.serializeJSON(a));
//MochiKit.Logging.logDebug("- b: " + Clipperz.Base.serializeJSON(a));
		aKeys = MochiKit.Base.keys(a).sort();
		bKeys = MochiKit.Base.keys(b).sort();
		
		result = MochiKit.Base.compare(aKeys, bKeys);
//if (result != 0) {
//	MochiKit.Logging.logDebug("- comparator 'keys':");
//	MochiKit.Logging.logDebug("- comparator aKeys: " + Clipperz.Base.serializeJSON(aKeys));
//	MochiKit.Logging.logDebug("- comparator bKeys: " + Clipperz.Base.serializeJSON(bKeys));
//}
		if (result == 0) {
			var	i, c;
			
			c = aKeys.length;
			for (i=0; (i<c) && (result == 0); i++) {
				result = MochiKit.Base.compare(a[aKeys[i]], b[bKeys[i]]);
//if (result != 0) {
//	MochiKit.Logging.logDebug("- comparator 'values':");
//	MochiKit.Logging.logDebug("- comparator a[aKeys[i]]: " + Clipperz.Base.serializeJSON(a[aKeys[i]]));
//	MochiKit.Logging.logDebug("- comparator b[bKeys[i]]: " + Clipperz.Base.serializeJSON(b[bKeys[i]]));
//}
			}
		}		
		
//MochiKit.Logging.logDebug("<<< comparator - result: " + result);
		return result;
	},
	true
);
