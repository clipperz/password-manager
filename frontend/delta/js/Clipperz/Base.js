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

"use strict";

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.Base) == 'undefined') { Clipperz.Base = {}; }

Clipperz.Base.VERSION = "0.2";
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

	'itemgetter': function (aKeyPath) {
//		return MochiKit.Base.compose.apply(null, [MochiKit.Base.itemgetter('key3')]);
		return MochiKit.Base.compose.apply(null,
					MochiKit.Base.map(
						MochiKit.Base.itemgetter,
						MochiKit.Iter.reversed(
							aKeyPath.split('.')
						)
					)
				);
	},

	//-------------------------------------------------------------------------

	'isUrl': function (aValue) {
		return (MochiKit.Base.urlRegExp.test(aValue));
	},

	'isEmail': function (aValue) {
		return (MochiKit.Base.emailRegExp.test(aValue));
	},

	//-------------------------------------------------------------------------

	'caseInsensitiveCompare': function (a, b) {
		return MochiKit.Base.compare(a.toLowerCase(), b.toLowerCase());
	},

	'reverseComparator': function (aComparator) {
		return MochiKit.Base.compose(function(aResult) { return -aResult; }, aComparator);
	},

	'caseInsensitiveKeyComparator': function (aKey) {
		return function (a, b) {
			return MochiKit.Base.compare(a[aKey].toLowerCase(), b[aKey].toLowerCase());
		}
	},
	//-------------------------------------------------------------------------
/*
	'dependsOn': function(module, deps) {
		if (!(module in Clipperz)) {
			MochiKit[module] = {};
		}

		if (typeof(dojo) != 'undefined') {
			dojo.provide('Clipperz.' + module);
		}
		for (var i = 0; i < deps.length; i++) {
			if (typeof(dojo) != 'undefined') {
				dojo.require('Clipperz.' + deps[i]);
			}
			if (typeof(JSAN) != 'undefined') {
				JSAN.use('Clipperz.' + deps[i], []);
			}
			if (!(deps[i] in Clipperz)) {
				throw 'Clipperz.' + module + ' depends on Clipperz.' + deps[i] + '!'
			}
		}
	},
*/
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

	//-------------------------------------------------------------------------

	'removeObjectAtIndexFromArray': function(anIndex, anArray) {
		anArray.splice(anIndex, 1);
	},
	
	//-------------------------------------------------------------------------

	'removeObjectFromArray': function(anObject, anArray) {
		var	objectIndex;
		
		objectIndex = Clipperz.Base.indexOfObjectInArray(anObject, anArray);
		if (objectIndex > -1) {
			Clipperz.Base.removeObjectAtIndexFromArray(objectIndex, anArray);
		} else {
			Clipperz.log("Trying to remove an object not present in the array");
			throw Clipperz.Base.exception.ObjectNotFound;
		}
	},
	
	'removeFromArray': function(anArray, anObject) {
		return Clipperz.Base.removeObjectFromArray(anObject, anArray);
	},

	'arrayWithUniqueValues': function (anArray) {
		return anArray.filter(function (value, index, self) { return self.indexOf(value) === index; });
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

//	'deepCompare': function (aObject, bObject) {
//		return (Clipperz.Base.serializeJSON(aObject) == Clipperz.Base.serializeJSON(bObject));
//	},

	//-------------------------------------------------------------------------

	'evalJSON': function(aString) {
		return JSON.parse(aString);
	},
	
	'serializeJSON': function(anObject) {
		return JSON.stringify(anObject);
	},

	'formatJSON': function (anObject, sIndent) {
		var realTypeOf = function (v) {
			if (typeof(v) == "object") {
				if (v === null) return "null";
				if (v.constructor == (new Array).constructor) return "array";
				if (v.constructor == (new Date).constructor) return "date";
				if (v.constructor == (new RegExp).constructor) return "regex";
				return "object";
			}
			return typeof(v);
		};

//	function FormatJSON(oData, sIndent) {
		if (arguments.length < 2) {
			var sIndent = "";
		}
//		var sIndentStyle = "    ";
		var sIndentStyle = "  ";
		var sDataType = realTypeOf(anObject);

		// open object
		if (sDataType == "array") {
			if (anObject.length == 0) {
				return "[]";
			}
			var sHTML = "[";
		} else if (sDataType == "object") {
			var sHTML = "{";
		} else {
			return "{}";
		}
//		} else {
//			var iCount = 0;
//			$.each(anObject, function() {
//				iCount++;
//				return;
//			});
//			if (iCount == 0) { // object is empty
//				return "{}";
//			}
//			var sHTML = "{";
//		}

		// loop through items
		var iCount = 0;
//		$.each(anObject, function(sKey, vValue) {
		MochiKit.Iter.forEach(MochiKit.Base.keys(anObject), function(sKey) {
			var vValue = anObject[sKey];

			if (iCount > 0) {
				sHTML += ",";
			}
			if (sDataType == "array") {
				sHTML += ("\n" + sIndent + sIndentStyle);
			} else {
				sHTML += ("\n" + sIndent + sIndentStyle + "\"" + sKey + "\"" + ": ");
			}
	
			// display relevant data type
			switch (realTypeOf(vValue)) {
				case "array":
				case "object":
					sHTML += Clipperz.Base.formatJSON(vValue, (sIndent + sIndentStyle));
					break;
				case "boolean":
				case "number":
					sHTML += vValue.toString();
					break;
				case "null":
					sHTML += "null";
					break;
				case "string":
					sHTML += ("\"" + vValue + "\"");
					break;
				default:
					sHTML += ("TYPEOF: " + typeof(vValue));
			}
	
			// loop
			iCount++;
		});

		// close object
		if (sDataType == "array") {
			sHTML += ("\n" + sIndent + "]");
		} else {
			sHTML += ("\n" + sIndent + "}");
		}

		// return
		return sHTML;
	},

	//-------------------------------------------------------------------------

	'mergeItems': function (anArrayOfValues) {
		var result;
		var i, c;
		
		result = {};
		
		c = anArrayOfValues.length;
		for (i=0; i<c; i++) {
			result[anArrayOfValues[i][0]] = anArrayOfValues[i][1];
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'map': function (fn, lstObj/*, lst... */) {
		var result;
		
		if (MochiKit.Base.isArrayLike(lstObj)) {
			result = MochiKit.Base.map.apply(this, arguments);
		} else {
			var	keys;
			var values;
			var computedValues;
			
			keys = MochiKit.Base.keys(lstObj);
			values = MochiKit.Base.values(lstObj);
			computedValues = MochiKit.Base.map(fn, values);
			
			result = Clipperz.Base.mergeItems(MochiKit.Base.zip(keys, computedValues));
		}
		
		return result;
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

	//-------------------------------------------------------------------------

	'module': function(aValue) {
//		aValue = 'Clipperz.PM.Compact'
//
//		if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
//		if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
//		if (typeof(Clipperz.PM.UI.Common.Components) == 'undefined') { Clipperz.PM.UI.Common.Components = {}; }

		var currentScope;
		var pathElements;
		var i,c;
		
		currentScope = window;
		pathElements = aValue.split('.');
		c = pathElements.length;
		for (i=0; i<c; i++) {
			if (typeof(currentScope[pathElements[i]]) == 'undefined') {
				currentScope[pathElements[i]] = {};
			}
			
			currentScope = currentScope[pathElements[i]];
		}
	},
	
	//-------------------------------------------------------------------------

	'exception': {
		'AbstractMethod': 		new MochiKit.Base.NamedError("Clipperz.Base.exception.AbstractMethod"),
		'UnknownType':    		new MochiKit.Base.NamedError("Clipperz.Base.exception.UnknownType"),
		'VulnerabilityIssue':	new MochiKit.Base.NamedError("Clipperz.Base.exception.VulnerabilityIssue"),
		'MandatoryParameter':	new MochiKit.Base.NamedError("Clipperz.Base.exception.MandatoryParameter"),
		'ObjectNotFound':		new MochiKit.Base.NamedError("Clipperz.Base.exception.ObjectNotFound"),
		'raise': function (aName) {
			throw Clipperz.Base.exception[aName];
		}
	},

	//-------------------------------------------------------------------------

	'extend': YAHOO.extendX,

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});

//	Original regExp courtesy of John Gruber: http://daringfireball.net/2009/11/liberal_regex_for_matching_urls
//	Updated to match Clipperz usage pattern.
//MochiKit.Base.urlRegExp = new RegExp(/\b(([\w-]+:\/\/?|www[.])[^\s()<>]+(?:\([\w\d]+\)|([^[:punct:]\s]|\/)))/);
MochiKit.Base.urlRegExp = new RegExp(/^((([\w-]+:\/\/?)|(www\.))[^\s()<>]+((?:\([\w\d]+\)|([^[:punct:]\s]|\/)))?)/);

//	RegExp found here: http://www.tipsntracks.com/117/validate-an-email-address-using-regular-expressions.html
MochiKit.Base.emailRegExp = new RegExp(/^([a-zA-Z0-9_\-\.]+)@(([a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,3}))|(([01]?\d\d?|2[0-4]\d|25[0-5])\.){3}([01]?\d\d?|25[0-5]|2[0-4]\d))$/);


MochiKit.Base.registerComparator('Object dummy comparator',
	function(a, b) {
		return ((a.constructor == Object) && (b.constructor == Object));
	},
	function(a, b) {
		var result;
		var aKeys;
		var bKeys;
		
		aKeys = MochiKit.Base.keys(a).sort();
		bKeys = MochiKit.Base.keys(b).sort();
		result = MochiKit.Base.compare(aKeys, bKeys);

		if (result == 0) {
			var	i, c;
			
			c = aKeys.length;
			for (i=0; (i<c) && (result == 0); i++) {
				result = MochiKit.Base.compare(a[aKeys[i]], b[bKeys[i]]);
			}
		}		
		
		return result;
	},
	true
);
