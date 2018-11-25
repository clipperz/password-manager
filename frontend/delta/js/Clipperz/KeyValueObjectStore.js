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

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }

//#############################################################################

Clipperz.KeyValueObjectStore = function(args) {
	args = args || {};

//	this._name = args['name'] || "unnamed KeyValueObjectStore";
	this._values = args['values'] || {};
//	this._referenceObjectStore = null;

	return this;
}

Clipperz.KeyValueObjectStore.prototype = MochiKit.Base.update(null, {

	'values': function() {
		return this._values;
	},

	'initWithValues': function (someValues) {
		this._values = Clipperz.Base.deepClone(someValues) || {};
		return this;
	},

	'setValues': function (someValues) {
		this._values = someValues;
		return this;
	},

//	'initWithObjectStore': function (anObjectStore) {
//		this._referenceObjectStore = anObjectStore;
//	},

	'removeAllData': function () {
		this._values = {};
	},

	//-------------------------------------------------------------------------

	'getValue': function(aKeyPath) {
		var result;
		var keys;
		var i,c;

		result = this.values();

		keys = (aKeyPath + '').split('.');
		c = keys.length;
		i = 0;

		while ((i<c) && (result != null)) {
			if (typeof result[keys[i]] != 'undefined') {
				result = result[keys[i]];
			} else {
				result = null;
			}
			
			i++;
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'setValue': function(aKeyPath, aValue) {
		var targetObject;
		var keys;
		var i,c;

		targetObject = this.values();
		keys = (aKeyPath + '').split('.');
		c = keys.length - 1;
		for (i=0; i<c; i++) {
			if (typeof targetObject[keys[i]] == 'undefined') {
				targetObject[keys[i]] = {}
			}

			targetObject = targetObject[keys[i]];
		}

		targetObject[keys[c]] = aValue;
		
		return aValue;
	},

	//-------------------------------------------------------------------------

	'removeValue': function (aKeyPath) {
//		this.setValue(aKeyPath, null);

		var targetObject;
		var keys;
		var i,c;
		
		targetObject = this.values();
		keys = ('' + aKeyPath).split('.');
		c = keys.length - 1;
		for (i=0; i<c; i++) {
			if (typeof targetObject[keys[i]] == 'undefined') {
				targetObject[keys[i]] = {}
			}

			targetObject = targetObject[keys[i]];
		}

		delete targetObject[keys[c]];
	},

	//-------------------------------------------------------------------------

	'deferredGetOrSet': function(aKeyPath, aGetterFunction) {
		var deferredResult;
		
		if (this.getValue(aKeyPath) != null) {
			deferredResult = MochiKit.Async.succeed(this.getValue(aKeyPath));
		} else {
			deferredResult = new Clipperz.Async.Deferred("KeyValueObjectStore.deferredGetOrSet [" + aKeyPath + "]", {trace:false});

			deferredResult.addCallback(aGetterFunction);
			deferredResult.addMethod(this, 'setValue', aKeyPath);
			deferredResult.callback();
		}
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'isEmpty': function () {
		return (MochiKit.Base.keys(this.values()).length == 0)
	},

	//-------------------------------------------------------------------------
/*
	'dumpData': function () {
		return Clipperz.Base.serializeJSON(this.values());
	},
*/
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
