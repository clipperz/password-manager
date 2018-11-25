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
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.DataModel) == 'undefined') { Clipperz.PM.DataModel = {}; }


//#############################################################################

Clipperz.PM.DataModel.DirectLoginBinding = function(aDirectLogin, args) {
	args = args || {};

	this._directLogin = aDirectLogin|| Clipperz.Base.exception.raise('MandatoryParameter');

	this._key =			args.key	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._fieldKey =	args.field	|| /* this.directLogin().fieldWithName(args.fieldName).reference()	||	*/	null;
	
	return this;
}

Clipperz.PM.DataModel.DirectLoginBinding.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "DirectLoginBinding (" + this.key() + ", " + this.fieldKey() + ")";
	},

	//-------------------------------------------------------------------------

	'directLogin': function () {
		return this._directLogin;
	},

	//-------------------------------------------------------------------------

	'key': function() {
		return this._key;
	},

	//-------------------------------------------------------------------------

	'fieldKey': function() {
		return this._fieldKey;
	},
	
	'setFieldKey': function(aValue) {
		this._fieldKey = aValue;
		
		return this.directLogin().setValue('bindingData' + '.' + this.key(), aValue);
	},
	
//	'fieldName': function() {
//		return this._fieldName;
//	},
	
	//-------------------------------------------------------------------------

	'field': function() {
		var deferredResult;

		if (this.fieldKey() != null) {
			deferredResult = Clipperz.Async.callbacks("DirectLoginBinding.field [1]", [
				MochiKit.Base.method(this.directLogin().record(), 'fields'),
				MochiKit.Base.itemgetter(this.fieldKey())
			], {trace:false});
//		} else if (this.fieldName() != null) {
//			WTF = TODO;
//			result = this.directLogin().record().fieldWithName(this.fieldName());
//			
//			this.setFieldKey(result.key());
		} else {
			deferredResult = MochiKit.Async.succeed(null);
		}
		
		return deferredResult;
	},
	
	'setField': function (aField) {
		this.setFieldKey(aField.reference());
	},

	//-------------------------------------------------------------------------
/*
	'fieldValue': function () {
		return Clipperz.Async.callbacks("DirectLoginBinding.fieldValue", [
			MochiKit.Base.method('field'),
			MochiKit.Base.methodcaller('value')
		], {trace:false});
	},
*/
	//-------------------------------------------------------------------------

	'serializedData': function() {
		return this.fieldKey();
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

