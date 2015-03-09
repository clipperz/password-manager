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
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.DataModel) == 'undefined') { Clipperz.PM.DataModel = {}; }


//#############################################################################

Clipperz.PM.DataModel.DirectLoginFormValue = function(aDirectLogin, args) {
	args = args || {};

	this._directLogin = aDirectLogin|| Clipperz.Base.exception.raise('MandatoryParameter');

	this._key			= args.key			|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._fieldOptions	= args.fieldOptions	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._value			= args.value		|| null;
	
	return this;
}

Clipperz.PM.DataModel.DirectLoginFormValue.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "DirectLoginFormValue (" + this.key() + ", " + this.value() + ")";
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

	'fieldOptions': function() {
		return this._fieldOptions;
	},

	//-------------------------------------------------------------------------

	'type': function () {
		return this.fieldOptions()['type'];
	},
	
	//-------------------------------------------------------------------------

	'value': function() {
		var	result;
		
		result = this._value;
		
//		if ((result == null) && (this.type() == 'checkbox')) {
//			result = false;
//		};
		
		return result;
	},
	
	'setValue': function (aValue) {
		this._value = aValue;
		return this.directLogin().setValue('formValues' + '.' + this.key(), aValue);
	},

	//-------------------------------------------------------------------------
/*
	'serializedData': function() {
		return this.value();
	},
*/
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

