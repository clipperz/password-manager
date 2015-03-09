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

Clipperz.PM.DataModel.DirectLoginBinding = function(aDirectLogin, aKey, args) {
//MochiKit.Logging.logDebug(">>> new DirectLoginBinding")
	args = args || {};
//MochiKit.Logging.logDebug("--- new DirectLoginBinding - args: " + Clipperz.Base.serializeJSON(args));

	this._directLogin = aDirectLogin || args.directLogin || null;
	this._key = aKey;

	this._fieldKey = args.fieldKey || null;
	this._fieldName = args.fieldName || null;
//MochiKit.Logging.logDebug("<<< new DirectLoginBinding")
	
	return this;
}

Clipperz.PM.DataModel.DirectLoginBinding.prototype = MochiKit.Base.update(null, {

	'directLogin': function() {
		return this._directLogin;
	},

	//-------------------------------------------------------------------------

	'key': function() {
		return this._key;
	},

	//-------------------------------------------------------------------------

	'fieldKey': function() {
//MochiKit.Logging.logDebug("=== Clipperz.PM.DataModel.DirectLoginBinding.fieldKey");
//MochiKit.Logging.logDebug("=== Clipperz.PM.DataModel.DirectLoginBinding.fieldKey - " + this._fieldKey);
		return this._fieldKey;
	},
	
	'setFieldKey': function(aValue) {
		this._fieldKey = aValue;
	},
	
	'fieldName': function() {
		return this._fieldName;
	},
	
	//-------------------------------------------------------------------------

	'field': function() {
		var result;

//MochiKit.Logging.logDebug(">>> Clipperz.PM.DataModel.DirectLoginBinding.field")		
//MochiKit.Logging.logDebug("--- Clipperz.PM.DataModel.DirectLoginBinding.field - 1 - this.fieldKey(): " + this.fieldKey());
//MochiKit.Logging.logDebug("--- Clipperz.PM.DataModel.DirectLoginBinding.field - 2 - this.fieldName(): " + this.fieldName());
		if (this.fieldKey() != null) {
			result = this.directLogin().record().currentVersion().fields()[this.fieldKey()];
//MochiKit.Logging.logDebug("--- Clipperz.PM.DataModel.DirectLoginBinding.field - 3 - result: " + result);
		} else if (this.fieldName() != null) {
			result = this.directLogin().record().currentVersion().fieldWithName(this.fieldName());
//MochiKit.Logging.logDebug("--- Clipperz.PM.DataModel.DirectLoginBinding.field - 4 - result: " + result);
			
			this.setFieldKey(result.key());
		} else {
			result = null;
		}
//MochiKit.Logging.logDebug("<<< Clipperz.PM.DataModel.DirectLoginBinding.field")		
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'serializedData': function() {
		return this.fieldKey();
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});

