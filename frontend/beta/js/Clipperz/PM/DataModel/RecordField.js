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

Clipperz.PM.DataModel.RecordField = function(args) {
	args = args || {};

	this._recordVersion = args.recordVersion || null;
	this._key	 = args.key	   || Clipperz.PM.Crypto.randomKey();
	this.setLabel(args.label  || '');
	this.setValue(args.value  || '');
	this.setType(args.type   || 'TXT');	//	valid types: 'TXT', 'PWD', 'URL', 'DATE', 'ADDR', 'CHECK', 'RADIO', ('NOTE' probably not), ...
	this._hidden = args.hidden || (args.type == 'PWD') || false;

	return this;
}

Clipperz.PM.DataModel.RecordField.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "Clipperz.PM.DataModel.RecordField - " + this.label() + " (" + this.key() + ")";
	},

	//-------------------------------------------------------------------------

	'recordVersion': function() {
		return this._recordVersion;
	},

	//-------------------------------------------------------------------------

	'key': function() {
		return this._key;
	},

	//-------------------------------------------------------------------------

	'label': function() {
		return this._label;
	},
	
	'setLabel': function(aValue) {
		this._label = aValue;
	},
	
	//-------------------------------------------------------------------------

	'value': function() {
		return this._value;
	},
	
	'setValue': function(aValue) {
		this._value = aValue;
	},
	
	//-------------------------------------------------------------------------

	'type': function() {
		return this._type;
	},
	
	'setType': function(aValue) {
		this._type = aValue;
		
		if (aValue == 'PWD') {
			this.setHidden(true);
		} else {
			this.setHidden(false);
		}
	},
	
	//-------------------------------------------------------------------------

	'serializeData': function() {
		var result;
		
//MochiKit.Logging.logDebug(">>> RecordField.serializeData - " + this);
		result = {
			label:  this.label(),
			value:	this.value(),
			type:   this.type(),
			hidden: this.hidden()
		};
//MochiKit.Logging.logDebug("<<< RecordField.serializeData");

		return result;
	},
	
	//-------------------------------------------------------------------------

	'typeShortDescription': function() {
//		return Clipperz.PM.DataModel.RecordField.TypeDescriptions[this.type()]['shortDescription'];
		return Clipperz.PM.Strings['recordFieldTypologies'][this.type()]['shortDescription'];
	},

	//-------------------------------------------------------------------------

	'hidden': function() {
		return this._hidden;
	},
	
	'setHidden': function(aValue) {
		this._hidden = aValue;
	},

	//-------------------------------------------------------------------------

	'clone': function(aRecordVersion) {
		var result;

		result = new Clipperz.PM.DataModel.RecordField({
			recordVersion:aRecordVersion,
			label:this.label(),
			value:this.value(),
			type:this.type(),
			hidden:this.hidden()
		});
		
		return result;
	},

	//-------------------------------------------------------------------------

	'isEmpty': function() {
		var result;
		
		if ((this.label() == "") && (this.value() == "") && (this.type() == 'TXT')) {
			result = true;
		} else {
			result = false;
		}
		
		return result;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});

//#############################################################################
/*
Clipperz.PM.DataModel.RecordField.TypeDescriptions = {
	'TXT': {
		description: 'simple text field',
		shortDescription: 'txt'
	},
	'PWD': {
		description: 'simple text field, with default status set to hidden',
		shortDescription: 'pwd'
	},
	'URL': {
		description: 'simple text field in edit mode, that became an active url in view mode',
		shortDescription: 'url'
	},
	'DATE': {
		description: 'a value set with a calendar helper',
		shortDescription: 'date'
	},
	'ADDR': {
		description: 'just like the URL, but the active link points to Google Maps (or similar service) passing the address value as argument',
		shortDescription: 'addr'
	},
	'CHECK': {
		description: 'check description',
		shortDescription: 'check'
	},
	'RADIO': {
		description: 'radio description',
		shortDescription: 'radio'
	},
	'SELECT': {
		description: 'select description',
		shortDescription: 'select'
	}
	
//	'NOTE': {
//		description: 'a simple text field, but with a bigger component dimension; possibly with "smart edit components"',
//		shortDescription: 'note'
//	}
};

Clipperz.PM.DataModel.RecordField.InputTypeToRecordFieldType = {
	'text': 'TXT',
	'password': 'PWD',
	'checkbox': 'CHECK',
	'radio': 'RADIO',
	'select': 'SELECT'
};
*/
