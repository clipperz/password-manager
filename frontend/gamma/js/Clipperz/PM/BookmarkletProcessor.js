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
if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }

Clipperz.PM.BookmarkletProcessor = function(aConfiguration) {
	this._configuration = aConfiguration;
	
	this._editableFields = null;
	this._favicon = null;
	
	return this;
}

Clipperz.PM.BookmarkletProcessor.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "Clipperz.PM.BookmarkletProcessor";
	},

	//-------------------------------------------------------------------------

	'configuration': function() {
		return this._configuration;
	},

	//-------------------------------------------------------------------------

	'pageTitle': function() {
		return this.configuration().page.title;
	},

	//-------------------------------------------------------------------------

	'fields': function() {
		return this.configuration().form.inputs;
	},

	//-------------------------------------------------------------------------

	'editableFields': function() {
		if (this._editableFields == null) {
			this._editableFields = MochiKit.Base.filter(function(aField) {
				var result;
				var type;
		
				type = aField['type'].toLowerCase();
				result = ((type != 'hidden') && (type != 'submit') && (type != 'checkbox') && (type != 'radio') && (type != 'select'));
		
				return result;
			}, this.fields())
		}
		
		return this._editableFields;
	},

	//-------------------------------------------------------------------------

	'hostname': function() {
		if (this._hostname == null) {
			var actionUrl;
			
			actionUrl = this.configuration()['form']['attributes']['action'];
			this._hostname = actionUrl.replace(/ ^ h t t p s ? : \ / \ / ( [ ^ \ / ] * ) \ / . * /, '$1');
		}
		
		return this._hostname;
	},
	
	'favicon': function() {
		if (this._favicon == null) {
			this._favicon = "http://" + this.hostname() + "/favicon.ico";
		}
		
		return this._favicon;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

//#############################################################################
/ *
Clipperz.PM.BookmarkletProcessor.createRecordFromBookmarkletConfiguration = function(anUser, aConfiguration) {
	var processor;
	var record;
	var recordVersion;
	var directLogin;
	var bindings;
	var i,c;

	processor = new Clipperz.PM.BookmarkletProcessor(aConfiguration);
			
	record = new Clipperz.PM.DataModel.Record({
		'label':	processor.pageTitle(),
		'notes':	"",
		'user':		anUser
	});
	recordVersion = new Clipperz.PM.DataModel.Record.Version(record, {})
	record.setCurrentVersion(recordVersion);
	
	bindings = {};
	
	c = processor.editableFields().length;
	for (i=0; i<c; i++) {
		var formField;
		var recordField;

		formField = processor.editableFields()[i];
		recordField = new Clipperz.PM.DataModel.RecordField({
			'label':	formField['name'],
			'value':	formField['value'],
			'type':		Clipperz.PM.Strings.inputTypeToRecordFieldType[formField['type']],
			'hidden': 	false,
			'recordVersion':	recordVersion
		});
		recordVersion.addField(recordField);
		
		bindings[formField['name']] = recordField.key();
	}

	directLogin = new Clipperz.PM.DataModel.DirectLogin({
		'record':		record,
		'label':		processor.pageTitle(),
		'favicon':		processor.favicon(),
		'formData':		processor.configuration()['form'],
		'bindingData':	bindings,
		'bookmarkletVersion':	'0.2'
	});
	record.addDirectLogin(directLogin);

	anUser.addRecord(record);
	
	return record;
};
* /
//-----------------------------------------------------------------------------

Clipperz.PM.BookmarkletProcessor.sanitizeBookmarkletConfiguration = function(aConfiguration) {
	var result;
	
//	throw "XSS Bookmarklet attempt";

	result = aConfiguration;
	
	return result;
};

//-----------------------------------------------------------------------------

Clipperz.PM.BookmarkletProcessor.checkBookmarkletConfiguration = function(aConfiguration) {
	var result;
	
	try {
		result = Clipperz.Base.evalJSON(aConfiguration);
		result = Clipperz.PM.BookmarkletProcessor.sanitizeBookmarkletConfiguration(result);
		
		if (result['version'] != '0.2.3') {
			throw "WrongBookmarkletVersion";
		}
	} catch (exception) {
		throw exception;
	}
	
	return result;
};

//-----------------------------------------------------------------------------
*/