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
//if (typeof(Clipperz.PM.BookmarkletProcessor) == 'undefined') { Clipperz.PM.BookmarkletProcessor = {}; }
//if (typeof(Clipperz.PM.BookmarkletProcessor.versions) == 'undefined') { Clipperz.PM.BookmarkletProcessor.versions = {}; }

/*
Clipperz.PM.BookmarkletProcessor.versions['abstract'] = function(anUser, aConfiguration) {
	this._user = anUser;
	this._configuration = aConfiguration;
	
	this._recordTitle = null;
	this._record = null;
	this._editableFields = null;
	
	return this;
}


Clipperz.PM.BookmarkletProcessor.versions['abstract'].prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "BookmarkletProcessor - " + this.user();
	},

	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},
	
	//-------------------------------------------------------------------------

	'configuration': function() {
		return this._configuration;
	},

	//-------------------------------------------------------------------------
	
	'record': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
*/

Clipperz.PM.BookmarkletProcessor = function(anUser, aConfiguration) {
	this._user = anUser;
	this._configuration = aConfiguration;
	
	this._recordTitle = null;
	this._record = null;
	this._editableFields = null;
	this._favicon = null;
	
	return this;
}

Clipperz.PM.BookmarkletProcessor.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "BookmarkletProcessor - " + this.user();
	},

	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},
	
	//-------------------------------------------------------------------------

	'configuration': function() {
		return this._configuration;
	},

	//-------------------------------------------------------------------------

	'recordTitle': function() {
		if (this._recordTitle == null) {
			this._recordTitle = this.configuration().page.title;
		}
		
		return this._recordTitle;
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
			
			actionUrl = Clipperz.Base.sanitizeUrl(this.configuration()['form']['attributes']['action']);
//MochiKit.Logging.logDebug("+++ actionUrl: " + actionUrl);
			this._hostname = actionUrl.replace(/^https?:\/\/([^\/]*)\/.*/, '$1');
		}
		
		return this._hostname;
	},
	
	'favicon': function() {
		if (this._favicon == null) {
			this._favicon = "http://" + this.hostname() + "/favicon.ico";
//MochiKit.Logging.logDebug("+++ favicon: " + this._favicon);
		}
		
		return this._favicon;
	},
	
	//-------------------------------------------------------------------------
	
	'record': function() {
		if (this._record == null) {
			var record;
			var recordVersion;
			var directLogin;
			var bindings;
			var i,c;
			
			record = new Clipperz.PM.DataModel.Record({
								label:this.recordTitle(),
								notes:"",
								user:this.user()
			});
			recordVersion = new Clipperz.PM.DataModel.RecordVersion(record, {})
			record.setCurrentVersion(recordVersion);
			
			bindings = {};
			
			c = this.editableFields().length;
			for (i=0; i<c; i++) {
				var formField;
				var recordField;

//MochiKit.Logging.logDebug(">>> adding a field");
				formField = this.editableFields()[i];
				recordField = new Clipperz.PM.DataModel.RecordField({
									recordVersion:recordVersion,
									label:formField['name'],
									value:formField['value'],
									type:Clipperz.PM.Strings.inputTypeToRecordFieldType[formField['type']],
									hidden:false
				});
				recordVersion.addField(recordField);
				
				bindings[formField['name']] = recordField.key();
//MochiKit.Logging.logDebug("<<< adding a field");
			}

			directLogin = new Clipperz.PM.DataModel.DirectLogin({
								record:record,
								label:this.recordTitle() + Clipperz.PM.Strings['newDirectLoginLabelSuffix'],
//								bookmarkletVersion:this.version(),
								bookmarkletVersion:'0.2',
								favicon:this.favicon(),
								formData:this.configuration()['form'],
								bindingData:bindings
			});
			record.addDirectLogin(directLogin);

			this.user().addRecord(record);
			
			this._record = record;
		}
		
		return this._record;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

//#############################################################################

Clipperz.PM.BookmarkletProcessor.createRecordFromBookmarkletConfiguration = function(anUser, aConfiguration) {
	var processor;

	processor = new Clipperz.PM.BookmarkletProcessor(anUser, aConfiguration);
	
	return processor.record();
};

//-----------------------------------------------------------------------------

Clipperz.PM.BookmarkletProcessor.sanitizeBookmarkletConfiguration = function(aConfiguration) {
	var result;
	
//	throw "XSS Bookmarklet attempt";

	result = aConfiguration;
	
	return result;
};

//-----------------------------------------------------------------------------

Clipperz.PM.BookmarkletProcessor.checkBookmarkletConfiguration = function(aConfiguration, aButton, aCallback) {
	var result;
	
	try {
		result = Clipperz.Base.evalJSON(aConfiguration);
		result = Clipperz.PM.BookmarkletProcessor.sanitizeBookmarkletConfiguration(result);
		
		if (result['version'] != '0.2.3') {
			throw "WrongBookmarkletVersion";
		}
	} catch (exception) {
		var title;
		var message;

		if (exception == "WrongBookmarkletVersion") {
			title = Clipperz.PM.Strings['newRecordPanelWrongBookmarkletVersionExceptionTitle'];
			message = Clipperz.PM.Strings['newRecordPanelWrongBookmarkletVersionExceptionMessage'];
		} else {
			title = Clipperz.PM.Strings['newRecordPanelGeneralExceptionTitle'];
			message = Clipperz.PM.Strings['newRecordPanelGeneralExceptionMessage'];
		}
		Clipperz.PM.Components.MessageBox().show({
			title:title,
			text:message,
			width:240,
			fn:aCallback,
			closable:false,
			showProgressBar:false,
			showCloseButton:false,
			buttons:{'ok':Clipperz.PM.Strings['newRecordPanelExceptionPanelCloseButtonLabel']}
		}, aButton);

		throw exception;
	}
	
	return result;
};

//-----------------------------------------------------------------------------
