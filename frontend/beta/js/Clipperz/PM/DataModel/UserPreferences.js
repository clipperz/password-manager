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

Clipperz.PM.DataModel.UserPreferences = function(args) {
	args = args || {};

	this._user = args['user']; delete args['user'];
	this._config = args;
	
	return this;
}

Clipperz.PM.DataModel.UserPreferences.prototype = MochiKit.Base.update(null, {

	//-------------------------------------------------------------------------

	'config': function() {
		return this._config;
	},

	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},
	
	//-------------------------------------------------------------------------

	'updateWithData': function(someValues) {
		var currentLanguage;

//MochiKit.Logging.logDebug(">>> Clipperz.PM.DataModel.UserPreferences.updateWithData: " + Clipperz.Base.serializeJSON(someValues));
		currentLanguage = this.preferredLanguage();
		
		MochiKit.Base.update(this._config, someValues);

		if (this.preferredLanguage() != currentLanguage) {
			Clipperz.PM.Strings.Languages.setSelectedLanguage(this.preferredLanguage());
		} else {
//MochiKit.Logging.logDebug("### keepping the browser selected language: " + Clipperz.PM.Strings.selectedLanguage);
		}

		return this;
	},

	//-------------------------------------------------------------------------

	'configValue': function(aConfigName, aDefaultValue) {
		var result;
		
//MochiKit.Logging.logDebug(">>> UserPreferences.configValue - config: " + Clipperz.Base.serializeJSON(this.config()));
		if (typeof(this.config()[aConfigName]) == 'undefined') {
			result = aDefaultValue;
		} else {
			result = this.config()[aConfigName];
		}
//MochiKit.Logging.logDebug("<<< UserPreferences.configValue");
		
		return result;
	},

	'setConfigValue': function(aConfigName, aValue) {
		var result;
		
		if (aValue != this.configValue(aConfigName)) {
			if (aValue == null) {
				delete this.config()[aConfigName]
			} else {
				this.config()[aConfigName] = aValue;
			}
		
			Clipperz.NotificationCenter.notify(this.user(), 'updatedSection', 'preferences', true);
			
			result = true;
		} else {
			result = false;
		}
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'useSafeEditMode': function() {
		return this.configValue('useSafeEditMode', true);
	},
	
	'setUseSafeEditMode': function(aValue) {
		this.setConfigValue('useSafeEditMode', aValue);
	},
	
	//-------------------------------------------------------------------------

	'preferredLanguage': function() {
		return this.configValue('preferredLanguage', null);
	},
	
	'setPreferredLanguage': function(aValue) {
		if (this.setConfigValue('preferredLanguage', aValue)) {
			Clipperz.PM.Strings.Languages.setSelectedLanguage(this.preferredLanguage());
		}
	},

	//-------------------------------------------------------------------------

	'shouldShowDonationPanel': function() {
		return this.configValue('shouldShowDonationPanel', true);
	},
	
	'setShouldShowDonationPanel': function(aValue) {
		this.setConfigValue('shouldShowDonationPanel', aValue);
	},

	//-------------------------------------------------------------------------
	
	'disableUnsecureFaviconLoadingForIE': function() {
		return this.configValue('disableUnsecureFaviconLoadingForIE', false);
	},
	
	'setDisableUnsecureFaviconLoadingForIE': function(aValue) {
		this.setConfigValue('disableUnsecureFaviconLoadingForIE', aValue);
	},
	
	//-------------------------------------------------------------------------
	
	'serializedData': function() {
		return this.config();
	},

	//-------------------------------------------------------------------------

	'saveChanges': function(aReferenceElement) {
		var	deferredResult;

		deferredResult = new MochiKit.Async.Deferred();
		
		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Components.MessageBox(), 'deferredShow'),
			{
				title:"",	//	Clipperz.PM.Strings['accountPreferencesSavingPanelTitle_Step1'],
				text:"",	//	Clipperz.PM.Strings['accountPreferencesSavingPanelText_Step1'],
				width:240,
				showProgressBar:true,
				showCloseButton:false
			},
			aReferenceElement
		);
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'account_savingPreferences_1');
		deferredResult.addCallback(MochiKit.Base.method(this.user(), 'encryptedData'));
		deferredResult.addCallback(function(res) {
			return {user:res};
		})
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'account_savingPreferences_2');
//		deferredResult.addCallback(MochiKit.Base.method(this.user().connection(), 'message'), 'updateData');
		deferredResult.addCallback(MochiKit.Base.method(this.user().connection(), 'message'), 'saveChanges');
		deferredResult.addCallback(Clipperz.PM.Components.MessageBox().hide, YAHOO.ext.Element.get('main'));
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedPreferences', null);

		deferredResult.callback();
		
		return deferredResult;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

