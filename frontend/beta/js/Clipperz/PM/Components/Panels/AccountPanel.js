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
if (typeof(Clipperz.PM.Components) == 'undefined') { Clipperz.PM.Components = {}; }
if (typeof(Clipperz.PM.Components.Panels) == 'undefined') { Clipperz.PM.Components.Panels = {}; }

//#############################################################################

Clipperz.PM.Components.Panels.AccountPanel = function(anElement, args) {
//MochiKit.Logging.logDebug(">>> new AccountPanel");
	args = args || {};

    Clipperz.PM.Components.Panels.AccountPanel.superclass.constructor.call(this, anElement, args);

	Clipperz.NotificationCenter.register(null, 'setupDone', this, 'render');

	this._shouldLoadLoginHistory = true;
	
//	this.render();
//MochiKit.Logging.logDebug("<<< new AccountPanel");

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Panels.AccountPanel, Clipperz.PM.Components.Panels.BasePanel, {

	'toString': function() {
		return "Clipperz.PM.Components.AccountPanel component";
	},

	//-------------------------------------------------------------------------
 
	'render': function() {
		var errorMessageActor;
		var	changePasswordButton;
		var deleteAccountButton;

try {
//MochiKit.Logging.logDebug(">>> AccountPanel.render");
		Clipperz.NotificationCenter.unregister(this);
		MochiKit.Signal.disconnectAllTo(this);

		this.element().update("");
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'table', border:'0', cellspacing:'0', cellpadding:'0', children:[
			{tag:'tbody', children:[
				{tag:'tr', children:[
					{tag:'td', valign:'top', width:'200', children:[
						{tag:'ul', id:"accountSubMenu", cls:'subMenu', children:[
							{tag:'li', id:'changePassphraseTab', htmlString:Clipperz.PM.Strings['changePasswordTabLabel']},
							{tag:'li', id:'manageOTPTab', htmlString:Clipperz.PM.Strings['manageOTPTabLabel']},
							{tag:'li', id:'accountPreferencesTab', htmlString:Clipperz.PM.Strings['accountPreferencesLabel']},
							{tag:'li', id:'loginHistoryTab', htmlString:Clipperz.PM.Strings['accountLoginHistoryLabel']},
							{tag:'li', id:'deleteAccountTab', htmlString:Clipperz.PM.Strings['deleteAccountTabLabel']}
//							{tag:'li', id:'paidAccountTab'), htmlString:Clipperz.PM.Strings['paidAccountTabLabel']}
						]}
					]},
					{tag:'td', valign:'top', children:[
						{tag:'ul', cls:'clipperzTabPanels', children:[
							{tag:'li', id:this.getId('changePassphrasePanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['changePasswordTabTitle']},
									{tag:'div', cls:'panelBody', id:'changePassphraseBlock', children:[
										{tag:'form', id:this.getId('changePassphraseForm'), children:[
											{tag:'h5', cls:'errorMessage', id:this.getId('changePassphrase_errorMessage')},
											{tag:'table', cls:'panelBody', children:[
												{tag:'tr', children:[
													{tag:'td', children:[
														{tag:'span', cls:'formLabel', htmlString:Clipperz.PM.Strings['changePasswordFormUsernameLabel']}
													]},
													{tag:'td', children:[
														{tag:'input', type:'text', name:'username', id:this.getId('changePassphrase_username')}
													]}
												]},
												{tag:'tr', children:[
													{tag:'td', children:[
														{tag:'span', cls:'formLabel', htmlString:Clipperz.PM.Strings['changePasswordFormOldPassphraseLabel']}
													]},
													{tag:'td', children:[
														{tag:'input', type:'password', name:'oldPassphrase', id:this.getId('changePassphrase_oldPassphrase')}
													]}
												]},
												{tag:'tr', children:[
													{tag:'td', children:[
														{tag:'span', cls:'formLabel', htmlString:Clipperz.PM.Strings['changePasswordFormNewPassphraseLabel']}
													]},
													{tag:'td', children:[
														{tag:'input', type:'password', name:'newPassphrase', id:this.getId('changePassphrase_newPassphrase')}
													]}
												]},
												{tag:'tr', children:[
													{tag:'td', children:[
														{tag:'span', cls:'formLabel', htmlString:Clipperz.PM.Strings['changePasswordFormRetypePassphraseLabel']}
													]},
													{tag:'td', children:[
														{tag:'input', type:'password', name:'renewPassphrase', id:this.getId('changePassphrase_renewPassphrase')}
													]}
												]},
												{tag:'tr', children:[
													{tag:'td', align:'right', children:[
														{tag:'input', type:'checkbox', id:this.getId('changePassphrase_safetyCheck')}
													]},
													{tag:'td', children:[
														{tag:'span', htmlString:Clipperz.PM.Strings['changePasswordFormSafetyCheckboxLabel']}
													]}
												]}
											]},
											{tag:'div', cls:'clipperzSubPanelButtonBox', children:[
												{tag:'div', id:this.getId('changePassphraseButton')}
											]}
										]}
									]}
								]}
							]},
							{tag:'li', id:this.getId('manageOTPPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['manageOTPTabTitle']},
									{tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['manageOTPTabDescription']},
									{tag:'div', id:'OTPComponent'}
								]}
							]},
							{tag:'li', id:this.getId('accountPreferencesPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['accountPreferencesTabTitle']},
									{tag:'div', cls:'panelBody', id:this.getId('preferencesPanelBody')}
								]}
							]},
							{tag:'li', id:this.getId('loginHistoryAccountPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['loginHistoryTabTitle']},
									{tag:'div', cls:'panelBody', id:'loginHistoryAccountBlock'}
								]}
							]},
							{tag:'li', id:this.getId('deleteAccountPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['deleteAccountTabTitle']},

									{tag:'div', cls:'panelBody', id:'deleteAccountBlock', children:[
										{tag:'form', id:this.getId('deleteAccountForm'), children:[
											{tag:'h5', cls:'errorMessage', id:this.getId('deleteAccount_errorMessage')},
											{tag:'table', cls:'panelBody', children:[
												{tag:'tr', children:[
													{tag:'td', children:[
														{tag:'span', cls:'formLabel', htmlString:Clipperz.PM.Strings['deleteAccountFormUsernameLabel']}
													]},
													{tag:'td', children:[
														{tag:'input', type:'text', name:'username', id:this.getId('deleteAccount_username')}
													]}
												]},
												{tag:'tr', children:[
													{tag:'td', children:[
														{tag:'span', cls:'formLabel', htmlString:Clipperz.PM.Strings['deleteAccountFormPassphraseLabel']}
													]},
													{tag:'td', children:[
														{tag:'input', type:'password', name:'passphrase', id:this.getId('deleteAccount_passphrase')}
													]}
												]},
												{tag:'tr', children:[
													{tag:'td', align:'right', children:[
														{tag:'input', type:'checkbox', id:this.getId('deleteAccount_safetyCheck')}
													]},
													{tag:'td', children:[
														{tag:'span', htmlString:Clipperz.PM.Strings['deleteAccountFormSafetyCheckboxLabel']}
													]}
												]}
											]},
											{tag:'div', cls:'clipperzSubPanelButtonBox', children:[
												{tag:'div', id:this.getId('deleteAccountButton')}
											]}
										]}
									]}
								]}
							]}
/*							
							{tag:'li', id:this.getId('paidAccountPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['upgradeAccountTabTitle']},
									{tag:'div', htmlString:Clipperz.PM.Strings['comingSoon']}
								]}
							]}
*/
						]}
					]}
				]}
			]}
		]});
		
//MochiKit.Logging.logDebug("--- AccountPanel.render - 1");
		MochiKit.Signal.connect(this.getId('changePassphraseForm'), 'onkeydown', this, 'onkeydown');
		errorMessageActor = this.getActor('changePassphrase_errorMessage');
		errorMessageActor.setVisibilityMode(YAHOO.ext.Element.DISPLAY);
		errorMessageActor.update("---");
		errorMessageActor.hide();
		changePasswordButton = new YAHOO.ext.Button(this.getDom('changePassphraseButton'), {text:Clipperz.PM.Strings['changePasswordFormSubmitLabel'], handler:this.doChangePassphrase, scope:this});

//MochiKit.Logging.logDebug("--- AccountPanel.render - 2");
		
		MochiKit.Signal.connect(this.getId('deleteAccountForm'), 'onkeydown', this, 'onkeydown');
		errorMessageActor = this.getActor('deleteAccount_errorMessage');
		errorMessageActor.setVisibilityMode(YAHOO.ext.Element.DISPLAY);
		errorMessageActor.update(Clipperz.PM.Strings['deleteAccountFormEmptyErrorMessage']);
		errorMessageActor.hide();
		deleteAccountButton = new YAHOO.ext.Button(this.getDom('deleteAccountButton'), {text:Clipperz.PM.Strings['deleteAccountFormSubmitLabel'], handler:this.doDeleteAccount, scope:this});
//MochiKit.Logging.logDebug("--- AccountPanel.render - 5");

		if (Clipperz.PM.Proxy.defaultProxy.isReadOnly()) {
			this.getElement('changePassphraseForm').addClass('read-only');
//			this.getElement('accountPreferencesForm').addClass('read-only');
			this.getElement('deleteAccountForm').addClass('read-only');
			changePasswordButton.disable();
			deleteAccountButton.disable();
		}
//MochiKit.Logging.logDebug("--- AccountPanel.render - 6");

		new Clipperz.PM.Components.PasswordEntropyDisplay(this.getElement('changePassphrase_oldPassphrase'));
		new Clipperz.PM.Components.PasswordEntropyDisplay(this.getElement('changePassphrase_newPassphrase'));
		
		new Clipperz.PM.Components.OTP.MainComponent(YAHOO.ext.Element.get('OTPComponent'), {user:this.user()});
		
		this.tabPanelController().setUp();
		Clipperz.NotificationCenter.register(null, 'tabSelected', this, 'tabSelectedHandler');
		Clipperz.NotificationCenter.register(null, 'updatedPreferences', this, 'renderPreferences');
		Clipperz.NotificationCenter.register(null, 'switchLanguage', this, 'switchLanguageHandler');
//MochiKit.Logging.logDebug("<<< AccountPanel.render");

} catch(exception) {
	MochiKit.Logging.logError("### " + exception);
	throw exception;
}
	},
	
	//-------------------------------------------------------------------------

	'tabPanelController': function() {
		if (this._tabPanelController == null) {
			var tabPanelControllerConfig;
			
			tabPanelControllerConfig = {}
			tabPanelControllerConfig['changePassphraseTab'] = this.getId('changePassphrasePanel');
			tabPanelControllerConfig['manageOTPTab'] = this.getId('manageOTPPanel');
			tabPanelControllerConfig['accountPreferencesTab'] = this.getId('accountPreferencesPanel');
			tabPanelControllerConfig['loginHistoryTab'] = this.getId('loginHistoryAccountPanel');
			tabPanelControllerConfig['deleteAccountTab'] = this.getId('deleteAccountPanel');
//			tabPanelControllerConfig['paidAccountTab'] = this.getId('paidAccountPanel');

			this._tabPanelController = new Clipperz.PM.Components.TabPanel.TabPanelController({
												name:'accountTabPanel',
												config:tabPanelControllerConfig,
												selectedTab:'changePassphraseTab'
			});
		}
		
		return this._tabPanelController;
	},
	
	//-------------------------------------------------------------------------

	'doChangePassphrase': function() {
		if (Clipperz.PM.Proxy.defaultProxy.isReadOnly() == false) {
			var	username;
			var	oldPassphrase;
			var newPassphrase;
			var renewPassphrase;
			var safetyCheck;
			var	areThereAnyErrors;
			var errorMessageActor;

			errorMessageActor = this.getActor('changePassphrase_errorMessage');

			areThereAnyErrors = false;
			username = this.getDom('changePassphrase_username').value;
			oldPassphrase= this.getDom('changePassphrase_oldPassphrase').value;
			newPassphrase= this.getDom('changePassphrase_newPassphrase').value;
			renewPassphrase= this.getDom('changePassphrase_renewPassphrase').value;
			safetyCheck = this.getDom('changePassphrase_safetyCheck').checked;

			if (this.user().username() != username) {
				this.showFormErrorMessageAnimation(errorMessageActor, Clipperz.PM.Strings['changePasswordFormWrongUsernameWarning']);
				this.getElement('changePassphrase_username').focus().dom.select();
				areThereAnyErrors = true;
			} else if (this.user().passphrase() != oldPassphrase) {
				this.showFormErrorMessageAnimation(errorMessageActor, Clipperz.PM.Strings['changePasswordFormWrongPassphraseWarning']);
				this.getElement('changePassphrase_oldPassphrase').focus().dom.select();
				areThereAnyErrors = true;
			} else if (newPassphrase != renewPassphrase) {
				this.showFormErrorMessageAnimation(errorMessageActor, Clipperz.PM.Strings['changePasswordFormWrongRetypePassphraseWarning']);
				this.getElement('changePassphrase_renewPassphrase').focus().dom.select();
				areThereAnyErrors = true;
			} else if (safetyCheck != true) {
				this.showFormErrorMessageAnimation(errorMessageActor, Clipperz.PM.Strings['changePasswordFormSafetyCheckWarning']);
				this.getElement('changePassphrase_safetyCheck').focus();
				areThereAnyErrors = true;
			}

			if (areThereAnyErrors == false) {
				errorMessageActor.hide();
				this.doChangePassphraseWithUsernameAndPassphrase(username, newPassphrase);
			}
		}
	},

	//-------------------------------------------------------------------------
	
	'doChangePassphraseWithUsernameAndPassphrase': function(anUsername, aPassphrase) {
		var deferredResult;

//MochiKit.Logging.logDebug(">>> AccountPanel.doChangePassphraseWithUsernameAndPassphrase - this.user: " + this.user());
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug(" AccountPanel.doChangePassphraseWithUsernameAndPassphrase 1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Components.MessageBox(), 'deferredShow'),
			{
				title:Clipperz.PM.Strings['changePasswordFormProgressDialogTitle'],
				text:Clipperz.PM.Strings['changePasswordFormProgressDialogEmptyText'],
				width:240,
				showProgressBar:true,
				showCloseButton:false,
				steps:4
			},
			this.getDom('changePassphraseButton')
		);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug(" AccountPanel.doChangePassphraseWithUsernameAndPassphrase 2: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this.user(), 'changeCredentials'), anUsername, aPassphrase);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug(" AccountPanel.doChangePassphraseWithUsernameAndPassphrase 3: " + res); return res;});
		deferredResult.addCallback(function() {
			Clipperz.PM.Components.MessageBox().update({
				title:Clipperz.PM.Strings['changePasswordFormProgressDialogConnectedMessageTitle'],
				text:Clipperz.PM.Strings['changePasswordFormProgressDialogConnectedMessageText'],
				/*showProgressBar:false,*/
				step:'next'
			});
		});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug(" AccountPanel.doChangePassphraseWithUsernameAndPassphrase 4: " + res); return res;});
		deferredResult.addCallback(MochiKit.Async.wait, 1);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug(" AccountPanel.doChangePassphraseWithUsernameAndPassphrase 5: " + res); return res;});
		deferredResult.addCallback(function(anAccountPanel, res) {
			Clipperz.PM.Components.MessageBox().hide(YAHOO.ext.Element.get('main'));
			
			anAccountPanel.getDom('changePassphrase_username').value = "";
			anAccountPanel.getDom('changePassphrase_oldPassphrase').value = "";
			anAccountPanel.getElement('changePassphrase_oldPassphrase').focus();
			anAccountPanel.getDom('changePassphrase_newPassphrase').value = "";
			anAccountPanel.getElement('changePassphrase_newPassphrase').focus();
			anAccountPanel.getDom('changePassphrase_renewPassphrase').value = "";
			anAccountPanel.getDom('changePassphrase_safetyCheck').checked = false;

			anAccountPanel.getElement('changePassphrase_username').focus();
			return res;
		}, this);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug(" AccountPanel.doChangePassphraseWithUsernameAndPassphrase 6: " + res); return res;});
		deferredResult.addErrback(function() {
			Clipperz.PM.Components.MessageBox().update({
				title:Clipperz.PM.Strings['changePasswordFormProgressDialogErrorMessageTitle'],
				text:Clipperz.PM.Strings['changePasswordFormProgressDialogErrorMessageText'],
				buttons:{'ok':"close"}
			});
		});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug(" AccountPanel.doChangePassphraseWithUsernameAndPassphrase 7: " + res); return res;});
		deferredResult.callback();

//MochiKit.Logging.logDebug("<<< AccountPanel.doChangePassphraseWithUsernameAndPassphrase");
	},

	//-------------------------------------------------------------------------
	
	'doDeleteAccount': function() {
		if (Clipperz.PM.Proxy.defaultProxy.isReadOnly() == false) {
			var	username;
			var	passphrase;
			var safetyCheck;
			var	areThereAnyErrors;
			var errorMessageActor;
		
			errorMessageActor = this.getActor('deleteAccount_errorMessage');

			areThereAnyErrors = false;
			username = this.getDom('deleteAccount_username').value;
			passphrase= this.getDom('deleteAccount_passphrase').value;
			safetyCheck = this.getDom('deleteAccount_safetyCheck').checked;

			if (this.user().username() != username) {
				this.showFormErrorMessageAnimation(errorMessageActor, Clipperz.PM.Strings['deleteAccountFormWrongUsernameWarning']);
				this.getElement('deleteAccount_username').focus().dom.select();
				areThereAnyErrors = true;
			} else if (this.user().passphrase() != passphrase) {
				this.showFormErrorMessageAnimation(errorMessageActor, Clipperz.PM.Strings['deleteAccountFormWrongPassphraseWarning']);
				this.getElement('deleteAccount_passphrase').focus().dom.select();
				areThereAnyErrors = true;
			} else if (safetyCheck != true) {
				this.showFormErrorMessageAnimation(errorMessageActor, Clipperz.PM.Strings['deleteAccountFormSafetyCheckWarning']);
				this.getElement('deleteAccount_safetyCheck').focus();
				areThereAnyErrors = true;
			}

			if (areThereAnyErrors == false) {
				var deferred;
			
				deferred = new MochiKit.Async.Deferred();
				errorMessageActor.hide();

				deferred.addCallback(function() {
					var deferredResult;

					//	TODO: if the form is submitted with the return key, the confirmation dialog is skipped!?
					deferredResult = new MochiKit.Async.Deferred();
					Clipperz.PM.Components.MessageBox().deferredShow({
						title:Clipperz.PM.Strings['accountPanelDeletingAccountPanelConfirmationTitle'],
						text:Clipperz.PM.Strings['accountPanelDeleteAccountPanelConfirmationText'],
						width:240,
						showProgressBar:false,
						showCloseButton:false,
						buttons:{
							'yes':Clipperz.PM.Strings['accountPanelDeleteAccountPanelConfirmButtonLabel'],
							'no':Clipperz.PM.Strings['accountPanelDeleteAccountPanelDenyButtonLabel']
	 					},
						fn:MochiKit.Base.partial(function(aDeferred, aResult) {
							if (aResult == 'yes') {
								aDeferred.callback(aResult);
							} else {
								aDeferred.errback(aResult);
							}
						}, deferredResult)
					});
				
					return deferredResult;
				});
				deferred.addCallback(MochiKit.Base.method(Clipperz.PM.Components.MessageBox(), 'deferredShow'),
					{
						title:Clipperz.PM.Strings['accountPanelDeletingAccountPanelProgressTitle'],
						text:Clipperz.PM.Strings['accountPanelDeletingAccountPanelProgressText'],
						width:240,
						showProgressBar:true,
						showCloseButton:false
					}
				);
				deferred.addCallback(MochiKit.Base.method(this.user(), 'deleteAccountAction'));
				deferred.addCallback(Clipperz.PM.exit, 'accountDeleted.html');
				deferred.addErrback(function(res) {
					alert(res);
				})
				deferred.callback();
			}
		}
	},
	
	//-------------------------------------------------------------------------

	'showFormErrorMessageAnimation': function(anActor, anErrorMessage, aCallback) {
		anActor.update(anErrorMessage);
		anActor.show(true);
		anActor.play(aCallback);
	},

	//-------------------------------------------------------------------------

	'onkeydown': function(anEvent) {
//MochiKit.Logging.logDebug(">>> onkeydown - " + anEvent.src().id);
		if (anEvent.key().code == 13) {
			anEvent.stop();

			if (anEvent.src() == this.getDom('changePassphraseForm')) {
				this.doChangePassphrase();
			} else if (anEvent.src() == this.getDom('deleteAccountForm')) {
				this.doDeleteAccount();
			} else {
			}
			
		}
	},

	//-------------------------------------------------------------------------

	'selectSelectedLanguageOption': function() {
		var	userSelectedLanguage;

		userSelectedLanguage = this.user().preferences().preferredLanguage() || "default";
		MochiKit.Base.filter(function(anOption) {return (anOption.value == userSelectedLanguage)}, this.getDom('languageSelector').childNodes)[0].selected = true;
	},

	//-------------------------------------------------------------------------

	'doSaveUserPreferences': function() {
		var selectedLanguage;
		var showDonationReminderDialog;
//		var disableUnsecureFaviconLoadingForIE;
		
//MochiKit.Logging.logDebug(">>> AccountPanel.doSaveUserPreferences");
		selectedLanguage = this.getDom('languageSelector').value;
		if (selectedLanguage == "default") {
			selectedLanguage = null;
		}
		this.user().preferences().setPreferredLanguage(selectedLanguage);

		showDonationReminderDialog = this.getDom('showDonationReminderCheckbox').checked;
		this.user().preferences().setShouldShowDonationPanel(showDonationReminderDialog);

//		disableUnsecureFaviconLoadingForIE = this.getDom('disableFaviconForIECheckbox').checked;
//		this.user().preferences().setDisableUnsecureFaviconLoadingForIE(disableUnsecureFaviconLoadingForIE);
		
		this.user().preferences().saveChanges(this.getDom('saveUserPreferences'));
	},
	
	'doCancelUserPreferences': function() {
		this.renderPreferences();
	},

//	'switchLanguage': function(anEvent) {
//		Clipperz.PM.Strings.Languages.setSelectedLanguage(anEvent.src().value);
//	},

	//-------------------------------------------------------------------------

	'renderLoginHistory': function() {
		var element;
		
//MochiKit.Logging.logDebug(">>> AccountPanel.renderLoginHistory");
		element = YAHOO.ext.Element.get('loginHistoryAccountBlock');

		if (Clipperz.PM.Proxy.defaultProxy.isReadOnly()) {
			element.update("");
			this.domHelper().append(element, {tag:'div', cls:'loginHistoryReadOnlyMessage', htmlString:Clipperz.PM.Strings['loginHistoryReadOnlyMessage']});
		} else {
			var deferredResult;
			
			deferredResult = new MochiKit.Async.Deferred();
			deferredResult.addCallback(MochiKit.Base.bind(function(anElement) {
				anElement.update("");
				Clipperz.YUI.DomHelper.append(anElement, {tag:'div', cls:'loadingMessage', htmlString:Clipperz.PM.Strings['loginHistoryLoadingMessage']});
			}, this), element);
			deferredResult.addCallback(MochiKit.Base.method(this.user(), 'loadLoginHistory'));
			deferredResult.addCallback(MochiKit.Base.bind(function(anElement, aResult) {
				var loginListItems;
				var tBodyElement;
				var imageExtension;
				var now;
				var i, c;

				loginListItems = aResult;
//MochiKit.Logging.logDebug("=== loginListItems: " + Clipperz.Base.serializeJSON(loginListItems));
				imageExtension = (Clipperz_IEisBroken == true) ? 'gif': 'png';

				now = new Date();
				anElement.update("");
				Clipperz.YUI.DomHelper.append(anElement, {tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['loginHistoryLoadedMessage']});
				Clipperz.YUI.DomHelper.append(anElement, {tag:'table', id:'loginHistoryTable', cellspacing:'0', cellpadding:'2', border:'0', children:[
					{tag:'tbody', id:this.getId('loginHistoryTBody'), children:[]}
				]});
//#				Clipperz.YUI.DomHelper.append(anElement, {tag:'div', id:'loginHistoryFooter', children:[
				Clipperz.YUI.DomHelper.append(anElement, {tag:'div', cls:'clipperzSubPanelButtonBox', children:[
					{tag:'div', id:this.getId('reloadHistoryButton')}
				]});
				
				new YAHOO.ext.Button(this.getDom('reloadHistoryButton'), {text:Clipperz.PM.Strings['loginHistoryReloadButtonLabel'], handler:this.reloadHistory, scope:this});
				
				tBodyElement = this.getElement('loginHistoryTBody');
				c = loginListItems.length;
				for (i=0; i<c; i++) {
					var ip;
					var date;
					var mainText;

					date = Clipperz.PM.Date.parseDateWithUTCFormat(loginListItems[i]['date']);

					if (loginListItems[i]['isCurrent'] === true) {
						mainText ={tag:'div', cls:'currentSession', htmlString:Clipperz.PM.Strings['loginHistoryCurrentSessionText']}
					} else {
						mainText = {tag:'div', cls:'elapsedTime', html:Clipperz.PM.Date.getElapsedTimeDescription(date)}
					}
					
					if (loginListItems[i]['connectionType'] == "ONE_TIME_PASSPHRASE") {
						optionalInfo = [
							{tag:'span', html:"OTP"}
						];
					} else {
						optionalInfo = [];
					}
					
					ip = (loginListItems[i]['ip'].match(/^\d{1,3}(.\d{1,3}){3}$/)) ? loginListItems[i]['ip'] : Clipperz.PM.Strings['unknown_ip'];
					Clipperz.YUI.DomHelper.append(tBodyElement, {tag:'tr', children:[
						{tag:'td', cls:'loginHistoryValues', valign:'top', children:[
							mainText,
							{tag:'div', cls:'fullDate', html:Clipperz.PM.Date.formatDateWithTemplate(date, Clipperz.PM.Strings['fullDate_format'])},
							{tag:'div', cls:'loginHistoryIP', children:[
								{tag:'span', cls:'loginHistoryIPLabel', htmlString:Clipperz.PM.Strings['loginHistoryIPLabel']},
								{tag:'span', cls:'loginHistoryIPValue', html:ip}
							]}
						]},
						{tag:'td', cls:'loginHistoryCountry', valign:'top', children:optionalInfo},
						{tag:'td', cls:'loginHistoryCountry', valign:'top', align:'center', children:[
							{tag:'img', title:Clipperz.PM.Strings['countries'][loginListItems[i]['country']], cls:'flag', src:Clipperz.PM.Strings['icons_baseUrl'] + "/flags/" + loginListItems[i]['country'].toLowerCase() + "." +  imageExtension, width:'32', height:'32'}
//							{tag:'span', cls:'label', htmlString:Clipperz.PM.Strings['countries'][loginListItems[i]['country']]}
						]},
						{tag:'td', cls:'loginHistoryBrowser', valign:'top', align:'center', children:[
							{tag:'img', title:Clipperz.PM.Strings['browsers'][loginListItems[i]['browser']], cls:'browser', src:Clipperz.PM.Strings['icons_baseUrl'] + "/browsers/" + loginListItems[i]['browser'].toLowerCase() + "." + imageExtension, width:'32', height:'32'}
//							{tag:'span', cls:'label', htmlString:Clipperz.PM.Strings['browsers'][loginListItems[i]['browser']]}
						]},
						{tag:'td', cls:'loginHistoryOperatingSystem', valign:'top', align:'center', children:[
							{tag:'img', title:Clipperz.PM.Strings['operatingSystems'][loginListItems[i]['operatingSystem']], cls:'operatingSystem', src:Clipperz.PM.Strings['icons_baseUrl'] + "/operatingSystems/" + loginListItems[i]['operatingSystem'].toLowerCase() + "." + imageExtension, width:'32', height:'32'}
//							{tag:'span', cls:'label', htmlString:Clipperz.PM.Strings['operatingSystems'][loginListItems[i]['operatingSystem']]}
						]}
					]});
				}

				Clipperz.Style.applyZebraStylesToTable('loginHistoryTable');
			}, this), element);
			
			deferredResult.callback();
		}
//MochiKit.Logging.logDebug("<<< AccountPanel.renderLoginHistory");
	},

	//-------------------------------------------------------------------------

	'renderPreferences': function() {
		var saveUserPreferencesButton;
		var cancelUserPreferencesButton;
		var preferencedPanelBodyElement;
		
		preferencedPanelBodyElement = this.getElement('preferencesPanelBody');
		
		preferencedPanelBodyElement.update("");
		Clipperz.YUI.DomHelper.append(preferencedPanelBodyElement,
			{tag:'form', id:this.getId('accountPreferencesForm'), children:[
				{tag:'table', cls:'panelBody', children:[
					{tag:'tr', cls:'openPreferenceBlock', children:[
						{tag:'td', children:[
							{tag:'div', cls:'preferenceBlockTitle', htmlString:Clipperz.PM.Strings['accountPreferencesLanguageTitle']},
							{tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['accountPreferencesLanguageDescription']},
							{tag:'div', cls:'panelDescription', children:[
								{tag:'select',
								 	id:this.getId('languageSelector'),
									children:MochiKit.Base.concat([{tag:'option', value:"default", html:"---"}], Clipperz.PM.Strings['loginPanelSwitchLanguageSelectOptions'])
								}
							]}
						]}
					]},
					{tag:'tr', cls:'openPreferenceBlock', children:[
						{tag:'td', children:[
							{tag:'div', cls:'preferenceBlockTitle', htmlString:Clipperz.PM.Strings['showDonationReminderPanelTitle']},
							{tag:'table', cellpadding:'0', cellspacing:'0', children:[
								{tag:'tbody', children:[
									{tag:'tr', children:[
										{tag:'td', valign:'top', children:[
											{tag:'div', cls:'panelDescription', children:[
												{tag:'input', type:'checkbox', id:this.getId('showDonationReminderCheckbox')}
											]}
										]},
										{tag:'td', valign:'top', children:[
											{tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['showDonationReminderPanelDescription']}
										]}
									]}
								]}
							]}
						]}
					]}	//,
/*
					{tag:'tr', cls:'openPreferenceBlock', children:[
						{tag:'td', children:[
							{tag:'div', cls:'preferenceBlockTitle', htmlString:Clipperz.PM.Strings['disableFaviconForIETitle']},
							{tag:'table', cellpadding:'0', cellspacing:'0', children:[
								{tag:'tbody', children:[
									{tag:'tr', children:[
										{tag:'td', valign:'top', children:[
											{tag:'div', cls:'panelDescription', children:[
												{tag:'input', type:'checkbox', id:this.getId('disableFaviconForIECheckbox')}
											]}
										]},
										{tag:'td', valign:'top', children:[
											{tag:'div', cls:'panelDescription', children:Clipperz.PM.Strings['disableFaviconForIEDescriptionConfig']}
										]}
									]}
								]}
							]}
						]}
					]},
*/
			//		{tag:'tr', cls:'openPreferenceBlock', children:[
			//			{tag:'td', children:[
			//				{tag:'div', cls:'preferenceBlockTitle', htmlString:Clipperz.PM.Strings['accountPreferencesInterfaceTitle']},
			//				{tag:'div', cls:'panelDescription', children:Clipperz.PM.Strings['accountPreferencesInterfaceDescriptionConfig']}
			//			]}
			//		]}
				]},
				{tag:'div', cls:'clipperzSubPanelButtonBox', children:[
					{tag:'table', border:'0', cellspacing:'0', cellpadding:'0', children:[
						{tag:'tbody', children:[
							{tag:'tr', children:[
								{tag:'td', width:'100', align:'right', cls:'newRecordPanelButtonTD', children:[
									{tag:'div', id:this.getId('saveUserPreferences')}
								]},
								{tag:'td', width:'10', html:"&nbsp;"},
								{tag:'td', cls:'newRecordPanelButtonTD', children:[
									{tag:'div', id:this.getId('cancelUserPreferences')}
								]}
							]}
						]}
					]}
				]}
			]}		
		);

		this.selectSelectedLanguageOption();
		if (this.user().preferences().shouldShowDonationPanel()) {
			this.getDom('showDonationReminderCheckbox').checked = true;
		}
//		if (this.user().preferences().disableUnsecureFaviconLoadingForIE()) {
//			this.getDom('disableFaviconForIECheckbox').checked = true;
//		}
		
//MochiKit.Logging.logDebug("--- AccountPanel.render - 3");
//#		saveUserPreferencesButton = new YAHOO.ext.Button(this.getDom('saveUserPreferences'), {text:Clipperz.PM.Strings['saveUserPreferencesFormSubmitLabel'], handler:this.doSaveUserPreferences, scope:this});
		saveUserPreferencesButton = new YAHOO.ext.Button(this.getDom('saveUserPreferences'), {text:'-----------------', handler:this.doSaveUserPreferences, scope:this});
		saveUserPreferencesButton.setText(Clipperz.PM.Strings['saveUserPreferencesFormSubmitLabel']);
//#		cancelUserPreferencesButton = new YAHOO.ext.Button(this.getDom('cancelUserPreferences'), {text:Clipperz.PM.Strings['cancelUserPreferencesFormSubmitLabel'], handler:this.doCancelUserPreferences, scope:this});
		cancelUserPreferencesButton = new YAHOO.ext.Button(this.getDom('cancelUserPreferences'), {text:'-----------------', handler:this.doCancelUserPreferences, scope:this});
		cancelUserPreferencesButton.setText(Clipperz.PM.Strings['cancelUserPreferencesFormSubmitLabel']);
//MochiKit.Logging.logDebug("--- AccountPanel.render - 4");

		if (Clipperz.PM.Proxy.defaultProxy.isReadOnly()) {
			this.getElement('accountPreferencesForm').addClass('read-only');
			saveUserPreferencesButton.disable();
			cancelUserPreferencesButton.disable();
		}

	},
	
	//-------------------------------------------------------------------------

	'reloadHistory': function() {
		this.setShouldLoadLoginHistory(true);
		this.renderLoginHistory();
	},
	
	'shouldLoadLoginHistory': function() {
		return this._shouldLoadLoginHistory;
	},
	
	'setShouldLoadLoginHistory': function(aValue) {
		this._shouldLoadLoginHistory = aValue;
	},
	
	'tabSelectedHandler': function(anEvent) {
		if (anEvent.parameters() == 'accountPreferencesTab') {
			this.renderPreferences();
		}

		if ((this.shouldLoadLoginHistory()) && (anEvent.parameters() == 'loginHistoryTab')) {
			this.renderLoginHistory();
			this.setShouldLoadLoginHistory(false);
		}
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});
	
