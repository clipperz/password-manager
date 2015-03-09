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

Clipperz.Base.module('Clipperz.PM.UI.Web.Components');

Clipperz.PM.UI.Web.Components.AccountPanel = function(args) {
	args = args || {};

	this._credentialVefificationFunction = args['credentialVefificationFunction'];

	Clipperz.PM.UI.Web.Components.AccountPanel.superclass.constructor.apply(this, arguments);

//	this._initiallySelectedTab = args.selected || 'ACCOUNT';
	this._initiallySelectedTab = args.selected || 'PASSPHRASE';
	this._tabPanelControllerConfiguration = {
//		'ACCOUNT': {
//			tab:	'accountTab',
//			panel:	'accountPanel'
//		},
		'PASSPHRASE': {
			tab:	'passphraseTab',
			panel:	'passphrasePanel'
		},
		'OTP': {
			tab:	'OTPTab',
			panel:	'OTPPanel'
		},
		'PREFERENCES': {
			tab:	'preferencesTab',
			panel:	'preferencesPanel'
		},
		'LOGIN_HISTORY': {
			tab:	'loginHistoryTab',
			panel:	'loginHistoryPanel'
		},
		'DELETE_ACCOUNT': {
			tab:	'deleteAccountTab',
			panel:	'deleteAccountPanel'
		}
	};

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.AccountPanel, Clipperz.PM.UI.Common.Components.TabPanelComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.AccountPanel component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
//Clipperz.log("AccountPanel.renderSelf element", this.element());
		this.append(this.element(), [
			{tag:'div', cls:'header', children:[
				{tag:'div', cls:'subPanelTabs', children:[
					{tag:'ul', children:[
//						{tag:'li', id:this.getId('accountTab'),			children:[{tag:'a', href:'#', html:'Account'}], cls:'first'},
						{tag:'li', id:this.getId('passphraseTab'),		children:[{tag:'a', href:'#', html:'Passphrase'}], cls:'first'},
						{tag:'li', id:this.getId('OTPTab'),				children:[{tag:'a', href:'#', html:'One Time Passwords'}]},
						{tag:'li', id:this.getId('preferencesTab'),		children:[{tag:'a', href:'#', html:'Preferences'}]},
						{tag:'li', id:this.getId('loginHistoryTab'),	children:[{tag:'a', href:'#', html:'Login history'}]},
						{tag:'li', id:this.getId('deleteAccountTab'),	children:[{tag:'a', href:'#', html:'Delete account'}]}
					]}
				]}
			]},
			{tag:'div', cls:'body', children:[
				{tag:'div', cls:'accountPanel', children:[
					{tag:'div', cls:'subPanelContent', children:[
						{tag:'ul', children:[
//							{tag:'li', id:this.getId('accountPanel'),	children:[
//								{tag:'h3', html:"-- Account --"}
//							]},
							{tag:'li', id:this.getId('passphrasePanel'),	children:[
								{tag:'h3', cls:'changePassphrase', html:"Change passphrase"},
								{tag:'form', id:this.getId('changePassphrase'), cls:'changePassphrase', children:[
									{tag:'div', cls:'currentCredentials', children:[
										{tag:'div', cls:'field username', children:[
											{tag:'label', html:"username", 'for':this.getId('currentUsername')},
											{tag:'input', id:this.getId('currentUsername')}
										]},
										{tag:'div', cls:'field passphrase', children:[
											{tag:'label', html:"passphrase", 'for':this.getId('currentPassphrase')},
											{tag:'input', id:this.getId('currentPassphrase')}
										]}
									]},
									{tag:'div', cls:'newPassphrase', children:[
										{tag:'div', cls:'field', children:[
											{tag:'label', html:"new passphrase", 'for':this.getId('newPassphrase')},
											{tag:'input', id:this.getId('newPassphrase')}
										]},
										{tag:'div', cls:'field', children:[
											{tag:'label', html:"re-new passphrase", 'for':this.getId('reNewPassphrase')},
											{tag:'input', id:this.getId('reNewPassphrase')}
										]}
									]},
									{tag:'div', cls:'confirm', children:[
										{tag:'input', type:'checkbox', id:this.getId('confirm')},
										{tag:'label', html:"I understand that Clipperz will not be able to recover a lost passphrase", 'for':this.getId('confirm')}
									]}
								]},
								{tag:'div', cls:'clear'},
								{tag:'div', cls:'confirmButton', id:this.getId('confirmationButton'), children:[
									{tag:'span', html:"change passphrase"}
								]}
							]},
							{tag:'li', id:this.getId('OTPPanel'),		children:[
//								{tag:'h3', html:"Manage One-Time Passphrases"}
							]},
							{tag:'li', id:this.getId('preferencesPanel'),		children:[
//								{tag:'h3', html:"-- Preferences --"}
							]},
							{tag:'li', id:this.getId('loginHistoryPanel'),		children:[
//								{tag:'h3', html:"-- Login History --"}
							]},
							{tag:'li', id:this.getId('deleteAccountPanel'),		children:[
								{tag:'h3', cls:'deleteAccount', html:"Delete your account"},
								{tag:'form', id:this.getId('deleteAccountForm'), cls:'deleteAccount', children:[
									{tag:'div', cls:'currentCredentials', children:[
										{tag:'div', cls:'field username', children:[
											{tag:'label', html:"username", 'for':this.getId('deleteAccount_currentUsername')},
											{tag:'input', id:this.getId('deleteAccount_currentUsername')}
										]},
										{tag:'div', cls:'field passphrase', children:[
											{tag:'label', html:"passphrase", 'for':this.getId('deleteAccount_currentPassphrase')},
											{tag:'input', type:'password', id:this.getId('deleteAccount_currentPassphrase')}
										]},
										{tag:'div', cls:'confirm', children:[
											{tag:'input', type:'checkbox', id:this.getId('deleteAccount_checkbox')},
											{tag:'label', html:"I understand that all my data will be deleted and that this action is irreversible", 'for':this.getId('deleteAccount_checkbox')}
										]}
									]}
								]},
								{tag:'div', cls:'clear'},
								{tag:'div', cls:'confirmButton', id:this.getId('confirmationButton'), children:[
									{tag:'a', id:this.getId('deleteAccountButton'), href:'#', html:"Delete my account", cls:'deleteAccountButton disabled'}
								]}

							]}
						]}
					]}
				]}
			]},
			{tag:'div', cls:'footer'}
		]);
		
		this.tabPanelController().setup({selected:this.initiallySelectedTab()});
		MochiKit.Signal.connect(this.getId('deleteAccountButton'),	'onclick',	this,	'deleteAccount');

		MochiKit.Signal.connect(this.getId('deleteAccount_currentUsername'),	'onchange',	this,	'deleteAccountFormChangedValue');
		MochiKit.Signal.connect(this.getId('deleteAccount_currentUsername'),	'onkeyup',	this,	'deleteAccountFormChangedValue');
		MochiKit.Signal.connect(this.getId('deleteAccount_currentPassphrase'),	'onchange',	this,	'deleteAccountFormChangedValue');
		MochiKit.Signal.connect(this.getId('deleteAccount_currentPassphrase'),	'onkeyup',	this,	'deleteAccountFormChangedValue');
		MochiKit.Signal.connect(this.getId('deleteAccount_checkbox'),			'onchange',	this,	'deleteAccountFormChangedValue');
	},
	
	//-------------------------------------------------------------------------

	'deleteAccount': function (anEvent) {
console.log("DELETE ACCOUNT");

		anEvent.preventDefault();
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'deleteAccount', this.deleteAccountCredentials());
	},

	'deleteAccountCredentials': function () {
		return {
			username: this.getElement('deleteAccount_currentUsername').value,
			passphrase: this.getElement('deleteAccount_currentPassphrase').value
		};
	},

	'deleteAccountFormChangedValue': function (anEvent) {
		anEvent.preventDefault();

		if (this.verifyDeleteAccountCredentials()) {
			MochiKit.DOM.removeElementClass(this.getElement('deleteAccountButton'), 'disabled');
		} else {
			MochiKit.DOM.addElementClass(this.getElement('deleteAccountButton'), 'disabled')
		}
	},

	'verifyDeleteAccountCredentials': function () {
		var result;
		var credentials;
		var	checkboxChecked;

		result = false;
		checkboxChecked = this.getElement('deleteAccount_checkbox').checked;

		if (checkboxChecked) {
			result = this._credentialVefificationFunction(this.deleteAccountCredentials());
		}

		return result;
	},

	//-------------------------------------------------------------------------


	__syntaxFix__: "syntax fix"
});
