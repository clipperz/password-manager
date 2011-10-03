/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz's Javascript Crypto Library.
Javascript Crypto Library provides web developers with an extensive
and efficient set of cryptographic functions. The library aims to
obtain maximum execution speed while preserving modularity and
reusability.
For further information about its features and functionalities please
refer to http://www.clipperz.com

* Javascript Crypto Library is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Javascript Crypto Library is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Javascript Crypto Library.  If not, see
  <http://www.gnu.org/licenses/>.

*/

Clipperz.Base.module('Clipperz.PM.UI.Web.Components');

Clipperz.PM.UI.Web.Components.AccountPanel = function(args) {
	args = args || {};

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
		'PREFERENCES': {
			tab:	'preferencesTab',
			panel:	'preferencesPanel'
		},
		'LOGIN_HISTORY': {
			tab:	'loginHistoryTab',
			panel:	'loginHistoryPanel'
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
						{tag:'li', id:this.getId('preferencesTab'),		children:[{tag:'a', href:'#', html:'Preferences'}]},
						{tag:'li', id:this.getId('loginHistoryTab'),	children:[{tag:'a', href:'#', html:'Login history'}]}
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
								]},

								{tag:'h3', cls:'manageOTP', html:"Manage One-Time Passphrases"},
								{}
							]},
							{tag:'li', id:this.getId('preferencesPanel'),		children:[
								{tag:'h3', html:"-- Preferences --"}
							]},
							{tag:'li', id:this.getId('loginHistoryPanel'),		children:[
								{tag:'h3', html:"-- Login History --"}
							]}
						]}
					]}
				]}
			]},
			{tag:'div', cls:'footer'}
		]);
		
		this.tabPanelController().setup({selected:this.initiallySelectedTab()});
	},
	
	//-------------------------------------------------------------------------


	__syntaxFix__: "syntax fix"
});
