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

Clipperz.PM.UI.Web.Components.NewUserCreationComponent = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.NewUserCreationComponent.superclass.constructor.apply(this, arguments);

	this._tabPanelController = null;

	this._initiallySelectedTab = args.selected || 'CREDENTIALS';
	this._tabPanelControllerConfiguration = {
		'CREDENTIALS': {
			tab:	'credentialsTab',
			panel:	'credentialsTabpanel'
		},
		'CHECK_CREDENTIALS': {
			tab:	'checkCredentialsTab',
			panel:	'checkCredentialsTabpanel'
		},
		'TERMS_OF_SERVICE': {
			tab:	'termsOfServiceTab',
			panel:	'termsOfServiceTabpanel'
		},
		'CREATE_USER': {
			tab:	'createUserTab',
			panel:	'createUserTabpanel'
		}	//,
/*
		'LOGIN': {
			tab:	'loginTab',
			panel:	'loginTabpanel'
		}
*/
	};

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.NewUserCreationComponent, Clipperz.PM.UI.Common.Components.TabPanelComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.NewUserCreationComponent component";
	},

	//=========================================================================

	'disableAllPanels': function () {
		this.tabPanelController().selectTab(null);
	},

	//-------------------------------------------------------------------------

	'enableCredentialsPanel': function () {
		this.tabPanelController().selectTab('CREDENTIALS');
	},

	'enableCheckCredentialsPanel': function () {
		this.tabPanelController().selectTab('CHECK_CREDENTIALS');
	},

	'enableTermsOfServicePanel': function () {
		this.tabPanelController().selectTab('TERMS_OF_SERVICE');
	},

	'enableCreateUserPanel': function () {
		this.tabPanelController().selectTab('CREATE_USER');
	},

//	'enableLoginPanel': function () {
//		this.tabPanelController().selectTab('LOGIN');
//	},

	//=========================================================================

	'shouldShowElementWhileRendering': function() {
		return false;
	},
	
	//=========================================================================

	'tabPanelController': function () {
		if (this._tabPanelController == null) {
			this._tabPanelController = new Clipperz.PM.UI.Common.Controllers.TabPanelController({
				component:this,
				configuration:this._tabPanelControllerConfiguration
			});
			
			MochiKit.Signal.connect(this._tabPanelController, 'tabSelected', this, 'handleTabSelected')
		}
		
		return this._tabPanelController;
	},

	//-------------------------------------------------------------------------

	'renderSelf': function() {
		this.append(this.element(), {tag:'div', cls:'NewUserCreation mainDialog', id:this.getId('panel'), children: [
			{tag:'form', id:this.getId('form'), cls:'newUserCreationForm', children:[
				{tag:'div', cls:'header', children:[
					{tag:'div', cls:'title', children:[
						{tag:'h3', id:this.getId('title'), html:"Create new user"}
					]}
				]},
				{tag:'div', id:this.getId('body'), cls:'body', children:[
					{tag:'div', cls:'tabContainer', children:[
						{tag:'ul', cls:'tabs', children:[
							{tag:'li', id:this.getId('credentialsTab'),			children:[{tag:'span', html:"credentials"}]},
							{tag:'li', id:this.getId('checkCredentialsTab'),	children:[{tag:'span', html:"credentials check"}]},
							{tag:'li', id:this.getId('termsOfServiceTab'),		children:[{tag:'span', html:"terms of service"}]},
							{tag:'li', id:this.getId('createUserTab'),			children:[{tag:'span', html:"create user"}]}	//,
//							{tag:'li', id:this.getId('loginTab'),				children:[{tag:'span', html:"login"}]},
						]},
						{tag:'ul', cls:'tabPanels', children:[
							{tag:'li', id:this.getId('credentialsTabpanel'), cls:'tabPanel credentials', children:[
								{tag:'div', cls:'wizardStepDescription', children:[{tag:'span', html:Clipperz.PM.Strings.getValue('Wizards.NewUserWizard.CREDENTIALS.description')}]},
								{tag:'ul', cls:'credentials', children:[
									{tag:'li', children:[{tag:'span', cls:'label', html:"username"},	{tag:'input', type:'text', id:this.getId('username')/*, value:'test'*/}]},
									{tag:'li', children:[{tag:'span', cls:'label', html:"passphrase"},	{tag:'input', type:'password', id:this.getId('passphrase')/*, value:'test'*/}]}
								]}
							]},
							{tag:'li', id:this.getId('checkCredentialsTabpanel'), cls:'tabPanel checkCredentials', children:[
								{tag:'div', cls:'wizardStepDescription', children:[{tag:'span', html:Clipperz.PM.Strings.getValue('Wizards.NewUserWizard.CHECK_CREDENTIALS.description')}]},
								{tag:'ul', cls:'credentials', children:[
									{tag:'li', children:[{tag:'span', cls:'label', html:"re-passphrase"},	{tag:'input', type:'password', id:this.getId('re-passphrase')/*, value:'test'*/}]}
								]}
							]},
							{tag:'li', id:this.getId('termsOfServiceTabpanel'), cls:'tabPanel termsOfService', children:[
								{tag:'div', cls:'wizardStepDescription', children:[{tag:'span', html:Clipperz.PM.Strings.getValue('Wizards.NewUserWizard.TERMS_OF_SERVICE.description')}]},
								{tag:'ul', cls:'termsOfService', children:[
									{tag:'li', children:[{tag:'input', type:'checkbox', id:this.getId('awareOfUnrecoverablePassphrase')/*, checked:true*/}, {tag:'label', cls:'label', 'for':this.getId('awareOfUnrecoverablePassphrase'), html:"I understand that Clipperz will not be able to recover a lost passphrase."}]},
									{tag:'li', children:[{tag:'input', type:'checkbox', id:this.getId('readTermsOfService')/*, checked:true*/}, {tag:'label', cls:'label', 'for':this.getId('readTermsOfService'), htmlString:"I have read and agreed to the <a href='https://www.clipperz.com/terms_service' target='_blank'>Terms of Service</a>."}]}
								]}
							]},
							{tag:'li', id:this.getId('createUserTabpanel'), cls:'tabPanel createUser', children:[
								{tag:'div', cls:'wizardStepDescription', children:[{tag:'span', html:Clipperz.PM.Strings.getValue('Wizards.NewUserWizard.CREATE_USER.description')}]},
								{tag:'ul', cls:'createUserStates', children:[
									{tag:'li', cls:'creating', id:this.getId('creatingRegistering'), children:[{tag:'span', html:"registering user"}]},
									{tag:'li', cls:'done', id:this.getId('creatingDone'), children:[{tag:'span', html:"done"}]},
									{tag:'li', cls:'fail', id:this.getId('creatingFailed'), children:[{tag:'span', html:"fail"}]}
								]}
							]}	//,
//							{tag:'li', id:this.getId('loginTabpanel'), cls:'tabPanel login', children:[
//								{tag:'div', cls:'wizardStepDescription', children:[{tag:'span', html:Clipperz.PM.Strings.getValue('Wizards.NewUserWizard.LOGIN.description')}]},
//							]}
						]}
					]}
				]},
				{tag:'div', id:this.getId('footer'), cls:'footer', children:[
					{tag:'div', cls:'buttonArea', children:[
//						{tag:'div', cls:'cancel', id:this.getId('cancelButton'), html:"cancel"},
//						{tag:'div', cls:'save disabled',   id:this.getId('saveButton'),   html:"save"}
					]}
				]}
			]},
			{tag:'div', cls:'clear'}
		]});

		this.tabPanelController().setup();
//		MochiKit.Signal.connect(this.getId('panel'), 'onkeydown', this, 'handleKeyEvent');
		MochiKit.Signal.connect(MochiKit.DOM.currentDocument().body, 'onkeydown', this, 'handleKeyEvent');
		MochiKit.Signal.connect(this.getId('awareOfUnrecoverablePassphrase'), 'onchange', this, 'handleTermsOfServiceCheckboxChange');
		MochiKit.Signal.connect(this.getId('readTermsOfService'), 'onchange', this, 'handleTermsOfServiceCheckboxChange');
	},

	//-------------------------------------------------------------------------
/*
	'resetContent': function () {
		this.getElement('username').value = '';
		this.getElement('passphrase').value = '';
	},
*/
	//-------------------------------------------------------------------------

	'displayElement': function() {
		return this.getElement('panel');
	},

	//-------------------------------------------------------------------------

	'handleTabSelected': function (aSelectedTab) {
/*
		switch (aSelectedTab) {
			case 'DETAILS':
				break;
			case 'DIRECT_LOGINS':
				MochiKit.Style.hideElement(this.getElement('backToDirectLoginList'));
				break;
			case 'SHARING':
				break;
		}
*/
	},

	//=========================================================================

	'username': function () {
		return this.getElement('username').value;
	},
	
	'usernameElement': function () {
		return this.getElement('username');
	},

	'passphrase': function () {
		return this.getElement('passphrase').value;
	},

	'rePassphrase': function () {
		return this.getElement('re-passphrase').value;
	},

	'awareOfUnrecoverablePassphrase': function () {
		return this.getElement('awareOfUnrecoverablePassphrase').value;
	},
	
	'readTermsOfService': function () {
		return this.getElement('readTermsOfService').value;
	},

	//=========================================================================
/*
	'incrementUpdateFaviconCounter': function () {
		this._updateFaviconCounter ++;
	},

	'decrementUpdateFaviconCounter': function () {
		this._updateFaviconCounter --;
	},

	'updateFaviconCounter': function () {
		return this._updateFaviconCounter;
	},
*/	
	//-------------------------------------------------------------------------
/*
	'updateFavicon': function () {
		this.decrementUpdateFaviconCounter();
		
		if (this.updateFaviconCounter() == 0) {
			this.setFavicon(this.favicon());
		}
	},
*/
	//=========================================================================
/*
	'bindingComponents': function () {
		return this._bindingComponents;
	},

	'clearAllBindingsComponents': function () {
		MochiKit.Iter.forEach(this.bindingComponents(), MochiKit.Base.methodcaller('remove'));
		this._bindingComponents = [];
		this.getElement('bindings').innerHTML = '';
	},

	'addBindingComponent': function (aBindingComponent) {
		this.bindingComponents().push(aBindingComponent);
		aBindingComponent.renderInNode(this.append(this.getElement('bindings'), {tag:'div'}));
	},
*/
	//=========================================================================
/*
	'formValueComponents': function () {
		return this._formValueComponents;
	},
	
	'clearAllFormValueComponents': function () {
		MochiKit.Iter.forEach(this.formValueComponents(), MochiKit.Base.methodcaller('remove'));
		this._formValueComponents = [];
		this.getElement('formValues').innerHTML = '';
	},

	'addFormValueComponent': function (aFormValueComponent) {
		this.formValueComponents().push(aFormValueComponent);
		aFormValueComponent.renderInNode(this.append(this.getElement('formValues'), {tag:'div'}));
	},
*/
	//=========================================================================

	'changedValue': function (anEvent) {
		MochiKit.Signal.signal(this, 'changedValue', anEvent);

		this.incrementUpdateFaviconCounter();
		MochiKit.Async.callLater(1, MochiKit.Base.method(this, 'updateFavicon'));
	},

	//-------------------------------------------------------------------------

	'handleBackClick': function (anEvent) {
		anEvent.preventDefault();
		
		MochiKit.Signal.signal(this, 'back');
	},

	//=========================================================================

	'bottomMargin': function () {
		var result;

//	TODO: WTF!!!
//		result =	MochiKit.Style.getElementPosition(this.element())['y'] +
//					MochiKit.Style.getElementDimensions(this.element())['h'];

//		result =	MochiKit.Style.getElementPosition(this.getElement('footer'))['y'];
//		result = 450;

		result =	Clipperz.PM.UI.Common.Components.BaseComponent.targetModalDimensionsAndPosition['position']['y'] +
					Clipperz.PM.UI.Common.Components.BaseComponent.targetModalDimensionsAndPosition['dimensions']['h'] -
					60;

		return result;
	},

	//=========================================================================
	
	'focusOnUsernameElement': function () {
		MochiKit.Async.callLater(0.1, MochiKit.Base.method(this.getElement('username'), 'focus'));
	},

	'focusOnRePassphraseElement': function () {
		this.getElement('re-passphrase').focus();
	},
/*	
	'focusOnBookmarkletConfigurationElement': function () {
		this.getElement('bookmarkletConfiguration').focus();
	},

	'focusOnFaviconElement': function () {
		this.getElement('faviconURL').focus();
	},
*/

	//=========================================================================

	'hideAllProgeressStates': function () {
		MochiKit.Style.hideElement(this.getElement('creatingRegistering'));
		MochiKit.Style.hideElement(this.getElement('creatingDone'));
		MochiKit.Style.hideElement(this.getElement('creatingFailed'));
	},

	'showProgressOnUserCreation': function () {
//Clipperz.log(">>> NewUserCreationComponent.showProgressOnUserCreation");
		this.hideAllProgeressStates();
		MochiKit.Style.showElement(this.getElement('creatingRegistering'));
	},

	'showUserCreationDone': function () {
//Clipperz.log(">>> NewUserCreationComponent.showUserCreationDone");
		this.hideAllProgeressStates();
		MochiKit.Style.showElement(this.getElement('creatingDone'));
//Clipperz.log("<<< NewUserCreationComponent.showUserCreationDone");
	},

	'showUserCreationFailed': function () {
//Clipperz.log(">>> NewUserCreationComponent.showUserCreationFailed");
		this.hideAllProgeressStates();
		MochiKit.Style.showElement(this.getElement('creatingFailed'));
	},

	//=========================================================================

	'handleKeyEvent': function (anEvent) {
		MochiKit.Signal.signal(this, 'keyPressed', anEvent);
/*
		if (anEvent.key().string == 'KEY_ENTER') {
			if (anEvent.target().nodeName != 'TEXTAREA') {
				MochiKit.Signal.signal(this, 'moveForward');
				anEvent.preventDefault();
			}
		} else if (anEvent.key().string == 'KEY_TAB') {
			if ((anEvent.target().nodeName == 'INPUT') || (anEvent.target().nodeName == 'TEXTAREA')) {
				MochiKit.Signal.signal(this, 'moveForward');
				anEvent.preventDefault();
			}
		}
*/
	},

	'handleTermsOfServiceCheckboxChange': function (anEvent) {
		MochiKit.Signal.signal(this, 'changedValue');
	},

	//=========================================================================

	'clear': function () {
		this.tabPanelController().selectTab(null);
		Clipperz.PM.UI.Web.Components.NewUserCreationComponent.superclass.clear.apply(this, arguments);
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});
