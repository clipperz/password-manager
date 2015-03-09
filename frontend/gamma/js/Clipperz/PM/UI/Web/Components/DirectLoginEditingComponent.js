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

Clipperz.PM.UI.Web.Components.DirectLoginEditingComponent = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.DirectLoginEditingComponent.superclass.constructor.apply(this, arguments);

	this._tabPanelController = null;

	this._initiallySelectedTab = args.selected || 'TYPE';
	this._tabPanelControllerConfiguration = {
		'LABEL': {
			tab:	'labelTab',
			panel:	'labelTabpanel'
		},
		'TYPE': {
			tab:	'typeTab',
			panel:	'typeTabpanel'
		},
		'CONFIGURATION': {
			tab:	'configurationTab',
			panel:	'configurationTabpanel'
		},
		'BINDINGS': {
			tab:	'bindingsTab',
			panel:	'bindingsTabpanel'
		},
		'FAVICON': {
			tab:	'faviconTab',
			panel:	'faviconTabpanel'
		},
		'DONE': {
			tab:	'doneTab',
			panel:	'doneTabpanel'
		}
	};

	this._directLoginReference = null;

	this._directLoginFavicon = null;

	this._updateFaviconCounter = 0;
	this._faviconComponent = null;

	this._bindingComponents	= [];
	this._formValueComponents = [];

	return this;
}

//=============================================================================

//Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.DirectLoginEditingComponent, Clipperz.PM.UI.Common.Components.BaseComponent, {
Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.DirectLoginEditingComponent, Clipperz.PM.UI.Common.Components.TabPanelComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.DirectLoginEditingComponent component";
	},

	//=========================================================================

	'directLoginReference': function () {
		return this._directLoginReference;
	},
	
	//-------------------------------------------------------------------------

	'setDirectLoginReference': function (aDirectLoginReference) {
		this._directLoginReference = aDirectLoginReference;

		return this._directLoginReference;
	},

	//=========================================================================

	'label': function () {
		return this.getElement('label').value
	},
	
	'setLabel': function (aValue) {
		this.getElement('label').value = (aValue ? aValue : '');
	},

	//-------------------------------------------------------------------------

	'favicon': function () {
		return this.getElement('faviconURL').value;
	},
	
	'setFavicon': function (aValue) {
		var regexp;
		var displayValue;

		regexp = new RegExp('^data\:\/\/.*', 'i');
		if (regexp.test(aValue)) {
			displayValue = ''
		} else {
			displayValue = (aValue ? aValue : '');
		}

		this.getElement('faviconURL').value = displayValue;
		this.faviconComponent().setSrc(aValue);
	},

//	'setFaviconData': function (aValue) {
//		this.getElement('faviconIcon').src = aValue;
//	},

	'directLoginFavicon': function () {
		return this._directLoginFavicon;
	},
	
	'setDirectLoginFavicon': function (aValue) {
		this._directLoginFavicon = aValue;
		this.setFavicon(aValue);
	},

	//-------------------------------------------------------------------------

	'bookmarkletConfiguration': function () {
		return this.getElement('bookmarkletConfiguration').value
	},
	
	'setBookmarkletConfiguration': function (aValue) {
		this.getElement('bookmarkletConfiguration').value = aValue;
	},

	'highlightConfigurationSyntaxError': function () {
		MochiKit.DOM.addElementClass(this.getElement('bookmarkletConfiguration'), 'error');
	},

	'removeHighlightConfigurationSyntaxError': function () {
		MochiKit.DOM.removeElementClass(this.getElement('bookmarkletConfiguration'), 'error');
	},

	//=========================================================================

	'disableAllPanels': function () {
		this.getElement('label').disabled = true;
		MochiKit.DOM.addElementClass(this.getElement('label').parentNode, 'disabled');

		this.tabPanelController().selectTab(null);
	},

	//-------------------------------------------------------------------------

//	'disableLabelField': function () {
//		this.getElement('label').disabled = true;
//		MochiKit.DOM.addElementClass(this.getElement('label').parentNode, 'disabled');
//	},
	
	'enableLabelField': function () {
		this.getElement('label').disabled = false;
		MochiKit.DOM.removeElementClass(this.getElement('label').parentNode, 'disabled');
		this.tabPanelController().selectTab('LABEL');
	},

	//-------------------------------------------------------------------------

//	'disableTypeField': function () {
//		this.tabPanelController().selectTab(null);
//	},
	
	'enableTypeField': function () {
		this.tabPanelController().selectTab('TYPE');
	},
	
	//-------------------------------------------------------------------------

//	'disableConfigurationField': function () {
//		this.tabPanelController().selectTab(null);
//	},
	
	'enableConfigurationField': function () {
		this.tabPanelController().selectTab('CONFIGURATION');
	},
	
	//-------------------------------------------------------------------------

//	'disableBindingFields': function () {
//		this.tabPanelController().selectTab(null);
//	},
	
	'enableBindingFields': function () {
		this.tabPanelController().selectTab('BINDINGS');
	},
	
	//-------------------------------------------------------------------------

//	'disableFaviconField': function () {
//		this.tabPanelController().selectTab(null);
//	},

	'enableFaviconField': function () {
		this.tabPanelController().selectTab('FAVICON');
	},

	//-------------------------------------------------------------------------

	'enableDonePanel': function () {
		this.tabPanelController().selectTab('DONE');
	},
	
	//=========================================================================

	'shouldShowElementWhileRendering': function() {
		return false;
	},
	
	//=========================================================================
	
	'faviconComponent': function () {
		if (this._faviconComponent == null) {
			this._faviconComponent = new Clipperz.PM.UI.Common.Components.FaviconComponent({element:this.getId('favicon')});
		}
		
		return this._faviconComponent;
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
		var	bookmarkletComponent;
		
		this.append(this.element(), {tag:'div', cls:'directLoginEditing', id:this.getId('panel'), children: [
//			{tag:'div', cls:'back', children:[
//				{tag:'a', href:'#', id:this.getId('back'), html:"&nbsp;"}
//			]},
			{tag:'form', id:this.getId('form'), cls:'directLoginEditingForm', children:[
				{tag:'div', cls:'title', children:[
					{tag:'img', id:this.getId('favicon'), cls:'favicon'},
					{tag:'input', type:'text', id:this.getId('label')}	//,
//					{tag:'div', cls:'wizardStepDescription', children:[{tag:'span', html:Clipperz.PM.Strings.getValue('Wizards.DirectLoginWizard.LABEL.description')}]},
				]},
				{tag:'div', cls:'tabContainer', children:[
					{tag:'ul', cls:'tabs', children:[
						{tag:'li', id:this.getId('labelTab'),			children:[{tag:'span', html:"label"}]},
						{tag:'li', id:this.getId('typeTab'),			children:[{tag:'span', html:"type"}]},
						{tag:'li', id:this.getId('configurationTab'),	children:[{tag:'span', html:"configuration"}]},
						{tag:'li', id:this.getId('bindingsTab'),		children:[{tag:'span', html:"bindings"}]},
						{tag:'li', id:this.getId('faviconTab'),			children:[{tag:'span', html:"favicon"}]},
						{tag:'li', id:this.getId('doneTab'),			children:[{tag:'span', html:"done"}]}
					]},
					{tag:'ul', cls:'tabPanels', children:[
						{tag:'li', id:this.getId('labelTabpanel'), cls:'tabPanel label', children:[
							{tag:'div', cls:'wizardStepDescription', children:[{tag:'span', html:Clipperz.PM.Strings.getValue('Wizards.DirectLoginWizard.LABEL.description')}]}
						]},
						{tag:'li', id:this.getId('typeTabpanel'), cls:'tabPanel type', children:[
							{tag:'div', cls:'wizardStepDescription', children:[{tag:'span', html:Clipperz.PM.Strings.getValue('Wizards.DirectLoginWizard.TYPE.description')}]},
							{tag:'h2', html:"type"}
						]},
						{tag:'li', id:this.getId('configurationTabpanel'), cls:'tabPanel configuration', children:[
							{tag:'div', cls:'wizardStepDescription', children:[{tag:'span', html:Clipperz.PM.Strings.getValue('Wizards.DirectLoginWizard.CONFIGURATION.description')}]},
							{tag:'div', cls:'bookmarkletConfigurationWrapper', children:[
								{tag:'textarea', id:this.getId('bookmarkletConfiguration'), value:""},
//								{tag:'div', cls:'bookmarkletComponentWrapper', children:[{tag:'div', id:this.getId('bookmarkletComponent'), cls:'bookmarkletComponent'}]}
								{tag:'div', id:this.getId('bookmarkletComponent'), cls:'bookmarkletComponent'}
							]}
						]},
						{tag:'li', id:this.getId('bindingsTabpanel'), cls:'tabPanel bindings', children:[
							{tag:'div', cls:'wizardStepDescription', children:[{tag:'span', html:Clipperz.PM.Strings.getValue('Wizards.DirectLoginWizard.BINDINGS.description')}]},
							{tag:'div', cls:'bindings', id:this.getId('bindings'), children:[]},
							{tag:'div', cls:'formValues', id:this.getId('formValues'), children:[]}
						]},
						{tag:'li', id:this.getId('faviconTabpanel'), cls:'tabPanel favicon', children:[
							{tag:'div', cls:'wizardStepDescription', children:[{tag:'span', html:Clipperz.PM.Strings.getValue('Wizards.DirectLoginWizard.FAVICON.description')}]},
							{tag:'div', cls:'favicon', children:[
								{tag:'input', type:'text', id:this.getId('faviconURL')}
							]}
						]},
						{tag:'li', id:this.getId('doneTabpanel'), cls:'tabPanel done', children:[
							{tag:'div', cls:'wizardStepDescription', children:[{tag:'span', id:this.getId('doneDescription')/*, html:Clipperz.PM.Strings.getValue('Wizards.DirectLoginWizard.DONE.description')*/}]}
						]}
					]}
				]}
			]},
			{tag:'div', cls:'clear'}
		]});

		bookmarkletComponent = new Clipperz.PM.UI.Web.Components.BookmarkletComponent({element:this.getElement('bookmarkletComponent')});
		bookmarkletComponent.render();

		this.tabPanelController().setup(/*{selected:this.initiallySelectedTab()}*/);

		MochiKit.Signal.connect(this.getId('label'),					'onchange', this, 'changedValue');
		MochiKit.Signal.connect(this.getId('label'),					'onkeyup',  this, 'changedValue');

		MochiKit.Signal.connect(this.getId('bookmarkletConfiguration'),	'onchange', this, 'changedValue');
		MochiKit.Signal.connect(this.getId('bookmarkletConfiguration'),	'onkeyup',  this, 'changedValue');

		MochiKit.Signal.connect(this.getId('faviconURL'),				'onchange', this, 'changedValue');
		MochiKit.Signal.connect(this.getId('faviconURL'),				'onkeyup',  this, 'changedValue');

		MochiKit.Signal.connect(this.getId('panel'),					'onkeydown',this, 'handleKeyEvent');
	},

	//-------------------------------------------------------------------------

	'handleTabSelected': function (aSelectedTab) {
		switch (aSelectedTab) {
			case 'DETAILS':
				break;
			case 'DIRECT_LOGINS':
				MochiKit.Style.hideElement(this.getElement('backToDirectLoginList'));
				break;
			case 'SHARING':
				break;
		}
	},

	//=========================================================================

	'incrementUpdateFaviconCounter': function () {
		this._updateFaviconCounter ++;
	},

	'decrementUpdateFaviconCounter': function () {
		this._updateFaviconCounter --;
	},

	'updateFaviconCounter': function () {
		return this._updateFaviconCounter;
	},
	
	//-------------------------------------------------------------------------

	'updateFavicon': function () {
		this.decrementUpdateFaviconCounter();
		
		if (this.updateFaviconCounter() == 0) {
			this.setFavicon(this.favicon());
		}
	},

	//=========================================================================

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

	//=========================================================================

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
		return	MochiKit.Style.getElementPosition(this.element().parentNode)['y'] +
				MochiKit.Style.getElementDimensions(this.element())['h'];
	},

	//=========================================================================

	'focusOnLabelElement': function () {
		this.getElement('label').focus();
	},
	
	'focusOnBookmarkletConfigurationElement': function () {
		this.getElement('bookmarkletConfiguration').focus();
	},

	'focusOnFaviconElement': function () {
		this.getElement('faviconURL').focus();
	},

	//=========================================================================

	'setDoneDescriptionWithKeys': function (someKeys) {
//		{tag:'div', cls:'wizardStepDescription', children:[{tag:'span', id:this.getId('doneDescription')/*, html:Clipperz.PM.Strings.getValue('Wizards.DirectLoginWizard.DONE.description')*/}]}
		this.getElement('doneDescription').innerHTML = Clipperz.PM.Strings.getValue('Wizards.DirectLoginWizard.DONE.description', someKeys);
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

	//=========================================================================
	__syntaxFix__: "syntax fix"
});
