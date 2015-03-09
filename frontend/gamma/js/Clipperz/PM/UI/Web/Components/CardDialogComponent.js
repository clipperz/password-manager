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

Clipperz.PM.UI.Web.Components.CardDialogComponent = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.CardDialogComponent.superclass.constructor.apply(this, arguments);

	this._tabPanelController = null;

	this._tabPanelControllerConfiguration = {
		'DETAILS': {
			tab:	'detailTab',
			panel:	'detailTabpanel'
		},
		'DIRECT_LOGINS': {
			tab:	'directLoginTab',
			panel:	'directLoginTabpanel'
		},
		'SHARING': {
			tab:	'sharingTab',
			panel:	'sharingTabpanel'
		}
	};
	
	this._tooltips = null;

	this._isSavingEnabled = false;
	this._hintMode = 'OFF';	//	'ON'

	this._fieldComponents = {};
	this._directLoginComponents = {};

	this._displayMode = 'fixed'; 	//	'scrollable';
	
	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.CardDialogComponent, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.CardDialogComponent component";
	},

	//-------------------------------------------------------------------------

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
		this.append(this.element(), {tag:'div', cls:'CardDialog mainDialog loading', id:this.getId('panel'), children: [
			{tag:'form', id:this.getId('form'), children:[
//				{tag:'input', type:'text', id:this.getId('hidden'), cls:'hidden'},
				{tag:'div', cls:'header', children:[
					{tag:'div', cls:'title', children:[
						{tag:'input', type:'text', id:this.getId('title')}
					]}
				]},
				{tag:'div', id:this.getId('body'), cls:'body', children:[
					{tag:'div', cls:'tabs', children:[
						{tag:'ul', cls:'tabs', children:[
							{tag:'li', id:this.getId('detailTab'), children:[{tag:'span', html:"details"}]},
							{tag:'li', id:this.getId('directLoginTab'), children:[
								{tag:'span', html:"direct logins"}//,
//								{tag:'div', id:this.getId('addDirectLoginButton'), cls:'addDirectLoginButton', children:[
//									{tag:'span', html:"+"}
//								]}
							]},
							{tag:'li', id:this.getId('sharingTab'), children:[{tag:'span', html:"sharing"}]}
						]}
					]},
					{tag:'div', cls:'tabPanels', children:[
						{tag:'ul', cls:'tabPanels', children:[
							{tag:'li', id:this.getId('detailTabpanel'), cls:'tabPanel', children:[
								{tag:'div', id:this.getId('recordFields'), children:[
									{tag:'table', cls:'fields', cellpadding:'0', id:this.getId('fieldTable'), cellspacing:'0', children:[
										{tag:'thead', children:[
											{tag:'tr', children:[
												{tag:'th', cls:'fieldStateTH',	html:""},
												{tag:'th', cls:'fieldLabelTH',	html:"label"},
												{tag:'th', cls:'fieldLockTH',	html:""},
												{tag:'th', cls:'fieldValueTH',	html:"value"},
												{tag:'th', cls:'fieldActionTH',	html:""},
												{tag:'th', cls:'fieldDeleteTH',	html:""}
											]}
										]},
										{tag:'tfoot'},
										{tag:'tbody', id:this.getId('tableBody'), children:[
											{tag:'tr', id:this.getId('newFieldTR'), cls:'newFieldTR', children:[
												{tag:'td', cls:'fieldState'},
												{tag:'td', cls:'fieldLabel', children:[
													{tag:'input', cls:'label', id:this.getId('newFieldLabel')}
												]},
												{tag:'td', cls:'fieldLock', children:[
													{tag:'div', cls:'unlocked', id:this.getId('newFieldIsLocked')}
												]},
												{tag:'td', cls:'fieldValue', children:[
													{tag:'div', cls:'unlocked', id:this.getId('newFieldValueWrapper'), children:[
														{tag:'input',	type:'text', cls:'value', id:this.getId('newFieldValue')}
													]}
												]},
												{tag:'td', cls:'fieldAction', children:[
													{tag:'div', html:'&nbsp;'}
												]},
												{tag:'td', cls:'fieldAddDelete', children:[
													{tag:'div', cls:'new', children:[
														{tag:'span', children:[
															{tag:'a', href:'#', id:this.getId('newFieldAddButton'), html:"add"}
														]}
													]}
												]}
											]}
										]}
									]}
								]},
								{tag:'div', cls:'notes', children:[
									{tag:'div', children:[
										{tag:'textarea', id:this.getId('recordNote'), value:""}
									]}
								]}
							]},
							{tag:'li', id:this.getId('directLoginTabpanel'), cls:'tabPanel', children:[
								{tag:'div', id:this.getId('directLoginsComponentContainer'), cls:'directLoginsComponentContainer', children:[
									{tag:'div', id:this.getId('directLogins'), cls:'directLogins', children:[
										{tag:'div', id:this.getId('addNewDirectLoginSplash'), cls:'addNewDirectLoginSplash', children:[
											{tag:'h3', html:"Here you can add a Direct Login for this card: instant access to your favorit website!"},
											{tag:'a', href:'#', id:this.getId('addNewDirectLoginSplashButton'), children:[{tag:'span', html:"Add Direct Login"}]}
										]},
										{tag:'div', id:this.getId('directLoginsList')},
										{tag:'div', cls:'addDirectLoginListItem', id:this.getId('addDirectLoginListItem'), children:[{tag:'a', href:'#', id:this.getId('addNewDirectLoginListItemButton'), children:[{tag:'span', html:"Add Direct Login"}]}]}
									]},
									{tag:'div', id:this.getId('directLoginEditDetail'), cls:'directLoginEditDetail'}
								]}
							]},
							{tag:'li', id:this.getId('sharingTabpanel'), cls:'tabPanel', children:[
								{tag:'h2', html:"Coming soon!"}
							]}
						]}
					]},
					{tag:'div', cls:'mask', children:[
						{tag:'div', id:this.getId('progress'), children:[
							{tag:'h3', id:this.getId('progressDescription'), cls:'progressDescription', html:"Loading"},
							{tag:'div', id:this.getId('progressBar')}
						]},
						{tag:'div', id:this.getId('error'), cls:'error', children:[
							{tag:'div', cls:'img'},
							{tag:'p', id:this.getId('errorMessage')}
						]}
					]}
				]},
				{tag:'div', cls:'footer', children:[
					{tag:'div', cls:'buttonArea', children:[
						{tag:'div', cls:'cancel', id:this.getId('cancelButton'), html:"cancel"},
						{tag:'div', cls:'save disabled',   id:this.getId('saveButton'),   html:"save"}
					]}
				]}
			]}
		]});


		this.insertAllTooltips();
		
		this.addComponent(new Clipperz.PM.UI.Common.Components.ProgressBar({'element':this.getElement('progressBar')}));

		this.tabPanelController().setup({selected:'DETAILS'});
//		this.tabPanelController().setup({selected:'DIRECT_LOGINS'});

		MochiKit.Style.hideElement(this.getId('error'));
		MochiKit.Style.hideElement(this.getElement('directLoginEditDetail'));
		MochiKit.Style.hideElement(this.getElement('addDirectLoginListItem'));
		this.plumbDetailsPanel();

		MochiKit.Signal.connect(this.getId('cancelButton'),						'onclick',		this, 'handleCancelEvent');
		MochiKit.Signal.connect(this.getId('saveButton'),						'onclick',		this, 'handleSaveEvent');

		MochiKit.Signal.connect(this.getId('addNewDirectLoginSplashButton'),	'onclick',		this, 'handleAddDirectLogin');
		MochiKit.Signal.connect(this.getId('addNewDirectLoginListItemButton'),	'onclick',		this, 'handleAddDirectLogin');
		
		MochiKit.Signal.connect(MochiKit.DOM.currentDocument().body,			'onkeydown',	this, 'handleKeyEvent');
	},

	//-------------------------------------------------------------------------

	'displayMode': function () {
		return this._displayMode;
	},
	
	'setDisplayMode': function (aValue) {
		this._displayMode = aValue;
	},

	//-------------------------------------------------------------------------

	'plumbDetailsPanel': function () {
		MochiKit.Signal.connect(this.getId('title'),	'onfocus',	MochiKit.Base.method(this, 'handleOnFocusEvent',	this.getElement('title').parentNode));
		MochiKit.Signal.connect(this.getId('title'),	'onblur',	MochiKit.Base.method(this, 'handleLooseFocusEvent', this.getElement('title').parentNode));
		MochiKit.Signal.connect(this.getId('title'),	'onchange',	MochiKit.Base.partial(MochiKit.Signal.signal, this, 'changedValue'));
		MochiKit.Signal.connect(this.getId('title'),	'onkeyup',	MochiKit.Base.partial(MochiKit.Signal.signal, this, 'changedValue'));

		MochiKit.Signal.connect(this.getId('recordNote'),	'onfocus',	MochiKit.Base.method(this, 'handleOnFocusEvent',	MochiKit.Selector.findChildElements(this.element(), ['div.notes'])[0]));
		MochiKit.Signal.connect(this.getId('recordNote'),	'onblur',	MochiKit.Base.method(this, 'handleLooseFocusEvent', MochiKit.Selector.findChildElements(this.element(), ['div.notes'])[0]));
		MochiKit.Signal.connect(this.getId('recordNote'),	'onchange',	MochiKit.Base.partial(MochiKit.Signal.signal, this, 'changedValue'));
		MochiKit.Signal.connect(this.getId('recordNote'),	'onkeyup',	MochiKit.Base.partial(MochiKit.Signal.signal, this, 'changedValue'));

		MochiKit.Signal.connect(this.getId('newFieldValue'),     'onkeydown',	this, 'handleKeyDownOnNewFieldValue');

		MochiKit.Signal.connect(this.getId('newFieldLabel'),	'onkeyup',	MochiKit.Base.partial(MochiKit.Signal.signal, this, 'changedValue'));
		MochiKit.Signal.connect(this.getId('newFieldIsLocked'),	'onclick',	this, 'toggleNewFieldIsHidden');
		MochiKit.Signal.connect(this.getId('newFieldValue'),	'onkeyup',	MochiKit.Base.partial(MochiKit.Signal.signal, this, 'changedValue'));

		MochiKit.Signal.connect(this.getId('newFieldAddButton'), 'onclick', 	this, 'handleAddClick');
	},

	//-------------------------------------------------------------------------

	'insertAllTooltips': function () {
		var tooltips;
		var tooltipEnabled;

		tooltips = {};
		tooltipEnabled = (this.hintMode() == 'ON');
		
		tooltips['title'] = new Clipperz.PM.UI.Common.Components.Tooltip({
			'element':	this.getElement('title'),
			'text':		"Insert here the title of the card",
			'position':	'RIGHT'
		});
		this.addComponent(tooltips['title']);
		MochiKit.Signal.connect(this.getId('title'),	'onfocus',	MochiKit.Base.method(this, 'showTooltipOnFocus', 'title'));
		MochiKit.Signal.connect(this.getId('title'),	'onblur',	MochiKit.Base.method(this, 'hideTooltipOnBlur',	 'title'));
		
		tooltips['newFieldTR'] = new Clipperz.PM.UI.Common.Components.Tooltip({
			'element':	this.getElement('newFieldTR'),
			'text':		"Insert your card new values here",
			'position':	'RIGHT'
		});
		this.addComponent(tooltips['newFieldTR']);
		MochiKit.Signal.connect(this.getId('newFieldLabel'),	'onfocus',	MochiKit.Base.method(this, 'showTooltipOnFocus', 'newFieldTR'));
		MochiKit.Signal.connect(this.getId('newFieldValue'),	'onblur',	MochiKit.Base.method(this, 'hideTooltipOnBlur',	 'newFieldTR'));

		tooltips['recordNote'] = new Clipperz.PM.UI.Common.Components.Tooltip({
			'element':	this.getElement('recordNote'),
			'text':		"You can insert some notes here",
			'position':	'RIGHT'
		});
		this.addComponent(tooltips['recordNote']);
		MochiKit.Signal.connect(this.getId('recordNote'),	'onfocus',	MochiKit.Base.method(this, 'showTooltipOnFocus', 'recordNote'));
		MochiKit.Signal.connect(this.getId('recordNote'),	'onblur',	MochiKit.Base.method(this, 'hideTooltipOnBlur',	 'recordNote'));

		this._tooltips = tooltips;
	},

	//.........................................................................

	'updateAllTooltipsEnabledMode': function (aStatus) {
		var	tooltipLabel;
		var tooltipEnabled;
		
		tooltipEnabled = (aStatus == 'ON') ? true : false;
		
		for (tooltipLabel in this.tooltips()) {
			this.tooltips()[tooltipLabel].setIsEnabled(tooltipEnabled);
		}
	},

	//.........................................................................

	'tooltips': function () {
		return this._tooltips;
	},

	//.........................................................................

	'showTooltipOnFocus': function (aTooltip, anEvent) {
		MochiKit.Async.callLater(0.5, MochiKit.Base.method(this.tooltips()[aTooltip], 'show'));
	},

	//.........................................................................

	'hideTooltipOnBlur': function (aTooltip, anEvent) {
		this.tooltips()[aTooltip].hide();
	},

	//-------------------------------------------------------------------------

	'displayElement': function() {
		return this.getElement('panel');
	},

	//-------------------------------------------------------------------------

	'fieldComponents': function () {
		return this._fieldComponents;
	},

	//-------------------------------------------------------------------------

	'directLoginComponents': function () {
		return this._directLoginComponents;
	},

	//-------------------------------------------------------------------------

	'hintMode': function () {
		return this._hintMode;
	},
	
	'setHintMode': function (aValue) {
		if (this._hintMode != aValue) {
			this._hintMode = aValue;

			this.updateAllTooltipsEnabledMode(this._hintMode);
//			if (this._hintMode == 'ON') {
//				this.enableHints();
//			}
		}
	},

	//-------------------------------------------------------------------------

	'focusOnNewFieldLabel': function () {
		this.getElement('newFieldLabel').focus();
	},
	
	//=========================================================================

	'isSavingEnabled': function () {
		return this._isSavingEnabled;
	},
	
	'setShouldEnableSaving': function (aValue) {
		this._isSavingEnabled = aValue || this.newFieldHasPendingChanges();

		if (this._isSavingEnabled == true) {
			MochiKit.DOM.addElementClass(this.getElement('panel'), 'hasPendingChanges');
			MochiKit.DOM.removeElementClass(this.getId('saveButton'), 'disabled');
		} else {
			MochiKit.DOM.removeElementClass(this.getElement('panel'), 'hasPendingChanges');
			MochiKit.DOM.addElementClass(this.getId('saveButton'), 'disabled');
		}
	},

	//=========================================================================

	'title': function () {
		return this.getElement('title').value;
	},

	'setTitle': function (aValue) {
		this.renderTitle(aValue);
	},

	//-------------------------------------------------------------------------

	'renderTitle': function (aValue) {
		this.getElement('title').value = Clipperz.Base.sanitizeString(aValue);
	},

	//-------------------------------------------------------------------------

	'setFocusOnTitleField': function () {
		this.getElement('title').focus();
	},

	//-------------------------------------------------------------------------

	'disableCardTitleEditing': function () {
		this.getElement('title').disabled = true;
		MochiKit.DOM.addElementClass(this.getElement('title').parentNode, 'disabled');
	},
	
	
	'enableCardTitleEditing': function () {
		this.getElement('title').disabled = false;
		MochiKit.DOM.removeElementClass(this.getElement('title').parentNode, 'disabled');
	},

	//=========================================================================

	'notes': function () {
		return this.getElement('recordNote').value;
	},

	'setNotes': function (aValue) {
		this.renderNotes(aValue);
	},

	//-------------------------------------------------------------------------

	'renderNotes': function (aValue) {
		var noteElement;

		noteElement = this.getElement('recordNote');
		
		if ((aValue != null) && (typeof(aValue) != 'undefined')) {
			noteElement.value = aValue;
		} else {
			noteElement.value = "";
		}
		
		this.fixNotesHeight();
	},

	//=========================================================================

	'addFieldRowComponent': function (aFieldComponent) {
		var	fieldTR;

		fieldTR = this.insertBefore(this.getElement('newFieldTR'), {tag:'tr', id:'recordFieldReferece_'+aFieldComponent.reference()});
		aFieldComponent.renderInNode(fieldTR);
		this.fieldComponents()[aFieldComponent.reference()] = aFieldComponent;
	},

	//=========================================================================

	'addDirectLoginComponent': function (aDirectLoginComponent) {
		var directLoginDIV;

		if (MochiKit.Base.keys(this.directLoginComponents()).length == 0) {
			this.hideNewDirectLoginSplash();
		}
		
		directLoginDIV = this.append(this.getElement('directLoginsList'), {tag:'div', cls:'directLoginItem'});
		aDirectLoginComponent.renderInNode(directLoginDIV);
		this.directLoginComponents()[aDirectLoginComponent.reference()] = aDirectLoginComponent;
	},

	'removeDirectLoginComponent': function (aDirectLoginComponent) {
		delete this.directLoginComponents()[aDirectLoginComponent.reference()];
		aDirectLoginComponent.remove();

		if (MochiKit.Base.keys(this.directLoginComponents()).length == 0) {
			this.showNewDirectLoginSplash();
		}
	},

	//=========================================================================

	'showNewDirectLoginSplash': function () {
		MochiKit.Style.showElement(this.getElement('addNewDirectLoginSplash'));
		MochiKit.Style.hideElement(this.getElement('addDirectLoginListItem'));
	},
	
	'hideNewDirectLoginSplash': function () {
		MochiKit.Style.hideElement(this.getElement('addNewDirectLoginSplash'));
		MochiKit.Style.showElement(this.getElement('addDirectLoginListItem'));
	},
	
	//=========================================================================

	'renderDirectLoginEditingComponent': function (aDirectLoginEditingComponent) {
		aDirectLoginEditingComponent.renderInNode(this.getElement('directLoginEditDetail'));
	},
	
	'placeDirectLoginEditingComponent': function () {
		var width;

		width =	 MochiKit.Style.getElementDimensions(this.getElement('directLoginsComponentContainer'))['w'];
		
		return Clipperz.Async.callbacks("CardDialogComponent.renderDirectLoginEditingComponent", [
			MochiKit.Base.bind(function () {
				MochiKit.Style.setElementPosition  (this.getElement('directLoginEditDetail'), {x:width, y:-MochiKit.Style.getElementDimensions(this.getElement('directLogins'))['h']});
				MochiKit.Style.setElementDimensions(this.getElement('directLoginEditDetail'), {w:width});
			}, this),

			MochiKit.Base.noop
		], {trace:false});
	},

	//=========================================================================

	'newFieldLabel': function () {
		return this.getElement('newFieldLabel').value;
	},

	'setNewFieldLabel': function (aValue) {
		this.getElement('newFieldLabel').value = aValue;
	},

	//-------------------------------------------------------------------------

	'newFieldValue': function () {
		return this.getElement('newFieldValue').value;
	},

	'setNewFieldValue': function (aValue) {
		this.getElement('newFieldValue').value = aValue;
	},

	//-------------------------------------------------------------------------

	'newFieldIsHidden': function () {
		return MochiKit.DOM.hasElementClass(this.getElement('newFieldIsLocked'), 'locked');
	},

	'setNewFieldIsHidden': function (aValue) {
		MochiKit.DOM.setElementClass(this.getElement('newFieldIsLocked'), (aValue ? 'locked': 'unlocked'));
		MochiKit.DOM.setElementClass(this.getElement('newFieldValueWrapper'), (aValue ? 'locked': 'unlocked'));
	},

	'toggleNewFieldIsHidden': function (anEvent) {
		anEvent.preventDefault();
		
		this.setNewFieldIsHidden(! this.newFieldIsHidden());
		MochiKit.Signal.signal(this, 'changedValue');
	},

	//-------------------------------------------------------------------------

	'newFieldHasPendingChanges': function () {
		return ((this.newFieldLabel() != '') || (this.newFieldValue() != '') || (this.newFieldIsHidden() == true));
	},

	'resetNewFieldInputs': function () {
		this.setNewFieldLabel('');
		this.setNewFieldValue('');
		this.setNewFieldIsHidden(false);
	},

	//=========================================================================

	'handleKeyDownOnNewFieldValue': function (anEvent) {
		MochiKit.Signal.signal(this, 'keyPressed', anEvent);
/*
		if ((anEvent.key().string == 'KEY_TAB') && this.newFieldHasPendingChanges()) {
			anEvent.preventDefault();

			MochiKit.Signal.signal(this, 'addField');
			this.getElement('newFieldLabel').focus();
		}
*/
	},

	//-------------------------------------------------------------------------

	'handleAddClick': function (anEvent) {
		anEvent.preventDefault();
		MochiKit.Signal.signal(this, 'addField');
		this.getElement('newFieldLabel').focus();
	},

	//=========================================================================

	'handleDeleteClick': function (aFieldKey, anEvent) {
		anEvent.preventDefault();
		MochiKit.Signal.signal(this, 'deleteField', aFieldKey);
	},

	//-------------------------------------------------------------------------

	'toggleLock': function (aFieldKey, anEvent) {
		var shouldRedrawAsLocked;
		var currentTRElement;
		
		anEvent.preventDefault();
		
		currentTRElement = Clipperz.DOM.get(aFieldKey);
		shouldRedrawAsLocked = (MochiKit.DOM.hasElementClass(currentTRElement, 'locked') ? false : true);

		this.renderFieldTR(currentTRElement, {
			label:MochiKit.Selector.findChildElements(currentTRElement, ['td.fieldLabel input'])[0].value,
			value:MochiKit.Selector.findChildElements(currentTRElement, ['td.fieldValue input'])[0].value
		}, shouldRedrawAsLocked, MochiKit.DOM.hasElementClass(currentTRElement, 'new'));
	},

	//=========================================================================

	'fixNotesHeight': function () {
		var element;
		
		element = this.getElement('recordNote');

		if (element.scrollHeight == 0) {
			MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'fixNotesHeight'));
		} else {
			var textareaHeight;

			textareaHeight = Math.min(Math.max(50, element.scrollHeight), 500);

			MochiKit.Style.setElementDimensions(element, {h:textareaHeight}, 'px');
			MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'fixRendering'));
		}
	},

	//-------------------------------------------------------------------------

	'fixRendering': function () {
//		var	height;
		var y;
		var	scrollHeight;
		var viewportHeight;
		var viewportY;
		var footerElement;
		var footerElementPosition;
		var footerElementDimensions;
		var footerComputedHeight;

//		height = MochiKit.Style.getElementDimensions(this.displayElement())['h'];
		y = MochiKit.Style.getElementPosition(this.displayElement())['y'];

		footerElement = MochiKit.Selector.findChildElements(this.displayElement(), ['div.footer'])[0];
		footerElementPosition = MochiKit.Style.getElementPosition(footerElement);
		footerElementDimensions = MochiKit.Style.getElementDimensions(footerElement);
		footerComputedHeight = footerElementPosition['y'] + footerElementDimensions['h'] - y;

//		scrollHeight = this.displayElement().scrollHeight;
		scrollHeight = footerComputedHeight;

		viewportHeight = MochiKit.Style.getViewportDimensions()['h'];
		viewportY = MochiKit.Style.getViewportPosition()['y'];

		if ((y + scrollHeight) > (viewportY + viewportHeight)) {
			this.setDisplayMode('scrollable');
			MochiKit.DOM.addElementClass(this.element(), 'scrollable');
			MochiKit.DOM.removeElementClass(this.element(), 'fixed');
			MochiKit.Style.setElementPosition(this.displayElement(), {y:Math.max(0, Math.min(y, (viewportY + viewportHeight) - scrollHeight))}, 'px');
			MochiKit.Visual.ScrollTo(this.displayElement(), {duration:0.5});
		} else {
			this.setDisplayMode('fixed');
			MochiKit.DOM.removeElementClass(this.element(), 'scrollable');
			MochiKit.DOM.addElementClass(this.element(), 'fixed');
		}
	},

	//=========================================================================

	'unselectCurrentSelectedItems': function () {
		MochiKit.Iter.forEach(MochiKit.Selector.findChildElements(this.displayElement(), ['.selectedField']), function (anElement) {
			MochiKit.DOM.removeElementClass(anElement, 'selectedField');
		});
	},

	//=========================================================================

	'hideProgressMask': function () {
		MochiKit.DOM.removeElementClass(this.getId('panel'), 'loading');
	},

	'showProgressMask': function () {
		this.getElement('progressDescription').innerHTML = "Saving";
		MochiKit.DOM.addElementClass(this.getId('panel'), 'loading');
	},

	'showError': function (anError) {
		MochiKit.Style.hideElement(this.getId('progress'));
		this.getElement('errorMessage').innerHTML = Clipperz.PM.Strings.errorDescriptionForException(anError['message']);
		MochiKit.Style.showElement(this.getId('error'));
	},

	//-------------------------------------------------------------------------

	'cancel': function () {
/*
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("CardDialogComponent.cancel", {trace:false});
		deferredResult.addCallback(MochiKit.Base.method(this, 'isSavingEnabled'));
		deferredResult.addIf([
			MochiKit.Base.method(this, 'askConfirmationForLoosingPendingChanges')
		], []);
		deferredResult.addCallback(MochiKit.Base.partial(MochiKit.Signal.signal, this, 'cancel'));
		deferredResult.callback();
		
		return deferredResult;
*/
		MochiKit.Signal.signal(this, 'cancel');
	},

	'handleCancelEvent': function (anEvent) {
		anEvent.preventDefault();
		this.cancel();
	},
	
	//-------------------------------------------------------------------------

	'handleSaveEvent': function (anEvent) {
		anEvent.preventDefault();
		
		if (! MochiKit.DOM.hasElementClass(anEvent.src(), 'disabled')) {
			MochiKit.Signal.signal(this, 'save');
		}
	},

	//-------------------------------------------------------------------------

	'handleAddDirectLogin': function (anEvent) {
		anEvent.preventDefault();
		
		MochiKit.Signal.signal(this, 'addDirectLogin');
	},

	//-------------------------------------------------------------------------

	'handleOnFocusEvent': function (anElement, anEvent) {
		this.unselectCurrentSelectedItems();
		MochiKit.DOM.addElementClass(anElement, 'selectedField');
	},

	'handleLooseFocusEvent': function (anElement, anEvent) {
		this.unselectCurrentSelectedItems();
	},

	//-------------------------------------------------------------------------

	'handleTabSelected': function (aSelectedTab) {
		this.unselectCurrentSelectedItems();

		switch (aSelectedTab) {
			case 'DETAILS':
//				MochiKit.Style.hideElement(this.getElement('addDirectLoginButton'));
				break;
			case 'DIRECT_LOGINS':
//				MochiKit.Style.showElement(this.getElement('addDirectLoginButton'));
				break;
			case 'SHARING':
//				MochiKit.Style.hideElement(this.getElement('addDirectLoginButton'));
				break;
		}
	},

	//-------------------------------------------------------------------------

	'handleKeyEvent': function (anEvent) {
		if (anEvent.key().string == 'KEY_ESCAPE') {
			MochiKit.Signal.signal(this, 'changedValue');
			this.cancel();
		} else if (anEvent.key().string == 'KEY_ENTER') {
			if (anEvent.target().nodeName == 'TEXTAREA') {
				
			} else {
				anEvent.preventDefault();
			}
		}
	},

	//=========================================================================

	'askConfirmationForLoosingPendingChanges': function () {
		var deferredResult;
		var confirmationDialog;
		
		confirmationDialog = new Clipperz.PM.UI.Common.Components.SimpleMessagePanel({
			title:	"Alert",
			text:	"Should lost pending changes?",
			type:	'ALERT',
			buttons: [
				{text:"Cancel",	result:'CANCEL', isDefault:true},
				{text:"Ok", result:'OK'}
			]
		});

		deferredResult = new Clipperz.Async.Deferred("CardDialogComponent.askConfirmationForLoosingPendingChanges", {trace:false});
//		deferredResult = confirmationDialog.deferredShow({openFromElement:anEvent.src(), onOkCloseToElement:MochiKit.DOM.currentDocument().body, onCancelCloseToElement:anEvent.src()});
		deferredResult.addMethod(confirmationDialog, 'deferredShow', {
			'openFromElement':			this.getElement('cancelButton'),
			'onOkCloseToElement':		null,	//	this.getElement('cancelButton'),
			'onCancelCloseToElement':	this.getElement('cancelButton')
		});
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'showDirectLoginEditingComponent': function () {
		var	width;
		var transition;
		var duration;
		
		width =	 MochiKit.Style.getElementDimensions(this.getElement('directLoginsComponentContainer'))['w'];
		transition = MochiKit.Visual.Transitions.sinoidal;
		duration = 1;

		return Clipperz.Async.callbacks("CardDialogComponent.showDirectLoginEditingComponent", [
			MochiKit.Base.method(this, 'disableCardTitleEditing'),
			MochiKit.Base.method(this.tabPanelController(), 'disable'),

			MochiKit.Base.bind(function () {
				MochiKit.Style.setElementPosition  (this.getElement('directLoginEditDetail'), {x:width, y:-MochiKit.Style.getElementDimensions(this.getElement('directLogins'))['h']});
				MochiKit.Style.setElementDimensions(this.getElement('directLoginEditDetail'), {w:width});
				MochiKit.Style.showElement(this.getElement('directLoginEditDetail'));
				MochiKit.Style.setOpacity(this.getElement('directLoginEditDetail'), 0);
				MochiKit.Style.setElementDimensions(this.getElement('directLoginsComponentContainer'), {
					h:Math.max(
						MochiKit.Style.getElementDimensions(this.getElement('directLogins'))['h'],
						MochiKit.Style.getElementDimensions(this.getElement('directLoginEditDetail'))['h']
					)
				});
//				MochiKit.Style.setElementDimensions(this.getElement('directLoginsComponentContainer'), {h:MochiKit.Style.getElementDimensions(this.getElement('directLogins'))['h']});
			}, this),
			MochiKit.Base.partial(Clipperz.Visual.deferredAnimations,	MochiKit.Visual.Parallel, [
				new MochiKit.Visual.Move(this.getElement('directLogins'),				{x:-width, y:0, mode:'relative', transition:transition, sync:true}),
				new MochiKit.Visual.Opacity(this.getElement('directLogins'),	 		{from:1.0, to:0.0, transition:transition, sync:true}),
				new MochiKit.Visual.Move(this.getElement('directLoginEditDetail'),		{x:-width, y:0, mode:'relative', transition:transition, sync:true}),
				new MochiKit.Visual.Opacity(this.getElement('directLoginEditDetail'),	{from:0.0, to:1.0, transition:transition, sync:true})
			], {duration:duration}),

			MochiKit.Base.noop
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'hideDirectLoginEditingComponent': function () {
		var	width;
		var transition;
		var duration;
	
		width =	 MochiKit.Style.getElementDimensions(this.getElement('directLoginsComponentContainer'))['w'];
		transition = MochiKit.Visual.Transitions.sinoidal;
		duration = 1;

		return Clipperz.Async.callbacks("CardDialogComponent.hideDirectLoginEditingComponent", [
			MochiKit.Base.partial(Clipperz.Visual.deferredAnimations,	MochiKit.Visual.Parallel, [
				new MochiKit.Visual.Move(this.getElement('directLogins'),				{x:width, y:0, mode:'relative', transition:transition, sync:true}),
				new MochiKit.Visual.Opacity(this.getElement('directLogins'), 			{from:0.0, to:1.0, transition:transition, sync:true}),
				new MochiKit.Visual.Move(this.getElement('directLoginEditDetail'),		{x:width, y:0, mode:'relative', transition:transition, sync:true}),
				new MochiKit.Visual.Opacity(this.getElement('directLoginEditDetail'),	{from:1.0, to:0.0, transition:transition, sync:true})
			], {duration:duration}),
//			MochiKit.Base.partial(MochiKit.Visual.appear, this.getElement('addDirectLoginButton'), {duration:0.3}),
			Clipperz.Async.clearResult,
			MochiKit.Base.partial(MochiKit.Style.hideElement, this.getElement('directLoginEditDetail')),
//			MochiKit.Base.partial(MochiKit.Style.showElement, this.getElement('directLogins')),
			MochiKit.Base.partial(MochiKit.Style.setElementDimensions, this.getElement('directLoginsComponentContainer'), {h:MochiKit.Style.getElementDimensions(this.getElement('directLogins'))['h']}),

			MochiKit.Base.method(this, 'enableCardTitleEditing'),
			MochiKit.Base.method(this.tabPanelController(), 'enable')
		], {trace:false});
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});
