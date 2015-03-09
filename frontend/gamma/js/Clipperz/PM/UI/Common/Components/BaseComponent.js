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

Clipperz.Base.module('Clipperz.PM.UI.Common.Components');

//#############################################################################

var _Clipperz_PM_Components_base_id_ = 0;

//#############################################################################

Clipperz.PM.UI.Common.Components.BaseComponent = function(args) {
	args = args || {};
	Clipperz.PM.UI.Common.Components.BaseComponent.superclass.constructor.call(this, args);

	this._element = args.element || null;
	this._ids = {};

	this._slots = {};
	this._slotComponents = {};

	this._components = {};

	this._cachedSlots = {};

	this._isModal = false;
	
	this._isActive = false;
	this._elementUsedToEnterModalState;

	this._isFullyRendered = false;
	this._renderingWaitingQueue = [];

//	this._slots = {
//		'header':	'header',
//		'body':		'body',
//		'footer':	'footer'
//	};

	return this;
}

//=============================================================================

//TODO get back to MochiKit.Base.update as we are not extending anything
//MochiKit.Base.update(Clipperz.PM.UI.Common.Components.BaseComponent.prototype, {
Clipperz.Base.extend(Clipperz.PM.UI.Common.Components.BaseComponent, /*Ext.Component*/ Object, {

	'isClipperzPMComponent': true,
	
	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Common.Components.BaseComponent component";
	},

	'componentId': function () {
		return this.getId('_id_');
	},

	//-------------------------------------------------------------------------
/*
	'slots': function() {
		return this._slots;
	},
*/
	'slotComponents': function() {
		return this._slotComponents;
	},

	//-------------------------------------------------------------------------

	'components': function () {
		return this._components;
	},

	'addComponent': function (aComponent) {
		this.components()[aComponent.componentId()] = aComponent;
	},
	
	'removeComponent': function (aComponent) {
		var componentId;
		
		componentId = aComponent.componentId();
		this.components()[componentId].remove();
		delete this.components()[componentId];
	},

	//-------------------------------------------------------------------------
/*
	'domHelper': function() {
		return Clipperz.YUI.DomHelper;
	},
*/	
	//-------------------------------------------------------------------------
/*
	'domHelperAppend': function(aValue) {
		Clipperz.YUI.DomHelper.append(this.element().dom, aValue);
	},
*/	
	//-------------------------------------------------------------------------

	'element': function() {
		return MochiKit.DOM.getElement(this._element);
	},
	
	'setElement': function(aNode) {
		this._element = aNode;
	},

	//-----------------------------------------------------

	'displayElement': function() {
		return this.element();
	},

	//-------------------------------------------------------------------------

	'renderInNode': function(aDomNode) {
		this.setElement(aDomNode);
		this.render();
	},

	'render': function() {
		this.clear();
		this.renderSelf();
		this.renderComponents();
		if (this.shouldShowTranslationHints()) {
			this.renderTranslationHints();
		}
		if (this.shouldShowElementWhileRendering()) {
			MochiKit.Style.showElement(this.displayElement());
		};

		this._isFullyRendered = true;
	
		MochiKit.Iter.forEach(this.renderingWaitingQueue(), MochiKit.Base.methodcaller('callback'));
		this.resetRenderingWaitingQueue();
	},

	'renderSelf': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'renderComponents': function() {
		var	slotName;
		
		for (slotName in this.slotComponents()) {
			this.slotComponents()[slotName].renderInNode(this.elementForSlotNamed(slotName));
		}
	},

	//.........................................................................

	'isFullyRendered': function () {
		return this._isFullyRendered;
	},

	//.........................................................................

	'renderingWaitingQueue': function () {
		return this._renderingWaitingQueue;
	},
	
	'resetRenderingWaitingQueue': function () {
		this._renderingWaitingQueue = [];
	},

	//.........................................................................

	'waitUntilFullyRendered': function () {
		var deferredResult;

		if (this.isFullyRendered() == true) {
			deferredResult = MochiKit.Async.succeed
		} else {
			deferredResult = new Clipperz.Async.Deferred("BaseComponent.waitUntilFullyRendered", {trace:false});
			this.renderingWaitingQueue().push(deferredResult);
		}

		return deferredResult;
	},

	//-----------------------------------------------------

	'renderTranslationHints': function () {
		var	translatableItems;
		
		translatableItems = MochiKit.Selector.findChildElements(this.displayElement(), ['[stringID]']);
		MochiKit.Iter.forEach(translatableItems, MochiKit.Base.method(this, 'enhanceTranslatableElement'))
	},

	'enhanceTranslatableElement': function (anElement) {
//Clipperz.log(">>> enhanceTranslatableElement", anElement);
//		new Clipperz.PM.UI.Common.Components.TranslatorWidget({
//			'element':	anElement
//		});

		MochiKit.Signal.connect(anElement, 'onmouseenter', MochiKit.Base.partial(Clipperz.PM.UI.Common.Components.TranslatorWidget.show, anElement, MochiKit.DOM.getNodeAttribute(anElement, 'stringID')));
		MochiKit.Signal.connect(anElement, 'onmouseleave', Clipperz.PM.UI.Common.Components.TranslatorWidget.hide);
//Clipperz.log("<<< enhanceTranslatableElement");
	},

	//-----------------------------------------------------

	'update': function(args) {
		throw Clipperz.Base.exception.AbstractMethod;
	},
	
	'updateSelf': function(args) {
		throw Clipperz.Base.exception.AbstractMethod;
	},
	
	'updateComponents': function(args) {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	//-----------------------------------------------------

	'refresh': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'refreshSelf': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'refreshComponents': function(args) {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	//-----------------------------------------------------

	'checkSlotNamed': function(aSlotName) {
		if (typeof(this._slots[aSlotName]) == 'undefined') {
			throw new Error("undefined slot");
		};
	},

	//-----------------------------------------------------

	'cachedSlots': function() {
		return this._cachedSlots;
	},
	
	'slotNamed': function(aSlotName) {
		var result;

		this.checkSlotNamed(aSlotName);
		if (typeof(this.cachedSlots()[aSlotName]) == 'undefined') {
			this.cachedSlots()[aSlotName] = new Clipperz.PM.UI.Common.Components.ComponentSlot(this,aSlotName);
		}
		
		result = this.cachedSlots()[aSlotName];
		
		return result;
	},

	//-----------------------------------------------------

	'elementForSlotNamed': function(aSlotName) {
		return MochiKit.DOM.getElement(this._slots[aSlotName]);
	},

	//-----------------------------------------------------

	'componentForSlotNamed': function(aSlotName) {
		return this.slotComponents()[aSlotName];
	},

	'setComponentForSlotNamed': function(aComponent, aSlotName) {
		var domNode;

		this.checkSlotNamed(aSlotName);

		if (this.slotComponents()[aSlotName] != null) {
			this.slotComponents()[aSlotName].remove();
		}

		this.slotComponents()[aSlotName] = aComponent;

//		domNode = MochiKit.DOM.getElement(this.slotNamed(aSlotName));
		domNode = this.elementForSlotNamed(aSlotName);

		if (domNode != null) {
			aComponent.renderInNode(domNode);
		}
	},

	//-----------------------------------------------------
/*
	'purgeListeners': function() {
		Clipperz.NotificationCenter.unregister(this);
		MochiKit.Signal.disconnectAllTo(this);
	},
*/	
	//-----------------------------------------------------

	'clear': function() {
		var	slotName;
		var componentId;

		MochiKit.Signal.disconnectAllTo(this);

		for (slotName in this.slotComponents()) {
			this.slotComponents()[slotName].clear();
		}

		for (componentId in this.components()) {
			this.components()[componentId].clear();
		}

//		if (this.element() != null) {
//			this.element().innerHTML = "";
//		}

		if (this.displayElement() != null) {
			if (this.element() != this.displayElement()) {
				MochiKit.DOM.removeElement(this.displayElement());
			} else {
				this.displayElement().innerHTML = "";
			}
		}
		
		if (this.isModal()) {
			//	TODO: cleanup when the closed element was shown modally.
		}
	},


	'remove': function() {
		var	slotName;
		var componentId;

		for (slotName in this.slotComponents()) {
			this.slotComponents()[slotName].remove();
			delete this.slotComponents()[slotName];
		}

		for (componentId in this.components()) {
			this.components()[componentId].remove();
			delete this.components()[componentId];
		}

		this.clear();
		MochiKit.Signal.disconnectAll(this);
	},

	'append': function(aNode, aValue) {
		return Clipperz.DOM.Helper.append(aNode, aValue);		
	},
	
	'insertBefore': function (aNode, aValue) {
		return Clipperz.DOM.Helper.insertBefore(aNode, aValue);
	},

	'insertAfter': function (aNode, aValue) {
		return Clipperz.DOM.Helper.insertAfter(aNode, aValue);
	},

	//-------------------------------------------------------------------------

	'getId': function(aValue) {
		var	result;

		if (typeof(aValue) != 'undefined') {
			result = this._ids[aValue];

			if (typeof(result) == 'undefined') {
				_Clipperz_PM_Components_base_id_ ++;

				result = "Clipperz_PM_Components_" + aValue + "_" + _Clipperz_PM_Components_base_id_;
				this._ids[aValue] = result;
			}
		} else {
//			result = Clipperz.PM.UI.Common.Components.BaseComponent.superclass.getId.call(this);
			throw "call to BaseComponent.getId with an undefined value";
		}
	
		return result;
	},

	'getAnchor': function (aValue) {
		return '#' + this.getId(aValue);
	},

	//-------------------------------------------------------------------------

	'getElement': function(aValue) {
		return Clipperz.DOM.get(this.getId(aValue));
	},

	//-------------------------------------------------------------------------

	'hideElement': function(anElementName) {
		MochiKit.Style.hideElement(this.getElement(anElementName));
	},

	'showElement': function(anElementName) {
		MochiKit.Style.showElement(this.getElement(anElementName));
	},

	//-------------------------------------------------------------------------
	
	'activate': function () {
		this._isActive = true;
	},
	
	'deactivate': function () {
		this._isActive = false;
	},
	
	'isActive': function () {
		return this._isActive;
	},
	
	//-------------------------------------------------------------------------

	'hideSlot': function(aSlotName) {
		if (this.componentForSlotNamed(aSlotName)) {
			this.componentForSlotNamed(aSlotName).deactivate();
		}
		MochiKit.Style.hideElement(this.elementForSlotNamed(aSlotName));
	},

	'showSlot': function(aSlotName) {
		if (this.componentForSlotNamed(aSlotName)) {
			this.componentForSlotNamed(aSlotName).activate();
		}
		MochiKit.Style.showElement(this.elementForSlotNamed(aSlotName));
	},
	
	//-------------------------------------------------------------------------

	'shouldShowTranslationHints': function () {
		return false;
	},
	
	'shouldShowElementWhileRendering': function() {
		return true;
	},

//	'shouldRemoveElementWhenClearningUp': function () {
//		return true;
//	},

	//-------------------------------------------------------------------------

	'isModal': function() {
		return this._isModal;
	},

	'setIsModal': function(aValue) {
		this._isModal = aValue;
	},

	//-------------------------------------------------------------------------

	'elementUsedToEnterModalState': function () {
		return this._elementUsedToEnterModalState;
	},

	'setElementUsedToEnterModalState': function (aValue) {
		this._elementUsedToEnterModalState = aValue;
	},

	//-------------------------------------------------------------------------

	'modalDialogMask': function () {
		return 'modalDialogMask';
	},
	
	'modalDialog': function () {
		return 'modalDialog';
	},
	
	'modalDialogFrame': function() {
		return 'modalDialogFrame'
	},

	//-------------------------------------------------------------------------

	'deferredShowModal': function(args) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("BaseComponent.deferredShowModal", {trace:false});

		deferredResult.addMethod(this, 'setIsModal', true);
		deferredResult.addCallback(MochiKit.Style.showElement, this.modalDialogMask());
		deferredResult.addCallback(MochiKit.Base.bind(function(someArgs) {
			var result;
			var duration;
			var from;
			var to;

			duration = someArgs.duration || 0.4;

			this.setElementUsedToEnterModalState(someArgs.openFromElement);
			from = Clipperz.Style.getSizeAndPosition(someArgs.openFromElement);
			this.renderInNode(this.modalDialog());
			MochiKit.DOM.addElementClass(this.modalDialog(), 'fixed');
			to = Clipperz.Style.getSizeAndPosition(this.displayElement());
			Clipperz.PM.UI.Common.Components.BaseComponent.targetModalDimensionsAndPosition = Clipperz.Base.deepClone(to);

			MochiKit.Style.hideElement(this.displayElement());
			MochiKit.Style.showElement(this.modalDialogFrame());
			
			result = {from:from, to:to, duration:duration};
			return result;
		}, this, args));
		deferredResult.addCallback(Clipperz.Visual.deferredResize, this.modalDialogFrame());
		deferredResult.addCallback(MochiKit.Base.bind(function(someArgs) {
			MochiKit.Style.hideElement(this.modalDialogFrame());
			MochiKit.Style.showElement(this.displayElement());
		}, this));
		deferredResult.addCallback(MochiKit.Async.succeed, arguments[arguments.length - 1]);
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'deferredHideModal': function(args) {
		var deferredResult;

		args = args || {};

		deferredResult = new Clipperz.Async.Deferred("BaseComponent.deferredHideModal", {trace:false});
		deferredResult.addCallback(MochiKit.Base.bind(function(someArgs) {
			var result;
			var from;
			var toElement;
			var to;
			var duration;

			toElement = args.closeToElement || this.elementUsedToEnterModalState();
			duration = someArgs.duration || 0.4;
			from = Clipperz.Style.getSizeAndPosition(this.displayElement());
			to = Clipperz.Style.getSizeAndPosition(toElement);

			MochiKit.Style.hideElement(this.displayElement());
			MochiKit.Style.showElement(this.modalDialogFrame());

			result = {from:from, to:to, duration:duration};
			return result;
		}, this, args));
		deferredResult.addCallback(Clipperz.Visual.deferredResize, this.modalDialogFrame());
		deferredResult.addCallback(MochiKit.Base.bind(function() {
			MochiKit.Style.hideElement(this.modalDialogFrame());
			MochiKit.Style.hideElement(this.modalDialogMask());
		}, this));
		deferredResult.addMethod(this, 'setIsModal', false);
		deferredResult.addMethod(this, 'clear');	//	##############
		deferredResult.addCallback(MochiKit.Async.succeed, arguments[arguments.length - 1]);
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
	
});

Clipperz.PM.UI.Common.Components.BaseComponent_modalDialog = function() {
	Clipperz.DOM.Helper.append(MochiKit.DOM.currentDocument().body,
		{tag:'div', id:'modalDialogWrapper', cls:'modalDialogWrapper', children:[
			{tag:'div', id:'modalDialogMask', cls:'modalDialogMask'},
			{tag:'div', id:'modalDialogFrame', cls:'modalDialogFrame' /*, html:"modal dialog frame"*/},
			{tag:'div', id:'modalDialog', cls:'modalDialog'}
//			{tag:'div', id:'modalDialog', cls:'modalDialog', children:[{tag:'div'}]}
		]}
	);
	
//	MochiKit.Style.hideElement('modalDialogWrapper');
	MochiKit.Style.hideElement('modalDialogMask');
	MochiKit.Style.hideElement('modalDialogFrame');
//	MochiKit.Style.hideElement('modalDialog');

};

//Clipperz.PM.UI.Common.Components.BaseComponent.targetModalDimensionsAndPosition = {'x':'X', 'y':'Y'};

MochiKit.DOM.addLoadEvent(Clipperz.PM.UI.Common.Components.BaseComponent_modalDialog);
