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

Clipperz.Base.module('Clipperz.PM.UI.Mobile.Components');

//#############################################################################

var _Clipperz_PM_Components_base_id_ = 0;

//#############################################################################

Clipperz.PM.UI.Mobile.Components.BaseComponent = function(args) {
	args = args || {};
	Clipperz.PM.UI.Mobile.Components.BaseComponent.superclass.constructor.call(this, args);

	this._element = args.element || null;
	this._ids = {};

	this._isFullyRendered = false;
//	this._renderingWaitingQueue = [];

	return this;
}

//=============================================================================

//MochiKit.Base.update(Clipperz.PM.UI.Mobile.Components.BaseComponent, Object, {
Clipperz.Base.extend(Clipperz.PM.UI.Mobile.Components.BaseComponent, Object, {

	'isClipperzPMComponent': true,
	
	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Mobile.Components.BaseComponent component";
	},

	'componentId': function () {
		return this.getId('_id_');
	},

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
//		this.renderComponents();
//		if (this.shouldShowTranslationHints()) {
//			this.renderTranslationHints();
//		}
		if (this.shouldShowElementWhileRendering()) {
			MochiKit.Style.showElement(this.displayElement());
		};

		this._isFullyRendered = true;
	
//		MochiKit.Iter.forEach(this.renderingWaitingQueue(), MochiKit.Base.methodcaller('callback'));
//		this.resetRenderingWaitingQueue();
	},

	'renderSelf': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},

//	'renderComponents': function() {
//		var	slotName;
//		
//		for (slotName in this.slotComponents()) {
//			this.slotComponents()[slotName].renderInNode(this.elementForSlotNamed(slotName));
//		}
//	},

	//.........................................................................

	'shouldShowElementWhileRendering': function() {
		return false;
	},

	//.........................................................................

	'isFullyRendered': function () {
		return this._isFullyRendered;
	},

	//.........................................................................
/*
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
*/
	//-----------------------------------------------------
/*
	'update': function(args) {
		throw Clipperz.Base.exception.AbstractMethod;
	},
	
	'updateSelf': function(args) {
		throw Clipperz.Base.exception.AbstractMethod;
	},
	
	'updateComponents': function(args) {
		throw Clipperz.Base.exception.AbstractMethod;
	},
*/
	//-----------------------------------------------------
/*
	'refresh': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'refreshSelf': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'refreshComponents': function(args) {
		throw Clipperz.Base.exception.AbstractMethod;
	},
*/
	//-----------------------------------------------------

	'clear': function() {
		var	slotName;
		var componentId;

		MochiKit.Signal.disconnectAllTo(this);

		if (this.displayElement() != null) {
			if (this.element() != this.displayElement()) {
				MochiKit.DOM.removeElement(this.displayElement());
			} else {
				this.displayElement().innerHTML = "";
			}
		}
	},

	'remove': function() {
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

	__syntaxFix__: "syntax fix"
	
});
