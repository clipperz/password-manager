/*

Copyright 2008-2018 Clipperz Srl

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

Clipperz.Base.module('Clipperz.PM.UI.Components');

Clipperz.PM.UI.Components.Overlay = function(args) {
	args = args || {};

	this._defaultDelay = 2;
	this._element = MochiKit.DOM.getElement('overlay');

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Components.Overlay, Object, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Components.Overlay component";
	},

	'element': function () {
//		return MochiKit.DOM.getElement('overlay');
		return this._element;
	},

	'getElement': function (aClass) {
		return MochiKit.Selector.findChildElements(this.element(), ['.'+aClass])[0];
	},

	//-------------------------------------------------------------------------

	'show': function (aMessage, showMask, showProgress, showOnCardDetail) {
		if (showMask === true) {
			this.showMask(showOnCardDetail);
		} else {
			this.hideMask();
		}

		if (showProgress === true) {
			this.showProgressBar();
		}

		if (! aMessage && ! showProgress) {
			MochiKit.DOM.addElementClass(this.element(), 'spinnerOnly');
		} else {
			MochiKit.DOM.removeElementClass(this.element(), 'spinnerOnly');
		}

		if (showOnCardDetail) {
			MochiKit.DOM.addElementClass(this.element(), 'card');
		} else {
			MochiKit.DOM.removeElementClass(this.element(), 'card');
		}

		this.resetStatus();
		this.setMessage(aMessage);
		MochiKit.DOM.removeElementClass(this.element(), 'ios-overlay-hide');
		MochiKit.DOM.addElementClass(this.element(), 'ios-overlay-show');
	},
	
	'done': function (aMessage, aDelayBeforeHiding) {
		this.hideMask();
		this.hideProgressBar();
		this.completed(this.showDoneIcon, aMessage, aDelayBeforeHiding);
	},
	
	'setCustomResultIcon': function (anIcon) {
		this.getElement('custom').innerHTML = anIcon;
		MochiKit.Style.showElement(this.getElement('custom'));
	},

	'customResult': function (anIcon, aMessage, aDelayBeforeHiding) {
		this.hideMask();
		this.hideProgressBar();
		this.completed(MochiKit.Base.method(this, 'setCustomResultIcon', anIcon), aMessage, aDelayBeforeHiding);
	},
	
	'failed': function (aMessage, aDelayBeforeHiding) {
		this.completed(this.showFailIcon, aMessage, aDelayBeforeHiding);
	},

	//-------------------------------------------------------------------------

	'maskElement': function () {
		return this.getElement('mask');
	},
	
	'showMask': function (showOnCardDetail) {
		MochiKit.DOM.removeElementClass(this.maskElement(), 'card');

		if (showOnCardDetail) {
			MochiKit.DOM.addElementClass(this.maskElement(), 'card');
		}
		MochiKit.DOM.removeElementClass(this.maskElement(), 'hidden');
	},
	
	'hideMask': function () {
		MochiKit.DOM.addElementClass(this.maskElement(), 'hidden');
	},
	
	//-------------------------------------------------------------------------

	'resetStatus': function () {
		MochiKit.Style.showElement(this.element());
		MochiKit.Style.showElement(this.getElement('spinner'));
		MochiKit.Style.hideElement(this.getElement('done'));
		MochiKit.Style.hideElement(this.getElement('failed'));
	},

	'setMessage': function (aMessage) {
		if (typeof(aMessage) != 'undefined') {
			this.getElement('title').innerHTML = aMessage;
		}
	},

	'completed': function (aFunctionToShowResult, aMessage, aDelayBeforeHiding) {
		var delay = aDelayBeforeHiding || this.defaultDelay();

		this.hideSpinner();
		MochiKit.Base.bind(aFunctionToShowResult, this)();
		this.setMessage(aMessage);

		return MochiKit.Async.callLater(delay, MochiKit.Base.bind(this.hide, this))
	},

	'hide': function (withoutAnimationTime) {
		var secondsBeforeHiding = withoutAnimationTime ? 0 : 1;
		var element = this.element();

//		this.getElement('custom').innerHTML = '';
//		MochiKit.Style.hideElement(this.getElement('custom'));

		this.hideProgressBar();
		MochiKit.DOM.removeElementClass(element, 'ios-overlay-show');
		MochiKit.DOM.addElementClass(element, 'ios-overlay-hide');
//		return MochiKit.Async.callLater(secondsBeforeHiding, MochiKit.Style.hideElement, element);
		return MochiKit.Async.callLater(secondsBeforeHiding, MochiKit.Base.method(this, 'hideElement'));
	},

	'hideElement': function () {
		this.getElement('custom').innerHTML = '';
		MochiKit.Style.hideElement(this.element())
	},

	'hideSpinner': function () {
		MochiKit.Style.hideElement(this.getElement('spinner'));
	},
	
	'showDoneIcon': function () {
		MochiKit.Style.showElement(this.getElement('done'));
	},
	
	'showFailIcon': function () {
		MochiKit.Style.showElement(this.getElement('failed'));
	},
	
	//-------------------------------------------------------------------------

	'showProgressBar': function () {
		MochiKit.Style.showElement(this.getElement('progressBar'));
	},

	'hideProgressBar': function () {
		MochiKit.Style.hideElement(this.getElement('progressBar'));
	},

	'updateProgress': function (aProgressPercentage) {
		MochiKit.Style.setElementDimensions(this.getElement('progress'), {'w': aProgressPercentage}, '%');
//console.log("OVERLAY - updating progress: " + aProgressPercentage + "%");
	},

	//-------------------------------------------------------------------------

	'defaultDelay': function () {
		return this._defaultDelay;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

