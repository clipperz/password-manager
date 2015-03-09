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

Clipperz.PM.UI.Common.Components.PasswordEntropyDisplay = function(anElement, args) {
	args = args || {};

	Clipperz.PM.UI.Common.Components.PasswordEntropyDisplay.superclass.constructor.call(this, anElement, args);

	this._wrapperElement = null;
	this._entropyElement = null;

	this.render();
	
	return this;
};

Clipperz.Base.extend(Clipperz.PM.UI.Common.Components.PasswordEntropyDisplay, Clipperz.PM.UI.Common.Components.BaseComponent, {
	
	'toString': function() {
		return "Clipperz.PM.UI.Common.Components.PasswordEntropyDisplay";
	},

	//-----------------------------------------------------

	'wrapperElement': function() {
		return this._wrapperElement;
	},
	
	'setWrapperElement': function(aValue) {
		this._wrapperElement = aValue;
	},
	
	//-----------------------------------------------------

	'passwordElement': function() {
		return this.element();
	},
	
	//-----------------------------------------------------

	'entropyElement': function() {
		return this._entropyElement;
	},
	
	'setEntropyElement': function(aValue) {
		this._entropyElement = aValue;
	},

	//-----------------------------------------------------

	'render': function() {
/*
		MochiKit.Signal.disconnectAllTo(this);

		this.setWrapperElement(this.element().wrap({tag:'div'}));
		this.setEntropyElement(Clipperz.DOM.Helper.append(this.wrapperElement().dom, {tag:'div', cls:'passwordEntropy', html:"&nbsp;"}, true));
//		this.setEntropyElement(Clipperz.DOM.Helper.insertBefore(this.element(), {tag:'div', cls:'passwordEntropy', html:"&nbsp;"}, true));
		this.entropyElement().wrap({tag:'div', cls:'passwordEntropyWrapper'});

		this.updateEntropyElement();
		
		this.connect('onkeyup', 'updateEntropyElement');
		this.connect('onchange', 'updateEntropyElement');
		this.connect('onblur', 'updateEntropyElement');
*/
		MochiKit.Signal.disconnectAllTo(this);

		this.setEntropyElement(this.element());
		this.entropyElement().addClass("entropyLevelIndicator");

		this.updateEntropyElement();

		this.connect('onkeyup', 'updateEntropyElement');
		this.connect('onchange', 'updateEntropyElement');
		this.connect('onblur', 'updateEntropyElement');
	},

	//-----------------------------------------------------

	'computeEntropyForString': function(aValue) {
		return Clipperz.PM.Crypto.passwordEntropy(aValue);
	},

	//-----------------------------------------------------

	'updateEntropyElement': function(anEvent) {
		var	entropy;
		
		entropy = Math.min(128, this.computeEntropyForString(this.passwordElement().dom.value));

		if (entropy == 0) {
			this.entropyElement().setStyle('background-position', "0px 26px");
		} else {
			this.entropyElement().setStyle('background-position', "0px -" + (128-entropy)*26 + "px");
		}
	},
	
	//-----------------------------------------------------
	__syntaxFix__: '__syntaxFix__'
});
