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

Clipperz.PM.UI.Mobile.Components.Preferences = function(args) {
	args = args || {};

	Clipperz.PM.UI.Mobile.Components.Preferences.superclass.constructor.apply(this, arguments);

	this.render();

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Mobile.Components.Preferences, Clipperz.PM.UI.Mobile.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Mobile.Components.Preferences component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function () {
//		var	pageElement;
		var	headerElement;
		var titleElement;

//		pageElement = this.element().parentNode;
//		MochiKit.DOM.updateNodeAttributes(pageElement, {'data-add-back-btn': 'true'})
		headerElement = MochiKit.Selector.findChildElements(this.element().parentNode, ['div[data-role=header]'])[0];
//		headerElement.innerHTML = "Preferences";
		titleElement = MochiKit.Selector.findChildElements(headerElement, ['h1'])[0];
		titleElement.innerHTML = "Preferences";
		this.append(this.element(),
			{tag:'div', id:this.getId('listBox'), children:[
				{tag:'h1', html:"Preferences"}
			]}
		);

		this.append(headerElement, [
			//	'data-direction':'reverse', 'data-rel':'back', 
			{tag:'a', href:"#", id:this.getId('back'), cls:'ui-btn-left', html:"back" },
			{tag:'a', href:"#", id:this.getId('save'), cls:'ui-btn-right', html:"save" }
		]);

		MochiKit.Signal.connect(this.getElement('back'), 'onclick', MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'back'));
		MochiKit.Signal.connect(this.getElement('save'), 'onclick', MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'savePreferences'));
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});
