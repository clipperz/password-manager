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

/*
Clipperz.Base.module('Clipperz.PM.UI.Web.Components');

//#############################################################################

Clipperz.PM.UI.Web.Components.DirectLoginColumnManager = function(args) {
	args = args || {};
	Clipperz.PM.UI.Web.Components.DirectLoginColumnManager.superclass.constructor.call(this, args);

	this._actionMethod = args.actionMethod || null;
	
	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.DirectLoginColumnManager, Clipperz.PM.UI.Web.Components.ColumnManager, {

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.DateColumnManager component";
	},

	//-------------------------------------------------------------------------

	'actionMethod': function () {
		return this._actionMethod;
	},

	//-------------------------------------------------------------------------

	'renderCell': function(aRowElement, anObject) {
		var	tdElement;
		var	linkElement;

		tdElement = Clipperz.DOM.Helper.append(aRowElement, {tag:'td', cls:this.cssClass(), children:[
			{tag:'div',  cls:'directLogin_directLogin', children:[
				{tag:'div', cls:'directLogin_directLogin_body', children:[
					{tag:'a', href:'#', html:anObject[this.name()]}
				]}
			]}
		]});

		linkElement = MochiKit.DOM.getFirstElementByTagAndClassName('a', null, tdElement);
//		MochiKit.Signal.connect(linkElement, 'onclick', MochiKit.Base.method(this, 'handleLinkClick', anObject['_rowObject']));
		this.connectEvent(linkElement, 'onclick', MochiKit.Base.method(this, 'handleLinkClick', anObject['_rowObject']));
	},

	//-----------------------------------------------------

	'handleLinkClick': function (anObject, anEvent) {
		anEvent.preventDefault();

		if (this.actionMethod() != null) {
			this.actionMethod()(anObject, anEvent);
		}
	},

	//-----------------------------------------------------
	'__syntax_fix__' : 'syntax fix'
});

*/