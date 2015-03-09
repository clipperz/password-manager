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

Clipperz.PM.UI.Web.Components.AppPage = function(args) {
	args = args || {};
	Clipperz.PM.UI.Web.Components.AppPage.superclass.constructor.call(this, args);

	this._element = args.element || null;

	this._slots = {
		'cardGrid' : this.getId('cardGrid'),
//		'directLoginGrid' : this.getId('directLoginGrid'),
		'accountPanel': this.getId('accountPanel'),
		'dataPanel': this.getId('dataPanel'),
		'toolsPanel': this.getId('toolsPanel'),
		'userInfoBox': this.getId('userInfoBox'),
		'tabSidePanel': this.getId('tabSidePanel')
	};
		
	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.AppPage, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.AppPage component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(),				{tag:'div', id:this.getId('userInfoBox'), cls:'userInfoBox'});
		this.append(this.element(),				{tag:'div', id:this.getId('sidePanels'), cls:'sidePanels'});
		this.append(this.getId('sidePanels'),	{tag:'div', id:this.getId('tabSidePanel'), cls:'tabSidePanel', children:[]});

		this.append(this.element(), {tag:'div', id:this.getId('mainPanels'), cls:'mainPanels'});
		this.append(this.getId('mainPanels'), {tag:'div', id:this.getId('cardGrid'), cls:'gridComponent cardGrid'});
//		this.append(this.getId('mainPanels'), {tag:'div', id:this.getId('directLoginGrid'), cls:'gridComponent directLoginGrid'});		
		this.append(this.getId('mainPanels'), {tag:'div', id:this.getId('accountPanel'), cls:'otherPanel'});		
		this.append(this.getId('mainPanels'), {tag:'div', id:this.getId('dataPanel'), cls:'otherPanel'});		
		this.append(this.getId('mainPanels'), {tag:'div', id:this.getId('toolsPanel'), cls:'otherPanel'});		
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});
