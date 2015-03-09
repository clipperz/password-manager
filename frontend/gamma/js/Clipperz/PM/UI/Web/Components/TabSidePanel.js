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

Clipperz.PM.UI.Web.Components.TabSidePanel = function(args) {
	args = args || {};
	Clipperz.PM.UI.Web.Components.TabSidePanel.superclass.constructor.call(this, args);

	this._element = args.element || null;

	this._slots = {
	};
		
	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.TabSidePanel, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.TabSidePanel component";
	},

	//-------------------------------------------------------------------------

	'deselectAllTabs': function() {
		var tabListItems;
		
		tabListItems = [
			'cardsLI',
//			'directLoginLI',
			'accountLI',
			'dataLI',
			'toolsLI'
		];
		
//Clipperz.log("=== TabSidePanel.tabSelected anEvent.src().id", anEvent.src().id);
		for (var i in tabListItems) {
//Clipperz.log("=== TabSidePanel.tabSelected aTabListItem", tabListItems[i]); 
			MochiKit.DOM.removeElementClass(this.getId(tabListItems[i]), 'selected');
		}
	},
	
	'selectTab': function(aTabName) {
		this.deselectAllTabs();
		MochiKit.DOM.addElementClass(this.getId(this.listItemIdForTabNamed(aTabName)), 'selected');
		MochiKit.Signal.signal(this, 'tabSelected', aTabName);
	},

	'tabNameForAnchorId': function(anId) {
		var result;
		
		switch(anId) {
			case 'cards_tabSidePanel':
				result = 'cards';
				break;
//			case 'directLogins_tabSidePanel':
//				result = 'directLogins';
//				break;
			case 'account_tabSidePanel':
				result = 'account';
				break;
			case 'data_tabSidePanel':
				result = 'data';
				break;
			case 'tools_tabSidePanel':
				result = 'tools';
				break;
		}
		
		return result;
	},
	
	'listItemIdForTabNamed': function(aTabName) {
		var result;
		
		switch (aTabName) {
			case 'cards':
				result = 'cardsLI';
				break;
//			case 'directLogins':
//				result = 'directLoginLI';
//				break;
			case 'account':
				result = 'accountLI';
				break;
			case 'data':
				result = 'dataLI';
				break;
			case 'tools':
				result = 'toolsLI';
				break;
		}
		
		return result;
	},
	
	'tabSelected': function (anEvent) {
		this.selectTab(this.tabNameForAnchorId(anEvent.src().id));
//		anEvent.stop();
		anEvent.preventDefault();
	},

	//-------------------------------------------------------------------------

	'addCard': function (anEvent) {
		anEvent.stop();
		MochiKit.Signal.signal(this, 'addCard', anEvent.src());
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(), [
			{tag:'div', cls:'header'},
			{tag:'div', cls:'body', children:[
				{tag:'ul', cls:'mainTabs', children:[
					{tag:'li', id:this.getId('cardsLI'), cls:'cards', children:[
						{tag:'a', id:'cards_tabSidePanel', href:'#', html:"cards"},
						{tag:'div', cls:'selectionHighlighter', children:[
							{tag:'img', src:'./images/old/main/tabs/selectionHighligher.png'},
							{tag:'a', id:this.getId('addCardA'), cls:'add', href:'#', children:[
								{tag:'span', html:"add"},
								{tag:'h3', html:"+"}
							]}
						]}
					]}
				]},
				{tag:'ul', cls:'otherTabs', children:[
					{tag:'li', id:this.getId('accountLI'), children:[
						{tag:'a', id:'account_tabSidePanel', href:'#', html:"account"},
						{tag:'div', cls:'selectionHighlighter', children:[
							{tag:'img', src:'./images/old/main/tabs/selectionHighligherGray.png'}
						]}
					]},
					{tag:'li', id:this.getId('dataLI'), children:[
						{tag:'a', id:'data_tabSidePanel', href:'#', html:"data"},
						{tag:'div', cls:'selectionHighlighter', children:[
							{tag:'img', src:'./images/old/main/tabs/selectionHighligherGray.png'}
						]}
					]},
					{tag:'li', id:this.getId('toolsLI'), children:[
						{tag:'a', id:'tools_tabSidePanel', href:'#', html:"tools"},
						{tag:'div', cls:'selectionHighlighter', children:[
							{tag:'img', src:'./images/old/main/tabs/selectionHighligherGray.png'}
						]}
					]}
				]}
			]},
			{tag:'div', cls:'footer'}
		]);
	
		MochiKit.Signal.connect('cards_tabSidePanel', 'onclick', this, 'tabSelected');
//		MochiKit.Signal.connect('directLogins_tabSidePanel', 'onclick', this, 'tabSelected');
		MochiKit.Signal.connect('account_tabSidePanel', 'onclick', this, 'tabSelected');
		MochiKit.Signal.connect('data_tabSidePanel', 'onclick', this, 'tabSelected');
		MochiKit.Signal.connect('tools_tabSidePanel', 'onclick', this, 'tabSelected');
		MochiKit.Signal.connect(this.getId('addCardA'), 'onclick', this, 'addCard');
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});
