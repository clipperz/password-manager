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

Clipperz.PM.UI.Web.Components.ToolsPanel = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.ToolsPanel.superclass.constructor.apply(this, arguments);
	
	this._initiallySelectedTab = args.selected || 'PASSWORD_GENERATOR';
	this._tabPanelControllerConfiguration = {
		'PASSWORD_GENERATOR': {
			tab:	'passwordGeneratorTab',
			panel:	'passwordGeneratorPanel'
		},
		'BOOKMARKLET': {
			tab:	'bookmarkletTab',
			panel:	'bookmarkletPanel'
		},
		'COMPACT_EDITION': {
			tab:	'compactEditionTab',
			panel:	'compactEditionPanel'
		},
		'HTTP_AUTH': {
			tab:	'httpAuthTab',
			panel:	'httpAuthPanel'
		}
	};

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.ToolsPanel, Clipperz.PM.UI.Common.Components.TabPanelComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.ToolsPanel component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(), [
			{tag:'div', cls:'header', children:[
				{tag:'div', cls:'subPanelTabs', children:[
					{tag:'ul', children:[
						{tag:'li', id:this.getId('passwordGeneratorTab'),	children:[{tag:'a', href:'#', html:'Password generator'}], cls:'first'},
						{tag:'li', id:this.getId('bookmarkletTab'),			children:[{tag:'a', href:'#', html:'Bookmarklet'}]},
						{tag:'li', id:this.getId('compactEditionTab'),		children:[{tag:'a', href:'#', html:'Compact edition'}]},
						{tag:'li', id:this.getId('httpAuthTab'),			children:[{tag:'a', href:'#', html:'HTTP Auth'}]}
					]}
				]}
			]},
			{tag:'div', cls:'body', children:[
				{tag:'div', cls:'accountPanel', children:[
					{tag:'div', cls:'subPanelContent', children:[
						{tag:'ul', children:[
							{tag:'li', id:this.getId('passwordGeneratorPanel'),	children:[
//								{tag:'h3', html:"Password generator"}
							]},
							{tag:'li', id:this.getId('bookmarkletPanel'),	children:[
//								{tag:'h3', html:"Bookmarklet"}
							]},
							{tag:'li', id:this.getId('compactEditionPanel'),		children:[
//								{tag:'h3', html:"Compact edition"}
							]},
							{tag:'li', id:this.getId('httpAuthPanel'),		children:[
//								{tag:'h3', html:"HTTP Auth"}
							]}
						]}
					]}
				]}
			]},
			{tag:'div', cls:'footer'}
		]);

		this.tabPanelController().setup({selected:this.initiallySelectedTab()});
	},
	
	//-------------------------------------------------------------------------


	__syntaxFix__: "syntax fix"
});
