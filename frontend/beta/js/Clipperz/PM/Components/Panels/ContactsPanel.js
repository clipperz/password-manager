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

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.Components) == 'undefined') { Clipperz.PM.Components = {}; }
if (typeof(Clipperz.PM.Components.Panels) == 'undefined') { Clipperz.PM.Components.Panels = {}; }

//#############################################################################

Clipperz.PM.Components.Panels.ContactsPanel = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Panels.ContactsPanel.superclass.constructor.call(this, anElement, args);

	this.render();
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Panels.ContactsPanel, Clipperz.PM.Components.Panels.BasePanel, {

	'toString': function() {
		return "Clipperz.PM.Components.ContactsPanel component";
	},

	//-------------------------------------------------------------------------

	'render': function() {
//		var tabPanelControllerConfig;

		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'table', border:'0', cellspacing:'0', cellpadding:'0', children:[
			{tag:'tbody', children:[
				{tag:'tr', children:[
					{tag:'td', valign:'top', width:'200', children:[
						{tag:'ul', id:"dataSubMenu", cls:'subMenu', children:[
							{tag:'li', id:this.getId('contacts'), htmlString:Clipperz.PM.Strings['contactsTabLabel']},
						]}
					]},
					{tag:'td', valign:'top', children:[
						{tag:'ul', cls:'clipperzTabPanels', children:[
							{tag:'li', id:this.getId('contactsPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['contactsTabTitle']},
									{tag:'div', htmlString:Clipperz.PM.Strings['comingSoon']}
								]}
							]}
						]}
					]}
				]}
			]}
		]});

//		tabPanelControllerConfig = {}
//		tabPanelControllerConfig[this.getId('contacts')] = this.getId('contactsPanel');
//		new Clipperz.PM.Components.TabPanel.TabPanelController({ config:tabPanelControllerConfig, selectedTab:this.getId('contacts') });
		this.tabPanelController().setUp();
	},
	
	//-------------------------------------------------------------------------

	'tabPanelController': function() {
		if (this._tabPanelController == null) {
			var tabPanelControllerConfig;
			
			tabPanelControllerConfig = {}
			tabPanelControllerConfig[this.getId('contacts')] = this.getId('contactsPanel');
			this._tabPanelController = new Clipperz.PM.Components.TabPanel.TabPanelController({ config:tabPanelControllerConfig, selectedTab:this.getId('contacts') });
		}
		
		return this._tabPanelController;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});
	
