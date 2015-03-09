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
if (typeof(Clipperz.PM.Components.TabPanel) == 'undefined') { Clipperz.PM.Components.TabPanel = {}; }

Clipperz.PM.Components.TabPanel.TabPanelController = function(args) {
	args = args || {};

    Clipperz.PM.Components.TabPanel.TabPanelController.superclass.constructor.call(this);

	this._name = args.name || 'undefined';
	this._config = args.config;
	this._selectedTab = args.selectedTab || ((MochiKit.Base.keys(args.config).length > 0) ? MochiKit.Base.keys(args.config)[0] : null);
	
	this._tabs = {};
	this._panels = {};

	Clipperz.NotificationCenter.register(null, 'selectTab', this, 'handleSelectTabNotification');
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.TabPanel.TabPanelController, YAHOO.ext.util.Observable, {

	//-------------------------------------------------------------------------

	'name': function() {
		return this._name;
	},
	
	//-------------------------------------------------------------------------

	'tabs': function() {
		return this._tabs;
	},
	
	//-------------------------------------------------------------------------

	'panels': function() {
		return this._panels;
	},
	
	//-------------------------------------------------------------------------

	'config': function() {
		return this._config;
	},

	//-------------------------------------------------------------------------

	'selectedTab': function() {
		return this._selectedTab;
	},
	
	'setSelectedTab': function(aValue) {
		this._selectedTab = aValue;
	},
	
	//-------------------------------------------------------------------------

	'setUp': function() {
		var	tabId;
		
//MochiKit.Logging.logDebug(">>> TabPanelController.setUp - config: " + Clipperz.Base.serializeJSON(this.config()));
		for (tabId in this.config()) {
			var	tabElement;
			var	panelElement;

//MochiKit.Logging.logDebug("--- TabPanelController.setUp - tabId: " + tabId);
//MochiKit.Logging.logDebug("--- TabPanelController.setUp - panelId: " + this.config()[tabId]);
			tabElement = YAHOO.ext.Element.get(tabId);
			tabElement.addClassOnOver("hover");
			MochiKit.Signal.connect(tabId, 'onclick', this, 'selectTabHandler');

			panelElement = YAHOO.ext.Element.get(this.config()[tabId]);
			
			this._tabs[tabId] = tabElement;
			this._panels[tabId] = panelElement;
			
			if (tabId == this.selectedTab()) {
				tabElement.addClass('selectedTab');
				panelElement.addClass('selectedPanel');
			} else {
				panelElement.addClass('hiddenPanel');
			}
		}
//MochiKit.Logging.logDebug("<<< TabPanelController.setUp");
	},

	//-------------------------------------------------------------------------

	'selectTab': function(aTab) {
		if (aTab != this.selectedTab()) {
			this.tabs()[this.selectedTab()].removeClass('selectedTab');
			this.panels()[this.selectedTab()].removeClass('selectedPanel').addClass('hiddenPanel');
		
			this.tabs()[aTab].addClass('selectedTab');
			this.panels()[aTab].addClass('selectedPanel').removeClass('hiddenPanel');
			
			this.setSelectedTab(aTab);
			
			Clipperz.NotificationCenter.notify(this, 'tabSelected', aTab);
		}
	},

	//-------------------------------------------------------------------------

	'selectTabHandler': function(anEvent) {
		this.selectTab(anEvent.src().id);
	},

	//-------------------------------------------------------------------------

	'handleSelectTabNotification': function(aNotificationEvent) {
		var parameters;
		var splittedParamters;
		var	targetTabPanel;

		parameters = aNotificationEvent.parameters();
		splittedParamters = parameters.split('.');
		targetTabPanel = splittedParamters[0];
		if (targetTabPanel == this.name()) {
			this.selectTab(splittedParamters[1])
		}
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
