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

Clipperz.Base.module('Clipperz.PM.UI.Common.Controllers');

Clipperz.PM.UI.Common.Controllers.TabPanelController = function(args) {
	args = args || {};

	this._component			= args.component;
	this._configuration 	= args.configuration;
	this._isEnabled			= args.enabled || true;

	this._selectedTab = null;
	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.Common.Controllers.TabPanelController.prototype, {

	'toString': function() {
		return "Clipperz.PM.UI.Common.Controllers.TabPanelController";
	},

	//-----------------------------------------------------------------------------
	
	'component': function() {
		return this._component;
	},
	
	'configuration': function() {
		return this._configuration;
	},

	//-----------------------------------------------------------------------------

	'getElement': function(anElementID) {
		return this.component().getElement(anElementID);
	},

	'tabForTabElement': function(anElement) {
		var	result;
		
		for (result in this.configuration()) {
			if (this.getElement(this.configuration()[result]['tab']) == anElement) {
				break;
			}
		}
		
		return result;
	},
	
	//-----------------------------------------------------------------------------

	'setupTab': function(aConfiguration) {
		var	tabElement;
		
		tabElement = this.getElement(aConfiguration['tab']);
		
		MochiKit.DOM.removeElementClass(tabElement, 'selected');
		MochiKit.Signal.connect(tabElement, 'onclick', this, 'handleTabClick')
	},

	'setupPanel': function(aConfiguration) {
		this.hidePanel(aConfiguration['panel']);
	},

	'setup': function(args) {
		args = args || {};
		
		MochiKit.Base.map(MochiKit.Base.method(this, 'setupTab'),	MochiKit.Base.values(this.configuration()));
		MochiKit.Base.map(MochiKit.Base.method(this, 'setupPanel'),	MochiKit.Base.values(this.configuration()));
		this.selectTab(args.selected);
	},
	
	//-----------------------------------------------------------------------------

	'hidePanel': function(aPanel) {
			MochiKit.DOM.removeElementClass(this.getElement(aPanel), 'selected');
	},
	
	'selectTab': function(aTab) {
		if ((aTab != this.selectedTab()) && (this.isEnabled())) {
			if (this.selectedTab() != null) {
				MochiKit.DOM.removeElementClass(this.getElement(this.configuration()[this.selectedTab()]['tab']),	'selected');
				MochiKit.DOM.removeElementClass(this.getElement(this.configuration()[this.selectedTab()]['panel']),	'selected');
			}

			if (aTab != null) {
				MochiKit.DOM.addElementClass(this.getElement(this.configuration()[aTab]['tab']),	'selected');
				MochiKit.DOM.addElementClass(this.getElement(this.configuration()[aTab]['panel']),	'selected');
			}

			this.setSelectedTab(aTab);
			MochiKit.Signal.signal(this, 'tabSelected', aTab);
		}
	},

	//-----------------------------------------------------------------------------

	'selectedTab': function() {
		return this._selectedTab;
	},

	'setSelectedTab': function(aTab) {
		this._selectedTab = aTab;
	},

	//-----------------------------------------------------------------------------

	'selectedTabElement': function() {
		var result;

		if (this.selectedTab() != null) {
			result = this.getElement(this.configuration()[this.selectedTab()]['tab']);
		} else {
			result = null;
		}
		
		return null;
	},

	'selectedTabPanelElement': function() {
		var result;
		
		if (this.selectedTab() != null) {
			result = this.getElement(this.configuration()[this.selectedTab()]['panel']);
		} else {
			result = null;
		}
		
		return result;
	},

	//-----------------------------------------------------------------------------

	'handleTabClick': function(anEvent) {
		this.selectTab(this.tabForTabElement(anEvent.src()));
		anEvent.preventDefault();
	},

	//=============================================================================

	'isEnabled': function () {
		return this._isEnabled;
	},

	'enable': function () {
		this._isEnabled = true;
		MochiKit.Base.map(MochiKit.Base.bind(function (aTabComponentID) {
			MochiKit.DOM.removeElementClass(this.getElement(this.configuration()[aTabComponentID]['tab']), 'disabled');
		}, this), MochiKit.Base.keys(this.configuration()));
	},
	
	'disable': function () {
		this._isEnabled = false;
		MochiKit.Base.map(MochiKit.Base.bind(function (aTabComponentID) {
			MochiKit.DOM.addElementClass(this.getElement(this.configuration()[aTabComponentID]['tab']), 'disabled');
		}, this), MochiKit.Base.keys(this.configuration()));
	},
	
	//=============================================================================

	__syntaxFix__: "syntax fix"
});
