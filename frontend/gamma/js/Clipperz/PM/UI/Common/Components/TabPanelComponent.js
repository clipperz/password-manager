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

Clipperz.PM.UI.Common.Components.TabPanelComponent = function(args) {
	args = args || {};
	Clipperz.PM.UI.Common.Components.TabPanelComponent.superclass.constructor.call(this, args);

	this._tabPanelController = null;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Common.Components.TabPanelComponent, Clipperz.PM.UI.Common.Components.BaseComponent, {

	'toString': function () {
		return "Clipperz.PM.UI.Common.Components.TabPanelComponent component";
	},

	//-------------------------------------------------------------------------

	'tabPanelControllerConfiguration': function() {
		return this._tabPanelControllerConfiguration;
	},

	'tabPanelController': function() {
		if (this._tabPanelController == null) {
			this._tabPanelController = new Clipperz.PM.UI.Common.Controllers.TabPanelController({component:this, configuration:this.tabPanelControllerConfiguration()});
		}
		
		return this._tabPanelController;
	},

	'initiallySelectedTab': function() {
		return this._initiallySelectedTab;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});
