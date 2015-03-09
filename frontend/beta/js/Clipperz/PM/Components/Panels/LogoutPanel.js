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

Clipperz.PM.Components.Panels.LogoutPanel = function(args) {
	args = args || {};

    Clipperz.PM.Components.Panels.LogoutPanel.superclass.constructor.call(this, args);

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Panels.LogoutPanel, Clipperz.PM.Components.Panels.BasePanel, {

	'toString': function() {
		return "Clipperz.PM.Components.LogoutPanel component";
	},
	
	//-------------------------------------------------------------------------

	'initPanel': function() {
		var result;
		var	layout;
			
		result = new YAHOO.ext.ContentPanel(this.getId('panel'), {title:'logout', closable:false, autoCreate:true});

		Clipperz.YUI.DomHelper.append(result.getEl().dom,
			{tag:'div', children:[
				{tag:'h2', html:'Logout panel'}
			]}
		);
		
		return result;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});
	
