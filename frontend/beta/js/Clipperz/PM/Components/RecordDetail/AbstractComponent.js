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
if (typeof(Clipperz.PM.Components.RecordDetail) == 'undefined') { Clipperz.PM.Components.RecordDetail = {}; }

//#############################################################################

Clipperz.PM.Components.RecordDetail.AbstractComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.RecordDetail.AbstractComponent.superclass.constructor.call(this, args);

	this._element = anElement;
	this._mainComponent = args.mainComponent;
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.AbstractComponent, Clipperz.PM.Components.BaseComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.AbstractComponent";
	},

	//-------------------------------------------------------------------------

	'mainComponent': function() {
		return this._mainComponent;
	},
	
	//-------------------------------------------------------------------------

	'record': function() {
		return this.mainComponent().record();
	},
	
	//-------------------------------------------------------------------------

	'editMode': function() {
		return this.mainComponent().editMode();
	},
	
	//-------------------------------------------------------------------------

	'render': function() {
		this.element().update("");
		this.update();
	},

	//-------------------------------------------------------------------------

	'update': function(anEvent) {
		if (this.editMode() == 'EDIT') {
			this.updateEditMode();
		} else if (this.editMode() == 'VIEW') {
			this.updateViewMode();
		}
	},

	//-------------------------------------------------------------------------

	'updateViewMode': function() {},
	'updateEditMode': function() {},
	'synchronizeComponentValues': function() {},
	
	//-------------------------------------------------------------------------

	'destroy': function() {
		this.element().remove();
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

