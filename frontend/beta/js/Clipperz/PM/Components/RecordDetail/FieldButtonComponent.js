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

Clipperz.PM.Components.RecordDetail.FieldButtonComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.RecordDetail.FieldButtonComponent.superclass.constructor.call(this, anElement, args);

	this._button = null;

	this.render();
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.FieldButtonComponent, Clipperz.PM.Components.RecordDetail.AbstractFieldSubComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.FieldButtonComponent";
	},

	//-------------------------------------------------------------------------

	'buttonText': function() {
		var	result;

		if (this.recordField() == null) {
			//	TODO: this is never used. It is just an obsolete legacy chunk of code
			result = Clipperz.PM.Strings['recordDetailAddFieldButtonLabel'];
		} else {
			result = Clipperz.PM.Strings['recordDetailRemoveFieldButtonLabel'];
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'button': function() {
		return this._button;
	},
	
	'setButton': function(aValue) {
		this._button = aValue;
	},
	
	//-------------------------------------------------------------------------

	'render': function() {
		this.element().update("");
		
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', id:this.getId('button')})
		this.setButton(new YAHOO.ext.Button(this.getDom('button'), {text:this.buttonText(), handler:this.handleButtonClick, scope:this}));

		this.update();
	},

	//-------------------------------------------------------------------------

	'handleButtonClick': function() {
		if (this.recordField() == null) {
			this.mainComponent().addNewField();
		} else {
			this.mainComponent().removeField(this.fieldComponent());
		}
	},
	
	//-------------------------------------------------------------------------

	'updateEditMode': function() {
		this.button().show();
	},

	//-------------------------------------------------------------------------
	
	'updateViewMode': function() {
		this.button().hide();
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

