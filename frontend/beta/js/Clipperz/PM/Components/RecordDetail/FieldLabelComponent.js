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

Clipperz.PM.Components.RecordDetail.FieldLabelComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.RecordDetail.FieldLabelComponent.superclass.constructor.call(this, anElement, args);

	this._inputElement = null;
	
	this.render();
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.FieldLabelComponent, Clipperz.PM.Components.RecordDetail.AbstractFieldSubComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.FieldLabelComponent component";
	},

	//-------------------------------------------------------------------------

	'value': function() {
		return this.recordField().label();
	},

	//-------------------------------------------------------------------------

	'inputElement': function() {
		return this._inputElement;
	},
	
	'setInputElement': function(aValue) {
		this._inputElement = aValue;
	},

	//-------------------------------------------------------------------------

	'render': function() {
		var	newTextFormField;
		this.element().update("");
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', cls:'Clipperz_recordFieldLabel', id:this.getId('label')});
//		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', style:'font-size:8pt;', html:this.recordField().key()});

//		this.setInputElement(new Clipperz.PM.Components.TextFormField(this.getElement('label'), {editMode:this.editMode(), value:this.value()}));
		newTextFormField = new Clipperz.PM.Components.TextFormField(this.getElement('label'), {editMode:this.editMode(), value:this.value()});
//		newTextFormField.inputElement().setStyle({border:'3px solid cyan;'});
		newTextFormField.on('change', this.notifyChanges, this, true)
//		this.inputElement().on('change', function() {alert("CHANGE");});
//		this.inputElement().getElement('editComponent_input').on('change', function() {alert("CHANGE");})
//		this.inputElement().on('blur', this.notifyChanges, this, true);
		
		this.setInputElement(newTextFormField);
		this.update();
	},

	'notifyChanges': function() {
//MochiKit.Logging.logDebug(">>> FieldLabelComponent.notifyChanges - " + this);
		this.synchronizeComponentValues();
		Clipperz.NotificationCenter.notify(this.recordField().recordVersion().record(), 'updatedFieldLabel');
//MochiKit.Logging.logDebug("<<< FieldLabelComponent.notifyChanges");
	},
	
	//-------------------------------------------------------------------------

	'update': function() {
//MochiKit.Logging.logDebug(">>> FieldLabelComponent.update");
		this.inputElement().update({editMode:this.editMode(), value:this.value()});
//MochiKit.Logging.logDebug("<<< FieldLabelComponent.update");
	},
	
	//-------------------------------------------------------------------------
/*
	'updateViewMode': function() {
		var	width;
		var	element;
		
		this.element().update("");
		width = this.element().getWidth();
		element = Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', html:this.value()}, true);
		element.setWidth(width-1);
	},

	//-------------------------------------------------------------------------
	
	'updateEditMode': function() {
		var	width;

		this.element().update("");
		width = this.element().getWidth(true);
		this.setInputElement(Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'input', type:'text', value:this.value()}, true));
		this.inputElement().setWidth(width-1);
	},
*/
	//-------------------------------------------------------------------------

	'synchronizeComponentValues': function() {
		if (this.inputElement() != null) {
			this.recordField().setLabel(this.inputElement().value());
		}
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

