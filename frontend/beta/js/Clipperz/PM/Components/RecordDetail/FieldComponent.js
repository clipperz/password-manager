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

Clipperz.PM.Components.RecordDetail.FieldComponent = function(anElement, args) {
//MochiKit.Logging.logDebug(">>> new FieldComponent");
	args = args || {};

    Clipperz.PM.Components.RecordDetail.FieldComponent.superclass.constructor.call(this, anElement, args);

	this._element = anElement;
	this._recordField = args.recordField || null;

	this._buttonComponent = null;
	this._labelComponent = null;
	this._dragHandler = null;
	this._valueComponent = null;
	this._typeComponent = null;

	this.mainComponent().addEditComponent(this);

	this.render();
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.FieldComponent, Clipperz.PM.Components.RecordDetail.AbstractComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.FieldComponent component";
	},

	//-------------------------------------------------------------------------

	'recordField': function() {
		return this._recordField;
	},
	
	//-------------------------------------------------------------------------

	'buttonComponent': function() {
		return this._buttonComponent;
	},
	
	'setButtonComponent': function(aValue) {
		this._buttonComponent = aValue;
	},

	//-------------------------------------------------------------------------

	'labelComponent': function() {
		return this._labelComponent;
	},
	
	'setLabelComponent': function(aValue) {
		this._labelComponent = aValue;
	},

	//-------------------------------------------------------------------------

	'dragHandler': function() {
		return this._dragHandler;
	},
	
	'setDragHandler': function(aValue) {
		this._dragHandler = aValue;
	},

	//-------------------------------------------------------------------------

	'valueComponent': function() {
		return this._valueComponent;
	},
	
	'setValueComponent': function(aValue) {
		this._valueComponent = aValue;
	},

	//-------------------------------------------------------------------------

	'typeComponent': function() {
		return this._typeComponent;
	},
	
	'setTypeComponent': function(aValue) {
		this._typeComponent = aValue;
	},

	//-------------------------------------------------------------------------

	'render': function() {
//MochiKit.Logging.logDebug(">>> RecordDetail.FieldComponent.render");
 		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'td',/* width:'32',*/ height:'24', cls:'removeFieldButton', align:'left', valign:'top', id:this.getId('button')});
 		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'td',/* width:'25%',*/ valign:'top', id:this.getId('label')});
 		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'td',/* width:'3',*/ valign:'top', id:this.getId('dragHandler')});
 		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'td',/* width:'50%',*/ valign:'top', children:[
			{tag:'div', cls:'Clipperz_recordFieldData', id:this.getId('value')}
		]});
		
		
		this.setButtonComponent(new Clipperz.PM.Components.RecordDetail.FieldButtonComponent(this.getElement('button'), {fieldComponent:this}));
		this.setLabelComponent(new Clipperz.PM.Components.RecordDetail.FieldLabelComponent(this.getElement('label'), {fieldComponent:this}));
		this.setDragHandler(new Clipperz.PM.Components.RecordDetail.FieldDragHandler(this.getElement('dragHandler'), {fieldComponent:this}));
		this.setValueComponent(new Clipperz.PM.Components.RecordDetail.FieldValueComponent(this.getElement('value'), {fieldComponent:this}));

		if (this.editMode() == 'EDIT') {
 			Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'td',/* width:'60',*/ align:'left', cls:'fieldTypeTD', valign:'top', id:this.getId('type')});
			this.setTypeComponent(new Clipperz.PM.Components.RecordDetail.FieldTypeComponent(this.getElement('type'), {fieldComponent:this}));
		}
		
		this.update();
//MochiKit.Logging.logDebug("<<< RecordDetail.FieldComponent.render");
	},

	//-------------------------------------------------------------------------

	'handleButtonClick': function() {
		this.mainComponent().record().removeField(this.recordField());

//		if (this.recordField() == null) {
//			this.mainComponent().record().addNewField();
//		} else {
//			this.mainComponent().record().removeField(this.recordField());
//		}
	},
	
	//-------------------------------------------------------------------------

	'update': function(anEvent) {
//MochiKit.Logging.logDebug(">>> RecordDetail.FieldComponent.update");
		this.buttonComponent().update();
		this.labelComponent().update();
		this.dragHandler().update();
		this.valueComponent().update();
		if (this.editMode() == 'EDIT') {
			this.typeComponent().update();
		}
//MochiKit.Logging.logDebug("<<< RecordDetail.FieldComponent.update");
	},

	//-------------------------------------------------------------------------

	'synchronizeComponentValues': function() {
//MochiKit.Logging.logDebug(">>> FieldComponent.synchronizeComponentValues");
		this.labelComponent().synchronizeComponentValues();
		this.valueComponent().synchronizeComponentValues();
		if (this.editMode() == 'EDIT') {
			this.typeComponent().synchronizeComponentValues();
		}
//MochiKit.Logging.logDebug("<<< FieldComponent.synchronizeComponentValues");
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

