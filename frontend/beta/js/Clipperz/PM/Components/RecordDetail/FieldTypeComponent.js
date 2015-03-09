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

Clipperz.PM.Components.RecordDetail.FieldTypeComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.RecordDetail.FieldTypeComponent.superclass.constructor.call(this, anElement, args);

	this._inputElement = null;
	
	this.render();
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.FieldTypeComponent, Clipperz.PM.Components.RecordDetail.AbstractFieldSubComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.FieldTypeComponent component";
	},

	//-------------------------------------------------------------------------

	'inputElement': function() {
		return this._inputElement;
	},
	
	'setInputElement': function(aValue) {
		this._inputElement = aValue;
	},
	
	//-------------------------------------------------------------------------

	'value': function() {
		return this.recordField().type();
	},

	'canChangeType': function() {
		var value;
		var result;
		
		value = this.value();
		result = ((value == 'TXT') || (value == 'PWD') || (value == 'URL') || (value == 'DATE') || (value == 'ADDR'));
		
		return result
	},
	
	//-------------------------------------------------------------------------

	'updateViewMode': function() {
		this.element().update("");
		if (this.canChangeType()) {
			var	width;
			var element;

			width = this.element().getWidth(true);
			element = Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', html:this.recordField().typeShortDescription()}, true);
			element.setWidth(width-1);
		}
	},

	//-------------------------------------------------------------------------
	
	'updateEditMode': function() {
		this.element().update("");
		
		if (this.canChangeType()) {
			var	width;

			width = this.element().getWidth(true);
			this.setInputElement(Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'select', children:[
				{tag:'option', value:'TXT', htmlString:Clipperz.PM.Strings['recordFieldTypologies']['TXT']['shortDescription']},
				{tag:'option', value:'PWD', htmlString:Clipperz.PM.Strings['recordFieldTypologies']['PWD']['shortDescription']},
				{tag:'option', value:'URL', htmlString:Clipperz.PM.Strings['recordFieldTypologies']['URL']['shortDescription']},
				{tag:'option', value:'DATE', htmlString:Clipperz.PM.Strings['recordFieldTypologies']['DATE']['shortDescription']},
				{tag:'option', value:'ADDR', htmlString:Clipperz.PM.Strings['recordFieldTypologies']['ADDR']['shortDescription']}
			
//				{tag:'option', value:'CHECK', html:Clipperz.PM.DataModel.RecordField.TypeDescriptions['CHECK']['shortDescription']},
//				{tag:'option', value:'RADIO', html:Clipperz.PM.DataModel.RecordField.TypeDescriptions['RADIO']['shortDescription']},
//				{tag:'option', value:'CHECK', html:Clipperz.PM.DataModel.RecordField.TypeDescriptions['SELECT']['shortDescription']}
//				{tag:'option', value:'NOTE', html:Clipperz.PM.DataModel.RecordField.TypeDescriptions['NOTE']['shortDescription']}
			]}, true));
			this.inputElement().setWidth(width-1);
			this.inputElement().addHandler('change', true, this.onChange, this, true);
//			this.selectCorrectOption();
			Clipperz.DOM.selectOptionMatchingValue(this.inputElement().dom, this.value());
		}
	},

	//-------------------------------------------------------------------------

	'onChange': function() {
		this.synchronizeComponentValues();
		this.fieldComponent().valueComponent().handleTypeChange();
	},

	//-------------------------------------------------------------------------
/*
	'selectCorrectOption': function() {
		var	options;
		var i,c;
		
		options = this.inputElement().getChildrenByTagName('option');
		c = options.length;
		for (i=0; i<c; i++) {
			if (options[i].dom.value == this.value()) {
				options[i].dom.selected = true;
			}
		}
	},
*/	
	//-------------------------------------------------------------------------

	'synchronizeComponentValues': function() {
		if (this.inputElement() != null) {
			this.recordField().setType(this.inputElement().dom.value);
		}
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

