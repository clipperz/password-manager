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

Clipperz.PM.Components.RecordDetail.DirectLoginBindingComponent = function(anElement, args) {
//MochiKit.Logging.logDebug(">>> new DirectLoginBindingComponent");
	args = args || {};

    Clipperz.PM.Components.RecordDetail.DirectLoginBindingComponent.superclass.constructor.call(this, anElement, args);

	this._directLoginBinding = args.directLoginBinding || null;
	this.render();

	Clipperz.NotificationCenter.register(this.record(), 'addNewRecordField',	this, 'syncAndUpdateEditMode');
	Clipperz.NotificationCenter.register(this.record(), 'removedField',		this, 'syncAndUpdateEditMode');
	Clipperz.NotificationCenter.register(this.record(), 'updatedFieldLabel',	this, 'syncAndUpdateEditMode');
//MochiKit.Logging.logDebug("<<< new DirectLoginBindingComponent");

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.DirectLoginBindingComponent, Clipperz.PM.Components.RecordDetail.AbstractComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.DirectLoginBindingComponent component";
	},

	//-------------------------------------------------------------------------

	'directLoginBinding': function() {
		return this._directLoginBinding;
	},

	//-------------------------------------------------------------------------

	'render': function() {
//		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'span', style:'font-weight:bold;', html:this.directLoginBinding().key()})
//		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'span', html:this.directLoginBinding().value()})
//MochiKit.Logging.logDebug(">>> DirectLoginBindingComponent.render");
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'td', cls:'directLoginBindingLabelTD', children:[
			{tag:'span', html:this.directLoginBinding().key()}
		]});
//MochiKit.Logging.logDebug("--- DirectLoginBindingComponent.render - 1");
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'td', cls:'directLoginBindingValueTD', children:[
			{tag:'div', id:this.getId('editModeBox'), children:[
				{tag:'select', id:this.getId('select'), children:this.recordFieldOptions()}
			]},
			{tag:'div', id:this.getId('viewModeBox'), children:[
				{tag:'span', id:this.getId('viewValue'), html:""}
			]}
		]});
//MochiKit.Logging.logDebug("--- DirectLoginBindingComponent.render - 2");
		this.getElement('editModeBox').setVisibilityMode(YAHOO.ext.Element.DISPLAY);
		this.getElement('viewModeBox').setVisibilityMode(YAHOO.ext.Element.DISPLAY);

		this.update();
//MochiKit.Logging.logDebug("<<< DirectLoginBindingComponent.render");
	},

	//-------------------------------------------------------------------------

	'recordFieldOptions': function() {
		var	result;
		var option;
		var	recordFieldKey;
		var	recordFields;

//MochiKit.Logging.logDebug(">>> DirectLoginBindingComponent.recordFieldOptions");
		recordFields = this.directLoginBinding().directLogin().record().currentVersion().fields();
		result = [];
		option = {tag:'option', value:null, html:'---'};
		result.push(option);
		for (recordFieldKey in recordFields) {
//	TODO: remove the value: field and replace it with element.dom.value = <some value>
			option = {tag:'option', value:recordFieldKey, html:Clipperz.Base.sanitizeString(recordFields[recordFieldKey].label())}
			if (recordFieldKey == this.directLoginBinding().fieldKey()) {
				option['selected'] = true;
			}
			result.push(option);
		}
//MochiKit.Logging.logDebug("<<< DirectLoginBindingComponent.recordFieldOptions");
		
		return result;
	},

	//-------------------------------------------------------------------------

	'syncAndUpdateEditMode': function() {
		this.synchronizeComponentValues();
		this.updateEditMode();
	},
	
	'updateEditMode': function() {
		var	selectElementBox;

//MochiKit.Logging.logDebug(">>> DirectLoginBindingComponent.updateEditMode");
		this.getElement('viewModeBox').hide();

		selectElementBox = this.getElement('editModeBox');
		selectElementBox.update("");

		Clipperz.YUI.DomHelper.append(selectElementBox.dom, {tag:'select', id:this.getId('select'), children:this.recordFieldOptions()});

/*
		selectElement = this.getElement('select');
		
		selectElement.update("");
		MochiKit.Iter.forEach(this.recordFieldOptions(), function(anOption) {
			Clipperz.YUI.DomHelper.append(selectElement.dom, anOption);
		});
*/


		this.getElement('editModeBox').show();
//MochiKit.Logging.logDebug("<<< DirectLoginBindingComponent.updateEditMode");
	},
	
	//-------------------------------------------------------------------------

	'updateViewMode': function() {
//MochiKit.Logging.logDebug(">>> DirectLoginBindingComponent.updateViewMode");
		this.getElement('editModeBox').hide();
		this.getElement('viewModeBox').show();

		this.getElement('viewValue').update(Clipperz.Base.sanitizeString(this.directLoginBinding().field().label()));
//MochiKit.Logging.logDebug("<<< DirectLoginBindingComponent.updateViewMode");
	},

	//-------------------------------------------------------------------------

	'synchronizeComponentValues': function() {
//MochiKit.Logging.logDebug(">>> DirectLoginBindingComponent.synchronizeComponentValues")
//MochiKit.Logging.logDebug("--- DirectLoginBindingComponent.synchronizeComponentValues - 1 - " + this.getId('select'));
		this.directLoginBinding().setFieldKey(this.getDom('select').value);
//MochiKit.Logging.logDebug("<<< DirectLoginBindingComponent.synchronizeComponentValues");
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

