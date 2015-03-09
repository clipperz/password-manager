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

Clipperz.PM.Components.RecordDetail.DirectLoginValueComponent = function(anElement, args) {
//MochiKit.Logging.logDebug(">>> new DirectLoginValueComponent");
	args = args || {};

    Clipperz.PM.Components.RecordDetail.DirectLoginValueComponent.superclass.constructor.call(this, anElement, args);

	this._directLoginInputValue = args.directLoginInputValue || null;
	this._value = this.directLoginInputValue().directLogin().formValues()[this.directLoginInputValue().name()];
	
	this.render();
//MochiKit.Logging.logDebug("<<< new DirectLoginValueComponent - record: " + this.record());

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.DirectLoginValueComponent, Clipperz.PM.Components.RecordDetail.AbstractComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.DirectLoginValueComponent component - " + this.directLoginInputValue().name();
	},

	//-------------------------------------------------------------------------

	'directLoginInputValue': function() {
		return this._directLoginInputValue;
	},

	//-------------------------------------------------------------------------

	'render': function() {
//MochiKit.Logging.logDebug(">>> DirectLoginValueComponent.render");
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'td', cls:'directLoginDataLabelTD', children:[
			{tag:'span', html:this.directLoginInputValue().name()}
		]});
//MochiKit.Logging.logDebug("--- DirectLoginValueComponent.render - 1");
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'td', cls:'directLoginDataValueTD', children:[
			{tag:'span', id:this.getId('inputElement')}
		]});
//MochiKit.Logging.logDebug("--- DirectLoginValueComponent.render - 2");
		this.update();
//MochiKit.Logging.logDebug("<<< DirectLoginValueComponent.render");
	},

	//-------------------------------------------------------------------------

	'inputElementConfiguration': function() {
		var result;
		var currentValue;

//MochiKit.Logging.logDebug(">>> DirectLoginValueComponent.inputElementConfiguration - " + this.directLoginInputValue().name());
		result = [];
		currentValue = this.value();
		
		switch (this.directLoginInputValue().type()) {
			case 'checkbox':
				var checkbox;
//{"type":"checkbox", "name":"rememberUsernameChk", "value":"checkbox"}
				checkbox = {tag:'input', id:this.getId('checkbox'), type:'checkbox'}
				if (currentValue == true) {
					checkbox.checked = true;
				}
				result.push(checkbox);
				break;
				
			case 'select':
				var input;
//{"type":"select", "name":"DOMAIN", "options":[{"selected":true, "label":"@tin.it", "value":"tin.it"}, {"selected":false, "label":"@virgilio.it", "value":"virgilio.it"}]}
				input = {tag:'select', id:this.getId('select'), name:this.directLoginInputValue().name(), children:[]};
				input.children.push({tag:'option', value:null, html:"---"});
				MochiKit.Iter.forEach(this.directLoginInputValue().args()['options'], function(anOption) {
					var option;
					
//	TODO: remove the value: field and replace it with element.dom.value = <some value>
					option = {tag:'option', value:anOption['value'], html:anOption['label']}
					if (currentValue == anOption['value']) {
						option.selected = true;
					}
					input.children.push(option);
				})
				result.push(input);
				break;
				
			case 'radio':
				var name;
				var radioBox;
				
//MochiKit.Logging.logDebug("--- DirectLoginValueComponent.inputElementConfiguration - 3");
				name = this.getId(this.directLoginInputValue().name());
				radioBox = {tag:'div', id:this.getId('radioBox'), children:[]};
				result.push(radioBox);
//MochiKit.Logging.logDebug("--- DirectLoginValueComponent.inputElementConfiguration - 3.1 - options.length: " + this.directLoginInputValue().args()['options'].length);
//{"name":"dominio", "type":"radio", "options":[{"value":"@alice.it", "checked":true}, {"value":"@tin.it", "checked":false}, {"value":"@virgilio.it", "checked":false}, {"value":"@tim.it", "checked":false}]}

				MochiKit.Iter.forEach(this.directLoginInputValue().args()['options'], function(anOption) {
					var	radio;
					
//MochiKit.Logging.logDebug("--- DirectLoginValueComponent.inputElementConfiguration - 3.1.1");
//	TODO: remove the value: field and replace it with element.dom.value = <some value>
					radio = {tag:'input', type:'radio', name:name, value:anOption['value']};
//MochiKit.Logging.logDebug("--- DirectLoginValueComponent.inputElementConfiguration - 3.1.2");
					if (currentValue == anOption['value']) {
//MochiKit.Logging.logDebug("--- DirectLoginValueComponent.inputElementConfiguration - 3.1.3");
						radio.checked = true;
//MochiKit.Logging.logDebug("--- DirectLoginValueComponent.inputElementConfiguration - 3.1.4");
					}
//MochiKit.Logging.logDebug("--- DirectLoginValueComponent.inputElementConfiguration - 3.1.5");
					radioBox.children.push({tag:'div', children:[ radio, {tag:'span', html:anOption['value']} ]})
//MochiKit.Logging.logDebug("--- DirectLoginValueComponent.inputElementConfiguration - 3.1.6");
				})
//MochiKit.Logging.logDebug("--- DirectLoginValueComponent.inputElementConfiguration - 3.2");
				break;
		}
//MochiKit.Logging.logDebug("<<< DirectLoginValueComponent.inputElementConfiguration");

		return result;
	},

	//-------------------------------------------------------------------------

	'inputValue': function() {
		var result;
		
		switch (this.directLoginInputValue().type()) {
			case 'checkbox':
				result = this.getDom('checkbox').checked;
				break;
			case 'select':
				result = this.getDom('select').value;
				break;
			case 'radio':
				var checkedRadioButtons;
				
				checkedRadioButtons = MochiKit.Base.filter(	function(aRadioButton) { return aRadioButton.dom.checked },
															this.getElement('radioBox').getChildrenByTagName('input'));
				
				if (checkedRadioButtons.length == 0) {
					result = null;
				} else {
					result = checkedRadioButtons[0].dom.value;
				}
				break;
		}
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'value': function() {
		return this._value;
	},
	
	'setValue': function(aValue) {
		this._value = aValue;
	},

	//-------------------------------------------------------------------------
	
	'updateEditMode': function() {
//MochiKit.Logging.logDebug(">>> DirectLoginValueComponent.updateEditMode - " + this);
		this.getElement('inputElement').update("");
//MochiKit.Logging.logDebug("--- DirectLoginValueComponent.updateEditMode - 1");
		Clipperz.YUI.DomHelper.append(this.getDom('inputElement'), {tag:'div', children:this.inputElementConfiguration()});
//MochiKit.Logging.logDebug("<<< DirectLoginValueComponent.updateEditMode");
	},
	
	//-------------------------------------------------------------------------

	'updateViewMode': function() {
//MochiKit.Logging.logDebug(">>> DirectLoginValueComponent.updateViewMode");
//		this.getElement('inputElement').update(this.directLoginInputValue().value());

		this.getElement('inputElement').update("");
		
		switch (this.directLoginInputValue().type()) {
			case 'checkbox':
				if (this.value() == true) {
					this.getElement('inputElement').update(Clipperz.PM.Strings['directLoginConfigurationCheckBoxFieldSelectedValue']);
				} else {
					this.getElement('inputElement').update(Clipperz.PM.Strings['directLoginConfigurationCheckBoxFieldNotSelectedValue'])
				}
				break;
			case 'select':
				var displayedValue;
				var selectedOptions;
				var currentValue;
				
				currentValue = this.value();
				selectedOptions = MochiKit.Base.filter(	function(anOption) { return (anOption['value'] == currentValue); },
														this.directLoginInputValue().args()['options']);
				if (selectedOptions.length == 0) {
					displayedValue = "---";
				} else {
//MochiKit.Logging.logDebug("+++ " + Clipperz.Base.serializeJSON(selectedOptions));
//MochiKit.Logging.logDebug("*** " + Clipperz.Base.serializeJSON(selectedOptions[0]));
					displayedValue = selectedOptions[0]['label'];
				}
				this.getElement('inputElement').update(displayedValue);
				break;
			case 'radio':
				this.getElement('inputElement').update(this.value());
				break;
		}
//MochiKit.Logging.logDebug("<<< DirectLoginValueComponent.updateViewMode");
	},

	//-------------------------------------------------------------------------

	'synchronizeComponentValues': function() {
//MochiKit.Logging.logDebug(">>> DirectLoginValueComponent.synchronizeComponentValues");
//MochiKit.Logging.logDebug("--- DirectLoginValueComponent.synchronizeComponentValues - 1; value: " + this.inputValue());
		this.setValue(this.inputValue());
		this.directLoginInputValue().directLogin().formValues()[this.directLoginInputValue().name()] = this.value();
//MochiKit.Logging.logDebug("<<< DirectLoginValueComponent.synchronizeComponentValues");
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

