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
if (typeof(Clipperz.PM.DataModel) == 'undefined') { Clipperz.PM.DataModel = {}; }

//#############################################################################

Clipperz.PM.DataModel.DirectLoginInput = function(args) {
	this._args = args;
	
	return this;
}

Clipperz.PM.DataModel.DirectLoginInput.prototype = MochiKit.Base.update(null, {

	'args': function() {
		return this._args;
	},
	
	//-------------------------------------------------------------------------

	'name': function() {
		return this.args()['name'];
	},

	//-------------------------------------------------------------------------

	'type': function() {
		var result;
		
		result = this.args()['type'];
		
		if (result != null) {
			result = result.toLowerCase();
		}
		return result;
	},

	//-------------------------------------------------------------------------

	'options': function() {
		return this.args()['options'];
	},

	//-------------------------------------------------------------------------

	'value': function() {
		return this.args()['value'];
	},
	
	//-------------------------------------------------------------------------
/*	
	'formConfiguration': function(someFormValues, someBindings, someFields) {
		var result;

		if (this.shouldSetValue()) {
			switch (this.type()) {
				case 'select':
					var currentValue;
					var options;
					
//					currentValue = this.directLogin()._configuration['formValues'][this.name()];
					currentValue = someFormValues[this.name()];
					options = this.args()['options'];
					
					result = MochiKit.DOM.SELECT({name:this.name()},
						MochiKit.Base.map(function(anOption) {
							var options;
							
							options = {value:anOption['value']};
							if (currentValue == anOption['value']) {
								options.selected = true;
							}
							
							return MochiKit.DOM.OPTION(options, anOption['label'])
						}, options)
					)
					break;
				case 'checkbox':
					var options;
					
					options = {type:'checkbox', name: this.name()};
//					if (this.directLogin()._configuration['formValues'][this.name()] == true) {
					if (someFormValues[this.name()] == true) {
						options['checked'] = true;
					};

					result = MochiKit.DOM.INPUT(options, null);
					break;
				case 'radio':
					var currentName;
					var currentValue;
					var options;
					
					currentName = this.name();
//					currentValue = this.directLogin()._configuration['formValues'][this.name()];
					currentValue = someFormValues[this.name()];
					options = this.args()['options'];
					
					result = MochiKit.DOM.DIV(null,
						MochiKit.Base.map(function(anOption) {
							var options;
							var isChecked;
							var inputNode;
							var divNode;
							
							options = {type:'radio', name:currentName, value:anOption['value']}
							isChecked = (currentValue == anOption['value']);
							if (isChecked) {
								options.checked = true;
							}

							if (Clipperz_IEisBroken == true) {
								var checkedValue;
								
								checkedValue = (isChecked ? " CHECKED" : "");
								inputNode = MochiKit.DOM.currentDocument().createElement("<INPUT TYPE='RADIO' NAME='" + currentName + "' VALUE='" + anOption['value'] + "'" + checkedValue + ">");
							} else {
								inputNode = MochiKit.DOM.INPUT(options, anOption['value']);
							}
							divNode = MochiKit.DOM.DIV(null, inputNode);
							
							return divNode;
						}, options)
					);
					break;
			}
		} else {
			var binding;
//			binding = this.directLogin().bindings()[this.name()];
			binding = someBindings[this.name()];

			result = MochiKit.DOM.INPUT({
				type:((this.type() != 'password') ? this.type() : 'text'),
				name:this.name(),
//				value:((binding != null)? binding.field().value() : this.value())
				value:((binding != null)? someFields[binding.fieldKey()]['value'] : this.value())
//				value:((binding != null)? someFields[binding.fieldKey()].value() : this.value())
			}, null);
		}

		return result;
	},
*/	
	//-------------------------------------------------------------------------
	
	'needsFormValue': function() {
		var type;
		var result;

		type = this.type();
		result = ((type == 'checkbox') || (type == 'radio') || (type == 'select'));

		return result;
	},

	'needsBinding': function() {
		var type;
		var result;

		type = this.type();
		result = ((type == 'text') || (type == 'password'));

		return result;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

