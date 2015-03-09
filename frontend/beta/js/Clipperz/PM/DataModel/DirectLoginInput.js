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

Clipperz.PM.DataModel.DirectLoginInput = function(aDirectLogin, args) {
	args = args || {};

//console.log(">>> new DirectLoginInput - args: %o" + args);
	this._directLogin = aDirectLogin;
	this._args = args;
	
	return this;
}

Clipperz.PM.DataModel.DirectLoginInput.prototype = MochiKit.Base.update(null, {

	'directLogin': function() {
		return this._directLogin;
	},

	//-------------------------------------------------------------------------

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

	'value': function() {
		return this.args()['value'];
	},
	
	//-------------------------------------------------------------------------

	'formConfiguration': function() {
		var result;
		
//MochiKit.Logging.logDebug(">>> DirectLoginInput.formConfiguration - " + this.name());
		if (this.shouldSetValue()) {
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 1");
			switch (this.type()) {
				case 'select':
					var currentValue;
					var options;
					
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 2");
					currentValue = this.directLogin().formValues()[this.name()];
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 2.1");
					options = this.args()['options'];
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 2.2");
					
					result = MochiKit.DOM.SELECT({name:this.name()},
						MochiKit.Base.map(function(anOption) {
							var options;
							
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 2.3");
//	TODO: remove the value: field and replace it with element.dom.value = <some value>
							options = {value:anOption['value']};
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 2.4");
							if (currentValue == anOption['value']) {
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 2.5");
								options.selected = true;
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 2.6");
							}
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 2.7");
							
							return MochiKit.DOM.OPTION(options, anOption['label'])
						}, options)
					)
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 2.8");
					break;
				case 'checkbox':
					var options;
					
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 3");
					options = {type:'checkbox', name: this.name()};
					if (this.directLogin().formValues()[this.name()] == true) {
						options['checked'] = true;
					};

					result = MochiKit.DOM.INPUT(options, null);
					break;
				case 'radio':
					var currentName;
					var currentValue;
					var options;
					
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4");
					currentName = this.name();
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.1");
					currentValue = this.directLogin().formValues()[this.name()];
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.2");
					options = this.args()['options'];
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.3");
					
					result = MochiKit.DOM.DIV(null,
						MochiKit.Base.map(function(anOption) {
							var options;
							var isChecked;
							var inputNode;
							var divNode;
							
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.4");
//	TODO: remove the value: field and replace it with element.dom.value = <some value>
							options = {type:'radio', name:currentName, value:anOption['value']}
							isChecked = (currentValue == anOption['value']);
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.5");
							if (isChecked) {
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.6");
								options.checked = true;
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.7");
							}
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.8 - options: " + Clipperz.Base.serializeJSON(options));
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.8 - value: " + anOption['value']);

							if (Clipperz_IEisBroken == true) {
								var checkedValue;
								
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.8.1");
								checkedValue = (isChecked ? " CHECKED" : "");
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.8.2");
								inputNode = MochiKit.DOM.currentDocument().createElement("<INPUT TYPE='RADIO' NAME='" + currentName + "' VALUE='" + anOption['value'] + "'" + checkedValue + ">");
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.8.3");
							} else {
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.8.4");
								inputNode = MochiKit.DOM.INPUT(options, anOption['value']);
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.8.5");
							}
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.9");
							divNode = MochiKit.DOM.DIV(null, inputNode);
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.10");
							
							return divNode;
//							return MochiKit.DOM.DIV(null, MochiKit.DOM.INPUT(options, anOption['value']));
						}, options)
					);
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 4.9");
					break;
			}
		} else {
			var binding;
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 5");
			binding = this.directLogin().bindings()[this.name()];

//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 6");
//	TODO: remove the value: field and replace it with element.dom.value = <some value>
			result = MochiKit.DOM.INPUT({
				type:((this.type() != 'password') ? this.type() : 'text'),
//				type:(((this.type() != 'password') && (this.type() != 'submit')) ? this.type() : 'text'),
				name:this.name(),
				value:((binding != null)? binding.field().value() : this.value())
			}, null);
//MochiKit.Logging.logDebug("--- DirectLoginInput.formConfiguration - 7");
		}

//MochiKit.Logging.logDebug("<<< DirectLoginInput.formConfiguration: ");
		return result;
	},
	
	//-------------------------------------------------------------------------
	
	'shouldSetValue': function() {
		var type;
		var result;

//MochiKit.Logging.logDebug(">>> DirectLoginInput.shouldSetValue");
		type = this.type();
		result = ((type == 'checkbox') || (type == 'radio') || (type == 'select'));
//if (result == true) {
//	MochiKit.Logging.logDebug("DIRECT LOGIN INPUT need value: " + Clipperz.Base.serializeJSON(this.args()));
//}
//MochiKit.Logging.logDebug("<<< DirectLoginInput.shouldSetValue");
		return result;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});

