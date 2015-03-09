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

Clipperz.PM.DataModel.DirectLogin = function(args) {
//MochiKit.Logging.logDebug(">>> new Clipperz.PM.DataModel.DirectLogin");
//console.log(">>> new Clipperz.PM.DataModel.DirectLogin - args: %o", args);
//console.log("--- formData: %s", Clipperz.Base.serializeJSON(args.formData));
	args = args || {};

//MochiKit.Logging.logDebug("--- new Clipperz.PM.DataModel.DirectLogin - args: " + Clipperz.Base.serializeJSON(MochiKit.Base.keys(args)));
	this._record = args.record || null;
	this._label = args.label || "unnamed record"
	this._reference = args.reference || Clipperz.PM.Crypto.randomKey();
	this._favicon = Clipperz.Base.sanitizeFavicon(args.favicon) || null;
	this._bookmarkletVersion = args.bookmarkletVersion || "0.1";

	this._directLoginInputs = null;
	
	this._formValues = args.formValues || {};
	this.setFormData(args.formData || null);
//console.log("=== formData: %o", this.formData());

	if (args.legacyBindingData == null) {
		this.setBindingData(args.bindingData || null);
	} else {
		this.setLegacyBindingData(args.legacyBindingData);
	}

	this._fixedFavicon = null;
	
//	this._formValues = args.formValues || (this.hasValuesToSet() ? {} : null);
//MochiKit.Logging.logDebug("<<< new Clipperz.PM.DataModel.DirectLogin");
	
	return this;
}

Clipperz.PM.DataModel.DirectLogin.prototype = MochiKit.Base.update(null, {

	'remove': function() {
		this.record().removeDirectLogin(this);
	},
	
	//-------------------------------------------------------------------------

	'record': function() {
		return this._record;
	},

	//-------------------------------------------------------------------------

	'user': function() {
		return this.record().user();
	},
	
	//-------------------------------------------------------------------------

	'reference': function() {
		return this._reference;
	},

	//-------------------------------------------------------------------------

	'label': function() {
		return this._label;
	},

	'setLabel': function(aValue) {
		this._label = aValue;
	},
	
	//-------------------------------------------------------------------------

	'favicon': function() {
		if (this._favicon == null) {
			var	actionUrl;
			var hostname;
			
			actionUrl = this.action();
			hostname = actionUrl.replace(/^https?:\/\/([^\/]*)\/.*/, '$1');
			this._favicon = Clipperz.Base.sanitizeFavicon("http://" + hostname + "/favicon.ico");
		}

		return this._favicon;
	},

	//-------------------------------------------------------------------------

	'fixedFavicon': function() {
		var result;
		
		if (this._fixedFavicon == null) {
			result = this.favicon();

			if (Clipperz_IEisBroken) {
				if (this.user().preferences().disableUnsecureFaviconLoadingForIE()) {
					if (result.indexOf('https://') != 0) {
						result = Clipperz.PM.Strings['defaultFaviconUrl_IE'];
						this.setFixedFavicon(result);
					}
				}
			}
		} else {
			result = this._fixedFavicon;
		}
		
		return result;
	},

	'setFixedFavicon': function(aValue) {
		this._fixedFavicon = aValue;
	},
	
	'action': function () {
		var	result;
		
		result = Clipperz.Base.sanitizeUrl(this.formData()['attributes']['action']);
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'bookmarkletVersion': function() {
		return this._bookmarkletVersion;
	},
	
	'setBookmarkletVersion': function(aValue) {
		this._bookmarkletVersion = aValue;
	},
	
	//-------------------------------------------------------------------------

	'formData': function() {
		return this._formData;
	},

	'setFormData': function(aValue) {
		var formData;
		
//MochiKit.Logging.logDebug(">>> DirectLogin.setFormData - " + Clipperz.Base.serializeJSON(aValue));
		switch (this.bookmarkletVersion()) {
			case "0.2":
				formData = aValue;
				break;
			case "0.1":
//MochiKit.Logging.logDebug("--- DirectLogin.setFormData - fixing form data from bookmarklet version 0.1");
				formData = this.fixFormDataFromBookmarkletVersion_0_1(aValue);
				break;
		}

		this._formData = aValue;
		this.setBookmarkletVersion("0.2");

//MochiKit.Logging.logDebug("--- DirectLogin.setFormData - formData: " + Clipperz.Base.serializeJSON(formData));
		if (formData != null) {
			var i,c;
			
			this._directLoginInputs = [];
			c = formData['inputs'].length;
			for (i=0; i<c; i++) {
				var directLoginInput;
				
				directLoginInput = new Clipperz.PM.DataModel.DirectLoginInput(this, formData['inputs'][i]);
				this._directLoginInputs.push(directLoginInput);
			}
		}
//MochiKit.Logging.logDebug("<<< DirectLogin.setFormData");
	},

	'fixFormDataFromBookmarkletVersion_0_1': function(aValue) {
//{"type":"radio", "name":"action", "value":"new-user", "checked":false }, { "type":"radio", "name":"action", "value":"sign-in", "checked":true }
//			||
//		   \  /
//			\/
//{"name":"dominio", "type":"radio", "options":[{"value":"@alice.it", "checked":true}, {"value":"@tin.it", "checked":false}, {"value":"@virgilio.it", "checked":false}, {"value":"@tim.it", "checked":false}]}
		var result;
		var inputs;
		var updatedInputs;
		var radios;

//MochiKit.Logging.logDebug(">>> DirectLogin.fixFormDataFromBookmarkletVersion_0_1");
		result = aValue;
		inputs = aValue['inputs'];
		
		updatedInputs = MochiKit.Base.filter(function(anInput) {
			var	result;
			var type;

			type = anInput['type'] || 'text';
			result =  type.toLowerCase() != 'radio';
			
			return result;
		}, inputs);
		radios = MochiKit.Base.filter(function(anInput) {
			var	result;
			var type;

			type = anInput['type'] || 'text';
			result =  type.toLowerCase() == 'radio';
			
			return result;
		}, inputs);
		
		if (radios.length > 0) {
			var updatedRadios;
			
			updatedRadios = {};
			MochiKit.Iter.forEach(radios, MochiKit.Base.bind(function(aRadio) {
				var	radioConfiguration;
				
				radioConfiguration = updatedRadios[aRadio['name']];
				if (radioConfiguration == null) {
					radioConfiguration = {type:'radio', name:aRadio['name'], options:[]};
					updatedRadios[aRadio['name']] = radioConfiguration;
				}
				
//	TODO: remove the value: field and replace it with element.dom.value = <some value>
				radioConfiguration.options.push({value:aRadio['value'], checked:aRadio['checked']});

				if ((aRadio['checked'] == true) && (this.formValues()[aRadio['name']] == null)) {
//MochiKit.Logging.logDebug("+++ setting value '" + aRadio['value'] + "' for key: '" + aRadio['name'] + "'");
					this.formValues()[aRadio['name']] = aRadio['value'];
				}
			}, this))
			
			updatedInputs = MochiKit.Base.concat(updatedInputs, MochiKit.Base.values(updatedRadios));
		}
		
		delete result.inputs;
		result.inputs = updatedInputs;
//MochiKit.Logging.logDebug("<<< DirectLogin.fixFormDataFromBookmarkletVersion_0_1");

		return result;
	},
	
	//.........................................................................

	'directLoginInputs': function() {
		return this._directLoginInputs;
	},

	//-------------------------------------------------------------------------

	'formValues': function() {
		return this._formValues;
	},
	
	'hasValuesToSet': function() {
		var result;
		
//MochiKit.Logging.logDebug(">>> DirectLogin.hasValuesToSet");
		if (this.directLoginInputs() != null) {
			result = MochiKit.Iter.some(this.directLoginInputs(), MochiKit.Base.methodcaller('shouldSetValue'));
		} else {
			result = false;
		}
//MochiKit.Logging.logDebug("<<< DirectLogin.hasValuesToSet");
		
		return result;
	},
	
//	'additionalValues': function() {
	'inputsRequiringAdditionalValues': function() {
		var	result;
		var inputs;
		
//MochiKit.Logging.logDebug(">>> DirectLogin.additionalValues");
		result = {};
		if (this.directLoginInputs() != null) {
			inputs = MochiKit.Base.filter(MochiKit.Base.methodcaller('shouldSetValue'), this.directLoginInputs());
			MochiKit.Iter.forEach(inputs, function(anInput) {
				result[anInput.name()] = anInput;
			})
		}
//MochiKit.Logging.logDebug("<<< DirectLogin.additionalValues");

		return result;
	},
	
	//-------------------------------------------------------------------------

	'bindingData': function() {
		return this._bindingData;
	},

	'setBindingData': function(aValue) {
//MochiKit.Logging.logDebug(">>> DirectLogin.setBindingData");
		if (aValue != null) {
			var bindingKey;

			this._bindingData = aValue;
			this._bindings = {};
		
			for (bindingKey in aValue) {
				var directLoginBinding;
			
				directLoginBinding = new Clipperz.PM.DataModel.DirectLoginBinding(this, bindingKey, {fieldKey:aValue[bindingKey]});
				this._bindings[bindingKey] = directLoginBinding;
			}
		} else {
			var editableFields;
			var bindings;
			
			bindings = {};
			
			editableFields = MochiKit.Base.filter(function(aField) {
				var result;
				var type;
		
				type = aField['type'].toLowerCase();
				result = ((type != 'hidden') && (type != 'submit') && (type != 'checkbox') && (type != 'radio') && (type != 'select'));
		
				return result;
			}, this.formData().inputs);
			
			MochiKit.Iter.forEach(editableFields, function(anEditableField) {
				bindings[anEditableField['name']] = new Clipperz.PM.DataModel.DirectLoginBinding(this, anEditableField['name']);
			}, this);
			
			this._bindings = bindings;
		}
//MochiKit.Logging.logDebug("<<< DirectLogin.setBindingData");
	},
	
	'setLegacyBindingData': function(aValue) {
//MochiKit.Logging.logDebug(">>> DirectLogin.setLegacyBindingData");
		var bindingKey;
	
		this._bindingData = aValue;
		this._bindings = {};
	
		for (bindingKey in aValue) {
			var directLoginBinding;
		
			directLoginBinding = new Clipperz.PM.DataModel.DirectLoginBinding(this, bindingKey, {fieldName:aValue[bindingKey]});
			this._bindings[bindingKey] = directLoginBinding;
		}
//MochiKit.Logging.logDebug("<<< DirectLogin.setLegacyBindingData");
	},
	
	//.........................................................................
	
	'bindings': function() {
		return this._bindings;
	},

	//-------------------------------------------------------------------------

	'serializedData': function() {
		var result;
		var	bindingKey;
		
		result = {};
//		result.reference = this.reference();
		result.label = this.label();
		result.favicon = this.favicon() || "";
		result.bookmarkletVersion = this.bookmarkletVersion();
		result.formData = this.formData();
		if (this.hasValuesToSet) {
			result.formValues = this.formValues();
		}
		result.bindingData = {};

		for (bindingKey in this.bindings()) {
			result.bindingData[bindingKey] = this.bindings()[bindingKey].serializedData();
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'handleMissingFaviconImage': function(anEvent) {
		anEvent.stop();
		MochiKit.Signal.disconnectAll(anEvent.src());
		this.setFixedFavicon(Clipperz.PM.Strings['defaultFaviconUrl']);
		anEvent.src().src = this.fixedFavicon();
	},
	
	//=========================================================================
	
	'runHttpAuthDirectLogin': function(aWindow) {
		MochiKit.DOM.withWindow(aWindow, MochiKit.Base.bind(function() {
			var completeUrl;
			var url;
			
			url = this.bindings()['url'].field().value();

			if (/^https?\:\/\//.test(url) == false) {
				url = 'http://' + url;
			}
			
			if (Clipperz_IEisBroken === true) {
				completeUrl = url;
			} else {
				var username;
				var password;

				username = this.bindings()['username'].field().value();
				password = this.bindings()['password'].field().value();

				/(^https?\:\/\/)?(.*)/.test(url);

				completeUrl = RegExp.$1 + username + ':' + password + '@' + RegExp.$2;
			}
			
			MochiKit.DOM.currentWindow().location.href = completeUrl;
		}, this));
	},
	
	//-------------------------------------------------------------------------

	'runSubmitFormDirectLogin': function(aWindow) {
		MochiKit.DOM.withWindow(aWindow, MochiKit.Base.bind(function() {
			var formElement;
			var	formSubmitFunction;
			var submitButtons;
			
//MochiKit.Logging.logDebug("### runDirectLogin - 3");
//			MochiKit.DOM.currentDocument().write('<html><head><title>' + this.label() + '</title><META http-equiv="Content-Type" content="text/html; charset=utf-8" /></head><body></body></html>')
//MochiKit.Logging.logDebug("### runDirectLogin - 3.1");
			MochiKit.DOM.appendChildNodes(MochiKit.DOM.currentDocument().body, MochiKit.DOM.H3(null, "Loading " + this.label() + " ..."));
//MochiKit.Logging.logDebug("### runDirectLogin - 4");
//console.log(this.formData()['attributes']);
			formElement = MochiKit.DOM.FORM(MochiKit.Base.update({id:'directLoginForm'}, {	'method':this.formData()['attributes']['method'],
																							'action': this.action()}));
//MochiKit.Logging.logDebug("### runDirectLogin - 5");
			formSubmitFunction = MochiKit.Base.method(formElement, 'submit');
//MochiKit.Logging.logDebug("### runDirectLogin - 6");

			MochiKit.DOM.appendChildNodes(MochiKit.DOM.currentDocument().body,
				MochiKit.DOM.DIV({style:'display:none; visibility:hidden;'}, formElement)
			);
//MochiKit.Logging.logDebug("### runDirectLogin - 7");
			MochiKit.DOM.appendChildNodes(formElement, MochiKit.Base.map(	MochiKit.Base.methodcaller("formConfiguration"),
																			this.directLoginInputs()));
//MochiKit.Logging.logDebug("### runDirectLogin - 8");

			submitButtons = MochiKit.Base.filter(function(anInputElement) {
//MochiKit.Logging.logDebug("### runDirectLogin - 8.1 - " + anInputElement);
//MochiKit.Logging.logDebug("### runDirectLogin - 8.2 - " + anInputElement.tagName);
//MochiKit.Logging.logDebug("### runDirectLogin - 8.3 - " + anInputElement.getAttribute('type'));
				return ((anInputElement.tagName.toLowerCase() == 'input') && (anInputElement.getAttribute('type').toLowerCase() == 'submit'));
			}, formElement.elements)
//MochiKit.Logging.logDebug("### runDirectLogin - 9");

			if (submitButtons.length == 0) {
//MochiKit.Logging.logDebug("### OLD submit")
				if (Clipperz_IEisBroken == true) {
//MochiKit.Logging.logDebug("### runDirectLogin - 10");
					formElement.submit();
				} else {
//MochiKit.Logging.logDebug("### runDirectLogin - 11");
					formSubmitFunction();
				}
			} else {
//MochiKit.Logging.logDebug("### NEW submit")
				submitButtons[0].click();
			}

		}, this));
	},
	
	//-------------------------------------------------------------------------

	'runDirectLogin': function(aNewWindow) {
		var	newWindow;

//console.log("formData.attributes", this.formData()['attributes']);
//		if (/^javascript/.test(this.formData()['attributes']['action'])) {
		if ((/^(https?|webdav|ftp)\:/.test(this.action()) == false) &&
			(this.formData()['attributes']['type'] != 'http_auth')
		) {
			var messageBoxConfiguration;

			if (typeof(aNewWindow) != 'undefined') {
				aNewWindow.close();
			}

			messageBoxConfiguration = {};
			messageBoxConfiguration.title = Clipperz.PM.Strings['VulnerabilityWarning_Panel_title'];
			messageBoxConfiguration.msg = Clipperz.PM.Strings['VulnerabilityWarning_Panel_message'];
			messageBoxConfiguration.animEl = YAHOO.ext.Element.get("mainDiv");
			messageBoxConfiguration.progress = false;
			messageBoxConfiguration.closable = false;
			messageBoxConfiguration.buttons = {'cancel': Clipperz.PM.Strings['VulnerabilityWarning_Panel_buttonLabel']};

			Clipperz.YUI.MessageBox.show(messageBoxConfiguration);

			throw Clipperz.Base.exception.VulnerabilityIssue;
		}

//MochiKit.Logging.logDebug("### runDirectLogin - 1 : " + Clipperz.Base.serializeJSON(this.serializedData()));
		if (typeof(aNewWindow) == 'undefined') {
			newWindow = window.open(Clipperz.PM.Strings['directLoginJumpPageUrl'], "");
		} else {
			newWindow = aNewWindow;
		}
//MochiKit.Logging.logDebug("### runDirectLogin - 2");

		if (this.formData()['attributes']['type'] == 'http_auth') {
			this.runHttpAuthDirectLogin(newWindow);
		} else {
			this.runSubmitFormDirectLogin(newWindow)
		}
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});

