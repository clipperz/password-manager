/*

Copyright 2008-2018 Clipperz Srl

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

Clipperz.Base.module('Clipperz.PM.DataModel');

Clipperz.PM.DataModel.DirectLogin = function(args) {
	args = args || {};

	Clipperz.PM.DataModel.DirectLogin.superclass.constructor.apply(this, arguments);

	this._reference 				=	args.reference
										||	Clipperz.PM.Crypto.randomKey();
	this._record					=	args.record
										||	Clipperz.Base.exception.raise('MandatoryParameter');

	this._retrieveIndexDataFunction	= 	args.retrieveIndexDataFunction
										||	this.record().retrieveDirectLoginIndexDataFunction()
										||	Clipperz.Base.exception.raise('MandatoryParameter');
	this._setIndexDataFunction		= 	args.setIndexDataFunction
										||	this.record().setDirectLoginIndexDataFunction()
										||	Clipperz.Base.exception.raise('MandatoryParameter');
	this._removeIndexDataFunction	=	args.removeIndexDataFunction
										||	this.record().removeDirectLoginIndexDataFunction()
										||	Clipperz.Base.exception.raise('MandatoryParameter');

	this._inputs			= null;
	this._bindings			= null;
	this._formValues		= null;
	
//	this._inputsDeferredLock			= new MochiKit.Async.DeferredLock();
//	this._bindingsDeferredLock			= new MochiKit.Async.DeferredLock();
//	this._formValuesDeferredLock		= new MochiKit.Async.DeferredLock();

	this._transientState = null;

	this._isBrandNew = MochiKit.Base.isUndefinedOrNull(args.reference);
	
	this.record().bindDirectLogin(this);

	return this;
}

Clipperz.Base.extend(Clipperz.PM.DataModel.DirectLogin, Object, {

	'toString': function() {
		return "DirectLogin (" + this.reference() + ")";
	},

	//=========================================================================

	'reference': function () {
		return this._reference;
	},

	//-------------------------------------------------------------------------

	'record': function () {
		return this._record;
	},

	//=========================================================================

	'isBrandNew': function () {
		return this._isBrandNew;
	},

	//=========================================================================

	'removeIndexDataFunction': function () {
		return this._removeIndexDataFunction;
	},

	'remove': function () {
		return Clipperz.Async.callbacks("DirectLogin.remove", [
			MochiKit.Base.partial(this.removeIndexDataFunction(), this.reference()),
			MochiKit.Base.method(this.record(), 'removeDirectLogin', this)
		], {trace:false});
	},

	//=========================================================================
/*	
	'inputsDeferredLock': function () {
		return this._inputsDeferredLock;
	},
	
	'bindingsDeferredLock': function () {
		return this._bindingsDeferredLock;
	},

	'formValuesDeferredLock': function () {
		return this._formValuesDeferredLock;
	},
*/
	//=========================================================================

	'label': function () {
		return this.getIndexDataForKey('label');
	},

	'setLabelKeepingBackwardCompatibilityWithBeta': function (aValue) {
		return Clipperz.Async.callbacks("DirectLogin.setLabelKeepingBackwardCompatibilityWithBeta", [
			MochiKit.Base.method(this, 'setIndexDataForKey', 'label', aValue),
			MochiKit.Base.method(this, 'setValue', 'label', aValue),
			MochiKit.Base.partial(MochiKit.Async.succeed, this),
		], {trace:false});
	},

	'setLabel': function (aValue) {
		return this.setLabelKeepingBackwardCompatibilityWithBeta(aValue);
//		return this.setIndexDataForKey('label', aValue);
	},

	//=========================================================================

	'favicon': function () {
		return this.getIndexDataForKey('favicon');
	},

	'setFavicon': function (aValue) {
		return this.setIndexDataForKey('favicon', aValue);
	},

	'faviconUrlWithBookmarkletConfiguration': function (aBookmarkletConfiguration) {
		var	result;

		if (! MochiKit.Base.isUndefinedOrNull(aBookmarkletConfiguration['page']['favicon'])) {
			result = aBookmarkletConfiguration['page']['favicon'];
		} else if (! MochiKit.Base.isUndefinedOrNull(aBookmarkletConfiguration['form']['attributes']['action'])) {
			var actionUrl;
			var hostname;

			actionUrl = aBookmarkletConfiguration['form']['attributes']['action'];
			hostname = actionUrl.replace(/^https?:\/\/([^\/]*)\/.*/, '$1');
			result = "http://" + hostname + "/favicon.ico";
		} else {
			result = null;
		}
		
		
		return result;
	},

	//-------------------------------------------------------------------------
/*
	'faviconData': function () {
		var regexp = new RegExp('^data\:\/\/.*', 'i');
		
		return Clipperz.Async.callbacks("DirectLogin.favicon", [
			MochiKit.Base.method(this, 'getIndexDataForKey', 'favicon'),
			MochiKit.Base.method(regexp, 'test'),
			Clipperz.Async.deferredIf("is data URL", [
				MochiKit.Base.method(this, 'getIndexDataForKey', 'favicon')
			], [
				MochiKit.Base.method(this, 'transientState'),
				MochiKit.Base.itemgetter('faviconData'),
				Clipperz.Async.deferredIf('has a chaced value for the favicon data', [
					MochiKit.Base.operator.identity
				], [
					MochiKit.Base.method(this, 'getIndexDataForKey', 'favicon'),
					MochiKit.Base.method(this, 'loadFaviconDataFromURL')
				])

			])
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'loadFaviconDataFromURL': function (anURL) {
		var deferredResult;
		var image;
		
		deferredResult = new Clipperz.Async.Deferred("DirectLogin.loadFaviconDataFromURL", {trace:false});
		deferredResult.addCallback(function (anEvent) {
			var image = anEvent.src();
		    var canvas = document.createElement("canvas");
			var result;

		    canvas.width = image.width;
		    canvas.height = image.height;

		    var ctx = canvas.getContext("2d");
		    ctx.drawImage(image, 0, 0);

			result = canvas.toDataURL(/*"image/png"* /);

			return result;
		});
		deferredResult.addErrback(MochiKit.Async.succeed, Clipperz.PM.Strings.getValue('defaultFaviconUrl'));
		deferredResult.addBoth(MochiKit.Base.bind(function (aDataUrl) {
			this.transientState()['faviconData'] = aDataUrl;
			
			return aDataUrl;
		}, this));

		image = new Image();
		MochiKit.Signal.connect(image, 'onload',  MochiKit.Base.method(deferredResult, 'callback'));
		MochiKit.Signal.connect(image, 'onerror', MochiKit.Base.method(deferredResult, 'errback'));
		MochiKit.Signal.connect(image, 'onabort', MochiKit.Base.method(deferredResult, 'errback'));

		image.src = anURL;
		
		return deferredResult;
	},
*/

	//=========================================================================

	'type': function () {
		return this.getValue('formData.attributes.type')
	},

	//=========================================================================

	'serializedData': function () {
		return Clipperz.Async.collectResults("DirectLogin.serializedData", {
			'favicon': MochiKit.Base.method(this,'favicon'),
			'label': MochiKit.Base.method(this,'label'),
			'bookmarkletVersion': MochiKit.Base.method(this, 'getValue', 'bookmarkletVersion'),
			'formData': MochiKit.Base.method(this, 'getValue', 'formData'),
			'formValues': MochiKit.Base.method(this, 'getValue', 'formValues'),
			'bindingData': [
				MochiKit.Base.method(this, 'bindings'),
				function (someBindings) {
					var result;
					var bindingKey;

					result = {}
					for (bindingKey in someBindings) {
						result[bindingKey] = someBindings[bindingKey].serializedData();
					}
					
					return result;
				}
			]
		}, {trace:false})()
	},

	//=========================================================================
/*
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

//	TODO: shoud remove the 'formValues' call, as it is now deferred
//				if ((aRadio['checked'] == true) && (this.formValues()[aRadio['name']] == null)) {
//					this.formValues()[aRadio['name']] = aRadio['value'];
//				}
			}, this))
			
			updatedInputs = MochiKit.Base.concat(updatedInputs, MochiKit.Base.values(updatedRadios));
		}
		
		delete result.inputs;
		result.inputs = updatedInputs;

		return result;
	},

	'_fixConfiguration': function (aConfiguration) {
		var fixedConfiguration;
//		var inputs;
//		var bindings;
//		var i,c;

		fixedConfiguration = Clipperz.Base.deepClone(aConfiguration);

//Clipperz.log("PROCESS CONFIGURATION", aConfiguration);
		switch (aConfiguration['bookmarkletVersion']) {
			case '0.1':
				fixedConfiguration['formData'] = this.fixFormDataFromBookmarkletVersion_0_1(aConfiguration['formData']);
				break;
			case '0.2':
				fixedConfiguration['formData'] = aConfiguration['formData'];
				break;
		}

/ *		
		aConfiguration['_inputs'] = [];
		c = formData['inputs'].length;
		for (i=0; i<c; i++) {
			aConfiguration['_inputs'].push(new Clipperz.PM.DataModel.DirectLoginInput(formData['inputs'][i]));
		}
* /
/ *
		aConfiguration['_bindings'] = {};
		if (aConfiguration['legacyBindingData'] == null) {
			if (aConfiguration['bindingData'] != null) {
				var bindingKey;

				for (bindingKey in aConfiguration['bindingData']) {
					var newBinding;
					
					newBinding = new Clipperz.PM.DataModel.DirectLoginBinding(bindingKey, {fieldKey:aConfiguration['bindingData'][bindingKey]});
					aConfiguration['_bindings'][newBinding.key()] = newBinding;
				}
			} else {
				var editableFields;
	
				editableFields = MochiKit.Base.filter(function(aField) {
					var result;
					var type;

					type = aField['type'].toLowerCase();
					result = ((type != 'hidden') && (type != 'submit') && (type != 'checkbox') && (type != 'radio') && (type != 'select'));

					return result;
				}, aConfiguration['_inputs']);
	
				MochiKit.Iter.forEach(editableFields, MochiKit.Base.bind(function(anEditableField) {
					var newBinding;
					
					newBinding = new Clipperz.PM.DataModel.DirectLoginBinding(anEditableField['name']);
					aConfiguration['_bindings'][newBinding.key()] = newBinding;
				}, this));
			}
			
		} else {
			var bindingKey;

			for (bindingKey in aConfiguration['legacyBindingData']) {
				var newBinding;
			
				newBinding = new Clipperz.PM.DataModel.DirectLoginBinding(bindingKey, {fieldName:aConfiguration['legacyBindingData'][bindingKey]});
				aConfiguration['_bindings'][newBinding.key()] = newBinding;
			}
		}
* /

		return fixedConfiguration;
	},

	//-------------------------------------------------------------------------

	'getObjectDataStore': function () {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("DirectLogin.getObjectDataStore", {trace:false});
		deferredResult.acquireLock(this.objectDataStoreDeferredLock());
		deferredResult.addCallback(MochiKit.Base.bind(function () {
			var innerDeferredResult;

			if (this._objectDataStore == null) {
				this._objectDataStore = new Clipperz.KeyValueObjectStore();

				innerDeferredResult = new Clipperz.Async.Deferred("DirectLogin.getObjectDataStore <inner deferred>", {trace:false});
//				innerDeferredResult.addMethod(this.record(), 'getValue', 'directLogins' + '.' + this.reference());
				innerDeferredResult.addMethod(this, 'getValue', ''),
				innerDeferredResult.addMethod(this, 'setOriginalState');
				innerDeferredResult.addMethod(this, '_fixConfiguration');
				innerDeferredResult.addMethod(this._objectDataStore, 'initWithValues');
//				innerDeferredResult.addMethod(this._objectDataStore, 'setValues');
				innerDeferredResult.callback();
			} else {
				innerDeferredResult = MochiKit.Async.succeed(this._objectDataStore);
			}

			return innerDeferredResult;
		}, this));
		deferredResult.releaseLock(this.objectDataStoreDeferredLock());
		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------
	
	'hasInitiatedObjectDataStore': function () {
		return (this._objectDataStore != null);
	},

	//-------------------------------------------------------------------------

	'resetObjectDataStore': function () {
		this._objectDataStore.removeAllData();
		this._objectDataStore = null;
	},
*/
	//=========================================================================

	'bookmarkletConfiguration': function () {
		return Clipperz.Async.callbacks("DirectLogin.bookmarkletConfiguration", [
			Clipperz.Async.collectResults("DirectLogin.bookmarkletConfiguration <inner results>", {
				'label': MochiKit.Base.method(this, 'label'),
				'configuration': MochiKit.Base.method(this, 'getValue', '')
			}, {trace:false}),
			function (someValues) {
				var result;
				
				if (someValues['configuration'] != null) {
					var	configuration;

					configuration = {
						'page': {
							'title': someValues['label']
						//	'favicon'
						//	'url'	
						},
						'form':		someValues['configuration']['formData'],
						'version':	someValues['configuration']['bookmarkletVersion']
					}

					result = Clipperz.Base.formatJSON(configuration);
				} else {
					result = '';
				}
				
				return result;
			}
		], {trace:false});

	},

	//-------------------------------------------------------------------------

	'setBookmarkletConfiguration': function (aValue) {
		var bookmarkletConfiguration;

		bookmarkletConfiguration = Clipperz.PM.DataModel.DirectLogin.checkBookmarkletConfiguration(aValue);

		return Clipperz.Async.callbacks("DirectLogin.setBookmarkletConfiguration", [
			MochiKit.Base.method(this, 'setValue', 'formData', bookmarkletConfiguration['form']),
			MochiKit.Base.method(this, 'setValue', 'bookmarkletVersion', bookmarkletConfiguration['version']),

			MochiKit.Base.method(this, 'favicon'),
			Clipperz.Async.deferredIf("the favicon is not set", [
			], [
				MochiKit.Base.method(this, 'faviconUrlWithBookmarkletConfiguration', bookmarkletConfiguration),
				MochiKit.Base.method(this, 'setFavicon')
			]),
			
			MochiKit.Base.method(this, 'updateInputsAfterChangingBookmarkletConfiguration'),
			MochiKit.Base.method(this, 'updateFormValuesAfterChangingBookmarkletConfiguration'),
			MochiKit.Base.method(this, 'updateBindingsAfterChangingBookmarkletConfiguration'),
			
//			MochiKit.Base.noop
			MochiKit.Base.partial(MochiKit.Async.succeed, this),
		], {trace:false});
	},

	//=========================================================================

	'formAttributes': function () {
		return this.getValue('formData.attributes');
	},

	//=========================================================================

	'inputs': function () {
		return Clipperz.Async.callbacks("DirectLogin.inputs", [
			Clipperz.Async.deferredIf("this._inputs is defined", [
			], [
				MochiKit.Base.method(this, 'updateInputsAfterChangingBookmarkletConfiguration')
			])
		], {trace:false}, this._inputs);
	},

	'setInputWithFormDataConfiguration': function (aFormDataConfiguration) {
		this._inputs = {};

		if (aFormDataConfiguration != null) {
			MochiKit.Iter.forEach(aFormDataConfiguration['inputs'], MochiKit.Base.bind(function (anInputData) {
				var newInput;
			
				newInput = new Clipperz.PM.DataModel.DirectLoginInput(anInputData);
				this._inputs[newInput.name()] = newInput;
			}, this));
		}
		
		return this._inputs;
	},

	'updateInputsAfterChangingBookmarkletConfiguration': function () {
		return Clipperz.Async.callbacks("DirectLogin.updateInputsAfterChangingBookmarkletConfiguration", [
			MochiKit.Base.method(this, 'getValue', 'formData'),
			MochiKit.Base.method(this, 'setInputWithFormDataConfiguration')
		], {trace:false});
	},

	//=========================================================================

	'inputValues': function () {
		return Clipperz.Async.callbacks("DirectLogin.inputValues", [
			MochiKit.Base.method(this, 'inputs'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.partial(MochiKit.Base.method(this, 'inputValue'))),
			Clipperz.Async.collectAll,
			Clipperz.Base.mergeItems
		], {trace:false});
	},

	'inputValue': function (anInput) {
		var	deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("DirectLogin.inputValue", {trace:false});

		if (anInput.needsFormValue()) {
			deferredResult.addMethod(this, 'formValues');
			deferredResult.addCallback(MochiKit.Base.itemgetter(anInput.name()));
			deferredResult.addMethodcaller('value');
		} else if (anInput.needsBinding()) {
			deferredResult.addMethod(this, 'bindings');
			deferredResult.addCallback(MochiKit.Base.itemgetter(anInput.name()));
			deferredResult.addMethodcaller('field');
			deferredResult.addMethodcaller('value');
		} else {
			deferredResult.addCallback(MochiKit.Async.succeed, anInput.value());
		}
		deferredResult.addCallback(function (anActualValue) {
			return [anInput.name(), anActualValue];
		});

		deferredResult.callback();
		
		return deferredResult;
	},

	//=========================================================================

	'bindings': function () {
		return Clipperz.Async.callbacks("DirectLogin.bindings", [
			Clipperz.Async.deferredIf("this._bindings is defined", [
			], [
				MochiKit.Base.method(this, 'updateBindingsAfterChangingBookmarkletConfiguration'),
				MochiKit.Base.bind(function () { return this._bindings;}, this)
			])
		], {trace:false}, this._bindings);
	},

	'bindFormFieldWithLabelToRecordFieldWithLabel': function (aFormFieldLabel, aRecordFieldLabel) {
//console.log("DirectLogin.bindFormFieldWithLabelToCardFieldWithLabel", aFormFieldLabel, aRecordFieldLabel);
		return Clipperz.Async.callbacks("DirectLogin.bindFormFieldWithLabelToCardFieldWithLabel", [
			Clipperz.Async.collectResults("DirectLogin.bindFormFieldWithLabelToCardFieldWithLabel - collect results", {
				'binding': [
					MochiKit.Base.method(this, 'bindings'),
					MochiKit.Base.itemgetter(aFormFieldLabel)
				],
				'field': [
					MochiKit.Base.method(this.record(), 'fieldWithLabel', aRecordFieldLabel)
				]
			}),
			function (someValues) {
				if (typeof(someValues['binding']) != 'undefined') {
					someValues['binding'].setField(someValues['field']);
				}
			}
		], {trace:false});
	},

	'setBindings': function (someBindings, originalFields) {
		var	self = this;

		return Clipperz.Async.callbacks("DirectLogin.setBindings", [
			function () {
				return MochiKit.Base.map(function (aBindingInfo) {
					var	result;
					
					try {
						result = self.bindFormFieldWithLabelToRecordFieldWithLabel(aBindingInfo[0], originalFields[aBindingInfo[1]]['label']);
					} catch (exception) {
						result = MochiKit.Async.succeed();
					}
					
					return result;
				}, MochiKit.Base.zip(MochiKit.Base.keys(someBindings), MochiKit.Base.values(someBindings)));
			},
			Clipperz.Async.collectAll,
			MochiKit.Base.partial(MochiKit.Async.succeed, this),
		], {trace:false});
	},

	//-------------------------------------------------------------------------
/*
	'bindingValues': function () {
		return Clipperz.Async.callbacks("DirectLogin.bindingValues", [
			Clipperz.Async.collectResults("DirectLogin.bindingValues [collectResults]", {
				'fieldValues': [
					MochiKit.Base.method(this, 'record'),
					MochiKit.Base.methodcaller('getFieldsValues')
				],
				'bindings': MochiKit.Base.method(this, 'bindings')
			}, {trace:false}),
			function (someData) {
				var result;
				var	bindingKey;
				
				result = {};
				for (bindingKey in someData['bindings']) {
					result[bindingKey] = someData['fieldValues'][someData['bindings'][bindingKey].fieldKey()]['value'];
				}

				return result;
			}
		], {trace:false});
	},
*/
	//-------------------------------------------------------------------------

	'updateBindingsAfterChangingBookmarkletConfiguration': function () {
		return Clipperz.Async.callbacks("DirectLogin.updateBindingsAfterChangingBookmarkletConfiguration", [
			Clipperz.Async.collectResults("DirectLogin.updateBindingsAfterChangingBookmarkletConfiguration<collect results>", {
				'currentValues':	MochiKit.Base.method(this, 'getValue', ''),
				'originalValues':	MochiKit.Base.method(this, 'originalConfiguration'),
				'inputs':			MochiKit.Base.method(this, 'inputs')
			}, {trace:false}),
			MochiKit.Base.bind(function (someValues) {
				var availableBindingValues;
				var inputRequiringBindingValues;
				var newBindingValues;

				if (MochiKit.Base.isUndefinedOrNull(someValues['originalValues']) || MochiKit.Base.isUndefinedOrNull(someValues['originalValues']['bindingData'])) {
					availableBindingValues = {};
				} else {
					availableBindingValues = Clipperz.Base.deepClone(someValues['originalValues']['bindingData'])
				}
				
				if (someValues['currentValues'] != null) {
					MochiKit.Base.update(availableBindingValues, someValues['currentValues']['bindingData']);
				}

				this._bindings = {};
				newBindingValues = {}
				MochiKit.Iter.forEach(MochiKit.Base.filter(MochiKit.Base.methodcaller('needsBinding'), MochiKit.Base.values(someValues['inputs'])), MochiKit.Base.bind(function (anInput) {
					var	newBinding;
					
					newBindingValues[anInput.name()] = availableBindingValues[anInput.name()];
					newBinding = new Clipperz.PM.DataModel.DirectLoginBinding(this, {
						'key':		anInput.name(),
						'field':	availableBindingValues[anInput.name()]
					});
					
					this._bindings[anInput.name()] = newBinding;
				}, this))

				return newBindingValues;

/*
				this._bindings = {};

				if (someValues['currentValues'] != null) {
					if (someValues['currentValues']['bindingData'] != null) {
						var bindingKey;

						for (bindingKey in someValues['currentValues']['bindingData']) {
							var newBinding;

							newBinding = new Clipperz.PM.DataModel.DirectLoginBinding(this, {
								'key':		bindingKey,
								'field':	someValues['currentValues']['bindingData'][bindingKey]
							});
							this._bindings[newBinding.key()] = newBinding;
						}
					} else  if (someValues['currentValues']['legacyBindingData'] == null) {
						var bindingKey;

						for (bindingKey in someValues['currentValues']['legacyBindingData']) {
							var newBinding;
		
							newBinding = new Clipperz.PM.DataModel.DirectLoginBinding(this, {
								'key':		bindingKey, 
								'field':	someValues['currentValues']['legacyBindingData'][bindingKey]
							});
							this._bindings[newBinding.key()] = newBinding;
						}
					} else {
						WTF = TODO;
					}
				}

				return this._bindings;
*/
			}, this),
			MochiKit.Base.method(this, 'setValue', 'bindingData')
		], {trace:false});
	},

	//=========================================================================

	'formValues': function () {
		return Clipperz.Async.callbacks("DirectLogin.formValues", [
			Clipperz.Async.deferredIf("this._formValues is defined", [
			], [
				MochiKit.Base.method(this, 'updateFormValuesAfterChangingBookmarkletConfiguration'),
				MochiKit.Base.bind(function () { return this._formValues;}, this)
			])
		], {trace:false}, this._formValues);
	},

	//-------------------------------------------------------------------------

	'updateFormValuesAfterChangingBookmarkletConfiguration': function () {
		return Clipperz.Async.callbacks("DirectLogin.updateFormValuesAfterChangingBookmarkletConfiguration", [
			Clipperz.Async.collectResults("DirectLogin.updateFormValuesAfterChangingBookmarkletConfiguration <collect results>", {
				'currentValues':	MochiKit.Base.method(this, 'getValue', ''),
				'originalValues':	MochiKit.Base.method(this, 'originalConfiguration'),
				'inputs':			MochiKit.Base.method(this, 'inputs')
			}, {trace:false}),
			MochiKit.Base.bind(function (someValues) {
				var availableFormValues;
				var inputRequiringFormValues;
				var newFormValues;

				if (MochiKit.Base.isUndefinedOrNull(someValues['originalValues']) || MochiKit.Base.isUndefinedOrNull(someValues['originalValues']['formValues'])) {
					availableFormValues = {};
				} else {
					availableFormValues = Clipperz.Base.deepClone(someValues['originalValues']['formValues'])
				}
				
				MochiKit.Base.update(availableFormValues, someValues['currentValues']['formValues']);

				this._formValues = {};
				newFormValues = {};
				MochiKit.Iter.forEach(MochiKit.Base.filter(MochiKit.Base.methodcaller('needsFormValue'), MochiKit.Base.values(someValues['inputs'])), MochiKit.Base.bind(function (anInput) {
					var	newFormValue;
					var fieldOptions;

					fieldOptions = {
						'type':		anInput.type(),
						'options':	anInput.options()
					};

					newFormValues[anInput.name()] = availableFormValues[anInput.name()]
					newFormValue = new Clipperz.PM.DataModel.DirectLoginFormValue(this, {
						'key':			anInput.name(),
						'fieldOptions':	fieldOptions,
						'value':		availableFormValues[anInput.name()]
					});
					
					this._formValues[anInput.name()] = newFormValue;
				}, this))
				
				return newFormValues;
			}, this),
			MochiKit.Base.method(this, 'setValue', 'formValues')
		], {trace:false});
	},

	//=========================================================================

	'retrieveIndexDataFunction': function () {
		return this._retrieveIndexDataFunction;
	},
	
	'getIndexDataForKey': function (aKey) {
		return Clipperz.Async.callbacks("DirectLogin.getIndexDataForKey", [
			MochiKit.Base.partial(this.retrieveIndexDataFunction(), this.reference()),
			Clipperz.Async.deferredIf("DirectLogin.getIndexDataForKey - index data not null", [
				MochiKit.Base.itemgetter(aKey)
			],[
				MochiKit.Async.succeed
			])
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'setIndexDataForKey': function (aKey, aValue) {
		return Clipperz.Async.callbacks("DirectLogin.setValueForKey", [
			MochiKit.Base.method(this, 'getIndexDataForKey', aKey),
			MochiKit.Base.bind(function (anActualValue) {
				var transientStateKey;
				
				transientStateKey = 'original_' + aKey;
				if (MochiKit.Base.isUndefinedOrNull(this.transientState()[transientStateKey])) {
					if (anActualValue != aValue) {
						this.transientState()[transientStateKey] = anActualValue;
					}
				} else if (this.transientState()[transientStateKey] == aValue) {
					this.transientState()[transientStateKey] = null;
				}
			}, this),
			MochiKit.Base.partial(this._setIndexDataFunction, this.reference(), aKey, aValue)
		], {trace:false})
	},

	//-------------------------------------------------------------------------
/*
	'setValueForKey': function (aKey, aValue) {
		return Clipperz.Async.callbacks("DirectLogin.setValueForKey", [
			MochiKit.Base.method(this, 'getIndexDataForKey', aKey),
			MochiKit.Base.bind(function (anActualValue) {
				var transientStateKey;
				
				transientStateKey = 'original_' + aKey;
				if (MochiKit.Base.isUndefinedOrNull(this.transientState()[transientStateKey])) {
					if (anActualValue != aValue) {
						this.transientState()[transientStateKey] = anActualValue;
					}
				} else if (this.transientState()[transientStateKey] == aValue) {
					this.transientState()[transientStateKey] = null;
				}
			}, this),
			MochiKit.Base.method(this, 'setIndexDataForKey', aKey, aValue)
		], {trace:false})
	},
*/
	//=========================================================================
/*
	'storedConfiguration': function () {
		return this.record().getValue('directLogins' + '.' + this.reference());
	},

//	'setStoredConfiguration': function (aValue) {
//		return this.record().setValue('directLogins' + '.' + this.reference(), aValue);
//	},
*/
	//=========================================================================

	'hasPendingChanges': function () {
		var	result;
		var deferredResult;
		
		result = false;
		result = result || this.isBrandNew();
		result = result || (! MochiKit.Base.isUndefinedOrNull(this.transientState()['original_label']));
		result = result || (! MochiKit.Base.isUndefinedOrNull(this.transientState()['original_favicon']));

		if ((result == false) && (this.originalConfiguration() != null)) {
			deferredResult = Clipperz.Async.callbacks("DirectLogin.hasPendingChanges", [
 				MochiKit.Base.method(this, 'serializedData'),
				MochiKit.Base.bind(function (aCurrentConfiguration) {
					var	originalConfiguration;
					var currentConfiguration;
					var result;

					originalConfiguration = this.originalConfiguration();
					currentConfiguration = aCurrentConfiguration;

					result = false;
					result = result || (MochiKit.Base.compare(originalConfiguration['bookmarkletVersion'],	currentConfiguration['bookmarkletVersion'])	!= 0);
					result = result || (MochiKit.Base.compare(originalConfiguration['formData'],			currentConfiguration['formData'])			!= 0);
					result = result || (MochiKit.Base.compare(originalConfiguration['formValues'],			currentConfiguration['formValues'])			!= 0);
					result = result || (MochiKit.Base.compare(originalConfiguration['bindingData'],			currentConfiguration['bindingData'])		!= 0);

					return result;
				}, this)
			], {trace:false});
		} else {
			deferredResult = MochiKit.Async.succeed(result);
		}

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'revertChanges': function () {
		var deferredResult;
		
		if (this.transientState()['original_label'] != null) {
			this.setLabel(this.transientState()['original_label']);
		}

		if (this.transientState()['original_favicon'] != null) {
			this.setFavicon(this.transientState()['original_favicon']);
		}

		if (this.originalConfiguration() != null) {
			deferredResult = this.setValue('', this.originalConfiguration());
		} else {
			deferredResult = MochiKit.Async.succeed();
		}

		this._inputs		= null;
		this._bindings		= null;
		this._formValues	= null;

		this.resetTransientState(false);

/*
		if (this.hasInitiatedObjectDataStore()) {
			deferredResult = Clipperz.Async.callbacks("DirectLogin.revertChanges", [
//				MochiKit.Base.method(this.record(), 'setValue', 'directLogins' + '.' + this.reference(), this.originalState()),
				MochiKit.Base.method(this, 'setValue', '', this.originalState()),
				MochiKit.Base.method(this, 'resetObjectDataStore')
			], {trace:false})
		} else {
			deferredResult = MochiKit.Async.succeed();
		}
*/
		return deferredResult;
	},


	//=========================================================================

	'transientState': function () {
		if (this._transientState == null) {
			this._transientState = {}
		}
		
		return this._transientState;
	},

	'resetTransientState': function (isCommitting) {
		this._transientState = null;
	},

	'commitTransientState': function (isCommitting) {
		this._transientState = null;
		this._isBrandNew = false;
	},

	//-------------------------------------------------------------------------
	
	'originalConfiguration': function () {
		return this.transientState()['original_configuration'];
	},

	'setOriginalConfiguration': function (aConfiguration) {
		this.transientState()['original_configuration'] = Clipperz.Base.deepClone(aConfiguration);
	},

	//=========================================================================

	'actualKey': function (aValueKey) {
		var actualKey;

		actualKey = 'directLogins' + '.' + this.reference();
		if (aValueKey != '') {
			actualKey = actualKey + '.' + aValueKey;
		}

		return actualKey;
	},

	//-------------------------------------------------------------------------

	'getValue': function (aValueKey) {
		return this.record().getValue(this.actualKey(aValueKey));
	},
	
	'setValue': function (aValueKey, aValue) {
//		return this.record().setValue(this.actualKey(aValueKey), aValue);
		
		return Clipperz.Async.callbacks("DirectLogin.setValue", [
			MochiKit.Base.method(this, 'getValue', ''),
			MochiKit.Base.bind(function (aValue) {
				if (this.originalConfiguration() == null) {
					this.setOriginalConfiguration(aValue);
				}
			}, this),
//			MochiKit.Base.method(this, 'originalConfiguration'),
//			Clipperz.Async.deferredIf("originalConfiguration has been set", [
//			], [
//				MochiKit.Base.method(this, 'getValue', ''),
//				MochiKit.Base.method(this, 'setOriginalConfiguration')
//			]),
			MochiKit.Base.method(this.record(), 'setValue', this.actualKey(aValueKey), aValue)
		], {trace:false});
	},
	
	'removeValue': function (aValueKey) {
//		return this.record().removeValue(this.actualKey(aValueKey));

		return Clipperz.Async.callbacks("DirectLogin.removeValue", [
			MochiKit.Base.method(this, 'originalConfiguration'),
			Clipperz.Async.deferredIf("originalConfiguration has been set", [
			], [
				MochiKit.Base.method(this, 'getValue', ''),
				MochiKit.Base.method(this, 'setOriginalConfiguration')
			]),
			MochiKit.Base.method(this.record(), 'removeValue', this.actualKey(aValueKey))
		], {trace:false});
	},

	//=========================================================================

	'content': function () {
//		return this.serializedData();
//		return MochiKit.Async.succeed(this);

		var deferredResult;
		var	fieldValues;

		fieldValues = {};
		deferredResult = new Clipperz.Async.Deferred("DirectLogin.content", {trace:false});
		deferredResult.addMethod(this, 'reference');
		deferredResult.addCallback(function (aValue) { fieldValues['reference'] = aValue; });
		deferredResult.addMethod(this, 'label');
		deferredResult.addCallback(function (aValue) { fieldValues['label'] = aValue; });
		deferredResult.addMethod(this, 'favicon');
		deferredResult.addCallback(function (aValue) { fieldValues['favicon'] = aValue; });
		deferredResult.addCallback(function () { return fieldValues; });
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'deleteAllCleanTextData': function () {
		this._inputs = null;
		this._bindings = null;
		this._formValues = null;

		this.resetTransientState();
	},

	//-------------------------------------------------------------------------
	
	'hasAnyCleanTextData': function () {
		var result;
		
		result = false;
		
		result = result || (this._inputs != null);
		result = result || (this._bindings != null);
		result = result || (this._formValues != null);
		result = result || (MochiKit.Base.keys(this.transientState()).length != 0);

		return MochiKit.Async.succeed(result);
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});

//#############################################################################

Clipperz.PM.DataModel.DirectLogin.exception = {
	'WrongBookmarkletConfiguration': new MochiKit.Base.NamedError("Clipperz.PM.DataModel.DirectLogin.exception.WrongBookmarkletConfiguration")
};

Clipperz.PM.DataModel.DirectLogin.checkBookmarkletConfiguration = function(aConfiguration) {
	var configuration;
	
	try {
		configuration = Clipperz.Base.evalJSON(aConfiguration);
//		configuration = Clipperz.PM.BookmarkletProcessor.sanitizeBookmarkletConfiguration(configuration);
		
		if (MochiKit.Base.isUndefinedOrNull(configuration['page']['title'])
		||	MochiKit.Base.isUndefinedOrNull(configuration['form']['attributes']['action'])
//		||	MochiKit.Base.isUndefinedOrNull(configuration['form']['attributes']['method'])
		||	MochiKit.Base.isUndefinedOrNull(configuration['form']['inputs'])
		||	MochiKit.Base.isUndefinedOrNull(configuration['version'])
		) {
			throw Clipperz.PM.DataModel.DirectLogin.exception.WrongBookmarkletConfiguration;
		}

//		if (MochiKit.Base.isUndefinedOrNull(configuration['favicon'])) {
//			throw Clipperz.PM.DataModel.DirectLogin.exception.WrongBookmarkletConfiguration;
//		}

	} catch (exception) {
		throw exception;
	}
	
	return configuration;
};
