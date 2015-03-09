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

Clipperz.Base.module('Clipperz.PM.UI.Web.Controllers');

Clipperz.PM.UI.Web.Controllers.DirectLoginWizardController = function(args) {
	this._directLoginEditingComponent	= args.directLoginEditingComponent	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._cardLabel 					= args.cardLabel					|| Clipperz.Base.exception.raise('MandatoryParameter');

	MochiKit.Signal.connect(this._directLoginEditingComponent, 'changedValue',	this, 'handleChangedValue');
	MochiKit.Signal.connect(this._directLoginEditingComponent, 'moveForward',	this, 'handleMoveForward');
	MochiKit.Signal.connect(this._directLoginEditingComponent, 'keyPressed',	this, 'handleDirectLoginEditingComponentKeyPressed');

	this._directLogin = null;
	this._directLoginHasJustBeenAdded = false;
	
	this._rulerComponent = null;

	this._steps = null;
	this._currentStepIndex = 0;
	this._isNextEnabled = false;

	this._recordFields = null;
	this._originalBindings = null;

	this._bindingComponents = [];
	this._formValueComponents = [];

	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.Web.Controllers.DirectLoginWizardController.prototype, {

	'toString': function() {
		return "Clipperz.PM.UI.Web.Controllers.DirectLoginWizardController";
	},

	//-------------------------------------------------------------------------

	'directLogin': function () {
		return this._directLogin;
	},

	//-------------------------------------------------------------------------

	'directLoginHasJustBeenAdded': function () {
		return this._directLoginHasJustBeenAdded;
	},

	'setDirectLoginHasJustBeenAdded': function (aValue) {
		this._directLoginHasJustBeenAdded = aValue;
	},

	//-------------------------------------------------------------------------

	'directLoginEditingComponent': function () {
		return this._directLoginEditingComponent;
	},

	//=============================================================================

	'cardLabel': function () {
		return this._cardLabel;
	},

	//=============================================================================

	'resetCurrentStepIndex': function () {
		this._currentStepIndex = 0;
		this.rulerComponent().resetStatus();
	},

	//-----------------------------------------------------------------------------

	'enableNext': function (aValue) {
		this.rulerComponent().enableNext(aValue);
		this._isNextEnabled = aValue;
	},
	
	'isNextEnabled': function () {
		return this._isNextEnabled;
	},
	
	//-----------------------------------------------------------------------------

	'enablePrevious': function (aValue) {
		this.rulerComponent().enablePrevious(aValue);
	},
	
	//=============================================================================

	'bindingComponents': function () {
		return this._bindingComponents;
	},
	
	'resetBindingComponents': function () {
		this.directLoginEditingComponent().clearAllBindingsComponents();
		this._bindingComponents = [];
	},

	//=============================================================================

	'formValueComponents': function () {
		return this._formValueComponents;
	},

	'resetFormValueComponents': function () {
		this.directLoginEditingComponent().clearAllFormValueComponents();
		this._formValueComponents = [];
	},

	//=============================================================================

	'recordFields': function () {
		return this._recordFields;
	},
	
	'setRecordFields': function (aValue) {
		this._recordFields = aValue;
	},

	'recordFieldWithReference': function (aReference) {
		var matchingValues;
		var result;

		matchingValues = MochiKit.Base.filter(function (aField) { return aField['reference'] == aReference; }, this.recordFields());

		if (matchingValues.length == 0) {
			result = null;
		} else {
			result = matchingValues[0];
		}
		
		return result;
	},

	//-----------------------------------------------------------------------------

	'originalBindings': function () {
		return this._originalBindings;
	},
	
	'setOriginalBindings': function (aValue) {
		this._originalBindings = aValue;
	},

	//=============================================================================

	'rulerComponent': function () {
		if (this._rulerComponent == null) {
			this._rulerComponent = new Clipperz.PM.UI.Web.Components.RulerComponent({
				translationContext:'Wizards.DirectLoginWizard'
			});
			this._rulerComponent.render();
			
			MochiKit.Signal.connect(this._rulerComponent, 'exit',			this, 'handleExit');
			MochiKit.Signal.connect(this._rulerComponent, 'done',			this, 'done');
			MochiKit.Signal.connect(this._rulerComponent, 'moveForward',	this, 'handleMoveForward');
			MochiKit.Signal.connect(this._rulerComponent, 'moveBackward',	this, 'handleMoveBackward');
			MochiKit.Signal.connect(this._rulerComponent, 'cursorMoved',	this, 'handleCursorMoved');
		}
		
		return this._rulerComponent;
	},

	//-----------------------------------------------------------------------------

	'showRuler': function (someSteps) {
		var rulerElement;

		this.setSteps(someSteps);

		rulerElement = this.rulerComponent().element();
		this.directLoginEditingComponent().disableAllPanels();

		MochiKit.Style.showElement(rulerElement);
		MochiKit.Style.setElementPosition(rulerElement, {x:-1000, y:this.directLoginEditingComponent().bottomMargin()});
		new MochiKit.Visual.Move(rulerElement, {
			x:0, y:this.directLoginEditingComponent().bottomMargin(),
			mode:'absolute',
			duration:1,
			afterFinish:MochiKit.Base.method(this, 'handleCursorMoved')
		});
	},

	'fixRulerRendering': function (aValue) {
		this.rulerComponent().setDisplayMode(aValue);
	},

	//-----------------------------------------------------------------------------

	'hideRuler': function () {
		new MochiKit.Visual.Move(this.rulerComponent().element(), {x:-1000, mode:'relative', duration:1});
	},

	'doneWithRuler': function () {
		var rulerComponentElement;
		
		rulerComponentElement = this.rulerComponent().element();
		new MochiKit.Visual.Move(this.rulerComponent().element(), {
			x:1000,
			mode:'relative',
			duration:1,
//			afterFinish:MochiKit.Base.partial(MochiKit.Style.hideElement, rulerComponentElement)
			afterFinish:function () { MochiKit.Style.hideElement(rulerComponentElement); }
		});
	},

	//=============================================================================

	'addNewDirectLoginRulerSteps': function () {
		return MochiKit.Base.concat([ 'LABEL'], this.editDirectLoginRulerSteps());
		
	},
	
	'editDirectLoginRulerSteps': function () {
		return [ /*'TYPE',*/ 'CONFIGURATION', 'BINDINGS','FAVICON', 'DONE'];
	},

	//-------------------------------------------------------------------------

	'runWithDirectLogin': function (aDirectLogin, hasJustBeenAdded) {
		this._directLogin = aDirectLogin;
		this.setDirectLoginHasJustBeenAdded(hasJustBeenAdded);

		return Clipperz.Async.callbacks("DirectLoginWizardController.runWithDirectLogin", [
			MochiKit.Base.method(aDirectLogin, 'label'),
			MochiKit.Base.method(this.directLoginEditingComponent(), 'setLabel'),

			MochiKit.Base.method(aDirectLogin, 'favicon'),
			MochiKit.Base.method(this.directLoginEditingComponent(), 'setDirectLoginFavicon'),

			MochiKit.Base.method(aDirectLogin, 'bookmarkletConfiguration'),
			MochiKit.Base.method(this.directLoginEditingComponent(), 'setBookmarkletConfiguration'),

			MochiKit.Base.method(aDirectLogin, 'bindings'),
			MochiKit.Base.method(this, 'setOriginalBindings'),

			MochiKit.Base.method(aDirectLogin, 'record'),
			MochiKit.Base.methodcaller('fields'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, Clipperz.Async.collectResults("Record.directLoginReferences - collectResults", {
				'reference': MochiKit.Base.methodcaller('reference'),
				'label': MochiKit.Base.methodcaller('label'),
				'isHidden': MochiKit.Base.methodcaller('isHidden'),
				'value': MochiKit.Base.methodcaller('value')
			}, {trace:false})),
			Clipperz.Async.collectAll,
			
			MochiKit.Base.method(this, 'setRecordFields'),

			MochiKit.Base.partial(MochiKit.Async.succeed, hasJustBeenAdded),
			Clipperz.Async.deferredIf("Direct login has just been added", [
				MochiKit.Base.method(this, 'addNewDirectLoginRulerSteps')
			], [
				MochiKit.Base.method(this, 'editDirectLoginRulerSteps')
			]),
			MochiKit.Base.method(this, 'showRuler')
		], {trace:false});
	},

	//-----------------------------------------------------------------------------
	
	'checkState': function () {
		var enablePrevious;
		var enableNext;

		enablePrevious = true;
		enableNext = false;

		this.directLoginEditingComponent().disableAllPanels();

		switch(this.currentStep()) {
			case 'LABEL':
				this.directLoginEditingComponent().enableLabelField();

				enableNext = (this.directLoginEditingComponent().label() != '');
				enablePrevious = false;
				break;
			case 'TYPE':
				this.directLoginEditingComponent().enableTypeField();

				enableNext = true;
				enablePrevious = true;
				break
			case 'CONFIGURATION':
				this.directLoginEditingComponent().enableConfigurationField();

				enableNext = (this.directLoginEditingComponent().bookmarkletConfiguration() != '');
				
				if (enableNext == true) {
					try {
						Clipperz.PM.DataModel.DirectLogin.checkBookmarkletConfiguration(this.directLoginEditingComponent().bookmarkletConfiguration());
						this.directLoginEditingComponent().removeHighlightConfigurationSyntaxError();
					} catch (e) {
						this.directLoginEditingComponent().highlightConfigurationSyntaxError();
						enableNext = false;
					}
				}
				break;
			case 'BINDINGS':
				enableNext = MochiKit.Iter.every(this.bindingComponents(), function (aBindingComponent) { return aBindingComponent.selectedValue() != null; })
				this.directLoginEditingComponent().enableBindingFields();
				break;
			case 'FAVICON':
				enableNext = true;
				this.directLoginEditingComponent().enableFaviconField();
				break;
			case 'DONE':
				enableNext = true;
				this.directLoginEditingComponent().enableDonePanel();
				break;
		}

		if (this.currentStepIndex() > 0) {
			this.enablePrevious(enablePrevious);
		} else {
			this.enablePrevious(false);
		}
		this.enableNext(enableNext);
	},

	//-----------------------------------------------------------------------------

	'setFocus': function () {
		switch(this.currentStep()) {
			case 'LABEL':
				this.directLoginEditingComponent().focusOnLabelElement();
				break;
			case 'TYPE':
				break;
			case 'CONFIGURATION':
				this.directLoginEditingComponent().focusOnBookmarkletConfigurationElement();
				break;
			case 'BINDINGS':
//				this.directLoginEditingComponent().getElement('???').focus();
				break;
			case 'FAVICON':
				this.directLoginEditingComponent().focusOnFaviconElement();
				break;
			case 'DONE':
				break;
		}
	},

	//=============================================================================

	'steps': function () {
		return this._steps;
	},

	'setSteps': function (aValue) {
		this._steps = aValue;

		this.rulerComponent().setSteps(aValue);
		this.resetCurrentStepIndex();
	},
	
	'currentStepIndex': function () {
		return this._currentStepIndex;
	},
	
	'currentStep': function () {
		return this.steps()[this.currentStepIndex()];
	},

	//=============================================================================

	'handleExit': function () {
		MochiKit.Signal.signal(this, 'exit');
	},

	'done': function () {
		this.doneWithRuler();

		Clipperz.Async.callbacks("DirectLoginWizardController.done", [
			MochiKit.Base.method(this.directLoginEditingComponent(), 'label'),
			MochiKit.Base.method(this.directLogin(), 'setLabel'),
			
			MochiKit.Base.method(this.directLoginEditingComponent(), 'bookmarkletConfiguration'),
			MochiKit.Base.method(this.directLogin(), 'setBookmarkletConfiguration'),

			//	Bindings
			MochiKit.Base.method(this.directLoginEditingComponent(), 'bindingComponents'),
//			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.bind(function (aBindingComponent) {
			Clipperz.Async.forEach(MochiKit.Base.bind(function (aBindingComponent) {
				return Clipperz.Async.callbacks("DirectLoginWizardController.done - update directLogin bindings", [
					MochiKit.Base.method(this.directLogin(), 'bindings'),
					MochiKit.Base.itemgetter(aBindingComponent.formFieldName()),
					MochiKit.Base.methodcaller('setFieldKey', aBindingComponent.selectedValue())
				], {trace:false});
			}, this)),

			MochiKit.Base.method(this.directLoginEditingComponent(), 'favicon'),
			MochiKit.Base.method(this.directLogin(), 'setFavicon'),
			
			MochiKit.Base.partial(MochiKit.Signal.signal, this, 'done', {
				'directLogin':		this.directLogin(),
				'hasJustBeenAdded':	this.directLoginHasJustBeenAdded()
			})
		], {trace:false});
	},
	
	//=============================================================================

	'handleMoveBackward': function () {
		if (this._currentStepIndex > 0) {
			var	afterMoveAction;

			this._currentStepIndex --;
			afterMoveAction = MochiKit.Base.noop;

			switch(this.currentStep()) {
				case 'LABEL':
					break;
				case 'TYPE':
					break;
				case 'CONFIGURATION':
					break;
				case 'BINDINGS':
					break;
				case 'FAVICON':
					break;
				case 'DONE':
					break;
			};

			this.rulerComponent().moveBackward(afterMoveAction);
		}
		
		if (this._currentStepIndex == 0) {
			this.enablePrevious(false);
		}
	},

	'handleMoveForward': function () {
		if (this.isNextEnabled()) {
			var	afterMoveAction;

			this._currentStepIndex ++;
			afterMoveAction = MochiKit.Base.noop;

			switch(this.currentStep()) {
				case 'LABEL':
					break;
				case 'TYPE':
					break;
				case 'CONFIGURATION':
					break;
				case 'BINDINGS':
					this.resetBindingComponents();
					this.resetFormValueComponents();

					afterMoveAction = MochiKit.Base.partial(Clipperz.Async.callbacks, "DirectLoginWizardController.handleMoveForward - BINDINGS", [
						MochiKit.Base.method(this.directLogin(), 'setBookmarkletConfiguration', this.directLoginEditingComponent().bookmarkletConfiguration()),

						MochiKit.Base.method(this.directLogin(), 'favicon'),
						MochiKit.Base.method(this.directLoginEditingComponent(), 'setDirectLoginFavicon'),

						MochiKit.Base.method(this.directLogin(), 'bindings'),
						MochiKit.Base.values,
						Clipperz.Async.forEach(MochiKit.Base.bind(function (aBinding) {
							var bindingComponent;

							bindingComponent = new Clipperz.PM.UI.Web.Components.DirectLoginEditingBindingComponent({
								formFieldName: aBinding.key(),
								fields: this.recordFields(),
								selectedFieldKey: aBinding.fieldKey()
							});

							this.bindingComponents().push(bindingComponent);

							MochiKit.Signal.connect(bindingComponent, 'bindChange', this, 'handleBindChange', bindingComponent);
							this.directLoginEditingComponent().addBindingComponent(bindingComponent);

						}, this)),

						MochiKit.Base.method(this.directLogin(), 'formValues'),
						MochiKit.Base.values,
						Clipperz.Async.forEach(MochiKit.Base.bind(function (aFormValue) {
							var formValueComponent;

							formValueComponent = new Clipperz.PM.UI.Web.Components.DirectLoginEditingFormValueComponent({
								'formFieldName': aFormValue.key(),
								'fieldOptions':	 aFormValue.fieldOptions(),
								'initialValue':	 aFormValue.value()
							});

							this.formValueComponents().push(formValueComponent);

							MochiKit.Signal.connect(formValueComponent, 'formValueChange', this, 'handleFormValueChange', formValueComponent);
							this.directLoginEditingComponent().addFormValueComponent(formValueComponent);
						}, this))
						
					], {trace:false});
					
					break;
				case 'FAVICON':
					break;
				case 'DONE':
					this.directLoginEditingComponent().setDoneDescriptionWithKeys({
						'__cardName__': this.cardLabel(),
						'__directLoginName__': this.directLoginEditingComponent().label()
					});
					break;
			};

			this.rulerComponent().moveForward(afterMoveAction);
		};
	},

	'handleCursorMoved': function () {
		this.checkState();
		this.setFocus();
	},

	//-------------------------------------------------------------------------

	'handleChangedValue': function (anEvent) {
		this.checkState();
	},

	//.........................................................................

	'handleBindChange': function (aDirectLoginEditingBindingComponent) {
		var	selectedField;

		selectedField = this.recordFieldWithReference(aDirectLoginEditingBindingComponent.selectedValue());

		return Clipperz.Async.callbacks("DirectLoginWizardController.handleBindChange", [
			MochiKit.Base.method(this.directLogin(), 'bindings'),
			MochiKit.Base.itemgetter(aDirectLoginEditingBindingComponent.formFieldName()),
			MochiKit.Base.methodcaller('setFieldKey', selectedField['reference']),
			function () {
				if (selectedField != null) {
					aDirectLoginEditingBindingComponent.setFieldValue(selectedField['value']);
					aDirectLoginEditingBindingComponent.setIsHidden(selectedField['isHidden']);
				} else {
					aDirectLoginEditingBindingComponent.setFieldValue('');
					aDirectLoginEditingBindingComponent.setIsHidden(false);
				}
			},
			MochiKit.Base.method(this, 'checkState')
		], {trace:false});
	},

	//.........................................................................

	'handleFormValueChange': function (someOptions) {
		return Clipperz.Async.callbacks("DirectLoginWizardController.handleFormValueChange", [
			MochiKit.Base.method(this.directLogin(), 'formValues'),
			MochiKit.Base.itemgetter(someOptions['fieldName']),
			MochiKit.Base.methodcaller('setValue', someOptions['selectedValue']),
			MochiKit.Base.method(this, 'checkState')
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'handleDirectLoginEditingComponentKeyPressed': function (anEvent) {
		if (anEvent.key().string == 'KEY_ENTER') {
			if (anEvent.target().nodeName != 'TEXTAREA') {
				anEvent.preventDefault();
				this.handleMoveForward();
			}
		} else if (anEvent.key().string == 'KEY_TAB') {
			this.handleMoveForward();
			if ((anEvent.target().nodeName == 'INPUT') || (anEvent.target().nodeName == 'TEXTAREA')) {
				anEvent.preventDefault();
			}
		} else if ((anEvent.key().string == 'KEY_ARROW_RIGHT') && (anEvent.modifier().meta == true)) {
			this.handleMoveForward();
		} else if ((anEvent.key().string == 'KEY_ARROW_LEFT') && (anEvent.modifier().meta == true)) {
			this.handleMoveBackward();
		} else if (anEvent.key().string == 'KEY_ESCAPE') {
			anEvent.stop();
			this.handleExit();
		}
	},

	//=============================================================================
	__syntaxFix__: "syntax fix"
});
