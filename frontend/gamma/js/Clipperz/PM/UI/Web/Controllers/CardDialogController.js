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

Clipperz.PM.UI.Web.Controllers.CardDialogController = function(args) {
	args = args || {};
	
	Clipperz.PM.UI.Web.Controllers.CardDialogController.superclass.constructor.call(this, args);
	
	this._record	= args.record		|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._delegate	= args.delegate		|| Clipperz.Base.exception.raise('MandatoryParameter');

	this._referenceElement = null;
	this._cardDialogComponent = null;

	this._fieldsReferences = {};
	this._directLoginReferences = {};

	this._directLoginWizardController = null;
	this._directLoginEditingComponent = null;
	this._isDirectLoginEditingComponentVisible = false;

	return this;
};

Clipperz.Base.extend(Clipperz.PM.UI.Web.Controllers.CardDialogController, Object, {

	'toString': function() {
		return "Clipperz.PM.UI.Web.Controllers.CardDialogController";
	},

	//-------------------------------------------------------------------------

	'record': function () {
		return this._record;
	},

	'delegate': function () {
		return this._delegate;
	},

	//-------------------------------------------------------------------------

	'fieldsReferences': function () {
		return this._fieldsReferences;
	},

	'directLoginReferences': function () {
		return this._directLoginReferences;
	},

	//-------------------------------------------------------------------------

	'referenceElement': function () {
		return this._referenceElement;
	},

	'setReferenceElement': function (anElement) {
		this._referenceElement = anElement;
 	},

	//-------------------------------------------------------------------------

	'cardDialogComponent': function () {
		if (this._cardDialogComponent == null) {
			this._cardDialogComponent = new Clipperz.PM.UI.Web.Components.CardDialogComponent();

			MochiKit.Signal.connect(this._cardDialogComponent, 'cancel',		this, 'handleCancel');
			MochiKit.Signal.connect(this._cardDialogComponent, 'save',			this, 'handleSave');

			MochiKit.Signal.connect(this._cardDialogComponent, 'addField',		this, 'handleAddField');
			MochiKit.Signal.connect(this._cardDialogComponent, 'changedValue',	this, 'handleChangedValue');

			MochiKit.Signal.connect(this._cardDialogComponent, 'addDirectLogin',this, 'handleAddDirectLogin');
			MochiKit.Signal.connect(this._cardDialogComponent, 'keyPressed',	this, 'handleCardDialogComponentKeyPressed');
		}
		
		return this._cardDialogComponent;
	},

	//=========================================================================

	'directLoginWizardController': function () {
		if (this._directLoginWizardController == null) {
			this._directLoginWizardController = new Clipperz.PM.UI.Web.Controllers.DirectLoginWizardController({
				'cardLabel': this.cardDialogComponent().title(),
				'directLoginEditingComponent': this.directLoginEditingComponent()
			})

			MochiKit.Signal.connect(this._directLoginWizardController, 'exit',	this, 'handleHideDirectLoginEditingComponent');
			MochiKit.Signal.connect(this._directLoginWizardController, 'done',	this, 'handleCompleteDirectLoginEditingComponent');
		}
		
		return this._directLoginWizardController;
	},

	//-------------------------------------------------------------------------

	'directLoginEditingComponent': function () {
		if (this._directLoginEditingComponent == null) {
			this._directLoginEditingComponent = new Clipperz.PM.UI.Web.Components.DirectLoginEditingComponent();

			this.cardDialogComponent().renderDirectLoginEditingComponent(this._directLoginEditingComponent);
			
//			MochiKit.Signal.connect(this._directLoginEditingComponent, 'back',			this, 'handleHideDirectLoginEditingComponent')
//			MochiKit.Signal.connect(this._directLoginEditingComponent, 'changedValue',	this, 'handleChangedValue');
//			MochiKit.Signal.connect(this.__directLoginEditingComponent, 'keyPressed',	this, 'handleDirectLoginEditingComponentKeyPressed');
		}
		
		return this._directLoginEditingComponent;
	},

	//-------------------------------------------------------------------------

	'isDirectLoginEditingComponentVisible': function () {
		return this._isDirectLoginEditingComponentVisible;
	},
	
	'setIsDirectLoginEditingComponentVisible': function (aValue) {
		this._isDirectLoginEditingComponentVisible = aValue;
	},
	
	//=========================================================================

	'run': function (anElement) {
		var deferredResult;
		
		this.setReferenceElement(anElement);

		deferredResult = new Clipperz.Async.Deferred("CardDialogController.run", {trace:false});
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'initProgress', {'steps':11});

		deferredResult.addMethod(this.cardDialogComponent(), 'deferredShowModal', {openFromElement:this.referenceElement()});
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');

		deferredResult.addMethod(this.record(), 'label');
		deferredResult.addMethod(this.cardDialogComponent(), 'setTitle');
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');

		deferredResult.addMethod(this, 'updateComponentState');
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'progressDone');

		deferredResult.addMethod(this.cardDialogComponent(), 'fixRendering');
		deferredResult.addMethod(this.cardDialogComponent(), 'hideProgressMask');
		
		if (this.record().isBrandNew()) {
			deferredResult.addMethod(this.cardDialogComponent(), 'setHintMode', 'ON');
			deferredResult.addMethod(this.cardDialogComponent(), 'setFocusOnTitleField');
		}

		deferredResult.addErrback(MochiKit.Base.method(this.cardDialogComponent(), 'showError'));
		deferredResult.callback();
		
		return deferredResult;
	},

	//=========================================================================

	'updateComponentState': function () {
		return Clipperz.Async.callbacks("CardDialogController.updateComponentState", [
			MochiKit.Base.method(this.record(), 'hasPendingChanges'),
			MochiKit.Base.method(this.cardDialogComponent(), 'setShouldEnableSaving'),
        	
			MochiKit.Base.method(this.record(), 'label'),
			MochiKit.Base.method(this.cardDialogComponent(), 'setTitle'),
			MochiKit.Base.method(this.record(), 'notes'),
			MochiKit.Base.method(this.cardDialogComponent(), 'setNotes'),
        	
			MochiKit.Base.method(this.record(), 'fields'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this, 'addCardDialogComponentWithField')),

			MochiKit.Base.method(this.record(), 'directLogins'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this, 'addCardDialogComponentWithDirectLogin')),

			MochiKit.Base.method(this.cardDialogComponent(), 'resetNewFieldInputs'),
			MochiKit.Base.noop
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'addCardDialogComponentWithField': function (aField) {
		var	fieldComponent;
		
		fieldComponent = new Clipperz.PM.UI.Web.Components.CardDialogRecordFieldComponent({reference: aField.reference()});
		MochiKit.Signal.connect(fieldComponent, 'changedValue',	this, 'handleChangedValue');
		MochiKit.Signal.connect(fieldComponent, 'performAction',this, 'handlePerformFieldAction');
		MochiKit.Signal.connect(fieldComponent, 'deleteField',	this, 'handleDeleteField');

//		this.fieldsReferences().push({'field':aField, 'component':fieldComponent});
		this.fieldsReferences()[aField.reference()] = {'field':aField, 'component':fieldComponent};

		return Clipperz.Async.callbacks("CardDialogController.addCardDialogComponentWithField", [
			MochiKit.Base.method(this.cardDialogComponent(), 'addFieldRowComponent', fieldComponent),

			MochiKit.Base.method(aField, 'label'),
			MochiKit.Base.method(fieldComponent, 'setLabel'),
			MochiKit.Base.method(aField, 'value'),
			MochiKit.Base.method(fieldComponent, 'setValue'),
			MochiKit.Base.method(aField, 'actionType'),
			MochiKit.Base.method(fieldComponent, 'setActionType'),
			MochiKit.Base.method(aField, 'isHidden'),
			MochiKit.Base.method(fieldComponent, 'setIsHidden')
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'addCardDialogComponentWithDirectLogin': function (aDirectLogin) {
		var directLoginComponent;
		
		directLoginComponent = new Clipperz.PM.UI.Web.Components.CardDialogRecordDirectLoginComponent({reference: aDirectLogin.reference()});
		MochiKit.Signal.connect(directLoginComponent, 'changedValue',		this, 'handleChangedValue');
		MochiKit.Signal.connect(directLoginComponent, 'deleteDirectLogin',	this, 'handleDeleteDirectLogin');
		MochiKit.Signal.connect(directLoginComponent, 'editDirectLogin',	this, 'handleEditDirectLogin');
		MochiKit.Signal.connect(directLoginComponent, 'openDirectLogin',	this, 'handleOpenDirectLogin');

		this.directLoginReferences()[aDirectLogin.reference()] = {'directLogin':aDirectLogin, 'component':directLoginComponent};

		return Clipperz.Async.callbacks("CardDialogController.addCardDialogComponentWithDirectLogin", [
			MochiKit.Base.method(this.cardDialogComponent(), 'addDirectLoginComponent', directLoginComponent),
			MochiKit.Base.method(this, 'refreshDirectLoginComponent', this.directLoginReferences()[aDirectLogin.reference()])
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'refreshDirectLoginComponent': function (aDirectLoginReference) {
		return Clipperz.Async.callbacks("CardDialogController.refreshDirectLoginComponent", [
			MochiKit.Base.method(aDirectLoginReference['directLogin'],	'favicon'),
//			MochiKit.Base.method(aDirectLoginReference['directLogin'],	'faviconData'),
			MochiKit.Base.method(aDirectLoginReference['component'],	'setFavicon'),
			MochiKit.Base.method(aDirectLoginReference['directLogin'],	'label'),
			MochiKit.Base.method(aDirectLoginReference['component'],	'setLabel')
		], {trace:false});
	},

	'refreshDirectLoginComponents': function () {
		return Clipperz.Async.callbacks("CardDialogController.refreshDirectLoginComponents", [
			MochiKit.Base.method(this, 'directLoginReferences'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this, 'refreshDirectLoginComponent')),
			Clipperz.Async.collectAll
		])
	},

	//-------------------------------------------------------------------------

	'updateRecordValues': function () {
		return Clipperz.Async.callbacks('CardDialogController.updateRecordValues', [
			MochiKit.Base.method(this.cardDialogComponent(), 'title'),
			MochiKit.Base.method(this.record(), 'setLabel'),
			MochiKit.Base.method(this.cardDialogComponent(), 'notes'),
			MochiKit.Base.method(this.record(), 'setNotes'),
			
			MochiKit.Base.method(this, 'fieldsReferences'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this, 'updateRecordFieldValues')),

			MochiKit.Base.method(this, 'directLoginReferences'),
			MochiKit.Base.values,
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this, 'updateRecordDirectLoginValues')),

			MochiKit.Base.method(this.directLoginEditingComponent(), 'directLoginReference'),
			MochiKit.Base.method(this.record(), 'directLoginWithReference'),
			MochiKit.Base.method(this, 'updateRecordDirectLoginDetails'),

			MochiKit.Base.noop
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'updateRecordFieldValues': function (aFieldReference) {
		var deferredResult;

		deferredResult = Clipperz.Async.callbacks('CardDialogController.updateRecordFieldValues', [
			MochiKit.Base.method(aFieldReference['component'],	'label'),
			MochiKit.Base.method(aFieldReference['field'],		'setLabel'),

			MochiKit.Base.method(aFieldReference['component'],	'value'),
			MochiKit.Base.method(aFieldReference['field'],		'setValue'),

			MochiKit.Base.method(aFieldReference['component'],	'isHidden'),
			MochiKit.Base.method(aFieldReference['field'],		'setIsHidden'),

			MochiKit.Base.method(aFieldReference['field'],		'actionType'),
			MochiKit.Base.method(aFieldReference['component'],	'setActionType')
		], {trace:false});
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'updateRecordDirectLoginValues': function (aDirectLoginReference) {
		var deferredResult;

		deferredResult = Clipperz.Async.callbacks('CardDialogController.updateRecordDirectLoginValues', [
			MochiKit.Base.method(aDirectLoginReference['component'], 'label'),
			MochiKit.Base.method(aDirectLoginReference['directLogin'], 'setLabel')
		], {trace:false});
		
		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	'updateRecordDirectLoginDetails': function (aDirectLogin) {
		var result;

		if (MochiKit.Base.isUndefinedOrNull(aDirectLogin)) {
			result = MochiKit.Async.succeed();
		} else {
			result = Clipperz.Async.callbacks("CardDialogController.updateRecordDirectLoginDetails", [
				MochiKit.Base.method(this.directLoginEditingComponent(), 'label'),
				MochiKit.Base.method(aDirectLogin, 'setLabel'),
				MochiKit.Base.method(this.directLoginEditingComponent(), 'favicon'),
				MochiKit.Base.method(aDirectLogin, 'setFavicon')
			], {trace:false});
		}
		
		return result;
	},

	//=========================================================================

	'addField': function () {
		return this.record().addField({
			'label':this.cardDialogComponent().newFieldLabel(),
			'value':this.cardDialogComponent().newFieldValue(),
			'isHidden':this.cardDialogComponent().newFieldIsHidden()
		});
	},

	'handleAddField': function () {
		return Clipperz.Async.callbacks("CardDialogController.handleAddField", [
			MochiKit.Base.method(this, 'addField'),

			MochiKit.Base.method(this, 'addCardDialogComponentWithField'),
			MochiKit.Base.method(this.cardDialogComponent(), 'resetNewFieldInputs'),

			MochiKit.Base.method(this.cardDialogComponent(), 'fixRendering'),
			MochiKit.Base.method(this, 'handleChangedValue')
		], {trace:false})
	},

	//-------------------------------------------------------------------------

	'handlePerformFieldAction': function (aFieldID, aTargetElement) {
		return Clipperz.Async.callbacks("CardDialogController.handleDeleteField", [
			MochiKit.Base.method(this.record(), 'fields'),
			MochiKit.Base.itemgetter(aFieldID),
			Clipperz.Async.collectResults("CardDialogController.handleDeleteField <collect results>", {
				'value':	MochiKit.Base.methodcaller('value'),
				'type':		MochiKit.Base.methodcaller('actionType')
			}, {trace:false}),
			MochiKit.Base.bind(function (someValues) {
				switch (someValues['type']) {
					case 'NONE':
						throw "this event handler should not be triggered for fields with type 'NONE'";
						break;
					case 'URL':
						var url;
						
						url = someValues['value'];
						if (/^https?\:\/\//.test(url) == false) {
							url = 'http://' + url;
						}
						
						window.open(url);
						break;
					case 'BITCOIN':
						window.open(someValues['value']);
						break;
					case 'EMAIL':
						var url;
						
						url = 'mailto:' + someValues['value'];
						
						MochiKit.DOM.currentWindow().location = url;
						break;
					case 'PASSWORD':
//Clipperz.log("SHOW PASSWORD " + someValues['value']);
						this.showPasswordTooltip(someValues['value'], aTargetElement);
						break;
				}
			}, this)
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'handleDeleteField': function (aFieldID) {
		return Clipperz.Async.callbacks("CardDialogController.handleDeleteField", [
			MochiKit.Base.method(this.record(), 'fields'),
			MochiKit.Base.itemgetter(aFieldID),
			MochiKit.Base.method(this.record(), 'removeField'),

			MochiKit.Base.method(this, 'fieldsReferences'),
			MochiKit.Base.itemgetter(aFieldID),
			MochiKit.Base.itemgetter('component'),

			function (aComponent) {
				return Clipperz.Async.callbacks("CardDialogController.handleDeleteField [fade and remove]", [
					MochiKit.Base.partial(Clipperz.Visual.deferredAnimation, MochiKit.Visual.fade,	 aComponent.element(), {from:1.0, to:0.0, duration:0.5}),
//					Clipperz.Visual.deferredAnimation(MochiKit.Visual.fade,	 aComponent.element(), {from:1.0, to:0.0, duration:0.5}),
					MochiKit.Base.method(aComponent, 'remove')
				], {trace:false});
			},
	
			MochiKit.Base.bind(function () {
				delete this.fieldsReferences()[aFieldID];
			}, this),

			MochiKit.Base.method(this.cardDialogComponent(), 'fixRendering'),
			MochiKit.Base.method(this, 'handleChangedValue')
		], {trace:false});
	},

	//=========================================================================

	'handleDeleteDirectLogin': function(aDirectLoginReference) {
		var cardDialogComponent;
		
		cardDialogComponent = this.cardDialogComponent();
		
		return Clipperz.Async.callbacks("CardDialogController.handleDeleteDirectLogin", [
			MochiKit.Base.method(this.record(), 'directLogins'),
			MochiKit.Base.itemgetter(aDirectLoginReference),
			MochiKit.Base.methodcaller('remove'),

			MochiKit.Base.method(this, 'directLoginReferences'),
			MochiKit.Base.itemgetter(aDirectLoginReference),
			MochiKit.Base.itemgetter('component'),

			function (aComponent) {
				return Clipperz.Async.callbacks("CardDialogController.handleDeleteDirectLogin [fade and remove]", [
					MochiKit.Base.partial(Clipperz.Visual.deferredAnimation, MochiKit.Visual.fade,	 aComponent.element(), {from:1.0, to:0.0, duration:0.5}),//					Clipperz.Visual.deferredAnimation(MochiKit.Visual.fade,	 aComponent.element(), {from:1.0, to:0.0, duration:0.5}),
///					MochiKit.Base.method(aComponent, 'remove')
					MochiKit.Base.method(cardDialogComponent, 'removeDirectLoginComponent', aComponent)
				], {trace:false});
			},
	
			MochiKit.Base.bind(function () {
				delete this.directLoginReferences()[aDirectLoginReference];
			}, this),

			MochiKit.Base.method(this.cardDialogComponent(), 'fixRendering'),
			MochiKit.Base.method(this, 'handleChangedValue')
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'handleOpenDirectLogin': function (aDirectLoginReference) {
		return Clipperz.Async.callbacks("CardDialogController.handleOpenDirectLogin", [
			MochiKit.Base.method(this.record(), 'directLoginWithReference', aDirectLoginReference),
			Clipperz.PM.UI.Common.Controllers.DirectLoginRunner.openDirectLogin
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'handleEditDirectLogin': function (aDirectLoginReference) {
		return Clipperz.Async.callbacks("CardDialogController.handleEditDirectLogin", [
			MochiKit.Base.method(this, 'setIsDirectLoginEditingComponentVisible', true),
			MochiKit.Base.method(this.directLoginEditingComponent(), 'setDirectLoginReference', aDirectLoginReference),
			MochiKit.Base.method(this.cardDialogComponent(), 'placeDirectLoginEditingComponent'),
			MochiKit.Base.method(this.record(), 'directLoginWithReference', aDirectLoginReference),
			MochiKit.Base.method(this.directLoginWizardController(), 'runWithDirectLogin'),
			MochiKit.Base.method(this.directLoginWizardController(), 'fixRulerRendering', this.cardDialogComponent().displayMode()),
			MochiKit.Base.method(this.cardDialogComponent(), 'showDirectLoginEditingComponent')
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'handleHideDirectLoginEditingComponent': function () {
		return Clipperz.Async.callbacks("CardDialogController.handleHideDirectLoginEditingComponent", [
			MochiKit.Base.method(this, 'setIsDirectLoginEditingComponentVisible', false),
			MochiKit.Base.method(this.directLoginWizardController(), 'hideRuler'),
			MochiKit.Base.method(this.directLoginEditingComponent(), 'setDirectLoginReference', null),
			MochiKit.Base.method(this, 'refreshDirectLoginComponents'),
			MochiKit.Base.method(this.cardDialogComponent(), 'hideDirectLoginEditingComponent')
		], {trace:false})
	},

	'handleCompleteDirectLoginEditingComponent': function (someParameters) {
		return Clipperz.Async.callbacks("CardDialogController.handleCompleteDirectLoginEditingComponent", [
			MochiKit.Base.method(this, 'setIsDirectLoginEditingComponentVisible', false),
			MochiKit.Base.method(this.directLoginEditingComponent(), 'setDirectLoginReference', null),
			MochiKit.Base.partial(MochiKit.Async.succeed, someParameters['hasJustBeenAdded']),
			Clipperz.Async.deferredIf("CardDialogController.handleCompleteDirectLoginEditingComponent - should addTheEditedDirectLogin", [
				MochiKit.Base.method(this, 'addCardDialogComponentWithDirectLogin', someParameters['directLogin'])
			], []),
			MochiKit.Base.method(this, 'refreshDirectLoginComponents'),
			MochiKit.Base.method(this, 'handleChangedValue'),
			MochiKit.Base.method(this.cardDialogComponent(), 'hideDirectLoginEditingComponent')
		], {trace:false})
	},

	//=========================================================================

	'handleChangedValue': function () {
		return Clipperz.Async.callbacks("CardDialogController.handleChangedValue", [
			MochiKit.Base.method(this, 'updateRecordValues'),
			MochiKit.Base.method(this.record(), 'hasPendingChanges'),
			MochiKit.Base.method(this.cardDialogComponent(), 'setShouldEnableSaving')
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'handleSave': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("CardDialogController.handleSave", {trace:false});
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'initProgress', {'steps':8});
		deferredResult.addMethod(this.cardDialogComponent(), 'showProgressMask');
		deferredResult.addMethod(this.cardDialogComponent(), 'newFieldHasPendingChanges');
		deferredResult.addIf([
			MochiKit.Base.method(this, 'addField')
		], [])
		deferredResult.addMethod(this, 'saveChanges');
		deferredResult.addMethod(this.cardDialogComponent(), 'deferredHideModal', {closeToElement:null});
		deferredResult.addMethod(this.cardDialogComponent(), 'remove');
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'cardDialogComponentClosed');
		
		deferredResult.callback();
		
		return deferredResult;
	},

	//.........................................................................

	'saveChanges': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("CardDialogController.handleSave", {trace:false});
		deferredResult.addMethod(this.delegate(), 'saveChanges');
deferredResult.addErrback(function (aValue) { Clipperz.log("SHIT HAPPENS!!"); return aValue; });

		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'handleCancel': function () {
		var deferredResult;

		if (this.isDirectLoginEditingComponentVisible()) {
			deferredResult = this.handleHideDirectLoginEditingComponent();
		} else {
			deferredResult = new Clipperz.Async.Deferred("CardDialogController.handleCancel", {trace:false});
	//		deferredResult.addMethod(this.record(), 'hasPendingChanges'),
			deferredResult.addMethod(this.delegate(), 'hasPendingChanges'),
			deferredResult.addIf([
				MochiKit.Base.method(this.cardDialogComponent(), 'askConfirmationForLoosingPendingChanges')
			], [])
			deferredResult.addMethod(this.delegate(), 'revertChanges');
			deferredResult.addMethod(this.cardDialogComponent(), 'deferredHideModal', {closeToElement:this.referenceElement()});
			deferredResult.addMethod(this.cardDialogComponent(), 'remove');
			deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'cardDialogComponentClosed');

			deferredResult.callback();
		}
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'handleAddDirectLogin': function () {
		return Clipperz.Async.callbacks("CardDialogController.handleAddDirectLogin", [
			MochiKit.Base.method(this.record(), 'createNewDirectLogin'),
			MochiKit.Base.bind(function (aDirectLogin) {
				return Clipperz.Async.callbacks("CardDialogController.handleAddDirectLogin - directLogin", [
					MochiKit.Base.method(this.cardDialogComponent(), 'newFieldHasPendingChanges'),
					Clipperz.Async.deferredIf("cardDialogComponent.newFieldHasPendingChanges", [
						MochiKit.Base.method(this, 'handleAddField')
					], []),

					MochiKit.Base.method(this.directLoginEditingComponent(), 'setDirectLoginReference', aDirectLogin.reference()),
					MochiKit.Base.method(this.cardDialogComponent(), 'placeDirectLoginEditingComponent'),
					MochiKit.Base.method(this.directLoginWizardController(), 'runWithDirectLogin', aDirectLogin, true),
					MochiKit.Base.method(this.directLoginWizardController(), 'fixRulerRendering', this.cardDialogComponent().displayMode()),
					MochiKit.Base.method(this.cardDialogComponent(), 'showDirectLoginEditingComponent')
				], {trace:false});
			}, this)
		], {trace:false});
	},

	//=========================================================================

	'handleCardDialogComponentKeyPressed': function (anEvent) {
		if ((anEvent.key().string == 'KEY_TAB') && this.cardDialogComponent().newFieldHasPendingChanges()) {
			anEvent.preventDefault();

//			MochiKit.Signal.signal(this.cardDialogComponent(), 'addField');
			this.handleAddField()
			this.cardDialogComponent().focusOnNewFieldLabel();
		}
	},

	//=========================================================================

	'showPasswordTooltip': function (aValue, anElement) {
		var	passwordTooltip;
		
		passwordTooltip = new Clipperz.PM.UI.Web.Components.PasswordTooltip({
			'referebceElement': anElement,
			'text': aValue
		});
		
		passwordTooltip.show();
		
		
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});
