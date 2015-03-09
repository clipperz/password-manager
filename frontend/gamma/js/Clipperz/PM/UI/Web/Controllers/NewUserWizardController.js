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

Clipperz.PM.UI.Web.Controllers.NewUserWizardController = function(args) {
	this._newUserCreationComponent	= args.newUserCreationComponent	|| Clipperz.Base.exception.raise('MandatoryParameter');

	MochiKit.Signal.connect(this._newUserCreationComponent, 'changedValue',	this, 'handleChangedValue');
	MochiKit.Signal.connect(this._newUserCreationComponent, 'moveForward',	this, 'handleMoveForward');
	MochiKit.Signal.connect(this._newUserCreationComponent, 'keyPressed',	this, 'handleNewUserCreationComponentKeyPressed');

	this._rulerComponent = null;

	this._steps = null;
	this._currentStepIndex = 0;
	this._isNextEnabled = false;

	this._userCreationState = 'IDLE';	//	'IN PROGRESS', 'DONE', 'FAILED'
	this._user = null;
	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.Web.Controllers.NewUserWizardController.prototype, {

	'toString': function() {
		return "Clipperz.PM.UI.Web.Controllers.NewUserWizardController";
	},

	//-------------------------------------------------------------------------

	'newUserCreationComponent': function () {
		return this._newUserCreationComponent;
	},

	//=============================================================================

	'user': function () {
		return this._user;
	},
	
	'setUser': function (aValue) {
		this._user = aValue;
	},

	//-----------------------------------------------------------------------------

	'userCreationState': function () {
		return this._userCreationState;
	},
	
	'setUserCreationState': function (aValue) {
		this._userCreationState = aValue;
		this.checkState();
	},

	//=============================================================================

	'resetCurrentStepIndex': function () {
		this._currentStepIndex = 0;
		this.rulerComponent().resetStatus({animateTransition:true});
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

	'rulerComponent': function () {
		if (this._rulerComponent == null) {
			this._rulerComponent = new Clipperz.PM.UI.Web.Components.RulerComponent({
				translationContext:'Wizards.NewUserWizard'
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

	'resetRuler': function () {
//		if (this._rulerComponent != null) {
//			this._rulerComponent.clear();
//		}
//		this._rulerComponent = null;
	},

	//-----------------------------------------------------------------------------

	'showRuler': function (someSteps) {
		var rulerElement;

		this.setSteps(someSteps);

		rulerElement = this.rulerComponent().element();
		this.newUserCreationComponent().disableAllPanels();

		MochiKit.Style.showElement(rulerElement);
		MochiKit.Style.setElementPosition(rulerElement, {x:-1000, y:this.newUserCreationComponent().bottomMargin()});
		new MochiKit.Visual.Move(rulerElement, {
			x:0, y:this.newUserCreationComponent().bottomMargin(),
			mode:'absolute',
			duration:0.5,
//			afterFinish:MochiKit.Base.method(this, 'handleCursorMoved')
			afterFinish:MochiKit.Base.method(this, 'handleRulerShowed')
		});
	},

	//-----------------------------------------------------------------------------

	'handleRulerShowed':function () {
		return Clipperz.Async.callbacks("NewUserWizardController.handlerRulerShowed", [
			MochiKit.Base.method(this.newUserCreationComponent(), 'waitUntilFullyRendered'),
			MochiKit.Base.method(this, 'handleCursorMoved')
		], {trace:false});
	},

	//-----------------------------------------------------------------------------

	'hideRuler': function () {
		new MochiKit.Visual.Move(this.rulerComponent().element(), {x:-1000, mode:'relative', duration:0.5});
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

	'createNewUserRulerSteps': function () {
		return [ 'CREDENTIALS', 'CHECK_CREDENTIALS', 'TERMS_OF_SERVICE', 'CREATE_USER'/*, 'LOGIN' */];
	},

	//-------------------------------------------------------------------------

	'run': function () {
		return Clipperz.Async.callbacks("NewUserWizardController.run", [
			MochiKit.Base.method(this, 'createNewUserRulerSteps'),
			MochiKit.Base.method(this, 'showRuler')
		], {trace:false});
	},

	//-----------------------------------------------------------------------------
	
	'checkState': function () {
		var enablePrevious;
		var enableNext;

		enablePrevious = true;
		enableNext = false;

		this.newUserCreationComponent().disableAllPanels();

		switch(this.currentStep()) {
			case 'CREDENTIALS':
				this.newUserCreationComponent().enableCredentialsPanel();

				enableNext = (
					(this.newUserCreationComponent().username() != '')
					&&
					(this.newUserCreationComponent().passphrase() != '')
				);
//				enablePrevious = false;
				break;
			case 'CHECK_CREDENTIALS':
				this.newUserCreationComponent().enableCheckCredentialsPanel();

				enableNext = (this.newUserCreationComponent().passphrase() == this.newUserCreationComponent().rePassphrase());
//				enablePrevious = true;
				break
			case 'TERMS_OF_SERVICE':
				this.newUserCreationComponent().enableTermsOfServicePanel();

				enableNext = (
					(this.newUserCreationComponent().awareOfUnrecoverablePassphrase() == 'on')
					&&
					(this.newUserCreationComponent().readTermsOfService() == 'on')
				)
				break;
			case 'CREATE_USER':
				this.newUserCreationComponent().enableCreateUserPanel();

				switch (this.userCreationState()) {
					case 'IDLE':
						this.setUserCreationState('IN PROGRESS');
						this.preformActualUserRegistration();

						enablePrevious = false;
						enableNext = false;
						break;
					case 'IN PROGRESS':
						enablePrevious = false;
						enableNext = false;
						break;
					case 'DONE':
						enablePrevious = false;
						enableNext = true;
						break;
					case 'FAILED':
						enablePrevious = true;
						enableNext = false;
						break;
				};
				break;
//			case 'LOGIN':
//				this.newUserCreationComponent().enableLoginPanel();
//				break;
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
			case 'CREDENTIALS':
				this.newUserCreationComponent().focusOnUsernameElement();
				break;
			case 'CHECK_CREDENTIALS':
				this.newUserCreationComponent().focusOnRePassphraseElement();
				break
			case 'TERMS_OF_SERVICE':
				break;
			case 'CREATE_USER':
				break;
//			case 'LOGIN':
//				break;
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
		return Clipperz.Async.callbacks("NewUserWizardController.handleExit", [
//			MochiKit.Base.method(this.newUserCreationComponent(), 'resetContent'),
			Clipperz.Async.forkAndJoin("NewUserWizardController.handleExit - fork and join", [
				MochiKit.Base.method(this, 'hideRuler'),
				MochiKit.Base.method(this.newUserCreationComponent(), 'deferredHideModal')
			], {trace:false}),
			MochiKit.Base.method(this, 'resetRuler'),
//			MochiKit.Base.method(this.newUserCreationComponent(), 'reset'),
			MochiKit.Base.partial(MochiKit.Signal.signal, this, 'exit')
		], {trace:false})
	},

	'done': function () {
		this.doneWithRuler();
		this.user().resetAllData();
		MochiKit.Signal.signal(this, 'done', {'user': this.user()});
	},
	
	//=============================================================================

	'handleMoveBackward': function () {
		if (this._currentStepIndex > 0) {
			var	afterMoveAction;

			afterMoveAction = MochiKit.Base.noop;

			switch(this.currentStep()) {
				case 'CREDENTIALS':
				case 'CHECK_CREDENTIALS':
				case 'TERMS_OF_SERVICE':
					this._currentStepIndex --;
					this.rulerComponent().moveBackward(afterMoveAction);
					break;
				case 'CREATE_USER':
					this.setUser(null);
					this.newUserCreationComponent().hideAllProgeressStates();
					this.resetCurrentStepIndex();
					this.setUserCreationState('IDLE');
					break;
//				case 'LOGIN':
//					break;
			};

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
				case 'CREDENTIALS':
					break;
				case 'CHECK_CREDENTIALS':
					break
				case 'TERMS_OF_SERVICE':
					break;
				case 'CREATE_USER':
					break;
//				case 'LOGIN':
//					break;
			};

			this.rulerComponent().moveForward(afterMoveAction);
		};
	},

	'handleCursorMoved': function () {
//		this.checkState();
//		this.setFocus();
		
		return Clipperz.Async.callbacks("NewUserWizardController.handleCursorMoved", [
			MochiKit.Base.method(this.newUserCreationComponent(), 'waitUntilFullyRendered'),
			MochiKit.Base.method(this, 'checkState'),
			MochiKit.Base.method(this, 'setFocus')
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'handleChangedValue': function (anEvent) {
		this.checkState();
	},

	//-------------------------------------------------------------------------

	'handleNewUserCreationComponentKeyPressed': function (anEvent) {
		if (anEvent.key().string == 'KEY_ENTER') {
			if (anEvent.target().nodeName != 'TEXTAREA') {
				anEvent.preventDefault();
				this.handleMoveForward();
			}
		} else if (anEvent.key().string == 'KEY_TAB') {
			if (anEvent.target() == this.newUserCreationComponent().usernameElement()) {
			} else {
				this.handleMoveForward();
				if ((anEvent.target().nodeName == 'INPUT') || (anEvent.target().nodeName == 'TEXTAREA')) {
					anEvent.preventDefault();
				}
			}
		} else if ((anEvent.key().string == 'KEY_ARROW_RIGHT') && (anEvent.modifier().meta == true)) {
			this.handleMoveForward();
		} else if ((anEvent.key().string == 'KEY_ARROW_LEFT') && (anEvent.modifier().meta == true)) {
			this.handleMoveBackward();
		} else if (anEvent.key().string == 'KEY_ESCAPE') {
			anEvent.stop();
			this.handleExit();
		} else {
			MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'checkState'));
		}
	},

	//=============================================================================

	'preformActualUserRegistration': function () {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("NewUSerWizardController.preformActualUserRegistration", {trace:false});
		deferredResult.addMethod(this.newUserCreationComponent(), 'showProgressOnUserCreation');
		deferredResult.addMethod(Clipperz.PM.RunTime.mainController, 'setPassphraseDelegate', MochiKit.Base.method(this.newUserCreationComponent(), 'passphrase'));
		deferredResult.addCallback(Clipperz.PM.DataModel.User.registerNewAccount,
			this.newUserCreationComponent().username(),
			MochiKit.Base.method(Clipperz.PM.RunTime.mainController, 'getPassphrase')
		);
		deferredResult.addMethod(this, 'setUser');
		deferredResult.addMethod(this.newUserCreationComponent(), 'showUserCreationDone');
		deferredResult.addMethod(this, 'setUserCreationState', 'DONE');
		
//		deferredResult.addErrback(MochiKit.Base.method(this.newUserCreationComponent(), 'showUserCreationFailed'));
//		deferredResult.addErrback(MochiKit.Base.method(this, 'setUser', null));
//		deferredResult.addErrback(MochiKit.Base.method(this, 'setUserCreationState', 'FAILED'));
		deferredResult.addErrback(MochiKit.Base.bind(function (aValue) {
			this.newUserCreationComponent().showUserCreationFailed();
			this.setUser(null);
			this.setUserCreationState('FAILED');
		}, this));
		deferredResult.callback();
		
		return deferredResult;
	},

	//=============================================================================
	__syntaxFix__: "syntax fix"
});
