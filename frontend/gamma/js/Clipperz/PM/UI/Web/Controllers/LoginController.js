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

Clipperz.PM.UI.Web.Controllers.LoginController = function(args) {
	this._args = args || {};

	this._loginPage = null;

	this._newUserWizardController = null;
	this._newUserCreationComponent = null;

	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.Web.Controllers.LoginController.prototype, {

	'toString': function() {
		return "Clipperz.PM.UI.Web.Controllers.LoginController";
	},

	'args': function () {
		return this._args;
	},

	//-----------------------------------------------------------------------------

	'loginPage': function() {
		if (this._loginPage == null) {
			this._loginPage = new Clipperz.PM.UI.Web.Components.LoginPage();
			
			MochiKit.Signal.connect(this._loginPage, 'createNewAccountClick', this, 'handleCreateNewAccountClick')
		}
		
		return this._loginPage;
	},

	//-----------------------------------------------------------------------------

	'run': function(args) {
		var	slot;
		var	loginPage;
		var	loginForm;

		slot = args.slot;

		loginForm =	new Clipperz.PM.UI.Web.Components.LoginForm({'autocomplete': this.args()['autocomplete']});

		slot.setContent(this.loginPage());
		this.loginPage().slotNamed('loginForm').setContent(loginForm);

		MochiKit.Signal.connect(loginForm, 'doLogin', MochiKit.Base.method(this, 'doLogin', loginForm));
		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'doLogin', MochiKit.Base.method(this, 'doLogin', loginForm));
	},

	//-----------------------------------------------------------------------------

	'doLogin': function(aLoginForm, anEvent) {
		var deferredResult;
		var	parameters;
		var loginProgress;
		var	user;
		var getPassphraseDelegate;

		parameters = anEvent;

		getPassphraseDelegate = MochiKit.Base.partial(MochiKit.Async.succeed, parameters.passphrase);
		user = new Clipperz.PM.DataModel.User({'username':parameters.username, 'getPassphraseFunction':MochiKit.Base.method(Clipperz.PM.RunTime.mainController, 'getPassphrase')});

		loginProgress = new Clipperz.PM.UI.Web.Components.LoginProgress();

		deferredResult = new Clipperz.Async.Deferred("LoginController.doLogin", {trace:false});
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'initProgress', {'steps':4});
		deferredResult.addMethod(Clipperz.PM.RunTime.mainController, 'setPassphraseDelegate', getPassphraseDelegate);
		deferredResult.addMethod(loginProgress, 'deferredShowModal', {deferredObject:deferredResult, openFromElement:aLoginForm.submitButtonElement()});
		deferredResult.addMethod(Clipperz.Crypto.PRNG.defaultRandomGenerator(), 'deferredEntropyCollection');
		deferredResult.addMethod(user, 'login');
		deferredResult.addCallback(function(aLoginProgress, res) {
			aLoginProgress.disableCancel();
			return res;
		}, loginProgress);
		deferredResult.addCallback(function () {
			MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'CARDS_CONTROLLER_DID_RUN',	MochiKit.Base.method(loginProgress, 'deferredHideModalAndRemove', {closeToElement:MochiKit.DOM.currentDocument().body}));
		})
		deferredResult.addMethod(this, 'userLoggedIn', user, loginProgress, aLoginForm);
		deferredResult.addErrback (MochiKit.Base.method(this, 'handleFailedLogin', loginProgress));

		deferredResult.addErrback (MochiKit.Base.method(loginProgress, 'deferredHideModalAndRemove', {closeToElement:aLoginForm.submitButtonElement()}));
		deferredResult.addErrbackPass (MochiKit.Base.method(aLoginForm, 'focusOnPassphraseField'));
		deferredResult.addBoth(MochiKit.Base.method(Clipperz.PM.RunTime.mainController, 'removePassphraseDelegate', getPassphraseDelegate));
		deferredResult.callback();

		MochiKit.Signal.connect(loginProgress, 'cancelEvent', deferredResult, 'cancel');

		return deferredResult;
	},

	//-----------------------------------------------------------------------------

	'userLoggedIn': function(aUser) {
//Clipperz.log(">>> LoginController.userLoggedIn");
		MochiKit.Signal.signal(this, 'userLoggedIn', {user: aUser});
//Clipperz.log("<<< LoginController.userLoggedIn");
	},

	//=========================================================================

	'handleCreateNewAccountClick': function (aComponent) {
//		return Clipperz.PM.DataModel.User.registerNewAccount("new", "user");
		return Clipperz.Async.callbacks("LoginController.handleCreateNewAccountClick", [
//'			MochiKit.Base.method(this, 'newUserCreationComponent'),
//			MochiKit.Base.methodcaller('deferredShowModal', {openFromElement:aComponent}),
//			MochiKit.Base.method(this.newUserWizardController(), 'run')


			MochiKit.Base.method(this, 'newUserCreationComponent'),
			Clipperz.Async.forkAndJoin("Async.test succeedingForkedAndWaitDeferrer", [
				MochiKit.Base.method(this.newUserCreationComponent(), 'deferredShowModal', {openFromElement:aComponent, duration:0.5}),
				MochiKit.Base.method(this.newUserWizardController(), 'run')
			], {trace:false}),
//			MochiKit.Base.method(this.newUserCreationComponent(), 'enableCredentialsField')			
		], {trace:false});
	},

	//-----------------------------------------------------------------------------

	'newUserWizardController': function () {
		if (this._newUserWizardController == null) {
			this._newUserWizardController = new Clipperz.PM.UI.Web.Controllers.NewUserWizardController({
				'newUserCreationComponent': this.newUserCreationComponent()
			})

//			MochiKit.Signal.connect(this._newUserWizardController, 'exit',	this, 'handleHideNewUserCreationComponent');
			MochiKit.Signal.connect(this._newUserWizardController, 'done',	this, 'handleCompleteNewUserCreationComponent');
		}
		
		return this._newUserWizardController;
	},

	//-------------------------------------------------------------------------

	'newUserCreationComponent': function () {
		if (this._newUserCreationComponent == null) {
			this._newUserCreationComponent = new Clipperz.PM.UI.Web.Components.NewUserCreationComponent();
		}
		
		return this._newUserCreationComponent;
	},

	'clearNewUserCreationComponent': function () {
		if (this._newUserCreationComponent != null) {
			this._newUserCreationComponent.clear();
		}
		this._newUserCreationComponent = null;
	},

	//-------------------------------------------------------------------------

	'handleHideNewUserCreationComponent': function () {
		this.clearNewUserCreationComponent();
	},

	'handleCompleteNewUserCreationComponent': function (someParameters) {
		var	deferredResult;
		var	user;
		var	newUserCreationComponent;

		user = someParameters.user;
		newUserCreationComponent = this.newUserCreationComponent();
		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'CARDS_CONTROLLER_DID_RUN',	MochiKit.Base.method(newUserCreationComponent, 'deferredHideModal', {closeToElement:MochiKit.DOM.currentDocument().body})),

		deferredResult = new Clipperz.Async.Deferred("LoginController.handleCompleteNewUserCreationComponent", {trace:false});

		deferredResult.addCallbackList([
			MochiKit.Base.method(Clipperz.Crypto.PRNG.defaultRandomGenerator(), 'deferredEntropyCollection'),
			MochiKit.Base.method(user, 'login'),
			MochiKit.Base.method(this, 'userLoggedIn', user),
			MochiKit.Base.method(this, 'clearNewUserCreationComponent')
		]);
		deferredResult.addErrback(function (aValue) { Clipperz.log("WTF!! Error doing the login after creating a new user" + aValue)});
		deferredResult.callback();
		
		return deferredResult;
	},


	//=========================================================================

	'handleFailedLogin': function(aLoginProgress, anError) {
		var result;

		if (anError instanceof MochiKit.Async.CancelledError) {
			result = anError;
		} else {
			var deferredResult;
			
Clipperz.logError("## MainController - FAILED LOGIN: " + anError);
			deferredResult = new MochiKit.Async.Deferred();

			aLoginProgress.showErrorMessage("failed login");
//			Clipperz.NotificationCenter.register(loginProgress, 'cancelEvent', deferredResult, 'callback');
			MochiKit.Signal.connect(aLoginProgress, 'cancelEvent', deferredResult, 'callback');
			deferredResult.addCallback(MochiKit.Async.fail, anError)
			result = deferredResult;
		}
	
		return result;
	},

	'handleGenericError': function(anError) {
		var result;
		
		if (anError instanceof MochiKit.Async.CancelledError) {
			result = anError;
		} else {
Clipperz.logError("## MainController - GENERIC ERROR" + "\n" + "==>> " + anError + " <<==\n" + anError.stack);
			result = new MochiKit.Async.CancelledError(anError);
		}
	
		return result;
	},

	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
