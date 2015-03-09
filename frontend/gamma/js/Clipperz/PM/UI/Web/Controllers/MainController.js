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

Clipperz.PM.UI.Web.Controllers.MainController = function(args) {
	this._args = args;

	//	controllers
	this._loginController =	null;
	this._appController =	null;

	//	components
	this._headerComponent = null;
	this._pageComponent =	null;
	this._footerComponent = null;
	
	this._passphraseDelegateLock = new MochiKit.Async.DeferredLock();
	this._passphraseDelegateLock.acquire();
//Clipperz.log('MainController init _passphraseDelegateLock', this._passphraseDelegateLock);
	this._passphraseDelegate = null;

	MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'remoteRequestSent',		this, 'handleRemoteRequestSent');
	MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'remoteRequestReceived',	this, 'handleRemoteRequestReceived');

	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.Web.Controllers.MainController.prototype, {

	'toString': function() {
		return "Clipperz.PM.UI.Web.Controllers.MainController";
	},

	'args': function () {
		return this._args;
	},

	//-----------------------------------------------------------------------------

	'headerComponent': function() {
		if (this._headerComponent == null) {
			this._headerComponent = new Clipperz.PM.UI.Web.Components.PageHeader();
		}
		
		return this._headerComponent;
	},

	'footerComponent': function() {
		if (this._footerComponent == null) {
			this._footerComponent = new Clipperz.PM.UI.Web.Components.PageFooter();
		}
		
		return this._footerComponent;
	},

	//-----------------------------------------------------------------------------

	'pageComponent': function() {
		if (this._pageComponent == null) {
			this._pageComponent = new Clipperz.PM.UI.Web.Components.Page({element:MochiKit.DOM.getElement('mainDiv')});
		}
		
		return this._pageComponent;
	},

	//-----------------------------------------------------------------------------

	'loginController': function() {
		if (this._loginController == null) {
			this._loginController = new Clipperz.PM.UI.Web.Controllers.LoginController(this.args());

			MochiKit.Signal.connect(this._loginController, 'userLoggedIn', this, 'loginControllerUserLoggedInCallback');
		}

		return this._loginController;
	},

	'appController': function() {
		if (this._appController == null) {
			this._appController = new Clipperz.PM.UI.Web.Controllers.AppController();

			MochiKit.Signal.connect(this._appController, 'logout', this, 'handleLogout');
		}

		return this._appController;
	},

	//-----------------------------------------------------------------------------

	'run': function(shoudShowRegistrationForm) {
		this.pageComponent().slotNamed('header').setContent(this.headerComponent());
		this.pageComponent().slotNamed('footer').setContent(this.footerComponent());

		this.pageComponent().render();
		
		this.loginController().run({slot:this.pageComponent().slotNamed('body')});
		
		if (shoudShowRegistrationForm) {
			MochiKit.Signal.signal(this.loginController().loginPage(), 'createNewAccountClick');
//			this.loginController().handleCreateNewAccountClick();
		}
	},

	//-----------------------------------------------------------------------------

	'getPassphrase': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("MainController.getPassphrase", {trace:false});

		deferredResult.acquireLock(this._passphraseDelegateLock);
		deferredResult.addMethod(this, 'invokePassphraseDelegate');
		deferredResult.releaseLock(this._passphraseDelegateLock);
		deferredResult.callback();
	
		return deferredResult;
	},
	
	//.........................................................................
	
	'invokePassphraseDelegate': function () {
		return this._passphraseDelegate();
	},

	'passphraseDelegateLock': function () {
		return this._passphraseDelegateLock;
	},

	//.........................................................................

	'setPassphraseDelegate': function (aDelegate) {
		var shouldReleaseLock;

		shouldReleaseLock = (this._passphraseDelegate == null);

		this._passphraseDelegate = aDelegate;

		if (shouldReleaseLock) {
			this._passphraseDelegateLock.release();
		}
	},
	
	//.........................................................................

	'removePassphraseDelegate': function (aDelegate) {
		if (this._passphraseDelegate == aDelegate) {
			this._passphraseDelegate = null;
			this._passphraseDelegateLock.acquire();
		}
	},

	//-------------------------------------------------------------------------

	'loginControllerUserLoggedInCallback': function(anEvent) {
		this.headerComponent().switchToLoggedMode();
		this.appController().run({slot:this.pageComponent().slotNamed('body'), user:anEvent['user']});
	},

	//-----------------------------------------------------------------------------

	'handleRemoteRequestSent': function () {
//Clipperz.log("REMOTE REQUEST sent >>>");
	},

	'handleRemoteRequestReceived': function () {
//Clipperz.log("REMOTE REQUEST received <<<");
	},

	//-----------------------------------------------------------------------------

	'handleLogout': function(anEvent) {
		this.exit('exit.html');
	},

	//-----------------------------------------------------------------------------

	'exit': function(aPageName) {
//Clipperz.log("### exit " + aPageName);
		MochiKit.Async.wait(0).addCallback(function() {
			window.location.href = "./" + aPageName + "?ln=" + Clipperz.PM.Strings.selectedLanguage;
		});
	},

	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
