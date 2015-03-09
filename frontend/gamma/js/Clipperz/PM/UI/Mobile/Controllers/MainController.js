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

Clipperz.Base.module('Clipperz.PM.UI.Mobile.Controllers');

Clipperz.PM.UI.Mobile.Controllers.MainController = function() {
//	this._jQTouch		= null;
	this._user			= null;
	this._proxy			= null;
//	this._overlay		= null;
	this._loginForm 	= null;
	this._cardList		= null;
	this._cardDetail	= null;
	this._preferences	= null;

	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.Mobile.Controllers.MainController.prototype, {

	'toString': function () {
		return "Clipperz.PM.UI.Mobile.Controllers.MainController";
	},

	//-------------------------------------------------------------------------

	'user': function () {
		return this._user;
	},

	'setUser': function (aValue) {
		this._user = aValue;
	},

	//=========================================================================

	'run': function () {
		var	defaultPageStructure;

		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'doLogin',			MochiKit.Base.method(this, 'doLogin'));
		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'showPreferences',	MochiKit.Base.method(this, 'showPreferences'));

		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'savePreferences',	MochiKit.Base.method(this, 'savePreferences'));
		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'back',				MochiKit.Base.method(this, 'back'));

		defaultPageStructure = [
			{tag:'div', 'data-role':'header', 'data-position':'fixed', children:[
				{tag:'h1', html:'clipperz'}
			]},
			{tag:'div', 'data-role':'content'}
		];
		Clipperz.DOM.Helper.insertAfter(MochiKit.DOM.getElement('loadingPage'), [
			{tag:'div', 'data-role':'page', id:'loginPage',			children:defaultPageStructure},
			{tag:'div', 'data-role':'page', id:'cardListPage',		children:defaultPageStructure},
			{tag:'div', 'data-role':'page', id:'cardDetailPage',	children:defaultPageStructure},
			{tag:'div', 'data-role':'page', id:'preferencesPage',	children:defaultPageStructure}
		])
//		$.mobile.initializePage();
		this.showLoginForm();
	},

	//=========================================================================

	'showAddToHomeScreenBaloon': function () {
	},

	//-------------------------------------------------------------------------

	'selectInitialProxy': function () {
		if (this.isOnline()) {
			this._proxy = Clipperz.PM.Proxy.defaultProxy;
		} else {
			if (this.hasLocalData()) {
				this._proxy = new Clipperz.PM.Proxy.OfflineCache({'shouldPayTolls':false});
			} else {
				this.showOfflineError();
			}
		}
	},

	//-------------------------------------------------------------------------

	'showLoginForm': function (args) {
		args = args || {};

		args['callback'] = MochiKit.Base.method(this, 'doLogin');

		if (Clipperz.PM.PIN.isSet()) {
			args['errorCallback'] = MochiKit.Base.method(this, 'handleFailedPinLogin');
			this.loginForm().setMode('PIN');
//			this.loginForm().showPinLogin(args);
		} else {
			args['errorCallback'] = MochiKit.Base.method(this, 'handleFailedCredentialsLogin');
			this.loginForm().setMode('CREDENTIALS');
			//this.loginForm().showCredentialsLogin(args);
		}
		this.loginForm().show(args);

//		MochiKit.Async.callLater(0.1, $.mobile.changePage, $('#loginPage'), {changeHash:false, showLoadMsg:false, role:'page', fromPage:$('#loadingPage'), 'data-transition':'slide'});
		MochiKit.Async.callLater(0.1, $.mobile.changePage, $('#loginPage'));
	},

	//.........................................................................

	'handleFailedCredentialsLogin': function () {
		this.overlay().failed("Failed login", 1);
		this.showLoginForm({'previousFailedAttempt':'LOGIN'});
	},

	//.........................................................................

	'handleFailedPinLogin': function () {
		var	failedAttempts;
		var	status;

		this.overlay().failed("Failed login", 1);
		failedAttempts = Clipperz.PM.PIN.recordFailedAttempt();
		this.showLoginForm({'previousFailedAttempt':'PIN', 'failedAttempts': failedAttempts});
	},

	//-------------------------------------------------------------------------

	'doLogin': function (someArgs) {
		var deferredResult;
		var credentials;
		var errorCallback;
		var user;
		var getPassphraseDelegate;

		credentials = someArgs['credentials'];
		errorCallback = someArgs['errorCallback'] || MochiKit.Base.noop;

		this.overlay().show("logging in");
		getPassphraseDelegate = MochiKit.Base.partial(MochiKit.Async.succeed, credentials.passphrase);
		user = new Clipperz.PM.DataModel.User({'username':credentials.username, 'getPassphraseFunction':getPassphraseDelegate});

		deferredResult = new Clipperz.Async.Deferred('MainController.doLogin', {trace:false});
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'initProgress', {'steps':4});
		deferredResult.addCallback(MochiKit.Async.wait, 0.1);
		deferredResult.addMethod(Clipperz.Crypto.PRNG.defaultRandomGenerator(), 'deferredEntropyCollection');
		deferredResult.addMethod(user, 'login');
		deferredResult.addCallbacks(
			MochiKit.Base.method(this, 'processSuccessfulLogin', user),
			errorCallback
		);
		deferredResult.callback();

		return deferredResult;
	},

	//..........................................................................

	'processSuccessfulLogin': function (aUser) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.processSuccessfulLogin', {trace:false});
		deferredResult.addMethod(Clipperz.PM.PIN, 'resetFailedAttemptCount');
		deferredResult.addMethod(this.overlay(), 'done', "", 1);
//		deferredResult.addMethod(this, 'removeLoginForm');
		deferredResult.addMethod(this, 'setUser', aUser);
		deferredResult.addMethod(this, 'setupApplication');
		deferredResult.addMethod(this, 'runApplication');
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'setupApplication': function () {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("MainController.setupApplication", {trace:false});
		deferredResult.addMethod(this, 'welcomeFirstTimeUser');
		deferredResult.addMethod(this, 'showPaymentReminder');
		deferredResult.addMethod(this, 'copyDataLocally');
		deferredResult.callback(arguments);

		return deferredResult;
	},

	//..........................................................................

	'isFirstTimeUser': function () {
		return false;
	},

	'welcomeFirstTimeUser': function () {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.welcomeFirstTimeUser', {trace:false});

		if (this.isFirstTimeUser()) {
			deferredResult.addCallback(function () { Clipperz.log("--> welcome"); });
		}
		deferredResult.callback();

		return deferredResult;
	},

	//..........................................................................

	'shouldShowPaymentReminder': function () {
		return true;
	},

	'showPaymentReminder': function () {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.showPaymentReminder', {trace:false});

		if (this.shouldShowPaymentReminder()) {
			deferredResult.addCallback(function () { Clipperz.log("--> payment reminder"); });
		}
		deferredResult.callback();

		return deferredResult;
	},

	//..........................................................................

	'canCopyDataLocally': function () {
		return false;
	},

	'copyDataLocally': function () {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.copyDataLocally', {trace:false});

		if (this.canCopyDataLocally()) {
			deferredResult.addCallback(function () { Clipperz.log("--> copy data locally"); });
		}
		deferredResult.callback();

		return deferredResult;

	},

	//-------------------------------------------------------------------------

	'runApplication': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.runApplication', {trace:false});
		deferredResult.addMethod(this, 'showCardListPage');
		deferredResult.addCallback(MochiKit.Async.wait, 0.5);
		deferredResult.addMethod(this.user(), 'getRecords');
//		deferredResult.addMethod(this, 'showCards');
		deferredResult.addMethod(this.cardList(), 'showCards');
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'showOfflineError': function (anException) {
		alert("Error: " + anException);
		throw anException;
	},

	//=========================================================================

	'isOnline': function() {
		return navigator.onLine;
	},

	'hasLocalData': function() {
		return false;
	},
	
	//=========================================================================

	'overlay': function () {
		if (this._overlay == null) {
			this._overlay = new Clipperz.PM.UI.Mobile.Components.Overlay();
		}
		
		return this._overlay;
	},

	//-------------------------------------------------------------------------

	'contentElementForPage': function (aPageName) {
		return MochiKit.Selector.findDocElements('#' + aPageName + ' div[data-role="content"]')[0];
	},

	'loginForm': function () {
		if (this._loginForm == null) {
			this._loginForm = new Clipperz.PM.UI.Mobile.Components.LoginForm({element:this.contentElementForPage('loginPage')});
		}
		
		return this._loginForm;
	},

	'removeLoginForm': function () {
		if (this._loginForm != null) {
			this._loginForm.remove();
			this._loginForm = null;
		}
	},

	//-------------------------------------------------------------------------

	'cardList': function () {
		if (this._cardList == null) {
			this._cardList = new Clipperz.PM.UI.Mobile.Components.CardList({element:this.contentElementForPage('cardListPage')});
			MochiKit.Signal.connect(this._cardList, 'selectedCard', this, 'selectCardHandler');
		}

		return this._cardList;
	},

	'showCardListPage': function (someCards) {
//		this.cardList().render();
		$.mobile.changePage($('#cardListPage'), {'transition':'flow'});	//	slide, flow, pop
	},

	//-------------------------------------------------------------------------

	'cardDetail': function () {
		if (this._cardDetail == null) {
			this._cardDetail = new Clipperz.PM.UI.Mobile.Components.CardDetail({element:MochiKit.DOM.getElement('cardDetail')});
		}

		return this._cardDetail;
	},

	'selectCardHandler': function (aCardReference) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("MainController.selectCardHandler", {trace:false});
		deferredResult.addMethod(this.cardDetail(), 'render');
		deferredResult.addMethod(this.user(), 'getRecord', aCardReference);
		deferredResult.addMethod(this.cardDetail(), 'showCard');
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'preferences': function () {
		if (this._preferences == null) {
			this._preferences = new Clipperz.PM.UI.Mobile.Components.Preferences({element:this.contentElementForPage('preferencesPage')});
		}

		return this._preferences;
	},

	'showPreferences': function (anEvent) {
//console.log("MainController.showPreferences", anEvent);
		this.preferences();
//		MochiKit.Async.callLater(0.1, $.mobile.changePage, $('#preferencesPage'), {transition:'flip'});
		$.mobile.changePage($('#preferencesPage'), {transition:'flip'});
	},

	'savePreferences': function (anEvent) {
console.log("MainController.savePreferences", anEvent);
	},

	'back': function (anEvent) {
//		MochiKit.Async.callLater(0.1, $.mobile.changePage, $('#cardListPage'), {transition:'flip', reverse:true});
		$.mobile.changePage($('#cardListPage'), {transition:'flip', reverse:true});
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});
