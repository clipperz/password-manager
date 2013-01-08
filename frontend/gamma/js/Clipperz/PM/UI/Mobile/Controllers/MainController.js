/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz Community Edition.
Clipperz Community Edition is an online password manager.
For further information about its features and functionalities please
refer to http://www.clipperz.com.

* Clipperz Community Edition is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Clipperz Community Edition is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Clipperz Community Edition.  If not, see
  <http://www.gnu.org/licenses/>.

*/

Clipperz.Base.module('Clipperz.PM.UI.Mobile.Controllers');

Clipperz.PM.UI.Mobile.Controllers.MainController = function() {
	this._jQTouch		= null;
	this._user			= null;
	this._proxy			= null;
	this._loginForm 	= null;
	this._cardList		= null;
	this._cardDetail	= null;

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

	//-------------------------------------------------------------------------

	'jQTouch': function () {
		return this._jQTouch;
	},

	'setJQTouch': function (aValue) {
		this._jQTouch = aValue;
	},

	//=========================================================================

	'run': function () {
		console.log("MainController.run");

		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'doLogin', MochiKit.Base.method(this, 'doLogin'));
		Clipperz.DOM.Helper.overwrite(MochiKit.DOM.currentDocument().body, {tag:'div', id:'jqt', children:[
			{tag:'div', id:'loginForm'},
			{tag:'div', id:'cardList'},
			{tag:'div', id:'cardDetail'},
			{tag:'div', id:'preferences'}
		]});

		this.showLoginForm();

		this.initjQTouch();


//		this.showAddToHomeScreenBaloon();
//		this.selectInitialProxy();
	},

	'initjQTouch': function () {
		var jqt;

		jqt = new $.jQTouch({
			icon: 'data:image/png;charset=utf-8;base64,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',
//			icon4: 'jqtouch4.png',
//			startupScreen: null,						//	Pass a string path to a 320px x 460px startup screen for full screen apps.
			statusBar: 'black-translucent',				//	Styles the status bar when running as a fullscreen app. Other options are `default`, `black`, and `black-translucent`.
//			addGlossToIcon: true,						//	Set to 'false' to prevent automatic glossy button effect on icon.
			preloadImages: false,						//	Pass an array of image paths to load them before page loads. Ex: `['images/link_over.png', 'images/link_select.png']`
			fixedViewport: true,						//	Removes the user's ability to scale the page. Ensures the site behaves more like an application.
//			fullScreen: true,							//	The website will become a fullscreen application when saved to a user's home screen. Set to `false` to disable.
//			fullScreenClass: 'fullscreen'				//	Adds a class to the `<body>` when running in full-screen mode, to allow for easy detection and styling. Set to `false` to disable.
//			themeSelectionSelector: '#jqt #themes ul',	//	???

//			useAnimations: true,						//	Set to `false` to disable all animations.
//			useFastTouch: true,							//	Removes ~350ms onClick delay when tapping a link (use in conjunction with the .tap() event)  **Experimental**
//			useTouchScroll: true,						//	Adds support for iOS5 scrolling. Set to false to disable. **Experimental**

			cacheGetRequests: false,						//	Automatically caches GET requests, so subsequent taps reference the pre-loaded views. (default: true)

//			backSelector: '.back, .cancel, .goback',	//	A CSS selector for back links/buttons. When clicked, the page history goes back one, automatically reversing whichever entrance animation was used.

//			cubeSelector: '.cube',						//	Link selector for a cube animation.
//			dissolveSelector: '.dissolve',				//	Link selector for a dissolve animation.
//			fadeSelector: '.fade',						//	Link selector for a fade animation.
//			flipSelector: '.flip',						//	Link selector for a 3d flip animation.
			formSelector: null,							//	Sets which forms are automatically submitted via Ajax. (default: 'form')
//			popSelector: '.pop',						//	Link selector for a pop animation. (default: '.pop')
//			slideSelector: 'body > * > ul li a',		//	Link selector for the default slide-left transition. By default applies to all links within an unordered list. Accepts any jQuery-capable selector `'li &gt; a, a:not(.dontslide)'`, etc. (default: 'body > * > ul li a')
//			slideupSelector: '.slideup',				//	Link selector for a slide up animation. (default: '.slideup')
//			submitSelector: '.submit',					//	Selector which, when clicked, will submit its parent form (and close keyboard if open). (default: '.submit')
//			swapSelector: '.swap',						//	Link selector for 3d swap animation. (default: '.swap')
//			touchSelector: 'a, .touch',					//	Selector for items which are automatically given expanded touch events. This makes ordinary links more responsive and provides trigger events like `swipe` (default: 'a, .touch')

			debug: false
		});

		this.setJQTouch(jqt);
	},

	//=========================================================================

	'showAddToHomeScreenBaloon': function () {
console.log(">>> showAddToHomeScreenBaloon");
	},

	//-------------------------------------------------------------------------

	'selectInitialProxy': function () {
//console.log(">>> selectInitialProxy");
		if (this.isOnline()) {
//console.log("--- selectInitialProxy: using default proxy");
			this._proxy = Clipperz.PM.Proxy.defaultProxy;
		} else {
			if (this.hasLocalData()) {
//console.log("--- selectInitialProxy: using local cache proxy");
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
			this.loginForm().showPinLogin(args);
		} else {
			args['errorCallback'] = MochiKit.Base.method(this, 'handleFailedCredentialsLogin');
			this.loginForm().showCredentialsLogin(args);
		}
	},

	//.........................................................................

	'handleFailedCredentialsLogin': function () {
console.log("LOGIN FAILED");
		this.showLoginForm({'previousFailedAttempt':'LOGIN'});
	},

	//.........................................................................

	'handleFailedPinLogin': function () {
		var	failedAttempts;
		var	status;

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

//console.log(">>> MainController.doLogin", someArgs);
		credentials = someArgs['credentials'];
		errorCallback = someArgs['errorCallback'] || MochiKit.Base.noop;

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

console.log(">>> setupApplication");
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
			deferredResult.addCallback(function () { console.log("--> welcome"); });
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
			deferredResult.addCallback(function () { console.log("--> payment reminder"); });
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
			deferredResult.addCallback(function () { console.log("--> copy data locally"); });
		}
		deferredResult.callback();

		return deferredResult;

	},

	//-------------------------------------------------------------------------

	'runApplication': function () {
		var deferredResult;

//console.log(">>> runApplication");
		deferredResult = new Clipperz.Async.Deferred('MainController.runApplication', {trace:true});
		deferredResult.addMethod(this.user(), 'getRecords');
		deferredResult.addMethod(this, 'showCards');
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

	'loginForm': function() {
		if (this._loginForm == null) {
			this._loginForm = new Clipperz.PM.UI.Mobile.Components.LoginForm({element:MochiKit.DOM.getElement('loginForm')});
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
			this._cardList = new Clipperz.PM.UI.Mobile.Components.CardList({element:MochiKit.DOM.getElement('cardList')});
			MochiKit.Signal.connect(this._cardList, 'selectedCard', this, 'selectCardHandler');
		}

		return this._cardList;
	},

	'showCards': function (someCards) {
		this.cardList().showCards(someCards);
		this.jQTouch().goTo('#cardList', 'slideleft');
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

		deferredResult = new Clipperz.Async.Deferred("MainController.selectCardHandler", {trace:true});
		deferredResult.addMethod(this.cardDetail(), 'render');
		deferredResult.addMethod(this.jQTouch(), 'goTo', '#cardDetail', 'slideleft');
		deferredResult.addMethod(this.user(), 'getRecord', aCardReference);
		deferredResult.addMethod(this.cardDetail(), 'showCard');
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});
