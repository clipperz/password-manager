/*

Copyright 2008-2013 Clipperz Srl

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

Clipperz.Base.module('Clipperz.PM.UI');

Clipperz.PM.UI.MainController = function() {
	var pages;

//	this._proxy		= null;
	this._user		= null;
	this._filter	= '';

//	this._currentPage = 'loadingPage';

	this._pageStack = ['loadingPage'];
	this._overlay =  new Clipperz.PM.UI.Components.Overlay();
	pages = {
		'loginPage':		new Clipperz.PM.UI.Components.LoginForm(),
		'registrationPage':	new Clipperz.PM.UI.Components.RegistrationWizard(),
		'cardListPage':		new Clipperz.PM.UI.Components.CardList(),
		'cardDetailPage':	new Clipperz.PM.UI.Components.CardDetail({card: {}}),
		'preferencePage':	new Clipperz.PM.UI.Components.PreferencePage(),
		'errorPage':		new Clipperz.PM.UI.Components.ErrorPage({message:''})
	};

	MochiKit.Base.map(function (anId) {React.renderComponent(pages[anId], MochiKit.DOM.getElement(anId))}, MochiKit.Base.keys(pages));
	this._pages = pages;
	this.registerForNotificationCenterEvents();
	MochiKit.Signal.connect(MochiKit.DOM.currentDocument(), 'onselectionchange', this, 'selectionChangeHandler');

	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.MainController.prototype, {

	toString: function () {
		return "Clipperz.PM.UI.MainController";
	},

	//=========================================================================

	overlay: function () {
		return this._overlay;
	},

	loginForm: function () {
		return this._loginForm;
	},

	registrationWizard: function () {
		return this._registrationWizard;
	},

	//=========================================================================

	isOnline: function() {
		return navigator.onLine;
//		return false;
	},

	hasLocalData: function() {
//		return false;
		return (Clipperz.PM.DataModel.devicePreferences.accountData() != null);
	},

	loginMode: function () {
		//	PIN is set using this command:
		//	Clipperz.PM.PIN.setCredentialsWithPIN('1234', {'username':'joe', 'passphrase':'clipperz'});

		return Clipperz.PM.PIN.isSet() ? 'PIN' : 'CREDENTIALS';
	},

	//=========================================================================

	pages: function () {
		return this._pages;
	},

	pageStack: function () {
		return this._pageStack;
	},

	//=========================================================================

	showOfflineError: function () {
console.log("THE BROWSER IS OFFLINE");
	},

	selectInitialProxy: function () {
		if (this.isOnline()) {
//			this._proxy = Clipperz.PM.Proxy.defaultProxy;
		} else {
			if (this.hasLocalData()) {
//				this._proxy = new Clipperz.PM.Proxy.Offline({dataStore: new Clipperz.PM.Proxy.Offline.LocalStorageDataStore(), shouldPayTolls:false});
				Clipperz.PM.Proxy.defaultProxy = new Clipperz.PM.Proxy.Offline({dataStore: new Clipperz.PM.Proxy.Offline.LocalStorageDataStore(), shouldPayTolls:false});
			} else {
				this.showOfflineError();
			}
		}
	},

//	proxy: function () {
//		return this._proxy;
//	},

	//=========================================================================

	registerForNotificationCenterEvents: function () {
		var	events	= [
			'doLogin',
			'registerNewUser',
			'showRegistrationForm',
			'goBack',
			'showRecord',
			'searchCards',
			'showPreferences',
			'runDirectLogin',
			'synchronizeLocalData'
		];
		var	self	= this;

		MochiKit.Base.map(function (anEvent) {
			MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, anEvent, MochiKit.Base.method(self, anEvent));
		}, events);

//		MochiKit.Signal.connect(window, 'onpopstate',		MochiKit.Base.method(this, 'historyGoBack'));
		MochiKit.Signal.connect(window, 'onbeforeunload',	MochiKit.Base.method(this, 'shouldExitApp'));
	},

	//-------------------------------------------------------------------------

	selectionChangeHandler: function (anEvent) {
		var	selection;
		var	selectionRange;
		var	selectionNode;
		var	valueElement;
//	other hints: http://www.bearpanther.com/2013/05/27/easy-text-selection-in-mobile-safari/
//	SELECTION:   https://developer.mozilla.org/en-US/docs/Web/API/Selection
//	RANGE:       https://developer.mozilla.org/en-US/docs/Web/API/Range
//	NODE TYPES:  https://developer.mozilla.org/en-US/docs/Web/API/Node.nodeType

		selection = MochiKit.DOM.currentWindow().getSelection();
//console.log("-- selection", selection);
		selectionRange = selection.getRangeAt(0);
		selectionNode = selectionRange.startContainer.childNodes[selectionRange.startOffset];
//console.log("-- selectionNode", selectionNode);

		if (selectionNode != undefined) {
			valueElement = MochiKit.DOM.getFirstElementByTagAndClassName('*', 'value', selectionNode);
//console.log("-- valueElement", valueElement);
		}

		if ((valueElement != null) && (valueElement != selectionNode)) {
			var range;
			range = MochiKit.DOM.currentDocument().createRange();
			range.selectNodeContents(valueElement);
			selection.removeAllRanges();
			selection.addRange(range);

			anEvent.preventDefault();
			anEvent.stopPropagation();

//console.log("updated selection", MochiKit.DOM.currentWindow().getSelection());
		}
//console.log("-----------");
	},

	//-------------------------------------------------------------------------

	run: function (parameters) {
		var shouldShowRegistrationForm;
		var	canRegisterNewUsers;

		canRegisterNewUsers = Clipperz.PM.Proxy.defaultProxy.canRegisterNewUsers();

		this.selectInitialProxy();
		shouldShowRegistrationForm = parameters['shouldShowRegistrationForm'] && canRegisterNewUsers;
		this.pages()['loginPage'].setProps({'mode':this.loginMode(), 'isNewUserRegistrationAvailable':canRegisterNewUsers});

		if (shouldShowRegistrationForm) {
			this.showRegistrationForm();
		} else {
			this.showLoginForm();
		}
		this.overlay().done("", 0.5);
	},

	//-------------------------------------------------------------------------

	showLoginForm: function () {
		var	loginFormPage;

		loginFormPage = this.pages()['loginPage'];
		loginFormPage.setProps({'mode':this.loginMode(), 'isNewUserRegistrationAvailable':Clipperz.PM.Proxy.defaultProxy.canRegisterNewUsers()});
		this.moveInPage(this.currentPage(), 'loginPage');
		MochiKit.Async.callLater(0.5, MochiKit.Base.method(loginFormPage, 'setInitialFocus'));
	},

	showRegistrationForm: function () {
		var currentPage;
		var	registrationPage;

		currentPage = this.currentPage();
		registrationPage = this.pages()['registrationPage'];
		this.setCurrentPage('loginPage');
		registrationPage.setProps({});
		this.moveInPage(currentPage, 'registrationPage');
		MochiKit.Async.callLater(0.5, MochiKit.Base.method(registrationPage, 'setInitialFocus'));
	},

	//=========================================================================

	doLogin: function (event) {
		var	credentials;
		var getPassphraseDelegate;
		var	user;

		user = null;

		this.overlay().show("logging in");
		this.pages()['loginPage'].setProps({disabled:true});

		if ('pin' in event) {
			credentials = Clipperz.PM.PIN.credentialsWithPIN(event['pin']);
		} else {
			credentials = event;
		}
		getPassphraseDelegate = MochiKit.Base.partial(MochiKit.Async.succeed, credentials.passphrase);
		user = new Clipperz.PM.DataModel.User({'username':credentials.username, 'getPassphraseFunction':getPassphraseDelegate});

		deferredResult = new Clipperz.Async.Deferred('MainController.doLogin', {trace:false});
		deferredResult.addCallback(MochiKit.Async.wait, 0.1);
		deferredResult.addMethod(Clipperz.Crypto.PRNG.defaultRandomGenerator(), 'deferredEntropyCollection');
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(Clipperz.PM.PIN, 'resetFailedAttemptCount');
		deferredResult.addMethod(this, 'setUser', user);

//		deferredResult.addMethod(this, 'setupApplication');
		deferredResult.addMethod(this, 'runApplication');
		deferredResult.addMethod(this.overlay(), 'done', "", 1);
		deferredResult.addErrback(MochiKit.Base.method(this, 'genericErrorHandler', event));
		deferredResult.addErrback(MochiKit.Base.bind(function (anEvent, anError) {
			if (anError['isPermanent'] != true) {
				this.pages()['loginPage'].setProps({disabled:false, 'mode':this.loginMode()});
				this.pages()['loginPage'].setInitialFocus();
			}
			return anError;
		}, this, event))
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	registerNewUser: function (credentials) {
		var	deferredResult;

		this.overlay().show("creating user");

		this.pages()['registrationPage'].setProps({disabled:true});
		deferredResult = new Clipperz.Async.Deferred('MainController.registerNewUser', {trace:false});
		deferredResult.addCallback(Clipperz.PM.DataModel.User.registerNewAccount,
			credentials['username'],
			MochiKit.Base.partial(MochiKit.Async.succeed, credentials['passphrase'])
		);
		deferredResult.addMethod(this, 'doLogin', credentials);
		deferredResult.addErrback(MochiKit.Base.method(this, 'genericErrorHandler', event));
		deferredResult.addErrback(MochiKit.Base.bind(function (anError) {
			if (anError['isPermanent'] != true) {
				this.pages()['registrationPage'].setProps({disabled:false});
				this.pages()['registrationPage'].setInitialFocus();
			}
			return anError;
		}, this));

		deferredResult.callback();
		
		return deferredResult;

	},

	//-------------------------------------------------------------------------

	user: function () {
		return this._user;
	},

	setUser: function (aUser) {
		this._user = aUser;
		return this._user;
	},

	//=========================================================================

	allCardInfo: function () {
		var deferredResult;
		var	cardInfo;

		cardInfo = {
			'_rowObject':			MochiKit.Async.succeed,
			'_reference':			MochiKit.Base.methodcaller('reference'),
			'_searchableContent':	MochiKit.Base.methodcaller('searchableContent'),
			'label':				MochiKit.Base.methodcaller('label'),
			'favicon':				MochiKit.Base.methodcaller('favicon')
		};

		deferredResult = new Clipperz.Async.Deferred('MainController.allCardInfo', {trace:false});
		deferredResult.addMethod(this.user(), 'getRecords');
		deferredResult.addCallback(MochiKit.Base.map, Clipperz.Async.collectResults("CardList.value - collectResults", cardInfo, {trace:false}));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.callback();

		return deferredResult;
	},

	filterCards: function (someCardInfo) {
		var filter;
		var	filterRegExp;
		var	result;

		filter = this.filter().replace(/[^A-Za-z0-9]/g, "\\$&");
		filterRegExp = new RegExp(filter, "i");
		result = MochiKit.Base.filter(function (aCardInfo) { return filterRegExp.test(aCardInfo['_searchableContent'])}, someCardInfo);

		return result;
	},

	sortCards: function (someCardInfo) {
		return someCardInfo.sort(Clipperz.Base.caseInsensitiveKeyComparator('label'));
	},

	showRecordList: function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.showRecordList', {trace:false});
		deferredResult.addMethod(this, 'allCardInfo');
		deferredResult.addMethod(this, 'filterCards');
		deferredResult.addMethod(this, 'sortCards');
		deferredResult.addCallback(MochiKit.Base.bind(function (someRecordInfo) {
			this.pages()['cardListPage'].setProps({cardList: someRecordInfo});
		}, this));
		deferredResult.callback();

		return deferredResult;
	},

	filter: function ()	{
		return this._filter;
	},

	setFilter: function (aValue) {
		this._filter = aValue;
	},

	searchCards: function (someParameters) {
//console.log("SEARCH CARDS", someParameters);
		this.setFilter(someParameters);
		this.showRecordList();
	},

	//=========================================================================

	runApplication: function () {
		MochiKit.Signal.connect(window, 'onpopstate',	MochiKit.Base.method(this, 'historyGoBack'));
///		TODO: remove this TEST HACK
		this.moveInPage(this.currentPage(), 'cardListPage');
		return this.showRecordList();

//		this.moveInPage(this.currentPage(), 'preferencePage');
	},

	showRecord: function (aRecordReference) {
//console.log("Show Record", aRecordReference);
		var	deferredResult;

		this.pages()['cardListPage'].setProps({selectedCard:aRecordReference});
		deferredResult = new Clipperz.Async.Deferred('MainController.runApplication', {trace:false});
		deferredResult.addMethod(this.user(), 'getRecord', aRecordReference);
		deferredResult.addMethodcaller('content');
		deferredResult.addCallback(MochiKit.Base.bind(function (aCard) {
//console.log("CARD DETAILS", aCard);
			this.pages()['cardDetailPage'].setProps({card: aCard});
			this.pages()['cardListPage'].setProps({selectedCard: null});
		}, this));
		deferredResult.addMethod(this, 'moveInPage', this.currentPage(), 'cardDetailPage', true);
		deferredResult.callback();

		return deferredResult;
	},

	runDirectLogin: function (someParameters) {
//console.log("RUN DIRECT LOGIN", someParameters);
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.runDirectLogin', {trace:false});
		deferredResult.addMethod(this.user(), 'getRecord', someParameters['record']);
		deferredResult.addMethodcaller('directLoginWithReference', someParameters['directLogin']);
		deferredResult.addCallback(Clipperz.PM.UI.DirectLoginRunner.openDirectLogin);
		deferredResult.callback();

		return deferredResult;
	},

	shouldExitApp: function (anEvent) {
//console.log("SHOULD EXIT APP");
		anEvent.preventDefault();
		anEvent.stopPropagation();
	},

	//=========================================================================

	showPreferences: function (anEvent) {
		var	deferredResult;

		this.pages()['preferencePage'].setProps({});
		deferredResult = new Clipperz.Async.Deferred('MainController.showPreferences', {trace:false});
		deferredResult.addMethod(this, 'moveInPage', this.currentPage(), 'preferencePage', true);
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	genericErrorHandler: function (anEvent, anError) {
		var errorMessage;
		var	result;

		result = anError;
		errorMessage = "login failed";

		if (anError['isPermanent'] === true) {
			this.pages()['errorPage'].setProps({message:anError.message});
			this.moveInPage(this.currentPage(), 'errorPage');
			errorMessage = "failure";
		} else {
			if ('pin' in anEvent) {
				errorCount = Clipperz.PM.PIN.recordFailedAttempt();
				if (errorCount == -1) {
					errorMessage = "PIN resetted";
				}
			}
		}
		this.overlay().failed(errorMessage, 1);

		return result;
	},

	//=========================================================================

	slidePage: function (fromPage, toPage, direction) {
		var	fromPosition;
		var toPosition;

		if (direction == "LEFT") {
			fromPosition = 'right';
			toPosition = 'left'
		} else {
			fromPosition = 'left';
			toPosition = 'right'
		}

		MochiKit.DOM.addElementClass(fromPage, toPosition + ' transition');

		MochiKit.DOM.addElementClass(toPage, fromPosition);
		MochiKit.DOM.removeElementClass(toPage, toPosition);
		MochiKit.DOM.addElementClass(toPage, 'transition');
		MochiKit.Async.callLater(0.1, function () {
			MochiKit.DOM.removeElementClass(toPage, fromPosition);
		})

		MochiKit.Async.callLater(0.5, function () {
			MochiKit.DOM.removeElementClass(fromPage, 'transition');
			MochiKit.DOM.removeElementClass(toPage, 'transition');
		})
	},

	rotateInPage: function (fromPage, toPage) {
		//	Broken! :(
		MochiKit.DOM.addElementClass(MochiKit.DOM.getElement('mainDiv'), 'show-right');
	},

	//.........................................................................

	goBack: function () {
		var	fromPage;
		var toPage;

		fromPage = this.pageStack().shift();
		toPage = this.currentPage();
		this.pages()[toPage].setProps({});
		this.moveOutPage(fromPage, toPage);
	},

	historyGoBack: function (anEvent) {
		anEvent.preventDefault();
		anEvent.stopPropagation();
		this.goBack();
	},

	currentPage: function () {
		return this.pageStack()[0];
	},

	setCurrentPage: function (aPage) {
		this.pageStack().unshift(aPage);
	},

	moveInPage: function (fromPage, toPage, addToHistory) {
		var	shouldAddItemToHistory;

		shouldAddItemToHistory = typeof(addToHistory) == 'undefined' ? false : addToHistory;

		this.slidePage(MochiKit.DOM.getElement(fromPage), MochiKit.DOM.getElement(toPage), 'LEFT');
		this.setCurrentPage(toPage);

		if (shouldAddItemToHistory) {
//console.log("ADD ITEM TO HISTORY");
//console.log("ADD ITEM TO HISTORY - window", window);
//console.log("ADD ITEM TO HISTORY - window.history", window.history);
			window.history.pushState({'fromPage': fromPage, 'toPage': toPage});
//#			window.history.pushState();
//console.log("ADDED ITEM TO HISTORY");
		} else {
//console.log("Skip HISTORY");
		}
	},

	moveOutPage: function (fromPage, toPage) {
		this.slidePage(MochiKit.DOM.getElement(fromPage), MochiKit.DOM.getElement(toPage), 'RIGHT');
		this.setCurrentPage(toPage);
	},

	//=========================================================================

	synchronizeLocalData: function (anEvent) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.synchronizeLocalData', {trace:true});
//		deferredResult.addMethod(this.proxy(), 'message', 'downloadAccountData', {});
		deferredResult.addMethod(this.user().connection(), 'message', 'downloadAccountData', {});
		deferredResult.addCallback(function (aResult) {
			Clipperz.PM.DataModel.devicePreferences.setAccountDataWityResponse(aResult);
//			localStorage.setItem('clipperz_dump_data', aResult['data']);
//			localStorage.setItem('clipperz_dump_version', aResult['version']);
//			localStorage.setItem('clipperz_dump_date', new Date());
		})
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================
/*
	wrongAppVersion: function (anError) {
//		this.pages()['errorPage'].setProps({message:anError.message});
//		this.moveInPage('errorPage', this.currentPage());
	},
*/
	//=========================================================================
	__syntaxFix__: "syntax fix"
});
