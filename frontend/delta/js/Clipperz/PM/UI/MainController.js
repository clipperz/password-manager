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

"use strict";
Clipperz.Base.module('Clipperz.PM.UI');

Clipperz.PM.UI.MainController = function() {

//	this._proxy		= null;
	this._mediaQueryStyle = "narrow";
	this._user		= null;
	this._filter	= {'type':'ALL'};
	this._notifications = [];

	this._shouldIncludeArchivedCards = false;

	this._isSelectionPanelOpen = false;
	this._isSettingsPanelOpen = false;
//	this._isAttachmentQueueBoxOpen = false;
//	this._isCertificateQueueBoxOpen = false;
	
	this._pageStack = ['loadingPage'];
	this._overlay =  new Clipperz.PM.UI.Components.Overlay();

	this._isTouchDevice = ('ontouchstart' in window || 'onmsgesturechange' in window);
	this._isDesktop = window.screenX != 0 && !this._isTouchDevice;
	this._hasKeyboard = this._isDesktop;

	this._userPreferences = null;
	this._recordsInfo = null;
	this._selectedCards = null;

	this._selectedCardInfo = null;

	this._closeMaskAction = null;

	this._lockListener = null;
	this._lockTimeout = null;

	this._pages = {};
	this.renderPages([
		'loginPage',
		'registrationPage',
		'unlockPage',
		'mainPage',
		'cardDetailPage',
		'errorPage',
	]);

	this.registerForNotificationCenterEvents([
		'doLogin', 'registerNewUser', 'showRegistrationForm', 'goBack',
		'updateUserAccountInfo',
		'logout',
		'enableLock', 'disableLock', 'unlock',
		'updatePIN', 'disablePIN', 'forcePassphraseLogin', 'forcePassphraseUnlock',
		'changePassphrase', 'deleteAccount',
		/*'updateUserPreferences',*/ 'setPreferences',
		'updateOTPListAndDetails', 'createNewOTP', 'deleteOTPs', 'changeOTPLabel',
		'importCards',
		'downloadExport',
		'updateProgress',
		'toggleSelectionPanel', 'hideSelectionPanel', 'toggleSettingsPanel', //	'toggleAttachmentQueueBox', 'toggleCertificateQueueBox',
		'matchMediaQuery', 'unmatchMediaQuery',
		'selectAllCards', 'selectRecentCards', 'selectCardsWithCertificate', 'selectCardsWithAttachments', 'selectUntaggedCards', 'tagSelected', 'search',
		'refreshCardEditDetail',
		'saveCardEdits', 'cancelCardEdits', 'copyFieldValueFeedback',
		'selectCard',
		'addCardClick',
		'deleteCard', 'toggleArchiveCard', 'cloneCard', 'editCard',	//	'createCertificate',
		'downloadCertificate',	'showCertificatePreview', 'hideCertificatePreview', 'showCertificateCard',	//	'closeCertificateNotification'
		'addTag', 'removeTag',
		'showArchivedCards', 'hideArchivedCards',
		'goBackToMainPage',
		'maskClick',
		'closeHelp',
		'downloadOfflineCopy',
		'runDirectLogin', 'removeDirectLogin',
		'exitSearch',
		'addAttachment', 'removeAttachment', 'getAttachment', 'cancelAttachment',	// 'closeAttachmentNotification', 'updateAttachmentQueueInfo', 
		'addNotification', 'acknowledgeNotification',
	]);

	this._attachmentController = new Clipperz.PM.UI.AttachmentController({
		'uploadMessageCallback':      MochiKit.Base.method(this, 'uploadMessageCallback'),
		'downloadMessageCallback':    MochiKit.Base.method(this, 'downloadMessageCallback'),
		'reloadServerStatusCallback': MochiKit.Base.method(this, 'reloadAttachmentServerStatusCallback')
	});
	
//	this._certificateQueueStatus = [];

	document.addEventListener('dragover', MochiKit.Base.method(this, 'blockDragOver'), false);

	Mousetrap.bind(['/'],					MochiKit.Base.method(this, 'focusOnSearch'));

	Mousetrap.bind(['left',  'h', 'esc'],	MochiKit.Base.method(this, 'exitCurrentSelection'));
	Mousetrap.bind(['right', 'l', 'enter'],	MochiKit.Base.method(this, 'selectDetail'));

	Mousetrap.bind(['up',    'k'],			MochiKit.Base.method(this, 'selectPreviousCard'));
	Mousetrap.bind(['down',  'j'],			MochiKit.Base.method(this, 'selectNextCard'));

	Mousetrap.bind(['*'],					MochiKit.Base.method(this, 'selectAllCards_handler'));

	Mousetrap.bind(['?'],					MochiKit.Base.method(this, 'showHelp_handler'));
	
	Mousetrap.bind(['l o c k'],				MochiKit.Base.method(this, 'lockShortcut'));

//	Mousetrap.bind(['t e s t'],				MochiKit.Base.method(this, 'downloadExport_handler'));

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

	updateProgress_handler: function (aProgressPercentage) {
		return Clipperz.Async.callbacks("MainController.updateProgress_handler", [
			MochiKit.Base.method(this.overlay(), 'updateProgress', aProgressPercentage)
		], {trace:false});
	},

	loginForm: function () {
		return this._loginForm;
	},

	registrationWizard: function () {
		return this._registrationWizard;
	},

	//=========================================================================
/*
	proxy: function () {
		return this._proxy;
	},

	setProxy: function (aValue) {
		this._proxy = aValue;
	},
*/
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

		return (Clipperz.PM.PIN.isLocalStorageSupported() && Clipperz.PM.PIN.isSet()) ? 'PIN' : 'CREDENTIALS';
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
Clipperz.log("THE BROWSER IS OFFLINE");
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

	capitaliseFirstLetter: function (aValue) {
	    return aValue.charAt(0).toUpperCase() + aValue.slice(1);
	},
	
	renderPages: function (pages) {
		var self = this;
		MochiKit.Iter.forEach(pages, function (aPageName) {
			self._pages[aPageName] = ReactDOM.render(
				Clipperz.PM.UI.Components.Pages[self.capitaliseFirstLetter(aPageName)](self.pageProperties(aPageName)),
				MochiKit.DOM.getElement(aPageName)
			);
		});
	},

	registerForNotificationCenterEvents: function (events) {
		var	self = this;

		MochiKit.Iter.forEach(events, function (anEvent) {
//			MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, anEvent, MochiKit.Base.method(self, anEvent + '_handler'));
			MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, anEvent, MochiKit.Base.method(self, 'eventHandlerErrorCatcher', anEvent));
			MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, anEvent, MochiKit.Base.method(self, 'resetLockTimeout'));
		});

//		MochiKit.Signal.connect(window, 'onpopstate',		MochiKit.Base.method(this, 'historyGoBack'));
//		MochiKit.Signal.connect(window, 'onbeforeunload',	MochiKit.Base.method(this, 'shouldExitApp'));
	},

	eventHandlerErrorCatcher: function (anEvent, aValue) {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("MainController.eventHandlerErrorCatcher", {trace: false});
		deferredResult.addMethod(this, anEvent + '_handler');
		deferredResult.addErrback(MochiKit.Base.bind(function (anError) {
			if (! anError instanceof MochiKit.Async.CancelledError) {
				this.overlay().hide(true);
				this.moveInPage(this.currentPage(), 'errorPage');
				this.pages()['errorPage'].setProps({'error':anError});
				console.log("CATCHED UNHANDLED ERROR");
			}
			return anError;
		}, this));
		deferredResult.callback(aValue);
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------
/*
	selectionChange_handler: function (anEvent) {
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
*/
	//-------------------------------------------------------------------------

	run: function (parameters) {
		var shouldShowRegistrationForm;
		var	canRegisterNewUsers;

		canRegisterNewUsers = Clipperz.PM.Proxy.defaultProxy.canRegisterNewUsers();
		this.selectInitialProxy();
		shouldShowRegistrationForm = parameters['shouldShowRegistrationForm'] && canRegisterNewUsers;

		this.showLoginForm();
		if (shouldShowRegistrationForm) {
			this.showRegistrationForm_handler();
		}
	
//		this.overlay().done("", 0.5);
		this.overlay().hide();
	},

	//-------------------------------------------------------------------------
	
	checkPassphrase: function( passphraseIn ) {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("MainController.checkPassphrase", {trace: false});
		deferredResult.addMethod(this.user(), 'getPassphrase');
		deferredResult.addCallback(function (candidatePassphrase, realPassphrase) { return candidatePassphrase == realPassphrase; }, passphraseIn );

		deferredResult.callback();
		
		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	showLoginForm: function () {
		var	loginFormPage;

		loginFormPage = this.pages()['loginPage'];
//		loginFormPage.setProps({'mode':this.loginMode(), 'isNewUserRegistrationAvailable':Clipperz.PM.Proxy.defaultProxy.canRegisterNewUsers()});
		loginFormPage.setProps({'mode':this.loginMode()});
		this.moveInPage(this.currentPage(), 'loginPage');
		MochiKit.Async.callLater(0.5, MochiKit.Base.method(loginFormPage, 'setInitialFocus'));
		MochiKit.Async.callLater(0.5, MochiKit.Base.method(this, 'recursivelyPollLoginFormForChanges'));
	},

	recursivelyPollLoginFormForChanges: function () {
		this.pages()['loginPage'].pollForChanges();

		if (this.currentPage() == 'loginPage') {
			MochiKit.Async.callLater(0.5, MochiKit.Base.method(this, 'recursivelyPollLoginFormForChanges'));
		}
	},

	showRegistrationForm_handler: function () {
		var currentPage;
		var	registrationPage;

		currentPage = this.currentPage();
		registrationPage = this.pages()['registrationPage'];

		return Clipperz.Async.callbacks("MainController.showRegistrationForm_handler", [
			MochiKit.Base.method(this, 'setCurrentPage', 'loginPage'),
			MochiKit.Base.method(registrationPage, 'setProps', {}),
			MochiKit.Base.method(this, 'moveInPage', currentPage, 'registrationPage'),
			MochiKit.Base.partial(MochiKit.Async.callLater, 0.5, MochiKit.Base.method(registrationPage, 'setInitialFocus'))
		], {trace:false});

	},

	forcePassphraseLogin_handler: function() {
		return Clipperz.Async.callbacks("MainController.forcePassphraseLogin_handler", [
			MochiKit.Base.method(this.pages()['loginPage'], 'setProps', {'forceCredentials': true}),
			MochiKit.Base.partial(MochiKit.Async.callLater, 0.1, MochiKit.Base.method(this.pages()['loginPage'], 'setInitialFocus'))
		], {trace:false});
	},

	forcePassphraseUnlock_handler: function() {
		return Clipperz.Async.callbacks("MainController.forcePassphraseUnlock_handler", [
			MochiKit.Base.method(this.pages()['unlockPage'], 'setProps', {'forceCredentials': true}),
			MochiKit.Base.partial(MochiKit.Async.callLater, 0.1, MochiKit.Base.method(this.pages()['unlockPage'], 'setInitialFocus'))
		], {trace:false});
	},

	//=========================================================================

	doLogin_handler: function (event) {
		return this.doLogin(event);
	},
	
	doLogin: function (someCredentials) {
		var deferredResult;
		var	credentials;
		var getPassphraseDelegate;
		var	user;

		user = null;

		this.overlay().show("logging in", false, false);
		this.pages()['loginPage'].setProps({disabled:true});

		if ('pin' in someCredentials) {
			credentials = Clipperz.PM.PIN.credentialsWithPIN(someCredentials['pin']);
		} else {
			credentials = someCredentials;
		}
		getPassphraseDelegate = MochiKit.Base.partial(MochiKit.Async.succeed, credentials.passphrase);
		user = new Clipperz.PM.DataModel.User({'username':credentials.username, 'getPassphraseFunction':getPassphraseDelegate});

		deferredResult = new Clipperz.Async.Deferred('MainController.doLogin', {trace:false});
		deferredResult.addCallback(MochiKit.Async.wait, 0.1);
		deferredResult.addMethod(Clipperz.Crypto.PRNG.defaultRandomGenerator(), 'deferredEntropyCollection');
		deferredResult.addMethod(user, 'login');
		if (Clipperz.PM.PIN.isLocalStorageSupported()) {
			deferredResult.addMethod(Clipperz.PM.PIN, 'resetFailedAttemptCount');
		}
		deferredResult.addMethod(this, 'setUser', user);
		deferredResult.addMethod(this, 'updateUserPreferences');
		deferredResult.addMethod(this, 'runApplication');
//		deferredResult.addMethod(this, 'updateCertificateQueueInfo');
		deferredResult.addMethod(this.overlay(), 'done', "", 1);
		deferredResult.addErrback(MochiKit.Base.method(this, 'genericErrorHandler', someCredentials, "login failed"));
		deferredResult.addErrback(MochiKit.Base.bind(function (anEvent, anError) {
			if (anError['isPermanent'] != true) {
				this.pages()['loginPage'].setProps({disabled:false, 'mode':this.loginMode()});
				this.pages()['loginPage'].setInitialFocus();
			}
			return anEvent; //	anError;
		}, this, someCredentials))
		deferredResult.callback();

		return deferredResult;
	},

	logout_handler: function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.logout', {trace:false});
		deferredResult.addMethod(this.user(), 'logout');
		deferredResult.addCallback(function () {
			window.location.href = '/';
		})
		deferredResult.callback();

		return deferredResult;
	},

	lockShortcut: function(anEvent) {
		anEvent.preventDefault();

		return this.lock();
	},

	lock: function () {
		var deferredResult;

		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'disableLock');

		deferredResult = new Clipperz.Async.Deferred('MainController.lock_handler', {trace:false});
		//	force exit edit state OR skip lock if pending changes are present
		deferredResult.addMethod(this, 'exitCurrentSelection');
		deferredResult.addMethod(this, 'resetPanels');
		deferredResult.addMethod(this, 'moveOutPage', this.currentPage(), 'unlockPage');
		deferredResult.addMethod(this.user(), 'lock');
		deferredResult.addMethod(this, 'deleteCleanTextData');
		deferredResult.addMethod(this.pages()['unlockPage'], 'setProps', {'username': this.user().username()});
		deferredResult.addCallback(MochiKit.Async.callLater, 0.1, MochiKit.Base.method(this.pages()['unlockPage'], 'resetUnlockForm'));
		deferredResult.addMethod(this.pages()['unlockPage'], 'setInitialFocus');
		deferredResult.addCallback(MochiKit.Async.callLater, 1, MochiKit.Base.method(this.pages()['mainPage'], 'replaceProps', {'locked': true}));
		
		deferredResult.callback();

		return deferredResult;
	},

	unlock_handler: function(someInfo) {
		var	deferredResult;
		var	credential = someInfo['credential'];
		var	credentialType = someInfo['credentialType'];
		var	user, passphrase, getPassphraseDelegate;

		// var user = this.user();
		var oldUser = this.user();
		var unlockPage = this.pages()['unlockPage'];
		var overlay = this.overlay();

		overlay.show("validating…", false);
		passphrase = (credentialType=='PIN') ? Clipperz.PM.PIN.credentialsWithPIN(credential)['passphrase'] : credential;

		getPassphraseDelegate = MochiKit.Base.partial(MochiKit.Async.succeed, passphrase);
		user = new Clipperz.PM.DataModel.User({'username':oldUser.username(), 'getPassphraseFunction':getPassphraseDelegate});

		deferredResult = new Clipperz.Async.Deferred('MainController.unlock_handler', {trace:false});

		deferredResult.addMethod(unlockPage, 'setProps', {'disabled': true});

		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(this, 'setUser', user);
		// deferredResult.addMethod(user, 'unlock', MochiKit.Base.partial(MochiKit.Async.succeed, passphrase));

		deferredResult.addErrback(MochiKit.Base.bind(function (aValue) {
			var innerDeferredResult;
			var errorMessage;

			errorMessage = 'failed';
			if (credentialType=='PIN') {
				var attemptsLeft = Clipperz.PM.PIN.recordFailedAttempt();

				if (attemptsLeft == -1) {
					errorMessage = 'PIN resetted';
				}
			}

			innerDeferredResult = new Clipperz.Async.Deferred('MainController.unlock_handler <incorrect passphrase>', {trace:false});
			innerDeferredResult.addMethod(unlockPage, 'setProps', {
				'disabled': false,
				'mode': this.loginMode(),
			});
			innerDeferredResult.addMethod(unlockPage, 'setInitialFocus');
			innerDeferredResult.addMethod(overlay, 'failed', errorMessage, 1);
			innerDeferredResult.addCallback(MochiKit.Async.fail, aValue);
			innerDeferredResult.callback();

			return aValue;
		}, this));

		if (credentialType=='PIN') {
			deferredResult.addMethod(Clipperz.PM.PIN, 'resetFailedAttemptCount');
		}
		deferredResult.addMethod(this, 'updateUserPreferences');
		deferredResult.addMethod(this, 'moveInPage', this.currentPage(), 'mainPage');
		deferredResult.addMethod(this, 'refreshUI');
		deferredResult.addMethod(unlockPage, 'setProps', {
			'disabled': false,
			'forceCredentials': false,
		});
		// deferredResult.addMethod(unlockPage, 'resetUnlockForm');
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'enableLock');
//		deferredResult.addMethod(this, 'updateCertificateQueueInfo');
		deferredResult.addMethod(overlay, 'done', "", 0.5);
		deferredResult.addErrback(MochiKit.Async.succeed, true);
		deferredResult.callback();

		return deferredResult;

		// this.user().setPassphraseFunction(function(){return passphrase;});
// TODO: check if passphrase is correct by try/catch on decrypting something
		// this.moveOutPage(this.currentPage(), 'mainPage');
// TODO: check why the unlock form keeps the value stored (doesn't happen with the login form...)
	},

	deleteCleanTextData: function() {
		this._recordsInfo = null;
		this._selectedCards = null;
		this._selectedCardInfo = null;
		this._userPreferences = null;
	},

	//-------------------------------------------------------------------------

	registerNewUser_handler: function (credentials) {
		var	deferredResult;

		this.overlay().show("creating user");

		this.pages()['registrationPage'].setProps({disabled:true});
		deferredResult = new Clipperz.Async.Deferred('MainController.registerNewUser', {trace:false});
		deferredResult.addCallback(Clipperz.PM.DataModel.User.registerNewAccount,
			credentials['username'],
			MochiKit.Base.partial(MochiKit.Async.succeed, credentials['passphrase'])
		);
		deferredResult.addMethod(this, 'doLogin', credentials);
		deferredResult.addMethod(this,'importCards', Clipperz.PM.DefaultCards);
		deferredResult.addErrback(MochiKit.Base.method(this, 'genericErrorHandler', credentials, "registration failed"));
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

	filter: function ()	{
		return this._filter;
	},

	setFilter: function (aType, aValue) {
		this._filter = {'type':aType, 'value':aValue};
		return this._filter;
	},


	shouldIncludeArchivedCards: function () {
		return this._shouldIncludeArchivedCards;
	},
	
	setShouldIncludeArchivedCards: function (aValue) {
		this._shouldIncludeArchivedCards = aValue;
	},

	//----------------------------------------------------------------------------

	collectRecordInfo: function (aRecord) {
		return Clipperz.Async.callbacks("MainController.updateOTPListAndDetails", [
			Clipperz.Async.collectResults("MainController.updateOTPListAndDetails <inner results>", {
				'recordInfo':			MochiKit.Base.method(aRecord,		'collectRecordInfo'),
//				'certificateMetadata':	MochiKit.Base.method(aRecord,		'certificateMetadata'),
				'hasPendingChanges':	MochiKit.Base.method(this.user(),	'hasPendingChanges'),
			}, {trace:false}),
			function (someData) {
				var result;
				
				result = someData['recordInfo'];
				result['hasPendingChanges'] = someData['hasPendingChanges'];
				
				return result;
			},
		], {trace:false});
		
		return deferredResult;
	},

	//----------------------------------------------------------------------------

	showCardDetailInNarrowView: function (aValue) {
		return Clipperz.Async.callbacks("MainController.showCardDetailInNarrowView", [
			MochiKit.Base.method(this, 'setPageProperties', 'cardDetailPage', 'features', this.features()),
			MochiKit.Base.method(this, 'setPageProperties', 'cardDetailPage', 'selectedCard', aValue),
			MochiKit.Base.method(this, 'moveInPage', this.currentPage(), 'cardDetailPage'),
			function () { return aValue; },
		], {trace:false});
	},

	updateSelectedCard: function (someInfo, shouldShowLoading, shouldShowCardDetail) {
		var deferredResult;
		var showLoading = typeof(shouldShowLoading) !== 'undefined' ? shouldShowLoading : true;
		if (someInfo == null) {
			this.setPageProperties('mainPage', 'selectedCard', {});
			deferredResult = MochiKit.Async.succeed();
		} else {
			if (showLoading) {
				// this.setPageProperties('mainPage', 'selectedCard', {'loading':true, 'label':someInfo['label'], '_reference':someInfo['reference']});
				this.overlay().show('', true, false, true)
			}

			deferredResult = new Clipperz.Async.Deferred('MainController.updateSelectedCard', {trace:false});
			deferredResult.addMethod(this.user(), 'getRecord', someInfo['reference']);
			deferredResult.addMethod(this, 'collectRecordInfo');
			deferredResult.addMethod(this, 'setPageProperties', 'mainPage', 'selectedCard');
			if ((this.mediaQueryStyle() == 'narrow') && shouldShowCardDetail) {
				deferredResult.addMethod(this, 'showCardDetailInNarrowView');
			}
			deferredResult.addMethod(this.overlay(), 'hide', true);
		
			MochiKit.Async.callLater(0.1, MochiKit.Base.method(deferredResult, 'callback'));
		}

		return deferredResult;
	},

	//............................................................................

	regExpFilterGenerator: function (aRegExp, aSearchField) {
		var	searchField = aSearchField ? aSearchField : Clipperz.PM.DataModel.Record.defaultSearchField;
		
		return function (aCardInfo) {
			aRegExp.lastIndex = 0;
			return aRegExp.test(aCardInfo[searchField]);
		}
	},
	
	selectedCardReference: function () {
		return	this.pages()['mainPage'].props && 
				this.pages()['mainPage'].props['selectedCard'] &&
				this.pages()['mainPage'].props['selectedCard']['_reference']
			?	this.pages()['mainPage'].props['selectedCard']['_reference']
			:	'';
	},

	isSelectedCardStillVisible: function (someCards) {
		var	result;
		var	reference;
		
		reference = this.selectedCardReference();
		result = MochiKit.Iter.some(someCards, function (aCardInfo) {
			return aCardInfo['_reference'] == reference;
		});
		
		return result;
	},

	//----------------------------------------------------------------------------
	
	recordsInfo: function () {
		return this._recordsInfo;
	},
	
	setRecordsInfo: function (someRecordsInfo) {
//console.log("setRecordsInfo", someRecordsInfo);
		this._recordsInfo = someRecordsInfo;
		return this._recordsInfo;
	},
	
	resetRecordsInfo: function () {
		this._recordsInfo = null;
		this._selectedCards = null;
	},
	
	getRecordsInfo: function (cardInfo, shouldIncludeArchivedCards) {
		var deferredResult;
		
		if (this._recordsInfo == null) {
			deferredResult = new Clipperz.Async.Deferred('MainController.getRecordsInfo', {trace:false});
			deferredResult.addMethod(this.user(), 'getRecordsInfo', Clipperz.PM.DataModel.Record.defaultCardInfo /*, shouldIncludeArchivedCards*/);
			deferredResult.addMethod(this, 'setRecordsInfo');
			deferredResult.callback();
		} else {
			deferredResult = MochiKit.Async.succeed(this._recordsInfo);
		}
		
		return deferredResult;
	},

	//............................................................................

	selectedCards: function () {
		return this._selectedCards;
	},
	
	setSelectedCards: function (aValue) {
		this._selectedCards = aValue;
		return this._selectedCards;
	},
	
	//----------------------------------------------------------------------------

	updateSelectedCards: function (shouldIncludeArchivedCards, aFilter) {
		var deferredResult;

		var	filterCriteria;
		var	sortCriteria;
		var	rangeFilter;
		var	fullFilterCriteria;
		var	selectedCardReference = this._selectedCardInfo ? this._selectedCardInfo['reference'] : null;

		if (aFilter['type'] == 'ALL') {
			filterCriteria = MochiKit.Base.operator.truth;
			sortCriteria = Clipperz.Base.caseInsensitiveKeyComparator('label');
			rangeFilter = MochiKit.Base.operator.identity;
		} else if (aFilter['type'] == 'RECENT') {
			filterCriteria = MochiKit.Base.operator.truth;
			sortCriteria = Clipperz.Base.reverseComparator(MochiKit.Base.keyComparator('_accessDate'));
			rangeFilter = function (someCards) { return someCards.slice(0, 10)};
		} else if (aFilter['type'] == 'WITH_CERTIFICATE') {
			filterCriteria = function (aRecordInfo) { return aRecordInfo['hasBeenCertified']; };
			sortCriteria = Clipperz.Base.caseInsensitiveKeyComparator('label');
			rangeFilter = MochiKit.Base.operator.identity;
		} else if (aFilter['type'] == 'WITH_ATTACHMENTS') {
			filterCriteria = function (aRecordInfo) { return aRecordInfo.attachmentsCount > 0; };
			sortCriteria = Clipperz.Base.caseInsensitiveKeyComparator('label');
			rangeFilter = MochiKit.Base.operator.identity;
		} else if (aFilter['type'] == 'SEARCH') {
			filterCriteria = this.regExpFilterGenerator(Clipperz.PM.DataModel.Record.regExpForSearch(aFilter['value']));
			sortCriteria = Clipperz.Base.caseInsensitiveKeyComparator('label');
			rangeFilter = MochiKit.Base.operator.identity;
		} else if (aFilter['type'] == 'TAG') {
			filterCriteria = this.regExpFilterGenerator(Clipperz.PM.DataModel.Record.regExpForTag(aFilter['value']));
			sortCriteria = Clipperz.Base.caseInsensitiveKeyComparator('label');
			rangeFilter = MochiKit.Base.operator.identity;
		} else if (aFilter['type'] == 'UNTAGGED') {
			filterCriteria = this.regExpFilterGenerator(Clipperz.PM.DataModel.Record.regExpForNoTag());
			sortCriteria = Clipperz.Base.caseInsensitiveKeyComparator('label');
			rangeFilter = MochiKit.Base.operator.identity;
		}

		fullFilterCriteria = function (aRecordInfo) {
			var	result;
			
			if (aRecordInfo['_reference'] == selectedCardReference) {
				result = true;
			} else {
				if ((shouldIncludeArchivedCards == false) && (aRecordInfo['_isArchived'])) {
					result = false;
				} else {
					result = filterCriteria(aRecordInfo);
				}
			}

			return result;
		}
		
		deferredResult = new Clipperz.Async.Deferred('MainController.updateSelectedCards', {trace:false});
		deferredResult.addMethod(this, 'getRecordsInfo', Clipperz.PM.DataModel.Record.defaultCardInfo, shouldIncludeArchivedCards);

		deferredResult.addCallback(MochiKit.Base.filter, fullFilterCriteria);
		deferredResult.addMethodcaller('sort', sortCriteria);
		deferredResult.addCallback(rangeFilter);

		deferredResult.addMethod(this, 'setPageProperties', 'mainPage', 'cards');
		deferredResult.addCallback(MochiKit.Base.bind(function (someCards) {
			this.setSelectedCards(someCards);
			if (!this.isSelectedCardStillVisible(someCards)) {
				this.updateSelectedCard(null);
			};
		}, this));
		deferredResult.addMethod(this, 'setPageProperties', 'mainPage', 'filter', this.filter());

		deferredResult.callback();

		return deferredResult;
	},
	
	refreshSelectedCards: function () {
		return this.updateSelectedCards(this.shouldIncludeArchivedCards(), this.filter());
	},

	refreshUI: function (selectedCardReference) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.refreshUI', {trace:false});
		deferredResult.addMethod(this, 'refreshSelectedCards');
		deferredResult.addMethod(this, 'renderTags');
		
		if (selectedCardReference != null) {
			deferredResult.addMethod(this.user(), 'getRecord', selectedCardReference);
			deferredResult.addMethod(this, 'collectRecordInfo');
			deferredResult.addMethod(this, 'setPageProperties', this.currentPage(), 'selectedCard');
		}

		deferredResult.callback();

		return deferredResult;
	},

	//----------------------------------------------------------------------------

	enableLock_handler: function () {
		return Clipperz.Async.callbacks("MainController.enableLock_handler", [
			MochiKit.Base.partial(MochiKit.Signal.connect, Clipperz.Signal.NotificationCenter, 'lock', MochiKit.Base.method(this, 'lock'))
		], {trace:false});
	},
	
	disableLock_handler: function () {
		return Clipperz.Async.callbacks("MainController.disableLock_handler", [
			MochiKit.Base.partial(MochiKit.Signal.disconnectAll, Clipperz.Signal.NotificationCenter, 'lock')
		], {trace:false});
	},
	
	updatePIN_handler: function(aPIN) {
		return Clipperz.Async.callbacks("MainController.updatePIN_handler", [
			MochiKit.Base.method(this.overlay(), 'show', "updating …", true),
			MochiKit.Base.method(Clipperz.PM.PIN, 'updatePin', this.user(), aPIN),
			MochiKit.Base.method(this.overlay(), 'done', "", 1)
		], {trace:false});
	},

	disablePIN_handler: function() {
		return Clipperz.Async.callbacks("MainController.disablePIN_handler", [
			MochiKit.Base.method(this.overlay(), 'show', "disabling …", true),
			MochiKit.Base.method(Clipperz.PM.PIN, 'disablePin'),
			MochiKit.Base.method(this.overlay(), 'done', "", 1)
		], {trace:false});
	},

	resetLockTimeout: function () {
		if (this.user()) {
			return Clipperz.Async.callbacks("MainController.resetLockTimeout", [
				MochiKit.Base.method(this.user(), 'getPreference', 'lock'),
				MochiKit.Base.bind(function (someLockInfo) {
					if (this._lockTimeout) {
						clearTimeout(this._lockTimeout);
					}

					if (someLockInfo['enabled'] == true) {
						var aDelay = someLockInfo['timeoutInMinutes'] * 60 * 1000;

						this._lockTimeout = setTimeout(function() {
							MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'lock');
						}, aDelay);
					}
				}, this)
			], {trace:false});
		}
	},
	
	//----------------------------------------------------------------------------

	getAllCardsCount: function () {
		var	archivedCardsFilter =	this.shouldIncludeArchivedCards()
									?	MochiKit.Async.succeed
									:	MochiKit.Base.partial(MochiKit.Base.filter, function (someRecordInfo) { return ! someRecordInfo['_isArchived']; });

		return Clipperz.Async.callbacks("MainController.getAllCardsCount", [
			MochiKit.Base.method(this.user(), 'getRecords'),
			MochiKit.Base.partial(MochiKit.Base.map, Clipperz.Async.collectResults("collectResults", {'_fullLabel':MochiKit.Base.methodcaller('fullLabel'), '_isArchived':MochiKit.Base.methodcaller('isArchived')}, {trace:false})),
			Clipperz.Async.collectAll,
			archivedCardsFilter,
			function (someCards) { return someCards.length; },
		], {trace:false});
	},
	
	getCardsWithCertificateCount: function () {
		var	archivedCardsFilter =	this.shouldIncludeArchivedCards()
									?	MochiKit.Async.succeed
									:	MochiKit.Base.partial(MochiKit.Base.filter, function (someRecordInfo) { return ! someRecordInfo['_isArchived']; });

		return Clipperz.Async.callbacks("MainController.getCardsWithCertificateCount", [
			MochiKit.Base.method(this.user(), 'getRecords'),
			MochiKit.Base.partial(MochiKit.Base.map, Clipperz.Async.collectResults("collectResults", {'hasBeenCertified':MochiKit.Base.methodcaller('hasBeenCertified'), '_isArchived':MochiKit.Base.methodcaller('isArchived')}, {trace:false})),
			Clipperz.Async.collectAll,
			archivedCardsFilter,
			function (aResult) {
				return MochiKit.Base.filter(function (aRecordInfo) { return aRecordInfo['hasBeenCertified'] === true; }, aResult).length;
			}
		], {trace:false});
	},

	getCardsWithAttachmentsCount: function () {
		var	archivedCardsFilter =	this.shouldIncludeArchivedCards()
									?	MochiKit.Async.succeed
									:	MochiKit.Base.partial(MochiKit.Base.filter, function (someRecordInfo) { return ! someRecordInfo['_isArchived']; });

		return Clipperz.Async.callbacks("MainController.getCardsWithAttachmentsCount", [
			MochiKit.Base.method(this.user(), 'getRecords'),
			MochiKit.Base.partial(MochiKit.Base.map, Clipperz.Async.collectResults("collectResults", {'attachmentsCount':MochiKit.Base.methodcaller('attachmentsCount'), '_isArchived':MochiKit.Base.methodcaller('isArchived')}, {trace:false})),
			Clipperz.Async.collectAll,
			archivedCardsFilter,
			function (aResult) {
				return MochiKit.Base.filter(function (aRecordInfo) { return aRecordInfo['attachmentsCount'] > 0; }, aResult).length;
			}
		], {trace:false});
	},

	getArchivedCardsCount: function () {
		return Clipperz.Async.callbacks("MainController.getArchivedCardsCount", [
			MochiKit.Base.method(this.user(), 'getRecords'),
			MochiKit.Base.partial(MochiKit.Base.map, Clipperz.Async.collectResults("collectResults", {'_isArchived':MochiKit.Base.methodcaller('isArchived')}, {trace:false})),
			Clipperz.Async.collectAll,
			function (aResult) {
				return MochiKit.Base.filter(function (aRecordInfo) { return aRecordInfo['_isArchived']; }, aResult).length;
			}
		], {trace:false});
	},
	
	getUntaggedCardsCount: function () {
		var	archivedCardsFilter =	this.shouldIncludeArchivedCards()
									?	MochiKit.Async.succeed
									:	MochiKit.Base.partial(MochiKit.Base.filter, function (someRecordInfo) { return ! someRecordInfo['_isArchived']; });
		
		return Clipperz.Async.callbacks("MainController.getUntaggedCardsCount", [
			MochiKit.Base.method(this.user(), 'getRecords'),
			MochiKit.Base.partial(MochiKit.Base.map, Clipperz.Async.collectResults("collectResults", {'_fullLabel':MochiKit.Base.methodcaller('fullLabel'), '_isArchived':MochiKit.Base.methodcaller('isArchived')}, {trace:false})),
			Clipperz.Async.collectAll,
			archivedCardsFilter,
			MochiKit.Base.partial(MochiKit.Base.filter, this.regExpFilterGenerator(Clipperz.PM.DataModel.Record.regExpForNoTag(), '_fullLabel')),
			function (someCards) { return someCards.length; },
		], {trace:false});
	},

	//----------------------------------------------------------------------------

	setPageProperties: function (aPageName, aKey, aValue) {
		var	props = {};
		props[aKey] = aValue;
		this.pages()[aPageName].setProps(props);

		return aValue;
	},

	allTags: function (shouldIncludeArchivedCards) {
		return this.user().getTags(shouldIncludeArchivedCards);
	},
	
	renderTags: function () {
		return Clipperz.Async.callbacks("MainController.renderTags", [
//			MochiKit.Base.method(this.user(), 'getTags', this.shouldIncludeArchivedCards()),
			MochiKit.Base.method(this, 'allTags', this.shouldIncludeArchivedCards()),
			MochiKit.Base.method(this, 'setPageProperties', 'mainPage', 'tags'),
			MochiKit.Base.method(this, 'allTags', true || this.shouldIncludeArchivedCards()),
			MochiKit.Base.keys,
			MochiKit.Base.method(this, 'setPageProperties', 'mainPage', 'allTags'),
			MochiKit.Base.method(this, 'getAllCardsCount'),
			MochiKit.Base.method(this, 'setPageProperties', 'mainPage', 'allCardsCount'),
			MochiKit.Base.method(this, 'getCardsWithCertificateCount'),
			MochiKit.Base.method(this, 'setPageProperties', 'mainPage', 'withCertificateCardsCount'),
			MochiKit.Base.method(this, 'getCardsWithAttachmentsCount'),
			MochiKit.Base.method(this, 'setPageProperties', 'mainPage', 'withAttachmentCardsCount'),
			MochiKit.Base.method(this, 'getArchivedCardsCount'),
			MochiKit.Base.method(this, 'setPageProperties', 'mainPage', 'archivedCardsCount'),
			MochiKit.Base.method(this, 'getUntaggedCardsCount'),
			MochiKit.Base.method(this, 'setPageProperties', 'mainPage', 'untaggedCardsCount'),
			MochiKit.Base.method(this, 'setPageProperties', 'mainPage', 'shouldIncludeArchivedCards', this.shouldIncludeArchivedCards()),
		], {trace:false});
	},
	
	renderAccountData: function () {
		return Clipperz.Async.callbacks("MainController.renderAccountData", [
			MochiKit.Base.method(this, 'setFilter', 'ALL'),
			MochiKit.Base.method(this, 'refreshUI', null)
		], {trace:false});
	},

	//=========================================================================

	historyReplaceState: function (args) {
		if ((window.document.origin != null) && (window.document.origin != "null")) {
			window.history.replaceState.apply(window.history, arguments);
		}
	},
	
	historyPushState: function (args) {
		if ((window.document.origin != null) && (window.document.origin != "null")) {
			window.history.pushState.apply(window.history, arguments);
		}
	},
	
	historyState: function (args) {
		if ((window.document.origin != null) && (window.document.origin != "null")) {
			return window.history.state.apply(window.history, arguments);
		}
	},

	//=========================================================================

	runApplication: function (anUser) {
		this.historyReplaceState({selectedCardInfo: null}, "", window.location.toString());
		this.historyPushState({selectedCardInfo: null}, "", window.location.toString());
		this.moveInPage(this.currentPage(), 'mainPage');
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'enableLock');
		this.resetLockTimeout();
		this.registerHistoryHandler();

		return this.renderAccountData();
	},

	runDirectLogin_handler: function (someParameters) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.runDirectLogin', {trace:false});
		deferredResult.addMethod(this.user(), 'getRecord', someParameters['record']);
		deferredResult.addMethodcaller('directLoginWithReference', someParameters['directLogin']);
		deferredResult.addCallback(Clipperz.PM.UI.DirectLoginController.openDirectLogin);
		deferredResult.callback();

		return deferredResult;
	},

	removeDirectLogin_handler: function (someInfo) {
		var deferredResult;
		var	record = someInfo['record'];
		var	directLoginReference = someInfo['directLoginReference'];

		deferredResult = new Clipperz.Async.Deferred("MainController.removeDirectLogin_handler", {'trace': false});
		
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(directLoginReference));
		deferredResult.addMethodcaller('remove');
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', record.reference());

		deferredResult.callback(record);

		return deferredResult;
	},
/*
	shouldExitApp: function (anEvent) {
//console.log("SHOULD EXIT APP");
		anEvent.preventDefault();
		anEvent.stopPropagation();
	},
*/
	//=========================================================================
/*
	showPreferences_handler: function (anEvent) {
		var	deferredResult;

		this.pages()['preferencePage'].setProps({});
		deferredResult = new Clipperz.Async.Deferred('MainController.showPreferences', {trace:false});
		deferredResult.addMethod(this, 'moveInPage', this.currentPage(), 'preferencePage', true);
		deferredResult.callback();

		return deferredResult;
	},
*/
	//=========================================================================

	genericErrorHandler: function (anEvent, aMessage, anError) {
		var errorMessage;
		var	result;

		result = anError;
//		errorMessage = "login failed";
		errorMessage = aMessage;

		if (anError['isPermanent'] === true) {
			this.pages()['errorPage'].setProps({message:anError.message});
			this.moveInPage(this.currentPage(), 'errorPage');
			errorMessage = "failure";
		} else {
			if ('pin' in anEvent) {
				var errorCount = Clipperz.PM.PIN.recordFailedAttempt();

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
		var	toPosition;
		var	itemToTransition;

		if (direction == "LEFT") {
			fromPosition = 'right';
			toPosition = 'left';
			itemToTransition = toPage;
		} else {
			fromPosition = 'left';
			toPosition = 'right';
			itemToTransition = fromPage;
		}

		MochiKit.DOM.addElementClass(itemToTransition, 'transition');

		MochiKit.Async.callLater(0, function () {
			MochiKit.DOM.addElementClass(fromPage, toPosition);
			MochiKit.DOM.removeElementClass(toPage, fromPosition);
		})

		MochiKit.Async.callLater(0.5, function () {
			MochiKit.DOM.removeElementClass(itemToTransition, 'transition');
		})
	},

	//.........................................................................

	goBack_handler: function () {
		var	fromPage;
		var toPage;

		fromPage = this.pageStack().shift();
		toPage = this.currentPage();

		return Clipperz.Async.callbacks("MainController.goBack_handler", [
			MochiKit.Base.method(this.pages()[toPage], 'setProps', {}),
			MochiKit.Base.method(this, 'moveOutPage', fromPage, toPage)
		], {trace:false});
	},

//	historyGoBack: function (anEvent) {
//		anEvent.preventDefault();
//		anEvent.stopPropagation();
//		this.goBack();
//	},

	currentPage: function () {
		return this.pageStack()[0];
	},

	setCurrentPage: function (aPage) {
		this.pageStack().unshift(aPage);
		this.refreshCurrentPage();
	},

	moveInPage: function (fromPage, toPage, addToHistory) {
		if (fromPage != toPage) {
			var	shouldAddItemToHistory;

			if (this.proxyInfo()['proxyType'] != 'OFFLINE_COPY') {
				shouldAddItemToHistory = typeof(addToHistory) == 'undefined' ? false : addToHistory;
			} else {
				shouldAddItemToHistory = false;
			}

			this.slidePage(MochiKit.DOM.getElement(fromPage), MochiKit.DOM.getElement(toPage), 'LEFT');
			this.setCurrentPage(toPage);

			// Skipping push on page change: history needs to be more granular
			// New states are pushed on user actions (view card, back to main page, ...)
			// if (shouldAddItemToHistory) {
			// 	window.history.pushState({'fromPage': fromPage, 'toPage': toPage}, "");
			// } else {
//console.log("Skip HISTORY");
			// }
		} else {
//console.log("No need to move in the same page");
		}
	},

	moveOutPage: function (fromPage, toPage) {
		this.slidePage(MochiKit.DOM.getElement(fromPage), MochiKit.DOM.getElement(toPage), 'RIGHT');
		this.setCurrentPage(toPage);
	},

	//-------------------------------------------------------------------------

	featureSet: function () {
		return this.userAccountInfo()['featureSet'];
	},

	features: function () {
//		return this.userAccountInfo()['features'];
		return Clipperz.PM.Proxy.defaultProxy.features(this.userAccountInfo()['features']);
	},

	//.........................................................................
	
	userInfo: function () {
		var result;
		result = {};
		
		result['checkPassphraseCallback'] = MochiKit.Base.bind(this.checkPassphrase, this);
		if (this.user() != null) {
			result['username'] = this.user().username();
		}
		
		return result;
	},
	
	userAccountInfo: function () {
		var	result;
		
		result = {};
		
		if (this.user() != null) {
			var	usefulFields = [
				'currentSubscriptionType',
				'expirationDate',
				'referenceDate',
				'featureSet',
				'features',
				'isExpired',
				'isExpiring',
				'paymentVerificationPending',
				
				'attachmentQuota',
				'certificateQuota'
			];
			
			var	attributes = this.user().accountInfo()._attributes;
			MochiKit.Iter.forEach(usefulFields, function (aFieldName) {
				result[aFieldName] = attributes[aFieldName];
			})
		};
		
		return result;
	},

	updateUserAccountInfo_handler: function (anEvent) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("updateUserAccountInfo_handler", {trace:false});
		deferredResult.addMethod(this.user(), 'updateAccountInfo');
		deferredResult.addMethod(this, 'refreshCurrentPage');
		deferredResult.callback();

		return deferredResult;
	},

	proxyInfo: function () {
		return {
			'proxyType':			Clipperz.PM.Proxy.defaultProxy.type(),
			'proxyTypeDescription':	Clipperz.PM.Proxy.defaultProxy.typeDescription()
		}
	},

	attachmentController: function () {
		return this._attachmentController;
	},

	attachmentQueueInfo: function() {
		var queue;
		var notifications;
		var elementFetchCallback;

		if (this._attachmentController) {
//console.log(">>> attachmentController", this._attachmentController);
			queue = this._attachmentController.getQueueInfo();
//			notifications = this._attachmentController.getNotificationsInfo();
			elementFetchCallback = MochiKit.Base.method(this._attachmentController, 'getQueueElement');
		} else {
//console.log(">>> attachmentController -> NULL");
			queue = [];
//			notifications = [];
			elementFetchCallback = null;
		}


//console.log("<<< attachmentController", queue, notifications);
		return {
			'queue': queue,
			'notifications': notifications,
			'elementFetchCallback': elementFetchCallback,
		}
	},

	//-------------------------------------------------------------------------

	messageBoxContent: function () {
		var	message;
		var	level;
		
		message = "";
		level = 'HIDE';
		
		if (this.featureSet() == 'EXPIRED') {
			message = "Expired subscription";
			level = 'ERROR';
		}

		return {
			'message': message,
			'level': level
		};
	},

	deviceProperties: function () {
		return {
			'style':			this.mediaQueryStyle(),
			'isTouchDevice':	this.isTouchDevice(),
			'isDesktop':		this.isDesktop(),
			'hasKeyboard':		this.hasKeyboard(),
			'PIN':				Clipperz.PM.PIN
		};
	},
	
	pageProperties: function (aPageName) {
		var	result;
		var	extraProperties = null;
		
		result = this.deviceProperties();

		if (this.user() != null) {
			result = MochiKit.Base.update(result, {'preferences': this.userPreferences()});
		}

		if (aPageName == 'loginPage') {
			extraProperties = {
				'mode':								this.loginMode(),
				'isNewUserRegistrationAvailable':	Clipperz.PM.Proxy.defaultProxy.canRegisterNewUsers(),
				'disabled':							false,
				'proxyInfo':						this.proxyInfo(),
			};
		} else if (aPageName == 'unlockPage') {
			extraProperties = {
				'mode':			this.loginMode(),
				'proxyInfo':	this.proxyInfo(),
			}
		} else if (aPageName == 'registrationPage') {
		} else if (aPageName == 'mainPage') {
			extraProperties = {
				'messageBox':					this.messageBoxContent(),
				'userInfo':						this.userInfo(),
				'accountInfo':					this.userAccountInfo(),
				'selectionPanelStatus':			this.isSelectionPanelOpen()	? 'OPEN' : 'CLOSED',
				'settingsPanelStatus':			this.isSettingsPanelOpen()	? 'OPEN' : 'CLOSED',
//				'attachmentQueueBoxStatus':		this.isAttachmentQueueBoxOpen()	? 'OPEN' : 'CLOSED',
//				'certificateQueueBoxStatus':	this.isCertificateQueueBoxOpen() ? 'OPEN' : 'CLOSED',
				'featureSet':					this.featureSet(),
				'features':						this.features(),
				'proxyInfo':					this.proxyInfo(),
				'locked':						false,
				'notifications':				this.notifications(),
				'attachmentQueueInfo':			this.attachmentQueueInfo(),
//				'certificateQueueInfo':			this.certificateQueueInfo(),
//				'shouldIncludeArchivedCards':	this.shouldIncludeArchivedCards(),
//				'cards':				…,
//				'tags':					…,
//				'selectedCard':			…,
			};
		} else if (aPageName == 'cardDetailPage') {
			extraProperties = {
				'notifications':				this.notifications(),
				'attachmentQueueInfo':			this.attachmentQueueInfo(),
//				'certificateQueueInfo':			this.certificateQueueInfo(),
				'proxyInfo':					this.proxyInfo(),
			};
		} else if (aPageName == 'errorPage') {
			extraProperties = {
				'message': ''
			};
		}
		
		result = MochiKit.Base.update(result, extraProperties);

		return result;
	},
	
	refreshCurrentPage: function () {
		if (this.pages()[this.currentPage()] != null) {
			this.pages()[this.currentPage()].setProps(this.pageProperties(this.currentPage()));
		}
	},

	//=========================================================================
/*
	synchronizeLocalData_handler: function (anEvent) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.synchronizeLocalData', {trace:false});
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
*/
	
	//=========================================================================

	resetPanels: function () {
		this._isSelectionPanelOpen = false;
		this._isSettingsPanelOpen = false;

		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'closeSettingsPanel');
	},

	featureAvailableForStyles: function (listOfSupportedStyles) {
		return MochiKit.Iter.some(listOfSupportedStyles, MochiKit.Base.partial(MochiKit.Base.operator.eq, this.mediaQueryStyle()));
	},

	shouldShowCardDetailWhenMovingBetweenCardsUsingKeys: function () {
		return !(this.featureAvailableForStyles(['narrow']) && (this.currentPage() == 'mainPage'));
	},
	
	isSelectionPanelHidable: function () {
		return !this.featureAvailableForStyles(['extra-wide']);
	},

	isSelectionPanelOpen: function () {
		return this._isSelectionPanelOpen;
	},

	toggleSelectionPanel_handler: function (anEvent) {
		if (this.isSelectionPanelHidable() == true) {
			if (this.currentPage() == 'cardDetailPage') {
				this.goBackToMainPage();
			}
			this._isSelectionPanelOpen = !this._isSelectionPanelOpen;
			this.setCloseMaskAction(MochiKit.Base.method(this, 'toggleSelectionPanel_handler'));
			this.refreshCurrentPage();
		}
	},

	hideSelectionPanel_handler: function (anEvent) {
		return Clipperz.Async.callbacks("MainController.hideSelectionPanel_handler", [
			MochiKit.Base.bind(function () {
				if (this.isSelectionPanelOpen()) {
					MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleSelectionPanel');
				}
			}, this)
		], {trace:false});
	},

	isSettingsPanelOpen: function () {
		return this._isSettingsPanelOpen;
	},

	toggleSettingsPanel_handler: function (anEvent) {
		return Clipperz.Async.callbacks("MainController.toggleSettingsPanel_handler", [
			MochiKit.Base.bind(function () {
				this._isSettingsPanelOpen = !this._isSettingsPanelOpen;
				this.setCloseMaskAction(MochiKit.Base.method(this, 'toggleSettingsPanel_handler'));
				if (this._isSettingsPanelOpen == false) {
					MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'closeSettingsPanel');
				}
				this.refreshCurrentPage();
			}, this)
		], {trace:false});
	},
/*
	isAttachmentQueueBoxOpen: function() {
		return this._isAttachmentQueueBoxOpen;
	},

	toggleAttachmentQueueBox_handler: function (anEvent) {
		return Clipperz.Async.callbacks("MainController.toggleAttachmentQueueBox_handler", [
			MochiKit.Base.bind(function () {
				this._isAttachmentQueueBoxOpen = !this._isAttachmentQueueBoxOpen;
				this._isCertificateQueueBoxOpen = false;
				this.refreshCurrentPage();
			}, this)
		], {trace:false});
	},
*/
	//----------------------------------------------------------------------------
/*
	isCertificateQueueBoxOpen: function() {
		return this._isCertificateQueueBoxOpen;
	},

	toggleCertificateQueueBox_handler: function (anEvent) {
		return Clipperz.Async.callbacks("MainController.toggleCertificateQueueBox_handler", [
			MochiKit.Base.bind(function () {
				this._isCertificateQueueBoxOpen = !this._isCertificateQueueBoxOpen;
				this._isAttachmentQueueBoxOpen = false;
				this.refreshCurrentPage();
			}, this)
		], {trace:false});
	},
*/
	//----------------------------------------------------------------------------

	selectedCardInfo: function () {
		return this._selectedCardInfo;
	},

	selectedCardIndex: function () {
		var	selectedCardInfo;
		var	result;
		
		result = -1;
		selectedCardInfo = this.selectedCardInfo();
		if (selectedCardInfo != null) {
			var	selectedCards;
			var	reference;
			var i, c;
			
			selectedCards = this.selectedCards();
			reference = selectedCardInfo['reference'];
			c = selectedCards.length;
			i = 0;
			while (i<c && result == -1) {
				if (selectedCards[i]['_reference'] == reference) {
					result = i;
				} else {
					i++;
				}
			}
		}

		return result;
	},

	cardInfoAtIndex: function (anIndex) {
		var	card;

		card = this.selectedCards()[anIndex];

		return {
			'label': card['label'],
			'reference': card['_reference']
		};
	},

	previousCardInfo: function () {
		var	currentIndex;
		var	nextIndex;
		var	result;

		currentIndex = this.selectedCardIndex();
		if (currentIndex == -1) {
			nextIndex = this.selectedCards().length - 1;
		} else {
			nextIndex = Math.max(currentIndex - 1, 0);
		}

		if (currentIndex == nextIndex) {
			result = null;
		} else {
			result = this.cardInfoAtIndex(nextIndex);
		}
		
		return result;
	},
	
	nextCardInfo: function () {
		var	currentIndex;
		var	nextIndex;
		var result;

		currentIndex = this.selectedCardIndex();
		if (currentIndex == -1) {
			nextIndex = 0;
		} else {
			nextIndex = Math.min(currentIndex + 1, this.selectedCards().length - 1);
		}

		if (currentIndex == nextIndex) {
			result = null;
		} else {
			result = this.cardInfoAtIndex(nextIndex);
		}
		
		return result;
	},

	//............................................................................

	goBackToMainPage: function (anEvent) {
		if (this.currentPage() == 'cardDetailPage') {
			var	resetSelection = null;
			this.updateSelectedCard(resetSelection, true, false);
			this.moveOutPage(this.currentPage(), 'mainPage');
		}
	},

	selectCard: function (someInfo, shouldUpdateCardDetail) {
		var result;

		if (this.featureSet() != 'EXPIRED') {
			this._selectedCardInfo = someInfo;
			this.refreshSelectedCards();
			result = this.updateSelectedCard(someInfo, true, shouldUpdateCardDetail);

//			# TODO:	make the selected element visible; 
//					this may not always be the case, as selection can also be changed using keys.
//			MochiKit.Visual.ScrollTo(MochiKit.DOM.getElement("xxx"));
		} else {
			result = MochiKit.Async.succeed();
		};
		
		return result;
	},
	
	resetSelectedCard: function () {
		this._selectedCardInfo = null;
	},

	selectCard_handler: function (someInfo) {
		var	cardInfo = someInfo['cardInfo'];
		var shouldUpdateCardDetail = someInfo['update'];

		return Clipperz.Async.callbacks("MainController.selectCard_handler", [
			MochiKit.Base.method(this, 'selectCard', cardInfo, shouldUpdateCardDetail),
			MochiKit.Base.method(this, 'historyPushState', {selectedCardInfo: cardInfo}, "", window.location.toString())
		], {trace:false});
	},

	blockDragOver: function (anEvent) {
		anEvent.stopPropagation();
		anEvent.preventDefault();
		anEvent.dataTransfer.dropEffect = 'none';
	},

	refreshCardEditDetail_handler: function (aRecordReference) {
		return Clipperz.Async.callbacks("MainController.refreshCardEditDetail_handler", [
			MochiKit.Base.method(this, 'updateSelectedCard', {'reference':aRecordReference}, false, true)
		], {trace:false});

	},

	//----------------------------------------------------------------------------

	downloadExport_handler: function () {
		var	exportController;
		var deferredResult;

		exportController = new Clipperz.PM.UI.ExportController();

		deferredResult = new Clipperz.Async.Deferred("MainController.downloadExport_handler", {trace: false});
		deferredResult.addMethod(this.overlay(), 'show', "loading …", true, true);
		deferredResult.addMethod(this.user(), 'getRecordsLoadingAllData');
		deferredResult.addCallbackPass(MochiKit.Base.method(this.overlay(), 'show', "exporting …", true, true));
		deferredResult.addMethod(exportController, 'run');
		deferredResult.addMethod(this.overlay(), 'done', "", 1);
		deferredResult.callback();

		return deferredResult;
	},
	
	//----------------------------------------------------------------------------

	changePassphrase_handler: function(newPassphrase) {
		var	currentPage = this.pages()[this.currentPage()];
		var deferredResult;
		var getPassphraseDelegate;
		var user;
		
		getPassphraseDelegate = MochiKit.Base.partial(MochiKit.Async.succeed, newPassphrase);
		user = new Clipperz.PM.DataModel.User({'username':this.user().username(), 'getPassphraseFunction':getPassphraseDelegate});
		
		deferredResult = new Clipperz.Async.Deferred("MainController.changePassphrase_handler", {trace: false});
		deferredResult.addMethod(this.overlay(), 'show', "changing …", true);
		deferredResult.addMethod(this.user(), 'changePassphrase', getPassphraseDelegate);
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(this, 'setUser', user);
		deferredResult.addMethod(this.overlay(), 'done', "saved", 1);
		deferredResult.callback();
		
		return deferredResult;
	},
	
	deleteAccount_handler: function() {
		var deferredResult;
		var doneMessageDelay = 2;
		
		deferredResult = new Clipperz.Async.Deferred("MainController.deleteAccount_handler", {trace: false});
		deferredResult.addCallback(MochiKit.Base.method(this, 'ask', {
			'question': "Do you really want to permanently delete your account?",
			'possibleAnswers':{
				'cancel':	{'label':"No",	'isDefault':true,	'answer':MochiKit.Base.methodcaller('cancel', new MochiKit.Async.CancelledError())},
				'revert':	{'label':"Yes",	'isDefault':false,	'answer':MochiKit.Base.methodcaller('callback')}
			}
		})),
		deferredResult.addMethod(this.overlay(), 'show', "deleting …", true);
		deferredResult.addMethod(this.user(), 'deleteAccount');
		deferredResult.addMethod(this.overlay(), 'done', "deleted", doneMessageDelay);
		deferredResult.addCallback(MochiKit.Async.callLater, doneMessageDelay, function() { window.location.href = '/'; });
		
		deferredResult.callback();
		
		return deferredResult;
	},

	//----------------------------------------------------------------------------
	
	userPreferences: function () {
		return this._userPreferences;
	},

	setUserPreferences: function (someValues) {
		this._userPreferences = someValues;
		return someValues;
	},

	updateUserPreferences: function() {
		return Clipperz.Async.callbacks("MainController.updateUserPreferences", [
			MochiKit.Base.method(this.user(), 'getPreferences'),
			MochiKit.Base.method(this, 'setUserPreferences'),
		], {trace:false});
	},

	setPreferences_handler: function(anObject) {
		return Clipperz.Async.callbacks("MainController.setPreferences_handler", [
			MochiKit.Base.method(this.overlay(), 'show', "", true),
			MochiKit.Base.method(this.user(), 'setPreferences', anObject),
			MochiKit.Base.method(this, 'updateUserPreferences'),
			MochiKit.Base.method(this, 'refreshCurrentPage'),
			MochiKit.Base.method(this.overlay(), 'done', "", 0.5),
		], {trace:false});
	},

	//----------------------------------------------------------------------------

	importCards_handler: function(data) {
		return Clipperz.Async.callbacks("MainController.importCards_handler", [
			MochiKit.Base.method(this.overlay(), 'show', "importing …", true),
			MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'toggleSettingsPanel'),
			function () { return data; },
			MochiKit.Base.method(this,'importCards'),
			MochiKit.Base.method(this.overlay(), 'done', "finished", 1),
			MochiKit.Base.method(this.pages()[this.currentPage()], 'setProps', {'mode':'view', 'showGlobalMask':false}),
		], {trace:false});
	},

	importCards: function(data) {
		return Clipperz.Async.callbacks("MainController.importCards", [
			function () { return data; },
			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this.user(), 'createNewRecordFromJSON')),
			Clipperz.Async.collectAll,
			MochiKit.Base.method(this.user(), 'saveChanges'),
			MochiKit.Base.partial(MochiKit.Base.method(this, 'resetRecordsInfo')),
			MochiKit.Base.partial(MochiKit.Base.method(this, 'refreshUI', null)),
		], {trace:false});
	},

	//----------------------------------------------------------------------------
	
	updateOTPListAndDetails: function() {
		return Clipperz.Async.callbacks("MainController.updateOTPListAndDetails", [
			Clipperz.Async.collectResults("MainController.updateOTPListAndDetails <inner results>", {
				'userInfo':		MochiKit.Base.method(this, 'userInfo'),
				'otpDetails':	Clipperz.Async.collectResults("User.updateOTPListAndDetails <otpDetails>", {
					'otpList':		MochiKit.Base.method(this.user(),'getOneTimePasswords'),
					'otpsDetails':	MochiKit.Base.method(this.user(),'getOneTimePasswordsDetails'),
				}),
			}, {trace:false}),
			function (someData) {
				return MochiKit.Base.update(someData['userInfo'], someData['otpDetails']);
			},
			MochiKit.Base.bind(function(someUserInfo) {
				this.setPageProperties('mainPage', 'userInfo', someUserInfo);
			}, this)
		], {trace:false});
	},
	
	/* Used only one time (the first time the OTP ExtraFeature loads), other times
	the list update is triggered by other operations. Maybe the first OTP list retrieval
	could be done during init, so that this would not be necessary. */
	updateOTPListAndDetails_handler: function () {
		return this.updateOTPListAndDetails();
	},
	
	createNewOTP_handler: function () {
		return Clipperz.Async.callbacks("MainController.createNewOTP_handler", [
			MochiKit.Base.method(this.overlay(), 'show', "", true),
			MochiKit.Base.method(this.user(), 'createNewOTP'),
			MochiKit.Base.method(this, 'updateOTPListAndDetails'),
			MochiKit.Base.method(this.overlay(), 'done', "", 0.5),
		], {trace:false});
	},
	
	deleteOTPs_handler: function (aList) {
		return Clipperz.Async.callbacks("MainController.deleteOTPs_handler", [
			MochiKit.Base.method(this.overlay(), 'show', "", true),
			MochiKit.Base.method(this.user(), 'deleteOTPs', aList),
			MochiKit.Base.method(this, 'updateOTPListAndDetails'),
			MochiKit.Base.method(this.overlay(), 'done', "", 0.5),
		], {trace:false});
	},
	
	changeOTPLabel_handler: function (someInfo) {
		var	reference = someInfo['reference'];
		var label = someInfo['label'];

		return Clipperz.Async.callbacks("MainController.changeOTPLabel_handler", [
			MochiKit.Base.method(this.overlay(), 'show', "", true),
			MochiKit.Base.method(this.user(), 'changeOTPLabel', reference, label),
			MochiKit.Base.method(this, 'updateOTPListAndDetails'),
			MochiKit.Base.method(this.overlay(), 'done', "", 0.5),
		], {trace:false});
	},
	
	//----------------------------------------------------------------------------

	saveChanges: function () {
		//	TODO: handle errors while savings
		return Clipperz.Async.callbacks("MainController.saveChanges", [
			MochiKit.Base.method(this.overlay(), 'show', "saving …", true),
			MochiKit.Base.method(this.user(), 'saveChanges'),
			MochiKit.Base.method(this, 'resetRecordsInfo'),
			MochiKit.Base.method(this.overlay(), 'done', "saved", 1),
		], {trace:false});
	},

	saveCardEdits_handler: function (aRecordReference) {
		var	currentPage = this.pages()[this.currentPage()];
		var	self = this;
		var record, wasBrandNew;
		
		return Clipperz.Async.callbacks("MainController.saveCardEdits_handler", [
			MochiKit.Base.method(currentPage, 'setProps', {'showGlobalMask':true}),

			MochiKit.Base.method(this.user(), 'getRecord', aRecordReference),
			function(aRecord) { record = aRecord; wasBrandNew = aRecord.isBrandNew(); },

			MochiKit.Base.method(this, 'saveChanges'),

			// When new record has attachments, server status should be updated as soon as it is available
			function() { return wasBrandNew; },
			Clipperz.Async.deferredIf('WasBrandNew',[
				function() { return record; },
				MochiKit.Base.method(this, 'reloadAttachmentServerStatusCallback')
			], [
//				MochiKit.Async.succeed
			]),
						
			MochiKit.Base.method(currentPage, 'setProps', {'mode':'view', 'showGlobalMask':false}),
			MochiKit.Base.method(this, 'refreshUI', aRecordReference),
			MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'enableLock'),
		], {trace:false});
	},

	cancelCardEdits_handler: function (aRecordReference) {
		return this.cancelCardEdits(aRecordReference);
	},

	cancelCardEdits: function (aRecordReference) {
		var	currentPage = this.pages()[this.currentPage()];
		var	self = this;
		var	wasBrandNew;

		return Clipperz.Async.callbacks("MainController.cancelCardEdits_handler", [
			MochiKit.Base.method(this.user(), 'getRecord', aRecordReference),
			MochiKit.Base.methodcaller('isBrandNew'),
			function (aValue) { wasBrandNew = aValue },
			
			MochiKit.Base.method(this.user(), 'hasPendingChanges'),
			Clipperz.Async.deferredIf('HasPendingChanges',[
				MochiKit.Base.method(self, 'ask', {
					'question': "There are pending changes to your card. Ignore changes?",
					'possibleAnswers':{
						'cancel':	{'label':"No",	'isDefault':true,	'answer':MochiKit.Base.methodcaller('cancel', new MochiKit.Async.CancelledError())},
						'revert':	{'label':"Yes",	'isDefault':false,	'answer':MochiKit.Base.methodcaller('callback')}
					}
				}),
			], [
//				MochiKit.Async.succeed
			]),
			MochiKit.Base.method(this.user(), 'revertChanges'),
			MochiKit.Base.method(currentPage, 'setProps', {'mode':'view'}),

			MochiKit.Base.bind(function () {
				var	info;
				if (wasBrandNew == true) {
					info = null;
				} else {
					info = {'reference': aRecordReference};
				}

				this.updateSelectedCard(info, false, true);
			}, this),

			MochiKit.Base.bind(function () {
				if ((wasBrandNew == true) && (this.currentPage() == 'cardDetailPage')) {
					this.goBackToMainPage();
				}
			},this),
			MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'enableLock'),
		], {trace:false});
	},

	//----------------------------------------------------------------------------
	
	ask: function (someInfo) {
		var	deferredResult;
		var	currentPage = this.pages()[this.currentPage()];

		deferredResult = new Clipperz.Async.Deferred('MainController.ask', {trace:false});
		currentPage.setProps({'ask': {'info': someInfo, 'deferred':deferredResult}, 'showGlobalMask':true });
		
		deferredResult.addBothPass(MochiKit.Base.method(currentPage, 'setProps', {'ask': null, 'showGlobalMask':false }));
		
		return deferredResult;
	},

	//----------------------------------------------------------------------------

	addCardClick_handler: function () {
		var newRecord;

		this.selectAllCards();

		return Clipperz.Async.callbacks("MainController.addCardClick_handler", [
			MochiKit.Base.method(this.user(), 'createNewRecord'),
			function (aValue) {
				newRecord = aValue;
				return newRecord;
			},
			MochiKit.Base.methodcaller('addField', {'label':"username", 'value':"", 'hidden':false}),
			function () { return newRecord; },
			MochiKit.Base.methodcaller('addField', {'label':"password", 'value':"", 'hidden':true}),
			function () { return newRecord; },
			MochiKit.Base.methodcaller('reference'),
			MochiKit.Base.method(this, 'refreshUI'),
			function () { return newRecord; },
			MochiKit.Base.methodcaller('reference'),
			MochiKit.Base.bind(function (aRecordReference) {
				return this.selectCard({
					'reference': aRecordReference,
					'label': ""
				}, true);
			}, this),
			MochiKit.Base.method(this, 'enterEditMode'),
		], {trace:false});
	},

	deleteCard_handler: function (anEvent) {
		var	self = this;
		
		return Clipperz.Async.callbacks("MainController.deleteCard_handler", [
			MochiKit.Base.method(self, 'ask', {
				'question': "Delete card?",
				'possibleAnswers':{
					'cancel':	{'label':"No",	'isDefault':true,	'answer':MochiKit.Base.methodcaller('cancel', new MochiKit.Async.CancelledError())},
					'delete':	{'label':"Yes",	'isDefault':false,	'answer':MochiKit.Base.methodcaller('callback')}
				}
			}),
			MochiKit.Base.method(this.user(), 'getRecord', anEvent['reference']),
			MochiKit.Base.method(this.user(), 'deleteRecord'),
			MochiKit.Base.method(this, 'saveChanges'),
			MochiKit.Base.method(this, 'exitCurrentSelection'),
			MochiKit.Base.method(this, 'refreshUI')
		], {trace:false});
	},
	
	toggleArchiveCard_handler: function (anEvent) {
		return Clipperz.Async.callbacks("MainController.archiveCard_handler", [
			MochiKit.Base.method(this.user(), 'getRecord', anEvent['reference']),
			MochiKit.Base.methodcaller('toggleArchive'),
			MochiKit.Base.method(this, 'saveChanges'),
			MochiKit.Base.method(this, 'refreshUI', anEvent['reference'])
		], {trace:false});
	},

	cloneCard_handler: function (anEvent) {
		var	cardInfo;

		return Clipperz.Async.callbacks("MainController.cloneCard_handler", [
			MochiKit.Base.method(this.user(), 'getRecord', anEvent['reference']),
			MochiKit.Base.method(this.user(), 'cloneRecord'),
			Clipperz.Async.collectResults("MainController.cloneCard_handler <card info>", {
				'label': MochiKit.Base.methodcaller('label'),
				'reference': MochiKit.Base.methodcaller('reference')
			}, {trace:false}),
			function (aValue) { cardInfo = aValue; return aValue; },
			MochiKit.Base.method(this, 'saveChanges'),

			function () {
				MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectCard', {'cardInfo':cardInfo, 'update':false});
			},
			
			function (aValue) { return cardInfo['reference']; },
			MochiKit.Base.method(this, 'refreshUI'),
		], {trace:false});
	},
	
	isPageInEditMode: function() {
		var	currentPage = this.pages()[this.currentPage()];
		return currentPage ? currentPage.props['mode'] == 'edit' : false;
	},
	
	enterEditMode: function () {
		var	currentPage = this.pages()[this.currentPage()];

		currentPage.setProps({'mode': 'edit'});
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'disableLock');

		return Clipperz.Async.callbacks("MainController.enterEditMode", [
			MochiKit.Base.method(this, 'allTags', true),
			MochiKit.Base.keys,
			function (aValue) {
				currentPage.setProps({'allTags': aValue});
			},
		], {trace:false});
	},
	
	editCard_handler: function (anEvent) {
		this.enterEditMode();
	},

	certificateDetails: function (aRecordReference) {
		return Clipperz.Async.callbacks("MainController.certificateDetails", [
			Clipperz.Async.collectResults("MainController.certificateDetails <inner results>", {
				'wallet':	MochiKit.Base.method(this.user(), 'getWallet'),
				'record':	MochiKit.Base.method(this.user(), 'getRecord', aRecordReference),
			}, {trace:false}),
			Clipperz.Async.collectResults("MainController.showCertificatePreview_handler <inner results>", {
				'transaction':	function (someValues) { return someValues['record'].computeCertificateInfo(someValues['wallet']); },
				'metadata':		[
					MochiKit.Base.itemgetter('record'),
					MochiKit.Base.methodcaller('certificateMetadata'),
					Clipperz.Base.evalJSON
				]
			}, {trace:false}),
		], {trace:false});
	},

	downloadCertificate_handler: function (aRecordReference) {
//console.log("DOWNLOAD CERTIFICATE", aRecordReference);
		var	certificateDownloadController;
		var deferredResult;

		certificateDownloadController = new Clipperz.PM.UI.CertificateDownloadController();

		deferredResult = new Clipperz.Async.Deferred("MainController.downloadCertificate_handler", {trace: false});
		deferredResult.addMethod(this.overlay(), 'show', "", true, false);
		deferredResult.addMethod(this.user(), 'getRecord', aRecordReference);
		deferredResult.collectResults({
			'attachments': MochiKit.Base.methodcaller('attachmentsInfo'),
			'certificateInfo': MochiKit.Base.methodcaller('certificateInfo'),
			'certificateDetails': MochiKit.Base.method(this, 'certificateDetails', aRecordReference),
		}, {trace:false});

		deferredResult.addMethod(certificateDownloadController, 'run');
		deferredResult.addMethod(this.overlay(), 'done', "", 1);
		deferredResult.callback();

		return deferredResult;
	},
	
	showCertificatePreview_handler: function (aRecordReference) {
		var	currentPage = this.pages()[this.currentPage()];
		currentPage.setProps({'showCertificatePreview': true});

		return Clipperz.Async.callbacks("MainController.showCertificatePreview_handler", [
			MochiKit.Base.method(this, 'certificateDetails', aRecordReference),
			function (aValue) {
				currentPage.setProps({'certificateDetails': aValue});
			},
		], {trace:false});
	},

	hideCertificatePreview_handler: function (aRecordReference) {
		return Clipperz.Async.callbacks("MainController.hideCertificatePreview_handler", [
			MochiKit.Base.method(this.pages()[this.currentPage()], 'setProps', {'showCertificatePreview': false})
		], {trace:false});
	},
/*
	createCertificate_handler: function (anEvent) {
//console.log("CREATE CERTIFICATE HANDLER", anEvent);
//console.log("CREATE CERTIFICATE", this.user().accountInfo(), this.userAccountInfo());
		var	certificateQuota = this.userAccountInfo()['certificateQuota'];
		var	availableCertificates =
			certificateQuota['totalNumber']
			- (certificateQuota['used']['published'] + certificateQuota['used']['requested']);
		var	result;
		var	self = this;

		if (availableCertificates > 0) {
			var	documentID;

//console.log("CREATE CERTIFICATE - availableCertificates", availableCertificates);
			result = Clipperz.Async.callbacks("MainController.createCertificate_handler [option A]", [
				MochiKit.Base.method(self, 'ask', {
					'question': "Create certificate?",
					'description': [
						React.DOM.p({'className':'cardCertificateWarning'}, "You are going to register the content of this card on the Bitcoin blockchain."),
						React.DOM.p({'className':'cardCertificateWarning'}, "Afterward you won't be able to make any change, so make sure the card contains all the desired data and files before proceeding."),
						React.DOM.p({'className':'cardCertificateWarning'}, availableCertificates + " registrations left"),
					],
					'possibleAnswers':{
						'cancel':	{'label':"Cancel",	'isDefault':false,	'answer':MochiKit.Base.methodcaller('cancel', new MochiKit.Async.CancelledError())},
						'delete':	{'label':"Continue",'isDefault':true,	'answer':MochiKit.Base.methodcaller('callback')}
					}
				}),

				MochiKit.Base.method(this.overlay(), 'show', "registering", true),
				MochiKit.Base.method(this.user(), 'getRecord', anEvent['reference']),
				MochiKit.Base.method(this.user(), 'createCertificateForRecord'),
				MochiKit.Base.method(this.user(), 'saveChangesWithExtraParameters'),

				MochiKit.Base.method(this.user(), 'getRecord', anEvent['reference']),
				MochiKit.Base.methodcaller('updateCertificateInfo'),

				MochiKit.Base.method(this.overlay(), 'done', "", 1),
				
				MochiKit.Base.method(this, 'updateCertificateQueueInfo'),
				MochiKit.Base.method(this, 'resetRecordsInfo'),
				MochiKit.Base.method(this, 'refreshUI', anEvent['reference']),
			], {trace:false});
		} else {
			result = Clipperz.Async.callbacks("MainController.createCertificate_handler [option B]", [
				MochiKit.Base.method(self, 'ask', {
					'question': "Sorry, registration not available",
					'description': [
						React.DOM.p({'className':'cardCertificateWarning'}, "You can't register this card on the Bitcoin blockchain because you have already consumed all your quota. Replenish your supply!"),
					],
					'possibleAnswers':{
						'cancel':		{'label':"Ok",		'isDefault':false,	'answer':MochiKit.Base.methodcaller('cancel', new MochiKit.Async.CancelledError())},
//						'subscribe':	{'label':"Upgrade",	'isDefault':true,	'answer':MochiKit.Base.noop, 'disabled':true}
					}
				}),
			], {trace:false});
		}
		
		return result;
	},
*/
	//...........................................................................
/*
	updateCertificateQueueInfo: function () {
		return Clipperz.Async.callbacks("MainController.updateCertificateQueueInfo", [
			MochiKit.Base.method(this.user().connection(), 'message', 'getCertificatesStatus', {}),
			MochiKit.Base.bind(function (certificatesStatus) {
				return MochiKit.Base.map(MochiKit.Base.bind(function (certificateStatus) {
					var	matchingRecords;
					var	result;
					matchingRecords = MochiKit.Base.filter(function (aRecordInfo) { return aRecordInfo['_reference'] == certificateStatus[0]; }, this._recordsInfo);
					if (matchingRecords.length == 1) {
						result = {'reference':certificateStatus[0], 'status':certificateStatus[1], 'label':matchingRecords[0]['label']};
					} else {
						result = null;
					}
					return result;
				}, this), MochiKit.Base.zip(MochiKit.Base.keys(certificatesStatus), MochiKit.Base.values(certificatesStatus)));
			}, this),
			MochiKit.Base.method(this, '_updatedCertificateQueueInfo'),
			MochiKit.Base.method(this, 'setPageProperties', 'mainPage', 'certificateQueueInfo'),
			MochiKit.Base.method(this, 'setPageProperties', 'cardDetailPage', 'certificateQueueInfo'),
		], {trace:false});
	},

	_updatedCertificateQueueInfo: function (newQueueStatus) {
		var	hasPendingPublishingCertificate;

		this._certificateQueueStatus = newQueueStatus;
		hasPendingPublishingCertificate = MochiKit.Iter.some(this._certificateQueueStatus, function (anInfo) { return (anInfo && (anInfo['status'] == 'requested')); });

//console.log("hasPendingPublishingCertificate", hasPendingPublishingCertificate);
		if (hasPendingPublishingCertificate) {
//console.log("Rescheduled call to 'updateCertificateQueueInfo'");
			MochiKit.Async.callLater(10 * 60, MochiKit.Base.method(this, 'updateCertificateQueueInfo'));
		}

		return this._certificateQueueStatus;
	},

	certificateQueueInfo: function() {
		return this._certificateQueueStatus;
	},

	closeCertificateNotification_handler: function (aRecordReference) {
		return Clipperz.Async.callbacks("MainController.closeCertificateNotification_handler", [
			MochiKit.Base.method(this.user().connection(), 'message', 'acknowledgeCertificateStatus', {'reference':aRecordReference}),
			MochiKit.Base.method(this, 'updateCertificateQueueInfo'),
		], {trace:false});
	},

	showCertificateCard_handler: function (aRecordReference) {
console.log("showCertificateCard_handler");
		return Clipperz.Async.callbacks("MainController.showCertificateCard_handler", [
//			MochiKit.Base.bind(function () { this._isAttachmentQueueBoxOpen = false; }, this),
			MochiKit.Base.method(this, 'selectCard_handler', {'cardInfo': {'reference':aRecordReference}, 'update': true}),
		], {trace:false});
	},
*/
	//===========================================================================
	
	addTag_handler: function (anEvent) {
		var	record = this.pages()[this.currentPage()].props['selectedCard']['_record'];
		var	tag = anEvent;
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.addTag', {trace:false});
		deferredResult.addMethod(record, 'addTag', tag);
		deferredResult.addMethod(this, 'collectRecordInfo', record);
		deferredResult.addMethod(this, 'setPageProperties', this.currentPage(), 'selectedCard');
//		deferredResult.addMethod(this, 'refreshCurrentPage');
		deferredResult.callback();
		
		return deferredResult;
	},
	
	removeTag_handler: function (anEvent) {
		var	record = this.pages()[this.currentPage()].props['selectedCard']['_record'];
		var	tag = anEvent;
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.removeTag', {trace:false});
		deferredResult.addMethod(record, 'removeTag', tag);
		deferredResult.addMethod(this, 'collectRecordInfo', record);
		deferredResult.addMethod(this, 'setPageProperties', this.currentPage(), 'selectedCard');
//		deferredResult.addMethod(this, 'refreshCurrentPage');
		deferredResult.callback();
		
		return deferredResult;
	},

	goBackToMainPage_handler: function (anEvent) {
		return Clipperz.Async.callbacks("MainController.goBackToMainPage_handler", [
			MochiKit.Base.method(this, 'goBackToMainPage', anEvent),
			MochiKit.Base.method(this, 'historyPushState', {selectedCardInfo: null}, "", window.location.toString())
		], {trace:false});
	},

	//============================================================================

	selectAllCards_handler: function () {
		return Clipperz.Async.callbacks("MainController.selectAllCards_handler", [
			MochiKit.Base.method(this, 'selectAllCards')
		], {trace:false});

	},

	selectAllCards: function () {
		this.setPageProperties('mainPage', 'searchTerm', '');
		this.resetSelectedCard();
		this.setFilter('ALL');
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'hideSelectionPanel');
		return this.refreshSelectedCards();
	},
	
	selectRecentCards_handler: function () {
		return Clipperz.Async.callbacks("MainController.selectRecentCards_handler", [
			MochiKit.Base.method(this, 'resetSelectedCard'),
			MochiKit.Base.method(this, 'setFilter', 'RECENT'),
			MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'hideSelectionPanel'),
			MochiKit.Base.method(this, 'refreshSelectedCards')
		], {trace:false});
	},

	selectCardsWithCertificate_handler: function () {
		return Clipperz.Async.callbacks("MainController.selectCardsWithCertificate_handler", [
			MochiKit.Base.method(this, 'resetSelectedCard'),
			MochiKit.Base.method(this, 'setFilter', 'WITH_CERTIFICATE'),
			MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'hideSelectionPanel'),
			MochiKit.Base.method(this, 'refreshSelectedCards')
		], {trace:false});
	},

	selectCardsWithAttachments_handler: function () {
		return Clipperz.Async.callbacks("MainController.selectCardsWithAttachments_handler", [
			MochiKit.Base.method(this, 'resetSelectedCard'),
			MochiKit.Base.method(this, 'setFilter', 'WITH_ATTACHMENTS'),
			MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'hideSelectionPanel'),
			MochiKit.Base.method(this, 'refreshSelectedCards')
		], {trace:false});
	},

	search_handler: function (aValue) {
		var	searchType;

		if (aValue == "") {
			searchType = 'ALL';
		} else {
			searchType = 'SEARCH';
		}
		
		return Clipperz.Async.callbacks("MainController.search_handler", [
			MochiKit.Base.method(this, 'resetSelectedCard'),
			MochiKit.Base.method(this, 'setFilter', searchType, aValue),
			MochiKit.Base.method(this, 'setPageProperties', 'mainPage', 'searchTerm', aValue),
			MochiKit.Base.method(this, 'refreshSelectedCards')
		], {trace:false});
	},

	tagSelected_handler: function (aTag) {
		return Clipperz.Async.callbacks("MainController.tagSelected_handler", [
			MochiKit.Base.method(this, 'resetSelectedCard'),
			MochiKit.Base.method(this, 'setFilter', 'TAG', aTag),
			MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'hideSelectionPanel'),
			MochiKit.Base.method(this, 'refreshSelectedCards')
		], {trace:false});
	},

	selectUntaggedCards_handler: function () {
		return Clipperz.Async.callbacks("MainController.selectUntaggedCards_handler", [
			MochiKit.Base.method(this, 'resetSelectedCard'),
			MochiKit.Base.method(this, 'setFilter', 'UNTAGGED'),
			MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'hideSelectionPanel'),
			MochiKit.Base.method(this, 'refreshSelectedCards')
		], {trace:false});
	},
	
	//............................................................................
	
	showArchivedCards_handler: function () {
		return Clipperz.Async.callbacks("MainController.showArchivedCards_handler", [
			MochiKit.Base.method(this, 'setShouldIncludeArchivedCards', true),
			MochiKit.Base.method(this, 'refreshUI')
		], {trace:false});
	},
	
	hideArchivedCards_handler: function () {
		return Clipperz.Async.callbacks("MainController.hideArchivedCards_handler", [
			MochiKit.Base.method(this, 'setShouldIncludeArchivedCards', false),
			MochiKit.Base.method(this, 'refreshUI')
		], {trace:false});

	},
	
	//----------------------------------------------------------------------------
	
	setCloseMaskAction: function (aFunction) {
		this._closeMaskAction = aFunction;
	},
	
	maskClick_handler: function () {
		return Clipperz.Async.callbacks("MainController.maskClick_handler", [
			MochiKit.Base.bind(function () {
				this._closeMaskAction.apply(this);
				this._closeMaskAction = null;
			}, this)
		], {trace:false});
	},

	//............................................................................

	showHelp_handler: function () {
		return Clipperz.Async.callbacks("MainController.showHelp_handler", [
			MochiKit.Base.bind(function () {
				if (this.currentPage() == 'mainPage') {
					this.setPageProperties(this.currentPage(), 'showHelp', true);
				}
			}, this)
		], {trace:false});
	},

	closeHelp_handler: function () {
		return Clipperz.Async.callbacks("MainController.closeHelp_handler", [
			MochiKit.Base.method(this, 'setPageProperties', this.currentPage(), 'showHelp', false)
		], {trace:false});
	},

	isShowingHelp: function () {
		return this.pages()[this.currentPage()].props['showHelp'];
	},

	//============================================================================

	matchMediaQuery_handler: function (newQueryStyle) {
		var wasInEditMode = this.isPageInEditMode();
		var currentPage = this.currentPage();
		var selectedCardInfo = this.selectedCardInfo();

		this._mediaQueryStyle = newQueryStyle;

		MochiKit.DOM.setElementClass(document.body, newQueryStyle);

		if (currentPage == 'cardDetailPage') {
			this.moveOutPage(this.currentPage(), 'mainPage');
		}

		if (selectedCardInfo) {
			this.pages()[currentPage].setProps({'mode': 'view'});

			if (currentPage == 'mainPage' && newQueryStyle == 'narrow') {
				this.selectCard(selectedCardInfo, true);
			}
			if (wasInEditMode) {
				MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'enterEditMode'));
				MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'updateSelectedCard', this.selectedCardInfo(), false, true));
			}
		}

		this.resetPanels();
		this.refreshCurrentPage();
	},

	unmatchMediaQuery_handler: function (queryStyle) {
	},

	mediaQueryStyle: function () {
		return this._mediaQueryStyle;
	},

	//----------------------------------------------------------------------------

	isTouchDevice: function () {
		return this._isTouchDevice;
	},
	
	isDesktop: function () {
		return this._isDesktop;
	},

	hasKeyboard: function () {
		return this._hasKeyboard;
	},

	//============================================================================

	downloadOfflineCopy_handler: function (anEvent) {
		var downloadHref;
		var	deferredResult;
		var newWindow;
		
		downloadHref = window.location.href.replace(/\/[^\/]*$/,'') + Clipperz_dumpUrl;
		newWindow = window.open("", "");

		deferredResult = new Clipperz.Async.Deferred("AppController.handleDownloadOfflineCopy", {trace:false});
		deferredResult.addCallback(MochiKit.Base.method(this.user().connection(), 'message'), 'echo', {'echo':"echo"});
		deferredResult.addCallback(function(aWindow) {
			aWindow.location.href = downloadHref;
		}, newWindow);
		deferredResult.callback();

		return deferredResult;
	},

	//============================================================================

	focusOnSearch: function (anEvent) {
		anEvent.preventDefault();
		
		if (this.pages()[this.currentPage()].props['mode'] == 'edit') {
			//	pass
		} else {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleSelectionPanel');
			MochiKit.DOM.getElement('searchValue').focus();
			MochiKit.DOM.getElement('searchValue').select();
		}
	},

	exitSearch_handler: function (anEvent) {
		return Clipperz.Async.callbacks("MainController.exitSearch_handler", [
			MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'toggleSelectionPanel'),
			MochiKit.Base.method(MochiKit.DOM.getElement('searchValue'), 'blur')
		], {trace:false});
	},

	selectPreviousCard: function () {
		var	prevCardInfo;
		var	shouldUpdateCardDetail;
		
		prevCardInfo = this.previousCardInfo();
		shouldUpdateCardDetail = this.shouldShowCardDetailWhenMovingBetweenCardsUsingKeys();

		if (prevCardInfo != null) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectCard', {'cardInfo':prevCardInfo, 'update':shouldUpdateCardDetail});
		}
	},

	selectNextCard: function () {
		var	nextCardInfo;
		var	shouldUpdateCardDetail;

		nextCardInfo = this.nextCardInfo();
		shouldUpdateCardDetail = this.shouldShowCardDetailWhenMovingBetweenCardsUsingKeys();

		if (nextCardInfo != null) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectCard', {'cardInfo':nextCardInfo, 'update':shouldUpdateCardDetail});
		}
	},

	selectDetail: function () {
//console.log("TODO: SELECT DETAIL (right arrow key)", this.mediaQueryStyle(), this.currentPage(), this.selectedCardInfo());
		if ((this.mediaQueryStyle() == 'narrow') && (this.currentPage() == 'mainPage')) {
			Clipperz.Async.callbacks("MainController.selectDetail", [
				MochiKit.Base.method(this.user(), 'getRecord', this.selectedCardInfo()['reference']),
				MochiKit.Base.method(this, 'collectRecordInfo'),
				MochiKit.Base.method(this, 'showCardDetailInNarrowView'),
			], {trace:false}).callback();
		}
	},

	exitCurrentSelection: function () {
		if (this.isShowingHelp()) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'closeHelp');
		} else {
			var	currentPage = this.pages()[this.currentPage()];
			
			if (currentPage.props['showCertificatePreview'] == true) {
				currentPage.setProps({'showCertificatePreview': false});
			} else if (this.currentPage() == 'cardDetailPage') {
				MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'goBackToMainPage', {'reference':this.selectedCardInfo()['reference']});
			} else if (this.currentPage() == 'mainPage') {
				MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectCard', {'cardInfo':null, 'update':true});
			}
		}
	},
	
	//============================================================================

	addNotification_handler: function (aNotification) {
//console.log("ADD NOTIFICATION", aNotification);
		var	notification;

		notification = {
			'id':		Clipperz.PM.Crypto.randomKey(),
			'date':		new Date(),
			'message':	aNotification['message'],
			'level':	aNotification['level'],
			'callback':	aNotification['callback'],
		};

		this.notifications().unshift(notification);
		this.updateNotifications();
	},

	acknowledgeNotification_handler: function (aNotificationReference) {
//console.log("ACKNOWLEDGE NOTIFICATION", aNotificationReference);
		var	objectIndex = -1;
		var	i, c;
		
		c = this.notifications().length;
		for (i=0; i<c; i++) {
			if (this.notifications()[i]['id'] == aNotificationReference) {
				objectIndex = i;
			}
		}
		
		if (objectIndex != -1) {
			Clipperz.Base.removeObjectAtIndexFromArray(objectIndex, this.notifications());
		} else {
			console.log("WARNING: notification with ID '" + aNotificationReference + "' not found");
		}
		this.updateNotifications();
	},

	notifications: function () {
		return this._notifications;
	},

	updateNotifications: function () {
//console.log("UPDATE NOTIFICATIONS");
		this.setPageProperties(this.currentPage(), 'notifications', this.notifications());
	},

/*
	updateAttachmentQueueInfo_handler: function(someProperties) {
		return Clipperz.Async.callbacks("MainController.updateAttachmentQueueInfo_handler", [
			MochiKit.Base.method(this, 'setPageProperties', this.currentPage(), 'attachmentQueueInfo', this.attachmentQueueInfo())
		], {trace:false});
	},
*/
	addAttachment_handler: function (aFileInfo) {	//	aReference, someMetadata, aKey, aNonce
		var deferredResult;
		var	record = aFileInfo['record'];
		var	file = aFileInfo['file'];
//console.log("addAttachment_handler", aFileInfo, record, file);
		deferredResult = new Clipperz.Async.Deferred("MainController.addCardAttachment_handler", {'trace':false});

		deferredResult.addMethod(this.overlay(), 'show', "reading", true);
		deferredResult.addMethod(record, 'createNewAttachment');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setFile', file));
		deferredResult.addMethod(this.attachmentController(), 'addAttachment');
		deferredResult.addMethod(this.overlay(), 'hide', true);
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', record.reference());
		deferredResult.callback();
		
		return deferredResult;
	},

	copyFieldValueFeedback_handler: function () {
//		this.overlay().show("copy", true);
//		MochiKit.Async.callLater(0.5, MochiKit.Base.method(this.overlay(), 'hide', true));
		this.overlay().show("copying …", true);
		this.overlay().customResult('copy', "copied", 0.5);
	},

	uploadMessageCallback: function (someArguments, aProgressCallback) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("MainController.uploadMessageCallback", {'trace':false});
		deferredResult.addMethod(this.user().connection(), 'uploadAttachment', someArguments, aProgressCallback);
		deferredResult.callback();

		return deferredResult;
	},

	//----------------------------------------------------------------------------

	removeAttachment_handler: function (anAttachmentInfo) {
		var deferredResult;
		var record = anAttachmentInfo['record'];
		var attachment = anAttachmentInfo['attachment'];
		
		deferredResult = new Clipperz.Async.Deferred("MainController.removeAttachment_handler", {trace: false});
		deferredResult.addCallback(MochiKit.Base.method(this, 'ask', {
			'question': "Do you really want to delete this attachment?",
			'possibleAnswers':{
				'cancel':	{'label':"No",	'isDefault':true,	'answer':MochiKit.Base.methodcaller('cancel', new MochiKit.Async.CancelledError())},
				'revert':	{'label':"Yes",	'isDefault':false,	'answer':MochiKit.Base.methodcaller('callback')}
			}
		})),
		deferredResult.addMethod(this, 'cancelAttachment_handler', attachment);
		deferredResult.addMethod(record, 'removeAttachment', attachment);
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', record.reference());

		deferredResult.callback();
		
		return deferredResult;
	},

	//----------------------------------------------------------------------------

	getAttachment_handler: function (anAttachment) {
		this.attachmentController().getAttachment(anAttachment);
	},

	downloadMessageCallback: function(anAttachment, aProgressCallback) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("MainController.downloadAttachment_handler", {'trace':false});
		deferredResult.addMethod(this.user().connection(), 'downloadAttachment', {
			'reference': anAttachment.reference()
		}, aProgressCallback);
		deferredResult.callback();

		return deferredResult;
	},

	//----------------------------------------------------------------------------

	cancelAttachment_handler: function(anAttachment) {
		return this.attachmentController().cancelAttachment(anAttachment);
	},
/*
	closeAttachmentNotification_handler: function(aNotificationId) {
		return Clipperz.Async.callbacks("MainController.", [
			MochiKit.Base.method(this.attachmentController(), 'removeNotification', aNotificationId)
		], {trace:false});

	},
*/
	//----------------------------------------------------------------------------

	reloadAttachmentServerStatusCallback: function(aRecord) {
		if (! aRecord.isBrandNew()) {
			return Clipperz.Async.callbacks("MainController.reloadAttachmentServerStatus_handler", [
				MochiKit.Base.method(aRecord, 'updatedAttachmentServerStatus'),
				MochiKit.Base.bind(function () { 
					if (this._selectedCardInfo && this._selectedCardInfo['reference']) {
						this.refreshUI(this._selectedCardInfo['reference']);
					}
				}, this),
			], {trace:false});
		}
	},

	//============================================================================

	registerHistoryHandler: function() {
		window.onpopstate = MochiKit.Base.method(this, 'handleOnpopstate');
	},

	handleOnpopstate: function (anEvent) {
		if (this.filter().type != 'ALL') {
			this.selectAllCards();
//			window.history.pushState(window.history.state, "", window.location.toString());
			this.historyPushState(this.historyState, "", window.location.toString());
		} else if(anEvent.state) {
			if (this.isPageInEditMode()) {
//				window.history.pushState({selectedCardInfo: this.selectedCardInfo()}, "", window.location.toString());
				this.historyPushState({selectedCardInfo: this.selectedCardInfo()}, "", window.location.toString());
				this.cancelCardEdits(this.selectedCardReference());
			} else {
				if (anEvent.state['selectedCardInfo']) {
					this.selectCard(anEvent.state['selectedCardInfo'], true);
				} else {
					this.selectCard(null, true);
					this.goBackToMainPage();
				}
			}
		}

		// console.log('History changed', anEvent.state);
	},

	//============================================================================
/*
	wrongAppVersion: function (anError) {
//		this.pages()['errorPage'].setProps({message:anError.message});
//		this.moveInPage('errorPage', this.currentPage());
	},
*/
	//=========================================================================
	__syntaxFix__: "syntax fix"
});
