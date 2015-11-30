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

	this._shouldIncludeArchivedCards = false;

	this._isSelectionPanelOpen = false;
	this._isSettingsPanelOpen = false;
	this._isAttachmentQueueBoxOpen = false;
	
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
		'logout',
		'enableLock', 'disableLock', 'unlock',
		'updatePIN', 'disablePIN', 'forcePassphraseLogin', 'forcePassphraseUnlock',
		'changePassphrase', 'deleteAccount',
		/*'updateUserPreferences',*/ 'setPreferences',
		'updateOTPListAndDetails', 'createNewOTP', 'deleteOTPs', 'changeOTPLabel',
		'importCards',
		'downloadExport',
		'updateProgress',
		'toggleSelectionPanel', 'hideSelectionPanel', 'toggleSettingsPanel', 'toggleAttachmentQueueBox',
		'matchMediaQuery', 'unmatchMediaQuery',
		'selectAllCards', 'selectRecentCards', 'selectCardsWithAttachments', 'selectUntaggedCards', 'tagSelected', 'search',
		'refreshCardEditDetail',
		'saveCardEdits', 'cancelCardEdits',
		'selectCard',
		'addCardClick',
		'deleteCard', 'toggleArchiveCard', 'cloneCard', 'editCard',
		'addTag', 'removeTag',
		'showArchivedCards', 'hideArchivedCards',
		'goBackToMainPage',
		'maskClick',
		'closeHelp',
		'downloadOfflineCopy',
		'runDirectLogin', 'removeDirectLogin',
		'exitSearch',
		'updateAttachmentQueueInfo', 'addAttachment', 'removeAttachment', 'getAttachment', 'cancelAttachment', 'closeAttachmentNotification',
	]);

	this._attachmentController = new Clipperz.PM.UI.AttachmentController({
		'uploadMessageCallback':      MochiKit.Base.method(this, 'uploadMessageCallback'),
		'downloadMessageCallback':    MochiKit.Base.method(this, 'downloadMessageCallback'),
		'reloadServerStatusCallback': MochiKit.Base.method(this, 'reloadAttachmentServerStatusCallback')
	});

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
		this.overlay().updateProgress(aProgressPercentage);
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
			self._pages[aPageName] = React.render(
				Clipperz.PM.UI.Components.Pages[self.capitaliseFirstLetter(aPageName)](self.pageProperties(aPageName)),
				MochiKit.DOM.getElement(aPageName)
			);
		});
	},

	registerForNotificationCenterEvents: function (events) {
		var	self = this;

		MochiKit.Iter.forEach(events, function (anEvent) {
			MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, anEvent, MochiKit.Base.method(self, anEvent + '_handler'));
			MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, anEvent, MochiKit.Base.method(self, 'resetLockTimeout'));
		});

//		MochiKit.Signal.connect(window, 'onpopstate',		MochiKit.Base.method(this, 'historyGoBack'));
//		MochiKit.Signal.connect(window, 'onbeforeunload',	MochiKit.Base.method(this, 'shouldExitApp'));
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
		this.setCurrentPage('loginPage');
		registrationPage.setProps({});
		this.moveInPage(currentPage, 'registrationPage');
		MochiKit.Async.callLater(0.5, MochiKit.Base.method(registrationPage, 'setInitialFocus'));
	},

	forcePassphraseLogin_handler: function() {
		this.pages()['loginPage'].setProps({'forceCredentials': true});
		MochiKit.Async.callLater(0.1, MochiKit.Base.method(this.pages()['loginPage'], 'setInitialFocus'));
	},

	forcePassphraseUnlock_handler: function() {
		this.pages()['unlockPage'].setProps({'forceCredentials': true});
		MochiKit.Async.callLater(0.1, MochiKit.Base.method(this.pages()['unlockPage'], 'setInitialFocus'));
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

		this.overlay().show("logging in");
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
		deferredResult.addMethod(this.overlay(), 'done', "", 1);
		deferredResult.addErrback(MochiKit.Base.method(this, 'genericErrorHandler', someCredentials, "login failed"));
		deferredResult.addErrback(MochiKit.Base.bind(function (anEvent, anError) {
			if (anError['isPermanent'] != true) {
				this.pages()['loginPage'].setProps({disabled:false, 'mode':this.loginMode()});
				this.pages()['loginPage'].setInitialFocus();
			}
			return anError;
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

	unlock_handler: function(aCredential, aCredentialType) {
		var deferredResult;
		var user, passphrase, getPassphraseDelegate;

		// var user = this.user();
		var oldUser = this.user();
		var unlockPage = this.pages()['unlockPage'];
		var overlay = this.overlay();

		overlay.show("validating…");
		passphrase = (aCredentialType=='PIN') ? Clipperz.PM.PIN.credentialsWithPIN(aCredential)['passphrase'] : aCredential;

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
			if (aCredentialType=='PIN') {
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

		if (aCredentialType=='PIN') {
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
		deferredResult.addMethod(overlay, 'done', "", 0.5);

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

	collectFieldInfo: function (aField) {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred('MainController.collectFieldInfo', {trace:false});
		deferredResult.setValue('_field');
		deferredResult.addMethod(aField, 'reference');
		deferredResult.setValue('_reference');
		deferredResult.addMethod(aField, 'label');
		deferredResult.setValue('label');
		deferredResult.addMethod(aField, 'value');
		deferredResult.setValue('value');
		deferredResult.addMethod(aField, 'actionType');
		deferredResult.setValue('actionType');
		deferredResult.addMethod(aField, 'isHidden');
		deferredResult.setValue('isHidden');
		deferredResult.values();

		deferredResult.callback(aField);
		
		return deferredResult;
	},

	collectDirectLoginInfo: function (aDirectLogin) {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred('MainController.collectDirectLoginInfo', {trace:false});
		deferredResult.addMethod(aDirectLogin, 'reference');
		deferredResult.setValue('_reference');
		deferredResult.addMethod(aDirectLogin, 'label');
		deferredResult.setValue('label');
		deferredResult.addMethod(aDirectLogin, 'favicon');
		deferredResult.setValue('favicon');
		deferredResult.values();

		deferredResult.callback();
		
		return deferredResult;
	},
	
	collectAttachmentInfo: function(anAttachment) {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred('MainController.collectAttachmentInfo', {trace:false});
		deferredResult.setValue('_attachment'); // The object itself, maybe this should be the only value being passed
		deferredResult.addMethod(anAttachment, 'reference');
		deferredResult.setValue('_reference');
		deferredResult.addMethod(anAttachment, 'name');
		deferredResult.setValue('name');
		deferredResult.addMethod(anAttachment, 'contentType');
		deferredResult.setValue('contentType');
		deferredResult.addMethod(anAttachment, 'size');
		deferredResult.setValue('size');
		deferredResult.values();

		deferredResult.callback(anAttachment);
		
		return deferredResult;
	},
	
	collectRecordInfo: function (aRecord) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.collectRecordInfo', {trace:false});
		deferredResult.setValue('_record');
		deferredResult.addMethod(aRecord, 'reference');
		deferredResult.setValue('_reference');
		deferredResult.addMethod(aRecord, 'isArchived');
		deferredResult.setValue('_isArchived');
		deferredResult.addMethod(aRecord, 'isBrandNew');
		deferredResult.setValue('_isBrandNew');
//		deferredResult.addMethod(aRecord, 'hasPendingChanges');
		deferredResult.addMethod(this.user(), 'hasPendingChanges');
		deferredResult.setValue('hasPendingChanges');
		deferredResult.addMethod(aRecord, 'label');
		deferredResult.setValue('label');
		deferredResult.addMethod(aRecord, 'notes');
		deferredResult.setValue('notes');
		deferredResult.addMethod(aRecord, 'tags');
		deferredResult.setValue('tags');

		deferredResult.addMethod(aRecord, 'fields');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.method(this, 'collectFieldInfo'));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.setValue('fields');

		deferredResult.addMethod(aRecord, 'attachments');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.method(this, 'collectAttachmentInfo'));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.setValue('attachments');

		deferredResult.addMethod(aRecord, 'directLogins');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.method(this, 'collectDirectLoginInfo'));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.setValue('directLogins');

		deferredResult.addMethod(aRecord, 'getAttachmentServerStatus');
		deferredResult.setValue('attachmentServerStatus');

		deferredResult.values();

		deferredResult.callback(aRecord);
		
		return deferredResult;
	},

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
		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'lock', MochiKit.Base.method(this, 'lock'));
	},
	
	disableLock_handler: function () {
		MochiKit.Signal.disconnectAll(Clipperz.Signal.NotificationCenter, 'lock');
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

	runApplication: function (anUser) {
		window.history.replaceState({selectedCardInfo: null}, "", window.location.toString());
		window.history.pushState({selectedCardInfo: null}, "", window.location.toString());	// Hack to support filters undo with no other actions
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

	removeDirectLogin_handler: function (aRecord, aDirectLoginReference) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("MainController.removeDirectLogin_handler", {'trace': false});
		
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(aDirectLoginReference));
		deferredResult.addMethodcaller('remove');
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', aRecord.reference());

		deferredResult.callback(aRecord);

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
				'paymentVerificationPending'
			];
			
			var	attributes = this.user().accountInfo()._attributes;
			MochiKit.Iter.forEach(usefulFields, function (aFieldName) {
				result[aFieldName] = attributes[aFieldName];
			})
		};
		
		return result;
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
			queue = this._attachmentController.getQueueInfo();
			notifications = this._attachmentController.getNotificationsInfo();
			elementFetchCallback = MochiKit.Base.method(this._attachmentController, 'getQueueElement');
		} else {
			queue = [];
			notifications = [];
			elementFetchCallback = null;
		}

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
				'attachmentQueueBoxStatus':		this.isAttachmentQueueBoxOpen()	? 'OPEN' : 'CLOSED',
				'featureSet':					this.featureSet(),
				'features':						this.features(),
				'proxyInfo':					this.proxyInfo(),
				'locked':						false,
				'attachmentQueueInfo':			this.attachmentQueueInfo(),
//				'shouldIncludeArchivedCards':	this.shouldIncludeArchivedCards(),
//				'cards':				…,
//				'tags':					…,
//				'selectedCard':			…,
			};
		} else if (aPageName == 'cardDetailPage') {
			extraProperties = {
				'attachmentQueueInfo':			this.attachmentQueueInfo(),
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
		if (this.isSelectionPanelOpen()) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleSelectionPanel');
		}
	},

	isSettingsPanelOpen: function () {
		return this._isSettingsPanelOpen;
	},

	toggleSettingsPanel_handler: function (anEvent) {
		this._isSettingsPanelOpen = !this._isSettingsPanelOpen;
		this.setCloseMaskAction(MochiKit.Base.method(this, 'toggleSettingsPanel_handler'));
		if (this._isSettingsPanelOpen == false) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'closeSettingsPanel');
		}
		this.refreshCurrentPage();
	},

	isAttachmentQueueBoxOpen: function() {
		return this._isAttachmentQueueBoxOpen;
	},

	toggleAttachmentQueueBox_handler: function (anEvent) {
		this._isAttachmentQueueBoxOpen = !this._isAttachmentQueueBoxOpen;
		this.refreshCurrentPage();
	},

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

	selectCard_handler: function (someInfo, shouldUpdateCardDetail) {
		this.selectCard(someInfo, shouldUpdateCardDetail);
		window.history.pushState({selectedCardInfo: someInfo}, "", window.location.toString());
		// console.log("pushing state");
	},

	refreshCardEditDetail_handler: function (aRecordReference) {
		this.updateSelectedCard({'reference':aRecordReference}, false, true);
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
	
	changeOTPLabel_handler: function (aReference, aLabel) {
		return Clipperz.Async.callbacks("MainController.changeOTPLabel_handler", [
			MochiKit.Base.method(this.overlay(), 'show', "", true),
			MochiKit.Base.method(this.user(), 'changeOTPLabel', aReference, aLabel),
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

			function (aValue) {
				MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectCard', cardInfo);
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
		this.goBackToMainPage(anEvent);
		window.history.pushState({selectedCardInfo: null}, "", window.location.toString());
	},

	//============================================================================

	selectAllCards_handler: function () {
		this.selectAllCards();
	},

	selectAllCards: function () {
		this.setPageProperties('mainPage', 'searchTerm', '');
		this.resetSelectedCard();
		this.setFilter('ALL');
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'hideSelectionPanel');
		return this.refreshSelectedCards();
	},
	
	selectRecentCards_handler: function () {
		this.resetSelectedCard();
		this.setFilter('RECENT');
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'hideSelectionPanel');
		return this.refreshSelectedCards();
	},

	selectCardsWithAttachments_handler: function () {
		this.resetSelectedCard();
		this.setFilter('WITH_ATTACHMENTS');
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'hideSelectionPanel');
		return this.refreshSelectedCards();
	},

	search_handler: function (aValue) {
		this.resetSelectedCard();

		if (aValue == "") {
			this.setFilter('ALL');
		} else {
			this.setFilter('SEARCH', aValue);
		}

		this.setPageProperties('mainPage', 'searchTerm', aValue);
		return this.refreshSelectedCards();
	},

	tagSelected_handler: function (aTag) {
		this.resetSelectedCard();
		this.setFilter('TAG', aTag);
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'hideSelectionPanel');
		return this.refreshSelectedCards();
	},

	selectUntaggedCards_handler: function () {
		this.resetSelectedCard();
		this.setFilter('UNTAGGED');
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'hideSelectionPanel');
		return this.refreshSelectedCards();
	},
	
	//............................................................................
	
	showArchivedCards_handler: function () {
		this.setShouldIncludeArchivedCards(true);
		return this.refreshUI();
	},
	
	hideArchivedCards_handler: function () {
		this.setShouldIncludeArchivedCards(false);
		return this.refreshUI();
	},
	
	//----------------------------------------------------------------------------
	
	setCloseMaskAction: function (aFunction) {
		this._closeMaskAction = aFunction;
	},
	
	maskClick_handler: function () {
		this._closeMaskAction.apply(this);
		this._closeMaskAction = null;
	},

	//............................................................................

	showHelp_handler: function () {
		if (this.currentPage() == 'mainPage') {
			this.setPageProperties(this.currentPage(), 'showHelp', true);
		}
	},

	closeHelp_handler: function () {
		this.setPageProperties(this.currentPage(), 'showHelp', false);
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
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleSelectionPanel');
		MochiKit.DOM.getElement('searchValue').blur();
	},

	selectPreviousCard: function () {
		var	prevCardInfo;
		var	shouldUpdateCardDetail;
		
		prevCardInfo = this.previousCardInfo();
		shouldUpdateCardDetail = this.shouldShowCardDetailWhenMovingBetweenCardsUsingKeys();

		if (prevCardInfo != null) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectCard', prevCardInfo, shouldUpdateCardDetail);
		}
	},

	selectNextCard: function () {
		var	nextCardInfo;
		var	shouldUpdateCardDetail;

		nextCardInfo = this.nextCardInfo();
		shouldUpdateCardDetail = this.shouldShowCardDetailWhenMovingBetweenCardsUsingKeys();

		if (nextCardInfo != null) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectCard', nextCardInfo, shouldUpdateCardDetail);
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
			if (this.currentPage() == 'cardDetailPage') {
				MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'goBackToMainPage', {'reference':this.selectedCardInfo()['reference']});
			} else if (this.currentPage() == 'mainPage') {
				MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectCard', null, true);
			}
		}
	},
	
	//============================================================================

	updateAttachmentQueueInfo_handler: function(someProperties) {
		this.setPageProperties(this.currentPage(), 'attachmentQueueInfo', this.attachmentQueueInfo());
	},

	addAttachment_handler: function(aRecord, aFile) {	//	aReference, someMetadata, aKey, aNonce
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("MainController.addCardAttachment_handler", {'trace':false});

		deferredResult.addMethod(aRecord, 'createNewAttachment');
		deferredResult.addCallback(MochiKit.Base.methodcaller('setFile', aFile));
		deferredResult.addMethod(this.attachmentController(), 'addAttachment');
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', aRecord.reference());
		deferredResult.callback();
		
		return deferredResult;
	},

	uploadMessageCallback: function(someArguments, aProgressCallback) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("MainController.uploadMessageCallback", {'trace':false});
		deferredResult.addMethod(this.user().connection(), 'uploadAttachment', someArguments, aProgressCallback);
		deferredResult.callback();

		return deferredResult;
	},

	//----------------------------------------------------------------------------

	removeAttachment_handler: function(aRecord, anAttachment) {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("MainController.removeAttachment_handler", {trace: false});
		deferredResult.addCallback(MochiKit.Base.method(this, 'ask', {
			'question': "Do you really want to delete this attachment?",
			'possibleAnswers':{
				'cancel':	{'label':"No",	'isDefault':true,	'answer':MochiKit.Base.methodcaller('cancel', new MochiKit.Async.CancelledError())},
				'revert':	{'label':"Yes",	'isDefault':false,	'answer':MochiKit.Base.methodcaller('callback')}
			}
		})),
		deferredResult.addMethod(this, 'cancelAttachment_handler', anAttachment);
		deferredResult.addMethod(aRecord, 'removeAttachment', anAttachment);
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', aRecord.reference());

		deferredResult.callback();
		
		return deferredResult;
	},

	//----------------------------------------------------------------------------

	getAttachment_handler: function(anAttachment) {
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

	closeAttachmentNotification_handler: function(aNotificationId) {
		return this.attachmentController().removeNotification(aNotificationId);
	},

	//----------------------------------------------------------------------------

	reloadAttachmentServerStatusCallback: function(aRecord) {
		if (! aRecord.isBrandNew()) {
			return Clipperz.Async.callbacks("MainController.reloadAttachmentServerStatus_handler", [
				MochiKit.Base.method(this.user(), 'getRecordDetail', aRecord),
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
			window.history.pushState(window.history.state, "", window.location.toString());
		} else if(anEvent.state) {
			if (this.isPageInEditMode()) {
				window.history.pushState({selectedCardInfo: this.selectedCardInfo()}, "", window.location.toString());
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
