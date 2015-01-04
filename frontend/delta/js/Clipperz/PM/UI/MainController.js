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

"use strict";
Clipperz.Base.module('Clipperz.PM.UI');

Clipperz.PM.UI.MainController = function() {
	var	genericPageProperties;

//	this._proxy		= null;
	this._mediaQueryStyle = "narrow";
	this._user		= null;
	this._filter	= {'type':'ALL'};

	this._shouldIncludeArchivedCards = false;

	this._isSelectionPanelOpen = false;
	this._isSettingsPanelOpen = false;
	
	this._pageStack = ['loadingPage'];
	this._overlay =  new Clipperz.PM.UI.Components.Overlay();

	this._isTouchDevice = ('ontouchstart' in window || 'onmsgesturechange' in window);
	this._isDesktop = window.screenX != 0 && !this._isTouchDevice;
	this._hasKeyboard = this._isDesktop;

	this._recordsInfo = null;
	this._selectedCards = null;

	this._selectedCardInfo = null;

	this._closeMaskAction = null;
	
	this._pages = {};
	this.renderPages([
		'loginPage',
		'registrationPage',
		'mainPage',
		'cardDetailPage',
		'errorPage',
	]);

	this.registerForNotificationCenterEvents([
		'doLogin', 'registerNewUser', 'showRegistrationForm', 'goBack',
		'toggleSelectionPanel', 'toggleSettingsPanel',
		'matchMediaQuery', 'unmatchMediaQuery',
		'selectAllCards', 'selectRecentCards', 'search', 'tagSelected', 'selectUntaggedCards',
		'refreshCardEditDetail',
		'saveCardEdits', 'cancelCardEdits',
		'selectCard',
		'addCardClick',
		'deleteCard', 'toggleArchiveCard', 'cloneCard', 'editCard',
		'addTag', 'removeTag',
		'showArchivedCards', 'hideArchivedCards',
		'goBackToMainPage',
		'maskClick',
		'downloadOfflineCopy',
		'runDirectLogin',
		'exitSearch'
	]);

	Mousetrap.bind({
		'/':	MochiKit.Base.method(this, 'focusOnSearch'),
		'up':	MochiKit.Base.method(this, 'selectPreviousCard'),
		'down':	MochiKit.Base.method(this, 'selectNextCard'),
	})

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

	capitaliseFirstLetter: function (aValue) {
	    return aValue.charAt(0).toUpperCase() + aValue.slice(1);
	},
	
	renderPages: function (pages) {
		var self = this;
		MochiKit.Iter.forEach(pages, function (aPageName) {
//console.log("RENDERING", aPageName);
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
		this.pages()['loginPage'].setProps({'mode':this.loginMode(), 'isNewUserRegistrationAvailable':canRegisterNewUsers});

		if (shouldShowRegistrationForm) {
			this.showRegistrationForm_handler();
		} else {
			this.showLoginForm();
		}

//		this.overlay().done("", 0.5);
		this.overlay().hide();
	},

	//-------------------------------------------------------------------------

	showLoginForm: function () {
		var	loginFormPage;

		loginFormPage = this.pages()['loginPage'];
		loginFormPage.setProps({'mode':this.loginMode(), 'isNewUserRegistrationAvailable':Clipperz.PM.Proxy.defaultProxy.canRegisterNewUsers()});
		this.moveInPage(this.currentPage(), 'loginPage');
		MochiKit.Async.callLater(0.5, MochiKit.Base.method(loginFormPage, 'setInitialFocus'));
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
		deferredResult.addMethod(Clipperz.PM.PIN, 'resetFailedAttemptCount');
		deferredResult.addMethod(this, 'setUser', user);
		deferredResult.addMethod(this, 'runApplication');
		deferredResult.addMethod(this.overlay(), 'done', "", 1);
		deferredResult.addErrback(MochiKit.Base.method(this, 'genericErrorHandler', someCredentials));
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
		deferredResult.addErrback(MochiKit.Base.method(this, 'genericErrorHandler', credentials));
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
//console.log("SET USER", aUser);
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
	
	collectRecordInfo: function (aRecord) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred('MainController.collectRecordInfo', {trace:false});
		deferredResult.setValue('_record');
		deferredResult.addMethod(aRecord, 'reference');
		deferredResult.setValue('_reference');
		deferredResult.addMethod(aRecord, 'isArchived');
		deferredResult.setValue('_isArchived');
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

		deferredResult.addMethod(aRecord, 'directLogins');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.method(this, 'collectDirectLoginInfo'));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.setValue('directLogins');

		deferredResult.values();

		deferredResult.callback(aRecord);
		
		return deferredResult;
	},

	updateSelectedCard: function (someInfo, shouldShowLoading) {
		var deferredResult;
		var showLoading = typeof(shouldShowLoading) !== 'undefined' ? shouldShowLoading : true;

		if (someInfo == null) {
			this.setPageProperties('mainPage', 'selectedCard', {});
			deferredResult = MochiKit.Async.succeed();
		} else {
			if (showLoading) {
				this.setPageProperties('mainPage', 'selectedCard', {'loading':true, 'label':someInfo['label'], '_reference':someInfo['reference']});
			}

			deferredResult = new Clipperz.Async.Deferred('MainController.updateSelectedCard', {trace:false});
			deferredResult.addMethod(this.user(), 'getRecord', someInfo['reference']);
			deferredResult.addMethod(this, 'collectRecordInfo');

//console.log("MEDIA QUERY STYLE", this.mediaQueryStyle());
			deferredResult.addMethod(this, 'setPageProperties', 'mainPage', 'selectedCard');
			if (this.mediaQueryStyle() == 'narrow') {
				deferredResult.addMethod(this, 'setPageProperties', 'cardDetailPage', 'selectedCard');
				deferredResult.addMethod(this, 'moveInPage', this.currentPage(), 'cardDetailPage');
//				deferredResult.addCallback(function (aValue) { console.log("SHOULD SLIDE IN PAGE DETAIL"); return aValue; });
//console.log("SHOULD SLIDE IN PAGE DETAIL");
			}
		
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
			sortCriteria = Clipperz.Base.reverseComparator(MochiKit.Base.keyComparator('accessDate'));
			rangeFilter = function (someCards) { return someCards.slice(0, 9)};
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
//		deferredResult.addMethod(this, 'resetRecordsInfo'),
		deferredResult.addMethod(this, 'refreshSelectedCards');
		deferredResult.addMethod(this, 'renderTags');
		
		if (selectedCardReference != null) {
			deferredResult.addMethod(this.user(), 'getRecord', selectedCardReference);
			deferredResult.addMethod(this, 'collectRecordInfo');
			deferredResult.addMethod(this, 'setPageProperties', 'mainPage', 'selectedCard');
		}
		deferredResult.callback();
		
		return deferredResult;
	},
	
	//----------------------------------------------------------------------------

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
		this.moveInPage(this.currentPage(), 'mainPage');
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

			shouldAddItemToHistory = typeof(addToHistory) == 'undefined' ? false : addToHistory;

			this.slidePage(MochiKit.DOM.getElement(fromPage), MochiKit.DOM.getElement(toPage), 'LEFT');
			this.setCurrentPage(toPage);

			if (shouldAddItemToHistory) {
//console.log("ADD ITEM TO HISTORY");
//console.log("ADD ITEM TO HISTORY - window", window);
//console.log("ADD ITEM TO HISTORY - window.history", window.history);
				window.history.pushState({'fromPage': fromPage, 'toPage': toPage});
//#				window.history.pushState();
//console.log("ADDED ITEM TO HISTORY");
			} else {
//console.log("Skip HISTORY");
			}
		} else {
//console.log("No need to move in the same page");
		}
	},

	moveOutPage: function (fromPage, toPage) {
		this.slidePage(MochiKit.DOM.getElement(fromPage), MochiKit.DOM.getElement(toPage), 'RIGHT');
		this.setCurrentPage(toPage);
	},

	//-------------------------------------------------------------------------

	messageBoxContent: function () {
		var	message;
		var	level;
		
		message = "";
		level = 'HIDE';
		
//console.log("messageBox - this.user()", this.user());
		if (this.user() != null && this.user().accountInfo() != null && this.user().accountInfo().featureSet() == 'EXPIRED') {
			message = "Exprired subscription";
			level = 'ERROR';
		}

		return {
			'message': message,
			'level': level
		};
	},

	userAccountInfo: function () {
		var	result;
		
		result = {};
		
		if (this.user() != null) {
			var	usefulFields = [
				'currentSubscriptionType',
				'expirationDate',
				'featureSet',
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
	
	genericPageProperties: function () {
		return {
			'style':			this.mediaQueryStyle(),
		    'isTouchDevice':	this.isTouchDevice(),
		    'isDesktop':		this.isDesktop(),
			'hasKeyboard':		this.hasKeyboard()
		};
	},
	
	pageProperties: function (aPageName) {
		var	result;
		var	extraProperties = null;
		
		result = this.genericPageProperties();

		if (aPageName == 'loginPage') {
			extraProperties = {
				'mode':								'CREDENTIALS',
				'isNewUserRegistrationAvailable':	true,
				'disabled':							false,
			};
		} else if (aPageName == 'registrationPage') {
		} else if (aPageName == 'mainPage') {
			extraProperties = {
				'messageBox':					this.messageBoxContent(),
				'accountStatus':				this.userAccountInfo(),
				'selectionPanelStatus':			this.isSelectionPanelOpen()	? 'OPEN' : 'CLOSED',
				'settingsPanelStatus':			this.isSettingsPanelOpen()	? 'OPEN' : 'CLOSED',
//				'shouldIncludeArchivedCards':	this.shouldIncludeArchivedCards(),
//				'cards':				…,
//				'tags':					…,
//				'selectedCard':			…,
			};
		} else if (aPageName == 'errorPage') {
			extraProperties = {
				'message': ''
			};
		}
		
		if (extraProperties != null) {
			result = MochiKit.Base.update(result, extraProperties);
		}
//console.log("MainController.pageProperties", result);
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
	},

	isSelectionPanelHidable: function () {
		var	result;

		if (this.mediaQueryStyle() == 'extra-wide') {
			result = false;
		} else {
			result = true;
		}

		return result;
	},

	isSelectionPanelOpen: function () {
		return this._isSelectionPanelOpen;
	},

	toggleSelectionPanel_handler: function (anEvent) {
		if (this.isSelectionPanelHidable() == true) {
			this._isSelectionPanelOpen = !this._isSelectionPanelOpen;
			this.setCloseMaskAction(MochiKit.Base.method(this, 'toggleSelectionPanel_handler'));
			this.refreshCurrentPage();
		}
	},

	isSettingsPanelOpen: function () {
		return this._isSettingsPanelOpen;
	},

	toggleSettingsPanel_handler: function (anEvent) {
		this._isSettingsPanelOpen = !this._isSettingsPanelOpen;
		this.setCloseMaskAction(MochiKit.Base.method(this, 'toggleSettingsPanel_handler'));
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

console.log("CARD INFO AT INDEX", anIndex);
		card = this.selectedCards()[anIndex];
		
		return {
			'label': card['label'],
			'reference': card['_reference']
		};
	},

	previousCardInfo: function () {
		var	currentIndex;
		var	nextIndex;

		currentIndex = this.selectedCardIndex();
		if (currentIndex == -1) {
			nextIndex = this.selectedCards().length - 1;
		} else {
			nextIndex = Math.max(currentIndex - 1, 0);
		}

		return this.cardInfoAtIndex(nextIndex);
	},
	
	nextCardInfo: function () {
		var	currentIndex;
		var	nextIndex;

		currentIndex = this.selectedCardIndex();
		if (currentIndex == -1) {
			nextIndex = 0;
		} else {
			nextIndex = Math.min(currentIndex + 1, this.selectedCards().length - 1);
		}

		return this.cardInfoAtIndex(nextIndex);
	},

	//............................................................................

	resetSelectedCard: function () {
		this._selectedCardInfo = null;
	},

	selectCard_handler: function (someInfo) {
		this._selectedCardInfo = someInfo;
		this.refreshSelectedCards();
		this.updateSelectedCard(someInfo);
	},

	refreshCardEditDetail_handler: function (aRecordReference) {
		this.updateSelectedCard({'reference':aRecordReference}, false);
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
		
		return Clipperz.Async.callbacks("MainController.saveCardEdits_handler", [
			MochiKit.Base.method(currentPage, 'setProps', {'showGlobalMask':true}),
			MochiKit.Base.method(this, 'saveChanges'),
			MochiKit.Base.method(currentPage, 'setProps', {'mode':'view', 'showGlobalMask':false}),
			MochiKit.Base.method(this, 'refreshUI', aRecordReference),
		], {trace:false});
	},

	cancelCardEdits_handler: function (aRecordReference) {
		var	currentPage = this.pages()[this.currentPage()];
		var	self = this;
		var	wasBrandNew;

		return Clipperz.Async.callbacks("MainController.cancelCardEdits_handler", [
			MochiKit.Base.method(this.user(), 'getRecord', aRecordReference),
			MochiKit.Base.methodcaller('isBrandNew'),
			function (aValue) { wasBrandNew = aValue },
			
			MochiKit.Base.method(this.user(), 'hasPendingChanges'),
//function (aValue) { console.log("2- USER.hasPendingChanges()", aValue); return aValue; },
			Clipperz.Async.deferredIf('HasPendingChanges',[
				MochiKit.Base.method(self, 'ask', {
					'question': "Lose pending changes?",
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

				this.updateSelectedCard(info, false);
			}, this)
		], {trace:false});
	},

	//----------------------------------------------------------------------------
/*
	askConfirmation: function (aMessage) {
		var	deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred('MainController.askConfirmation', {trace:false});
		deferredResult.callback();
//		deferredResult.cancel();
		
		return deferredResult;
	},
*/
	
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
		return Clipperz.Async.callbacks("MainController.addCardClick_handler", [
			MochiKit.Base.method(this.user(), 'createNewRecord'),
			MochiKit.Base.methodcaller('reference'),
			MochiKit.Base.method(this, 'refreshUI'),
//			MochiKit.Base.bind(function () {
//				this.pages()[this.currentPage()].setProps({'mode': 'edit'});
//			}, this),
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

//console.log("CLONE CARD", anEvent['reference']);
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
		], {trace:true});
	},
	
	enterEditMode: function () {
		var	currentPage = this.pages()[this.currentPage()];

		currentPage.setProps({'mode': 'edit'});
		
		return Clipperz.Async.callbacks("MainController.enterEditMode", [
			MochiKit.Base.method(this, 'allTags', true),
			MochiKit.Base.keys,
			function (aValue) {
				currentPage.setProps({'allTags': aValue});
			},
		], {trace:false});
		
	},
	
	editCard_handler: function (anEvent) {
//console.log("EDIT CARD", anEvent['reference']);
//		this.pages()[this.currentPage()].setProps({'mode': 'edit'});
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
		this.updateSelectedCard();
		this.moveOutPage(this.currentPage(), 'mainPage');
	},

	//============================================================================

	selectAllCards_handler: function () {
		this.resetSelectedCard();
		this.setFilter('ALL');
		return this.refreshSelectedCards();
	},
	
	selectRecentCards_handler: function () {
		this.resetSelectedCard();
		this.setFilter('RECENT');
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
		return this.refreshSelectedCards();
	},

	selectUntaggedCards_handler: function () {
		this.resetSelectedCard();
		this.setFilter('UNTAGGED');
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

	//============================================================================

	matchMediaQuery_handler: function (newQueryStyle) {
		this._mediaQueryStyle = newQueryStyle;

		if (this.currentPage() == 'cardDetailPage') {
			this.moveOutPage(this.currentPage(), 'mainPage');
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
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleSelectionPanel');
		MochiKit.DOM.getElement('searchValue').focus();
	},
	
	exitSearch_handler: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleSelectionPanel');
		MochiKit.DOM.getElement('searchValue').blur();
	},

	selectPreviousCard: function () {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectCard', this.previousCardInfo());
	},

	selectNextCard: function () {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectCard', this.nextCardInfo());
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
