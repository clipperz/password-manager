/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz's Javascript Crypto Library.
Javascript Crypto Library provides web developers with an extensive
and efficient set of cryptographic functions. The library aims to
obtain maximum execution speed while preserving modularity and
reusability.
For further information about its features and functionalities please
refer to http://www.clipperz.com

* Javascript Crypto Library is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Javascript Crypto Library is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Javascript Crypto Library.  If not, see
  <http://www.gnu.org/licenses/>.

*/

Clipperz.Base.module('Clipperz.PM.UI.Web.Controllers');

Clipperz.PM.UI.Web.Controllers.AppController = function(args) {

	this._user = null;
	this._tabSlotNames = {
		//tabName:		 slotName
		'cards':		'cardGrid',
//		'directLogins':	'directLoginGrid',
		'account':		'accountPanel',
		'data':			'dataPanel',
		'tools':		'toolsPanel'
	};
	
	//controllers
	this._cardsController	= null;
//	this._directLoginsController = null;
	this._filterController	= null;		//	new Clipperz.PM.UI.Web.Controllers.FilterController();
	
	//components
	this._appPage = null;
	this._userInfoBox = null;
	this._tabSidePanel = null;

//	MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'editCard',		this, 'handleEditCard');
//	MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'deleteCard',	this, 'handleDeleteCard');

	MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'userDataSuccessfullySaved',	this, 'userDataSuccessfullySavedHandler');

	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.Web.Controllers.AppController.prototype, {

	'toString': function() {
		return "Clipperz.PM.UI.Web.Controllers.AppController";
	},

	//-----------------------------------------------------------------------------
	
	'setUser': function(anUser) {
		this._user = anUser;
	},
	
	'user': function() {
		return this._user;
	},

	//-----------------------------------------------------------------------------
/*	
	'tabSlotNames': function() {
		return this._tabSlotNames;
	},
*/
	'slotNameForTab': function(aTabName) {
		return this._tabSlotNames[aTabName];
	},
	
	'hideAllAppPageTabSlots': function() {
		var aTabName;
		
		for (aTabName in this._tabSlotNames) {
			this.appPage().hideSlot(this.slotNameForTab(aTabName));
		}
	},
	
	//-----------------------------------------------------------------------------

	'appPage': function() {
		if (this._appPage == null) {
			this._appPage = new Clipperz.PM.UI.Web.Components.AppPage();
		}
		
		return this._appPage;
	},

	//-----------------------------------------------------------------------------

	'tabSidePanel': function() {
		if (this._tabSidePanel == null) {
			this._tabSidePanel = new Clipperz.PM.UI.Web.Components.TabSidePanel();
		}
		
		return this._tabSidePanel;
	},

	//-----------------------------------------------------------------------------

	'userInfoBox': function() {
		if (this._userInfoBox == null) {
			this._userInfoBox = new Clipperz.PM.UI.Web.Components.UserInfoBox();
			
			MochiKit.Signal.connect(this._userInfoBox, 'logout',	this, 'handleLogout');
			MochiKit.Signal.connect(this._userInfoBox, 'lock',		this, 'handleLock');
			MochiKit.Signal.connect(this._userInfoBox, 'unlock',	this, 'handleUnlock');
		}
		
		return this._userInfoBox;
	},

	//-----------------------------------------------------------------------------

	'accountPanel': function () {	
		if (this._accountPanel == null) {
			this._accountPanel = new Clipperz.PM.UI.Web.Components.AccountPanel(/*{selected:'Preferences'}*/);
		}
		
		return this._accountPanel;
	},

	//.........................................................................

	'dataPanel': function () {	
		if (this._dataPanel == null) {
			this._dataPanel = new Clipperz.PM.UI.Web.Components.DataPanel();
		}
		
		return this._dataPanel;
	},
	
	//.........................................................................

	'toolsPanel': function () {	
		if (this._toolsPanel == null) {
			this._toolsPanel = new Clipperz.PM.UI.Web.Components.ToolsPanel();
		}
		
		return this._toolsPanel;
	},

	//-----------------------------------------------------------------------------

	'filterController': function () {
		if (this._filterController == null) {
			this._filterController = new Clipperz.PM.UI.Web.Controllers.FilterController();
		}
		
		return this._filterController;
	},
	
	'cardsController': function() {
		if (this._cardsController == null) {
			this._cardsController = new Clipperz.PM.UI.Web.Controllers.CardsController({'filterController':this._filterController});
		}
		
		return this._cardsController;
	},
	
	//-----------------------------------------------------------------------------
/*
	'directLoginsController': function() {
//Clipperz.log(">>> AppController.directLoginsController");
		if (this._directLoginsController == null) {
			this._directLoginsController = new Clipperz.PM.UI.Web.Controllers.DirectLoginsController({'filterController':this._filterController});
		}
//Clipperz.log("<<< AppController.directLoginsController");
		
		return this._directLoginsController;
	},
*/
	//-----------------------------------------------------------------------------

	'populateUserInfo': function() {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("AppController.populateUserInfo", {trace:false});
		deferredResult.collectResults({
			'username':	MochiKit.Base.methodcaller('displayName'),
			'cardsNumber':	[
				MochiKit.Base.methodcaller('getRecords'),
				function (someResults) { return someResults.length; }
			],
			'directLoginsNumber': [
				MochiKit.Base.methodcaller('getDirectLogins'),
				function (someResults) { return someResults.length; }
			]
		})
		deferredResult.addMethod(this.userInfoBox(), 'updateUserDetails');
		deferredResult.callback(this.user());

		return deferredResult;
	},

	//-----------------------------------------------------------------------------

	'run': function(args) {
		var deferredResult;
		var	slot;
		var	page;
		var user;
		
		slot = args.slot;
		user = args.user;

		this.setUser(user);
		
		slot.setContent(this.appPage());

		this.appPage().slotNamed('userInfoBox').setContent(this.userInfoBox());
		this.appPage().slotNamed('tabSidePanel').setContent(this.tabSidePanel());

		this.appPage().slotNamed('accountPanel').setContent(this.accountPanel());
		this.appPage().slotNamed('dataPanel').setContent(this.dataPanel());
		this.appPage().slotNamed('toolsPanel').setContent(this.toolsPanel());

		this.hideAllAppPageTabSlots();
		this.appPage().showSlot(this.slotNameForTab('cards'));
		
		MochiKit.Signal.connect(this.tabSidePanel(),				'tabSelected',	this, 'handleTabSelected');
		MochiKit.Signal.connect(this.tabSidePanel(),				'addCard',		this, 'handleAddCard');
		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'addCard',		this, 'handleAddCard');

		deferredResult = new Clipperz.Async.Deferred("AppController.run", {trace:false});

		deferredResult.addMethod(this.cardsController(),        'run', {slot:this.appPage().slotNamed('cardGrid'), user:user});
//		deferredResult.addMethod(this.directLoginsController(), 'run', {slot:this.appPage().slotNamed('directLoginGrid'), user:user});
		deferredResult.addMethod(this,						   	'populateUserInfo');
		
		deferredResult.addCallback(MochiKit.Visual.ScrollTo, 'miscLinks', {duration:0});
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'CARDS_CONTROLLER_DID_RUN');
		deferredResult.addMethod(this.tabSidePanel(), 'selectTab', 'cards');
		deferredResult.callback();
	},

	//-----------------------------------------------------------------------------

	'handleTabSelected': function (selectedTabName) {
		var aTabName;
		var aSlotName;
		
//Clipperz.log(">>> AppController.handleTabSelected", selectedTabName); 
		this.hideAllAppPageTabSlots();
		this.appPage().showSlot(this.slotNameForTab(selectedTabName));

		switch (selectedTabName) {
			case 'cards':
				this.cardsController().focus();
				break;
//			case 'directLogins':
//				this.directLoginsController().focus();
//				break;
			case 'data':
				break;
			case 'account':
				break;
			case 'tools':
				break;
		}
//Clipperz.log("<-- AppController.handleTabSelected", aTabName);
	},
	
	//=============================================================================

	'handleAddCard': function (aSourceElement) {
//Clipperz.log("=== AppController.addCard", aSourceElement);
		this.cardsController().addCard(aSourceElement);
	},

	//=============================================================================

	'userDataSuccessfullySavedHandler': function (anEvent) {
		this.populateUserInfo();
	},

	//=============================================================================

	'handleLogout': function(anEvent) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("AppController.handleLogout", {trace:false});
		deferredResult.addMethod(this.user(), 'logout');
		deferredResult.addCallback(MochiKit.Signal.signal, this, 'logout');
		deferredResult.callback();
		
		return deferredResult;
	},

	//-----------------------------------------------------------------------------

	'handleLock': function (anEvent) {
		return Clipperz.Async.callbacks("AppController.handleLock", [
			MochiKit.Base.method(this.cardsController(), 'deleteAllCleanTextData'),
			MochiKit.Base.method(this.user(), 'lock')
		], {trace:false});
	},

	//.............................................................................

	'handleUnlock': function (anEvent) {
		return Clipperz.Async.callbacks("AppController.handleUnock", [
			MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'initProgress'),
			MochiKit.Base.method(this.user(), 'login'),
			MochiKit.Base.method(this.cardsController(), 'focus'),
			MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'progressDone'),
			MochiKit.Base.method(this.userInfoBox(), 'unlock')
		], {trace:false});
	},

	//=============================================================================
	__syntaxFix__: "syntax fix"
});
