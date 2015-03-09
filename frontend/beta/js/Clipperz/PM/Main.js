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

if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }

Clipperz.PM.VERSION = "0.1";
Clipperz.PM.NAME = "Clipperz.PM";

//#############################################################################

Clipperz.PM.Main = function() {
	this._loginPanel = null;
	this._user = null;

	this._isRunningCompact = false;
	
	Clipperz.NotificationCenter.register(null, 'userConnected', this, 'userConnectedCallback');
	Clipperz.NotificationCenter.register(null, 'switchLanguage', this, 'switchLanguageHandler');

	Clipperz.NotificationCenter.register(null, 'EXCEPTION', this, 'reportException');
	
	return this;
}

//=============================================================================

MochiKit.Base.update(Clipperz.PM.Main.prototype, {
	'toString': function() {
		return "Clipperz.PM.Main";
	},

	'switchLanguageHandler': function() {
//MochiKit.Logging.logDebug(">>> main.switchLanguageHandler");
		YAHOO.ext.Element.get('donateHeaderIconLink').dom.href = Clipperz.PM.Strings['donateHeaderLinkUrl'];
		YAHOO.ext.Element.get('donateHeaderLink').update(Clipperz.PM.Strings['donateHeaderLinkLabel']).dom.href = Clipperz.PM.Strings['donateHeaderLinkUrl'];
//		YAHOO.ext.Element.get('creditsHeaderLink').update(Clipperz.PM.Strings['creditsHeaderLinkLabel']).dom.href = Clipperz.PM.Strings['creditsHeaderLinkUrl'];
		YAHOO.ext.Element.get('feedbackHeaderLink').update(Clipperz.PM.Strings['feedbackHeaderLinkLabel']).dom.href = Clipperz.PM.Strings['feedbackHeaderLinkUrl'];
		YAHOO.ext.Element.get('helpHeaderLink').update(Clipperz.PM.Strings['helpHeaderLinkLabel']).dom.href = Clipperz.PM.Strings['helpHeaderLinkUrl'];
		YAHOO.ext.Element.get('forumHeaderLink').update(Clipperz.PM.Strings['forumHeaderLinkLabel']).dom.href = Clipperz.PM.Strings['forumHeaderLinkUrl'];

		if (YAHOO.ext.Element.get('logout') != null) {
			YAHOO.ext.Element.get('logout').update(Clipperz.PM.Strings['logoutMenuLabel']);
			YAHOO.ext.Element.get('lock').update(Clipperz.PM.Strings['lockMenuLabel']);

			YAHOO.ext.Element.get('recordsTabAnchor').update(Clipperz.PM.Strings['recordMenuLabel']);
			YAHOO.ext.Element.get('accountTabAnchor').update(Clipperz.PM.Strings['accountMenuLabel']);
			YAHOO.ext.Element.get('dataTabAnchor').update(Clipperz.PM.Strings['dataMenuLabel']);
//			YAHOO.ext.Element.get('contactsTabAnchor').update(Clipperz.PM.Strings['contactsMenuLabel']);
			YAHOO.ext.Element.get('toolsTabAnchor').update(Clipperz.PM.Strings['toolsMenuLabel']);
		}
//MochiKit.Logging.logDebug("<<< main.switchLanguageHandler");
	},
	
	//-------------------------------------------------------------------------

	'fixToDrawTheMainTabsCorrectlyOnSafari': function() {
		this.switchLanguageHandler();
	},
	
	//-------------------------------------------------------------------------

	'run': function(shouldShowRegistrationForm) {
		var	mainElement;

		Clipperz.NotificationCenter.register(null, 'updatedProgressState', this, 'updateProgressDialogStatus');

		YAHOO.ext.Element.get('recordDetailEditModeHeaderMask').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide().unmask();
		YAHOO.ext.Element.get('recordDetailEditModeVerticalMask').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide().unmask();
		
//MochiKit.Logging.logDebug(">>> Main.run");
		mainElement = YAHOO.ext.Element.get('main');
		if (Clipperz.PM.Proxy.defaultProxy.isReadOnly()) {
			YAHOO.ext.Element.get('applicationVersionType').dom.className = "readOnly";
		}
		mainElement.update("");

		Clipperz.YUI.DomHelper.append(mainElement.dom, {tag:'ul', cls:'clipperzTabPanels', children:[
			{tag:'li', id:'loginPanel'}
		]})

		this.setLoginPanel(new Clipperz.PM.Components.Panels.LoginPanel(YAHOO.ext.Element.get('loginPanel')));

//MochiKit.Logging.logDebug("--- Main.run - selecting active form to show ...");
		if (shouldShowRegistrationForm == true) {
			this.loginPanel().showRegistrationForm(false);
		} else {
			this.loginPanel().showLoginForm(false);
		}

		this.switchLanguageHandler();
//MochiKit.Logging.logDebug("--- Main.run - selecting active form to show. done.");
//MochiKit.Logging.logDebug("<<< Main.run");
	},

	//-------------------------------------------------------------------------

	'runCompact': function() {
		this.setIsRunningCompact(true);
		YAHOO.ext.Element.get(document.body).addClass("compact");
		new Clipperz.PM.Components.Compact.LoginForm(YAHOO.ext.Element.get('mainDiv'));
	},
	
	'showCompactInterface': function() {
//MochiKit.Logging.logDebug(">>> main.showCompactInterface");
		new Clipperz.PM.Components.Compact.CompactInterface(YAHOO.ext.Element.get('compactBody'), {user:this.user()});
//MochiKit.Logging.logDebug("<<< main.showCompactInterface");
	},
	
	//-------------------------------------------------------------------------

	'mainPage': function() {
		if (this._mainPage == null) {
			this._mainPage = new Clipperz.PM.Components.MainPage();
		}
		
		return this._mainPage;
	},

	//-------------------------------------------------------------------------
	
	'loginPanel': function() {
		return this._loginPanel;
	},
	
	'setLoginPanel': function(aValue) {
		this._loginPanel = aValue;
	},
	
	//-------------------------------------------------------------------------

	'showMainPanels': function() {
		var	mainElement;
		var logoutBlock;
		var lockBlock;
		var menusTRElement;
		
		this.loginPanel().remove();
		this.setLoginPanel(null);

		logoutBlock = YAHOO.ext.Element.get('logoutLI');
		Clipperz.YUI.DomHelper.append(logoutBlock.dom, {tag:'a', href:"#", id:'logout', htmlString:Clipperz.PM.Strings['logoutMenuLabel']});
		MochiKit.Signal.connect('logout', 'onclick', this, 'doLogoutEventHandler');

		lockBlock = YAHOO.ext.Element.get('lockLI');
		Clipperz.YUI.DomHelper.append(lockBlock.dom, {tag:'a', href:"#", id:'lock', htmlString:Clipperz.PM.Strings['lockMenuLabel']});
		MochiKit.Signal.connect('lock', 'onclick', this, 'doLockEventHandler');

		menusTRElement = YAHOO.ext.Element.get('menusTR');
		Clipperz.YUI.DomHelper.append(menusTRElement.dom, {tag:'td', id:'recordsTab', children:[{tag:'div', children:[{tag:'a', id:'recordsTabAnchor', htmlString:Clipperz.PM.Strings['recordMenuLabel']}]}]});
		Clipperz.YUI.DomHelper.append(menusTRElement.dom, {tag:'td', id:'accountTab', children:[{tag:'div', children:[{tag:'a', id:'accountTabAnchor', htmlString:Clipperz.PM.Strings['accountMenuLabel']}]}]});
		Clipperz.YUI.DomHelper.append(menusTRElement.dom, {tag:'td', id:'dataTab', children:[{tag:'div', children:[{tag:'a', id:'dataTabAnchor', htmlString:Clipperz.PM.Strings['dataMenuLabel']}]}]});
//		Clipperz.YUI.DomHelper.append(menusTRElement.dom, {tag:'td', id:'contactsTab', children:[{tag:'div', children:[{tag:'a', id:'contactsTabAnchor', htmlString:Clipperz.PM.Strings['contactsMenuLabel']}]}]});
		Clipperz.YUI.DomHelper.append(menusTRElement.dom, {tag:'td', id:'toolsTab', children:[{tag:'div', children:[{tag:'a', id:'toolsTabAnchor', htmlString:Clipperz.PM.Strings['toolsMenuLabel']}]}]});

		mainElement = YAHOO.ext.Element.get('main');
		mainElement.update("");
		Clipperz.YUI.DomHelper.append(mainElement.dom, {tag:'ul', cls:'clipperzTabPanels', children:[
			{tag:'li', id:'recordsPanel'},
			{tag:'li', id:'accountPanel'},
			{tag:'li', id:'dataPanel'},
//			{tag:'li', id:'contactsPanel'},
			{tag:'li', id:'toolsPanel'}
		]}, true)

		new Clipperz.PM.Components.TabPanel.TabPanelController({
									name: 'mainTabPanel',
									config:{	'recordsTab':'recordsPanel',
												'accountTab':'accountPanel',
												'dataTab':'dataPanel',
//												'contactsTab':'contactsPanel',
												'toolsTab':'toolsPanel'},
									selectedTab:'recordsTab'
		}).setUp();
		
		new Clipperz.PM.Components.Panels.MainPanel(YAHOO.ext.Element.get('recordsPanel'), {user:this.user()});
		new Clipperz.PM.Components.Panels.AccountPanel(YAHOO.ext.Element.get('accountPanel'), {user:this.user()});
		new Clipperz.PM.Components.Panels.DataPanel(YAHOO.ext.Element.get('dataPanel'), {user:this.user()});
//		new Clipperz.PM.Components.Panels.ContactsPanel(YAHOO.ext.Element.get('contactsPanel'), {user:this.user()});
		new Clipperz.PM.Components.Panels.ToolsPanel(YAHOO.ext.Element.get('toolsPanel'), {user:this.user()});

		this.fixToDrawTheMainTabsCorrectlyOnSafari();	//	fix to
//MochiKit.Logging.logDebug("<<< Main.showMainPanels");
	},
	
	//-------------------------------------------------------------------------

	'userConnectedCallback': function(anEvent) {
//MochiKit.Logging.logDebug(">>> Main.userConnectedCallback");
//MochiKit.Logging.logDebug(">>> doConnect - user: " + this.user());
		this.setUser(anEvent.source());
		
		if (this.isRunningCompact()) {
			this.showCompactInterface();
		} else {
			this.showMainPanels();
		}
//MochiKit.Logging.logDebug("<<< Main.userConnectedCallback");
	},

	//-----------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},
	
	'setUser': function(aValue) {
		this._user = aValue;
	},

	//-----------------------------------------------------------------------------

	'doLogoutEventHandler': function(anEvent) {
		var deferred;
		
		anEvent.stop();

		deferred = new MochiKit.Async.Deferred();
//deferred.addBoth(function(res) {MochiKit.Logging.logDebug("Main.doLogoutEventHandler - 1: " + res); return res;});
		deferred.addCallback(MochiKit.Base.method(this.user(), 'doLogout'));
//deferred.addBoth(function(res) {MochiKit.Logging.logDebug("Main.doLogoutEventHandler - 2: " + res); return res;});
		deferred.addCallback(Clipperz.PM.exit, 'logout.html');
//deferred.addBoth(function(res) {MochiKit.Logging.logDebug("Main.doLogoutEventHandler - 3: " + res); return res;});
		deferred.callback();
	},

	//-----------------------------------------------------------------------------
	
	'doLockEventHandler': function(anEvent) {
		var	deferredResult;
		var	lockDialogElement;
		var lockDialog;
		var unlockButton;

		anEvent.stop();

		Clipperz.NotificationCenter.notify(this, 'accountLocked', null, true);
		
		lockDialogElement = Clipperz.YUI.DomHelper.append(document.body, {tag:'div', id:'lockDialog', children:[
			{tag:'div', cls:'ydlg-hd', htmlString:Clipperz.PM.Strings['lockTitle']},
			{tag:'div', cls:'ydlg-bd', children:[
				{tag:'div', cls:'alert-message', id:'lockMessage', children:[
					{tag:'div', htmlString:Clipperz.PM.Strings['lockDescription']},
					{tag:'form', id:'lockDialogForm', children:[
						{tag:'input', type:'password', id:'lockPassphrase'}
					]}
				]}
			]},
			{tag:'div', cls:'ydlg-ft'}
		]}, true);
		new Clipperz.PM.Components.PasswordEntropyDisplay(YAHOO.ext.Element.get('lockPassphrase'));

		lockDialog = new YAHOO.ext.BasicDialog(
			lockDialogElement, { 
				closable:false,
				modal:true,
				autoTabs:false,
				resizable:false,
				fixedcenter:true,
				constraintoviewport:false,
				width:350,
				height:130,
				shadow:true
			}
		);

		unlockButton = lockDialog.addButton(Clipperz.PM.Strings['unlockButtonLabel'], MochiKit.Base.method(this, 'exitLock', lockDialog));
//MochiKit.Logging.logDebug("--- Main.showAlertDialog - 5");
		lockDialog.setDefaultButton(unlockButton);

		MochiKit.Signal.connect('lockDialogForm', 'onsubmit', MochiKit.Base.method(this, 'exitLock', lockDialog));
		lockDialog.on('show', function() {YAHOO.ext.Element.get('lockPassphrase').focus();});
		lockDialog.show('main');
//		this.user().lock();
	},
	
	'exitLock': function(aLockDialog, anEvent) {
//		var deferredResult;
		
//MochiKit.Logging.logDebug(">>> Exiting lock");
		if (typeof(anEvent.stop) != 'undefined') {
			anEvent.stop();
		}
		
		if (this.user().passphrase() == YAHOO.ext.Element.get('lockPassphrase').dom.value) {
			aLockDialog.hide(MochiKit.Base.method(aLockDialog, 'destroy', true));
			Clipperz.NotificationCenter.notify(this, 'accountUnlocked', null, true);
		} else {
			YAHOO.ext.Element.get('lockPassphrase').dom.value = "";
			YAHOO.ext.Element.get('lockPassphrase').focus();
		}

//		deferredResult = new MochiKit.Async.Deferred();
//		deferredResult.addCallback(MochiKit.Base.method(this.user(), 'unlockWithPassphrase'));
//		deferredResult.addCallback(MochiKit.Base.method(aLockDialog, 'hide', MochiKit.Base.method(aLockDialog, 'destroy', true)));
//		deferredResult.addCallback(MochiKit.Base.method(Clipperz.NotificationCenter, 'notify', this, 'accountUnlocked', null, true));
//		deferredResult.addErrback(function() {
//			YAHOO.ext.Element.get('lockPassphrase').dom.value = "";
//			YAHOO.ext.Element.get('lockPassphrase').focus();
//		});
//		deferredResult.callback(YAHOO.ext.Element.get('lockPassphrase').dom.value);

		return false;
	},
	
	//-----------------------------------------------------------------------------

	'updateProgressDialogStatus': function(anEvent) {
//MochiKit.Logging.logDebug(">>> main.updateProgressDialogStatus - " + anEvent.parameters());
//try {
		if (Clipperz.Base.objectType(anEvent.parameters()) == 'string') {
			Clipperz.PM.Components.MessageBox().update(Clipperz.PM.Strings.messagePanelConfigurations[anEvent.parameters()]());
		} else {
			Clipperz.PM.Components.MessageBox().update(anEvent.parameters());
		}
//} catch (exception) {
//console.log("updateProgressDialogStatus - anEvent", anEvent);
//	MochiKit.Logging.logError("Main.updateProgressDialogStatus: " + exception);
//	throw exception;
//}
//MochiKit.Logging.logDebug("<<< main.updateProgressDialogStatus");
	},

	//-----------------------------------------------------------------------------

	'defaultErrorHandler': function(anErrorString, anException) {
MochiKit.Logging.logDebug(">>> DEFAULT ERROR HANDLER: " + anErrorString + " (exception: " + Clipperz.Base.serializeJSON(anException) + ")");
	},

	//-----------------------------------------------------------------------------

	'isRunningCompact': function() {
		return this._isRunningCompact;
	},
	
	'setIsRunningCompact': function(aValue) {
		this._isRunningCompact = aValue;
	},
	
	//-----------------------------------------------------------------------------

	'reportException': function(anError) {
/*
		var deferredResult;
		
		deferredResult = new MochiKit.Async.Deferred();
		
		Clipperz.PM.Components.MessageBox().show(
			{
				title:Clipperz.PM.Strings['fatalErrorMessageTitle'],
				text:Clipperz.PM.Strings['fatalErrorMessageText'],
				width:240,
				showProgressBar:false,
				showCloseButton:false,
				fn:MochiKit.Base.method(deferredResult, 'callback'),
				scope:this,
				buttons:{
					'ok':	Clipperz.PM.Strings['fatalErrorMessageCloseButtonLabel']
				}
			}
		);

		deferredResult.addCallback(function() {
			window.document.body.innerHTML = "";
			window.location.reload(true);
		});
*/
		Clipperz.PM.exit('error.html');
	},
	
	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});



//#############################################################################

MochiKit.Base.update(Clipperz.PM, {

	__repr__: function() {
		return "[" + this.NAME + " " + this.VERSION + "]";
	},
	
	toString: function() {
		return this.__repr__();
	},

	//-----------------------------------------------------------------------------
	
	'initPage': function() {
		var	main;
		var shouldShowRegistrationForm;
		var useCompactDesign;
		
//MochiKit.Logging.logWarning("Just testing logging system");
		Clipperz.PM.Strings.Languages.initSetup();
//		DWRUtil.useLoadingMessage(Clipperz.PM.Strings['DWRUtilLoadingMessage']);

		if (window.location.search.indexOf("registration") != -1) {
			shouldShowRegistrationForm = true;
		} else {
			shouldShowRegistrationForm = false;
		}

		if (window.location.search.indexOf("compact") != -1) {
			useCompactDesign = true;
		} else {
			useCompactDesign = false;
		}
		
		main = new Clipperz.PM.Main();

		if (useCompactDesign == true) {
			main.runCompact();
		} else {
			if (Clipperz_IEisBroken === true) {
				if (Clipperz.PM.Proxy.defaultProxy.isReadOnly()) {
					var logoParentNode;
					
					YAHOO.ext.Element.get('donateHeaderIcon').remove();
					logoParentNode = YAHOO.ext.Element.get('logo').dom.parentNode;
					YAHOO.ext.Element.get('logo').remove();
					Clipperz.YUI.DomHelper.append(logoParentNode, {tag:'span', children:[
						{tag:'span', cls:'clipperzLogoSpan', html:'clipper'},
						{tag:'span', cls:'clipperzLogoZSpan', html:'z'}
					]})
				} else {
					YAHOO.ext.Element.get('donateHeaderIcon').dom.src = "./images/smiles.gif";
					YAHOO.ext.Element.get('logo').dom.src = "./images/logo.gif";
				}
			} else {
				YAHOO.ext.Element.get('donateHeaderIcon').dom.src = "data:image/gif;charset=utf-8;base64,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";
				YAHOO.ext.Element.get('logo').dom.src = "data:image/gif;charset=utf-8;base64,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";
			}
			
			main.run(shouldShowRegistrationForm);
		}
		
		Clipperz.PM.defaultErrorHandler = main.defaultErrorHandler;
		
		//	DEBUG
		if ((typeof(_clipperz_pm_test_user) != 'undefined') && (typeof(_clipperz_pm_test_passphrase) != 'undefined')) {
//------- automatic login with test/test --------------			
//			Clipperz.PM.Proxy.defaultProxy = new Clipperz.PM.Proxy.Offline();
//MochiKit.Logging.logWarning("activating AUTOLOGIN (" + _clipperz_pm_test_user + "/" + _clipperz_pm_test_passphrase + ")");
//			MochiKit.Async.callLater(0.9, MochiKit.Base.bind(main.doLogin, main), _clipperz_pm_test_user, _clipperz_pm_test_passphrase, YAHOO.ext.Element.get('Clipperz_PM_Components_Panels_login_submit_9'));
			MochiKit.Async.callLater(0.5, MochiKit.Base.bind(main.loginPanel().doLoginWithUsernameAndPassphrase, main.loginPanel()), _clipperz_pm_test_user, _clipperz_pm_test_passphrase);
//------- automatic registration --------------			
//MochiKit.Logging.logWarning("Testing registration (user,passwd)");
//MochiKit.Logging.logDebug("mainPanel: " + main.mainPage().mainPanel().content());
//			main.mainPage().getActivePanel().showRegistrationFormAnimator().play();
//			MochiKit.Async.callLater(1.9, MochiKit.Base.bind(main.doRegistration, main), 'user', 'passwd');
//-------------------------------------
//			main.showMainPanels('ok');
		};

		if 	(/fastEntropyAccumulationForTestingPurpose/.test(window.location.search)) {
			Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();
		}

//		Clipperz.PM.Proxy.defaultProxy.knock();
	},
	
	//-------------------------------------------------------------------------

	'showDonationSplashScreen': function(aUser, aFocusElementId) {
		var deferredResult;
		var donateElement;
		var	donateDialog;
		var closeButton;
		var closeFunction;
		var donateButton;
		var donateFunction;

		deferredResult = new MochiKit.Async.Deferred();

//MochiKit.Logging.logDebug(">>> Main.showRegistrationSplashScreen");
		donateElement = Clipperz.YUI.DomHelper.append(document.body, {tag:'div', id:'donateSplash', children:[
			{tag:'div', cls:'ydlg-hd', htmlString:Clipperz.PM.Strings['donateSplashPanelTitle']},
			{tag:'div', cls:'ydlg-bd', children:[
				{tag:'div', cls:'alert-message', id:'donateMessage', children:[
					{tag:'div', cls:'donateSplashPanelIcon', children:[
						{tag:'img', src:Clipperz.PM.Strings['donateSplashPanelIconUrl']}
					]},
					{tag:'div', cls:'donateSplashPanelDescription', htmlString:Clipperz.PM.Strings['donateSplashPanelDescription']}
				]}
			]},
			{tag:'div', cls:'ydlg-ft'}
		]}, true);

		donateDialog = new YAHOO.ext.BasicDialog(
			donateElement, { 
				closable:false,
				modal:true,
				autoTabs:false,
				resizable:false,
				fixedcenter:true,
				constraintoviewport:false,
				width:450,
				height:220,
				shadow:true,
				minWidth:300,
				minHeight:300
			}
		);
	
		closeFunction = MochiKit.Base.partial(deferredResult.callback, false);
		donateFunction = MochiKit.Base.partial(deferredResult.callback, true);
		donateButton = donateDialog.addButton(Clipperz.PM.Strings['donateDonateButtonLabel'], donateFunction, deferredResult);

		donateDialog.addKeyListener(27, closeFunction, deferredResult);
		closeButton = donateDialog.addButton(Clipperz.PM.Strings['donateCloseButtonLabel'], closeFunction, deferredResult);

		donateDialog.setDefaultButton(donateButton);
		donateDialog.show(aFocusElementId /*'recordListAddRecordButton'*/);

		deferredResult.addCallback(MochiKit.Base.bind(function(shouldOpenDonatePage) {
			var result;
			
			if (shouldOpenDonatePage) {
				window.open(Clipperz.PM.Strings['donateHeaderLinkUrl'], "donate");
				aUser.preferences().setShouldShowDonationPanel(false);
				aUser.preferences().saveChanges();
			}
		}, this));
		deferredResult.addBoth(MochiKit.Base.method(donateDialog, 'hide'));
		deferredResult.addBoth(MochiKit.Base.method(donateElement, 'remove'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Main.showDonationSplashScreen - 2: " + res); return res;});
//MochiKit.Logging.logDebug("<<< Main.showRegistrationSplashScreen");
		
		return deferredResult;
	},
	
	//-----------------------------------------------------------------------------

	'exit': function(aPageName) {
//		alert("ERROR: " + aPageName);

//		YAHOO.ext.Element.get('menus').update("");
//		YAHOO.ext.Element.get('logoutLI').update("");
//		YAHOO.ext.Element.get('lockLI').update("");
//		YAHOO.ext.Element.get('main').update("");
//		Clipperz.YUI.DomHelper.append('main', {tag:'div', id:'exitBlock', children:Clipperz.PM.Strings['exitConfig']});
		
		MochiKit.Async.wait(0).addCallback(function() {
//			window.location.href = "http://www.google.com/search?hl=" + Clipperz.PM.Strings.preferredLanguage + "&q=phishing&btnI=Google+Search";
			window.location.href = "./" + aPageName;
		});
	},

	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});

//#############################################################################

//Clipperz.PM.SerializeAsyncCalls = function(aDelay, aFunction) {
//	aFunction.apply(extend(null, arguments, 1));
//};

MochiKit.DOM.addLoadEvent(Clipperz.PM.initPage);
