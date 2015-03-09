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

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.Components) == 'undefined') { Clipperz.PM.Components = {}; }
if (typeof(Clipperz.PM.Components.Compact) == 'undefined') { Clipperz.PM.Components.Compact = {}; }

Clipperz.PM.Components.Compact.CompactInterface = function(anElement, args) {

    Clipperz.PM.Components.Compact.CompactInterface.superclass.constructor.call(this, anElement, args);

	this._directLoginItemTemplate = null;
	this._user = args.user;
	this._autoLockTimer = null;
	
	Clipperz.NotificationCenter.register(null, 'updatedProgressState', this, 'userNotificationHandler')
	Clipperz.NotificationCenter.register(null, 'directLoginAdded', this, 'directLoginAddedHandler');
	
	this.render();
	
	return this;
};

YAHOO.extendX(Clipperz.PM.Components.Compact.CompactInterface, Clipperz.PM.Components.BaseComponent, {
	
	'toString': function() {
		return "Clipperz.PM.Components.Compact.CompactInterface";
	},

	//-----------------------------------------------------
	
	'render': function() {
		var result;
		var	layout;
		var registerButton;
		
//MochiKit.Logging.logDebug(">>> CompactInterface.render");
		this.element().update("");
		
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', children:[
			{tag:'div', id:this.getId('cantentPanel'), children:[
				{tag:'h4', id:this.getId('message')},
				{tag:'ul', id:'directLogins', children:[]}
			]},
			{tag:'div', id:this.getId('lockPanel'), cls:'lockPanel', children:[
				{tag:'div', htmlString:Clipperz.PM.Strings['lockDescription']},
				{tag:'form', id:'lockDialogForm', children:[
					{tag:'input', type:'password', id:this.getId('lockPassphrase')}
				]},
				{tag:'div', id:this.getId('unlock')}
			]}
		]});

		this.getElement('lockPanel').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();

		YAHOO.ext.Element.get('lockBlock').show();
		MochiKit.Signal.connect(YAHOO.ext.Element.get('lock').dom, 'onclick', this, 'doLockEventHandler');
		new YAHOO.ext.Button(this.getId('unlock'), {text:Clipperz.PM.Strings['unlockButtonLabel'], handler:this.doUnlockEventHandler, scope:this, minWidth:0});
		this.getElement('unlock').swallowEvent('click', true);
		new Clipperz.PM.Components.PasswordEntropyDisplay(this.getElement('lockPassphrase'));
		MochiKit.Signal.connect('lockDialogForm', 'onsubmit', this, 'doUnlockEventHandler');

		this.getElement('cantentPanel').setVisibilityMode(YAHOO.ext.Element.DISPLAY);
//MochiKit.Logging.logDebug("<<< CompactInterface.render");

		return result;
	},

	//-----------------------------------------------------

	'directLoginAddedHandler': function(anEvent) {
		this.redrawDirectLoginItems();
	},

	//-----------------------------------------------------

	'compareDirectLogins': function(a, b) {
		return MochiKit.Base.compare(a.label().toLowerCase(), b.label().toLowerCase());
	},

	//-----------------------------------------------------

	'redrawDirectLoginItems': function() {
		var template;
		var allDirectLogins;

		this.getElement('message').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
//MochiKit.Logging.logDebug(">>> CompactInterface.redrawDirectLoginItems");
//MochiKit.Logging.logDebug("--- CompactInterface.redrawDirectLoginItems - 0");
		MochiKit.Iter.forEach(YAHOO.ext.Element.get('directLogins').getChildrenByTagName('li'), function(aDirectLoginElement) {
			MochiKit.Signal.disconnectAll(aDirectLoginElement.dom);
//MochiKit.Logging.logDebug("disconnecting IMG " + aDirectLoginElement.getChildrenByTagName('img')[0].dom.src);
			MochiKit.Signal.disconnectAll(aDirectLoginElement.getChildrenByTagName('img')[0].dom);
		})
//MochiKit.Logging.logDebug("--- CompactInterface.redrawDirectLoginItems - 1");
		YAHOO.ext.Element.get('directLogins').update("");
//MochiKit.Logging.logDebug("--- CompactInterface.redrawDirectLoginItems - 2");
		allDirectLogins = MochiKit.Base.values(this.user().directLoginReferences());
//MochiKit.Logging.logDebug("--- CompactInterface.redrawDirectLoginItems - 3");
		allDirectLogins.sort(this.compareDirectLogins);

//MochiKit.Logging.logDebug("--- CompactInterface.redrawDirectLoginItems - 4");
		template = this.directLoginItemTemplate();
//MochiKit.Logging.logDebug("--- CompactInterface.redrawDirectLoginItems - 5");
		MochiKit.Iter.forEach(allDirectLogins, MochiKit.Base.bind(function(aDirectLogin) {
			var	directLoginElement;
			var	faviconImageElementID;
			
			faviconImageElementID = aDirectLogin.reference() + "_faviconIMG";
			directLoginElement = template.append('directLogins', {
				elementID:faviconImageElementID,
				faviconUrl:aDirectLogin.fixedFavicon(), 
				directLoginTitle:aDirectLogin.label(),
				directLoginReference:aDirectLogin.reference()
			}, true);
//MochiKit.Logging.logDebug("--- CompactInterface.redrawDirectLoginItems - 6: " + recordElement.dom);
			directLoginElement.addClassOnOver("hover");
			MochiKit.Signal.connect(directLoginElement.dom, 'onclick', this, 'handleDirectLoginClick');

			MochiKit.Signal.connect(faviconImageElementID, 'onload', this, 'handleLoadedFaviconImage');
			MochiKit.Signal.connect(faviconImageElementID, 'onerror', aDirectLogin, 'handleMissingFaviconImage');
			MochiKit.Signal.connect(faviconImageElementID, 'onabort', aDirectLogin, 'handleMissingFaviconImage');
			
//			YAHOO.ext.Element.get(faviconImageElementID).dom.src = aDirectLogin.fixedFavicon();
		}, this));
		
		this.resetAutoLockTimer();
//MochiKit.Logging.logDebug("<<< CompactInterface.redrawDirectLoginItems");
	},

	//-----------------------------------------------------

	'directLoginItemTemplate': function() {
		if (this._directLoginItemTemplate == null) {
			this._directLoginItemTemplate = Clipperz.YUI.DomHelper.createTemplate({tag:'li', id:'{directLoginReference}', children:[
				{tag:'table', border:'0', cellpadding:'0', cellspacing:'0', children:[
					{tag:'tbody', children:[
						{tag:'tr', children:[
							{tag:'td', width:'20', align:'center', valign:'top', children:[
								{tag:'img', id:'{elementID}', src:'{faviconUrl}'}
							]},
							{tag:'td', valign:'top', children:[
								{tag:'a', cls:'directLoginItemTitle', html:'{directLoginTitle}'}
							]}
						]}
					]}
				]}
			]});
			this._directLoginItemTemplate.compile();
		}
		
		return this._directLoginItemTemplate;
	},

	//-------------------------------------------------------------------------

	'handleDirectLoginClick': function(anEvent) {
		var	directLoginReference;
//MochiKit.Logging.logDebug(">>> MainPanel.handleDirectLoginClick !!!");

		directLoginReference = this.user().directLoginReferences()[anEvent.src().id];
		this.openDirectLogin(directLoginReference);
		this.resetAutoLockTimer();
//MochiKit.Logging.logDebug("<<< MainPanel.handleDirectLoginClick");		
	},

	//-----------------------------------------------------

	'openDirectLogin': function(aDirectLoginReference) {
		var	deferredResult;
		var	newWindow;
				
//MochiKit.Logging.logDebug(">>> MainPanel.openDirectLogin - " + aDirectLoginReference.label());
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("MainPanel.openDirectLogin - 1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(aDirectLoginReference, 'setupJumpPageWindow'));
		deferredResult.addCallback(MochiKit.Base.method(aDirectLoginReference, 'deferredDirectLogin'));
		deferredResult.addCallback(function(aDirectLogin) {
			aDirectLogin.runDirectLogin(newWindow);
		});

		newWindow = window.open(Clipperz.PM.Strings['directLoginJumpPageUrl'], "");
//		MochiKit.Signal.connect(newWindow, 'onload', MochiKit.Base.method(deferredResult, 'callback', newWindow))
//		MochiKit.Signal.connect(newWindow, 'onload', MochiKit.Base.partial(alert, "done"));
		deferredResult.callback(newWindow);
//MochiKit.Logging.logDebug("<<< MainPanel.openDirectLogin");
	},

	//-------------------------------------------------------------------------

	'handleLoadedFaviconImage': function(anEvent) {
//MochiKit.Logging.logDebug(">>> MainPanel.handleLoadedFaviconImage");
		MochiKit.Signal.disconnectAll(anEvent.src())
//MochiKit.Logging.logDebug("<<< MainPanel.handleLoadedFaviconImage");
	},

	//-------------------------------------------------------------------------

	'doLockEventHandler': function(anEvent) {
		anEvent.stop();
		this.lock();
	},

	'doUnlockEventHandler': function(anEvent) {
		if (typeof(anEvent.stop) != 'undefined') {
			anEvent.stop();
		}
		this.unlock();
	},
	
	//-------------------------------------------------------------------------

	'autolock': function() {
		var	shouldAutoLock;
		
		shouldAutoLock = YAHOO.ext.Element.get('autolock').dom.checked;
		
		if (shouldAutoLock) {
			this.lock();
		} else {
			this.resetAutoLockTimer();
		}
	},
	
	'lock': function() {
//MochiKit.Logging.logDebug(">>> lock");
		this.getDom('lockPassphrase').value = "";
		this.getElement('lockPanel').show();
		this.getElement('cantentPanel').hide();
		YAHOO.ext.Element.get('lockBlock').hide();
		//this.getElement('lockPassphrase').focus();
//MochiKit.Logging.logDebug("<<< lock");
	},
	
	'unlock': function(anEvent) {
//MochiKit.Logging.logDebug(">>> unlock");
		if (this.getDom('lockPassphrase').value == this.user().passphrase()) {
			this.getElement('lockPanel').hide();
			this.getElement('cantentPanel').show();
			YAHOO.ext.Element.get('lockBlock').show();
			this.resetAutoLockTimer();
		} else {
			this.getDom('lockPassphrase').value = "";
			this.getElement('lockPassphrase').focus();
		}
//MochiKit.Logging.logDebug("<<< unlock");
	},
	
	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},
	
	//-----------------------------------------------------

	'autoLockTimer': function() {
		if (this._autoLockTimer == null) {
//MochiKit.Logging.logDebug("--- timer started - 1");
			this._autoLockTimer = MochiKit.Async.callLater(60, MochiKit.Base.method(this, 'autolock'));
//MochiKit.Logging.logDebug("--- timer started - 2");
		}
		
		return this._autoLockTimer;
	},

	'resetAutoLockTimer': function() {
//MochiKit.Logging.logDebug(">>> timer resetted");
		this.autoLockTimer().cancel();
		this._autoLockTimer = null;
//MochiKit.Logging.logDebug("--- timer resetted - 1");
		this.autoLockTimer();
//MochiKit.Logging.logDebug("<<< timer resetted");
	},

	//-----------------------------------------------------
	
	'userNotificationHandler': function(anEvent) {
		this.getElement('message').update(anEvent.parameters().text);
	},
	
	//-----------------------------------------------------
	__syntaxFix__: '__syntaxFix__'
});


