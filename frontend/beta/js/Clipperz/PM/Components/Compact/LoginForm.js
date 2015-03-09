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

Clipperz.PM.Components.Compact.LoginForm = function(anElement, args) {

    Clipperz.PM.Components.Compact.LoginForm.superclass.constructor.call(this, anElement, args);

	this.render();
	Clipperz.NotificationCenter.register(null, 'updatedProgressState', this, 'userNotificationHandler')
	
	return this;
};

YAHOO.extendX(Clipperz.PM.Components.Compact.LoginForm, Clipperz.PM.Components.BaseComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Compact.LoginForm";
	},

	//-----------------------------------------------------
	
	'render': function() {
		var result;
		var	layout;
		
		MochiKit.Signal.disconnectAllTo(this);
		this.element().update("");
		
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', id:this.getId('baseDiv'), cls:'LoginPanel', children:[
			{tag:'div', id:'compactHeader'},
			{tag:'div', id:'compactBody', children:[
				{tag:'form', id:this.getId('loginForm_form'), children:[
					{tag:'dl', children:[
						{tag:'dt', htmlString:Clipperz.PM.Strings['loginFormUsernameLabel']},
						{tag:'dd', children:[
							{tag:'input', id:this.getId('login_username'), type:'text', size:'30', name:'username'}
						]},
						{tag:'dt', htmlString:Clipperz.PM.Strings['loginFormPassphraseLabel']},
						{tag:'dd', children:[
							{tag:'input', id:this.getId('login_passphrase'), type:'password', size:'30', name:'passphrase'}
						]}
					]},
					{tag:'div', id:this.getId('login_submit')}
				]},
				{tag:'h4', id:this.getId('message')}
			]}
		]});

		new Clipperz.PM.Components.Compact.CompactHeader(YAHOO.ext.Element.get('compactHeader'));

		MochiKit.Signal.connect(this.getId('loginForm_form'), 'onsubmit', this, 'stopFormSubmit');
		new YAHOO.ext.Button(this.getId('login_submit'), {text:Clipperz.PM.Strings['loginFormButtonLabel'], handler:this.doLogin, scope:this, minWidth:0});
		this.getElement('login_submit').swallowEvent('click', true);
		
		MochiKit.Signal.connect(this.getId('loginForm_form'), 'onkeydown', this, 'onkeydown');

		new Clipperz.PM.Components.PasswordEntropyDisplay(this.getElement('login_passphrase'));
		this.getElement('login_username').focus();

		return result;
	},

	//-----------------------------------------------------
	
	'doLogin': function(e) {
//MochiKit.Logging.logDebug(">>> compact.LoginForm.doLogin");
		if (this.checkLoginForm()) {
			this.doLoginWithUsernameAndPassphrase(this.getDom('login_username').value, this.getDom('login_passphrase').value);
		}
//MochiKit.Logging.logDebug("<<< compact.LoginForm.doLogin");
	},

	//.........................................................................

	'doLoginWithUsernameAndPassphrase': function(anUsername, aPassphrase) {
		var deferredResult;
		var user;

//MochiKit.Logging.logDebug(">>> compact.LoginForm.doLoginWithUsernameAndPassphrase");
		user = new Clipperz.PM.DataModel.User({username:anUsername, passphrase:aPassphrase});

		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(MochiKit.Base.method(user, 'connect'));
		deferredResult.addCallback(MochiKit.Base.method(user, 'loadPreferences'));
		deferredResult.addCallback(MochiKit.Base.method(user, 'loadRecords'));
		deferredResult.addCallback(MochiKit.Base.method(user, 'loadDirectLogins'));
		deferredResult.addErrback(MochiKit.Base.bind(function() {
			this.getElement('loginForm_form').setVisibilityMode(YAHOO.ext.Element.DISPLAY).show();
			this.getElement('message').update(Clipperz.PM.Strings['loginMessagePanelFailureText']);
			this.getDom('login_passphrase').value = "";
			this.getElement('login_passphrase').focus();
		}, this))
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("compact.LoginForm.doLogin - 6: " + res); return res;});
		deferredResult.callback("token");
//MochiKit.Logging.logDebug("<<< compact.LoginForm.doLoginWithUsernameAndPassphrase");
		
		return deferredResult;
	},
	
	//.........................................................................

	'checkLoginForm': function() {
		var result;
		var username
		var	passphrase;

//MochiKit.Logging.logDebug(">>> checkLoginForm");
		username = this.getDom('login_username').value;
		passphrase = this.getDom('login_passphrase').value;
		
		if ((username != "") && (passphrase != "")) {
			result = true;
		} else {
			if (username == "") {
				this.getElement('login_username').focus();
			} else if (passphrase == "") {
				this.getElement('login_passphrase').focus();
			}
			
			result = false;
		}
//MochiKit.Logging.logDebug("<<< checkLoginForm - " + result);
		
		return result;
	},

	//-------------------------------------------------------------------------
	
	'stopFormSubmit': function(anEvent) {
		anEvent.stop();
	},

	//-------------------------------------------------------------------------
	
	'onkeydown': function(anEvent) {
//MochiKit.Logging.logDebug(">>> onkeydown - " + anEvent.src().id);
		if (anEvent.key().code == 13) {
			this.doLogin();
			anEvent.stop();
		}
	},

	//-----------------------------------------------------

	'userNotificationHandler': function(anEvent) {
//MochiKit.Logging.logDebug(">>> compact.LoginForm.userNotificationHandler");
//MochiKit.Logging.logDebug("userNotificationHandler - event: " + anEvent.event());
		this.getElement('loginForm_form').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		if (this.getDom('message') != null) {
			this.getElement('message').update(Clipperz.PM.Strings.messagePanelConfigurations[anEvent.parameters()]()['text']);
		}
//MochiKit.Logging.logDebug("<<< compact.LoginForm.userNotificationHandler");
	},

	//-----------------------------------------------------
	__syntaxFix__: '__syntaxFix__'
});


