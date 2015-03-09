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
if (typeof(Clipperz.PM.Components.Panels) == 'undefined') { Clipperz.PM.Components.Panels = {}; }


Clipperz.PM.Components.Panels.LoginPanel = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Panels.LoginPanel.superclass.constructor.call(this, anElement, args);

	this._showLoginFormAnimator = null;
	this._showRegistrationFormAnimator = null;
	this._shouldShowRegistrationAlert = true;
	
	this._visibleForm = null;
//	this._isPassphraseVisible = true;

	Clipperz.NotificationCenter.register(null, 'switchLanguage', this, 'switchLanguageHandler');
	
	this.render();
	
	return this;
}

//=============================================================================

//MochiKit.Base.update(Clipperz.PM.Components.Panels.LoginPanel.prototype, {
YAHOO.extendX(Clipperz.PM.Components.Panels.LoginPanel, Clipperz.PM.Components.Panels.BasePanel, {

	'toString': function() {
		return "Clipperz.PM.Components.LoginPanel component";
	},
	
	//-------------------------------------------------------------------------

	'render': function() {
		var result;
		var	layout;
		var registerButton;
		
		MochiKit.Signal.disconnectAllTo(this);
		this.element().update("");
		
//MochiKit.Logging.logDebug(">>> LoginPanel.initPanel");
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', id:this.getId('baseDiv'), cls:'LoginPanel', children:[
			{tag:'table', children:[
				{tag:'thead'},
				{tag:'tbody', children:[
					{tag:'tr', children:[
						{tag:'td', valign:'top', children:[
							{tag:'div', cls:'clipperzServiceDescription', htmlString:Clipperz.PM.Strings['clipperzServiceDescription']}
						]},
						{tag:'td', valign:'top', align:'right', children:[
{tag:'div', id:this.getId('forms'), cls:'clipperzLoginForm', children:[
	{tag:'div', id:this.getId('loginForm'), cls:'loginForm', children:[
		{tag:'div', cls:'loginFormHeaderBox', children:[{tag:'h3', htmlString:Clipperz.PM.Strings['loginFormTitle']}]},
		{tag:'form', id:this.getId('loginForm_form'), autocomplete:'off', children:[

			{tag:'table', cls:'formLayout', children:[
				{tag:'thead'},
				{tag:'tbody', children:[
					{tag:'tr', cls:'formFieldTR', children:[
						{tag:'td', width:'90', htmlString:Clipperz.PM.Strings['loginFormUsernameLabel']},
						{tag:'td', children:[
							{tag:'input', id:this.getId('login_username'), cls:'loginFormField', type:'text', name:'username'}
						]}
					]},
					{tag:'tr', cls:'formFieldTR', children:[
						{tag:'td', htmlString:Clipperz.PM.Strings['loginFormPassphraseLabel']},
						{tag:'td', children:[
							{tag:'div', id:this.getId('passphraseDIV'), children:[
								{tag:'input', id:this.getId('login_passphrase'), cls:'loginFormField', type:'password', name:'passphrase'}
							]},
							{tag:'div', cls:'oneTimePassword', id:this.getId('oneTimePasswordDIV'), children:[
								{tag:'input', type:'text', id:this.getId('oneTimePassword_1'), name:'passphrase'},
								{tag:'span', html:'-'},
								{tag:'input', type:'text', id:this.getId('oneTimePassword_2'), name:'passphrase'},
								{tag:'span', html:'-'},
								{tag:'input', type:'text', id:this.getId('oneTimePassword_3'), name:'passphrase'},
								{tag:'span', html:'-'},
								{tag:'input', type:'text', id:this.getId('oneTimePassword_4'), name:'passphrase'}
							]}
						]}
					]},
					{tag:'tr', cls:'formFieldTR', id:this.getId('passwordTypeChooserTR'), children:[
						{tag:'td', valign:'top', align:'right', children:[
							{tag:'input', type:'checkbox', cls:'passwordTypeCheckbox', id:this.getId('useOneTimePasswordCheckbox')}
						]},
						{tag:'td', children:[
							{tag:'div', cls:'passwordTypeChooser', children:[
								{tag:'h4', htmlString:Clipperz.PM.Strings['loginFormOneTimePasswordCheckboxLabel']},
								{tag:'span', htmlString:Clipperz.PM.Strings['loginFormOneTimePasswordCheckboxDescription']}
							]}
						]}
					]},
					{tag:'tr', children:[
						{tag:'td'},
						{tag:'td', children:[
							{tag:'div', id:this.getId('login_submit')}
						]}
					]}
				]},
				{tag:'tfoot'}
			]}
		]},

		{tag:'div', cls:'loginFormFooterBox', children:[
			{tag:'ul', children:[
				{tag:'li', id:this.getId('showRegistrationLI'), children:[
					{tag:'span', htmlString:Clipperz.PM.Strings['loginFormDontHaveAnAccountLabel']},
					{tag:'a', href:'.', id:this.getId('showRegistration'), cls:'clipperzActionLink', htmlString:Clipperz.PM.Strings['loginFormCreateOneLabel']}
				]},
				{tag:'li', children:[
					{tag:'span', htmlString:Clipperz.PM.Strings['loginFormForgotYourCredentialsLabel']},
					{tag:'a', href:Clipperz.PM.Strings['loginFormAarghThatsBadUrl'], target:'Clipperz_Help', htmlString:Clipperz.PM.Strings['loginFormAarghThatsBadLabel']}
				]},
				{tag:'li', children:[
					{tag:'span', htmlString:Clipperz.PM.Strings['loginFormAfraidOfMaliciousScriptsLabel']},
					{tag:'a', href:Clipperz.PM.Strings['loginFormVerifyTheCodeUrl'], target:'Clipperz_Help', htmlString:Clipperz.PM.Strings['loginFormVerifyTheCodeLabel']}
				]}
			]}
		]}
	]},



	{tag:'div', id:this.getId('registrationForm'), cls:'registrationForm', children:[
		{tag:'div', cls:'loginFormHeaderBox', children:[{tag:'h3', htmlString:Clipperz.PM.Strings['registrationFormTitle']}]},
		{tag:'form', id:this.getId('registrationForm_form'), children:[
			{tag:'h5', cls:'errorMessage', id:this.getId('registration_errorMessage')},
			{tag:'table', cls:'formLayout', children:[
				{tag:'thead'},
				{tag:'tbody', children:[
					{tag:'tr', cls:'formFieldTR', children:[
						{tag:'td', width:'90', htmlString:Clipperz.PM.Strings['registrationFormUsernameLabel']},
						{tag:'td', children:[
							{tag:'input', id:this.getId('registration_username'), cls:'loginFormField', type:'text', name:'username'}
						]}
					]},
					{tag:'tr', cls:'formFieldTR', children:[
						{tag:'td', htmlString:Clipperz.PM.Strings['registrationFormPassphraseLabel']},
						{tag:'td', children:[
							{tag:'input', id:this.getId('registration_passphrase'), cls:'loginFormField', type:'password', name:'passphrase'}
						]}
					]},
					{tag:'tr', cls:'formFieldTR', children:[
						{tag:'td', htmlString:Clipperz.PM.Strings['registrationFormRetypePassphraseLabel']},
						{tag:'td', children:[
							{tag:'input', id:this.getId('registration_repassphrase'), cls:'loginFormField', type:'password', name:'repeat-passphrase'}
						]}
					]},
					{tag:'tr', cls:'formFieldTR', children:[
						{tag:'td', align:'right', valign:'top', children:[
							{tag:'input', id:this.getId('registration_check'), type:'checkbox', name:'safetyCheck'}
						]},
						{tag:'td', children:[
							{tag:'span', htmlString:Clipperz.PM.Strings['registrationFormSafetyCheckLabel']}
						]}
					]},
					{tag:'tr', cls:'formFieldTR', children:[
						{tag:'td', align:'right', valign:'top', children:[
							{tag:'input', id:this.getId('registration_termsOfServiceCheck'), type:'checkbox', name:'termsOfServiceCheck'}
						]},
						{tag:'td', children:[
							{tag:'span', htmlString:Clipperz.PM.Strings['registrationFormTermsOfServiceCheckLabel']}
						]}
					]},
//					{tag:'tr', cls:'formFieldTR', children:[
//						{tag:'td', htmlString:Clipperz.PM.Strings['registrationFormInvitationCodeLabel']},
//						{tag:'td', children:[
//							{tag:'input', id:this.getId('registration_invitationCode'), type:'text', size:'30', name:'invitationCode'}
//						]}
//					]},
					{tag:'tr', children:[
						{tag:'td'},
						{tag:'td', children:[
							{tag:'div', id:this.getId('registration_submit')}
						]}
					]}
				]},
				{tag:'tfoot'}
			]}
		]},
		{tag:'div', cls:'loginFormFooterBox', children:[
			{tag:'ul', children:[
				{tag:'li', children:[
					{tag:'span', htmlString:Clipperz.PM.Strings['registrationFormDoYouAlreadyHaveAnAccountLabel']},
					{tag:'a', href:'.', id:this.getId('showLogin'), cls:'clipperzActionLink', htmlString:Clipperz.PM.Strings['registrationFormSimplyLoginLabel']}
				]}
			]}
		]}
	]}
]},
							{tag:'div', cls:'loginPanelSwitchLanguageBox', children:[
								{tag:'div', cls:'loginPanelSwitchLanguageDescription', htmlString:Clipperz.PM.Strings['loginPanelSwithLanguageDescription']},
								{tag:'div', cls:'loginPanelSwitchLanguageSelect', children:[
									{tag:'select', id:this.getId('languageSelector'), children:Clipperz.PM.Strings['loginPanelSwitchLanguageSelectOptions']}
								]}
							]},
							{tag:'div', cls:'browserCompatibilityBox', htmlString:Clipperz.PM.Strings['browserCompatibilityDescription']}
						]}
					]}
				]}
			]}
		]});

		MochiKit.Signal.connect(this.getId('loginForm_form'), 'onsubmit', function(e){e.stop();});
		MochiKit.Signal.connect(this.getId('registrationForm_form'), 'onsubmit', function(e){e.stop();});

		MochiKit.Signal.connect(this.getId('showRegistration'), 'onclick', this, 'showRegistrationFormEventHandler');
		MochiKit.Signal.connect(this.getId('showLogin'), 'onclick', this, 'showLoginFormEventHandler');

		new YAHOO.ext.Button(this.getId('login_submit'), {text:Clipperz.PM.Strings['loginFormButtonLabel'], handler:this.doLogin, scope:this, minWidth:0});
		registerButton = new YAHOO.ext.Button(this.getId('registration_submit'), {text:Clipperz.PM.Strings['registrationFormButtonLabel'], handler:this.doRegister, scope:this, minWidth:0})

		if (Clipperz.PM.Proxy.defaultProxy.isReadOnly()) {
//			this.getElement('showRegistrationLI').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
			this.getElement('registrationForm_form').addClass('read-only');
			registerButton.disable();
			this.getElement('passwordTypeChooserTR').addClass('read-only');
			this.getDom('useOneTimePasswordCheckbox').disabled = true;
		}
		
		MochiKit.Signal.connect(this.getId('loginForm'), 'onkeydown', this, 'onkeydown');
		MochiKit.Signal.connect(this.getId('registrationForm'), 'onkeydown', this, 'onkeydown');
//		MochiKit.Signal.connect(this.getId('useOneTimePasswordCheckbox'), 'onchange', this, 'renderPasswordField');
		MochiKit.Signal.connect(this.getId('useOneTimePasswordCheckbox'), 'onclick', this, 'renderPasswordField');
		this.renderPasswordField();

		this.selectSelectedLanguageOption();
		MochiKit.Signal.connect(this.getId('languageSelector'), 'onchange', this, 'switchLanguage');

		MochiKit.Signal.connect(this.getId('oneTimePassword_1'), 'onkeypress', this, 'handleOneTimePasswordFieldKeyPress');
		MochiKit.Signal.connect(this.getId('oneTimePassword_2'), 'onkeypress', this, 'handleOneTimePasswordFieldKeyPress');
		MochiKit.Signal.connect(this.getId('oneTimePassword_3'), 'onkeypress', this, 'handleOneTimePasswordFieldKeyPress');
		MochiKit.Signal.connect(this.getId('oneTimePassword_4'), 'onkeypress', this, 'handleOneTimePasswordFieldKeyPress');

		MochiKit.Signal.connect(this.getId('oneTimePassword_1'), 'onkeyup', this, 'handleOneTimePasswordFieldKeyUp');
		MochiKit.Signal.connect(this.getId('oneTimePassword_2'), 'onkeyup', this, 'handleOneTimePasswordFieldKeyUp');
		MochiKit.Signal.connect(this.getId('oneTimePassword_3'), 'onkeyup', this, 'handleOneTimePasswordFieldKeyUp');
		MochiKit.Signal.connect(this.getId('oneTimePassword_4'), 'onkeyup', this, 'handleOneTimePasswordFieldKeyUp');

		new Clipperz.PM.Components.PasswordEntropyDisplay(this.getElement('login_passphrase'));
		new Clipperz.PM.Components.PasswordEntropyDisplay(this.getElement('registration_passphrase'));
//MochiKit.Logging.logDebug("<<< LoginPanel.initPanel");

		return result;
	},

	//-------------------------------------------------------------------------

	'renderPasswordField': function() {
		if (this.getDom('useOneTimePasswordCheckbox').checked == false) {
			this.getElement('oneTimePasswordDIV').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide(false);
			this.getElement('passphraseDIV').setVisibilityMode(YAHOO.ext.Element.DISPLAY).show(false);
			this.getElement('login_passphrase').focus();
		} else {
			this.getElement('passphraseDIV').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
			this.getElement('oneTimePasswordDIV').setVisibilityMode(YAHOO.ext.Element.DISPLAY).show();
			this.getElement('oneTimePassword_1').focus();
		}

	},
	
	//.........................................................................

	'handleOneTimePasswordFieldKeyPress': function(anEvent) {
		if (anEvent.key().string == '-') {
			switch (anEvent.src().id) {
				case this.getId('oneTimePassword_1'):
					this.getElement('oneTimePassword_2').focus();
					break;
				case this.getId('oneTimePassword_2'):
					this.getElement('oneTimePassword_3').focus();
					break;
				case this.getId('oneTimePassword_3'):
					this.getElement('oneTimePassword_4').focus();
					break;
			}
			
			anEvent.stop();
		}
	},

/*
  var ctrl = document.getElementById("txtTargetText");
  var saveText = ctrl.value;
  ctrl.focus();

  var range = document.selection.createRange();
  var specialchar = String.fromCharCode(1);
  range.text = specialchar;
  var pos = ctrl.value.indexOf(specialchar);
  ctrl.value = saveText;
  range = ctrl.createTextRange();
  range.move('character', pos);
  range.select();
  range.text = 
 document.getElementById("txtSourceText").value;
  document.getElementById("txtTargetText").focus();
  window.event.returnValue = false;
*/

	'handleOneTimePasswordFieldKeyUp': function(anEvent) {
		var	field;
		var	fieldValue;
		var key;
		
//console.log("keyUp", anEvent);
		field = anEvent.src();
		fieldValue = field.value;
		key = anEvent.key();

//			&&	(key.string != 'KEY_TAB')
//			&&	(key.string != 'KEY_SHIFT')
//			&&	(key.string != 'KEY_BACKSPACE')
//			&&	(key.string != 'KEY_DELETE')
//			&&	(key.string.indexOf('KEY_ARROW') != 0)
		

		if ((fieldValue.replace(/\s/g, '').length == 8) && (key.string == 'KEY_SPACEBAR')) {
//			field.value = Clipperz.Base.trim(fieldValue).replace(/.{4}/g, '$&' + ' ');
			
			switch (field.id) {
				case this.getId('oneTimePassword_1'):
					this.getElement('oneTimePassword_2').focus();
					break;
				case this.getId('oneTimePassword_2'):
					this.getElement('oneTimePassword_3').focus();
					break;
				case this.getId('oneTimePassword_3'):
					this.getElement('oneTimePassword_4').focus();
					break;
			}
//		} else if (fieldValue.replace(/\s/g, '').length > 8) {
		}
		
//MochiKit.Logging.logDebug("-- fieldValue: " + fieldValue);
	},
	
	//.........................................................................
	
	'doLogin': function() {
//MochiKit.Logging.logDebug(">>> LoginPanel.doLogin");
		if (this.checkLoginForm()) {
			if (this.getDom('useOneTimePasswordCheckbox').checked == false) {
				this.doLoginWithUsernameAndPassphrase(this.getDom('login_username').value, this.getDom('login_passphrase').value);
			} else {
				var	oneTimePasswordValue;
				
				oneTimePasswordValue = this.getDom('oneTimePassword_1').value + this.getDom('oneTimePassword_2').value + this.getDom('oneTimePassword_3').value + this.getDom('oneTimePassword_4').value;
				this.doLoginWithUsernameAndOneTimePassword(this.getDom('login_username').value, oneTimePasswordValue);
			}
		}
//MochiKit.Logging.logDebug("<<< LoginPanel.doLogin");
	},

	//.........................................................................

	'doLoginWithUsernameAndPassphrase': function(anUsername, aPassphrase) {
		var deferredResult;
		var user;
		var loginPanel;

//MochiKit.Logging.logDebug(">>> LoginPanel.doLoginWithUsernameAndPassphrase");
		user = new Clipperz.PM.DataModel.User({username:anUsername, passphrase:aPassphrase});
		loginPanel = this;

		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("0 - LoginPanel.doLogin - 0: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Components.MessageBox(), 'deferredShow'),
			{
				title: "",
				text: "",
				width:240,
				showProgressBar:true,
				showCloseButton:false,
				fn:MochiKit.Base.method(deferredResult, 'cancel'),
				scope:this
			},
			this.getDom('login_submit')
		);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1 - LoginPanel.doLogin - 1: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'login_start');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("2 - LoginPanel.doLogin - 2: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(user, 'connect'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("3 - LoginPanel.doLogin - 3: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'connection_done');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("4 - LoginPanel.doLogin - 4: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'connection_loadingUserData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("5 - LoginPanel.doLogin - 5: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(user, 'loadPreferences'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("6 - LoginPanel.doLogin - 6: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(user, 'loadRecords'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("7 - LoginPanel.doLogin - 7: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(user, 'loadDirectLogins'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("8 - LoginPanel.doLogin - 8: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'login_connected');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("9 - LoginPanel.doLogin - 9: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, user, 'setupDone', null);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("10 - LoginPanel.doLogin - 10: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});

		deferredResult.addCallback(MochiKit.Async.wait, 0.5);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("11 - LoginPanel.doLogin - 11: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});

		deferredResult.addCallback(function(res) {
			Clipperz.PM.Components.MessageBox().hide(YAHOO.ext.Element.get('main'));
			return res;
		});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("12 - LoginPanel.doLogin - 12: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});

		deferredResult.addErrback(MochiKit.Base.bind(function() {
			Clipperz.PM.Components.MessageBox().update({
				title:Clipperz.PM.Strings['loginMessagePanelFailureTitle'],
				text:Clipperz.PM.Strings['loginMessagePanelFailureText'],
				showProgressBar:false,
				buttons:{'ok':Clipperz.PM.Strings['loginMessagePanelFailureButtonLabel']},
				fn:MochiKit.Base.bind(function() {
					this.getElement('login_passphrase').focus();
					this.getElement('login_passphrase').dom.select();
				}, this),
				scope:this
			});
		}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("13 - LoginPanel.doLogin - 13: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		
		deferredResult.callback("token");
//MochiKit.Logging.logDebug("<<< LoginPanel.doLoginWithUsernameAndPassphrase");
		
		return deferredResult;
	},

	//.........................................................................

	'doLoginWithUsernameAndOneTimePassword': function(anUsername, aOneTimePassword) {
		var deferredResult;
		var loginPanel;
		var oneTimePassword;
		
		oneTimePassword = Clipperz.PM.DataModel.OneTimePassword.normalizedOneTimePassword(aOneTimePassword);

//MochiKit.Logging.logDebug(">>> LoginPanel.doLoginWithUsernameAndPassphrase");
		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Components.MessageBox(), 'deferredShow'),
			{
				title: "",
				text: "",
				width:240,
				showProgressBar:true,
				showCloseButton:false,
				fn:MochiKit.Base.method(deferredResult, 'cancel'),
				scope:this,
				steps:7,
				buttons:{
				//	'ok':Clipperz.PM.Strings['loginMessagePanelInitialButtonLabel']
				}
			},
			this.getDom('login_submit')
		);
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'OTP_login_start');
		deferredResult.addCallback(function(anUsername, aOneTimePassword) {
			var args;
			
			args = {
				'message': 'oneTimePassword',
				'version': Clipperz.PM.Crypto.communicationProtocol.currentVersion,
				'parameters': {
					'oneTimePasswordKey': Clipperz.PM.DataModel.OneTimePassword.computeKeyWithUsernameAndPassword(anUsername, aOneTimePassword),
					'oneTimePasswordKeyChecksum': Clipperz.PM.DataModel.OneTimePassword.computeKeyChecksumWithUsernameAndPassword(anUsername, aOneTimePassword)
				}
			}
			
			return args;
		}, anUsername, oneTimePassword);
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'OTP_login_loadingOTP');
		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Proxy.defaultProxy, 'handshake'));
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'OTP_login_extractingPassphrase');
		deferredResult.addCallback(function(aResult) {
			return Clipperz.PM.Crypto.deferredDecrypt(oneTimePassword, aResult['data'], aResult['version']);
		});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("LoginPanel.otpLogin - 4: " + res); return res;});
		deferredResult.addCallback(function(aResult) {
//MochiKit.Logging.logDebug("aResult")			
			return (new Clipperz.ByteArray().appendBase64String(aResult['passphrase'])).asString();
		});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("LoginPanel.otpLogin - 5: " + res); return res;});
		deferredResult.addCallbacks(
			MochiKit.Base.method(this, 'doLoginWithUsernameAndPassphrase', anUsername),
			MochiKit.Base.bind(function(aLoginPanel) {
				Clipperz.PM.Components.MessageBox().update({
					title:Clipperz.PM.Strings['loginMessagePanelFailureTitle'],
					text:Clipperz.PM.Strings['loginMessagePanelFailureText'],
					showProgressBar:false,
					buttons:{'ok':Clipperz.PM.Strings['loginMessagePanelFailureButtonLabel']},
					fn:MochiKit.Base.bind(function() {
						this.getElement('oneTimePassword_1').focus();
					}, this),
					scope:aLoginPanel
				});
			}, this)
		);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("LoginPanel.otpLogin - 6: " + res); return res;});
		deferredResult.callback("token");
//MochiKit.Logging.logDebug("<<< LoginPanel.doLoginWithUsernameAndPassphrase");
		
		return deferredResult;
	},
	
	//.........................................................................

	'checkLoginForm': function() {
		var result;
		var username

		result = true;
		
//MochiKit.Logging.logDebug(">>> checkLoginForm");
		username = Clipperz.Base.trim(this.getDom('login_username').value);
		if (username == "") {
			this.getElement('login_username').focus();
			result = false;
		} else {
			if (this.getDom('useOneTimePasswordCheckbox').checked == false) {
				var	passphrase;
			
				passphrase = Clipperz.Base.trim(this.getDom('login_passphrase').value);

				if (passphrase == "") {
					this.getElement('login_passphrase').focus();
					result = false;
				}
			} else {
				if (Clipperz.Base.trim(this.getDom('oneTimePassword_1').value) == "") {
					this.getElement('oneTimePassword_1').focus();
					result = false;
				} else  if (Clipperz.Base.trim(this.getDom('oneTimePassword_2').value) == "") {
					this.getElement('oneTimePassword_2').focus();
					result = false;
				} else if (Clipperz.Base.trim(this.getDom('oneTimePassword_3').value) == "") {
					this.getElement('oneTimePassword_3').focus();
					result = false;
				} else if (Clipperz.Base.trim(this.getDom('oneTimePassword_4').value) == "") {
					this.getElement('oneTimePassword_4').focus();
					result = false;
				}
			}
		}
//MochiKit.Logging.logDebug("<<< checkLoginForm - " + result);
		
		return result;
	},

	//.........................................................................

	'doRegister': function(anEvent) {
		if ((Clipperz.PM.Proxy.defaultProxy.isReadOnly() == false) && (this.checkRegistrationForm())) {
			this.doRegistrationWithUsernameAndPassphrase(this.getDom('registration_username').value, this.getDom('registration_passphrase').value /*, this.getDom('registration_invitationCode').value*/);
		}
	},

	//.........................................................................

	'checkRegistrationForm': function() {
		var result;
		var username
		var	passphrase;
		var	rePassphrase;
		var	safetyCheck;
		var	termsOfServiceCheck;
//		var	invitationCode;
		
		username = this.getDom('registration_username').value;
		passphrase = this.getDom('registration_passphrase').value;
		rePassphrase = this.getDom('registration_repassphrase').value;
		safetyCheck = this.getDom('registration_check').checked;
		termsOfServiceCheck = this.getDom('registration_termsOfServiceCheck').checked;
//		invitationCode = this.getDom('registration_invitationCode').value;
		
		if ((username != "") && (passphrase != "") && (rePassphrase != "") /*&& (invitationCode != "")*/) {
			if (passphrase != rePassphrase) {
				//	show alert that the passphrase and rePassphrase should be equal
//MochiKit.Logging.logDebug("WARNING: passphrase != rePassphrase");
				this.showRegistrationFormErrorMessageAnimation(Clipperz.PM.Strings['registrationFormWarningMessageNotMatchingPassphrases']);
				this.getElement('registration_repassphrase').focus().dom.select();
//				this.getElement('registration_repassphrase').select();
				result = false;
			} else if (safetyCheck != true) {
				//	show alert that the safetyCheck should be checed
//MochiKit.Logging.logDebug("WARNING: safetyCheck not checked");
				this.showRegistrationFormErrorMessageAnimation(Clipperz.PM.Strings['registrationFormWarningMessageSafetyCheckNotSelected']);
				this.getElement('registration_check').focus();
				result = false;
			} else if (termsOfServiceCheck != true) {
				this.showRegistrationFormErrorMessageAnimation(Clipperz.PM.Strings['registrationFormWarningMessageTermsOfServiceCheckNotSelected']);
				this.getElement('registration_termsOfServiceCheck').focus();
			} else {
				result = true;
			}
		} else {
			if (username == "") {
				this.getElement('registration_username').focus();
			} else if (passphrase == "") {
				this.getElement('registration_passphrase').focus();
			} else if (rePassphrase == "") {
				this.getElement('registration_repassphrase').focus();
//			} else if (invitationCode == "") {
//				this.showRegistrationFormErrorMessageAnimation(Clipperz.PM.Strings['registrationFormWarningMessageInvitationCodeMissing']);
//				this.getElement('registration_invitationCode').focus();
			}
			
			result = false;
		}
		
		if (result === true) {
			this.getActor('registration_errorMessage').hide();
		}
		
		return result;
	},

	//.........................................................................

	'showRegistrationFormErrorMessageAnimation': function(anErrorMessage, aCallback) {
		var	errorMessageActor;
		
		errorMessageActor = this.getActor('registration_errorMessage');
		errorMessageActor.update(anErrorMessage);
		errorMessageActor.show(true);
		errorMessageActor.play(aCallback);
	},

	//.........................................................................
	
	'doRegistrationWithUsernameAndPassphrase': function(anUsername, aPassphrase /*, anInvitationCode */) {
		var deferredResult;
		var user;
		var loginPanel;

//MochiKit.Logging.logDebug(">>> LoginPanel.doRegistrationWithUsernameAndPassphrase");
		user = new Clipperz.PM.DataModel.User({username:anUsername, passphrase:aPassphrase});
//MochiKit.Logging.logDebug("--- LoginPanel.doRegistrationWithUsernameAndPassphrase - 1");
		loginPanel = this;
//MochiKit.Logging.logDebug("--- LoginPanel.doRegistrationWithUsernameAndPassphrase - 2");

		deferredResult = new MochiKit.Async.Deferred();
//MochiKit.Logging.logDebug("--- LoginPanel.doRegistrationWithUsernameAndPassphrase - 3");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("LoginPanel.doRegistrationWithUsernameAndPassphrase - 1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Components.MessageBox(), 'deferredShow'),
			{
				title:Clipperz.PM.Strings['registrationMessagePanelInitialTitle'],
				text: Clipperz.PM.Strings['registrationMessagePanelInitialText'],
				width:240,
				showProgressBar:true,
				showCloseButton:false,
				fn:MochiKit.Base.method(deferredResult, 'cancel'),
				scope:this,
				steps:9,
				buttons:{
					'ok':Clipperz.PM.Strings['registrationMessagePanelInitialButtonLabel']
				}
			},
			this.getDom('registration_submit')
		);
//MochiKit.Logging.logDebug("--- LoginPanel.doRegistrationWithUsernameAndPassphrase - 4");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("LoginPanel.doRegistrationWithUsernameAndPassphrase - 2: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(user, 'register'), /*anInvitationCode*/ "VeryBraveBetaTester");
//MochiKit.Logging.logDebug("--- LoginPanel.doRegistrationWithUsernameAndPassphrase - 5");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("LoginPanel.doRegistrationWithUsernameAndPassphrase - 3: " + res); return res;});
		deferredResult.addCallback(function(res) {
			Clipperz.PM.Components.MessageBox().update({
				title:Clipperz.PM.Strings['registrationMessagePanelRegistrationDoneTitle'],
				text:Clipperz.PM.Strings['registrationMessagePanelRegistrationDoneText'],
				step:'next'
			});
			return res;
		});
//MochiKit.Logging.logDebug("--- LoginPanel.doRegistrationWithUsernameAndPassphrase - 6");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("LoginPanel.doRegistrationWithUsernameAndPassphrase - 5: " + res); return res;});
		deferredResult.addCallback(MochiKit.Async.wait, 1);
//MochiKit.Logging.logDebug("--- LoginPanel.doRegistrationWithUsernameAndPassphrase - 7");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("LoginPanel.doRegistrationWithUsernameAndPassphrase - 6: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(user, 'connect'));
//MochiKit.Logging.logDebug("--- LoginPanel.doRegistrationWithUsernameAndPassphrase - 8");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("LoginPanel.doRegistrationWithUsernameAndPassphrase - 7: " + res); return res;});
		deferredResult.addCallback(MochiKit.Async.wait, 1);
//MochiKit.Logging.logDebug("--- LoginPanel.doRegistrationWithUsernameAndPassphrase - 9");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("LoginPanel.doRegistrationWithUsernameAndPassphrase - 8: " + res); return res;});
		deferredResult.addCallback(function(res) {
			Clipperz.PM.Components.MessageBox().hide(YAHOO.ext.Element.get('main'));
			return res;
		});
		deferredResult.addCallback(MochiKit.Base.method(this, 'showRegistrationSplashScreen'), user);
//MochiKit.Logging.logDebug("--- LoginPanel.doRegistrationWithUsernameAndPassphrase - 10");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("LoginPanel.doRegistrationWithUsernameAndPassphrase - 9: " + res); return res;});
		deferredResult.addErrback(function(anError) {
			Clipperz.PM.Components.MessageBox().update({
				title:Clipperz.PM.Strings['registrationMessagePanelFailureTitle'],
				text:anError.message,
				showProgressBar:false,
				buttons:{'ok':Clipperz.PM.Strings['registrationMessagePanelFailureButtonLabel']}});
			return anError;
		});
//MochiKit.Logging.logDebug("--- LoginPanel.doRegistrationWithUsernameAndPassphrase - 11");
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("LoginPanel.doRegistrationWithUsernameAndPassphrase - 10: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, user, 'setupDone', null);
		deferredResult.callback("token");
//MochiKit.Logging.logDebug("--- LoginPanel.doRegistrationWithUsernameAndPassphrase - 12");
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'showRegistrationSplashScreen': function(anUser) {
		var deferredResult;
		var alertElement;
		var	alertDialog;
		var closeButton;
		var closeFunction;

		deferredResult = new MochiKit.Async.Deferred();

//MochiKit.Logging.logDebug(">>> Main.showRegistrationSplashScreen");
		alertElement = Clipperz.YUI.DomHelper.append(document.body, {tag:'div', id:'registrationSplash', children:[
			{tag:'div', cls:'ydlg-hd', htmlString:Clipperz.PM.Strings['registrationSplashPanelTitle']},
			{tag:'div', cls:'ydlg-bd', children:[
				{tag:'div', cls:'alert-message', id:'splashMessage', children:[
					{tag:'div', htmlString:Clipperz.PM.Strings['registrationSplashPanelDescription']},
					{tag:'table', border:'0', cellpadding:'5', children:[
						{tag:'tbody', children:[
							{tag:'tr', children:[
								{tag:'td', valign:'top', children:[
									{tag:'span', cls:'label', htmlString:Clipperz.PM.Strings['registrationSplashPanelUsernameLabel']}
								]},
								{tag:'td', valign:'top', children:[
									{tag:'span', cls:'value', html:Clipperz.Base.escapeHTML(anUser.username())}
								]}
							]},
							{tag:'tr', children:[
								{tag:'td', valign:'top', children:[
									{tag:'span', cls:'label', htmlString:Clipperz.PM.Strings['registrationSplashPanelPassphraseLabel']}
								]},
								{tag:'td', valign:'top', children:[
									{tag:'div', id:this.getId('showPassphraseDiv'), children:[
										{tag:'span', cls:'value', html:Clipperz.Base.escapeHTML(anUser.passphrase())}
									]},
									{tag:'div', id:this.getId('hidePassphraseDiv'), cls:'Clipperz_recordFieldData', children:[
										{tag:'input', id:this.getId('passwordField'), type:'text', cls:'scrambledField', title:Clipperz.PM.Strings['recordDetailPasswordFieldTooltipLabel'], value:"anUser.passphrase()"}
									]}
								]}
							]},
							{tag:'tr', children:[
								{tag:'td'},
								{tag:'td', valign:'top', children:[
//									{tag:'a', href:"#", id:this.getId('togglePassphraseVisibility'), htmlString:Clipperz.PM.Strings['registrationSplashPanelShowPassphraseButtonLabel']} 
									{tag:'input', type:'checkbox', id:this.getId('showPassphraseCheckbox')},
									{tag:'span', cls:'activeText', id:this.getId('showPassphraseText'), htmlString:Clipperz.PM.Strings['registrationSplashPanelShowPassphraseButtonLabel']}
								]}
							]}
						]}
					]}
				]}
			]},
			{tag:'div', cls:'ydlg-ft'}
		]}, true);
//MochiKit.Logging.logDebug("--- Main.showRegistrationSplashScreen - 1");

		this.getElement('passwordField').dom.value = anUser.passphrase();
		this.getElement('passwordField').wrap({tag:'div', cls:'passwordBackground'}).setStyle('background-position', "0px -" + Math.min(128, Clipperz.PM.Crypto.passwordEntropy(anUser.passphrase())) + "px").setStyle('width', '71px');
		MochiKit.Signal.connect(this.getId('showPassphraseCheckbox'), 'onclick', this, 'togglePassphraseVisibility');
		MochiKit.Signal.connect(this.getId('showPassphraseText'), 'onclick', this, 'togglePassphraseCheckbox');
//MochiKit.Logging.logDebug("--- Main.showRegistrationSplashScreen - 1.1");

		this.getElement('showPassphraseDiv').setVisibilityMode(YAHOO.ext.Element.DISPLAY);
		this.getElement('hidePassphraseDiv').setVisibilityMode(YAHOO.ext.Element.DISPLAY);
//MochiKit.Logging.logDebug("--- Main.showRegistrationSplashScreen - 1.1.1");
		this.togglePassphraseVisibility();
//MochiKit.Logging.logDebug("--- Main.showRegistrationSplashScreen - 1.2");
		
		alertDialog = new YAHOO.ext.BasicDialog(
			alertElement, { 
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
//MochiKit.Logging.logDebug("--- Main.showRegistrationSplashScreen - 2");
	
		closeFunction = deferredResult.callback;
//MochiKit.Logging.logDebug("--- Main.showRegistrationSplashScreen - 3");
		alertDialog.addKeyListener(27, closeFunction);
//MochiKit.Logging.logDebug("--- Main.showRegistrationSplashScreen - 4");
		closeButton = alertDialog.addButton(Clipperz.PM.Strings['splashAlertCloseButtonLabel'], closeFunction, deferredResult);
//MochiKit.Logging.logDebug("--- Main.showRegistrationSplashScreen - 5");
		alertDialog.setDefaultButton(closeButton);
//MochiKit.Logging.logDebug("--- Main.showRegistrationSplashScreen - 6");
		alertDialog.show('main');
//MochiKit.Logging.logDebug("--- Main.showRegistrationSplashScreen - 7");

		deferredResult.addBoth(MochiKit.Base.method(alertDialog, 'hide'));
		deferredResult.addCallback(MochiKit.Signal.disconnectAllTo, this)
//MochiKit.Logging.logDebug("--- Main.showRegistrationSplashScreen - 8");
		deferredResult.addCallback(MochiKit.Async.succeed);
//MochiKit.Logging.logDebug("<<< Main.showRegistrationSplashScreen");
		
		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	'showLoginFormEventHandler': function(anEvent) {
		anEvent.stop();
		this.showLoginForm(true);
	},
	
	'showLoginForm': function(shouldAnimate) {
		if (shouldAnimate) {
			this.showLoginFormAnimator().play();
		} else {
			this.hideRegistrationForm(false);
			this.getActor('loginForm').show(false);
			this.getElement('login_username').focus();
		}
		this.setVisibleForm('login');
	},

	'showLoginFormAnimator': function() {
		if (this._showLoginFormAnimator == null) {
			var animator;
			var	loginFormActor;
			var registrationFormActor;
			var usernameFieldActor;
			
			animator = new YAHOO.ext.Animator();
			loginFormActor = this.getActor('loginForm', animator);
			loginFormActor.setVisibilityMode(YAHOO.ext.Element.DISPLAY);
			registrationFormActor = this.getActor('registrationForm', animator);
			registrationFormActor.setVisibilityMode(YAHOO.ext.Element.DISPLAY);
			usernameFieldActor = this.getActor('login_username', animator);
			
			animator.startCapture();
			registrationFormActor.hide(true);
			loginFormActor.show(true);
			usernameFieldActor.focus();
			animator.stopCapture();

			this._showLoginFormAnimator = animator;
		}
		
		return this._showLoginFormAnimator;
	},

	
	'hideLoginForm': function(shouldAnimate) {
		var loginFormActor;

		loginFormActor = this.getActor('loginForm');
		loginFormActor.setVisibilityMode(YAHOO.ext.Element.DISPLAY);
		loginFormActor.hide(shouldAnimate);
	},

	//-------------------------------------------------------------------------

	'showRegistrationFormEventHandler': function(anEvent) {
		anEvent.stop();
		this.showRegistrationForm(true);
	},
	
	'showRegistrationForm': function(shouldAnimate) {
		if (shouldAnimate) {
			this.showRegistrationFormAnimator().play(MochiKit.Base.bind(this.showRegistrationAlert, this));
		} else {
			var	errorMessageActor;
			
			this.hideLoginForm(shouldAnimate)
			this.getActor('registrationForm').show(false);
			this.getElement('registration_username').focus();

			errorMessageActor = this.getActor('registration_errorMessage');
			errorMessageActor.setVisibilityMode(YAHOO.ext.Element.DISPLAY);
			errorMessageActor.update("---");
			errorMessageActor.hide();
			
			this.showRegistrationAlert();
		}
		this.setVisibleForm('registration');
	},

	'showRegistrationFormAnimator': function() {
		if (this._showRegistrationFormAnimator == null) {
			var animator;
			var	loginFormActor;
			var registrationFormActor;
			var usernameFieldActor;
			var	errorMessageActor;
			
			animator = new YAHOO.ext.Animator();
			loginFormActor = this.getActor('loginForm', animator);
			loginFormActor.setVisibilityMode(YAHOO.ext.Element.DISPLAY);
			registrationFormActor = this.getActor('registrationForm', animator);
			registrationFormActor.setVisibilityMode(YAHOO.ext.Element.DISPLAY);
			usernameFieldActor = this.getActor('registration_username', animator);
			errorMessageActor = this.getActor('registration_errorMessage', animator);
			errorMessageActor.setVisibilityMode(YAHOO.ext.Element.DISPLAY);
			
			animator.startCapture();
			loginFormActor.hide(true);
			errorMessageActor.update("---");
			errorMessageActor.hide();
			registrationFormActor.show(true);
			usernameFieldActor.focus();
			animator.stopCapture();

			this._showRegistrationFormAnimator = animator;
		}
		
		return this._showRegistrationFormAnimator;
	},
	
	'hideRegistrationForm': function(shouldAnimate) {
		var registrationFormActor;

		registrationFormActor = this.getActor('registrationForm');
		registrationFormActor.setVisibilityMode(YAHOO.ext.Element.DISPLAY);
		registrationFormActor.hide(shouldAnimate);
	},

	//-------------------------------------------------------------------------

	'shouldShowRegistrationAlert': function() {
		return this._shouldShowRegistrationAlert;
	},
	
	'showRegistrationAlert': function() {
		if ((this.shouldShowRegistrationAlert()) && (Clipperz.PM.Proxy.defaultProxy.isReadOnly() == false)) {
			var alertElement;
			var	alertDialog;
			var closeButton;
			var closeFunction;

			alertElement = Clipperz.YUI.DomHelper.append(document.body, {tag:'div', id:'alert', children:[
				{tag:'div', cls:'ydlg-hd', htmlString:Clipperz.PM.Strings['splashAlertTitle']},
				{tag:'div', cls:'ydlg-bd', children:[
					{tag:'div', cls:'alert-message', id:'splashMessage', htmlString:Clipperz.PM.Strings['splashAlertText']}
				]},
				{tag:'div', cls:'ydlg-ft'}
			]}, true);

			alertDialog = new YAHOO.ext.BasicDialog(
				alertElement, { 
					closable:false,
					modal:true,
					autoTabs:false,
					resizable:false,
					fixedcenter:true,
					constraintoviewport:false,
					width:450,
					height:320,
					shadow:true,
					minWidth:300,
					minHeight:300
				}
			);
		
			closeFunction = MochiKit.Base.partial(MochiKit.Base.bind(this.closeResigrationAlert, this), alertDialog);
			alertDialog.addKeyListener(27, closeFunction);
			closeButton = alertDialog.addButton(Clipperz.PM.Strings['splashAlertCloseButtonLabel'], closeFunction, this);
			alertDialog.setDefaultButton(closeButton);
			alertDialog.show('main');
			
			this._shouldShowRegistrationAlert = false;
		}
	},

	'closeResigrationAlert': function(anAlertDialog) {
		anAlertDialog.hide(MochiKit.Base.bind(function() {anAlertDialog.destroy(true); this.focusOnVisibleForm();}, this));
	},
	
	//-------------------------------------------------------------------------

	'onkeydown': function(anEvent) {
//MochiKit.Logging.logDebug(">>> onkeydown - " + anEvent.src().id);
		if (anEvent.key().code == 13) {
			if (anEvent.src() == this.getDom('loginForm')) {
				this.doLogin();
			} else if (anEvent.src() == this.getDom('registrationForm')) {
				this.doRegister();
			} else {
			}
			
			anEvent.stop();
		}
	},

	//-------------------------------------------------------------------------

	'visibleForm': function() {
		return this._visibleForm;
	},

	'setVisibleForm': function(aValue) {
		this._visibleForm = aValue;
	},

	//-------------------------------------------------------------------------

	'focusOnVisibleForm': function() {
		if (this.visibleForm() == 'registration') {
			this.getElement('registration_username').focus();
		} else {
			this.getElement('login_username').focus();
		}
	},

	//-------------------------------------------------------------------------
	
	'show': function() {
		if (this.visibleForm() == 'registration') {
			this.showRegistrationForm(false);
		} else {
			this.showLoginForm(false);
		}
	},
	
	//-------------------------------------------------------------------------
	
	'switchLanguage': function(anEvent) {
		Clipperz.PM.Strings.Languages.setSelectedLanguage(anEvent.src().value);
	},

	//-------------------------------------------------------------------------

	'selectSelectedLanguageOption': function() {
		Clipperz.DOM.selectOptionMatchingValue(this.getDom('languageSelector'), Clipperz.PM.Strings.selectedLanguage, true);
	},

	//-------------------------------------------------------------------------

	'switchLanguageHandler': function() {
		this.render();
		this.show();
	},

	//-------------------------------------------------------------------------

	'togglePassphraseCheckbox': function(anEvent) {
		this.getDom('showPassphraseCheckbox').click();
	},
	
	'togglePassphraseVisibility': function(anEvent) {
		if (this.getDom('showPassphraseCheckbox').checked == true) {
			this.getElement('showPassphraseDiv').show();
			this.getElement('hidePassphraseDiv').hide();
		} else {
			this.getElement('showPassphraseDiv').hide();
			this.getElement('hidePassphraseDiv').show();
		}
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});
