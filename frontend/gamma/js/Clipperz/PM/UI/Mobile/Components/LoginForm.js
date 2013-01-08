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

Clipperz.Base.module('Clipperz.PM.UI.Mobile.Components');

Clipperz.PM.UI.Mobile.Components.LoginForm = function(args) {
	args = args || {};

	this._pin = '';

	this._message = null;
	this._steps = 0;
	this._actualSteps = 0;

	this._callback = null;
	this._errorCallback  = null;

	this._mode = 'CREDENTIALS';

	Clipperz.PM.UI.Mobile.Components.LoginForm.superclass.constructor.apply(this, arguments);

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Mobile.Components.LoginForm, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Mobile.Components.LoginForm component";
	},

	//-------------------------------------------------------------------------

	'callback': function () {
		return this._callback;
	},

	'errorCallback': function () {
		return this._errorCallback;
	},

	//-------------------------------------------------------------------------

	'mode': function () {
		return this._mode;
	},

	'setMode': function (aValue) {
		this._mode = aValue;
	},

	//..........................................................................

	'pin': function () {
		return this._pin;
	},

	'setPin': function (aValue) {
		this._pin = aValue;
	},

	//..........................................................................

	'username': function () {
		return this._username;
	},

	'setUsername': function (aValue) {
		this._username = aValue;
	},

	//..........................................................................

	'passphrase': function () {
		return this._passphrase;
	},

	'setPassphrase': function (aValue) {
		this._passphrase = aValue;
	},

	//-------------------------------------------------------------------------

	'message': function () {
		return this._message;
	},

	'_setMessage': function (aValue) {
		this._message = aValue;

		if (aValue == null) {
			MochiKit.Style.hideElement(this.getElement('credentialsMessage'));
		} else {
			this.getElement('message').innerHTML = aValue;
			MochiKit.Style.showElement(this.getElement('credentialsMessage'));
		}
	},

	'setMessage': function (aValue) {
		this._setMessage(aValue);
		MochiKit.DOM.removeElementClass(this.getElement('credentialsMessage'), 'error');
	},

	'setErrorMessage': function (aValue) {
		this._setMessage(aValue);
		MochiKit.DOM.addElementClass(this.getElement('credentialsMessage'), 'error');
	},

	//-------------------------------------------------------------------------

	'setCallbacks': function (args) {
		this._callback = args['callback'];
		this._errorCallback = args['errorCallback'];
	},

	'showErrors': function (args) {
//console.log("LoginForm.showErrors", args);
		if (args['previousFailedAttempt'] == 'LOGIN') {
			this.setErrorMessage("Wrong credentials");
		} else if (args['previousFailedAttempt'] == 'PIN') {
			if (args['failedAttempts'] == -1) {
				this.setErrorMessage("Wrong PIN - Resetted");
			} else {
				this.setErrorMessage("Wrong PIN");
			}
		} else {
			this.setMessage(null);
		}
	},

	'updateWithArgs': function (args) {
		this.renderIfNeeded();
		this.setCallbacks(args);
		this.showErrors(args);
		this.updateRendering();
	},

	'showPinLogin': function (args) {
		this.setPin('');
		this.setMode('PIN');
		this.updateWithArgs(args);

//		$(this.getAnchor('PIN')).focus();
		this.getElement('PIN').focus();
	},

	'showCredentialsLogin': function (args) {
		this.setMode('CREDENTIALS');
		this.updateWithArgs(args);

		if (this.getElement('usernameField').value.length == 0) {
//			$(this.getAnchor('usernameField')).focus();
			this.getElement('usernameField').focus();
		} else {
//			$(this.getAnchor('passphraseField')).focus();
			this.getElement('passphraseField').focus();
			this.getElement('passphraseField').select();
		}
	},

	//-------------------------------------------------------------------------

	'renderIfNeeded': function () {
		if (this.isFullyRendered() == false) {
			this.render();
		};
		this.updateRendering();
	},

	'updateRendering': function () {
		MochiKit.Style.showElement(this.getElement('credentialsBody'));
		MochiKit.Style.hideElement(this.getElement('validating'));

//		this.hideAllPanes();
		MochiKit.Base.map(function (aNode) { MochiKit.Style.hideElement(aNode); },  MochiKit.Selector.findDocElements('div.credentialsBody > div'));
		if (this.mode() == 'CREDENTIALS') {
			selectedPanel = this.getElement('credentials')
		} else if (this.mode() == 'PIN') {
			selectedPanel = this.getElement('pin')
//			this.updatePinDisplay();
		} else {
			throw 'Unhandled login form mode';
		}
		MochiKit.Style.showElement(selectedPanel);

		MochiKit.Style.hideElement(this.getElement('validating'));
	},

	'renderSelf': function() {
		var selectedPanel;
		this.append(this.element(), {tag:'div', id:'login', children:[
			{tag:'div', cls:'toolbar', children:[
				{tag:'h1', html:"clipperz"}
			]},
			{tag:'div', cls:'scroll', children:[
				//==================================================================
				{tag:'div', cls:'credentialsMessage', id:this.getId('credentialsMessage'), children:[
					{tag:'h1', cls:'message', id:this.getId('message'), html:"Message"}
				]},
				//==================================================================
				{tag:'div', cls:'credentialsBody', id:this.getId('credentialsBody'), children:[
					//--------------------------------------------------------------
					{tag:'div', cls:'pin', id:this.getId('pin'), children:[
						{tag:'form', cls:'scroll', id:this.getId('pinForm'), children:[
							{tag:'ul', cls:'edit rounded', children:[
								{tag:'li', children:[{tag:'input', type:'number',	name:'PIN',	placeholder:"PIN",	  id:this.getId('PIN')   }]},
							]},
							{tag:'a',  href:'#', cls:'greenButton', id:this.getId('pinSubmitButton'), html:"Login"}
						]}
					]},
					//--------------------------------------------------------------
					{tag:'div', cls:'credentials', id:this.getId('credentials'), children:[
						{tag:'form', cls:'scroll', id:this.getId('credentialsForm'), children:[
							{tag:'ul', cls:'edit rounded', children:[
								{tag:'li', children:[{tag:'input', type:'email',	name:'name',		/*value:'joe',*/		placeholder:"username",	  id:this.getId('usernameField')   }]},
								{tag:'li', children:[{tag:'input', type:'password',	name:'passphrase',	/*value:'clipperz',*/	placeholder:"passphrase", id:this.getId('passphraseField') }]}
							]},
							{tag:'a',  href:'#', cls:'greenButton', id:this.getId('credentialsSubmitButton'), html:"Login"}
//							{tag:'input', type:'submit', cls:'greenButton', id:this.getId('credentialsSubmitButton'), value:"Login"}

						]}
					]},
					//--------------------------------------------------------------
				]},
				//==================================================================
				{tag:'div', cls:'validating', id:this.getId('validating'), children:[
					{tag:'div', cls:'loading', children:[
						{tag:'div', cls:'spinner', children:[
							{tag:'div', cls:'bar01'},
							{tag:'div', cls:'bar02'},
							{tag:'div', cls:'bar03'},
							{tag:'div', cls:'bar04'},
							{tag:'div', cls:'bar05'},
							{tag:'div', cls:'bar06'},
							{tag:'div', cls:'bar07'},
							{tag:'div', cls:'bar08'},
							{tag:'div', cls:'bar09'},
							{tag:'div', cls:'bar10'},
							{tag:'div', cls:'bar11'},
							{tag:'div', cls:'bar12'}
						]}
					]},
					{tag:'div', id:this.getId('loadingMessage')},
					{tag:'a',  href:'#', cls:'grayButton', id:this.getId('loginCancelButton'), html:"Cancel"}
				]}
				//==================================================================
			]}
		]});

		MochiKit.Signal.connect(this.getElement('credentialsForm'),			'onsubmit',		this, 'submitCredentialsHandler');
		MochiKit.Signal.connect(this.getElement('credentialsSubmitButton'),	'onclick',		this, 'submitCredentialsHandler');

		MochiKit.Signal.connect(this.getElement('pinForm'),					'onsubmit',		this, 'submitPinHandler');
		MochiKit.Signal.connect(this.getElement('pinSubmitButton'),			'onclick',		this, 'submitPinHandler');

		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'initProgress',		this, 'initProgressHandle');
		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'updateProgress',	this, 'updateProgressHandle');
		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'advanceProgress',	this, 'advanceProgressHandle');
		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'progressDone',		this, 'progressDoneHandle');
	},

	//-------------------------------------------------------------------------

	'submitPinHandler': function (anEvent) {
		var	pin;

		this.setMessage(null);
		pin = this.getElement('PIN').value;
//		$(this.getAnchor('PIN')).blur();
		this.getElement('PIN').blur();

		credentials = Clipperz.PM.PIN.credentialsWithPIN(pin);
		this.loginWithCredentials(credentials);
	},

	'submitCredentialsHandler': function (anEvent) {
//console.log("submitCredentialsHandler");
		var	credentials;

		this.setMessage(null);

		credentials = {};
		credentials['username'] = this.getElement('usernameField').value;
		credentials['passphrase'] = this.getElement('passphraseField').value;
//		$(this.getAnchor('passphraseField')).blur();
		this.getElement('passphraseField').blur();

		this.loginWithCredentials(credentials);
	},

	//-------------------------------------------------------------------------

	'loginWithCredentials': function (someCredentials) {
		var	args;

		args = {};
		args['credentials'] = someCredentials;
		args['errorCallback'] = this.errorCallback();

		MochiKit.Style.hideElement(this.getElement('credentialsBody'));
		MochiKit.Style.showElement(this.getElement('validating'));

		MochiKit.Async.callLater(0.1, this.callback(), args);
	},

	//-------------------------------------------------------------------------

	'initProgressHandle': function (anEvent) {
//console.log("** initProgressHandle", anEvent);
		this._steps = anEvent['steps'];
		this._actualSteps = 0;
	},

	'updateProgressHandle': function (anEvent) {
//console.log("** updateProgressHandle", anEvent);
		this._steps += anEvent['extraSteps'];
	},

	'advanceProgressHandle': function (anEvent) {
//console.log("** advanceProgressHandle", anEvent);
		this._actualSteps ++;
//console.log("STEPS: " + this._actualSteps + "/" + this._steps);
	},

	'progressDoneHandle': function (anEvent) {
//console.log("** progressDoneHandle", anEvent);
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
