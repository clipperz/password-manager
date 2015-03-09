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

//Clipperz.Base.extend(Clipperz.PM.UI.Mobile.Components.LoginForm, Clipperz.PM.UI.Common.Components.BaseComponent, {
Clipperz.Base.extend(Clipperz.PM.UI.Mobile.Components.LoginForm, Clipperz.PM.UI.Mobile.Components.BaseComponent, {

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

//		if (aValue == null) {
//			MochiKit.Style.hideElement(this.getElement('credentialsMessage'));
//		} else {
//			this.getElement('message').innerHTML = aValue;
//			MochiKit.Style.showElement(this.getElement('credentialsMessage'));
//		}
	},

	'setMessage': function (aValue) {
		this._setMessage(aValue);
//		MochiKit.DOM.removeElementClass(this.getElement('credentialsMessage'), 'error');
	},

	'setErrorMessage': function (aValue) {
		this._setMessage(aValue);
//		MochiKit.DOM.addElementClass(this.getElement('credentialsMessage'), 'error');
	},

	//-------------------------------------------------------------------------

	'setCallbacks': function (args) {
		this._callback = args['callback'];
		this._errorCallback = args['errorCallback'];
	},

	'show': function (args) {
		this.updateWithArgs(args);

		if (this.mode() == 'PIN') {
			this.setPin('');
			this.getElement('PIN').focus();
		} else if (this.mode() == 'CREDENTIALS') {
			if (this.getElement('usernameField').value.length == 0) {
				this.getElement('usernameField').focus();
			} else {
				this.getElement('passphraseField').focus();
				this.getElement('passphraseField').select();
			}
		}
	},

	'showErrors': function (args) {
		if (args['previousFailedAttempt'] == 'LOGIN') {
			$(this.getAnchor('credentialsSubmitButton')).button('enable');
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
		this.renderOnlyOnce();
		this.setCallbacks(args);
		this.showErrors(args);
//		this.updateRendering();
	},

	//-------------------------------------------------------------------------

	'renderOnlyOnce': function () {
		if (this.isFullyRendered() == false) {
			this.render();
		};
//		this.updateRendering();
	},
/*
	'updateRendering': function () {
		MochiKit.Style.showElement(this.getElement('credentialsBody'));
		MochiKit.Style.hideElement(this.getElement('validating'));

//		this.hideAllPanes();
		MochiKit.Base.map(function (aNode) { MochiKit.Style.hideElement(aNode); },  MochiKit.Selector.findDocElements('div.credentialsBody > div'));
		if (this.mode() == 'CREDENTIALS') {
			selectedPanel = this.getElement('credentials');
			$(this.getAnchor('credentialsSubmitButton')).button('enable');
		} else if (this.mode() == 'PIN') {
			selectedPanel = this.getElement('pin')
//			this.updatePinDisplay();
		} else {
			throw 'Unhandled login form mode';
		}

		MochiKit.Style.showElement(selectedPanel);
		MochiKit.Style.hideElement(this.getElement('validating'));
	},
*/
/*
	'_renderSelf': function() {
		var selectedPanel;
		this.append(this.element(), {tag:'div', id:'login', children:[
			{tag:'div', cls:'toolbar text-center', children:[
				{tag:'h1', cls:'clipperz', html:"clipperz"}
			]},
			{tag:'div', cls:'', children:[
				//==================================================================
				{tag:'div', cls:'credentialsMessage', id:this.getId('credentialsMessage'), children:[
					{tag:'h1', cls:'message', id:this.getId('message'), html:"Message"}
				]},
				//==================================================================
				{tag:'div', cls:'credentialsBody', id:this.getId('credentialsBody'), children:[
					//--------------------------------------------------------------
					{tag:'div', cls:'pin', id:this.getId('pin'), children:[
						{tag:'form', cls:'', id:this.getId('pinForm'), children:[
							{tag:'ul', cls:'edit rounded', children:[
								{tag:'li', children:[{tag:'input', type:'number',	name:'PIN',	placeholder:"PIN",	  id:this.getId('PIN')   }]},
							]},
							{tag:'a',  href:'#', cls:'greenButton', id:this.getId('pinSubmitButton'), html:"Login"}
						]}
					]},
					//--------------------------------------------------------------
					{tag:'div', cls:'credentials', id:this.getId('credentials'), children:[
						{tag:'form', cls:'text-center', id:this.getId('credentialsForm'), children:[
							{tag:'fieldset', children:[
//								{tag:'legend', html:"Legend"},
								{tag:'input', type:'email',		name:'name',		/*value:'joe',* /		placeholder:"username",	  id:this.getId('usernameField')   },
//								{tag:'span', cls:'help-block', html:"Example of help text here"},
								{tag:'input', type:'password',	name:'passphrase',	/*value:'clipperz',* /	placeholder:"passphrase", id:this.getId('passphraseField') },
							]},

							{tag:'button', cls:'btn btn-primary btn-large', type:'submit', id:this.getId('credentialsSubmitButton'), html:"Login"}
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
*/
	'renderSelf': function() {
		if (this.isFullyRendered() == false) {
			this.append(this.element(), //	[
//				{tag:'div', 'data-role':'header', children:[
//					{tag:'h1', html:'clipperz'}
//				]},
//				{tag:'div', 'data-role':'content', children:[
					{tag:'form', id:this.getId('credentialsForm'), children:[
						{tag:'div', 'data-role':'fieldcontain', cls:'ui-hide-label', children:[
							{tag:'label', 'for':'name', cls:'ui-input-text', html:"username"},
							{tag:'input', type:'email',	name:'name',		/*value:'joe',*/		placeholder:"username",	  id:this.getId('usernameField')   },
							{tag:'label', 'for':'passphrase', cls:'ui-input-text', html:"passphrase"},
							{tag:'input', type:'password',	name:'passphrase',	/*value:'clipperz',*/	placeholder:"passphrase", id:this.getId('passphraseField') }
						]},
						{tag:'button', type:'submit', id:this.getId('credentialsSubmitButton'), html:"login"}
					]}
//				]}
//			]
			);

			MochiKit.Signal.connect(this.getElement('credentialsForm'),			'onsubmit',		this, 'submitCredentialsHandler');
			MochiKit.Signal.connect(this.getElement('credentialsSubmitButton'),	'onclick',		this, 'submitCredentialsHandler');
		}
	}, 

	//-------------------------------------------------------------------------

	'submitPinHandler': function (anEvent) {
		var	pin;

		this.setMessage(null);
		pin = this.getElement('PIN').value;
		$(this.getAnchor('PIN')).blur();
//		this.getElement('PIN').blur();

		credentials = Clipperz.PM.PIN.credentialsWithPIN(pin);
		this.loginWithCredentials(credentials);
	},

	'submitCredentialsHandler': function (anEvent) {
		var	credentials;

		anEvent.preventDefault();

//		this.setMessage(null);
		$(this.getAnchor('usernameField')).blur();
		$(this.getAnchor('passphraseField')).blur();
		$(this.getAnchor('credentialsSubmitButton')).button('disable');

		credentials = {};
		credentials['username'] = this.getElement('usernameField').value;
		credentials['passphrase'] = this.getElement('passphraseField').value;

		this.loginWithCredentials(credentials);
	},

	//-------------------------------------------------------------------------

	'loginWithCredentials': function (someCredentials) {
		var	args;

		args = {};
		args['credentials'] = someCredentials;
		args['errorCallback'] = this.errorCallback();

//		MochiKit.Style.hideElement(this.getElement('credentialsBody'));
//		MochiKit.Style.showElement(this.getElement('validating'));

		MochiKit.Async.callLater(0.1, this.callback(), args);
	},

	//-------------------------------------------------------------------------

	'initProgressHandle': function (anEvent) {
		this._steps = anEvent['steps'];
		this._actualSteps = 0;
	},

	'updateProgressHandle': function (anEvent) {
		this._steps += anEvent['extraSteps'];
	},

	'advanceProgressHandle': function (anEvent) {
		this._actualSteps ++;
	},

	'progressDoneHandle': function (anEvent) {
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
