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

Clipperz.Base.module('Clipperz.PM.UI.Web.Components');

Clipperz.PM.UI.Web.Components.LoginForm = function(args) {
	args = args || {};

	this._autocomplete = args.autocomplete || 'off';

	Clipperz.PM.UI.Web.Components.LoginForm.superclass.constructor.apply(this, arguments);

	this._slots = {
		'passphraseEntropy':	this.getId('passphraseEntropy')
	};

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.LoginForm, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.LoginForm component";
	},

	'autocomplete': function () {
		return this._autocomplete;
	},

	//-------------------------------------------------------------------------

	'renderSelf': function() {
		this.append(this.element(), {tag:'div', id:'loginBox', children:[
			{tag:'div', cls:'header'},
			{tag:'div', cls:'body', id:this.getId('body'), children:[
				{tag:'div', id:this.getId('loginForm'), children:[
					{tag:'div', children:[
						{tag:'h4', html:'Login'},
//						{tag:'form', cls:'loginForm', autocomplete:this.autocomplete(), children:[
						{tag:'form', id:this.getId('form'), cls:'loginForm', children:[
							{tag:'label', html:'username', 'for':this.getId('usernameField')},
							{tag:'input', id:this.getId('usernameField'), type:'text', cls:'username'/*, value:'joe'*/},
							{tag:'ul', id:this.getId('passwordOptions'), children:[
								{tag:'li', id:this.getId('passphraseOption'), children:[
									{tag:'label', html:'passphrase / OTP', 'for':this.getId('passphraseField')},
									{tag:'input', id:this.getId('passphraseField'), type:'password', cls:'password'/*, value:'clipperz'*/}
								]}	//	,
/*
								{tag:'li', id:this.getId('otpOption'), children:[
									{tag:'label', html:'one-time password', 'for':this.getId('otpField_1')},
									{tag:'input', id:this.getId('otpField_1'), type:'text', cls:'otp', value:'abcd-efgh'},
									{tag:'input', id:this.getId('otpField_2'), type:'text', cls:'otp', value:'abcd-efgh'},
									{tag:'input', id:this.getId('otpField_3'), type:'text', cls:'otp', value:'abcd-efgh'},
									{tag:'input', id:this.getId('otpField_4'), type:'text', cls:'otp', value:'abcd-efgh'}
								]}
*/
							]},
//							{tag:'input', id:this.getId('otpCheckbox'), type:'checkbox', cls:'checkbox'},
//							{tag:'label', html:'use a one-time passphrase', 'for':this.getId('otpCheckbox'), cls:'checkbox'},

							{tag:'div', cls:'translations', children:[
								{tag:'h4', html:'choose your language'},
								{tag:'ul', children:[
									{tag:'li', cls:'selected', html:'english'},
									{tag:'li', html:'italiano'},
									{tag:'li', html:'dutch'},
									{tag:'li', html:'french'},
									{tag:'li', html:'spanish'},
									{tag:'li', html:'chinese'},
									{tag:'li', html:'japanese'},
									{tag:'li', html:'portugal'},
									{tag:'li', html:'arabic'}
								]}
							]},

							{tag:'input', id:this.getId('submitButton'), type:'submit', value:'login', cls:'submit'}
						]}
					]}
				]}
			]},
			{tag:'div', cls:'footer'}
		]});

		if (this.autocomplete() == 'off') {
			MochiKit.DOM.updateNodeAttributes(this.getElement('form'), {autocomplete:'off'});
		}

//		Clipperz.Style.setBackgroundGradient(this.getElement('body'), {from:"#ff9955", to:"#ff6622"})
		
//		this.setEntropyDisplay(new Clipperz.PM.UI.Common.Components.PasswordEntropyDisplay(this.getElement('passphraseField')));

//		MochiKit.Signal.connect(this.getId('otpCheckbox'), 'onclick', this, 'togglePasswordFields');
//		this.showPassphraseField();
		
		this.getElement('usernameField').focus();

		MochiKit.Signal.connect(this.getElement('loginForm'), 'onsubmit', this, 'loginEventHandler');
	},

	//-----------------------------------------------------------------------------
/*
	'togglePasswordFields': function(anEvent) {
		var shouldUseOTP;
		
		shouldUseOTP = this.getElement('otpCheckbox').checked;
		
		if (shouldUseOTP == false) {
			this.showPassphraseField();
		} else {
			this.showOTPFields();
		}
	},
*/
	//-----------------------------------------------------------------------------
/*
	'showPassphraseField': function() {
		this.showElement('passphraseOption');
		this.hideElement('otpOption');
	},
*/
	//-----------------------------------------------------------------------------

	'focusOnPassphraseField': function () {
		this.getElement('passphraseField').focus();
		this.getElement('passphraseField').select();
	},

	//-----------------------------------------------------------------------------
/*
	'showOTPFields': function() {
		this.hideElement('passphraseOption');
		this.showElement('otpOption');
	},
*/
	//-------------------------------------------------------------------------
	
	'loginEventHandler': function(anEvent) {
		var	username;
		var passphrase;
//		var shouldUseOTP;
//		var otp;
		var signalArguments;

		anEvent.preventDefault();

		username = this.getElement('usernameField').value;
		passphrase = this.getElement('passphraseField').value;
//		otp =	this.getElement('otpField_1').value +
//				this.getElement('otpField_2').value +
//				this.getElement('otpField_3').value +
//				this.getElement('otpField_4').value;
//		shouldUseOTP = this.getElement('otpCheckbox').checked;

		signalArguments = {username:username};

//		if (shouldUseOTP) {
//			signalArguments.otp = otp;
//		} else {
			signalArguments.passphrase = passphrase;
//		}

		MochiKit.Signal.signal(this, 'doLogin', signalArguments);
	},

	//-------------------------------------------------------------------------

	'submitButtonElement': function() {
		return this.getElement('submitButton');
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});
