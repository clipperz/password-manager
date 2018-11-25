/*

Copyright 2008-2018 Clipperz Srl

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
Clipperz.Base.module('Clipperz.PM.UI.Components.Pages');

Clipperz.PM.UI.Components.Pages.LoginPageClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.Pages.LoginPage',

	propTypes: {
		mode:							React.PropTypes.oneOf(['CREDENTIALS','PIN']).isRequired,
		isNewUserRegistrationAvailable:	React.PropTypes.bool.isRequired,
		disabled:						React.PropTypes.bool.isRequired
	},
/*
	getDefaultProps: function () {
		return {
			mode: 'CREDENTIALS',
			isNewUserRegistrationAvailable: false,
			disabled: false,
//			template: Clipperz.PM.UI.Components.PageTemplate
		}
	},
*/
	getInitialState: function () {
		return {
			username: '',
			passphrase: '',
			pin: '',
		};
	},

	//=========================================================================

	mode: function() {
		return (this.props['mode'] == 'CREDENTIALS' || this.props['forceCredentials']) ? 'CREDENTIALS' : 'PIN';
	},

	//=========================================================================

	handleChange: function (anEvent) {
		var	refs = this.refs;
		var refName = MochiKit.Base.filter(function (aRefName) { return refs[aRefName] == anEvent.target}, MochiKit.Base.keys(this.refs))[0];
		var newState = {};

		newState[refName] = anEvent.target.value;
		this.setState(newState);
	},

	pollForChanges: function() {
		if (this.mode() == 'CREDENTIALS') {
			var newState;

			var usernameValue = this.refs['username'].value;
			var passphraseValue = this.refs['passphrase'].value;

			newState = {};

			newState['username'] = (usernameValue) ? usernameValue : "";
			newState['passphrase'] = (passphraseValue) ? passphraseValue : "";

			this.setState(newState);
		}
	},

	//=========================================================================

	handleCredentialSubmit: function (event) {
		event.preventDefault();

		this.refs['passphrase'].blur();

		var credentials = {
			'username': this.refs['username'].value,
			'passphrase': this.refs['passphrase'].value
		}
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'doLogin', credentials);
	},

	handleRegistrationLinkClick: function (event) {
		event.preventDefault();
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'showRegistrationForm');
	},

	//-------------------------------------------------------------------------

	shouldEnableLoginButton: function () {
		var result;

		return	(
					((this.state['username'] != '') && (this.state['passphrase'] != '')) 
//					||
//					(this.state['pin'] != '')
				)
				&&
				!this.props['disabled'];
	},

	loginForm: function () {
		return	React.DOM.form({'key':'loginForm', 'className':'loginForm credentials', 'autoComplete':'off', 'autoCorrect':'off', 'autoCapitalize':'off', 'onChange':this.handleChange, 'onSubmit':this.handleCredentialSubmit}, [
					React.DOM.div({'key':'fields'},[
						React.DOM.label({'key':'username-label', 'htmlFor' :'name'}, "username"),
						React.DOM.input({'key':'username', 'type':'text', 'name':'name', 'ref':'username', 'placeholder':"username", 'autoComplete':'off', 'autoCorrect':'off', 'autoCapitalize':'off', 'spellCheck': false}),
						React.DOM.label({'key':'passphrase-label', 'htmlFor' :'passphrase'}, "passphrase"),
						React.DOM.input({'key':'passphrase', 'type':'password', 'name':'passphrase', 'ref':'passphrase', 'placeholder':"passphrase"})
					]),
					React.DOM.button({'key':'button', 'type':'submit', 'disabled':!this.shouldEnableLoginButton(), 'className':'button'}, "login")
				]);
	},

	submitPIN: function (event) {
		this.refs['pin'].blur();

		var credentials = {
			pin: this.refs['pin'].value
		}

		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'doLogin', credentials);
	},

	forcePassphraseLogin: function() {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'forcePassphraseLogin');
	},

	handlePinChange: function(anEvent) {
		if (anEvent.target.value.length == Clipperz.PM.PIN.DEFAULT_PIN_LENGTH) {
			this.submitPIN();
		}

		this.setState({
			'pin': anEvent.target.value
		})
	},

	// handlePinFocus: function(anEvent) {
	// 	// anEvent.preventDefault();
	// 	this.refs['pin'].focus();
	// },

	// pinFormDigits: function() {
	// 	var i;
	// 	var result;

	// 	result = [];
	// 	for (i = 0; i<Clipperz.PM.PIN.DEFAULT_PIN_LENGTH; i++) {
	// 		result.push(React.DOM.input({
	// 			'key': 'pin-digit-'+i,
	// 			'ref': 'pin-digit-'+i,
	// 			'name': 'pin-digit-'+i,
	// 			'className': 'pinDigit',
	// 			'readOnly': true,
	// 			'type': 'text',
	// 			'value': this.state['pin'][i],
	// 			'onFocus': this.handlePinFocus,
	// 		}));
	// 	}

	// 	return result;
	// },

	pinForm: function () {
		return	React.DOM.form({
			'key':'pinForm',
			'className':'pinForm pin',
			'autoComplete':'off',
			'onSubmit': function(anEvent) {anEvent.preventDefault();},
		}, [
				React.DOM.div({'key':'pinFormDiv'},[
					React.DOM.label({'key':'pinLabel', 'htmlFor':'pin'}, "Enter your PIN"),
					React.DOM.input({
						'type':'tel',
						'name':'pin',
						'ref':'pin',
						'id': 'pinValue',
						'className': 'pinValue',
						'placeholder':"PIN",
						'key':'pin',
						'autoCapitalize':'none',
						'value': this.state['pin'],
						'onChange': this.handlePinChange,
					}),
					// React.DOM.div({'className': 'pinContainer'}, this.pinFormDigits()),
					React.DOM.a({
						'key':'pinAnchor',
						'className': 'passphraseLogin',
						'onClick': this.forcePassphraseLogin,
					}, "Login with passphrase")
				]),
				// React.DOM.button({'key':'submitButton', 'type':'submit', 'disabled':this.props.disabled, 'className':'button'}, "login")
			]);
	},

	setInitialFocus: function () {
		if (this.mode() == 'PIN') {
			this.setState({
				'pin': ''
			})
			this.refs['pin'].focus();
		} else {
			if (this.refs['username'].value == '') {
				this.refs['username'].focus();
			} else{
				this.refs['passphrase'].select();
			}
		}
	},

	showUrl: function (anUrl) {
		return function () {
			window.open(anUrl, 'clipperz_about');
		}
	},

	render: function() {
		var	registrationLink = React.DOM.a({'key':'signup', 'className':'registrationLink', 'onClick':this.handleRegistrationLinkClick}, "sign up");

		return React.DOM.div({'key':'loginForm', 'className':'loginForm content ' + this.props['style']}, [
			Clipperz.PM.UI.Components.AccountStatus(MochiKit.Base.update(this.props['proxyInfo'])),
			React.DOM.header({'key':'header'}, [
				React.DOM.div({'key':'div', 'className':'headerContent'}, [
					React.DOM.h3({'key':'h3'}, 'clipperz'),
					React.DOM.h5({'key':'h5'}, 'keep it to yourself'),
				]),
			]),
			React.DOM.div({'key':'formWrapper', 'className':'form body'}, [
				React.DOM.div({'key':'div', 'className':'bodyContent'}, [
					this.mode() == 'PIN' ? this.pinForm() : this.loginForm(),
					this.props['isNewUserRegistrationAvailable'] ? registrationLink : null,
				]),
			]),
			React.DOM.div({'key':'afterBody', 'className':'afterBody'}),
			React.DOM.div({'key':'other', 'className':'other'}, [
				React.DOM.div({'key':'otherContent', 'className':'otherContent'}, [
					React.DOM.div({'key':'links', 'className':'links'}, [
						React.DOM.ul({'key':'ul'}, [
							React.DOM.li({'key':'about',   'onClick':this.showUrl('/about/')}, "About"),
							React.DOM.li({'key':'terms',   'onClick':this.showUrl('/terms_service/')}, "Terms of service"),
							React.DOM.li({'key':'privacy', 'onClick':this.showUrl('/privacy_policy/')}, "Privacy"),
						])
					]),
				])
			]),
			React.DOM.footer({'key':'footer'}, [
				React.DOM.div({'key':'div', 'className':'footerContent'}, [
					React.DOM.div({'key':'applicationVersion', 'className':'applicationVersion'}, [
						React.DOM.span({'key':'applicationVersionLabel'}, "application version"),
						React.DOM.a({'key':'applicationVersionLink', 'href':'https://github.com/clipperz/password-manager/commit/' + Clipperz_version, 'target':'github'}, Clipperz_version)
					])
				])
			])
		]);
	}
});

Clipperz.PM.UI.Components.Pages.LoginPage = React.createFactory(Clipperz.PM.UI.Components.Pages.LoginPageClass);
