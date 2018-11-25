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

Clipperz.PM.UI.Components.Pages.UnlockPageClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.Pages.UnlockPage',

	propTypes: {
//		username:	React.PropTypes.string.isRequired,
		mode:		React.PropTypes.oneOf(['CREDENTIALS','PIN']).isRequired,
//		disabled:	React.PropTypes.bool.isRequired,
	},

	getInitialState: function () {
		return {
			passphrase: '',
			pin: ''
		};
	},

	//=========================================================================

	mode: function() {
		return (this.props['mode'] == 'CREDENTIALS' || this.props['forceCredentials']) ? 'CREDENTIALS' : 'PIN';
	},

	//=========================================================================

	handleChange: function (anEvent) {
		var newState = {};

		newState['passphrase'] = anEvent.target.value;

		this.setState(newState);
	},

	handlePinChange: function(anEvent) {
		if (anEvent.target.value.length == this.props['PIN'].DEFAULT_PIN_LENGTH) {
			this.submitPIN();
		}

		this.setState({
			'pin': anEvent.target.value
		})
	},

	//=========================================================================

	handlePassphraseSubmit: function (event) {
		event.preventDefault();

		this.refs['passphrase'].blur();
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'unlock', {'credential':this.refs['passphrase'].value, 'credentialType':'PASSPHRASE'});
	
		// this.resetUnlockForm();
	},

	submitPIN: function() {
		this.refs['pin'].blur();

		var pin = this.refs['pin'].value;

		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'unlock', {'credential':pin, 'credentialType':'PIN'});

		// this.resetUnlockForm();
	},

	resetUnlockForm: function() {
		if (this.mode() == 'CREDENTIALS') {
			this.refs['passphrase'].value = '';
			this.refs['passphrase'].blur();
		} else if (this.mode() == 'PIN') {
			this.refs['pin'].value = '';
			this.refs['pin'].blur();
		}
	},

	forcePassphraseUnlock: function() {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'forcePassphraseUnlock');
	},

	//-------------------------------------------------------------------------

	setInitialFocus: function () {
		if (this.mode() == 'PIN') {
			this.refs['pin'].focus();
		} else {
			this.refs['passphrase'].focus();
		}
	},

	showUrl: function (anUrl) {
		return function () {
			window.open(anUrl, 'clipperz_about');
		}
	},

	loginForm: function () {
		return	React.DOM.form({'key':'form', 'className':'loginForm credentials', 'autoComplete':'off', 'autoCorrect':'off', 'autoCapitalize':'off', 'onChange':this.handleChange, 'onSubmit':this.handlePassphraseSubmit}, [
					React.DOM.div({'key':'fields'},[
						React.DOM.label({'key':'username-label', 'htmlFor' :'name'}, "username"),
						React.DOM.input({'key':'username', 'type':'text', 'name':'name', 'ref':'username', 'placeholder':"username", 'autoComplete':'off', 'autoCorrect':'off', 'autoCapitalize':'off', 'spellCheck': false, 'value':this.props['username'], 'disabled':true}),
						React.DOM.label({'key':'passphrase-label', 'htmlFor' :'passphrase'}, "passphrase"),
						React.DOM.input({'key':'passphrase', 'type':'password', 'name':'passphrase', 'ref':'passphrase', 'placeholder':"passphrase"})
					]),
					React.DOM.button({'key':'button', 'type':'submit', 'disabled':!this.shouldEnableUnlockButton(), 'className':'button'}, "unlock")
				]);
	},

	pinForm: function () {
		return	React.DOM.form({
			'className':'pinForm pin',
			'autoComplete':'off',
		}, [
				React.DOM.div({'key':'pinFormDiv'},[
					React.DOM.label({'htmlFor':'pin'}, "Enter your PIN"),
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
						'className': 'passphraseLogin',
						'onClick': this.forcePassphraseUnlock,
					}, "Unlock with passphrase")
				]),
				// React.DOM.button({'key':'submitButton', 'type':'submit', 'disabled':this.props.disabled, 'className':'button'}, "login")
			]);
	},

	shouldEnableUnlockButton: function () {
		var result;

		return	(
					(this.state['passphrase'] != '')
					||
					(this.state['pin'] != '')
				)
				&&
				!this.props['disabled'];
	},

	// componentDidUpdate: function() {
	// 	this.setInitialFocus();
	// },

	render: function() {
		return React.DOM.div({'key':'unlockForm', 'className':'unlockForm content ' + this.props['style']}, [
			Clipperz.PM.UI.Components.AccountStatus(MochiKit.Base.update(this.props['proxyInfo'])),
			React.DOM.header({'key':'header'}, [
				React.DOM.div({'className':'headerContent'}, [
					React.DOM.h3({}, 'clipperz'),
					React.DOM.h5({}, 'keep it to yourself'),
				]),
			]),
			React.DOM.div({'key':'formWrapper', 'className':'form body'}, [
				React.DOM.div({'className':'bodyContent'}, [
					this.mode() == 'PIN' ? this.pinForm() : this.loginForm(),
				]),
			]),
			React.DOM.div({'key':'afterBody', 'className':'afterBody'}),
			React.DOM.div({'className':'other', 'key':'other'}, [
				React.DOM.div({'className':'otherContent'}, [
					React.DOM.div({'key':'links', 'className':'links'}, [
						React.DOM.ul({}, [
							React.DOM.li({'key':'about',   'onClick':this.showUrl('/about/')}, "About"),
							React.DOM.li({'key':'terms',   'onClick':this.showUrl('/terms_service/')}, "Terms of service"),
							React.DOM.li({'key':'privacy', 'onClick':this.showUrl('/privacy_policy/')}, "Privacy"),
						])
					]),
				])
			]),
			React.DOM.footer({'key':'footer'}, [
				React.DOM.div({'className':'footerContent'}, [
					React.DOM.div({'key':'applicationVersion', 'className':'applicationVersion'}, [
						React.DOM.span({'key':'applicationVersionLabel'}, "application version"),
						React.DOM.a({'key':'applicationVersionLink', 'href':'https://github.com/clipperz/password-manager/commit/' + Clipperz_version, 'target':'github'}, Clipperz_version)
					])
				])
			])
		]);
	}
});

Clipperz.PM.UI.Components.Pages.UnlockPage = React.createFactory(Clipperz.PM.UI.Components.Pages.UnlockPageClass);
