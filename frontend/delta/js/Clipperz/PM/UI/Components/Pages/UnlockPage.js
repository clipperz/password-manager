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

"use strict";
Clipperz.Base.module('Clipperz.PM.UI.Components.Pages');

Clipperz.PM.UI.Components.Pages.UnlockPageClass = React.createClass({

	propTypes: {
		mode:							React.PropTypes.oneOf(['CREDENTIALS','PIN']).isRequired,
		// isNewUserRegistrationAvailable:	React.PropTypes.bool.isRequired,
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
			passphrase: '',
			pin: ''
		};
	},

	//=========================================================================

	handleChange: function (anEvent) {
		var newState = {};

		newState['passphrase'] = anEvent.target.value;

	    this.setState(newState);
	},

	// pollForChanges: function() {
	// 	if (this.props.mode == 'CREDENTIALS') {
	// 		var newState;

	// 		var usernameValue = this.refs['username'].getDOMNode().value;
	// 		var passphraseValue = this.refs['passphrase'].getDOMNode().value;

	// 		newState = {};

	// 		newState['username'] = (usernameValue) ? usernameValue : "";
	// 		newState['passphrase'] = (passphraseValue) ? passphraseValue : "";

	// 		this.setState(newState);
	// 	}
	// },

	//=========================================================================

	handlePassphraseSubmit: function (event) {
		event.preventDefault();

		this.refs['passphrase'].getDOMNode().blur();

		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'unlock', this.refs['passphrase'].getDOMNode().value);

		
	},

	resetUnlockForm: function() {
		this.refs['passphrase'].getDOMNode().value = '';
		this.replaceState(this.getInitialState());
	},

	//-------------------------------------------------------------------------

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

	unlockForm: function () {
		return	React.DOM.form({'key':'form', 'className':'unlockForm credentials', 'autoComplete':'off', 'autoCorrect':'off', 'autoCapitalize':'off', 'onChange':this.handleChange, 'onSubmit':this.handlePassphraseSubmit}, [
					React.DOM.div({'key':'fields'},[
						React.DOM.label({'key':'passphrase-label', 'htmlFor' :'passphrase'}, "passphrase"),
						React.DOM.input({'key':'passphrase', 'type':'password', 'name':'passphrase', 'ref':'passphrase', 'placeholder':"passphrase"})
					]),
					React.DOM.button({'key':'button', 'type':'submit', 'disabled':!this.shouldEnableUnlockButton(), 'className':'button'}, "unlock")
				]);
	},

	// handlePINSubmit: function (event) {
	// 	event.preventDefault();

	// 	this.refs['pin'].getDOMNode().blur();

	// 	var credentials = {
	// 		pin: this.refs['pin'].getDOMNode().value
	// 	}

	// 	MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'doLogin', credentials);
	// },

	// pinForm: function () {
	// 	return	React.DOM.form({'className':'pinForm pin', 'autoComplete':'off', 'onChange':this.handleChange, 'onSubmit':this.handlePINSubmit}, [
	// 				React.DOM.div({'key':'pinFormDiv'},[
	// 					React.DOM.label({'for':'pin'}, "pin"),
	// 					React.DOM.input({'type':'text', 'name':'pin', 'ref':'pin', placeholder:"PIN", 'key':'pin', 'autocapitalize':'none'})
	// 				]),
	// 				React.DOM.button({'key':'submitButton', 'type':'submit', 'disabled':this.props.disabled, 'className':'button'}, "login")
	// 			]);
	// },

	setInitialFocus: function () {
		if (this.props.mode == 'PIN') {
			this.refs['pin'].getDOMNode().select();
		} else {
			this.refs['passphrase'].getDOMNode().select();
		}
	},

	// showUrl: function (anUrl) {
	// 	return function () {
	// 		window.open(anUrl, 'clipperz_about');
	// 	}
	// },

	render: function() {
//console.log("LOGIN PAGE", this.props);
		return React.DOM.div({'key':'unlockForm', 'className':'unlockForm ' + this.props['style']}, [
			Clipperz.PM.UI.Components.AccountStatus(MochiKit.Base.update(this.props['proxyInfo'])),
			React.DOM.header({'key':'header'}, [
				React.DOM.h3({}, 'Type your passphrase to unlock'),
			]),
			React.DOM.div({'key':'formWrapper', 'className':'form'}, [
				this.props.mode == 'PIN' ? this.pinForm() : this.unlockForm(),
			]),
			React.DOM.footer({'key':'footer'}, [
				React.DOM.div({'key':'applicationVersion', 'className':'applicationVersion'}, [
					React.DOM.span({'key':'applicationVersionLabel'}, "application version"),
					React.DOM.a({'key':'applicationVersionLink', 'href':'https://github.com/clipperz/password-manager/commit/' + Clipperz_version, 'target':'github'}, Clipperz_version)
				])
			])
		]);
	}
});

Clipperz.PM.UI.Components.Pages.UnlockPage = React.createFactory(Clipperz.PM.UI.Components.Pages.UnlockPageClass);
