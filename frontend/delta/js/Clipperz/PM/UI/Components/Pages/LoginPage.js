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

Clipperz.PM.UI.Components.Pages.LoginPageClass = React.createClass({

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
			pin: ''
		};
	},

	//=========================================================================

	handleChange: function (anEvent) {
		var	refs = this.refs;
		var refName = MochiKit.Base.filter(function (aRefName) { return refs[aRefName].getDOMNode() == anEvent.target}, MochiKit.Base.keys(this.refs))[0];
		var newState = {};

		newState[refName] = anEvent.target.value;
	    this.setState(newState);
	},

	//=========================================================================

	handleCredentialSubmit: function (event) {
		event.preventDefault();

		this.refs['passphrase'].getDOMNode().blur();

		var credentials = {
			'username': this.refs['username'].getDOMNode().value,
			'passphrase': this.refs['passphrase'].getDOMNode().value
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
					||
					(this.state['pin'] != '')
				)
				&&
				!this.props['disabled'];
	},

	loginForm: function () {
		return	React.DOM.form({'key':'form', 'className':'loginForm credentials', 'autoComplete':'off', 'onChange':this.handleChange, 'onSubmit':this.handleCredentialSubmit}, [
					React.DOM.div({'key':'fields'},[
						React.DOM.label({'key':'username-label', 'htmlFor' :'name'}, "username"),
						React.DOM.input({'key':'username', 'type':'text', 'name':'name', 'ref':'username', 'placeholder':"username", 'autoCapitalize':'none'}),
						React.DOM.label({'key':'passphrase-label', 'htmlFor' :'passphrase'}, "passphrase"),
						React.DOM.input({'key':'passphrase', 'type':'password', 'name':'passphrase', 'ref':'passphrase', 'placeholder':"passphrase"})
					]),
					React.DOM.button({'key':'button', 'type':'submit', 'disabled':!this.shouldEnableLoginButton(), 'className':'button'}, "login")
				]);
	},

	handlePINSubmit: function (event) {
		event.preventDefault();

		this.refs['pin'].getDOMNode().blur();

		var credentials = {
			pin: this.refs['pin'].getDOMNode().value
		}

		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'doLogin', credentials);
	},

	pinForm: function () {
		return	React.DOM.form({'className':'pinForm pin', 'autoComplete':'off', 'onChange':this.handleChange, 'onSubmit':this.handlePINSubmit}, [
					React.DOM.div({'key':'pinFormDiv'},[
						React.DOM.label({'for':'pin'}, "pin"),
						React.DOM.input({'type':'text', 'name':'pin', 'ref':'pin', placeholder:"PIN", 'key':'pin', 'autocapitalize':'none'})
					]),
					React.DOM.button({'key':'submitButton', 'type':'submit', 'disabled':this.props.disabled, 'className':'button'}, "login")
				]);
	},

	setInitialFocus: function () {
		if (this.props.mode == 'PIN') {
			this.refs['pin'].getDOMNode().select();
		} else {
			if (this.refs['username'].getDOMNode().value == '') {
				this.refs['username'].getDOMNode().focus();
			} else{
				this.refs['passphrase'].getDOMNode().select();
			}
		}
	},

	render: function() {
//console.log("LOGIN PAGE", this.props);
//		var registrationLink =	React.DOM.footer({'key':'registrationLink', 'className':'registrationLink'}, [
//									React.DOM.a({'key':'signup', 'onClick':this.handleRegistrationLinkClick}, "Sign up")
//								]);

		var	registrationLink = React.DOM.a({'key':'signup', 'className':'registrationLink', 'onClick':this.handleRegistrationLinkClick}, "Sign up");

		return React.DOM.div({'key':'loginForm', 'className':'loginForm ' + this.props['style']}, [
			React.DOM.header({'key':'header'}, 'clipperz'),
			React.DOM.div({'key':'formWrapper', 'className':'form'}, [
				this.props.mode == 'PIN' ? this.pinForm() : this.loginForm(),
			]),
			React.DOM.footer({'key':'footer'}, [
				this.props['isNewUserRegistrationAvailable'] ? registrationLink : null,
				React.DOM.div({'key':'applicationVersion', 'className':'applicationVersion'}, [
					React.DOM.span({'key':'applicationVersionLabel'}, "application version"),
					React.DOM.a({'key':'applicationVersionLink', 'href':'https://github.com/clipperz/password-manager/commit/' + Clipperz_version, 'target':'github'}, Clipperz_version)
				])
			])
//			this.props['isNewUserRegistrationAvailable'] ? registrationLink : null
		]);
	}
});

Clipperz.PM.UI.Components.Pages.LoginPage = React.createFactory(Clipperz.PM.UI.Components.Pages.LoginPageClass);
