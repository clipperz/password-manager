/*

Copyright 2008-2013 Clipperz Srl

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

Clipperz.PM.UI.Components.LoginForm = React.createClass({

	getDefaultProps: function () {
		return {
			mode: 'CREDENTIALS',
			isNewUserRegistrationAvailable: false,
			disabled: false,
			template: Clipperz.PM.UI.Components.PageTemplate
		}
	},

	propTypes: {
		mode:							React.PropTypes.oneOf(['CREDENTIALS','PIN']),
		isNewUserRegistrationAvailable:	React.PropTypes.bool,
		disabled:						React.PropTypes.bool,
		template:						React.PropTypes.func
	},

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

		newState[refName] = event.target.value;
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
				) && !this.props['disabled'];
	},


	loginForm: function () {
		registrationLink =	React.DOM.div({'className':'registrationLink'}, [
								React.DOM.a({'onClick':this.handleRegistrationLinkClick}, "Sign up")
							]);
		return	React.DOM.div({'className':'loginForm credentials'},[
					React.DOM.form({onChange: this.handleChange, onSubmit:this.handleCredentialSubmit}, [
						React.DOM.div(null,[
							React.DOM.label({'for' :'name'}, "username"),
							React.DOM.input({'type':'text', 'name':'name', 'ref':'username', 'placeholder':"username", 'key':'username', 'autoCapitalize':'none'}),
							React.DOM.label({'for' :'passphrase'}, "passphrase"),
							React.DOM.input({'type':'password', 'name':'passphrase', 'ref':'passphrase', 'placeholder':"passphrase", 'key':'passphrase'})
						]),
						React.DOM.button({'type':'submit', 'disabled':!this.shouldEnableLoginButton(), 'className':'button'}, "login")
					]),
					this.props.isNewUserRegistrationAvailable ? registrationLink : null
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
		return	React.DOM.div({'className':'loginForm pin'},[
					React.DOM.form({onChange: this.handleChange, onSubmit:this.handlePINSubmit}, [
						React.DOM.div(null,[
							React.DOM.label({'for':'pin'}, "pin"),
							React.DOM.input({'type':'text', 'name':'pin', 'ref':'pin', placeholder:"PIN", 'key':'pin', 'autocapitalize':'none'})
						]),
						React.DOM.button({'type':'submit', 'disabled':this.props.disabled, 'className':'button'}, "login")
					])
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
		return	new this.props.template({'innerComponent': this.props.mode == 'PIN' ? this.pinForm() : this.loginForm()});
	}
});
