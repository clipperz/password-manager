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
		username:	React.PropTypes.string.isRequired,
		mode:		React.PropTypes.oneOf(['CREDENTIALS','PIN']).isRequired,
		disabled:	React.PropTypes.bool.isRequired,
	},

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

	setInitialFocus: function () {
		if (this.props.mode == 'PIN') {
			this.refs['pin'].getDOMNode().select();
		} else {
			this.refs['passphrase'].getDOMNode().select();
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
					this.props.mode == 'PIN' ? this.pinForm() : this.loginForm(),
				]),
			]),
			React.DOM.footer({'key':'footer'}, [
				React.DOM.div({'className':'footerContent'}, [
					React.DOM.div({'key':'links', 'className':'links'}, [
						React.DOM.ul({}, [
							React.DOM.li({'key':'about',   'onClick':this.showUrl('/about/')}, "About"),
							React.DOM.li({'key':'terms',   'onClick':this.showUrl('/terms_service/')}, "Terms of service"),
							React.DOM.li({'key':'privacy', 'onClick':this.showUrl('/privacy_policy/')}, "Privacy"),
						])
					]),
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
