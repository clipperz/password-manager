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
Clipperz.Base.module('Clipperz.PM.UI.Components.ExtraFeatures');

Clipperz.PM.UI.Components.ExtraFeatures.PassphraseClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.Passphrase',

	propTypes: {
	},
	
	getInitialState: function() {
		return {
			'username': 'empty',
			'old-passphrase': 'empty',
			'new-passphrase': 'empty',
			'confirm-new-passphrase': 'empty',
			'confirm': '',
		};
	},

	//=========================================================================

	resetForm: function () {
		this.setState(this.getInitialState());

		this.refs['username'].value = '';
		this.refs['old-passphrase'].value = '';
		this.refs['new-passphrase'].value = '';
		this.refs['confirm-new-passphrase'].value = '';
		this.refs['confirm'].checked = false;
	},

	handleChangePassphrase: function(event) {
		var	newPassphrase;
		
		event.preventDefault();
		newPassphrase = this.refs['new-passphrase'].value;
		this.resetForm();

		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'changePassphrase', newPassphrase);
	},

	handleFormChange: function() {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("Passphrase.handleFormChange", {trace: false});
		deferredResult.addCallback(this.props.userInfo['checkPassphraseCallback'], this.refs['old-passphrase'].value);
		deferredResult.addMethod(this, function(passCheck){
			var username = this.refs['username'].value;
			var oldPassphrase = this.refs['old-passphrase'].value;
			var newPassphrase = this.refs['new-passphrase'].value;
			var confirmNewPassphrase = this.refs['confirm-new-passphrase'].value;
			
			this.setState({
				'username': (username != '') ? [(username == this.props.userInfo['username']) ? 'valid' : 'invalid'] : 'empty',
				'old-passphrase': (oldPassphrase != '') ? [(passCheck) ? 'valid' : 'invalid'] : 'empty',
				'new-passphrase': (newPassphrase != '') ? 'valid' : 'empty',
				'confirm-new-passphrase': (confirmNewPassphrase != '') ? [(confirmNewPassphrase == newPassphrase) ? 'valid' : 'invalid'] : 'empty',
				'confirm': this.refs['confirm'].checked,
			});
		});
		
		deferredResult.callback();
		
		return deferredResult;
	},
	
	shouldEnableChangePassphraseButton: function() {
		return (
			this.state['username'] == 'valid' &&
			this.state['old-passphrase'] == 'valid' &&
			this.state['new-passphrase'] == 'valid' &&
			this.state['confirm-new-passphrase'] == 'valid' &&
			this.state['confirm']
		);
	},

	//=========================================================================

	render: function () {
		return	React.DOM.div({className:'extraFeature passphrase'}, [
			React.DOM.div({'className':'header'}, [
				React.DOM.h1({}, "Change passphrase"),
			]),
			React.DOM.div({'className': 'content'}, [
				React.DOM.form({'key':'form', 'className':'changePassphraseForm', 'onChange': this.handleFormChange, 'onSubmit':this.handleChangePassphrase}, [
					React.DOM.div({'key':'fields'},[
						React.DOM.label({'key':'username-label', 'htmlFor' :'name'}, "username"),
						React.DOM.input({'key':'username', 'className':this.state['username'], 'type':'text', 'name':'name', 'ref':'username', 'placeholder':"username", 'autoCapitalize':'none'}),

						React.DOM.label({'key':'old-passphrase-label', 'htmlFor' :'old-passphrase'}, "old passphrase"),
						React.DOM.input({'key':'old-passphrase', 'className':this.state['old-passphrase'], 'type':'password', 'name':'old-passphrase', 'ref':'old-passphrase', 'placeholder':"old passphrase"}),

						React.DOM.label({'key':'new-passphrase-label', 'autoFocus': 'true', 'htmlFor' :'new-passphrase'}, "new passphrase"),
						React.DOM.input({'key':'new-passphrase', 'className':this.state['new-passphrase'], 'type':'password', 'name':'new-passphrase', 'ref':'new-passphrase', 'placeholder':"new passphrase"}),

						React.DOM.label({'key':'confirm-new-passphrase-label', 'htmlFor' :'confirm-new-passphrase'}, "confirm new passphrase"),
						React.DOM.input({'key':'confirm-new-passphrase', 'className':this.state['confirm-new-passphrase'], 'type':'password', 'name':'confirm-new-passphrase', 'ref':'confirm-new-passphrase', 'placeholder':"confirm new passphrase"}),

						React.DOM.p({}, [
							React.DOM.input({'key':'confirm', 'id':'changePassphraseConfirmCheckbox', 'className':'confirmCheckbox', 'type':'checkbox', 'name':'confirm', 'ref':'confirm'}),
							React.DOM.label({'htmlFor':'changePassphraseConfirmCheckbox'}, "I understand that Clipperz is unable to recover a lost passphrase.")
						]),
					]),
					React.DOM.button({'key':'button', 'type':'submit', 'disabled':!this.shouldEnableChangePassphraseButton(), 'className':'button'}, "Change passphrase"),
				])
			])
		]);
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.ExtraFeatures.Passphrase = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.PassphraseClass);
