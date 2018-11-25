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
Clipperz.Base.module('Clipperz.PM.UI.Components.DeleteAccount');

Clipperz.PM.UI.Components.ExtraFeatures.DeleteAccountClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.DeleteAccount',

	propTypes: {
//		featureSet:			React.PropTypes.oneOf(['FULL', 'EXPIRED', 'TRIAL']).isRequired,
//		'level':	React.PropTypes.oneOf(['hide', 'info', 'warning', 'error']).isRequired
	},
	
	getInitialState: function() {
		return {
			'username': 'empty',
			'passphrase': 'empty',
			'confirm': '',
		};
	},

	//=========================================================================
	
	handleDeleteAccount: function(event) {
		event.preventDefault();
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'deleteAccount');
	},
	
	handleFormChange: function() {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("DeleteAccount.handleDeleteAccount", {trace: false});
		deferredResult.addCallback(this.props.userInfo['checkPassphraseCallback'], this.refs['passphrase'].value);
		deferredResult.addMethod(this, function(passCheck){
			var username = this.refs['username'].value;
			var passphrase = this.refs['passphrase'].value;
			
			this.setState({
				'username': (username != '') ? (username == this.props.userInfo['username']) ? 'valid' : 'invalid' : 'empty',
				'passphrase': (passphrase != '') ? (passCheck) ? 'valid' : 'invalid' : 'empty',
				'confirm': this.refs['confirm'].checked,
			});
		});
		
		deferredResult.callback();
		
		return deferredResult;
	},
	
	shouldEnableDeleteAccountButton: function() {
		return (this.state['username'] == 'valid' && this.state['passphrase'] == 'valid' && this.state['confirm']);
	},
	
	//=========================================================================

	render: function () {
		return	React.DOM.div({className:'extraFeature deleteAccount'}, [
			React.DOM.div({'className':'header'}, [
				React.DOM.h1({}, "Delete account"),
			]),
			React.DOM.div({'className': 'content'}, [
				React.DOM.form({'key':'form', 'className':'deleteAccountForm', 'onChange': this.handleFormChange, 'onSubmit':this.handleDeleteAccount}, [
					React.DOM.div({'key':'fields'},[
						React.DOM.label({'key':'username-label', 'htmlFor':'name'}, "username"),
						React.DOM.input({'key':'username', 'className': this.state['username'], 'type':'text', 'name':'name', 'ref':'username', 'placeholder':"username", 'autoCapitalize':'none'}),
						React.DOM.label({'key':'passphrase-label', 'autoFocus': 'true', 'htmlFor':'passphrase'}, "passphrase"),
						React.DOM.input({'key':'passphrase', 'className': this.state['passphrase'], 'type':'password', 'name':'passphrase', 'ref':'passphrase', 'placeholder':"passphrase"}),
						React.DOM.p({}, [
							React.DOM.input({'key':'confirm', 'className':'confirmCheckbox', 'type':'checkbox', 'id':'deleteAccountConfirmCheckbox', 'name':'confirm', 'ref':'confirm'}),
							React.DOM.label({'htmlFor':'deleteAccountConfirmCheckbox'}, "All my data will be permanently deleted. I understand that this action cannot be undone or canceled.")
						]),
					]),
					React.DOM.button({'key':'button', 'type':'submit', 'disabled':!this.shouldEnableDeleteAccountButton(), 'className':'button'}, "Delete my account")
				])
			])
		]);
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.ExtraFeatures.DeleteAccount = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DeleteAccountClass);
