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
Clipperz.Base.module('Clipperz.PM.UI.Components.ExtraFeatures');

Clipperz.PM.UI.Components.ExtraFeatures.PassphraseClass = React.createClass({

	propTypes: {
//		featureSet:			React.PropTypes.oneOf(['FULL', 'EXPIRED', 'TRIAL']).isRequired,
//		'level':	React.PropTypes.oneOf(['hide', 'info', 'warning', 'error']).isRequired
	},
	
	getInitialState: function() {
		return {
//			'username': '',
//			'old-passphrase': '',
			'new-passphrase': '',
			'confirm-new-passphrase': ''
		};
	},

	//=========================================================================

	shouldEnableChangePassphraseButton: function() {
		return (
//			this.state['username'] && 
//			this.state['old-passphrase'] &&
			this.state['new-passphrase'] &&
			this.state['confirm-new-passphrase'] &&
			(this.state['new-passphrase'] == this.state['confirm-new-passphrase'])
		);
	},
	
	handleFormChange: function() {
		this.setState({
//			'username': this.refs['username'].getDOMNode().value,
//			'old-passphrase': this.refs['old-passphrase'].getDOMNode().value,
			'new-passphrase': this.refs['new-passphrase'].getDOMNode().value,
			'confirm-new-passphrase': this.refs['confirm-new-passphrase'].getDOMNode().value
		});
	},

	handleChangePassphrase: function(event) {
		event.preventDefault();
		
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'changePassphrase', this.refs['new-passphrase'].getDOMNode().value);

		this.refs['new-passphrase'].getDOMNode().value = '';
		this.refs['confirm-new-passphrase'].getDOMNode().value = '';
	},

	//=========================================================================

	render: function () {
		return	React.DOM.div({className:'extraFeature passphrase'}, [
			React.DOM.h1({}, "Change Passphrase"),
			React.DOM.form({'key':'form', 'className':'changePassphraseForm', 'onChange': this.handleFormChange, 'onSubmit':this.handleChangePassphrase}, [
				React.DOM.div({'key':'fields'},[
//					React.DOM.label({'key':'username-label', 'htmlFor' :'name'}, "username"),
//					React.DOM.input({'key':'username', 'type':'text', 'name':'name', 'ref':'username', 'placeholder':"username", 'autoCapitalize':'none'}),
//					React.DOM.label({'key':'old-passphrase-label', 'htmlFor' :'old-passphrase'}, "old passphrase"),
//					React.DOM.input({'key':'old-passphrase', 'type':'password', 'name':'old-passphrase', 'ref':'old-passphrase', 'placeholder':"old passphrase"}),
					React.DOM.label({'key':'new-passphrase-label', 'autoFocus': 'true', 'htmlFor' :'new-passphrase'}, "new passphrase"),
					React.DOM.input({'key':'new-passphrase', 'type':'password', 'name':'new-passphrase', 'ref':'new-passphrase', 'placeholder':"new passphrase"}),
					React.DOM.label({'key':'confirm-new-passphrase-label', 'htmlFor' :'confirm-new-passphrase'}, "confirm new passphrase"),
					React.DOM.input({'key':'confirm-new-passphrase', 'type':'password', 'name':'confirm-new-passphrase', 'ref':'confirm-new-passphrase', 'placeholder':"confirm new passphrase"})
				]),
				React.DOM.button({'key':'button', 'type':'submit', 'disabled':!this.shouldEnableChangePassphraseButton(), 'className':'button'}, "Change")
			]),
		]);
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.ExtraFeatures.Passphrase = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.PassphraseClass);
