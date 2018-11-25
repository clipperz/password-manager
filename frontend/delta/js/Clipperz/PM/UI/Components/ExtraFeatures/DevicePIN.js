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

Clipperz.PM.UI.Components.ExtraFeatures.DevicePINClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.DevicePIN',

	propTypes: {
//		featureSet:			React.PropTypes.oneOf(['FULL', 'EXPIRED', 'TRIAL']).isRequired,
//		'level':	React.PropTypes.oneOf(['hide', 'info', 'warning', 'error']).isRequired
	},

	getInitialState: function() {
		return {
			isEditing: false,
			pinValue: ''
		}
	},

	//=========================================================================

	handleChange: function(anEvent) {
		var newValue = anEvent.target.value;

		if (!isNaN(newValue) && newValue.length <= this.props['PIN'].DEFAULT_PIN_LENGTH) {
			this.setState({
				'pinValue': anEvent.target.value
			});
		}
	},

	handleSubmit: function(anEvent) {
		var isSubmitEnabled = (this.state['pinValue'].length == this.props['PIN'].DEFAULT_PIN_LENGTH);

		if (isSubmitEnabled) { this.savePIN(); }

		anEvent.preventDefault();
	},

	setFocus: function() {
		this.refs['pinValue'].focus();
	},

	resetPIN: function() {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'disablePIN');
		MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'setState', this.getInitialState()));
		MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'setFocus'));
	},

	savePIN: function() {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'updatePIN', this.state.pinValue);
		MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'setState', this.getInitialState()));
	},

	//=========================================================================

	renderButton: function() {
		var isButtonEnabled = (this.props['PIN'].isSet() || this.state['pinValue'].length == this.props['PIN'].DEFAULT_PIN_LENGTH);
		var buttonText = this.props['PIN'].isSet() ? "Reset" : "Save";
		var buttonOnClick = (this.props['PIN'].isSet()) ? this.resetPIN : this.savePIN;

		return React.DOM.a({
			'className': 'button' + ((isButtonEnabled) ? '' : ' disabled'),
			'onClick': (isButtonEnabled) ? buttonOnClick : null,
		}, buttonText);
	},

	render: function () {
		var isInputEnabled = ! this.props['PIN'].isSet();
		var displayedPin = (this.props['PIN'].isSet()) ? '*****' : this.state.pinValue;

		return React.DOM.div({className:'extraFeature devicePIN'}, [
			React.DOM.div({'className':'header'}, [
				React.DOM.h1({}, "Device PIN"),
				React.DOM.div({'className':'description'}, [
					React.DOM.p({}, "You may create a 5-digit PIN to be used instead of your passphrase. Please note that the PIN is specific to the device you are now using."),
					React.DOM.p({}, [
						React.DOM.strong({}, "Warning"),
						": enabling a PIN on your device may represent a security risk! Make sure to keep the device with you at all times!",
					]),
				]),
			]),
			React.DOM.div({'className': 'content'}, [
//				React.DOM.p({}, "PIN is "+((this.props['PIN'].isSet()) ? '' : 'not ')+"set on this device"),
				React.DOM.p({}, ((this.props['PIN'].isSet()) ? "PIN is set on this device" : "PIN is not set on this device")),
				React.DOM.form({
						'onSubmit': this.handleSubmit,
				},[
					React.DOM.input({
						'type': 'tel',
						'key': 'pinValue',
						'ref': 'pinValue',
						'className': 'pinValue',
						'disabled': ! isInputEnabled,
						'onChange': this.handleChange,
						'onBlur': this.handleBlur,
						'value': displayedPin,
					}),
					this.renderButton(),
				])
			])
		]);
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.ExtraFeatures.DevicePIN = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DevicePINClass);
