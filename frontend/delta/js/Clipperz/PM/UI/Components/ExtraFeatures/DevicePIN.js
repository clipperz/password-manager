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

Clipperz.PM.UI.Components.ExtraFeatures.DevicePINClass = React.createClass({

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

	_editModeLocked: false,

	//=========================================================================

	enterEditMode: function() {
		this.setState({
			'isEditing': true,
			'pinValue': ''
		});
	},

	exitEditMode: function() {
		this.setState({
			'isEditing': false,
		});
	},

	lockEditMode: function() {
		this._editModeLocked = true;
	},

	unlockEditMode: function() {
		this._editModeLocked = false;
	},

	handleFocus: function(anEvent) {
		anEvent.preventDefault();

		this.refs['pinValue'].getDOMNode().focus();
	},

	handleBlur: function(anEvent) {
		if (! this._editModeLocked) {
			if (anEvent.target.value.length < this.props['PIN'].DEFAULT_PIN_LENGTH) {
				this.exitEditMode();
			}
		}
	},

	handleKeyDown: function(anEvent) {
		if (anEvent.keyCode == 27) {
			this.refs['pinValue'].getDOMNode().blur();
		}
	},

	handleChange: function(anEvent) {
		if (anEvent.target.value.length == this.props['PIN'].DEFAULT_PIN_LENGTH) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'updatePIN', anEvent.target.value);
			this.refs['pinValue'].getDOMNode().blur();
			this.exitEditMode();
		} else {
			this.setState({
				'pinValue': anEvent.target.value
			});
		}
	},

	handleCheckboxChange: function(anEvent) {
		if (this.props['PIN'].isSet() || this.state['isEditing']) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'disablePIN', anEvent.target.value);
			this.exitEditMode();
		} else {
			this.enterEditMode();
		}
	},

	handleResetPIN: function() {
		this.enterEditMode();
	},

	//=========================================================================

	// renderDigitInputs: function() {
	// 	var i;
	// 	var result;

	// 	result = [];
	// 	for (i = 0; i<this.props['PIN'].DEFAULT_PIN_LENGTH; i++) {
	// 		var boxIsFull = (this.state['isEditing']&&this.state['pinValue'][i]) 
	// 						||
	// 						(!this.state['isEditing']&&this.props['PIN'].isSet())

	// 		result.push(React.DOM.input({
	// 			'key': 'pin-digit-'+i,
	// 			'ref': 'pin-digit-'+i,
	// 			'name': 'pin-digit-'+i,
	// 			'className': 'pinDigit',
	// 			'readOnly': true,
	// 			'type': 'text',
	// 			'value': boxIsFull ? '*' : '',
	// 			'min': 0,
	// 			'max': 9,
	// 			'disabled': !this.state['isEditing'],
	// 			'onFocus': this.handleFocus,
	// 		}));
	// 	}

	// 	return result;
	// },

	//-------------------------------------------------------------------------

	componentDidUpdate: function() {
		if (this.state['isEditing']) {
			this.refs['pinValue'].getDOMNode().focus();
		}
	},

	render: function () {
		var displayedPin;
		var isFormEnabled = (this.props['PIN'].isSet() || this.state.isEditing);
		var isResetButtonEnabled = (! this.state['isEditing'] && this.props['PIN'].isSet());

		if (this.state.isEditing) {
			displayedPin = this.state['pinValue'];
		} else {
			displayedPin = (this.props['PIN'].isSet()) ? '*****' : '';
		}

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
				React.DOM.form({},[
					React.DOM.p({}, [
						React.DOM.input({
							'type': 'checkbox',
							'key': 'pinEnabled',
							'checked': isFormEnabled,
							'onChange': this.handleCheckboxChange,
							'onMouseDown': this.lockEditMode,
							'onMouseUp': this.unlockEditMode,
						}),
						React.DOM.label({
							'key': 'pinEnabledLabel', 
							'htmlFor': 'pinEnabled',
							'onClick': this.handleCheckboxChange,
							'onMouseDown': this.lockEditMode,
							'onMouseUp': this.unlockEditMode,
						}, "Enable PIN on your device")
					]),
					// this.renderDigitInputs(),
					React.DOM.input({
						'type': 'tel',
						'key': 'pinValue',
						'ref': 'pinValue',
						'className': 'pinValue',
						'disabled': !this.state['isEditing'],
						'onKeyDown': this.handleKeyDown,
						'onChange': this.handleChange,
						'onBlur': this.handleBlur,
						'value': displayedPin,
						// 'style': {'position': 'fixed', 'top': -1000}
					}),
					React.DOM.a({
						'className': 'button'+(isResetButtonEnabled ? '' : ' disabled'),
						'onClick': (isResetButtonEnabled) ? this.handleResetPIN : null
					}, "Reset PIN"),
				])
			])
		]);
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.ExtraFeatures.DevicePIN = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DevicePINClass);
