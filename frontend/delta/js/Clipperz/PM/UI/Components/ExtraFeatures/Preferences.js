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

Clipperz.PM.UI.Components.ExtraFeatures.PreferencesClass = React.createClass({

	getInitialState: function() {
		return {
			'preferenceBeingEdited': null,
			'preferenceValue': '',
		};
	},

	propTypes: {
	},

	//=========================================================================

	setEditedPreference: function(aKey, aValue) {
		this.setState({
			'preferenceBeingEdited': aKey,
			'preferenceValue': aValue
		});
	},

	handleChange: function(anEvent) {
		var newState = this.state;

		newState['preferenceValue'] = anEvent.target.value;

		this.setState(newState);
	},

	handleSave: function() {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'setPreference', this.state['preferenceBeingEdited'], this.state['preferenceValue']);
		this.setEditedPreference(null, '');
	},

	handleCancel: function() {
		this.setEditedPreference(null, '');
	},

	handleKeyPressed: function(anEvent) {
		switch (anEvent.keyCode) {
			case  9: // tab
				this.handleSave();
				// TODO: edit next preference
				break;
			case 13: // enter
				this.handleSave();
				break;
			case 27: // escape
				this.handleCancel();
				break;
		}
	},

	//=========================================================================

	renderPreferenceValueElement: function(aKey) {
		var result;

		var preferenceValue = this.props.userInfo.preferences.getValue(aKey);

		if (this.state.preferenceBeingEdited == aKey) {
			result = React.DOM.input({
				'autoFocus': true,
				'key': aKey,
				'type': 'text',
				'value': this.state.preferenceValue,
				'onChange': this.handleChange,
				'onKeyDown': MochiKit.Base.method(this, 'handleKeyPressed'),
			});
		} else {
			result = React.DOM.p({
				'className': 'preferenceValue',
				'onClick': MochiKit.Base.method(this, 'setEditedPreference', aKey, preferenceValue)
			}, preferenceValue);
		}

		return result;
	},

	renderPreferences: function() {
		var result;
		
		result = [
			React.DOM.li({'key': 'autoLockAfterMinutes'}, [
				React.DOM.p({'className': 'preferenceName'}, "Automatic lock (minutes) - m"),
				React.DOM.p({'className': 'preferenceDescription'}, "Automatically lock Clipperz after N minutes. (0 = auto lock disabled)"),
				this.renderPreferenceValueElement('lock.timeoutInMinutes')
			]),
			React.DOM.li({'key': 'shouldShowDonationPanel'}, [
				React.DOM.p({'className': 'preferenceName'}, "Donation Panel - m"),
				React.DOM.p({'className': 'preferenceDescription'}, "Select whether to display the donation panel or not"),
				this.renderPreferenceValueElement('shouldShowDonationPanel')
			])
		];

		return result;
	},

	render: function () {
		var result;

		if (! this.props.userInfo.preferences) {
			result = React.DOM.p({}, "spinner...");
		} else {
			result = React.DOM.div({'className':'extraFeature preferences'}, [
				React.DOM.div({'className':'header'}, [
					React.DOM.h1({}, "Preferences"),
					React.DOM.div({'className':'description'}, [
						React.DOM.p({}, "Insert copy here..."),
					])
				]),
				React.DOM.div({'className':'content'}, [
					React.DOM.ul({'className':'preferenceList'}, this.renderPreferences()),
				])
			]);
		}

		return result;
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.ExtraFeatures.Preferences = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.PreferencesClass);
