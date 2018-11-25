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

Clipperz.PM.UI.Components.ExtraFeatures.PreferencesClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.Preferences',

	getInitialState: function() {
		return {
			'editedPreferences': new Clipperz.KeyValueObjectStore()
		}
	},

	propTypes: {
	},

	//============================================================================

	preference: function (aKeyPath) {
		return (this.state['editedPreferences'].getValue(aKeyPath)!=null) ? this.state['editedPreferences'].getValue(aKeyPath) : this.props['preferences'].getValue(aKeyPath);
	},

	removeEditedPreference: function(aKeyPath) {
		var newEditedPreferences;
		var parentKeyPath;

		parentKeyPath = aKeyPath.split('.');
		parentKeyPath.pop();
		parentKeyPath = parentKeyPath.join('.');

		newEditedPreferences = this.state['editedPreferences'];
		newEditedPreferences.removeValue(aKeyPath);

		if (parentKeyPath.length > 0 && MochiKit.Base.keys(newEditedPreferences.getValue(parentKeyPath)).length == 0) {
			this.removeEditedPreference(parentKeyPath);
		}

		this.setState({
			'editedPreferences': newEditedPreferences,
		});
	},

	//============================================================================
	
	checkboxClick: function (aRef) {
		return MochiKit.Base.bind(function (anEvent) {
// console.log("CHECKBOX CLICK", this, this.refs, this.refs[aRef]);
			this.refs[aRef].click();
		}, this);
	},

	//----------------------------------------------------------------------------

	handleChange: function(aKeyPath, aValuePattern) {
		return MochiKit.Base.method(this, 'handleEditEvent', aKeyPath, aValuePattern);
	},

	handleEditEvent: function(aKeyPath, aValuePattern, anEvent) {
		var newEditedPreferences;

		var target = anEvent.target;
		var value = (target.type == 'checkbox') ? target.checked : target.value;

		if (!aValuePattern || aValuePattern.test(value)) {
			if (value == this.props['preferences'].getValue(aKeyPath)) {
				this.removeEditedPreference(aKeyPath);
			} else {
				newEditedPreferences = this.state['editedPreferences'];
				newEditedPreferences.setValue(aKeyPath, value);

				this.setState({
					'editedPreferences': newEditedPreferences,
				});
			}
		}
	},

	handleSave: function() {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'setPreferences', this.state['editedPreferences'].values());
		MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'setState', this.getInitialState()));
	},

	//============================================================================
	
	isSaveButtonEnabled: function() {
		return MochiKit.Base.keys(this.state['editedPreferences'].values()).length != 0;
	},
	
	//----------------------------------------------------------------------------

	render: function () {
		var result;
		if (! this.props['preferences']) {
			result = React.DOM.p({}, "spinner...");	//	TODO: replace with actual spinner (if actually needed)
		} else {
			result = React.DOM.div({'className':'extraFeature preferences'}, [
				React.DOM.div({'className':'header'}, [
					React.DOM.h1({}, "Preferences"),
//					React.DOM.div({'className':'description'}, [
//						React.DOM.p({}, "Insert copy here..."),
//					])
				]),
				React.DOM.div({'className':'content'}, [
					React.DOM.ul({'className':'preferenceList'}, [
						React.DOM.li({'key': 'lock'}, [
							React.DOM.h3({'key':'1'}, "Lock"),
							React.DOM.div({'key':'2', 'className':'row two lockEnabled'}, [
								React.DOM.div({'className':'col one'}, [
									React.DOM.input({'type':'checkbox', 'checked':this.preference('lock.enabled'), 'onClick':this.handleChange('lock.enabled'), 'ref':'lock-enabled'}),
								]),
								React.DOM.div({'className':'col two'}, [
									React.DOM.span({'className':'clickable', 'onClick':this.checkboxClick('lock-enabled')}, "Enable auto-lock"),
								]),
							]),
							React.DOM.div({'key':'3', 'className':'row one lockTimeout'}, [
								React.DOM.p({'className':(this.preference('lock.enabled') ? 'enabled' : 'disabled')}, [
									React.DOM.span({}, "Lock timeout"),
									React.DOM.input({'type':'text', 'value':this.preference('lock.timeoutInMinutes'), 'onChange':this.handleChange('lock.timeoutInMinutes', /^[1-9][0-9]*$/)}),
									React.DOM.span({'className':'timeUnit'}, "minutes"),
								])
							]),
						]),

						React.DOM.li({'key': 'passwordGenerator'}, [
							React.DOM.h3({'key':'1'}, "Password generator"),
							React.DOM.div({'key':'2', 'className':'row one passwordLength'}, [
								React.DOM.p({}, [
									React.DOM.span({}, "Password length"),
									React.DOM.input({'type':'text', 'value':this.preference('passwordGenerator.length'), 'onChange':this.handleChange('passwordGenerator.length', /^[1-9][0-9]*$/)}),
									React.DOM.span({'className':'sizeUnit'}, "characters"),
								])
							]),
							React.DOM.div({'key':'3', 'className':'row one passwordCharSets'}, [
								React.DOM.p({'key':'label'}, "Characters"),
								React.DOM.ul({'key':'list'}, [
									React.DOM.li({'key':'A-Z'},   [ React.DOM.span({'className':'clickable', 'onClick':this.checkboxClick('A-Z')},   "A-Z"),   React.DOM.input({'type':'checkbox', 'checked':this.preference('passwordGenerator.characters.A-Z'),   'onChange':this.handleChange('passwordGenerator.characters.A-Z'),   'ref':'A-Z'}) ]),
									React.DOM.li({'key':'a-z'},   [ React.DOM.span({'className':'clickable', 'onClick':this.checkboxClick('a-z')},   "a-z"),   React.DOM.input({'type':'checkbox', 'checked':this.preference('passwordGenerator.characters.a-z'),   'onChange':this.handleChange('passwordGenerator.characters.a-z'),   'ref':'a-z'}) ]),
									React.DOM.li({'key':'0-9'},   [ React.DOM.span({'className':'clickable', 'onClick':this.checkboxClick('0-9')},   "0-9"),   React.DOM.input({'type':'checkbox', 'checked':this.preference('passwordGenerator.characters.0-9'),   'onChange':this.handleChange('passwordGenerator.characters.0-9'),   'ref':'0-9'}) ]),
									React.DOM.li({'key':'space'}, [ React.DOM.span({'className':'clickable', 'onClick':this.checkboxClick('space')}, "space"), React.DOM.input({'type':'checkbox', 'checked':this.preference('passwordGenerator.characters.space'), 'onChange':this.handleChange('passwordGenerator.characters.space'), 'ref':'space'}) ]),
									React.DOM.li({'key':'!#?'},   [ React.DOM.span({'className':'clickable', 'onClick':this.checkboxClick('!#?')},   "!#?"),   React.DOM.input({'type':'checkbox', 'checked':this.preference('passwordGenerator.characters.!#?'),   'onChange':this.handleChange('passwordGenerator.characters.!#?'),   'ref':'!#?'}) ]),
								]),
//								React.DOM.p({}, [
//									React.DOM.span({}, "Charset"),
//									React.DOM.input({'type':'text', 'value':this.preference('passwordGenerator.charset'), 'onKeyDown':this.handleKeyDown('passwordGenerator.charset')}),
//								]),
							]),
						]),
/*
						React.DOM.li({'key': 'language'}, [
							React.DOM.h3({'key':'1'}, "Language"),
							React.DOM.div({'key':'2', 'className':'row one language'}, [
								React.DOM.select({'value':this.preference('preferredLanguage'), 'onChange':this.handleChange('preferredLanguage')}, [
									React.DOM.option({'value':'en'}, "English"),
									React.DOM.option({'value':'fr'}, "Français"),
									React.DOM.option({'value':'it'}, "Italiano"),
								])
							]),
						]),

						React.DOM.li({'key': 'donationReminder'}, [
							React.DOM.h3({'key':'1'}, "Donation reminder"),
							React.DOM.div({'key':'2', 'className':'row two donationReminder'}, [
								React.DOM.div({'className':'col one'}, [
									React.DOM.input({'type':'checkbox', 'checked':this.preference('shouldShowDonationPanel'), 'onChange':this.handleChange('shouldShowDonationPanel'), 'ref':'shouldShowDonationPanel'}),
								]),
								React.DOM.div({'className':'col two'}, [
									React.DOM.span({'className':'clickable', 'onClick':this.checkboxClick('shouldShowDonationPanel')}, "Show donation reminder"),
								]),
							]),
						]),
*/
					]),
					React.DOM.a({
						'className': 'button'+((this.isSaveButtonEnabled()) ? '' : ' disabled'),
						'onClick': (this.isSaveButtonEnabled()) ? this.handleSave : null,
					}, "Save Preferences"),
				])
			]);
		}

		return result;
	},

	//============================================================================
});

Clipperz.PM.UI.Components.ExtraFeatures.Preferences = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.PreferencesClass);
