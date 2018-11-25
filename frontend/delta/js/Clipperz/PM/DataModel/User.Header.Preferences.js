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

try { if (typeof(Clipperz.PM.DataModel.User) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.PM.DataModel.User.Header.Preferences depends on Clipperz.PM.DataModel.User!";
}  

if (typeof(Clipperz.PM.DataModel.User.Header) == 'undefined') { Clipperz.PM.DataModel.User.Header = {}; }

Clipperz.PM.DataModel.User.Header.Preferences = function(args) {
	Clipperz.PM.DataModel.User.Header.Preferences.superclass.constructor.apply(this, arguments);

	return this;
}

Clipperz.Base.extend(Clipperz.PM.DataModel.User.Header.Preferences, Clipperz.PM.DataModel.EncryptedRemoteObject, {

	'toString': function() {
		return "Clipperz.PM.DataModel.User.Header.Preferences";
	},

	//-------------------------------------------------------------------------

	'mergePreferences': function(somePreferences, someOtherPreferences) {
		var result;

		result = new Clipperz.KeyValueObjectStore();

		result.setValues(MochiKit.Base.updatetree(
			Clipperz.Base.deepClone(someOtherPreferences),
			somePreferences
		));

		return result;
	},
	
	'mergeDefaultPreferences': function(somePreferences) {
		return this.mergePreferences(somePreferences, Clipperz.PM.DataModel.User.Header.Preferences.defaultPreferences);
	},

	'mergeUserPreferences': function(somePreferences) {
		return this.mergePreferences(somePreferences, this._objectDataStore.values());
	},

	'getPreferences': function() {
		return Clipperz.Async.callbacks("User.Header.Preferences.getPreferences", [
			MochiKit.Base.method(this, 'values'),
			MochiKit.Base.method(this, 'mergeDefaultPreferences')
		], {trace:false});
	},

	'getPreference': function(aKey) {
		return Clipperz.Async.callbacks("User.Header.Preferences.getPreference", [
			MochiKit.Base.method(this, 'getPreferences'),
			MochiKit.Base.methodcaller('getValue', aKey)
		], {trace:false});
	},

	'setPreferences': function(anObject) {
		return Clipperz.Async.callbacks("User.Header.Preferences.setPreferences", [
			MochiKit.Base.method(this, 'mergeUserPreferences', anObject),
			MochiKit.Base.methodcaller('values'),
			MochiKit.Base.method(this, 'setValues')
		], {trace:false});
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});


Clipperz.PM.DataModel.User.Header.Preferences.defaultPreferences = {
	'lock': {
		'enabled': false,
		'timeoutInMinutes': 10,
	},
	'passwordGenerator': {
		'length': 24,
		'characters': {
			'A-Z':   true,
			'a-z':   true,
			'0-9':   true,
			'space': false,
			'!#?':   true,
		},
//		'charset': '',
	},

	//	legacy preferences
	'preferredLanguage': 'en',
	'shouldShowDonationPanel': true,
};