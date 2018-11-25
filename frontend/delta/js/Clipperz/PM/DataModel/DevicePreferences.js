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

Clipperz.PM.DataModel.DevicePreferences = function (args) {
	args = args || {};

	this._data = null;

	Clipperz.PM.DataModel.DevicePreferences.superclass.constructor.apply(this, arguments);

	return this;
}

Clipperz.Base.extend(Clipperz.PM.DataModel.DevicePreferences, Object, {

	toString: function () {
		return "Clipperz.PM.DataModel.DevicePreferences";
	},

	//-------------------------------------------------------------------------

	shouldStoreDataLocally: function () {
		return (localStorage.getItem('shouldStoreDataLocally') === 'true');
	},

	setShouldStoreDataLocally: function (aValue) {
		localStorage.setItem('shouldStoreDataLocally', aValue);
	},

	//-------------------------------------------------------------------------

	setAccountDataWityResponse: function (aResponse) {
		localStorage.setItem('clipperz_dump_data',		aResponse['data']);
		localStorage.setItem('clipperz_dump_version',	aResponse['version']);
		localStorage.setItem('clipperz_dump_date',		new Date());

		this._data = null;
	},

	accountData: function () {
		if (this._data == null) {
			var	data;

			data = localStorage.getItem('clipperz_dump_data');
			if (data != null) {
				this._data = JSON.parse(data);
			}
		}

		return this._data;
	},

	latestDownload: function () {
		var	result;
		var	date;

		date = localStorage.getItem('clipperz_dump_date');
		if (date != null) {
			result = new Date(date);
		} else {
			result = null;
		}

		return result;
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});

