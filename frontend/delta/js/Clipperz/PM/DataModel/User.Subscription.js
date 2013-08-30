/*

Copyright 2008-2013 Clipperz Srl

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
	throw "Clipperz.PM.DataModel.User.Subscription depends on Clipperz.PM.DataModel.User!";
}  

Clipperz.PM.DataModel.User.Subscription = function(args) {
	this._attributes = args;
	return this;
}


Clipperz.Base.extend(Clipperz.PM.DataModel.User.Subscription, Object, {

	'features': function () {
		return this._attributes['features'];
	},

	'type': function () {
		return this._attributes['type'];
	},

	'validity': function () {
		return {
			'from':	this._attributes['fromDate'],
			'to':	this._attributes['toDate']
		};
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});
