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

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }

//=============================================================================

Clipperz.PM.Proxy.Offline = function(args) {
	args = args || {};
	
	Clipperz.PM.Proxy.Offline.superclass.constructor.call(this, args);

	this._dataStore = args.dataStore || new Clipperz.PM.Proxy.Offline.DataStore(args);

	return this;
}

YAHOO.extendX(Clipperz.PM.Proxy.Offline, Clipperz.PM.Proxy, {

	'toString': function () {
		return "Clipperz.PM.Proxy.Offline";
	},

	//-------------------------------------------------------------------------

	'dataStore': function () {
		return this._dataStore;
	},

	//-------------------------------------------------------------------------

	'sendMessage': function(aFunctionName, someParameters) {
		return this.dataStore().processMessage(aFunctionName, someParameters);
	},

	//-------------------------------------------------------------------------

	'isReadOnly': function () {
		return true;
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
	
});

