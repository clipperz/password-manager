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
	//=========================================================================
	__syntaxFix__: "syntax fix"
});


