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

Clipperz.Base.module('Clipperz.PM.UI.Compact');

Clipperz.PM.UI.Compact.MainController = function() {
//	this._loginPanel = null;
//	this._user = null;
//
//	this._isRunningCompact = false;
//	
//	Clipperz.NotificationCenter.register(null, 'userConnected', this, 'userConnectedCallback');
//	Clipperz.NotificationCenter.register(null, 'switchLanguage', this, 'switchLanguageHandler');
//
//	Clipperz.NotificationCenter.register(null, 'EXCEPTION', this, 'reportException');

	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.Compact.MainController.prototype, {

	'toString': function() {
		return "Clipperz.PM.UI.Compact.MainController";
	},

	//-----------------------------------------------------------------------------

	'run': function(shouldShowRegistrationForm) {
		Clipperz.logDebug("running " + this.toString());
	},
	
	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});