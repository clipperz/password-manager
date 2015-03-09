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
if (typeof(Clipperz.PM.Proxy) == 'undefined') { Clipperz.PM.Proxy = {}; }

//=============================================================================

Clipperz.PM.Proxy.Test = function(args) {
	args = args || {};

	Clipperz.PM.Proxy.Offline.call(this, args);

	
	
	return this;
}

Clipperz.PM.Proxy.Test.prototype = MochiKit.Base.update(new Clipperz.PM.Proxy.Offline(), {

	'toString': function() {
		return "Clipperz.PM.Proxy.Test";
	},

	//-------------------------------------------------------------------------

	'isTestData': function() {
		return typeof(this.userData()['__masterkey_test_value__'] != 'undefined');
	},
	
	//-------------------------------------------------------------------------

	'userDetails': function() {
		var result;

		if (this.isTestData()) {
			var serializedHeader;
			var version;
		
			version = this.userData()['version'];
			serializedHeader = Clipperz.Base.serializeJSON(this.userData()['userDetails']);
			result = Clipperz.PM.Crypto.encryptingFunctions.versions[version].encrypt(this.userData()['__masterkey_test_value__'], serializedHeader);
		} else {
			result = Clipperz.PM.Proxy.Offline.prototype.userDetails.call(this);
		}
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'statistics': function() {
		var result;
		var serializedStatistics;
		var version;
		
		version = this.userData()['version'];
		serializedStatistics = Clipperz.Base.serializeJSON(this.userData()['statistics']);
		result = Clipperz.PM.Crypto.encryptingFunctions.versions[version].encrypt(this.userData()['__masterkey_test_value__'], serializedStatistics);
		
		return result;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});

