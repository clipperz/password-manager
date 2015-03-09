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
	throw "Clipperz.PM.DataModel.User.Header.OneTimePasswords depends on Clipperz.PM.DataModel.User!";
}  
if (typeof(Clipperz.PM.DataModel.User.Header) == 'undefined') { Clipperz.PM.DataModel.User.Header = {}; }

//-----------------------------------------------------------------------------

Clipperz.PM.DataModel.User.Header.OneTimePasswords = function(args) {
	Clipperz.PM.DataModel.User.Header.OneTimePasswords.superclass.constructor.apply(this, arguments);

	this._oneTimePasswords = null;
	
	return this;
}

//-----------------------------------------------------------------------------

Clipperz.Base.extend(Clipperz.PM.DataModel.User.Header.OneTimePasswords, Clipperz.PM.DataModel.EncryptedRemoteObject, {

	'toString': function() {
		return "Clipperz.PM.DataModel.User.Header.OneTimePasswords";
	},

	//-------------------------------------------------------------------------
/*
	'packData': function (someData) {	//	++
		var result;

		result = Clipperz.PM.DataModel.User.Header.OneTimePasswords.superclass.packData.apply(this, arguments);

		return result;
	},
*/
	//-------------------------------------------------------------------------
/*
	'packRemoteData': function (someData) {
		var result;

		result = Clipperz.PM.DataModel.User.Header.OneTimePasswords.superclass.packRemoteData.apply(this, arguments);

		return result;
	},
*/
	//-------------------------------------------------------------------------
/*
	'prepareRemoteDataWithKey': function (aKey) {
		var result;

		result = Clipperz.PM.DataModel.User.Header.OneTimePasswords.superclass.prepareRemoteDataWithKey.apply(this, arguments);

		return result;
	},
*/
	//=========================================================================

	'oneTimePasswords': function () {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("User.Header.OneTimePasswords.oneTimePasswords", {trace:false});
		if (this._oneTimePasswords == null) {
			deferredResult.addMethod(this, 'values')
			deferredResult.addCallback(MochiKit.Base.bind(function (someData) {
				var	otpKey;
				
				this._oneTimePasswords = {};
				
				for (otpKey in someData) {
					var otp;
					var otpParameters;
					
					otpParameters = Clipperz.Base.deepClone(someData[otpKey]);
					otpParameters['reference'] = otpKey;
					
					otp = new Clipperz.PM.DataModel.OneTimePassword(otpParameters);
					this._oneTimePasswords[otpKey] = otp;
				}

				return this._oneTimePasswords;

			}, this));
			deferredResult.callback();
		} else {
			deferredResult = MochiKit.Async.succeed(this._oneTimePasswords);
		}
		
		return deferredResult;
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});

//-----------------------------------------------------------------------------

