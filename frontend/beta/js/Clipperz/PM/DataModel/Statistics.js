/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz's Javascript Crypto Library.
Javascript Crypto Library provides web developers with an extensive
and efficient set of cryptographic functions. The library aims to
obtain maximum execution speed while preserving modularity and
reusability.
For further information about its features and functionalities please
refer to http://www.clipperz.com

* Javascript Crypto Library is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Javascript Crypto Library is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Javascript Crypto Library.  If not, see
  <http://www.gnu.org/licenses/>.

*/

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.DataModel) == 'undefined') { Clipperz.PM.DataModel = {}; }


//#############################################################################

Clipperz.PM.DataModel.Statistics = function(args) {
	args = args || {};

	this._user = args.user;
	this._data = args.data || null;

	return this;
}

Clipperz.PM.DataModel.Statistics.prototype = MochiKit.Base.update(null, {

	//-------------------------------------------------------------------------

	'decrypt': function(aVersion, someEncryptedData) {
		var deferredResult;

//MochiKit.Logging.logDebug(">>> Statistics.decrypt");
		if (someEncryptedData == Clipperz.PM.Crypto.nullValue) {
			this.setData({});
			deferredResult = MochiKit.Async.succeed(this.data());
		} else {
			var	statistic;
			var user;
		
			statistic = this;
			user = this.user();
			deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addCallback(function() { console.time("Statistics.decrypt.deferredDecrypt")});
			deferredResult.addCallback(Clipperz.PM.Crypto.deferredDecrypt, user.passphrase(), someEncryptedData, aVersion);
//deferredResult.addCallback(function() { console.timeEnd("Statistics.decrypt.deferredDecrypt")});
//deferredResult.addCallback(function() { console.time("Statistics.decrypt.setup")});
			deferredResult.addCallbacks(
				MochiKit.Base.partial(function (aStatistic, someData) {
					aStatistic.setData(someData);
					return aStatistic.data();
				}, statistic),
				MochiKit.Base.partial(function (aStatistic) {
					MochiKit.Logging.logWarning("resetting user statistics due to an error while decrypting stored data");
					aStatistic.setData({});
					return aStatistic.data();
				}, statistic)
			);
//deferredResult.addCallback(function() { console.timeEnd("Statistics.decrypt.setup")});

			deferredResult.callback();
		}
//MochiKit.Logging.logDebug("<<< Statistics.decrypt");
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},
	
	//-------------------------------------------------------------------------

	'data': function() {
		return this._data;
	},
	
	'setData': function(aValue) {
		this._data = aValue;
		
		this.extractInfoFromData(aValue);
	},

	//-------------------------------------------------------------------------

	'extractInfoFromData': function(someValues) {
		
	},
	
	//-------------------------------------------------------------------------

	'encryptedData': function() {
		return Clipperz.PM.Crypto.deferredEncryptWithCurrentVersion(this.user().passphrase(), this.serializedData());
	},

	//-------------------------------------------------------------------------
	
	'serializedData': function() {
		var result;
		
//MochiKit.Logging.logDebug(">>> Statistics.serializedData");
		result = {};
//MochiKit.Logging.logDebug("<<< Statistics.serializedData");

		return result;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});

