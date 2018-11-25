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

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.Proxy) == 'undefined') { Clipperz.PM.Proxy = {}; }

//=============================================================================

Clipperz.PM.Proxy.Test = function(args) {
	Clipperz.PM.Proxy.Test.superclass.constructor.call(this, args);

	args = args || {};

	this._expectedRequests = (args.shouldCheckExpectedRequests === true) ? [] : null;
	this._isExpectingRequests = true;
	this._unexpectedRequests = [];

	this.dataStore().resetData();

	return this;
}

Clipperz.Base.extend(Clipperz.PM.Proxy.Test, Clipperz.PM.Proxy.Offline, {

	'toString': function() {
		return "Clipperz.PM.Proxy.Test";
	},

	//=========================================================================

	'expectedRequests': function () {
		return this._expectedRequests;
	},

	//-------------------------------------------------------------------------

	'shouldCheckExpectedRequests': function () {
		return (this._expectedRequests != null);
	},

	'setShouldCheckExpectedRequests': function(aValue) {
		if (aValue) {
			this._expectedRequests = aValue;
		} else {
			this._expectedRequests = null;
		}
	},

	//-------------------------------------------------------------------------

	'shouldNotReceiveAnyFurtherRequest': function () {
		this._isExpectingRequests = false;
	},

	'mayReceiveMoreRequests': function () {
		this._isExpectingRequests = true;
		this.resetUnexpectedRequests();
	},

	'isExpectingRequests': function () {
		return this._isExpectingRequests;
	},

	//-------------------------------------------------------------------------

	'unexpectedRequests': function () {
		return this._unexpectedRequests;
	},

	'resetUnexpectedRequests': function () {
		this._unexpectedRequests = [];
	},

	//-------------------------------------------------------------------------

	'testExpectedRequestParameters': function (aPath, anActualRequest, anExpectedRequest) {
		var aKey;
		for (aKey in anExpectedRequest) {
			if (typeof(anActualRequest[aKey]) == 'undefined') {
				throw "the expected paramter [" + aKey + "] is missing from the actual request";
			}
			if (typeof(anExpectedRequest[aKey]) == 'object') {
				this.testExpectedRequestParameters(aPath + "." + aKey,  anActualRequest[aKey], anExpectedRequest[aKey])
			} else {
				if (! anExpectedRequest[aKey](anActualRequest[aKey])) {
					throw "wrong value for paramter [" + aKey + "]; got '" + anActualRequest[aKey] + "'";
				}
			}
		}
	},

	//-------------------------------------------------------------------------

	'checkRequest': function(aFunctionName, someParameters) {
		if (this.shouldCheckExpectedRequests()) {
			var expectedRequest;

			expectedRequest = this.expectedRequests().pop();
			if (expectedRequest == null) {
				throw "Proxy.Test.sentMessage: no expected result specified. Got request '" + aFunctionName + "': " + someParameters;
			}

			try {
				if (aFunctionName != expectedRequest.functionName) {
					throw "wrong function name. Got '" + aFunctionName + "', expected '" + expectedRequest.request.functionName + "'";
				}

				this.testExpectedRequestParameters("parameters", someParameters, expectedRequest.parameters);
			} catch(exception) {
				throw "Proxy.Test.sentMessage[" + expectedRequest.name + "]: " + exception;
			}
		}
	},

	//=========================================================================

	'_sendMessage': function(aFunctionName, aVersion, someParameters) {
		var result;

		if (this.isExpectingRequests() == false) {
//			throw Clipperz.PM.Connection.exception.UnexpectedRequest;
Clipperz.log("UNEXPECTED REQUEST " + aFunctionName /* + ": " + Clipperz.Base.serializeJSON(someParameters) */);
			this.unexpectedRequests().push({'functionName':aFunctionName, 'someParameters': someParameters});
		};
//if (aFunctionName == 'knock') {
//	console.log(">>> send message - " + aFunctionName, someParameters);
//} else {
//	console.log(">>> SEND MESSAGE - " + aFunctionName + " [" + someParameters['parameters']['message'] + "]", someParameters['parameters']['parameters']);
//}
		this.checkRequest(aFunctionName, someParameters);
		result = Clipperz.PM.Proxy.Test.superclass._sendMessage.call(this, aFunctionName, aVersion, someParameters);

		return result;
	},
	
	//=========================================================================
	__syntaxFix__: "syntax fix"
	
});

