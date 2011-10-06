/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz Community Edition.
Clipperz Community Edition is an online password manager.
For further information about its features and functionalities please
refer to http://www.clipperz.com.

* Clipperz Community Edition is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Clipperz Community Edition is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Clipperz Community Edition.  If not, see
  <http://www.gnu.org/licenses/>.

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
//console.log(">>> Proxy.testExpectedRequestParameters [" + aPath  + "]", anActualRequest, anExpectedRequest);
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
//console.log("<<< Proxy.testExpectedRequestParameters");
	},

	//-------------------------------------------------------------------------

	'checkRequest': function(aFunctionName, someParameters) {
		if (this.shouldCheckExpectedRequests()) {
			var expectedRequest;

//console.log(">>> Proxy.Test.checkRequest - " + aFunctionName, someParameters);
			expectedRequest = this.expectedRequests().pop();
//console.log("--- Proxy.Test.checkRequest - expectedRequest", expectedRequest);
			if (expectedRequest == null) {
				throw "Proxy.Test.sentMessage: no expected result specified. Got request '" + aFunctionName + "': " + someParameters;
			}

			try {
				if (aFunctionName != expectedRequest.functionName) {
					throw "wrong function name. Got '" + aFunctionName + "', expected '" + expectedRequest.request.functionName + "'";
				}

				this.testExpectedRequestParameters("parameters", someParameters, expectedRequest.parameters);
			} catch(exception) {
//console.log("EXCEPTION: Proxy.Test.sentMessage[" + expectedRequest.name + "]", exception)
				throw "Proxy.Test.sentMessage[" + expectedRequest.name + "]: " + exception;
			}
		}
//console.log("<<< Proxy.Test.checkRequest");
	},

	//=========================================================================

	'sendMessage': function(aFunctionName, someParameters) {
		var result;

		if (this.isExpectingRequests() == false) {
//			throw Clipperz.PM.Connection.exception.UnexpectedRequest;
Clipperz.log("UNEXPECTED REQUEST " + aFunctionName /* + ": " + Clipperz.Base.serializeJSON(someParameters) */);
			this.unexpectedRequests().push({'functionName':aFunctionName, 'someParameters': someParameters});
		};
		this.checkRequest(aFunctionName, someParameters);
		result = Clipperz.PM.Proxy.Test.superclass.sendMessage.call(this, aFunctionName, someParameters);

		return result;
	},
	
	//=========================================================================
	__syntaxFix__: "syntax fix"
	
});

