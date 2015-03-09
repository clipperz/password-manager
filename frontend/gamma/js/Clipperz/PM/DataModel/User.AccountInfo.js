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

Clipperz.PM.DataModel.User.AccountInfo = function(args) {
	Clipperz.PM.DataModel.User.AccountInfo.superclass.constructor.apply(this, arguments);
	console.log("new AccountInfo", args);

//	{
//	    "expirationDate": "Sat, 18 January 2014 11:56:17 UTC",
//	    "featureSet": "EXPIRED",
//	    "features": [
//	        "OFFLINE_COPY",
//	        "LIST_CARDS"
//	    ],
//	    "isExpired": true,
//	    "isExpiring": false,
//	    "latestActiveLevel": "TRIAL",
//	    "latestActiveThreshold": 0.0,
//	    "paymentVerificationPending": false,
//	    "payments": [],
//		"referenceDate": "Mon, 03 March 2014 09:54:50 UTC"
//	}

	this._args = args;

	return this;
}

//-----------------------------------------------------------------------------

Clipperz.Base.extend(Clipperz.PM.DataModel.User.AccountInfo, Object, {

	'toString': function() {
		return "Clipperz.PM.DataModel.User.AccountInfo";
	},

	'args': function () {
		return this._args;
	},
	
	'expirationDate': function () {
		return Clipperz.PM.Date.parse(this.args()['expirationDate']);
	},
	
	'latestActiveLevel': function () {
		return this.args()['latestActiveLevel'];
	},
	
	'latestActiveThreshold': function () {
		return this.args()['latestActiveThreshold'];
	},
	
	'status': function () {
		return this.args()['featureSet'];
	},
	
	'isExpiring': function () {
		return this.args()['isExpiring'];
	},

	'isExpired': function () {
		return this.args()['isExpired'];
	},

	'paymentVerificationPending': function () {
		return this.args()['paymentVerificationPending'];
	},
	
	//=========================================================================
	__syntaxFix__: "syntax fix"
});

//-----------------------------------------------------------------------------

