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

try { if (typeof(Clipperz.PM.Proxy.Offline.DataStore) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.PM.Proxy.Offline.LocalStorageDataStore depends on Clipperz.PM.Proxy.Offline.DataStore!";
}  

//=============================================================================

Clipperz.PM.Proxy.Offline.LocalStorageDataStore = function(args) {
	args = args || {};
	
//	this._data = args.data || (typeof(_clipperz_dump_data_) != 'undefined' ? _clipperz_dump_data_ : null);
	this._data = JSON.parse(localStorage.getItem('clipperz_dump_data'));

	this._isReadOnly = (typeof(args.readOnly) == 'undefined' ? true : args.readOnly);
	this._shouldPayTolls = args.shouldPayTolls || false;

	this._tolls = {};
	this._currentStaticConnection = null;

//	Clipperz.PM.Proxy.Offline.LocalStorageDataStore.superclass.constructor.apply(this, arguments);
	
	return this;
}

Clipperz.Base.extend(Clipperz.PM.Proxy.Offline.LocalStorageDataStore, Clipperz.PM.Proxy.Offline.DataStore, {

	//=========================================================================

	'_knock': function(aConnection, someParameters) {
		var result;
		
		result = {
			toll: this.getTollForRequestType(someParameters['requestType'])
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'_registration': function(aConnection, someParameters) {
		throw Clipperz.PM.Proxy.Offline.DataStore.exception.ReadOnly;
	},

	//-------------------------------------------------------------------------

	'_handshake': function(aConnection, someParameters) {
		var result;
		var	nextTollRequestType;

		result = {};
		if (someParameters.message == "connect") {
			var userData;
			var randomBytes;
			var v;

			userData = this.data()['users'][someParameters.parameters.C];
			
			if ((typeof(userData) != 'undefined') && (userData['version'] == someParameters.version)) {
				aConnection['userData'] = userData;
				aConnection['C'] = someParameters.parameters.C;
			} else {
				aConnection['userData'] = this.data()['users']['catchAllUser'];
			}

			randomBytes = Clipperz.Crypto.Base.generateRandomSeed();
			aConnection['b'] = new Clipperz.Crypto.BigInt(randomBytes, 16);
			v = new Clipperz.Crypto.BigInt(aConnection['userData']['v'], 16);
			aConnection['B'] = (Clipperz.Crypto.SRP.k().multiply(v)).add(Clipperz.Crypto.SRP.g().powerModule(aConnection['b'], Clipperz.Crypto.SRP.n()));
			
			aConnection['A'] = someParameters.parameters.A;
			
			result['s'] = aConnection['userData']['s'];
			result['B'] = aConnection['B'].asString(16);
			
			nextTollRequestType = 'CONNECT';
		} else if (someParameters.message == "credentialCheck") {
			var v, u, s, S, A, K, M1;
			var stringHash = function (aValue) {
				return Clipperz.PM.Crypto.encryptingFunctions.versions[someParameters.version].hash(new Clipperz.ByteArray(aValue)).toHexString().substring(2);
			};
			
			v = new Clipperz.Crypto.BigInt(aConnection['userData']['v'], 16);
			A = new Clipperz.Crypto.BigInt(aConnection['A'], 16);
			u = new Clipperz.Crypto.BigInt(Clipperz.PM.Crypto.encryptingFunctions.versions[someParameters.version].hash(new Clipperz.ByteArray(A.asString(10) + aConnection['B'].asString(10))).toHexString(), 16);
			s = new Clipperz.Crypto.BigInt(aConnection['userData']['s'], 16);
			S = (A.multiply(v.powerModule(u, Clipperz.Crypto.SRP.n()))).powerModule(aConnection['b'], Clipperz.Crypto.SRP.n());

			K = stringHash(S.asString(10));

			M1 = stringHash(
				"597626870978286801440197562148588907434001483655788865609375806439877501869636875571920406529" +
				stringHash(aConnection['C']) +
				s.asString(10) +
				A.asString(10) +
				aConnection['B'].asString(10) +
				K
			);
			if (someParameters.parameters.M1 == M1) {
				var M2;
				
				M2 = stringHash(
					A.asString(10) +
					someParameters.parameters.M1 +
					K
				);
				result['M2'] = M2;
			} else {
				throw new Error("Client checksum verification failed! Expected <" + M1 + ">, received <" + someParameters.parameters.M1 + ">.", "Error");
			}

			nextTollRequestType = 'MESSAGE';
		} else if (someParameters.message == "oneTimePassword") {
			var otpData;
			
			otpData = this.data()['onetimePasswords'][someParameters.parameters.oneTimePasswordKey];

			try {
				if (typeof(otpData) != 'undefined') {
					if (otpData['status'] == 'ACTIVE') {
						if (otpData['keyChecksum'] == someParameters.parameters.oneTimePasswordKeyChecksum) {
							result = {
								'data':		otpData['data'],
								'version':	otpData['version']
							}

							otpData['status'] = 'USED';
						} else {
							otpData['status'] = 'DISABLED';
							throw "The requested One Time Password has been disabled, due to a wrong keyChecksum";
						}
					} else {
						throw "The requested One Time Password was not active";
					}
				} else {
					throw "The requested One Time Password has not been found"
				}
			} catch (exception) {
				result = {
					'data':		Clipperz.PM.Crypto.randomKey(),
					'version':	Clipperz.PM.Connection.communicationProtocol.currentVersion
				}
			}
			nextTollRequestType = 'CONNECT';
		} else {
			Clipperz.logError("Clipperz.PM.Proxy.Test.handshake - unhandled message: " + someParameters.message);
		}

		result = {
			result: result,
			toll:   this.getTollForRequestType(nextTollRequestType)
		}

		return result;
	},

	//-------------------------------------------------------------------------

	'_message': function(aConnection, someParameters) {
		var result;

		result = {};

		//=====================================================================
		//
		//		R	E	A	D	-	O	N	L	Y		M e t h o d s
		//
		//=====================================================================
		if (someParameters.message == 'getUserDetails') {
			var recordsStats;
			var recordReference;

			recordsStats = {};
			for (recordReference in aConnection['userData']['records']) {
				recordsStats[recordReference] = {
					'updateDate': aConnection['userData']['records'][recordReference]['updateDate']
				}
			}

			result['header'] =  this.userDetails(aConnection);
			result['statistics'] = this.statistics(aConnection);
			result['maxNumberOfRecords'] = aConnection['userData']['maxNumberOfRecords'];
			result['version'] = aConnection['userData']['userDetailsVersion'];
			result['recordsStats'] = recordsStats;
			
			if (this.isReadOnly() == false) {
				var	lock;
				
				if (typeof(aConnection['userData']['lock']) == 'undefined') {
					aConnection['userData']['lock'] = "<<LOCK>>";
				}
				
				result['lock'] = aConnection['userData']['lock'];
			}

		//=====================================================================
		} else if (someParameters.message == 'getRecordDetail') {
/*
			var	recordData;
			var currentVersionData;
			
			recordData = this.userData()['records'][someParameters['parameters']['reference']];
			result['reference'] = someParameters['parameters']['reference'];
			result['data'] = recordData['data'];
			result['version'] = recordData['version'];
			result['creationData'] = recordData['creationDate'];
			result['updateDate'] = recordData['updateDate'];
			result['accessDate'] = recordData['accessDate'];

			currentVersionData = recordData['versions'][recordData['currentVersion']];

			result['currentVersion'] = {};
			result['currentVersion']['reference'] = recordData['currentVersion'];
			result['currentVersion']['version'] = currentVersionData['version'];
			result['currentVersion']['header'] = currentVersionData['header'];
			result['currentVersion']['data'] = currentVersionData['data'];
			result['currentVersion']['creationData'] = currentVersionData['creationDate'];
			result['currentVersion']['updateDate'] = currentVersionData['updateDate'];
			result['currentVersion']['accessDate'] = currentVersionData['accessDate'];
			if (typeof(currentVersionData['previousVersion']) != 'undefined') {
				result['currentVersion']['previousVersionKey'] = currentVersionData['previousVersionKey'];
				result['currentVersion']['previousVersion'] = currentVersionData['previousVersion'];
			}
*/
			MochiKit.Base.update(result, aConnection['userData']['records'][someParameters['parameters']['reference']]);
			result['reference'] = someParameters['parameters']['reference'];

		//=====================================================================
		//
		//		R	E	A	D	-	W	R	I	T	E		M e t h o d s
		//
		//=====================================================================
		} else if (someParameters.message == 'upgradeUserCredentials') {
			if (this.isReadOnly() == false) {
				var parameters;
				var credentials;

				parameters = someParameters['parameters'];
				credentials = parameters['credentials'];

				if ((credentials['C'] == null)
				||	(credentials['s'] == null)
				||	(credentials['v'] == null)
				||	(credentials['version'] != Clipperz.PM.Connection.communicationProtocol.currentVersion)
				) {
					result = Clipperz.PM.DataModel.User.exception.CredentialUpgradeFailed;
				} else {
					var	oldCValue;
					oldCValue = aConnection['C'];

					this.data()['users'][credentials['C']] = aConnection['userData'];
					aConnection['C'] = credentials['C'];

					aConnection['userData']['s'] = credentials['s'];
					aConnection['userData']['v'] = credentials['v'];
					aConnection['userData']['version'] = credentials['version'];

					aConnection['userData']['userDetails'] = parameters['user']['header'];
					aConnection['userData']['userDetailsVersion'] = parameters['user']['version'];
					aConnection['userData']['statistics'] = parameters['user']['statistics'];

					aConnection['userData']['lock'] = parameters['user']['lock'];
					
					delete this.data()['users'][oldCValue];

					result = {result:"done", parameters:parameters};
				}
			} else {
				throw Clipperz.PM.Proxy.Offline.DataStore.exception.ReadOnly;
			}

		//=====================================================================

		} else if (someParameters.message == 'saveChanges') {
			if (this.isReadOnly() == false) {
				var i, c;

				if (aConnection['userData']['lock']	!= someParameters['parameters']['user']['lock']) {
					throw "the lock attribute is not processed correctly"
				}

				aConnection['userData']['userDetails']			= someParameters['parameters']['user']['header'];
				aConnection['userData']['statistics']			= someParameters['parameters']['user']['statistics'];
				aConnection['userData']['userDetailsVersion']	= someParameters['parameters']['user']['version'];

				c = someParameters['parameters']['records']['updated'].length;
				for (i=0; i<c; i++) {
					var currentRecord;
					var currentRecordData;

					currentRecordData = someParameters['parameters']['records']['updated'][i];
					currentRecord = aConnection['userData']['records'][currentRecordData['record']['reference']];

					if (
						(typeof(aConnection['userData']['records'][currentRecordData['record']['reference']]) == 'undefined')
						&&
						(typeof(currentRecordData['currentRecordVersion']) == 'undefined')
					) {
						throw "Record added without a recordVersion";
					}

					if (currentRecord == null) {
						currentRecord = {};
						currentRecord['versions'] = {};
						currentRecord['creationDate']	= Clipperz.PM.Date.formatDateWithUTCFormat(new Date());
						currentRecord['accessDate']		= Clipperz.PM.Date.formatDateWithUTCFormat(new Date());

						aConnection['userData']['records'][currentRecordData['record']['reference']] = currentRecord;
					}
					
					currentRecord['data']		= currentRecordData['record']['data'];
					currentRecord['version']	= currentRecordData['record']['version'];
					currentRecord['updateDate']	= Clipperz.PM.Date.formatDateWithUTCFormat(new Date());

					if (typeof(currentRecordData['currentRecordVersion']) != 'undefined') {
						currentRecord['currentVersion'] = currentRecordData['currentRecordVersion']['reference'];
						currentRecord['versions'][currentRecordData['currentRecordVersion']['reference']] = {
							'data':					currentRecordData['currentRecordVersion']['data'],
							'version':				currentRecordData['currentRecordVersion']['version'],
							'previousVersion':		currentRecordData['currentRecordVersion']['previousVersion'],
							'previousVersionKey':	currentRecordData['currentRecordVersion']['previousVersionKey'],
							'creationDate':	Clipperz.PM.Date.formatDateWithUTCFormat(new Date()),
							'updateDate':	Clipperz.PM.Date.formatDateWithUTCFormat(new Date()),
							'accessDate':	Clipperz.PM.Date.formatDateWithUTCFormat(new Date())
						}
					}
				}

				c = someParameters['parameters']['records']['deleted'].length;
				for (i=0; i<c; i++) {
					var currentRecordReference;

					currentRecordReference = someParameters['parameters']['records']['deleted'][i];
					delete aConnection['userData']['records'][currentRecordReference];
				}

				aConnection['userData']['lock'] = Clipperz.PM.Crypto.randomKey();
				result['lock'] = aConnection['userData']['lock'];
				result['result'] = 'done';
			} else {
				throw Clipperz.PM.Proxy.Offline.DataStore.exception.ReadOnly;
			}

		//=====================================================================
		//
		//		U	N	H	A	N	D	L	E	D		M e t h o d
		//
		//=====================================================================
		} else {
			Clipperz.logError("Clipperz.PM.Proxy.Test.message - unhandled message: " + someParameters.message);
		}
	
		result = {
			result: result,
			toll:   this.getTollForRequestType('MESSAGE')
		}

//		return MochiKit.Async.succeed(result);
		return result;
	},

	//-------------------------------------------------------------------------

	'_logout': function(someParameters) {
//		return MochiKit.Async.succeed({result: 'done'});
		return {result: 'done'};
	},

	//=========================================================================
	//#########################################################################
/*
	'userDetails': function(aConnection) {
		var result;
		
		if (this.isTestData(aConnection)) {
			var serializedHeader;
			var version;

//Clipperz.logDebug("### test data");
			version = aConnection['userData']['userDetailsVersion'];
			serializedHeader = Clipperz.Base.serializeJSON(aConnection['userData']['userDetails']);
			result = Clipperz.PM.Crypto.encryptingFunctions.versions[version].encrypt(aConnection['userData']['__masterkey_test_value__'], serializedHeader);
		} else {
//Clipperz.logDebug("### NOT test data");
 			result = aConnection['userData']['userDetails'];
		}
		
		return result;
	},
	
	'statistics': function(aConnection) {
		var result;

		if (aConnection['userData']['statistics'] != null) {
			if (this.isTestData(aConnection)) {
				var serializedStatistics;
				var version;

				version = aConnection['userData']['userDetailsVersion'];
				serializedStatistics = Clipperz.Base.serializeJSON(aConnection['userData']['statistics']);
				result = Clipperz.PM.Crypto.encryptingFunctions.versions[version].encrypt(aConnection['userData']['__masterkey_test_value__'], serializedStatistics);
			} else {
			 	result = aConnection['userData']['statistics'];
			}
		} else {
			result = null;
		}
		
		return result;
	},
*/
	//=========================================================================
	__syntaxFix__: "syntax fix"
});

