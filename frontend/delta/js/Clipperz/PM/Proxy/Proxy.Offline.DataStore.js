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

try { if (typeof(Clipperz.PM.Proxy.Offline) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.PM.Proxy.Offline.DataStore depends on Clipperz.PM.Proxy.Offline!";
}  

//=============================================================================

Clipperz.PM.Proxy.Offline.DataStore = function(args) {
	args = args || {};
	
	this._data = args.data || (typeof(_clipperz_dump_data_) != 'undefined' ? _clipperz_dump_data_ : null);
	this._isReadOnly = (typeof(args.readOnly) == 'undefined' ? true : args.readOnly);
	this._shouldPayTolls = args.shouldPayTolls || false;

	this._tolls = {};
	this._currentStaticConnection = null;

	return this;
}

Clipperz.Base.extend(Clipperz.PM.Proxy.Offline.DataStore, Object, {

	//-------------------------------------------------------------------------

	'isReadOnly': function () {
		return this._isReadOnly;
	},
	
	'canRegisterNewUsers': function () {
		return false;
	},

	
	//-------------------------------------------------------------------------

	'shouldPayTolls': function() {
		return this._shouldPayTolls;
	},

	//-------------------------------------------------------------------------

	'data': function () {
		return this._data;
	},

	//-------------------------------------------------------------------------

	'tolls': function () {
		return this._tolls;
	},

	//=========================================================================

	'resetData': function() {
		this._data = {
			'users': {
				'catchAllUser': {
					__masterkey_test_value__: 'masterkey',
					s: '112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00',
					v: '112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00'
				}
			}
		};
	},

	//-------------------------------------------------------------------------

	'setupWithEncryptedData': function(someData) {
		this._data = Clipperz.Base.deepClone(someData);
	},

	//-------------------------------------------------------------------------

	// Should this be updated to include OTP field?
	'setupWithData': function(someData) {
		var deferredResult;
		var resultData;
		var i, c;

//Clipperz.log(">>> Proxy.Test.setupWithData");
		resultData = this._data;

		deferredResult = new Clipperz.Async.Deferred("Proxy.Test.seupWithData", {trace:false});
		c = someData['users'].length;

		for (i=0; i<c; i++) {
			var	newConnection;
			var	recordConfiguration;

			deferredResult.addMethod(this, 'userSerializedEncryptedData', someData['users'][i]);
			deferredResult.addCallback(MochiKit.Base.bind(function(aUserSerializationContext) {
				resultData['users'][aUserSerializationContext['credentials']['C']] = {
					's':		aUserSerializationContext['credentials']['s'],
					'v':		aUserSerializationContext['credentials']['v'],
					'version':				aUserSerializationContext['data']['connectionVersion'],
					'userDetails':			aUserSerializationContext['encryptedData']['user']['header'],
					'userDetailsVersion':	aUserSerializationContext['encryptedData']['user']['version'],
					'statistics':			aUserSerializationContext['encryptedData']['user']['statistics'],
					'lock':					aUserSerializationContext['encryptedData']['user']['lock'],
					'records': 				this.rearrangeRecordsData(aUserSerializationContext['encryptedData']['records'])
				}
			}, this));
		}
		
		deferredResult.addCallback(MochiKit.Base.bind(function() {
			this._data = resultData;
		}, this));
		
		deferredResult.callback();
//Clipperz.log("<<< Proxy.Test.setupWithData");

		return deferredResult;
	},

	//=========================================================================

	'getTollForRequestType': function (aRequestType) {
		var	result;
		var	targetValue;
		var cost;
		
		targetValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32).toHexString().substring(2);
		switch (aRequestType) {
			case 'REGISTER':
				cost = 5;
				break;
			case 'CONNECT':
				cost = 5;
				break;
			case 'MESSAGE':
				cost = 2;
				break;
		}
		
		result = {
				requestType: aRequestType,
				targetValue: targetValue,
				cost: cost
		}

		if (this.shouldPayTolls()) {
			this.tolls()[targetValue] = result;
		}

		return result;
	},

	//-------------------------------------------------------------------------

	'checkToll': function (aFunctionName, someParameters) {
		if (this.shouldPayTolls()) {
			var localToll;
			var	tollParameters;
			
			tollParameters = someParameters['toll'];
			localToll = this.tolls()[tollParameters['targetValue']];
			
			if (localToll != null) {
				if (! Clipperz.PM.Toll.validate(tollParameters['targetValue'], tollParameters['toll'], localToll['cost'])) {
					throw "Toll value too low.";
				};
			} else {
				throw "Missing toll";
			}
		}
	},

	//=========================================================================

	'currentStaticConnection': function () {
		if (this._currentStaticConnection == null) {
			this._currentStaticConnection = {};
		}
		
		return this._currentStaticConnection;
	},

	//-------------------------------------------------------------------------

	'getConnectionForRequest': function (aFunctionName, someParameters) {
		var	result;

		if (this.shouldPayTolls()) {
			if ((typeof(someParameters['toll']) != 'undefined') && (typeof(someParameters['toll']['targetValue']) != 'undefined')) {
				result = this.tolls()[someParameters['toll']['targetValue']]['connection'];
				if (typeof(result) == 'undefined') {
					result = {};
				}
			} else {
				result = {};
			}
		} else {
			result = this.currentStaticConnection();
		}

		return result;
	},

	//-------------------------------------------------------------------------

	'storeConnectionForRequestWithConnectionAndResponse': function (aFunctionName, someParameters, aConnection, aResponse) {
		if (this.shouldPayTolls()) {
			if ((typeof(aResponse['toll']) != 'undefined')
			 &&	(typeof(aResponse['toll']['targetValue']) != 'undefined')
			 &&	(typeof(this.tolls()[aResponse['toll']['targetValue']]) != 'undefined')
			) {
				this.tolls()[aResponse['toll']['targetValue']]['connection'] = aConnection;
			}
		}
	},

	//=========================================================================

	'processMessage': function (aFunctionName, someParameters) {
		var result;
		var	connection;

		connection = this.getConnectionForRequest(aFunctionName, someParameters);

		switch(aFunctionName) {
			case 'knock':
				result = this._knock(connection, someParameters);
				break;
			case 'registration':
				this.checkToll(aFunctionName, someParameters);
				result = this._registration(connection, someParameters.parameters);
				break;
			case 'handshake':
				this.checkToll(aFunctionName, someParameters);
				result = this._handshake(connection, someParameters.parameters);
				break;
			case 'message':
				this.checkToll(aFunctionName, someParameters);
				result = this._message(connection, someParameters.parameters);
				break;
			case 'logout':
				this._currentStaticConnection = null;
				result = this._logout(connection, someParameters.parameters);
				break;
		}

		this.storeConnectionForRequestWithConnectionAndResponse(aFunctionName, someParameters, connection, result);

		return MochiKit.Async.succeed(result);
	},

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
		if (this.isReadOnly() == false) {
			if (typeof(this.data()['users'][someParameters['credentials']['C']]) == 'undefined') {
				this.data()['users'][someParameters['credentials']['C']] = {
					's':		someParameters['credentials']['s'],
					'v':		someParameters['credentials']['v'],
					'version':	someParameters['credentials']['version'],
//					'lock':		Clipperz.Crypto.Base.generateRandomSeed(),
					'userDetails':			someParameters['user']['header'],
					'statistics':			someParameters['user']['statistics'],
					'userDetailsVersion':	someParameters['user']['version'],
					'records':	{},
					'accountInfo': Clipperz.PM.Proxy.Offline.DataStore.defaultAccountInfo
				}
			} else {
				throw "user already exists";
			}
		} else {
		throw Clipperz.PM.Proxy.Offline.DataStore.exception.ReadOnly;
		}

		result = {
			result: {
				'lock':		this.data()['users'][someParameters['credentials']['C']]['lock'],
				'result':	'done'
	},
			toll:   this.getTollForRequestType('CONNECT')
		}

		return result;
	},

	//-------------------------------------------------------------------------

	'_handshake': function(aConnection, someParameters) {
		var result;
		var	nextTollRequestType;

		result = {};
		if (someParameters.message == "connect") {
			var userData;
			var otpsData, userOTPs;
			var randomBytes;
			var v;

			if (typeof(this.data()['onetimePasswords']) == 'undefined') {
				this.data()['onetimePasswords'] = {};
			}

			userData = this.data()['users'][someParameters.parameters.C];
			otpsData = this.data()['onetimePasswords'];

//console.log("Proxy.Offline.DataStore._handshake: otpsData:", otpsData);

			userOTPs = {};
			MochiKit.Base.map(function(aOTP) {
				if (aOTP['user'] == someParameters.parameters.C) {
					userOTPs[aOTP['key']] = aOTP;
				}
			},MochiKit.Base.values(otpsData));

//console.log("Proxy.Offline.DataStore._handshake: userOTPs:", userOTPs);
//console.log("Proxy.Offline.DataStore._handshake(): userOTPs:",userOTPs);

			if ((typeof(userData) != 'undefined') && (userData['version'] == someParameters.version)) {
				aConnection['userData'] = userData;
				aConnection['userOTPs'] = userOTPs;
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
				result['accountInfo'] = aConnection['userData']['accountInfo'];
			} else {
				throw new Error("Client checksum verification failed! Expected <" + M1 + ">, received <" + someParameters.parameters.M1 + ">.", "Error");
			}

			nextTollRequestType = 'MESSAGE';
		} else if (someParameters.message == "oneTimePassword") {
			var otpData = this.getOneTimePasswordFromKey(someParameters.parameters.oneTimePasswordKey);			

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
// console.log("connect.oneTimePassword: Exception!", exception);
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
					'accessDate': aConnection['userData']['records'][recordReference]['accessDate'],
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
			MochiKit.Base.update(result, aConnection['userData']['records'][someParameters['parameters']['reference']]);
			result['reference'] = someParameters['parameters']['reference'];
		} else if (someParameters.message == 'getAllRecordDetails') {
			MochiKit.Base.update(result, aConnection['userData']['records']);
		} else if (someParameters.message == 'getOneTimePasswordsDetails') {
			var result = MochiKit.Iter.reduce(function(prev, cur){
				prev[cur.reference] = {
					'status': cur.status,
					'usageDate': cur.usage_date,
					'connection': {
						'ip': 'n/a',
						'browser': 'n/a'
					}
				};
				return prev;
			}, MochiKit.Base.values(aConnection['userOTPs']), {});

			MochiKit.Base.update(result, result);
// console.log("Proxy.Offline.DataStore.getOneTimePasswordsDetails:",result);
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
					var key;
					var onetimePasswords = this.data()['onetimePasswords'];
					var oldCValue = aConnection['C'];

					this.data()['users'][credentials['C']] = aConnection['userData'];
					aConnection['C'] = credentials['C'];

					aConnection['userData']['s'] = credentials['s'];
					aConnection['userData']['v'] = credentials['v'];
					aConnection['userData']['version'] = credentials['version'];

					aConnection['userData']['userDetails'] = parameters['user']['header'];
					aConnection['userData']['userDetailsVersion'] = parameters['user']['version'];
					aConnection['userData']['statistics'] = parameters['user']['statistics'];

					aConnection['userData']['lock'] = parameters['user']['lock'];

					for (key in onetimePasswords) {
						onetimePasswords[key]['user']    = credentials['C'];
						onetimePasswords[key]['data']    = parameters['oneTimePasswords'][key]['data'];
						onetimePasswords[key]['version'] = parameters['oneTimePasswords'][key]['version'];
					}
					
					delete this.data()['users'][oldCValue];

					result = {result:"done", parameters:parameters};
				}
			} else {
				throw Clipperz.PM.Proxy.Offline.DataStore.exception.ReadOnly;
			}
		//=====================================================================
/*		} else if (someParameters.message == 'updateData') {
			if (this.isReadOnly() == false) {
				var i, c;

				if (this.userData()['lock']	!= someParameters['parameters']['user']['lock']) {
					throw "the lock attribute is not processed correctly"
				}

				this.userData()['userDetails']			= someParameters['parameters']['user']['header'];
				this.userData()['statistics']			= someParameters['parameters']['user']['statistics'];
				this.userData()['userDetailsVersions']	= someParameters['parameters']['user']['version'];

				c = someParameters['parameters']['records'].length;
				for (i=0; i<c; i++) {
					var currentRecord;
					var currentRecordData;

					currentRecordData = someParameters['parameters']['records'][i];
					currentRecord = this.userData()['records'][currentRecordData['record']['reference']];
					
					if (currentRecord == null) {
					}
					
					currentRecord['data'] = currentRecordData['record']['data'];
					currentRecord['version'] = currentRecordData['record']['version'];
					currentRecord['currentVersion'] = currentRecordData['currentRecordVersion']['reference'];
					
					currentRecord['versions'][currentRecordData['currentRecordVersion']['reference']] = {
						'data':					currentRecordData['currentRecordVersion']['data'],
						'version':				currentRecordData['currentRecordVersion']['version'],
						'previousVersion':		currentRecordData['currentRecordVersion']['previousVersion'],
						'previousVersionKey':	currentRecordData['currentRecordVersion']['previousVersionKey']
					}
				}

				this.userData()['lock'] = Clipperz.PM.Crypto.randomKey();
				result['lock'] = this.userData()['lock'];
				result['result'] = 'done';
			} else {
				throw Clipperz.PM.Proxy.Offline.DataStore.exception.ReadOnly;
			}
*/		//=====================================================================
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
		} else if (someParameters.message == 'addNewOneTimePassword') {
			if (this.isReadOnly() == false) {
//console.log("Proxy.Offline.DataStore.addNewOneTimePassword: someParameters:", someParameters);

				var otpKey = someParameters['parameters']['oneTimePassword'].key;
				var otpReference = someParameters['parameters']['oneTimePassword'].reference;

				if (aConnection['userData']['lock']	!= someParameters['parameters']['user']['lock']) {
					throw "the lock attribute is not processed correctly"
				}

				aConnection['userData']['userDetails']			= someParameters['parameters']['user']['header'];
				aConnection['userData']['statistics']			= someParameters['parameters']['user']['statistics'];
				aConnection['userData']['userDetailsVersion']	= someParameters['parameters']['user']['version'];

				aConnection['userOTPs'][otpKey] = someParameters['parameters']['oneTimePassword'];
				aConnection['userOTPs'][otpKey]['user'] = aConnection['C'];
				aConnection['userOTPs'][otpKey]['status'] = 'ACTIVE';
				aConnection['userOTPs'][otpKey]['creation_date'] = new Date().toISOString().substr(0, 19).replace('T', ' '); // Not an elegant way to give the date the same format as the others
				aConnection['userOTPs'][otpKey]['request_date'] = "4001-01-01 09:00:00";
				aConnection['userOTPs'][otpKey]['usage_date'] = "4001-01-01 09:00:00";

				this.data()['onetimePasswords'][otpReference] = aConnection['userOTPs'][otpKey];
//console.log("Proxy.Offline.DataStore.addNewOneTimePassword: aConnection:", aConnection);
			} else {
				throw Clipperz.PM.Proxy.Offline.DataStore.exception.ReadOnly;
			}
			
		} else if (someParameters.message == 'updateOneTimePasswords') {
			if (this.isReadOnly() == false) {
//console.log("Proxy.Offline.DataStore.updateOneTimePasswords: someParameters:", someParameters);

				if (aConnection['userData']['lock']	!= someParameters['parameters']['user']['lock']) {
					throw "the lock attribute is not processed correctly"
				}

				aConnection['userData']['userDetails']			= someParameters['parameters']['user']['header'];
				aConnection['userData']['statistics']			= someParameters['parameters']['user']['statistics'];
				aConnection['userData']['userDetailsVersion']	= someParameters['parameters']['user']['version'];

//console.log("Proxy.Offline.DataStore.updateOneTimePasswords: userOTPs:", aConnection['userOTPs']);

				MochiKit.Base.map(function(aOTP) {
					if (someParameters['parameters']['oneTimePasswords'].indexOf(aOTP.reference) < 0) {
						delete aConnection['userOTPs'][aOTP.key];
					}
				},MochiKit.Base.values(aConnection['userOTPs']));
			} else {
				throw Clipperz.PM.Proxy.Offline.DataStore.exception.ReadOnly;
			}			
		//=====================================================================
		//
		//		U	N	H	A	N	D	L	E	D		M e t h o d
		//
		//=====================================================================
		} else {
			Clipperz.logError("Clipperz.PM.Proxy.Test.message - unhandled message (Proxy.Offline.DataStore): " + someParameters.message);
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

	'getOneTimePasswordFromKey': function(aKey) {
		var result,i;

		var onetimePasswordsValues = MochiKit.Base.values(this.data()['onetimePasswords']);

		for (i = 0; i < onetimePasswordsValues.length; i++) {
			if (onetimePasswordsValues[i]['key'] == aKey) {
				result = onetimePasswordsValues[i];
			}
		}

		return result;
	},

	//=========================================================================

	'isTestData': function(aConnection) {
		return (typeof(aConnection['userData']['__masterkey_test_value__']) != 'undefined');
	},

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

/*
	'userSerializedEncryptedData': function(someData) {
		var deferredResult;
		var deferredContext;
		
		deferredContext = { 'data': someData };
		
		deferredResult = new Clipperz.Async.Deferred('Proxy.Test.serializeUserEncryptedData', {trace:false});
		deferredResult.addCallback(MochiKit.Base.bind(function(aDeferredContext) {
			aDeferredContext['user'] = this.createUserUsingConfigurationData(aDeferredContext['data']);
			return aDeferredContext;
		}, this));
		deferredResult.addCallback(function(aDeferredContext) {
//			return aDeferredContext['user'].encryptedDataUsingVersion(aDeferredContext['data']['version']);
			return aDeferredContext['user'].serializedDataUsingVersion(MochiKit.Base.values(aDeferredContext['user'].records()), aDeferredContext['data']['version']);
		});
		deferredResult.addCallback(function(aUserEncryptedData) {
			deferredContext['encryptedData'] = aUserEncryptedData;
			return deferredContext;
		});
		deferredResult.addCallback(function(aDeferredContext) {
			var connection;

			connection = new Clipperz.PM.Connection.communicationProtocol.versions[aDeferredContext['data']['connectionVersion']]()
			aDeferredContext['credentials'] = connection.serverSideUserCredentials(aDeferredContext['user'].username(),aDeferredContext['user'].passphrase());

			return aDeferredContext;
		});

//		deferredResult.addCallback(function(aDeferredContext) {
//			return aDeferredContext['user'].serializedDataUsingVersion(MochiKit.Base.values(aDeferredContext['user'].records()), aDeferredContext['data']['version']);
//		}, deferredContext);
//		deferredResult.addCallback(function(aUserSerializedData) {
//		});
//
//		deferredResult.addCallback(MochiKit.Async.succeed, deferredContext);
		deferredResult.callback(deferredContext);
		
		return deferredResult;
	},

	'createUserUsingConfigurationData': function(someData) {
		var result;
		var user;
		var recordLabel;

		user = new Clipperz.PM.DataModel.User();
		user.initForTests();
		user.setUsername(someData['username']);
		user.setPassphrase(someData['passphrase']);

		for (recordLabel in someData['records']) {
			var recordData;
			var record;
			var i, c;
			
			recordData = someData['records'][recordLabel];
			record = new Clipperz.PM.DataModel.Record({user:user, label:recordLabel});
			record.setNotes(recordData['notes']);
			
			c = recordData['fields'].length;
			for (i=0; i<c; i++) {
				var recordField;
				
				recordField = new Clipperz.PM.DataModel.RecordField();
				recordField.setLabel(recordData['fields'][i]['name']);
				recordField.setValue(recordData['fields'][i]['value']);
				recordField.setType(recordData['fields'][i]['type']);
				record.addField(recordField);
			}
			user.addRecord(record, true);
		}
		
		result = user;
		
		return result;
	},
*/
	//=========================================================================
	__syntaxFix__: "syntax fix"
});

Clipperz.PM.Proxy.Offline.DataStore['exception'] = {
	'ReadOnly': 		new MochiKit.Base.NamedError("Clipperz.PM.Proxy.Offline.DataStore.exception.ReadOnly")
};

Clipperz.PM.Proxy.Offline.DataStore.defaultAccountInfo = {
	'features': [
		'UPDATE_CREDENTIALS',
		'EDIT_CARD',
		'CARD_DETAILS',
		'ADD_CARD',
		'DELETE_CARD',
		'OFFLINE_COPY',
		'LIST_CARDS'
	],
	'paymentVerificationPending': false,
	'currentSubscriptionType': 'EARLY_ADOPTER',
	'isExpiring': false,
	'latestActiveLevel': 'EARLY_ADOPTER',
	'payments': [],
	'featureSet': 'FULL',
	'latestActiveThreshold': '-1.00000000',
	'referenceDate': 'Fri, 03 April 2015 08:17:46 UTC',
	'isExpired': false,
	'expirationDate': 'Mon, 01 January 4001 00:00:00 UTC'
};