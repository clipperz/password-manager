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
	this._connections = {};

	this._C = null;
	this._b = null;
	this._B = null;
	this._A = null;
	this._userData = null;

	return this;
}

//Clipperz.Base.extend(Clipperz.PM.Proxy.Offline.DataStore, Object, {
Clipperz.PM.Proxy.Offline.DataStore.prototype = MochiKit.Base.update(null, {

	//-------------------------------------------------------------------------

	'isReadOnly': function () {
		return this._isReadOnly;
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

	//-------------------------------------------------------------------------

	'connections': function () {
		return this._connections;
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

	'setupWithData': function(someData) {
		var deferredResult;
		var resultData;
		var i, c;

//Clipperz.log(">>> Proxy.Test.setupWithData");
		resultData = this._data;

		deferredResult = new MochiKit.Async.Deferred();
		c = someData['users'].length;

		for (i=0; i<c; i++) {
			var	newConnection;
			var	recordConfiguration;

			deferredResult.addCallback(MochiKit.Base.method(this, 'userSerializedEncryptedData', someData['users'][i]));
			deferredResult.addCallback(MochiKit.Base.bind(function(aUserSerializationContext) {
//console.log("SERIALIZED USER", aUserSerializationContext);
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
//console.log("this._data", resultData);
			this._data = resultData;
		}, this));
		
		deferredResult.callback();
//Clipperz.log("<<< Proxy.Test.setupWithData");

		return deferredResult;
	},

	//=========================================================================

	'C': function() {
		return this._C;
	},

	'set_C': function(aValue) {
		this._C = aValue;
	},

	//-------------------------------------------------------------------------

	'b': function() {
		return this._b;
	},

	'set_b': function(aValue) {
		this._b = aValue;
	},

	//-------------------------------------------------------------------------

	'B': function() {
		return this._B;
	},

	'set_B': function(aValue) {
		this._B = aValue;
	},

	//-------------------------------------------------------------------------

	'A': function() {
		return this._A;
	},

	'set_A': function(aValue) {
		this._A = aValue;
	},

	//-------------------------------------------------------------------------

	'userData': function() {
		return this._userData;
	},
	
	'setUserData': function(aValue) {
		this._userData = aValue;
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

	'processMessage': function (aFunctionName, someParameters) {
		var result;

		switch(aFunctionName) {
			case 'knock':
				result = this._knock(someParameters);
				break;
			case 'registration':
				this.checkToll(aFunctionName, someParameters);
				result = this._registration(someParameters.parameters);
				break;
			case 'handshake':
				this.checkToll(aFunctionName, someParameters);
				result = this._handshake(someParameters.parameters);
				break;
			case 'message':
				this.checkToll(aFunctionName, someParameters);
				result = this._message(someParameters.parameters);
				break;
			case 'logout':
				result = this._logout(someParameters.parameters);
				break;
		}

		return result;
	},

	//=========================================================================

	'_knock': function(someParameters) {
		var result;
		
		result = {
			toll: this.getTollForRequestType(someParameters['requestType'])
//			toll: {
//				requestType: someParameters['requestType'],
//				targetValue: "3a1ba0be23580f902885c6c8a6b035e228ed1ca74d77de5f9bb0e0c899f07cfe",
//				cost: 
//			}
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'_registration': function(someParameters) {
//console.log("_registration", someParameters);
		if (this.isReadOnly() == false) {
			if (typeof(this.data()['users'][someParameters['credentials']['C']]) == 'undefined') {
				this.data()['users'][someParameters['credentials']['C']] = {
					's':		someParameters['credentials']['s'],
					'v':		someParameters['credentials']['v'],
					'version':	someParameters['credentials']['version'],
	//				'lock':		someParameters['user']['lock'],
					'lock':		Clipperz.Crypto.Base.generateRandomSeed(),
	//				'maxNumberOfRecords':	'100',
					'userDetails':			someParameters['user']['header'],
					'statistics':			someParameters['user']['statistics'],
					'userDetailsVersion':	someParameters['user']['version'],
					'records':	{}
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

		return MochiKit.Async.succeed(result);
	},

	//-------------------------------------------------------------------------

	'_handshake': function(someParameters) {
		var result;
		var	nextTollRequestType;

//Clipperz.log(">>> Proxy.Offline.DataStore._handshake");
		result = {};
		if (someParameters.message == "connect") {
			var userData;
			var randomBytes;
			var b, B, v;

//console.log(">>> Proxy.Offline.DataStore._handshake.connect", someParameters);
			userData = this.data()['users'][someParameters.parameters.C];
			
			if ((typeof(userData) != 'undefined') && (userData['version'] == someParameters.version)) {
				this.setUserData(userData);
			} else {
				this.setUserData(this.data()['users']['catchAllUser']);
			}

			randomBytes = Clipperz.Crypto.Base.generateRandomSeed();
			this.set_C(someParameters.parameters.C);
			this.set_b(new Clipperz.Crypto.BigInt(randomBytes, 16));
			v = new Clipperz.Crypto.BigInt(this.userData()['v'], 16);
			this.set_B((Clipperz.Crypto.SRP.k().multiply(v)).add(Clipperz.Crypto.SRP.g().powerModule(this.b(), Clipperz.Crypto.SRP.n())));
			
			this.set_A(someParameters.parameters.A);
			
			result['s'] = this.userData()['s'];
			result['B'] = this.B().asString(16);
			
			nextTollRequestType = 'CONNECT';
		} else if (someParameters.message == "credentialCheck") {
			var v, u, s, S, A, K, M1;
			var stringHash = function (aValue) {
				return Clipperz.PM.Crypto.encryptingFunctions.versions[someParameters.version].hash(new Clipperz.ByteArray(aValue)).toHexString().substring(2);
			};
			
//console.log(">>> Proxy.Offline.DataStore._handshake.credentialCheck", someParameters);
			v = new Clipperz.Crypto.BigInt(this.userData()['v'], 16);
			A = new Clipperz.Crypto.BigInt(this.A(), 16);
			u = new Clipperz.Crypto.BigInt(Clipperz.PM.Crypto.encryptingFunctions.versions[someParameters.version].hash(new Clipperz.ByteArray(A.asString(10) + this.B().asString(10))).toHexString(), 16);
			s = new Clipperz.Crypto.BigInt(this.userData()['s'], 16);
			S = (A.multiply(v.powerModule(u, Clipperz.Crypto.SRP.n()))).powerModule(this.b(), Clipperz.Crypto.SRP.n());

			K = stringHash(S.asString(10));

			M1 = stringHash(
				"597626870978286801440197562148588907434001483655788865609375806439877501869636875571920406529" +
				stringHash(this.C()) +
				s.asString(10) +
				A.asString(10) +
				this.B().asString(10) +
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
			
//console.log("HANDSHAKE WITH OTP", someParameters.parameters.oneTimePasswordKey);
//console.log("someParameters", someParameters);
//console.log("data.OTP", Clipperz.Base.serializeJSON(this.data()['onetimePasswords']));
			otpData = this.data()['onetimePasswords'][someParameters.parameters.oneTimePasswordKey];

			try {
				if (typeof(otpData) != 'undefined') {
					if (otpData['status'] == 'ACTIVE') {
						if (otpData['key_checksum'] == someParameters.parameters.oneTimePasswordKeyChecksum) {
							result = {
								'data':		otpData['data'],
								'version':	otpData['version']
							}

							otpData['status'] = 'REQUESTED';
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
			MochiKit.Logging.logError("Clipperz.PM.Proxy.Test.handshake - unhandled message: " + someParameters.message);
		}
//console.log("<<< Proxy.Offline._handshake", result);

		result = {
			result: result,
			toll:   this.getTollForRequestType(nextTollRequestType)
		}

		return MochiKit.Async.succeed(result);
	},

	//-------------------------------------------------------------------------

	'_message': function(someParameters) {
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

//try {		
			recordsStats = {};
			for (recordReference in this.userData()['records']) {
				recordsStats[recordReference] = {
					'updateDate': this.userData()['records'][recordReference]['updateDate']
				}
			}

			result['header'] =  this.userDetails();
			result['statistics'] = this.statistics();
			result['maxNumberOfRecords'] = this.userData()['maxNumberOfRecords'];
			result['version'] = this.userData()['userDetailsVersion'];
			result['recordsStats'] = recordsStats;
			
			if (this.isReadOnly() == false) {
				var	lock;
				
				if (typeof(this.userData()['lock']) == 'undefined') {
					this.userData()['lock'] = "<<LOCK>>";
				}
				
				result['lock'] = this.userData()['lock'];
			}
//} catch (exception) {
//	console.log("*#*#*#*#*#*#*", exception);
//	throw exception;
//}
		//=====================================================================
		} else if (someParameters.message == 'getRecordDetail') {
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

		//=====================================================================
		//
		//		R	E	A	D	-	W	R	I	T	E		M e t h o d s
		//
		//=====================================================================
		} else if (someParameters.message == 'upgradeUserCredentials') {
			if (this.isReadOnly() == false) {
				var parameters;
				parameters = someParameters.parameters;

				if (parameters['C'] == null) {
					result = Clipperz.PM.DataModel.User.exception.CredentialUpgradeFailed;
				} else if (parameters['s'] == null) {
					result = Clipperz.PM.DataModel.User.exception.CredentialUpgradeFailed;
				} else if (parameters['v'] == null) {
					result = Clipperz.PM.DataModel.User.exception.CredentialUpgradeFailed;
				} else if (parameters['version'] != Clipperz.PM.Connection.communicationProtocol.currentVersion) {
					result = Clipperz.PM.DataModel.User.exception.CredentialUpgradeFailed;
				} else {
					result = {result:"done", parameters:parameters};
				}
			} else {
				throw Clipperz.PM.Proxy.Offline.DataStore.exception.ReadOnly;
			}
		//=====================================================================
/*		} else if (someParameters.message == 'updateData') {
			if (this.isReadOnly() == false) {
				var i, c;

//console.log("###===============================================================");
//console.log("###>>>", Clipperz.Base.serializeJSON(someParameters));
//console.log("###--- userData", Clipperz.Base.serializeJSON(this.userData()));
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
//console.log("###<<< userData", Clipperz.Base.serializeJSON(this.userData()));
			} else {
				throw Clipperz.PM.Proxy.Offline.DataStore.exception.ReadOnly;
			}
*/		//=====================================================================
		} else if (someParameters.message == 'saveChanges') {
			if (this.isReadOnly() == false) {
				var i, c;

//console.log("###===============================================================");
//console.log("###>>>", someParameters);
//console.log("###>>>", Clipperz.Base.serializeJSON(someParameters));
//console.log("###--- userData", Clipperz.Base.serializeJSON(this.userData()));
//console.log("###===============================================================");
//console.log("--- userData.lock  ", this.userData()['lock']);
//console.log("--- parameters.lock", someParameters['parameters']['user']['lock']);
				if (this.userData()['lock']	!= someParameters['parameters']['user']['lock']) {
					throw "the lock attribute is not processed correctly"
				}

				this.userData()['userDetails']			= someParameters['parameters']['user']['header'];
				this.userData()['statistics']			= someParameters['parameters']['user']['statistics'];
				this.userData()['userDetailsVersions']	= someParameters['parameters']['user']['version'];

				c = someParameters['parameters']['records']['updated'].length;
				for (i=0; i<c; i++) {
					var currentRecord;
					var currentRecordData;

					currentRecordData = someParameters['parameters']['records']['updated'][i];
					currentRecord = this.userData()['records'][currentRecordData['record']['reference']];

					if (
						(typeof(this.userData()['records'][currentRecordData['record']['reference']]) == 'undefined')
						&&
						(typeof(currentRecordData['currentRecordVersion']) == 'undefined')
					) {
//console.log("######## SHIT HAPPENS");
						throw "Record added without a recordVersion";
					}

					if (currentRecord == null) {
						currentRecord = {};
						currentRecord['versions'] = {};
						currentRecord['creationDate']	= Clipperz.PM.Date.formatDateWithUTCFormat(new Date());
						currentRecord['accessDate']		= Clipperz.PM.Date.formatDateWithUTCFormat(new Date());

						this.userData()['records'][currentRecordData['record']['reference']] = currentRecord;
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
//console.log("DELETING records", currentRecordReference);
					delete this.userData()['records'][currentRecordReference];
				}

				this.userData()['lock'] = Clipperz.PM.Crypto.randomKey();
				result['lock'] = this.userData()['lock'];
				result['result'] = 'done';
//console.log("###<<< userData", Clipperz.Base.serializeJSON(this.userData()));
			} else {
				throw Clipperz.PM.Proxy.Offline.DataStore.exception.ReadOnly;
			}

		//=====================================================================
		//
		//		U	N	H	A	N	D	L	E	D		M e t h o d
		//
		//=====================================================================
		} else {
			MochiKit.Logging.logError("Clipperz.PM.Proxy.Test.message - unhandled message: " + someParameters.message);
		}
	
		result = {
			result: result,
			toll:   this.getTollForRequestType('MESSAGE')
		}

		return MochiKit.Async.succeed(result);
	},

	//-------------------------------------------------------------------------

	'_logout': function(someParameters) {
		return MochiKit.Async.succeed({result: 'done'});
	},

	//=========================================================================
	//#########################################################################

	'isTestData': function() {
		return (typeof(this.userData()['__masterkey_test_value__']) != 'undefined');
	},

	'userDetails': function() {
		var result;
		
		if (this.isTestData()) {
			var serializedHeader;
			var version;

//MochiKit.Logging.logDebug("### test data");
			version = this.userData()['userDetailsVersion'];
			serializedHeader = Clipperz.Base.serializeJSON(this.userData()['userDetails']);
			result = Clipperz.PM.Crypto.encryptingFunctions.versions[version].encrypt(this.userData()['__masterkey_test_value__'], serializedHeader);
		} else {
//MochiKit.Logging.logDebug("### NOT test data");
 			result = this.userData()['userDetails'];
		}
		
		return result;
	},
	
	'statistics': function() {
		var result;

		if (this.userData()['statistics'] != null) {
			if (this.isTestData()) {
				var serializedStatistics;
				var version;

				version = this.userData()['userDetailsVersion'];
				serializedStatistics = Clipperz.Base.serializeJSON(this.userData()['statistics']);
				result = Clipperz.PM.Crypto.encryptingFunctions.versions[version].encrypt(this.userData()['__masterkey_test_value__'], serializedStatistics);
			} else {
			 	result = this.userData()['statistics'];
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
//console.log("#-#-#-#-#", aDeferredContext);
//			return aDeferredContext['user'].serializedDataUsingVersion(MochiKit.Base.values(aDeferredContext['user'].records()), aDeferredContext['data']['version']);
//		}, deferredContext);
//		deferredResult.addCallback(function(aUserSerializedData) {
//console.log("USER SERIALIZED DATA", aUserSerializedData);
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