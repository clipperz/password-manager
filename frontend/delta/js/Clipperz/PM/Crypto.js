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

"use strict";
if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.Crypto) == 'undefined') { Clipperz.PM.Crypto = {}; }

Clipperz.PM.Crypto.VERSION = "0.2";
Clipperz.PM.Crypto.NAME = "Clipperz.PM.Crypto";

Clipperz.PM.Crypto.encryptingFunctions = {};

MochiKit.Base.update(Clipperz.PM.Crypto, {

	'__repr__': function () {
		return "[" + this.NAME + " " + this.VERSION + "]";
	},

	//-------------------------------------------------------------------------

	'toString': function () {
		return this.__repr__();
	},

	//-------------------------------------------------------------------------
/*
	'communicationProtocol': {
		'currentVersion': '0.2',
		'versions': {
			'0.1': Clipperz.PM.Connection.SRP['1.0'],	//Clipperz.Crypto.SRP.versions['1.0'].Connection,
			'0.2': Clipperz.PM.Connection.SRP['1.1']	//Clipperz.Crypto.SRP.versions['1.1'].Connection
		},
		'fallbackVersions': {
			'current':	'0.1',
			'0.2':		'0.1',
			'0.1':		null
		}
	},
*/
	//-------------------------------------------------------------------------

	'encryptingFunctions': {
		'currentVersion': '0.4',
		'versions': {

		//#####################################################################

			'0.1': {
				'encrypt': function(aKey, aValue) {
					return Clipperz.Crypto.Base.encryptUsingSecretKey(aKey, Clipperz.Base.serializeJSON(aValue));
				},
			
				'deferredEncrypt': function(aKey, aValue) {
					var deferredResult;
					
					deferredResult = new Clipperz.Async.Deferred("Crypto[0.1].deferredEncrypt");
					deferredResult.addCallback(Clipperz.PM.Crypto.encryptingFunctions.versions['0.1'].encrypt, aKey, aValue);
					deferredResult.callback();
					
					return deferredResult;
				},
				
				'decrypt': function(aKey, aValue) {
					var result;
				
					if (aValue != null) {
						result = Clipperz.Base.evalJSON(Clipperz.Crypto.Base.decryptUsingSecretKey(aKey, aValue));
					} else {
						result = null;
					}
				
					return result;
				},
				
				'deferredDecrypt': function(aKey, aValue) {
					var deferredResult;
					
					deferredResult = new Clipperz.Async.Deferred("Crypto.[0.1].deferredDecrypt");
					deferredResult.addCallback(Clipperz.PM.Crypto.encryptingFunctions.versions['0.1'].decrypt, aKey, aValue);
					deferredResult.callback();
					
					return deferredResult;
				},
			
				'hash': function(aValue) {
					var result;
					var stringResult;
				
					stringResult = Clipperz.Crypto.Base.computeHashValue(aValue.asString());	//	!!!!!!!
					result = new Clipperz.ByteArray("0x" + stringResult);
				
					return result;
				},
				
				'deriveKey': function(aStringValue) {
					return Clipperz.Crypto.Base.computeHashValue(aStringValue);
				}
			},

		//#####################################################################

			'0.2': {
				'encrypt': function(aKey, aValue, aNonce) {
					var result;
					var	key, value;
					var dataToEncrypt;
					var encryptedData;
				
					key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
					value = new Clipperz.ByteArray(Clipperz.Base.serializeJSON(aValue));
					dataToEncrypt = Clipperz.Crypto.SHA.sha_d256(value).appendBlock(value);
					encryptedData = Clipperz.Crypto.AES.encrypt(key, dataToEncrypt, aNonce);
					result = encryptedData.toBase64String();
				
					return result;
				},

				'deferredEncrypt': function(aKey, aValue, aNonce) {
					var deferredResult;
					var	key, value;
					var dataToEncrypt;
//					var encryptedData;
				
					key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
					value = new Clipperz.ByteArray(Clipperz.Base.serializeJSON(aValue));
					dataToEncrypt = Clipperz.Crypto.SHA.sha_d256(value).appendBlock(value);
					
					deferredResult = new Clipperz.Async.Deferred("Crypto[0.2].deferredEncrypt")
					deferredResult.addCallback(Clipperz.Crypto.AES.deferredEncrypt, key, dataToEncrypt, aNonce);
					deferredResult.addCallback(function(aResult) {
						return aResult.toBase64String();
					})
					deferredResult.callback();
					
					return deferredResult;
				},
			
				'decrypt': function(aKey, aValue) {
					var result;
					
					if (aValue != null) {
						var key, value;
						var decryptedData;
						var decryptedValue;

						key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
						value = new Clipperz.ByteArray().appendBase64String(aValue);
				
						decryptedData = Clipperz.Crypto.AES.decrypt(key, value);
						decryptedValue = decryptedData.split((256/8));

						try {
							result = Clipperz.Base.evalJSON(decryptedValue.asString());
						} catch (exception) {
							Clipperz.logError("Error while decrypting data [1]");
							throw Clipperz.Crypto.Base.exception.CorruptedMessage;
						}
					} else {
						result = null;
					}
					
					return result;
				},

				'deferredDecrypt': function(aKey, aValue) {
					var result;

					if (aValue != null) {
						var deferredResult;
						var key, value;
//						var decryptedData;

						key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
						value = new Clipperz.ByteArray().appendBase64String(aValue);

						deferredResult = new Clipperz.Async.Deferred("Crypto.[0.2].deferredDecrypt");
						deferredResult.addCallback(Clipperz.Crypto.AES.deferredDecrypt, key, value);
						deferredResult.addCallback(function(aResult) {
							var result;
							var decryptedData;

							decryptedData = aResult.split((256/8));
							
							try {
								result = Clipperz.Base.evalJSON(decryptedData.asString());
							} catch (exception) {
								Clipperz.logError("Error while decrypting data [2]");
								throw Clipperz.Crypto.Base.exception.CorruptedMessage;
							}

							return result;
						})
						deferredResult.callback();
						
						result = deferredResult;
					} else {
						result = MochiKit.Async.succeed(null);
					}
					
					return result;
				},
			
				'hash': Clipperz.Crypto.SHA.sha_d256,
				
				'deriveKey': function(aStringValue) {
					var	byteData;
					var result;
					
					byteData = new Clipperz.ByteArray(aStringValue);
					result = Clipperz.Crypto.SHA.sha_d256(byteData);
					
					return result;
				}
			},

		//#####################################################################

			'0.3': {
				'encrypt': function(aKey, aValue, aNonce) {
					var result;
					var	key, value;
					var data;
					var dataToEncrypt;
					var encryptedData;
				
					key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
					value = Clipperz.Base.serializeJSON(aValue);
					data = new Clipperz.ByteArray(value);
					encryptedData = Clipperz.Crypto.AES.encrypt(key, data, aNonce);
					result = encryptedData.toBase64String();
				
					return result;
				},
				
				'deferredEncrypt': function(aKey, aValue, aNonce) {
					var deferredResult;
					var	key, value;
					var data;
					var dataToEncrypt;
					var encryptedData;
				
					key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
					value = Clipperz.Base.serializeJSON(aValue);
					data = new Clipperz.ByteArray(value);
					
					deferredResult = new Clipperz.Async.Deferred("Crypto[0.3].deferredEncrypt")
					deferredResult.addCallback(Clipperz.Crypto.AES.deferredEncrypt, key, data, aNonce);
					deferredResult.addCallback(function(aResult) {
						return aResult.toBase64String();
					})
					deferredResult.callback();
					
					return deferredResult;
				},
			
				'decrypt': function(aKey, aValue) {
					var result;
					
					if (aValue != null) {
						var key, value;
						var decryptedData;

						key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
						value = new Clipperz.ByteArray().appendBase64String(aValue);
				
						decryptedData = Clipperz.Crypto.AES.decrypt(key, value);

						value = decryptedData.asString();
						try {
							result = Clipperz.Base.evalJSON(value);
						} catch (exception) {
							Clipperz.logError("Error while decrypting data [3]");
							throw Clipperz.Crypto.Base.exception.CorruptedMessage;
						}
					} else {
						result = null;
					}
					
					return result;
				},
			
				'deferredDecrypt': function(aKey, aValue) {
					var deferredResult;
					
					deferredResult = new Clipperz.Async.Deferred("Crypto[0.3].deferredDecrypt", {trace: false});
//					now = new Date;
					
					if (aValue != null) {
						var key, value;
//						var decryptedData;

						key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
						value = new Clipperz.ByteArray().appendBase64String(aValue);

						deferredResult.addCallback(Clipperz.Crypto.AES.deferredDecrypt, key, value);
						deferredResult.addCallback(MochiKit.Async.wait, 0.1);
						deferredResult.addCallback(function(aResult) {
							return aResult.asString();
						});
						deferredResult.addCallback(MochiKit.Async.wait, 0.1);
						deferredResult.addCallback(Clipperz.Base.evalJSON);
						deferredResult.addErrback(function(anError) {
							Clipperz.logError("Error while decrypting data [3]");
							throw Clipperz.Crypto.Base.exception.CorruptedMessage;
						})
					} else {
						deferredResult.addCallback(function() {
							return null;
						});
					}
					deferredResult.callback();
					
					return deferredResult;
				},
				
				'hash': Clipperz.Crypto.SHA.sha_d256,

				'deriveKey': function(aStringValue) {
					var	byteData;
					var result;
					
					byteData = new Clipperz.ByteArray(aStringValue);
					result = Clipperz.Crypto.SHA.sha_d256(byteData);
					
					return result;
				}
			},

		//#####################################################################

			'0.4': {
				'encrypt': function(aKey, aValue, aNonce) {
					var result;
					var	key, value;
					var data;
					var dataToEncrypt;
					var encryptedData;
				
					key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
					value = Clipperz.Base.serializeJSON(aValue);
					data = new Clipperz.ByteArray(value);
					encryptedData = Clipperz.Crypto.AES_2.encrypt(key, data, aNonce);
					result = encryptedData.toBase64String();
				
					return result;
				},
				
				'deferredEncrypt': function(aKey, aValue, aNonce) {
					var deferredResult;
					var	key, value;
					var data;
					var dataToEncrypt;
					var encryptedData;
				
					key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
					value = Clipperz.Base.serializeJSON(aValue);
					data = new Clipperz.ByteArray(value);
					
					deferredResult = new Clipperz.Async.Deferred("Crypto[0.4].deferredEncrypt")
					deferredResult.addCallback(Clipperz.Crypto.AES_2.deferredEncrypt, key, data, aNonce);
					deferredResult.addCallback(function(aResult) {
						return aResult.toBase64String();
					})
					deferredResult.callback();
					
					return deferredResult;
				},
			
				'decrypt': function(aKey, aValue) {
					var result;
					
					if (aValue != null) {
						var key, value;
						var decryptedData;

						key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
						value = new Clipperz.ByteArray().appendBase64String(aValue);
				
						decryptedData = Clipperz.Crypto.AES_2.decrypt(key, value);

						value = decryptedData.asString();
						try {
							result = Clipperz.Base.evalJSON(value);
						} catch (exception) {
							Clipperz.logError("Error while decrypting data [4]");
							throw Clipperz.Crypto.Base.exception.CorruptedMessage;
						}
					} else {
						result = null;
					}
					
					return result;
				},
			
				'deferredDecrypt': function(aKey, aValue) {
					var deferredResult;
					
					deferredResult = new Clipperz.Async.Deferred("Crypto[0.4].deferredDecrypt", {trace: false});
					
					if (aValue != null) {
						var key, value;

						key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
						value = new Clipperz.ByteArray().appendBase64String(aValue);

						deferredResult.addCallback(Clipperz.Crypto.AES_2.deferredDecrypt, key, value);
						deferredResult.addCallback(MochiKit.Async.wait, 0.1);
						deferredResult.addCallback(function(aResult) {
							return aResult.asString();
						});
						deferredResult.addCallback(MochiKit.Async.wait, 0.1);
						deferredResult.addCallback(Clipperz.Base.evalJSON);
						deferredResult.addErrback(function(anError) {
							Clipperz.logError("Error while decrypting data [4]");
							throw Clipperz.Crypto.Base.exception.CorruptedMessage;
						});
					} else {
						deferredResult.addCallback(function() {
							return null;
						});
					}
					deferredResult.callback();
					
					return deferredResult;
				},
				
				'hash': Clipperz.Crypto.SHA.sha_d256,

				'deriveKey': function(aStringValue) {
					var	byteData;
					var result;
					
					byteData = new Clipperz.ByteArray(aStringValue);
					result = Clipperz.Crypto.SHA.sha_d256(byteData);
					
					return result;
				}
			},

		//#####################################################################
			__syntaxFix__: "syntax fix"
		}
	},
	
	//-------------------------------------------------------------------------
	
	'encrypt': function(aKey, aValue, aVersion) {
		return Clipperz.PM.Crypto.encryptingFunctions.versions[aVersion].encrypt(aKey, aValue);
	},

	'deferredEncrypt': function(someParameters) {
		return Clipperz.PM.Crypto.encryptingFunctions.versions[someParameters['version']].deferredEncrypt(someParameters['key'], someParameters['value']);
	},

	//.........................................................................

	'decrypt': function(aKey, aValue, aVersion) {
		return Clipperz.PM.Crypto.encryptingFunctions.versions[aVersion].decrypt(aKey, aValue);
	},

	'deferredDecrypt': function(someParameters) {
		return Clipperz.PM.Crypto.encryptingFunctions.versions[someParameters['version']].deferredDecrypt(someParameters['key'], someParameters['value']);
	},

	//-------------------------------------------------------------------------

	'hash': function(aValue) {
		return Clipperz.PM.Crypto.encryptingFunctions.versions[Clipperz.PM.Crypto.encryptingFunctions.currentVersion]['hash'](aValue);
	},
	
	//-------------------------------------------------------------------------

	'randomKey': function() {
		return Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32).toHexString().substring(2);
	},

	//-------------------------------------------------------------------------

	'deriveKey': function(aValue) {
		return Clipperz.PM.Crypto.encryptingFunctions.versions[Clipperz.PM.Crypto.encryptingFunctions.currentVersion].deriveKey(aValue);
	},
	
	//-------------------------------------------------------------------------

	'passwordEntropy': function(aValue) {
		var result;
		var	bitPerChar;
		
		bitPerChar = 4;
		if (/[a-z]/.test(aValue)) {
			bitPerChar ++;
		}
		if (/[A-Z]/.test(aValue)) {
			bitPerChar ++;
		}
		if (/[^a-zA-Z0-9]/.test(aValue)) {
			bitPerChar ++;
		}

		result = aValue.length * bitPerChar;
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'nullValue': '####',

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});

//*****************************************************************************

//MochiKit.Base.update(Clipperz.PM.Connection.communicationProtocol.versions, {
//	'current': Clipperz.PM.Connection.communicationProtocol.versions[Clipperz.PM.Connection.communicationProtocol.currentVersion]
//});

MochiKit.Base.update(Clipperz.PM.Crypto.encryptingFunctions.versions, {
	'current': Clipperz.PM.Crypto.encryptingFunctions.versions[Clipperz.PM.Crypto.encryptingFunctions.currentVersion]
});

//*****************************************************************************
