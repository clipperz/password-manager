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
if (typeof(Clipperz.PM.Crypto) == 'undefined') { Clipperz.PM.Crypto = {}; }

Clipperz.PM.Crypto.VERSION = "0.2";
Clipperz.PM.Crypto.NAME = "Clipperz.PM.Crypto";

MochiKit.Base.update(Clipperz.PM.Crypto, {

	'__repr__': function () {
		return "[" + this.NAME + " " + this.VERSION + "]";
	},

	//-------------------------------------------------------------------------

	'toString': function () {
		return this.__repr__();
	},

	//-------------------------------------------------------------------------

	'communicationProtocol': {
		'currentVersion': '0.2',
		'versions': {
			'0.1': Clipperz.PM.Connection.SRP['1.0'],	//Clipperz.Crypto.SRP.versions['1.0'].Connection,
			'0.2': Clipperz.PM.Connection.SRP['1.1']	//Clipperz.Crypto.SRP.versions['1.1'].Connection,
		},
		'fallbackVersions': {
			'current':	'0.1',
			'0.2':		'0.1',
			'0.1':		null
		}
	},

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
					
					deferredResult = new MochiKit.Async.Deferred();
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
					
					deferredResult = new MochiKit.Async.Deferred();
					deferredResult.addCallback(Clipperz.PM.Crypto.encryptingFunctions.versions['0.1'].decrypt, aKey, aValue);
					deferredResult.callback();
					
					return deferredResult;
				},
			
				'hash': function(aValue) {
					var result;
					var strngResult;
				
					stringResult = Clipperz.Crypto.Base.computeHashValue(aValue.asString());	//	!!!!!!!
					result = new Clipperz.ByteArray("0x" + stringResult);
				
					return result;
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
					var encryptedData;
				
					key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
					value = new Clipperz.ByteArray(Clipperz.Base.serializeJSON(aValue));
					dataToEncrypt = Clipperz.Crypto.SHA.sha_d256(value).appendBlock(value);
					
					deferredResult = new MochiKit.Async.Deferred()
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
						var decryptedData;

						key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
						value = new Clipperz.ByteArray().appendBase64String(aValue);
				
						decryptedData = Clipperz.Crypto.AES.decrypt(key, value);
						decryptedData = decryptedData.split((256/8));

						try {
							result = Clipperz.Base.evalJSON(decryptedData.asString());
						} catch (exception) {
							MochiKit.Logging.logError("Error while decrypting data");
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
						var decryptedData;

						result = new MochiKit.Async.Deferred();

						key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
						value = new Clipperz.ByteArray().appendBase64String(aValue);
				

						deferredResult = new MochiKit.Async.Deferred()
						deferredResult.addCallback(Clipperz.Crypto.AES.deferredDecrypt, key, value);
						deferredResult.addCallback(function(aResult) {
							var result;
							var decryptedData;

							decryptedData = aResult.split((256/8));
							
							try {
								result = Clipperz.Base.evalJSON(decryptedData.asString());
							} catch (exception) {
								MochiKit.Logging.logError("Error while decrypting data");
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
			
				'hash': Clipperz.Crypto.SHA.sha_d256
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
					
					deferredResult = new MochiKit.Async.Deferred()
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.PM.Crypto.deferredEncrypt - 1: " + res); return res;});
					deferredResult.addCallback(Clipperz.Crypto.AES.deferredEncrypt, key, data, aNonce);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.PM.Crypto.deferredEncrypt - 2: " + res); return res;});
					deferredResult.addCallback(function(aResult) {
						return aResult.toBase64String();
					})
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.PM.Crypto.deferredEncrypt - 3: " + res); return res;});
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

						value = decryptedData.asString();
						try {
							result = Clipperz.Base.evalJSON(value);
						} catch (exception) {
							MochiKit.Logging.logError("Error while decrypting data");
							throw Clipperz.Crypto.Base.exception.CorruptedMessage;
						}
					} else {
						result = null;
					}
					
					return result;
				},
			
				'deferredDecrypt': function(aKey, aValue) {
					var deferredResult;
//					var now;
					
					deferredResult = new MochiKit.Async.Deferred();
//					now = new Date;
					
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("[" + (new Date() - now) + "] Clipperz.PM.Crypto.deferredDecrypt - 1: " + res); return res;});
					if (aValue != null) {
						var key, value;
						var decryptedData;
						var decryptedValue;

						key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
//MochiKit.Logging.logDebug("[" + (new Date() - now) + "] computed key");
						value = new Clipperz.ByteArray().appendBase64String(aValue);
//MochiKit.Logging.logDebug("[" + (new Date() - now) + "] appendedBase64String");

//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("[" + (new Date() - now) + "] Clipperz.PM.Crypto.deferredDecrypt - 1.1: " /* + res*/); return res;});
						deferredResult.addCallback(Clipperz.Crypto.AES.deferredDecrypt, key, value);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("[" + (new Date() - now) + "] Clipperz.PM.Crypto.deferredDecrypt - 2: " /* + res*/); return res;});
						deferredResult.addCallback(MochiKit.Async.wait, 0.1);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("[" + (new Date() - now) + "] Clipperz.PM.Crypto.deferredDecrypt - 3: " /* + res*/); return res;});
						deferredResult.addCallback(function(aResult) {
							return aResult.asString();
						});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("[" + (new Date() - now) + "] Clipperz.PM.Crypto.deferredDecrypt - 4: " /* + res*/); return res;});
						deferredResult.addCallback(MochiKit.Async.wait, 0.1);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("[" + (new Date() - now) + "] Clipperz.PM.Crypto.deferredDecrypt - 5: " /* + res*/); return res;});
						deferredResult.addCallback(Clipperz.Base.evalJSON);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("[" + (new Date() - now) + "] Clipperz.PM.Crypto.deferredDecrypt - 6: " /* + res*/); return res;});
						deferredResult.addErrback(function(anError) {
							MochiKit.Logging.logError("Error while decrypting data");
							throw Clipperz.Crypto.Base.exception.CorruptedMessage;
						})
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("[" + (new Date() - now) + "] Clipperz.PM.Crypto.deferredDecrypt - 7: " /* + res*/); return res;});
					} else {
						deferredResult.addCallback(function() {
							return null;
						});
					}
					deferredResult.callback();
					
					return deferredResult;
				},
				
				'hash': Clipperz.Crypto.SHA.sha_d256
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
					
					deferredResult = new MochiKit.Async.Deferred()
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
						var decryptedValue;

						key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
						value = new Clipperz.ByteArray().appendBase64String(aValue);
				
						decryptedData = Clipperz.Crypto.AES_2.decrypt(key, value);

						value = decryptedData.asString();
						try {
							result = Clipperz.Base.evalJSON(value);
						} catch (exception) {
							MochiKit.Logging.logError("Error while decrypting data");
							throw Clipperz.Crypto.Base.exception.CorruptedMessage;
						}
					} else {
						result = null;
					}
					
					return result;
				},
			
				'deferredDecrypt': function(aKey, aValue) {
					var deferredResult;
					
					deferredResult = new MochiKit.Async.Deferred();
					if (aValue != null) {
						var key, value;
						var decryptedData;
						var decryptedValue;

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
							MochiKit.Logging.logError("Error while decrypting data");
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
				
				'hash': Clipperz.Crypto.SHA.sha_d256
			},

		//#####################################################################
			__syntaxFix__: "syntax fix"
		}
	},
	
	//-------------------------------------------------------------------------
	
	'encrypt': function(aKey, aValue, aVersion) {
		return Clipperz.PM.Crypto.encryptingFunctions.versions[aVersion].encrypt(aKey, aValue);
	},

	'deferredEncrypt': function(aKey, aValue, aVersion) {
		return Clipperz.PM.Crypto.encryptingFunctions.versions[aVersion].deferredEncrypt(aKey, aValue);
	},

	'encryptWithCurrentVersion': function(aKey, aValue) {
		return Clipperz.PM.Crypto.encrypt(aKey, aValue, Clipperz.PM.Crypto.encryptingFunctions.currentVersion);
	},

	'deferredEncryptWithCurrentVersion': function(aKey, aValue) {
		return Clipperz.PM.Crypto.deferredEncrypt(aKey, aValue, Clipperz.PM.Crypto.encryptingFunctions.currentVersion);
	},
	
	//.........................................................................

	'decrypt': function(aKey, aValue, aVersion) {
		return Clipperz.PM.Crypto.encryptingFunctions.versions[aVersion].decrypt(aKey, aValue);
	},

	'deferredDecrypt': function(aKey, aValue, aVersion) {
		return Clipperz.PM.Crypto.encryptingFunctions.versions[aVersion].deferredDecrypt(aKey, aValue);
	},
	
	//-------------------------------------------------------------------------

	'randomKey': function() {
		return Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32).toHexString().substring(2);
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
//MochiKit.Logging.logDebug("--- bitPerChar: " + bitPerChar);

		result = aValue.length * bitPerChar;
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'nullValue': "####",

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});

//*****************************************************************************

MochiKit.Base.update(Clipperz.PM.Crypto.communicationProtocol.versions, {
	'current': Clipperz.PM.Crypto.communicationProtocol.versions[Clipperz.PM.Crypto.communicationProtocol.currentVersion]
});

MochiKit.Base.update(Clipperz.PM.Crypto.encryptingFunctions.versions, {
	'current': Clipperz.PM.Crypto.encryptingFunctions.versions[Clipperz.PM.Crypto.encryptingFunctions.currentVersion]
});

//*****************************************************************************
