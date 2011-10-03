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

//-----------------------------------------------------------------------------
//
//		Abstract   C O N N E C T I O N   class
//
//-----------------------------------------------------------------------------

Clipperz.PM.Connection = function (args) {
	args = args || {};

	this._proxy = args.proxy || Clipperz.PM.Proxy.defaultProxy;
	this._getCredentialsFunction = args.getCredentialsFunction;

	this._clipperz_pm_crypto_version = null;
	this._connectionId = null;
	this._sharedSecret = null;

	return this;
}

Clipperz.PM.Connection.prototype = MochiKit.Base.update(null, {

	'toString': function() {
		return "Connection [" + this.version() + "]";
	},
	
	//=========================================================================

	'version': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},
	
	'clipperz_pm_crypto_version': function() {
		if (this._clipperz_pm_crypto_version == null) {
			var connectionVersions;
			var	versions;
			var	version;
			var i, c;

			version = null;
			connectionVersions = Clipperz.PM.Connection.communicationProtocol.versions;
			versions = MochiKit.Base.keys(connectionVersions);
			c = versions.length;
			for (i=0; i<c; i++) {
				if (! (versions[i] == 'current')) {
					if (this instanceof connectionVersions[versions[i]]) {
						version = versions[i];
					};
				}
			}
			
			this._clipperz_pm_crypto_version = version;
		}
		
		return this._clipperz_pm_crypto_version;
	},

	//-------------------------------------------------------------------------

	'defaultErrorHandler': function(anErrorString, anException) {
MochiKit.Logging.logError("### Connection.defaultErrorHandler: " + anErrorString + " (" + anException + ")");
	},
	
	//-------------------------------------------------------------------------

	'getCredentialsFunction': function () {
		return this._getCredentialsFunction;
	},

	'normalizedCredentials': function(someValues) {
		throw Clipperz.Base.exception.AbstractMethod;
	},
	
	//=========================================================================

	'proxy': function () {
		return this._proxy;
	},

	//=========================================================================

	'register': function () {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'login': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	//-------------------------------------------------------------------------

	'message': function(someArguments, aCallback) {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	//-------------------------------------------------------------------------

	'serverSideUserCredentials': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	//=========================================================================

	'sharedSecret': function () {
		return this._sharedSecret;
	},
	
	'setSharedSecret': function (aValue) {
		this._sharedSecret = aValue;
	},

	//-------------------------------------------------------------------------

	'connectionId': function() {
		return this._connectionId;
	},
	
	'setConnectionId': function(aValue) {
		this._connectionId = aValue;
	},

	//=========================================================================
/*
//	TODO: ?????
	'oneTimePassword': function() {
		return this._oneTimePassword;
	},
	
	'setOneTimePassword': function(aValue) {
		this._oneTimePassword = aValue;
	},
*/
	//=========================================================================

	'reset': function() {
		this.setSharedSecret(null);
		this.setConnectionId(null);
	},
	
	//=========================================================================
	__syntaxFix__: "syntax fix"

}
);


if (typeof(Clipperz.PM.Connection.SRP) == 'undefined') { Clipperz.PM.Connection.SRP = {}; }
//-----------------------------------------------------------------------------
//
//		S R P [ 1 . 0 ]    C O N N E C T I O N   class
//
//-----------------------------------------------------------------------------

Clipperz.PM.Connection.SRP['1.0'] = function (args) {
	Clipperz.PM.Connection.call(this, args);
	
	return this;
}

Clipperz.PM.Connection.SRP['1.0'].prototype = MochiKit.Base.update(new Clipperz.PM.Connection(), {

	'version': function() {
		return '1.0';
	},

	//=========================================================================
	
	'register': function (someUserData) {
		var	deferredResult;
		var cryptoVersion;
		var srpConnection;

		cryptoVersion = this.clipperz_pm_crypto_version();

		deferredResult = new Clipperz.Async.Deferred("Connection.registerWithVersion", {trace:false});
		deferredResult.collectResults({
			'credentials': [
				this.getCredentialsFunction(),
				MochiKit.Base.method(this, 'normalizedCredentials'),
				MochiKit.Base.bind(function(someCredentials) {
					var srpConnection;
					var result;

					srpConnection = new Clipperz.Crypto.SRP.Connection({ C:someCredentials['username'], P:someCredentials['password'], hash:this.hash() });
					result = srpConnection.serverSideCredentials();
					result['version'] = Clipperz.PM.Connection.communicationProtocol.currentVersion;

					return result;
				}, this)
			],
			'user':		MochiKit.Base.partial(MochiKit.Async.succeed, someUserData),
			'version':	MochiKit.Base.partial(MochiKit.Async.succeed, Clipperz.PM.Connection.communicationProtocol.currentVersion),
			'message':	MochiKit.Base.partial(MochiKit.Async.succeed, 'completeRegistration')
		});
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');
		deferredResult.addMethod(this.proxy(), 'registration');
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');

		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'updateCredentials': function (aUsername, aPassphrase, someUserData) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("Connection.updateCredentials", {trace:false});
		deferredResult.collectResults({
			'credentials': [
				MochiKit.Base.method(this, 'normalizedCredentials', {username:aUsername, password:aPassphrase}),
				MochiKit.Base.bind(function(someCredentials) {
					var srpConnection;
					var result;

					srpConnection = new Clipperz.Crypto.SRP.Connection({ C:someCredentials['username'], P:someCredentials['password'], hash:this.hash() });
					result = srpConnection.serverSideCredentials();
					result['version'] = Clipperz.PM.Connection.communicationProtocol.currentVersion;

					return result;
				}, this)
			],
			'user':		MochiKit.Base.partial(MochiKit.Async.succeed, someUserData)
		});
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');
		deferredResult.addMethod(this, 'message', 'upgradeUserCredentials');
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');
		deferredResult.callback();
		
		return deferredResult;
		
	},

	//=========================================================================

	'redeemOneTimePassword': function (someParameters) {
//console.log("Connections.redeemOneTimePassword", someParameters['username'], someParameters['password']);
/*
	//=========================================================================
		//	LOGIN WITH PASSPHRASE, extracted from the TRUNK version (LoginPanel.js)
		deferredResult.addCallback(function(anUsername, aOneTimePassword) {
			var args;
			
			args = {
				'message': 'oneTimePassword',
				'version': Clipperz.PM.Crypto.communicationProtocol.currentVersion,
				'parameters': {
					'oneTimePasswordKey': Clipperz.PM.DataModel.OneTimePassword.computeKeyWithUsernameAndPassword(anUsername, aOneTimePassword),
					'oneTimePasswordKeyChecksum': Clipperz.PM.DataModel.OneTimePassword.computeKeyChecksumWithUsernameAndPassword(anUsername, aOneTimePassword)
				}
			}
			
			return args;
		}, anUsername, oneTimePassword);
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'OTP_login_loadingOTP');
		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Proxy.defaultProxy, 'handshake'));
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'OTP_login_extractingPassphrase');
		deferredResult.addCallback(function(aResult) {
			return Clipperz.PM.Crypto.deferredDecrypt(oneTimePassword, aResult['data'], aResult['version']);
		});
		deferredResult.addCallback(function(aResult) {
			return (new Clipperz.ByteArray().appendBase64String(aResult['passphrase'])).asString();
		});
		deferredResult.addMethod(this, 'doLoginWithUsernameAndPassphrase', anUsername),
*/
		var args;
		var normalizedOTP;

		normalizedOTP = Clipperz.PM.DataModel.OneTimePassword.normalizedOneTimePassword(someParameters['password']);

		args = {
			'message': 'oneTimePassword',
			'version': Clipperz.PM.Connection.communicationProtocol.currentVersion,
			'parameters': {
				'oneTimePasswordKey':			Clipperz.PM.DataModel.OneTimePassword.computeKeyWithUsernameAndPassword(someParameters['username'], normalizedOTP),
				'oneTimePasswordKeyChecksum':	Clipperz.PM.DataModel.OneTimePassword.computeKeyChecksumWithUsernameAndPassword(someParameters['username'], normalizedOTP)
			}
		}

		return Clipperz.Async.callbacks("Connction.redeemOTP", [
			MochiKit.Base.method(this.proxy(), 'handshake', args),
			function(aResult) {
				return Clipperz.PM.Crypto.deferredDecrypt({
					value:	aResult['data'],
					key:	normalizedOTP,
					version:aResult['version']
				});
			},
			function(aResult) {
				return (new Clipperz.ByteArray().appendBase64String(aResult['passphrase'])).asString();
			}			
		], {trace:false})
	},

	'login': function(/*anUsername, aPassphrase*/) {
		var	deferredResult;
		var cryptoVersion;
		var srpConnection;

		cryptoVersion = this.clipperz_pm_crypto_version();

		deferredResult = new Clipperz.Async.Deferred("Connection.login", {trace:false});
		deferredResult.addCallback(this.getCredentialsFunction());
		deferredResult.addMethod(this, 'normalizedCredentials');
//		deferredResult.addCallbackPass(MochiKit.Signal.signal, this, 'updatedProgressState', 'connection_sendingCredentials');
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');
		deferredResult.addCallback(MochiKit.Base.bind(function(someCredentials) {
			srpConnection = new Clipperz.Crypto.SRP.Connection({ C:someCredentials['username'], P:someCredentials['password'], hash:this.hash() });
		}, this));
		deferredResult.addCallback(function() {
			var result;

			result = {
				message: 'connect',
				version: cryptoVersion,
				parameters: {
					C: srpConnection.C(),
					A: srpConnection.A().asString(16)
//					reconnecting: this.connectionId()	
				}
			};

//	TODO: ?????			
//			if (isReconnecting == true) {
//				args.parameters['reconnecting'] = aConnection.connectionId();
//			}

			return result;
		});
		deferredResult.addMethod(this.proxy(), 'handshake');
//		deferredResult.addCallbackPass(MochiKit.Signal.signal, this, 'updatedProgressState', 'connection_credentialVerification');
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');
		deferredResult.addCallback(function(someParameters) {
			var result;

			srpConnection.set_s(new Clipperz.Crypto.BigInt(someParameters['s'], 16));
			srpConnection.set_B(new Clipperz.Crypto.BigInt(someParameters['B'], 16));

//	TODO: ?????
//			if (typeof(someParameters['oneTimePassword']) != 'undefined') {
//				this.setOneTimePassword(someParameters['oneTimePassword']);
//			}
			
			result = {
				message: 'credentialCheck',
				version: cryptoVersion,
				parameters: {
					M1: srpConnection.M1()
				}
			};

			return result;
		});
		deferredResult.addMethod(this.proxy(), 'handshake');
		deferredResult.addCallback(function(someParameters) {
			var result;

			if (someParameters['M2'] == srpConnection.M2()) {
				result = MochiKit.Async.succeed(someParameters);
			} else {
				result = MochiKit.Async.fail(Clipperz.PM.Connection.exception.WrongChecksum);
			}

			return result;
		});
		deferredResult.addCallback(MochiKit.Base.bind(function(someParameters) {
			this.setConnectionId(someParameters['connectionId']);
			this.setSharedSecret(srpConnection.K());
			
//	TODO: ?????			
//			if (this.oneTimePassword() != null) {
///	??			result = this.user().oneTimePasswordManager().archiveOneTimePassword(this.oneTimePassword()));
//			}
			return someParameters;
		}, this));
//		deferredResult.addCallbackPass(MochiKit.Signal.signal, this, 'updatedProgressState', 'connection_loggedIn');
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'advanceProgress');
		deferredResult.addCallback(MochiKit.Async.succeed, {result:"done"});

		deferredResult.callback();
		
		return deferredResult;
	},

	//=========================================================================

	'logout': function() {
		return Clipperz.Async.callbacks("Connection.logout", [
			MochiKit.Base.method(this, 'setSharedSecret'),
			MochiKit.Base.method(this.proxy(), 'logout', {})
		], {trace:false});
	},

	//=========================================================================

	'ping': function () {
		//	TODO: ping the server in order to have a valid session
	},

	//=========================================================================

	'message': function(aMessageName, someParameters) {
		var args;

//console.log(">>> Connection.message", aMessageName, someParameters);
		args = {
			message: aMessageName,
			srpSharedSecret: this.sharedSecret(),
			parameters: (someParameters || {})
		}

		return this.sendMessage(args);
	},

	//-------------------------------------------------------------------------

	'sendMessage': function(someArguments) {
		var	deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("Connection.sendMessage", {trace:false});
		deferredResult.addMethod(this.proxy(), 'message', someArguments);
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			if (typeof(res['lock']) != 'undefined') {
//	TODO: ?????
//	??			this.user().setLock(res['lock']);
			}
			return res;
		}, this));

		deferredResult.addErrback(MochiKit.Base.method(this, 'messageExceptionHandler'), someArguments);
		deferredResult.callback();
		
		return deferredResult
	},
	
	//-------------------------------------------------------------------------

	'messageExceptionHandler': function(anOriginalMessageArguments, anError) {
		var result;

console.log(">>> Connection.messageExceptionHandler", anError, anError.message);
		if (anError instanceof MochiKit.Async.CancelledError) {
			result = anError;
		} else {
			if ((anError.message == 'Trying to communicate without an active connection')	||
				(anError.message == 'No tollManager available for current session')		
			) {
				result = this.reestablishConnection(anOriginalMessageArguments);
			} else if (anError.message == 'Session with stale data') {
				MochiKit.Signal.signal(this, 'EXCEPTION');
			} else {
				result = anError;
			}
		}
console.log("<<< Connection.messageExceptionHandler", anError)
		
		return result;;
	},
	
	//=========================================================================

	'reestablishConnection': function(anOriginalMessageArguments) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("Connection.reestablishConnection");
		deferredResult.addMethod(this, 'reset');
		deferredResult.addMethod(this, 'login', true);
		deferredResult.addCallback(MochiKit.Base.bind(function(aMessage) {
			aMessage['srpSharedSecret'] = this.sharedSecret();
			return aMessage;
		}, this), anOriginalMessageArguments);
		deferredResult.addMethod(this, 'sendMessage');
		deferredResult.addErrback(MochiKit.Signal.signal, this, 'EXCEPTION', null);
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'serverSideUserCredentials': function(aUsername, aPassword) {
		var	result;
		var	newSrpConnection;
		var normalizedAttributes;
		
		normalizedAttributes = this.normalizedCredentials({username:aUsername, password:aPassword});
		newSrpConnection = new Clipperz.Crypto.SRP.Connection({ C:normalizedAttributes['username'], P:normalizedAttributes['password'], hash:this.hash() });
		result = newSrpConnection.serverSideCredentials();
		result['version'] = this.clipperz_pm_crypto_version();

		return result;
	},

	//=========================================================================

	'normalizedCredentials': function(someValues) {
		var result;

		result = {}
		result['username'] = this.hash()(new Clipperz.ByteArray(someValues['username'])).toHexString().substring(2);
		result['password'] = this.hash()(new Clipperz.ByteArray(someValues['password'] + someValues['username'])).toHexString().substring(2);

		return result;
	},

	//-----------------------------------------------------------------------------

	'hash': function() {
		return Clipperz.PM.Crypto.encryptingFunctions.versions['0.1'].hash;
	},
	
	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});



//-----------------------------------------------------------------------------
//
//		S R P [ 1 . 1 ]    C O N N E C T I O N   class
//
//-----------------------------------------------------------------------------

Clipperz.PM.Connection.SRP['1.1'] = function (args) {
	Clipperz.PM.Connection.SRP['1.0'].call(this, args);
	
	return this;
}

Clipperz.PM.Connection.SRP['1.1'].prototype = MochiKit.Base.update(new Clipperz.PM.Connection.SRP['1.0'](), {

	'version': function() {
		return '1.1';
	},

	//-----------------------------------------------------------------------------

	'normalizedCredentials': function(someValues) {
		var result;
		
		result = {}
		result['username'] = this.hash()(new Clipperz.ByteArray(someValues['username'] + someValues['password'])).toHexString().substring(2);
		result['password'] = this.hash()(new Clipperz.ByteArray(someValues['password'] + someValues['username'])).toHexString().substring(2);

		return result;
	},

	//-----------------------------------------------------------------------------

	'hash': function() {
		return Clipperz.PM.Crypto.encryptingFunctions.versions['0.2'].hash;
	},

	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});

Clipperz.PM.Connection.exception = {
	WrongChecksum:		new MochiKit.Base.NamedError("Clipperz.ByteArray.exception.InvalidValue"),
	UnexpectedRequest:	new MochiKit.Base.NamedError("Clipperz.ByteArray.exception.UnexpectedRequest")
};


Clipperz.PM.Connection.communicationProtocol = {
	'currentVersion': '0.2',
	'versions': {
		'0.1': Clipperz.PM.Connection.SRP['1.0'],	//Clipperz.Crypto.SRP.versions['1.0'].Connection,
		'0.2': Clipperz.PM.Connection.SRP['1.1']	//Clipperz.Crypto.SRP.versions['1.1'].Connection
	},
	'fallbackVersions': {
//		'current':	'0.1',
		'0.2':		'0.1',
		'0.1':		null
	}
};

MochiKit.Base.update(Clipperz.PM.Connection.communicationProtocol.versions, {
	'current': Clipperz.PM.Connection.communicationProtocol.versions[Clipperz.PM.Connection.communicationProtocol.currentVersion]
});

MochiKit.Base.update(Clipperz.PM.Connection.communicationProtocol.fallbackVersions, {
	'current': Clipperz.PM.Connection.communicationProtocol.fallbackVersions[Clipperz.PM.Connection.communicationProtocol.currentVersion]
});



