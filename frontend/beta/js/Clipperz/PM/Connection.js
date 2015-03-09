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

//-----------------------------------------------------------------------------
//
//		Abstract   C O N N E C T I O N   class
//
//-----------------------------------------------------------------------------

Clipperz.PM.Connection = function (args) {
	args = args || {};

	this._user = args.user;
	this._clipperz_pm_crypto_version = null;
	this._connectionId = null;
	this._oneTimePassword = null;
	
	return this;
}

Clipperz.PM.Connection.prototype = MochiKit.Base.update(null, {

	'user': function() {
		return this._user;
	},
	
	'toString': function() {
		return "Connection [" + this.version() + "] - user: " + this.user();
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
			connectionVersions = Clipperz.PM.Crypto.communicationProtocol.versions;
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

	'login': function(someArguments, aCallback) {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	//-------------------------------------------------------------------------

	'message': function(someArguments, aCallback) {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	//-------------------------------------------------------------------------

	'sharedSecret': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	'serverSideUserCredentials': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},

	//=========================================================================

	'connectionId': function() {
		return this._connectionId;
	},
	
	'setConnectionId': function(aValue) {
		this._connectionId = aValue;
	},

	//=========================================================================

	'oneTimePassword': function() {
		return this._oneTimePassword;
	},
	
	'setOneTimePassword': function(aValue) {
		this._oneTimePassword = aValue;
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
	args = args || {};
	Clipperz.PM.Connection.call(this, args);
	
	this._C = null;
	this._P = null;
	this._srpConnection = null;
	
	return this;
}

Clipperz.PM.Connection.SRP['1.0'].prototype = MochiKit.Base.update(new Clipperz.PM.Connection(), {

	'version': function() {
		return '1.0';
	},

	//=========================================================================
	
	'register': function(anInvitationCode) {
		var deferredResult;
		var	parameters;

//MochiKit.Logging.logError(">>> Connection.register: " + this);
		parameters = {};
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.register - 1: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'registration_verify');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.register - 2: " + res); return res;});
		deferredResult.addCallback(function(aConnection, anInvitationCode) {
			var args;
			
			args = {};
			args.message = 'register';
			args.version = aConnection.clipperz_pm_crypto_version();
			args.invitationCode = anInvitationCode;
			
			return args;
		}, this);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.register - 3: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'registration_sendingCredentials');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.register - 4: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this.user(), 'encryptedData'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.register - 5: " + res); return res;});
		deferredResult.addCallback(function(someParameters, anUser, anEncryptedData) {
			var currentVersionConnection;
			var args;

			currentVersionConnection = new Clipperz.PM.Crypto.communicationProtocol.versions['current']({user:anUser});
			
			args = someParameters
			args.credentials = currentVersionConnection.serverSideUserCredentials();
			args.user = anEncryptedData;
			args.version = args.credentials.version;
			args.message = "completeRegistration";

			return args;
		}, parameters, this.user());
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.register - 6: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Proxy.defaultProxy, 'registration'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.register - 7: " + Clipperz.Base.serializeJSON(res)); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			this.user().setLock(res['lock']);

			return res;
		}, this));
		deferredResult.callback(anInvitationCode);
//MochiKit.Logging.logError("<<< Connection.register");
		
		return deferredResult;
	},

	//=========================================================================

	'login': function(isReconnecting) {
		var	deferredResult;
		
//MochiKit.Logging.logDebug(">>> Connection.login: "/* + this*/);
//MochiKit.Logging.logDebug("--- Connection.login - isReconnecting: " + (isReconnecting == true));
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.3.1 - Connection.login - 1: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'connection_sendingCredentials');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.3.2 - Connection.login - 2: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(function(aConnection) {
			var args;
			
			args = {};
			args.message = 'connect';
			args.version = aConnection.clipperz_pm_crypto_version();
			args.parameters = {};
//MochiKit.Logging.logDebug("=== Connection.login - username: " + aConnection.srpConnection().C());
			args.parameters['C'] = aConnection.srpConnection().C();
			args.parameters['A'] = aConnection.srpConnection().A().asString(16);
			
			if (isReconnecting == true) {
//MochiKit.Logging.logDebug("--- Connection.login - reconnecting");
//#				args.parameters['reconnecting'] = "yes";
				args.parameters['reconnecting'] = aConnection.connectionId();
			}
//MochiKit.Logging.logDebug("--- Connection.login - args: " + Clipperz.Base.serializeJSON(args));
//MochiKit.Logging.logDebug("--- Connection.login - srp.a: " + aConnection.srpConnection().a().asString(16));

			return args;
		});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.3.3 - Connection.login - 3: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Proxy.defaultProxy, 'handshake'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.3.4 - Connection.login - 4: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'connection_credentialVerification');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.3.5 - Connection.login - 5: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
deferredResult.addErrback(MochiKit.Base.bind(function(res) {MochiKit.Logging.logDebug("ERROR - c: " + this.srpConnection().C() + " # version: " + this.clipperz_pm_crypto_version()); return res;}, this));
		deferredResult.addCallback(MochiKit.Base.bind(function(someParameters) {
			var args;

			this.srpConnection().set_s(new Clipperz.Crypto.BigInt(someParameters['s'], 16));
			this.srpConnection().set_B(new Clipperz.Crypto.BigInt(someParameters['B'], 16));

			if (typeof(someParameters['oneTimePassword']) != 'undefined') {
				this.setOneTimePassword(someParameters['oneTimePassword']);
			}
			
			args = {};
			args.message = 'credentialCheck';
			args.version = this.clipperz_pm_crypto_version();
			args.parameters = {};
			args.parameters['M1'] = this.srpConnection().M1();

			return args;
		}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.3.6 - Connection.login - 6: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Proxy.defaultProxy, 'handshake'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.3.7 - Connection.login - 7: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
//#		deferredResult.addCallback(MochiKit.Base.method(this, 'loginDone'));
		deferredResult.addCallback(MochiKit.Base.bind(function(someParameters) {
			var result;
		
//MochiKit.Logging.logDebug(">>> Connection.loginDone: " + this + " (M2: " + this.srpConnection().M2() + ")");
			if (someParameters['M2'] == this.srpConnection().M2()) {
				result = new MochiKit.Async.Deferred();
				
//MochiKit.Logging.logDebug("--- Connection.loginDone - someParameters: " + Clipperz.Base.serializeJSON(someParameters));
				this.setConnectionId(someParameters['connectionId']);
				this.user().setLoginInfo(someParameters['loginInfo']);
				this.user().setShouldDownloadOfflineCopy(someParameters['offlineCopyNeeded']);

				if ((isReconnecting == true) && (this.user().lock() != someParameters['lock'])) {
					throw Clipperz.PM.Connection.exception.StaleData;
				}
			
				if (this.oneTimePassword() != null) {
					result.addCallback(MochiKit.Base.method(this.user().oneTimePasswordManager(), 'archiveOneTimePassword', this.oneTimePassword()));
				}

				result.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'connection_loggedIn');
				result.addCallback(MochiKit.Async.succeed, someParameters);
				
				result.callback();
//MochiKit.Logging.logDebug("--- Connection.loginDone - 1 - result: "/* + Clipperz.Base.serializeJSON(result)*/);
			} else {
//MochiKit.Logging.logDebug("--- Connection.loginDone - 2 - ERROR");
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
				result = MochiKit.Async.fail(Clipperz.PM.Connection.exception.WrongChecksum);
			}
//MochiKit.Logging.logDebug("<<< Connection.loginDone - result: " + Clipperz.Base.serializeJSON(result));

			return result;
		}, this));

//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("1.3.8 - Connection.login - 8: "/* + res*/); return res;});
//deferredResult.addErrback(function(res) {MochiKit.Logging.logDebug("ERROR: " + res); return res;});
		deferredResult.callback(this);
//MochiKit.Logging.logDebug("<<< Connection.login");
		
		return deferredResult;
	},

	//=========================================================================

	'logout': function() {
		var deferredResult;

//MochiKit.Logging.logDebug(">>> Connection.logout: " + this);
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.logout - 1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Proxy.defaultProxy, 'logout'), {});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.logout - 2: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'resetSrpConnection'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.logout - 3: " + res); return res;});
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< Connection.logout");
		
		return deferredResult;
	},
	
	//=========================================================================

	'message': function(aMessageName, someParameters) {
		var args;
		var deferredResult;
		
//MochiKit.Logging.logDebug(">>> Connection.message: " + this);
		args = {}
		args['message'] = aMessageName;
		args['srpSharedSecret'] = this.srpConnection().K();
//		args['lock'] = this.user().lock();

		if (someParameters != null) {
			args['parameters'] = someParameters;
		} else {
			args['parameters'] = {};
		} 
//MochiKit.Logging.logDebug("--- Connection.message - args: " + Clipperz.Base.serializeJSON(args));

//		deferredResult = new MochiKit.Async.Deferred();		//	### ?????????????
		
		return this.sendMessage(args);
	},

	//-------------------------------------------------------------------------

	'sendMessage': function(someArguments) {
		var	deferredResult;
		
//MochiKit.Logging.logDebug(">>> Connection.sendMessage: " + this);
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.sendMessage - 1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Proxy.defaultProxy, 'message'), someArguments);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.sendMessage - 2: " + res); return res;});

		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			if (typeof(res['lock']) != 'undefined') {
				this.user().setLock(res['lock']);
			}
			return res;
		}, this));

		deferredResult.addErrback(MochiKit.Base.method(this, 'messageExceptionHandler'), someArguments);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.sendMessage - 3: " + res); return res;});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.sendMessage - 3: " + Clipperz.Base.serializeJSON(res)); return res;});
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< Connection.sendMessage");
		
		return deferredResult
	},
	
	//-------------------------------------------------------------------------

	'messageExceptionHandler': function(anOriginalMessageArguments, anError) {
		var result;

//MochiKit.Logging.logDebug(">>> Connection.messageExceptionHandler - this: " + this + ", anError: " + anError);
		if (anError instanceof MochiKit.Async.CancelledError) {
//MochiKit.Logging.logDebug("--- Connection.messageExceptionHandler - 1");
			result = anError;
//MochiKit.Logging.logDebug("--- Connection.messageExceptionHandler - 2");
		} else {
//MochiKit.Logging.logDebug("--- Connection.messageExceptionHandler - 3 - anError.name: " + anError.name + ", message: " + anError.message);
			if ((anError.message == 'Trying to communicate without an active connection')	||
				(anError.message == 'No tollManager available for current session')		
			) {
//MochiKit.Logging.logDebug("--- Connection.messageExceptionHandler - 4");
				result = this.reestablishConnection(anOriginalMessageArguments);
//MochiKit.Logging.logDebug("--- Connection.messageExceptionHandler - 5");
			} else if (anError.message == 'Session with stale data') {
//MochiKit.Logging.logDebug("--- Connection.messageExceptionHandler - 5.1");
				Clipperz.NotificationCenter.notify(this, 'EXCEPTION');
//MochiKit.Logging.logDebug("--- Connection.messageExceptionHandler - 5.2");
			} else {
//MochiKit.Logging.logDebug("--- Connection.messageExceptionHandler - 6");
				result = anError;
//MochiKit.Logging.logDebug("--- Connection.messageExceptionHandler - 7");
			}
//MochiKit.Logging.logDebug("--- Connection.messageExceptionHandler - 8");
		}
//MochiKit.Logging.logDebug("<<< Connection.messageExceptionHandler");
		
		return result;;
	},
	
	//=========================================================================

	'reestablishConnection': function(anOriginalMessageArguments) {
		var deferredResult;

//MochiKit.Logging.logDebug("+++ Connection.reestablishConnection: " + Clipperz.Base.serializeJSON(anOriginalMessageArguments));

//MochiKit.Logging.logDebug(">>> Connection.reestablishConnection: " + this);
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.reestablishConnection 1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'resetSrpConnection'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.reestablishConnection 2: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'login'), true);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.reestablishConnection 3: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(aMessage) {
			aMessage['srpSharedSecret'] = this.srpConnection().K();
			return aMessage;
		}, this), anOriginalMessageArguments);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.reestablishConnection 4: " + Clipperz.Base.serializeJSON(res)); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'sendMessage'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.reestablishConnection 5: " + res); return res;});
		deferredResult.addErrback(Clipperz.NotificationCenter.deferredNotification, this, 'EXCEPTION', null);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Connection.reestablishConnection 6: " + res); return res;});
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< Connection.reestablishConnection");

		return deferredResult;
	},

	//=========================================================================

	'sharedSecret': function() {
		return this.srpConnection().K();
	},

	//=========================================================================

	'serverSideUserCredentials': function() {
		var	result;
		var	newSrpConnection;

//MochiKit.Logging.logDebug(">>> Connection.serverSideUserCredentials");
		newSrpConnection = new Clipperz.Crypto.SRP.Connection({ C:this.C(), P:this.P(), hash:this.hash() });
		result = newSrpConnection.serverSideCredentials();
		result['version'] = this.clipperz_pm_crypto_version();

//MochiKit.Logging.logDebug("<<< Connection.serverSideUserCredentials - result: " + Clipperz.Base.serializeJSON(result));
		return result;
	},

	//=========================================================================

	'C': function() {
		if (this._C == null) {
			this._C = this.hash()(new Clipperz.ByteArray(this.user().username())).toHexString().substring(2);
		}
		
		return this._C;
	},

	//-----------------------------------------------------------------------------

	'P': function() {
		if (this._P == null) {
			this._P = this.hash()(new Clipperz.ByteArray(this.user().passphrase() + this.user().username())).toHexString().substring(2);
		}
		
		return this._P;
	},

	//-----------------------------------------------------------------------------

	'hash': function() {
		return Clipperz.PM.Crypto.encryptingFunctions.versions['0.1'].hash;
	},
	
	//-----------------------------------------------------------------------------

	'srpConnection': function() {
		if (this._srpConnection == null) {
			this._srpConnection = new Clipperz.Crypto.SRP.Connection({ C:this.C(), P:this.P(), hash:this.hash() });
		}
		
		return this._srpConnection;
	},
	
	'resetSrpConnection': function() {
		this._C = null;
		this._P = null;
		this._srpConnection = null;
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
	args = args || {};
	Clipperz.PM.Connection.SRP['1.0'].call(this, args);
	
	return this;
}

Clipperz.PM.Connection.SRP['1.1'].prototype = MochiKit.Base.update(new Clipperz.PM.Connection.SRP['1.0'](), {

	'version': function() {
		return '1.1';
	},

	//-----------------------------------------------------------------------------

	'C': function() {
		if (this._C == null) {
			this._C = this.hash()(new Clipperz.ByteArray(this.user().username() + this.user().passphrase())).toHexString().substring(2);
		}
		
		return this._C;
	},

	//-----------------------------------------------------------------------------

	'P': function() {
		if (this._P == null) {
			this._P = this.hash()(new Clipperz.ByteArray(this.user().passphrase() + this.user().username())).toHexString().substring(2);
		}
		
		return this._P;
	},
	
	//-----------------------------------------------------------------------------

	'hash': function() {
		return Clipperz.PM.Crypto.encryptingFunctions.versions['0.2'].hash;
	},

	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});

Clipperz.PM.Connection.exception = {
	WrongChecksum: new MochiKit.Base.NamedError("Clipperz.ByteArray.exception.InvalidValue"),
	StaleData: new MochiKit.Base.NamedError("Stale data")
};
