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

Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();

var tests = {

    //-------------------------------------------------------------------------

	'simple_tests': function () {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("simple_tests", {trace:false});
		deferredResult.addCallback(function() {
			var connection;
		
			SimpleTest.is(Clipperz.PM.Connection.communicationProtocol.versions['current'], Clipperz.PM.Connection.SRP['1.1'], "the current connection is 1.1");

			connection = new Clipperz.PM.Connection.communicationProtocol.versions['current']();
			SimpleTest.ok(connection != null, "can create a connection with the 'current' communication protocol");

			SimpleTest.is(connection.proxy(), Clipperz.PM.Proxy.defaultProxy, "the connection uses the defaultProxy if no proxy is specified on the constructor");
		});
		
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'simpleProxy_tests': function () {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("simple_tests", {trace:false});
		deferredResult.addCallback(function() {
			var connection;
			var proxy;

			proxy = new Clipperz.PM.Proxy();
			connection = new Clipperz.PM.Connection.communicationProtocol.versions['current']({proxy:proxy});
			SimpleTest.is(connection.proxy(), proxy, "the connection uses the specified proxy, when present");
		});
		
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'login_test': function () {
		var deferredResult;
		var connection;
		var proxy;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:false, shouldCheckExpectedRequests:true});
		connection = new Clipperz.PM.Connection.communicationProtocol.versions['current']({
			proxy: proxy,
			getCredentialsFunction: function() { return {username:'joe', password:'clipperz'}; }
		});
		
		deferredResult = new Clipperz.Async.Deferred("login_tests", {trace:false});
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addCallback(function() {
			proxy.expectedRequests().unshift({
				name: "First handshake step",
				functionName: 'handshake',
				parameters: {
					parameters: {
						message: MochiKit.Base.partial(MochiKit.Base.operator.eq, "connect"),
						version: MochiKit.Base.partial(MochiKit.Base.operator.eq, "0.2"),
						parameters: {
							C: MochiKit.Base.partial(MochiKit.Base.operator.eq, "f527cdd90d0d47f8524b4e165398ad1455eba515d04abd101d1e93b3c6ae0674"),
							A: MochiKit.Base.operator.truth
						}
					}
				}
			});		
		});
		deferredResult.addCallback(function() {
			proxy.expectedRequests().unshift({
				name: "Second handshake step",
				functionName: 'handshake',
				parameters: {
					parameters: {
						message: MochiKit.Base.partial(MochiKit.Base.operator.eq, "credentialCheck"),
						version: MochiKit.Base.partial(MochiKit.Base.operator.eq, "0.2"),
						parameters: {
							M1: MochiKit.Base.operator.truth
						}
					}
				}
			});		
		});
		deferredResult.addMethod(connection, 'login'/*, 'joe', 'clipperz'*/);
		deferredResult.addCallback(function(aResult) {
			SimpleTest.is(aResult.result, 'done', "successfully logged in");
		})
		deferredResult.addErrback(SimpleTest.ok, false, "the login procedure should succeed, and NOT going through this code path");
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'login_with_toll_test': function () {
		var deferredResult;
		var connection;
		var proxy;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, shouldCheckExpectedRequests:true});
		connection = new Clipperz.PM.Connection.communicationProtocol.versions['current']({
			proxy:proxy,
			getCredentialsFunction: function() { return {username:'joe', password:'clipperz'}; }
		});
		
		deferredResult = new Clipperz.Async.Deferred("login_with_toll_test", {trace:false});
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addCallback(function() {
			proxy.expectedRequests().unshift({
				name: "knock step",
				functionName: 'knock',
				parameters: {
					requestType: MochiKit.Base.partial(MochiKit.Base.operator.eq, "CONNECT")
				}
			});		
		});
		deferredResult.addCallback(function() {
			proxy.expectedRequests().unshift({
				name: "First handshake step",
				functionName: 'handshake',
				parameters: {
					parameters: {
						message: MochiKit.Base.partial(MochiKit.Base.operator.eq, "connect"),
						version: MochiKit.Base.partial(MochiKit.Base.operator.eq, "0.2"),
						parameters: {
							C: MochiKit.Base.partial(MochiKit.Base.operator.eq, "f527cdd90d0d47f8524b4e165398ad1455eba515d04abd101d1e93b3c6ae0674"),
							A: MochiKit.Base.operator.truth
						}
					},
					toll: {
						targetValue: MochiKit.Base.operator.truth,
						toll:		 MochiKit.Base.operator.truth
					}
				}
			});		
		});
		deferredResult.addCallback(function() {
			proxy.expectedRequests().unshift({
				name: "Second handshake step",
				functionName: 'handshake',
				parameters: {
					parameters: {
						message: MochiKit.Base.partial(MochiKit.Base.operator.eq, "credentialCheck"),
						version: MochiKit.Base.partial(MochiKit.Base.operator.eq, "0.2"),
						parameters: {
							M1: MochiKit.Base.operator.truth
						}
					},
					toll: {
						targetValue: MochiKit.Base.operator.truth,
						toll:		 MochiKit.Base.operator.truth
					}
				}
			});		
		});
		deferredResult.addMethod(connection, 'login'/*, 'joe', 'clipperz'*/);
		deferredResult.addCallback(function(aResult) {
			SimpleTest.is(aResult.result, 'done', "successfully logged in");
		})
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'getUserDetails_test': function () {
		var deferredResult;
		var connection;
		var proxy;
		
		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, shouldCheckExpectedRequests:false});
		connection = new Clipperz.PM.Connection.communicationProtocol.versions['current']({
			proxy:proxy,
			getCredentialsFunction: function() { return {username:'joe', password:'clipperz'}; }
		});
		
		deferredResult = new Clipperz.Async.Deferred("getUserDetails_test", {trace:false});
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(connection, 'login'/*, 'joe', 'clipperz'*/);
		deferredResult.addCallback(function(aResult) {
			SimpleTest.is(aResult.result, 'done', "successfully logged in");
		});
		deferredResult.addMethod(proxy, 'setShouldCheckExpectedRequests', [
			{
				name: "Get user details",
				functionName: 'message',
				parameters: {
					parameters: {
						message: MochiKit.Base.partial(MochiKit.Base.operator.eq, "getUserDetails"),
						srpSharedSecret: MochiKit.Base.operator.truth
					},
					toll: {
						targetValue: MochiKit.Base.operator.truth,
						toll:		 MochiKit.Base.operator.truth
					}
				}
			}		
		]);
		deferredResult.addMethod(connection, 'message', 'getUserDetails');
		deferredResult.addCallback(function(aResult) {
			SimpleTest.ok(aResult['header'] != null, "got user details - header");
			SimpleTest.ok(aResult['statistics'] != null, "got user details - statistics");
			SimpleTest.ok(aResult['version'] == '0.3', "got user details - version");
		});
		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};

//#############################################################################

SimpleTest.runDeferredTests("Clipperz.PM.Connection", tests, {trace:false});
