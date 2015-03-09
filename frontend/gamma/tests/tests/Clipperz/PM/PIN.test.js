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

Clipperz.PM.PIN['CREDENTIALS'] =	'TEST.CLIPPERZ.CREDENTIALS';
Clipperz.PM.PIN['FAILURE_COUNT'] =	'TEST.CLIPPERZ.FAILED_LOGIN_COUNT';


var tests = {

    //-------------------------------------------------------------------------

    'clearCredentials': function () {
		localStorage.removeItem(Clipperz.PM.PIN['CREDENTIALS']);
		localStorage.removeItem(Clipperz.PM.PIN['FAILURE_COUNT']);
    },

    //-------------------------------------------------------------------------

	'isSet': function () {
		tests.clearCredentials();

		is(false, Clipperz.PM.PIN.isSet(), "after cleaning all values, credentials should not be set");

		Clipperz.PM.PIN.setCredentialsWithPIN("1234", {'username':'joe', 'passphrase':'eoj'});
		is(true, Clipperz.PM.PIN.isSet(), "once saved, they should be found");
	},

	'recordFailedAttempt': function () {
		tests.clearCredentials();

		Clipperz.PM.PIN.setCredentialsWithPIN("1234", {'username':'joe', 'passphrase':'eoj'});
		is(true, Clipperz.PM.PIN.isSet(), "once saved, they should be found");
		Clipperz.PM.PIN.recordFailedAttempt();		
		is(true, Clipperz.PM.PIN.isSet(), "1st wrong PIN -> keep credentials");
		Clipperz.PM.PIN.recordFailedAttempt();		
		is(true, Clipperz.PM.PIN.isSet(), "2nd wrong PIN -> keep credentials");
		Clipperz.PM.PIN.recordFailedAttempt();		
		is(false, Clipperz.PM.PIN.isSet(), "3rd wrong PIN -> REMOVE credentials");

		Clipperz.PM.PIN.setCredentialsWithPIN("1234", {'username':'joe', 'passphrase':'eoj'});
		is(true, Clipperz.PM.PIN.isSet(), "once saved, they should be found");
		Clipperz.PM.PIN.recordFailedAttempt();		
		is(true, Clipperz.PM.PIN.isSet(), "1st wrong PIN -> keep credentials");
		Clipperz.PM.PIN.recordFailedAttempt();		
		is(true, Clipperz.PM.PIN.isSet(), "2nd wrong PIN -> keep credentials");
		Clipperz.PM.PIN.resetFailedAttemptCount();
		Clipperz.PM.PIN.recordFailedAttempt();		
		is(true, Clipperz.PM.PIN.isSet(), "3rd wrong PIN, but with a successful use in between -> keep credentials");
	},

	'credentialsWithPIN': function () {
		var	credentials;
		var	pin;
		var decryptedCredentials;

		tests.clearCredentials();

		credentials = {'username': 'joe', 'passphrase':'foobar'};
		pin = '1234';
		Clipperz.PM.PIN.setCredentialsWithPIN(pin, credentials);
		decryptedCredentials = Clipperz.PM.PIN.credentialsWithPIN(pin);

		is(decryptedCredentials['username'],	credentials['username']);
		is(decryptedCredentials['passphrase'],	credentials['passphrase']);
	},

    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};

//#############################################################################

SimpleTest.runDeferredTests("Clipperz.PM.PIN", tests, {trace:false});
