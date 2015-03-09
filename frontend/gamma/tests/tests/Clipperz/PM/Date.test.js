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

var tests = {

    //-------------------------------------------------------------------------

	'offlineDataParsing_test': function () {
		var value;
		var	parsedValue;
		var expectedValue;

		value = 'Tue Mar 27 06:41:37 PDT 2007';
		parsedValue = Clipperz.PM.Date.parse(value);
		expectedValue = new Date();
		expectedValue.setUTCFullYear(2007, 03 - 1, 27);
		expectedValue.setUTCHours(13, 41, 37);
//		SimpleTest.is(parsedValue.toString(), 'Tue Mar 27 2007 15:41:37 GMT+0200 (CEST)', "the data in the format stored in the offline copy is processed correctly [1]");
		SimpleTest.is(parsedValue.toString(), expectedValue.toString(), "the data in the format stored in the offline copy is processed correctly [1]");

		value = 'Thu May 10 15:01:21 PDT 2007';
		parsedValue = Clipperz.PM.Date.parse(value);
		expectedValue = new Date();
		expectedValue.setUTCFullYear(2007, 05 - 1, 10);
		expectedValue.setUTCHours(22, 01, 21);
//		SimpleTest.is(parsedValue.toString(), 'Fri May 11 2007 00:01:21 GMT+0200 (CEST)', "the data in the format stored in the offline copy is processed correctly [3]");
		SimpleTest.is(parsedValue.toString(), expectedValue.toString(), "the data in the format stored in the offline copy is processed correctly [3]");

		value = 'Thu May 10 15:01:21 PST 2007';
		parsedValue = Clipperz.PM.Date.parse(value);
		expectedValue = new Date();
		expectedValue.setUTCFullYear(2007, 05 - 1, 10);
		expectedValue.setUTCHours(23, 01, 21);
//		SimpleTest.is(parsedValue.toString(), 'Fri May 11 2007 01:01:21 GMT+0200 (CEST)', "the data in the format stored in the offline copy is processed correctly [3]");
		SimpleTest.is(parsedValue.toString(), expectedValue.toString(), "the data in the format stored in the offline copy is processed correctly [3]");
	},

    //-------------------------------------------------------------------------

	'onlineDataParsing_test': function () {
		var value;
		var	parsedValue;
		var expectedValue;

		value = 'Tue, 27 March 2007 06:41:37 PDT';
		parsedValue = Clipperz.PM.Date.parse(value);
		expectedValue = new Date();
		expectedValue.setUTCFullYear(2007, 03 - 1, 27);
		expectedValue.setUTCHours(13, 41, 37);

//		SimpleTest.is(parsedValue.toString(), 'Tue Mar 27 2007 15:41:37 GMT+0200 (CEST)', "the data in the format returned by the server is processed correctly");
		SimpleTest.is(parsedValue.toString(), expectedValue.toString(), "the data in the format returned by the server is processed correctly");
	},

    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};

//#############################################################################

Clipperz.PM.Strings.Languages.initSetup();
SimpleTest.runDeferredTests("Clipperz.PM.Date", tests, {trace:false});



