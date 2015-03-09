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

	'test1_test': function (someTestArgs) {
		var deferredResult;
		var	keePassProcessor;

		keePassProcessor = new Clipperz.KeePassExportProcessor();
	
		deferredResult = new Clipperz.Async.Deferred("test1_test", someTestArgs);
		deferredResult.addCallback(function(aResult) { return "[Gmail]\nGroup: General\nUserName: joe.clipperz\nURL: http://www.gmail.com\nPassword: NHy08ZCMYsqUeLQCawR7\n\n[del.icio.us]\nGroup: General\nUserName: joe69\nURL: http://del.icio.us\nPassword: tS1cIEeqp5y0wkU\n\n[Amazon]\nGroup: General\nUserName: jclipperz\nURL: http://www.amazon.com\nPassword: wvpkqNPIsqlI5g6XE9Tz\n\n[Paypal]\nGroup: General\nUserName: joeclipperz\nURL: http://www.paypal.com\nPassword: 24T4wIcvHnM28T3L\n\n[Technorati]\nGroup: General\nUserName: jclipperz\nURL: http://www.technorati.com\nPassword: UcVeNqF\n\n[American Airlines]\nGroup: General\nUserName: joe.clipperz\nURL: http://www.aa.com\nPassword: AtrYbmi7lmSjR\n" });
		deferredResult.addMethod(keePassProcessor, 'deferredParse');
		deferredResult.addCallback(function(aResult) { is(MochiKit.Base.serializeJSON(aResult), "[{\"Title\":\"Gmail\", \"Group\":\"General\", \"UserName\":\"joe.clipperz\", \"URL\":\"http://www.gmail.com\", \"Password\":\"NHy08ZCMYsqUeLQCawR7\"}, {\"Title\":\"del.icio.us\", \"Group\":\"General\", \"UserName\":\"joe69\", \"URL\":\"http://del.icio.us\", \"Password\":\"tS1cIEeqp5y0wkU\"}, {\"Title\":\"Amazon\", \"Group\":\"General\", \"UserName\":\"jclipperz\", \"URL\":\"http://www.amazon.com\", \"Password\":\"wvpkqNPIsqlI5g6XE9Tz\"}, {\"Title\":\"Paypal\", \"Group\":\"General\", \"UserName\":\"joeclipperz\", \"URL\":\"http://www.paypal.com\", \"Password\":\"24T4wIcvHnM28T3L\"}, {\"Title\":\"Technorati\", \"Group\":\"General\", \"UserName\":\"jclipperz\", \"URL\":\"http://www.technorati.com\", \"Password\":\"UcVeNqF\"}, {\"Title\":\"American Airlines\", \"Group\":\"General\", \"UserName\":\"joe.clipperz\", \"URL\":\"http://www.aa.com\", \"Password\":\"AtrYbmi7lmSjR\"}]", "first test"); });
		deferredResult.addErrback(function(anError) { is("ERROR", anError) });

		deferredResult.callback();
		
		return deferredResult;
	},

	'test2_test': function (someTestArgs) {
		var deferredResult;
		var	keePassProcessor;

		keePassProcessor = new Clipperz.KeePassExportProcessor();
	
		deferredResult = new Clipperz.Async.Deferred("test2_test", someTestArgs);
		deferredResult.addCallback(function(aResult) { return "[Gmail]\nGroup: General\nUserName: joe.clipperz\nURL: http://www.gmail.com\nPassword: NHy08ZCMYsqUeLQCawR7\nNotes: Personal account\n\n[del.icio.us]\nGroup: General\nUserName: joe69\nURL: http://del.icio.us\nPassword: tS1cIEeqp5y0wkU\nNotes: social bookmarking site\n\n[Amazon]\nGroup: General\nUserName: jclipperz\nURL: http://www.amazon.com\nPassword: wvpkqNPIsqlI5g6XE9Tz\nNotes: The US online store\n\n[Paypal]\nGroup: General\nUserName: joeclipperz\nURL: http://www.paypal.com\nPassword: 24T4wIcvHnM28T3L\nNotes: Linked to my savings account\n\n[Technorati]\nGroup: General\nUserName: jclipperz\nURL: http://www.technorati.com\nPassword: UcVeNqF\nNotes: Blog ranking and searching\n\n[American Airlines]\nGroup: General\nUserName: joe.clipperz\nURL: http://www.aa.com\nPassword: AtrYbmi7lmSjR\nNotes: Adavantages card n. 795495\n" });
		deferredResult.addMethod(keePassProcessor, 'deferredParse');
		deferredResult.addCallback(function(aResult) { is(MochiKit.Base.serializeJSON(aResult), "[{\"Title\":\"Gmail\", \"Group\":\"General\", \"UserName\":\"joe.clipperz\", \"URL\":\"http://www.gmail.com\", \"Password\":\"NHy08ZCMYsqUeLQCawR7\", \"Notes\":\"Personal account\"}, {\"Title\":\"del.icio.us\", \"Group\":\"General\", \"UserName\":\"joe69\", \"URL\":\"http://del.icio.us\", \"Password\":\"tS1cIEeqp5y0wkU\", \"Notes\":\"social bookmarking site\"}, {\"Title\":\"Amazon\", \"Group\":\"General\", \"UserName\":\"jclipperz\", \"URL\":\"http://www.amazon.com\", \"Password\":\"wvpkqNPIsqlI5g6XE9Tz\", \"Notes\":\"The US online store\"}, {\"Title\":\"Paypal\", \"Group\":\"General\", \"UserName\":\"joeclipperz\", \"URL\":\"http://www.paypal.com\", \"Password\":\"24T4wIcvHnM28T3L\", \"Notes\":\"Linked to my savings account\"}, {\"Title\":\"Technorati\", \"Group\":\"General\", \"UserName\":\"jclipperz\", \"URL\":\"http://www.technorati.com\", \"Password\":\"UcVeNqF\", \"Notes\":\"Blog ranking and searching\"}, {\"Title\":\"American Airlines\", \"Group\":\"General\", \"UserName\":\"joe.clipperz\", \"URL\":\"http://www.aa.com\", \"Password\":\"AtrYbmi7lmSjR\", \"Notes\":\"Adavantages card n. 795495\"}]", "second test"); });
		deferredResult.addErrback(function(anError) { is("ERROR", anError) });

		deferredResult.callback();
		
		return deferredResult;
	},

	'test3_test': function (someTestArgs) {
		var deferredResult;
		var	keePassProcessor;

		keePassProcessor = new Clipperz.KeePassExportProcessor();
	
		deferredResult = new Clipperz.Async.Deferred("test3_test", someTestArgs);
		deferredResult.addCallback(function(aResult) { return "[Gmail]\nGroup: General\nUserName: joe.clipperz\nURL: http://www.gmail.com\nPassword: NHy08ZCMYsqUeLQCawR7\nNotes: Personal account\nwith some notes stored\non multiple lines\n\n[del.icio.us]\nGroup: General\nUserName: joe69\nURL: http://del.icio.us\nPassword: tS1cIEeqp5y0wkU\nNotes: social bookmarking site\n\n[Amazon]\nGroup: General\nUserName: jclipperz\nURL: http://www.amazon.com\nPassword: wvpkqNPIsqlI5g6XE9Tz\nNotes: The US online store\n\n[Paypal]\nGroup: General\nUserName: joeclipperz\nURL: http://www.paypal.com\nPassword: 24T4wIcvHnM28T3L\nNotes: Linked to my savings account\n\n[Technorati]\nGroup: General\nUserName: jclipperz\nURL: http://www.technorati.com\nPassword: UcVeNqF\nNotes: Blog ranking and searching\n\n[American Airlines]\nGroup: General\nUserName: joe.clipperz\nURL: http://www.aa.com\nPassword: AtrYbmi7lmSjR\nNotes: Adavantages card n. 795495\n" });
		deferredResult.addMethod(keePassProcessor, 'deferredParse');
		deferredResult.addCallback(function(aResult) { is(MochiKit.Base.serializeJSON(aResult), "[{\"Title\":\"Gmail\", \"Group\":\"General\", \"UserName\":\"joe.clipperz\", \"URL\":\"http://www.gmail.com\", \"Password\":\"NHy08ZCMYsqUeLQCawR7\", \"Notes\":\"Personal account\\nwith some notes stored\\non multiple lines\\n\"}, {\"Title\":\"del.icio.us\", \"Group\":\"General\", \"UserName\":\"joe69\", \"URL\":\"http://del.icio.us\", \"Password\":\"tS1cIEeqp5y0wkU\", \"Notes\":\"social bookmarking site\"}, {\"Title\":\"Amazon\", \"Group\":\"General\", \"UserName\":\"jclipperz\", \"URL\":\"http://www.amazon.com\", \"Password\":\"wvpkqNPIsqlI5g6XE9Tz\", \"Notes\":\"The US online store\"}, {\"Title\":\"Paypal\", \"Group\":\"General\", \"UserName\":\"joeclipperz\", \"URL\":\"http://www.paypal.com\", \"Password\":\"24T4wIcvHnM28T3L\", \"Notes\":\"Linked to my savings account\"}, {\"Title\":\"Technorati\", \"Group\":\"General\", \"UserName\":\"jclipperz\", \"URL\":\"http://www.technorati.com\", \"Password\":\"UcVeNqF\", \"Notes\":\"Blog ranking and searching\"}, {\"Title\":\"American Airlines\", \"Group\":\"General\", \"UserName\":\"joe.clipperz\", \"URL\":\"http://www.aa.com\", \"Password\":\"AtrYbmi7lmSjR\", \"Notes\":\"Adavantages card n. 795495\"}]", "third test"); });
		deferredResult.addErrback(function(anError) { is("ERROR", anError) });

		deferredResult.callback();
		
		return deferredResult;
	},

    //-------------------------------------------------------------------------

	'Barb_Newman_test': function (someTestArgs) {
		var deferredResult;
		var	keePassProcessor;

		keePassProcessor = new Clipperz.KeePassExportProcessor();
	
		deferredResult = new Clipperz.Async.Deferred("Barb_Newman_test", someTestArgs);
//		deferredResult.addCallback(function () { return testData['Barb_Newman_data']; });
		deferredResult.addMethod(keePassProcessor, 'deferredParse');
		deferredResult.addBoth(function(aResult) {
			SimpleTest.is(aResult.length, 121, "Barb's data contains 121 records");
			return aResult;
		});
		deferredResult.addBoth(function(aResult) {
			SimpleTest.is(MochiKit.Base.serializeJSON(aResult[0]), "{\"Title\":\"Domain Administration Account\", \"Group\":\"Windows\", \"User Name\":\"_ABC\", \"URL\":\"\", \"Password\":\"123456\", \"Notes\":\"\"}", "Barb's first record data matches");
			return aResult;
		});
//		deferredResult.callback();
		deferredResult.callback(testData['Barb_Newman_data']);

		return deferredResult;
	},
	
    //-------------------------------------------------------------------------
/*
	'Jordan_Curzon_test': function (someTestArgs) {
		var deferredResult;
		var	keePassProcessor;

		keePassProcessor = new Clipperz.KeePassExportProcessor();
	
		deferredResult = new Clipperz.Async.Deferred("Jordan_Curzon_test", someTestArgs);
		deferredResult.addCallback(function () { return testData['Jordan_Curzon_data']; });
		deferredResult.addMethod(keePassProcessor, 'deferredParse');
		deferredResult.addBoth(function(aResult) {
			SimpleTest.is(aResult.length, 2, "Jordarn's data contains 2 records");
			return aResult;
		});
		deferredResult.addBoth(function(aResult) {
			SimpleTest.is(MochiKit.Base.serializeJSON(aResult[0]), "{\"Title\":\"Allstate\", \"Username\":\"jones\", \"Url\":\"allstate.com\", \"Password\":\"bobfred\", \"Comment\":\"\"}", "Barb's first record data matches");
			return aResult;
		});
		deferredResult.callback();

		return deferredResult;
	},
*/	
    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};



//#############################################################################

SimpleTest.runDeferredTests("Clipperz.KeyPassExportProcessor", tests, {trace:false});
