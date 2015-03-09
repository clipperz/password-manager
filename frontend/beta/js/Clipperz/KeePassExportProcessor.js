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


Clipperz.KeePassExportProcessor = function(args) {
	args = args || {};

	return this;
}

//=============================================================================

Clipperz.KeePassExportProcessor.prototype = MochiKit.Base.update(null, {

	//-------------------------------------------------------------------------
/*
	'parse': function(aValue) {
		var result;

//MochiKit.Logging.logDebug(">>> KeePassExportProcessor.parse");
		result = [];
//MochiKit.Logging.logDebug("--- KeePassExportProcessor.parse - result: " + Clipperz.Base.serializeJSON(result));
//MochiKit.Logging.logDebug("<<< KeePassExportProcessor.parse");

		return result;
	},
*/
	//-------------------------------------------------------------------------

	'deferredParse_core': function(aContext) {
		var deferredResult;

//MochiKit.Logging.logDebug(">>> KeePassExportProcessor.deferredParse_core");		
//MochiKit.Logging.logDebug("--- KeePassExportProcessor.deferredParse_core - (1) aContext.line: " + aContext.line.replace(/\n/g, "\\n").substring(0,50));		
//MochiKit.Logging.logDebug("--- KeePassExportProcessor.deferredParse_core - aContext: " + Clipperz.Base.serializeJSON(aContext).substring(0,50));		
//MochiKit.Logging.logDebug("--- KeePassExportProcessor.deferredParse_core - (2) aContext.line: " + aContext.line.replace(/\n/g, "\\n").substring(0,50));		
//console.log("deferredParse_core - aContext", aContext);
//MochiKit.Logging.logDebug("--- KeePassExportProcessor.deferredParse_core - (3) aContext.line: " + aContext.line.replace(/\n/g, "\\n").substring(0,50));		
		if (aContext.line == "") {
			deferredResult = MochiKit.Async.succeed(aContext.result);
		} else {
			var record;
			
			record = this.parseRecord(aContext);
			if (record != null) {
				aContext.result.push(record);
			}

//MochiKit.Logging.logDebug("--> KeePassExportProcessor.deferredParse_core - aContext.line: " + aContext.line.replace(/\n/g, "\\n").substring(0,50));		
			aContext.line = aContext.line.replace(/^\n*/g, "").replace(/\n$/g, "");
//MochiKit.Logging.logDebug("<-- KeePassExportProcessor.deferredParse_core - aContext.line: " + aContext.line.replace(/\n/g, "\\n").substring(0,50));		
			
			deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassExportProcessor.deferredParse_core - 1.1 " + res); return res;});
			deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'importProcessorProgressUpdate', {status:'processing', size:aContext.size, progress:(aContext.size - aContext.line.length)});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassExportProcessor.deferredParse_core - 1.2 " + res); return res;});
			deferredResult.addCallback(MochiKit.Async.wait, 0.2);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassExportProcessor.deferredParse_core - 1.3 " + res); return res;});
//deferredResult.addBoth(function(res) {console.log("KeePassExportProcessor.deferredParse_core - 1.3 ", res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this, 'deferredParse_core'))
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassExportProcessor.deferredParse_core - 1.4 " + res); return res;});
			deferredResult.callback(aContext);
		}
//MochiKit.Logging.logDebug("<<< KeePassExportProcessor.deferredParse_core");		
		
		return deferredResult;
	},
	
	//.........................................................................
	
	'deferredParse': function(aValue) {
		var deferredResult;
		var lines;
		var context;

//MochiKit.Logging.logDebug(">>> KeePassExportProcessor.deferredParse");
//MochiKit.Logging.logDebug("--- KeePassExportProcessor.deferredParse - aValue: " + aValue.length);
		lines = aValue.replace(/\r?\n/g, "\n");
//MochiKit.Logging.logDebug("--- KeePassExportProcessor.deferredParse - lines: " + lines.length);
		context = {
			line: lines,
			size: lines.length,
			result: []
		}
//MochiKit.Logging.logDebug("--- KeePassExportProcessor.deferredParse - context: " + Clipperz.Base.serializeJSON(context).substring(0,50));
//console.log("--- KeePassExportProcessor.deferredParse - context: ", context);

		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassExportProcessor.deferredParse - 1 " + res); return res;});
//deferredResult.addBoth(function(res) {console.log("KeePassExportProcessor.deferredParse - 1 ", res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'deferredParse_core'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassExportProcessor.deferredParse - 2 " + res); return res;});
		deferredResult.callback(context);
//MochiKit.Logging.logDebug("<<< KeePassExportProcessor.deferredParse");

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'parseRecord': function(aContext) {
		var result;
		var recordLabelRegexp;
		var	fieldLabelRegexp;
		var fieldValueRegexp;
		var fullLineRegexp;
/*
[Record name]
Group Tree: 
UserName: 
URL: 
Password: 
Notes: test
UUID: 525f62430079bae48b79ed2961924b05
Icon: 0
Creation Time: 2007-06-26 17:56:03
Last Access: 2007-10-25 16:23:51
Last Modification: 2007-10-25 16:23:51
Expires: 2999-12-28 23:59:59

[Record name]								==> Title
Group: General								==> Group
Group Tree: 								==> Group Tree
UserName: 									==> UserName
URL: 										==>	URL
Password: 									==>	Password
Notes: test									==>	Notes
UUID: 525f62430079bae48b79ed2961924b05		==>	UUID
Icon: 0										==>	Icon
Creation Time: 2007-06-26 17:56:03			==>	Creation Time
Last Access: 2007-10-25 16:23:51			==>	Last Access
Last Modification: 2007-10-25 16:23:51		==>	Last Modification
Expires: 2999-12-28 23:59:59				==> Expires
Attachment Description:						==> Attachment Description
Attachment:									==> Attachment
*/
		recordLabelRegexp = new RegExp("^\\[(.*)\\]\\n");
//		recordLabelRegexp = new RegExp("^\\[(.*)\\]$", "m");
		fieldLabelRegexp = new RegExp("^(Group|Group Tree|UserName|URL|Password|Notes|UUID|Icon|Creation Time|Last Access|Last Modification|Expires|Attachment Description|Attachment|Valid until): ");
		fieldValueRegexp = new RegExp("(.*)(\\n|$)");
		fullLineRegexp = new RegExp("^(.*\\n)");


		if (recordLabelRegexp.test(aContext.line) == true) {
			var line;
			
//MochiKit.Logging.logDebug("1.0");
			line = aContext.line;
//MochiKit.Logging.logDebug("0 - line: " + line.replace(/\n/g, "\\n").substring(0,50));
			
			result = {};
			result['Title'] = line.match(recordLabelRegexp)[1];
//MochiKit.Logging.logDebug("1 - title: " + result['Title']);
			line = line.replace(/^.*\n/, "");
//MochiKit.Logging.logDebug("2 - line: " + line.replace(/\n/g, "\\n").substring(0,50));
//MochiKit.Logging.logDebug("=======================================");
			while (fieldLabelRegexp.test(line) == true) {
				var fieldName;
				var fieldValue;
				
				fieldName = RegExp.$1;
//MochiKit.Logging.logDebug("3 - fieldName: " + fieldName);
				line = RegExp.rightContext;
//MochiKit.Logging.logDebug("4 - line: " + line.replace(/\n/g, "\\n").substring(0,50));
				
				fieldValue = line.match(fieldValueRegexp)[1];
//MochiKit.Logging.logDebug("5 - fieldValue: " + fieldValue);
				line = RegExp.rightContext;
//MochiKit.Logging.logDebug("6 - line: " + line.replace(/\n/g, "\\n").substring(0,50));

				if (fieldName == 'Notes') {
					var isMultiline;
					
					isMultiline = false;
					
//MochiKit.Logging.logDebug("7 - fieldLabelRegexp.test(line): " + fieldLabelRegexp.test(line) + " - recordLabelRegexp.test(line): " + recordLabelRegexp.test(line));
					if ((line != "") && (fieldLabelRegexp.test(line) == false) && (recordLabelRegexp.test(line) == false)) {
						fieldValue += '\n';
					}
					
					while ((line != "") && (fieldLabelRegexp.test(line) == false) && (recordLabelRegexp.test(line) == false)) {
						var newLineValue;
						
						newLineValue = line.match(fullLineRegexp)[1];
						if (newLineValue != "\n") {
							isMultiline = true;
						}
						fieldValue += newLineValue;
//MochiKit.Logging.logDebug("8 - fieldValue: " + fieldValue);
						line = RegExp.rightContext;
//MochiKit.Logging.logDebug("9 - line: " + line.replace(/\n/g, "\\n").substring(0,50));
//MochiKit.Logging.logDebug("10 - fieldLabelRegexp.test(line): " + fieldLabelRegexp.test(line) + " - recordLabelRegexp.test(line): " + recordLabelRegexp.test(line));
					}

					if (isMultiline) {
						fieldValue = fieldValue.replace(/\n$/g, "");
					} else {
						fieldValue = fieldValue.replace(/\n\n$/g, "");
					}
					
					line = line.replace(/^\n/, '');
				}
//MochiKit.Logging.logDebug("5 - fieldValue: " + fieldValue);
				
				result[fieldName] = fieldValue;
//MochiKit.Logging.logDebug("6 - line: " + line.replace(/\n/g, "\\n").substring(0,50));
//MochiKit.Logging.logDebug("---------------------------------------");
			}
		} else {
//MochiKit.Logging.logDebug("2.0");
			result = null;
		}
		
		aContext.line = line;
//MochiKit.Logging.logDebug("#######################################");
		
		return result;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});


