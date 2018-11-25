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

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }


Clipperz.KeePassExportProcessor = function(args) {
	args = args || {};

	return this;
}

//=============================================================================

Clipperz.KeePassExportProcessor.prototype = MochiKit.Base.update(null, {

	//-------------------------------------------------------------------------

	'deferredParse_core': function(aContext) {
		var deferredResult;

		if (aContext.line == "") {
			deferredResult = MochiKit.Async.succeed(aContext.result);
		} else {
			var record;
			
			record = this.parseRecord(aContext);
			if (record != null) {
				aContext.result.push(record);
			}

			aContext.line = aContext.line.replace(/^\n*/g, "").replace(/\n$/g, "");
			
			deferredResult = new Clipperz.Async.Deferred("KeePassExportProcessor.deferredParse_core");
			deferredResult.addCallbackPass(MochiKit.Signal.signal, this, 'importProcessorProgressUpdate', {status:'processing', size:aContext.size, progress:(aContext.size - aContext.line.length)});
			deferredResult.addCallback(MochiKit.Async.wait, 0.2);
			deferredResult.addMethod(this, 'deferredParse_core');
			deferredResult.callback(aContext);
		}

		return deferredResult;
	},
	
	//.........................................................................
	
	'deferredParse': function(aValue) {
		var deferredResult;
		var lines;
		var context;

		lines = aValue.replace(/\r?\n/g, "\n");
		context = {
			line: lines,
			size: lines.length,
			result: []
		}

		deferredResult = new Clipperz.Async.Deferred("KeePassExportProcessor.deferredResult");
		deferredResult.addMethod(this, 'deferredParse_core');
		deferredResult.callback(context);

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
//		recordLabelRegexp = new RegExp("(^\\[(.*)\\]\\n|^Title:\s*(.*)\\n)");
		recordLabelRegexp = new RegExp("^\\[(.*)\\]\\n|^Title:\s*(.*)\\n");
		fieldLabelRegexp = new RegExp("^\s?(Group|Group Tree|Username|UserName|User Name|Url|URL|Password|Notes|Comment|UUID|Icon|Creation Time|Last Access|Last Modification|Expires|Attachment Description|Attachment|Valid until): ");
		fieldValueRegexp = new RegExp("(.*)(\\n|$)");
		fullLineRegexp = new RegExp("^(.*\\n)");

		if (recordLabelRegexp.test(aContext.line) == true) {
			var line;
			
			line = aContext.line;
			
			result = {};
			result['Title'] = line.match(recordLabelRegexp)[1];
			line = line.replace(/^.*\n/, "");
			while (fieldLabelRegexp.test(line) == true) {
				var fieldName;
				var fieldValue;
				
				fieldName = RegExp.$1;
				line = RegExp.rightContext;
				
				fieldValue = line.match(fieldValueRegexp)[1];
				line = RegExp.rightContext;

				if (fieldName == 'Notes') {
					var isMultiline;
					
					isMultiline = false;
					
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
						line = RegExp.rightContext;
					}

					if (isMultiline) {
						fieldValue = fieldValue.replace(/\n$/g, "");
					} else {
						fieldValue = fieldValue.replace(/\n\n$/g, "");
					}
					
					line = line.replace(/^\n/, '');
				}
				
				result[fieldName] = fieldValue;
			}
		} else {
			result = null;
		}
		
		aContext.line = line;
		
		return result;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});


