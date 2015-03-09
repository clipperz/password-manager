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
if (typeof(Clipperz.PM.Components) == 'undefined') { Clipperz.PM.Components = {}; }
if (typeof(Clipperz.PM.Components.Import) == 'undefined') { Clipperz.PM.Components.Import = {}; }

//#############################################################################

Clipperz.PM.Components.Import.RoboFormImportComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Import.RoboFormImportComponent.superclass.constructor.call(this, anElement, args);

	this.render();

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Import.RoboFormImportComponent, Clipperz.PM.Components.Import.GenericImportComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Import.RoboFormImportComponent component";
	},

	//-------------------------------------------------------------------------

	'render': function() {
//MochiKit.Logging.logDebug(">>> Import.RoboFormImportComponent.render");
		this.domHelper().append(this.element(), {tag:'div', cls:'roboFormImportWizard', children:[
			{tag:'h3', htmlString:Clipperz.PM.Strings['RoboForm_ImportWizard_Title']},
			{tag:'div', cls:'importSteps', id:this.getId('importSteps')},
			{tag:'div', cls:'importStepBlocks', children:[
				{tag:'div', cls:'step_0', id:this.getId('step_0'), children:[
					{tag:'div', children:[
						{tag:'div', cls:'importOptionsDescription', htmlString:Clipperz.PM.Strings['importOptions_roboForm_description']},
						{tag:'div', cls:'importOptionsParameters', children:[]},
						this.textAreaConfig()
					]}
				]},
				{tag:'div', cls:'step_1', id:this.getId('step_1'), children:[
					{tag:'div', children:[
						{tag:'div', id:this.getId('previewDiv'), html:"preview"}
					]}
				]},
				{tag:'div', cls:'step_2', id:this.getId('step_2'), children:[
					{tag:'div', children:[
						{tag:'h4', html:"done"}
					]}
				]}
			]},
			{tag:'div', cls:'importOptionsButtons', children:[
				{tag:'table', children:[
					{tag:'tbody', children:[
						{tag:'tr', children:[
							{tag:'td', html:'&nbsp;'},
							{tag:'td', children:[
								{tag:'div', id:this.getId('backActionButton')}
							]},
							{tag:'td', html:'&nbsp;'},
							{tag:'td', children:[
								{tag:'div', id:this.getId('nextActionButton')}
							]},
							{tag:'td', html:'&nbsp;'}
						]}
					]}
				]}
			]}
		]});
		
		this.updateSteps();
		
		this.setBackButton(new YAHOO.ext.Button(this.getDom('backActionButton'), {text:"back", handler:this.backAction, scope:this}));
		this.setNextButton(new YAHOO.ext.Button(this.getDom('nextActionButton'), {text:"next", handler:this.nextAction, scope:this}));
		
		this.getElement('step_0').setVisibilityMode(YAHOO.ext.Element.DISPLAY).show()
		this.getElement('step_1').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.getElement('step_2').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
//MochiKit.Logging.logDebug("<<< Import.RoboFormImportComponent.render");
	},

	//-------------------------------------------------------------------------
	
	'nextAction': function() {
		switch (this.currentStep()) {
			case 0:	//	-> 1
				this.previewValues();
				break;
			case 1:	//	-> 2
				this.importValues();
				break;
		}
	},
	
	//-------------------------------------------------------------------------

	'deferredPreviewValues': function() {
		var deferredResult;

//		this.setFormValues(MochiKit.DOM.formContents(this.getDom('dataForm')));

		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			this.startProcessing();
			
			return res;
		}, this));
		deferredResult.addCallback(MochiKit.Base.method(this, 'processRoboFormValues'));
		deferredResult.addCallback(MochiKit.Base.method(this, 'setProcessedValues'));
		deferredResult.addCallback(MochiKit.Base.method(this, 'previewRecordValues'));
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			this.processingDone();
			this.getElement('step_0').hide();
			this.getElement('step_1').show();
			this.backButton().enable();
			
			return res;
		}, this));
//		deferredResult.addErrback(MochiKit.Base.bind(function() {
//			this.processingAborted();
//		}, this))
		deferredResult.callback(this.textAreaContent());		

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'processRoboFormValues': function(someData) {
		var result;
		
		if (someData.match(/^\<HTML\>\<HEAD\>\<TITLE\>RoboForm Passcards List /g)) {
			result = this.processRoboFormPasscardsValues(someData);
		} else if (someData.match(/\<HTML\>\<HEAD\>\<TITLE\>RoboForm Safenotes List /g)) {
			result = this.processRoboFormSafenotesValues(someData);
		}

		return result;
	},

	//.........................................................................

	'processRoboFormPasscardsValues': function(someData) {
		var deferredResult;
		
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues - 1: "/* + res*/); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'parseImportData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues - 2: "/* + res*/); return res;});
		deferredResult.addCallback(function(someData) {
			var result;
			var data;
			
			data = someData.replace(/\r?\n/g, "");
			result = data.match(/\<TABLE width\=\"100\%\"\>.*?\<\/TABLE\>/g);

			return result;
		});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues - 3: "/* + res*/); return res;});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues - 3.1: " + res.length); return res;});
		deferredResult.addCallback(function(res) {
			return Clipperz.NotificationCenter.deferredNotification(this, 'updatedProgressState', {steps:(res.length)}, res);
		})
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues - 4: "/* + res*/); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(someRecordValues) {
			var innerDeferredResult;
			var records;
			var i,c;

			innerDeferredResult = new MochiKit.Async.Deferred();
			records = [];
			
			c = someRecordValues.length;
			for(i=0; i<c; i++) {
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues __inner loop__ - 1: " + res); return res;});
				innerDeferredResult.addCallback(MochiKit.Async.wait, 0.2);
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues __inner loop__ - 2: " + res); return res;});
				innerDeferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', {});
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues __inner loop__ - 3: " + res); return res;});
				innerDeferredResult.addCallback(MochiKit.Base.bind(function(someRecords, someData) {
					var data;
					var record;
					var recordVersion;
					var fields;
					var ii, cc;
					var hasNotes;

					var caption;
					var subcaption;

//MochiKit.Logging.logDebug("data: " + someData);
					data = someData.replace(/\<WBR\>/g, "");
					hasNotes = false;
					
					/\<TD class\=caption colSpan\=3\>(.*?)\<\/TD\>/.test(data);		//	<TD class=caption colSpan=3>110mb</TD>
					caption = RegExp.$1;
//MochiKit.Logging.logDebug("caption: " + caption);
					
					/\<TD class\=subcaption colSpan\=3\>(.*?)\<\/TD\>/.test(data);		//	<TD class=subcaption colSpan=3>110<WBR>mb.com</TD>
					subcaption = RegExp.$1;
//MochiKit.Logging.logDebug("subcaption: " + subcaption);
					
					record = new Clipperz.PM.DataModel.Record({user:this.user()});
					recordVersion = record.currentVersion()

					record.setLabel(caption);
//					record.setNotes(subcaption);
					if (subcaption != null) {
						var recordField;
						
						recordField = new Clipperz.PM.DataModel.RecordField({
							recordVersion:	recordVersion,
							label:			"url",
							value:			subcaption,
							type:			'URL'
						});
						recordVersion.addField(recordField);
					}

					fields = data.match(/\<TR\>.*?\<\/TR\>/g) || [];
					cc = fields.length;
//MochiKit.Logging.logDebug("fields.length: " + cc);
					for (ii=0; ii<cc; ii++) {
						var recordField;
						var fieldString;
						var fieldName;
						var fieldValue;

//MochiKit.Logging.logDebug("fieldString: " + fields[ii]);
						fieldString = fields[ii];
//MochiKit.Logging.logDebug("fieldString (cleaned): " + fieldString);
						/\<TD class\=field vAlign\=top align\=left width\=\"40\%\"\>(.*?)\<\/TD\>/.test(fieldString);
						fieldName = RegExp.$1;
						
						/\<TD class\=wordbreakfield vAlign\=top align\=left width\=\"55\%\"\>(.*?)\<\/TD\>/.test(fieldString);
						fieldValue = RegExp.$1;

						if (fieldName == "Note$") {
							record.setNotes(fieldValue);
							hasNotes = true;
						} else {
							var fieldType;

							if (((ii == 1) && (hasNotes == false)) || ((ii == 2) && (hasNotes == true))) {
								fieldType = 'PWD';
							} else {
								fieldType = 'TXT';
							}
							
							recordField = new Clipperz.PM.DataModel.RecordField({
								recordVersion:	recordVersion,
								label:			fieldName,
								value:			fieldValue,
								type:			fieldType
							});
							recordVersion.addField(recordField);
						}
					}
					
					someRecords.push(record);
				
					return someRecords;
				}, this), records, someRecordValues[i]);
			}
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues __inner loop__ - 4: " + res); return res;});
			innerDeferredResult.addCallback(MochiKit.Async.succeed, records);
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues __inner loop__ - 5: " + res); return res;});
			innerDeferredResult.callback();
			
			return innerDeferredResult;
		}, this));
		deferredResult.callback(someData);

		return deferredResult;
	},


	//.........................................................................

	'processRoboFormSafenotesValues': function(someData) {
		var deferredResult;
		
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues - 1: "/* + res*/); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'parseImportData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues - 2: "/* + res*/); return res;});
		deferredResult.addCallback(function(someData) {
			var result;
			var data;
			
			data = someData.replace(/\r?\n/g, "");
			result = data.match(/\<TABLE width\=\"100\%\"\>.*?\<\/TABLE\>/g);

			return result;
		});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues - 3: "/* + res*/); return res;});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues - 3.1: " + res.length); return res;});
		deferredResult.addCallback(function(res) {
			return Clipperz.NotificationCenter.deferredNotification(this, 'updatedProgressState', {steps:(res.length)}, res);
		})
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues - 4: "/* + res*/); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(someRecordValues) {
			var innerDeferredResult;
			var records;
			var i,c;

			innerDeferredResult = new MochiKit.Async.Deferred();
			records = [];
			
			c = someRecordValues.length;
			for(i=0; i<c; i++) {
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues __inner loop__ - 1: " + res); return res;});
				innerDeferredResult.addCallback(MochiKit.Async.wait, 0.2);
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues __inner loop__ - 2: " + res); return res;});
				innerDeferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', {});
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues __inner loop__ - 3: " + res); return res;});
				innerDeferredResult.addCallback(MochiKit.Base.bind(function(someRecords, someData) {
					var data;
					var record;
					var recordVersion;

					var caption;
					var wordbreakfield;

//MochiKit.Logging.logDebug("data: " + someData);
					data = someData.replace(/\<WBR\>/g, "");
					hasNotes = false;
					
					/\<TD class\=caption colSpan\=3\>(.*?)\<\/TD\>/.test(data);		//	<TD class=caption colSpan=3>110mb</TD>
					caption = RegExp.$1;
//MochiKit.Logging.logDebug("caption: " + caption);
					
					/\<TD class\=wordbreakfield vAlign=top align\=left width\=\"\1\0\0\%\"\>(.*?)\<\/TD\>/.test(data);		//	<TD class=wordbreakfield vAlign=top align=left width="100%">7759500</TD>
					wordbreakfield = RegExp.$1;
//MochiKit.Logging.logDebug("subcaption: " + subcaption);
					
					record = new Clipperz.PM.DataModel.Record({user:this.user()});
					recordVersion = record.currentVersion()

					record.setLabel(caption);
					record.setNotes(wordbreakfield);
					
					someRecords.push(record);
				
					return someRecords;
				}, this), records, someRecordValues[i]);
			}
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues __inner loop__ - 4: " + res); return res;});
			innerDeferredResult.addCallback(MochiKit.Async.succeed, records);
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("RoboFormImportComponent.processRoboFormValues __inner loop__ - 5: " + res); return res;});
			innerDeferredResult.callback();
			
			return innerDeferredResult;
		}, this));
		deferredResult.callback(someData);

		return deferredResult;
	},


	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

