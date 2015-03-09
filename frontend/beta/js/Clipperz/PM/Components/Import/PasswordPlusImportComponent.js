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

Clipperz.PM.Components.Import.PasswordPlusImportComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Import.PasswordPlusImportComponent.superclass.constructor.call(this, anElement, args);

	this.render();

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Import.PasswordPlusImportComponent, Clipperz.PM.Components.Import.GenericImportComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Import.PasswordPlusImportComponent component";
	},

	//-------------------------------------------------------------------------

	'render': function() {
//MochiKit.Logging.logDebug(">>> Import.PasswordPlusImportComponent.render");
		this.domHelper().append(this.element(), {tag:'div', cls:'passwordPlusImportWizard', children:[
			{tag:'h3', htmlString:Clipperz.PM.Strings['PasswordPlus_ImportWizard_Title']},
			{tag:'div', cls:'importSteps', id:this.getId('importSteps')},
			{tag:'div', cls:'importStepBlocks', children:[
				{tag:'div', cls:'step_0', id:this.getId('step_0'), children:[
					{tag:'div', children:[
						{tag:'div', cls:'importOptionsDescription', htmlString:Clipperz.PM.Strings['importOptions_passwordPlus_description']},
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
//MochiKit.Logging.logDebug("<<< Import.PasswordPlusImportComponent.render");
	},

	//-------------------------------------------------------------------------
/*
	'backAction': function() {
		switch (this.currentStep()) {
			case 1:	//	-> 0
				this.backButton().disable();
				this.getElement('step_1').hide();
				this.setCurrentStep(0);
				this.getElement('step_0').show();
				break;
		}
	},
*/
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
		deferredResult.addCallback(MochiKit.Base.method(this, 'processPasswordPlusValues'));
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

	'processPasswordPlusValues': function(someData) {
		var deferredResult;
		var csvProcessor;

		csvProcessor = new Clipperz.CSVProcessor({binary:true});
		
		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'parseImportData');
		deferredResult.addCallback(function(res) {
			return Clipperz.NotificationCenter.deferredNotification(this, 'updatedProgressState', {steps:(res.length * 2)}, res);
		})
		deferredResult.addCallback(MochiKit.Base.method(csvProcessor, 'deferredParse'));
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'previewImportData');
		deferredResult.addCallback(function(res) {
			return Clipperz.NotificationCenter.deferredNotification(this, 'updatedProgressState', {steps:(res.length * 2), step:res.length}, res);
		})
		deferredResult.addCallback(MochiKit.Base.bind(function(someCSVValues) {
			var innerDeferredResult;
			var records;
			var i,c;
			
			innerDeferredResult = new MochiKit.Async.Deferred();
			records = [];
			
			c = someCSVValues.length;
			i=0;
			i++;	//	Dataviz Passwords Plus Export, Version,1, Minimum Version To Read,1
			i++;	//	Is Template,Title,Category,Field 1 Label,Field 1 Value,Field 1 Hidden,Field 2 Label,Field 2 Value,Field 2 Hidden,Field 3 Label,Field 3 Value,Field 3 Hidden,Field 4 Label,Field 4 Value,Field 4 Hidden,Field 5 Label,Field 5 Value,Field 5 Hidden,Field 6 Label,Field 6 Value,Field 6 Hidden,Field 7 Label,Field 7 Value,Field 7 Hidden,Field 8 Label,Field 8 Value,Field 8 Hidden,Field 9 Label,Field 9 Value,Field 9 Hidden,Field 10 Label,Field 10 Value,Field 10 Hidden,Note

			for(    ; i<c; i++) {
				innerDeferredResult.addCallback(MochiKit.Async.wait, 0.2);
				innerDeferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', {});
				innerDeferredResult.addCallback(MochiKit.Base.bind(function(someRecords, someData) {
					if (someData[0] == '0') {
						var record;
						var recordVersion;
						var ii, cc;
					
						record = new Clipperz.PM.DataModel.Record({user:this.user()});
						if (someData[1] != "") {
							record.setLabel(someData[1]);
						} else {
							record.setLabel("imported record [" + (i+1) + "]");
						}
						record.setNotes(someData[33]);
						recordVersion = record.currentVersion()

						cc = 10;
						for (ii=0; ii<cc; ii++) {
							var currentFieldValueIndex;
							var currentType;
				
							currentFieldValueIndex = (ii * 3) + 4;
						
							if (someData[currentFieldValueIndex] != "") {
								var recordField;
								var recordFieldType;
								
								recordFieldType = 'TXT';
								if (someData[currentFieldValueIndex + 1] == 1) {
									recordFieldType = 'PWD';
								} else if (/^http/.test(someData[currentFieldValueIndex])) {
									recordFieldType = 'URL';
								}
							
								recordField = new Clipperz.PM.DataModel.RecordField({
									recordVersion:	recordVersion,
									label:			someData[currentFieldValueIndex - 1],
									value:			someData[currentFieldValueIndex],
									type:			recordFieldType
								});
								recordVersion.addField(recordField);
							}
						}

	//					this.user().addRecord(record, true);
					
						someRecords.push(record);
					}
				
					return someRecords;
				}, this), records, someCSVValues[i]);
			}
			innerDeferredResult.addCallback(MochiKit.Async.succeed, records);
			innerDeferredResult.callback();
			
			return innerDeferredResult;
		}, this));
		deferredResult.callback(someData);

		return deferredResult;

/*
0	Is Template
1	Title
2	Category

3	Field 1 Label
4	Field 1 Value
5	Field 1 Hidden

6	Field 2 Label
7	Field 2 Value
8	Field 2 Hidden

9	Field 3 Label
10	Field 3 Value
11	Field 3 Hidden

12	Field 4 Label
13	Field 4 Value
14	Field 4 Hidden

15	Field 5 Label
16	Field 5 Value
17	Field 5 Hidden

18	Field 6 Label
19	Field 6 Value
20	Field 6 Hidden

21	Field 7 Label
22	Field 7 Value
23	Field 7 Hidden

24	Field 8 Label
25	Field 8 Value
26	Field 8 Hidden

27	Field 9 Label
28	Field 9 Value
29	Field 9 Hidden

30	Field 10 Label
31	Field 10 Value
32	Field 10 Hidden

33	Note
*/
	},


	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

