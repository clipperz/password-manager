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

Clipperz.PM.Components.Import.CSVImportComponent = function(anElement, args) {
	args = args || {};

	this._steps = this._steps || ['CSV_EDIT', 'CSV_COLUMNS', 'CSV_HEADER', 'CSV_TITLE', 'CSV_NOTES', 'CSV_FIELDS', 'PREVIEW', 'IMPORT'];

    Clipperz.PM.Components.Import.CSVImportComponent.superclass.constructor.call(this, anElement, args);

	this._step1Component = null;
	this._step2Component = null;
	this._step3Component = null;
	this._step4Component = null;
	this._step5Component = null;

	this._isFirstRowHeader = false;
	this._titleColumnIndex = -1;
	this._notesColumnIndex = -1;
	this._fieldSettings = {};
	this._skippedColumns = new Clipperz.Set();

	this.render();

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Import.CSVImportComponent, Clipperz.PM.Components.Import.GenericImportComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Import.CSVImportComponent component";
	},

	//-------------------------------------------------------------------------

	'render': function() {
		this.domHelper().append(this.element(), {tag:'div', cls:'csvImportWizard', children:[
			{tag:'h3', htmlString:Clipperz.PM.Strings['CSV_ImportWizard_Title']},
			{tag:'div', cls:'importSteps', id:this.getId('importSteps')},
			{tag:'div', cls:'importStepBlocks', children:[
				{tag:'div', cls:'step_0', id:this.getId('step_0'), children:[
					{tag:'div', children:[
						{tag:'div', cls:'importOptionsDescription', htmlString:Clipperz.PM.Strings['importOptions_csv_description']},
						{tag:'div', cls:'importOptionsParameters', children:[
							{tag:'div', cls:'CSVImportOptionsParameters', children:[
								{tag:'ul', children:[
									{tag:'li', children:[
										{tag:'label', 'for':this.getId('CSV_inputOptions_separator'), html:"separator"},
										{tag:'select', name:this.getId('CSV_inputOptions_separator'), id:this.getId('CSV_inputOptions_separator'), children:[
											{tag:'option', name:'comma', value:',', html:"comma (,)", selected:true},
											{tag:'option', name:'tab', value:'\t', html:"tab"}
										]}
									]},

									{tag:'li', children:[
										{tag:'label', 'for':this.getId('CSV_inputOptions_quote'), html:"quote"},
										{tag:'select', name:this.getId('CSV_inputOptions_quote'), id:this.getId('CSV_inputOptions_quote'), children:[
											{tag:'option', name:'doubleQuote', value:'\"', html:"double quote (\")", selected:true},
											{tag:'option', name:'singleQuote', value:'\'', html:"single quote (\')"}
										]}
									]},

									{tag:'li', children:[
										{tag:'label', 'for':this.getId('CSV_inputOptions_escape'), html:"escape"},
										{tag:'select', name:this.getId('CSV_inputOptions_escape'), id:this.getId('CSV_inputOptions_escape'), children:[
											{tag:'option', name:'doubleQuote', value:'\"', html:"double quote (\")", selected:true},
											{tag:'option', name:'slash', value:'\/', html:"slash (\/)"},
											{tag:'option', name:'backslash', value:'\\', html:"backslash (\\)"}
										]}
									]}
								]}
							]}
						]},
						this.textAreaConfig()
					]}
				]},
				{tag:'div', cls:'step_1', id:this.getId('step_1'), children:[]},
				{tag:'div', cls:'step_2', id:this.getId('step_2'), children:[]},
				{tag:'div', cls:'step_3', id:this.getId('step_3'), children:[]},
				{tag:'div', cls:'step_4', id:this.getId('step_4'), children:[]},
				{tag:'div', cls:'step_5', id:this.getId('step_5'), children:[]},
				{tag:'div', cls:'step_6', id:this.getId('step_6'), children:[
					{tag:'div', children:[
						{tag:'div', id:this.getId('previewDiv'), html:"preview"}
					]}
				]},
				{tag:'div', cls:'step_7', id:this.getId('step_7'), children:[
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
		
		this.setBackButton(new YAHOO.ext.Button(this.getDom('backActionButton'), {text:"back", handler:this.backAction, scope:this}));
		this.setNextButton(new YAHOO.ext.Button(this.getDom('nextActionButton'), {text:"next", handler:this.nextAction, scope:this}));

		this.updateSteps();
		
		this.getElement('step_0').setVisibilityMode(YAHOO.ext.Element.DISPLAY).show()
		this.getElement('step_1').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.getElement('step_2').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.getElement('step_3').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.getElement('step_4').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.getElement('step_5').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.getElement('step_6').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.getElement('step_7').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		
//		this.backButton().disable();
	},

	//-------------------------------------------------------------------------
	
	'nextAction': function() {
		switch (this.currentStep()) {
			case 0:	//	-> 1
				Clipperz.PM.Components.MessageBox.showProgressPanel(
					MochiKit.Base.method(this, 'deferredParseValues'),
					MochiKit.Base.method(this, 'handleParseError'),
					this.getDom('nextActionButton')
				);
				break;
			case 1:	//	-> 2
				this.getElement('step_1').hide();
				this.step2Component().render();
				this.setCurrentStep(2);
				this.getElement('step_2').show();
				break;
			case 2:	//	-> 3
				this.getElement('step_2').hide();
				this.step3Component().render();
				this.setCurrentStep(3);
				this.getElement('step_3').show();
				break;
			case 3:	//	-> 4
				this.getElement('step_3').hide();
				this.step4Component().render();
				this.setCurrentStep(4);
				this.getElement('step_4').show();
				break;
			case 4:	//	-> 5
				this.getElement('step_4').hide();
				this.step5Component().render();
				this.setCurrentStep(5);
				this.getElement('step_5').show();
				break;
			case 5:	//	-> 6
				this.previewValues();
				break;
			case 6:	//	-> 7
				this.importValues();
				break;
		}
	},

	//-------------------------------------------------------------------------
	
	'deferredParseValues': function() {
		var deferredResult;

		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("CSVImportComponent.deferredParseValues - 1 " + res.substring(0,50)); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'parseImportData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("CSVImportComponent.deferredParseValues - 2 " + res.substring(0,50)); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			this.startProcessing();
			
			return res;
		}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("CSVImportComponent.deferredParseValues - 3 " + res.substring(0,50)); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'parseCSVValues'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("CSVImportComponent.deferredParseValues - 4 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'setParsedValues'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("CSVImportComponent.deferredParseValues - 5 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this.step1Component(), 'render'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("CSVImportComponent.deferredParseValues - 6 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			this.processingDone();
			this.getElement('step_0').hide();
			this.getElement('step_1').show();
			this.backButton().enable();
			
			return res;
		}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("CSVImportComponent.deferredParseValues - 7 " + res); return res;});
		deferredResult.callback(this.textAreaContent());		

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'deferredPreviewValues': function() {
		var deferredResult;

//MochiKit.Logging.logDebug(">>> CSVImportComponent.deferredPreviewValues");
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 1 " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'previewImportData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 2 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			this.startProcessing();
			
			return res;
		}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 3 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'processCSVParsedValues'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 4 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'setProcessedValues'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 5 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'previewRecordValues'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 6 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			this.processingDone();
			this.getElement('step_5').hide();
			this.getElement('step_6').show();
			this.backButton().enable();
			
			return res;
		}, this));
		deferredResult.callback(this.parsedValues());		
//MochiKit.Logging.logDebug("<<< CSVImportComponent.deferredPreviewValues");

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'csvProcessor': function() {
		return new Clipperz.CSVProcessor({
			quoteChar:		this.getDom('CSV_inputOptions_quote').value,
			escapeChar:		this.getDom('CSV_inputOptions_escape').value,
			separatorChar:	this.getDom('CSV_inputOptions_separator').value,
			binary:true
		});
	},
	
	//-------------------------------------------------------------------------
	
	'parseCSVValues': function(someData) {
		var deferredResult;
		var csvProcessor;

		csvProcessor = this.csvProcessor();		
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.parseKeePassValues - 1 " + res.substring(0,50)); return res;});
		deferredResult.addCallback(function(res) {
			return Clipperz.NotificationCenter.deferredNotification(this, 'updatedProgressState', {steps:(res.length)}, res);
		})
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.parseKeePassValues - 2 " + res.substring(0,50)); return res;});
		deferredResult.addCallback(MochiKit.Base.method(csvProcessor, 'deferredParse'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.parseKeePassValues - 3 " + res); return res;});
		deferredResult.callback(someData);

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'processCSVParsedValues': function (someValues) {
		var deferredResult;
		var records;
		var titleColumnIndex;
		var notesColumnIndex;
		var i,c;
		
		deferredResult = new MochiKit.Async.Deferred();
		records = [];

		titleColumnIndex = this.titleColumnIndex();
		notesColumnIndex = this.notesColumnIndex();

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', {steps:(someValues.length)});
		
		c = someValues.length;
		if (this.isFirstRowHeader()) {
			i = 1;
		} else {
			i = 0;
		}
		
		for(  ; i<c; i++) {
			deferredResult.addCallback(MochiKit.Async.wait, 0.2);
			deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', {});
			deferredResult.addCallback(MochiKit.Base.bind(function(someRecords, someData) {
				var record;
				var recordVersion;
				var ii;

				record = new Clipperz.PM.DataModel.Record({user:this.user()});
				record.setLabel(someData[titleColumnIndex]);
				if (notesColumnIndex != -1) {
					record.setNotes(someData[notesColumnIndex]);
				}
				recordVersion = record.currentVersion()

				for (ii in someData) {
//					if ((ii != titleColumnIndex) && (ii != notesColumnIndex) && (typeof(this.fieldSettings()[ii]) != 'undefined')) {
					if ((ii != titleColumnIndex) && (ii != notesColumnIndex) && (this.isColumnSelected(ii))) {
						var recordField;

						recordField = new Clipperz.PM.DataModel.RecordField({
							recordVersion:	recordVersion,
							label:			this.labelForColumn(ii),
							value:			someData[ii],
							type:			this.typeForColumn(ii)
						});
						recordVersion.addField(recordField);
					}
				}
			
				someRecords.push(record);
			
				return someRecords;
			}, this), records, someValues[i]);
		}
		deferredResult.addCallback(MochiKit.Async.succeed, records);
		deferredResult.callback();
		
		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	'step1Component': function() {
		if (this._step1Component == null) {
			this._step1Component = new Clipperz.PM.Components.Import.CSVImport.CSVImportColumns(this.getElement('step_1'), {mainComponent:this});
		}
		
		return this._step1Component;
	},

	'step2Component': function() {
		if (this._step2Component == null) {
			this._step2Component = new Clipperz.PM.Components.Import.CSVImport.CSVImportHeader(this.getElement('step_2'), {mainComponent:this});
		}
		
		return this._step2Component;
	},

	'step3Component': function() {
		if (this._step3Component == null) {
			this._step3Component = new Clipperz.PM.Components.Import.CSVImport.CSVImportTitle(this.getElement('step_3'), {mainComponent:this});
		}
		
		return this._step3Component;
	},

	'step4Component': function() {
		if (this._step4Component == null) {
			this._step4Component = new Clipperz.PM.Components.Import.CSVImport.CSVImportNotes(this.getElement('step_4'), {mainComponent:this});
		}
		
		return this._step4Component;
	},

	'step5Component': function() {
		if (this._step5Component == null) {
			this._step5Component = new Clipperz.PM.Components.Import.CSVImport.CSVImportFields(this.getElement('step_5'), {mainComponent:this});
		}
		
		return this._step5Component;
	},

	//-------------------------------------------------------------------------

	'isFirstRowHeader': function() {
		return this._isFirstRowHeader;
	},
	
	'setIsFirstRowHeader': function(aValue) {
		this._isFirstRowHeader = aValue;
	},

	//-------------------------------------------------------------------------

	'titleColumnIndex': function() {
		return this._titleColumnIndex;
	},
	
	'setTitleColumnIndex': function(aValue) {
		this._titleColumnIndex = aValue;
	},
	
	//-------------------------------------------------------------------------

	'notesColumnIndex': function() {
		return this._notesColumnIndex;
	},
	
	'setNotesColumnIndex': function(aValue) {
		this._notesColumnIndex = aValue;
	},

	//-------------------------------------------------------------------------

	'fieldSettings': function() {
		return this._fieldSettings;
	},

	//-------------------------------------------------------------------------

	'skippedColumns': function() {
		return this._skippedColumns;
	},
	
	//-------------------------------------------------------------------------

	'isColumnSelected': function(aColumnIndex) {
		return !this.skippedColumns().contains("" + aColumnIndex);
	},

	//=========================================================================
	
	'labelForColumn': function(aColumnIndex) {
		var result;
		
		if ((typeof(this.fieldSettings()) != 'undefined') && (typeof(this.fieldSettings()[aColumnIndex]) != 'undefined')) {
			if (this.isFirstRowHeader()) {
				result = this.fieldSettings()[aColumnIndex]['_firstRowLabel'];
//MochiKit.Logging.logDebug("--- updateInputFieldValues - [" + aColumnIndex + "] _firstRowLabel: " + label);
			} else {
				result = this.fieldSettings()[aColumnIndex]['_emptyLabel'];
//MochiKit.Logging.logDebug("--- updateInputFieldValues - [" + aColumnIndex + "] _emptyLabel: " + label);
			}
		} else {
			result = "";
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'setLabelForColumn': function(aLabel, aColumnIndex) {
		var fieldSettings;

//MochiKit.Logging.logDebug(">>> setLabelForColumn[" + aColumnIndex + "]: " + aLabel);
		fieldSettings = this.fieldSettings();

		if (typeof(fieldSettings[aColumnIndex]) == 'undefined') {
			fieldSettings[aColumnIndex] = {}
		}
	
		if (this.isFirstRowHeader()) {
//MochiKit.Logging.logDebug("--- setLabelForColumn -> _firstRowLabel");
			fieldSettings[aColumnIndex]['_firstRowLabel'] = aLabel;
		} else {
			if (typeof(fieldSettings[aColumnIndex]['_emptyLabel']) == 'undefined') {
				if (aLabel == null) {
//MochiKit.Logging.logDebug("--- setLabelForColumn -> _emptyLabel = \"\"");
					fieldSettings[aColumnIndex]['_emptyLabel'] = "";
				} else {
					fieldSettings[aColumnIndex]['_emptyLabel'] = aLabel;
				}
			} else {
//MochiKit.Logging.logDebug("--- setLabelForColumn -> _emptyLabel = " + aLabel);
				if (aLabel != null) {
					fieldSettings[aColumnIndex]['_emptyLabel'] = aLabel;
				}
			}
		}
//MochiKit.Logging.logDebug("<<< setLabelForColumn[" + aColumnIndex + "]: " + aLabel);
	},
	
	//=========================================================================
	
	'typeForColumn': function(aColumnIndex) {
		var result;
		
		if ((typeof(this.fieldSettings()) != 'undefined') && (typeof(this.fieldSettings()[aColumnIndex]) != 'undefined') && (typeof(this.fieldSettings()[aColumnIndex]['type']) != 'undefined')) {
			result = this.fieldSettings()[aColumnIndex]['type'];
		} else {
			result = 'UNDEFINED';
		}
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'setTypeForColumn': function(aType, aColumnIndex) {
		var fieldSettings;

		fieldSettings = this.fieldSettings();

		if (typeof(fieldSettings[aColumnIndex]) == 'undefined') {
			fieldSettings[aColumnIndex] = {}
		}
	
		fieldSettings[aColumnIndex]['type'] = aType;
	},
	
	//=========================================================================
	__syntaxFix__: "syntax fix"
});

