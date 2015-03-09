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

Clipperz.PM.Components.Import.GenericImportComponent = function(anElement, args) {
	args = args || {};

	this._steps = this._steps || ['EDIT', 'PREVIEW', 'IMPORT'];

    Clipperz.PM.Components.Import.GenericImportComponent.superclass.constructor.call(this, anElement, args);

	this._user = args['user'];
	
	this._currentStep = 0;
	this._currentStatus = 'IDLE'; //	'PROCESSING'

	this._parsedValues = null;
	this._processedValues = null;

	this._backButton = null;
	this._nextButton = null;

	Clipperz.NotificationCenter.register(null, 'importProcessorProgressUpdate', this, 'updateProgressDialogStatus');
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Import.GenericImportComponent, Clipperz.PM.Components.BaseComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Import.GenericImportComponent component";
	},

	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},
	
	//-------------------------------------------------------------------------

	'textAreaConfig': function() {
		return {tag:'textarea', name:this.getId('importTextArea'), cls:'importTextArea', id:this.getId('importTextArea'), cols:60, rows:15, html:""};
	},

	'textAreaContent': function() {
		return this.getDom('importTextArea').value
	},
	
	//-------------------------------------------------------------------------

	'steps': function() {
		return this._steps;
	},
	
	'currentStep': function() {
		return this._currentStep;
	},
	
	'setCurrentStep': function(aValue) {
		this._currentStep = aValue;
		this.updateSteps();
	},

	//-------------------------------------------------------------------------

	'currentStatus': function() {
		return this._currentStatus;
	},

	'startProcessing': function() {
		this._currentStatus = 'PROCESSING';
		this.updateSteps();
	},
	
	'processingDone': function() {
		this._currentStatus = 'IDLE';
		this.setCurrentStep(this.currentStep() + 1);
	},
	
	'processingAborted': function() {
		this._currentStatus = 'IDLE';
		this.updateSteps();
	},
	
	//-------------------------------------------------------------------------
	
	'stepsConfig': function() {
		var result;
		var i,c;
		
		result = [];
		c = this.steps().length;
		for (i=0; i<c; i++) {
			var cls;
			
			if (this.currentStep() == i) {
				if (this.currentStatus() == 'IDLE') {
					cls = 'current';
				} else {
					cls = 'currentProcessing';
				}
			} else {
				cls = "";
			}
			
			result.push({tag:'td', cls:cls, children:[
				{tag:'div', children:[{tag:'span', htmlString:Clipperz.PM.Strings['ImportWizard'][this.steps()[i]]}]}
			]})
			if (i < (c-1)) {
				if ((this.currentStep() == i) && (this.currentStatus() == 'PROCESSING')) {
					cls = 'stepSeparatorProcessing';
				} else {
					cls = 'stepSeparator';
				}
				
				result.push({tag:'td', cls:cls, children:[
					{tag:'div', children:[{tag:'span', html:">"}]}
				]});
			}
		}
		
		result = [{tag:'div', cls:'importWizardStepsBox', children:[
			{tag:'div', cls:'importWizardStepsInnerBox', children:[
				{tag:'table', cls:'importWizardSteps', children:[
					{tag:'tbody', children:[
						{tag:'tr', children:result}
					]}
				]}
			]},
			{tag:'div', cls:'importWizardStepsBoxFooter'}
		]}];
		
		return result;
	},
	
	'updateSteps': function() {
		this.getElement('importSteps').update("");
		Clipperz.YUI.DomHelper.append(this.getDom('importSteps'), {tag:'div', children:this.stepsConfig()});
	},

	//-------------------------------------------------------------------------

	'backAction': function() {
//MochiKit.Logging.logDebug(">>> backAction");
		if (this.currentStep() == 0) {
			Clipperz.NotificationCenter.notify(this, 'importCancelled');
		} else {
			this.getElement('step_' + this.currentStep()).hide();
			this.setCurrentStep(this.currentStep() - 1);
			this.getElement('step_' + this.currentStep()).show();
		
			this.nextButton().enable();
		}
//MochiKit.Logging.logDebug("<<< backAction");
	},
	
	//-------------------------------------------------------------------------

	'backButton': function() {
		return this._backButton;
	},

	'setBackButton': function(aValue) {
		this._backButton = aValue;
	},
	
	'nextButton': function() {
		return this._nextButton;
	},
	
	'setNextButton': function(aValue) {
		this._nextButton = aValue;
	},
	
	//-------------------------------------------------------------------------

	'render': function() {
//MochiKit.Logging.logDebug(">>> Import.GenericImportComponent.render");
		this.domHelper().append(this.element(), {tag:'div', children:[
			{tag:'h2', html:this.toString()}
		]});
//MochiKit.Logging.logDebug("<<< Import.GenericImportComponent.render");
	},

	//-------------------------------------------------------------------------

	'previewValues': function() {
		Clipperz.PM.Components.MessageBox.showProgressPanel(
			MochiKit.Base.method(this, 'deferredPreviewValues'),
			MochiKit.Base.method(this, 'handlePreviewError'),
			this.getDom('nextActionButton')
		);
	},

	'deferredPreviewValues': function() {
		throw Clipperz.Base.exception.AbstractMethod;
	},
	
	'handlePreviewError': function(anError) {
console.log("anError", anError);
		MochiKit.Logging.logError("An error occurred while previewing the data: " + anError);
		alert("An error occurred while previewing the data");
		Clipperz.PM.Components.MessageBox().hide();
	},

	//-------------------------------------------------------------------------

	'previewRecordValues': function(someProcessedRecords) {
//MochiKit.Logging.logDebug(">>> previewRecordValues");
		this.getElement('previewDiv').update("");
//MochiKit.Logging.logDebug("--- previewRecordValues - 1");
		this.domHelper().append(this.getElement('previewDiv'), {tag:'div', cls:'importPreviewDiv', children:[{tag:'table', id:'importPreview', cellspacing:'0', children:[
			{tag:'tbody', children:
				MochiKit.Base.map(MochiKit.Base.bind(function(aRecord) {
					var result;
//MochiKit.Logging.logDebug("--- previewRecordValues - 1.1");
//console.log("fields", aRecord.currentVersion().fields());
					result = {tag:'tr', children:[{tag:'td', children:[
						{tag:'table', cls:'importPreview_record', children:[
							{tag:'tbody', children:[
								{tag:'tr', children:[
									{tag:'td', rowspan:'2', valign:'top', children:[
										{tag:'input', type:'checkbox', id:this.getId(aRecord.reference()), value:"aRecord.reference()", checked:true}
									]},
									{tag:'td', colspan:'2', children:[
										{tag:'span', cls:'importPreview_title', html:aRecord.label()}
									]}
								]},
								{tag:'tr', children:[
									{tag:'td', valign:'top', children:[
										{tag:'span', cls:'importPreview_notes', html:(MochiKit.Base.isNotEmpty(aRecord.notes()) ? aRecord.notes().replace(/\n/g, '<br>') : '&nbsp;')}
									]},
									{tag:'td', valign:'top', cls:'importPreview_fieds', children:[
										{tag:'table', cls:'importPreview_fields', children:[
											{tag:'tbody', children:MochiKit.Base.map(function(aField) {
												var result;
//MochiKit.Logging.logDebug("--- previewRecordValues - 1.1.1");
												result = {tag:'tr', children:[
													{tag:'td', valign:'top', children:[
														{tag:'span', cls:'importPreview_fields_label', html:aField.label()}
													]},
													{tag:'td', valign:'top', children:[
														{tag:'span', cls:'importPreview_fields_value', html:aField.value()}
													]}
												]};
//MochiKit.Logging.logDebug("--- previewRecordValues - 1.1.2");
												return result;
											}, MochiKit.Base.values(aRecord.currentVersion().fields()))}
										]}
									]}
								]}
							]}
						]}
					]}]};
//MochiKit.Logging.logDebug("--- previewRecordValues - 1.2");
					return result;
				}, this), someProcessedRecords)
			}
		]}]});
//MochiKit.Logging.logDebug("--- previewRecordValues - 2");

		MochiKit.Base.map(MochiKit.Base.bind(function(aRecord) {
			this.getElement(aRecord.reference()).dom.value = aRecord.reference();
		}, this), someProcessedRecords);
		
		Clipperz.Style.applyZebraStylesToTable('importPreview');
//MochiKit.Logging.logDebug("<<< previewRecordValues");
	},

	//-------------------------------------------------------------------------

	'updateProgressDialogStatus': function(anEvent) {
		Clipperz.PM.Components.MessageBox().update({step:anEvent.parameters().progress});
	},
	
	//-------------------------------------------------------------------------

	'parsedValues': function() {
		return this._parsedValues;
	},
	
	'setParsedValues': function(aValue) {
		this._parsedValues = aValue;

		return this._parsedValues;
	},

	//-------------------------------------------------------------------------

	'processedValues': function() {
		return this._processedValues;
	},

	'setProcessedValues': function(aValue) {
		this._processedValues = aValue;
		return this._processedValues;
	},
	
	//-------------------------------------------------------------------------

	'importValues': function() {
		var deferredResult;
		
		deferredResult = new MochiKit.Async.Deferred();
		
		deferredResult.addCallback(MochiKit.Base.bind(function() {
			this.nextButton().disable();
			this.startProcessing();
		},this));
		deferredResult.addCallback(MochiKit.Base.method(this, 'importProcessedValues'));
		deferredResult.addCallback(MochiKit.Base.method(this, 'processingDone'));
		deferredResult.addErrback (MochiKit.Base.method(this, 'processingAborted'));
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------
	
	'importProcessedValues': function() {
		var deferredResult;
		var processedValues;
		var selectedRecords;
		var i,c;

//MochiKit.Logging.logDebug(">>> GenericImportComponent.importProcessedValues");
		processedValues = this.processedValues();
		selectedRecords = [];
		
		c = processedValues.length;
		for (i=0; i<c; i++) {
			var currentRecord;
			
			currentRecord = processedValues[i];
			if (this.getDom(currentRecord.reference()).checked == true) {
				selectedRecords.push(currentRecord);
			}
		}

		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("GenericImportComponent - 1: " + res); return res;});
		deferredResult.addCallback(function(someRecords) {
			var innerDeferredResult;
			var text;
			
			text = Clipperz.PM.Strings['importData_importConfirmation_text'];
			text = text.replace(/__numberOfRecords__/, someRecords.length);
			
			innerDeferredResult = new MochiKit.Async.Deferred();
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("GenericImportComponent - 1.1: " + res); return res;});
			innerDeferredResult.addCallback(MochiKit.Async.succeed, someRecords);
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("GenericImportComponent - 1.2: " + res); return res;});

			Clipperz.PM.Components.MessageBox().deferredShow({
				title:Clipperz.PM.Strings['importData_importConfirmation_title'],
				text:text,
				width:240,
				showProgressBar:false,
				showCloseButton:false,
				buttons:{
					'yes':"yes",			//	Clipperz.PM.Strings['mainPanelDeleteRecordPanelConfirmButtonLabel'],
					'no':"no"				//	Clipperz.PM.Strings['mainPanelDeleteRecordPanelDenyButtonLabel']
				},
				fn:MochiKit.Base.partial(function(aDeferred, aResult) {
					if (aResult == 'yes') {
						aDeferred.callback(aResult);
					} else {
						aDeferred.errback(aResult);
					}
				}, innerDeferredResult)
			}/*, this.getId('nextActionButton')*/);
			
			return innerDeferredResult;
		});

//-------------------
//		deferredResult.addCallback(MochiKit.Base.bind(function(someRecords) {
//			Clipperz.PM.Components.MessageBox.showProgressPanel(
//				MochiKit.Base.method(this, 'importProcessedValues_core', someRecords),
//				MochiKit.Base.method(this, 'handleProcessError'),
//				this.getDom('mainDiv')
//			);
//		}, this));

//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("GenericImportComponent - 2: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Components.MessageBox(), 'deferredShow'),
			{
//				title:Clipperz.PM.Strings['accountPanelDeletingAccountPanelProgressTitle'],
//				text:Clipperz.PM.Strings['accountPanelDeletingAccountPanelProgressText'],
				width:240,
				showProgressBar:true,
				showCloseButton:false
			}
		);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("GenericImportComponent - 3: " + res); return res;});
		
		deferredResult.addCallback(MochiKit.Base.bind(function(someRecords) {
			var innerDeferredResult;

//MochiKit.Logging.logDebug(">>> inner deferred");
			innerDeferredResult = new MochiKit.Async.Deferred();
			
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("GenericImportComponent - 3.1: " + res); return res;});
			innerDeferredResult.addCallback(MochiKit.Base.method(this, 'importProcessedValues_core', someRecords));
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("GenericImportComponent - 3.2: " + res); return res;});
			innerDeferredResult.addErrback(MochiKit.Base.method(this, 'handleProcessError'));
//innerDeferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("GenericImportComponent - 3.3: " + res); return res;});
			innerDeferredResult.callback(someRecords);
//MochiKit.Logging.logDebug("<<< inner deferred");
			
			return innerDeferredResult;
		}, this), selectedRecords);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("GenericImportComponent - 4: " + res); return res;});

		deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Components.MessageBox(), 'hide'), 'mainDiv');
		
		deferredResult.addErrback(MochiKit.Base.bind(function() {
			this.nextButton().enable();
			this.setCurrentStep(this.currentStep() -1);
		}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("GenericImportComponent - 5: " + res); return res;});

		deferredResult.callback(selectedRecords);
//MochiKit.Logging.logDebug("<<< GenericImportComponent.importProcessedValues");

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'importProcessedValues_core': function(someRecords) {
		var deferredResult;
		
		deferredResult = new MochiKit.Async.Deferred();

		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'processingImportData');
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', {steps:(someRecords.length + 6 + 1)});
		deferredResult.addCallback(MochiKit.Async.wait, 0.5);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("importProcessedValues_core - 3: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(someRecords) {
			var i,c;
			
			c = someRecords.length;
			for (i=0; i<c; i++) {
				this.user().addRecord(someRecords[i], true);
			}
			
			return someRecords;
		}, this));
		deferredResult.addCallback(MochiKit.Async.wait, 0.5);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("importProcessedValues_core - 4: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'recordAdded', null);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("importProcessedValues_core - 5: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this.user(), 'saveRecords'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("importProcessedValues_core - 6: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'selectTab', 'mainTabPanel.recordsTab');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("importProcessedValues_core - 7: " + res); return res;});
		
		if (this.user().preferences().shouldShowDonationPanel()) {
			deferredResult.addCallback(Clipperz.PM.showDonationSplashScreen, this.user(), 'mainDiv');
		}
		
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'importCompleted', null);

		deferredResult.callback(someRecords);

		return deferredResult;
	},

	//-------------------------------------------------------------------------
	
	'handleParseError': function(res) {
		this.processingAborted();
		MochiKit.Logging.logError("An error occurred while parsing the values: " + res);
		alert("An error occurred while parsing the values: " + res);
		Clipperz.PM.Components.MessageBox().hide();
	},

	'handleProcessError': function(res) {
		this.processingAborted();
		MochiKit.Logging.logError("An error occurred while processing the values: " + res);
		alert("An error occurred while processing the values: " + res);
		Clipperz.PM.Components.MessageBox().hide();
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

