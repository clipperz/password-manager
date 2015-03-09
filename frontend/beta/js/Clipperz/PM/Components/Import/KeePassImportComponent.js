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

Clipperz.PM.Components.Import.KeePassImportComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Import.KeePassImportComponent.superclass.constructor.call(this, anElement, args);

	this._steps = ['EDIT', 'KEEPASS_SETTINGS', 'PREVIEW', 'IMPORT'];
	this._definedFields = ['Group', 'Group Tree', 'UserName', 'URL', 'Password', 'Notes', 'UUID', 'Icon', 'Creation Time', 'Last Access', 'Last Modification', 'Expires', 'Attachment Description', 'Attachment'];

	this.render();

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Import.KeePassImportComponent, Clipperz.PM.Components.Import.GenericImportComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Import.KeePassImportComponent component";
	},

	//-------------------------------------------------------------------------

	'render': function() {
//MochiKit.Logging.logDebug(">>> Import.KeePassImportComponent.render");
		this.domHelper().append(this.element(), {tag:'div', cls:'keePassImportWizard', children:[
			{tag:'h3', htmlString:Clipperz.PM.Strings['KeePass_ImportWizard_Title']},
			{tag:'div', cls:'importSteps', id:this.getId('importSteps')},
			{tag:'div', cls:'importStepBlocks', children:[
				{tag:'div', cls:'step_0', id:this.getId('step_0'), children:[
					{tag:'div', children:[
						{tag:'div', cls:'importOptionsDescription', htmlString:Clipperz.PM.Strings['importOptions_keePass_description']},
						{tag:'div', cls:'importOptionsParameters', children:[]},
						this.textAreaConfig()
					]}
				]},
				{tag:'div', cls:'step_1', id:this.getId('step_1'), children:[
					{tag:'div', children:[
						{tag:'div', id:this.getId('settingsDiv'), children:[
							{tag:'table', id:'KeePassSettings', children:[
								{tag:'tbody', children:[
									{tag:'tr', children:[
										{tag:'td', width:'50%', valign:'top', children:[
											{tag:'table', children:[
												{tag:'tbody', children:[
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('Group_checkbox'), name:'Group'/*, checked:true*/}]},
														{tag:'td', width:'150', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('Group_label'), html:"Group"}]}
													]},
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('Group Tree_checkbox'), name:'Group Tree'/*, checked:true*/}]},
														{tag:'td', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('Group Tree_label'), html:"Group Tree"}]}
													]},
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('UserName_checkbox'), name:'UserName', checked:true}]},
														{tag:'td', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('UserName_label'), html:"UserName"}]}
													]},
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('URL_checkbox'), name:'URL', checked:true}]},
														{tag:'td', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('URL_label'), html:"URL"}]}
													]},
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('Password_checkbox'), name:'Password', checked:true}]},
														{tag:'td', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('Password_label'), html:"Password"}]}
													]},
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('Notes_checkbox'), name:'Notes', checked:true}]},
														{tag:'td', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('Notes_label'), html:"Notes"}]}
													]},
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('UUID_checkbox'), name:'UUID'/*, checked:true*/}]},
														{tag:'td', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('UUID_label'), html:"UUID"}]}
													]}
												]}
											]}
										]},
										{tag:'td', width:'50%', valign:'top', children:[
											{tag:'table', children:[
												{tag:'tbody', children:[
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('Icon_checkbox'), name:'Icon'/*, checked:true*/}]},
														{tag:'td', width:'150', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('Icon_label'), html:"Icon"}]}
													]},
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('Creation Time_checkbox'), name:'Creation Time'/*, checked:true*/}]},
														{tag:'td', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('Creation Time_label'), html:"Creation Time"}]}
													]},
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('Last Access_checkbox'), name:'Last Access'/*, checked:true*/}]},
														{tag:'td', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('Last Access_label'), html:"Last Access"}]}
													]},
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('Last Modification_checkbox'), name:'Last Modification'/*, checked:true*/}]},
														{tag:'td', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('Last Modification_label'), html:"Last Modification"}]}
													]},
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('Expires_checkbox'), name:'Expires'/*, checked:true*/}]},
														{tag:'td', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('Expires_label'), html:"Expires"}]}
													]},
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('Attachment Description_checkbox'), name:'Attachment Description', checked:true}]},
														{tag:'td', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('Attachment Description_label'), html:"Attachment Description"}]}
													]},
													{tag:'tr', children:[
														{tag:'td', valign:'top', children:[{tag:'input', type:'checkbox', id:this.getId('Attachment_checkbox'), name:'Attachment', checked:true}]},
														{tag:'td', valign:'top', children:[{tag:'span', cls:'keePassFieldLabel', id:this.getId('Attachment_label'), html:"Attachment"}]}
													]}
												]}
											]}
										]}
									]}
								]}
							]}
						]}
					]}
				]},
				{tag:'div', cls:'step_2', id:this.getId('step_2'), children:[
					{tag:'div', children:[
						{tag:'div', id:this.getId('previewDiv'), html:"preview"}
					]}
				]},
				{tag:'div', cls:'step_3', id:this.getId('step_3'), children:[
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
		this.getElement('step_3').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
//MochiKit.Logging.logDebug("<<< Import.KeePassImportComponent.render");
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
				this.previewValues();
				break;
			case 2:	//	-> 3
				this.importValues();
				break;
		}
	},

	//-------------------------------------------------------------------------
	
	'deferredParseValues': function() {
		var deferredResult;

		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredParseValues - 1 " + res.substring(0,50)); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'parseImportData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredParseValues - 2 " + res.substring(0,50)); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			this.startProcessing();
			
			return res;
		}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredParseValues - 3 " + res.substring(0,50)); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'parseKeePassValues'));	//	processPasswordPlusValues
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredParseValues - 4 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'setParsedValues'));		//	setProcessedValues
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredParseValues - 5 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'showSettings'));			//	previewRecordValues
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredParseValues - 6 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			this.processingDone();
			this.getElement('step_0').hide();
			this.getElement('step_1').show();
			this.backButton().enable();
			
			return res;
		}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredParseValues - 7 " + res); return res;});
		deferredResult.callback(this.textAreaContent());		

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'deferredPreviewValues': function() {
		var deferredResult;

//MochiKit.Logging.logDebug(">>> KeePassImportComonent.deferredPreviewValues");
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 1 " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'previewImportData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 2 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			this.startProcessing();
			
			return res;
		}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 3 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'processKeePassParsedValues'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 4 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'setProcessedValues'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 5 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'previewRecordValues'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 6 " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			this.processingDone();
			this.getElement('step_1').hide();
			this.getElement('step_2').show();
			this.backButton().enable();
			
			return res;
		}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 7 " + res); return res;});
//		deferredResult.addErrback(MochiKit.Base.bind(function() {
//			this.processingAborted();
//		}, this))
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.deferredPreviewValues - 8 " + res); return res;});
		deferredResult.callback(this.parsedValues());		
//MochiKit.Logging.logDebug("<<< KeePassImportComonent.deferredPreviewValues");

		return deferredResult;
	},

	//-------------------------------------------------------------------------
	
	'definedFields': function() {
		return this._definedFields;
	},

	//-------------------------------------------------------------------------
	
	'parseKeePassValues': function(someData) {
		var deferredResult;
		var keePassProcessor;

		keePassProcessor = new Clipperz.KeePassExportProcessor();
		
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.parseKeePassValues - 1 " + res.substring(0,50)); return res;});
		deferredResult.addCallback(function(res) {
			return Clipperz.NotificationCenter.deferredNotification(this, 'updatedProgressState', {steps:(res.length)}, res);
		})
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.parseKeePassValues - 2 " + res.substring(0,50)); return res;});
		deferredResult.addCallback(MochiKit.Base.method(keePassProcessor, 'deferredParse'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("KeePassImportComponent.parseKeePassValues - 3 " + res); return res;});
		deferredResult.callback(someData);

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'showSettings': function(someValues) {
		var availableFields;
		var i,c;

//MochiKit.Logging.logDebug(">>> KeePassImportCOmponent.showSettings");
		availableFields = new Clipperz.Set();
		c = this.parsedValues().length;
		for (i=0; i<c; i++) {
			var fieldLabel;

			for (fieldLabel in this.parsedValues()[i]) {
				availableFields.add(fieldLabel);
			}
		}
		
		c = this.definedFields().length;
		for (i=0; i<c; i++) {
			var definedField;
			
			definedField = this.definedFields()[i];
			if (availableFields.contains(definedField)) {
//MochiKit.Logging.logDebug("enabling field " + definedField);
				this.getDom(definedField + '_checkbox').disabled = false;
				this.getElement(definedField + '_label').removeClass('disabled');
			} else {
//MochiKit.Logging.logDebug("disabling field " + definedField);
				this.getDom(definedField + '_checkbox').disabled = true;
				this.getDom(definedField + '_checkbox').checked = false;	//	????
				this.getElement(definedField + '_label').addClass('disabled');
			}
		}
//MochiKit.Logging.logDebug("<<< KeePassImportCOmponent.showSettings");
		
		return MochiKit.Async.succeed(someValues);
	},

	//-------------------------------------------------------------------------

	'shouldImportField': function(aFieldName) {
		var fieldCheckbox;
		var result;

//MochiKit.Logging.logDebug(">>> shouldImportField: " + aFieldName);
//		fieldCheckbox = this.getDom(aFieldName + '_checkbox');
		fieldCheckbox = MochiKit.DOM.getElement(this.getId(aFieldName + '_checkbox'));
		if (fieldCheckbox != null) {
			result = fieldCheckbox.checked;
		} else {
			result = false;
		}
//MochiKit.Logging.logDebug("<<< shouldImportField: " + result);
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	'processKeePassParsedValues': function(someValues) {
		var deferredResult;
		var records;
		var i,c;
		
//MochiKit.Logging.logDebug(">>> processKeePassParsedValues");
		deferredResult = new MochiKit.Async.Deferred();
		records = [];
		
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("processKeePassParsedValues - 1: " + res); return res;});
		c = someValues.length;
		deferredResult.addCallback(function(res) {
			return Clipperz.NotificationCenter.deferredNotification(this, 'updatedProgressState', {steps:c}, res);
		})
		for(i=0; i<c; i++) {
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("[" + i + "] processKeePassParsedValues - 1.1: " + res); return res;});
			deferredResult.addCallback(MochiKit.Async.wait, 0.2);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("[" + i + "] processKeePassParsedValues - 1.2: " + res); return res;});
			deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', {});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("[" + i + "] processKeePassParsedValues - 1.3: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.bind(function(someRecords, someData) {
				var record;
				var recordVersion;
				var ii;

				record = new Clipperz.PM.DataModel.Record({user:this.user()});
				record.setLabel(someData['Title']);
				if (this.shouldImportField('Notes')) {
					record.setNotes(someData['Notes']);
				}
				recordVersion = record.currentVersion()

				for (ii in someData) {
					if ((ii != 'Title') && (ii != 'Notes') && (typeof(someData[ii]) != "undefined") && (this.shouldImportField(ii))) {
						var recordField;
						var recordFieldType;
						
						recordFieldType = 'TXT';
						if (ii == 'Password') {
							recordFieldType = 'PWD';
						} else if (ii == 'URL') {
							recordFieldType = 'URL';
						} else if ((ii == 'Creation Time') || (ii == 'Last Access') || (ii == 'Last Modification') || (ii == 'Expires')) {
							recordFieldType = 'Date';
						}
					
						recordField = new Clipperz.PM.DataModel.RecordField({
							recordVersion:	recordVersion,
							label:			ii,
							value:			someData[ii],
							type:			recordFieldType
						});
						recordVersion.addField(recordField);
					}
				}
			
				someRecords.push(record);
			
				return someRecords;
			}, this), records, someValues[i]);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("[" + i + "] processKeePassParsedValues - 1.4: " + res); return res;});
		}
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("processKeePassParsedValues - 2: " + res); return res;});
		deferredResult.addCallback(MochiKit.Async.succeed, records);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("processKeePassParsedValues - 3: " + res); return res;});
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< processKeePassParsedValues");
		
		return deferredResult;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

