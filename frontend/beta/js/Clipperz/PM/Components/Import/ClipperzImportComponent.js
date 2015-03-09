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

Clipperz.PM.Components.Import.ClipperzImportComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Import.ClipperzImportComponent.superclass.constructor.call(this, anElement, args);

	this.render();

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Import.ClipperzImportComponent, Clipperz.PM.Components.Import.GenericImportComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Import.ClipperzImportComponent component";
	},

	//-------------------------------------------------------------------------

	'render': function() {
//MochiKit.Logging.logDebug(">>> Import.ClipperzImportComponent.render");
		this.domHelper().append(this.element(), {tag:'div', cls:'clipperzImportWizard', children:[
			{tag:'h3', htmlString:Clipperz.PM.Strings['Clipperz_ImportWizard_Title']},
			{tag:'div', cls:'importSteps', id:this.getId('importSteps')},
			{tag:'div', cls:'importStepBlocks', children:[
				{tag:'div', cls:'step_0', id:this.getId('step_0'), children:[
					{tag:'div', children:[
						{tag:'div', cls:'importOptionsDescription', htmlString:Clipperz.PM.Strings['importOptions_clipperz_description']},
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
//MochiKit.Logging.logDebug("<<< Import.ClipperzImportComponent.render");
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

		deferredResult = new MochiKit.Async.Deferred();
		deferredResult.addCallback(MochiKit.Base.bind(function(res) {
			this.startProcessing();
			
			return res;
		}, this));
		deferredResult.addCallback(MochiKit.Base.method(this, 'processClipperzValues'));
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

	'processClipperzValues': function(someData) {
		var deferredResult;
		
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Record.processClipperzValues - 1: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'parseImportData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Record.processClipperzValues - 2: " + res); return res;});
		deferredResult.addCallback(Clipperz.Base.evalJSON);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Record.processClipperzValues - 3: " + res); return res;});
		deferredResult.addCallback(function(res) {
			return Clipperz.NotificationCenter.deferredNotification(this, 'updatedProgressState', {steps:(res.length)}, res);
		})
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Record.processClipperzValues - 4: " + res); return res;});
		deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', 'previewImportData');
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Record.processClipperzValues - 5: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.bind(function(someClipperzValues) {
			var innerDeferredResult;
			var records;
			var i,c;
			
			innerDeferredResult = new MochiKit.Async.Deferred();
			records = [];
			
			c = someClipperzValues.length;
			for(i=0; i<c; i++) {
				innerDeferredResult.addCallback(MochiKit.Async.wait, 0.2);
				innerDeferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'updatedProgressState', {});
				innerDeferredResult.addCallback(MochiKit.Base.bind(function(someRecords, someData) {
					var record;
					var recordVersion;

//MochiKit.Logging.logDebug("=== someData: " + Clipperz.Base.serializeJSON(someData));
					record = new Clipperz.PM.DataModel.Record({user:this.user()});
					record.setLabel(someData['label']);
					record.setShouldProcessData(true);
					record.processData(someData);

					someRecords.push(record);

					return someRecords;
				}, this), records, someClipperzValues[i]);
			}
			innerDeferredResult.addCallback(MochiKit.Async.succeed, records);
			innerDeferredResult.callback();
			
			return innerDeferredResult;
		}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Record.processClipperzValues - 6: " + res); return res;});
		deferredResult.callback(someData);

		return deferredResult;
	},


	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

