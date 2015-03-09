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
if (typeof(Clipperz.PM.Components.Panels) == 'undefined') { Clipperz.PM.Components.Panels = {}; }

//#############################################################################

Clipperz.PM.Components.Panels.MainPanel = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Panels.MainPanel.superclass.constructor.call(this, anElement, args);

	this._recordListDataModel = null;
	this._selectedRecord = null;
	this._recordDetailComponent = null;
	this._recordListGrid = null;

	this._directLinkItemTemplate = null;
	this._recordItemTemplate = null;
	
	this._addNewRecordButton = null;
	this._deleteRecordButton = null;

	this._creationWizard = null;

	Clipperz.NotificationCenter.register(null, 'selectAndEnterEditMode', this, 'selectRecordAndEnterEditModeHandler');

	Clipperz.NotificationCenter.register(null, 'recordAdded', this, 'recordAddedHandler');
	Clipperz.NotificationCenter.register(null, 'recordUpdated', this, 'recordUpdatedHandler');
	Clipperz.NotificationCenter.register(null, 'recordRemoved', this, 'recordRemovedHandler');

	Clipperz.NotificationCenter.register(null, 'directLoginAdded', this, 'directLoginAddedHandler');
	Clipperz.NotificationCenter.register(null, 'directLoginUpdated', this, 'directLoginUpdatedHandler');
	Clipperz.NotificationCenter.register(null, 'directLoginRemoved', this, 'directLoginRemovedHandler');

	Clipperz.NotificationCenter.register(null, 'accountLocked', this, 'accountLockedHandler');

	MochiKit.Signal.connect(MochiKit.DOM.currentWindow(), 'onresize', this, 'resizeModalMask');
	this.render();
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Panels.MainPanel, Clipperz.PM.Components.Panels.BasePanel, {

	'toString': function() {
		return "Clipperz.PM.Components.Panels.MainPanel component";
	},
	
	//-------------------------------------------------------------------------

	'render': function() {
		this.element().update("");
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'table', id:'mainPanelTABLE', border:'0', cellspacing:'0', cellpadding:'0', children:[
			{tag:'tbody', children:[
				{tag:'tr', children:[
					{tag:'td', width:'15', children:[
						{tag:'div', cls:'mainPanelMinHeightDiv'}
					]},
					{tag:'td', valign:'top', id:'directLoginsTD', width:'200', children:[
						{tag:'div', id:'accountStatus', children:[
							{tag:'div', cls:'header', children:[{tag:'h5', cls:'title', htmlString:"Account info"}]},
//							{tag:'div', cls:'accountStatus', html:"early adopter"},
							{tag:'div', cls:'accountLevel', children:[
								{tag:'span', cls:'label', html:"status"},
								{tag:'span', cls:'status', html:"early adopter"}
							]},
							{tag:'div', cls:'accountLevel', children:[
								{tag:'span', cls:'label', html:"level"},
//								{tag:'span', cls:'level', html:"★☆☆☆"}
								{tag:'span', cls:'level', html:"☆☆☆☆"}
							]},
							{tag:'div', cls:'accountExpiration', children:[
								{tag:'span', cls:'label', html:"expires"},
//								{tag:'span', cls:'expriation', html:"on 26 April 2014"}
								{tag:'span', cls:'expriation', html:"never"}
							]},
//							{tag:'div', cls:'payButton', children:[
//								{tag:'a', href:'#', cls:'info', html:"info"}
//							]}
							{tag:'div', id:'payButton', cls:'payButton'}
						]},
						{tag:'div', id:'directLoginsBlock', children:[
							{tag:'div', cls:'directLoginsBlockHeaderBox', children:[{tag:'h3', id:'directLoginTitle', htmlString:Clipperz.PM.Strings['mainPanelDirectLoginBlockLabel']}]},
							{tag:'div', id:'directLoginsDescription', htmlString:Clipperz.PM.Strings['mainPanelDirectLoginBlockDescription']},
							{tag:'ul', id:'directLogins'}
						]}
					]},
					{tag:'td', width:'15', children:[
						{tag:'div', cls:'mainPanelMinHeightDiv'}
					]},
					{tag:'td', valign:'top', children:[
						{tag:'div', id:'mainContent', children:[
							{tag:'div', id:'recordListBlockHeader'},
							{tag:'div', id:'recordListAndDetailBlock', children:[
								{tag:'table', id:'recordListAndDetailBlockTABLE', border:'0', cellspacing:'0', cellpadding:'0', children:[
									{tag:'tbody', children:[
										{tag:'tr', children:[
											{tag:'td', valign:'top', width:'250', children:[
												{tag:'div', id:'recordListBlock', children:[
													{tag:'div', id:'recordListFilterHeader'},
													{tag:'ul', id:'records'}
												]}
											]},
											{tag:'td', id:'recordDetailSeparatorTD', rowspan:'2', valign:'top', bgcolor:'#ddddff', html:'&nbsp;'},
											{tag:'td', valign:'top', children:[
												{tag:'div', id:'recordDetailMainBlock', children:[
													{tag:'div', id:'recordTitleTopBlock'},
													{tag:'div', id:'recordDetailBlock', children:[
														{tag:'div', id:'recordDetail'}
													]}
												]},
												{tag:'div', id:'recordCreationWizardMainBlock', children:[
													{tag:'div', id:'recordCreationWizard', html:"WIZARD"}
												]}
											]}
										]},
										{tag:'tr', children:[
											{tag:'td', id:'cardBoxLowerLeftTD', html:'&nbsp;'},
											{tag:'td', id:'cardBoxLowerRightTD', html:'&nbsp;'}
										]}
									]}
								]}
							]}
						]}
					]},
					{tag:'td', width:'15', html:"&nbsp;"}
				]}
			]}
		]});

		this.renderRecordListBlockHeader();
//		this.renderRecordListFilterHeader();

		YAHOO.ext.Element.get('directLogins').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		//	TODO
		new YAHOO.ext.Button('payButton', {text:"Info", handler:this.payButtonHandler, scope:this});
		
		this.recordDetailComponent();

		YAHOO.ext.Element.get('recordDetailMainBlock').setVisibilityMode(YAHOO.ext.Element.DISPLAY).show();
		YAHOO.ext.Element.get('recordCreationWizardMainBlock').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
	},

	//-------------------------------------------------------------------------

	'payButtonHandler': function(anEvent) {
		window.open('https://www.clipperz.com/pricing/', '_blank');
	},
	
	//-------------------------------------------------------------------------

	'addNewRecordButton': function() {
		return this._addNewRecordButton;
	},
	
	'setAddNewRecordButton': function(aValue) {
		this._addNewRecordButton = aValue;
	},
	
	'deleteRecordButton': function() {
		return this._deleteRecordButton;
	},
	
	'setDeleteRecordButton': function(aValue) {
		this._deleteRecordButton = aValue;
	},

	//-------------------------------------------------------------------------
	
	'addNewRecord': function(anEvent) {
		var deferredResult;
//		var currentNumberOfRecords;
		
		deferredResult = new MochiKit.Async.Deferred();
		
//		currentNumberOfRecords = MochiKit.Base.keys(this.user().records()).length;
/*
//		if ((this.user().preferences().shouldShowDonationPanel()) && (currentNumberOfRecords > 0) && ((currentNumberOfRecords%10) == 0)) {
//		if (true) {
		if ((this.user().preferences().shouldShowDonationPanel()) && (currentNumberOfRecords >= 5)) {
			deferredResult.addCallback(Clipperz.PM.showDonationSplashScreen, this.user(), 'recordListAddRecordButton');
		}
*/
		deferredResult.addCallback(MochiKit.Base.bind(function() {
			var currentlySelectedRecord;
			
			currentlySelecedRecord = this.selectedRecord();
			this.setSelectedRecord(null);
		
			YAHOO.ext.Element.get('recordDetailMainBlock').hide();
			YAHOO.ext.Element.get('recordCreationWizardMainBlock').show();
			this.setCreationWizard(new Clipperz.PM.Components.RecordDetail.CreationWizard(YAHOO.ext.Element.get('recordCreationWizardMainBlock'), {previouslySelectedRecord:currentlySelecedRecord, mainComponent:this}));
		
			this.enterModalView();
		}, this));

		deferredResult.callback();
	},

	//-------------------------------------------------------------------------

	'creationWizard': function() {
		return this._creationWizard;
	},
	
	'setCreationWizard': function(aValue) {
		this._creationWizard = aValue;
	},
	
	//-------------------------------------------------------------------------
	
	'exitWizard': function(aSelectedRecord, shouldEnterEditMode) {
//MochiKit.Logging.logDebug(">>> MainPanel.exitWizard - " + aSelectedRecord)
		YAHOO.ext.Element.get('recordCreationWizardMainBlock').hide();
		YAHOO.ext.Element.get('recordDetailMainBlock').show();

		if (shouldEnterEditMode == true) {
			this.selectRecordAndEnterEditMode(aSelectedRecord);
		} else {
			this.setSelectedRecord(aSelectedRecord);
			this.exitModalView();
		}
		
		this.creationWizard().remove();
		this.setCreationWizard(null);
//MochiKit.Logging.logDebug("<<< MainPanel.exitWizard");
	},
	
	//-------------------------------------------------------------------------

	'selectRecordAndEnterEditMode': function(aRecord) {
		this.setSelectedRecord(aRecord);
		this.recordDetailComponent().setEditMode('EDIT');
	},

	'selectRecordAndEnterEditModeHandler': function(anEvent) {
		this.selectRecordAndEnterEditMode(anEvent.source());
	},

	//-------------------------------------------------------------------------

	'resizeModalMask': function() {
//MochiKit.Logging.logDebug(">>> MainPanel.resizeModalMask");
		MochiKit.Style.setElementDimensions('recordDetailEditModeHeaderMask', {w:MochiKit.Style.getElementDimensions('mainDiv').w, h:119});

		MochiKit.Style.setElementDimensions('recordDetailEditModeVerticalMask', {w:511, h:MochiKit.Style.getElementDimensions('mainDiv').h - 119});
//MochiKit.Logging.logDebug("<<< MainPanel.resizeModalMask");
	},

	//-------------------------------------------------------------------------
	
	'enterModalView': function() {
		if (this.user().preferences().useSafeEditMode()) {
			var headerMaskElement;
			var verticalMaskElement;

			this.resizeModalMask();

			headerMaskElement = YAHOO.ext.Element.get('recordDetailEditModeHeaderMask');
			headerMaskElement.show();
			headerMaskElement.mask();

			verticalMaskElement = YAHOO.ext.Element.get('recordDetailEditModeVerticalMask');
			verticalMaskElement.show();
			verticalMaskElement.mask();
		}
	},
	
	//-------------------------------------------------------------------------

	'exitModalView': function() {
		if (this.user().preferences().useSafeEditMode()) {
			var headerMaskElement;
			var verticalMaskElement;
			
			headerMaskElement = YAHOO.ext.Element.get('recordDetailEditModeHeaderMask');
			headerMaskElement.unmask();
			headerMaskElement.hide();

			verticalMaskElement = YAHOO.ext.Element.get('recordDetailEditModeVerticalMask');
			verticalMaskElement.unmask();
			verticalMaskElement.hide();
		}
	},

	//-------------------------------------------------------------------------

	'removeSelectedRecord': function() {
		var selectedRecordReferences;

//MochiKit.Logging.logDebug(">>> MainPanel.removeSelectedRecord");
		if (this.selectedRecord() != null) {
			selectedRecordReferences = [this.selectedRecord().reference()];
		} else {
			selectedRecordReferences = [];
		}

		if (selectedRecordReferences.length > 0 ) {
			var	recordReference;
			var	records;
			var deferred;
			
			records = [];
			for (recordReference in selectedRecordReferences) {
				var record;
		
//MochiKit.Logging.logDebug("### MainPanel.removeSelectedRecord - recordReference: " + selectedRecordReferences[recordReference]);
				record = this.user().records()[selectedRecordReferences[recordReference]];
//MochiKit.Logging.logDebug("### MainPanel.removeSelectedRecord - record: " + record);
				records.push(record);
			}
//MochiKit.Logging.logDebug("### MainPanel.removeSelectedRecord - records.length: " + records.length);

			deferred = new MochiKit.Async.Deferred();
//MochiKit.Logging.logDebug("--- MainPanel.removeSelectedRecord - 1:");
//deferred.addBoth(function(res) {MochiKit.Logging.logDebug("MainPanel.removeSelectedRecord - 1: " + res); return res;});
//MochiKit.Logging.logDebug("--- MainPanel.removeSelectedRecord - 2:");
			deferred.addCallback(function() {
				var deferredResult;
				
				deferredResult = new MochiKit.Async.Deferred();
				Clipperz.PM.Components.MessageBox().deferredShow({
					title:Clipperz.PM.Strings['mainPanelDeletingRecordPanelConfirmationTitle'],
					text:Clipperz.PM.Strings['mainPanelDeleteRecordPanelConfirmationText'],
					width:240,
					showProgressBar:false,
					showCloseButton:false,
					buttons:{
						'yes':Clipperz.PM.Strings['mainPanelDeleteRecordPanelConfirmButtonLabel'],
						'no':Clipperz.PM.Strings['mainPanelDeleteRecordPanelDenyButtonLabel']
 					},
					fn:MochiKit.Base.partial(function(aDeferred, aResult) {
						if (aResult == 'yes') {
							aDeferred.callback(aResult);
						} else {
							aDeferred.errback(aResult);
						}
					}, deferredResult)
				}, 'recordListRemoveRecordButton');
				
				return deferredResult;
			});
//MochiKit.Logging.logDebug("--- MainPanel.removeSelectedRecord - 3:");
//deferred.addBoth(function(res) {MochiKit.Logging.logDebug("MainPanel.removeSelectedRecord - 2: " + res); return res;});
//MochiKit.Logging.logDebug("--- MainPanel.removeSelectedRecord - 4:");
//deferred.addBoth(function(res) {MochiKit.Logging.logDebug("MainPanel.removeSelectedRecord - 3: " + res); return res;});
			deferred.addCallback(MochiKit.Base.method(Clipperz.PM.Components.MessageBox(), 'deferredShow'),
				{
					title:Clipperz.PM.Strings['mainPanelDeletingRecordPanelInitialTitle'],
					text:Clipperz.PM.Strings['mainPanelDeletingRecordPanelInitialText'],
					width:240,
					showProgressBar:true,
					showCloseButton:false,
					steps:5
				}
			);
//MochiKit.Logging.logDebug("--- MainPanel.removeSelectedRecord - 5:");
//deferred.addBoth(function(res) {MochiKit.Logging.logDebug("MainPanel.removeSelectedRecord - 4: " + res); return res;});
			deferred.addCallback(MochiKit.Base.method(this.user(), 'deleteRecordsAction'), records);
//MochiKit.Logging.logDebug("--- MainPanel.removeSelectedRecord - 6:");
//deferred.addBoth(function(res) {MochiKit.Logging.logDebug("MainPanel.removeSelectedRecord - 5: " + res); return res;});
//MochiKit.Logging.logDebug("--- MainPanel.removeSelectedRecord - 7:");
//deferred.addBoth(function(res) {MochiKit.Logging.logDebug("MainPanel.removeSelectedRecord - 6: " + res); return res;});
			deferred.addCallback(function() {
				Clipperz.PM.Components.MessageBox().update({
					title:null,
					text:Clipperz.PM.Strings['mainPanelDeletingRecordPanelCompletedText'],
					step:'next',
					buttons:{}
				});
			});
//MochiKit.Logging.logDebug("--- MainPanel.removeSelectedRecord - 8:");
//deferred.addBoth(function(res) {MochiKit.Logging.logDebug("MainPanel.removeSelectedRecord - 7: " + res); return res;});
			deferred.addCallback(MochiKit.Async.wait, 1);
//MochiKit.Logging.logDebug("--- MainPanel.removeSelectedRecord - 9:");
//deferred.addBoth(function(res) {MochiKit.Logging.logDebug("MainPanel.removeSelectedRecord - 8: " + res); return res;});
			deferred.addCallback(function(res) {
				Clipperz.PM.Components.MessageBox().hide(YAHOO.ext.Element.get('main'));
				return res;
			});
//MochiKit.Logging.logDebug("--- MainPanel.removeSelectedRecord - 10:");
//deferred.addBoth(function(res) {MochiKit.Logging.logDebug("MainPanel.removeSelectedRecord - 9: " + res); return res;});
			deferred.callback();
		} else {
//MochiKit.Logging.logDebug("+++ MainPanel.removeSelectedRecord - nothing selected");
		}
	},

	//-------------------------------------------------------------------------

	'recordDetailComponent': function() {
//MochiKit.Logging.logDebug(">>> MainPanel.recordDetailComponent");
		if (this._recordDetailComponent == null) {
//MochiKit.Logging.logDebug("--- MainPanel.recordDetailComponent - 1");
//MochiKit.Logging.logDebug("--- MainPanel.recordDetailComponent - 1 - user: " + this.user());
			this._recordDetailComponent = new Clipperz.PM.Components.RecordDetail.MainComponent(
				YAHOO.ext.Element.get('recordDetail'),
				{user:this.user(), mainPanel:this}
			);
		}

//MochiKit.Logging.logDebug("<<< MainPanel.recordDetailComponent");
		
		return this._recordDetailComponent;
	},

	//-------------------------------------------------------------------------

	'selectedRecord': function() {
		return this._selectedRecord;
	},
	
	'setSelectedRecord': function(aValue) {
//		this.hideNewRecordPanel();
//MochiKit.Logging.logDebug(">>> MainPanel.setSelectedRecord");
		if (aValue != this._selectedRecord) {
//MochiKit.Logging.logDebug("--- MainPanel.setSelectedRecord - 1");
			this._selectedRecord = aValue;
//MochiKit.Logging.logDebug("--- MainPanel.setSelectedRecord - 2");
			this.redrawRecordItems();
//MochiKit.Logging.logDebug("--- MainPanel.setSelectedRecord - 3");
			this.recordDetailComponent().setRecord(aValue);
//MochiKit.Logging.logDebug("--- MainPanel.setSelectedRecord - 4");
		}
		
		if ((aValue == null) || (Clipperz.PM.Proxy.defaultProxy.isReadOnly())) {
			this.deleteRecordButton().disable();
		} else {
			this.deleteRecordButton().enable();
		}
//MochiKit.Logging.logDebug("<<< MainPanel.setSelectedRecord");
	},

	//-------------------------------------------------------------------------

	'recordAddedHandler': function(anEvent) {
//MochiKit.Logging.logDebug(">>> MainPanel.recordAddedHandler");
		this.recordDetailComponent();
		this.redrawRecordItems();
//MochiKit.Logging.logDebug("<<< MainPanel.recordAddedHandler");
	},
	
	'recordUpdatedHandler': function(anEvent) {
//MochiKit.Logging.logDebug(">>> MainPanel.recordUpdatedHandler");
		this.redrawRecordItems();
//MochiKit.Logging.logDebug("<<< MainPanel.recordUpdatedHandler");
	},

	'recordRemovedHandler': function(anEvent) {
//MochiKit.Logging.logDebug(">>> MainPanel.recordRemovedHandler");
		this.setSelectedRecord(null);
//MochiKit.Logging.logDebug("--- MainPanel.recordRemovedHandler - 1");
		this.redrawRecordItems();
//MochiKit.Logging.logDebug("<<< MainPanel.recordRemovedHandler");
	},
	
	'compareRecords': function(a, b) {
//MochiKit.Logging.logDebug("=== compareRecords: " + a.toString() + " - " + b.toString());
		return MochiKit.Base.compare(a.label().toLowerCase(), b.label().toLowerCase());
	},
	
	'redrawRecordItems': function() {
		var template;
		var allRecords;

//MochiKit.Logging.logDebug(">>> MainPanel.redrawRecordItems");
		MochiKit.Iter.forEach(YAHOO.ext.Element.get('records').getChildrenByTagName('li'), function(aRecordElement) {
			MochiKit.Signal.disconnectAll(aRecordElement.dom);
		})
//MochiKit.Logging.logDebug("--- MainPanel.redrawRecordItems - 1");
		YAHOO.ext.Element.get('records').update("");
//MochiKit.Logging.logDebug("--- MainPanel.redrawRecordItems - 2");
		allRecords = MochiKit.Base.values(this.user().records());
//MochiKit.Logging.logDebug("--- MainPanel.redrawRecordItems - 3");
		allRecords.sort(this.compareRecords);
//MochiKit.Logging.logDebug("--- MainPanel.redrawRecordItems - 4");
		template = this.recordItemTemplate();
//MochiKit.Logging.logDebug("--- MainPanel.redrawRecordItems - 5");
		MochiKit.Iter.forEach(allRecords, MochiKit.Base.bind(function(aRecord) {
			var	recordElement;
			recordElement = template.append('records', {
				recordTitle:aRecord.label(),
				recordReference:aRecord.reference(),
				cls:((aRecord == this.selectedRecord()) ? 'selected': '')
			}, true);
//MochiKit.Logging.logDebug("--- MainPanel.redrawRecordItems - 6: " + recordElement.dom);
			recordElement.addClassOnOver('hover');
			MochiKit.Signal.connect(recordElement.dom, 'onclick', this, 'selectRecord');
		}, this));
//MochiKit.Logging.logDebug("<<< MainPanel.redrawRecordItems");
	},

	'selectRecord': function(anEvent) {
//MochiKit.Logging.logDebug(">>> MainPanel.selectRecord");
//MochiKit.Logging.logDebug("--- MainPanel.selectRecord !!! - ", this.user().records()[anEvent.src().id].label());
		this.setSelectedRecord(this.user().records()[anEvent.src().id]);
//MochiKit.Logging.logDebug("<<< MainPanel.selectRecord");		
	},
	
	//-------------------------------------------------------------------------

	'directLoginAddedHandler': function(anEvent) {
//MochiKit.Logging.logDebug(">>> MainPanel.recordRemovedHandler");
		this.redrawDirectLoginItems();
//MochiKit.Logging.logDebug("<<< MainPanel.recordRemovedHandler");
	},
	
	'directLoginUpdatedHandler': function(anEvent) {
//MochiKit.Logging.logDebug(">>> MainPanel.directLoginUpdatedHandler");
		this.redrawDirectLoginItems();
//MochiKit.Logging.logDebug("<<< MainPanel.directLoginUpdatedHandler");
	},
	
	'directLoginRemovedHandler': function(anEvent) {
//MochiKit.Logging.logDebug(">>> MainPanel.directLoginRemovedHandler");
		this.redrawDirectLoginItems();
//MochiKit.Logging.logDebug("<<< MainPanel.directLoginRemovedHandler");
	},

	'compareDirectLogins': function(a, b) {
		return MochiKit.Base.compare(a.label().toLowerCase(), b.label().toLowerCase());
	},
	
	'redrawDirectLoginItems': function() {
		var template;
		var allDirectLogins;

//MochiKit.Logging.logDebug(">>> MainPanel.redrawDirectLoginItems");
		MochiKit.Iter.forEach(YAHOO.ext.Element.get('directLogins').getChildrenByTagName('li'), function(aDirectLoginElement) {
			MochiKit.Signal.disconnectAll(aDirectLoginElement.dom);
//MochiKit.Logging.logDebug("disconnecting IMG " + aDirectLoginElement.getChildrenByTagName('img')[0].dom.src);
			MochiKit.Signal.disconnectAll(aDirectLoginElement.getChildrenByTagName('img')[0].dom);
		})
//MochiKit.Logging.logDebug("--- MainPanel.redrawDirectLoginItems - 1");
		YAHOO.ext.Element.get('directLogins').update("");
//MochiKit.Logging.logDebug("--- MainPanel.redrawDirectLoginItems - 2");
		allDirectLogins = MochiKit.Base.values(this.user().directLoginReferences());
//MochiKit.Logging.logDebug("--- MainPanel.redrawDirectLoginItems - 3");
		allDirectLogins.sort(this.compareDirectLogins);

		if (allDirectLogins.length == 0) {
			YAHOO.ext.Element.get('directLoginsDescription').show();
			YAHOO.ext.Element.get('directLogins').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		} else {
			YAHOO.ext.Element.get('directLoginsDescription').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
			YAHOO.ext.Element.get('directLogins').show();
		}
//MochiKit.Logging.logDebug("--- MainPanel.redrawDirectLoginItems - 4");
		template = this.directLoginItemTemplate();
//MochiKit.Logging.logDebug("--- MainPanel.redrawDirectLoginItems - 5");
		MochiKit.Iter.forEach(allDirectLogins, MochiKit.Base.bind(function(aDirectLogin) {
			var	directLoginElement;
			var	faviconImageElementID;
			
			faviconImageElementID = aDirectLogin.reference() + "_faviconIMG";
			directLoginElement = template.append('directLogins', {
				elementID:faviconImageElementID,
				faviconUrl:aDirectLogin.fixedFavicon(), 
				directLoginTitle:aDirectLogin.label(),
				directLoginReference:aDirectLogin.reference()
			}, true);
//MochiKit.Logging.logDebug("--- MainPanel.redrawDirectLoginItems - 6: " + recordElement.dom);
			directLoginElement.addClassOnOver("hover");
			MochiKit.Signal.connect(directLoginElement.dom, 'onclick', this, 'handleDirectLoginClick');

			MochiKit.Signal.connect(faviconImageElementID, 'onload', this, 'handleLoadedFaviconImage');
			MochiKit.Signal.connect(faviconImageElementID, 'onerror', aDirectLogin, 'handleMissingFaviconImage');
			MochiKit.Signal.connect(faviconImageElementID, 'onabort', aDirectLogin, 'handleMissingFaviconImage');
			
//			YAHOO.ext.Element.get(faviconImageElementID).dom.src = aDirectLogin.fixedFavicon();
		}, this));
//MochiKit.Logging.logDebug("<<< MainPanel.redrawDirectLoginItems");
	},

	//-------------------------------------------------------------------------

	'handleDirectLoginClick': function(anEvent) {
		var	directLoginReference;
//MochiKit.Logging.logDebug(">>> MainPanel.handleDirectLoginClick !!!");

		directLoginReference = this.user().directLoginReferences()[anEvent.src().id];
		if (anEvent.target().className == 'directLoginItemEditButton') {
			this.editDirectLogin(directLoginReference);
		} else {
			this.openDirectLogin(directLoginReference);
		}
//MochiKit.Logging.logDebug("<<< MainPanel.handleDirectLoginClick");		
	},

	'editDirectLogin': function(aDirectLoginReference) {
//MochiKit.Logging.logDebug("=== MainPanel.editDirectLogin - " + aDirectLoginReference.label());
		this.setSelectedRecord(aDirectLoginReference.record());
	},
	
	'openDirectLogin': function(aDirectLoginReference) {
		var	deferredResult;
		var	newWindow;
				
//MochiKit.Logging.logDebug(">>> MainPanel.openDirectLogin - " + aDirectLoginReference.label());
		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("MainPanel.openDirectLogin - 1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(aDirectLoginReference, 'setupJumpPageWindow'));
		deferredResult.addCallback(MochiKit.Base.method(aDirectLoginReference, 'deferredDirectLogin'));
		deferredResult.addCallback(function(aDirectLogin) {
			aDirectLogin.runDirectLogin(newWindow);
		});

		newWindow = window.open(Clipperz.PM.Strings['directLoginJumpPageUrl'], "");
//		MochiKit.Signal.connect(newWindow, 'onload', MochiKit.Base.method(deferredResult, 'callback', newWindow))
//		MochiKit.Signal.connect(newWindow, 'onload', MochiKit.Base.partial(alert, "done"));
		deferredResult.callback(newWindow);
//MochiKit.Logging.logDebug("<<< MainPanel.openDirectLogin");
	},

	//-------------------------------------------------------------------------

	'handleLoadedFaviconImage': function(anEvent) {
//MochiKit.Logging.logDebug(">>> MainPanel.handleLoadedFaviconImage");
		MochiKit.Signal.disconnectAll(anEvent.src())
//MochiKit.Logging.logDebug("<<< MainPanel.handleLoadedFaviconImage");
	},
	
	//-------------------------------------------------------------------------

	'recordItemTemplate': function() {
		if (this._recordItemTemplate == null) {
			this._recordItemTemplate = Clipperz.YUI.DomHelper.createTemplate({tag:'li', cls:'{cls}', id:'{recordReference}', children:[
				{tag:'span', html:'{recordTitle}'}
			]});
			this._recordItemTemplate.compile();
		}
		
		return this._recordItemTemplate;
	},
	
	'directLoginItemTemplate': function() {
		if (this._directLoginItemTemplate == null) {
			this._directLoginItemTemplate = Clipperz.YUI.DomHelper.createTemplate({tag:'li', id:'{directLoginReference}', children:[
				{tag:'table', border:'0', cellpadding:'0', cellspacing:'0', children:[
					{tag:'tbody', children:[
						{tag:'tr', children:[
							{tag:'td', width:'20', align:'center', valign:'top', children:[
								{tag:'img', id:'{elementID}', src:'{faviconUrl}'}
							]},
							{tag:'td', valign:'top', children:[
								{tag:'a', cls:'directLoginItemTitle', html:'{directLoginTitle}'}
							]},
							{tag:'td', valign:'top', align:'right', children:[
//								{tag:'span', cls:'directLoginItemEditButton', htmlString:Clipperz.PM.Strings['directLinkReferenceShowButtonLabel']}
								{tag:'a', cls:'directLoginItemEditButton', htmlString:Clipperz.PM.Strings['directLinkReferenceShowButtonLabel']}
							]}
						]}
					]}
				]}
			]});
			this._directLoginItemTemplate.compile();
		}
		
		return this._directLoginItemTemplate;
	},
	
	//-------------------------------------------------------------------------
/*
	'newRecordButton': function() {
		return this._newRecordButton;
	},

	'setNewRecordButton': function(aValue) {
		this._newRecordButton = aValue;
	},
	
	'newRecordCancelButton': function() {
		return this._newRecordCancelButton;
	},
	
	'setNewRecordCancelButton': function(aValue) {
		this._newRecordCancelButton = aValue;
	},
*/	
	//-------------------------------------------------------------------------

	'onkeydown': function(anEvent) {
//MochiKit.Logging.logDebug(">>> onkeydown - " + anEvent.src().id + ": " + anEvent.key().code);
		switch (anEvent.src().id) {
/*
			case this.getId('newRecordForm'):
				if (anEvent.key().code == 13) {
					this.newRecordButton().focus();
//					this.addNewRecord();
				} else if (anEvent.key().code == 27) {
					this.newRecordCancelButton().focus();
					this.hideNewRecordPanel(true);
				}
				break;
*/
			case "recordFilterSearchForm":
				if (anEvent.key().code == 13) {
//MochiKit.Logging.logDebug("SEARCH");
					this.filterCardsWithName(YAHOO.ext.Element.get('recordFilterSearchValue').dom.value);
					anEvent.event().stopPropagation();
					YAHOO.ext.Element.get('recordFilterSearchValue').focus();
				} else if (anEvent.key().code == 27) {
					this.hideRecordFilterSearchPanel(true);
					this.showRecordFilterAllPanel();
				}
				break;
		}

	},

	//-------------------------------------------------------------------------

	'renderRecordListBlockHeader': function(){
		var recordListBlockHeaderElement;
		
		recordListBlockHeaderElement = YAHOO.ext.Element.get('recordListBlockHeader');
		recordListBlockHeaderElement.update("");
		Clipperz.YUI.DomHelper.append(recordListBlockHeaderElement.dom, 		
			{tag:'table', cls:'recordListBlockHeaderTABLE', border:'0', cellspacing:'0', cellpadding:'0', children:[
				{tag:'tbody', children:[
					{tag:'tr', children:[
						{tag:'td', /*width:'50%',*/ cls:'recordBlockTitleTD', children:[
							{tag:'h3', id:'recordBlockTitle', htmlString:Clipperz.PM.Strings['mainPanelRecordsBlockLabel']}
						]},
						{tag:'td', align:'right', children:[
							{tag:'table', id:'recordListButtonsTABLE', border:'0', cellspacing:'0', cellpadding:'0', children:[
								{tag:'tbody', children:[
									{tag:'tr', children:[
										{tag:'td', cls:'recordButtonTD', align:'right', children:[
											{tag:'div', cls:'recordButton', id:'recordListAddRecordButton'}
										]},
										{tag:'td', cls:'recordButtonTD', align:'left', children:[
											{tag:'div', cls:'recordButton', id:'recordListRemoveRecordButton'}
										]}
									]}
								]}
							]}
						]},
						{tag:'td', width:'15', html:"&nbsp;"}
					]}
				]}
			]}
		);
		
		this.setAddNewRecordButton(new YAHOO.ext.Button('recordListAddRecordButton', {text:Clipperz.PM.Strings['mainPanelAddRecordButtonLabel'], handler:this.addNewRecord, scope:this}));
//		this.setAddNewRecordButton(new YAHOO.ext.Button('recordListAddRecordButton', {text:Clipperz.PM.Strings['mainPanelAddRecordButtonLabel'], handler:this.showNewRecordPanel, scope:this}));
		this.setDeleteRecordButton(new YAHOO.ext.Button('recordListRemoveRecordButton', {text:Clipperz.PM.Strings['mainPanelRemoveRecordButtonLabel'], handler:this.removeSelectedRecord, scope:this}));


		if ((Clipperz.PM.Proxy.defaultProxy.isReadOnly()) || (this.selectedRecord() == null)) {
			this.deleteRecordButton().disable();
		}
		
		if (Clipperz.PM.Proxy.defaultProxy.isReadOnly()) {
			this.addNewRecordButton().disable();
		}

	},

	//-------------------------------------------------------------------------

	'renderRecordListFilterHeader': function(){
		var recordListFilterHeaderElement;
		
		recordListFilterHeaderElement = YAHOO.ext.Element.get('recordListFilterHeader');
		recordListFilterHeaderElement.update("");
		Clipperz.YUI.DomHelper.append(recordListFilterHeaderElement.dom, 		
			{tag:'div', id:'recordFiltersDIV', children:[
				{tag:'div', id:'recordFiltersTableWrapper', children:[
					{tag:'table', id:'recordFiltersTABLE', border:'0', cellspacing:'0', cellpadding:'0', children:[
						{tag:'tbody', children:[
							{tag:'tr', children:[
								{tag:'td', id:'recordFilterAllTD', children:[{tag:'div', children:[{tag:'a', id:'recordFilterAll', htmlString:Clipperz.PM.Strings['mainPanelRecordFilterBlockAllLabel']}]}]},
								{tag:'td', id:'recordFilterTagsTD', children:[{tag:'div', children:[{tag:'a', id:'recordFilterTags', htmlString:Clipperz.PM.Strings['mainPanelRecordFilterBlockTagsLabel']}]}]},
								{tag:'td', id:'recordFilterSearchTD', children:[{tag:'div', children:[{tag:'a', id:'recordFilterSearch', htmlString:Clipperz.PM.Strings['mainPanelRecordFilterBlockSearchLabel']}]}]}
							]}
						]}
					]}
				]},
				{tag:'div', id:'recordFiltersTagsPanel'},
				{tag:'div', id:'recordFiltersSearchPanel', children:[{tag:'div', id:'recordFiltersSearchInnerPanel', children:[{tag:'div', id:'recordFiltersSearchInnerInnerPanel', children:[
					{tag:'form', id:'recordFilterSearchForm', children:[
						{tag:'input', type:'text', name:'search', id:'recordFilterSearchValue'}
					]}
				]}]}]}
			]}
		);
		
///		YAHOO.ext.Element.get('recordFiltersSearchPanel').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.showRecordFilterAllPanel(false);

		MochiKit.Signal.connect('recordFilterSearchForm', 'onkeydown', this, 'onkeydown');
		MochiKit.Signal.connect('recordFilterSearchForm', 'onsubmit', this, 'onkeydown');
		
		MochiKit.Signal.connect('recordFilterAll', 'onclick', this, 'showRecordFilterAllPanel');
		MochiKit.Signal.connect('recordFilterTags', 'onclick', this, 'showRecordFilterTagsPanel');
		MochiKit.Signal.connect('recordFilterSearch', 'onclick', this, 'showRecordFilterSearchPanel');
	},

	//-------------------------------------------------------------------------

	'showRecordFilterAllPanel': function(shouldSlide) {
		this.hideRecordFilterTagsPanel(shouldSlide);
		this.hideRecordFilterSearchPanel(shouldSlide);
		YAHOO.ext.Element.get('recordFilterAllTD').addClass('selectedTab');
	},
	
	'hideRecordFilterAllPanel': function(shouldSlide) {
		YAHOO.ext.Element.get('recordFilterAllTD').removeClass('selectedTab');
	},
	
	//-------------------------------------------------------------------------

	'showRecordFilterTagsPanel': function(shouldSlide) {
		this.hideRecordFilterAllPanel(shouldSlide);
		this.hideRecordFilterSearchPanel(shouldSlide);
		YAHOO.ext.Element.get('recordFilterTagsTD').addClass('selectedTab');
	},

	'hideRecordFilterTagsPanel': function(shouldSlide) {
		YAHOO.ext.Element.get('recordFilterTagsTD').removeClass('selectedTab');
	},

	//-------------------------------------------------------------------------

	'showRecordFilterSearchPanel': function(shouldSlide) {
		var searchPanelActor;
		
		this.hideRecordFilterAllPanel(shouldSlide);
		this.hideRecordFilterTagsPanel(shouldSlide);
		YAHOO.ext.Element.get('recordFilterSearchTD').addClass('selectedTab');
		YAHOO.ext.Element.get('recordFilterSearchValue').dom.value = "";
		
		searchPanelActor = new YAHOO.ext.Actor('recordFiltersSearchPanel');

		searchPanelActor.startCapture(true);
		searchPanelActor.slideShow('top', 54);
		searchPanelActor.play(MochiKit.Base.bind(function() {
			YAHOO.ext.Element.get('recordFilterSearchValue').focus();
		}, this));
	},

	'hideRecordFilterSearchPanel': function(shouldSlide) {
		var searchPanelActor;
		var	callback;
		
		YAHOO.ext.Element.get('recordFilterSearchTD').removeClass('selectedTab');

		searchPanelActor = new YAHOO.ext.Actor('recordFiltersSearchPanel');

		searchPanelActor.startCapture(true)
		if (shouldSlide === false) {
			searchPanelActor.hide();
			searchPanelActor.slideHide('top');
			searchPanelActor.show();
		} else {
			searchPanelActor.slideHide('top');
		}
		
		callback = MochiKit.Base.bind(function() {
		}, this);
		
		searchPanelActor.play(callback);
	},

	//-------------------------------------------------------------------------

	'filterCardsWithName': function(aValue) {
MochiKit.Logging.logDebug(">>> filterCardsWithName: " + aValue);
		
MochiKit.Logging.logDebug("<<< filterCardsWithName");
	},

	'accountLockedHandler': function() {
		this.setSelectedRecord(null);
	},
	
	//-------------------------------------------------------------------------

	'switchLanguageHandler': function() {
		YAHOO.ext.Element.get('directLoginTitle').update(Clipperz.PM.Strings['mainPanelDirectLoginBlockLabel']);
		YAHOO.ext.Element.get('directLoginsDescription').update("");
		MochiKit.Iter.forEach(Clipperz.PM.Strings['mainPanelDirectLoginBlockDescriptionConfig'], function(aConfigItem) {
			Clipperz.YUI.DomHelper.append(YAHOO.ext.Element.get('directLoginsDescription').dom, aConfigItem);
		});
		YAHOO.ext.Element.get('recordBlockTitle').update(Clipperz.PM.Strings['mainPanelRecordsBlockLabel']);
		this.renderRecordListBlockHeader();
//		this.renderRecordListFilterHeader();

//		YAHOO.ext.Element.get('newRecordPanelTitleH2').update(Clipperz.PM.Strings['mainPanelNewRecordPanelTitle']);
//		YAHOO.ext.Element.get('newRecordPanelTitleLabel').update(Clipperz.PM.Strings['mainPanelNewRecordPanelRecordTitleLabel']);
//		YAHOO.ext.Element.get('newRecordPanelConfigLabel').update("");
//		MochiKit.Iter.forEach(Clipperz.PM.Strings['mainPanelNewRecordPanelRecordConfigConfig'], function(aConfigItem) {
//			Clipperz.YUI.DomHelper.append(YAHOO.ext.Element.get('newRecordPanelConfigLabel').dom, aConfigItem);
//		});
//		this.newRecordButton().setText(Clipperz.PM.Strings['mainPanelNewRecordPanelCreateButtonLabel']);
//		this.newRecordCancelButton().setText(Clipperz.PM.Strings['mainPanelNewRecordPanelCancelButtonLabel']);
		
		this.recordDetailComponent().render();
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});
