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
if (typeof(Clipperz.PM.Components.RecordDetail) == 'undefined') { Clipperz.PM.Components.RecordDetail = {}; }

//#############################################################################

Clipperz.PM.Components.RecordDetail.MainComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.RecordDetail.MainComponent.superclass.constructor.call(this, anElement, args);

//	this._element = args.element;
	this._user = args.user;
	this._editMode = args.editMode || 'VIEW';	//	[ 'VIEW' | 'EDIT' ]
	this._mainPanel = args.mainPanel;
	
	this._record = null;
	this._editComponents = [];
	this._addFieldButton = null;

	this._enableSaveButton = true;
	this._shouldShowLoginInfo = (Clipperz.PM.Proxy.defaultProxy.isReadOnly() ? false : true);
	
//	this._mainLayoutManager = null;
//	this._layoutRegion = null;
	
	Clipperz.NotificationCenter.register(null, 'loadingRecordData', this, 'render');
	Clipperz.NotificationCenter.register(null, 'decryptingRecordData', this, 'render');
	Clipperz.NotificationCenter.register(null, 'loadingRecordVersionData', this, 'render');
	Clipperz.NotificationCenter.register(null, 'decryptingRecordVersionData', this, 'render');
	Clipperz.NotificationCenter.register(null, 'setupDone', this, 'render');
	Clipperz.NotificationCenter.register(null, 'switchLanguage', this, 'render');
	
	this.render();
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.MainComponent, Clipperz.PM.Components.BaseComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.MainComponent component";
	},

	//-------------------------------------------------------------------------

	'editMode': function() {
		return this._editMode;
	},

	'setEditMode': function(aValue, aButtonElement, shouldSkipComponentSynchronization) {
//MochiKit.Logging.logDebug(">>> MainComponent.setEditingMode");
		this.scrollToTop();
		
		if (aValue == 'VIEW') {
			if (shouldSkipComponentSynchronization == true) {
				this.exitModalView();
			} else {
				this.synchronizeComponentValues();
				if (this.record().hasPendingChanges()) {
					if (this.record().isBrandNew()) {
						this.record().removeEmptyFields();
					}
					this.saveCurrentRecordChanges(aButtonElement);
				} else {
					if (this.record().isBrandNew()) {
						this.record().user().removeRecord(this.record());
					}
					this.exitModalView();
				}
			}
		} else if (aValue == 'EDIT') {
			this.enterModalView();
		} else {
			//	????
		}

		this._editMode = aValue;
		this.render();
	},
	
	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},

	//-------------------------------------------------------------------------
	
	'mainPanel': function() {
		return this._mainPanel;
	},
	
	//-------------------------------------------------------------------------

	'render': function() {
//MochiKit.Logging.logDebug(">>> RecordDetail.MainComponent.render");
		this.setEnableSaveButton(true);
		this.element().update("");

		if (this.record() == null) {
			if (MochiKit.Base.keys(this.user().records()).length == 0) {
				this.renderWithNoRecordAtAll();
			} else {
				this.renderWithNoSelectedRecord();
			}
		} else {
			this.renderWithSelectedRecord();
		}
//MochiKit.Logging.logDebug("<<< RecordDetail.MainComponent.render");
	},

	//-------------------------------------------------------------------------

	'renderWithNoRecordAtAll': function() {
//MochiKit.Logging.logDebug(">>> RecordDetail.MainComponent.renderWithNoRecordAtAll");
		Clipperz.YUI.DomHelper.append(this.element().dom,
			{tag:'form', cls:'noRecordAtAllFORM', children:[
				{tag:'div', cls:'recordTitleBlock', children:[
					{tag:'h2', id:'recordTitle', htmlString:Clipperz.PM.Strings['recordDetailNoRecordAtAllTitle']}
				]},
				{tag:'table', border:'0', cellspacing:'0', cellpadding:'0', children:[
					{tag:'tbody', children:[
						{tag:'tr', children:[
							{tag:'td', colspan:'5', children:[
								{tag:'div', cls:'recordDetailDescriptionBox', htmlString:Clipperz.PM.Strings['recordDetailNoRecordAtAllDescription']}
							]}
						]}
					]}
				]}
			]}
		);
//MochiKit.Logging.logDebug("<<< RecordDetail.MainComponent.renderWithNoRecordAtAll");
	},

	//-------------------------------------------------------------------------
	
	'renderWithNoSelectedRecord': function() {
//MochiKit.Logging.logDebug(">>> RecordDetail.MainComponent.renderWithNoSelectedRecord");
		Clipperz.YUI.DomHelper.append(this.element().dom,
			{tag:'form', cls:'noRecordSelectedFORM', children:[
				{tag:'div', cls:'recordTitleBlock', children:[
					{tag:'h2', id:'recordTitle', htmlString:Clipperz.PM.Strings['recordDetailNoRecordSelectedTitle']}
				]},
				{tag:'table', border:'0', cellspacing:'0', cellpadding:'0', children:[
					{tag:'tbody', children:[
						{tag:'tr', children:[
							{tag:'td', colspan:'5', children:[
								{tag:'div', cls:'recordDetailDescriptionBox', htmlString:Clipperz.PM.Strings['recordDetailNoRecordSelectedDescription']}
							]}
						]},
						{tag:'tr', colspan:'5', children:[
							{tag:'td', colspan:'5', children:this.loginInfo()}
						]}
					]}
				]}
			]}
		);
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithNoSelectedRecord - 1");

		if (MochiKit.DOM.getElement('fullLoginHistoryLink') != null) {
			MochiKit.Signal.connect('fullLoginHistoryLink', 'onclick', this, 'showLoginHistoryPanel');
		}
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithNoSelectedRecord - 2");
		
		if (MochiKit.DOM.getElement('offlineCopyDownloadWarningLink') != null) {
			MochiKit.Signal.connect('offlineCopyDownloadWarningLink', 'onclick', this, 'showDownloadOfflineCopyPanel');
		}
//MochiKit.Logging.logDebug("<<< RecordDetail.MainComponent.renderWithNoSelectedRecord");
	},

	//-------------------------------------------------------------------------

	'renderWithSelectedRecord': function() {
//MochiKit.Logging.logDebug(">>> RecordDetail.MainComponent.renderWithSelectedRecord");
		if (this.record().shouldLoadData() === true) {
//			this.renderWithSelectedRecordLoading();
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecord - 1.1");
			this.renderWhileProcessingWithMessage(Clipperz.PM.Strings['recordDetailLoadingRecordMessage']);
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecord - 1.2");
		} else if (this.record().shouldDecryptData() === true) {
//			this.renderWithSelectedRecordDecrypting();
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecord - 2.1");
			this.renderWhileProcessingWithMessage(Clipperz.PM.Strings['recordDetailDecryptingRecordMessage']);
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecord - 2.2");
		} else if (this.record().currentVersion().shouldLoadData() === true) {
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecord - 3.1");
			this.renderWhileProcessingWithMessage(Clipperz.PM.Strings['recordDetailLoadingRecordVersionMessage']);
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecord - 3.2");
		} else if (this.record().currentVersion().shouldDecryptData() === true) {
//			this.renderWithSelectedRecordCurrentVersionDecrypting();
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecord - 4.1");
			this.renderWhileProcessingWithMessage(Clipperz.PM.Strings['recordDetailDecryptingRecordVersionMessage']);
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecord - 4.2");
		} else {
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecord - 5.1");
			this.renderWithSelectedRecordData();
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecord - 5.2");
		}
//MochiKit.Logging.logDebug("<<< RecordDetail.MainComponent.renderWithSelectedRecord");
	},

	//.........................................................................

	'renderWhileProcessingWithMessage': function(aMessage) {
//MochiKit.Logging.logDebug(">>> RecordDetail.MainComponent.renderWhileProcessingWithMessage");
		Clipperz.YUI.DomHelper.append(this.element().dom,
			{tag:'form', cls:'processingRecordFORM', children:[
				{tag:'div', cls:'recordTitleBlock', children:[
					{tag:'h2', id:'recordTitle', html:this.record().label()}
				]},
				{tag:'table', border:'0', cellspacing:'0', cellpadding:'0', children:[
					{tag:'tbody', children:[
						{tag:'tr', cls:'recordTR', children:[
							{tag:'td', colspan:'5', children:[
								{tag:'div', cls:'recordDetailDescriptionBox', children:[
									{tag:'h5', cls:'recordLoadingMessage', html:aMessage}
								]}
							]}
						]}
					]}
				]}
			]}
		);
//MochiKit.Logging.logDebug("<<< RecordDetail.MainComponent.renderWhileProcessingWithMessage");
	},

	//.........................................................................
/*
	'renderWithSelectedRecordLoading': function() {
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', cls:'', style:'border:1px solid red; padding: 20px;', children:[
			{tag:'div', cls:'Clipprez_RecordDetailTitle', children:[
				{tag:'h3', html:this.record().label()},
				{tag:'h3', html:"loading"}
			]}
		]});
	},

	//.........................................................................

	'renderWithSelectedRecordDecrypting': function() {
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', cls:'', style:'border:1px solid red; padding: 20px;', children:[
			{tag:'div', cls:'Clipprez_RecordDetailTitle', children:[
				{tag:'h3', html:this.record().label()},
				{tag:'h3', html:"decrypting ... "}
			]}
		]});
	},

	//.........................................................................

	'renderWithSelectedRecordCurrentVersionDecrypting': function() {
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', cls:'', style:'border:1px solid red; padding: 20px;', children:[
			{tag:'div', cls:'Clipprez_RecordDetailTitle', children:[
				{tag:'h3', html:this.record().label()},
				{tag:'h3', html:"decrypting version ... "}
			]}
		]});
	},
*/
	//-------------------------------------------------------------------------

	'renderWithErrorMessage': function(anErrorMessage) {
//MochiKit.Logging.logDebug(">>> RecordDetail.MainComponent.renderWithErrorMessage");
		this.element().update("");
		
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithErrorMessage - 1");
		Clipperz.YUI.DomHelper.append(this.element().dom,
			{tag:'form', cls:'errorMessageFORM', children:[
				{tag:'div', cls:'recordTitleBlock', children:[
					{tag:'h2', id:'recordTitle', html:this.record().label()}
				]},
				{tag:'table', border:'0', cellspacing:'0', cellpadding:'0', children:[
					{tag:'tbody', children:[
						{tag:'tr', cls:'recordTR', children:[
							{tag:'td', colspan:'5', children:[
								{tag:'div', cls:'recordDetailDescriptionBox loadingError', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['recordDetailLoadingErrorMessageTitle']},
									{tag:'p', html:anErrorMessage.message}
								]}
							]}
						]}
					]}
				]}
			]}
		);
//MochiKit.Logging.logDebug("<<< RecordDetail.MainComponent.renderWithErrorMessage");
	},

	//-------------------------------------------------------------------------

	'renderWithSelectedRecordData': function() {
		var	columns;
		
		this.resetEditComponents();

		columns = [
			{tag:'td', width:'25', html:'&#160'},
			{tag:'td', width:'25%', htmlString:Clipperz.PM.Strings['recordDetailLabelFieldColumnLabel']},
			{tag:'td', width:'3', html:'&#160'},
			{tag:'td', /*width:'80%',*/ htmlString:Clipperz.PM.Strings['recordDetailDataFieldColumnLabel']}
		];

		if (this.editMode() == 'EDIT') {
			columns.push({tag:'td', /*width:'55',*/ htmlString:Clipperz.PM.Strings['recordDetailTypeFieldColumnLabel']})
		}
		
//MochiKit.Logging.logDebug(">>> RecordDetail.MainComponent.renderWithSelectedRecordData");
		Clipperz.YUI.DomHelper.append(this.element().dom,
			{tag:'form', cls:'recordDataFORM', children:[
				{tag:'div', cls:'recordTitleBlock', id:this.getId('title')},
				{tag:'div', id:'recordDetailDataBox', cls:'recordDetailDataBox', children:[

{tag:'table', width:'100%', border:'0', cellspacing:'0', cellpadding:'0', children:[
	{tag:'tbody', children:[
		{tag:'tr', children:[
			{tag:'td', width:'5', html:"&nbsp;"},
			{tag:'td', children:[

					{tag:'table', cls:'recordDetailDataBoxTABLE', border:'0', cellspacing:'0', cellpadding:'0', children:[
						{tag:'tbody', id:this.getId('tbody'), children:[
							{tag:'tr', /*cls:'recordNoteTR',*/ children:[
								{tag:'td', colspan:'5', id:this.getId('notes')}
							]},
							{tag:'tr', cls:'recordFieldsTR', children:columns /*	[
								{tag:'td', width:'25', html:'&#160'},
								{tag:'td', width:'25%', htmlString:Clipperz.PM.Strings['recordDetailLabelFieldColumnLabel']},
								{tag:'td', width:'3', html:'&#160'},
								{tag:'td', / *width:'80%',* / htmlString:Clipperz.PM.Strings['recordDetailDataFieldColumnLabel']},
								{tag:'td', / *width:'55',* / htmlString:Clipperz.PM.Strings['recordDetailTypeFieldColumnLabel']}
							] */	}
						]}
					]},
					{tag:'div', cls:'addFieldButton', id:this.getId('addField'), children:[
						{tag:'div', id:this.getId('addFieldButton')}
					]},
					{tag:'div', id:this.getId('directLogins')},
					{tag:'div', id:this.getId('footer')}
				
			]}
		]}
	]}
]}

				]}
			]}
		);
		
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecordData - 1");

		new Clipperz.PM.Components.RecordDetail.TitleComponent(this.getElement('title'), {mainComponent:this});
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecordData - 2");
		new Clipperz.PM.Components.RecordDetail.NotesComponent(this.getElement('notes'), {mainComponent:this});
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecordData - 3");
		new Clipperz.PM.Components.RecordDetail.DirectLoginsComponent(this.getElement('directLogins'), {mainComponent:this});
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecordData - 4");
		new Clipperz.PM.Components.RecordDetail.HeaderComponent(this.getElement('footer'), {mainComponent:this});
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecordData - 5");
		MochiKit.Iter.forEach(MochiKit.Base.values(this.record().currentVersion().fields()), this.appendFieldComponent, this);
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecordData - 6");
		this.setAddFieldButton(new YAHOO.ext.Button(this.getDom('addFieldButton'), {text:Clipperz.PM.Strings['recordDetailAddFieldButtonLabel'], handler:this.addNewRecordField, scope:this}));
//MochiKit.Logging.logDebug("--- RecordDetail.MainComponent.renderWithSelectedRecordData - 7");

		this.update();

//MochiKit.Logging.logDebug("<<< RecordDetail.MainComponent.renderWithSelectedRecordData");
	},

	//-------------------------------------------------------------------------
	
	'editComponents': function() {
		return this._editComponents;
	},
	
	'resetEditComponents': function() {
		this._editComponents = [];
	},
	
	'addEditComponent': function(aValue) {
		this.editComponents().push(aValue);
	},
	
	'removeEditComponent': function(aValue) {
		Clipperz.Base.removeFromArray(this.editComponents(), aValue);
	},
	
	//-------------------------------------------------------------------------

	'record': function() {
		return this._record;
	},
	
	'setRecord': function(aValue) {
		var	result;
		
//MochiKit.Logging.logDebug(">>> MainComponent.setRecord")
		if (this._record != aValue) {
			var	deferredResult;
			
			deferredResult = new MochiKit.Async.Deferred();
			
			if ((this._record != null) && (this.editMode() == 'EDIT')) {
				this.synchronizeComponentValues();
				deferredResult.addCallback(MochiKit.Base.method(this._record, 'saveChanges'));
			}
			
			this._record = aValue;
			
			if (aValue != null) {
				this.setShouldShowLoginInfo(false);
				deferredResult.addCallback(MochiKit.Base.method(this._record, 'deferredData'));
			}
			deferredResult.addCallbacks(
				MochiKit.Base.method(this, 'render'),
				MochiKit.Base.method(this, 'renderWithErrorMessage')
			);
			deferredResult.callback();
			this.scrollToTop();
			
			result = deferredResult;
		} else {
			result = MochiKit.Async.success();
		}
//MochiKit.Logging.logDebug("<<< MainComponent.setRecord")
		
		return result;
	},

	//-------------------------------------------------------------------------

	'saveCurrentRecordChanges': function(aButtonElement) {
		var deferred;
		var currentNumberOfRecords;
		
		deferred = new MochiKit.Async.Deferred();
		deferred.addCallback(MochiKit.Base.method(Clipperz.PM.Components.MessageBox(), 'deferredShow'),
			{
				title:Clipperz.PM.Strings['recordDetailSavingChangesMessagePanelInitialTitle'],
				text:Clipperz.PM.Strings['recordDetailSavingChangesMessagePanelInitialText'],
				width:240,
				showProgressBar:true,
				showCloseButton:false,
				steps:6
			},
			aButtonElement.dom
		);
		deferred.addCallback(MochiKit.Base.method(this, 'exitModalView'));
		deferred.addCallback(MochiKit.Base.method(this.record(), 'saveChanges'));
		deferred.addCallback(Clipperz.NotificationCenter.deferredNotification, this.record(), 'recordUpdated');
		deferred.addCallback(function(res) {
			Clipperz.PM.Components.MessageBox().hide(YAHOO.ext.Element.get('main'));
			return res;
		});

		currentNumberOfRecords = MochiKit.Base.keys(this.user().records()).length;
		if ((this.record().isBrandNew()) && (this.user().preferences().shouldShowDonationPanel()) && (currentNumberOfRecords >= 5)) {
			deferred.addCallback(Clipperz.PM.showDonationSplashScreen, this.user(), 'recordListAddRecordButton');
		}

		deferred.callback();
	},
	
	//-------------------------------------------------------------------------

	'update': function(anEvent) {
		if (this.editMode() == 'EDIT') {
			this.updateEditMode();
		} else if (this.editMode() == 'VIEW') {
			this.updateViewMode();
		}

		MochiKit.Iter.forEach(this.editComponents(), MochiKit.Base.methodcaller('update'));
	},

	//-------------------------------------------------------------------------

	'updateViewMode': function() {
		this.addFieldButton().hide();
	},

	//-------------------------------------------------------------------------
	
	'updateEditMode': function() {
		this.addFieldButton().show();
	},
	
	//-------------------------------------------------------------------------

	'appendFieldComponent': function(aRecordField) {
//MochiKit.Logging.logDebug(">>> MainComponent.appendFieldComponent");
		new Clipperz.PM.Components.RecordDetail.FieldComponent(
			Clipperz.YUI.DomHelper.append(this.getDom('tbody'), {tag:'tr'}, true),
			{recordField:aRecordField, mainComponent:this}
		);
//MochiKit.Logging.logDebug("<<< MainComponent.appendFieldComponent");
	},
	
	//-------------------------------------------------------------------------

	'removeField': function(aFieldComponent) {
		var	recordField;
		
//MochiKit.Logging.logDebug(">>> MainComponent.removeField")		
		recordField = aFieldComponent.recordField();
		this.removeEditComponent(aFieldComponent);
		aFieldComponent.destroy();
		this.record().removeField(recordField);

		Clipperz.NotificationCenter.notify(this.record(), 'removedField');
//MochiKit.Logging.logDebug("<<< MainComponent.removeField")		
	},

	//-------------------------------------------------------------------------
	
	'synchronizeComponentValues': function() {
		MochiKit.Iter.forEach(this.editComponents(), MochiKit.Base.methodcaller('synchronizeComponentValues'));
	},

	//=========================================================================

	'addFieldButton': function() {
		return this._addFieldButton;
	},
	
	'setAddFieldButton': function(aValue) {
		this._addFieldButton = aValue;
	},
	
	'addNewRecordField': function() {
		var	newField;
		
		newField = this.record().addNewField();
		this.appendFieldComponent(newField);

		Clipperz.NotificationCenter.notify(this.record(), 'addNewRecordField');
	},
	
	//-------------------------------------------------------------------------
	
	'enterModalView': function() {
/*
		if (this.user().preferences().useSafeEditMode()) {
			var headerMaskElement;
			var verticalMaskElement;

			headerMaskElement = YAHOO.ext.Element.get('recordDetailEditModeHeaderMask');
			headerMaskElement.show().mask();

			verticalMaskElement = YAHOO.ext.Element.get('recordDetailEditModeVerticalMask');
			verticalMaskElement.show().mask();
		}
*/
		this.mainPanel().enterModalView();
	},
	
	//-------------------------------------------------------------------------

	'exitModalView': function() {
/*
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
*/
		this.mainPanel().exitModalView();
	},

	//-------------------------------------------------------------------------

	'enableSaveButton': function() {
		return this._enableSaveButton;
	},
	
	'setEnableSaveButton': function(aValue) {
		this._enableSaveButton = aValue;
	},
	
	//-------------------------------------------------------------------------

	'scrollToTop': function() {
		YAHOO.ext.Element.get('recordTitleTopBlock').scrollIntoView(document.body);
	},

	//-------------------------------------------------------------------------

	'loginInfo': function() {
		var	result;

		if (this.shouldShowLoginInfo() == true) {
//			 && (typeof(this.user().loginInfo()['latest']) != 'undefined')) {
			var	imageExtension;
			var currentConnectionText;
			var currentIP;
			var contentChildren;
			
			result = [];
			contentChildren = [];
			
			imageExtension = (Clipperz_IEisBroken == true) ? 'gif': 'png';

			contentChildren.push({tag:'h4', valign:'top', htmlString:Clipperz.PM.Strings['WELCOME_BACK']});

			currentIP = (this.user().loginInfo()['current']['ip'].match(/^\d{1,3}(.\d{1,3}){3}$/)) ? this.user().loginInfo()['current']['ip'] : Clipperz.PM.Strings['unknown_ip'];
			currentConnectionText = Clipperz.PM.Strings['currentConnectionText'];
			currentConnectionText = currentConnectionText.replace(/__ip__/, "<b>" + Clipperz.Base.sanitizeString(this.user().loginInfo()['current']['ip']) + "</b>");
			currentConnectionText = currentConnectionText.replace(/__country__/, "<b>" + Clipperz.PM.Strings['countries'][Clipperz.Base.sanitizeString(this.user().loginInfo()['current']['country'])] + "</b>");
			currentConnectionText = currentConnectionText.replace(/__browser__/, "<b>" + Clipperz.PM.Strings['browsers'][Clipperz.Base.sanitizeString(this.user().loginInfo()['current']['browser'])] + "</b>");
			currentConnectionText = currentConnectionText.replace(/__operatingSystem__/, "<b>" + Clipperz.PM.Strings['operatingSystems'][Clipperz.Base.sanitizeString(this.user().loginInfo()['current']['operatingSystem'])] + "</b>");

			contentChildren.push(
				{tag:'div', cls:'loginInfo_now', children:[
					{tag:'div', cls:'text', htmlString:currentConnectionText},
					{tag:'div', cls:'icons', children:[
						{tag:'img', title:Clipperz.PM.Strings['countries'][Clipperz.Base.sanitizeString(this.user().loginInfo()['current']['country'])], cls:'flag', src:Clipperz.PM.Strings['icons_baseUrl'] + "/flags/" + Clipperz.Base.sanitizeString(this.user().loginInfo()['current']['country']).toLowerCase() + "." +  imageExtension, width:'32', height:'32'},
						{tag:'img', title:Clipperz.PM.Strings['browsers'][Clipperz.Base.sanitizeString(this.user().loginInfo()['current']['browser'])], src:Clipperz.PM.Strings['icons_baseUrl'] + "/browsers/" + Clipperz.Base.sanitizeString(this.user().loginInfo()['current']['browser']).toLowerCase() + "." + imageExtension, width:'32', height:'32'},
						{tag:'img', title:Clipperz.PM.Strings['operatingSystems'][Clipperz.Base.sanitizeString(this.user().loginInfo()['current']['operatingSystem'])], src:Clipperz.PM.Strings['icons_baseUrl'] + "/operatingSystems/" + Clipperz.Base.sanitizeString(this.user().loginInfo()['current']['operatingSystem']).toLowerCase() + "." + imageExtension, width:'32', height:'32'}
					]}
				]}
			);
			
			if (typeof(this.user().loginInfo()['latest']) != 'undefined') {
				var latestLoginDate;
				var elapsedTimeDescription;
				var latestIP;
				var latestConnectionText;

				latestLoginDate = Clipperz.PM.Date.parseDateWithUTCFormat(Clipperz.Base.sanitizeString(this.user().loginInfo()['latest']['date']));
			
				elapsedTimeDescription = Clipperz.PM.Date.getElapsedTimeDescription(latestLoginDate);
				latestIP =  (this.user().loginInfo()['latest']['ip'].match(/^\d{1,3}(.\d{1,3}){3}$/)) ? this.user().loginInfo()['latest']['ip'] : Clipperz.PM.Strings['unknown_ip'];

				latestConnectionText = Clipperz.PM.Strings['latestConnectionText'];
				latestConnectionText = latestConnectionText.replace(/__elapsedTimeDescription__/, "<b>" + elapsedTimeDescription + "</b>");
				latestConnectionText = latestConnectionText.replace(/__time__/, Clipperz.PM.Date.formatDateWithTemplate(latestLoginDate, Clipperz.PM.Strings['fullDate_format']));
				latestConnectionText = latestConnectionText.replace(/__ip__/, "<b>" + Clipperz.Base.sanitizeString(this.user().loginInfo()['latest']['ip']) + "</b>");
				latestConnectionText = latestConnectionText.replace(/__country__/, "<b>" + Clipperz.PM.Strings['countries'][Clipperz.Base.sanitizeString(this.user().loginInfo()['latest']['country'])] + "</b>");
				latestConnectionText = latestConnectionText.replace(/__browser__/, "<b>" + Clipperz.PM.Strings['browsers'][Clipperz.Base.sanitizeString(this.user().loginInfo()['latest']['browser'])] + "</b>");
				latestConnectionText = latestConnectionText.replace(/__operatingSystem__/, "<b>" + Clipperz.PM.Strings['operatingSystems'][Clipperz.Base.sanitizeString(this.user().loginInfo()['latest']['operatingSystem'])] + "</b>");


				contentChildren.push(
					{tag:'div', cls:'loginInfo_latest', children:[
						{tag:'div', cls:'inner_header', html:'&nbsp;'},
						{tag:'div', cls:'content', children:[
							{tag:'div', cls:'text', htmlString:latestConnectionText},
							{tag:'div', cls:'icons', children:[
								{tag:'img', title:Clipperz.PM.Strings['countries'][Clipperz.Base.sanitizeString(this.user().loginInfo()['latest']['country'])], cls:'flag', src:Clipperz.PM.Strings['icons_baseUrl'] + "/flags/" + Clipperz.Base.sanitizeString(this.user().loginInfo()['latest']['country']).toLowerCase() + "." +  imageExtension, width:'32', height:'32'},
								{tag:'img', title:Clipperz.PM.Strings['browsers'][Clipperz.Base.sanitizeString(this.user().loginInfo()['latest']['browser'])], src:Clipperz.PM.Strings['icons_baseUrl'] + "/browsers/" + Clipperz.Base.sanitizeString(this.user().loginInfo()['latest']['browser']).toLowerCase() + "." + imageExtension, width:'32', height:'32'},
								{tag:'img', title:Clipperz.PM.Strings['operatingSystems'][Clipperz.Base.sanitizeString(this.user().loginInfo()['latest']['operatingSystem'])], src:Clipperz.PM.Strings['icons_baseUrl'] + "/operatingSystems/" + Clipperz.Base.sanitizeString(this.user().loginInfo()['latest']['operatingSystem']).toLowerCase() + "." + imageExtension, width:'32', height:'32'}
							]}
						]},
						{tag:'div', children:[
							{tag:'a', href:'#', id:'fullLoginHistoryLink', htmlString:Clipperz.PM.Strings['fullLoginHistoryLinkLabel']}
						]},
						{tag:'div', cls:'inner_footer', html:'&nbsp;'}
					]}
				);
			}
			
			contentChildren.push(
				{tag:'table', id:'shouldDownloadOfflineCopyWarningBox', children:[
					{tag:'tbody', width:'100%', children:[
						{tag:'tr', children:[
							{tag:'td', cls:'offlineCopyDownloadWarningIconTD', valign:'top', align:'center', width:'50', children:(this.user().shouldDownloadOfflineCopy() ? [{tag:'img', src:Clipperz.PM.Strings['icons_baseUrl'] + "/misc/offlineCopyWarning.png" , width:'32', height:'32'}]: [])},
							{tag:'td', children:[
								{tag:'div', cls:'offlineCopyDownloadWarning', htmlString:(this.user().shouldDownloadOfflineCopy() ? Clipperz.PM.Strings['offlineCopyDownloadWarning']: Clipperz.PM.Strings['offlineCopyDownloadOk'])}
							]}
						]}
					]}
				]}
			);


			result = [{tag:'div', id:'loginInfoWrapper', children:[{tag:'div', id:'loginInfo', children:[
				{tag:'div', cls:'header', html:'&nbsp;'},
				{tag:'div', cls:'content', children:contentChildren},
				{tag:'div', cls:'footer', html:'&nbsp;'}
			]}]}];

//			this.setShouldShowLoginInfo(false);
		} else {
			resut = [];
		}

		return result;
	},

	//-------------------------------------------------------------------------

	'shouldShowLoginInfo': function() {
		return this._shouldShowLoginInfo;
	},
	
	'setShouldShowLoginInfo': function(aValue) {
		this._shouldShowLoginInfo = aValue;
	},

	//-------------------------------------------------------------------------

	'showLoginHistoryPanel': function(anEvent) {
		anEvent.stop();
		
		Clipperz.NotificationCenter.notify(this, 'selectTab', 'mainTabPanel.accountTab', true);
		Clipperz.NotificationCenter.notify(this, 'selectTab', 'accountTabPanel.loginHistoryTab', true);
	},
	
	//-------------------------------------------------------------------------

	'showDownloadOfflineCopyPanel': function(anEvent) {
		anEvent.stop();
		
		Clipperz.NotificationCenter.notify(this, 'selectTab', 'mainTabPanel.dataTab', true);
		Clipperz.NotificationCenter.notify(this, 'selectTab', 'dataTabPanel.offlineCopyTab', true);
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

