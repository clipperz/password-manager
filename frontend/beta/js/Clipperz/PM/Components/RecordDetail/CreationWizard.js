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

Clipperz.PM.Components.RecordDetail.CreationWizard = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.RecordDetail.CreationWizard.superclass.constructor.call(this, anElement, args);

	this._mainComponent = args.mainComponent;
	this._previouslySelectedRecord = args.previouslySelectedRecord;
//MochiKit.Logging.logDebug("--- new CreationWizard - previouslySelectedRecord: " + args.previouslySelectedRecord);
	this._createButton_header = null;
	this._createButton_footer = null;

	this._cancelButton_header = null;
	this._cancelButton_footer = null;
	
	this.render();
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.CreationWizard, Clipperz.PM.Components.BaseComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.CreationWizard component";
	},

	//-------------------------------------------------------------------------

	'previouslySelectedRecord': function() {
		return this._previouslySelectedRecord;
	},
	
	//-------------------------------------------------------------------------

	'render': function() {
		var	templateListElement;
		var	templates;
		
		this.element().update("");
		
		Clipperz.YUI.DomHelper.append(this.element().dom,
			{tag:'form', cls:'recordDataFORM', id:this.getId('form'), children:[
				{tag:'div', id:'recordDetailDataBox', cls:'recordDetailDataBox', children:[
					{tag:'div', id:this.getId('wizardBox'), cls:'recordCreationWizard', children:[
						{tag:'div', id:this.getId('recordCreationWizardTitleBox'), cls:'recordCreationWizardTitleBox', htmlString:Clipperz.PM.Strings['newRecordWizardTitleBox']},
						{tag:'ul', id:this.getId('templateList'), cls:'radioList'}
					]}
				]}
			]}
		);

		Clipperz.YUI.DomHelper.append(this.getDom('recordCreationWizardTitleBox'), {tag:'div', cls:'newRecordWizardHeader', children:[
			{tag:'table', width:'100%', cellpadding:'5', children:[
				{tag:'tbody', children:[
					{tag:'tr', children:[
						{tag:'td', width:'49%', align:'right', children:[
							{tag:'div', id:this.getId('cancelButton_header')}
						]},
						{tag:'td', width:'10', html:'&nbsp;'},
						{tag:'td', width:'49%', align:'left', children:[
							{tag:'div', id:this.getId('createButton_header')}
						]}
					]}
				]}
			]}
		]});

		templateListElement = this.getElement('templateList');
		templates = Clipperz.PM.Strings['recordTemplates'];
		MochiKit.Iter.forEach(MochiKit.Base.keys(templates), MochiKit.Base.bind(function(aTemplateKey) {
			Clipperz.YUI.DomHelper.append(templateListElement.dom, {tag:'li', children:[
				{tag:'table', children:[
					{tag:'tbody', children:[
						{tag:'tr', children:[
							{tag:'td', valign:'top', children:[
								{tag:'input', id:this.getId(aTemplateKey+"_radio"), type:'radio', name:'recordTemplate', value:"aTemplateKey"}
							]},
							{tag:'td', valign:'top', children:[
								{tag:'h4', id:this.getId(aTemplateKey+"_title"), html:templates[aTemplateKey]['title']},
								{tag:'div', cls:'templateDescription', htmlString:templates[aTemplateKey]['description']}
							]}
						]}
					]}
				]}
			]});
			this.getElement(aTemplateKey+"_radio").dom.value = aTemplateKey;
			MochiKit.Signal.connect(this.getDom(aTemplateKey+"_title"), 'onclick', MochiKit.Base.partial(function(aRadioButton) {aRadioButton.click();}, this.getDom(aTemplateKey+"_radio")));
		}, this));

		Clipperz.YUI.DomHelper.append(templateListElement.dom, {tag:'li', children:[
			{tag:'table', children:[
				{tag:'tbody', children:[
					{tag:'tr', children:[
						{tag:'td', valign:'top', children:[
							{tag:'input', type:'radio', name:'recordTemplate', id:this.getId('bookmarkletRadioButton'), value:'BookmarkletConfigurationTemplate'}
						]},
						{tag:'td', valign:'top', children:[
							{tag:'h4', htmlString:Clipperz.PM.Strings['newRecordWizardBookmarkletConfigurationTitle']},
							{tag:'div', cls:'templateDescription', htmlString:Clipperz.PM.Strings['newRecordWizardBookmarkletConfigurationDescription']},
							{tag:'div', cls:'bookmarkletConfiguration', children:[
//								{tag:'span', htmlString:Clipperz.PM.Strings['newRecordWizardBookmarkletConfigurationLabel']},
								{tag:'div', htmlString:Clipperz.PM.Strings['recordDetailNewDirectLoginDescription']},
								{tag:'textarea', id:this.getId('bookmarkletConfiguration')}
							]}
						]}
					]}
				]}
			]}
		]});

		Clipperz.YUI.DomHelper.append(this.getDom('wizardBox'), {tag:'div', cls:'newRecordWizardFooter', children:[
			{tag:'table', width:'100%', cellpadding:'5', children:[
				{tag:'tbody', children:[
					{tag:'tr', children:[
						{tag:'td', width:'49%', align:'right', children:[
							{tag:'div', id:this.getId('cancelButton_footer')}
						]},
						{tag:'td', width:'10', html:'&nbsp;'},
						{tag:'td', width:'49%', align:'left', children:[
							{tag:'div', id:this.getId('createButton_footer')}
						]}
					]}
				]}
			]}
		]});
		
		this.setCreateButton_header(new YAHOO.ext.Button(this.getDom('createButton_header'), {text:Clipperz.PM.Strings['newRecordWizardCreateButtonLabel'], handler:this.createRecord, scope:this}));
		this.setCreateButton_footer(new YAHOO.ext.Button(this.getDom('createButton_footer'), {text:Clipperz.PM.Strings['newRecordWizardCreateButtonLabel'], handler:this.createRecord, scope:this}));

		this.setCancelButton_header(new YAHOO.ext.Button(this.getDom('cancelButton_header'), {text:Clipperz.PM.Strings['newRecordWizardCancelButtonLabel'], handler:this.exitWizard, scope:this}));
		this.setCancelButton_footer(new YAHOO.ext.Button(this.getDom('cancelButton_footer'), {text:Clipperz.PM.Strings['newRecordWizardCancelButtonLabel'], handler:this.exitWizard, scope:this}));

		this.createButton_header().disable();
		this.createButton_footer().disable();
		
		MochiKit.Iter.forEach(this.getElement('form').getChildrenByTagName('input'), MochiKit.Base.bind(function(anInput) {
//			MochiKit.Signal.connect(anInput.dom, 'onchange', this, 'enableCreateButton');
			MochiKit.Signal.connect(anInput.dom, 'onclick', this, 'enableCreateButton');	//	for Safari
		},this));

		MochiKit.Signal.connect(this.getDom('bookmarkletConfiguration'), 'onkeyup', this, 'enableCreateButton');
		MochiKit.Signal.connect(this.getDom('bookmarkletConfiguration'), 'onkeydown', this, 'enableCreateButton');	//	for Safari
	},

	//-------------------------------------------------------------------------

	'createButton_header': function() {
		return this._createButton_header;
	},
	
	'setCreateButton_header': function(aValue) {
		this._createButton_header = aValue;
	},

	//.........................................................................

	'createButton_footer': function() {
		return this._createButton_footer;
	},
	
	'setCreateButton_footer': function(aValue) {
		this._createButton_footer = aValue;
	},


	//-------------------------------------------------------------------------

	'cancelButton_header': function() {
		return this._cancelButton_header;
	},
	
	'setCancelButton_header': function(aValue) {
		this._cancelButton_header = aValue;
	},

	//.........................................................................
	
	'cancelButton_footer': function() {
		return this._cancelButton_footer;
	},
	
	'setCancelButton_footer': function(aValue) {
		this._cancelButton_footer = aValue;
	},

	//-------------------------------------------------------------------------

	'enableCreateButton': function(anEvent, skipKeyDownCheck) {
//MochiKit.Logging.logDebug(">>> CreationWizard.enableCreateButton (" + anEvent.type() + ")");
		if ((anEvent.type() == "keydown") && (skipKeyDownCheck != true)) {
//MochiKit.Logging.logDebug("--- CreationWizard.enableCreateButton - handling 'keydown' event with a postponed execution of the check");
			MochiKit.Async.callLater(0.3, MochiKit.Base.method(this, 'enableCreateButton', anEvent, true));
		} else {
			var shouldEnableCreateButton;
			var isBookmarkletConfigurationEmpty;

//MochiKit.Logging.logDebug("--- CreationWizard.enableCreateButton - common execution");

			shouldEnableCreateButton = true;
			
			isBookmarkletConfigurationEmpty = !/[^ \n]/.test(this.getDom('bookmarkletConfiguration').value);
//MochiKit.Logging.logDebug("--- CreationWizard.enableCreateButton - isBookmarkletConfigurationEmpty: " + isBookmarkletConfigurationEmpty);

			if ((anEvent.src() == this.getDom('bookmarkletConfiguration')) && !isBookmarkletConfigurationEmpty) {
				this.getDom('bookmarkletRadioButton').checked = true;
			}

			if ((this.getDom('bookmarkletRadioButton').checked) && isBookmarkletConfigurationEmpty) {
				shouldEnableCreateButton = false;
			}
		
			if (shouldEnableCreateButton) {
//MochiKit.Logging.logDebug("--- CreationWizard.enableCreateButton - enabling button");
				this.createButton_header().enable();
				this.createButton_footer().enable();
			} else {
//MochiKit.Logging.logDebug("--- CreationWizard.enableCreateButton - disabling button");
				this.createButton_header().disable();
				this.createButton_footer().disable();
			}
		}
//MochiKit.Logging.logDebug("<<< CreationWizard.enableCreateButton");
	},
	
	//-------------------------------------------------------------------------
	
	'createRecord': function() {
		var	selectedTemplateKey;
		var	newRecord;
		
		selectedTemplateKey = MochiKit.Base.filter(function(aCheckBoxElement) {
			return aCheckBoxElement.dom.checked;
		},this.getElement('form').getChildrenByTagName('input'))[0].dom.value;

//MochiKit.Logging.logDebug("--- CreationWizard.createRecord - selectedTemplateKey: " + selectedTemplateKey);
		if (selectedTemplateKey == 'BookmarkletConfigurationTemplate') {
			var bookmarkletConfiguration;

			this.mainComponent().exitModalView();
			bookmarkletConfiguration = Clipperz.PM.BookmarkletProcessor.checkBookmarkletConfiguration(this.getDom('bookmarkletConfiguration').value, this.getDom('createButton'), MochiKit.Base.method(this.mainComponent(), 'enterModalView'));
			this.mainComponent().enterModalView();
			newRecord = Clipperz.PM.BookmarkletProcessor.createRecordFromBookmarkletConfiguration(this.mainComponent().user(), bookmarkletConfiguration);
		} else {
			var	fieldsConfigurations;

			newRecord = this.mainComponent().user().addNewRecord();
			newRecord.setLabel(Clipperz.PM.Strings['recordTemplates'][selectedTemplateKey]['title']);
			
			fieldsConfigurations = Clipperz.PM.Strings['recordTemplates'][selectedTemplateKey]['fields'];
		
			MochiKit.Iter.forEach(fieldsConfigurations, MochiKit.Base.partial(function(aRecord, aFieldConfiguration) {
				var newField;
			
				newField = new Clipperz.PM.DataModel.RecordField({recordVersion:aRecord.currentVersion()});
				newField.setLabel(aFieldConfiguration['label']);
				newField.setType(aFieldConfiguration['type']);
				aRecord.currentVersion().addField(newField);
			}, newRecord));
		}
		
		this.mainComponent().exitWizard(newRecord, true);
	},

	//-------------------------------------------------------------------------

	'exitWizard': function() {
//MochiKit.Logging.logDebug(">>> CreationWizard.exitWizard - " + this.previouslySelectedRecord());
		this.mainComponent().exitWizard(this.previouslySelectedRecord());
//MochiKit.Logging.logDebug("<<< CreationWizard.exitWizard");
	},

	//-------------------------------------------------------------------------
	
	'mainComponent': function() {
		return this._mainComponent;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

