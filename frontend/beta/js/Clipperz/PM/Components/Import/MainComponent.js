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

Clipperz.PM.Components.Import.MainComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Import.MainComponent.superclass.constructor.call(this, anElement, args);

	this._user = args.user;
	this._wizardComponent = null;

	this._backButton = null;
	this._nextButton = null;

	this._selectedComponent = null;
	
	this.render();

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Import.MainComponent, Clipperz.PM.Components.BaseComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Import.MainComponent component";
	},

	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},

	//-------------------------------------------------------------------------
	
	'wizardComponent': function() {
		return this._wizardComponent;
	},
	
	'setWizardComponent': function(aValue) {
		if (this._wizardComponent != null) {
			this._wizardComponent.remove();
		}
		
		if (aValue != null) {
			this.getElement('importCover').hide();
			this.getElement('importWizard').show();
		}
		this._wizardComponent = aValue;
	},

	'resetImportComponent': function() {
//MochiKit.Logging.logDebug(">>> resetImportComponent");
		this.setWizardComponent(null);
		this.getElement('wizardComponent').update("");

		this.getElement('importCover').show();
		this.getElement('importWizard').hide();
//MochiKit.Logging.logDebug("<<< resetImportComponent");
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
//MochiKit.Logging.logDebug(">>> Import.MainComponent.render");
		Clipperz.NotificationCenter.unregister(this);
		MochiKit.Signal.disconnectAllTo(this);

		this.element().update("");
		this.domHelper().append(this.element(), {tag:'div', id:this.getId('mainDiv'), children:[
			{tag:'div', id:this.getId('importCover'), children:[
				{tag:'h5', htmlString:Clipperz.PM.Strings['importTabTitle']},
				{tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['importTabDescription']},
				{tag:'div', cls:'importFormats', children:[
					{tag:'ul', cls:'radioList', children:[
						{tag:'li', children:[
							{tag:'table', children:[{tag:'tbody', children:[{tag:'tr', children:[
								{tag:'td', valign:'top', children:[
									{tag:'input', id:this.getId('CSV_radio'), type:'radio', name:'importFormat', value:'CSV'}
								]},
								{tag:'td', valign:'top', children:[
									{tag:'h4', id:this.getId('CSV_title'), htmlString:Clipperz.PM.Strings['importFormats']['CSV']['label']},
									{tag:'div', cls:'templateDescription', htmlString:Clipperz.PM.Strings['importFormats']['CSV']['description']}
								]}
							]}]}]}
						]},
						{tag:'li', children:[
							{tag:'table', children:[{tag:'tbody', children:[{tag:'tr', children:[
								{tag:'td', valign:'top', children:[
									{tag:'input', id:this.getId('Excel_radio'), type:'radio', name:'importFormat', value:'EXCEL'}
								]},
								{tag:'td', valign:'top', children:[
									{tag:'h4', id:this.getId('Excel_title'), htmlString:Clipperz.PM.Strings['importFormats']['Excel']['label']},
									{tag:'div', cls:'templateDescription', htmlString:Clipperz.PM.Strings['importFormats']['Excel']['description']}
								]}
							]}]}]}
						]},
						{tag:'li', children:[
							{tag:'table', children:[{tag:'tbody', children:[{tag:'tr', children:[
								{tag:'td', valign:'top', children:[
									{tag:'input', id:this.getId('KeePass_radio'), type:'radio', name:'importFormat', value:'KEEPASS'}
								]},
								{tag:'td', valign:'top', children:[
									{tag:'h4', id:this.getId('KeePass_title'), htmlString:Clipperz.PM.Strings['importFormats']['KeePass']['label']},
									{tag:'div', cls:'templateDescription', htmlString:Clipperz.PM.Strings['importFormats']['KeePass']['description']}
								]}
							]}]}]}
						]},
						{tag:'li', children:[
							{tag:'table', children:[{tag:'tbody', children:[{tag:'tr', children:[
								{tag:'td', valign:'top', children:[
									{tag:'input', id:this.getId('Roboform_radio'), type:'radio', name:'importFormat', value:'ROBOFORM'}
								]},
								{tag:'td', valign:'top', children:[
									{tag:'h4', id:this.getId('Roboform_title'), htmlString:Clipperz.PM.Strings['importFormats']['Roboform']['label']},
									{tag:'div', cls:'templateDescription', htmlString:Clipperz.PM.Strings['importFormats']['Roboform']['description']}
								]}
							]}]}]}
						]},
						{tag:'li', children:[
							{tag:'table', children:[{tag:'tbody', children:[{tag:'tr', children:[
								{tag:'td', valign:'top', children:[
									{tag:'input', id:this.getId('PasswordPlus_radio'), type:'radio', name:'importFormat', value:'PASSWORD_PLUS'}
								]},
								{tag:'td', valign:'top', children:[
									{tag:'h4', id:this.getId('PasswordPlus_title'), htmlString:Clipperz.PM.Strings['importFormats']['PasswordPlus']['label']},
									{tag:'div', cls:'templateDescription', htmlString:Clipperz.PM.Strings['importFormats']['PasswordPlus']['description']}
								]}
							]}]}]}
						]},
						{tag:'li', children:[
							{tag:'table', children:[{tag:'tbody', children:[{tag:'tr', children:[
								{tag:'td', valign:'top', children:[
									{tag:'input', id:this.getId('ClipperzExport_radio'), type:'radio', name:'importFormat', value:'CLIPPERZ_EXPORT'}
								]},
								{tag:'td', valign:'top', children:[
									{tag:'h4', id:this.getId('ClipperzExport_title'), htmlString:Clipperz.PM.Strings['importFormats']['ClipperzExport']['label']},
									{tag:'div', cls:'templateDescription', htmlString:Clipperz.PM.Strings['importFormats']['ClipperzExport']['description']}
								]}
							]}]}]}
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
				]}
			]},
			{tag:'div', id:this.getId('importWizard'), children:[
				{tag:'form', id:this.getId('importWizardForm'), children:[
					{tag:'div', cls:'wizardComponent', id:this.getId('wizardComponent'), children:[]}
				]}
			]}
		]});

		this.setBackButton(new YAHOO.ext.Button(this.getDom('backActionButton'), {text:"back", handler:this.backAction, scope:this}));
		this.setNextButton(new YAHOO.ext.Button(this.getDom('nextActionButton'), {text:"next", handler:this.nextAction, scope:this}));

		this.backButton().disable();
		this.nextButton().disable();
		
		this.getElement('importCover').setVisibilityMode(YAHOO.ext.Element.DISPLAY).show();
		this.getElement('importWizard').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();

		if (Clipperz.PM.Proxy.defaultProxy.isReadOnly()) {
			this.getElement('mainDiv').addClass('read-only');

			this.getDom('ClipperzExport_radio').disabled = true;
			this.getDom('CSV_radio').disabled = true;
			this.getDom('Excel_radio').disabled = true;
			this.getDom('PasswordPlus_radio').disabled = true;
			this.getDom('Roboform_radio').disabled = true;
			this.getDom('KeePass_radio').disabled = true;
		} else {
			MochiKit.Signal.connect(this.getId('ClipperzExport_radio'), 'onclick', MochiKit.Base.method(this, 'selectComponent'));
			MochiKit.Signal.connect(this.getId('CSV_radio'), 'onclick', MochiKit.Base.method(this, 'selectComponent'));
			MochiKit.Signal.connect(this.getId('Excel_radio'), 'onclick', MochiKit.Base.method(this, 'selectComponent'));
			MochiKit.Signal.connect(this.getId('PasswordPlus_radio'), 'onclick', MochiKit.Base.method(this, 'selectComponent'));
			MochiKit.Signal.connect(this.getId('Roboform_radio'), 'onclick', MochiKit.Base.method(this, 'selectComponent'));
			MochiKit.Signal.connect(this.getId('KeePass_radio'), 'onclick', MochiKit.Base.method(this, 'selectComponent'));

			if (Clipperz_IEisBroken != true) {
				MochiKit.Signal.connect(this.getId('ClipperzExport_title'), 'onclick', this.getDom('ClipperzExport_radio'), 'click');
				MochiKit.Signal.connect(this.getId('CSV_title'), 'onclick', this.getDom('CSV_radio'), 'click');
				MochiKit.Signal.connect(this.getId('Excel_title'), 'onclick', this.getDom('Excel_radio'), 'click');
				MochiKit.Signal.connect(this.getId('PasswordPlus_title'), 'onclick', this.getDom('PasswordPlus_radio'), 'click');
				MochiKit.Signal.connect(this.getId('Roboform_title'), 'onclick', this.getDom('Roboform_radio'), 'click');
				MochiKit.Signal.connect(this.getId('KeePass_title'), 'onclick', this.getDom('KeePass_radio'), 'click');
			}

			Clipperz.NotificationCenter.register(null, 'importCompleted', this, 'resetImportComponent');
			Clipperz.NotificationCenter.register(null, 'importCancelled', this, 'resetImportComponent');
		}

//MochiKit.Logging.logDebug("<<< Import.MainComponent.render");
	},

	//-------------------------------------------------------------------------
/*
	'selectedFormat': function() {
		return this.getDom('importSelectionOptions').value;
	},
*/
	//-------------------------------------------------------------------------

	'updateSelectedImportWizardComponent': function(aComponent) {
		var newWizardComponent;

//MochiKit.Logging.logDebug(">>> Import.MainComponent.updateSelectedImportWizardComponent");
		this.getElement('wizardComponent').update("");
		
		switch(aComponent) {
			case 'CLIPPERZ_EXPORT':
				newWizardComponent = new Clipperz.PM.Components.Import.ClipperzImportComponent(this.getElement('wizardComponent'), {user:this.user()});
				break;
			case 'CSV':
				newWizardComponent = new Clipperz.PM.Components.Import.CSVImportComponent(this.getElement('wizardComponent'), {user:this.user()});
				break;
			case 'EXCEL':
				newWizardComponent = new Clipperz.PM.Components.Import.ExcelImportComponent(this.getElement('wizardComponent'), {user:this.user()});
				break;
			case 'PASSWORD_PLUS':
				newWizardComponent = new Clipperz.PM.Components.Import.PasswordPlusImportComponent(this.getElement('wizardComponent'), {user:this.user()});
				break;
			case 'ROBOFORM':
				newWizardComponent = new Clipperz.PM.Components.Import.RoboFormImportComponent(this.getElement('wizardComponent'), {user:this.user()});;
				break;
			case 'KEEPASS':
				newWizardComponent = new Clipperz.PM.Components.Import.KeePassImportComponent(this.getElement('wizardComponent'), {user:this.user()});
				break;
		}
		
		this.setWizardComponent(newWizardComponent);
//MochiKit.Logging.logDebug("<<< Import.MainComponent.updateSelectedWizardComponent");
	},

	//-------------------------------------------------------------------------

	'selectComponent': function(anEvent) {
		this.setSelectedComponent(anEvent.src().value);
		this.nextButton().enable();
	},

	'selectedComponent': function() {
		return this._selectedComponent;
	},
	
	'setSelectedComponent': function(aValue) {
		this._selectedComponent = aValue;
	},
	
	//-------------------------------------------------------------------------

	'backAction': function() {
	},
	
	//-------------------------------------------------------------------------

	'nextAction': function() {
		this.updateSelectedImportWizardComponent(this.selectedComponent());
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

