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

Clipperz.PM.Components.RecordDetail.HeaderComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.RecordDetail.HeaderComponent.superclass.constructor.call(this, anElement, args);
	this.mainComponent().addEditComponent(this);
	
	this._saveButton = null;
	
	this.render();
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.HeaderComponent, Clipperz.PM.Components.RecordDetail.AbstractComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.HeaderComponent component";
	},

	//-------------------------------------------------------------------------

	'render': function() {
		var editButton;
		
//MochiKit.Logging.logDebug(">>> RecordDetail.HeaderComponent.appendTo");
 		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', cls:'recordDetailButtonsBox', children:[
			{tag:'div', id:this.getId('editButtonBox'), children:[
				{tag:'table', cls:'recordDetailButtonsTABLE', border:'0', cellpadding:'0', cellspacing:'0', children:[
					{tag:'tbody', children:[
						{tag:'tr', children:[
							{tag:'td', align:'center', children:[
								{tag:'div', id:this.getId('editButton')}
							]}
						]}
					]}
				]}
			]},
			{tag:'div', id:this.getId('saveCancelButtonBox'), children:[
				{tag:'table', cls:'recordDetailButtonsTABLE', border:'0', cellpadding:'0', cellspacing:'0', children:[
					{tag:'tbody', children:[
						{tag:'tr', children:[
							{tag:'td', width:'49%', align:'right', children:[
								{tag:'div', id:this.getId('saveButton')}
							]},
							{tag:'td', html:'&nbsp'},
							{tag:'td', width:'49%', align:'left', children:[
								{tag:'div', id:this.getId('cancelButton')}
							]}
						]}
					]}
				]}
			]}
		]});

		this.getElement('editButtonBox').setVisibilityMode(YAHOO.ext.Element.DISPLAY);
		this.getElement('saveCancelButtonBox').setVisibilityMode(YAHOO.ext.Element.DISPLAY);

		editButton = new YAHOO.ext.Button(this.getDom('editButton'), {text:Clipperz.PM.Strings['recordDetailEditButtonLabel'], handler:this.editButtonHandler, scope:this});
		this.setSaveButton(new YAHOO.ext.Button(this.getDom('saveButton'), {text:Clipperz.PM.Strings['recordDetailSaveButtonLabel'], handler:this.saveButtonHandler, scope:this}));
		new YAHOO.ext.Button(this.getDom('cancelButton'), {text:Clipperz.PM.Strings['recordDetailCancelButtonLabel'], handler:this.cancelButtonHandler, scope:this});

		if (Clipperz.PM.Proxy.defaultProxy.isReadOnly()) {
			editButton.disable();
		}
		
		this.update();
//MochiKit.Logging.logDebug("<<< RecordDetail.HeaderComponent.appendTo");
	},

	//-------------------------------------------------------------------------

	'updateViewMode': function() {
//MochiKit.Logging.logDebug(">>> HeaderComponent.updateViewMode");
		this.getElement('editButtonBox').show();
		this.getElement('saveCancelButtonBox').hide();
//MochiKit.Logging.logDebug("<<< HeaderComponent.updateViewMode");
	},

	//-------------------------------------------------------------------------
	
	'updateEditMode': function() {
		this.getElement('editButtonBox').hide();
		this.getElement('saveCancelButtonBox').show();
		if (this.mainComponent().enableSaveButton() == true) {
//MochiKit.Logging.logDebug("--- HeaderComponent.updateViewMode - ENABLE");
			this.saveButton().enable();
		} else {
			this.saveButton().disable();
		}
	},
	
	//-------------------------------------------------------------------------

	'saveButton': function() {
		return this._saveButton;
	},
	
	'setSaveButton': function(aValue) {
		this._saveButton = aValue;
	},

	//-------------------------------------------------------------------------

	'editButtonHandler': function(anEvent) {
		this.mainComponent().setEditMode('EDIT');
	},

	//-------------------------------------------------------------------------

	'saveButtonHandler': function(anEvent) {
//MochiKit.Logging.logDebug(">>> RecordDetail.HeaderComponent.saveButtonHandler");
		this.mainComponent().setEditMode('VIEW', this.getElement('saveButton'));
//MochiKit.Logging.logDebug("<<< RecordDetail.HeaderComponent.saveButtonHandler");
	},
	
	//-------------------------------------------------------------------------

	'cancelButtonHandler': function(anEvent) {
		this.record().cancelChanges();
//MochiKit.Logging.logDebug("--- HeaderComponent.cancelButtonHandler - " + Clipperz.Base.serializeJSON(this.record().currentDataSnapshot()));
		this.mainComponent().setEditMode('VIEW', null, true);
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});

