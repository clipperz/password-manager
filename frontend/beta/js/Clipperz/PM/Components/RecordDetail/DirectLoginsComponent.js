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

Clipperz.PM.Components.RecordDetail.DirectLoginsComponent = function(anElement, args) {
//MochiKit.Logging.logDebug(">>> new Clipperz.PM.Components.RecordDetail.DirectLoginsComponent");
	args = args || {};

//MochiKit.Logging.logDebug("--- new Clipperz.PM.Components.RecordDetail.DirectLoginsComponent - 0");
    Clipperz.PM.Components.RecordDetail.DirectLoginsComponent.superclass.constructor.call(this, anElement, args);
//MochiKit.Logging.logDebug("--- new Clipperz.PM.Components.RecordDetail.DirectLoginsComponent - 1");

	this._addDirectLoginButton = null;
	
//MochiKit.Logging.logDebug("--- new Clipperz.PM.Components.RecordDetail.DirectLoginsComponent - 2");
	this.mainComponent().addEditComponent(this);
//MochiKit.Logging.logDebug("--- new Clipperz.PM.Components.RecordDetail.DirectLoginsComponent - 3");
	this.render();
//MochiKit.Logging.logDebug("<<< new Clipperz.PM.Components.RecordDetail.DirectLoginsComponent");
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.DirectLoginsComponent, Clipperz.PM.Components.RecordDetail.AbstractComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.DirectLoginsComponent component";
	},

	//-------------------------------------------------------------------------

	'addDirectLoginButton': function() {
		return this._addDirectLoginButton;
	},

	'setAddDirectLoginButton': function(aValue) {
		this._addDirectLoginButton = aValue;
	},
	
	//-------------------------------------------------------------------------

	'render': function() {
		this.element().update("");

		Clipperz.YUI.DomHelper.append(this.element().dom,
			{tag:'div', cls:'directLoginsRecordBox', children:[
				{tag:'h3', htmlString:Clipperz.PM.Strings['recordDetailDirectLoginBlockTitle']},
				{tag:'ul', id:this.getId('directLogins')},

				{tag:'div', cls:'addDirectLoginBox', id:this.getId('addDirectLogin'), children:[
					{tag:'div', cls:'addDirectLoginBoxContent', children:[
						{tag:'div', cls:'bookmarkletConfiguration', children:[
//							{tag:'span', htmlString:Clipperz.PM.Strings['newRecordWizardBookmarkletConfigurationLabel']},
							{tag:'div', htmlString:Clipperz.PM.Strings['recordDetailNewDirectLoginDescription']},
							{tag:'textarea', id:this.getId('addDirectLoginTextarea')}
						]},
						{tag:'div', id:this.getId('addDirectLoginButton')}
					]}
				]}
			]}
		);

		if (MochiKit.Base.keys(this.record().directLogins()).length == 0) {
//MochiKit.Logging.logDebug("--- DirectLoginsComponent.render - 3");
			Clipperz.YUI.DomHelper.append(this.getElement('directLogins'),
				{tag:'li', children:[
//					{tag:'span', htmlString:Clipperz.PM.Strings['recordDetailDirectLoginBlockNoDirectLoginConfiguredLabel']}
					{tag:'div', cls:'recordDetailNoDirectLoginDescriptionBox', htmlString:Clipperz.PM.Strings['recordDetailDirectLoginBlockNoDirectLoginConfiguredDescription']}
				]}
			);
//MochiKit.Logging.logDebug("--- DirectLoginsComponent.render - 4");
		} else {
//MochiKit.Logging.logDebug("--- DirectLoginsComponent.render - 5");
			for (directLoginReference in this.record().directLogins()) {
//MochiKit.Logging.logDebug("--- DirectLoginsComponent.render - 6");
				this.addDirectLogin(this.record().directLogins()[directLoginReference]);
//MochiKit.Logging.logDebug("--- DirectLoginsComponent.render - 7");
			}
//MochiKit.Logging.logDebug("--- DirectLoginsComponent.render - 8");
		}
//MochiKit.Logging.logDebug("--- DirectLoginsComponent.render - 9");

		this.setAddDirectLoginButton(new YAHOO.ext.Button(this.getDom('addDirectLoginButton'), {
						text:Clipperz.PM.Strings['recordDetailAddNewDirectLoginButtonLabel'],
						handler:this.addNewDirectLogin,
						scope:this
		}));
		MochiKit.Signal.connect(this.getId('addDirectLoginTextarea'), 'onkeydown', this, 'onkeydown');
//MochiKit.Logging.logDebug("--- DirectLoginsComponent.render - 11");

		this.update();
//MochiKit.Logging.logDebug("<<< DirectLoginsComponent.render");
	},

	//-------------------------------------------------------------------------

	'addDirectLogin': function(aDirectLogin) {
//MochiKit.Logging.logDebug(">>> DirectLoginsComponent.addDirectLogin");
		new Clipperz.PM.Components.RecordDetail.DirectLoginComponent(
				Clipperz.YUI.DomHelper.append(this.getDom('directLogins'), {tag:'div'}, true),
				{
					mainComponent:this.mainComponent(),
					directLogin:aDirectLogin
				}
		);
//MochiKit.Logging.logDebug("<<< DirectLoginsComponent.addDirectLogin");
	},
	
	//-------------------------------------------------------------------------

	'addNewDirectLogin': function() {
		var	newDirectLogin;
		var	configuration;

//MochiKit.Logging.logDebug(">>> DirectLoginsComponent.addNewDirectLogin");
		if (MochiKit.Base.keys(this.record().directLogins()).length == 0) {
			this.getElement('directLogins').update("");
		}
		
		this.mainComponent().synchronizeComponentValues();
		
		this.mainComponent().exitModalView();
		configuration = Clipperz.PM.BookmarkletProcessor.checkBookmarkletConfiguration(
								this.getDom('addDirectLoginTextarea').value,
								this.getDom('addDirectLoginButton'),
								MochiKit.Base.method(this.mainComponent(), 'enterModalView')
		);
		this.mainComponent().enterModalView();
		
		newDirectLogin = new Clipperz.PM.DataModel.DirectLogin({record:this.record(),
																label:configuration['page']['title'],
																bookmarkletVersion:'0.2',
//																bookmarkletVersion:configuration['version'],
																formData:configuration['form']});
		this.record().addDirectLogin(newDirectLogin);
		this.addDirectLogin(newDirectLogin);
		this.getDom('addDirectLoginTextarea').value = "";
//MochiKit.Logging.logDebug("<<< DirectLoginsComponent.addNewDirectLogin");
	},
	
	//-------------------------------------------------------------------------

	'updateViewMode': function() {
		this.getElement('addDirectLogin').setVisibilityMode(YAHOO.ext.Element.DISPLAY);
		this.getElement('addDirectLogin').hide();
	},

	//-------------------------------------------------------------------------

	'updateEditMode': function() {
		this.getElement('addDirectLogin').show();
	},

	//-------------------------------------------------------------------------

	'onkeydown': function(anEvent) {
//MochiKit.Logging.logDebug(">>> onkeydown - " + anEvent.src().id + ": " + anEvent.key().code);
		if (anEvent.key().code == 13) {
			this.addNewDirectLogin();
		}
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

