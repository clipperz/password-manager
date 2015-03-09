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

Clipperz.PM.Components.RecordDetail.DirectLoginComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.RecordDetail.DirectLoginComponent.superclass.constructor.call(this, anElement, args);

	this._directLogin = args.directLogin || null;
//	this._titleElement = null;
	this._structureElement = null;
	this._removeButton = null;
	this._directLoginBindingComponents = null;
	this._collapser = null;
	
	this.mainComponent().addEditComponent(this);
	this.render();

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.DirectLoginComponent, Clipperz.PM.Components.RecordDetail.AbstractComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.DirectLoginComponent component";
	},

	//-------------------------------------------------------------------------

	'directLogin': function() {
		return this._directLogin;
	},

	'directLoginBindingComponents': function() {
		return this._directLoginBindingComponents;
	},
	
	//-------------------------------------------------------------------------

	'removeDirectLogin': function() {
//MochiKit.Logging.logDebug(">>> DirectLoginComponent.removeDirectLogin");
		this.mainComponent().synchronizeComponentValues();
		this.directLogin().remove();
		this.mainComponent().removeEditComponent(this);
		this.mainComponent().render();
//MochiKit.Logging.logDebug("<<< DirectLoginComponent.removeDirectLogin");
	},

	//-------------------------------------------------------------------------
/*
	'formDataValue': function() {
		return Clipperz.Base.serializeJSON(this.directLogin().formData());
	},
	
	'setFormDataValue': function(aValue) {
		
	},
*/	
	//-------------------------------------------------------------------------

	'removeButton': function() {
		return this._removeButton;
	},
	
	'setRemoveButton': function(aValue) {
		this._removeButton = aValue;
	},

	//-------------------------------------------------------------------------
/*
	'titleElement': function() {
		return this._titleElement;
	},
	
	'setTitleElement': function(aValue) {
		this._titleElement = aValue;
	},
*/	
	//-------------------------------------------------------------------------

	'structureElement': function() {
		return this._structureElement;
	},
	
	'setStructureElement': function(aValue) {
		this._structureElement = aValue;
	},
	
	//-------------------------------------------------------------------------

	'render': function() {
//MochiKit.Logging.logDebug(">>> DirectLoginComponent.render");
		try {
			this.element().update("");
			Clipperz.YUI.DomHelper.append(this.element().dom,
				{tag:'li', children:[
					{tag:'table', width:'100%', border:'0', cellpadding:'0', cellspacing:'0', children:[
						{tag:'tbody', children:[
							{tag:'tr', children:[
								{tag:'td', rowspan:'2', width:'30', valign:'top', html:'&#160', children:[
									{tag:'div', id:this.getId('removeDirectLogin'), children:[
										{tag:'div', id:this.getId('removeDirectLoginButton')}
									]},
									{tag:'div', id:this.getId('collapseLink'), cls:'directLoginCollapseLink'}
								]},
								{tag:'td', valign:'top', children:[
									{tag:'table', width:'100%', border:'0', cellpadding:'0', cellspacing:'0', children:[
										{tag:'tbody', children:[
											{tag:'tr', children:[
												{tag:'td', width:'20', valign:'top', children:[
													{tag:'a', href:'#', id:this.getId('directLogin'), children:[
														{tag:'img', id:this.getId('faviconImage'), width:'16', height:'16', src:this.directLogin().fixedFavicon()}
													]}
												]},
												{tag:'td', valign:'top', children:[
													{tag:'div', cls:'directLoginDetailTitle', children:[
														{tag:'div', id:this.getId('titleViewBox'), children:[
															{tag:'a', href:'#', id:this.getId('titleLink')}
														]},
														{tag:'div', id:this.getId('titleEditBox'), children:[
															{tag:'input', type:'text', id:this.getId('titleInput')}
														]}
													]}
												]}
											]}
										]}
									]}
								]}
							]},
							{tag:'tr', children:[
								{tag:'td', /*colspan:'2',*/ children:[
									{tag:'div', id:this.getId('details'), children:[
										{tag:'table', cls:'directLoginBindings', border:'0', cellpadding:'0', cellspacing:'0', children:[
											{tag:'tbody', id:this.getId('tbodyBindings'), children:[]}
										]}
									]}
								]}
							]}
						]}
					]}
				]}
			);

			MochiKit.Signal.connect(this.getId('faviconImage'), 'onload', this, 'handleLoadedFaviconImage');
			MochiKit.Signal.connect(this.getId('faviconImage'), 'onerror', this.directLogin(), 'handleMissingFaviconImage');
			MochiKit.Signal.connect(this.getId('faviconImage'), 'onabort', this.directLogin(), 'handleMissingFaviconImage');

			this.getElement('removeDirectLogin').setVisibilityMode(YAHOO.ext.Element.DISPLAY);
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 1");
			this.getElement('collapseLink').addClassOnOver('hover');
			this._collapser = new Clipperz.YUI.Collapser(this.getElement('collapseLink'), this.getElement('details'), true);
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 2");
			MochiKit.Signal.connect(this.getId('directLogin'), 'onclick', this, 'runDirectLogin');
//			this.getElement('directLogin').on('click', this.runDirectLogin, this, false);
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 3");
//			this.setTitleElement(new Clipperz.PM.Components.TextFormField(this.getElement('title'), {
//														editMode:this.editMode(),
//														value:this.directLogin().label()
//			}));
			this.getElement('titleViewBox').setVisibilityMode(YAHOO.ext.Element.DISPLAY);
			this.getElement('titleEditBox').setVisibilityMode(YAHOO.ext.Element.DISPLAY);
//-			this.getElement('titleLink').on('click', this.runDirectLogin, this, false);
			MochiKit.Signal.connect(this.getId('titleLink'), 'onclick', this, 'runDirectLogin');

//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 4");
//-			this.setStructureElement(new Clipperz.PM.Components.TextFormField(this.getElement('formStructure'), {
//-														editMode:this.editMode(),
//-														value:this.formDataValue(), multiline:true
//-			}));
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 5");
			{
				var	bindingKey;
				var valueName;
				var inputsRequiringAdditionalValues;
				var bindingsElement;
				var i,c;
			
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 6");
				this._directLoginBindingComponents = [];
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 7");
				bindingsElement = this.getElement('tbodyBindings');
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 8");
				for (bindingKey in this.directLogin().bindings()) {
					try {
						var directLoginBindingElement;
						var directLoginBindingComponent;
				
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 9");
						directLoginBindingElement = Clipperz.YUI.DomHelper.append(bindingsElement.dom, {tag:'tr'}, true);
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 10");
						directLoginBindingComponent =	new Clipperz.PM.Components.RecordDetail.DirectLoginBindingComponent(directLoginBindingElement, {
																mainComponent:this,
																directLoginBinding:this.directLogin().bindings()[bindingKey]
															});
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 11");
						this._directLoginBindingComponents.push(directLoginBindingComponent);
					} catch (e) {
						MochiKit.Logging.logError("Error while rendering a DirectLoginBindingComponent - " + e);
					}
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 12");
				}
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 13");

				inputsRequiringAdditionalValues = this.directLogin().inputsRequiringAdditionalValues();
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 13.1");
				for (valueName in inputsRequiringAdditionalValues) {
//-					Clipperz.YUI.DomHelper.append(bindingsElement.dom, {tag:'tr', children:[
//-						{tag:'td', html:valueName},
//-						{tag:'td', children:inputsRequiringAdditionalValues[valueName].inputElementConfiguration()}
//-					]}, true)
					var directLoginValueElement;
					var directLoginValueComponent;
				
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 13.2");
					directLoginValueElement = Clipperz.YUI.DomHelper.append(bindingsElement.dom, {tag:'tr'}, true);
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 13.3");
					directLoginValueComponent =	new Clipperz.PM.Components.RecordDetail.DirectLoginValueComponent(directLoginValueElement, {
															mainComponent:this,
															directLoginInputValue:inputsRequiringAdditionalValues[valueName]
														});
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 13.4");
					this._directLoginBindingComponents.push(directLoginValueComponent);
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 13.5");
				}
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 13.6");
			}
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 14");
			this.setRemoveButton(new YAHOO.ext.Button(this.getDom('removeDirectLoginButton'), {text:Clipperz.PM.Strings['recordDetailDeleteDirectLoginButtonLabel'], handler:this.removeDirectLogin, scope:this}));
//MochiKit.Logging.logDebug("--- DirectLoginComponent.render - 15");
			this.update();
		} catch (e) {
			MochiKit.Logging.logError("Error while rendering a DirectLoginComponent - " + e);
		}
//MochiKit.Logging.logDebug("<<< DirectLoginComponent.render");
	},

	//-------------------------------------------------------------------------

	'collapser': function() {
		return this._collapser;
	},
	
	//-------------------------------------------------------------------------

	'handleLoadedFaviconImage': function(anEvent) {
		MochiKit.Signal.disconnectAll(anEvent.src())
	},
	
	//-------------------------------------------------------------------------
	
	'update': function() {
		var i,c;
		var bindingComponents;

//MochiKit.Logging.logDebug(">>> DirectLoginComponent.update");
		bindingComponents = this.directLoginBindingComponents();
		c = bindingComponents.length;
		for (i=0; i<c; i++) {
			bindingComponents[i].update();
		}
		
    	Clipperz.PM.Components.RecordDetail.DirectLoginComponent.superclass.update.call(this);
//MochiKit.Logging.logDebug("<<< DirectLoginComponent.update");
	},
	
	//-------------------------------------------------------------------------

	'updateEditMode': function() {
//		this.element().update("");
//		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', style:'border:4px solid red; padding:10px;', children:[
//			{tag:'div', style:'font-weight:bold;', html:this.directLogin().label()},
//			{tag:'div', style:'border:1px solid #aaaaaa;', html:Clipperz.Base.serializeJSON(this.directLogin().formData())},
//			{tag:'div', style:'border:1px solid #aaaaaa;', html:Clipperz.Base.serializeJSON(this.directLogin().bindings())}
//		]});
		
		this.getElement('titleEditBox').show();
		this.getElement('titleViewBox').hide();
		
		this.getDom('titleInput').value = this.directLogin().label();
		
//MochiKit.Logging.logDebug(">>> DirectLoginComponent.updateEditMode");
		this.collapser().expand();
		this.getElement('collapseLink').hide();
		this.getElement('removeDirectLogin').show();
//		this.removeButton().show();
//MochiKit.Logging.logDebug("<<< DirectLoginComponent.updateEditMode");
	},
	
	//-------------------------------------------------------------------------

	'updateViewMode': function() {
//MochiKit.Logging.logDebug(">>> DirectLoginComponent.updateViewMode");
		this.getElement('titleEditBox').hide();
		this.getElement('titleViewBox').show();
		this.getElement('titleLink').update(this.directLogin().label());

		this.getElement('collapseLink').show();
		this.getElement('removeDirectLogin').hide();
//		this.removeButton().hide();
//MochiKit.Logging.logDebug("<<< DirectLoginComponent.updateViewMode");
	},

	//-------------------------------------------------------------------------

	'synchronizeComponentValues': function() {
//MochiKit.Logging.logDebug(">>> DirectLoginComponent.syncronizeComponentValues");
		this.directLogin().setLabel(this.getDom('titleInput').value);
//		this.setFormDataValue(this.structureElement().value());
		
		MochiKit.Iter.forEach(this.directLoginBindingComponents(), MochiKit.Base.methodcaller('synchronizeComponentValues'));
//MochiKit.Logging.logDebug("<<< DirectLoginComponent.syncronizeComponentValues");
	},

	//-------------------------------------------------------------------------

	'runDirectLogin': function(anEvent) {
//MochiKit.Logging.logDebug("--- DirectLoginComponent.runDirectLogin - 1");
//MochiKit.Logging.logDebug("--- DirectLoginComponent.runDirectLogin - 1 anEvent: " + anEvent);
		anEvent.stop();
//MochiKit.Logging.logDebug("--- DirectLoginComponent.runDirectLogin - 2");
		this.directLogin().runDirectLogin();
//MochiKit.Logging.logDebug("--- DirectLoginComponent.runDirectLogin - 3");
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

