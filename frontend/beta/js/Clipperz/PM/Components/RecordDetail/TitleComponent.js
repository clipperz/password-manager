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

Clipperz.PM.Components.RecordDetail.TitleComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.RecordDetail.TitleComponent.superclass.constructor.call(this, anElement, args);

//	this._inputElement = null;

	this.mainComponent().addEditComponent(this);

	this.render();
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.TitleComponent, Clipperz.PM.Components.RecordDetail.AbstractComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.TitleComponent component";
	},

	//-------------------------------------------------------------------------

	'value': function() {
		return this.record().label();
	},

	'setValue': function(aValue) {
		this.record().setLabel(aValue);
	},
	
	//-------------------------------------------------------------------------
/*
	'inputElement': function() {
		return this._inputElement;
	},
	
	'setInputElement': function(aValue) {
		this._inputElement = aValue;
	},
*/
	//-------------------------------------------------------------------------

	'render': function() {
//		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'td', html:'&#160'});
//		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'td', colspan:"3", html:'&#160', children:[
//			{tag:'div', /*style:'border: 1px solid green;',*/ id:this.getId('title')}
//		]});
//		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'td', html:'&#160'});
//		
//		this.setInputElement(new Clipperz.PM.Components.TextFormField(this.getElement('title'), {editMode:this.editMode(), value:this.value()}));
		
		this.update();
	},

	//-------------------------------------------------------------------------
/*
	'update': function() {
		this.inputElement().update({value:this.value(), editMode:this.editMode()});
	},
*/	
	//-------------------------------------------------------------------------

	'updateViewMode': function() {
		this.element().update("");
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'h2', html:this.value()});
	},

	//-------------------------------------------------------------------------
	
	'updateEditMode': function() {
//MochiKit.Logging.logDebug(">>> TitleComponent.updateEditMode");
//		this.getElement('title').update("");
//		Clipperz.YUI.DomHelper.append(this.getDom('title'), {tag:'div', id:this.getId('title_input')});
//		this.setInputElement(Clipperz.YUI.DomHelper.append(this.getDom('title_input'), {tag:'input', type:'text', value:this.value()}, true));
		
		this.element().update("");
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'input', id:this.getId('titleField'), type:'text', value:"this.value()"});
		this.getElement('titleField').dom.value = this.value();
		
//MochiKit.Logging.logDebug("<<< TitleComponent.updateEditMode");
	},

	//-------------------------------------------------------------------------

	'synchronizeComponentValues': function() {
		var inputElement;
		
//MochiKit.Logging.logDebug(">>> TitleComponent.synchronizeComponentValues");
		inputElement = this.element().getChildrenByTagName('input')[0];
		
		if (inputElement != null) {
			this.setValue(inputElement.dom.value);
		}
//MochiKit.Logging.logDebug("<<< TitleComponent.synchronizeComponentValues");
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

