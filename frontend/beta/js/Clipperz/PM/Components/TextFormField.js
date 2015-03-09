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

Clipperz.PM.Components.TextFormField = function(anElement, args) {
	args = args || {};

//MochiKit.Logging.logDebug(">>> new TextFormField");
	Clipperz.PM.Components.TextFormField.superclass.constructor.call(this, args);
	
	this._element = anElement;
	this._editMode = args.editMode || 'VIEW';
	this._value = args.value || "";
	this._inputElement = null;
	this._wrapper = null;
	this._multiline = args.multiline || false;
	
//	this.multiline = args.multiline || true;
//	this.editing = true;
//	this.completeOnBlur = true;
//	this.autoSizeTask = new YAHOO.ext.util.DelayedTask(this.autoSize, this);
//	this.textSizeEl = Clipperz.YUI.DomHelper.append(document.body, {
//		tag: 'div',
//		cls: 'yinline-editor-sizer ' + (this.cls || '')
//	});
	
	this.render();
//MochiKit.Logging.logDebug("<<< new TextFormField");
	
	return this;
};

YAHOO.extendX(Clipperz.PM.Components.TextFormField, Clipperz.PM.Components.BaseComponent, {
	
	'toString': function() {
		return "Clipperz.PM.Components.TextFormField";
	},
	
	//-----------------------------------------------------

	'value': function() {
		if (this.inputElement() != null) {
			this._value = this.inputElement().dom.value;
		}
		
		return this._value;
//		return this.inlineEditor().getValue();
	},
	
	'setValue': function(aValue) {
		this._value = aValue;
//		this.getElement('viewComponent_Content').update(aValue);
//		this.inlineEditor().setValue(aValue);
	},

	//-----------------------------------------------------

	'multiline': function() {
		return this._multiline;
	},
	
	//-----------------------------------------------------

	'editMode': function() {
		return this._editMode;
	},
	
	'setEditMode': function(aValue) {
		this._editMode = aValue;
	},

	//-----------------------------------------------------

	'inputElement': function() {
		return this._inputElement;
	},

	'setInputElement': function(aValue) {
		this._inputElement = aValue;
	},

	//-----------------------------------------------------

	'on': function(anEventName, anHandler, aScope, shouldOverride) {
//MochiKit.Logging.logDebug(">>> TextFormField.on - inputElement: " + this.inputElement());
		return this.inputElement().on(anEventName, anHandler, aScope, shouldOverride);
//MochiKit.Logging.logDebug("<<< TextFormField.on - inputElement: " + this.inputElement());
	},

	//-----------------------------------------------------
	
	'wrapper': function() {
		return this._wrapper;
	},
	
	//-----------------------------------------------------

	'render': function() {
		var editModeConfiguration;
		var viewModeConfiguration;

		editModeConfiguration = {tag:'div', id:this.getId('editComponent'), children:[]};
		if (this.multiline() == false) {
			editModeConfiguration.children.push({tag:'input', type:'text', id:this.getId('editComponent_input'), value:"this.value(1)"});
		} else {
			editModeConfiguration.children.push({tag:'textarea', id:this.getId('editComponent_input'), html:"this.value(2)"});
		}

		viewModeConfiguration = {tag:'div', id:this.getId('viewComponent'), /*style:'border: 1px solid blue;',*/ children:[
			{tag:'span', id:this.getId('viewComponent_Content'), html:this.value()}
		]}

//MochiKit.Logging.logDebug(">>> TextFormField.render");
		this._wrapper = Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', id:this.getId('wrapper'), children:[
			{tag:'div', id:this.getId('editModeBox'), children:[editModeConfiguration]},
			{tag:'div', id:this.getId('viewModeBox'), children:[viewModeConfiguration]}
		]}, true);
		
		this.getElement('editModeBox').setVisibilityMode(YAHOO.ext.Element.DISPLAY);
		this.getElement('viewModeBox').setVisibilityMode(YAHOO.ext.Element.DISPLAY);

		this.getElement('editComponent_input').dom.value = this.value();
		this.setInputElement(this.getElement('editComponent_input'));
		
		this.update();
//MochiKit.Logging.logDebug("<<< TextFormField.render");
	},

	//-----------------------------------------------------

	'update': function(args) {
		args = args || {};
		
//MochiKit.Logging.logDebug(">>> TextFormField.update");
		if (typeof(args.value) != 'undefined') {
			this.setValue(args.value);
		}
		if (typeof(args.editMode) != 'undefined') {
			this.setEditMode(args.editMode)
		}
		
		if (this.editMode() == 'VIEW') {
			this.updateViewMode();
		} else if (this.editMode() == 'EDIT') {
			this.updateEditMode();
		} else {
			//	?????
		}
//MochiKit.Logging.logDebug("<<< TextFormField.update");
	},

	//-----------------------------------------------------

	'updateEditMode': function() {
//MochiKit.Logging.logDebug(">>> TextFormField.updateEditMode");
		this.getElement('viewModeBox').hide();
		this.getElement('editModeBox').show();

		if (this.multiline() == false) {
			this.getElement('editComponent_input').dom.value = this.value();
		} else {
			this.getElement('editComponent_input').update(Clipperz.Base.sanitizeString(this.value()));
		}
//MochiKit.Logging.logDebug("<<< TextFormField.updateEditMode");
	},

	//-----------------------------------------------------

	'updateViewMode': function() {
//MochiKit.Logging.logDebug(">>> TextFormField.updateViewMode");
		this.getElement('editModeBox').hide();
		this.getElement('viewModeBox').show();

		this.getElement('viewComponent_Content').update(Clipperz.Base.sanitizeString(this.value()));
//MochiKit.Logging.logDebug("<<< TextFormField.updateViewMode");
	},
    
	//#####################################################
	//#####################################################
	//#####################################################
	//#####################################################
/*	
	'onEnter': function(k, e) {
MochiKit.Logging.logDebug(">>> TextFormField.onEnter");
		if (this.multiline && (e.ctrlKey || e.shiftKey)) {
			return;
		} else {
			this.completeEdit();
			e.stopEvent();
		}
MochiKit.Logging.logDebug("<<< TextFormField.onEnter");
	},

	//-----------------------------------------------------

	'onEsc': function() {
MochiKit.Logging.logDebug(">>> TextFormField.onEsc");
//		if (this.ignoreNoChange) {
//			this.revert(true);
//		} else {
			this.revert(false);
			this.completeEdit();
//		}
MochiKit.Logging.logDebug("<<< TextFormField.onEsc");
	},

	//-----------------------------------------------------

	onBlur : function(){
MochiKit.Logging.logDebug(">>> TextFormField.onBlur");
		if (this.editing && this.completeOnBlur !== false) {
			this.completeEdit();
		}
MochiKit.Logging.logDebug("<<< TextFormField.onBlur");
	},

	//-----------------------------------------------------

	'onKeyUp': function(e) {
		var k = e.getKey();
		if (this.editing && (k < 33 || k > 40) && k != 27) {
			this.autoSizeTask.delay(50);
		}
	},

	//-----------------------------------------------------

	'autoSize': function() {
		var el = this.inputElement();
		var wrap = this.getElement('editComponent');
		var v = el.dom.value;
		var ts = this.textSizeEl;

		if (v.length < 1) {
			ts.innerHTML = "&#160;&#160;";
		} else {
			v = v.replace(/[<> ]/g, '&#160;');
			if (this.multiline) {
				v = v.replace(/\n/g, '<br />&#160;');
			}
			ts.innerHTML = v;
		}
		
		var ww = wrap.dom.offsetWidth;
		var wh = wrap.dom.offsetHeight;
		var w = ts.offsetWidth;
		var h = ts.offsetHeight;
		// lots of magic numbers in this block - wtf?
		// the logic is to prevent the scrollbars from flashing
		// in firefox. Updates the right element first
		// so there's never overflow.
		if (ww > w+4) {
			el.setWidth(w+4);
			wrap.setWidth(w+8);
		} else {
			wrap.setWidth(w+8);
			el.setWidth(w+4);
		}
		if (wh > h+4) {
			el.setHeight(h);
			wrap.setHeight(h+4);
		} else {
			wrap.setHeight(h+4);
			el.setHeight(h);
		}
	},

	//-----------------------------------------------------

	'completeEdit': function() {
MochiKit.Logging.logDebug(">>> TextFormField.completeEdit");
		
	},

	'revert': function() {
MochiKit.Logging.logDebug(">>> TextFormField.revert");
		
	},
*/	
	//-----------------------------------------------------
	__syntaxFix__: '__syntaxFix__'
});
