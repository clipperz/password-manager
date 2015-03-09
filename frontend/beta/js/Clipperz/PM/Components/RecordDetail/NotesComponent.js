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



Clipperz.PM.Components.RecordDetail.NotesComponent = function(anElement, args) {
//MochiKit.Logging.logDebug(">>> new NotesComponent");
	args = args || {};

    Clipperz.PM.Components.RecordDetail.NotesComponent.superclass.constructor.call(this, anElement, args);

	this.mainComponent().addEditComponent(this);

	this._staticOffset = null;
	this._componentHeight = 50;
	this._mouseMoveIdentifier = null;
	this._mouseUpIdentifier = null;
	
	this.element().setVisibilityMode(YAHOO.ext.Element.DISPLAY);
	
	this.render();
//MochiKit.Logging.logDebug("<<< new NotesComponent");
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.NotesComponent, Clipperz.PM.Components.RecordDetail.AbstractComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.NotesComponent component";
	},

	//-------------------------------------------------------------------------

	'value': function() {
		return this.record().notes();
	},

	'setValue': function(aValue) {
		this.record().setNotes(aValue);
	},
	
	//-------------------------------------------------------------------------

	'render': function() {
//MochiKit.Logging.logDebug(">>> NotesComponent.render");
/*
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'td', colspan:'5', children:[
			{tag:'span', cls:'noteFieldLabel', htmlString:Clipperz.PM.Strings['recordDetailNotesLabel']},
			{tag:'div', cls:'noteFieldContent', id:this.getId('notes')}
		]});
*/		
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'span', cls:'noteFieldLabel', htmlString:Clipperz.PM.Strings['recordDetailNotesLabel']});
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', cls:'noteFieldContent', id:this.getId('notes'), children:[
			{tag:'div', id:this.getId('resizableDiv'), cls:'resizable-textarea', children:[
				{tag:'div', id:this.getId('contentView'), cls:'viewMode', html:""},
				{tag:'div', id:this.getId('contentEdit'), children:[
					{tag:'span', children:[
						{tag:'textarea', id:this.getId('textarea'), html:""}
					]}
				]},
				{tag:'div', id:this.getId('grippie'), cls:'grippie'}
			]}
		]});

		this.getElement('contentView').setVisibilityMode(YAHOO.ext.Element.DISPLAY);
		this.getElement('contentEdit').setVisibilityMode(YAHOO.ext.Element.DISPLAY);

		MochiKit.Signal.connect(this.getId('grippie'), 'onmousedown', this, 'startResize');
		
		this.update();
//MochiKit.Logging.logDebug("<<< NotesComponent.render");
	},

	//-------------------------------------------------------------------------

	'updateViewMode': function() {
//MochiKit.Logging.logDebug(">>> NotesComponent.updateViewMode");
//		this.getElement('notes').update(this.value().replace(/\n/g, '<br>'));

		this.getElement('contentView').update(Clipperz.Base.sanitizeString(this.value()).replace(/\n/g, '<br>'));

		if (this.isNoteEmpty()) {
			this.element().hide();
		} else {
			this.getElement('contentView').show();
			this.getElement('contentView').setHeight(this.componentHeight());
		}
		this.getElement('contentEdit').hide();

//MochiKit.Logging.logDebug("<<< NotesComponent.updateViewMode");
	},

	//-------------------------------------------------------------------------
	
	'updateEditMode': function() {
//MochiKit.Logging.logDebug(">>> NotesComponent.updateEditMode");
		this.getDom('textarea').value = this.value().replace(/\n/g, Clipperz_normalizedNewLine);

		this.getElement('contentView').hide();
		this.getElement('contentEdit').show();
		
		this.getElement('textarea').setHeight(this.componentHeight());
//MochiKit.Logging.logDebug("<<< NotesComponent.updateEditMode");
	},

	//-------------------------------------------------------------------------

	'synchronizeComponentValues': function() {
//MochiKit.Logging.logDebug(">>> NotesComponent.synchronizeComponentValues");
		if (this.getElement('textarea') != null) {
			this.setValue(this.getDom('textarea').value.replace(/(\x0a\x0d|\x0d\x0a)/g,'\n'));
		}
//MochiKit.Logging.logDebug("<<< NotesComponent.synchronizeComponentValues");
	},
	
	//-------------------------------------------------------------------------

	'componentHeight': function() {
		return this._componentHeight;
	},
	
	'setComponentHeight': function(aValue) {
		this._componentHeight = aValue;
	},

	//-------------------------------------------------------------------------

	'isNoteEmpty': function() {
		return !/[^ \n]/.test(this.value());
	},
	
	//-------------------------------------------------------------------------
	
	'staticOffset': function() {
		return this._staticOffset;
	},
	
	'setStaticOffset': function(aValue) {
		this._staticOffset = aValue;
	},

	//-------------------------------------------------------------------------

	'startResize': function(anEvent) {
//MochiKit.Logging.logDebug(">>> startResize");
		if (this.editMode() == 'VIEW') {
			this.setStaticOffset(this.getElement('contentView').getHeight() - anEvent.mouse().page['y'])
		} else {
			this.setStaticOffset(this.getElement('textarea').getHeight() - anEvent.mouse().page['y'])
//			this.getElement('textarea').setStyle('opacity', 0.25);
		}
		this.setMouseMoveIdentifier(MochiKit.Signal.connect(MochiKit.DOM.currentDocument(), 'onmousemove', this, 'whileResizing'));
		this.setMouseUpIdentifier(MochiKit.Signal.connect(MochiKit.DOM.currentDocument(), 'onmouseup', this, 'endResize'));
		anEvent.stop();
//MochiKit.Logging.logDebug("<<< startResize");
	},

	//-------------------------------------------------------------------------

	'whileResizing': function(anEvent) {
//MochiKit.Logging.logDebug(">>> whileResizing");
		this.getElement('textarea').setHeight(Math.max(32, this.staticOffset() + anEvent.mouse().page['y']) + 'px');
		this.getElement('contentView').setHeight(Math.max(32, this.staticOffset() + anEvent.mouse().page['y']) + 'px');
		anEvent.stop();
//MochiKit.Logging.logDebug("<<< whileResizing");
	},
	
	//-------------------------------------------------------------------------

	'endResize': function(anEvent) {
//MochiKit.Logging.logDebug(">>> endResize");
		MochiKit.Signal.disconnect(this.mouseMoveIdentifier());
		this.setMouseMoveIdentifier(null);
		MochiKit.Signal.disconnect(this.mouseUpIdentifier());
		this.setMouseUpIdentifier(null);
//		this.getElement('textarea').setStyle('opacity', 1);

		this.setComponentHeight(this.getElement('textarea').getHeight());
//MochiKit.Logging.logDebug("<<< endResize");
	},
	
	//-------------------------------------------------------------------------

	'mouseMoveIdentifier': function() {
		return this._mouseMoveIdentifier;
	},
	
	'setMouseMoveIdentifier': function(aValue) {
		this._mouseMoveIdentifier = aValue;
	},

	//-------------------------------------------------------------------------
	
	'mouseUpIdentifier': function() {
		return this._mouseUpIdentifier;
	},
	
	'setMouseUpIdentifier': function(aValue) {
		this._mouseUpIdentifier = aValue;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

