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
if (typeof(Clipperz.PM.Components.Import.CSVImport) == 'undefined') { Clipperz.PM.Components.Import.CSVImport = {}; }

//#############################################################################

Clipperz.PM.Components.Import.CSVImport.CSVImportHeader = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Import.CSVImport.CSVImportHeader.superclass.constructor.call(this, anElement, args);
	this._mainComponent = args.mainComponent;

	this._pendingDeferredLabelFieldHandlerEvents = 0;
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Import.CSVImport.CSVImportHeader, Clipperz.PM.Components.BaseComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Import.CSVImport.CSVImportHeader component";
	},

	//-------------------------------------------------------------------------

	'mainComponent': function() {
		return this._mainComponent;
	},

	//-------------------------------------------------------------------------

	'render': function() {
		var thConfigs;
		var i,c;
		
//MochiKit.Logging.logDebug(">>> CSVImportHeader.render");
		Clipperz.NotificationCenter.unregister(this);
		MochiKit.Signal.disconnectAllTo(this);

		thConfigs = [];
		c = this.mainComponent().parsedValues()[0].length;
		for (i=0; i<c; i++) {
			if (this.mainComponent().isColumnSelected(i)) {
//				thConfigs.push({tag:'th', children:[{tag:'input', type:'text', id:this.getId('headerTextField_' + i), value:this.mainComponent().labelForColumn(i)}]});
				thConfigs.push({tag:'th', children:[{tag:'input', type:'text', id:this.getId('headerTextField_' + i), value:""}]});
			}
		}
		
		this.element().update("");
		this.domHelper().append(this.element(), {tag:'div', children:[
			{tag:'div', cls:'importStepDescription', htmlString:Clipperz.PM.Strings['CSV_ImportWizard_Header']},
			{tag:'div', cls:'importStepParameters', children:[
				{tag:'input', type:'checkbox', name:'isFistRowHeader', id:this.getId('isFirstRowHeader_checkbox')},
				{tag:'span', id:this.getId('isFirstRowHeader_span'), cls:'clickableSpan', htmlString:Clipperz.PM.Strings['CSV_ImportWizard_Header_Settings_firstRowHeaderLabel']}
			]},
			{tag:'div', id:this.getId('dataDiv'), children:[
				{tag:'div', cls:'csvImportPreview', children:[
					{tag:'table', id:this.getId('previewDada'), cls:'csvImportPreview header', cellspacing:'0', children:[
						{tag:'thead', children:[{tag:'tr', children:thConfigs}]},
						{tag:'tbody', id:this.getId('previewData_tbody')}
					]}
				]}
			]}
		]});

		for (i=0; i<c; i++) {
			if (this.mainComponent().isColumnSelected(i)) {
				this.getElement('headerTextField_' + i).dom.value = this.mainComponent().labelForColumn(i);
			}
		}
		
		this.renderData(this.getElement('previewData_tbody'), this.mainComponent().parsedValues());

		if (this.mainComponent().isFirstRowHeader()) {
			this.getDom('isFirstRowHeader_checkbox').click();
		}

		c = this.mainComponent().parsedValues()[0].length;
		for (i=0; i<c; i++) {
			if (this.mainComponent().isColumnSelected(i)) {
				MochiKit.Signal.connect(this.getDom('headerTextField_' + i), 'onchange',   MochiKit.Base.partial(MochiKit.Base.method(this, 'labelFieldHandler'), i));
				MochiKit.Signal.connect(this.getDom('headerTextField_' + i), 'onkeypress', MochiKit.Base.partial(MochiKit.Base.method(this, 'deferredLabelFieldHandler'), i));
			}
		}

		MochiKit.Signal.connect(this.getDom('isFirstRowHeader_checkbox'), 'onclick', this, 'toggleFirstRowHeaderCheckboxHandler');
		if (Clipperz_IEisBroken != true) {
			MochiKit.Signal.connect(this.getDom('isFirstRowHeader_span'), 'onclick', this.getDom('isFirstRowHeader_checkbox'), 'click');
		}
//MochiKit.Logging.logDebug("<<< CSVImportHeader.render");
	},

	//-------------------------------------------------------------------------
	
	'renderData': function(anElement, someData) {
		var trConfigs;
		var data;
		var i,c;
		
//		anElement.update("");
		MochiKit.DOM.replaceChildNodes(anElement.dom);

		if (this.mainComponent().isFirstRowHeader()) {
			data = someData.slice(1);
		} else {
			data = someData;
		}
		
		trConfigs = MochiKit.Base.map(MochiKit.Base.bind(function(aRowData) {
			var result;
			var i,c;
			
			result = {tag:'tr', children:[]};
			c = aRowData.length;
			for (i=0; i<c; i++) {
				if (this.mainComponent().isColumnSelected(i)) {
					result.children.push({tag:'td', valign:'top', html:(MochiKit.Base.isNotEmpty(aRowData[i]) ? aRowData[i].replace(/\n/g, '<br>') : '&nbsp;')});
				}
			}
			
			return result;
		}, this), data);
		
		MochiKit.Base.map(function(aRowConfig) {Clipperz.YUI.DomHelper.append(anElement, aRowConfig);}, trConfigs);

		Clipperz.Style.applyZebraStylesToTable(this.getId('previewDada'));
	},

	//-------------------------------------------------------------------------

	'toggleFirstRowHeaderCheckboxHandler': function() {
		var firstRowData;
		var i,c;

//MochiKit.Logging.logDebug(">>> toggleFirstRowHeaderCheckboxHandler");
		this.mainComponent().setIsFirstRowHeader(this.getDom('isFirstRowHeader_checkbox').checked);

		firstRowData = this.mainComponent().parsedValues()[0];
		
		c = firstRowData.length;
		for (i=0; i<c; i++) {
			if (this.mainComponent().isColumnSelected(i)) {
				var label;
				
				if (this.mainComponent().isFirstRowHeader()) {
					label = firstRowData[i];
				} else {
					label = null;
				}

				this.mainComponent().setLabelForColumn(label, i);
			}
		};

		this.updateInputFieldValues();
		this.renderData(this.getElement('previewData_tbody'), this.mainComponent().parsedValues());
//MochiKit.Logging.logDebug("<<< toggleFirstRowHeaderCheckboxHandler");
	},

	//-------------------------------------------------------------------------

	'updateInputFieldValues': function() {
		var i,c;

//MochiKit.Logging.logDebug(">>> updateInputFieldValues");
		c = this.mainComponent().parsedValues()[0].length;
		for (i=0; i<c; i++) {
			if (this.mainComponent().isColumnSelected(i)) {
				this.getDom('headerTextField_' + i).value = this.mainComponent().labelForColumn(i);
			}
		}
//console.log('[1] fieldSettings', fieldSettings);
//MochiKit.Logging.logDebug("<<< updateInputFieldValues");
	},

	//-------------------------------------------------------------------------
	
	'labelFieldHandler': function(aColumnIndex, anEvent) {
		var inputField;
		
//MochiKit.Logging.logDebug(">>> labelFieldHandler");
		inputField = anEvent.src();
		
		this.mainComponent().setLabelForColumn(inputField.value, aColumnIndex);
//MochiKit.Logging.logDebug("##### [" + anEvent.src().id + "] -> label[" + aColumnIndex + "]: '" + inputField.value + "'");
//MochiKit.Logging.logDebug("<<< labelFieldHandler");
	},
	
	'deferredLabelFieldHandler': function(aColumnIndex, anEvent) {
//MochiKit.Logging.logDebug(">>> deferredLabelFieldHandler");
		this._pendingDeferredLabelFieldHandlerEvents ++;
		MochiKit.Async.callLater(1, MochiKit.Base.partial(MochiKit.Base.method(this, 'deferredLabelFieldHandlerCatcher'), aColumnIndex, anEvent));
//MochiKit.Logging.logDebug("<<< deferredLabelFieldHandler");
	},

	'deferredLabelFieldHandlerCatcher': function(aColumnIndex, anEvent) {
//MochiKit.Logging.logDebug(">>> deferredLabelFieldHandlerCatcher");
		this._pendingDeferredLabelFieldHandlerEvents --;
		if (this._pendingDeferredLabelFieldHandlerEvents == 0) {
			this.labelFieldHandler(aColumnIndex, anEvent);
		}
//MochiKit.Logging.logDebug("<<< deferredLabelFieldHandlerCatcher");
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

