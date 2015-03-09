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

Clipperz.PM.Components.Import.CSVImport.CSVImportColumns = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Import.CSVImport.CSVImportColumns.superclass.constructor.call(this, anElement, args);
	this._mainComponent = args.mainComponent;
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Import.CSVImport.CSVImportColumns, Clipperz.PM.Components.BaseComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Import.CSVImport.CSVImportColumns component";
	},

	//-------------------------------------------------------------------------

	'mainComponent': function() {
		return this._mainComponent;
	},

	//-------------------------------------------------------------------------

	'render': function() {
		var i,c;
		var columnSelectorCheckboxCells;
		var checkboxes;
		var data;

//MochiKit.Logging.logDebug(">>> CSVImportColumns.render");
		Clipperz.NotificationCenter.unregister(this);
		MochiKit.Signal.disconnectAllTo(this);

		this.element().update("");

		data = this.mainComponent().parsedValues();
		columnSelectorCheckboxCells = [];
		
		c =	data[0].length;
		for (i=0; i<c; i++) {
			columnSelectorCheckboxCells.push({tag:'th', valign:'top', cls:(this.mainComponent().isColumnSelected(i) ? 'selectedColumn': 'skippedColumn'), children:[
				{tag:'input', type:'checkbox', id:this.getId('columnCheckbox_' + i), value:i}
			]})
		}
		
		this.domHelper().append(this.element(), {tag:'div', children:[
			{tag:'div', cls:'importStepDescription', htmlString:Clipperz.PM.Strings['CSV_ImportWizard_Columns']},
			{tag:'div', id:this.getId('dataDiv'), cls:'csvImportPreview', children:[
				{tag:'table', id:this.getId('previewDada'), cls:'csvImportPreview columns', cellspacing:'0', children:[
					{tag:'thead', id:this.getId('previewData_thead'), children:[
						{tag:'tr', children:columnSelectorCheckboxCells}
					]},
					{tag:'tbody', id:this.getId('previewData_tbody'), children:[]}
				]}
			]}
		]});

		c =	data[0].length;
		for (i=0; i<c; i++) {
			if (this.mainComponent().isColumnSelected(i)) {
				this.getDom('columnCheckbox_' + i).checked = true;
			}
		}
		
		this.renderData(this.getElement('previewData_tbody'), data);

		checkboxes = MochiKit.DOM.getElementsByTagAndClassName('input', null, this.getDom('previewData_thead'));
		c = checkboxes.length;
		for (i=0; i<c; i++) {
			MochiKit.Signal.connect(checkboxes[i], 'onclick', this, 'renderDataHandler');
		}
//MochiKit.Logging.logDebug("<<< CSVImportColumns.render");
	},

	//-------------------------------------------------------------------------
	
	'renderData': function(anElement, someData) {
		var config;
		var i,c;
		
//MochiKit.Logging.logDebug(">>> CSVImportColumns.renderData");
//		anElement.update("");
		MochiKit.DOM.replaceChildNodes(anElement.dom);

		config = MochiKit.Base.map(MochiKit.Base.bind(function(aRowData) {
			var result;
			var i,c;
			
			result = {tag:'tr', children:[]};
			c = aRowData.length;
			for (i=0; i<c; i++) {
				var field;
				
				field = aRowData[i];
				result.children.push({tag:'td', valign:'top', cls:(this.mainComponent().isColumnSelected(i) ? 'selectedColumn': 'skippedColumn'), html:(MochiKit.Base.isNotEmpty(field) ? field.replace(/\n/g, '<br>') : '&nbsp;')});
			}
			
			return result;
		}, this), someData);
		
		MochiKit.Base.map(function(aRowConfig) {Clipperz.YUI.DomHelper.append(anElement, aRowConfig);}, config);

		Clipperz.Style.applyZebraStylesToTable(this.getId('previewDada'));
//MochiKit.Logging.logDebug("<<< CSVImportColumns.renderData");
	},

	//-------------------------------------------------------------------------

	'renderDataHandler': function(anEvent) {
		var thElement;
		
		thElement = YAHOO.ext.Element.get(anEvent.src().parentNode);

		if (anEvent.src().checked == true) {
			this.mainComponent().skippedColumns().remove(anEvent.src().value);
			thElement.addClass('selectedColumn');
			thElement.removeClass('skippedColumn');
		} else {
			this.mainComponent().skippedColumns().add(anEvent.src().value);
			thElement.removeClass('selectedColumn');
			thElement.addClass('skippedColumn');
		}

		if (this.mainComponent().skippedColumns().allItems().length == this.mainComponent().parsedValues()[0].length) {
			this.mainComponent().nextButton().disable();
		} else {
			this.mainComponent().nextButton().enable();
		}
		
		this.renderData(this.getElement('previewData_tbody'), this.mainComponent().parsedValues());
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

