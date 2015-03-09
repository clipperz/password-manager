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

Clipperz.PM.Components.Import.CSVImport.CSVImportFields = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Import.CSVImport.CSVImportFields.superclass.constructor.call(this, anElement, args);
	this._mainComponent = args.mainComponent;

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Import.CSVImport.CSVImportFields, Clipperz.PM.Components.BaseComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Import.CSVImport.CSVImportFields component";
	},

	//-------------------------------------------------------------------------

	'mainComponent': function() {
		return this._mainComponent;
	},

	//-------------------------------------------------------------------------

	'render': function() {
		var	fieldsHeaderCells;
		var titleColumnIndex;
		var notesColumnIndex;
		var i,c;

		Clipperz.NotificationCenter.unregister(this);
		MochiKit.Signal.disconnectAllTo(this);

		this.element().update("");

		titleColumnIndex = this.mainComponent().titleColumnIndex()
		notesColumnIndex = this.mainComponent().notesColumnIndex()

		fieldsHeaderCells = [];
		fieldsHeaderCells.push({tag:'td', valign:'top', cls:'title', html:this.mainComponent().labelForColumn(titleColumnIndex)});
		
		c =	this.mainComponent().parsedValues()[0].length;
		for (i=0; i<c; i++) {
			if ((i != titleColumnIndex) && (i != notesColumnIndex) && (this.mainComponent().isColumnSelected(i))) {
				var trimmedLabel;
				
				trimmedLabel = Clipperz.Base.trim(this.mainComponent().labelForColumn(i));
				fieldsHeaderCells.push({tag:'td', valign:'top', id:this.getId('fieldHeaderTD_' + i), cls:((trimmedLabel == "") ? 'missingLabelWarning' : (this.isColumnSetup(i) ? 'configuredColumn': 'unconfiguredColumn')), children:[
					{tag:'span', html:((trimmedLabel == "") ? Clipperz.PM.Strings['CSV_ImportWizard_Fields_MissingLabelWarning'] : trimmedLabel)/*, cls:((trimmedLabel == "") ? 'missingLabelWarning' : '')*/},
					{tag:'select', id:this.getId('select_' + i), name:i, children:[
						{tag:'option', value:'UNDEFINED', html:"select data type", cls:'disabledOption'},
						{tag:'option', value:'TXT', htmlString:Clipperz.PM.Strings['recordFieldTypologies']['TXT']['shortDescription']},
						{tag:'option', value:'PWD', htmlString:Clipperz.PM.Strings['recordFieldTypologies']['PWD']['shortDescription']},
						{tag:'option', value:'URL', htmlString:Clipperz.PM.Strings['recordFieldTypologies']['URL']['shortDescription']},
						{tag:'option', value:'DATE', htmlString:Clipperz.PM.Strings['recordFieldTypologies']['DATE']['shortDescription']},
						{tag:'option', value:'ADDR', htmlString:Clipperz.PM.Strings['recordFieldTypologies']['ADDR']['shortDescription']}
					]}
				]})
			}
		}

		if (notesColumnIndex != -1) {
			fieldsHeaderCells.push({tag:'td', valign:'top', cls:'notes', html:this.mainComponent().labelForColumn(notesColumnIndex)});
		}

		this.domHelper().append(this.element(), {tag:'div', children:[
			{tag:'div', cls:'importStepDescription', htmlString:Clipperz.PM.Strings['CSV_ImportWizard_Fields']},
			{tag:'div', id:this.getId('dataDiv'), children:[
				{tag:'div', children:[
				]},
				{tag:'div', cls:'csvImportPreview', children:[
					{tag:'table', id:this.getId('previewDada'), cls:'csvImportPreview', cellspacing:'0', children:[
						{tag:'thead', id:this.getId('previewData_thead'), children:[
							{tag:'tr', cls:'CSV_previewData_header', children:fieldsHeaderCells}
						]},
						{tag:'tbody', id:this.getId('previewData_tbody'), children:[]}
					]}
				]}
			]}
		]});
		
		for (i=0; i<c; i++) {
			if ((i != titleColumnIndex) && (i != notesColumnIndex) && (this.mainComponent().isColumnSelected(i))) {
				Clipperz.DOM.selectOptionMatchingValue(this.getDom('select_' + i), this.mainComponent().typeForColumn(i));
				MochiKit.Signal.connect(this.getDom('select_' + i), 'onchange', this, 'renderDataRowsHandler');
			}
		}
		
		this.renderDataRows(this.getElement('previewData_tbody'));
//		Clipperz.NotificationCenter.register(null, 'updatedCSVImportColumnHeader', this, 'renderDataRowsHandler');
	},

	//-------------------------------------------------------------------------

	'isColumnSetup': function(aColumnIndex) {
		return ((Clipperz.Base.trim(this.mainComponent().labelForColumn(aColumnIndex)) != "") && (this.mainComponent().typeForColumn(aColumnIndex) != 'UNDEFINED'));
	},

	//-------------------------------------------------------------------------

	'renderDataRowsHandler': function(anEvent) {
		var columnIndex;
		var tdElement;

//MochiKit.Logging.logDebug(">>> renderDataRowsHandler")
		columnIndex = anEvent.src().name;
		this.mainComponent().setTypeForColumn(anEvent.src().value, columnIndex);
		
		tdElement = this.getElement('fieldHeaderTD_' + columnIndex);
		
		if (this.isColumnSetup(columnIndex)) {
			tdElement.removeClass('unconfiguredColumn');
			tdElement.addClass('configuredColumn');
		} else {
			tdElement.addClass('unconfiguredColumn');
			tdElement.removeClass('configuredColumn');
		}
		
		this.renderDataRows(this.getElement('previewData_tbody'));
	},
	
	//-------------------------------------------------------------------------
	
	'renderDataRows': function(anElement) {
		var titleColumnIndex;
		var notesColumnIndex;
		var data
		var i,c;

//MochiKit.Logging.logDebug("#### >> renderDataRows");
//		anElement.update("");
		MochiKit.DOM.replaceChildNodes(anElement.dom);

		if (this.mainComponent().isFirstRowHeader()) {
			data = this.mainComponent().parsedValues().slice(1);
		} else {
			data = this.mainComponent().parsedValues();
		}
		
		
		titleColumnIndex = this.mainComponent().titleColumnIndex();
		notesColumnIndex = this.mainComponent().notesColumnIndex();
		
		c = data.length;
		for (i=0; i<c; i++) {
			var rowData;
			var rowConfig;
			var ii, cc;
			
			rowData = data[i];
			
			rowConfig = {tag:'tr', children:[
				{tag:'td', valign:'top', cls:'title', html:(MochiKit.Base.isNotEmpty(rowData[titleColumnIndex]) ? rowData[titleColumnIndex].replace(/\n/g, '<br>') : '&nbsp;')}
			]};

			cc = rowData.length;
			for (ii=0; ii<cc; ii++) {
//				if ((ii != titleColumnIndex) && (ii != notesColumnIndex)) {
				if ((ii != titleColumnIndex) && (ii != notesColumnIndex) && (this.mainComponent().isColumnSelected(ii))) {
					rowConfig.children.push({
						tag:'td',
						valign:'top',
						cls:(this.isColumnSetup(ii) ? 'configuredColumn' : 'unconfiguredColumn'),
						html:(MochiKit.Base.isNotEmpty(rowData[ii]) ? rowData[ii].replace(/\n/g, '<br>') : '&nbsp;')
					});
				}
			}
			if (notesColumnIndex != -1) {
				rowConfig.children.push({tag:'td', valign:'top', cls:'notes', html:(MochiKit.Base.isNotEmpty(rowData[notesColumnIndex]) ? rowData[notesColumnIndex].replace(/\n/g, '<br>') : '&nbsp;')});
			}
			
			this.domHelper().append(anElement, rowConfig);
		}

		Clipperz.Style.applyZebraStylesToTable(this.getId('previewDada'));

		this.checkWetherToEnableNextButton();
//MochiKit.Logging.logDebug("#### << renderDataRows");
	},

	//-------------------------------------------------------------------------

	'checkWetherToEnableNextButton': function() {
		var result;
		var titleColumnIndex;
		var notesColumnIndex;
		var i,c;

		titleColumnIndex = this.mainComponent().titleColumnIndex()
		notesColumnIndex = this.mainComponent().notesColumnIndex()

		result = true;
		c =	this.mainComponent().parsedValues()[0].length;
		for (i=0; i<c; i++) {
			if ((i != titleColumnIndex) && (i != notesColumnIndex) && (this.mainComponent().isColumnSelected(i))) {
				result = result && this.isColumnSetup(i);
			}
		}
		
		if (result) {
			this.mainComponent().nextButton().enable();
		} else {
			this.mainComponent().nextButton().disable();
		}
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

