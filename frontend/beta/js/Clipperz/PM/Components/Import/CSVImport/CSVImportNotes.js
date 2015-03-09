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

Clipperz.PM.Components.Import.CSVImport.CSVImportNotes = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Import.CSVImport.CSVImportNotes.superclass.constructor.call(this, anElement, args);
	this._mainComponent = args.mainComponent;
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Import.CSVImport.CSVImportNotes, Clipperz.PM.Components.BaseComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Import.CSVImport.CSVImportNotes component";
	},

	//-------------------------------------------------------------------------

	'mainComponent': function() {
		return this._mainComponent;
	},

	//-------------------------------------------------------------------------

	'render': function() {
		var	notesSelectorCheckboxCells;
		var totalNumberOfColumns;
		var titleColumnIndex;
		var notesColumnIndex;
		var i,c;

		Clipperz.NotificationCenter.unregister(this);
		MochiKit.Signal.disconnectAllTo(this);

		this.element().update("");

		titleColumnIndex = this.mainComponent().titleColumnIndex()
		notesColumnIndex = this.mainComponent().notesColumnIndex()

		totalNumberOfColumns = this.mainComponent().parsedValues()[0].length;
		
		notesSelectorCheckboxCells = [{tag:'th', cls:'title', html:this.mainComponent().labelForColumn(titleColumnIndex)}];
		c =	totalNumberOfColumns;
		for (i=0; i<c; i++) {
			if ((i != titleColumnIndex) && (this.mainComponent().isColumnSelected(i))) {
				notesSelectorCheckboxCells.push({tag:'th', id:this.getId('th_' + i), valign:'top', children:[
					{tag:'input', type:'radio', id:this.getId('radio_' + i), name:'CSVImportNotesColumn', value:i},
					{tag:'span', cls:'clickableSpan', id:this.getId('columnLabel_' + i), html:this.mainComponent().labelForColumn(i)}
				]})
			}
		}

		this.domHelper().append(this.element(), {tag:'div', children:[
			{tag:'div', cls:'importStepDescription', htmlString:Clipperz.PM.Strings['CSV_ImportWizard_Notes']},
			{tag:'div', id:this.getId('dataDiv'), children:[
				{tag:'div', cls:'importStepParameters', children:[
					{tag:'input', id:this.getId('doNotSetNotes_radio'), type:'radio', name:'CSVImportNotesColumn', value:-1},
					{tag:'span', id:this.getId('doNotSetNotes_span'), cls:'clickableSpan', htmlString:Clipperz.PM.Strings['CSV_ImportWizard_Notes_Settings_noSelectionLabel']}
				]},
				{tag:'div', cls:'csvImportPreview', children:[
					{tag:'table', id:this.getId('previewDada'), cls:'csvImportPreview', cellspacing:'0', children:[
						{tag:'thead', id:this.getId('previewData_thead'), children:[
							{tag:'tr', children:notesSelectorCheckboxCells}
						]},
						{tag:'tbody', id:this.getId('previewData_tbody'), children:[]}
					]}
				]}
			]}
		]});
		
		this.renderData(this.getElement('previewData_tbody'), this.mainComponent().parsedValues());

		if ((notesColumnIndex >= totalNumberOfColumns) || (notesColumnIndex == titleColumnIndex) || !(this.mainComponent().isColumnSelected(notesColumnIndex))) {
			this.mainComponent().setNotesColumnIndex(-1);
			notesColumnIndex = -1;
		}
		
		c =	totalNumberOfColumns;
		for (i=0; i<c; i++) {
			if ((i != titleColumnIndex) && (this.mainComponent().isColumnSelected(i))) {
				MochiKit.Signal.connect(this.getDom('radio_' + i), 'onclick', this, 'renderDataHandler');
				if (Clipperz_IEisBroken != true) {
					MochiKit.Signal.connect(this.getDom('columnLabel_' + i), 'onclick', this.getDom('radio_' + i), 'click');
				}
			}
		}

		MochiKit.Signal.connect(this.getDom('doNotSetNotes_radio'), 'onclick', this, 'renderDataHandler');
		if (Clipperz_IEisBroken != true) {
			MochiKit.Signal.connect(this.getDom('doNotSetNotes_span'), 'onclick', this.getDom('doNotSetNotes_radio'), 'click');
		}

		if (notesColumnIndex == -1) {
			this.getDom('doNotSetNotes_radio').click();
		} else {
			this.getDom('radio_' + notesColumnIndex).click();
		}
	},

	//-------------------------------------------------------------------------
	
	'renderData': function(anElement, someData) {
		var data;
		var config;
		var titleColumnIndex;
		var notesColumnIndex;
		var i,c;
		
//		anElement.update("");
		MochiKit.DOM.replaceChildNodes(anElement.dom);

		titleColumnIndex = this.mainComponent().titleColumnIndex();
		notesColumnIndex = this.mainComponent().notesColumnIndex();
		
		if (this.mainComponent().isFirstRowHeader()) {
			data = someData.slice(1);
		} else {
			data = someData;
		}

		
//		config = [{tag:'tr', cls:'CSV_previewData_header', children:[{tag:'td', valign:'top', html:header[titleColumnIndex], cls:'title'}]}];
//		c = header.length;
//		for (i=0; i<c; i++) {
//			if (i != titleColumnIndex) {
//				config[0].children.push({tag:'td', valign:'top', html:header[i], cls:((notesColumnIndex == i) ? 'notesColumn': '')})
//			}
//		}
		
		config = MochiKit.Base.map(MochiKit.Base.bind(function(aTitleColumnIndex, aRowData) {
			var result;
			var i,c;
			
			result = {tag:'tr', children:[{tag:'td', valign:'top', cls:'title', html:(MochiKit.Base.isNotEmpty(aRowData[aTitleColumnIndex]) ? aRowData[aTitleColumnIndex].replace(/\n/g, '<br>') : '&nbsp;')}]};
			c = aRowData.length;
			for (i=0; i<c; i++) {
				if ((i != titleColumnIndex) && (this.mainComponent().isColumnSelected(i))) {
					result.children.push({tag:'td', valign:'top', cls:((notesColumnIndex == i) ? 'notesColumn': ''), html:(MochiKit.Base.isNotEmpty(aRowData[i]) ? aRowData[i].replace(/\n/g, '<br>') : '&nbsp;')});
				}
			}
			
			return result;
		}, this, titleColumnIndex), data);
		
		MochiKit.Base.map(function(aRowConfig) {Clipperz.YUI.DomHelper.append(anElement, aRowConfig);}, config);

		Clipperz.Style.applyZebraStylesToTable(this.getId('previewDada'));
	},

	//-------------------------------------------------------------------------

	'renderDataHandler': function(anEvent) {
		var titleColumnIndex;
		var i,c;
		
		this.mainComponent().setNotesColumnIndex(anEvent.src().value);
		titleColumnIndex = this.mainComponent().titleColumnIndex();

		c = this.mainComponent().parsedValues()[0].length;
		for (i=0; i<c; i++) {
			if ((i != titleColumnIndex) && (this.mainComponent().isColumnSelected(i))) {
				this.getElement('th_' + i).removeClass('notesColumn');
			}
		}
		if (anEvent.src().value != -1) {
			this.getElement('th_' + anEvent.src().value).addClass('notesColumn');
		}

		this.renderData(this.getElement('previewData_tbody'), this.mainComponent().parsedValues());
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

