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

Clipperz.PM.Components.Import.CSVImport.CSVImportTitle = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Import.CSVImport.CSVImportTitle.superclass.constructor.call(this, anElement, args);
	this._mainComponent = args.mainComponent;
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Import.CSVImport.CSVImportTitle, Clipperz.PM.Components.BaseComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Import.CSVImport.CSVImportTitle component";
	},

	//-------------------------------------------------------------------------

	'mainComponent': function() {
		return this._mainComponent;
	},

	//-------------------------------------------------------------------------

	'render': function() {
		var	titleSelectorCheckboxCells;
		var titleColumnIndex;
		var i,c;
		
		Clipperz.NotificationCenter.unregister(this);
		MochiKit.Signal.disconnectAllTo(this);

		this.element().update("");

		titleColumnIndex = this.mainComponent().titleColumnIndex()
		titleSelectorCheckboxCells = [];
		c =	this.mainComponent().parsedValues()[0].length;
		for (i=0; i<c; i++) {
			if (this.mainComponent().isColumnSelected(i)) {
				titleSelectorCheckboxCells.push({tag:'th', valign:'top', id:this.getId('th_' + i), children:[
					{tag:'input', type:'radio', id:this.getId('radio_' + i), name:'CSVImportTitleColumn', value:i},
					{tag:'span', cls:'clickableSpan', id:this.getId('columnLabel_' + i), html:this.mainComponent().labelForColumn(i)}
				]})
			}
		}
		
		if (titleColumnIndex >= titleSelectorCheckboxCells.length) {
			this.mainComponent().setTitleColumnIndex(-1);
		}
		
		this.domHelper().append(this.element(), {tag:'div', children:[
			{tag:'div', cls:'importStepDescription', htmlString:Clipperz.PM.Strings['CSV_ImportWizard_Title']},
			{tag:'div', id:this.getId('dataDiv'), cls:'csvImportPreview', children:[
				{tag:'table', id:this.getId('previewDada'), cls:'csvImportPreview', cellspacing:'0', children:[
					{tag:'thead', id:this.getId('previewData_thead'), children:[
						{tag:'tr', children:titleSelectorCheckboxCells}
					]},
					{tag:'tbody', id:this.getId('previewData_tbody'), children:[]}
				]}
			]}
		]});
		
		this.renderData(this.getElement('previewData_tbody'), this.mainComponent().parsedValues());

		c =	this.mainComponent().parsedValues()[0].length;
		for (i=0; i<c; i++) {
			if (this.mainComponent().isColumnSelected(i)) {
				MochiKit.Signal.connect(this.getDom('radio_' + i), 'onclick', this, 'renderDataHandler');
				if (Clipperz_IEisBroken != true) {
					MochiKit.Signal.connect(this.getDom('columnLabel_' + i), 'onclick', this.getDom('radio_' + i), 'click');
				}
			}
		}
		
		if (titleColumnIndex != -1) {
			this.getDom('radio_' + titleColumnIndex).click();
		} else {
			this.mainComponent().nextButton().disable();
		}
		
	},

	//-------------------------------------------------------------------------
	
	'renderData': function(anElement, someData) {
		var data;
		var config;
		var titleColumnIndex;
		var i,c;
		
//		anElement.update("");
		MochiKit.DOM.replaceChildNodes(anElement.dom);

		titleColumnIndex = this.mainComponent().titleColumnIndex()
		
		if (this.mainComponent().isFirstRowHeader()) {
			data = someData.slice(1);
		} else {
			data = someData;
		}

		config = MochiKit.Base.map(MochiKit.Base.bind(function(aRowData) {
			var result;
			var i,c;
			
			result = {tag:'tr', children:[]};
			c = aRowData.length;
			for (i=0; i<c; i++) {
				if (this.mainComponent().isColumnSelected(i)) {
					var field;
				
					field = aRowData[i];
					result.children.push({tag:'td', valign:'top', cls:((titleColumnIndex == i) ? 'titleColumn': ''), html:(MochiKit.Base.isNotEmpty(field) ? field.replace(/\n/g, '<br>') : '&nbsp;')});
				}
			}
			
			return result;
		}, this), data);
		
		MochiKit.Base.map(function(aRowConfig) {Clipperz.YUI.DomHelper.append(anElement, aRowConfig);}, config);

		Clipperz.Style.applyZebraStylesToTable(this.getId('previewDada'));
	},

	//-------------------------------------------------------------------------

	'renderDataHandler': function(anEvent) {
		var i,c;
		
		this.mainComponent().setTitleColumnIndex(anEvent.src().value);

		c = this.mainComponent().parsedValues()[0].length;
		for (i=0; i<c; i++) {
			if (this.mainComponent().isColumnSelected(i)) {
				this.getElement('th_' + i).removeClass('titleColumn');
			}
		}
		this.getElement('th_' + anEvent.src().value).addClass('titleColumn');
		
		if (anEvent.src().value != -1) {
			this.mainComponent().nextButton().enable();
		} else {
			this.mainComponent().nextButton().disable();
		}
		
		this.renderData(this.getElement('previewData_tbody'), this.mainComponent().parsedValues());
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

