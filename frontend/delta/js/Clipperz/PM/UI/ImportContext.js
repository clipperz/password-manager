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

"use strict";
Clipperz.Base.module('Clipperz.PM.UI');

Clipperz.PM.UI.ImportContext = function(args) {
	
	this.inputString = null;
		
	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.ImportContext.prototype, {

	'toString': function() {
		return "Clipperz.PM.UI.ImportContext";
	},

	//=============================================================================
	
	'resetContext': function() {
		delete this.inputString;
		delete this.format;
		delete this.jsonToImport;
		delete this.recordsToImport;
	},
	
	'getInitialJsonContext': function(aJsonList) {
		return {
			'format': 'json',
			'jsonToImport': aJsonList,
			'recordsToImport': aJsonList.map(function(d){return d._importId})
		};
	},
	
	'getInitialCsvContext': function(aCsvTable) {
		var result;
		var nColumns;
		var defaultSelectedColumns;
		var defaultHiddenColumns;
		var defaultColumnLabels;
		var columnLabelsFirstrow;
		var i;

		nColumns = aCsvTable[0].length;
		
		defaultSelectedColumns = {};
		defaultHiddenColumns = {};
		defaultColumnLabels = {};
		columnLabelsFirstrow = {};
		for (i=0; i<nColumns; i++) {
			defaultSelectedColumns[i] = true;
			defaultHiddenColumns[i] = false;
			defaultColumnLabels[i] = "";
			columnLabelsFirstrow[i] = aCsvTable[0][i];
		}
	
		return {
			'format': 'csv',
			'parsedCsv': aCsvTable,
			'nColumns': nColumns,
			'selectedColumns': defaultSelectedColumns,
			'firstRowAsLabels': false,
			'columnLabels': defaultColumnLabels,
			'columnLabelsFirstrow': columnLabelsFirstrow,
			'titlesColumn': null,
			'notesColumn': null,
			'hiddenColumns': defaultHiddenColumns,
		};
	},
	
	'getCsvLabels': function() {
		return (this.firstRowAsLabels) ? this.columnLabelsFirstrow : this.columnLabels;
	},
	
	'processCsv': function() {
		var jsonToImport;
		var recordsToImport;
		var columnLabels = this.getCsvLabels();
		
		jsonToImport = [];
		
		for (rowCount=0; rowCount<this.parsedCsv.length; rowCount++) {
			var rowCount,cellCount;
			
			if (rowCount != 0 || ! this.firstRowAsLabels) {
				var record;
				
				record = {};
				record._importId = rowCount;
				record.label = this.parsedCsv[rowCount][this.titlesColumn];
				record.data = {'notes': ""};
				record.currentVersion = {'fields': {}};
				
				for (cellCount=0; cellCount<this.parsedCsv[rowCount].length; cellCount++) {
					if (this.selectedColumns[cellCount] && cellCount != this.notesColumn && cellCount != this.titlesColumn) {
						var fieldKey = rowCount+"-"+cellCount;
						var field = {
							'label': columnLabels[cellCount],
							'value': this.parsedCsv[rowCount][cellCount],
							'hidden': this.hiddenColumns[cellCount]
						};
						record.currentVersion.fields[fieldKey] = field;
					} else if (cellCount == this.notesColumn) {
						record.data.notes = this.parsedCsv[rowCount][cellCount];
					}
				}
				
				jsonToImport.push(record);
			}
		}
		
		if (typeof(this.recordsToImport) == 'undefined') {
			recordsToImport = MochiKit.Base.map(function(r){return r._importId},jsonToImport);
		} else {
			recordsToImport = this.recordsToImport;
		}
		
		return {
			'jsonToImport': jsonToImport,
			'recordsToImport': recordsToImport
		};
	},
	
	//=============================================================================
	__syntaxFix__: "syntax fix"
});
