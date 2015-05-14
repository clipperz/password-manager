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
Clipperz.Base.module('Clipperz.PM.UI.Components.ExtraFeatures');

Clipperz.PM.UI.Components.ExtraFeatures.DataImportClass = React.createClass({

	getInitialState: function() {
		return {
			'currentStep': 'input',
			'importData': {'input': ""},
			'recordsToImport': null
		};
	},
	
	goToStep: function(aStep) {
		this.setState({'currentStep': aStep});
	},
	
	resetState: function() {
		this.replaceState( this.getInitialState() );
	},

	//=========================================================================
	
	addImportIds: function (someJson) {
		var count;
		
		for (count=0; count < someJson.length; count++) {
			someJson[count]['_importId'] = count;
		}
	},
	
	toggleRecordToImport: function(record) {
		var newRecordsToImport;
		var recordPosition;

		newRecordsToImport = this.state.recordsToImport;
		recordPosition = newRecordsToImport.indexOf(record._importId);
		
		if (recordPosition === -1) {
			newRecordsToImport.push(record._importId);
		} else {
			newRecordsToImport.splice(recordPosition,1);
		}
		
		this.setState({'recordsToImport': newRecordsToImport});
	},
	
	isRecordToImport: function(record) {
		return (this.state.recordsToImport.indexOf(record._importId)>=0) ? true : false;
	},

	extractJsonFromClipperzExport: function(someHtml) {
		var temporaryTextarea;
		var regexMatch;
		var result;
		
		// Should move the regex to global?
		var re = new RegExp('.*<textarea>(.*)<\/textarea>.*','g');
		
		if (re.test(someHtml)) {
			// Needed to escape HTML entities
			temporaryTextarea = document.createElement('textarea');
			temporaryTextarea.innerHTML = someHtml.replace(re, '$1');
			result = temporaryTextarea.innerHTML;
		} else {
			result = false;
		}

		return result;
	},
	
	parseJson: function(aJsonString) {
		var result;
		var jsonData;
		
		try {
			jsonData = JSON.parse(aJsonString);
			this.addImportIds(jsonData);
			result = {
				'importData': {
					'format': 'json',
					'input': aJsonString,
				},
				'jsonToImport': jsonData,
				'recordsToImport': jsonData.map(function(d){return d._importId}),
				'currentStep': 'preview',
				'previousStep': 'input'
			};
		} catch(e) {
			result = false;
		}
		
		return result;
	},
	
	parseCsv: function(aCsvString) {
		var result;
		var parsedCSV;
		var nColumns;
		var defaultSelectedColumns;
		var defaultHiddenColumns;
		var defaultColumnLabels;
		var columnLabelsFirstrow;
		var i;
	
		var papaParsedCSV = Papa.parse(aCsvString);
	
		event.preventDefault();
	
		if (papaParsedCSV.errors.length != 0) {
			result = false;
		} else {
			parsedCSV = this.csvFillEmptyCells(papaParsedCSV.data);
			nColumns = parsedCSV[0].length;
			
			defaultSelectedColumns = {};
			defaultHiddenColumns = {};
			defaultColumnLabels = {};
			columnLabelsFirstrow = {};
			for (i=0; i<nColumns; i++) {
				defaultSelectedColumns[i] = true;
				defaultHiddenColumns[i] = false;
				defaultColumnLabels[i] = "";
				columnLabelsFirstrow[i] = parsedCSV[0][i];
			}
		
			result = {
				'importData': {
					'format': 'csv',
					'input': aCsvString,
					'parsedCSV': parsedCSV,
					'nColumns': nColumns,
					'selectedColumns': defaultSelectedColumns,
					'firstRowAsLabels': false,
					'columnLabels': defaultColumnLabels,
					'columnLabelsFirstrow': columnLabelsFirstrow,
					'titlesColumn': null,
					'notesColumn': null,
					'hiddenColumns': defaultHiddenColumns,
					'json': []
				},
				'currentStep': 'csv-columns'
			};
		}

		return result;
	},

	csvFillEmptyCells: function(table) {
		var i,j;
		
		var result = [];
		
		var maxColumns = MochiKit.Iter.reduce(function(prev,next) {
			return Math.max(prev,next)
		}, MochiKit.Base.map(function(row) {return row.length;}, table) );
		
		for (i=0; i<table.length; i++) {
			
			result[i] = [];
			for (j=0; j<maxColumns; j++) {
				result[i][j] = (typeof(table[i][j]) != "undefined") ? table[i][j] : "";
			}
		}
		
		return result;
	},
	
	csvGetColumnLabels: function() {
		return (this.state.importData.firstRowAsLabels) ? this.state.importData.columnLabelsFirstrow : this.state.importData.columnLabels;
	},
	
	csvToJson: function() {
		var result;
		
		var importData = this.state.importData;
		var columnLabels = this.csvGetColumnLabels();
		
		result = [];
		
		for (rowCount=0; rowCount<importData.parsedCSV.length; rowCount++) {
			var rowCount,cellCount;
			
			if (rowCount != 0 || ! importData.firstRowAsLabels) {
				var record;
				
				record = {};
				record._importId = rowCount;
				record.label = importData.parsedCSV[rowCount][importData.titlesColumn];
				record.data = {'notes': ""};
				record.currentVersion = {'fields': {}};
				
				for (cellCount=0; cellCount<importData.parsedCSV[rowCount].length; cellCount++) {
					if (importData.selectedColumns[cellCount] && cellCount != importData.notesColumn && cellCount != importData.titlesColumn) {
						var fieldKey = rowCount+"-"+cellCount;
						var field = {
							'label': columnLabels[cellCount],
							'value': importData.parsedCSV[rowCount][cellCount],
							'hidden': importData.hiddenColumns[cellCount]
						};
						record.currentVersion.fields[fieldKey] = field;
					} else if (cellCount == importData.notesColumn) {
						record.data.notes = importData.parsedCSV[rowCount][cellCount];
					}
				}
				
				result.push(record);
			}
		}
		
		return result;
	},
	
	//=========================================================================
	
	csvRenderTbody: function() {
		var rowCount;
		var cellCount;
		
		var firstRowAsLabels = this.state.importData.firstRowAsLabels;
		var selectedColumns = this.state.importData.selectedColumns;
		
		rowCount = 0;
		return MochiKit.Base.map(function(row){
			var result;
			
			cellCount = 0;
			
			if (rowCount == 0 && firstRowAsLabels) {
				result = null;
			} else {							
				result = React.DOM.tr({'key': 'csv-row-'+(rowCount)}, MochiKit.Base.map(function(cell) {
					return (selectedColumns[cellCount]) ? React.DOM.td({'key': 'csv-cell-'+rowCount+'-'+(cellCount++)},cell) : null;
				}, row));
			}
			
			rowCount++;
			
			return result;
		}, this.state.importData.parsedCSV);
	},
	
	csvRenderTheadInput: function(stepName, inputType, valueCallback, onChange, disabledCallback, showLabels) {
		var cellCount;
		
		var importData = this.state.importData;
		
		cellCount = 0;
		return React.DOM.tr({},
			MochiKit.Base.map(MochiKit.Base.bind(function(cell) {
				var result;
				
				var columnLabels = (importData.firstRowAsLabels) ? importData.columnLabelsFirstrow : importData.columnLabels;
				var inputLabel = (showLabels) ? React.DOM.label({'htmlFor': 'csv-'+stepName+'-input-'+cellCount}, columnLabels[cellCount]) : null;
				
				if (! importData.selectedColumns[cellCount]) {
					result = null;
				} else {
					var inputProps = {
						'type': inputType,
						'id': 'csv-'+stepName+'-input-'+cellCount,
						'key': 'csv-'+stepName+'-input-'+cellCount,
						'ref': 'csv-'+stepName+'-input-'+cellCount,
						'onChange': MochiKit.Base.partial(onChange,cellCount)
					}
					
					if (inputType == 'radio' || inputType == 'checkbox') {
						inputProps['checked'] = MochiKit.Base.partial(valueCallback,cellCount)();
					} else {
						inputProps['value'] = MochiKit.Base.partial(valueCallback,cellCount)();
					}
					
					if (disabledCallback) {
						inputProps['disabled'] = MochiKit.Base.partial(disabledCallback,cellCount)();
					}
					
					result = React.DOM.th({'key': 'csv-'+stepName+'-header-'+cellCount}, [
						inputLabel,
						React.DOM.input(inputProps)
					]);
				}
				
				cellCount++;
				
				return result;
			}, this), this.state.importData.parsedCSV[0])
		)
	},
	
	setStateCB: function(aState) {
		this.setState(aState);
	},
	
	_renderStepMethods: {
		'input': function() {
			return new Clipperz.PM.UI.Components.ExtraFeatures.DataImport.Input({
				'importState': this.state,
				'setImportStateCallback': this.setStateCB,
				'goToStepCallback': this.goToStep,
				'extractJsonFromClipperzExportCallback': this.extractJsonFromClipperzExport,
				'parseJsonCallback': this.parseJson,
				'parseCsvCallback': this.parseCsv
			});
		},
		
		'csv-columns': function() {
			return new Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvColumns({
				'importState': this.state,
				'setImportStateCallback': this.setStateCB,
				'goToStepCallback': this.goToStep,
			});
		},
		
		'csv-labels': function() {
			return new Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvLabels({
				'importState': this.state,
				'setImportStateCallback': this.setStateCB,
				'goToStepCallback': this.goToStep,
				'csvRenderTheadInputCallback': this.csvRenderTheadInput,
				'csvRenderTbodyCallback': this.csvRenderTbody,
				'csvGetColumnLabelsCallback': this.csvGetColumnLabels
			});
		},
		
		'csv-titles': function() {
			return new Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvTitles({
				'importState': this.state,
				'setImportStateCallback': this.setStateCB,
				'goToStepCallback': this.goToStep,
				'csvRenderTheadInputCallback': this.csvRenderTheadInput,
				'csvRenderTbodyCallback': this.csvRenderTbody,
			});
		},
		
		'csv-notes': function() {
			return new Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvNotes({
				'importState': this.state,
				'setImportStateCallback': this.setStateCB,
				'goToStepCallback': this.goToStep,
				'csvRenderTheadInputCallback': this.csvRenderTheadInput,
				'csvRenderTbodyCallback': this.csvRenderTbody,
			});
		},
		
		'csv-hidden': function() {
			return new Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvHidden({
				'importState': this.state,
				'setImportStateCallback': this.setStateCB,
				'goToStepCallback': this.goToStep,
				'csvRenderTheadInputCallback': this.csvRenderTheadInput,
				'csvRenderTbodyCallback': this.csvRenderTbody,
				'csvToJsonCallback': this.csvToJson
			});
		},
		
		//-------------------------------------------------------------------------
		
		'preview': function() {
			return new Clipperz.PM.UI.Components.ExtraFeatures.DataImport.Preview({
				'importState': this.state,
				'resetImportStateCallback': this.resetState,
				'goToStepCallback': this.goToStep,
				'isRecordToImportCallback': this.isRecordToImport,
				'toggleRecordToImportCallback': this.toggleRecordToImport
			});
		},
	},
	
	
	//=========================================================================
	
	renderStep: function(step) {
		return MochiKit.Base.method(this, this._renderStepMethods[step])();
	},

	render: function () {
		return	React.DOM.div({className:'extraFeature'}, [
			React.DOM.h1({}, "Import"),
			this.renderStep(this.state.currentStep)
		]);
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImportClass);
