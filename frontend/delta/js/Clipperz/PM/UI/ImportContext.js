/*

Copyright 2008-2018 Clipperz Srl

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

Clipperz.PM.UI.ImportContext = function(anInputComponent) {
	this._importComponent = anInputComponent;

	this._status = {
		'inputString': '',
		'isInputStringValid': false,
		'inputFormat': 'UNDEFINED',
		'currentStep': 'Input',
		'useImportTag': true,
		'importTag': Clipperz.PM.UI.ImportContext.getDefaultImportTag()
	};
	
	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.ImportContext.prototype, {

	toString: function() {
		return "Clipperz.PM.UI.ImportContext";
	},

	release: function () {
		this._importComponent = null;
	},

	//=============================================================================

	ensureStateConsistency: function () {
		var	csvData;
		
		csvData = this._status['csvData'];
		if (csvData != null) {
			if (csvData['titleIndex'] == csvData['notesIndex']) {
				csvData['notesIndex'] = null;
			}
			
			csvData['hiddenFields'][csvData['titleIndex']] = false;
			csvData['hiddenFields'][csvData['notesIndex']] = false;
		}
	},

	updateImportComponent: function () {
		this._importComponent.setState({'importContext': this});
	},

	//=============================================================================

	state: function (aKeyPath) {
		var	result;
		var	keys;
		var	i, c;

		result = this._status;
		keys = aKeyPath.split('.');
		c = keys.length;
		
		for (i=0; i<c; i++) {
			result = result[keys[i]];
		}

		return result;
	},
	
	setState: function (aKeyPath, aValue) {
		var	object;
		var	keys;
		var i, c;
		
		object = this._status;
		keys = aKeyPath.split('.');
		c = keys.length - 1;
		
		for (i=0; i<c; i++) {
			object = object[keys[i]];
		}
		object[keys[c]] = aValue;

		this.ensureStateConsistency();
		this.updateImportComponent();
	},

	//=============================================================================

	stepStatus: function (aStep) {
		var result;
		
		if (aStep == this.currentStep()) {
			result = 'active';
		} else {
			result = 'disabled';
		}
		
		return result;
	},

	currentStep: function () {
		return this.state('currentStep');
	},
	
	setCurrentStep: function (aValue) {
		this.setState('currentStep', aValue);
	},

	currentStepIndex: function () {
		return MochiKit.Base.findValue(this.steps(), this.currentStep());
	},
	
	steps: function () {
		var	result;
		
		if (this.inputFormat() == 'JSON') {
			result = ['Input', 'Preview', 'Import'];
		} else if (this.inputFormat() == 'CSV') {
			result = ['Input', 'CSV.Columns', 'CSV.Labels', 'CSV.Titles', 'CSV.Notes', 'CSV.Hidden', 'Preview', 'Import'];
		} else {
			result = ['Input'];
		}

		return result;
	},

	//=============================================================================

	inputFormat: function () {
		return this.state('inputFormat');
	},

	setInputFormat: function (aValue) {
		this.setState('inputFormat', aValue);
		
		if (aValue == 'UNDEFINED') {
			this.setIsInputStringValid(false);
		} else {
			this.setIsInputStringValid(true);
		}
	},

	//-----------------------------------------------------------------------------

	isInputStringValid: function () {
		return this.state('isInputStringValid');
	},

	setIsInputStringValid: function (aValue) {
		this.setState('isInputStringValid', aValue);
	},

	//=============================================================================

	showJsonPreview: function (jsonData) {
		MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'setCurrentStep', 'Preview'));
	},

	setJsonData: function (someData) {
		if (someData != null) {
			this.setInputFormat('JSON');
		}
		this.setState('jsonData', someData);
		//	TODO: before setting 'recordsToImport', filter 'someData' to remove cards marked as ARCHIVED
		this.setState('recordsToImport', someData);
	},

	enhanceJsonDataWithCardReferences: function (someJsonData) {
		return MochiKit.Base.map(function (item) {
			item['reference'] = Clipperz.PM.Crypto.randomKey();
//			item['label'] = "COPY - " + item['label'];
			return item;
		}, someJsonData);
	},
	
	enhanceJsonDataWithImportTag: function(someJsonData, aTag) {
		return MochiKit.Base.map(function (item) {
			item['label'] = item['label'] + ' ' + Clipperz.PM.DataModel.Record.tagChar + aTag;
			return item;
		}, someJsonData);
	},

	//-----------------------------------------------------------------------------

	startCsvWizard: function (csvData) {
		MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'setCurrentStep', 'CSV.Columns'));
	},

	setCsvData: function (someData) {
		if (someData != null) {
			this.setInputFormat('CSV');

			this.setState('csvData', {
				'data': someData,
				'selectedColumns': MochiKit.Base.map(function () { return true; }, someData[0]),
				'labels': MochiKit.Base.map(function () { return ""; }, someData[0]),
				'useFirstRowAsLabels': false,
				'titleIndex': null,
				'notesIndex': null,
				'hiddenFields': MochiKit.Base.map(function () { return false; }, someData[0]),
			});
		} else {
			this.setState('csvData', null);
		}
	},

	csvFillEmptyCells: function(table) {
		var result = [];
		var i,j;

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

	//............................................................................

	'createJsonDataFromCSV': function(csvData) {
		return MochiKit.Base.map(function (row) {
			var	fields;

			fields = MochiKit.Base.map(function (cellInfo) {
				return {
					'label': csvData['labels'][cellInfo[0]],
					'value': cellInfo[1],
					'hidden': csvData['hiddenFields'][cellInfo[0]],
				}
			},	MochiKit.Base.filter(function (cellInfo) {
					return ((csvData['titleIndex'] != cellInfo[0]) && (csvData['notesIndex'] != cellInfo[0]) && (csvData['selectedColumns'][cellInfo[0]]));
				}, Clipperz.Base.zipWithRange(row))
			);
			
			return {
				'reference': Clipperz.PM.Crypto.randomKey(),
				'label': row[csvData['titleIndex']],
				'data': {
					'notes': ((csvData['notesIndex'] != null) ? row[csvData['notesIndex']] : "")
				},
				'currentVersion': {
					'fields': MochiKit.Iter.reduce(function (accumulator, field) {
						accumulator[Clipperz.PM.Crypto.randomKey()] = field; return accumulator;
					}, fields, {})
				}
			};
		}, (csvData['useFirstRowAsLabels']) ? csvData['data'].slice(1) : csvData['data']);
	},

	//=============================================================================

	inputString: function () {
		return this.state('inputString');
	},

	setInputString: function (aValue, isUploadingFile) {
		var textarea;
		var result;

		result = aValue;
		this.setInputFormat('UNDEFINED');
		this.setJsonData(null);
		this.setCsvData(null);

		if (isUploadingFile) {
			var isExportContent;
			
			isExportContent = new RegExp('[\\s\\S]*<textarea>([\\s\\S]*)<\/textarea>[\\s\\S]*', 'g');
			if (isExportContent.test(aValue)) {
				textarea = MochiKit.DOM.TEXTAREA();
				textarea.innerHTML = aValue.replace(isExportContent, '$1');
				result = textarea.innerHTML.replace(/&amp;/g, '&');
			}
		}

		try {
			var	jsonData;

			jsonData = JSON.parse(result);
			jsonData = this.enhanceJsonDataWithCardReferences(jsonData);

			this.setJsonData(jsonData);
			if (isUploadingFile == true) {
				this.showJsonPreview();
			}
		} catch(e) {
			var parsedCsv;
			
			parsedCsv = Papa.parse(result);
			if (parsedCsv.errors.length == 0) {
				var csvData;
				csvData = this.csvFillEmptyCells(parsedCsv.data);

				this.setCsvData(csvData);
				if (isUploadingFile == true) {
					this.startCsvWizard(csvData);
				}
			}
		}

		this.setState('inputString', result);
		return result;
	},

	//=============================================================================

	backButtonStatus: function () {
		var result;
		
		result = 'DISABLED';
		if (this.currentStepIndex() > 0) {
			result = 'ENABLED';
		}

		return result;
	},

	//.............................................................................

	forwardButtonStatus: function () {
		var result;
		
		result = 'DISABLED';
		
		if (this.currentStep() == 'Input') {
			if (this.isInputStringValid()) {
				result = 'ENABLED';
			}
		} else if (this.currentStep() == 'Preview') {
			if (this.state('recordsToImport').length > 0) {
				result = 'ENABLED';
			}
		} else if (this.currentStep() == 'CSV.Columns') {
			if (MochiKit.Iter.some(this.state('csvData.selectedColumns'), MochiKit.Base.operator.identity)) {
				result = 'ENABLED';
			}
		} else if (this.currentStep() == 'CSV.Labels') {
			var	selectedColumns = this.state('csvData.selectedColumns');
			if (MochiKit.Iter.every(Clipperz.Base.zipWithRange(this.state('csvData.labels')), function (labelInfo) { return (Clipperz.Base.trim(labelInfo[1]).length > 0) || (selectedColumns[labelInfo[0]] == false)})) {
				result = 'ENABLED';
			}
		} else if (this.currentStep() == 'CSV.Titles') {
			if ((this.state('csvData.titleIndex') != null) && (this.state('csvData.selectedColumns')[this.state('csvData.titleIndex')] == true)) {
				result = 'ENABLED';
			}
		} else if (this.currentStep() == 'CSV.Notes') {
			result = 'ENABLED';
		} else if (this.currentStep() == 'CSV.Hidden') {
			result = 'ENABLED';
		}

		return result;
	},

	//=============================================================================

	goBackHandler: function () {
		return MochiKit.Base.bind(function (anEvent) {
			if (this.backButtonStatus() == 'ENABLED') {
				this.goBack();
			}
		}, this);
	},

	goForwardHandler: function () {
		return MochiKit.Base.bind(function (anEvent) {
			if (this.forwardButtonStatus() == 'ENABLED') {
				this.goForward();
			}
		}, this);
	},

	//=============================================================================

	goBack: function () {
		this.setCurrentStep(this.steps()[this.currentStepIndex() - 1]);
	},

	goForward: function () {
		if (this.currentStep() == 'CSV.Hidden') {
			var	jsonData;

			jsonData = this.createJsonDataFromCSV(this.state('csvData'));
			this.setState('jsonData', jsonData);
			this.setState('recordsToImport', jsonData);
		}

		this.setCurrentStep(this.steps()[this.currentStepIndex() + 1]);
	},

	//=============================================================================

	renderCsvTableBody: function (hideDeselectedColumns) {
		var	importContext = this;

		return React.DOM.tbody({}, MochiKit.Base.map(function (rowInfo) {
			var result;
			var	rowIndex = rowInfo[0];
			var	row = rowInfo[1]

			result = React.DOM.tr({'key': 'csv-row-' + rowIndex},
				MochiKit.Base.map(function (cellInfo) {
					var result;
					var	columnIndex = cellInfo[0];
					var	columnValue = cellInfo[1];
				
					if (importContext.state('csvData.selectedColumns')[columnIndex] || !hideDeselectedColumns) {
						result = React.DOM.td({
							'key':'csv-cell-' + rowIndex + '-' + columnIndex,
							'className':(importContext.state('csvData.hiddenFields')[columnIndex]) ? 'PASSWORD' : null
						}, columnValue);
					} else{
						result = null;
					}
				
					return  result;
				}, Clipperz.Base.zipWithRange(row))
			);
			
			return result;
		}, Clipperz.Base.zipWithRange((importContext.state('csvData.useFirstRowAsLabels')) ? importContext.state('csvData.data').slice(1) : importContext.state('csvData.data'))))
	},

	//=============================================================================
	__syntaxFix__: "syntax fix"
});

Clipperz.PM.UI.ImportContext.getDefaultImportTag = function() {
	var now  = new XDate();
	var	dateString = now.toString('yyyyMMdd');

	return "Import_" + dateString;
};
