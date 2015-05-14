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
Clipperz.Base.module('Clipperz.PM.UI.Components.ExtraFeatures.DataImport');

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvLabelsClass = React.createClass({
	
	toggleFirstRow: function() {
		var newState;
		var cellCount;

		newState = {'importData': this.props.importState.importData};
		newState.importData.firstRowAsLabels = ! newState.importData.firstRowAsLabels;
		
		cellCount = 0;
		MochiKit.Base.map(function(cell){
			newState.importData.columnLabelsFirstrow[cellCount++] = cell;
		}, this.props.importState.importData.parsedCSV[0]);
		
		this.props.setImportStateCallback(newState);
	},
	
	isNextDisabled: function() {
		var result;
		
		var importData = this.props.importState.importData;
		
		var columnLabels = (importData.firstRowAsLabels) ? importData.columnLabelsFirstrow : importData.columnLabels;
		
		result = false;
		for (i in columnLabels) {
			result = result || (columnLabels[i] == '');
		}
		
		return result;
	},
	
	valueCallback: function(columnN) {
		var columnLabels = this.props.csvGetColumnLabelsCallback();
		return columnLabels[columnN];
	},
	
	onChangeCallback: function(columnN) {
		var newState;
	
		newState = {'importData': this.props.importState.importData};
		if (this.props.importState.importData.firstRowAsLabels) {
			newState.importData.columnLabelsFirstrow[columnN] = this.refs['csv-labels-input-'+columnN].getDOMNode().value;
		} else {
			newState.importData.columnLabels[columnN] = this.refs['csv-labels-input-'+columnN].getDOMNode().value;
		}
	
		this.props.setImportStateCallback(newState);
	},
	
	render: function() {
		
		var importData = this.props.importState.importData;
			
		return React.DOM.div({},[
			React.DOM.h2({},"Labels"),
			
			Clipperz.PM.UI.Components.ExtraFeatures.DataImport.StepsNavigation({
				'format': 'csv',
				'stepId': 'csv-labels',
				'prevStep': 'csv-columns',
				'nextStep': 'csv-titles',
				'goToStepCallback': this.props.goToStepCallback,
				'nextDisabled': this.isNextDisabled()
			}),
			
			React.DOM.p({}, "Set a label for each field in your data. If the first row of the CSV file contains field labels, tick off the checkbox below."),
			React.DOM.input({
				'id': 'csv-labels-firstrow',
				'type': 'checkbox',
				'checked': this.props.importState.importData.firstRowAsLabels,
				'onChange': this.toggleFirstRow
			}),
			React.DOM.label({'htmlFor':'csv-labels-firstrow'}, "Use the first row as labels"),
			React.DOM.table({'style': {'background': 'white'}},[
				React.DOM.thead({},
					this.props.csvRenderTheadInputCallback('labels', 'text', this.valueCallback, this.onChangeCallback, null, false)
				),
				React.DOM.tbody({},
					this.props.csvRenderTbodyCallback()
				)
		
			])
		]);
	}

});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvLabels = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvLabelsClass);