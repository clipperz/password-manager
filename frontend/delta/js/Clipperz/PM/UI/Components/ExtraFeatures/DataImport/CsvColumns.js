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

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvColumnsClass = React.createClass({
	
	toggleColumn: function(columnN) {
		var newState;
	
		newState = {'importData': this.props.importState.importData};
		newState.importData.selectedColumns[columnN] = ! newState.importData.selectedColumns[columnN];
		
		this.props.setImportStateCallback(newState);
	},

	render: function() {
		
		var columnSelectors;
		var rowCount;
		var i;
		
		columnSelectors = [];
		for (i=0; i<this.props.importState.importData.nColumns; i++) {
			columnSelectors.push( React.DOM.td({'key': 'csv-colsel-'+i}, React.DOM.input({
				'type': 'checkbox',
				'checked': this.props.importState.importData.selectedColumns[i],
				'onChange': MochiKit.Base.partial(this.toggleColumn,i)
			}) ) );
		}
		
		rowCount = 0;
		
		return React.DOM.div({},[
			React.DOM.h2({},"Columns"),
			Clipperz.PM.UI.Components.ExtraFeatures.DataImport.StepsNavigation({
				'format': 'csv',
				'stepId': 'csv-columns',
				'prevStep': 'input',
				'nextStep': 'csv-labels',
				'goToStepCallback': this.props.goToStepCallback,
			}),
			React.DOM.p({}, "Select the columns you want to import."),
			React.DOM.table({'style': {'background': 'white'}},[
				React.DOM.thead({}, React.DOM.tr({'className': 'columnSelectors', 'key': 'csv-colsel'}, columnSelectors)),
				React.DOM.tbody({},
					MochiKit.Base.map(function(row){
						var cellCount;
						var result
						
						cellCount = 0;
						result = React.DOM.tr({'key': 'csv-row-'+(rowCount++)}, MochiKit.Base.map(function(cell) {
							return React.DOM.td({'key': 'csv-cell-'+rowCount+'-'+(cellCount++)},cell);
						}, row));
						rowCount++;
						
						return result;
					}, this.props.importState.importData.parsedCSV)
				),
			])
		]);
	}

});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvColumns = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvColumnsClass);