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
	
	getInitialState: function() {
		return {
			'firstRowAsLabels': this.props.importContext.firstRowAsLabels,
			'columnLabels': this.props.importContext.columnLabels,
			'columnLabelsFirstrow': this.props.importContext.columnLabelsFirstrow
		};
	},
	
	componentDidMount() {
		this.props.setNextStepCallback((this.isNextDisabled()) ? null : this.handleNextStep);
	},
	
	//-------------------------------------------------------------------------

	handleNextStep: function() {
		return this.state;
	},
	
	updateNextStatus: function() {
		this.props.setNextStepCallback((! this.isNextDisabled()) ? this.handleNextStep : null);
	},
	
	isNextDisabled: function() {
		var result;
		
		var importContext = this.props.importContext;
		var columnLabels = this.getLabels();
		
		result = false;
		for (i in columnLabels) {
			result = result || ((columnLabels[i] == '')&&(importContext.selectedColumns[i]));
		}
		
		return result;
	},
	
	//=========================================================================
	
	getLabels: function() {
		return (this.state.firstRowAsLabels) ? this.state.columnLabelsFirstrow : this.state.columnLabels;
	},
	
	toggleFirstRow: function() {
		var newState;
		var cellCount;

		newState = this.state;
		newState.firstRowAsLabels = ! newState.firstRowAsLabels;
		
		cellCount = 0;
		MochiKit.Base.map(function(cell){
			newState.columnLabelsFirstrow[cellCount++] = cell;
		}, this.props.importContext.parsedCsv[0]);
		
		this.updateNextStatus();
		
		this.setState(newState);
	},
	
	onChangeCallback: function(columnN) {
		var newState;
	
		newState = this.state;
		if (newState.firstRowAsLabels) {
			newState.columnLabelsFirstrow[columnN] = this.refs['csv-labels-input-' + columnN].getDOMNode().value;
		} else {
			newState.columnLabels[columnN] = this.refs['csv-labels-input-' + columnN].getDOMNode().value;
		}
	
		this.updateNextStatus();
		
		this.setState(newState);
	},
	
	render: function() {
//console.log("labels-render",this.props.importContext);
//return React.DOM.p({}, "labels")
		var rowCount, cellCount;

		var importContext = this.props.importContext;
		var columnLabels = this.getLabels();
		
		rowCount = 0;
		cellCount = 0;
		return React.DOM.div({},[
			React.DOM.p({}, "Set a label for each field in your data. If the first row of the CSV file contains field labels, tick off the checkbox below."),
			React.DOM.input({
				'id': 'csv-labels-firstrow',
				'type': 'checkbox',
				'checked': this.state.firstRowAsLabels,
				'onChange': this.toggleFirstRow
			}),
			React.DOM.label({'htmlFor':'csv-labels-firstrow'}, "Use the first row as labels"),
			React.DOM.table({'className': 'csvTable'},[
				React.DOM.thead({},
					React.DOM.tr({},
						MochiKit.Base.map(MochiKit.Base.bind(function(cell) {
							var result;
							
							if (! importContext.selectedColumns[cellCount]) {
								result = null;
							} else {
								result = React.DOM.th({'key': 'csv-labels-header-' + cellCount}, [
									React.DOM.input({
										'type': 'text',
										'id': 'csv-labels-input-' + cellCount,
										'key': 'csv-labels-input-' + cellCount,
										'ref': 'csv-labels-input-' + cellCount,
										'value': columnLabels[cellCount],
										'onChange': MochiKit.Base.partial(this.onChangeCallback,cellCount)
									})
								]);
							}
							
							cellCount++;
							
							return result;
						}, this), this.props.importContext.parsedCsv[0])
					)
				),
				React.DOM.tbody({},
					MochiKit.Base.map(MochiKit.Base.bind(function(row){
						var result;
						
						cellCount = 0;
						
						if (rowCount == 0 && this.state.firstRowAsLabels) {
							result = null;
						} else {							
							result = React.DOM.tr({'key': 'csv-row-' + (rowCount)}, MochiKit.Base.map( function(cell) {
								var result;
								
								if (importContext.selectedColumns[cellCount]) {
									result = React.DOM.td({'key': 'csv-cell-' + rowCount + '-' + (cellCount)},cell);
								} else{
									result = null;
								}
								
								cellCount++;
								return  result;
							}, row));
						}
						
						rowCount++;
						
						return result;
					},this), importContext.parsedCsv)
				)
		
			])
		]);
	}

});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvLabels = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvLabelsClass);