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

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvTitlesClass = React.createClass({

	getInitialState: function() {
		return {
			'titlesColumn': this.props.importContext.titlesColumn,
			'notesColumn': this.props.importContext.notesColumn
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
		return (this.state.titlesColumn != 0 && ! this.state.titlesColumn );
	},
	
	//=========================================================================
	
	onChangeCallback: function(columnN) {
		var newState = this.state;
	
		if (newState.notesColumn == columnN) {
			newState.notesColumn = null;
		}
		newState.titlesColumn = columnN;
		
		this.updateNextStatus();
		
		this.setState(newState);
	},

	render: function() {
		var rowCount, cellCount;
		
		var importContext = this.props.importContext;		
		var columnLabels = importContext.getCsvLabels();

		rowCount = 0;
		cellCount = 0;
		return React.DOM.div({},[
			React.DOM.p({}, "Select the column that contains titles of the cards you are importing. (mandatory)"),
			React.DOM.table({'className': 'csvTable'},[
				React.DOM.thead({},
					React.DOM.tr({},
						MochiKit.Base.map(MochiKit.Base.bind(function(cell) {
							var result;
							
							var thId = 'csv-titles-header-' + cellCount;
							var inputId = 'csv-titles-input-' + cellCount;
							
							if (! importContext.selectedColumns[cellCount]) {
								result = null;
							} else {
								result = React.DOM.th({'key': thId}, [
									React.DOM.label({'htmlFor': inputId}, columnLabels[cellCount]),
									React.DOM.input({
										'type': 'radio',
										'id': inputId,
										'key': inputId,
										'ref': inputId,
										'checked': cellCount == this.state.titlesColumn,
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
						
						if (rowCount == 0 && importContext.firstRowAsLabels) {
							result = null;
						} else {							
							result = React.DOM.tr({'key': 'csv-row-'+(rowCount)}, MochiKit.Base.map( function(cell) {
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

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvTitles = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvTitlesClass);