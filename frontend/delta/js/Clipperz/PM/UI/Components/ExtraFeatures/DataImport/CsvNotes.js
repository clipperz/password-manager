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

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvNotesClass = React.createClass({
	
	getInitialState: function() {
		return {
			'notesColumn': this.props.importContext.notesColumn
		};
	},
	
	componentDidMount() {
		this.props.setNextStepCallback(this.handleNextStep);
	},
	
	//-------------------------------------------------------------------------

	handleNextStep: function() {
		return this.state;
	},
	
	//=========================================================================
	
	onChangeCallback: function(columnN) {
		this.setState({'notesColumn': columnN});
	},

	render: function() {
		var cellCount, rowCount;
		
		var importContext = this.props.importContext;
		
		cellCount = 0;
		rowCount = 0;
		return React.DOM.div({},[
			React.DOM.p({}, "Select the column that represents a \"notes\" field. (optional)"),
			React.DOM.input({
				'id': 'csv-notes-nonotes',
				'type': 'radio',
				'checked': ! this.state.notesColumn,
				'onChange': MochiKit.Base.partial(this.onChangeCallback, null)
			}),
			React.DOM.label({'htmlFor': 'csv-notes-nonotes'}, "\"notes\" field not present"),
			React.DOM.table({'className': 'csvTable'},[
				React.DOM.thead({},

				React.DOM.tr({},
						MochiKit.Base.map(MochiKit.Base.bind(function(cell) {
							var result;
							
							var thId = 'csv-notes-header-' + cellCount;
							var inputId = 'csv-notes-input-' + cellCount;
							
							if (! importContext.selectedColumns[cellCount]) {
								result = null;
							} else {
								result = React.DOM.th({'key': thId}, [
									React.DOM.label({'htmlFor': inputId}, importContext.getCsvLabels()[cellCount]),
									React.DOM.input({
										'type': 'radio',
										'id': inputId,
										'key': inputId,
										'ref': inputId,
										'checked': cellCount == this.state.notesColumn,
										'onChange': MochiKit.Base.partial(this.onChangeCallback,cellCount),
										'disabled': cellCount == importContext.titlesColumn
									})
								]);
							}
							
							cellCount++;
							
							return result;
						}, this), importContext.parsedCsv[0])
					)

				),
				React.DOM.tbody({},
					
					MochiKit.Base.map(MochiKit.Base.bind(function(row){
						var result;
						
						cellCount = 0;
						
						if (rowCount == 0 && importContext.firstRowAsLabels) {
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

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvNotes = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvNotesClass);