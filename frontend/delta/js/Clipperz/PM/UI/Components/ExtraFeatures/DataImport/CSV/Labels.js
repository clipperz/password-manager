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
Clipperz.Base.module('Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV');

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.LabelsClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.Labels',

	getInitialState: function() {
		return {
			'useFirstRowAsLabels': this.props.importContext.state('csvData.useFirstRowAsLabels'),
			'labels': this.props.importContext.state('csvData.labels'),
		};
	},
	
	//-------------------------------------------------------------------------

	updateImportContextState: function () {
		this.props.importContext.setState('csvData.useFirstRowAsLabels', this.state['useFirstRowAsLabels']);
		this.props.importContext.setState('csvData.labels', this.state['labels']);
	},

	toggleFirstRow: function() {
		var newState;

		newState = this.state;
		newState['useFirstRowAsLabels'] = ! this.state['useFirstRowAsLabels'];
		if (newState['useFirstRowAsLabels']) {
			newState['labels'] = MochiKit.Base.map(Clipperz.Base.trim, this.props.importContext.state('csvData.data')[0]);
		}

		this.setState(newState);
		this.updateImportContextState();
	},

	onChangeLabelCallback: function(columnIndex) {
		var newState;
	
		newState = this.state;
		newState['labels'][columnIndex] = this.refs['csv-labels-input-' + columnIndex].value;

		this.setState(newState);
		this.updateImportContextState();
	},
	
	render: function() {
		var	importContext = this.props.importContext;

		return React.DOM.div({},[
			React.DOM.p({}, "Set a label for each field in your data. If the first row of the CSV file contains field labels, tick off the checkbox below."),
			React.DOM.div({}, [
				React.DOM.input({
					'id': 'csv-labels-firstrow',
					'type': 'checkbox',
					'checked': this.state['useFirstRowAsLabels'],
					'onChange': this.toggleFirstRow
				}),
				React.DOM.label({'htmlFor':'csv-labels-firstrow'}, "Use the first row as labels")
			]),
			React.DOM.table({'className': 'csvTable'},[
				React.DOM.thead({},
					React.DOM.tr({},
						MochiKit.Base.map(MochiKit.Base.bind(function(cellInfo) {
							var result;
							var	columnIndex = cellInfo[0];
							var	columnValue = cellInfo[1];
							var thClasses = {
								'title': (columnIndex == importContext.state('csvData.titleIndex')),
								'notes': (columnIndex == importContext.state('csvData.notesIndex')),
							}
							
							if (importContext.state('csvData.selectedColumns')[columnIndex]) {
								result = React.DOM.th({
									'key':'csv-labels-header-' + columnIndex,
									'className': Clipperz.PM.UI.Components.classNames(thClasses)
								}, [
									React.DOM.input({
										'type': 'text',
										'id': 'csv-labels-input-' + columnIndex,
										'key': 'csv-labels-input-' + columnIndex,
										'ref': 'csv-labels-input-' + columnIndex,
										'value': columnValue,
										'placeholder': "…",
										'onChange': MochiKit.Base.partial(this.onChangeLabelCallback, columnIndex)
									})
								]);
							} else {
								result = null;
							}
							
							return result;
						}, this), Clipperz.Base.zipWithRange(this.state['labels']))
					)
				),
				importContext.renderCsvTableBody(true)
			])
		]);
	}
});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.Labels = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.LabelsClass);