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

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.TitlesClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.Titles',

	getInitialState: function() {
		return {
			'titleIndex': this.props.importContext.state('csvData.titleIndex'),
		};
	},
	
	onChangeCallback: function(columnIndex) {
		this.setState({'titleIndex': columnIndex});
		this.props.importContext.setState('csvData.titleIndex', columnIndex);
	},

	render: function() {
		var importContext = this.props.importContext;		

		return React.DOM.div({},[
			React.DOM.p({}, "Select the column that contains the title of cards you are importing."),
			React.DOM.table({'className': 'csvTable'},[
				React.DOM.thead({},
					React.DOM.tr({}, MochiKit.Base.map(MochiKit.Base.bind(function (cellInfo) {
						var result;
						var	columnIndex = cellInfo[0];
						var columnValue = cellInfo[1];
						var thClasses = {
							'title': (columnIndex == importContext.state('csvData.titleIndex')),
							'notes': (columnIndex == importContext.state('csvData.notesIndex')),
						}
						
						if (importContext.state('csvData.selectedColumns')[columnIndex]) {
							result = React.DOM.th({
								'key':'csv-titles-header-' + columnIndex,
								'className': Clipperz.PM.UI.Components.classNames(thClasses)
							}, [
								React.DOM.input({
									'type': 'radio',
									'id':  'csv-titles-input-' + columnIndex,
									'key': 'csv-titles-input-' + columnIndex,
									'ref': 'csv-titles-input-' + columnIndex,
									'checked': (columnIndex == this.state['titleIndex']),
									'onChange': MochiKit.Base.partial(this.onChangeCallback, columnIndex)
								}),
								React.DOM.label({'htmlFor': 'csv-titles-input-' + columnIndex}, columnValue),
							]);
						} else {
							result = null;
						}

						return result;
					}, this), Clipperz.Base.zipWithRange(importContext.state('csvData.labels'))))
				),
				importContext.renderCsvTableBody(true)
			])
		]);
	}
});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.Titles = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.TitlesClass);