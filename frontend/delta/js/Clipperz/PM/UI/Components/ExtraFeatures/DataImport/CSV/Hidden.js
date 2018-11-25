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

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.HiddenClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.Hidden',

	getInitialState: function() {
		return {
			'hiddenFields': this.props.importContext.state('csvData.hiddenFields'),
		};
	},
	
	onChangeCallback: function (columnIndex) {
		var newHiddenColumns = this.state['hiddenFields'];
	
		newHiddenColumns[columnIndex] = ! newHiddenColumns[columnIndex];
		
		this.setState({'hiddenFields': newHiddenColumns});
		this.props.importContext.setState('csvData.hiddenFields', newHiddenColumns);
	},

	render: function() {
		var importContext = this.props.importContext;
		
		return React.DOM.div({},[
			React.DOM.p({}, "Select the fields that should be hidden. (passwords, PINs, ...)"),
			React.DOM.table({'className': 'csvTable'},[
				React.DOM.thead({},
					React.DOM.tr({},
						MochiKit.Base.map(MochiKit.Base.bind(function (cellInfo) {
							var result;
							var	columnIndex = cellInfo[0];
							var	columnValue = cellInfo[1];
							var thClasses = {
								'title': (columnIndex == importContext.state('csvData.titleIndex')),
								'notes': (columnIndex == importContext.state('csvData.notesIndex')),
							}

							if (importContext.state('csvData.selectedColumns')[columnIndex]) {
								result = React.DOM.th({
									'key': 'csv-notes-header-' + columnIndex,
									'className': Clipperz.PM.UI.Components.classNames(thClasses)
								}, [
									React.DOM.input({
										'type': 'checkbox',
										'id':  'csv-notes-input-' + columnIndex,
										'key': 'csv-notes-input-' + columnIndex,
										'ref': 'csv-notes-input-' + columnIndex,
										'checked': this.state['hiddenFields'][columnIndex],
										'onChange': MochiKit.Base.partial(this.onChangeCallback, columnIndex),
										'disabled': ((columnIndex == importContext.state('csvData.titleIndex')) || (columnIndex == importContext.state('csvData.notesIndex')))
									}),
									React.DOM.label({'htmlFor': 'csv-notes-input-' + columnIndex}, columnValue),
								]);
							} else {
								result = null;
							}
							
							return result;
						}, this), Clipperz.Base.zipWithRange(importContext.state('csvData.labels')))
					)
				),
				importContext.renderCsvTableBody(true)
			])
		]);
	}

});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.Hidden = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.HiddenClass);