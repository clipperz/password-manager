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

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.ColumnsClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.Columns',
	
	getInitialState: function() {
		return {
			'selectedColumns': this.props.importContext.state('csvData.selectedColumns'),
		};
	},
	
	toggleColumn: function(columnIndex) {
		var newSelectedColumns;
	
		newSelectedColumns = this.state['selectedColumns'];
		newSelectedColumns[columnIndex] = ! newSelectedColumns[columnIndex];
		
		this.setState({'selectedColumns': newSelectedColumns});
		this.props.importContext.setState('csvData.selectedColumns', newSelectedColumns);
	},

	render: function() {
		var	importContext = this.props.importContext;

		return React.DOM.div({},[
			React.DOM.p({}, "Select the columns you want to import."),
			React.DOM.table({'className':'csvTable', 'key':'csvTableColumns'},[
				React.DOM.thead({}, React.DOM.tr({'className':'columnSelectors', 'key':'csv-colsel'}, MochiKit.Base.map(MochiKit.Base.bind(function (columnIndex) {
					var thClasses = {
						'title': (columnIndex == importContext.state('csvData.titleIndex')),
						'notes': (columnIndex == importContext.state('csvData.notesIndex')),
					}

					return React.DOM.th({
						'key': 'csv-colsel-' + columnIndex,
						'className': Clipperz.PM.UI.Components.classNames(thClasses)
					}, React.DOM.input({
						'key': 'csv-label-input-' + columnIndex,
						'type': 'checkbox',
						'checked': this.state['selectedColumns'][columnIndex],
						'onChange': MochiKit.Base.partial(this.toggleColumn, columnIndex)
					}));
				}, this), MochiKit.Iter.range(this.state['selectedColumns'].length)))),
				this.props.importContext.renderCsvTableBody(false)
			])
		]);
	}
});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.Columns = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CSV.ColumnsClass);
