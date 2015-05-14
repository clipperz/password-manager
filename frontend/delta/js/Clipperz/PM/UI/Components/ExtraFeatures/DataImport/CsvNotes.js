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
	
	checkedCallback: function(columnN) {
		return columnN == this.props.importState.importData.notesColumn;
	},
	
	onChangeCallback: function(columnN) {
		var newState = {'importData': this.props.importState.importData};
	
		newState.importData.notesColumn = columnN;
		
		this.setState(newState);
	},
	
	disabledCallback: function(columnN) {
		return columnN == this.props.importState.importData.titlesColumn;
	},

	render: function() {
		return React.DOM.div({},[
			React.DOM.h2({},"Notes"),
			
			Clipperz.PM.UI.Components.ExtraFeatures.DataImport.StepsNavigation({
				'format': 'csv',
				'stepId': 'csv-notes',
				'prevStep': 'csv-titles',
				'nextStep': 'csv-hidden',
				'goToStepCallback': this.props.goToStepCallback
			}),
			
			React.DOM.p({}, "Select the column that represents a \"notes\" field. (optional)"),
			React.DOM.input({
				'id': 'csv-notes-nonotes',
				'type': 'radio',
				'checked': ! this.props.importState.importData.notesColumn,
				'onChange': MochiKit.Base.partial(this.onChangeCallback, null)
			}),
			React.DOM.label({'htmlFor': 'csv-notes-nonotes'}, "\"notes\" field not present"),
			React.DOM.table({'style': {'background': 'white'}},[
				React.DOM.thead({},
					this.props.csvRenderTheadInputCallback('notes', 'radio', this.checkedCallback, this.onChangeCallback, this.disabledCallback, true)
				),
				React.DOM.tbody({},
					this.props.csvRenderTbodyCallback()
				)
		
			])
		]);
	}

});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvNotes = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvNotesClass);