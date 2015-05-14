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

	checkedCallback: function(columnN) {
		return columnN == this.props.importState.importData.titlesColumn;
	},
	
	onChangeCallback: function(columnN) {
		var newState = {'importData': this.props.importState.importData};
	
		if (this.props.importState.importData.notesColumn == columnN) {
			newState.importData.notesColumn = null;
		}
		newState.importData.titlesColumn = columnN;
		
		this.props.setImportStateCallback(newState);
	},

	render: function() {
		
		var importData = this.props.importState.importData;
		
		return React.DOM.div({},[
			React.DOM.h2({},"Titles"),
			
			
			Clipperz.PM.UI.Components.ExtraFeatures.DataImport.StepsNavigation({
				'format': 'csv',
				'stepId': 'csv-titles',
				'prevStep': 'csv-labels',
				'nextStep': 'csv-notes',
				'goToStepCallback': this.props.goToStepCallback,
				'nextDisabled': (importData.titlesColumn != 0 && ! importData.titlesColumn )
			}),
			
			React.DOM.p({}, "Select the column that contains titles of the cards you are importing. (mandatory)"),
			React.DOM.table({'style': {'background': 'white'}},[
				React.DOM.thead({},
					this.props.csvRenderTheadInputCallback('titles', 'radio', this.checkedCallback, this.onChangeCallback, null, true)
				),
				React.DOM.tbody({},
					this.props.csvRenderTbodyCallback()
				)
		
			])
		]);
	}

});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvTitles = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvTitlesClass);