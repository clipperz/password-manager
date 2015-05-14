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

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvHiddenClass = React.createClass({
	
	checkedCallback: function(columnN) {
		return this.props.importState.importData.hiddenColumns[columnN];
	},
	
	onChangeCallback: function(columnN) {
		var newState = {'importData': this.props.importState.importData};
	
		newState.importData.hiddenColumns[columnN] = ! newState.importData.hiddenColumns[columnN];
		
		this.setState(newState);
	},
	
	disabledCallback: function(columnN) {
		return (columnN == this.props.importState.importData.titlesColumn || columnN == this.props.importState.importData.notesColumn)
	},

	render: function() {
		var importData = this.props.importState.importData;
		
		return React.DOM.div({},[
			React.DOM.h2({},"Hidden"),
			
			Clipperz.PM.UI.Components.ExtraFeatures.DataImport.StepsNavigation({
				'format': 'csv',
				'stepId': 'csv-hidden'
			}),
			React.DOM.button({'onClick': MochiKit.Base.partial(this.props.goToStepCallback, 'csv-notes') }, "Back"),
			React.DOM.span({}, " - "),
			React.DOM.button({'onClick': MochiKit.Base.bind(function() {
				var importData = this.props.importState.importData;
				var json = this.props.csvToJsonCallback();
				this.props.setImportStateCallback({
					'importData': importData,
					'jsonToImport': json,
					'recordsToImport': MochiKit.Base.map(function(r){return r._importId},json),
					'currentStep': 'preview',
					'previousStep': 'csv-hidden'
				});
			}, this) }, "Preview"),
			React.DOM.p({}, "Select the fields that should be hidden. (passwords, PINs, ...)"),
			React.DOM.table({'style': {'background': 'white'}},[
				React.DOM.thead({},
					this.props.csvRenderTheadInputCallback('hidden', 'checkbox', this.checkedCallback, this.onChangeCallback, this.disabledCallback, true)
				),
				React.DOM.tbody({},
					this.props.csvRenderTbodyCallback()
				)
		
			])
		]);
	}

});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvHidden = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.CsvHiddenClass);