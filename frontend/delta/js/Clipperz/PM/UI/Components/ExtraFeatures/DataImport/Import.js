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
Clipperz.Base.module('Clipperz.PM.UI.Components.ExtraFeatures.DataImport');

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.ImportClass = React.createClass({
	//=========================================================================

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.DataImport.Import',

	importHandler: function (anEvent) {
		var recordsToImport = this.props.importContext.state('recordsToImport');

		if (this.props.importContext.state('useImportTag') && this.props.importContext.state('importTag')) {
			this.props.importContext.enhanceJsonDataWithImportTag(recordsToImport, this.props.importContext.state('importTag'))
		}

		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'importCards', recordsToImport);
	},

	render: function() {
		return React.DOM.div({}, [
			React.DOM.h5({}, "Cards to import: " + this.props.importContext.state('recordsToImport').length + " (of " + this.props.importContext.state('jsonData').length + ")"),
			React.DOM.a({'className': 'button import', 'onClick': this.importHandler}, "Import")
			
		]);
		
	},

});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.Import = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.ImportClass);