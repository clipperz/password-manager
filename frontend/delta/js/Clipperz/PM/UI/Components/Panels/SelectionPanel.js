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
Clipperz.Base.module('Clipperz.PM.UI.Components.Panels');

Clipperz.PM.UI.Components.Panels.SelectionPanelClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.Panels.SelectionPanel',

	propTypes: {
		selectionPanelStatus:	React.PropTypes.oneOf(['OPEN', 'CLOSED']).isRequired
	},

	//=========================================================================

	render: function () {
//console.log("SelectionPanel", this.props);
		var	classes = Clipperz.PM.UI.Components.classNames({
			'panel': true,
			'left': true,
			'open': this.props['selectionPanelStatus'] == 'OPEN'
		});

		return	React.DOM.div({'key':'selectionPanel', 'id':'selectionPanel', 'className':classes}, [
			Clipperz.PM.UI.Components.Selections(this.props),
		]);
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.Panels.SelectionPanel = React.createFactory(Clipperz.PM.UI.Components.Panels.SelectionPanelClass);
