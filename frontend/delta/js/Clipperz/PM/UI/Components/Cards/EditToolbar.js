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
Clipperz.Base.module('Clipperz.PM.UI.Components.Cards');

Clipperz.PM.UI.Components.Cards.EditToolbarClass = React.createClass({

	//============================================================================

	displayName: 'Clipperz.PM.UI.Components.Cards.EditToolbar',

	propTypes: {
		'hasPendingChanges':	React.PropTypes.bool.isRequired,
	},

	//----------------------------------------------------------------------------

	hasPendingChanges: function () {
		return this.props['hasPendingChanges'];
	},
	
	//----------------------------------------------------------------------------

	cancel: function () {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'cancelCardEdits', this.props['_reference']);
	},
	
	save: function () {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'saveCardEdits', this.props['_reference']);
	},
	
	//----------------------------------------------------------------------------

	render: function () {
		var style = this.props['style'];
		var	classes = {
			'cardDetailToolbar':	true,
			'edit':					true,
			'hasPendingChanges':	this.hasPendingChanges(),
		};
		classes[style] = true;

		return	React.DOM.div({'className':Clipperz.PM.UI.Components.classNames(classes)}, [
					React.DOM.ul({}, [
						React.DOM.li({'onClick':this.cancel, 'className':'cancel'},	[React.DOM.span({}, "cancel")]),
						React.DOM.li({'onClick':this.save,   'className':'save'},	[React.DOM.span({}, "save")]),
					])
				]);
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.Cards.EditToolbar = React.createFactory(Clipperz.PM.UI.Components.Cards.EditToolbarClass);