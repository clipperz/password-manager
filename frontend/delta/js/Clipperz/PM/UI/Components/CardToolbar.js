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

Clipperz.Base.module('Clipperz.PM.UI.Components');

Clipperz.PM.UI.Components.CardToolbar = React.createClass({

	propTypes: {
//		'style':			React.PropTypes.oneOf(['extra-short', 'narrow', 'wide', 'extra-wide']).isRequired,
		'style':			React.PropTypes.oneOf(Clipperz_PM_UI_availableStyles).isRequired,
		'enableSidePanels':	React.PropTypes.bool.isRequired,
		'accountInfo':		React.PropTypes.object.isRequired,
		'messageBox':		React.PropTypes.object.isRequired,
		'filter':			React.PropTypes.object /*.isRequired */
	},

	//----------------------------------------------------------------------------

	selectionToggleHandler: function (anEvent) {
//console.log("selectionToggleHandler");
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleSelectionPanel');
		
	},
	
	settingsToggleHandler: function (anEvent) {
//console.log("settingsToggleHandler");
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleSettingsPanel');
	},
	
	//============================================================================

	renderWithSidePanels: function () {
		return [
			React.DOM.div({className:'selectionToggle'}, [
				Clipperz.PM.UI.Components.Button({eventName:'selectionToggleButton', label:"tags", handler:this.selectionToggleHandler})
			]),
			this.renderWithoutSidePanels(),
			React.DOM.div({className:'settingsToggle'}, [
				Clipperz.PM.UI.Components.Button({eventName:'settingsToggleButton', label:"menu", handler:this.settingsToggleHandler})
			])
		];
	},
	
	renderWithoutSidePanels: function () {
		var	result;

		if (this.props['filter']) {
//console.log("CARD TOOLBAR", this.props['filter']['type']);

			if (this.props['filter']['type'] == 'RECENT') {
				result = [React.DOM.div({className:'clipperz'}, [React.DOM.span({className:'logo recent'}, "recent")])];
			} else if (this.props['filter']['type'] == 'TAG') {
				result = [React.DOM.div({className:'clipperz'}, [
					React.DOM.span({className:'logo tag'}, "tag"),
					React.DOM.span({className:'value'}, this.props['filter']['value'])
				])];
			} else if (this.props['filter']['type'] == 'UNTAGGED') {
				result = [React.DOM.div({className:'clipperz'}, [
					React.DOM.span({className:'logo tag'}, "tag"),
					React.DOM.span({className:'value'}, "untagged")
				])];
			} else if (this.props['filter']['type'] == 'SEARCH') {
				result = [React.DOM.div({className:'clipperz'}, [
					React.DOM.span({className:'logo search'}, "search"),
					React.DOM.span({className:'value'}, this.props['filter']['value'])
					])];
			} else {
				result = [React.DOM.div({className:'clipperz'}, [React.DOM.span({className:'logo clipperz'}, "clipperz")])];
			}
		} else {
			result = [React.DOM.div({className:'clipperz'}, [React.DOM.span({className:'logo clipperz'}, "clipperz")])];
		}
		
		return result;
	},

	render: function () {
//console.log("CardToolbar props", this.props);
		return	React.DOM.div({className:'cardToolbar ' + this.props['style']}, [
//			React.DOM.div({className:'header'}, this.props['enableSidePanels'] ? this.renderWithSidePanels() : this.renderWithoutSidePanels()),
			React.DOM.header({}, this.props['enableSidePanels'] ? this.renderWithSidePanels() : this.renderWithoutSidePanels()),
			Clipperz.PM.UI.Components.AccountStatus(this.props['accountInfo']),
			Clipperz.PM.UI.Components.MessageBox(this.props['messageBox']),
		]);
	}

	//============================================================================
});
