/*

Copyright 2008-2013 Clipperz Srl

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

Clipperz.PM.UI.Components.PreferencePage = React.createClass({

	getDefaultProps: function () {
		return {
		}
	},

	propTypes: {
//		card: React.PropTypes.object.isRequired
//		checked: React.PropTypes.boolean.isRequired
	},

	getInitialState: function () {
//		return {
//			shouldStoreDataLocally: false
//		};
	},

	handleBackClick: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'goBack');
	},

	toggleShouldStoreDataLocally: function (anEvent) {
//		this.setState({shouldStoreDataLocally: !this.state['shouldStoreDataLocally']});
		Clipperz.PM.DataModel.devicePreferences.setShouldStoreDataLocally(!Clipperz.PM.DataModel.devicePreferences.shouldStoreDataLocally());
		this.setState({});
	},

	shouldStoreDataLocally: function () {
		return Clipperz.PM.DataModel.devicePreferences.shouldStoreDataLocally();
	},

	syncNow: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'synchronizeLocalData');
	},

	//=========================================================================

	render: function () {
		return	React.DOM.div({className:'preferences'}, [
			React.DOM.div({className:'header'}, [
				React.DOM.div({className:'titleWrapper'}, React.DOM.div({className:'title'}, "Preferences")),
				React.DOM.div({className:'backWrapper'},  React.DOM.a({className:'button back', onClick:this.handleBackClick}, "back")),
			]),
			React.DOM.div({className:'content'}, [
				React.DOM.form(null, [
					React.DOM.div({className:'section'}, [
						React.DOM.h4(null, "Local storage"),
						React.DOM.p(null, "Store you account data locally for offline viewing"),
						new Clipperz.PM.UI.Components.Checkbox({'id':'shouldStoreLocally_checkbox', 'checked':this.shouldStoreDataLocally(), 'eventHandler':this.toggleShouldStoreDataLocally}),
						this.shouldStoreDataLocally() ? React.DOM.div({className:'syncInfo'}, [
//							React.DOM.h5(null, "data were never synchronized before"),
							React.DOM.a({className:'button', onClick:this.syncNow}, "Sync now")
						]) : null
					])
				])
			]),
			React.DOM.div({className:'footer'}, [

			])
		]);
	}

	//=========================================================================
});
