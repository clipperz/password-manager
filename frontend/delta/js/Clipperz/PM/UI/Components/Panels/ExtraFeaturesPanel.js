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

Clipperz.Base.module('Clipperz.PM.UI.Components.Panels');

Clipperz.PM.UI.Components.Panels.ExtraFeaturesPanel = React.createClass({
	settingsToggleHandler: function (anEvent) {
		// console.log("settingsToggleHandler");
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleSettingsPanel');
	},

	//=========================================================================
	render: function () {
		var	classes = {
			'panel': true,
			'right': true,
			'open': this.props['settingsPanelStatus'] == 'OPEN'
		}

		return	React.DOM.div({key:'extraFeaturesPanel', id:'extraFeaturesPanel', className:React.addons.classSet(classes)}, [
			React.DOM.header({}, [
				React.DOM.div({className:'settingsToggle'}, [
					Clipperz.PM.UI.Components.Button({eventName:'settingsToggleButton', label:"menu", handler:this.settingsToggleHandler})
				])
			]),
			React.DOM.div({className: "notifications"},
				React.DOM.label({}, "Notifications"),
					React.DOM.ul({className:"items"},
						React.DOM.li({className:"information"}, 
							React.DOM.span({}, "Accont details saved successfully!"),
							React.DOM.button({"type":"button", "className":"close", "data-dismiss":"modal"},
								React.DOM.span({"aria-hidden":"true"}, "×")
							)
						),
						React.DOM.li({className:"critical"}, 
							React.DOM.span({}, "Your credentials were not verified."),
							React.DOM.button({"type":"button", "className":"close", "data-dismiss":"modal"},
								React.DOM.span({"aria-hidden":"true"}, "×")
							)
						),
						React.DOM.li({className:"warning"}, 
							React.DOM.span({}, "Your credentials were not verified."),
							React.DOM.button({"type":"button", "className":"close", "data-dismiss":"modal"},
								React.DOM.span({"aria-hidden":"true"}, "×")
							)
						)
					)
			),
			React.DOM.div({className: "account"},
				React.DOM.ul({},
					React.DOM.li({}, "Synchronize local data"),
					React.DOM.li({}, "Account"),
					React.DOM.li({}, "Subscription")
				)
			),
			React.DOM.div({className: "data"},
				React.DOM.ul({},
					React.DOM.li({}, "Local Data"),
					React.DOM.li({}, "OTP")
				)
			),
			React.DOM.div({className: "donation"},
				React.DOM.ul({},
					React.DOM.li({},
						React.DOM.a({}, "Make a donation")
					)
				)			
			)		
		]);
	}

	//=========================================================================
});
