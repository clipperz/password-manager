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

Clipperz.PM.UI.Components.Panels.NotificationCenter = React.createClass({
	render: function(){
		var notification_list = [];
		var notifications = this.props.notifications;

		for (var i=0; i < notifications.length; i++) {
			notification_list.push(
				React.DOM.li({className: notifications[i].severity}, 
					React.DOM.span({}, notifications[i].text),
					React.DOM.button({"type":"button", "className":"close", "data-dismiss":"modal"},
						React.DOM.span({"aria-hidden":"true"}, "×")
					)
				)
			);
		}

		return React.DOM.div({className: "notifications" + (notifications.length == 0 ? " hidden" : "")},
			React.DOM.label({}, "Notifications"),
				React.DOM.ul({className:"items"}, notification_list)
		);
	}	
});

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
		var  notifications = [];
		// {
		// 	text: 'Accont details saved successfully!',
		// 	severity: 'information'
		// },{
		// 	text: 'Your credentials were not verified',
		// 	severity: 'warning'
		// },{
		// 	text: 'Please change your login credentials',
		// 	severity: 'critical'
		// }];

		return	React.DOM.div({key:'extraFeaturesPanel', id:'extraFeaturesPanel', className:React.addons.classSet(classes)}, [
			React.DOM.header({}, [
				React.DOM.div({className:'settingsToggle'}, [
					Clipperz.PM.UI.Components.Button({eventName:'settingsToggleButton', label:"menu", handler:this.settingsToggleHandler})
				])
			]),
			Clipperz.PM.UI.Components.Panels.NotificationCenter({
				ref: "notificationCenter",
				notifications: notifications
			}),
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
