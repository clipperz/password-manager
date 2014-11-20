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
	getInitialState: function(){
		var notifications = this.getNotifications();
		return {
			notificationCount: notifications.length,
			notifications: notifications
		};
	},
	getNotifications: function(){
		return [{
				text: 'Accont details saved successfully!',
				severity: 'information'
			},
			{
				text: 'Your credentials were not verified',
				severity: 'warning'
			},
			{
				text: 'Please change your login credentials',
				severity: 'critical'
		}];
	},
	settingsToggleHandler: function (e) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleSettingsPanel');
	},
	updateNotificationCount: function(notificationBox, e){		
		this.setState({notificationCount:notificationBox.getNotificationCount()});
	},
	render: function () {
		var	classes = {
			'panel': true,
			'right': true,
			'open': this.props['settingsPanelStatus'] == 'OPEN'
		}

		return	React.DOM.div({key:'extraFeaturesPanel', id:'extraFeaturesPanel', className:React.addons.classSet(classes)}, [
			React.DOM.header({}, [
				React.DOM.div({className:'settingsToggle'}, [
					Clipperz.PM.UI.Components.Button({eventName:'settingsToggleButton', label:"menu", handler:this.settingsToggleHandler}),
					React.DOM.div({
							className: "notifications-counter" + (this.state.notifications.length == 0 ? " hidden" : ""),
						},
						this.state.notificationCount
					)
				])
			]),
			Clipperz.PM.UI.Components.Panels.ExtraFeaturesPanel.NotificationBox({
				ref: "notificationBox",
				notifications: this.state.notifications,
				updateNotificationCount: this.updateNotificationCount
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
});

Clipperz.PM.UI.Components.Panels.ExtraFeaturesPanel.NotificationBox = React.createClass({
	getInitialState: function() {
	    return {notifications: this.props.notifications};
	},
	getNotificationCount: function(){
		return this.state.notifications.length;
	},
	closeItemHandler: function(e){
		var button = e.target;
		var item = button.parentNode; //#notification-{index}
		var notifications = this.state.notifications;
		var index = parseInt(item.getAttribute("data-index"));

		if (index > -1) {
		    notifications.splice(index, 1);
		}

		this.setState({notifications: notifications});
		this.props.updateNotificationCount(this);
	},
	render: function(){
		var notification_list_items = [];

		for (var i=0; i < this.state.notifications.length; i++) {
			notification_list_items.push(
				React.DOM.li({id: "notification-" + i, className: this.state.notifications[i].severity, "data-index": i},
					React.DOM.span({}, this.state.notifications[i].text),
					React.DOM.button({
							type:"button", 
							className:"close", 
							"data-dismiss":"modal",
							onClick:this.closeItemHandler
						},
						React.DOM.span({"aria-hidden":"true"}, "×")
					)
				)
			)
		}

		return React.DOM.div({className: "notifications" + (this.state.notifications.length == 0 ? " hidden" : "")},
			React.DOM.label({}, "Notifications"),
				React.DOM.ul({className:"items"}, notification_list_items
			)
		);
	}	
});
