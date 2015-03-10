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
Clipperz.Base.module('Clipperz.PM.UI.Components.Panels');

Clipperz.PM.UI.Components.Panels.ExtraFeaturesPanel = React.createClass({

	settingsToggleHandler: function (anEvent) {
//console.log("settingsToggleHandler");
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleSettingsPanel');
	},

	handleDownloadOfflineCopyLink: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'downloadOfflineCopy');
	},

	propTypes: {
		'accountInfo':	React.PropTypes.object.isRequired,
	},

	getInitialState: function() {
		return {
			'account':		false,
			'subscription':	false,
			'data':			false,
		};
	},

	toggleState: function (section) {
		return MochiKit.Base.bind(function () {
			var	newState = {};
			
			newState[section] = !this.state[section];
			this.setState(newState);
		}, this);
	},

	//=========================================================================

	render: function () {
//console.log("ExtraFeaturesPanel props", this.props);
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

			React.DOM.div({}, [
				React.DOM.ul({}, [
					React.DOM.li({'className':this.state['account'] ? 'open' : 'closed'}, [
						React.DOM.h1({'onClick':this.toggleState('account')}, "Account"),
						React.DOM.ul({}, [
							React.DOM.li({}, [
								React.DOM.h2({}, "Passphrase"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							]),
							React.DOM.li({}, [
								React.DOM.h2({}, "One Time Passwords"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							]),
							React.DOM.li({}, [
								React.DOM.h2({}, "Device PIN"),
								React.DOM.div({}, [
									React.DOM.p({}, "Configure a PIN that will allow to get access to your cards, but only on this device.")
								])
							]),
							React.DOM.li({}, [
								React.DOM.h2({}, "Preferences"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							]),
							React.DOM.li({}, [
								React.DOM.h2({}, "Delete account"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							])
						])
					]),
					React.DOM.li({'className':this.state['subscription'] ? 'open' : 'closed'}, [
						React.DOM.h1({'onClick':this.toggleState('subscription')}, "Subscription"),
						React.DOM.ul({}, [
							React.DOM.li({}, [
								React.DOM.h2({}, "x1"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							]),
							React.DOM.li({}, [
								React.DOM.h2({}, "x2"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							]),
							React.DOM.li({}, [
								React.DOM.h2({}, "x3"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							]),
							React.DOM.li({}, [
								React.DOM.h2({}, "x4"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							])
						])
					]),
					React.DOM.li({'className':this.state['data'] ? 'open' : 'closed'}, [
						React.DOM.h1({'onClick':this.toggleState('data')}, "Data"),
						React.DOM.ul({}, [
							React.DOM.li({}, [
								React.DOM.h2({}, "Offline copy"),
								React.DOM.div({}, [
									React.DOM.p({}, "With just one click you can dump all your encrypted data from Clipperz servers to your hard disk and create a read-only offline version of Clipperz to be used when you are not connected to the Internet."),
									React.DOM.a({'className':'button', 'onClick':this.handleDownloadOfflineCopyLink}, "Download")
								])
							]),
							React.DOM.li({}, [
								React.DOM.h2({}, "Import"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							]),
							React.DOM.li({}, [
								React.DOM.h2({}, "Export"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							]),
							React.DOM.li({}, [
								React.DOM.h2({}, "Sharing"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							])
						])
					])
				])
			])
		]);
	}

	//=========================================================================
});
