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

Clipperz.PM.UI.Components.Panels.ExtraFeaturesPanelClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.Panels.ExtraFeaturesPanel',

	componentDidMount: function () {
		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'closeSettingsPanel', MochiKit.Base.method(this, 'hideExtraFeatureContent'));
	},

	settingsToggleHandler: function (anEvent) {
		this.hideExtraFeatureContent();
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleSettingsPanel');
	},

	handleDownloadOfflineCopyLink: function (anEvent) {
		if (this.isFeatureEnabled('OFFLINE_COPY')) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'downloadOfflineCopy');
		}
	},

	propTypes: {
		'accountInfo':	React.PropTypes.object.isRequired,
		'userInfo':		React.PropTypes.object.isRequired
	},

	getInitialState: function() {
		return {
			'index': {
				'account':		false,
				'subscription':	false,
				'data':			false,
				'about':		false,
			},
			'isFullyOpen':	false,
			'extraFeatureComponentName': null,
			'extraFeatureContent': null
		};
	},

	toggleIndexState: function (section) {
		return MochiKit.Base.bind(function () {
			var	newState = { 'index': this.state['index'] };
			
			newState['index'][section] = !this.state['index'][section];
			this.setState(newState);
		}, this);
	},

	isFeatureEnabled: function (aValue) {
		return (this.props['features'].indexOf(aValue) > -1);
	},

	showUrl: function (anUrl) {
		return function () {
			window.open(anUrl, 'clipperz_about');
		}
	},

	lock: function () {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'lock');
	},

	logout: function () {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'logout');
	},

	//=========================================================================

	toggleExtraFeatureComponent: function (aComponentName) {
		return MochiKit.Base.bind(function () {
			if (this.state['extraFeatureComponentName'] != aComponentName) {
				this.showExtraFeatureContent(Clipperz.PM.UI.Components.ExtraFeatures[aComponentName], aComponentName);
			} else {
				this.hideExtraFeatureContent();
			}
		}, this);
	},

	extraFeaturesProps: function () {
		return this.props;
	},

	//-------------------------------------------------------------------------

	hideExtraFeatureContent: function () {
		this.setState({
			'isFullyOpen': false,
			'extraFeatureComponentName': null,
			'extraFeatureContent': null,
			'extraFeatureContentComponent': null
		});
	},

	showExtraFeatureContent: function (aComponent, aComponentName) {
		if (aComponentName == 'OTP') {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'updateOTPListAndDetails');
		}
		
		if (aComponentName == 'Plan') {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'updateUserAccountInfo');
		}
		
		this.setState({
			'isFullyOpen':true,
			'extraFeatureComponentName': aComponentName,
			'extraFeatureContentComponent': aComponent // Trying to instantiate the component at every render
		});
	},
	
	//=========================================================================

	acknowledgeNotification: function (aNotificationID) {
		return MochiKit.Base.bind(function () {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'acknowledgeNotification', aNotificationID);
		}, this);
	},

	renderNotification: function (aNotification) {
		return	React.DOM.div({'key':aNotification['id'], 'className':Clipperz.PM.UI.Components.classNames('notification', aNotification['level'])}, [
			React.DOM.span({'className':'acknowledge', 'onClick':this.acknowledgeNotification(aNotification['id'])}, [
				React.DOM.span({}, "close")
			]),
			React.DOM.span({'className':'message'}, aNotification['message'])
		]);
	},

	renderIndex: function () {
		var	offlineCopyButtonClasses = {
			'button': true,
			'disabled': !this.isFeatureEnabled('OFFLINE_COPY')
		}
		
		return	React.DOM.div({'className':'extraFeatureIndex'}, [
			React.DOM.header({'key':'header'}, [
				React.DOM.div({'key':'headerDiv', 'className':'settingsToggle'}, [
					Clipperz.PM.UI.Components.Button({'key':'button', 'eventName':'settingsToggleButton', 'label':"menu", 'handler':this.settingsToggleHandler})
				])
			]),
			React.DOM.div({'key':'ulWrapper'}, [
				this.props['notifications'].length ? React.DOM.div({'key':'notifications', 'className':'notifications'}, MochiKit.Base.map(MochiKit.Base.method(this, 'renderNotification'), this.props['notifications'])) : null,
				React.DOM.ul({'key':'ul'}, [
					React.DOM.li({'key':'account', 'className':this.state['index']['account'] ? 'open' : 'closed'}, [
						React.DOM.h1({'key':'accountH1', 'onClick':this.toggleIndexState('account')}, "Account"),
						React.DOM.ul({'key':'accountUL'}, [
							React.DOM.li({'key':'account_0', 'onClick':this.toggleExtraFeatureComponent('Preferences'), 'className':(this.state['extraFeatureComponentName'] == 'Preferences') ? 'selected' : ''}, [
								React.DOM.h2({}, "Preferences"),
							]),
							React.DOM.li({'key':'account_1', 'onClick':this.toggleExtraFeatureComponent('Passphrase'), 'className':(this.state['extraFeatureComponentName'] == 'Passphrase') ? 'selected' : ''}, [
								React.DOM.h2({}, "Passphrase"),
//								React.DOM.div({'key':'account_1_div'}, [
//									React.DOM.p({'key':'account_1_p'}, "Change your account passphrase.")
//								])
							]),
							React.DOM.li({'key':'account_2', 'onClick':this.toggleExtraFeatureComponent('Plan'), 'className':(this.state['extraFeatureComponentName'] == 'Plan') ? 'selected' : ''}, [
								React.DOM.h2({}, "Plan"),
//								React.DOM.div({}, [
//									React.DOM.p({}, "Current plan.")
//								])
							]),
							React.DOM.li({'key':'account_3', 'onClick':this.toggleExtraFeatureComponent('OTP')}, [
								React.DOM.h2({}, "One-Time Passwords"),
//								React.DOM.div({}, [
//									React.DOM.p({}, "Manage your OTPs.")
//								])
							]),

							React.DOM.li({'key':'account_4', 'onClick':this.toggleExtraFeatureComponent('DevicePIN')}, [
								React.DOM.h2({}, "Device PIN"),
//								React.DOM.div({}, [
//									React.DOM.p({}, "Configure a PIN that will allow to get access to your cards, but only on this device.")
//								])
							]),

							React.DOM.li({'key':'account_5', 'onClick':this.toggleExtraFeatureComponent('DeleteAccount'), 'className':(this.state['extraFeatureComponentName'] == 'DeleteAccount') ? 'selected' : ''}, [
								React.DOM.h2({}, "Delete account"),
//								React.DOM.div({}, [
//									React.DOM.p({}, "Delete your account for good.")
//								])
							])
						])
					]),
/*
					React.DOM.li({'key':'subscription', 'className':this.state['index']['subscription'] ? 'open' : 'closed'}, [
						React.DOM.h1({'onClick':this.toggleIndexState('subscription')}, "Subscription"),
						React.DOM.ul({'key':'subscription'}, [
							React.DOM.li({'key':'subscription_1'}, [
								React.DOM.h2({}, "x1"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							]),
							React.DOM.li({'key':'subscription_2'}, [
								React.DOM.h2({}, "x2"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							]),
							React.DOM.li({'key':'subscription_3'}, [
								React.DOM.h2({}, "x3"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							]),
							React.DOM.li({'key':'subscription_4'}, [
								React.DOM.h2({}, "x4"),
								React.DOM.div({}, [
									React.DOM.p({}, "")
								])
							])
						])
					]),
*/
					React.DOM.li({'key':'data', 'className':this.state['index']['data'] ? 'open' : 'closed'}, [
						React.DOM.h1({'onClick':this.toggleIndexState('data')}, "Data"),
						React.DOM.ul({'key':'data'}, [
//							React.DOM.li({'key':'data_1'}, [
//								React.DOM.h2({}, "Offline copy"),
//								React.DOM.div({}, [
//									React.DOM.p({}, "With just one click you can dump all your encrypted data from Clipperz servers to your hard disk and create a read-only offline version of Clipperz to be used when you are not connected to the Internet."),
//									React.DOM.a({'className':Clipperz.PM.UI.Components.classNames(offlineCopyButtonClasses), 'onClick':this.handleDownloadOfflineCopyLink}, "Download")
//								])
//							]),
							React.DOM.li({'key':'data_2', 'onClick':this.toggleExtraFeatureComponent('DataImport'), 'className':(this.state['extraFeatureComponentName'] == 'DataImport') ? 'selected' : ''}, [
								React.DOM.h2({}, "Import"),
//								React.DOM.div({}, [
//									React.DOM.p({}, "CSV, JSON, …")
//								])
							]),
							React.DOM.li({'key':'data_3', 'onClick':this.toggleExtraFeatureComponent('DataExport'), 'className':(this.state['extraFeatureComponentName'] == 'DataExport') ? 'selected' : ''}, [
								React.DOM.h2({}, "Export"),
//								React.DOM.div({}, [
//									React.DOM.p({}, "Offline copy, printable version, JSON, …")
//								])
							]),
/*
							React.DOM.li({'key':'data_4'}, [
								React.DOM.h2({}, "Sharing"),
								React.DOM.div({}, [
									React.DOM.p({}, "Securely share cards with other users")
								])
							])
*/
						])
					]),
					React.DOM.li({'key':'about', 'className':this.state['index']['about'] ? 'open' : 'closed'}, [
						React.DOM.h1({'key':'aboutH1', 'onClick':this.toggleIndexState('about')}, "About"),
						React.DOM.div({'key':'address', 'className':'address'}, [
							"Clipperz Srl",
							"Via Selice, 66/a",
							"40026 Imola",
							"Italy"
						]),
						React.DOM.ul({'key':'data'}, [
							React.DOM.li({'key':'about_1', 'className':'link', 'onClick':this.showUrl('/')}, [
								React.DOM.h2({}, "Website"),
							]),
							React.DOM.li({'key':'about_2', 'className':'link', 'onClick':this.showUrl('/forum/')}, [
								React.DOM.h2({}, "Forum"),
							]),
//							React.DOM.li({'key':'about_3', 'className':'link', 'onClick':this.showUrl('/blog')}, [
//								React.DOM.h2({}, "Blog"),
//							]),
							React.DOM.li({'key':'about_4', 'className':'link', 'onClick':this.showUrl('https://twitter.com/clipperz')}, [
								React.DOM.h2({}, "Twitter"),
							]),
							React.DOM.li({'key':'about_5', 'className':'link', 'onClick':this.showUrl('/about/contacts/')}, [
								React.DOM.h2({}, "Contacts"),
							]),
							React.DOM.li({'key':'about_6', 'className':'link', 'onClick':this.showUrl('/terms_service/')}, [
								React.DOM.h2({}, "Terms of service"),
							]),
							React.DOM.li({'key':'about_7', 'className':'link', 'onClick':this.showUrl('/privacy_policy/')}, [
								React.DOM.h2({}, "Privacy"),
							]),
						])
					]),
					React.DOM.li({'key':'logout', 'className':'lock-logout'}, [
						React.DOM.h2({'className':'lock',   'onClick':this.lock},   "Lock"),
						React.DOM.h2({'className':'logout', 'onClick':this.logout}, "Logout"),
					])
				])
			]),
			React.DOM.footer({'key':'footer', 'className':'applicationVersion'}, [
				React.DOM.span({'key':'applicationVersion'}, "application version"),
				React.DOM.a({'key':'applicationVersionLink', 'href':'https://github.com/clipperz/password-manager/commit/' + Clipperz_version, 'target':'github'}, Clipperz_version)
			])
		]);
	},
	
	renderContent: function () {
		return	React.DOM.div({'className':'extraFeatureContent'}, [
			React.DOM.header({}, [
				React.DOM.div({'className':'button', 'onClick':this.hideExtraFeatureContent}, "close")
			]),
			this.state['extraFeatureContentComponent'] ? this.state['extraFeatureContentComponent'](this.props) : null,
//			React.DOM.footer({}, "")
		]);
	},

	render: function () {
//console.log("ExtraFeaturesPanel props", this.props);
		var isOpen = (this.props['settingsPanelStatus'] == 'OPEN');
		var isFullyOpen = isOpen && this.state['isFullyOpen'];
		
		var	classes = {
			'panel': true,
			'right': true,
			'open': isOpen,
			'fullOpen': isFullyOpen
		}
		
		return	React.DOM.div({'key':'extraFeaturesPanel', 'id':'extraFeaturesPanel', 'className':Clipperz.PM.UI.Components.classNames(classes)}, [
			this.renderIndex(),
			this.renderContent(),
//			(this.props['settingsPanelStatus'] == 'OPEN') ? this.renderContent() : null,
		]);
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.Panels.ExtraFeaturesPanel = React.createFactory(Clipperz.PM.UI.Components.Panels.ExtraFeaturesPanelClass);
