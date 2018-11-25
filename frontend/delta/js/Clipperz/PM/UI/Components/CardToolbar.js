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

Clipperz.Base.module('Clipperz.PM.UI.Components');

Clipperz.PM.UI.Components.CardToolbarClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.CardToolbar',

	propTypes: {
//		'style':				React.PropTypes.oneOf(['extra-short', 'narrow', 'wide', 'extra-wide']).isRequired,
		'style':				React.PropTypes.oneOf(Clipperz_PM_UI_availableStyles).isRequired,
		'enableSidePanels':		React.PropTypes.bool.isRequired,
		'accountInfo':			React.PropTypes.object.isRequired,
		'proxyInfo':			React.PropTypes.object.isRequired,
		'messageBox':			React.PropTypes.object.isRequired,
		'filter':				React.PropTypes.object /*.isRequired */,
		'attachmentQueueInfo':	React.PropTypes.object.isRequired,
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
	
	attachmentQueueToggleHandler: function(anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleAttachmentQueueBox');
	},

	//============================================================================

	certificateQueueToggleHandler: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleCertificateQueueBox');
	},
	
	//============================================================================

	notificationCounter: function () {
		var	result;
		
//		console.log("CARD TOOLBAR NOTIFICATIONS", this.props['notifications']);

		if (this.props['notifications']) {
			result = this.props['notifications'].length;
		} else {
			result = null;
		}
/*
		var attachmentDownloadNotificationNumber = MochiKit.Base.filter(function(anElement) {
			return anElement['queueElement']['process'] == 'DOWNLOAD';
		}, this.props['attachmentQueueInfo']['notifications']).length;

		var attachmentUploadNotificationNumber = this.props['attachmentQueueInfo']['notifications'].length - attachmentDownloadNotificationNumber;
//		attachmentDownloadNotificationNumber = 2;
//		attachmentUploadNotificationNumber = 3;
/*
		var verifyingCertificateNotificationNumber = this.props['certificateQueueInfo'] ? MochiKit.Base.filter(function (anCertificationInfo) {
			return anCertificationInfo['status'] == 'requested';
		}, this.props['certificateQueueInfo']).length : 0;
		var verifiedCertificateNotificationNumber = this.props['certificateQueueInfo'] ? MochiKit.Base.filter(function (anCertificationInfo) {
			return anCertificationInfo['status'] == 'published';
		}, this.props['certificateQueueInfo']).length : 0;
*/
		return result;
	},

	notificationLevel: function () {
		var	result = 'info';
		var i, c;
		
		c = this.props['notifications'].length;
		for (i=0; i<c; i++) {
			var	level;
			
			level = this.props['notifications'][i]['level'];
			if (level == 'error') {
				result = 'error';
			} else if ((level == 'warning') && (result == 'info')) {
				result = 'warning';
			}
		}
		
		return result;
	},

	renderWithSidePanels: function () {
		return [
			React.DOM.div({className:'selectionToggle'}, [
				Clipperz.PM.UI.Components.Button({'eventName':'selectionToggleButton', 'label':"tags", 'handler':this.selectionToggleHandler})
			]),
			this.renderWithoutSidePanels(),

			// TODO: validate and adjust names
			React.DOM.div({'className':'settingsToggle'}, [
//				Clipperz.PM.UI.Components.Button({eventName:'certificateQueueToggleButton', label:"certificate",  handler:this.certificateQueueToggleHandler, badgeTopContent:verifyingCertificateNotificationNumber, badgeBottomContent:verifiedCertificateNotificationNumber}),
//				Clipperz.PM.UI.Components.Button({eventName:'attachmentQueueToggleButton',  label:"\u2191\u2193", handler:this.attachmentQueueToggleHandler,  badgeTopContent:attachmentDownloadNotificationNumber, badgeBottomContent:attachmentUploadNotificationNumber}),
				Clipperz.PM.UI.Components.Button({'eventName':'settingsToggleButton', 'label':"menu", 'handler':this.settingsToggleHandler, 'badgeTopContent':this.notificationCounter(), 'badgeTopLevel':this.notificationLevel()})
			])
		];
	},
	
	renderWithoutSidePanels: function () {
		var	result;
//console.log("CARD TOOLBAR", this.props);

		if (this.props['filter']) {
//console.log("CARD TOOLBAR", this.props['filter']['type']);

			if (this.props['filter']['type'] == 'RECENT') {
				result = [React.DOM.div({'className':'clipperz'}, [React.DOM.span({'className':'logo recent'}, "recent")])];
			} else if (this.props['filter']['type'] == 'WITH_ATTACHMENTS') {
					result = [React.DOM.div({'className':'clipperz'}, [React.DOM.span({'className':'logo withAttachments'}, "attachment")])];
			} else if (this.props['filter']['type'] == 'TAG') {
				result = [React.DOM.div({'className':'clipperz'}, [
					React.DOM.span({'className':'logo tag'}, "tag"),
					React.DOM.span({'className':'value'}, this.props['filter']['value'])
				])];
			} else if (this.props['filter']['type'] == 'UNTAGGED') {
				result = [React.DOM.div({'className':'clipperz'}, [
					React.DOM.span({'className':'logo tag'}, "tag"),
					React.DOM.span({'className':'value'}, "untagged")
				])];
			} else if (this.props['filter']['type'] == 'SEARCH') {
				result = [React.DOM.div({'className':'clipperz'}, [
					React.DOM.span({'className':'logo search'}, "search"),
					React.DOM.span({'className':'value'}, this.props['filter']['value'])
					])];
			} else {
				result = [React.DOM.div({'className':'clipperz'}, [React.DOM.span({'className':'logo clipperz'}, "clipperz")])];
			}
		} else {
			result = [React.DOM.div({'className':'clipperz'}, [React.DOM.span({'className':'logo clipperz'}, "clipperz")])];
		}
		
		return result;
	},

	render: function () {
		return	React.DOM.div({'className':'cardToolbar ' + this.props['style']}, [
			React.DOM.header({}, this.props['enableSidePanels'] ? this.renderWithSidePanels() : this.renderWithoutSidePanels()),
			Clipperz.PM.UI.Components.AccountStatus(MochiKit.Base.update(this.props['accountInfo'], this.props['proxyInfo'])),
			Clipperz.PM.UI.Components.MessageBox(this.props['messageBox']),
		]);
	}

	//============================================================================
});

Clipperz.PM.UI.Components.CardToolbar = React.createFactory(Clipperz.PM.UI.Components.CardToolbarClass);
