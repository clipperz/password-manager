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
Clipperz.Base.module('Clipperz.PM.UI.Components');

Clipperz.PM.UI.Components.AttachmentQueueBoxClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.AttachmentQueueBox',

	propTypes: {
		'attachmentQueueInfo':		React.PropTypes.object.isRequired,
		'attachmentQueueBoxStatus':	React.PropTypes.string.isRequired,
	},

	getInitialState: function() {
		return {
			'unreadNotifications': [],
		};
	},

	//=========================================================================
	// Actions
	//=========================================================================

	handleClose: function(aNotificationElement) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'closeAttachmentNotification', aNotificationElement['id']);
	},

	//=========================================================================
	// Render Methods
	//=========================================================================

	renderNotificationElement: function(aNotificationElement) {
		var processIcon, status, closeButton, progressIndicator;

		var queueElement = aNotificationElement['queueElement'];

		processIcon = (queueElement['process'] == 'UPLOAD') ? "\u2191" : "\u2193";

		status = "waiting";
		closeButton = null;
		progressIndicator = null;
		if (queueElement['status'] == 'DOWNLOADING' || queueElement['status'] == 'UPLOADING') {
			status = Math.floor(queueElement['requestProgress']*100) + '%';
			progressIndicator = React.DOM.span({'className': 'progress'}, Clipperz.PM.UI.Components.RadialProgressIndicator({'progress': queueElement['requestProgress']}));
		}

		if (queueElement['status'] == 'DONE' || queueElement['status'] == 'CANCELED' || queueElement['status'] == 'FAILED') {
			status = (queueElement['status'] == 'DONE') ? 'completed' : (queueElement['status'] == 'FAILED') ? "failed" : "canceled";

			closeButton = React.DOM.span({'className': 'close'}, React.DOM.a({
				'className': 'close',
				'onClick': MochiKit.Base.method(this, 'handleClose', aNotificationElement),
			}, "remove field"));
		}

		return React.DOM.li({},[
			React.DOM.span({'className': 'contentType'}, Clipperz.PM.DataModel.Attachment.contentTypeIcon(queueElement['meta']['type'])),
			React.DOM.span({'className': 'name'}, queueElement['meta']['name']),
			// React.DOM.span({'className': 'size'}, queueElement['meta']['size']),
			React.DOM.span({'className': 'status'}, [
				React.DOM.span({'className': 'statusString'}, status),
				React.DOM.span({'className': 'processIcon'}, processIcon),
			]),
			progressIndicator,
			closeButton,
		]);
	},

	renderNotifications: function(someNotifications) {
		var result;

		if (someNotifications.length == 0) {
			result = React.DOM.p({}, "No attachments in queue");
		} else {
			result = MochiKit.Base.map(MochiKit.Base.method(this, 'renderNotificationElement'), someNotifications);
		}

		return result;
	},

	// renderNotifications: function(someNotifications) {
	// 	console.log('AttachmentQueueBox.renderNotifications:', someNotifications);
	// },

	render: function () {
		//test
//		this.renderNotifications(this.props['attachmentQueueInfo']['notifications']);

		return	React.DOM.div({
			'className': 'attachmentQueueStatus '+this.props['attachmentQueueBoxStatus'],
		}, [
			React.DOM.div({'className': 'arrow'}),
			React.DOM.ul({}, this.renderNotifications(this.props['attachmentQueueInfo']['notifications'])),
		]);
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.AttachmentQueueBox = React.createFactory(Clipperz.PM.UI.Components.AttachmentQueueBoxClass);
