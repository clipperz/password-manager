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

Clipperz.PM.UI.Components.CertificateQueueBoxClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.CertificateQueueBox',

	propTypes: {
		'certificateQueueInfo':			React.PropTypes.object.isRequired,
		'certificateQueueBoxStatus':	React.PropTypes.string.isRequired,
	},

	acknowledgeCertificate: function (aRecordReference) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'closeCertificateNotification', aRecordReference);
	},

	showCertificateCard: function (aRecordReference) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'showCertificateCard', aRecordReference);
	},
	
	renderCertificateInfo: function (aCertificateInfo) {
		var	result;

		if (aCertificateInfo['status'] != 'acknowledged') {
			result = React.DOM.li({'className':aCertificateInfo['status']}, [
				React.DOM.span({'className':'icon'}, "certificate"),
				React.DOM.span({'className':'name', 'onClick':MochiKit.Base.method(this, 'showCertificateCard', aCertificateInfo['reference'])}, aCertificateInfo['label']),
				React.DOM.span({'className':'status'}, [
					React.DOM.span({'className':'statusString'}, aCertificateInfo['status'] == 'requested' ? 'pending' : 'registered')
				]),
				React.DOM.span({'className':'close'}, [
					React.DOM.a({'className':'close', 'onClick':MochiKit.Base.method(this, 'acknowledgeCertificate', aCertificateInfo['reference'])}, "remove field")
				])
			]);
		} else {
			result = null;
		}

		return result;
	},

	render: function () {
		var	certificatesInfo;
		
		certificatesInfo = this.props['certificateQueueInfo'] ? MochiKit.Base.map(MochiKit.Base.method(this, 'renderCertificateInfo'), this.props['certificateQueueInfo']) : [];
		certificatesInfo = MochiKit.Base.filter(function (aValue) { return aValue != null; }, certificatesInfo);

		return	React.DOM.div({
			'className': 'certificateQueueStatus ' + this.props['certificateQueueBoxStatus'],
		}, [
			React.DOM.div({'className': 'arrow'}),
			React.DOM.ul({'className':'certificateInfos'}, (certificatesInfo.length > 0) ? certificatesInfo : React.DOM.p({}, "No certificates pending")),
		]);
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.CertificateQueueBox = React.createFactory(Clipperz.PM.UI.Components.CertificateQueueBoxClass);
