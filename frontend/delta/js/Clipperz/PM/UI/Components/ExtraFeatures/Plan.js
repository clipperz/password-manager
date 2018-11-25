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
Clipperz.Base.module('Clipperz.PM.UI.Components.ExtraFeatures');

Clipperz.PM.UI.Components.ExtraFeatures.PlanClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.Plan',

	propTypes: {
	},

	formatQuota: function (aValue) {
		return filesize(aValue);
	},

	percentageOfUsedAttachmentQuota: function () {
		return this.props['accountInfo']['attachmentQuota']['used'] / this.props['accountInfo']['attachmentQuota']['available'] * 100;
	},

	//=========================================================================

	hasSomeCertificate: function () {
		return ((this.props['accountInfo']['certificateQuota']['used']['published'] > 0) || (this.props['accountInfo']['certificateQuota']['used']['requested'] > 0));
	},

	renderCertificateSection: function () {
		var	result;
		
		if (this.hasSomeCertificate()) {
			result = React.DOM.div({'className': 'certificates'}, [
				React.DOM.h2({}, "Certificates"),
				this.renderCertificateIcons(),
			]);
		} else {
			result = null;
		}
		
		return result;
	},

	renderCertificateIcons: function () {
		var	result;
		var	totalCertificates;
		var	publishedCertificates;
		var	requestedCertificates;
		var	availableCertificates;
//		var	i, c;
		
//		result = [];
		totalCertificates = this.props['accountInfo']['certificateQuota']['totalNumber'];
		publishedCertificates = this.props['accountInfo']['certificateQuota']['used']['published'];
		requestedCertificates = this.props['accountInfo']['certificateQuota']['used']['requested'];
		availableCertificates = totalCertificates - (publishedCertificates + requestedCertificates);
		
//		c = totalCertificates;
//		for (i=0; i<c; i++) {
//			var className;
//			
//			if (i < publishedCertificates) {
//				className = 'published';
//			} else if (i < (publishedCertificates + requestedCertificates)) {
//				className = 'requested';
//			} else {
//				className = 'available'
//			};
//			result.push(React.DOM.li({'className':className}, "certificate"));
//		}
		
		return React.DOM.ul({}, [
			React.DOM.li({'className':'published'}, "published: " + ((publishedCertificates > 0) ? publishedCertificates : '-')),
			React.DOM.li({'className':'requested'}, "requested: " + ((requestedCertificates > 0) ? requestedCertificates : '-')),
//			React.DOM.li({'className':'available'}, "available: " + ((availableCertificates > 0) ? availableCertificates : '-')),
		]);
	},
	
	render: function () {
		return	React.DOM.div({className:'extraFeature plan'}, [
			React.DOM.div({'className':'header'}, [
				React.DOM.h1({}, "Current plan"),
			]),
			React.DOM.div({'className': 'content'}, [

				React.DOM.div({'className': 'attachments'}, [
					React.DOM.h2({}, "Attachments quota"),
					React.DOM.dl({}, [
						React.DOM.dt({}, [
							React.DOM.span({'className':'key'}, 'available')
						]), React.DOM.dd({}, this.formatQuota(this.props['accountInfo']['attachmentQuota']['available'])),
						React.DOM.dt({}, [
							React.DOM.span({'className':'key'}, 'used'),
						]), React.DOM.dd({}, this.formatQuota(this.props['accountInfo']['attachmentQuota']['used'])),
					]),
					React.DOM.div({'className': 'progressBar'}, [
						React.DOM.span({'className': 'progress', 'style': {'width': this.percentageOfUsedAttachmentQuota() + '%'}})
//						MochiKit.Style.setElementDimensions(this.getElement('progress'), {'w': aProgressPercentage}, '%');
					])
				]),

				this.renderCertificateSection(),
				
//				React.DOM.div({'className': 'upgrade'}, [
//					React.DOM.button({'className':'button disabled', 'type':'submit'}, "upgrade")
//				])

			])
		]);
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.ExtraFeatures.Plan = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.PlanClass);
