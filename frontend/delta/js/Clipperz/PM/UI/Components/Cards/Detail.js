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

Clipperz.Base.module('Clipperz.PM.UI.Components.Cards');

Clipperz.PM.UI.Components.Cards.DetailClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.Cards.Detail',

	propTypes: {
		'allTags':	React.PropTypes.array,
	},

	viewComponentProps: function () {
		var	result;
		var	props = this.props;
		var	propertiesToPassAlong = [
			'features',
			'style',
			'showGlobalMask',
			'allTags',
			'preferences',
			'attachmentQueueInfo',
			'proxyInfo',
			'showCertificatePreview',
			'certificateDetails'
		];

		result = props['selectedCard'];
		if (result) {
//			result['features'] = this.props['features'];
//			result['style'] = this.props['style'];
			result['ask'] = (props['style'] == 'narrow') ? props['ask'] : null;
//			result['showGlobalMask'] = this.props['showGlobalMask'];
//			result['allTags'] = this.props['allTags'];
//			result['preferences'] = this.props['preferences'];
//			result['attachmentQueueInfo'] = this.props['attachmentQueueInfo'];
//			result['proxyInfo'] = this.props['proxyInfo'];
//			result['showCertificatePreview'] = this.props['showCertificatePreview'];
			MochiKit.Iter.forEach(propertiesToPassAlong, function (aProperty) { result[aProperty] = props[aProperty]; });
		}
		
		return result;
	},
	
	render: function () {
		var	result;

		if (this.props['mode'] == 'edit') {
			result = Clipperz.PM.UI.Components.Cards.Edit(this.viewComponentProps());
		} else {
			result = Clipperz.PM.UI.Components.Cards.View(this.viewComponentProps());
		}
		
		return result;
	},
});

Clipperz.PM.UI.Components.Cards.Detail = React.createFactory(Clipperz.PM.UI.Components.Cards.DetailClass);