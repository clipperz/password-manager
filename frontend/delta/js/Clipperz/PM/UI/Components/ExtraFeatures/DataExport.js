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

Clipperz.PM.UI.Components.ExtraFeatures.DataExportClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.DataExport',

	propTypes: {
//		featureSet:			React.PropTypes.oneOf(['FULL', 'EXPIRED', 'TRIAL']).isRequired,
//		'level':	React.PropTypes.oneOf(['hide', 'info', 'warning', 'error']).isRequired
	},
/*	
	jsonExport: function () {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'export', 'json');
	},
	
	htmlExport: function () {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'export', 'html');
	},
*/

	isFeatureEnabled: function (aValue) {
		return (this.props['features'].indexOf(aValue) > -1);
	},

	handleDownloadOfflineCopyLink: function (anEvent) {
		if (this.isFeatureEnabled('OFFLINE_COPY')) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'downloadOfflineCopy');
		}
	},

	handleExportLink: function () {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'downloadExport');
	},


	//=========================================================================

	render: function () {
		return	React.DOM.div({className:'extraFeature devicePIN'}, [
			React.DOM.div({'className':'header'}, [
				React.DOM.h1({}, "Export"),
			]),
			React.DOM.div({'className': 'content'}, [
				React.DOM.ul({}, [
					React.DOM.li({}, [
						React.DOM.h3({}, "Offline copy"),
						React.DOM.div({'className':'description'}, [
							React.DOM.p({}, "Download a read-only portable version of Clipperz. Very convenient when no Internet connection is available."),
							React.DOM.p({}, "An offline copy is just a single HTML file that contains both the whole Clipperz web application and your encrypted data, except file attachments."),
							React.DOM.p({}, "It is as secure as the hosted Clipperz service since they both share the same code and security architecture.")
						]),
						React.DOM.a({'className':'button', 'onClick':this.handleDownloadOfflineCopyLink}, "download offline copy")
					]),
					React.DOM.li({}, [
						React.DOM.h3({}, "HTML + JSON"),
						React.DOM.div({'className':'description'}, [
							React.DOM.p({}, "Download a printer-friendly HTML file that lists the content of all your cards."),
							React.DOM.p({}, "This same file also contains all your data in JSON format. Please note that file attachments are not included."),
							React.DOM.p({'className':'warning'}, "Beware: all data are unencrypted! Therefore make sure to properly store and manage this file.")
						]),
						React.DOM.a({'className':'button', 'onClick':this.handleExportLink}, "download HTML+JSON")
					]),
/*
					React.DOM.li({}, [
						React.DOM.h3({}, "Printing"),
						React.DOM.div({'className':'description'}, [
							React.DOM.p({}, "Click on the button below to open a new window displaying all your cards in a printable format."),
							React.DOM.p({}, "If you are going to print for backup purposes, please consider the safer option provided by the “offline copy”.")
						]),
						React.DOM.a({'className':'button', 'onClick':this.htmlExport}, "HTML")
					]),
					React.DOM.li({}, [
						React.DOM.h3({}, "Exporting to JSON"),
						React.DOM.div({'className':'description'}, [
							React.DOM.p({}, "JSON enables a “lossless” export of your cards. All the information will be preserved, including direct login configurations."),
							React.DOM.p({}, "This custom format it’s quite convenient if you need to move some of all of your cards to a different Clipperz account. Or if you want to restore a card that has been accidentally deleted."),
							React.DOM.p({}, "Click on the button below to start the export process.")
						]),
						React.DOM.a({'className':'button', 'onClick':this.jsonExport}, "JSON"),
					])
*/
				])
			])
		]);
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.ExtraFeatures.DataExport = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataExportClass);
