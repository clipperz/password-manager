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

Clipperz.PM.UI.Components.DocumentEncryptionSandboxClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.DocumentEncryptionSandbox',

	getInitialState: function() {
		return {
			'documentMeta': null,
			'encryptedDocument': null,
		};
	},

	//=========================================================================
	// Event Handlers
	//=========================================================================

	handleFileSelect: function(anEvent) {
// console.log("handleFileSelect: files:", anEvent.target.files);

		var fileToRead = anEvent.target.files[0];

// console.log(fileToRead);
// return;

		if (fileToRead) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'uploadNewFile', null, fileToRead);
		}
	},

	//=========================================================================
	// File Handling Methods
	//=========================================================================
		// handleEncryptionSynchronously: function(aKey, aByteArray) {
		// 	var encryptedDocument;
		// 	var startTime, endTime;

		// console.log("Encrypting...");
		// 	startTime = Date.now()
		// 	encryptedDocument = Clipperz.Crypto.AES.encrypt(aKey, aByteArray);
		// 	endTime   = Date.now();
		// console.log("Finished! Encryption took", (endTime-startTime)/1000, "seconds.");

		// 	this.setState({
		// 		'encryptedDocument': encryptedDocument,
		// 	});
		// },

	downloadFile: function(aFileReference) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'downloadFile', aFileReference);
	},

	//=========================================================================
	// Render Methods
	//=========================================================================

	renderUploadButton: function() {
		var result;

		if (window.File && window.FileReader && window.FileList && window.Blob) {
			result = React.DOM.input({
				'type': 'file',
				'id': 'files',
				'name': 'files',
				'onChange': this.handleFileSelect,
				'multiple': false
			})
		} else {
		  result = React.DOM.p({}, "Browser doesn't support the FileReader API");
		}

		return result;
	},

	renderQueue: function() {
		return MochiKit.Base.map(MochiKit.Base.bind(function(anElement) {
			return React.DOM.li({},[
				React.DOM.ul({}, [
					React.DOM.li({}, anElement['meta']['name']),
					React.DOM.li({}, anElement['meta']['size']),
					React.DOM.li({}, anElement['status']),
					React.DOM.li({}, Math.floor((anElement['currentByte']/anElement['meta']['size'])*100)+'%'),
					React.DOM.li({}, React.DOM.button({
						'disabled': ! anElement.isDataReady,
						'onClick': MochiKit.Base.method(this, 'downloadFile', anElement['reference']),
					}, "Download File")),
				])
				
			]);
		}, this), this.props['attachmentQueueInfo']);
	},

	render: function () {
		return	React.DOM.div({
			'style': {
				'position': 'fixed',
				'width': '80%',
				'height': '80%',
				'background': 'white',
				'color': 'black',
				'top': '10%',
				'left': '10%',
				'border': '2px solid black',
				'zIndex': '10'
			}
		}, [
			React.DOM.p({}, "This is the doc encryption proof of concept."),
			React.DOM.p({}, "*Status*: working streaming encryption and decryption! Current size limit is 250MB"),
			this.renderUploadButton(),
			React.DOM.ul({},this.renderQueue()),
		]);
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.DocumentEncryptionSandbox = React.createFactory(Clipperz.PM.UI.Components.DocumentEncryptionSandboxClass);
