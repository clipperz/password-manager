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
Clipperz.Base.module('Clipperz.PM.UI');

Clipperz.PM.UI.AttachmentController = function(someParameters) {
		this.MAX_SIMULTANEOUS_READ		= 1;
		this.MAX_SIMULTANEOUS_UPLOAD	= 1;
		this.MAX_SIMULTANEOUS_DOWNLOAD	= 1;
		this.MAX_SIMULTANEOUS_ENCRYPT	= 1;
		this.MAX_SIMULTANEOUS_DECRYPT	= 1;

		this.LATEST_ENCRYPTION_VERSION = '1.0';	// Versions aren't handled completely yet!

		this.fileQueue       = [];
//		this.notifications   = [];
		this.operationsCount = null;

		this.encryptedDocument = null;

		this.uploadMessageCallback      = someParameters['uploadMessageCallback'];
		this.downloadMessageCallback    = someParameters['downloadMessageCallback'];
		this.reloadServerStatusCallback = someParameters['reloadServerStatusCallback'];

		// this.cryptoObject = window.crypto || window.msCrypto; // Not needed anymore because of polyfill
		this.cryptoObject = window.crypto

		return this;
	}

MochiKit.Base.update(Clipperz.PM.UI.AttachmentController.prototype, {

	toString: function () {
		return "Clipperz.PM.UI.AttachmentController";
	},

	//-------------------------------------------------------------------------
/*
	notifyUpdate: function() {
//		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'updateAttachmentQueueInfo', this.getQueueInfo(), this.getNotificationsInfo());
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'updateNotifications');
	},
*/
	getQueueInfo: function() {
		return this.fileQueue;
	},
/*
	getNotificationsInfo: function() {
		return this.notifications;
	},
*/
	//=========================================================================
	// Entry points
	//=========================================================================

	addAttachment: function (anAttachment) {
		var	deferredResult;
		var	actualDeferredResult;

		actualDeferredResult = new Clipperz.Async.Deferred("Clipperz.PM.UI.AttachmentController.uploadAttachment-result", {trace:false});
		actualDeferredResult.addMethod(anAttachment, 'setValue', 'hash');

		deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.UI.AttachmentController.uploadAttachment", {trace:false});
		deferredResult.collectResults({
			'_attachment': MochiKit.Base.partial(MochiKit.Async.succeed, anAttachment),
			'_record': MochiKit.Base.method(anAttachment, 'record'),
			'reference': MochiKit.Base.method(anAttachment, 'reference'),
			'meta': MochiKit.Base.method(anAttachment, 'metadata'),
			'key': MochiKit.Base.method(anAttachment, 'key'),
			'nonce': MochiKit.Base.method(anAttachment, 'nonce'),
			'status': MochiKit.Base.partial(MochiKit.Async.succeed, 'WAITING_READ'),
			'file': MochiKit.Base.method(anAttachment, 'file'),
			'recordReference': MochiKit.Base.method(anAttachment.record(), 'reference'),
			'process': MochiKit.Base.partial(MochiKit.Async.succeed, 'UPLOAD'),	// Used only to differentiate notifications
		}, {trace: false});
		deferredResult.addCallback(function (someInfo) { someInfo['deferredResult'] = actualDeferredResult; return someInfo; });
		deferredResult.addMethod(this, 'addFileToQueue');
		deferredResult.callback();

//		return deferredResult;
		return actualDeferredResult;
	},

	getAttachment: function(anAttachment, aMessageCallback) {
		if (this.getQueuePosition(anAttachment.reference()) >= 0) {
			this.removeFileFromQueue(anAttachment.reference());
		}
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.UI.AttachmentController.downloadAttachment", {trace:false});
		deferredResult.collectResults({
			'_attachment': MochiKit.Base.partial(MochiKit.Async.succeed, anAttachment),
			'_record': MochiKit.Base.method(anAttachment, 'record'),
			'reference': MochiKit.Base.method(anAttachment, 'reference'),
			'meta': MochiKit.Base.method(anAttachment, 'metadata'),
			'key': MochiKit.Base.method(anAttachment, 'key'),
			'nonce': MochiKit.Base.method(anAttachment, 'nonce'),
			'status': MochiKit.Base.partial(MochiKit.Async.succeed, 'WAITING_DOWNLOAD'),
			'messageCallback': MochiKit.Async.succeed(aMessageCallback),
			'process': MochiKit.Base.partial(MochiKit.Async.succeed, 'DOWNLOAD'),	// Used only to differentiate notifications
		}, {trace: false});
		deferredResult.addCallback(function(aResult){
			MochiKit.Base.update(aResult, {'messageCallback': aMessageCallback});
			return aResult;
		});
		deferredResult.addMethod(this, 'addFileToQueue');
		deferredResult.callback();

		return deferredResult;
	},

	cancelAttachment: function(anAttachment) {
		var deferredResult;

		var reference = anAttachment.reference()
		var queueElement = this.getQueueElement(reference);

		var isElementInProgress = (queueElement && queueElement['status'] != 'DONE' && queueElement['status'] != 'CANCELED' && queueElement['status'] != 'FAILED');

		if (isElementInProgress) {
			deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.UI.AttachmentController.cancelAttachment", {trace:false});
			deferredResult.addMethod(this, 'updateFileInQueue', reference, {'status': 'CANCELED'});
			if (queueElement['deferredRequest']) {
				deferredResult.addMethod(queueElement['deferredRequest'], 'cancel');
			}
			

			deferredResult.callback();
			// TODO: We may also want do delete stuff in the queue element
		} else {
			deferredResult = MochiKit.Async.succeed();
		}

		return deferredResult;
	},

	//=========================================================================
	// Queue management
	//=========================================================================

	dispatchQueueOperations: function() {
		var currentElement;
		var processNextElements;

		var count = this.updateOperationsCount();

//		this.notifyUpdate();

		processNextElements = true;
		for (i in this.fileQueue) {
			if (processNextElements) {
				currentElement = this.fileQueue[i];
				switch (currentElement['status']) {
					case 'WAITING_READ':
						if ((count['READING']) < this.MAX_SIMULTANEOUS_READ) {
							this.readFile(currentElement['reference'], currentElement['file']);
							processNextElements = false;
						}
						break;

					case 'WAITING_ENCRYPT':
						if (count['ENCRYPTING'] < this.MAX_SIMULTANEOUS_ENCRYPT) {
							this.encryptFile(currentElement['reference'], currentElement['originalArray'], currentElement['key'], currentElement['nonce']);
							processNextElements = false;
						}
						break;

					case 'WAITING_UPLOAD':
						if (count['UPLOADING'] < this.MAX_SIMULTANEOUS_UPLOAD) {
							this.uploadFile(currentElement['reference'], currentElement['encryptedArray']);
							processNextElements = false;
						}
						break;

					case 'WAITING_DOWNLOAD':
						if (count['DOWNLOADING'] < this.MAX_SIMULTANEOUS_DOWNLOAD) {
							this.downloadFile(currentElement['reference'], currentElement['messageCallback']);
							processNextElements = false;
						}
						break;

					case 'WAITING_DECRYPT':
						if (count['DECRYPTING'] < this.MAX_SIMULTANEOUS_DECRYPT) {
							this.decryptFile(currentElement['reference'], currentElement['encryptedArray'], currentElement['key'], currentElement['nonce']);
							processNextElements = false;
						}
						break;

					case 'WAITING_SAVE':
						this.saveFile(currentElement['reference'], currentElement['decryptedArray'], currentElement['meta']['name'], currentElement['meta']['type']);
						processNextElements = false;
//						Clipperz.Sound.beep();
						break;
				}
			}
		}
	},

	updateOperationsCount: function() {
		var count;

		count = {
			'WAITING_READ':		0,
			'READING':			0,
			'WAITING_ENCRYPT':	0,
			'ENCRYPTING':		0,
			'WAITING_UPLOAD':	0,
			'UPLOADING':		0,
			'WAITING_DOWNLOAD':	0,
			'DOWNLOADING':		0,
			'WAITING_DECRYPT':	0,
			'DECRYPTING':		0,
			'WAITING_SAVE':		0,
			'DONE':				0,
			'CANCELED':			0,
			'FAILED':			0,
		};

		for (var i in this.fileQueue) {
			count[this.fileQueue[i]['status']]++;
		}

		this.operationsCount = count;

		return this.operationsCount;
	},

	addFileToQueue: function(someParameters) {
		this.fileQueue.push(someParameters);

		this.dispatchQueueOperations();
	},

	removeFileFromQueue: function(aFileReference) {
		this.fileQueue.splice(this.getQueuePosition(aFileReference), 1);

		this.dispatchQueueOperations();
	},

	getQueueElement: function(aFileReference) {
		var i = this.getQueuePosition(aFileReference);
		return this.fileQueue[i];
	},

	updateFileInQueue: function(aFileReference, someParameters) {
		var queuePosition = this.getQueuePosition(aFileReference);

		MochiKit.Base.update(this.fileQueue[queuePosition], someParameters);

		if ((someParameters['status'] == 'DONE') || (someParameters['status'] == 'FAILED')) {
			this.addNotification(this.fileQueue[queuePosition], someParameters['status']);
		}

		this.dispatchQueueOperations();
	},

	appendResult: function(aFileReference, anArray) {
		var queueElement = this.getQueueElement(aFileReference);
		queueElement['result'].set(anArray, queueElement['currentByte']);
	},

	getQueuePosition: function(aFileReference) {
		var result;

		result = -1;
		for (var i in this.fileQueue) {
			if (this.fileQueue[i].reference == aFileReference) {
				result = i;
			}
		}

		return result;
	},

	//=========================================================================
	// Notifications
	//=========================================================================

	addNotification: function(aQueueElement, aStatus) {
//console.log("ADD ATTACHMENT NOTIFICATION", aQueueElement);
		var messagePrefix = "";
		var	message;
		var	filename;
		var	level;
		
		if (aStatus == 'DONE') {
			level = 'info';
			if (aQueueElement['process'] == 'DOWNLOAD') {
				messagePrefix = "downloaded attachment ";
			} else if (aQueueElement['process'] == 'UPLOAD') {
				messagePrefix = "uploaded attachment ";
			}
		} else if (aStatus == 'FAILED') {
			level = 'error';
			if (aQueueElement['process'] == 'DOWNLOAD') {
				messagePrefix = "error downloading attachment ";
			} else if (aQueueElement['process'] == 'UPLOAD') {
				messagePrefix = "error uploading attachment ";
			}
		} else {
			level = 'warning';
			messagePrefix = "???";
		}

		filename = aQueueElement['meta']['name'];
		message = messagePrefix + filename;

		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'addNotification', {'message':message, 'level':level});
	},
/*
	removeNotification: function(aNotificationId) {
		var i, position;

		position = -1;
		for (i in this.notifications) {
			if (this.notifications[i]['id'] == aNotificationId) {
				position = i;
			}
		}

		if (position >= 0) {
			this.notifications.splice(position, 1);
		}

//		this.notifyUpdate();
	},
*/
/*
	randomId: function() {
		return Clipperz.Crypto.randomKey();
//		return Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(32).toHexString().substring(2);
	},
*/
	//=========================================================================
	// Queue Processing: READ
	//=========================================================================

	readFile: function(aFileReference, aFile) {
		var reader = new FileReader();

		this.updateFileInQueue(aFileReference, {
			'status': 'READING',
		});

		reader.onload = MochiKit.Base.method(this, 'readFileOnload', aFileReference);
		reader.readAsArrayBuffer(aFile);
	},

	readFileOnload: function(aFileReference, anEvent) {
		var	fileContent;
		var	fileContentSha256Hash;
		
		fileContent = new Uint8Array(anEvent.target.result);
		fileContentSha256Hash = npm.bitcoin.crypto.sha256(fileContent).toString('hex');
		this.getQueueElement(aFileReference)['deferredResult'].callback(fileContentSha256Hash);

		this.updateFileInQueue(aFileReference, {
			'status': 'WAITING_ENCRYPT',
			'originalArray': fileContent,
		})
	},

	//=========================================================================
	// Queue Processing: ENCRYPT
	//=========================================================================

	encryptFile: function(aFileReference, anArrayBuffer, aKey, aNonce) {
		this.updateFileInQueue(aFileReference, {
			'status': 'ENCRYPTING',
		});

		this.cryptoObject.subtle.importKey(
			"raw",
			aKey,								//this is an example jwk key, "raw" would be an ArrayBuffer
			{ name: "AES-CBC" },				//this is the algorithm options
			false,								//whether the key is extractable (i.e. can be used in exportKey)
			["encrypt"]							//can be "encrypt", "decrypt", "wrapKey", or "unwrapKey"
		)
		// .then(MochiKit.Base.method(this, 'doEncrypt', aFileReference, anArrayBuffer, aNonce))
		.then(this.doEncrypt.bind(this,aFileReference, anArrayBuffer, aNonce))
		.catch(MochiKit.Base.method(this, 'handleException', aFileReference, 'encryptFile(): encryption failed'));
	},

	doEncrypt: function(aFileReference, anArrayBuffer, anIV, aWebcryptoKey) {
		this.cryptoObject.subtle.encrypt(
			{
				name: "AES-CBC",
				iv: anIV,
			},
			aWebcryptoKey,
			anArrayBuffer
		)
		.then(MochiKit.Base.method(this, 'doneEncrypt', aFileReference))
		.catch(MochiKit.Base.method(this, 'handleException', aFileReference, 'doEncrypt(): encryption failed'));
	},

	doneEncrypt: function(aFileReference, anArrayBuffer) {
		this.updateFileInQueue(aFileReference, {
			'status': 'WAITING_UPLOAD',
			'encryptedArray': new Uint8Array(anArrayBuffer),
		});
	},


	//=========================================================================
	// Queue Processing: UPLOAD
	//=========================================================================

	uploadFile: function(aFileReference, anEncryptedArray) {
		this.updateFileInQueue(aFileReference, {
			'status': 'UPLOADING',
			'deferredRequest': this.uploadFileRequest(aFileReference, anEncryptedArray),
			'requestProgress': 0,
		});
	},

	uploadFileRequest: function(aFileReference, anEncryptedArray) {
		var deferredResult;
		var queueElement = this.getQueueElement(aFileReference);

		deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.UI.AttachmentController.uploadFileRequest", {trace:false});
		deferredResult.addCallback(this.uploadMessageCallback, {
				'attachmentReference': queueElement['_attachment'].reference(),
				'recordReference': queueElement['_attachment'].record().reference(),
				'arrayBufferData': anEncryptedArray,
				'version': this.LATEST_ENCRYPTION_VERSION,
			}, MochiKit.Base.method(this, 'uploadFileProgress', aFileReference));
		deferredResult.addMethod(this, 'uploadFileDone', aFileReference);
		deferredResult.addErrback(MochiKit.Base.method(this, 'handleException', aFileReference, 'uploadFileRequest(): request failed or canceled'));
		deferredResult.callback();

		return deferredResult;
	},

	uploadFileDone: function(aFileReference, aResult){
		var record = this.getQueueElement(aFileReference)['_record'];

		return Clipperz.Async.callbacks("AttachmentController.uploadFileDone", [
			MochiKit.Base.partial(this.reloadServerStatusCallback, record),
			MochiKit.Base.method(this, 'updateFileInQueue', aFileReference, {
				'status': 'DONE',
				'requestProgress': 1,
			}),
			//	add notification
		], {trace:false});
	},

	uploadFileProgress: function(aFileReference, anEvent) {
		var newProgress = (anEvent.lengthComputable) ? (anEvent.loaded / anEvent.total) : -1;
		this.updateFileInQueue(aFileReference, {
			'requestProgress': newProgress,
		});
	},


	//=========================================================================
	// Queue Processing: DOWNLOAD
	//=========================================================================

	downloadFile: function(aFileReference) {
		var deferredRequest;
		var queueElement = this.getQueueElement(aFileReference);

		deferredRequest = new Clipperz.Async.Deferred("Clipperz.PM.UI.AttachmentController.downloadFile", {trace:false});
		deferredRequest.addCallback(this.downloadMessageCallback, queueElement['_attachment'], MochiKit.Base.method(this, 'downloadFileProgress', aFileReference));
		deferredRequest.addMethod(this, 'downloadFileDone', aFileReference);
		deferredRequest.addErrback(MochiKit.Base.method(this, 'handleException', aFileReference, 'downloadFile(): download filed or canceled'));
		deferredRequest.callback();

		this.updateFileInQueue(aFileReference, {
			'status': 'DOWNLOADING',
			'deferredRequest': deferredRequest,
			'requestProgress': 0,
		});
	},

	downloadFileDone: function(aFileReference, aResult){
		var queueElement = this.getQueueElement(aFileReference);
		var encryptedArray = new Uint8Array(aResult);

		this.updateFileInQueue(aFileReference, {
			'status': 'WAITING_DECRYPT',
			'key': queueElement['key'],
			'nonce': queueElement['nonce'],
			'encryptedArray': encryptedArray,
			'requestProgress': 1,
		});

	},

	downloadFileProgress: function(aFileReference, anEvent) {
		var newProgress = (anEvent.lengthComputable) ? (anEvent.loaded / anEvent.total) : -1;

		this.updateFileInQueue(aFileReference, {
			'requestProgress': newProgress,
		});
	},

	
	//=========================================================================
	// Queue Processing: DECRYPT
	//=========================================================================

	decryptFile: function(aFileReference, anArrayBuffer, aKey, aNonce) {
		this.updateFileInQueue(aFileReference, {
			'status': 'DECRYPTING',
		});
	
		this.cryptoObject.subtle.importKey(
		    "raw",
		    aKey,								//this is an example jwk key, "raw" would be an ArrayBuffer
		    {name: "AES-CBC"},					//this is the algorithm options
		    false,								//whether the key is extractable (i.e. can be used in exportKey)
		    ["decrypt"]							//can be "encrypt", "decrypt", "wrapKey", or "unwrapKey"
		)
		.then(MochiKit.Base.method(this, 'doDecrypt', aFileReference, anArrayBuffer, aNonce))
		.catch(MochiKit.Base.method(this, 'handleException', aFileReference, 'decryptFile(): decryption failed'));
	},

	doDecrypt: function(aFileReference, anArrayBuffer, anIV, aWebcryptoKey) {
		this.cryptoObject.subtle.decrypt(
		    {name: "AES-CBC", iv: anIV},
		    aWebcryptoKey,
		    anArrayBuffer
		)
		.then(MochiKit.Base.method(this, 'doneDecrypt', aFileReference))
		.catch(MochiKit.Base.method(this, 'handleException', aFileReference, 'doDecrypt(): decryption failed'));
	},

	doneDecrypt: function(aFileReference, anArrayBuffer) {
		this.updateFileInQueue(aFileReference, {
			'status': 'WAITING_SAVE',
			'decryptedArray': new Uint8Array(anArrayBuffer),
		});
	},

	
	//=========================================================================
	// Queue Processing: SAVE
	//=========================================================================

	saveFile: function(aFileReference, anArray, aFileName, aFileType) {
		var blob = new Blob([anArray], {type: aFileType});
		saveAs(blob, aFileName);

		this.updateFileInQueue(aFileReference, {
			'status': 'DONE',
		});
	},

	
	//=========================================================================
	// Exceptions
	//=========================================================================

	/** Handles exceptions for upload/download and encrypt/decrypt. Note that
	 *  an exception is thrown also when the user manually cancels the file
	 *  processing. In this case the status remains 'CANCELED'.
	 */
	handleException: function(aFileReference, aMessage, anException) {
		var queueElement = this.getQueueElement(aFileReference);
		var messageString = aMessage ? " (" + aMessage + ")" : "";

		try {
			if (Clipperz.Base.evalJSON(anException['req']['response'])['message'] == "not enough space available for uploading the attachment") {
				console.log("NOT ENOUGH ATTACHMENT QUOTA EXCEPTION");
			}
		} catch (exception) {
		}

		if (queueElement['status'] != 'CANCELED') {
			this.updateFileInQueue(aFileReference, {
				'status': 'FAILED',
			});
		}

		if (aMessage) {
			console.log("AttachmentController: caught exception" + messageString + ":", anException);
		}
	},

	//=========================================================================

	__syntaxFix__: "syntax fix"
});