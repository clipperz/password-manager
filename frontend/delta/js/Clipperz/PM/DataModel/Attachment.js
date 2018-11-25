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

Clipperz.Base.module('Clipperz.PM.DataModel');

Clipperz.PM.DataModel.Attachment = function(args) {
	args = args || {};

	Clipperz.PM.DataModel.Attachment.superclass.constructor.apply(this, arguments);

	this._reference 				=	args.reference
										||	Clipperz.PM.Crypto.randomKey();
	this._record					=	args.record
										||	Clipperz.Base.exception.raise('MandatoryParameter');

//	this._retrieveIndexDataFunction	= 	args.retrieveIndexDataFunction
//										||	this.record().retrieveAttachmentIndexDataFunction()
//										||	Clipperz.Base.exception.raise('MandatoryParameter');
//	this._setIndexDataFunction		= 	args.setIndexDataFunction
//										||	this.record().setAttachmentIndexDataFunction()
//										||	Clipperz.Base.exception.raise('MandatoryParameter');
//	this._removeIndexDataFunction	=	args.removeIndexDataFunction
//										||	this.record().removeAttachmentIndexDataFunction()
//										||	Clipperz.Base.exception.raise('MandatoryParameter');

	// this.setFile(args.file);
	
	this._transientState = null;

	this._isBrandNew = MochiKit.Base.isUndefinedOrNull(args.reference);
	
	this.record().bindAttachment(this);

	if (this._isBrandNew) {
		this.setKey(Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(256/8));
		this.setNonce(Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(128/8));
	}

	return this;
}

Clipperz.Base.extend(Clipperz.PM.DataModel.Attachment, Object, {

	'toString': function() {
		return "Attachment (" + this.reference() + ")";
	},

	//=========================================================================

	'reference': function () {
		return this._reference;
	},

	//-------------------------------------------------------------------------

	'record': function () {
		return this._record;
	},

	//=========================================================================

	'isBrandNew': function () {
		return this._isBrandNew;
	},

	//=========================================================================

	'removeIndexDataFunction': function () {
		return this._removeIndexDataFunction;
	},

	'remove': function () {
		return Clipperz.Async.callbacks("DirectLogin.remove", [
			MochiKit.Base.partial(this.removeIndexDataFunction(), this.reference()),
			MochiKit.Base.method(this.record(), 'removeAttachment', this)
		], {trace:false});
	},

	//=========================================================================

	'file': function () {
//		return this.getValue('name');
		return MochiKit.Async.succeed(this._file);
	},
	
	'setFile': function (aFile) {
		var	hashValue;
//console.log("ATTACHMENT SET FILE", aFile);
		this._file			= aFile			|| null;
//		hashValue = aFile ? npm.bitcoin.address.toBase58Check(npm.bitcoin.crypto.hash160(aFile), NETWORK.pubKeyHash) : null;

		/* These ones will disappear when the application is closed */
		this._name			= aFile ? aFile['name'] : null;
		this._contentType	= aFile ? aFile['type'] : null;
		this._size			= aFile ? aFile['size'] : null;
//		this._hash			= hashValue;
		
		/* These ones will be saved in the Record */
		return Clipperz.Async.callbacks("Attachment.setFile", [
			MochiKit.Base.method(this, 'setValue', 'name', aFile['name']),
			MochiKit.Base.method(this, 'setValue', 'contentType', aFile['type']),
			MochiKit.Base.method(this, 'setValue', 'size', aFile['size']),
//			MochiKit.Base.method(this, 'setValue', 'hash', hashValue),
			
			MochiKit.Base.partial(MochiKit.Async.succeed, this),
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'name': function () {
		return this.getValue('name');
	},
	
	'contentType': function () {
		return this.getValue('contentType');
	},

	'size': function () {
		return this.getValue('size');
	},
	
	'hash': function () {
		return this.getValue('hash');
	},
	
	'metadata': function () {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("Attachment.metadata [collect results]", {trace:false});
		deferredResult.collectResults({
			'name':	MochiKit.Base.method(this, 'name'),
			'type': MochiKit.Base.method(this, 'contentType'),
			'size': MochiKit.Base.method(this, 'size'),
			'hash': MochiKit.Base.method(this, 'hash'),
		}, {trace:false});
		deferredResult.callback();

		return deferredResult;

//		return {
//			'name': this._name,
//			'type': this._type,
//			'size': this._size,
//		}
	},

	//-------------------------------------------------------------------------

	'key': function () {
		var byteArray;

		byteArray = new Clipperz.ByteArray();

		return Clipperz.Async.callbacks("Attachment.key", [
			MochiKit.Base.method(this, 'getValue', 'key'),
			MochiKit.Base.method(byteArray, 'appendBase64String'),
			function(aByteArray) { return new Uint8Array(aByteArray.arrayValues()); }
		], {trace:false});
	},
	
	// 'key': function () {
	// 	var result;

	// 	result = new Clipperz.ByteArray();

	// 	return Clipperz.Async.callbacks("Attachment.key", [
	// 		MochiKit.Base.method(this, 'getValue', 'key'),
	// 		MochiKit.Base.method(result, 'appendBase64String'),
	// 		function(aByteArray) { return new Clipperz.Crypto.AES.Key({key: aByteArray}); }
	// 	], {trace:false});
	// },
	
	'nonce': function () {
		var byteArray;

		byteArray = new Clipperz.ByteArray();

		return Clipperz.Async.callbacks("Attachment.nonce", [
			MochiKit.Base.method(this, 'getValue', 'nonce'),
			MochiKit.Base.method(byteArray, 'appendBase64String'),
			function(aByteArray) { return new Uint8Array(aByteArray.arrayValues()); }
		], {trace:false});
	},
	
	// 'nonce': function () {
	// 	var result;

	// 	result = new Clipperz.ByteArray();

	// 	return Clipperz.Async.callbacks("Attachment.nonce", [
	// 		MochiKit.Base.method(this, 'getValue', 'nonce'),
	// 		MochiKit.Base.method(result, 'appendBase64String')
	// 	], {trace:false});
	// },

	'setKey': function (aByteArray) {
		this.setValue('key', aByteArray.toBase64String());
	},
	
	// 'setKey': function (aKey) {
	// 	var byteArray = aKey.key();
	// 	var serializedData = byteArray.toBase64String();

	// 	this.setValue('key', serializedData);
	// },
	
	'setNonce': function (aByteArray) {
		this.setValue('nonce', aByteArray.toBase64String());
	},
	
	//=========================================================================

	'serializedData': function () {
		return Clipperz.Async.collectResults("Attachment.serializedData", {
			'name':			MochiKit.Base.method(this, 'name'),
			'contentType':	MochiKit.Base.method(this, 'contentType'),
			'size':			MochiKit.Base.method(this, 'size'),
		}, {trace:false})()
	},

	//=========================================================================

	'hasPendingChanges': function () {
		var	result;
//		var deferredResult;
		
		result = false;
		result = result || this.isBrandNew();

		return MochiKit.Async.succeed(result);
	},

	//-------------------------------------------------------------------------

	'revertChanges': function () {
		return MochiKit.Async.succeed();
	},


	//=========================================================================

	'transientState': function () {
		if (this._transientState == null) {
			this._transientState = {}
		}
		
		return this._transientState;
	},

	'resetTransientState': function (isCommitting) {
		this._transientState = null;
	},

	'commitTransientState': function (isCommitting) {
		this._transientState = null;
		this._isBrandNew = false;
	},

	//=========================================================================

	'actualKey': function (aValueKey) {
		var actualKey;

		actualKey = 'attachments' + '.' + this.reference();
		if (aValueKey != '') {
			actualKey = actualKey + '.' + aValueKey;
		}

		return actualKey;
	},

	//-------------------------------------------------------------------------

	'getValue': function (aValueKey) {
		return this.record().getValue(this.actualKey(aValueKey));
	},
	
	'setValue': function (aValueKey, aValue) {
		return Clipperz.Async.callbacks("Attachment.setValue", [
			// MochiKit.Base.method(this, 'getValue', ''),
			// MochiKit.Base.bind(function (aValue) {
			// 	if (this.originalConfiguration() == null) {
			// 		this.setOriginalConfiguration(aValue);
			// 	}
			// }, this),
			MochiKit.Base.method(this.record(), 'setValue', this.actualKey(aValueKey), aValue)
		], {trace:false});
	},
	
	'removeValue': function (aValueKey) {
		return this.record().removeValue(this.actualKey(aValueKey));
	},

	//=========================================================================

	'content': function () {
//		return this.serializedData();
//		return MochiKit.Async.succeed(this);

		var deferredResult;
		var	fieldValues;

		fieldValues = {};
		deferredResult = new Clipperz.Async.Deferred("Attachment.content", {trace:false});
		deferredResult.addMethod(this, 'reference');
		deferredResult.addCallback(function (aValue) { fieldValues['reference'] = aValue; });
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'deleteAllCleanTextData': function () {
		this._name			= null;
		this._contentType	= null;
		this._size			= null;

		this.resetTransientState();
	},

	//-------------------------------------------------------------------------
	
	'hasAnyCleanTextData': function () {
		var result;
		
		result = false;

		result = result || (this._name != null);
		result = result || (this._contentType != null);
		result = result || (this._size != null);
		result = result || (MochiKit.Base.keys(this.transientState()).length != 0);

		return MochiKit.Async.succeed(result);
	},

	//=========================================================================
/*
	computeAttachmentCertificateInfo: function () {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("Attachment.computeAttachmentCertificateInfo", {trace:false});
		deferredResult.collectResults({
			'name':			MochiKit.Base.method(this, 'name'),
			'contentType':	MochiKit.Base.method(this, 'contentType'),
			'size':			MochiKit.Base.method(this, 'size'),
			'hash': [
				MochiKit.Base.method(attachmentController, 'getAttachment', this),
function (aValue) { console.log("STEP 1", aValue); return aValue; },
//				npm.bitcoin.crypto.hash160,
//function (aValue) { console.log("STEP 2", aValue); return aValue; },
//				function (anHash160) { return npm.bitcoin.address.toBase58Check(anHash160, NETWORK.pubKeyHash);}
				function () { return MochiKit.Async.succeed("bingo"); }
			]
		});
		deferredResult.callback();

		return deferredResult;
	},
*/
	//=========================================================================
	__syntaxFix__: "syntax fix"
});

Clipperz.PM.DataModel.Attachment.MAX_ATTACHMENT_SIZE = 50*1024*1024;

Clipperz.PM.DataModel.Attachment.contentTypeIcon = function (aContentType) {
	var	result;
	
	result = 'other file';

	if (aContentType == null) {
		result = 'other file';
	} else if (aContentType == "application/pdf") {
		result = 'pdf file';
	} else if (aContentType.indexOf('image/') == 0) {
		result = 'image file';
	} else if (aContentType.indexOf('model/') == 0) {
		result = 'other file';
	} else if (aContentType.indexOf('audio/') == 0) {
		result = 'audio file';
	} else if (aContentType.indexOf('text/') == 0) {
		result = 'text file';
	} else if (aContentType.indexOf('video/') == 0) {
		result = 'video file';
	}
	
	return result;
};
