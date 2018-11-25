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

try { if (typeof(Clipperz.PM.DataModel.User) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.PM.DataModel.User.Header.Wallet depends on Clipperz.PM.DataModel.User!";
}  

if (typeof(Clipperz.PM.DataModel.User.Header) == 'undefined') { Clipperz.PM.DataModel.User.Header = {}; }

Clipperz.PM.DataModel.User.Header.Wallet = function(args) {
	Clipperz.PM.DataModel.User.Header.Wallet.superclass.constructor.apply(this, arguments);

	return this;
}

Clipperz.Base.extend(Clipperz.PM.DataModel.User.Header.Wallet, Clipperz.PM.DataModel.EncryptedRemoteObject, {

	'toString': function() {
		return "Clipperz.PM.DataModel.User.Header.Wallet";
	},

	//-------------------------------------------------------------------------

	getRandomSeed: function () {
		return Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(512/8).toHexString().substring(2);
	},

	getWalletSeed: function () {
		return this.getOrSetValue('walletSeed', MochiKit.Base.method(this, 'getRandomSeed'));
	},
	
	userMasterKey: function () {
		return Clipperz.Async.callbacks("User.Header.Wallet.userMasterKey", [
			MochiKit.Base.method(this, 'getWalletSeed'),
			function (aValue) {
				return npm.bitcoin.HDNode.fromSeedHex(aValue, NETWORK);
			}
		], {trace:false});
	},

	getNextID: function (aKeyPath) {
//console.log("WALLET - getNextID", aKeyPath);
		return Clipperz.Async.callbacks("User.Header.Wallet.userMasterKey", [
			MochiKit.Base.method(this, 'getValue', aKeyPath),
			function (aValue) {
				var	result;
				
				if (aValue == null) {
					result = 0;
				} else {
					result = aValue + 1;
				}
				
				return result;
			},
			MochiKit.Base.method(this, 'setValue', aKeyPath)
		], {trace:false});
	},

	getKeyForDocumentWithID: function (anID) {
		return Clipperz.Async.callbacks("User.Header.Wallet.getKeyForDocumentWithID", [
			MochiKit.Base.method(this, 'userMasterKey'),
			MochiKit.Base.methodcaller('derive', 0),
			MochiKit.Base.methodcaller('derive', anID)
		], {trace:false});
	},

//	getPublicKeyForDocumentWithID: function (anID) {
//		return Clipperz.Async.callbacks("User.Header.Wallet.getPublicKeyForDocumentWithID", [
//			MochiKit.Base.method(this, 'getKeyForDocumentWithID', anID),
//			MochiKit.Base.methodcaller('getPublicKeyBuffer'),
//			MochiKit.Base.methodcaller('toString', 'hex'),
//		], {trace:false});
//	},

//	getAddressForDocumentWithID: function (anID) {
//console.log("WALLET - address for document with ID", anID);
//		return Clipperz.Async.callbacks("User.Header.Wallet.getAddressForDocumentWithID", [
//			MochiKit.Base.method(this, 'getKeyForDocumentWithID', anID),
//			MochiKit.Base.methodcaller('getAddress')
//		], {trace:false});
//	},

	//#########################################################################

//	prepareRemoteDataWithKey
//		packData
//			encryptDataWithKey
//				packRemoteData [encryptedDataKeypath (?), encryptedVersionKeypath (?)]
/*
	prepareRemoteDataWithKey: function (aKey) {
console.log("USER HEADER WALLET - prepareRemoteDataWithKey", aKey);
//console.log("super", Clipperz.PM.DataModel.User.Header.Wallet.superclass.prepareRemoteDataWithKey);
		return Clipperz.PM.DataModel.User.Header.Wallet.superclass.prepareRemoteDataWithKey.apply(this, arguments);
	},

	packData: function (someData) {	//	++
console.log("USER HEADER WALLET - packData", someData);
		return Clipperz.PM.DataModel.User.Header.Wallet.superclass.packData.apply(this, arguments);
	},

	packRemoteData: function (someData) {
console.log("USER HEADER WALLET - packRemoteData", someData);
		return Clipperz.PM.DataModel.User.Header.Wallet.superclass.packRemoteData.apply(this, arguments);
	},
*/
	//#########################################################################
/*
	'mergePreferences': function(somePreferences, someOtherPreferences) {
		var result;

		result = new Clipperz.KeyValueObjectStore();

		result.setValues(MochiKit.Base.updatetree(
			Clipperz.Base.deepClone(someOtherPreferences),
			somePreferences
		));

		return result;
	},
	
	'mergeDefaultPreferences': function(somePreferences) {
		return this.mergePreferences(somePreferences, Clipperz.PM.DataModel.User.Header.Preferences.defaultPreferences);
	},

	'mergeUserPreferences': function(somePreferences) {
		return this.mergePreferences(somePreferences, this._objectDataStore.values());
	},

	'getPreferences': function() {
		return Clipperz.Async.callbacks("User.Header.Preferences.getPreferences", [
			MochiKit.Base.method(this, 'values'),
			MochiKit.Base.method(this, 'mergeDefaultPreferences')
		], {trace:false});
	},

	'getPreference': function(aKey) {
		return Clipperz.Async.callbacks("User.Header.Preferences.getPreference", [
			MochiKit.Base.method(this, 'getPreferences'),
			MochiKit.Base.methodcaller('getValue', aKey)
		], {trace:false});
	},

	'setPreferences': function(anObject) {
		return Clipperz.Async.callbacks("User.Header.Preferences.setPreferences", [
			MochiKit.Base.method(this, 'mergeUserPreferences', anObject),
			MochiKit.Base.methodcaller('values'),
			MochiKit.Base.method(this, 'setValues')
		], {trace:false});
	},
*/
	//=========================================================================
	__syntaxFix__: "syntax fix"
});
