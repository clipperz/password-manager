/*

Copyright 2008-2015 Clipperz Srl

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

//function logElapsedTime(aDescription, aStartTime, anEndTime) {
//	MochiKit.Logging.logDebug(aDescription + " - took " + (anEndTime - aStartTime) + "ms");
//	SimpleTest.ok(true, aDescription + " - took " + (anEndTime - aStartTime) + "ms");
//}

var asciiTestString		= longAsciiText;
//asciiTestString = asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString;
//asciiTestString = asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString;
//asciiTestString = asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString;
//asciiTestString = asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString + asciiTestString;
//asciiTestString = asciiTestString + asciiTestString;
//asciiTestString = asciiTestString + asciiTestString;

var isoLatin1TestString	= longIsoLatin1Text;
//isoLatin1TestString = isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString;
//isoLatin1TestString = isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString;
//isoLatin1TestString = isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString + isoLatin1TestString;
//isoLatin1TestString = isoLatin1TestString + isoLatin1TestString;
//isoLatin1TestString = isoLatin1TestString + isoLatin1TestString;

var utf8TestString		= longUtf8Text;
//utf8TestString = utf8TestString + utf8TestString + utf8TestString + utf8TestString + utf8TestString + utf8TestString + utf8TestString + utf8TestString + utf8TestString + utf8TestString;
//utf8TestString = utf8TestString + utf8TestString + utf8TestString + utf8TestString + utf8TestString + utf8TestString + utf8TestString + utf8TestString + utf8TestString + utf8TestString;
//utf8TestString = utf8TestString + utf8TestString;
//utf8TestString = utf8TestString + utf8TestString;

var times = {
	'Clipperz': {},
	'JSCrypto': {}
};

function appendResults (aDescription, aTimer) {
	MochiKit.DOM.appendChildNodes(MochiKit.DOM.getElement('timerTBODY'),
		MochiKit.DOM.TR(null, 
			MochiKit.DOM.TH({align:'left'}, aDescription),
			MochiKit.DOM.TH(null, aTimer['ascii']['encrypt'] 		+ ' - ' + aTimer['ascii']['decrypt'])//,
//			MochiKit.DOM.TH(null, aTimer['isoLatin1']['encrypt']	+ ' - ' + aTimer['isoLatin1']['decrypt']),
//			MochiKit.DOM.TH(null, aTimer['utf8']['encrypt'] 		+ ' - ' + aTimer['utf8']['decrypt'])
		)
	);
	
}

//=============================================================================

function timeRegularFunction (aDescription, aString, anEncryptFunction, aDecryptFunction, aTimer, aKey) {
	var start;
	var end;
	var encryptTime;
	var decryptTime;
	var ciphertext;
	var plaintext;

	
	start = new Date();
	ciphertext = anEncryptFunction('trustno1', aString);
	end = new Date();
	encryptTime = end - start;

	start = new Date();
	plaintext = aDecryptFunction('trustno1', ciphertext);
	end = new Date();
	decryptTime = end - start;
	aTimer[aKey] = { 'encrypt': encryptTime, 'decrypt': decryptTime };
	SimpleTest.is(aString, plaintext, aDescription);
}
/*
function timeRegularFunction (anEncryptFunction, aDecryptFunction, aTimer) {
	var start;
	var end;
	var encryptTime;
	var decryptTime;
	var ciphertext;
	var plaintext;

	
	start = new Date();
	ciphertext = anEncryptFunction('trustno1', asciiTestString);
	end = new Date();
	encryptTime = end - start;

	start = new Date();
	plaintext = aDecryptFunction('trustno1', ciphertext);
	end = new Date();
	decryptTime = end - start;
	aTimer['ascii'] = { 'encrypt': encryptTime, 'decrypt': decryptTime };
	SimpleTest.is(asciiTestString, plaintext, "Encrypt/decrypt the ASCII text");


	start = new Date();
	ciphertext = anEncryptFunction('trustno1', isoLatin1TestString);
	end = new Date();
	encryptTime = end - start;

	start = new Date();
	plaintext = aDecryptFunction('trustno1', ciphertext);
	end = new Date();
	decryptTime = end - start;
	aTimer['isoLatin1'] = { 'encrypt': encryptTime, 'decrypt': decryptTime };
	SimpleTest.is(isoLatin1TestString, plaintext, "Encrypt/decrypt the ISO-Latin 1 text");


	start = new Date();
	ciphertext = anEncryptFunction('trustno1', utf8TestString);
	end = new Date();
	encryptTime = end - start;

	start = new Date();
	plaintext = aDecryptFunction('trustno1', ciphertext);
	end = new Date();
	decryptTime = end - start;
	aTimer['utf8'] = { 'encrypt': encryptTime, 'decrypt': decryptTime };
	SimpleTest.is(utf8TestString, plaintext, "Encrypt/decrypt the UTF-8 text");
}
*/
function timeDeferredFunction (aDescription, aString, anEncryptFunction, aDecryptFunction, aTimer, aKey, someTestArgs) {
	var start;
	var end;
	
	var deferredResult;

	aTimer[aKey] = {};
	
	deferredResult = new Clipperz.Async.Deferred("timeDeferredFunction", someTestArgs);
	deferredResult.addCallback(function (aValue) { start = new Date(); return aValue});
	deferredResult.addCallback(anEncryptFunction, 'trustno1', aString);
	deferredResult.addCallback(function (aValue) {
		end = new Date();
		aTimer[aKey]['encrypt'] = end-start;
		return aValue;
	});
	deferredResult.addCallback(function (aValue) { start = new Date(); return aValue});
	deferredResult.addCallback(aDecryptFunction, 'trustno1');
	deferredResult.addCallback(function (aValue) {
		end = new Date();
		aTimer[aKey]['decrypt'] = end-start;
		return aValue;
	});
	deferredResult.addCallback(function (aValue) {
		SimpleTest.is(aString, aValue, aDescription);
	});

	deferredResult.callback();
	
	return deferredResult;
}

//=============================================================================

function encryptUsingJSCrypto (aKey, aValue) {
	var salt;
	var key;
	var cipher;
	var iv;
	var plaintext;
	var ciphertext;
	var tag;
	var adata;
	
	salt = [1,2,3,4,5,6,7,8];
	key = generateKey(aKey, salt);

	cipher = new aes(key, CCM);
	iv = Random.random_words(4);

	plaintext = aValue;
	ciphertext = [];
	tag = [];
	adata = "";

	cipher.encrypt(iv, plaintext, ciphertext, adata, tag);

	return ciphertext;
}

//-----------------------------------------------------------------------------

function decryptUsingJSCrypto (aKey, aValue) {
	var salt;
	var key;
	var cipher;
	var ciphertext;
	var plaintext;
	var tag;
	var adata;
	
	salt = [1,2,3,4,5,6,7,8];
	key = generateKey(aKey, salt);
	tag = [];
	adata = "";

	cipher = new aes(key, CCM);
	ciphertext = aValue;
	plaintext = cipher.decrypt(ciphertext, adata, tag);

	return plaintext;
}

//=============================================================================

function encryptUsingClipperz (aKey, aValue) {
	var key;
	var value;
	var data;
	var encryptedData;
	
	key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
	value = aValue;
	data = new Clipperz.ByteArray(value);
	encryptedData = Clipperz.Crypto.AES.encrypt(key, data);
	return encryptedData.toBase64String();
}

//-----------------------------------------------------------------------------

function decryptUsingClipperz (aKey, aValue) {
	var key;
	var value;
	var decryptedData;
	
	key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
	value = new Clipperz.ByteArray().appendBase64String(aValue);

	decryptedData = Clipperz.Crypto.AES.decrypt(key, value).asString();

	return decryptedData;
}

//=============================================================================

function encryptUsingClipperzAndJSON (aKey, aValue) {
	var key;
	var value;
	var data;
	var encryptedData;
	
	key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
	value = Clipperz.Base.serializeJSON(aValue);
	data = new Clipperz.ByteArray(value);
	encryptedData = Clipperz.Crypto.AES.encrypt(key, data);
	return encryptedData.toBase64String();
}

//-----------------------------------------------------------------------------

function decryptUsingClipperzAndJSON (aKey, aValue) {
	var key;
	var value;
	var decryptedData;
	
	key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
	value = new Clipperz.ByteArray().appendBase64String(aValue);

	decryptedData = Clipperz.Crypto.AES.decrypt(key, value).asString();

	return Clipperz.Base.evalJSON(decryptedData);
}

//=============================================================================

function deferredEncryptUsingClipperz (aKey, aValue) {
	var deferredResult;
	var	key;
	var data;

	key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
	data = new Clipperz.ByteArray(aValue);
	
	deferredResult = new Clipperz.Async.Deferred("Clipperz.deferredEncrypt", {trace:false});
	deferredResult.addCallback(Clipperz.Crypto.AES.deferredEncrypt, key, data);
	deferredResult.addCallback(function(aResult) {
		return aResult.toBase64String();
	})
	deferredResult.callback();
	
	return deferredResult;
}

//-----------------------------------------------------------------------------

function deferredDecryptUsingClipperz (aKey, aValue) {
	var key, value;

	key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(aKey));
	value = new Clipperz.ByteArray().appendBase64String(aValue);

	deferredResult = new Clipperz.Async.Deferred("Clipperz.deferredDecrypt", {trace:false});
	deferredResult.addCallback(Clipperz.Crypto.AES.deferredDecrypt, key, value);
	deferredResult.addCallback(function(aResult) {
		return aResult.asString();
	});
	deferredResult.callback();
	
	return deferredResult;
}

//=============================================================================

var tests = {

    //-------------------------------------------------------------------------

	'encryptMultipleStringsUsingClipperzFunctions': function (someTestArgs) {
//		timeRegularFunction(encryptUsingClipperz, decryptUsingClipperz, times['Clipperz']['Regular']);
		times['Clipperz']['NO JSON'] = {};

		timeRegularFunction("Clipperz - NO JSON - Ascii",		asciiTestString,		encryptUsingClipperz, decryptUsingClipperz, times['Clipperz']['NO JSON'], 'ascii');
		timeRegularFunction("Clipperz - NO JSON - ISO Latin 1",	isoLatin1TestString,	encryptUsingClipperz, decryptUsingClipperz, times['Clipperz']['NO JSON'], 'isoLatin1');
		timeRegularFunction("Clipperz - NO JSON - UTF-8",		utf8TestString,			encryptUsingClipperz, decryptUsingClipperz, times['Clipperz']['NO JSON'], 'utf8');

		appendResults("Clipperz - NO JSON", times['Clipperz']['NO JSON']);
	},

    //-------------------------------------------------------------------------

	'encryptMultipleStringsUsingClipperzAndJSONFunctions': function (someTestArgs) {
//		timeRegularFunction(encryptUsingClipperzAndJSON, decryptUsingClipperzAndJSON, times['Clipperz']['JSON']);
		times['Clipperz']['JSON'] = {};
		
		timeRegularFunction("Clipperz - JSON - Ascii",			asciiTestString,		encryptUsingClipperzAndJSON, decryptUsingClipperzAndJSON, times['Clipperz']['JSON'], 'ascii');
		timeRegularFunction("Clipperz - JSON - ISO Latin 1",	isoLatin1TestString,	encryptUsingClipperzAndJSON, decryptUsingClipperzAndJSON, times['Clipperz']['JSON'], 'isoLatin1');
		timeRegularFunction("Clipperz - JSON - UTF-8",			utf8TestString,			encryptUsingClipperzAndJSON, decryptUsingClipperzAndJSON, times['Clipperz']['JSON'], 'utf8');

		appendResults("Clipperz - JSON", times['Clipperz']['JSON']);
	},

    //-------------------------------------------------------------------------

	'encryptMultipleStringsUsingClipperzDeferredFunctions': function (someTestArgs) {
		times['Clipperz']['Deferred'] = {};
		times['Clipperz']['Deferred [NO JSON]'] = {};

		return Clipperz.Async.callbacks("encryptMultipleStringsUsingClipperzDeferredFunctions", [
			MochiKit.Base.partial(timeDeferredFunction, "Deferred Ascii",		asciiTestString,		deferredEncryptUsingClipperz, deferredDecryptUsingClipperz, times['Clipperz']['Deferred [NO JSON]'], 'ascii',		someTestArgs),
//			MochiKit.Base.partial(timeDeferredFunction, "Deferred IsoLatin1",	isoLatin1TestString,	deferredEncryptUsingClipperz, deferredDecryptUsingClipperz, times['Clipperz']['Deferred [NO JSON]'], 'isoLatin1',	someTestArgs),
//			MochiKit.Base.partial(timeDeferredFunction, "Deferred UTF-8",		utf8TestString,			deferredEncryptUsingClipperz, deferredDecryptUsingClipperz, times['Clipperz']['Deferred [NO JSON]'], 'utf8',		someTestArgs),

			MochiKit.Base.partial(appendResults, "Clipperz - deferred [NO JSON]", times['Clipperz']['Deferred [NO JSON]']),
			
			MochiKit.Base.partial(timeDeferredFunction, "Deferred Ascii",		asciiTestString,		Clipperz.PM.Crypto.encryptingFunctions.versions['0.3'].deferredEncrypt, Clipperz.PM.Crypto.encryptingFunctions.versions['0.3'].deferredDecrypt, times['Clipperz']['Deferred'], 'ascii',		someTestArgs),
//			MochiKit.Base.partial(timeDeferredFunction, "Deferred IsoLatin1",	isoLatin1TestString,	Clipperz.PM.Crypto.encryptingFunctions.versions['0.3'].deferredEncrypt, Clipperz.PM.Crypto.encryptingFunctions.versions['0.3'].deferredDecrypt, times['Clipperz']['Deferred'], 'isoLatin1',	someTestArgs),
//			MochiKit.Base.partial(timeDeferredFunction, "Deferred UTF-8",		utf8TestString,			Clipperz.PM.Crypto.encryptingFunctions.versions['0.3'].deferredEncrypt, Clipperz.PM.Crypto.encryptingFunctions.versions['0.3'].deferredDecrypt, times['Clipperz']['Deferred'], 'utf8',		someTestArgs),

			MochiKit.Base.partial(appendResults, "Clipperz - PM", times['Clipperz']['Deferred'])
		], someTestArgs);
	},

    //-------------------------------------------------------------------------

	'encryptMultipleStringsUsingJSCryptoFunctions': function (someTestArgs) {
		timeRegularFunction("JSCrypto - Ascii",			asciiTestString,		encryptUsingJSCrypto, decryptUsingJSCrypto, times['JSCrypto'], 'ascii');
		timeRegularFunction("JSCrypto - ISO Latin 1",	isoLatin1TestString,	encryptUsingJSCrypto, decryptUsingJSCrypto, times['JSCrypto'], 'isoLatin1');
		timeRegularFunction("JSCrypto - UTF-8",			utf8TestString,			encryptUsingJSCrypto, decryptUsingJSCrypto, times['JSCrypto'], 'utf8');

		appendResults("JSCrypto", times['JSCrypto']);
	},

    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
};

//#############################################################################

SimpleTest.waitForExplicitFinish();
Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();
Random.set_default_paranoia(0);

//Random.addEventListener("seeded", MochiKit.Base.partial(SimpleTest.runDeferredTests, "Clipperz.Crypto.JSCrypto_vs_Clipperz", tests, {trace:false}));
//Random.addEventListener("seeded", function () { console.log("SEEDED!")});


MochiKit.DOM.appendChildNodes(MochiKit.DOM.currentDocument().body,
	MochiKit.DOM.TABLE({border:'1', cellpadding:'4', cellspacing:'0'}, 
		MochiKit.DOM.THEAD(),
		MochiKit.DOM.TBODY({id:'timerTBODY'}, 
			MochiKit.DOM.TR(null, 
				MochiKit.DOM.TH(null, "algorithm"),
				MochiKit.DOM.TH(null, "ascii [" + asciiTestString.length + "]"),
				MochiKit.DOM.TH(null, "ISO Latin 1 [" + isoLatin1TestString.length * 2 + "]"),
				MochiKit.DOM.TH(null, "UTF 8 [" + utf8TestString.length * 3 + "]")
			)
		),
		MochiKit.DOM.TFOOT()
	)//,
//	MochiKit.DOM.H4(null, "AES chunkSize: " + Clipperz.Crypto.AES.DeferredExecution.chunkSize),
//	MochiKit.DOM.H4(null, "AES pauseTime: " + Clipperz.Crypto.AES.DeferredExecution.pauseTime)
);


SimpleTest.runDeferredTests("Clipperz.Crypto.JSCrypto_vs_Clipperz", tests, {trace:false});