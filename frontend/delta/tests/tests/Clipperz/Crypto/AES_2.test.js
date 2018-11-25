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

function testEncryptedData (tool, keyValue, encryptedText, expectedCleanText, someTestArgs) {
	var key = Clipperz.Crypto.SHA.sha_d256(new Clipperz.ByteArray(keyValue));
	var value = new Clipperz.ByteArray().appendBase64String(encryptedText);

	var deferredResult = new Clipperz.Async.Deferred("pythonCompatibility_test", someTestArgs);
	deferredResult.addCallback(Clipperz.Crypto.AES_2.deferredDecrypt, key, value);
	deferredResult.addCallback(function(aResult) {
		return aResult.asString();
	});
	deferredResult.addTest(expectedCleanText, tool);
	deferredResult.callback();
	
	return deferredResult;
}

//=============================================================================

var tests = {
	
	'incrementNonce_test': function (someTestArgs) {
		var nonce;
		
		nonce = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
		Clipperz.Crypto.AES_2.incrementNonce(nonce)
		SimpleTest.eq(nonce, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1], "increment 0 based nonce");

		nonce = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
		Clipperz.Crypto.AES_2.incrementNonce(nonce)
		SimpleTest.eq(nonce, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2], "increment '1' nonce");

		nonce = [58,231,19,199,48,86,154,169,188,141,46,196,83,34,37,89]
		Clipperz.Crypto.AES_2.incrementNonce(nonce)
		SimpleTest.eq(nonce, [58,231,19,199,48,86,154,169,188,141,46,196,83,34,37,90], "increment '1' nonce");
		return
	},

	'pythonCompatibility_test': function (someTestArgs) {
		var keyValue  = "clipperz"
		var cleanText = "Lorem īpsum dōlōr siÞ ǽmēt, stet voluptatum ei eum, quō pērfecto lobortis eā, vel ċu deserūisse comprehēƿsam. Eu sed cībō veniam effīciendi, Þe legere ðominġ est, ðuō ċu saperet inermis pērfeċto. Vim ei essent consetētūr, quo etīam saepē æpeirian in, et atqūi velīÞ sǣepe his? Æn porrō putanÞ sinġulis mei, ēx sonet noster mea, tē alterum praesent percipitur qūo. ViÞaē neċessitatibus ne vim, per ex communē sentēntiǣe! Qui stet ǽdhūċ uÞ."

//		def testEncrypt (keyValue, cleanText):
//			key  = keyDerivation(keyValue)
//			iv = random.getrandbits(128)
//			ctr = Crypto.Util.Counter.new(128, initial_value=iv)
//			cipher = AES.new(key, Crypto.Cipher.AES.MODE_CTR, counter=ctr)
//			encryptedValue = cipher.encrypt(cleanText.encode('utf-8'))
//			data = base64.b64encode(base64.b16decode(hex(iv).upper()[2:-1]) + encryptedValue)
//			
//			return data
		
		var pythonEncryptedData = "9AFIXRO2nY0mkLJI6Xd4bd+Ov1g+kYUh73nICEVUM8OGt5FnfV/w2BfmTvdMGZjs+rF8w0ksrS9Ny8j2+2zPUUrKnVRXO6eGVPSN5VfuYFSHucV98msINH0FpOZHftuKCuJkB/orjQhoIbj9SXT0yUwB3b4R2bk48Br7R8G2bhxqrHRmnYQn22AQVA83UstNvCOdXT7ArfwJZbVSSMkdmvcziZ8ObMvaH+FXD/K8i7dzS1yP03MMBtIkYN8PnyUMS2uAHKiR11jGuha9QfXjLJlWUQWZgNB9NKyOKf7tN+OgtAoWmHmKlpTshfwbfFD8wBPR0kkhR0cC+7queIjpCDnBJ+Nod78zWgPDR8g64sph7OB686HkP03cO66aH/LNuAt03gxaVyE8ufvoStRjlIthOuys5xYWP+hTFYDC7OhCOLKvhZoY4Tr/FP+TjporX3ivCJUEEvwvXeftAxFVRl4JDin0ys0iPTQ7QlbtVa+iep2n9FUG1NOn5boD9y+iw64UJAcex4MqEIdpCHne9LjpiqshcwLmfEeLlFab28LHnvYPGkXDrSRjCujx8ZmmTw96sAIDqER8p1AqaSojwvONYBGrq+f5/f4xjzZJAknMmxYEN14Phbxc8WEhpe5omWdB80C1Kv6CLsoQnGAIshURSZryToXL"
		return testEncryptedData("python", keyValue, pythonEncryptedData, cleanText, someTestArgs)
	},

	//-------------------------------------------------------------------------

	'streamEncrypt': function (someTestArgs) {
		// var key = new Clipperz.Crypto.AES.Key({key: Clipperz.Crypto.SHA.deriveKey("super secure passphrase")});
		var key = new Clipperz.Crypto.AES.Key({key: Clipperz.PM.Crypto.deriveKey("super secure passphrase")});
		var nonce = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(128/8);

		var message = new Clipperz.ByteArray("THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG");
		
		var context = new Clipperz.Crypto.AES_2.EncryptionStreamingExecutionContext({
			nonce: nonce,
			key: key
		});

		var	originalNonce = context.nonce().clone();

		var deferredResult;
		deferredResult = new Clipperz.Async.Deferred("AES_test.streamEncrypt", {trace:false});
		deferredResult.addMethod(context, 'deferredProcessBlock', message);
		deferredResult.addCallback(function () {
			console.log("originalNonce", originalNonce.toHexString());
			console.log("currentNonce", context.nonce().toHexString());
		});
		deferredResult.callback();

		return deferredResult;
	},

	'blockStreamEncrypt': function(someTestArgs) {
		var lastNonce;

		var initialKey = Clipperz.PM.Crypto.deriveKey("super secure passphrase");
		var key = new Clipperz.Crypto.AES.Key({key: Clipperz.PM.Crypto.deriveKey("super secure passphrase")});
		var nonce = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(128/8);
		
		var message = new Clipperz.ByteArray("PRANZO D'ACQUA FA VOLTI SGHEMBI.");
		var encryptedMessage = Clipperz.Crypto.AES_2.encrypt(initialKey, message, nonce);

		var chunk1 = new Clipperz.ByteArray("PRANZO D'ACQUA F");
		var chunk2 = new Clipperz.ByteArray("A VOLTI SGHEMBI.");
		var encryptedChunk1;
		var encryptedChunk2;

		var context = new Clipperz.Crypto.AES_2.EncryptionStreamingExecutionContext({
			nonce: nonce,
			key: key
		});

		var deferredResult;
		deferredResult = new Clipperz.Async.Deferred("AES_test.blockStreamEncrypt", {trace:false});
		deferredResult.addMethod(context, 'deferredProcessBlock', chunk1);
		deferredResult.addCallback(function (aResult) {
			encryptedChunk1 = aResult;
			lastNonce = context.nonce().clone();
		});
		deferredResult.addMethod(context, 'deferredProcessBlock', chunk2);
		deferredResult.addCallback(function (aResult) {
			encryptedChunk2 = aResult;

			var compositeEncryptedMessage = new Clipperz.ByteArray();
			// compositeEncryptedMessage.appendBytes(context.nonce().arrayValues());
			compositeEncryptedMessage.appendBytes(lastNonce.arrayValues());
			compositeEncryptedMessage.appendBytes(encryptedChunk1.arrayValues());
			compositeEncryptedMessage.appendBytes(encryptedChunk2.arrayValues());

			console.log('encrypted message', encryptedMessage.toHexString());
			console.log('composite message', compositeEncryptedMessage.toHexString());

			console.log('encrypted message', encryptedMessage.toBase64String());
			console.log('composite message', compositeEncryptedMessage.toBase64String());

			console.log('encrypted message', encryptedMessage.arrayValues());
			console.log('composite message', compositeEncryptedMessage.arrayValues());
		});
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------
	'syntaxFix': MochiKit.Base.noop
}

//=============================================================================

Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();
SimpleTest.runDeferredTests("Clipperz.Crypto.AES_2", tests, {trace:false});

