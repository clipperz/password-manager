/*

Copyright 2008-2013 Clipperz Srl

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

var hashString = function(aValue) {
	return Clipperz.Crypto.SHA.sha256(new Clipperz.ByteArray(aValue)).toHexString().substring(2)	
}

var computeSharedSecret = function (aMinuend, aSubtrahend, aPower, aModulo) {
	var	result;
	var	isBaseNegative;
	var	baseAbsoluteValue;

	isBaseNegative = (aMinuend.compare(aSubtrahend) === -1);

	if (isBaseNegative) {
		baseAbsoluteValue = aSubtrahend.subtract(aMinuend);
	} else {
		baseAbsoluteValue = aMinuend.subtract(aSubtrahend);
	}

	result = baseAbsoluteValue.powerModulo(aPower, aModulo);
	
	if (isBaseNegative && (aPower.modulo(2) == 1)) {
		result = aModulo.subtract(result);
	}
	
	return result;
}

var tests = {

	'test_01': function (someTestArgs) {
		var	username, passphrase;
		var C, P, salt, x, v;
	
		username = "giulio.cesare";
		passphrase = "trustno1";
	
		C = hashString(username);
		is (C, "bde3c7b5fdcd9d6ce72782ca1ae912fc4397d668fcb3a73a04e5d47852670c4a", "C");
	
		P  = hashString(passphrase + username);
		is (P, "d79f5c5a04e91e1c85fb64cb6ee9481cb52c181047f69da02cd6c3ce6d058a76", "P");

		salt = "cf1fa93393ade60318b8276f1f39420098419445005a7dc9117975fe1f8d9988";

		x = hashString(salt + P);
		is(x, "21fe88a158e420aade86e00b5eb12a4c19bf15482fa34c542c90b1afdbd5b5fd", "x");
	
		v = Clipperz.Crypto.SRP.g().powerModulo(new Clipperz.Crypto.BigInt(x, 16), Clipperz.Crypto.SRP.n());
		is(v.asString(10), "33816467430011076413789931449607305355248467973000153409872503376381719918118", "v");
		is(v.asString(16), "4ac37139dbf32ebabd2c43f91dd085066d3c457d059efd5902d32ed247fcb626", "v (base 16)");
	},
	
	'test_02': function (someTestArgs) {
		var	username, passphrase;
		var C, P, salt, x, v;

		username = "giulio.cesare.debug";
		passphrase = "trustno1";
	
		C = hashString(username);
		is (C, "fa1af609123b97a10d676158ed538d4657a89ac33a102b22bd9a66712039e208", "C");

		P  = hashString(passphrase + username);
		is (P, "e1bfba03dd626b12f29458a6ad63fb2c01b4765548504e1e2f6b1503c82e4253", "P");

		salt = "cf1fa93393ade60318b8276f1f39420098419445005a7dc9117975fe1f8d9988";

		x = hashString(salt + P);
		is(x, "93d4af3cdcd2447a745d309826dff3161feed4b15f32db8e909ff032a2bc8fb8", "x");
	
		v = Clipperz.Crypto.SRP.g().powerModulo(new Clipperz.Crypto.BigInt(x, 16), Clipperz.Crypto.SRP.n());
		is(v.asString(10), "115049747015252903452664067168789229427785288458366249918596663144588656606556", "v");
	},
	
	'test_03': function (someTestArgs) {
		var srpConnection;
		var C, P, salt;
	
		C = 	"da8602c2f847306f4eb9acdaad925277d1fad1408f173f128a078aea15e60b1e";
		P =		"77643559beca49dd21c1c31db10bb0a9009662cb504413dc3fa3b7303c7e02ba";
		salt =	"000108cbbacda1f03ea9360301045434ec7d82ba150936df08a229cbb4832ce1";
	
		srpConnection = new Clipperz.Crypto.SRP.Connection({C:C, P:P, hash:Clipperz.Crypto.SHA.sha_d256});
		srpConnection._a = new Clipperz.Crypto.BigInt("37532428169486597638072888476611365392249575518156687476805936694442691012367", 10);
		srpConnection.set_s(new Clipperz.Crypto.BigInt(salt, 16));
		is (srpConnection.s().asString(16, 64), salt, "salt read/write is coherent");
		srpConnection.set_B(new Clipperz.Crypto.BigInt("123541032067854367017620977654446651448957899464139861291542193929199957895435", 10));

		is(	srpConnection.serverSideCredentialsWithSalt(salt).v,
			"c73169c8236d37bf9ef11a2349e3064b7dc6e883a58d64443ea9235677520030",
			"server side credentials - v"
		)
//		is(srpConnection.S(), "84134227508133659832466942692590826994401065200828529664948840490489960952050", "Server side 'S'");
		is(srpConnection.S(), "55780214238882962241382243585437722049275373112288850873807972077317415224224", "Server side 'S'");

		srpConnection = new Clipperz.Crypto.SRP.Connection({C:C, P:P, hash:Clipperz.Crypto.SHA.sha_d256});
		try {
			srpConnection.set_B(new Clipperz.Crypto.BigInt("0", 10));
			ok(false, "Setting B to 0 should raise an exception");
		} catch(e) {
			ok(true, "Setting B to 0 should raise an exception");
		}
	},
	
	'rfc_test_vectors': function (someTestArgs) {
		var I = "alice";
		var P = "password123";
		var s = new Clipperz.Crypto.BigInt("beb25379d1a8581eb5a727673a2441ee", 16);

		var N = new Clipperz.Crypto.BigInt("eeaf0ab9adb38dd69c33f80afa8fc5e86072618775ff3c0b9ea2314c9c256576d674df7496ea81d3383b4813d692c6e0e0d5d8e250b98be48e495c1d6089dad15dc7d7b46154d6b6ce8ef4ad69b15d4982559b297bcf1885c529f566660e57ec68edbc3c05726cc02fd4cbf4976eaa9afd5138fe8376435b9fc61d2fc0eb06e3", 16);
		var sizeN = N.asString(16).length;
		var g = new Clipperz.Crypto.BigInt(2);

//		k = SHA1(N | PAD(g))
		var k = new Clipperz.Crypto.BigInt(Clipperz.Crypto.SHA.sha1(new Clipperz.ByteArray('0x' + N.asString(16) + g.asString(16, sizeN))).toHexString(false), 16);
		is("7556aa045aef2cdd07abaf0f665c3e818913186f", k.asString(16), "k");

//		x = SHA1(s | SHA1(I | ":" | P))
		var hash_I_P = Clipperz.Crypto.SHA.sha1(new Clipperz.ByteArray(I + ":" + P)).toHexString(false);
		var x = new Clipperz.Crypto.BigInt(Clipperz.Crypto.SHA.sha1(new Clipperz.ByteArray('0x' + s.asString(16) + hash_I_P)).toHexString(false), 16);
		is("94b7555aabe9127cc58ccf4993db6cf84d16c124", x.asString(16), "x");
		
//		v = g^x % N
		var v = g.powerModulo(x, N);
		is("7e273de8696ffc4f4e337d05b4b375beb0dde1569e8fa00a9886d8129bada1f1822223ca1a605b530e379ba4729fdc59f105b4787e5186f5c671085a1447b52a48cf1970b4fb6f8400bbf4cebfbb168152e08ab5ea53d15c1aff87b2b9da6e04e058ad51cc72bfc9033b564e26480d78e955a5e29e7ab245db2be315e2099afb", v.asString(16), "v");

		var a = new Clipperz.Crypto.BigInt("60975527035cf2ad1989806f0407210bc81edc04e2762a56afd529ddda2d4393", 16);
		var b = new Clipperz.Crypto.BigInt("e487cb59d31ac550471e81f00f6928e01dda08e974a004f49e61f5d105284d20" ,16);
		is("e487cb59d31ac550471e81f00f6928e01dda08e974a004f49e61f5d105284d20", b.asString(16), "b");

//		A = pow(g, a, N)
		var A = g.powerModulo(a, N);
		is("61d5e490f6f1b79547b0704c436f523dd0e560f0c64115bb72557ec44352e8903211c04692272d8b2d1a5358a2cf1b6e0bfcf99f921530ec8e39356179eae45e42ba92aeaced825171e1e8b9af6d9c03e1327f44be087ef06530e69f66615261eef54073ca11cf5858f0edfdfe15efeab349ef5d76988a3672fac47b0769447b", A.asString(16), "A");

//		B = k*v + g^b % N
		var B = (k.multiply(v)).add(g.powerModulo(b, N)).modulo(N);
		is("bd0c61512c692c0cb6d041fa01bb152d4916a1e77af46ae105393011baf38964dc46a0670dd125b95a981652236f99d9b681cbf87837ec996c6da04453728610d0c6ddb58b318885d7d82c7f8deb75ce7bd4fbaa37089e6f9c6059f388838e7a00030b331eb76840910440b1b27aaeaeeb4012b7d7665238a8e3fb004b117b58", B.asString(16), "B");

//		u = SHA1(PAD(A) | PAD(B))
		var u = new Clipperz.Crypto.BigInt(Clipperz.Crypto.SHA.sha1(new Clipperz.ByteArray('0x' + A.asString(16, sizeN) + B.asString(16, sizeN))).toHexString(false), 16);
		is("ce38b9593487da98554ed47d70a7ae5f462ef019", u.asString(16), "u");

//		secret = (B - (k * g^x)) ^ (a + (u * x)) % N
		var secret = computeSharedSecret(B, k.multiply(g.powerModulo(x, N)), a.add(u.multiply(x)), N);
		is("b0dc82babcf30674ae450c0287745e7990a3381f63b387aaf271a10d233861e359b48220f7c4693c9ae12b0a6f67809f0876e2d013800d6c41bb59b6d5979b5c00a172b4a2a5903a0bdcaf8a709585eb2afafa8f3499b200210dcc1f10eb33943cd67fc88a2f39a4be5bec4ec0a3212dc346d7e474b29ede8a469ffeca686e5a", secret.asString(16), "secret");
	},
}

SimpleTest.runDeferredTests("Clipperz.Crypto.SRP", tests, {trace:false});

