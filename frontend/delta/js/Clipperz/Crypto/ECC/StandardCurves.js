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

//try { if (typeof(Clipperz.Crypto.ECC.BinaryField.Curve) == 'undefined') { throw ""; }} catch (e) {
//	throw "Clipperz.Crypto.ECC depends on Clipperz.Crypto.ECC.BinaryField.Curve!";
//}
//try { if (typeof(Clipperz.Crypto.ECC.Koblitz.Curve) == 'undefined') { throw ""; }} catch (e) {
//	throw "Clipperz.Crypto.ECC depends on Clipperz.Crypto.ECC.Koblitz.Curve!";
//}

Clipperz.Crypto.ECC.StandardCurves = {};

MochiKit.Base.update(Clipperz.Crypto.ECC.StandardCurves, {

	//==============================================================================

	'_K571': null,
	'K571': function() {	//	f(z) = z^571 + z^10 + z^5 + z^2 + 1
		if ((Clipperz.Crypto.ECC.StandardCurves._K571 == null) && (typeof(Clipperz.Crypto.ECC.Koblitz.Curve) != 'undefined')) {
			Clipperz.Crypto.ECC.StandardCurves._K571 = new Clipperz.Crypto.ECC.Koblitz.Curve({
				modulus: new Clipperz.Crypto.ECC.Koblitz.Value('08000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000425', 16),
				a: new Clipperz.Crypto.ECC.Koblitz.Value('0', 16),
				b: new Clipperz.Crypto.ECC.Koblitz.Value('1', 16),
				G: new Clipperz.Crypto.ECC.Koblitz.Point({
					x: new Clipperz.Crypto.ECC.Koblitz.Value('026eb7a8 59923fbc 82189631 f8103fe4 ac9ca297 0012d5d4 60248048 01841ca4 43709584 93b205e6 47da304d b4ceb08c bbd1ba39 494776fb 988b4717 4dca88c7 e2945283 a01c8972', 16),
					y: new Clipperz.Crypto.ECC.Koblitz.Value('0349dc80 7f4fbf37 4f4aeade 3bca9531 4dd58cec 9f307a54 ffc61efc 006d8a2c 9d4979c0 ac44aea7 4fbebbb9 f772aedc b620b01a 7ba7af1b 320430c8 591984f6 01cd4c14 3ef1c7a3', 16)
				}),
				r: new Clipperz.Crypto.ECC.Koblitz.Value('02000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 131850e1 f19a63e4 b391a8db 917f4138 b630d84b e5d63938 1e91deb4 5cfe778f 637c1001', 16),
				h: new Clipperz.Crypto.ECC.Koblitz.Value('4', 16),
				primeFactor: new Clipperz.Crypto.ECC.Koblitz.Value('02000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 131850e1 f19a63e4 b391a8db 917f4138 b630d84b e5d63938 1e91deb4 5cfe778f 637c1001', 16)
			});
		}
		
		return Clipperz.Crypto.ECC.StandardCurves._K571;
	},

	//-----------------------------------------------------------------------------

	'_K283': null,
	'K283': function() {	//	f(z) = z^283 + z^12 + z^7 + z^5 + 1
		if ((Clipperz.Crypto.ECC.StandardCurves._K283 == null) && (typeof(Clipperz.Crypto.ECC.Koblitz.Curve) != 'undefined')) {
			Clipperz.Crypto.ECC.StandardCurves._K283 = new Clipperz.Crypto.ECC.Koblitz.Curve({
				modulus: new Clipperz.Crypto.ECC.Koblitz.Value('08000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 000010a1', 16),
				a: new Clipperz.Crypto.ECC.Koblitz.Value('0', 16),
				b: new Clipperz.Crypto.ECC.Koblitz.Value('1', 16),
				G: new Clipperz.Crypto.ECC.Koblitz.Point({
					x: new Clipperz.Crypto.ECC.Koblitz.Value('0503213f 78ca4488 3f1a3b81 62f188e5 53cd265f 23c1567a 16876913 b0c2ac24 58492836', 16),
					y: new Clipperz.Crypto.ECC.Koblitz.Value('01ccda38 0f1c9e31 8d90f95d 07e5426f e87e45c0 e8184698 e4596236 4e341161 77dd2259', 16)
				}),
				r: new Clipperz.Crypto.ECC.Koblitz.Value('01ffffff ffffffff ffffffff ffffffff ffffe9ae 2ed07577 265dff7f 94451e06 1e163c61', 16),
				h: new Clipperz.Crypto.ECC.Koblitz.Value('4', 16),
				primeFactor: new Clipperz.Crypto.ECC.Koblitz.Value('01ffffff ffffffff ffffffff ffffffff ffffe9ae 2ed07577 265dff7f 94451e06 1e163c61', 16)
			});
		}
		
		return Clipperz.Crypto.ECC.StandardCurves._K283;
	},

	//==============================================================================

	'_B571': null,
	'B571': function() {	//	f(z) = z^571 + z^10 + z^5 + z^2 + 1
		if ((Clipperz.Crypto.ECC.StandardCurves._B571 == null) && (typeof(Clipperz.Crypto.ECC.BinaryField.Curve) != 'undefined')) {
			Clipperz.Crypto.ECC.StandardCurves._B571 = new Clipperz.Crypto.ECC.BinaryField.Curve({
				modulus: new Clipperz.Crypto.ECC.BinaryField.Value('08000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000425', 16),
				a: new Clipperz.Crypto.ECC.BinaryField.Value('1', 16),
				b: new Clipperz.Crypto.ECC.BinaryField.Value('02f40e7e 2221f295 de297117 b7f3d62f 5c6a97ff cb8ceff1 cd6ba8ce 4a9a18ad 84ffabbd 8efa5933 2be7ad67 56a66e29 4afd185a 78ff12aa 520e4de7 39baca0c 7ffeff7f 2955727a', 16),
				G: new Clipperz.Crypto.ECC.BinaryField.Point({
					x: new Clipperz.Crypto.ECC.BinaryField.Value('0303001d 34b85629 6c16c0d4 0d3cd775 0a93d1d2 955fa80a a5f40fc8 db7b2abd bde53950 f4c0d293 cdd711a3 5b67fb14 99ae6003 8614f139 4abfa3b4 c850d927 e1e7769c 8eec2d19', 16),
					y: new Clipperz.Crypto.ECC.BinaryField.Value('037bf273 42da639b 6dccfffe b73d69d7 8c6c27a6 009cbbca 1980f853 3921e8a6 84423e43 bab08a57 6291af8f 461bb2a8 b3531d2f 0485c19b 16e2f151 6e23dd3c 1a4827af 1b8ac15b', 16)
				}),
				r: new Clipperz.Crypto.ECC.BinaryField.Value('03ffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff e661ce18 ff559873 08059b18 6823851e c7dd9ca1 161de93d 5174d66e 8382e9bb 2fe84e47', 16),
				h: new Clipperz.Crypto.ECC.BinaryField.Value('2', 16)

//				S: new Clipperz.Crypto.ECC.BinaryField.Value('2aa058f73a0e33ab486b0f610410c53a7f132310', 10),
//				n: new Clipperz.Crypto.ECC.BinaryField.Value('03ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe661ce18ff55987308059b186823851ec7dd9ca1161de93d5174d66e8382e9bb2fe84e47', 16)
			});
			
			//-----------------------------------------------------------------------------
			//
			//	Guide to Elliptic Curve Cryptography
			//	Darrel Hankerson, Alfred Menezes, Scott Vanstone
			//	- Pag: 56, Alorithm 2.45 (with a typo!!!)
			//	
			//-----------------------------------------------------------------------------
			//
			//		http://www.milw0rm.com/papers/136
			//
			//		-------------------------------------------------------------------------
			//		  Polynomial Reduction Algorithm Modulo f571 
			//		-------------------------------------------------------------------------
			//		
			//		  Input:  Polynomial p(x) of degree 1140 or less, stored as 
			//		          an array of 2T machinewords.
			//		  Output: p(x) mod f571(x)
			//		
			//		  FOR i = T-1, ..., 0 DO
			//		      SET X  := P[i+T]
			//		      P[i]   := P[i]   ^ (X<<5) ^ (X<<7) ^ (X<<10) ^ (X<<15)
			//		      P[i+1] := P[i+1] ^ (X>>17) ^ (X>>22) ^ (X>>25) ^ (X>>27)
			//		
			//		  SET X  := P[T-1] >> 27
			//		  P[0]   := P[0] ^ X ^ (X<<2) ^ (X<<5) ^ (X<<10)
			//		  P[T-1] := P[T-1] & 0x07ffffff
			//		
			//		  RETURN P[T-1],...,P[0]
			//		
			//		-------------------------------------------------------------------------
			//		
			Clipperz.Crypto.ECC.StandardCurves._B571.finiteField().slowModule = Clipperz.Crypto.ECC.StandardCurves._B571.finiteField().module;
			Clipperz.Crypto.ECC.StandardCurves._B571.finiteField().module = function(aValue) {
				var	result;
				
				if (aValue.bitSize() > 1140) {
					Clipperz.logWarning("ECC.StandarCurves.B571.finiteField().module: falling back to default implementation");
					result = Clipperz.Crypto.ECC.StandardCurves._B571.finiteField().slowModule(aValue);
				} else {
					var	C, T;
					var i;
		
					C = aValue._value.slice(0);
					for (i=35; i>=18; i--) {
						T = C[i];
						C[i-18] = (((C[i-18] ^ (T<<5) ^ (T<<7) ^ (T<<10) ^ (T<<15)) & 0xffffffff) >>> 0);
						C[i-17] = ((C[i-17] ^ (T>>>27) ^ (T>>>25) ^ (T>>>22) ^ (T>>>17)) >>> 0);
					}
					T = (C[17] >>> 27);
					C[0] = ((C[0] ^ T ^ ((T<<2) ^ (T<<5) ^ (T<<10)) & 0xffffffff) >>> 0);
					C[17] = (C[17] & 0x07ffffff);

					for(i=18; i<=35; i++) {
						C[i] = 0;
					}
				
					result = new Clipperz.Crypto.ECC.BinaryField.Value(C);
				}
			
				return result;
			};
		}
		
		return Clipperz.Crypto.ECC.StandardCurves._B571;
	},

	//-----------------------------------------------------------------------------

	'_B283': null,
	'B283': function() {	//	f(z) = z^283 + z^12 + z^7 + z^5 + 1
		if ((Clipperz.Crypto.ECC.StandardCurves._B283 == null) && (typeof(Clipperz.Crypto.ECC.BinaryField.Curve) != 'undefined')) {
			Clipperz.Crypto.ECC.StandardCurves._B283 = new Clipperz.Crypto.ECC.BinaryField.Curve({
				modulus: new Clipperz.Crypto.ECC.BinaryField.Value('08000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 000010a1', 16),
				a: new Clipperz.Crypto.ECC.BinaryField.Value('1', 16),
				b: new Clipperz.Crypto.ECC.BinaryField.Value('027b680a c8b8596d a5a4af8a 19a0303f ca97fd76 45309fa2 a581485a f6263e31 3b79a2f5', 16),
				G: new Clipperz.Crypto.ECC.BinaryField.Point({
					x: new Clipperz.Crypto.ECC.BinaryField.Value('05f93925 8db7dd90 e1934f8c 70b0dfec 2eed25b8 557eac9c 80e2e198 f8cdbecd 86b12053', 16),
					y: new Clipperz.Crypto.ECC.BinaryField.Value('03676854 fe24141c b98fe6d4 b20d02b4 516ff702 350eddb0 826779c8 13f0df45 be8112f4', 16)
				}),
				r: new Clipperz.Crypto.ECC.BinaryField.Value('03ffffff ffffffff ffffffff ffffffff ffffef90 399660fc 938a9016 5b042a7c efadb307', 16),
				h: new Clipperz.Crypto.ECC.BinaryField.Value('2', 16)
			});
			
			//-----------------------------------------------------------------------------
			//
			//	Guide to Elliptic Curve Cryptography
			//	Darrel Hankerson, Alfred Menezes, Scott Vanstone
			//	- Pag: 56, Alorithm 2.43
			//	
			//-----------------------------------------------------------------------------
			Clipperz.Crypto.ECC.StandardCurves._B283.finiteField().slowModule = Clipperz.Crypto.ECC.StandardCurves._B283.finiteField().module;
			Clipperz.Crypto.ECC.StandardCurves._B283.finiteField().module = function(aValue) {
				var	result;

				if (aValue.bitSize() > 564) {
					Clipperz.logWarning("ECC.StandarCurves.B283.finiteField().module: falling back to default implementation");
					result = Clipperz.Crypto.ECC.StandardCurves._B283.finiteField().slowModule(aValue);
				} else {
					var	C, T;
					var i;
					
					C = aValue._value.slice(0);
					for (i=17; i>=9; i--) {
						T = C[i];
						C[i-9] = (((C[i-9] ^ (T<<5) ^ (T<<10) ^ (T<<12) ^ (T<<17)) & 0xffffffff) >>> 0);
						C[i-8] = ((C[i-8] ^ (T>>>27) ^ (T>>>22) ^ (T>>>20) ^ (T>>>15)) >>> 0);
					}
					T = (C[8] >>> 27);
					C[0] = ((C[0] ^ T ^ ((T<<5) ^ (T<<7) ^ (T<<12)) & 0xffffffff) >>> 0);
					C[8] = (C[8] & 0x07ffffff);

					for(i=9; i<=17; i++) {
						C[i] = 0;
					}
				
					result = new Clipperz.Crypto.ECC.BinaryField.Value(C);
				}
				
				return result;
			};
		}
		
		return Clipperz.Crypto.ECC.StandardCurves._B283;
	},

	//==============================================================================
	__syntaxFix__: "syntax fix"
});



