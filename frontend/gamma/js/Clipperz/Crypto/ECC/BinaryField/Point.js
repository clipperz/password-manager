/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz's Javascript Crypto Library.
Javascript Crypto Library provides web developers with an extensive
and efficient set of cryptographic functions. The library aims to
obtain maximum execution speed while preserving modularity and
reusability.
For further information about its features and functionalities please
refer to http://www.clipperz.com

* Javascript Crypto Library is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Javascript Crypto Library is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Javascript Crypto Library.  If not, see
  <http://www.gnu.org/licenses/>.

*/

//try { if (typeof(Clipperz.ByteArray) == 'undefined') { throw ""; }} catch (e) {
//	throw "Clipperz.Crypto.ECC depends on Clipperz.ByteArray!";
//}  
if (typeof(Clipperz.Crypto.ECC) == 'undefined') { Clipperz.Crypto.ECC = {}; }
if (typeof(Clipperz.Crypto.ECC.BinaryField) == 'undefined') { Clipperz.Crypto.ECC.BinaryField = {}; }

Clipperz.Crypto.ECC.BinaryField.Point = function(args) {
	args = args || {};
	this._x = args.x;
	this._y = args.y;
	
	return this;
}

Clipperz.Crypto.ECC.BinaryField.Point.prototype = MochiKit.Base.update(null, {

	'asString': function() {
		return "Clipperz.Crypto.ECC.BinaryField.Point (" + this.x() + ", " + this.y() + ")";
	},

	//-----------------------------------------------------------------------------

	'x': function() {
		return this._x;
	},
	
	'y': function() {
		return this._y;
	},

	//-----------------------------------------------------------------------------

	'isZero': function() {
		return (this.x().isZero() && this.y().isZero())
	},
	
	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
