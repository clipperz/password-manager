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
