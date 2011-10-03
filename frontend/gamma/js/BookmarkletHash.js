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

//	18f820faffcdb5e847d4c5d5c4a1de6743baa1a0
//	9b30434c73fb009b15fecaa904b44f9ced807577
//	9b30434c73fb009b15fecaa904b44f9ced807577
var	xh;
var	documentText;

try {
	xh=new XMLHttpRequest();
} catch(e) {
	xh=new ActiveXObject("Msxml2.XMLHTTP");
}

xh.open("GET", window.location, false);
xh.send(null);
	
documentText = "#####" + xh.responseText + "####";
//documentText = document.body.innerHTML;

//console.log(documentText);