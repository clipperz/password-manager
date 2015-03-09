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

console.log(documentText);