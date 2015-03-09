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

Clipperz.Base.module('Clipperz.PM.UI.Web.Components');

Clipperz.PM.UI.Web.Components.Page = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.Page.superclass.constructor.apply(this, arguments);

	this._slots = {
		'header':	'pageHeader',
		'body':		'pageBody',
		'footer':	'pageFooter'
	};

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.Page, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.Page component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(), [
			{tag:'div', id:'pageHeaderAndBody', cls:'pageHeaderAndBody', children:[
				{tag:'div', id:'pageHeader', cls:'pageHeader'},
				{tag:'div', id:'pageBody',   cls:'pageBody'}
			]},
			{tag:'div', id:'pageFooter', cls:'pageFooter'}
		]);
		
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});
