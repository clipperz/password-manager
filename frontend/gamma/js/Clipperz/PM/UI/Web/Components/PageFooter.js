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

Clipperz.Base.module('Clipperz.PM.UI.Web.Components');

Clipperz.PM.UI.Web.Components.PageFooter = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.PageFooter.superclass.constructor.apply(this, arguments);

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.PageFooter, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.PageFooter component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(), [
			{tag:'div', cls:'footerWrapper', children:[
				{tag:'div', cls:'footerContent', children:[
//					{tag:'div', cls:'footerStarIcon'},
					{tag:'canvas', id:this.getId('footerStarIcon'), cls:'footerStarIcon'},
					{tag:'span', cls:'copyright', html:'Copyright &copy; 2009 Clipperz Srl'},
					{tag:'a', href:'http://www.clipperz.com/terms_of_service',	target:'_blank', html:'terms of service'},
					{tag:'a', href:'http://www.clipperz.com/privacy_policy',	target:'_blank', html:'privacy policy'},
					{tag:'span', cls:'applicationVersion', html:'application version: [1992]'}
				]}
			]}
		]);

		Clipperz.PM.UI.Canvas.star.normal(this.getElement('footerStarIcon'), "#7e7e7e");
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});
