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
					{tag:'canvas', id:this.getId('footerStarIcon'), cls:'footerStarIcon'},
					{tag:'span', cls:'copyright', html:'Copyright &copy; 2009-2013 Clipperz Srl'},
					{tag:'a', href:'https://www.clipperz.com/terms_service',	target:'_blank', html:'terms of service'},
					{tag:'a', href:'http://www.clipperz.com/privacy_policy',	target:'_blank', html:'privacy policy'},
					{tag:'div', cls:'applicationVersion', htmlString:'application version: <a href="https://github.com/clipperz/password-manager/tree/' + Clipperz_version + '" target="github">' + Clipperz_version + '</a>'}
				]}
			]}
		]);

		Clipperz.PM.UI.Canvas.star.normal(this.getElement('footerStarIcon'), "#7e7e7e");
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});
