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

Clipperz.Base.module('Clipperz.PM.UI.Common.Components');

Clipperz.PM.UI.Common.Components.FaviconComponent = function(args) {
	args = args || {};

	Clipperz.PM.UI.Common.Components.FaviconComponent.superclass.constructor.apply(this, arguments);

	this.render();
	this.setSrc(args.src);

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Common.Components.FaviconComponent, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Common.Components.FaviconComponent component";
	},

	//-------------------------------------------------------------------------

	'src': function () {
		return this.element().src;
	},
	
	'setSrc': function (aValue) {
		this.element().src = (aValue || Clipperz.PM.Strings.getValue('defaultFaviconUrl'));
	},

	//-------------------------------------------------------------------------

	'clear': function () {},

	//-------------------------------------------------------------------------

	'renderSelf': function () {
		MochiKit.Signal.connect(this.element(), 'onerror',	this, 'setDefaultFavicon');
		MochiKit.Signal.connect(this.element(), 'onabort',	this, 'setDefaultFavicon');
		MochiKit.Signal.connect(this.element(), 'onload',	this, 'handleOnLoad');
	},

	//-------------------------------------------------------------------------

	'setDefaultFavicon': function (anEvent) {
		MochiKit.Signal.disconnectAll(anEvent.src());
		this.setSrc(null);
	},

	'handleOnLoad': function (anEvent) {
		MochiKit.Signal.disconnectAll(anEvent.src());

		if (anEvent.src().complete == false) {
			this.setSrc(null);
		}
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
