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

//#############################################################################

Clipperz.PM.UI.Web.Components.FaviconColumnManager = function(args) {
	args = args || {};
	Clipperz.PM.UI.Web.Components.FaviconColumnManager.superclass.constructor.call(this, args);
	
	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.FaviconColumnManager, Clipperz.PM.UI.Web.Components.ColumnManager, {

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.FaviconColumnManager component";
	},
	
	//-------------------------------------------------------------------------

	'renderCell': function(aRowElement, anObject) {
		var	faviconImageElement;
		var faviconUrl;
		
		faviconImageElement = this.getId('favicon');
		faviconUrl = anObject[this.name()];

		if (faviconUrl == null) {
			faviconUrl = Clipperz.PM.Strings.getValue('defaultFaviconUrl');
		}

		Clipperz.DOM.Helper.append(aRowElement, {tag:'td', cls:this.cssClass(), children:[
			{tag:'img', id:faviconImageElement, src:faviconUrl}
		]});

		MochiKit.Signal.connect(faviconImageElement, 'onload',  this, 'handleLoadedFaviconImage');
		MochiKit.Signal.connect(faviconImageElement, 'onerror', this, 'handleMissingFaviconImage');
		MochiKit.Signal.connect(faviconImageElement, 'onabort', this, 'handleMissingFaviconImage');
	},

	//-----------------------------------------------------

	'handleLoadedFaviconImage': function(anEvent) {
		MochiKit.Signal.disconnectAllTo(anEvent.src());
		if (anEvent.src().complete == false) {
			anEvent.src().src = Clipperz.PM.Strings.getValue('defaultFaviconUrl');
		}
	},

	//-----------------------------------------------------

	'handleMissingFaviconImage': function(anEvent) {
		MochiKit.Signal.disconnectAllTo(anEvent.src());
		anEvent.src().src = Clipperz.PM.Strings.getValue('defaultFaviconUrl');
	},

	//-----------------------------------------------------
	'__syntax_fix__' : 'syntax fix'
});

