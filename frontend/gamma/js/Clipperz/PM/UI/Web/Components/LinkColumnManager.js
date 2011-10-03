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

Clipperz.PM.UI.Web.Components.LinkColumnManager = function(args) {
	args = args || {};
	Clipperz.PM.UI.Web.Components.LinkColumnManager.superclass.constructor.call(this, args);

	this._actionMethod = args.actionMethod || null;
	
	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.LinkColumnManager, Clipperz.PM.UI.Web.Components.ColumnManager, {

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.DateColumnManager component";
	},

	//-------------------------------------------------------------------------

	'actionMethod': function () {
		return this._actionMethod;
	},

	//-------------------------------------------------------------------------

	'renderCell': function(aRowElement, anObject) {
		var	tdElement;
		var	linkElement;

		tdElement = Clipperz.DOM.Helper.append(aRowElement, {tag:'td', cls:this.cssClass(), children:[
			{tag:'span', children:[
				{tag:'a', href:'#', html:anObject[this.name()]}
			]}
		]});

		linkElement = MochiKit.DOM.getFirstElementByTagAndClassName('a', null, tdElement);
//		MochiKit.Signal.connect(linkElement, 'onclick', MochiKit.Base.method(this, 'handleLinkClick', anObject['_rowObject']));
		this.connectEvent(linkElement, 'onclick', MochiKit.Base.method(this, 'handleLinkClick', anObject['_rowObject']));
	},

	//-----------------------------------------------------

	'handleLinkClick': function (anObject, anEvent) {
		anEvent.preventDefault();

		if (this.actionMethod() != null) {
			var deferredResult;

			deferredResult = new Clipperz.Async.Deferred("LinkColumnManager.handleLinkClick", {trace:false});
//			deferredResult.addCallbackPass(MochiKit.Signal.signal, this, 'selectRow', anObject);
			deferredResult.addCallback(this.actionMethod(), anObject, anEvent);
//			deferredResult.addBothPass(MochiKit.Signal.signal, this, 'unselectRow', anObject);
			deferredResult.callback();
		}
	},

	//-----------------------------------------------------
	'__syntax_fix__' : 'syntax fix'	
});

