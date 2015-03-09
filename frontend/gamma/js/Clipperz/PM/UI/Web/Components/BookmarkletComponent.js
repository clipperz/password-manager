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

Clipperz.PM.UI.Web.Components.BookmarkletComponent = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.BookmarkletComponent.superclass.constructor.apply(this, arguments);
	MochiKit.Signal.connect(Clipperz.PM.Strings.Languages, 'switchLanguage', this, 'updateBookmarkletURLs');

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.BookmarkletComponent, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.BookmarkletComponent component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function() {
//		var bookmarkletUrl;
//
//		if (Clipperz_IEisBroken == true) {
//			bookmarkletUrl = bookmarklet_ie;
//		} else {
///			bookmarkletUrl = bookmarklet;
//			bookmarkletUrl = Clipperz.PM.Strings['bookmarklet'];
//		}

		this.append(this.element(), {tag:'div', cls:'bookmarklet', children: [
			{tag:'div', id:this.getId('linkBlock'), cls:'bookmarklet_link', children:[
//				{tag:'a', id:this.getId('link'), href:bookmarkletUrl, children:[
				{tag:'a', id:this.getId('link'), href:'#', children:[
					{tag:'div', cls:'icon'},
					{tag:'div', cls:'text', children:[
						{tag:'span', html:"add to Clipperz"}
					]}
				]}
			]}
		]});

		new Clipperz.PM.UI.Common.Components.Tooltip({
			element:	this.getElement('linkBlock'),
			text:		"Drag and drop the \"add to Clipperz\" link above to the bookmark bar.",
			position:	'BELOW'
		});
		
		MochiKit.Signal.connect(this.getId('link'), 'onclick', this, 'handleOnclick');
		this.updateBookmarkletURLs();
	},

	//-------------------------------------------------------------------------

	'handleOnclick': function (anEvent) {
		anEvent.preventDefault();
		
Clipperz.log("BOOKMARKLET CLICK");
	},

	//-------------------------------------------------------------------------

	'updateBookmarkletURLs': function () {
		var bookmarkletUrl;

		if (Clipperz_IEisBroken == true) {
			bookmarkletUrl = bookmarklet_ie;
		} else {
//			bookmarkletUrl = bookmarklet;
			bookmarkletUrl = Clipperz.PM.Strings.getValue('bookmarklet');
		}

		this.getElement('link').href = bookmarkletUrl;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
