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

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.Style) == 'undefined') { Clipperz.Style = {}; }

Clipperz.Style.VERSION = "0.1";
Clipperz.Style.NAME = "Clipperz.DOM";

MochiKit.Base.update(Clipperz.Style, {

	//-------------------------------------------------------------------------

	'__repr__': function () {
		return "[" + this.NAME + " " + this.VERSION + "]";
	},

	//-------------------------------------------------------------------------

	'toString': function () {
		return this.__repr__();
	},

	//-------------------------------------------------------------------------

	'applyZebraStylesToTable': function(aTable) {
		var tbody;
		var tbodyRows;
		var i,c;
		
		tbody = MochiKit.DOM.getFirstElementByTagAndClassName('tbody', null, aTable);
		tbodyRows = tbody.childNodes;
//		tbodyRows = MochiKit.DOM.getElementsByTagAndClassName('tr', null, tbody)
		c = tbodyRows.length;
		for (i=0; i<c; i++) {
			var element;

			element = YAHOO.Element.get(tbodyRows[i]);
			element.addClass(((i%2 == 0) ? "zebra_odd": "zebra_even"));
			element.removeClass(((i%2 == 1) ? "zebra_odd": "zebra_even"));
		}
	},

	//-------------------------------------------------------------------------

	'getSizeAndPosition': function (anElement) {
		var result;

		if (anElement != null) {
			result =	{ dimensions:MochiKit.Style.getElementDimensions(anElement), position:MochiKit.Style.getElementPosition(anElement)};
		} else {
			result =	{ dimensions:MochiKit.Style.getViewportDimensions(), position:MochiKit.Style.getViewportPosition()};
		}

		return result;
	},

	'setBackgroundGradient': function (anElement, someParameters) {
//		background: -webkit-gradient(linear, 0% 0%, 0% 100%, from(#ff9955), to(#ff6622), color-stop(1,#333333));
//		background: -moz-linear-gradient(0% 100% 90deg,#ff6622, #ff9955);
		MochiKit.Style.setStyle(anElement, {'background': '-webkit-gradient(linear, 0% 0%, 0% 100%, from(' + someParameters['from'] + '), to(' +  someParameters['to'] + '), color-stop(1,#333333))'});
		MochiKit.Style.setStyle(anElement, {'background': '-moz-linear-gradient(0% 100% 90deg,' +  someParameters['to'] + ', ' + someParameters['from'] + ')'});
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});

