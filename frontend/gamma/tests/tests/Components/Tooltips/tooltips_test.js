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

Clipperz.Base.module('Clipperz.Tests.Tooltips');

Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();
Clipperz.PM.Strings.Languages.initSetup();

Clipperz.Tests.Tooltips.Tester = function(args) {
	args = args || {};
	
	Clipperz.Tests.Tooltips.Tester.superclass.constructor.call(this, args);

	return this;
};

Clipperz.Base.extend(Clipperz.Tests.Tooltips.Tester, Object, {

	'toString': function() {
		return "Clipperz.Tests.Tooltips.Tester";
	},

	//-------------------------------------------------------------------------

	'run': function () {
		var	tooltip;

//Clipperz.log("================== ABOVE ==================");
		tooltip = new Clipperz.PM.UI.Common.Components.Tooltip({
			'element':	MochiKit.DOM.getElement('ABOVE'),
			'text':		"Tooltip displayed above the element",
			'position':	'ABOVE'
		});
		tooltip.show();

//Clipperz.log("================== BELOW ==================");
		tooltip = new Clipperz.PM.UI.Common.Components.Tooltip({
			'element':	MochiKit.DOM.getElement('BELOW'),
			'text':		"Tooltip displayed below the element",
			'position':	'BELOW'
		});
		tooltip.show();

//Clipperz.log("================== LEFT ==================");
		tooltip = new Clipperz.PM.UI.Common.Components.Tooltip({
			'element':	MochiKit.DOM.getElement('LEFT'),
			'text':		"Tooltip displayed to the left of the element",
			'position':	'LEFT'
		});
		tooltip.show();

//Clipperz.log("================== RIGHT ==================");
		tooltip = new Clipperz.PM.UI.Common.Components.Tooltip({
			'element':	MochiKit.DOM.getElement('RIGHT'),
			'text':		"Tooltip displayed to the right the element",
			'position':	'RIGHT'
		});
		tooltip.show();

	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});




















init = function () {
	var	tester;
	
	tester = new Clipperz.Tests.Tooltips.Tester();
	tester.run();
};

MochiKit.DOM.addLoadEvent(init);
