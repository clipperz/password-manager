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

Clipperz.Base.module('Clipperz.Tests.ProgressBar');

Clipperz.PM.Strings.Languages.initSetup();

Clipperz.Tests.ProgressBar.Tester = function(args) {
	args = args || {};
	
	Clipperz.Tests.ProgressBar.Tester.superclass.constructor.call(this, args);

	return this;
};

Clipperz.Base.extend(Clipperz.Tests.ProgressBar.Tester, Object, {

	'toString': function() {
		return "Clipperz.Tests.ProgressBar.Tester";
	},

	//-------------------------------------------------------------------------

	'run': function () {
		var progressBar;
/*
<div id="progressBar_5"></div><hr>
<div id="progressBar_10"></div><hr>
<div id="progressBar_15"></div><hr>
<div id="progressBar_20"></div><hr>
<div id="progressBar_50"></div><hr>
<div id="progressBar_80"></div><hr>
<div id="progressBar_85"></div><hr>
<div id="progressBar_90"></div><hr>
<div id="progressBar_95"></div><hr>
<div id="progressBar_100"></div><hr>
*/
		
		progressBar = new Clipperz.PM.UI.Common.Components.ProgressBar({'element':MochiKit.DOM.getElement('progressBar_5')});
		progressBar.updateProgressHandler('5');

		progressBar = new Clipperz.PM.UI.Common.Components.ProgressBar({'element':MochiKit.DOM.getElement('progressBar_10')});
		progressBar.updateProgressHandler('10');

		progressBar = new Clipperz.PM.UI.Common.Components.ProgressBar({'element':MochiKit.DOM.getElement('progressBar_15')});
		progressBar.updateProgressHandler('15');

		progressBar = new Clipperz.PM.UI.Common.Components.ProgressBar({'element':MochiKit.DOM.getElement('progressBar_20')});
		progressBar.updateProgressHandler('20');

		progressBar = new Clipperz.PM.UI.Common.Components.ProgressBar({'element':MochiKit.DOM.getElement('progressBar_50')});
		progressBar.updateProgressHandler('50');

		progressBar = new Clipperz.PM.UI.Common.Components.ProgressBar({'element':MochiKit.DOM.getElement('progressBar_80')});
		progressBar.updateProgressHandler('80');

		progressBar = new Clipperz.PM.UI.Common.Components.ProgressBar({'element':MochiKit.DOM.getElement('progressBar_85')});
		progressBar.updateProgressHandler('85');

		progressBar = new Clipperz.PM.UI.Common.Components.ProgressBar({'element':MochiKit.DOM.getElement('progressBar_90')});
		progressBar.updateProgressHandler('90');

		progressBar = new Clipperz.PM.UI.Common.Components.ProgressBar({'element':MochiKit.DOM.getElement('progressBar_95')});
		progressBar.updateProgressHandler('95');

		progressBar = new Clipperz.PM.UI.Common.Components.ProgressBar({'element':MochiKit.DOM.getElement('progressBar_100')});
		progressBar.updateProgressHandler('100');
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});




















init = function () {
	var	tester;
	
	tester = new Clipperz.Tests.ProgressBar.Tester();
	tester.run();
};

MochiKit.DOM.addLoadEvent(init);
