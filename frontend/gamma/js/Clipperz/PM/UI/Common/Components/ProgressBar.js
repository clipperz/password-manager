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

Clipperz.PM.UI.Common.Components.ProgressBar = function(args) {
	args = args || {};

	Clipperz.PM.UI.Common.Components.ProgressBar.superclass.constructor.apply(this, arguments);

	this._element	= args.element	|| Clipperz.Base.exception.raise('MandatoryParameter');

	this.renderSelf();

	MochiKit.Signal.connect(Clipperz.PM.UI.Common.Controllers.ProgressBarController.defaultController, 'updateProgress', this, 'updateProgressHandler')

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Common.Components.ProgressBar, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Common.Components.ProgressBar component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function() {
		this.append(this.element(), {tag:'div', cls:'loadingBar', children:[
			{tag:'div', cls:'loadingBarProgressBox', children:[
				{tag:'div', id:this.getId('loadingBarProgress'), cls:'loadingBarProgress'}
			]}
		]});
	},

	//-------------------------------------------------------------------------

	'updateProgressHandler': function (anEvent) {
		MochiKit.Style.setElementDimensions(this.getId('loadingBarProgress'), {w:anEvent}, '%');
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
