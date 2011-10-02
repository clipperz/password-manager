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

Clipperz.Base.module('Clipperz.PM.UI.Common.Components');

Clipperz.PM.UI.Common.Components.MessagePanelWithProgressBar = function(args) {
	args = args || {};

	Clipperz.PM.UI.Common.Components.MessagePanelWithProgressBar.superclass.constructor.apply(this, arguments);

//	this._openFromElement			= args.openFromElement			|| null;
	this._onOkCloseToElement		= args.onOkCloseToElement		|| null;
	this._onCancelCloseToElement	= args.onCancelCloseToElement	|| null;

	this._canCancelWhileProcessing	= ((typeof(args.canCancelWhileProcessing) == 'undefined') ? true : args.canCancelWhileProcessing);

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Common.Components.MessagePanelWithProgressBar, Clipperz.PM.UI.Common.Components.SimpleMessagePanel, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Common.Components.MessagePanelWithProgressBar component";
	},

	//-------------------------------------------------------------------------
/*
	'openFromElement': function () {
		return this._openFromElement;
	},
*/
	//-------------------------------------------------------------------------

	'onOkCloseToElement': function () {
		return this._onOkCloseToElement;
	},

	'setOnOkCloseToElement': function (anElement) {
		this._onOkCloseToElement = anElement;
	},

	//-------------------------------------------------------------------------

	'onCancelCloseToElement': function () {
		return this._onCancelCloseToElement;
	},

	'setOnCancelCloseToElement': function (anElement) {
		this._onCancelCloseToElement = anElement;
	},

	//-------------------------------------------------------------------------

	'canCancelWhileProcessing': function () {
		return this._canCancelWhileProcessing;
	},

	//-------------------------------------------------------------------------

	'deferredShowModal': function (someArgs, aResult) {
		if (someArgs['onOkCloseToElement'] != null) {
			this.setOnOkCloseToElement(someArgs['onOkCloseToElement']);
		}

		if (someArgs['onCancelCloseToElement'] != null) {
			this.setOnCancelCloseToElement(someArgs['onCancelCloseToElement']);
		}

		Clipperz.PM.UI.Common.Components.MessagePanelWithProgressBar.superclass.deferredShowModal.apply(this, arguments);
		return this.deferred();
	},

	//-------------------------------------------------------------------------

	'showProgressBar': function () {
		var	progressBarElement;
		
		this.getElement('container').innerHTML = '';

		progressBarElement = this.append(this.getElement('container'), {tag:'div', cls:'progressBarWrapper'});
		this.addComponent(new Clipperz.PM.UI.Common.Components.ProgressBar({'element':progressBarElement}));

		if (this.canCancelWhileProcessing() == true) {
			this.setButtons([{text:"Cancel", result:'CANCEL'}]);
		} else {
			this.setButtons([]);
		}
	},

	//-------------------------------------------------------------------------

	'showFailure': function (someParameters) {
//		this.setType('ALERT');
		this.setType(someParameters['type']);
//		this.setTitle("Login failed");
		this.setTitle(someParameters['title']);
//		this.setText("Wrong passphrase; the unlock has failed.");
		this.setText(someParameters['text']);
//		this.getElement('container').innerHTML = '';
		this.getElement('container').innerHTML = '';
//		this.setButtons([{text:"Close", result:'CANCEL', isDefault:true}]);
		this.setButtons(someParameters['buttons']);
	},

	//-------------------------------------------------------------------------

	'closeOk': function () {
//console.log("=== closeOk");
		this.showProgressBar();
		MochiKit.Async.callLater(0.5, MochiKit.Base.method(this.deferred(), 'callback'));
		this._deferred = null;
	},
	
	'closeCancel': function () {
//console.log("=== closeCancel");
		this.deferredHideModal({closeToElement:this.onCancelCloseToElement()});
		this.deferred().cancel();
		this._deferred = null;
	},

	//-------------------------------------------------------------------------

	'deferredDone': function () {
//console.log("=== deferredDone");
		return this.deferredHideModal({closeToElement:this.onOkCloseToElement()});
	},
	
	'deferredError': function (someParameters) {
//console.log("=== deferredError");
		this.showFailure(someParameters);
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
