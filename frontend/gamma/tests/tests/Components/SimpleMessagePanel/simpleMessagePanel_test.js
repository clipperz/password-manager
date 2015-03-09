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

Clipperz.Base.module('Clipperz.Tests.SimpleMessagePanel');

Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();
Clipperz.PM.Strings.Languages.initSetup();

Clipperz.Tests.SimpleMessagePanel.Tester = function(args) {
	args = args || {};
	
	Clipperz.Tests.SimpleMessagePanel.Tester.superclass.constructor.call(this, args);
//	this._user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});

	return this;
};

Clipperz.Base.extend(Clipperz.Tests.SimpleMessagePanel.Tester, Object, {

	'toString': function() {
		return "Clipperz.Tests.SimpleMessagePanel.Tester";
	},

	//-------------------------------------------------------------------------
/*
	'user': function () {
		return this._user;
	},
*/
	//-------------------------------------------------------------------------

	'run': function () {
		var deferredResult;
		var confirmationDialog;
		
		confirmationDialog = new Clipperz.PM.UI.Common.Components.SimpleMessagePanel({
			title:	"Alert",
			text:	"Should lost pending changes? But I must explain to you how all this mistaken idea of denouncing pleasure and praising pain was born and I will give you a complete account of the system, and expound the actual teachings of the great explorer of the truth, the master-builder of human happiness",
			type:	'ALERT',
			buttons: [
				{text:"Cancel",	result:'CANCEL', isDefault:true},
				{text:"Ok",		result:'OK'}
			]
		});

		deferredResult = new Clipperz.Async.Deferred("CardDialogComponent.askConfirmationForLoosingPendingChanges", {trace:false});
		deferredResult.addMethod(confirmationDialog, 'deferredShow', {
			'openFromElement':			null,
			'onOkCloseToElement':		null,	//	this.getElement('cancelButton'),
			'onCancelCloseToElement':	null
		});
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});



init = function () {
	var	tester;
	
	tester = new Clipperz.Tests.SimpleMessagePanel.Tester();
	tester.run();
};

MochiKit.DOM.addLoadEvent(init);
