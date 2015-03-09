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

	this._user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:MochiKit.Base.method(this, 'getUserPassphrase')});
	this._unlockPasswordComponent = new Clipperz.PM.UI.Web.Components.UnlockPasswordComponent({
		'title':	"Unlock account",
		'text':		"Insert the passprase to unlock the account",
		'type':		'INFO',
		'buttons': [
			{text:"Cancel",	result:'CANCEL'},
			{text:"Unlock",	result:'OK',	isDefault:true}
		],
		'openFromElement':			null,
		'onOkCloseToElement':		null,	//	this.getElement('cancelButton'),
		'onCancelCloseToElement':	null
	});

	return this;
};

Clipperz.Base.extend(Clipperz.Tests.SimpleMessagePanel.Tester, Object, {

	'toString': function() {
		return "Clipperz.Tests.SimpleMessagePanel.Tester";
	},

	//-------------------------------------------------------------------------

	'user': function () {
		return this._user;
	},
	
	'unlockPasswordComponent': function () {
		return this._unlockPasswordComponent;
	},

	//-------------------------------------------------------------------------

	'run': function () {
		var deferredResult;
		var proxy;

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});

		deferredResult = new Clipperz.Async.Deferred("unlockPassword_test.run", {trace:false});
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(this.user(), 'login');
		deferredResult.addMethod(this.user(), 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');
//		deferredResult.addCallback(MochiKit.Base.bind(function (aRecord) {
//			cardDialogController = new Clipperz.PM.UI.Web.Controllers.CardDialogController({record:aRecord, delegate:this});
//			cardDialogController.run();
//		}, this));
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'getUserPassphrase': function () {
		return this.unlockPasswordComponent().getPassphrase();
/*
		var deferredResult;
		var confirmationDialog;

		confirmationDialog = new Clipperz.PM.UI.Web.Components.UnlockPasswordComponent({
			title:	"Unlock account",
			text:	"Insert the passprase to unlock the account",
			type:	'INFO',
			buttons: [
				{text:"Cancel",	result:'CANCEL'},
				{text:"Unlock",	result:'OK',	isDefault:true}
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
*/
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
