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

Clipperz.Base.module('Clipperz.Tests.GridLayout');

Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();
Clipperz.PM.Strings.Languages.initSetup();

Clipperz.Tests.GridLayout.Tester = function(args) {
	args = args || {};
	
	Clipperz.Tests.GridLayout.Tester.superclass.constructor.call(this, args);
//#	this._user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
	this._user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

	return this;
};

Clipperz.Base.extend(Clipperz.Tests.GridLayout.Tester, Object, {

	'toString': function() {
		return "Clipperz.Tests.GridLayout.Tester";
	},

	//-------------------------------------------------------------------------

	'user': function () {
		return this._user;
	},

	//-------------------------------------------------------------------------

	'run': function () {
		var deferredResult;
		var proxy;
		var	cardDialogController;
		var	cardDialogComponent;
		var	component;
		var	filterController;
		var cardsController;
		
		filterController = new Clipperz.PM.UI.Web.Controllers.FilterController();
		cardsController = new Clipperz.PM.UI.Web.Controllers.CardsController({'filterController':filterController});

		component = new Clipperz.Tests.GridLayout.TestPageComponent({element:MochiKit.DOM.getElement('component')});

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});

		deferredResult = new Clipperz.Async.Deferred("GridLayout_test.init", {trace:false});
//#		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data']);
		deferredResult.addMethod(this.user(), 'login');

		deferredResult.addMethod(cardsController,	'run', {slot:component.slotNamed('cardList'), user:this.user()});
//		deferredResult.addMethod(xxxxController,	'run', {slot:component.slotNamed('cardGrid'), user:this.user()});

		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'saveChanges': function () {
		return this.user().saveChanges();
	},

    //-------------------------------------------------------------------------

	'hasPendingChanges': function () {
		return this.user().hasPendingChanges();
	},

    //-------------------------------------------------------------------------

	'revertChanges': function () {
		return this.user().revertChanges();
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});




init = function () {
	var	tester;
	
	tester = new Clipperz.Tests.GridLayout.Tester();
	tester.run();
};

MochiKit.DOM.addLoadEvent(init);
