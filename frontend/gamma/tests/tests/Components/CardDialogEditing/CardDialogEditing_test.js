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

Clipperz.Base.module('Clipperz.Tests.CardDialogEditing');

Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();
Clipperz.PM.Strings.Languages.initSetup();

Clipperz.Tests.CardDialogEditing.Tester = function(args) {
	args = args || {};
	
	Clipperz.Tests.CardDialogEditing.Tester.superclass.constructor.call(this, args);
//#	this._user = new Clipperz.PM.DataModel.User({username:'test', getPassphraseFunction:function () { return 'test';}});
	this._user = new Clipperz.PM.DataModel.User({username:'joe', getPassphraseFunction:function () { return 'clipperz';}});

	return this;
};

Clipperz.Base.extend(Clipperz.Tests.CardDialogEditing.Tester, Object, {

	'toString': function() {
		return "Clipperz.Tests.CardDialogEditing.Tester";
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

		proxy = new Clipperz.PM.Proxy.Test({shouldPayTolls:true, isDefault:true, readOnly:false});

		deferredResult = new Clipperz.Async.Deferred("CardDialogEditing_test.init", {trace:false});
//#		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['test_test_offline_copy_data']);
		deferredResult.addMethod(proxy.dataStore(), 'setupWithEncryptedData', testData['joe_clipperz_offline_copy_data_newVersion']);
		deferredResult.addMethod(this.user(), 'login');
//#		deferredResult.addMethod(this.user(), 'getRecord', '8280842f41162b673335b63860637e8472e8bbff0efa2bc78b0dbc5e09712e13');	

//		deferredResult.addMethod(this.user(), 'getRecord', '062af892bcfba49ffcff05c56d99b7af2d508358e39c058c2e1fc83531436f80');	//	Linkedin
//		deferredResult.addMethod(this.user(), 'getRecord', '084e23120544603f0297368fd3891a3818e0fe13488e2f2c6079913c8d1bed8d');	//	Example Attack
		deferredResult.addMethod(this.user(), 'getRecord', '13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551');	//	Amazon
//		deferredResult.addMethod(this.user(), 'getRecord', '507f38b06d587d8889698ae1ebbba7ef8f0539b82550dd25779fd9ee88fc0c7a');	//	MyBlogLog
//		deferredResult.addMethod(this.user(), 'getRecord', '6026370f3db3860d2c46a08e389a7e906dc14f98c8444b21be9a7e9f405a2728');	//	Del.icio.us
//		deferredResult.addMethod(this.user(), 'getRecord', '6c25be8e145efb26a1abd59590522f73fb2e3dbc139af2217074d9e2ba92c16a');	//	Microsoft Office Key
//		deferredResult.addMethod(this.user(), 'getRecord', '6d45c2fec275b7482d41c76b20507100cfb6ab49922b876f9dd3040d361f4a18');	//	The NewYork Times
//		deferredResult.addMethod(this.user(), 'getRecord', '7bb69b6366a8012f181c01e368ba18d4f7a82bcabb4959189736ad124c4bbfbb');	//	Web Password
//		deferredResult.addMethod(this.user(), 'getRecord', '8b18e8593b5bc2f7ea39a5fab222047034ef2f380fee05be0fa6e1c0972fea39');	//	Jaiku
//		deferredResult.addMethod(this.user(), 'getRecord', '9dcd2a8a0fcb7e57d234dc4fea347f020a6a01793e40cf56a0d22379e590e291');	//	Home Burglar Alarm
//		deferredResult.addMethod(this.user(), 'getRecord', 'ca01bcb7691f70818feed46c9a2a91883ac543997a395535aedbb49de166690c');	//	MySpace
//		deferredResult.addMethod(this.user(), 'getRecord', 'd620764a656bfd4e1d3758500d5db72e460a0cf729d56ed1a7755b5725c50045');	//	American Airlines
//		deferredResult.addMethod(this.user(), 'getRecord', 'de13c0d036234c44214062bc0a89e5f127470c464333493a485729f43cdc26e4');	//	Luftansa
//		deferredResult.addMethod(this.user(), 'getRecord', 'eeda70e0392261967bda71c3764da78989c45bbd2bb7be6b941b90f81d9b81b5');	//	Google Account
//		deferredResult.addMethod(this.user(), 'getRecord', 'f215d89bf4583c12f5ed4f4330f488dad3fffa448f4dc784f15ef135dda2c732');	//	Expedia
//		deferredResult.addMethod(this.user(), 'getRecord', 'fe21497ef7435d31f9746c132e4b5ecf5aac5f13b5961ddb55d2bdc3409f28f6');	//	Bloglines

//		deferredResult.addMethod(this.user(), 'getRecord', '5cdac63b317f3942da38f3a3de3b7f0e5d6678200951c6216230295550f63fb4');	//	WEB PASSWORD (0)
//		deferredResult.addMethod(this.user(), 'getRecord', '36ec1a41118813ced3553534fa2607d781cba687768db305beed368a8e06e113');	//	DIGG  (1)
//		deferredResult.addMethod(this.user(), 'getRecord', 'c0ce9130ca365bb02418d4305ea1d29e49c3f0e96d44b9d3cb6b4b6843d25065');	//	SAP   (2)
//		deferredResult.addMethod(this.user(), 'getRecord', 'd5f700b9c3367c39551ea49e00a9ab20dae09dd79d46047b983fc7c4bfaa050d');	//	YAHOO (4)

//		deferredResult.addMethod(this.user(), 'createNewRecord');

/*
		deferredResult.addCallback(function (aRecord) {
			return Clipperz.Async.callbacks("CardDialogEditing_test.init - setup new record", [
				MochiKit.Base.method(aRecord, 'setLabel', "Clipperz staging site"),
				MochiKit.Base.method(aRecord, 'addField', {'label':"URL", 'value':"http://d6.clipperz.com", 'isHidden':false}),
				MochiKit.Base.method(aRecord, 'addField', {'label':"USERNAME", 'value':"staging", 'isHidden':false}),
				MochiKit.Base.method(aRecord, 'addField', {'label':"PASSWORD", 'value':"drupal", 'isHidden':true}),

				MochiKit.Base.method(aRecord, 'createNewDirectLogin'),
				function (aDirectLogin) {
					return Clipperz.Async.callbacks("CardDialogEditing_test.init - setup new record [directLogin]", [
						MochiKit.Base.method(aDirectLogin, 'setLabel', 'Clipperz staging site'),
						MochiKit.Base.method(aDirectLogin, 'setFavicon', 'http://www.apple.com/favicon.ico'),
						MochiKit.Base.method(aDirectLogin, 'setBookmarkletConfiguration', '{ "page":{"title":"HTTP authentication"},\n  "form":{"attributes": {\n    "action":"",\n    "type":"http_auth"\n  }, "inputs": [\n    {"type":"text","name":"url","value":""},\n    {"type":"text","name":"username","value":""},\n    {"type":"password","name":"password","value":""}\n  ]}, "version":"0.2.3"}'),

						MochiKit.Base.method(aDirectLogin, 'bindFormFieldWithLabelToRecordFieldWithLabel', 'url', 'URL'),
						MochiKit.Base.method(aDirectLogin, 'bindFormFieldWithLabelToRecordFieldWithLabel', 'username', 'USERNAME'),
						MochiKit.Base.method(aDirectLogin, 'bindFormFieldWithLabelToRecordFieldWithLabel', 'password', 'PASSWORD')
					], {trace:false});
				},
				MochiKit.Base.partial(MochiKit.Async.succeed, aRecord)
			], {trace:false});
		});

//		deferredResult.addMethod(this.user(), 'createNewRecord');
		deferredResult.addCallback(function (aRecord) {
			return Clipperz.Async.callbacks("CardDialogEditing_test.init - setup another record", [
				MochiKit.Base.method(aRecord, 'setLabel', "Twitter site"),
				MochiKit.Base.method(aRecord, 'addField', {'label':"twitter_url", 'value':"http://www.twitter.com", 'isHidden':false}),
				MochiKit.Base.method(aRecord, 'addField', {'label':"twitter_username", 'value':"staging", 'isHidden':false}),
				MochiKit.Base.method(aRecord, 'addField', {'label':"twitter_password", 'value':"drupal", 'isHidden':true}),

				MochiKit.Base.method(aRecord, 'createNewDirectLogin'),
				function (aDirectLogin) {
					return Clipperz.Async.callbacks("CardDialogEditing_test.init - setup another record [directLogin]", [
						MochiKit.Base.method(aDirectLogin, 'setLabel', 'Twitter'),
						MochiKit.Base.method(aDirectLogin, 'setFavicon', 'http://www.twitter.com/favicon.ico'),
						MochiKit.Base.method(aDirectLogin, 'setBookmarkletConfiguration', '{"page": {"title": "Twitter"},\n"form": {"attributes": {"action": "https://twitter.com/sessions",\n"method": "post"},\n"inputs": [{"type": "hidden",\n"name": "authenticity_token",\n"value": "b78a9460629980ed4c705fe94e6c4f37bfc32fed"},\n{"type": "text",\n"name": "session[username_or_email]",\n"value": ""},\n{"type": "password",\n"name": "session[password]",\n"value": ""},\n{"type": "checkbox",\n"name": "remember_me",\n"value": "1"},\n{"type": "hidden",\n"name": "q",\n"value": ""}]},\n"version": "0.2.3"}'),

						MochiKit.Base.method(aDirectLogin, 'bindFormFieldWithLabelToRecordFieldWithLabel', 'session[username_or_email]', 'twitter_username'),
						MochiKit.Base.method(aDirectLogin, 'bindFormFieldWithLabelToRecordFieldWithLabel', 'session[password]', 'twitter_password'),

						MochiKit.Base.method(aDirectLogin, 'formValues'),
						MochiKit.Base.itemgetter('remember_me'),
						MochiKit.Base.methodcaller('setValue', 1)
						
					], {trace:false});
				},
				MochiKit.Base.partial(MochiKit.Async.succeed, aRecord)
			], {trace:false});
		});
		
//		deferredResult.addMethod(this.user(), 'createNewRecord');
		deferredResult.addCallback(function (aRecord) {
			return Clipperz.Async.callbacks("CardDialogEditing_test.init - setup third record", [
				MochiKit.Base.method(aRecord, 'setLabel', "R@cine"),
				MochiKit.Base.method(aRecord, 'addField', {'label':"R@cine_username", 'value':"joe", 'isHidden':false}),
				MochiKit.Base.method(aRecord, 'addField', {'label':"R@cine_password", 'value':"clipperz", 'isHidden':true}),

				MochiKit.Base.method(aRecord, 'createNewDirectLogin'),
				function (aDirectLogin) {
					return Clipperz.Async.callbacks("CardDialogEditing_test.init - setup third record [directLogin]", [
						MochiKit.Base.method(aDirectLogin, 'setLabel', 'R@cine'),
						MochiKit.Base.method(aDirectLogin, 'setFavicon', 'http://www.racine.ra.it/horde/imp/graphics/favicon.ico'),
						MochiKit.Base.method(aDirectLogin, 'setBookmarkletConfiguration', '{"page": {"title": "IMP :: Welcome to Horde"},\n"form": {"attributes": {"action": "http://www.racine.ra.it/horde/imp/redirect.php",\n"method": "post"},\n"inputs": [{"type": "hidden",\n"name": "actionID",\n"value": "105"},\n{"type": "hidden",\n"name": "url",\n"value": ""},\n{"type": "hidden",\n"name": "mailbox",\n"value": "INBOX"},\n{"type": "text",\n"name": "imapuser",\n"value": ""},\n{"type": "password",\n"name": "pass",\n"value": ""},\n{"type": "hidden",\n"name": "server",\n"value": "localhost"},\n{"type": "hidden",\n"name": "port",\n"value": "143"},\n{"type": "hidden",\n"name": "namespace",\n"value": ""},\n{"type": "hidden",\n"name": "maildomain",\n"value": "racine.ra.it"},\n{"type": "hidden",\n"name": "protocol",\n"value": "imap/notls"},\n{"type": "hidden",\n"name": "realm",\n"value": "racine.ra.it"},\n{"type": "hidden",\n"name": "folders",\n"value": "mail/"},\n{"type": "select",\n"name": "new_lang",\n"options": [{"selected": false,\n"label": "Bulgarian",\n"value": "bg_BG"},\n{"selected": false,\n"label": "Chinese (Simplified)",\n"value": "zh_CN"},\n{"selected": false,\n"label": "Chinese (Traditional)",\n"value": "zh_TW"},\n{"selected": false,\n"label": "Czech",\n"value": "cs_CZ"},\n{"selected": false,\n"label": "Dansk",\n"value": "da_DK"},\n{"selected": false,\n"label": "Deutsch",\n"value": "de_DE"},\n{"selected": false,\n"label": "English (GB)",\n"value": "en_GB"},\n{"selected": true,\n"label": "English (US)",\n"value": "en_US"},\n{"selected": false,\n"label": "Español",\n"value": "es_ES"},\n{"selected": false,\n"label": "Eesti",\n"value": "et_EE"},\n{"selected": false,\n"label": "Français",\n"value": "fr_FR"},\n{"selected": false,\n"label": "Greek",\n"value": "el_GR"},\n{"selected": false,\n"label": "Italiano",\n"value": "it_IT"},\n{"selected": false,\n"label": "Japanese",\n"value": "ja_JP"},\n{"selected": false,\n"label": "Korean",\n"value": "ko_KR"},\n{"selected": false,\n"label": "Latviešu",\n"value": "lv_LV"},\n{"selected": false,\n"label": "Lietuviskas",\n"value": "lt_LT"},\n{"selected": false,\n"label": "Magyar",\n"value": "hu_HU"},\n{"selected": false,\n"label": "Nederlands",\n"value": "nl_NL"},\n{"selected": false,\n"label": "Norsk bokmål",\n"value": "nb_NO"},\n{"selected": false,\n"label": "Norsk nynorsk",\n"value": "nn_NO"},\n{"selected": false,\n"label": "Polski",\n"value": "pl_PL"},\n{"selected": false,\n"label": "Português",\n"value": "pt_PT"},\n{"selected": false,\n"label": "Português Brasileiro",\n"value": "pt_BR"},\n{"selected": false,\n"label": "Romana",\n"value": "ro_RO"},\n{"selected": false,\n"label": "Russian (Windows)",\n"value": "ru_RU"},\n{"selected": false,\n"label": "Russian (KOI8-R)",\n"value": "ru_RU.KOI8-R"},\n{"selected": false,\n"label": "Slovak",\n"value": "sk_SK"},\n{"selected": false,\n"label": "Slovenscina",\n"value": "sl_SI"},\n{"selected": false,\n"label": "Suomi",\n"value": "fi_FI"},\n{"selected": false,\n"label": "Svenska",\n"value": "sv_SE"},\n{"selected": false,\n"label": "Ukranian",\n"value": "uk_UA"}]},\n{"type": "submit",\n"name": "button",\n"value": "Log in"}]},\n"version": "0.2.3"}'),

						MochiKit.Base.method(aDirectLogin, 'bindFormFieldWithLabelToRecordFieldWithLabel', 'imapuser', 'R@cine_username'),
						MochiKit.Base.method(aDirectLogin, 'bindFormFieldWithLabelToRecordFieldWithLabel', 'pass', 'R@cine_password'),

						MochiKit.Base.method(aDirectLogin, 'formValues'),
						MochiKit.Base.itemgetter('new_lang'),
						MochiKit.Base.methodcaller('setValue', 'en_US')
						
					], {trace:false});
				},
				MochiKit.Base.partial(MochiKit.Async.succeed, aRecord)
			], {trace:false});
		});
*/

		deferredResult.addCallback(function (aRecord) {
			return Clipperz.Async.callbacks("CardDialogEditing_test.init - setup Kiva", [
				MochiKit.Base.method(aRecord, 'setLabel', "KIVA"),
				MochiKit.Base.method(aRecord, 'addField', {'label':"kiva_email", 'value':"giulio.cesare@solaroli.it", 'isHidden':false}),
				MochiKit.Base.method(aRecord, 'addField', {'label':"kiva_password", 'value':"G0ELE0ngyTZf", 'isHidden':true}),

				MochiKit.Base.method(aRecord, 'createNewDirectLogin'),
				function (aDirectLogin) {
					return Clipperz.Async.callbacks("CardDialogEditing_test.init - setup Kiva [directLogin]", [
						MochiKit.Base.method(aDirectLogin, 'setLabel', 'Kiva'),
						MochiKit.Base.method(aDirectLogin, 'setFavicon', 'http://www.kiva.org/favicon.ico'),
						MochiKit.Base.method(aDirectLogin, 'setBookmarkletConfiguration', '{\n  "page": {\n    "title": "kiva.org"\n  },\n  "form": {\n    "attributes": {\n      "action": "https://www.kiva.org/app.php?page=login&action=doLogin",\n      "method": "post"\n    },\n    "inputs": [\n      {\n        "type": "text",\n        "name": "email",\n        "value": ""\n      },\n      {\n        "type": "password",\n        "name": "password",\n        "value": ""\n      },\n      {\n        "type": "submit",\n        "name": "submit",\n        "value": "Login >>"\n      },\n      {\n        "type": "hidden",\n        "name": "currURL",\n        "value": "https://www.kiva.org/app.php?page=login"\n      }\n    ]\n  },\n  "version": "0.2.3"\n}'),

						MochiKit.Base.method(aDirectLogin, 'bindFormFieldWithLabelToRecordFieldWithLabel', 'email', 'kiva_email'),
						MochiKit.Base.method(aDirectLogin, 'bindFormFieldWithLabelToRecordFieldWithLabel', 'password', 'kiva_password')
					], {trace:false});
				},
				MochiKit.Base.partial(MochiKit.Async.succeed, aRecord)
			], {trace:false});
		});

		deferredResult.addCallback(MochiKit.Base.bind(function (aRecord) {
			cardDialogController = new Clipperz.PM.UI.Web.Controllers.CardDialogController({record:aRecord, delegate:this});
			cardDialogController.run();
		}, this));
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
	
	tester = new Clipperz.Tests.CardDialogEditing.Tester();
	tester.run();
};

MochiKit.DOM.addLoadEvent(init);
