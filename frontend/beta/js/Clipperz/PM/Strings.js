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
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.Strings) == 'undefined') { Clipperz.PM.Strings = {}; }
if (typeof(Clipperz.PM.Strings.Languages) == 'undefined') { Clipperz.PM.Strings.Languages = {}; }

Clipperz.PM.Strings.standardStrings = {

	'loginPanelSwitchLanguageSelectOptions':	[
/*
		{tag:'option', html:"Arabic (Oman) (العربية)", value:'ar-OM', disabled:true},
		{tag:'option', html:"Arabic (Syria) (العربية)", value:'ar-SY', disabled:true},
		{tag:'option', html:"Bahasa Indonesia", value:'id-ID', disabled:true},
		{tag:'option', html:"Bulgarian (Български)", value:'bg-BG', disabled:true},
		{tag:'option', html:"Català", value:'ca-ES', disabled:true},
		{tag:'option', html:"Chinese (Simplified) (简体中文)", value:'zh-CN', disabled:true},
		{tag:'option', html:"Chinese (Traditional) (正體中文)", value:'zh-TW', disabled:true},
		{tag:'option', html:"Czech (Česky)", value:'cs-CZ', disabled:true},
		{tag:'option', html:"Dansk", value:'da-DK', disabled:true},
		{tag:'option', html:"Deutsch", value:'de-DE'/ *, disabled:true* /},
		{tag:'option', html:"English (American)", value:'en-US'/ *, disabled:true* /},
		{tag:'option', html:"English (British)", value:'en-GB'/ *, disabled:true* /},
		{tag:'option', html:"English (Canadian)", value:'en-CA'/ *, disabled:true* /},
		{tag:'option', html:"Español", value:'es-ES', disabled:true},
		{tag:'option', html:"Eesti", value:'et-EE', disabled:true},
		{tag:'option', html:"Français", value:'fr-FR', disabled:true},
		{tag:'option', html:"Galego", value:'gl-ES', disabled:true},
		{tag:'option', html:"Greek (Ελληνικά)", value:'el-GR', disabled:true},
		{tag:'option', html:"Íslenska", value:'is-IS', disabled:true},
		{tag:'option', html:"Italiano", value:'it-IT'/ *, disabled:true* /},
		{tag:'option', html:"Japanese (日本語)", value:'ja-JP', disabled:true},
		{tag:'option', html:"Korean (한국어)", value:'ko-KR', disabled:true},
		{tag:'option', html:"Latviešu", value:'lv-LV', disabled:true},
		{tag:'option', html:"Lietuvių", value:'lt-LT', disabled:true},
		{tag:'option', html:"Macedonian (Македонски)", value:'mk-MK', disabled:true},
		{tag:'option', html:"Magyar", value:'hu-HU', disabled:true},
		{tag:'option', html:"Nederlands", value:'nl-NL', disabled:true},
		{tag:'option', html:"Norsk bokmål", value:'nb-NO', disabled:true},
		{tag:'option', html:"Norsk nynorsk", value:'nn-NO', disabled:true},
		{tag:'option', html:"Persian (Western) (فارسى)", value:'fa-IR', disabled:true},
		{tag:'option', html:"Polski", value:'pl-PL', disabled:true},
		{tag:'option', html:"Português", value:'pt-PT'/ *, disabled:true* /},
		{tag:'option', html:"Português Brasileiro", value:'pt-BR'/ *, disabled:true* /},
		{tag:'option', html:"Românä", value:'ro-RO', disabled:true},
		{tag:'option', html:"Russian (Русский)", value:'ru-RU', disabled:true},
		{tag:'option', html:"Slovak (Slovenčina)", value:'sk-SK', disabled:true},
		{tag:'option', html:"Slovenian (Slovenščina)", value:'sl-SI', disabled:true},
		{tag:'option', html:"Suomi", value:'fi-FI', disabled:true},
		{tag:'option', html:"Svenska", value:'sv-SE', disabled:true},
		{tag:'option', html:"Thai (ไทย)", value:'th-TH', disabled:true},
		{tag:'option', html:"Türkçe", value:'tr-TR', disabled:true},
		{tag:'option', html:"Ukrainian (Українська)", value:'uk-UA', disabled:true}
*/
		{tag:'option', html:"Arabic (العربية)", value:"ar", disabled:true, cls:'disabledOption'},
//		{tag:'option', html:"Chinese (中文)", value:"zh", disabled:true},
		{tag:'option', html:"Chinese (Simplified) (简体中文)", value:'zh-CN'},
		{tag:'option', html:"Dutch (Nederlands)", value:"nl-NL", disabled:true, cls:'disabledOption'},
		{tag:'option', html:"English", value:"en-US"},
		{tag:'option', html:"French (Français)", value:"fr-FR"},
		{tag:'option', html:"German (Deutsch)", value:"de-DE"/* -- */, disabled:true, cls:'disabledOption' /* */},
		{tag:'option', html:"Greek (Ελληνικά)", value:"el-GR"/* -- */, disabled:true, cls:'disabledOption' /* */},
		{tag:'option', html:"Hebrew (עברית)", value:"he-IL"/* -- */, disabled:true, cls:'disabledOption' /* */},
		{tag:'option', html:"Italian (Italiano)", value:"it-IT"},
		{tag:'option', html:"Japanese (日本語)", value:"ja-JP"},
		{tag:'option', html:"Korean (한국어)", value:"ko-KR", disabled:true, cls:'disabledOption'},
		{tag:'option', html:"Norwegian (Norsk)", value:"no", disabled:true, cls:'disabledOption'},
		{tag:'option', html:"Persian (فارسی)", value:"fa-IR", disabled:true, cls:'disabledOption'},
		{tag:'option', html:"Polish (Polski)", value:"pl-PL", disabled:true, cls:'disabledOption'},
		{tag:'option', html:"Portuguese (Português)", value:"pt-BR"},
		{tag:'option', html:"Russian (Русский)", value:"ru-RU"},
		{tag:'option', html:"Spanish (Español)", value:"es-ES"},
		{tag:'option', html:"Swedish (Svenska)", value:"sv-SE", disabled:true, cls:'disabledOption'},
		{tag:'option', html:"Turkish (Türkçe)", value:"tr-TR", disabled:true, cls:'disabledOption'},
		{tag:'option', html:"Vietnamese (Tiếng Việt)", value:"vi-VN", disabled:true, cls:'disabledOption'}
	]
}

Clipperz.PM.Strings.GeneralSettings = {
	'en-us': {
		'loginFormAarghThatsBadUrl':	"http://www.clipperz.com/support/faq/account_faq",
		'loginFormVerifyTheCodeUrl':	"http://www.clipperz.com/learn_more/reviewing_the_code",

		'donateHeaderLinkUrl':			"http://www.clipperz.com/donations",
		'creditsHeaderLinkUrl':			"http://www.clipperz.com/credits",
		'feedbackHeaderLinkUrl':		"http://www.clipperz.com/contact",
		'helpHeaderLinkUrl':			"http://www.clipperz.com/support/user_guide",
		'forumHeaderLinkUrl':			"http://www.clipperz.com/forum",

		'httpAuthBookmarkletConfiguration':	{tag:'textarea', id:'httpAuthDefaultConfiguration', html:"" +
													"{ \"page\":{\"title\":\"HTTP authentication\"}," + "\n" +
													"  \"form\":{\"attributes\": {" + "\n" +
													"    \"action\":\"\"," + "\n" +
													"    \"type\":\"http_auth\"" + "\n" +
													"  }, \"inputs\": [" + "\n" +
													"    {\"type\":\"text\",\"name\":\"url\",\"value\":\"\"}," + "\n" +
													"    {\"type\":\"text\",\"name\":\"username\",\"value\":\"\"}," + "\n" +
													"    {\"type\":\"password\",\"name\":\"password\",\"value\":\"\"}" + "\n" +
													"  ]}, \"version\":\"0.2.3\"}"
											},

		'directLoginJumpPageUrl':	"",
		'defaultFaviconUrl':		"data:application/octet-stream;charset=utf-8;base64,AAABAAEAFxcAAAEAGAD8BgAAFgAAACgAAAAXAAAALgAAAAEAGAAAAAAAAAAAABIXAAASFwAAAAAAAAAAAAD///////////////////////////////////////////////////////////////////////////////////////////9zAC////////////////////////////////////////////////////////////////////////////////////////////9pAG////////////////////////////////////////////////////////////////////////////////////////////9rAC////////////////////////////////////////////////////////////////////////////////////////////9yAHP////////////////////////IyMizs7O6urrq6ur////////////Ozs6zs7Ozs7Pq6ur///////////////////////8AAAD////////////////////V1dWXl5eXl5eXl5elpaX4+Pj////Ozs6Xl5eXl5eXl5eenp7///////////////////////8AAAD////////////////////Ozs6Xl5eXl5eXl5eXl5fBwcHq6uqenp6Xl5eXl5eXl5eXl5f///////////////////////8AAAD////////////////////j4+OXl5eXl5eXl5eXl5eXl5elpaWXl5eXl5eXl5eXl5ezs7P///////////////////////8AAAD////////////////////////IyMiXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eenp7x8fH////////////////////////////////////////////////////4+PilpaWXl5eXl5eXl5eXl5eXl5eXl5eXl5fOzs7////////////////////////////////////////////////////////q6uq6urqXl5eXl5eXl5eXl5eXl5eXl5eenp7V1dX4+Pj///////////////////////8AAAD////////////4+PjOzs6lpaWXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5e6urrj4+P///////////////8AAAD////////////BwcGXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5fx8fH///////////8AAAD///////////+zs7OXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5fj4+P///////////8AAAD////////////IyMiXl5eXl5eXl5eXl5e6urqXl5eXl5eXl5eXl5esrKylpaWXl5eXl5eXl5eenp7x8fH///////////8AAAD////////////////Ozs7Ozs7V1dX4+Pj///+Xl5eXl5eXl5eXl5fOzs7////q6urOzs7Ozs7q6ur///////////////8AAAD///////////////////////////////////+Xl5eXl5eXl5eXl5fOzs7///////////////////////////////////8AAAD///////////////////////////////////+Xl5eXl5eXl5eXl5fOzs7///////////////////////////////////8AAAD///////////////////////////////////+Xl5eXl5eXl5eXl5fOzs7///////////////////////////////////8AAAD////////////////////////////////////IyMiXl5eXl5eenp7x8fH///////////////////////////////////8AAAD////////////////////////////////////////j4+Pj4+Px8fH///////////////////////////////////////8AAAD///////////////////////////////////////////////////////////////////////////////////////////8AAAD///////////////////////////////////////////////////////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAo=",
		'defaultFaviconUrl_IE':		"https://www.clipperz.com/images/icons/misc/favicon.ico",

		'donateSplashPanelIconUrl':	"./images/smiles_big.gif",

		'icons_baseUrl':	"https://www.clipperz.com/images/icons",

		'passwordGeneratorLowercaseCharset':	"abcdefghijklmnopqrstuvwxyz",
		'passwordGeneratorUppercaseCharset':	"ABCDEFGHIJKLMNOPQRSTUVWXYZ",
		'passwordGeneratorNumberCharset':		"0123456789",
		'passwordGeneratorSymbolCharset':		"!@#$%^&*+?[]{}/|\\<>,.;:~=-_",

		'_': ""
	}
}

Clipperz.PM.Strings.defaultLanguages = {
	'default':	"en-us",

//	'de':	"de-de",
//	'el':	"el-gr",
//	'he':	"he-il",
//	'ru':	"ru-ru",
	
	
	'fr':	"fr-fr", 
	'es':	"es-es", 
	'zh':	"zh-cn", 
	'ja':	"ja-jp", 
	'pt':	"pt-br", 
	'it':	"it-it",
	'en': 	"en-us"
}

Clipperz.PM.Strings.inputTypeToRecordFieldType = {
	'text': 'TXT',
	'password': 'PWD',
	'checkbox': 'CHECK',
	'radio': 'RADIO',
	'select': 'SELECT'
};

Clipperz.PM.Strings.Languages.setSelectedLanguage = function(aLanguage) {
	var language;
	var	selectedLanguage;
	
	language = (aLanguage || Clipperz.PM.Strings.preferredLanguage || 'default').toLowerCase();
//MochiKit.Logging.logDebug("1 - language: " + language);
	if (typeof(Clipperz.PM.Strings.defaultLanguages[language]) != 'undefined') {
		language = Clipperz.PM.Strings.defaultLanguages[language];
//MochiKit.Logging.logDebug("2 - language: " + language);
	}

	if (typeof(Clipperz.PM.Strings.Languages[language]) != 'undefined') {
		selectedLanguage = language;
//MochiKit.Logging.logDebug("### selectedLanguage full match: " + selectedLanguage);
	} else if (typeof(Clipperz.PM.Strings.defaultLanguages[language.substr(0,2)]) != 'undefined') {
		selectedLanguage = Clipperz.PM.Strings.defaultLanguages[language.substr(0,2)];
//MochiKit.Logging.logDebug("### selectedLanguage partial match: " + selectedLanguage);
	} else {
		selectedLanguage = Clipperz.PM.Strings.defaultLanguages['default'];
//MochiKit.Logging.logDebug("### selectedLanguage default match: " + selectedLanguage);
	}
	
//MochiKit.Logging.logDebug("### selectedLanguage: " + selectedLanguage);
	if (selectedLanguage != Clipperz.PM.Strings.selectedLanguage) {
//MochiKit.Logging.logDebug(">>> setting Clipperz.PM.Strings.selectedLanguage: " + selectedLanguage);
		Clipperz.PM.Strings.selectedLanguage = selectedLanguage;

		MochiKit.Base.update(Clipperz.PM.Strings, Clipperz.PM.Strings.standardStrings)
//MochiKit.Logging.logDebug("=== 1: " + Clipperz.PM.Strings['bookmarkletTabInstructions']);
		MochiKit.Base.updatetree(Clipperz.PM.Strings, Clipperz.PM.Strings.Languages[Clipperz.PM.Strings.defaultLanguages['default']]);
//MochiKit.Logging.logDebug("=== 2: " + Clipperz.PM.Strings['bookmarkletTabInstructions']);
		MochiKit.Base.updatetree(Clipperz.PM.Strings, Clipperz.PM.Strings.GeneralSettings[Clipperz.PM.Strings.defaultLanguages['default']]);
//MochiKit.Logging.logDebug("=== 3: " + Clipperz.PM.Strings['bookmarkletTabInstructions']);
		MochiKit.Base.updatetree(Clipperz.PM.Strings, Clipperz.PM.Strings.Languages[selectedLanguage]);
//MochiKit.Logging.logDebug("=== 4: " + Clipperz.PM.Strings['bookmarkletTabInstructions']);
		MochiKit.Base.updatetree(Clipperz.PM.Strings, Clipperz.PM.Strings.GeneralSettings[selectedLanguage]);
//MochiKit.Logging.logDebug("=== 5: " + Clipperz.PM.Strings['bookmarkletTabInstructions']);

		Clipperz.NotificationCenter.notify(Clipperz.PM.Strings.Languages, 'switchLanguage', selectedLanguage);
//MochiKit.Logging.logDebug("<<< setting Clipperz.PM.Strings.selectedLanguage. Done");
	}
}

Clipperz.PM.Strings.Languages.initSetup = function() {
	var	language;
	var	languageParser;

	language = navigator.language || navigator.userLanguage;	//	en, en-US, .... "de", "nb-no"
	languageParser = new RegExp("language=([a-z]{2}(?:\-[a-z]{2})?)(\&|$)", "i");
	if (languageParser.test(window.location.search)) {
//MochiKit.Logging.logDebug("LANGUAGE specified in the query string");
		language = RegExp.$1;
	}

//MochiKit.Logging.logDebug("+++ preferredLanguage: " + language);
	Clipperz.PM.Strings.preferredLanguage = language.toLowerCase();
//MochiKit.Logging.logDebug("## preferredLanguage: " + Clipperz.PM.Strings.preferredLanguage);
	Clipperz.PM.Strings.Languages.setSelectedLanguage(Clipperz.PM.Strings.preferredLanguage);
}

//MochiKit.DOM.addLoadEvent(Clipperz.PM.Strings.Languages.initSetup);
