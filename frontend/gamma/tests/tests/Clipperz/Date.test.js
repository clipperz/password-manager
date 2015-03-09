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

var tests = {

	'001_test': function () {
		var	date;
		var locale;
	
//		date = new Date(0);
//		date.setMilliseconds(Date.parse("Fri, 21 Sep 2007 10:04:24 UTC"));

		date = new Date(Date.parse("Fri, 21 Sep 2007 10:04:24 UTC"));
	
		locale = {
			'amDesignation':	"am",
			'pmDesignation':	"pm",
//			'shortDateFormat':	"d/m/y",
//			'longDateFormat':	"",
			'months': 			["January",		"February",	"March",	"April",
								 "May",			"June",		"July",		"August",
								 "September",	"October",	"November",	"December"	],
			'shortMonths':		["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
			'days':				["Sunday",	"Monday",	"Tuesday",	"Wednesday",	"Thursday",	"Friday",	"Saturday"	],
			'shortDays': 		["Sun",		"Mon",		"Tue",		"Wed",			"Thu",		"Fri",		"Sat"		]
		}

		is(Clipperz.Date.formatDateWithPHPLikeTemplateAndLocale(date, "d/m/y", locale), '21/09/07', "formatDate - 1");
		is(Clipperz.Date.formatDateWithPHPLikeTemplateAndLocale(date, "D M Y", locale), 'Fri Sep 2007', "formatDate - 2");
		is(Clipperz.Date.formatDateWithPHPLikeTemplateAndLocale(date, "D, d M Y", locale), 'Fri, 21 Sep 2007', "formatDate - 3");
		is(Clipperz.Date.formatDateWithPHPLikeTemplateAndLocale(date, "D, d M Y H:i:s", locale), 'Fri, 21 Sep 2007 12:04:24', "formatDate - 4");
//		is(Clipperz.Date.formatDateWithPHPLikeTemplateAndLocale(date, "D, d M Y H:i:s (T)", locale), 'Fri, 21 Sep 2007 11:04:24 (CEST)', "formatDate - 5");
	},

    //-------------------------------------------------------------------------
    'syntaxFix': MochiKit.Base.noop
}

//=============================================================================

SimpleTest.runDeferredTests("Clipperz.Date", tests, {trace:false});
