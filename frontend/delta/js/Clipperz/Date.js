/*

Copyright 2008-2018 Clipperz Srl

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

"use strict";
Clipperz.Base.module('Clipperz.Date');

Clipperz.Date.VERSION = "0.1";
Clipperz.Date.NAME = "Clipperz.Date";

MochiKit.Base.update(Clipperz.Date, {

	//-------------------------------------------------------------------------

	'__repr__': function () {
		return "[" + this.NAME + " " + this.VERSION + "]";
	},

	//-------------------------------------------------------------------------

	'toString': function () {
		return this.__repr__();
	},

	//-------------------------------------------------------------------------

	'daysInMonth': [31,28,31,30,31,30,31,31,30,31,30,31],

	//-------------------------------------------------------------------------

	'englishOrdinalDaySuffixForDate': function(aDate) {
		var result;
		
		switch (aDate.getDate()) {
			case 1:
			case 21:
			case 31:
				result =  "st";
				break;
			case 2:
			case 22:
				result = "nd";
				break;
			case 3:
			case 23:
				result = "rd";
				break;
			default:
				result = "th";
				break;
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'isLeapYear': function(aDate) {
		var year;
		var result;
		
		year = aDate.getFullYear();
		result = ((year & 0x03) == 0 && (year % 100 || (year % 400 == 0 && year)));
		
		return result;
	},

	//-------------------------------------------------------------------------

	'getDaysInMonth': function(aDate) {
		var result;
		
		if (aDate.getMonth() == 1) {
			Clipperz.Date.isLeapYear(aDate)
			result += Clipperz.Date.isLeapYear(aDate) ? 29 : 28;
		} else {
			result = Clipperz.Date.daysInMonth[aDate.getMonth()];
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'getTimezone': function(aDate) {
		var result;

		result = aDate.toString();
		result = result.replace(/([A-Z]{3}) [0-9]{4}/, '$1');
		result = result.replace(/^.*?\(([A-Z])[a-z]+ ([A-Z])[a-z]+ ([A-Z])[a-z]+\)$/, "$1$2$3");
		
		return result;
	},

	'getGMTOffset': function(aDate) {
		return (aDate.getTimezoneOffset() > 0 ? "-" : "+")	+ MochiKit.Format.numberFormatter('00')(Math.floor(this.getTimezoneOffset() / 60))
															+ MochiKit.Format.numberFormatter('00')(this.getTimezoneOffset() % 60);
	},

	//-------------------------------------------------------------------------
	
	'dayOfYear': function(aDate) {
		var result;
		var i,c;
		
		result = 0;
		c = aDate.getMonth();
		for (i=0; i<c; i++) {
			if (i == 1) {
				result += Clipperz.Date.isLeapYear(aDate) ? 29 : 28;
			} else {
				result += Clipperz.Date.daysInMonth[i];
			}
		}
		return num + this.getDate() - 1;
	},

	//-------------------------------------------------------------------------

	'getPHPLikeFormatCode': function(aCharacter) {
		var result;

		switch (aCharacter) {
			case "d":
				result = " + MochiKit.Format.numberFormatter('00')(aDate.getDate())";
				break;
			case "D":
				result = " + aLocale['shortDays'][aDate.getDay()]";
				break;
			case "j":
				result = " + aDate.getDate()";
				break;
			case "l":
				result = " + aLocale['days'][aDate.getDay()]";
				break;
			case "S":
				result = " + Clipperz.Date.englishOrdinalDaySuffixForDate(aDate)";
				break;
			case "w":
				result = " + aDate.getDay()";
				break;
			case "z":
				result = " + aDate.getDayOfYear()";
				break;
			case "W":
				result = " + aDate.getWeekOfYear()";
				break;
			case "F":
				result = " + aLocale['months'][aDate.getMonth()]";
				break;
			case "m":
				result = " + MochiKit.Format.numberFormatter('00')(aDate.getMonth() + 1)";
				break;
			case "M":
				result = " + aLocale['shortMonths'][aDate.getMonth()]";
				break;
			case "n":
				result = " + (aDate.getMonth() + 1)";
				break;
			case "t":
				result = " + Clipperz.Date.getDaysInMonth(aDate)";
				break;
			case "L":
				result = " + (Clipperz.Date.isLeapYear(aDate) ? 1 : 0)";
				break;
			case "Y":
				result = " + aDate.getFullYear()";
				break;
			case "y":
				result = " + ('' + aDate.getFullYear()).substring(2, 4)";
				break;
			case "a":
				result = " + (aDate.getHours() < 12 ? aLocale['amDesignation'] : aLocale['pmDesignation'])";
				break;
			case "A":
				result = " + (aDate.getHours() < 12 ? aLocale['amDesignation'].toUpperCase() : aLocale['pmDesignation'].toUpperCase())";
				break;
			case "g":
				result = " + ((aDate.getHours() %12) ? aDate.getHours() % 12 : 12)";
				break;
			case "G":
				result = " + aDate.getHours()";
				break;
			case "h":
				result = " + MochiKit.Format.numberFormatter('00')((aDate.getHours() %12) ? aDate.getHours() % 12 : 12)";
				break;
			case "H":
				result = " + MochiKit.Format.numberFormatter('00')(aDate.getHours())";
				break;
			case "i":
				result = " + MochiKit.Format.numberFormatter('00')(aDate.getMinutes())";
				break;
			case "s":
				result = " + MochiKit.Format.numberFormatter('00')(aDate.getSeconds())";
				break;
			case "O":
				result = " + aDate.getGMTOffset()";
				break;
			case "T":
				result = " + Clipperz.Date.getTimezone(aDate)";
				break;
			case "Z":
				result = " + ( + aDate.getTimezoneOffset() * -60)";
				break;
			default:
				result = " + '" + aCharacter + "'";
				break;
		};
		
		return result;
	},

	//=========================================================================

	'formatDateWithPHPLikeTemplateAndLocale': function(aDate, aFormat, aLocale) {
		var result;
		var formatterCode;
		var formatter;
		var i,c;
		
		formatterCode = "Clipperz.Date.__scratchFormatter = function(aDate, aLocale){return ''";

		c = aFormat.length;
		i = 0;
		
		while (i<c) {
	    	var character;
	
	        character = aFormat.charAt(i);
			if (character == "\\") {
				i++;
				character = aFormat.charAt(i);
				formatterCode += " + '" + character + "'"
			} else {
	            formatterCode += Clipperz.Date.getPHPLikeFormatCode(character);
			}
			
			i++;
		}
		
		formatterCode += ";}";
		eval(formatterCode);
		
		result = Clipperz.Date.__scratchFormatter.call(this, aDate, aLocale);
		delete Clipperz.Date.__scratchFormatter;

		return result;
	},

	//-------------------------------------------------------------------------
	
	'parseDateWithPHPLikeTemplateAndLocale': function(aString, aFormat, aLocale) {
		return new Date();
	},

	//=========================================================================
	
	'formatDateWithUTCFormatAndLocale': function(aDate, aLocale) {
//		return Clipperz.Date.formatWithJavaLikeTemplateAndLocale(aDate, "EEE, dd MMMM yyyy HH:mm:ss zzz", aLocale);
		return aDate.toString();
	},
	
	'parseDateWithUTCFormatAndLocale': function(aValue, aLocale) {
		return new Date(Date.parse(aValue));
	},

	//=========================================================================
	
	'exception': {
//		'AbstractMethod': new MochiKit.Base.NamedError("Clipperz.Base.exception.AbstractMethod"),
//		'UnknownType':    new MochiKit.Base.NamedError("Clipperz.Base.exception.UnknownType") 
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});

