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
if (typeof(Clipperz.PM.Date) == 'undefined') { Clipperz.PM.Date = {}; }

Clipperz.PM.Date.VERSION = "0.1";
Clipperz.PM.Date.NAME = "Clipperz.PM.Date";

MochiKit.Base.update(Clipperz.PM.Date, {

	'__repr__': function () {
		return "[" + this.NAME + " " + this.VERSION + "]";
	},

	//-------------------------------------------------------------------------

	'toString': function () {
		return this.__repr__();
	},

	//-------------------------------------------------------------------------

	'locale': function() {
		return {
			'amDesignation':	Clipperz.PM.Strings['calendarStrings']['amDesignation'],
			'pmDesignation':	Clipperz.PM.Strings['calendarStrings']['pmDesignation'],
			'days':				Clipperz.PM.Strings['calendarStrings']['days'],
			'shortDays':		Clipperz.PM.Strings['calendarStrings']['shortDays'],
			'shortMonths':		Clipperz.PM.Strings['calendarStrings']['shortMonths'],
			'months':			Clipperz.PM.Strings['calendarStrings']['months']
		}
	},
	
	//=========================================================================
/*
	'formatDateWithPHPLikeTemplate': function(aDate, aTemplate) {
		return Clipperz.Date.formatDateWithPHPLikeTemplateAndLocale(aDate, aTemplate, Clipperz.PM.Date.locale());
	},

	'parseDateWithPHPLikeTemplate': function(aDate, aTemplate) {
		return Clipperz.Date.parseDateWithPHPTemplateAndLocale(aDate, aTemplate, Clipperz.PM.Date.locale());
	},

	//=========================================================================

	'formatDateWithJavaLikeTemplate': function(aDate, aTemplate) {
		return Clipperz.Date.formatDateWithJavaLikeTemplateAndLocale(aDate, aTemplate, Clipperz.PM.Date.locale());
	},

	'parseDateWithJavaLikeTemplate': function(aDate, aTemplate) {
		return Clipperz.Date.parseDateWithJavaLikeTemplateAndLocale(aDate, aTemplate, Clipperz.PM.Date.locale());
	},
*/
	//=========================================================================

	'formatDateWithTemplate': function(aDate, aTemplate) {
		var result;

		if (aDate == null) {
			result = ""
		} else {
		 	result = Clipperz.Date.formatDateWithPHPLikeTemplateAndLocale(aDate, aTemplate, Clipperz.PM.Date.locale());
		};
		
		return result;
	},

	'parseDateWithTemplate': function(aValue, aTemplate) {
		return Clipperz.Date.parseDateWithPHPTemplateAndLocale(aValue, aTemplate, Clipperz.PM.Date.locale());
	},

	//=========================================================================

	'formatDateWithUTCFormat': function(aDate) {
		return Clipperz.Date.formatDateWithUTCFormatAndLocale(aDate,  Clipperz.PM.Date.locale());
	},
	
	'parseDateWithUTCFormat': function(aValue) {
		var result;
		
		if (aValue == null) {
			result = null;
		} else {
		 	result = Clipperz.Date.parseDateWithUTCFormatAndLocale(aValue,  Clipperz.PM.Date.locale());
		}
		
		return result;
	},

	//=========================================================================

	'getElapsedTimeDescription': function(aDate) {
		var result;

		result = ""
		
		if (aDate != null) {
			var now;
			var elapsedTime;
		
			var millisencondsInAMinute;
			var millisencondsInAnHour;
			var millisencondsInADay;
			var millisencondsInAWeek;
			var millisencondsInAMonth;

			now = new Date();
			elapsedTime = now.getTime() - aDate.getTime();
		
			millisencondsInAMinute = 60 * 1000;
			millisencondsInAnHour = millisencondsInAMinute * 60;
			millisencondsInADay = millisencondsInAnHour * 24;
			millisencondsInAWeek = millisencondsInADay * 7;
			millisencondsInAMonth = millisencondsInAWeek * 5;

			if ((elapsedTime / millisencondsInAMonth) > 1) {
				result = Clipperz.PM.Strings['elapsedTimeDescriptions']['MORE_THAN_A_MONTH_AGO'];
			} else if ((elapsedTime / millisencondsInAWeek) > 1) {
				var elapsedWeeks;
			
				elapsedWeeks = Math.floor((elapsedTime / millisencondsInAWeek));
				if (elapsedWeeks == 1) {
					result = Clipperz.PM.Strings['elapsedTimeDescriptions']['MORE_THAN_A_WEEK_AGO'];
				} else {
					result = Clipprez.PM.Strings['elapsedTimeDescriptions']['MORE_THAN_*_WEEKS_AGO'].replace(/__elapsed__/, elapsedWeeks);
				}
			} else if ((elapsedTime / millisencondsInADay) > 1) {
				var elapsedDays;
			
				elapsedDays = Math.floor((elapsedTime / millisencondsInADay));
				if (elapsedDays == 1) {
					result = Clipperz.PM.Strings['elapsedTimeDescriptions']['YESTERDAY'];
				} else {
					result = Clipperz.PM.Strings['elapsedTimeDescriptions']['*_DAYS_AGO'].replace(/__elapsed__/, elapsedDays);
				}
			} else if ((elapsedTime / millisencondsInAnHour) > 1) {
				var elapsedHours;
			
				elapsedHours = Math.floor((elapsedTime / millisencondsInAnHour));
				if (elapsedHours == 1) {
					result = Clipperz.PM.Strings['elapsedTimeDescriptions']['ABOUT_AN_HOUR_AGO'];
				} else {
					result = Clipperz.PM.Strings['elapsedTimeDescriptions']['*_HOURS_AGO'].replace(/__elapsed__/, elapsedHours);
				}
			} else {
				var elapsed10Minutes;
			
				elapsed10Minutes = (Math.floor((elapsedTime / millisencondsInAMinute) / 10)) * 10;
				if (elapsed10Minutes == 0) {
					result = Clipperz.PM.Strings['elapsedTimeDescriptions']['JUST_A_FEW_MINUTES_AGO'];
				} else {
					result = Clipperz.PM.Strings['elapsedTimeDescriptions']['ABOUT_*_MINUTES_AGO'].replace(/__elapsed__/, elapsed10Minutes+"");
				}
			}
		}
		
		return result;
	},
	
	//-------------------------------------------------------------------------

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});

