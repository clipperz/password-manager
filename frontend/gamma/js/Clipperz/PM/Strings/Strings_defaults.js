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

//=============================================================================
//
//		D E F A U L T S  ( defaults )
//
//=============================================================================

Clipperz.PM.Strings.Languages['defaults'] = {

'elapsedTimeDescriptions': {
	'MORE_THAN_A_MONTH_AGO':		"more than a month ago",
	'MORE_THAN_A_WEEK_AGO':			"more than a week ago",
	'MORE_THAN_*_WEEKS_AGO':		"more than __elapsed__ weeks ago",
	'YESTERDAY':					"yesterday",
	'*_DAYS_AGO':					"__elapsed__ days ago",
	'ABOUT_AN_HOUR_AGO':			"about an hour ago",
	'*_HOURS_AGO':					"__elapsed__ hours ago",
	'JUST_A_FEW_MINUTES_AGO':		"just a few minutes ago",
	'ABOUT_*_MINUTES_AGO':			"about __elapsed__ minutes ago"
},
/*
'unknown_ip':						"unknown",
	
'countries': {
	'--':			"unknown",
	'AD':			"Andorra",
	'AE':			"United Arab Emirates",
	'AF':			"Afghanistan",
	'AG':			"Antigua and Barbuda",
	'AI':			"Anguilla",
	'AL':			"Albania",
	'AM':			"Armenia",
	'AN':			"Netherlands Antilles",
	'AO':			"Angola",
	'AP':			"Non-Spec Asia Pas Location",
	'AR':			"Argentina",
	'AS':			"American Samoa",
	'AT':			"Austria",
	'AU':			"Australia",
	'AW':			"Aruba",
	'AX':			"Aland Islands",
	'AZ':			"Azerbaijan",
	'BA':			"Bosnia and Herzegowina",
	'BB':			"Barbados",
	'BD':			"Bangladesh",
	'BE':			"Belgium",
	'BF':			"Burkina Faso",
	'BG':			"Bulgaria",
	'BH':			"Bahrain",
	'BI':			"Burundi",
	'BJ':			"Benin",
	'BM':			"Bermuda",
	'BN':			"Brunei Darussalam",
	'BO':			"Bolivia",
	'BR':			"Brazil",
	'BS':			"Bahamas",
	'BT':			"Bhutan",
	'BW':			"Botswana",
	'BY':			"Belarus",
	'BZ':			"Belize",
	'CA':			"Canada",
	'CD':			"Congo the Democratic Republic of the",
	'CF':			"Central African Republic",
	'CH':			"Switzerland",
	'CI':			"Cote D'ivoire",
	'CK':			"Cook Islands",
	'CL':			"Chile",
	'CM':			"Cameroon",
	'CN':			"China",
	'CO':			"Colombia",
	'CR':			"Costa Rica",
	'CS':			"Serbia and Montenegro",
	'CU':			"Cuba",
	'CY':			"Cyprus",
	'CZ':			"Czech Republic",
	'DE':			"Germany",
	'DJ':			"Djibouti",
	'DK':			"Denmark",
	'DO':			"Dominican Republic",
	'DZ':			"Algeria",
	'EC':			"Ecuador",
	'EE':			"Estonia",
	'EG':			"Egypt",
	'ER':			"Eritrea",
	'ES':			"Spain",
	'ET':			"Ethiopia",
	'EU':			"European Union",
	'FI':			"Finland",
	'FJ':			"Fiji",
	'FM':			"Micronesia Federated States of",
	'FO':			"Faroe Islands",
	'FR':			"France",
	'GA':			"Gabon",
	'GB':			"United Kingdom",
	'GD':			"Grenada",
	'GE':			"Georgia",
	'GF':			"French Guiana",
	'GG':			"Guernsey",
	'GH':			"Ghana",
	'GI':			"Gibraltar",
	'GL':			"Greenland",
	'GM':			"Gambia",
	'GP':			"Guadeloupe",
	'GR':			"Greece",
	'GT':			"Guatemala",
	'GU':			"Guam",
	'GW':			"Guinea-Bissau",
	'GY':			"Guyana",
	'HK':			"Hong Kong",
	'HN':			"Honduras",
	'HR':			"Croatia (Local Name: Hrvatska)",
	'HT':			"Haiti",
	'HU':			"Hungary",
	'ID':			"Indonesia",
	'IE':			"Ireland",
	'IL':			"Israel",
	'IM':			"Isle of Man",
	'IN':			"India",
	'IO':			"British Indian Ocean Territory",
	'IQ':			"Iraq",
	'IR':			"Iran (Islamic Republic of)",
	'IS':			"Iceland",
	'IT':			"Italy",
	'JE':			"Jersey",
	'JM':			"Jamaica",
	'JO':			"Jordan",
	'JP':			"Japan",
	'KE':			"Kenya",
	'KG':			"Kyrgyzstan",
	'KH':			"Cambodia",
	'KI':			"Kiribati",
	'KN':			"Saint Kitts and Nevis",
	'KR':			"Korea Republic of",
	'KW':			"Kuwait",
	'KY':			"Cayman Islands",
	'KZ':			"Kazakhstan",
	'LA':			"Lao People's Democratic Republic",
	'LB':			"Lebanon",
	'LC':			"Saint Lucia",
	'LI':			"Liechtenstein",
	'LK':			"Sri Lanka",
	'LR':			"Liberia",
	'LS':			"Lesotho",
	'LT':			"Lithuania",
	'LU':			"Luxembourg",
	'LV':			"Latvia",
	'LY':			"Libyan Arab Jamahiriya",
	'MA':			"Morocco",
	'MC':			"Monaco",
	'MD':			"Moldova Republic of",
	'MG':			"Madagascar",
	'MH':			"Marshall Islands",
	'MK':			"Macedonia the Former Yugoslav Republic of",
	'ML':			"Mali",
	'MM':			"Myanmar",
	'MN':			"Mongolia",
	'MO':			"Macau",
	'MP':			"Northern Mariana Islands",
	'MR':			"Mauritania",
	'MS':			"Montserrat",
	'MT':			"Malta",
	'MU':			"Mauritius",
	'MV':			"Maldives",
	'MW':			"Malawi",
	'MX':			"Mexico",
	'MY':			"Malaysia",
	'MZ':			"Mozambique",
	'NA':			"Namibia",
	'NC':			"New Caledonia",
	'NF':			"Norfolk Island",
	'NG':			"Nigeria",
	'NI':			"Nicaragua",
	'NL':			"Netherlands",
	'NO':			"Norway",
	'NP':			"Nepal",
	'NR':			"Nauru",
	'NU':			"Niue",
	'NZ':			"New Zealand",
	'OM':			"Oman",
	'PA':			"Panama",
	'PE':			"Peru",
	'PF':			"French Polynesia",
	'PG':			"Papua New Guinea",
	'PH':			"Philippines",
	'PK':			"Pakistan",
	'PL':			"Poland",
	'PR':			"Puerto Rico",
	'PS':			"Palestinian Territory Occupied",
	'PT':			"Portugal",
	'PW':			"Palau",
	'PY':			"Paraguay",
	'QA':			"Qatar",
	'RO':			"Romania",
	'RS':			"Serbia",
	'RU':			"Russian Federation",
	'RW':			"Rwanda",
	'SA':			"Saudi Arabia",
	'SB':			"Solomon Islands",
	'SC':			"Seychelles",
	'SD':			"Sudan",
	'SE':			"Sweden",
	'SG':			"Singapore",
	'SI':			"Slovenia",
	'SK':			"Slovakia (Slovak Republic)",
	'SL':			"Sierra Leone",
	'SM':			"San Marino",
	'SN':			"Senegal",
	'SR':			"Suriname",
	'SV':			"El Salvador",
	'SY':			"Syrian Arab Republic",
	'SZ':			"Swaziland",
	'TC':			"Turks and Caicos Islands",
	'TG':			"Togo",
	'TH':			"Thailand",
	'TJ':			"Tajikistan",
	'TM':			"Turkmenistan",
	'TN':			"Tunisia",
	'TO':			"Tonga",
	'TR':			"Turkey",
	'TT':			"Trinidad and Tobago",
	'TV':			"Tuvalu",
	'TW':			"Taiwan Province of China",
	'TZ':			"Tanzania United Republic of",
	'UA':			"Ukraine",
	'UG':			"Uganda",
	'US':			"United States",
	'UY':			"Uruguay",
	'UZ':			"Uzbekistan",
	'VA':			"Holy See (Vatican City State)",
	'VE':			"Venezuela",
	'VG':			"Virgin Islands (British)",
	'VI':			"Virgin Islands (U.S.)",
	'VN':			"Viet Nam",
	'VU':			"Vanuatu",
	'WF':			"Wallis and Futuna Islands",
	'WS':			"Samoa",
	'YE':			"Yemen",
	'ZA':			"South Africa",
	'ZM':			"Zambia",
	'ZW':			"Zimbabwe",
	'ZZ':			"Reserved"
},

'browsers': {
	'UNKNOWN':		"Unknown",
	'MSIE':			"Internet Explorer",
	'FIREFOX':		"Firefox",
	'OPERA':		"Opera",
	'SAFARI':		"Safari",
	'OMNIWEB':		"OmniWeb",
	'CAMINO':		"Camino",
	'CHROME':		"Chrome"
},

'operatingSystems': {
	'UNKNOWN':		"Unknown",
	'WINDOWS':		"Windows",
	'MAC':			"Mac",
	'LINUX':		"Linux",
	'IPHONE':		"iPhone",
	'MOBILE':		"Mobile",
	'OPENBSD':		"OpenBSD",
	'FREEBSD':		"FreeBSD",
	'NETBSD':		"NetBSD"
},
*/
	
//	Calendar texts
'calendarStrings': {
	'months': 	{
		'0':	"January",
		'1':	"February",
		'2':	"March",
		'3':	"April",
		'4':	"May",
		'5':	"June",
		'6':	"July",
		'7':	"August",
		'8':	"September",
		'9':	"October",
		'10':	"November",
		'11':	"December"
	},
	'shortMonths':	{
		'0':	"Jan",
		'1':	"Feb",
		'2':	"Mar",
		'3':	"Apr",
		'4':	"May",
		'5':	"Jun",
		'6':	"Jul",
		'7':	"Aug",
		'8':	"Sep",
		'9':	"Oct",
		'10':	"Nov",
		'11':	"Dec"
	},

	'days':	{
		'0':	"Sunday",
		'1':	"Monday",
		'2':	"Tuesday",
		'3':	"Wednesday",
		'4':	"Thursday",
		'5':	"Friday",
		'6':	"Saturday"
	},

	'shortDays':	{
		'0':	"Sun",
		'1':	"Mon",
		'2':	"Tue",
		'3':	"Wed",
		'4':	"Thu",
		'5':	"Fri",
		'6':	"Sat"
	},

	'veryShortDays':	{
		'0':	"Su",
		'1':	"Mo",
		'2':	"Tu",
		'3':	"We",
		'4':	"Th",
		'5':	"Fr",
		'6':	"Sa"
	},

	'amDesignation':	"am",
	'pmDesignation':	"pm"

},

// Date format
'fullDate_format':	"l, F d, Y H:i:s",

//################################################################################

'pageHeader': {
	'donation':	"donate",
	'forum':	"forum",
	'credits':	"credits",
	'feedback':	"feedback",
	'help':		"help"
},

'bookmarkletCopy': {
	'noExceptionMessage':	"The direct login configuration has been collected.",
	'exceptionMessage':		"Sorry! There was an error while processing the page.",
	'copy':					"copy",
	'successfulMessage':	"DONE!",
	'failMessage':			"Failed! :("
},

//################################################################################

__syntaxFix__: "syntax fix"
}
