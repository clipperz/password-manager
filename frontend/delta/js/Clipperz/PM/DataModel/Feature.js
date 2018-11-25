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
Clipperz.Base.module('Clipperz.PM.DataModel');

Clipperz.PM.DataModel.Feature = {
	'featureSets': {
		'TRIAL':	'Trial',
		'FULL':		'All features',
		'EXPIRED':	'Expired'
	},
	'features': {
		'OFFLINE_COPY':			'Offline copy',
		'LIST_CARDS':			'List cards',
		'CARD_DETAILS':			'Card details',
		'EDIT_CARD':			'Edit card',
		'ADD_CARD':				'Add card',
		'DELETE_CARD':			'Delete card',
		'UPDATE_CREDENTIALS':	'Update credentials',
	}
}

