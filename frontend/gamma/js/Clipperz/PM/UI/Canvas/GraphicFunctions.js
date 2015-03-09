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

Clipperz.Base.module('Clipperz.PM.UI.Canvas');

MochiKit.Base.update(Clipperz.PM.UI.Canvas , {
	'marks': {
		'!':	Clipperz_PM_UI_Canvas_Marks_exclamationMark,
		'?':	Clipperz_PM_UI_Canvas_Marks_questionMark,
		'i':	Clipperz_PM_UI_Canvas_Marks_info
	},

	'features': {
		'store':		Clipperz_PM_UI_Canvas_Features_store,
		'protect':		Clipperz_PM_UI_Canvas_Features_protect,
		'directLogin':	Clipperz_PM_UI_Canvas_Features_directLogin,
		'share':		Clipperz_PM_UI_Canvas_Features_share
	},
	
	'tips': {
		'open':			Clipperz_PM_UI_Canvas_Tips_open,
		'close':		Clipperz_PM_UI_Canvas_Tips_close
	},

	'star': {
		'normal':		Clipperz_PM_UI_Canvas_Star_normal
	},

	'coverActions': {
		'look':			Clipperz_PM_UI_Canvas_CoverActions_look,
		'download':		Clipperz_PM_UI_Canvas_CoverActions_download
	},

	'registerButton': {
		'normal':		Clipperz_PM_UI_Canvas_RegisterButton_normal
	},

	'logo': {
		'normal':		Clipperz_PM_UI_Canvas_Logo_normal
	},
	
	__syntaxFix__: "syntax fix"
});
