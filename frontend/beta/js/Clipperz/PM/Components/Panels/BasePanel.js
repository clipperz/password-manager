/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz's Javascript Crypto Library.
Javascript Crypto Library provides web developers with an extensive
and efficient set of cryptographic functions. The library aims to
obtain maximum execution speed while preserving modularity and
reusability.
For further information about its features and functionalities please
refer to http://www.clipperz.com

* Javascript Crypto Library is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Javascript Crypto Library is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Javascript Crypto Library.  If not, see
  <http://www.gnu.org/licenses/>.

*/

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.Components) == 'undefined') { Clipperz.PM.Components = {}; }
if (typeof(Clipperz.PM.Components.Panels) == 'undefined') { Clipperz.PM.Components.Panels = {}; }

//var _Clipperz_PM_Components_Panels_base_id_ = 0;

//#############################################################################

Clipperz.PM.Components.Panels.BasePanel = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Panels.BasePanel.superclass.constructor.call(this, anElement, args);
	
	this._user = args.user || null;
	this._delegate = args.delegate || null;
	this._tabPanelController = null;
//	Clipperz.NotificationCenter.register(null, 'switchLanguage', this, 'switchLanguageHandler');

//	this._ids = {};

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Panels.BasePanel, Clipperz.PM.Components.BaseComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Panels.BasePanel component";
	},

	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},
	
	'setUser': function(aValue) {
		this._user = aValue;
	},

	//-------------------------------------------------------------------------

	'delegate': function() {
		return this._delegate;
	},
	
	'setDelegate': function(aValue) {
		this._delegate = aValue;
	},

	//-------------------------------------------------------------------------

	'tabPanelController': function() {
		return this._tabPanelController;
	},
	
	'switchLanguageHandler': function() {
//MochiKit.Logging.logDebug(">>> BasePanel.switchLanguageHandler [" + this.toString() + "]");
		this.render();
//MochiKit.Logging.logDebug("<<< BasePanel.switchLanguageHandler [" + this.toString() + "]");
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});
