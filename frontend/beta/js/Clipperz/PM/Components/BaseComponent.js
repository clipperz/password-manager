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
if (typeof(Clipperz.PM.Components) == 'undefined') { Clipperz.PM.Components = {}; }

//#############################################################################

var _Clipperz_PM_Components_Panels_base_id_ = 0;

//#############################################################################

Clipperz.PM.Components.BaseComponent = function(anElement, args) {
	args = args || {};
//	MochiKit.Base.bindMethods(this);
//	Clipperz.PM.Components.BaseComponent.superclass.constructor.call(this, args);

	this._element = anElement;
	this._ids = {};
	
	return this;
}

//=============================================================================

//MochiKit.Base.update(Clipperz.PM.Components.BaseComponent.prototype, {
YAHOO.extendX(Clipperz.PM.Components.BaseComponent, YAHOO.ext.util.Observable, {

	'isClipperzPMComponent': true,
	
	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.Components.BaseComponent component";
	},

	//-------------------------------------------------------------------------

	'domHelper': function() {
		return Clipperz.YUI.DomHelper;
	},
	
	//-------------------------------------------------------------------------

	'element': function() {
//MochiKit.Logging.logDebug(">>> BaseComponent.element");
		return this._element;
	},
	
	'setElement': function(aValue) {
		this._element = aValue;
	},

	//-----------------------------------------------------

	'remove': function() {
//MochiKit.Logging.logDebug(">>> BaseComponent.remove");
		Clipperz.NotificationCenter.unregister(this);
		MochiKit.Signal.disconnectAllTo(this);
//MochiKit.Logging.logDebug("<<< BaseComponent.remove");
	},
	
	//-------------------------------------------------------------------------

	'getId': function(aValue) {
		var	result;
		
		result = this._ids[aValue];
		
		if (typeof(result) == 'undefined') {
			_Clipperz_PM_Components_Panels_base_id_ ++;
			
			result = "Clipperz_PM_Components_Panels_" + aValue + "_" + _Clipperz_PM_Components_Panels_base_id_;
			this._ids[aValue] = result;
//MochiKit.Logging.logDebug(">>> getId(" + aValue + ") = " + result);
		} else {
//MochiKit.Logging.logDebug("<<< getId(" + aValue + ") = " + result);
		}
	
		return result;
	},

	'getDom': function(aValue) {
		return YAHOO.util.Dom.get(this.getId(aValue));
	},
	
	'getElement': function(aValue) {
		return YAHOO.ext.Element.get(this.getId(aValue));
	},
	
	'getActor': function(aValue, anAnimator) {
 		return new YAHOO.ext.Actor(this.getDom(aValue), anAnimator);
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});
