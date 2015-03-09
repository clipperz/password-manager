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
if (typeof(Clipperz.YUI) == 'undefined') { Clipperz.YUI = {}; }


Clipperz.YUI.IBLayoutRegion = function(aManager, aName, aConfig) {
	this._configuration = aConfig;

//	Clipperz.YUI.IBLayoutRegion.superclass.constructor.call();
	Clipperz.YUI.IBLayoutRegion.superclass.constructor.call(this, aManager, aConfig, aName);
};

YAHOO.extendX(Clipperz.YUI.IBLayoutRegion, YAHOO.ext.LayoutRegion, {

	'toString': function() {
		return "IBLayoutRegion (" + this.name() + ")";
	},

	//-----------------------------------------------------

	'name': function() {
		return this.position;
	},

	//-----------------------------------------------------

	'manager': function() {
		return this.mgr;
	},

	'configuration': function() {
		return this._configuration;
	},

	//-----------------------------------------------------

	'getAttributeValue': function(anAttribute) {
		var result;
		
		switch(anAttribute) {
			case "top":
				result = this.element().getTop();
				break;
			case "left":
				result = this.element().getLeft();
				break;
			case "bottom":
				result = this.element().getBottom();
				break;
			case "right":
				result = this.element().getRight();
				break;
			case "height":
				result = this.element().getHeight();
				break;
			case "width":
				result = this.element().getWidth();
				break;
		}
//MochiKit.Logging.logDebug("--- " + this.name() + " [" + anAttribute + "] = " + result);
		
		return result;
	},
	
	//-----------------------------------------------------

	'normalizeConfigureValue': function(aConfigurationValue) {
		var result;

//MochiKit.Logging.logDebug("--- normalizeConfigureValue - " + aConfigurationValue);
		if (typeof(aConfigurationValue) == 'number') {
			result = aConfigurationValue;
		} else if (aConfigurationValue == 'auto') {
			result = aConfigurationValue;
		} else {
			var splitValues;
			var referenceValue;
			var deltaValue;
			var targetRegion;
			var targetAttribute;
			
			splitValues = aConfigurationValue.split('+');
			referenceValue = Clipperz.Base.trim(splitValues[0]);
			deltaValue = Clipperz.Base.trim(splitValues[1] || "");
			
			splitValues = referenceValue.split('.');
			targetRegion = splitValues[0];
			targetAttribute = splitValues[1];
			
//MochiKit.Logging.logDebug("> " + aConfigurationValue);
//MochiKit.Logging.logDebug(">> manager: " + this.manager());
//MochiKit.Logging.logDebug(">> targetRegion: " + targetRegion);
//MochiKit.Logging.logDebug(">>> " + this.manager().getRegion(targetRegion));
			targetValue = this.manager().getRegion(targetRegion).getAttributeValue(targetAttribute);
//MochiKit.Logging.logDebug(">>>> " + targetRegion + "." + targetAttribute + " + " + deltaValue + " = " + targetValue);

			result = targetValue + (deltaValue - 0);

//MochiKit.Logging.logDebug("<<< " + aConfigurationValue + " = " + result);
		}

		return result;
	},
	
	'normalizedConfiguration': function(aConfiguration) {
		var	result;
		var	key;
		
		result = {};

//MochiKit.Logging.logDebug("--- normalizedConfiguration - keys: " + Clipperz.Base.serializeJSON(MochiKit.Base.keys(aConfiguration)));
		for (key in aConfiguration) {
			if ((key == 'top') || (key == 'bottom') || (key == 'left') || (key == 'rigth') || (key == 'width') || (key == 'height')) {
				result[key] = this.normalizeConfigureValue(aConfiguration[key]);
			} else {
				result[key] = aConfiguration[key];
			}
		}
		
		return result;
	},
	
	//-----------------------------------------------------

	'element': function() {
		return this.el;
	},

	//-----------------------------------------------------
/*
	'hide': function() {
MochiKit.Logging.logDebug(">>> IBLayoutManager.hide()")
		Clipperz.YUI.IBLayoutRegion.superclass.hide.call(this);
	},
*/
	//-----------------------------------------------------
/*
	'add': function(aPanel) {
		Clipperz.YUI.IBLayoutRegion.superclass.add.call(this, aPanel);
		aPanel.el.fitToParent(true);
	},
*/
	//-----------------------------------------------------

	'updateBox': function(aBox) {
//MochiKit.Logging.logDebug(">>> IBLayoutRegion.updateBox - " + aBox);
		Clipperz.YUI.IBLayoutRegion.superclass.updateBox.call(this, aBox);
	},
	
	//-----------------------------------------------------

	'layout': function() {
		var	top, left, bottom, right, width, height;
		var	element;
		var config;
		var windowSize;
		var containerSize;

//MochiKit.Logging.logDebug(">>> IBLayoutRegion.layout - " + this);
		config = this.normalizedConfiguration(this.configuration());
		element = this.element();
//		containerSize = this.manager().getSize(true);
		containerSize = this.manager().getSize(false);
		windowSize = {width: YAHOO.util.Dom.getViewportWidth(), height: YAHOO.util.Dom.getViewportHeight()};

//		element.setStyle("position", "absolute");
//		element.setStyle("overflow", "none");

		if (typeof(config.top) == 'number') {
			top = config.top;
			
			if (typeof(config.bottom) == 'number') { 
				height =  containerSize.height - top - config.bottom;
			} else if (typeof(config.height) == 'number') {
				height = config.height;
			} else {
				//	???
			}
		} else {
			if ((typeof(config.bottom) == 'number') && (typeof(config.height) == 'number')) {
				top = containerSize.height - (config.height + config.bottom);
				height = config.height;
			} else if ((config.bottom == 'auto') && (typeof(config.height) == 'number')) {
				top = ((containerSize.height - config.height) / 2);
				height = config.height;
			}
		}

		if (typeof(config.left) == 'number') {
			left = config.left;
			
			if (typeof(config.right) == 'number') { 
				width =  (containerSize.width - left - config.right);
			} else if (typeof(config.width) == 'number') {
				width = config.width;
			} else {
				//	???
			}
		} else {
			if ((typeof(config.right) == 'number') && (typeof(config.width) == 'number')) {
				left = containerSize.width - (config.width + config.right);
				width = config.width;
			} else if ((config.right == 'auto') && (typeof(config.width) == 'number')) {
				left = ((containerSize.width - config.width) / 2);
				width = config.width;
			}
		}
//MochiKit.Logging.logDebug("--- setting position (top: " + top + ", left: " + left + ", width: " + width + ", height: " + height + ")");
		element.setTop(top);
		element.setLeft(left);
		element.setWidth(width);
		element.setHeight(height);
		
		if (this.activePanel != null) {
			this.activePanel.setSize(width, height);
		}
//MochiKit.Logging.logDebug("<<< IBLayoutRegion.layout");
	},

	//-----------------------------------------------------
	__syntaxFix__: '__syntaxFix__'
});
