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


Clipperz.YUI.IBLayoutManager = function(container, config) {
	var regionName;
	var	element;
	
	config = config || {};
	
    Clipperz.YUI.IBLayoutManager.superclass.constructor.call(this, container);
    this.hideOnLayout = config.hideOnLayout || false;

	element = YAHOO.ext.Element.get(container);
	element.setStyle('position', 'absolute');
	element.setStyle('overflow', 'hidden');
	
	for (regionName in config.regions) {
		var newRegion;
		
		newRegion = new new Clipperz.YUI.IBLayoutRegion(this, regionName, config.regions[regionName]);
		this.addRegion(regionName, newRegion);
	}
	
	this.layout();
};

YAHOO.extendX(Clipperz.YUI.IBLayoutManager, YAHOO.ext.LayoutManager, {
	
	'toString': function() {
		return "IBLayoutManager (" + this.el.id + ")";
	},
	
	//-----------------------------------------------------

	'add': function(aName, aPanel) {
		var regionName;
		
        regionName = aName.toLowerCase();
        return this.regions[regionName].add(aPanel);
	},

	//-----------------------------------------------------
    
	'addRegion': function(aRegion) {
		var regionName;
		
		regionName = aRegion.name().toLowerCase();
		if (!this.regions[regionName]) {
//MochiKit.Logging.logDebug("--- adding region with name: " + aRegion.name());
			this.regions[regionName] = aRegion;
		} else {
			// ????
		}
		
		return aRegion;
	},

	//-----------------------------------------------------

	'getRegion': function(target){
		return this.regions[target.toLowerCase()];
	},
	
	//-----------------------------------------------------

	'layout': function(){
		var	region;

//MochiKit.Logging.logDebug(">>> IBLayoutManager.layout - regions: " + Clipperz.Base.serializeJSON(MochiKit.Base.keys(this.regions)));
		for (region in this.regions) {
//MochiKit.Logging.logDebug("--- IBLayoutManager.layout - region: " + region);
			this.regions[region].layout();
		}
//MochiKit.Logging.logDebug("<<< IBLayoutManager.layout");
	},
	
	//-----------------------------------------------------

	'getSize': function() {
		return this.el.getSize();
	},
    
	//-----------------------------------------------------
	__syntaxFix__: '__syntaxFix__'
});
