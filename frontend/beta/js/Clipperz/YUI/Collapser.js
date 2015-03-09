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

//	found on YUI-EXT forum (http://www.yui-ext.com/forum/viewtopic.php?t=683&highlight=accordion)
Clipperz.YUI.Collapser = function(clickEl, collapseEl, initiallyCollapsed) { 
	this.clickEl = getEl(clickEl); 
	this.collapseEl = getEl(collapseEl); 
	this.clickEl.addClass('collapser-expanded'); 
	if (initiallyCollapsed == true) {
		this.afterCollapse();
	}
	this.clickEl.mon('click', function(){ 
		this.collapsed === true ? this.expand() : this.collapse(); 
	}, this, true); 
}; 

Clipperz.YUI.Collapser.prototype = { 
	'collapse': function(){ 
		this.collapseEl.clip(); 
		this.collapseEl.setHeight(1, true, .35, this.afterCollapse.createDelegate(this), YAHOO.util.Easing.easeOut); 
		this.clickEl.replaceClass('collapser-expanded','collapser-collapsed'); 
	}, 

	'afterCollapse': function(){ 
		this.collapsed = true; 
		this.collapseEl.setDisplayed(false); 
		this.clickEl.replaceClass('collapser-expanded','collapser-collapsed'); 
	}, 

	'expand': function(){ 
		this.collapseEl.setDisplayed(true); 
		this.collapseEl.autoHeight(true, .35, this.afterExpand.createDelegate(this), YAHOO.util.Easing.easeOut); 
		this.clickEl.replaceClass('collapser-collapsed','collapser-expanded'); 
	}, 

	'afterExpand': function(){ 
		this.collapsed = false; 
		this.collapseEl.unclip(); 
		this.collapseEl.setStyle('height', '');    
		this.clickEl.replaceClass('collapser-collapsed','collapser-expanded'); 
	},

	//-----------------------------------------------------
	__syntaxFix__: '__syntaxFix__'
};
