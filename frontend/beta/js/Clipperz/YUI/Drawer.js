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


Clipperz.YUI.Drawer = function(anElement, aRegion) {
	this._status = 'slideIn';
	
	this._element = YAHOO.ext.Element.get(anElement);
	this._region = aRegion || null;

	this._collapsedElement = this.element().getChildrenByClassName("drawer-collapsed")[0];
	this._contentElement = this.element().getChildrenByClassName("drawer-content")[0];

	
	this._wholeCollapedElement = this.enhanceCollapsedElement();
	this._wholeCollapedElement.setWidth(this.region().element().getWidth());
	this._wholeCollapedElement.setHeight(this.region().element().getHeight());
	
	this._contentWrapper = this.enhanceContentElement();
	this._contentElementActor = new YAHOO.ext.Actor(this.contentWrapper().dom);
	this.contentElementActor().hide();
	
	this._contentWidth = 200;
};

YAHOO.extendX(Clipperz.YUI.Drawer, YAHOO.ext.util.Observable, {

	'element': function() {
		return this._element;
	},

	//-----------------------------------------------------

	'status': function() {
		return this._status;
	},
	
	'setStatus': function(aValue) {
		this._status = aValue;
	},
	
	//-----------------------------------------------------

	'collapsedElement': function() {
		return this._collapsedElement;
	},

	//-----------------------------------------------------

	'contentElement': function() {
		return this._contentElement;
	},
	
	//-----------------------------------------------------

	'contentElementActor': function() {
		return this._contentElementActor;
	},
	
	//-----------------------------------------------------

	'contentWrapper': function() {
		return this._contentWrapper;
	},
	
	//-----------------------------------------------------

	'contentWidth': function() {
		return this._contentWidth;
	},

	//-----------------------------------------------------

	'region': function() {
		return this._region;
	},
	
	//-----------------------------------------------------

	'enhanceCollapsedElement': function() {
		var	wrapper;
		var link;
		
		wrapper = this.collapsedElement().wrap({tag:'div', cls:'drawer-collapsedElement-wrapper', children:[
			{tag:'div', cls:'drawer-pin-button', children:[
				{tag:'a', cls:'drawer-pin-button', href:"#", children:[
					{tag:'img', src:'./images/directLogins/drawer/mm-expand.gif'}
				]}
			]}
		]});
		
		link = wrapper.getChildrenByClassName('drawer-pin-button', 'a')[0];
		MochiKit.Signal.connect(link.dom, 'onclick', this, 'pinDrawer');
		
		this.collapsedElement().setHeight('100%');
		this.collapsedElement().setStyle('cursor', 'pointer');
		MochiKit.Signal.connect(this.collapsedElement().dom, 'onclick', this, 'showDrawer');
		
		return wrapper;
	},

	//-----------------------------------------------------

	'enhanceContentElement': function() {
		var wrapper;
		
		wrapper = this.contentElement().wrap({tag:'div', cls:'drawer-content-wrapper', children:[
			{tag:'div', cls:'drawer-content-header', html:'direct login', style:'width:100%;'}
		]});
		
		MochiKit.Signal.connect(wrapper.dom, 'onclick', this, 'hideDrawer');
		return wrapper;
	},

	//-----------------------------------------------------

	'pinDrawer': function() {
		alert("pin drawer");
	},

	//-----------------------------------------------------

	'showDrawer': function() {
		if (this.status() == 'slideIn') {
			var actor;

			this.setStatus('slidingOut');
			actor = this.contentElementActor();
			actor.setHeight(this.region().element().getHeight());
		
			actor.startCapture(true);
			actor.alignTo(this.element(), 'tr');
			actor.blindShow('left', this.contentWidth(), .35);
			actor.play(this.onSlideOut.createDelegate(this));
		}
	},
	
	//-----------------------------------------------------

	'onSlideOut': function() {
		this.setStatus('slideOut');
MochiKit.Logging.logDebug(">>> onSlideOut");
//		alert("done");
	},

	//-----------------------------------------------------
/*
	'showContentElement': function() {
		var top, left, width, height;

MochiKit.Logging.logDebug(">>> showContentElement");


		top = this.element().getTop(true);
		left = this.element().getRight();
		width = this.contentWidth();
		height = this.element().getHeight();

		this.contentWrapper().setStyle('position', 'absolute');
		this.contentWrapper().setStyle('overflow', 'none');
		this.contentWrapper().setStyle('visibility', 'visible');
		this.contentWrapper().setStyle('z-index', '10');

		this.contentWrapper().setLeft(left);
		this.contentWrapper().setTop(top);
		this.contentWrapper().setHeight(height);
		this.contentWrapper().setWidth(width);

		this.contentWrapper().show();
	},
*/	
	//-----------------------------------------------------

	'hideDrawer': function() {
		if (this.status() == 'slideOut') {
			var actor;

			this.setStatus('slidingIn');

			actor = this.contentElementActor();
			actor.setHeight(this.region().element().getHeight());
		
			actor.startCapture(true);
			actor.alignTo(this.element(), 'tr');
			actor.blindHide('left', .35);
			actor.setVisible(false);
			actor.play(this.onSlideIn.createDelegate(this));
		}
	},
	
	//-----------------------------------------------------

	'onSlideIn': function() {
		this.setStatus('slideIn');
MochiKit.Logging.logDebug(">>> onSlideIn");
//		alert("done");
	},

	//-----------------------------------------------------
	
	'hideContentElement': function() {
		this.contentWrapper().hide();
	},
	
	//-----------------------------------------------------
	//-----------------------------------------------------

	//-----------------------------------------------------
	__syntaxFix__: '__syntaxFix__'
});