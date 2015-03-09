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

Clipperz.Base.module('Clipperz.PM.UI.Common.Components');

Clipperz.PM.UI.Common.Components.Tooltip = function(args) {
	args = args || {};

	Clipperz.PM.UI.Common.Components.Tooltip.superclass.constructor.apply(this, arguments);

	this._element	= args.element	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._text		= args.text		|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._position	= args.position	|| 'BELOW';		//	'BELOW', 'ABOVE', 'LEFT', 'RIGHT'

	this._boxDimensions = null;
	this._enabled = (typeof(args.enabled) == 'undefined' ? true : args.enabled);
	this._isVisible = false;

	this.renderSelf();

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Common.Components.Tooltip, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Common.Components.Tooltip component";
	},

	//-------------------------------------------------------------------------

	'text': function () {
		return this._text;
	},
	
	'setText': function (aValue) {
		this._text = aValue;
	},

	//-------------------------------------------------------------------------

	'position': function () {
		return this._position;
	},
	
	'setPosition': function (aValue) {
		this._position = aValue;
	},

	//-------------------------------------------------------------------------

	'enabled': function () {
		return this._enabled;
	},
	
	'setIsEnabled': function (aValue) {
		this._enabled = aValue;
	},

	//-------------------------------------------------------------------------

	'isVisible': function () {
		return this._isVisible;
	},
	
	'setIsVisible': function (aValue) {
		this._isVisible = aValue;
	},

	//-------------------------------------------------------------------------

	'renderSelf': function() {
//		this.append(this.element(), {tag:'div', id:this.getId('tooltip'), cls:'tooltip ' + this.position(), children:[
//		this.append(MochiKit.DOM.currentDocument().body, {tag:'div', id:this.getId('tooltip'), cls:'tooltip ' + this.position(), children:[
		this.append(MochiKit.DOM.getElement('Clipperz_PM_UI_Common_Components_Tooltip_wrapperNode'), {tag:'div', id:this.getId('tooltip'), cls:'tooltip ' + this.position(), children:[
			{tag:'div', id:this.getId('body'), cls:'tooltip_body', children:[
				{tag:'div', cls:'tooltip_text', children:[
					{tag:'span', html:this.text()}
				]}//,
//				{tag:'div', id:this.getId('footer'), cls:'tooltip_footer'}
			]},
			{tag:'div', id:this.getId('arrow'), cls:'tooltip_arrow'}
		]});

		this._boxDimensions = MochiKit.Style.getElementDimensions(this.getId('body'));
//		this._boxDimensions.h += MochiKit.Style.getElementDimensions(this.getId('footer')).h;

		MochiKit.Style.hideElement(this.displayElement());
		MochiKit.Signal.connect(this.element(), 'onmouseenter', this, 'show');
		MochiKit.Signal.connect(this.element(), 'onmouseleave', this, 'hide');
	},

	//-----------------------------------------------------

	'displayElement': function() {
		return this.getElement('tooltip');
	},

	//-------------------------------------------------------------------------

	'boxDimensions': function () {
		return this._boxDimensions;
	},

	//-------------------------------------------------------------------------

	'show': function () {
		var elementSizeAndPosition;
		var arrowPosition;
		var bodyPosition;

		if (this.isVisible() == false) {
			arrowPosition = {};
			bodyPosition = {};

			this.setIsVisible(true);
			elementSizeAndPosition = Clipperz.Style.getSizeAndPosition(this.element());
			switch (this.position()) {
				case 'ABOVE':
//					MochiKit.Style.setElementDimensions(this.getId('arrow'), {w:36, h:13}, 'px');
					bodyPosition.x = elementSizeAndPosition.position.x + (elementSizeAndPosition.dimensions.w/2 - this.boxDimensions().w/2);
					bodyPosition.y = elementSizeAndPosition.position.y - this.boxDimensions().h - 13;
				
//					arrowPosition.x = elementSizeAndPosition.position.x + (elementSizeAndPosition.dimensions.w/2 - 36/2);
//					arrowPosition.y = elementSizeAndPosition.position.y - 13;
					break;
				case 'BELOW':
//					MochiKit.Style.setElementDimensions(this.getId('arrow'), {w:36, h:13}, 'px');
					bodyPosition.x = elementSizeAndPosition.position.x + (elementSizeAndPosition.dimensions.w/2 - this.boxDimensions().w/2);
					bodyPosition.y = elementSizeAndPosition.position.y + elementSizeAndPosition.dimensions.h + 13;
				
//					arrowPosition.x = elementSizeAndPosition.position.x + (elementSizeAndPosition.dimensions.w/2 - 36/2);
//					arrowPosition.y = elementSizeAndPosition.position.y + elementSizeAndPosition.dimensions.h;
					break;
				case 'LEFT':
//					MochiKit.Style.setElementDimensions(this.getId('arrow'), {w:13, h:36}, 'px');
					bodyPosition.x = elementSizeAndPosition.position.x - this.boxDimensions().w - 13;
					bodyPosition.y = elementSizeAndPosition.position.y + (elementSizeAndPosition.dimensions.h/2 - this.boxDimensions().h/2);

//					arrowPosition.x = elementSizeAndPosition.position.x -13;
//					arrowPosition.y = elementSizeAndPosition.position.y + (elementSizeAndPosition.dimensions.h/2 - 36/2);
					break;
				case 'RIGHT':
//					MochiKit.Style.setElementDimensions(this.getId('arrow'), {w:13, h:36}, 'px');
					bodyPosition.x = elementSizeAndPosition.position.x + elementSizeAndPosition.dimensions.w + 13;
					bodyPosition.y = elementSizeAndPosition.position.y + (elementSizeAndPosition.dimensions.h/2 - this.boxDimensions().h/2);

//					arrowPosition.x = elementSizeAndPosition.position.x + elementSizeAndPosition.dimensions.w;
//					arrowPosition.y = elementSizeAndPosition.position.y + (elementSizeAndPosition.dimensions.h/2 - 36/2);
					break;
			}

			MochiKit.Style.setElementPosition(this.getId('tooltip'), bodyPosition);
//			MochiKit.Style.setElementPosition(this.getId('body'), bodyPosition);
//			MochiKit.Style.setElementPosition(this.getId('arrow'), arrowPosition);
			MochiKit.Visual.appear(this.displayElement(), {duration:0.4});
		}
	},

	'hide': function () {
		if (this.isVisible() == true) {
			MochiKit.Visual.fade(this.displayElement(), {duration:0.4});
			this.setIsVisible(false);
		}
	},

	//-------------------------------------------------------------------------
/*
	'shouldRemoveElementWhenClearningUp': function () {
		return false;
	},
*/
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

Clipperz.PM.UI.Common.Components.Tooltip.initTooltips = function () {
	Clipperz.DOM.Helper.insertBefore(MochiKit.DOM.currentDocument().body.childNodes[0], {tag:'div', id:'Clipperz_PM_UI_Common_Components_Tooltip_wrapperNode'});
}

MochiKit.DOM.addLoadEvent(Clipperz.PM.UI.Common.Components.Tooltip.initTooltips);
