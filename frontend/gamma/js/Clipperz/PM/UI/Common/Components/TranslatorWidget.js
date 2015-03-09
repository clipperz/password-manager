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

Clipperz.PM.UI.Common.Components.TranslatorWidget = function(args) {
Clipperz.log(">>> TranslatorWidget.new");
	args = args || {};

	Clipperz.PM.UI.Common.Components.TranslatorWidget.superclass.constructor.apply(this, arguments);

//	this._element	= args.element	|| Clipperz.Base.exception.raise('MandatoryParameter');
//	this._stringID	= args.stringID	|| MochiKit.DOM.getNodeAttribute(this.element(), 'stringID')	|| Clipperz.Base.exception.raise('MandatoryParameter');

//	MochiKit.Signal.connect(this.element(), 'onmouseenter', this, 'show');
//	MochiKit.Signal.connect(this.element(), 'onmouseleave', this, 'hide');

Clipperz.log("<<< TranslatorWidget.new");
	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Common.Components.TranslatorWidget, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Common.Components.TranslatorWidget component";
	},

	//-------------------------------------------------------------------------
/*
	'renderSelf': function() {
		this.append(this.element(), {tag:'div', id:this.getId('tooltip'), cls:'tooltip ' + this.position(), children:[
			{tag:'div', id:this.getId('body'), cls:'tooltip_body', children:[
				{tag:'div', cls:'tooltip_text', children:[
					{tag:'span', html:this.text()}
				]},
				{tag:'div', id:this.getId('footer'), cls:'tooltip_footer'}
			]},
			{tag:'div', id:this.getId('arrow'), cls:'tooltip_arrow'}
		]});

		this._boxDimensions = MochiKit.Style.getElementDimensions(this.getId('body'));
//		this._boxDimensions.h += MochiKit.Style.getElementDimensions(this.getId('footer')).h;

		MochiKit.Style.hideElement(this.displayElement());
		MochiKit.Signal.connect(this.element(), 'onmouseenter', this, 'show');
		MochiKit.Signal.connect(this.element(), 'onmouseleave', this, 'hide');
	},
*/
	//-----------------------------------------------------
/*
	'displayElement': function() {
		return this.getElement('tooltip');
	},
*/
	//-------------------------------------------------------------------------
/*
	'boxDimensions': function () {
		return this._boxDimensions;
	},
*/
	//-------------------------------------------------------------------------

	'show': function (anElement, aStringID) {
		Clipperz.log(">>> Clipperz.PM.UI.Common.Components.TranslatorWidget.show: " + aStringID);
/*
		var elementSizeAndPosition;
		var arrowPosition;
		var bodyPosition;

		arrowPosition = {};
		bodyPosition = {};

		elementSizeAndPosition = Clipperz.Style.getSizeAndPosition(this.element());
		switch (this.position()) {
			case 'ABOVE':
				MochiKit.Style.setElementDimensions(this.getId('arrow'), {w:36, h:13}, 'px');
				bodyPosition.x = elementSizeAndPosition.position.x + (elementSizeAndPosition.dimensions.w/2 - this.boxDimensions().w/2);
				bodyPosition.y = elementSizeAndPosition.position.y - this.boxDimensions().h - 13;
				
				arrowPosition.x = elementSizeAndPosition.position.x + (elementSizeAndPosition.dimensions.w/2 - 36/2);
				arrowPosition.y = elementSizeAndPosition.position.y - 13;
				break;
			case 'BELOW':
				MochiKit.Style.setElementDimensions(this.getId('arrow'), {w:36, h:13}, 'px');
				bodyPosition.x = elementSizeAndPosition.position.x + (elementSizeAndPosition.dimensions.w/2 - this.boxDimensions().w/2);
				bodyPosition.y = elementSizeAndPosition.position.y + elementSizeAndPosition.dimensions.h + 13;
				
				arrowPosition.x = elementSizeAndPosition.position.x + (elementSizeAndPosition.dimensions.w/2 - 36/2);
				arrowPosition.y = elementSizeAndPosition.position.y + elementSizeAndPosition.dimensions.h;
				break;
			case 'LEFT':
				MochiKit.Style.setElementDimensions(this.getId('arrow'), {w:13, h:36}, 'px');
				bodyPosition.x = elementSizeAndPosition.position.x - this.boxDimensions().w - 13;
				bodyPosition.y = elementSizeAndPosition.position.y + (elementSizeAndPosition.dimensions.h/2 - this.boxDimensions().h/2);
				
				arrowPosition.x = elementSizeAndPosition.position.x -13;
				arrowPosition.y = elementSizeAndPosition.position.y + (elementSizeAndPosition.dimensions.h/2 - 36/2);
				break;
			case 'RIGHT':
				MochiKit.Style.setElementDimensions(this.getId('arrow'), {w:13, h:36}, 'px');
				break;
		}

//		MochiKit.Style.setElementPosition(this.getId('body'), bodyPosition);
		MochiKit.Style.setElementPosition(this.getId('body'), bodyPosition);
		MochiKit.Style.setElementPosition(this.getId('arrow'), arrowPosition);
		MochiKit.Visual.appear(this.displayElement(), {duration:0.4});
*/
	},

	//-------------------------------------------------------------------------

	'hide': function () {
		Clipperz.log("<<< Clipperz.PM.UI.Common.Components.TranslatorWidget.hide");
//		MochiKit.Visual.fade(this.displayElement(), {duration:0.4});
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

//#############################################################################

Clipperz.PM.UI.Common.Components.TranslatorWidget._widget = null;

Clipperz.PM.UI.Common.Components.TranslatorWidget.widget = function () {
	if (Clipperz.PM.UI.Common.Components.TranslatorWidget._widget == null) {
		Clipperz.PM.UI.Common.Components.TranslatorWidget._widget = new Clipperz.PM.UI.Common.Components.TranslatorWidget();
	}
	
	return Clipperz.PM.UI.Common.Components.TranslatorWidget._widget;
}
Clipperz.PM.UI.Common.Components.TranslatorWidget.show = function (anElement, aStringID) {
	Clipperz.PM.UI.Common.Components.TranslatorWidget.widget().show(anElement, aStringID);
}

Clipperz.PM.UI.Common.Components.TranslatorWidget.hide = function () {
	Clipperz.PM.UI.Common.Components.TranslatorWidget.widget().hide();
}

//#############################################################################
