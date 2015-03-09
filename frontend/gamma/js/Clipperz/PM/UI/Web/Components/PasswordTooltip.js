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

Clipperz.PM.UI.Web.Components.PasswordTooltip = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.PasswordTooltip.superclass.constructor.apply(this, arguments);

	this._referenceElement	= args.referenceElement	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._text				= args.text				|| Clipperz.Base.exception.raise('MandatoryParameter');

	this._boxDimensions = null;
	this._isVisible = false;

	this.renderSelf();

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.PasswordTooltip, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.PasswordTooltip component";
	},

	//-------------------------------------------------------------------------

	'referenceElement': function () {
		return this._referenceElement;
	},

	//-------------------------------------------------------------------------

	'text': function () {
		return this._text;
	},
	
	'setText': function (aValue) {
		this._text = aValue;
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
		this.append(MochiKit.DOM.getElement('Clipperz_PM_UI_Common_Components_Tooltip_wrapperNode'), {tag:'div', id:this.getId('tooltip'), cls:'passwordTooltip', children:[
			{tag:'div', id:this.getId('body'), cls:'passwordTooltip_body', children:[
				{tag:'div', cls:'passwordTooltip_text', children:[
					{tag:'span', html:this.text()}
				]},
				{tag:'div', id:this.getId('footer'), cls:'passwordTooltip_footer'}
			]},
			{tag:'div', id:this.getId('arrow'), cls:'passwordTooltip_arrow'}
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

			MochiKit.Style.setElementDimensions(this.getId('arrow'), {w:36, h:13}, 'px');
			bodyPosition.x = elementSizeAndPosition.position.x + (elementSizeAndPosition.dimensions.w/2 - this.boxDimensions().w/2);
			bodyPosition.y = elementSizeAndPosition.position.y - this.boxDimensions().h - 13;
		
			arrowPosition.x = elementSizeAndPosition.position.x + (elementSizeAndPosition.dimensions.w/2 - 36/2);
			arrowPosition.y = elementSizeAndPosition.position.y - 13;

			MochiKit.Style.setElementPosition(this.getId('body'), bodyPosition);
			MochiKit.Style.setElementPosition(this.getId('arrow'), arrowPosition);
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

Clipperz.PM.UI.Web.Components.PasswordTooltip.initTooltips = function () {
	Clipperz.DOM.Helper.insertBefore(MochiKit.DOM.currentDocument().body.childNodes[0], {tag:'div', id:'Clipperz_PM_UI_Web_Components_PasswordTooltip_wrapperNode'});
}

MochiKit.DOM.addLoadEvent(Clipperz.PM.UI.Web.Components.PasswordTooltip.initTooltips);
