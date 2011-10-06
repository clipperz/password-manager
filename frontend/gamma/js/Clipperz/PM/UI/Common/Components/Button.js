/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz Community Edition.
Clipperz Community Edition is an online password manager.
For further information about its features and functionalities please
refer to http://www.clipperz.com.

* Clipperz Community Edition is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Clipperz Community Edition is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Clipperz Community Edition.  If not, see
  <http://www.gnu.org/licenses/>.

*/

Clipperz.Base.module('Clipperz.PM.UI.Common.Components');

Clipperz.PM.UI.Common.Components.Button = function(args) {
	args = args || {};

	Clipperz.PM.UI.Common.Components.Button.superclass.constructor.apply(this, arguments);

	this._element	= args.element		|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._text		= args.text			|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._isDefault	= args.isDefault	|| false;

	this.render();

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Common.Components.Button, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Common.Components.Button component";
	},

	//-------------------------------------------------------------------------

	'text': function () {
		return this._text;
	},

	'isDefault': function () {
		return this._isDefault;
	},

	//-------------------------------------------------------------------------

	'renderSelf': function () {
		this.append(this.element(), {tag:'div', id:this.getId('wrapper'), cls:'button_wrapper', children:[
			{tag:'div', id:this.getId('bodyWrapper'), cls:'button_bodyWrapper', children:[
				{tag:'div', id:this.getId('body'), cls:'button_body', children:[
					{tag:'span', html:this.text()}
				]},
				{tag:'div', id:this.getId('footer'), cls:'button_footer'}
			]}
		]});

		if (this.isDefault()) {
			MochiKit.DOM.addElementClass(this.getId('wrapper'), 'default');
		}

		MochiKit.Signal.connect(this.getId('wrapper'), 'onmouseenter',	this, 'handleOnMouseEnter');
		MochiKit.Signal.connect(this.getId('wrapper'), 'onmouseleave',	this, 'handleOnMouseLeave');
		MochiKit.Signal.connect(this.getId('wrapper'), 'onmousedown',	this, 'handleOnMouseDown');
		MochiKit.Signal.connect(this.getId('wrapper'), 'onclick',		this, 'handleOnClick');
	},

	//-------------------------------------------------------------------------

	'handleOnMouseEnter': function (anEvent) {
		MochiKit.DOM.addElementClass(this.getId('wrapper'), 'hover');
	},
	
	'handleOnMouseLeave': function (anEvent) {
		MochiKit.DOM.removeElementClass(this.getId('wrapper'), 'hover');
		MochiKit.DOM.removeElementClass(this.getId('wrapper'), 'clicked');
	},

	'handleOnMouseDown': function (anEvent) {
		MochiKit.DOM.addElementClass(this.getId('wrapper'), 'clicked');
	},
	
	'handleOnClick': function (anEvent) {
		MochiKit.Signal.signal(this, 'onclick', anEvent);
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
