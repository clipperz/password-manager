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

Clipperz.Base.module('Clipperz.PM.UI.Web.Components');

Clipperz.PM.UI.Common.Components.SimpleMessagePanel = function(args) {
	args = args || {};

	Clipperz.PM.UI.Common.Components.SimpleMessagePanel.superclass.constructor.apply(this, arguments);

	this._title		= args.title		|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._text		= args.text			|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._type		= args.type			|| Clipperz.Base.exception.raise('MandatoryParameter');	//	ALERT, INFO, ERROR
	this._buttons	= args.buttons		|| Clipperz.Base.exception.raise('MandatoryParameter');

	this._buttonComponents = [];
	this._deferred = null;

	this.renderModalMask();

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Common.Components.SimpleMessagePanel, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Common.Components.SimpleMessagePanel component";
	},

	//-------------------------------------------------------------------------

	'deferred': function() {
		if (this._deferred == null) {
			this._deferred = new Clipperz.Async.Deferred("SimpleMessagePanel.deferred", {trace:false});
		}

		return this._deferred;
	},

	//-------------------------------------------------------------------------

	'title': function () {
		return this._title;
	},
	
	'setTitle': function (aValue) {
		this._title = aValue;

		if (this.getElement('title') != null) {
			this.getElement('title').innerHTML = aValue;
		}
	},

	//-------------------------------------------------------------------------

	'text': function () {
		return this._text;
	},
	
	'setText': function (aValue) {
		this._text = aValue;

		if (this.getElement('text') != null) {
			this.getElement('text').innerHTML = aValue;
		}
	},

	//-------------------------------------------------------------------------

	'type': function () {
		return this._type;
	},
	
	'setType': function (aValue) {
		if (this.getElement('icon') != null) {
			MochiKit.DOM.removeElementClass(this.getId('icon'), this._type);
			MochiKit.DOM.addElementClass(this.getId('icon'), aValue);
		}

		this._type = aValue;
	},

	//-------------------------------------------------------------------------

	'buttons': function () {
		return this._buttons;
	},
	
	'setButtons': function (someValues) {
		MochiKit.Iter.forEach(this.buttonComponents(), MochiKit.Base.methodcaller('clear'));
		
		this._buttons = someValues;

		if (this.getElement('buttonArea') != null) {
			this.renderButtons();
		}
	},

	//.........................................................................

	'buttonComponents': function () {
		return this._buttonComponents;
	},

	//-------------------------------------------------------------------------

	'renderSelf': function() {
		this.append(this.element(), {tag:'div', cls:'SimpleMessagePanel', id:this.getId('panel'), children: [
			{tag:'div', cls:'header', children:[]},
			{tag:'div', cls:'body', children:[
				{tag:'div',	id:this.getId('icon'),	cls:'img ' + this.type(), children:[{tag:'div'}]},
				{tag:'h3',	id:this.getId('title'),	html:this.title()},
				{tag:'p',	id:this.getId('text'),	html:this.text()},
				{tag:'div', id:this.getId('container')},
				{tag:'div', id:this.getId('buttonArea'), cls:'buttonArea', children:[]}
			]},
			{tag:'div', cls:'footer', children:[]}
		]});

		MochiKit.Signal.connect(this.getId('panel'), 'onkeydown', this, 'keyDownHandler');

		this.renderButtons();
	},

	//-------------------------------------------------------------------------

	'renderButtons': function () {
		this.getElement('buttonArea').innerHTML = '';

		MochiKit.Base.map(MochiKit.Base.bind(function (aButton) {
			var buttonElement;
			var buttonComponent;
			
//			element = this.append(this.getElement('buttonArea'), {tag:'div', cls:'button' + (aButton['isDefault'] === true ? ' default' : ''), children:[
//				{tag:'a', href:'#'/*, id:this.getId('buttonLink')*/, html:aButton['text']}
//			]});

			buttonElement = this.append(this.getElement('buttonArea'), {tag:'div'});
			buttonComponent = new Clipperz.PM.UI.Common.Components.Button({'element':buttonElement, 'text':aButton['text'], 'isDefault':aButton['isDefault']});
			this.buttonComponents().push(buttonComponent);

			MochiKit.Signal.connect(buttonComponent, 'onclick', MochiKit.Base.method(this, 'buttonEventHandler', aButton));
		}, this), MochiKit.Iter.reversed(this.buttons()));
	},

	//-------------------------------------------------------------------------

	'displayElement': function() {
		return this.getElement('panel');
	},

	//-------------------------------------------------------------------------

	'closeOk': function () {
		this.deferred().callback();
		this._deferred = null;
	},
	
	'closeCancel': function () {
		this.deferred().cancel();
		this._deferred = null;
	},

	'closeError': function () {
		this.deferred().errback();
		this._deferred = null;
	},

	//-------------------------------------------------------------------------

	'buttonEventHandler': function(aButton, anEvent) {
		anEvent.preventDefault();
		
//		MochiKit.Signal.signal(this, 'cancelEvent');
		switch (aButton['result']) {
			case 'OK':
//console.log("==> OK");
				this.closeOk();
				break;
			case 'CANCEL':
//console.log("==> CANCEL");
				this.closeCancel();
				break;
			default:
//console.log("==> ????");
				this.closeError();
				break;
		}
//console.log("<==");
	},

	//-------------------------------------------------------------------------

	'deferredShow': function (someArgs, aResult) {
		this.deferredShowModal(someArgs);

		this.deferred().addMethod(this, 'deferredHideModal', {closeToElement:someArgs.onOkCloseToElement });
		this.deferred().addErrback (MochiKit.Base.method(this, 'deferredHideModal', {closeToElement:someArgs.onCancelCloseToElement }));
		this.deferred().addCallback(MochiKit.Async.succeed, aResult);

		return this.deferred();
	},

	//-------------------------------------------------------------------------

	'modalDialogMask': function () {
		return this.getId('modalDialogMask');
	},
	
	'modalDialog': function () {
		return this.getId('modalDialog');
	},
	
	'modalDialogFrame': function() {
		return this.getId('modalDialogFrame');
	},

	//-------------------------------------------------------------------------
	
	'renderModalMask': function () {
		Clipperz.DOM.Helper.append(MochiKit.DOM.currentDocument().body,
			{tag:'div', id:this.getId('modalDialogWrapper'), cls:'modalDialogWrapper simpleMessagePanelMask', children:[
				{tag:'div', id:this.getId('modalDialogMask'), cls:'modalDialogMask simpleMessagePanelMask'},
				{tag:'div', id:this.getId('modalDialogFrame'), cls:'modalDialogFrame simpleMessagePanelMask'},
				{tag:'div', id:this.getId('modalDialog'), cls:'modalDialog simpleMessagePanelMask'}
			]}
		);
	
		MochiKit.Style.hideElement(this.getId('modalDialogMask'));
		MochiKit.Style.hideElement(this.getId('modalDialogFrame'));
	},

	//-------------------------------------------------------------------------

	'keyDownHandler': function (anEvent) {
		if (anEvent.key().string == 'KEY_ENTER') {
			anEvent.preventDefault();
//console.log("13 - RETURN ?", this);
			this.closeOk();
//console.log('<<< 13')
		}

		if (anEvent.key().string == 'KEY_ESCAPE') {
			anEvent.preventDefault();
//console.log("27 - ESC ?", this);
			this.closeCancel();
//console.log("<<< 27");
		}
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
