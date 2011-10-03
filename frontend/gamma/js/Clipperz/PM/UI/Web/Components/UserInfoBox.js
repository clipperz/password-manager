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

Clipperz.PM.UI.Web.Components.UserInfoBox = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.UserInfoBox.superclass.constructor.apply(this, arguments);

	this._slots = {	};
	this._isLocked = false;
	this._lockTooltip = null;
	
	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.UserInfoBox, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.UserInfoBox component";
	},

	//-------------------------------------------------------------------------

	'handleLogout': function(anEvent) {
//Clipperz.log(">>> UserInfoBox.handleLogout");
		anEvent.preventDefault();
		MochiKit.Signal.signal(this, 'logout');
//Clipperz.log("<<< UserInfoBox.handleLogout");
	},

	//-------------------------------------------------------------------------

	'lockTooltip': function () {
		return this._lockTooltip;
	},

	//-------------------------------------------------------------------------

	'isLocked': function () {
		return this._isLocked;
	},
	
	'setIsLocked': function (aValue) {
		this._isLocked = aValue;
	},

	'toggleLock': function(anEvent) {
		var deferredResult;
		var shouldLock;

//console.log(">>> UserInfoBox.toggleLock [locked: " + this.isLocked() + "]");
		anEvent.preventDefault();
		this.lockTooltip().hide();

		shouldLock = (this.isLocked() == false);

		if (shouldLock) {
			var maskElement;

			this.setIsLocked(true);
			maskElement = this.getId('modalDialogMask');
			deferredResult = Clipperz.Async.callbacks("UserInfoBox.toggleLock [lock]", [
				MochiKit.Base.partial(MochiKit.DOM.addElementClass, this.element(), 'locked'),
				MochiKit.Base.partial(Clipperz.Visual.deferredAnimation, MochiKit.Visual.appear, maskElement, {from:0.0, to:0.75, duration:0.5}),
				MochiKit.Base.method(Clipperz.PM.RunTime.mainController, 'setPassphraseDelegate', MochiKit.Base.method(this, 'askForPassphrase')),
				MochiKit.Base.partial(MochiKit.Signal.signal, this, 'lock')
			], {trace:false});
		} else {
			deferredResult = Clipperz.Async.callbacks("UserInfoBox.toggleLock [unlock]", [
				MochiKit.Base.partial(MochiKit.Signal.signal, this, 'unlock')
			], {trace:false});
		}
//console.log("<<< UserInfoBox.toggleLock");
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'unlock': function () {
		var deferredResult;
		var maskElement;
		
		this.setIsLocked(false);
		maskElement = this.getId('modalDialogMask');
		
		deferredResult = Clipperz.Async.callbacks("UserInfoBox.unlock", [
			MochiKit.Base.partial(Clipperz.Visual.deferredAnimation, MochiKit.Visual.fade, maskElement, {from:0.75, to:0.0, duration:0.5}),
//			Clipperz.Visual.deferredAnimation(MochiKit.Visual.fade, maskElement, {from:0.75, to:0.0, duration:0.5}),
			MochiKit.Base.partial(MochiKit.DOM.removeElementClass, this.element(), 'locked')
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'askForPassphrase': function () {
		var	unlockPasswordComponent;
/*
		var	deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("UserInfoBox.askForPassphrase", {trace:false});
		deferredResult.addCallback(MochiKit.Async.succeed, 'test');
		
		deferredResult.callback();
		
		return deferredResult;
*/
//console.log(">>> UserInfoBox.askForPassphrase");
		unlockPasswordComponent = new Clipperz.PM.UI.Web.Components.UnlockPasswordComponent({
			'title':	"Unlock account",
			'text':		"Insert the passprase to unlock the account",
			'type':		'INFO',
			'buttons': [
				{text:"Cancel",	result:'CANCEL'},
				{text:"Unlock",	result:'OK',	isDefault:true}
			],
			'openFromElement':			this.getElement('lock'),
			'onOkCloseToElement':		null,
			'onCancelCloseToElement':	this.getId('lock')
		});
//console.log("<<< UserInfoBox.askForPassphrase");

		return unlockPasswordComponent.getPassphrase();
	},

	//=========================================================================

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(), [
//			{tag:'canvas', id:this.getId('canvas'), cls:'canvas', width:'188', height:'154'},
			{tag:'div', cls:'header', children:[
				{tag:'h1', html:"Welcome"},
				{tag:'a', cls:'lockButton', href:'#', id:this.getId('lock'), html:'&nbsp;'}
			]},
			{tag:'div', cls:'body', children:[
				{tag:'h3', id:this.getId('username'), html:""},
				{tag:'ul', children:[
					{tag:'li', id:this.getId('cards'), children:[
						{tag:'span', id:this.getId('cardsNumber'), cls:'number', html:"-"},
						{tag:'span', id:this.getId('cardsLabel'), html:"cards"}
					]},
					{tag:'li', id:this.getId('directLogins'), children:[
						{tag:'span', id:this.getId('directLoginsNumber'), cls:'number', html:"-"},
						{tag:'span', id:this.getId('directLoginsLabel'), html:"direct logins"}
					]}
				]},
				{tag:'a', href:'#', id:this.getId('logout'), html:"logout >"}
			]},
			{tag:'div', cls:'footer'}
		]);

		MochiKit.Signal.connect(this.getElement('logout'), 'onclick', this, 'handleLogout');
		MochiKit.Signal.connect(this.getElement('lock'), 'onclick', this, 'toggleLock');

		this._lockTooltip = new Clipperz.PM.UI.Common.Components.Tooltip({
			element:	this.getElement('lock'),
			text:		"Click here to lock/unlock your account.",
			position:	'RIGHT'
		});

		Clipperz.DOM.Helper.append(MochiKit.DOM.currentDocument().body,
			{tag:'div', id:this.getId('modalDialogWrapper'), cls:'modalDialogWrapper', children:[
				{tag:'div', id:this.getId('modalDialogMask'), cls:'modalDialogMask userInfoBoxMask'}
			]}
		);
		MochiKit.Style.hideElement(this.getId('modalDialogMask'));
		
//		this.drawUserInfoBackground(this.getElement('canvas'));
	},

	//-------------------------------------------------------------------------
/*
	'drawUserInfoBackground': function (canvas) {
		var kMyDrawingFunctionWidth = 188.0;
		var kMyDrawingFunctionHeight = 154.0;

		var context = canvas.getContext("2d");
		var color;
		var resolution;
		var alignStroke;
		var path;
		var pointX;
		var pointY;
		var controlPoint1X;
		var controlPoint1Y;
		var controlPoint2X;
		var controlPoint2Y;
		var gradient;
		if (window.devicePixelRatio)
			resolution = window.devicePixelRatio;
		else
			resolution = 1.0;
		resolution *= 0.5 * (canvas.width / kMyDrawingFunctionWidth + canvas.height / kMyDrawingFunctionHeight);
	
		context.save();
		context.scale(canvas.width / kMyDrawingFunctionWidth, canvas.height / kMyDrawingFunctionHeight);
		context.clearRect(0.0, 0.0, kMyDrawingFunctionWidth, kMyDrawingFunctionHeight);
	
		// Setup for Shadow Effect
		color = "rgba(0.0%, 0.0%, 0.0%, 0.667)";
		context.save();
		context.shadowColor = color;
		context.shadowBlur = 3.0;
		context.shadowOffsetX = 5.729 * Math.cos(7.592) * resolution;
		context.shadowOffsetY = 5.729 * Math.sin(7.592) * resolution;
	
		// Layer 1
	
		alignStroke = 0.0;
		context.beginPath();
		pointX = 169.5;
		pointY = 141.5;
		pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
		pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
		context.moveTo(pointX, pointY);
		pointX = 177.5;
		pointY = 133.5;
		pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
		pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
		controlPoint1X = 173.889;
		controlPoint1Y = 141.5;
		controlPoint1X = (Math.round(resolution * controlPoint1X + alignStroke) - alignStroke) / resolution;
		controlPoint1Y = (Math.round(resolution * controlPoint1Y + alignStroke) - alignStroke) / resolution;
		controlPoint2X = 177.5;
		controlPoint2Y = 137.889;
		controlPoint2X = (Math.round(resolution * controlPoint2X + alignStroke) - alignStroke) / resolution;
		controlPoint2Y = (Math.round(resolution * controlPoint2Y + alignStroke) - alignStroke) / resolution;
		context.bezierCurveTo(controlPoint1X, controlPoint1Y, controlPoint2X, controlPoint2Y, pointX, pointY);
		pointX = 177.5;
		pointY = 19.5;
		pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
		pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
		context.lineTo(pointX, pointY);
		pointX = 169.5;
		pointY = 11.5;
		pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
		pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
		controlPoint1X = 177.5;
		controlPoint1Y = 15.111;
		controlPoint1X = (Math.round(resolution * controlPoint1X + alignStroke) - alignStroke) / resolution;
		controlPoint1Y = (Math.round(resolution * controlPoint1Y + alignStroke) - alignStroke) / resolution;
		controlPoint2X = 173.889;
		controlPoint2Y = 11.5;
		controlPoint2X = (Math.round(resolution * controlPoint2X + alignStroke) - alignStroke) / resolution;
		controlPoint2Y = (Math.round(resolution * controlPoint2Y + alignStroke) - alignStroke) / resolution;
		context.bezierCurveTo(controlPoint1X, controlPoint1Y, controlPoint2X, controlPoint2Y, pointX, pointY);
		pointX = 18.5;
		pointY = 11.5;
		pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
		pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
		context.lineTo(pointX, pointY);
		pointX = 10.5;
		pointY = 19.5;
		pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
		pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
		controlPoint1X = 14.111;
		controlPoint1Y = 11.5;
		controlPoint1X = (Math.round(resolution * controlPoint1X + alignStroke) - alignStroke) / resolution;
		controlPoint1Y = (Math.round(resolution * controlPoint1Y + alignStroke) - alignStroke) / resolution;
		controlPoint2X = 10.5;
		controlPoint2Y = 15.111;
		controlPoint2X = (Math.round(resolution * controlPoint2X + alignStroke) - alignStroke) / resolution;
		controlPoint2Y = (Math.round(resolution * controlPoint2Y + alignStroke) - alignStroke) / resolution;
		context.bezierCurveTo(controlPoint1X, controlPoint1Y, controlPoint2X, controlPoint2Y, pointX, pointY);
		pointX = 10.5;
		pointY = 133.5;
		pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
		pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
		context.lineTo(pointX, pointY);
		pointX = 18.5;
		pointY = 141.5;
		pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
		pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
		controlPoint1X = 10.5;
		controlPoint1Y = 137.889;
		controlPoint1X = (Math.round(resolution * controlPoint1X + alignStroke) - alignStroke) / resolution;
		controlPoint1Y = (Math.round(resolution * controlPoint1Y + alignStroke) - alignStroke) / resolution;
		controlPoint2X = 14.111;
		controlPoint2Y = 141.5;
		controlPoint2X = (Math.round(resolution * controlPoint2X + alignStroke) - alignStroke) / resolution;
		controlPoint2Y = (Math.round(resolution * controlPoint2Y + alignStroke) - alignStroke) / resolution;
		context.bezierCurveTo(controlPoint1X, controlPoint1Y, controlPoint2X, controlPoint2Y, pointX, pointY);
		pointX = 169.5;
		pointY = 141.5;
		pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
		pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
		context.lineTo(pointX, pointY);
		context.closePath();
		gradient = context.createLinearGradient(94.0, 11.5, 94.0, 141.5);
		color = "#EE9B69";
		gradient.addColorStop(0.0, color);
		color = "#E38D62";
		gradient.addColorStop(1.0, color);
		context.fillStyle = gradient;
		context.fill();
	
		// Shadow Effect
		context.restore();
	
		context.restore();
	},
*/
	//-------------------------------------------------------------------------

	'updateUserDetails': function (someUserInfo) {
		var	elementName;
		
		for (elementName in someUserInfo) {
			this.getElement(elementName).innerHTML = someUserInfo[elementName];
		}
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
