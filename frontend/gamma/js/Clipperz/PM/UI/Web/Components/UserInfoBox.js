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
			MochiKit.Base.partial(MochiKit.DOM.removeElementClass, this.element(), 'locked')
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'askForPassphrase': function () {
		var	unlockPasswordComponent;

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
//				{tag:'div', cls:'accountInfo', id:this.getId('accountInfo'), children:[
//					{tag:'h5', html:"Account info"},
//				]},
				{tag:'div', cls:'accountInfo', children:[
					{tag:'div', cls:'payButton', children:[
						{tag:'a', href:'#', id:this.getId('infoButton'), cls:'info', html:"info"}
					]},
					{tag:'h5', html:"Account info"},
					{tag:'div', cls:'accountStatus', children:[
						{tag:'span', cls:'label', html:"status"},
						{tag:'span', cls:'status', html:"early adopter"}
					]},
					{tag:'div', cls:'accountLevel', children:[
						{tag:'span', cls:'label', html:"level"},
						{tag:'span', cls:'level', html:"☆☆☆☆"}
					]},
					{tag:'div', cls:'accountExpiration', children:[
						{tag:'span', cls:'label', html:"expires"},
						{tag:'span', cls:'expriation', html:"never"}
					]}
				]},
				{tag:'a', href:'#', id:this.getId('logout'), html:"logout >"}
			]},
			{tag:'div', cls:'footer'}
		]);

		MochiKit.Signal.connect(this.getElement('logout'),		'onclick', this, 'handleLogout');
		MochiKit.Signal.connect(this.getElement('lock'),		'onclick', this, 'toggleLock');
		MochiKit.Signal.connect(this.getElement('infoButton'),	'onclick', this, 'handleInfoButton');

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
	},

	//-------------------------------------------------------------------------

	'handleInfoButton': function (anEvent) {
		anEvent.preventDefault();
		window.open('https://www.clipperz.com/pricing/', '_blank');
	},

	'handlePayButton': function (anEvent) {
		anEvent.preventDefault();
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'makePayment', anEvent);
	},


	//-------------------------------------------------------------------------

	'updateUserDetails': function (someUserInfo) {
		var	elementName;
		
		for (elementName in someUserInfo) {
			this.getElement(elementName).innerHTML = someUserInfo[elementName];
		}
	},

	'numberOfStarsMatchingActiveLevel': function (aValue) {
		var	maxLevel = 15;
		var	value;
		var	result;

		value = Math.min(aValue, maxLevel);
		result = value / maxLevel * 5;
		
		return result;
	},
	
	'starsLabel': function(numberOfActiveStars) {
		var	result;
		var	i;
		
		//	"★☆☆☆"
		result = "";
		for (i=0; i<5; i++) {
			if (i < numberOfActiveStars) {
				result = result + "★";
			} else {
				result = result + "☆";
			}
		}
		
		return result;
	},
/*
	'updateCurrentSubscriptionDetails': function (anAccountInfo) {
//console.log("AccountInfo", anAccountInfo);
		var	subscriptionLevel					= anAccountInfo.latestActiveLevel();

		var	expireLabel;
		var	formattedDate;

		if (anAccountInfo.expirationDate() == Clipperz.Date.distantFuture) {
			formattedDate = "never";
		} else {
			formattedDate = Clipperz.PM.Date.formatDateWithTemplate(anAccountInfo.expirationDate(), "D, d M Y");
		}

		this.append(this.getElement('accountInfo'), [
			{tag:'div', cls:'accountStatus', children:[
				{tag:'span', cls:'label', html:"status"},
				{tag:'span', cls:'status', html:anAccountInfo.status()}
			]}
		]);

		if (anAccountInfo.isExpired()) {
			expireLabel = "expired";
		} else {
			expireLabel = "expires";
		}
		
		this.append(this.getElement('accountInfo'), [

			{tag:'div', cls:'accountLevel', children:[
				{tag:'span', cls:'label', html:"level"},
				{tag:'span', cls:'level', html:this.starsLabel(this.numberOfStarsMatchingActiveLevel(anAccountInfo.latestActiveThreshold()))}
			]},

			{tag:'div', cls:'accountExpiration', children:[
				{tag:'span', cls:'label', html:expireLabel},
				{tag:'span', cls:'expriation', html:formattedDate}
			]}
		]);

		if (anAccountInfo.isExpired() || anAccountInfo.isExpiring()) {
			var	buttonLabel;

			if (anAccountInfo.paymentVerificationPending()) {
				buttonLabel = "Verify payment";
			} else {
				if (subscriptionLevel == 'TRIAL') {
					buttonLabel = "Subscribe";
				} else if (subscriptionLevel == 'EARLY_ADOPTER') {
					buttonLabel = "Donate";
				} else if (subscriptionLevel == 'PATRON') {
					buttonLabel = "Donate";
				} else if (subscriptionLevel == 'PAYMENT_PENDING') {
					buttonLabel = "Verify payment";
				} else {
					buttonLabel = "Renew subscription";
				}
			}

			this.append(this.getElement('accountInfo'), [
				{tag:'div', cls:'payButton', children:[
					{tag:'a', href:'#', id:this.getId('payButton'), cls:'info', html:buttonLabel}
				]},
			]);			

			MochiKit.Signal.connect(this.getElement('payButton'),	'onclick', this, 'handlePayButton');

		}
	},
*/
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
