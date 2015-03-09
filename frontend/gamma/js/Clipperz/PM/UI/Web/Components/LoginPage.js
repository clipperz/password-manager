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

Clipperz.PM.UI.Web.Components.LoginPage = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.LoginPage.superclass.constructor.apply(this, arguments);

	this._slots = {
		'loginForm':	this.getId('loginBoxSlot')
	};

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.LoginPage, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.LoginPage component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(), [
			{tag:'div', id:this.getId('loginBoxSlot')},
			{tag:'div', id:'main', children:[
				{tag:'div', id:'featurePoints', children:[
					{tag:'table', children:[
						{tag:'tr', children:[
							{tag:'td', children:[
								{tag:'div', cls:'block', children:[
									{tag:'h3', html:'Clipperz is:'},
									{tag:'ul', children:[
										{tag:'li', html:'a secure and simple password manager'},
										{tag:'li', html:'an effective single sign-on solution'},
										{tag:'li', html:'a digital vault for your personal data'}
									]}
								]},
								{tag:'div', cls:'block', children:[
									{tag:'h3', html:'Clipperz benefits:'},
									{tag:'ul', children:[
										{tag:'li', html:'free and completely anonymous'},
										{tag:'li', html:'access it any time from any computer'},
										{tag:'li', html:'no software to download and nothing to install'},
										{tag:'li', html:'avoid keeping secrets on your PC or on paper'}
									]}
								]}
							]}, {tag:'td', children:[
								{tag:'div', cls:'block', children:[
									{tag:'h3', html:'Clipperz security:'},
									{tag:'ul', children:[
										{tag:'li', html:'your secretes are locally encrypted by your browser before being uploaded to Clipperz'},
										{tag:'li', html:'the encryption key is a passphrase known only to you'},
										{tag:'li', html:'Clipperz hosts your sensitive data in an encrypted form and could never access the data in its plain form'},
										{tag:'li', html:'Clipperz is built upon standard encryption schemes, nothing fancies of homemade'},
										{tag:'li', html:'you can review the source code anytime you like, but you need to know nothing about cryptography to be an happy user!'}
									]}
								]}
							]}
						]}
					]}
				]},
				{tag:'div', cls:'activeFeatures', children:[
					{tag:'div', id:this.getId('registerButton'), cls:'createAccountLink', children:[
						{tag:'canvas', id:this.getId('registerButtonIcon')},
						{tag:'a', href:'#', id:this.getId('createAccountLink'), cls:'createAccountLink', children:[
							{tag:'span', cls:'payoff', html:"Free sign up!"},
							{tag:'span', cls:'link', html:"Create account >>"}
						]}
					]},
					{tag:'div', cls:'keepTogether', children:[
						{tag:'div', id:this.getId('screenshotLink'), cls:'screenshotLink', children:[
							{tag:'canvas', id:this.getId('lookIcon')},
							{tag:'a', href:'#', cls:'screenshotLink', children:[
								{tag:'span', cls:'payoff', html:"Look Clipperz!"},
								{tag:'span', cls:'link', html:"screenshot tour >>"}
							]}
						]},
						{tag:'div', id:this.getId('offlineLink'), cls:'offlineLink', children:[
							{tag:'canvas', id:this.getId('downloadIcon')},
							{tag:'a', href:'#', cls:'offlineLink', children:[
								{tag:'span', cls:'payoff', html:"Download!"},
								{tag:'span', cls:'link', html:"Offline version >>"}
							]}
						]}
					]}
				]}
			]}
		]);

		this.setRegistrationButtonIconDefaultColors();
		this.setLookIconDefaultColors();
		this.setDownloadIconDefaultColors();

//		MochiKit.Signal.connect(this.getElement('createAccountLink'), 'onclick', this, 'handleCreateAccountLink')

		MochiKit.Signal.connect(this.getElement('registerButton'), 'onmouseenter', this, 'handleMouseEnterOnRegisterButtonIcon');
		MochiKit.Signal.connect(this.getElement('registerButton'), 'onmouseleave', this, 'handleMouseLeaveOnRegisterButtonIcon');

		MochiKit.Signal.connect(this.getElement('screenshotLink'), 'onmouseenter', this, 'handleMouseEnterOnLookIcon');
		MochiKit.Signal.connect(this.getElement('screenshotLink'), 'onmouseleave', this, 'handleMouseLeaveOnLookIcon');

		MochiKit.Signal.connect(this.getElement('offlineLink'), 'onmouseenter', this, 'handleMouseEnterOnDownloadIcon');
		MochiKit.Signal.connect(this.getElement('offlineLink'), 'onmouseleave', this, 'handleMouseLeaveOnDownloadIcon');
		
		MochiKit.Signal.connect(this.getElement('createAccountLink'), 'onclick', this, 'handleCreateAccountLink')
	},

	//-------------------------------------------------------------------------

	'setRegistrationButtonIconDefaultColors': function () {
		Clipperz.PM.UI.Canvas.registerButton.normal(this.getElement('registerButtonIcon'), "#eeeeee", "#eeeeee", "#ecab12", "#e14624", "#ffffff");
	},

	'setRegistrationButtonIconHoverColors': function () {
		Clipperz.PM.UI.Canvas.registerButton.normal(this.getElement('registerButtonIcon'), "#cccccc", "#999999", "#ffb710", "#ff4d27", "#ffffff");
	},

	'handleMouseEnterOnRegisterButtonIcon': function (anEvent) {
		this.setRegistrationButtonIconHoverColors();
	},

	'handleMouseLeaveOnRegisterButtonIcon': function (anEvent) {
		this.setRegistrationButtonIconDefaultColors();
	},

	//-------------------------------------------------------------------------

	'setLookIconDefaultColors': function () {
		Clipperz.PM.UI.Canvas.coverActions.look(this.getElement('lookIcon'), "#7e7e7e", "#ffffff", 1);
	},

	'setLookIconHoverColors': function () {
		Clipperz.PM.UI.Canvas.coverActions.look(this.getElement('lookIcon'), "#666666", "#ffffff", 2);
	},

	'handleMouseEnterOnLookIcon': function (anEvent) {
		this.setLookIconHoverColors();
	},
	
	'handleMouseLeaveOnLookIcon': function (anEvent) {
		this.setLookIconDefaultColors();
	},

	//-------------------------------------------------------------------------

	'setDownloadIconDefaultColors': function () {
		Clipperz.PM.UI.Canvas.coverActions.download(this.getElement('downloadIcon'), "#7e7e7e", "#ffffff", 1);
	},

	'setDownloadIconHoverColors': function () {
		Clipperz.PM.UI.Canvas.coverActions.download(this.getElement('downloadIcon'), "#666666", "#ffffff", 2);
	},

	'handleMouseEnterOnDownloadIcon': function (anEvent) {
		this.setDownloadIconHoverColors();
	},
	
	'handleMouseLeaveOnDownloadIcon': function (anEvent) {
		this.setDownloadIconDefaultColors();
	},

	//-------------------------------------------------------------------------

	'handleCreateAccountLink': function (anEvent) {
		anEvent.preventDefault();
		
		MochiKit.Signal.signal(this, 'createNewAccountClick', anEvent.src());
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});
