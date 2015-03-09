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

Clipperz.PM.UI.Web.Components.PageHeader = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.PageHeader.superclass.constructor.apply(this, arguments);
	this._newsIsOpen = args.newsIsOpen || false;
	this._animationDuration = args.animationDuration || 0.5;
	
	this._offset = 82;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.PageHeader, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.PageHeader component";
	},

	//-------------------------------------------------------------------------

	'iframeURL': function () {
//		return './rss_view.html';
		return 'https://www.clipperz.com/tips/index.html';
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(), [
			{tag:'div', id:'miscLinks', children:[
				{tag:'ul', children:[
					{tag:'li', children:[{tag:'a', id:'donateHeaderLink',	stringID:'pageHeader.donation',	href:'http://www.clipperz.com/donations',			target:'_blank', html:Clipperz.PM.Strings.getValue('pageHeader.donation')}]},
					{tag:'li', children:[{tag:'a', id:'forumHeaderLink',	stringID:'pageHeader.forum',	href:'http://www.clipperz.com/forum',				target:'_blank', html:Clipperz.PM.Strings.getValue('pageHeader.forum')}]},
//					{tag:'li', children:[{tag:'a', id:'creditsHeaderLink',	stringID:'pageHeader.credits',	href:'http://www.clipperz.com/credits',				target:'_blank', html:Clipperz.PM.Strings.getValue('pageHeader.credits')}]},
					{tag:'li', children:[{tag:'a', id:'feedbackHeaderLink',	stringID:'pageHeader.feedback',	href:'http://www.clipperz.com/contact',				target:'_blank', html:Clipperz.PM.Strings.getValue('pageHeader.feedback')}]},
					{tag:'li', children:[{tag:'a', id:'helpHeaderLink',		stringID:'pageHeader.help',		href:'http://www.clipperz.com/support/user_guide',	target:'_blank', html:Clipperz.PM.Strings.getValue('pageHeader.help')}]}
				]}
			]},
			{tag:'div', id:'logoFrame', children:[
				{tag:'a', href:'http://www.clipperz.com', target:'_blank', children:[
//					{tag:'h1', cls:'logo', html:"clipperz"},
					{tag:'canvas', id:this.getId('logo'), cls:'logo'},
					{tag:'h5', cls:'clipperzPayoff', html:"keep it to yourself!"}
				]}
			]},
			{tag:'div', id:'news', cls:'hidden', children:[
//				{tag:'div', cls:'close', children:[
//					{tag:'a', href:'#', id:this.getId('closeTips'), html:'x'}
//				]},
				{tag:'div', id:'newsframe', children:[
					{tag:'iframe', id:this.getId('iframe'), src:this.iframeURL()}
				]},
				{tag:'div', id:this.getId('newsGrip'), cls:'grip', children:[
					{tag:'div', cls:'gripHandler', children:[]}
				]}
			]},
			{tag:'div', id:'featureTabs', children:[
				{tag:'table', children:[{tag:'tr', children:[
					{tag:'td', children:[{tag:'div', id:'feature_store',		children:[{tag:'canvas', cls:'featureIcon', id:this.getId('storeIcon')},		{tag:'span', html:"Store and manage your password and online credentials"}]}]},
					{tag:'td', children:[{tag:'div', id:'feature_protect',		children:[{tag:'canvas', cls:'featureIcon', id:this.getId('protectIcon')},		{tag:'span', html:"Protect all your sensitive data"}]}]},
					{tag:'td', children:[{tag:'div', id:'feature_directLogin',	children:[{tag:'canvas', cls:'featureIcon', id:this.getId('directLoginIcon')},	{tag:'span', html:"Login to your web services without entering any username or password"}]}]},
					{tag:'td', children:[{tag:'div', id:'feature_share',		children:[{tag:'canvas', cls:'featureIcon', id:this.getId('shareIcon')},		{tag:'span', html:"Share secret with family members and associates"}]}]}
				]}]}
			]}
		]);

		Clipperz.PM.UI.Canvas.features.store(this.getElement('storeIcon'), "#ffffff");
		Clipperz.PM.UI.Canvas.features.protect(this.getElement('protectIcon'), "#ffffff");
		Clipperz.PM.UI.Canvas.features.directLogin(this.getElement('directLoginIcon'), "#ffffff");
		Clipperz.PM.UI.Canvas.features.share(this.getElement('shareIcon'), "#ffffff", "#ffffff", "#ff0000");

		MochiKit.Signal.connect(this.getElement('newsGrip'), 'onclick', this, 'toggleTips');
		MochiKit.Signal.connect(this.getElement('iframe'), 'onload', this, 'handleIframeDidLoad');
		this.setLogoDefaultColors();
	},

	//-------------------------------------------------------------------------

	'switchToLoggedMode': function() {
//		MochiKit.Style.addElementClass(this.element(), 'logged');
		MochiKit.Style.hideElement('featureTabs');
	},

	//-------------------------------------------------------------------------

	'animationDuration': function () {
		return this._animationDuration;
	},

	'offset': function () {
		return this._offset;
	},

	//-------------------------------------------------------------------------

	'isNewsOpen': function () {
		return 	this._newsIsOpen;
	},
	
	'toggleNewsIsOpen': function () {
		this._newsIsOpen = !this._newsIsOpen;
	},

	'toggleTips': function(anEvent) {
		anEvent.preventDefault();

		if (this.isNewsOpen() == true) {
			MochiKit.Visual.Move(Clipperz.DOM.get('news'), {
				x: 0,
				y: -this.offset(),
				mode: 'relative',
				duration: this.animationDuration(),
				beforeStart: function () {
					MochiKit.DOM.setElementClass(Clipperz.DOM.get('news'), 'hiding');
				},
				afterFinish: function () {
					Clipperz.DOM.get('newsframe').innerHTML = "";
					MochiKit.DOM.setElementClass(Clipperz.DOM.get('news'), 'hidden');
				}
			})
			this.toggleNewsIsOpen();
		} else {
			MochiKit.DOM.addElementClass('newsframe', 'loading');
			MochiKit.Visual.Move(Clipperz.DOM.get('news'), {
				x: 0,
				y: this.offset(),
				mode: 'relative',
				duration: this.animationDuration(),
				beforeStart: MochiKit.Base.bind(function () {
					this.append(Clipperz.DOM.get('newsframe'), {tag:'iframe', id:this.getId('iframe'), src:this.iframeURL()});

					MochiKit.Signal.connect(this.getElement('iframe'), 'onload', this, 'handleIframeDidLoad');
					MochiKit.DOM.setElementClass(Clipperz.DOM.get('news'), 'opening');
				}, this),
				afterFinish: function () {
					MochiKit.DOM.setElementClass(Clipperz.DOM.get('news'), 'open');
				}
			})
			this.toggleNewsIsOpen();
		}
	},

	'setLogoDefaultColors': function () {
		Clipperz.PM.UI.Canvas.logo.normal(this.getElement('logo'), "clipperz", "28.0pt", "#ffffff");
	},
	
	//-------------------------------------------------------------------------

	'handleIframeDidLoad': function (anEvent) {
		if (this.isNewsOpen() == false) {
			this.toggleTips(anEvent);
		}
		
		MochiKit.DOM.removeElementClass('newsframe', 'loading');
		MochiKit.Signal.disconnectAllTo(this.getElement('iframe'));
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
