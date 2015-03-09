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

Clipperz.PM.UI.Web.Components.LoginProgress = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.LoginProgress.superclass.constructor.apply(this, arguments);

	this._deferred = null;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.LoginProgress, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.LoginProgress component";
	},

	//-------------------------------------------------------------------------

	'deferred': function() {
		return this._deferred;
	},

	'setDeferred': function(aValue) {
		this._deferred = aValue;
	},

	//-------------------------------------------------------------------------

	'renderSelf': function() {
//		var loginProgressElement;
//		
//		loginProgressElement = MochiKit.DOM.getElement('loginProgress');
//		
//		if (loginProgressElement == null) {
//			loginProgressElement = this.append(this.element(), {tag:'div', id:'loginProgress', cls:'LoginProgress'}, true);
//		}

		this.append(this.element(), {tag:'div', id:'loginProgress', cls:'LoginProgress', children: [
//		this.append(loginProgressElement, [
			{tag:'div', cls:'header', children:[
				{tag:'h3', id:this.getId('title'), html:"login progress"}
			]},
			{tag:'div', cls:'body', children:[
				{tag:'div', id:this.getId('progressBar')},
				{tag:'div', id:this.getId('errorBox'), cls:'errorBox', children:[
//					{tag:'div',	cls:'img ALERT', children:[{tag:'div'}]},
					{tag:'div',	cls:'img ALERT', children:[{tag:'canvas', id:this.getId('canvas')}]},
					{tag:'p', html:"Login failed"}
				]}
			]},
			{tag:'div', cls:'footer', children:[
				{tag:'div', cls:'buttonArea', id:this.getId('buttonArea'), children:[
//					{tag:'div', cls:'button', id:this.getId('button'), children:[
//						{tag:'a', href:'#', id:this.getId('buttonLink'), html:"cancel"}
//					]}
					{tag:'a', cls:'button', id:this.getId('button'), html:"cancel"}
				]}
			]}
		]});
//		]);

		Clipperz.PM.UI.Canvas.marks['!'](this.getElement('canvas'), "#ffffff");

		this.addComponent(new Clipperz.PM.UI.Common.Components.ProgressBar({'element':this.getElement('progressBar')}));
		MochiKit.Style.hideElement(this.getElement('errorBox'));
		
//		MochiKit.Signal.connect(this.getId('buttonLink'), 'onclick', this, 'cancelEventHandler');
		MochiKit.Signal.connect(this.getId('button'), 'onclick', this, 'cancelEventHandler');
	},

	//-------------------------------------------------------------------------

	'displayElement': function() {
		return MochiKit.DOM.getElement('loginProgress');
	},

	//-------------------------------------------------------------------------

	'cancelEventHandler': function(anEvent) {
		anEvent.preventDefault();
		
		MochiKit.Signal.signal(this, 'cancelEvent');
	},

	//-------------------------------------------------------------------------

	'disableCancel': function() {
		MochiKit.Style.hideElement(this.getElement('buttonArea'));
	},

	//-------------------------------------------------------------------------

	'showErrorMessage': function() {
//		this.getElement('buttonLink').innerHTML = "close";
		this.getElement('button').innerHTML = "close";
		MochiKit.DOM.addElementClass(this.getElement('button'), 'default');

		MochiKit.Style.hideElement(this.getElement('progressBar'));

		this.getElement('title').innerHTML = "Error";
		MochiKit.Style.showElement(this.getElement('errorBox'));
		MochiKit.Style.showElement(this.getElement('buttonArea'));
	},

	//-------------------------------------------------------------------------

	'deferredHideModalAndRemove': function(someParameters, aResult) {
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("LoginProgress.deferredHideModalAndRemove", {trace:false});
		deferredResult.addMethod(this, 'deferredHideModal');
		deferredResult.addMethod(this, 'remove');
		deferredResult.addCallback(function () {
			return aResult;
		});
		deferredResult.callback(someParameters);

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});
