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

Clipperz.Base.module('Clipperz.PM.UI.iPhone.Components');

Clipperz.PM.UI.iPhone.Components.LoginForm = function(args) {
	args = args || {};

	Clipperz.PM.UI.iPhone.Components.LoginForm.superclass.constructor.apply(this, arguments);

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.iPhone.Components.LoginForm, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.iPhone.Components.LoginForm component";
	},

	//-------------------------------------------------------------------------

	'focusOnUsername': function () {
		this.getElement('username').focus();
	},

	//-------------------------------------------------------------------------

	'username': function () {
		return this.getElement('username').value;
	},
	
	'passphrase': function () {
		return this.getElement('passphrase').value;
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(), [
			{tag:'div', cls:'toolbar iPhoneClipperzToolbar', children:[
				{tag:'h1', id:'pageTitle', html:'Clipperz'},
				{tag:'a', id:'backButton', cls:'button', href:'#', html:"back"}
			]},
			{tag:'form', title:'Theaters', cls:'panel toolbarlessPanel loginForm', id:this.getId('loginFormPanel'), children:[
				{tag:'fieldset', id:this.getId('fieldset'), children:[
					{tag:'div', cls:'row', children:[
						{tag:'label', html:"username"},
						{tag:'input', type:'text', name:'username', value:"", autocorrect:'off', autocapitalize:'off', id:this.getId('username')}
					]},
					{tag:'div', cls:'row', children:[
						{tag:'label', html:"passphrase"},
						{tag:'input', type:'password', name:'passphrase', value:"", id:this.getId('passphrase')}
					]}
				]},
				{tag:'a', cls:'whiteButton', type:'submit', href:'#', html:"Login", id:this.getId('submit')}
			]},
			{tag:'div', cls:'panel toolbarlessPanel loginProgressPanel', id:this.getId('loginProgressPanel'), children:[
				{tag:'div', id:this.getId('progressBar')} //,
//				{tag:'a', cls:'whiteButton', type:'submit', href:'#', html:"Cancel", id:this.getId('cancel')}
			]},
			{tag:'div', cls:'panel loginErrorPanel', id:this.getId('loginErrorPanel'), children:[
				{tag:'div', cls:'errorMessage', id:this.getId('errorMessageBox'), children:[
					{tag:'h2', id:this.getId('errorMessage'), html:"Login failed"}
				]}
			]}
		]);

		MochiKit.Signal.connect(this.getElement('submit'),	'onclick',	this, 'submitHandler');
		MochiKit.Signal.connect(this.getElement('loginFormPanel'),	'onsubmit',	this, 'submitHandler');

//		MochiKit.Signal.connect(this.getElement('cancel'),	'onclick',	this, 'cancelHandler');
		MochiKit.Signal.connect('backButton',	'onclick',	this, 'backHandler');

		this.addComponent(new Clipperz.PM.UI.Common.Components.ProgressBar({'element':this.getElement('progressBar')}));

//		MochiKit.Style.hideElement(this.getElement('errorMessage'));

		this.showLoginForm();
//		MochiKit.Async.callLater(0.2, MochiKit.Base.method(this, 'focusOnUsername'));
	},

	//-------------------------------------------------------------------------

	'showLoginForm': function () {
		MochiKit.Style.showElement(this.getElement('loginFormPanel'));
		MochiKit.Style.hideElement(this.getElement('loginProgressPanel'));
		MochiKit.Style.hideElement(this.getElement('loginErrorPanel'));
		MochiKit.Style.hideElement('backButton');
	},

	'slideInLoginForm': function () {
		var	offset;

		offset = ((MochiKit.DOM.getNodeAttribute(MochiKit.DOM.currentDocument().body, 'orientation') == 'portrait') ? 320 : 480);

		MochiKit.Style.showElement(this.getElement('loginFormPanel'));
		MochiKit.Style.setElementPosition(this.getElement('loginFormPanel'), {x:-offset, y:0});

		new MochiKit.Visual.Sequence([
			new MochiKit.Visual.Parallel([
				new MochiKit.Visual.Move(this.getElement('loginErrorPanel'),	{x:offset, y:0,	mode:'relative',	transition:MochiKit.Visual.Transitions.linear, sync:true}),
				new MochiKit.Visual.Move(this.getElement('loginFormPanel'),		{x:0, y:0,		mode:'absolute',	transition:MochiKit.Visual.Transitions.linear, sync:true}),
				MochiKit.Visual.fade    ('backButton',							{									transition:MochiKit.Visual.Transitions.linear, sync:true})
			], {duration:0.5, sync:true}),
			MochiKit.Visual.fade(this.getElement('loginErrorPanel'), {duration:0, sync:true})
		], {})
	},

	'showLoginProgress': function () {
		MochiKit.Style.hideElement(this.getElement('loginFormPanel'));
		MochiKit.Style.showElement(this.getElement('loginProgressPanel'));
	},

	'showLoginError': function (anError) {
		this.getElement('errorMessage').innerHTML = "Login error";

		MochiKit.Style.showElement('backButton');
		MochiKit.Style.hideElement(this.getElement('loginProgressPanel'));
		MochiKit.Style.showElement(this.getElement('loginErrorPanel'));
		MochiKit.Style.setElementPosition(this.getElement('loginErrorPanel'), {x:0, y:45});
	},

	//-------------------------------------------------------------------------
/*
	'disableCancelButton': function () {
		MochiKit.DOM.hideElement(this.getElement('cancel'));
	},
*/
	//-------------------------------------------------------------------------

	'submitHandler': function (anEvent) {
		anEvent.preventDefault();

		MochiKit.Signal.signal(this, 'doLogin', {'username':this.username(), 'passphrase':this.passphrase()});
	},

	'cancelHandler': function (anEvent) {
		anEvent.preventDefault();
		
//console.log("CANCEL");
	},

	'backHandler': function (anEvent) {
		anEvent.preventDefault();

		this.slideInLoginForm();
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});
