/*

Copyright 2008-2018 Clipperz Srl

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

Clipperz.Base.module('Clipperz.PM.UI');

Clipperz.PM.UI.DirectLoginController = function(args) {
	this._directLogin	= args['directLogin']		|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._target		= Clipperz.PM.Crypto.randomKey();

	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.DirectLoginController.prototype, {

	'toString': function() {
		return "Clipperz.PM.UI.DirectLoginController";
	},

	//-----------------------------------------------------------------------------

	'directLogin': function () {
		return this._directLogin;
	},

	//-----------------------------------------------------------------------------

	'target': function () {
		return this._target;
	},

	//=============================================================================

	'setWindowTitle': function (aWindow, aTitle) {
		aWindow.document.title = aTitle;
	},
	
	'setWindowBody': function (aWindow, anHTML) {
		aWindow.document.body.innerHTML = anHTML;
	},

	//=============================================================================

	'initialWindowSetup': function (aWindow) {
		this.setWindowTitle(aWindow, "Loading Clipperz Direct Login");
		this.setWindowBody (aWindow, MochiKit.DOM.toHTML(MochiKit.DOM.H3("Loading Clipperz Direct Login ...")));
	},

	//-----------------------------------------------------------------------------
	
	'updateWindowWithDirectLoginLabel': function (aWindow, aLabel) {
		var titleText;
		var bodyText;
		
		titleText = "Loading '__label__' Direct Login".replace(/__label__/, aLabel)
		bodyText  = "Loading '__label__' Direct Login... ".replace(/__label__/, aLabel)

		this.setWindowTitle(aWindow, titleText);
		this.setWindowBody (aWindow, MochiKit.DOM.toHTML(MochiKit.DOM.H3(bodyText)));
	},

	//-----------------------------------------------------------------------------
	
	'updateWindowWithHTMLContent': function (aWindow, anHtml) {
		this.setWindowBody(aWindow, anHtml);
	},

	//=============================================================================

	'submitLoginForm': function(aWindow, aSubmitFunction) {
		MochiKit.DOM.withWindow(aWindow, MochiKit.Base.bind(function () {
			var formElement;
			var submitButtons;

			formElement = MochiKit.DOM.getElement('directLoginForm');

			submitButtons = MochiKit.Base.filter(function(anInputElement) {
				return ((anInputElement.tagName.toLowerCase() == 'input') && (anInputElement.getAttribute('type').toLowerCase() == 'submit'));
			}, formElement.elements);

			if (submitButtons.length == 0) {
				if (typeof(formElement.submit) == 'function') {
					formElement.submit();
				} else {
					aSubmitFunction.apply(formElement);
				}
/*
				var	formSubmitFunction;

				formSubmitFunction = MochiKit.Base.method(formElement, 'submit');
				if (Clipperz_IEisBroken == true) {
					formElement.submit();
				} else {
					formSubmitFunction();
				}
*/
			} else {
				submitButtons[0].click();
			}
		}, this));
	},

	//-------------------------------------------------------------------------

	'runSubmitFormDirectLogin': function (aWindow, someAttributes) {
		var html;
		var formElement;
		var submitFunction;

		formElement = MochiKit.DOM.FORM({
			'id':'directLoginForm',
			'method':someAttributes['formAttributes']['method'],
			'action':someAttributes['formAttributes']['action']
		});

		submitFunction = formElement.submit;
		
		MochiKit.DOM.appendChildNodes(formElement, MochiKit.Base.map(function (anInputAttributes) {
			return MochiKit.DOM.INPUT({'type':'hidden', 'name':anInputAttributes[0], 'value':anInputAttributes[1]});
		}, MochiKit.Base.items(someAttributes['inputValues'])));

		html =	'';
		html += '<h3>Loading ' + someAttributes['label'] + ' ...</h3>';
		html +=	MochiKit.DOM.appendChildNodes(MochiKit.DOM.DIV(), MochiKit.DOM.appendChildNodes(MochiKit.DOM.DIV({style:'display:none; visibility:hidden;'}), formElement)).innerHTML; 

		this.updateWindowWithHTMLContent(aWindow, html);
		this.submitLoginForm(aWindow, submitFunction);
	},
	
	//-------------------------------------------------------------------------

	'runHttpAuthDirectLogin': function(aWindow, someAttributes) {
		var completeUrl;
		var url;

		url = someAttributes['inputValues']['url'];

		if (/^https?\:\/\//.test(url) == false) {
			url = 'http://' + url;
		}
		
		if (Clipperz_IEisBroken === true) {
			completeUrl = url;
		} else {
			var username;
			var password;
			
			username = someAttributes['inputValues']['username'];
			password = someAttributes['inputValues']['password'];
			/(^https?\:\/\/)?(.*)/.test(url);

			completeUrl = RegExp.$1 + username + ':' + password + '@' + RegExp.$2;
		}
		
		window.open(completeUrl, this.target());
	},

	//=============================================================================

	'runDirectLogin': function (aWindow) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("DirectLoginRunner.openDirectLogin", {trace:false});
		deferredResult.addMethod(this, 'initialWindowSetup', aWindow);
		deferredResult.addMethod(this.directLogin(), 'label');
		deferredResult.addMethod(this, 'updateWindowWithDirectLoginLabel', aWindow);
		deferredResult.collectResults({
			'type':				MochiKit.Base.method(this.directLogin(), 'type'),
			'label':			MochiKit.Base.method(this.directLogin(), 'label'),
			'formAttributes':	MochiKit.Base.method(this.directLogin(), 'formAttributes'),
			'inputValues':		MochiKit.Base.method(this.directLogin(), 'inputValues')
		});
		deferredResult.addCallback(MochiKit.Base.bind(function (someAttributes) {
			switch (someAttributes['type']) {
				case 'http_auth':
					this.runHttpAuthDirectLogin(aWindow, someAttributes);
					break;
				case 'simple_url':
					this.runSimpleUrlDirectLogin(aWindow, someAttributes);
					break;
				default:
					this.runSubmitFormDirectLogin(aWindow, someAttributes);
					break;
			}
		}, this));
		deferredResult.callback();

		return deferredResult;
	},

	//=============================================================================

	'run': function () {
		var newWindow;

		newWindow = window.open(Clipperz.PM.Strings.getValue('directLoginJumpPageUrl'), this.target());

		return this.runDirectLogin(newWindow);
	},
	
	//=============================================================================

	'test': function () {
		var iFrame;
		var newWindow;

		iFrame = MochiKit.DOM.createDOM('iframe');
		MochiKit.DOM.appendChildNodes(MochiKit.DOM.currentDocument().body, iFrame);

		newWindow = iFrame.contentWindow;

		return this.runDirectLogin(newWindow);
	},
	
	//=============================================================================
	__syntaxFix__: "syntax fix"
});

//-----------------------------------------------------------------------------

Clipperz.PM.UI.DirectLoginController.openDirectLogin = function (aDirectLogin) {
	var	runner;
	
	runner = new Clipperz.PM.UI.DirectLoginController({directLogin:aDirectLogin});
	return runner.run();
};

//-----------------------------------------------------------------------------

Clipperz.PM.UI.DirectLoginController.testDirectLogin = function (aDirectLogin) {
	var	runner;

	runner = new Clipperz.PM.UI.DirectLoginController({directLogin:aDirectLogin});
	return runner.test();
};

//-----------------------------------------------------------------------------
