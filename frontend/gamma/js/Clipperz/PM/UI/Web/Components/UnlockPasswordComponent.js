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

Clipperz.PM.UI.Web.Components.UnlockPasswordComponent = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.UnlockPasswordComponent.superclass.constructor.apply(this, arguments);

	this._openFromElement			= args.openFromElement			|| null;
	this._onOkCloseToElement		= args.onOkCloseToElement		|| null;
	this._onCancelCloseToElement	= args.onCancelCloseToElement	|| null;

	this._progressBarComponent = null;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.UnlockPasswordComponent, Clipperz.PM.UI.Common.Components.SimpleMessagePanel, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.UnlockPasswordComponent component";
	},

	//-------------------------------------------------------------------------

	'getPassphrase': function () {
/*		var deferredResult;

		if (this.passphrase() == null) {
			this.deferredShowModal({'openFromElement': this.openFromElement()});
			deferredResult = this.deferred();
		} else {
			deferredResult = MochiKit.Async.succeed(this.passphrase());
		}

		return deferredResult;
*/

		this.deferredShowModal({'openFromElement': this.openFromElement()});

		return this.deferred();
	},

	//-------------------------------------------------------------------------

	'deferredShowModal': function (someParameters) {
		return Clipperz.Async.callbacks("UnlockPasswordComponent.deferredShowModal", [
			MochiKit.Base.bind(Clipperz.PM.UI.Web.Components.UnlockPasswordComponent.superclass.deferredShowModal, this, someParameters),
			MochiKit.Base.method(this, 'getElement', 'passphrase'),
			MochiKit.Base.methodcaller('focus')
		], {trace:false})
	},

	//-------------------------------------------------------------------------

	'openFromElement': function () {
		return this._openFromElement;
	},

	'onOkCloseToElement': function () {
		return this._onOkCloseToElement;
	},
	
	'onCancelCloseToElement': function () {
		return this._onCancelCloseToElement;
	},

	//-------------------------------------------------------------------------

	'renderSelf': function() {
		Clipperz.PM.UI.Web.Components.UnlockPasswordComponent.superclass.renderSelf.apply(this, arguments);
		
		this.append(this.getElement('container'), {tag:'div', cls:'passphrase', children: [
//			{tag:'form', id:this.getId('passphraseForm'), children:[
				{tag:'input', id:this.getId('passphrase'), type:'password', name:'passphrase', value:''}
//			]}
		]});
		
		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'userSuccessfullyLoggedIn', this, 'userSuccessfullyLoggedInHandler');
		MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'userLoginFailed',			this, 'userLoginFailedHandler');

		
//		MochiKit.Async.callLater(0.1, MochiKit.Base.method(this.getElement('passphrase'), 'focus'));
//		this.getElement('passphrase').select();
	},

	//-------------------------------------------------------------------------

	'showProgressBar': function () {
		var	progressBarElement;
		
		this.getElement('container').innerHTML = '';

		progressBarElement = this.append(this.getElement('container'), {tag:'div', cls:'progressBarWrapper'});
		this.addComponent(new Clipperz.PM.UI.Common.Components.ProgressBar({'element':progressBarElement}));
		
		this.setButtons([{text:"Cancel", result:'CANCEL'}]);
	},

	//-------------------------------------------------------------------------

	'showFailure': function () {
		this.setType('ALERT');
		this.setTitle("Login failed");
		this.setText("Wrong passphrase; the unlock has failed.");
		this.getElement('container').innerHTML = '';
		this.setButtons([{text:"Close", result:'CANCEL', isDefault:true}]);
	},

	//-------------------------------------------------------------------------

	'closeOk': function () {
		var passphrase;
		
		passphrase = this.getElement('passphrase').value;
		this.showProgressBar();
//		this.deferred().callback(passphrase);
		MochiKit.Async.callLater(0.5, MochiKit.Base.method(this.deferred(), 'callback', passphrase));
		this._deferred = null;
	},
	
	'closeCancel': function () {
		this.deferredHideModal({closeToElement:this.onCancelCloseToElement()});
		this.deferred().cancel();
		this._deferred = null;
	},

	//-------------------------------------------------------------------------

	'userSuccessfullyLoggedInHandler': function (anEvent) {
		this.deferredHideModal({closeToElement:this.onOkCloseToElement()});
	},
	
	'userLoginFailedHandler': function (anEvent) {
		this.showFailure();
	},

	//-------------------------------------------------------------------------
/*
	'deferredShow': function (someArgs, aResult) {
		this.deferredShowModal(someArgs);

//		this.deferred().addMethod(this, 'deferredHideModal', {closeToElement:someArgs.onOkCloseToElement });
//		this.deferred().addErrback (MochiKit.Base.method(this, 'deferredHideModal', {closeToElement:someArgs.onCancelCloseToElement }));
//		this.deferred().addCallback(MochiKit.Async.succeed, aResult);

		return this.deferred();
	},
*/
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
