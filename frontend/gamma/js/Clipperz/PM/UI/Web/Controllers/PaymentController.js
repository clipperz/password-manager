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

Clipperz.Base.module('Clipperz.PM.UI.Web.Controllers');

Clipperz.PM.UI.Web.Controllers.PaymentController = function(args) {
	Clipperz.PM.UI.Web.Controllers.PaymentController.superclass.constructor.apply(this, arguments);

	this._delegate = args['delegate'];
	this._referenceElement = null;
	this._paymentComponent = null;

	return this;
}

//MochiKit.Base.update(Clipperz.PM.UI.Web.Controllers.AppController.prototype, {
Clipperz.Base.extend(Clipperz.PM.UI.Web.Controllers.PaymentController, Object, {

	'toString': function() {
		return "Clipperz.PM.UI.Web.Controllers.PaymentController";
	},

	'delegate': function () {
		return this._delegate;
	},
	
	//-------------------------------------------------------------------------

	'referenceElement': function () {
		return this._referenceElement;
	},

	'setReferenceElement': function (anElement) {
		this._referenceElement = anElement;
 	},

	//-------------------------------------------------------------------------

	'paymentComponent': function () {
		if (this._paymentComponent == null) {
			var	paymentComponent = new Clipperz.PM.UI.Web.Components.PaymentComponent();

			MochiKit.Signal.connect(paymentComponent, 'cancel',		this, 'handleCancel');
			MochiKit.Signal.connect(paymentComponent, 'pay',		this, 'handlePaymentRequest');

			this._paymentComponent = paymentComponent;
		}
		
		return this._paymentComponent;
	},

	//-------------------------------------------------------------------------

	'run': function (anElement) {
		var	deferredResult;

		this.setReferenceElement(anElement);

		deferredResult = new Clipperz.Async.Deferred("PaymentController.run", {trace:false});
		deferredResult.addMethod(this.paymentComponent(), 'deferredShowModal', {openFromElement:this.referenceElement()});
		deferredResult.addMethod(this.delegate(), 'subscriptionOptions');
		deferredResult.addMethod(this.paymentComponent(), 'setSubscriptionOptions');
		deferredResult.callback();
		
		return deferredResult;
	},

	//=============================================================================

	'handlePaymentRequest': function (someOptions) {
console.log("PaymentController - handlePaymentRequest", someOptions['currency'], someOptions['type']);
		var	deferredResult;

		deferredResult = new Clipperz.Async.Deferred("PaymentController.handlePaymentRequest", {trace:false});
		deferredResult.addMethod(this.delegate(), 'getPaymentSubscriptionInfo');
		deferredResult.addCallback(function (subscriptionOptions) { return subscriptionOptions['subscription']});
		deferredResult.addMethod(this.delegate(), 'getPaymentAddress', someOptions['currency'], someOptions['type']);
		deferredResult.addMethod(this.paymentComponent(), 'showQrCode');
//		deferredResult.addCallback(function (subscriptionInfo) { console.log("SUBSCRIPTION INFO", subscriptionInfo); });
		deferredResult.callback();

		return deferredResult;
	},

	'handleCancel': function (anEvent) {
console.log("PaymentController - handleCancel");
	},
	
	//=============================================================================
	__syntaxFix__: "syntax fix"
});
