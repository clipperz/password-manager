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

Clipperz.PM.UI.Web.Components.PaymentComponent = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.PaymentComponent.superclass.constructor.apply(this, arguments);

	this._subscriptionOptions = null;
	this._currencies = null;
	this._selectedCurrency = 'mBTC';
	this._selectedOption = 'FAN';
	
//	this._displayMode = 'fixed'; 	//	'scrollable';
	
	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.PaymentComponent, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.PaymentComponent component";
	},

	//-------------------------------------------------------------------------

	'subscriptionOptions': function () {
		return this._subscriptionOptions;
	},
	
	'setSubscriptionOptions': function (someSubscriptionOptions) {
		this._subscriptionOptions = someSubscriptionOptions['options'];
		this._currencies = MochiKit.Base.keys(MochiKit.Base.values(MochiKit.Base.values(someSubscriptionOptions['options'])[0])[0]['value']);
		if (typeof(someSubscriptionOptions['latestActiveLevel']) != 'undefined') {
			this._selectedOption = someSubscriptionOptions['latestActiveLevel'];
		}

		this.renderSubscriptionOptions(this._subscriptionOptions);
	},

	//............................................................................

	'currencies': function () {
		return this._currencies;
	},
	
	'selectedCurrency': function () {
		return this._selectedCurrency;
	},
	
	'setSelectedCurrency': function (aValue) {
		this._selectedCurrency = aValue;
	},
	
	'setSelectedCurrencyIfNull': function (aValue) {
		if (this._selectedCurrency == null) {
			this._selectedCurrency = aValue;
		}
	},

	//............................................................................

	'selectedOption': function () {
		return this._selectedOption;
	},
	
	'setSelectedOption': function (aValue) {
		this._selectedOption = aValue;
	},
	
	//-------------------------------------------------------------------------
	
	'renderSelf': function () {
		this.append(this.element(), {tag:'div', cls:'PaymentDialog loading', id:this.getId('panel'), children: [
			{tag:'div', id:this.getId('subscriptions'), cls:'subscriptions'},
			{tag:'div', cls:'footer', children:[
				{tag:'div', id:this.getId('cancelButton'),	cls:'button cancel',	html:"cancel"},
				{tag:'div', id:this.getId('payButton'), 	cls:'button pay',		html:"pay"}
			]}
		]});
		
		MochiKit.Signal.connect(this.getElement('cancelButton'),	'onclick', this, 'handleCancel');
		MochiKit.Signal.connect(this.getElement('payButton'),		'onclick', this, 'handlePayButtonClick');
	},

	//-------------------------------------------------------------------------

	'priceOptionComponent': function (aCurrency, aSelectedOption, aSubscriptionInfo) {
		var	isSelected = (aSubscriptionInfo['code'] == aSelectedOption);
		
		return 	{tag:'li', 'data-type':'option', 'data-code':aSubscriptionInfo['code'], cls:isSelected ? 'selected' : '', children:[
			{tag:'h3', html:aSubscriptionInfo['name']},
			{tag:'h5', cls:'currency', html:aCurrency},
			{tag:'h5', cls:'value', html:aSubscriptionInfo['value'][aCurrency]}
		]};
	},
	
	'optionComparator': function (aOption, bOption) {
		return MochiKit.Base.compare(aOption['CLZ'], bOption['CLZ']);
	},

	'renderCurrency': function (aSelectedCurrency, aCurrency) {
		return {tag:'li', 'data-type':'currency', 'data-code':aCurrency, cls:aCurrency == aSelectedCurrency ? 'selected' : '', html:aCurrency}
	},
	
	'allSubscriptionOptions': function () {
		return MochiKit.Selector.findChildElements(this.getElement('subscriptions'), ["div.options li"]);
	},

	'allCurrencyOptions': function () {
		return MochiKit.Selector.findChildElements(this.getElement('subscriptions'), ["ul.currencies li"]);
	},

	'renderSubscriptionOptions': function (someSubscriptionOptions, overwrite) {
		var	currentCurrency = this.selectedCurrency();
		var	currencyRenderer = MochiKit.Base.partial(this.renderCurrency, this.selectedCurrency())
		var	subscriptionOptionRenderer = MochiKit.Base.partial(this.priceOptionComponent, this.selectedCurrency(), this.selectedOption());
		var	self = this;

		if (overwrite) {
			MochiKit.Base.map(MochiKit.Signal.disconnectAll, this.allCurrencyOptions());
			MochiKit.Base.map(MochiKit.Signal.disconnectAll, this.allSubscriptionOptions());
			this.getElement('subscriptions').innerHTML = '';
		}

		this.append(this.getElement('subscriptions'),  {tag:'ul', cls:'currencies', children:MochiKit.Base.map(currencyRenderer, this.currencies())});
	
		this.append(this.getElement('subscriptions'),  {tag:'div', cls:'options', children:[
			{tag:'div', cls:'yearly', children:[
				{tag:'h1', html:"Yearly"},
				{tag:'ul', children:MochiKit.Base.map(subscriptionOptionRenderer, MochiKit.Base.values(someSubscriptionOptions['P1Y']).sort(this.optionComparator))}
			]},
			{tag:'div', cls:'perpetual', children:[
				{tag:'h1', html:"Perpetual"},
				{tag:'ul', children:MochiKit.Base.map(subscriptionOptionRenderer, MochiKit.Base.values(someSubscriptionOptions['--']).sort(this.optionComparator))}
			]}
		]});
		
		MochiKit.Base.map(function (aLi) { MochiKit.Signal.connect(aLi, 'onclick', self, 'handleChangeCurrencySelection') }, this.allCurrencyOptions());
		MochiKit.Base.map(function (aLi) { MochiKit.Signal.connect(aLi, 'onclick', self, 'handleChangeOptionSelection') },   this.allSubscriptionOptions());
	},

	//-------------------------------------------------------------------------

	'handleChangeOptionSelection': function (anEvent) {
		var	code = MochiKit.DOM.getNodeAttribute(anEvent.src(), 'data-code');

		if (code != this.selectedOption()) {
			MochiKit.Base.map(function (aLi) { MochiKit.DOM.removeElementClass(aLi, 'selected') }, this.allSubscriptionOptions());
			MochiKit.DOM.addElementClass(anEvent.src(), 'selected');
			this.setSelectedOption(code);
		}
	},
	
	'handleChangeCurrencySelection': function (anEvent) {
		var	code = MochiKit.DOM.getNodeAttribute(anEvent.src(), 'data-code');

		if (code != this.selectedCurrency()) {
			MochiKit.Base.map(function (aLi) { MochiKit.DOM.removeElementClass(aLi, 'selected') }, this.allCurrencyOptions());
			MochiKit.DOM.addElementClass(anEvent.src(), 'selected');
			this.setSelectedCurrency(code);
			this.renderSubscriptionOptions(this.subscriptionOptions(), true);
		}
	},

	'handlePayButtonClick': function (anEvent) {
		console.log("PAY: ", this.selectedOption(), this.selectedCurrency());
		MochiKit.Signal.signal(this, 'pay', {currency:this.selectedCurrency(), type:this.selectedOption()});
	},

	'handleCancel': function (anEvent) {
console.log("CANCEL");
		MochiKit.Signal.signal(this, 'cancel');
	},

/*	
	'displayMode': function () {
		return this._displayMode;
	},
	
	'setDisplayMode': function (aValue) {
		this._displayMode = aValue;
	},
*/
	
	//=========================================================================

	'showQrCode': function (someParameters) {
console.log("SHOW QR CODE", someParameters);
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});
