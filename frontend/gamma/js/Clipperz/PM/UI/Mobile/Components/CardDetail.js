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

Clipperz.Base.module('Clipperz.PM.UI.Mobile.Components');

Clipperz.PM.UI.Mobile.Components.CardDetail = function(args) {
	args = args || {};

	Clipperz.PM.UI.Mobile.Components.CardDetail.superclass.constructor.apply(this, arguments);

//	this._cardReference = null;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Mobile.Components.CardDetail, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Mobile.Components.CardDetail component";
	},

	//-------------------------------------------------------------------------
/*
	'cardReference': function () {
		return this._cardReference;
	},
	
	'setCardReference': function (aValue) {
		this._cardReference = aValue;
	},
*/
	//-------------------------------------------------------------------------

	'renderSelf': function () {
		this.append(this.element(), {tag:'div', cls:'cardDetail', children:[
			{tag:'div', cls:'toolbar', children:[
				{tag:'a', href:'#', cls:'back', html:"List"},
				{tag:'h1', id:this.getId('cardTitle'), html:"…"}
			]},
			{tag:'div', cls:'scroll', id:this.getId('cardDetails'), children:[
			]}
		]});
	},
/*
	'renderSelf': function() {
		this.append(this.element(), [
			{tag:'div', cls:'cardDetail', id:this.getId('cardDetail'), children:[
				{tag:'div', id:this.getId('progressBar')} //,
			]}
		]);

		this.addComponent(new Clipperz.PM.UI.Common.Components.ProgressBar({'element':this.getElement('progressBar')}));
		MochiKit.Signal.signal(Clipperz.PM.UI.Common.Controllers.ProgressBarController.defaultController, 'updateProgress', 0);
	},
*/

	'setTitle': function (aValue) {
		this.getElement('cardTitle').innerHTML = aValue;
	},

	'fieldListElement': function () {
		var	result;

		result = this.getElement('fieldList');
		if (result == null) {
			result = this.append(this.getElement('cardDetails'), {tag:'ul', cls:'rounded', id:this.getId('fieldList')});
		}

		return result;
	},

	'renderFieldValues': function (someFieldValues) {
		var	fieldClass;

		if ((someFieldValues['actionType'] != 'NONE') || (someFieldValues['label'] != '') && (someFieldValues['value'] != '')) {
			if (someFieldValues['isHidden'] == true) {
				fieldClass = 'password';
			} else {
				fieldClass = '';
			}

			this.append(this.fieldListElement(), {tag:'li', cls:'cardField', children:[
				{tag:'a', href:'#', cls:fieldClass, html:someFieldValues['value'], children:[
					{tag:'small', cls:'label', html:someFieldValues['label']}
				]}
			]})
		}
	},

	'addField': function (aField) {
		var deferredResult;
		var	fieldValues;

		fieldValues = {};
		deferredResult = new Clipperz.Async.Deferred("CardDetail.addField", {trace:false});
		deferredResult.addMethod(aField, 'label');
		deferredResult.addCallback(function (aValue) { fieldValues['label'] = aValue; });
		deferredResult.addMethod(aField, 'value');
		deferredResult.addCallback(function (aValue) { fieldValues['value'] = aValue; });
		deferredResult.addMethod(aField, 'actionType');
		deferredResult.addCallback(function (aValue) { fieldValues['actionType'] = aValue; });
		deferredResult.addMethod(aField, 'isHidden');
		deferredResult.addCallback(function (aValue) { fieldValues['isHidden'] = aValue; });
		deferredResult.addMethod(this, 'renderFieldValues', fieldValues);
		deferredResult.callback();

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'directLoginElement': function () {
		var	result;

		result = this.getElement('directLoginList');
		if (result == null) {
			this.append(this.getElement('cardDetails'), {tag:'h2', html:"Direct login"});
			result = this.append(this.getElement('cardDetails'), {tag:'ul', cls:'rounded', id:this.getId('directLoginList')});
		}

		return result;
	},

	'addDirectLogin': function (aDirectLogin) {
		this.append(this.directLoginElement(), {tag:'li', cls:'directLogin forward', children:[
			{tag:'a', href:'#', html:"direct login", children:[
				{tag:'small', cls:'favicon', children:[{tag:'img', cls:'favicon', src:'http://www.clipperz.com/favicon.ico'}]}
			]}
		]})

	},

	//=========================================================================

	'showCard': function (aCard) {
		var deferredResult;

//		this.render();

		deferredResult = new Clipperz.Async.Deferred("CardDetail.showCard", {trace:false});
		deferredResult.addMethod(aCard, 'label');
		deferredResult.addMethod(this, 'setTitle');

		deferredResult.addMethod(aCard, 'fields');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.method(this, 'addField'));

		deferredResult.addMethod(aCard, 'directLogins');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.method(this, 'addDirectLogin'));


		deferredResult.callback();

		return deferredResult;
//		return Clipperz.Async.callbacks("CardDialogController.updateComponentState", [
//			MochiKit.Base.method(this.record(), 'hasPendingChanges'),
//			MochiKit.Base.method(this.cardDialogComponent(), 'setShouldEnableSaving'),
//        	
//			MochiKit.Base.method(this.record(), 'label'),
//			MochiKit.Base.method(this.cardDialogComponent(), 'setTitle'),
//			MochiKit.Base.method(this.record(), 'notes'),
//			MochiKit.Base.method(this.cardDialogComponent(), 'setNotes'),
//        	
//			MochiKit.Base.method(this.record(), 'fields'),
//			MochiKit.Base.values,
//			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this, 'addCardDialogComponentWithField')),
//
//			MochiKit.Base.method(this.record(), 'directLogins'),
//			MochiKit.Base.values,
//			MochiKit.Base.partial(MochiKit.Base.map, MochiKit.Base.method(this, 'addCardDialogComponentWithDirectLogin')),
//
//			MochiKit.Base.method(this.cardDialogComponent(), 'resetNewFieldInputs'),
//			MochiKit.Base.noop
//		], {trace:false});

	},

	//=========================================================================

	'showCardDetails': function (someData) {
		this.element().innerHTML = '';
		this.append(this.element(), [
			{tag:'fieldset', id:this.getId('fields'), children:MochiKit.Base.map(function (aFieldData) {
				return {tag:'div', cls:'row', children:[
					{tag:'label', html:aFieldData['label']},
//					{tag:'span', cls:('fieldValue ' + (aFieldData['isHidden']	? 'password' : 'text')), html:aFieldData['value']}
					{tag:'div', cls:('fieldValue ' + (aFieldData['isHidden']	? 'password' : 'text')), children:[
						{tag:'div', children:[{tag:'p', html:aFieldData['value']}]}
					]}
//					{tag:'input', type:'text', cls:('fieldValue ' + (aFieldData['isHidden']	? 'password' : 'text')), value:aFieldData['value'], disabled:true}

				]}
			}, someData['fields'])}
		]);

		MochiKit.Iter.forEach(MochiKit.Selector.findChildElements(this.element(), ['span.password']), MochiKit.Base.bind(function (aPasswordElement) {
			MochiKit.Signal.connect(aPasswordElement, 'onclick', function (anEvent) { alert(MochiKit.DOM.scrapeText(anEvent.src())); })
		}, this));

		if (someData['directLogins'].length > 0) {
			this.append(this.element(), [
				{tag:'h2', html:"Direct logins"},
				{tag:'fieldset', id:this.getId('directLogins'), children:MochiKit.Base.map(function (aDirectLoginData) {
					return {tag:'div', cls:'row', id:('directLogin_' + aDirectLoginData['_reference']), children:[
						{tag:'img', cls:'favicon', src:aDirectLoginData['favicon']},
//						{tag:'input', cls:'directLogin', disabled:'disabled', type:'text', name:aDirectLoginData['label'], value:aDirectLoginData['label']}
						{tag:'span', cls:'directLogin', html:aDirectLoginData['label']}
					]}
				}, someData['directLogins'])}
			]);

			MochiKit.Base.map(MochiKit.Base.bind(function (aRowNode) {
					MochiKit.Signal.connect(aRowNode, 'onclick', this, 'directLoginClickHandler');
				}, this),
				MochiKit.Selector.findChildElements(this.getElement('directLogins'), ['div.row'])
			)
		};
		
		if (someData['notes'] != '') {
			this.append(this.element(), [
				{tag:'h2', html:"Notes"},
				{tag:'fieldset', id:this.getId('fieldset'), children:[
					{tag:'div', cls:'row notes', children:[
						{tag:'span', html:someData['notes']}
					]}
				]}
			]);
		};

		return true;
	},

	//-------------------------------------------------------------------------
/*
	'toggleClickHandler': function (anEvent) {
		var	nextState;
		var	fieldValue;
		
		anEvent.preventDefault;
		fieldValue = MochiKit.Selector.findChildElements(anEvent.src().parentNode.parentNode, ['span.password'])[0];

		nextState = (MochiKit.DOM.getNodeAttribute(anEvent.src(), 'toggled') != 'true');
		if (nextState) {
			MochiKit.DOM.removeElementClass(fieldValue, 'clear');
		} else {
			MochiKit.DOM.addElementClass(fieldValue, 'clear');
		}
		
		MochiKit.DOM.setNodeAttribute(anEvent.src(), 'toggled', nextState);
	},
* /
	//=========================================================================
/*
	'directLoginClickHandler': function (anEvent) {
		anEvent.preventDefault();
		
		if (/(directLogin_)/.test(anEvent.src().id)) {
			var directLoginReference;
			
			directLoginReference = anEvent.src().id.match(/(directLogin_)(.*)/)[2];
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectedDirectLogin', {cardReference:this.cardReference(), directLoginReference:directLoginReference});
		}
	},
*/
	//=========================================================================

	__syntaxFix__: "syntax fix"
});
