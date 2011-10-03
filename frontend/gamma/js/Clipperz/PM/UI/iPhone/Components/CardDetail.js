/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz's Javascript Crypto Library.
Javascript Crypto Library provides web developers with an extensive
and efficient set of cryptographic functions. The library aims to
obtain maximum execution speed while preserving modularity and
reusability.
For further information about its features and functionalities please
refer to http://www.clipperz.com

* Javascript Crypto Library is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Javascript Crypto Library is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Javascript Crypto Library.  If not, see
  <http://www.gnu.org/licenses/>.

*/

Clipperz.Base.module('Clipperz.PM.UI.iPhone.Components');

Clipperz.PM.UI.iPhone.Components.CardDetail = function(args) {
	args = args || {};

	Clipperz.PM.UI.iPhone.Components.CardDetail.superclass.constructor.apply(this, arguments);

	this._cardReference = null;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.iPhone.Components.CardDetail, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.iPhone.Components.CardDetail component";
	},

	//-------------------------------------------------------------------------

	'cardReference': function () {
		return this._cardReference;
	},
	
	'setCardReference': function (aValue) {
		this._cardReference = aValue;
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(), [
			{tag:'div', cls:'cardDetail', id:this.getId('cardDetail'), children:[
				{tag:'div', id:this.getId('progressBar')} //,
//				{tag:'h1', cls:'loading', html:"loading"}
			]}
		]);

		this.addComponent(new Clipperz.PM.UI.Common.Components.ProgressBar({'element':this.getElement('progressBar')}));
		MochiKit.Signal.signal(Clipperz.PM.UI.Common.Controllers.ProgressBarController.defaultController, 'updateProgress', 0);
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
		
//console.log("TOGGLE");
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
*/
	//=========================================================================

	'directLoginClickHandler': function (anEvent) {
		anEvent.preventDefault();
		
		if (/(directLogin_)/.test(anEvent.src().id)) {
			var directLoginReference;
			
			directLoginReference = anEvent.src().id.match(/(directLogin_)(.*)/)[2];
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectedDirectLogin', {cardReference:this.cardReference(), directLoginReference:directLoginReference});
		}
	},

	//=========================================================================

	__syntaxFix__: "syntax fix"
});
