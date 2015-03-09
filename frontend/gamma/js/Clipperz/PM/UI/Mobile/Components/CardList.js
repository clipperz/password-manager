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

Clipperz.PM.UI.Mobile.Components.CardList = function(args) {
	args = args || {};

	Clipperz.PM.UI.Mobile.Components.CardList.superclass.constructor.apply(this, arguments);

	this._cardDetail = null;
	this.render();
	
	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Mobile.Components.CardList, Clipperz.PM.UI.Mobile.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Mobile.Components.CardList component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function () {
		var	headerElement;

		headerElement = MochiKit.Selector.findChildElements(this.element().parentNode, ['div[data-role=header]'])[0];
		this.append(this.element(),
			{tag:'div', /*cls:'scroll',*/ id:this.getId('listBox'), children:[
				{tag:'ul', /*cls:'rounded',*/ id:this.getId('list'), children:[
					{tag:'li', html:'loading'}
				]}
			]}
		);

		this.append(headerElement, 
//			{tag:'a', href:"#", 'data-icon':'gear', cls:'ui-btn-right', html:"Options" }
			{tag:'a', href:"#", id:this.getId('preferences'), cls:'ui-btn-right', html:"options" }
		);

		MochiKit.Signal.connect(this.getElement('preferences'), 'onclick', MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'showPreferences'));

/*
		this.append(this.element(), {tag:'div', cls:'cardList', children:[
			{tag:'div', cls:'toolbar', children:[
				{tag:'h1', html:"clipperz"},
//				{tag:'input', name:'search', type:'search', autocomplete:'off', placeholder:"search", id:this.getId('search')},
				{tag:'a', href:'#', id:'settings', cls:'button', html:"*"}
			]},
			{tag:'div', cls:'scroll', id:this.getId('listBox'), children:[
				{tag:'ul', cls:'rounded', id:this.getId('list'), children:[
					{tag:'li', html:'loading'}
				]}
			]}
		]});

		MochiKit.Signal.connect(this.getElement('list'),			'onclick',		this, 'cardSelectionHandler');
		MochiKit.Signal.connect(this.getElement('list'),			'ontouchstart',	this, 'cardSelectionHandler');
//		MochiKit.Signal.connect(this.getElement('cardListSearchForm'),	'onsubmit',		this,	'searchHandler');
//		MochiKit.Signal.connect(this.getElement('cardListSearchForm'),	'onkeydown',	this,	'searchHandler');
//		MochiKit.Signal.connect(this.getElement('cardListSearchForm'),	'onkeyup',		this,	'searchHandler');
		
//		MochiKit.Signal.connect(this.getElement('cardListPanel'),		'onclick',		this,	'cardListClickHandler');
//		MochiKit.Signal.connect('backButton',	'onclick',		this,	'backButtonClickHandler');

//		MochiKit.Style.hideElement('backButton');
//		MochiKit.Style.hideElement(this.getElement('cardDetail'));
*/
	},

	'showCards': function (someCards) {
		var	cardListElement;
		if (this.isFullyRendered() == false) {
			this.render();
		};

		cardListElement = this.getElement('list')

		cardInfo = {
			'_rowObject':			MochiKit.Async.succeed,
			'_reference':			MochiKit.Base.methodcaller('reference'),
			'_searchableContent':	MochiKit.Base.methodcaller('searchableContent'),
			'label':				MochiKit.Base.methodcaller('label'),
			'favicon':				MochiKit.Base.methodcaller('favicon')
		};

		deferredResult = new Clipperz.Async.Deferred("CardList.showCards", {trace:false});
		deferredResult.addCallback(MochiKit.Base.map, Clipperz.Async.collectResults("CardList.value - collectResults", cardInfo, {trace:false}));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addCallback(MochiKit.Base.methodcaller('sort', Clipperz.Base.caseInsensitiveKeyComparator('label')));
		deferredResult.addCallbackPass(MochiKit.DOM.replaceChildNodes, cardListElement);
//		deferredResult.addCallbackPass(MochiKit.DOM.removeElementClass, cardListElement, 'loading');
		deferredResult.addCallback(MochiKit.Base.map, MochiKit.Base.method(this, 'appendCardToList', cardListElement));
		deferredResult.callback(someCards);
	},

	'appendCardToList': function (aCardListElement, aCardInfo) {
		this.append(aCardListElement, {tag:'li', cls:'cardListItem arrow', cardreference:aCardInfo['_reference'], children:[
			{tag:'a', href:'#', html:aCardInfo['label'], children:[
//				{tag:'small', cls:'favicon', children:[{tag:'img', cls:'favicon', src:aCardInfo['favicon']}]}
			]}
		]});
	},

	'cardSelectionHandler': function (anEvent) {
		var listElement;
		var	cardReference;

		anEvent.preventDefault();

		listElement = anEvent.target();
		if (MochiKit.DOM.getNodeAttribute(listElement, 'cardreference') == null) {
			listElement = MochiKit.DOM.getFirstParentByTagAndClassName(anEvent.target(), tagName='li', className='cardListItem');
		}
		cardReference = MochiKit.DOM.getNodeAttribute(listElement, 'cardreference');
		//	TODO: Notify card with reference MochiKit.DOM.getNodeAttribute(listElement, 'cardreference') has been selected
		MochiKit.Signal.signal(this, 'selectedCard', cardReference);
	},

	//-------------------------------------------------------------------------
/*
	'searchHandler': function (anEvent) {
		if ((typeof(anEvent.key()) != 'undefined') && (anEvent.key().string == 'KEY_ENTER')) {				//	RETURN
			anEvent.preventDefault();
		} else {
			if ((typeof(anEvent.key()) != 'undefined') && (anEvent.key().string == 'KEY_ESCAPE')) {
				anEvent.target().value = "";
			}
	
			if (anEvent.type() == 'keyup') {
				MochiKit.Signal.signal(this, 'searchEvent', anEvent.target().value);
			}
		}
	},

	//-------------------------------------------------------------------------

	'update': function (someObjects) {
		var	cardListPanel;
		var i,c;

		cardListPanel = this.getElement('cardListPanel');
		cardListPanel.innerHTML = '';

		c = someObjects.length;
		
		for (i=0; i<c; i++) {
			this.append(cardListPanel, {tag:'li', cls:'cardListItem', id:('cardListItem_' + someObjects[i]['_reference']), children:[
				{tag:'img', src:(someObjects[i]['favicon'] ? someObjects[i]['favicon'] : 'data:application/octet-stream;charset=utf-8;base64,AAABAAEAFxcAAAEAGAD8BgAAFgAAACgAAAAXAAAALgAAAAEAGAAAAAAAAAAAABIXAAASFwAAAAAAAAAAAAD///////////////////////////////////////////////////////////////////////////////////////////9zAC////////////////////////////////////////////////////////////////////////////////////////////9pAG////////////////////////////////////////////////////////////////////////////////////////////9rAC////////////////////////////////////////////////////////////////////////////////////////////9yAHP////////////////////////IyMizs7O6urrq6ur////////////Ozs6zs7Ozs7Pq6ur///////////////////////8AAAD////////////////////V1dWXl5eXl5eXl5elpaX4+Pj////Ozs6Xl5eXl5eXl5eenp7///////////////////////8AAAD////////////////////Ozs6Xl5eXl5eXl5eXl5fBwcHq6uqenp6Xl5eXl5eXl5eXl5f///////////////////////8AAAD////////////////////j4+OXl5eXl5eXl5eXl5eXl5elpaWXl5eXl5eXl5eXl5ezs7P///////////////////////8AAAD////////////////////////IyMiXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eenp7x8fH////////////////////////////////////////////////////4+PilpaWXl5eXl5eXl5eXl5eXl5eXl5eXl5fOzs7////////////////////////////////////////////////////////q6uq6urqXl5eXl5eXl5eXl5eXl5eXl5eenp7V1dX4+Pj///////////////////////8AAAD////////////4+PjOzs6lpaWXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5e6urrj4+P///////////////8AAAD////////////BwcGXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5fx8fH///////////8AAAD///////////+zs7OXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eXl5fj4+P///////////8AAAD////////////IyMiXl5eXl5eXl5eXl5e6urqXl5eXl5eXl5eXl5esrKylpaWXl5eXl5eXl5eenp7x8fH///////////8AAAD////////////////Ozs7Ozs7V1dX4+Pj///+Xl5eXl5eXl5eXl5fOzs7////q6urOzs7Ozs7q6ur///////////////8AAAD///////////////////////////////////+Xl5eXl5eXl5eXl5fOzs7///////////////////////////////////8AAAD///////////////////////////////////+Xl5eXl5eXl5eXl5fOzs7///////////////////////////////////8AAAD///////////////////////////////////+Xl5eXl5eXl5eXl5fOzs7///////////////////////////////////8AAAD////////////////////////////////////IyMiXl5eXl5eenp7x8fH///////////////////////////////////8AAAD////////////////////////////////////////j4+Pj4+Px8fH///////////////////////////////////////8AAAD///////////////////////////////////////////////////////////////////////////////////////////8AAAD///////////////////////////////////////////////////////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAo=')},
				{tag:'a', id:('cardListReference_' + someObjects[i]['_reference']), href:'#', html:someObjects[i]['label']}
			]})
			
			MochiKit.Signal.connect('cardListItem_' + someObjects[i]['_reference'], 'onclick', this, 'cardListClickHandler');
		}

	},

	'cardListClickHandler': function (anEvent) {
		anEvent.preventDefault();

		if (/(cardListReference_|cardListItem_)/.test(anEvent.target().id)) {
			var cardListReference;
			
			cardListReference = anEvent.target().id.match(/(cardListReference_|cardListItem_)(.*)/)[2];
			MochiKit.Signal.signal(this, 'selectedCard', cardListReference);
		}
	},

	//=========================================================================

	'cardDetail': function (someData) {
		if (this._cardDetail == null) {
			this._cardDetail = new Clipperz.PM.UI.Mobile.Components.CardDetail({element:this.getElement('cardDetail')});
		}
		
		return this._cardDetail;
	},

	//-------------------------------------------------------------------------

	'removeCardDetail': function () {
		if (this._cardDetail != null) {
			this._cardDetail.remove();
			this._cardDetail = null;
		}
	},

	//=========================================================================

	'showCard': function (someData) {
		var	deferredResult;
		var	offset;

		offset = ((MochiKit.DOM.getNodeAttribute(MochiKit.DOM.currentDocument().body, 'orientation') == 'portrait') ? 320 : 480);
		this.cardDetail().render();
		this.cardDetail().setCardReference(someData['_reference']);
		MochiKit.Style.setElementPosition(this.cardDetail().element(), {x:offset});
		new MochiKit.Visual.Sequence([
//			new MochiKit.Visual.Move(this.cardDetail().element(), {x:offset, y:45, mode:'absolute', duration:0, sync:true}),
			new MochiKit.Visual.Parallel([
				new MochiKit.Visual.Move(this.getElement('cardList'),		{x:-offset, y:0, mode:'relative',	transition:MochiKit.Visual.Transitions.linear, sync:true}),
				new MochiKit.Visual.Move(this.getElement('cardDetail'),		{x:0, y:45,		 mode:'absolute',	transition:MochiKit.Visual.Transitions.linear, sync:true}),
//				new MochiKit.Visual.ScrollTo('toolbar', {sync:true}),
				MochiKit.Visual.appear  ('backButton',						{									transition:MochiKit.Visual.Transitions.linear, sync:true})
			], {duration:1, sync:true}),
			MochiKit.Visual.fade(this.getElement('cardList'), {duration:0, sync:true})
		], {})

		MochiKit.DOM.getElement('pageTitle').innerHTML = someData['title'];

		return true;
	},

	//-------------------------------------------------------------------------

	'showCardDetails': function (someData) {
		return this.cardDetail().showCardDetails(someData);
	},

	//=========================================================================

	'backButtonClickHandler': function (anEvent) {
		var	offset;

		anEvent.preventDefault();

		MochiKit.DOM.getElement('pageTitle').innerHTML = "cards";

		offset = ((MochiKit.DOM.getNodeAttribute(MochiKit.DOM.currentDocument().body, 'orientation') == 'portrait') ? 320 : 480);
		MochiKit.Style.setElementPosition(this.getElement('cardList'), {x:-offset});
		MochiKit.DOM.showElement(this.getElement('cardList'));

		new MochiKit.Visual.Parallel([
			new MochiKit.Visual.Move(this.getElement('cardList'),		{x:offset, y:0, mode:'relative',	transition:MochiKit.Visual.Transitions.linear, sync:true}),
			new MochiKit.Visual.Move(this.getElement('cardDetail'),		{x:offset, y:0, mode:'relative',	transition:MochiKit.Visual.Transitions.linear, sync:true}),
			MochiKit.Visual.fade    (this.getElement('cardDetail'),		{									transition:MochiKit.Visual.Transitions.linear, sync:true}),
			MochiKit.Visual.fade    ('backButton',						{									transition:MochiKit.Visual.Transitions.linear, sync:true})
		], {duration:1, afterFinish:MochiKit.Base.method(this, 'removeCardDetail')})
		
	},
*/
	//=========================================================================
	__syntaxFix__: "syntax fix"
});
