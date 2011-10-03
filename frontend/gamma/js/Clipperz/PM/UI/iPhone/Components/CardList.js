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

Clipperz.PM.UI.iPhone.Components.CardList = function(args) {
	args = args || {};

	Clipperz.PM.UI.iPhone.Components.CardList.superclass.constructor.apply(this, arguments);

	this._cardDetail = null;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.iPhone.Components.CardList, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.iPhone.Components.CardList component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(), [
			{tag:'div', cls:'toolbar', id:'toolbar', children:[
				{tag:'h1', id:'pageTitle', html:"cards"},
				{tag:'a', id:'backButton', cls:'button', href:'#', html:"cards"}
			]},
			{tag:'div', cls:'cardList', id:this.getId('cardList'), children:[
				{tag:'form', title:'search', cls:'panel cardListSearchForm', id:this.getId('cardListSearchForm'), children:[
					{tag:'input', type:'search', name:'search', value:"", placeholder:"search", id:this.getId('searchField')}
				]},
				{tag:'ul', cls:'panel cardListPanel', id:this.getId('cardListPanel'), children:[]}
			]},
			{tag:'div', cls:'panel cardDetailPanel', id:this.getId('cardDetail')}
		]);

		MochiKit.Signal.connect(this.getElement('cardListSearchForm'),	'onsubmit',		this,	'searchHandler');
		MochiKit.Signal.connect(this.getElement('cardListSearchForm'),	'onkeydown',	this,	'searchHandler');
		MochiKit.Signal.connect(this.getElement('cardListSearchForm'),	'onkeyup',		this,	'searchHandler');
		
		MochiKit.Signal.connect(this.getElement('cardListPanel'),		'onclick',		this,	'cardListClickHandler');
		MochiKit.Signal.connect('backButton',	'onclick',		this,	'backButtonClickHandler');

		MochiKit.Style.hideElement('backButton');
		MochiKit.Style.hideElement(this.getElement('cardDetail'));
	},

	//-------------------------------------------------------------------------

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
//console.log("Showing detail for card named", cardListReference);
			MochiKit.Signal.signal(this, 'selectedCard', cardListReference);
		}
	},

	//=========================================================================

	'cardDetail': function (someData) {
		if (this._cardDetail == null) {
			this._cardDetail = new Clipperz.PM.UI.iPhone.Components.CardDetail({element:this.getElement('cardDetail')});
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

	//=========================================================================
	__syntaxFix__: "syntax fix"
});
