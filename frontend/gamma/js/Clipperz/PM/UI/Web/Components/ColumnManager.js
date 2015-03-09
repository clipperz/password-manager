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

//#############################################################################

Clipperz.PM.UI.Web.Components.ColumnManager = function(args) {
	args = args || {};
	Clipperz.PM.UI.Web.Components.ColumnManager.superclass.constructor.call(this, args);

	this._name		 = args.name		|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._selector	 = args.selector	|| Clipperz.Base.exception.raise('MandatoryParameter');;
	this._label		 = args.label		|| null;
	this._isSortable = args.sortable	|| false;
	this._comparator = args.comparator	|| null;
	this._sorted	 = args.sorted		|| 'UNSORTED';	//	'ASCENDING' | 'DESCENDING' | 'UNSORTED'
	this._cssClass	 = args.cssClass	|| '';

	this._signalIdentifiers = [];
	
	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.ColumnManager, Clipperz.PM.UI.Common.Components.BaseComponent, {

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.ColumnManager - " + this._name;
	},
	
	'name': function () {
		return this._name;
	},

	'label': function () {
		return this._label;
	},
	
	'selector': function () {
		return this._selector;
	},
	
	'comparator': function() {
		return this._comparator;
	},
	
	'cssClass': function() {
		return this._cssClass;
	},

	//-------------------------------------------------------------------------

	'isSortable': function () {
		return this._isSortable;
	},

	//-------------------------------------------------------------------------

	'sorted': function () {
		return this._sorted;
	},
	
	'isSorted': function () {
		return (this.sorted() != 'UNSORTED');
	},
	
	'setSorted': function(aValue) {
		this._sorted = aValue;
		this.updateSortIcon();
	},

	//-------------------------------------------------------------------------

	'signalIdentifiers': function () {
		return this._signalIdentifiers;
	},

	'resetSignalIdentifiers': function () {
		this._signalIdentifiers = [];
	},

	//-------------------------------------------------------------------------

	'disconnectRowsSignals': function () {
		MochiKit.Base.map(MochiKit.Signal.disconnect, this.signalIdentifiers());
		this.resetSignalIdentifiers();
	},

	'connectEvent': function () {
		var ident;
		
		ident = MochiKit.Signal.connect.apply(null, arguments);
		this.signalIdentifiers().push(ident);
	},

	//-------------------------------------------------------------------------

	'renderHeader': function(aTRElement) {
		var	thElement;

		thElement = Clipperz.DOM.Helper.append(aTRElement, {tag:'th', cls:(this.cssClass() + 'TH'), id:this.getId('sortLink'), children:[
			{tag:'span', html:this.label() ? this.label() : '&nbsp;'}
		]});
		
		if (this.isSortable()) {
			Clipperz.DOM.Helper.append(thElement, {tag:'span', cls:'sortable', children:[
				{tag:'a', href:'#'/*, id:this.getId('sortLink')*/, html:'&nbsp;'}				
			]});

			MochiKit.DOM.addElementClass(thElement, 'sortable');
			MochiKit.Signal.connect(thElement, 'onclick', this, 'handleClickOnSortingCriteria');
		};
		
		this.updateSortIcon();
	},

	//-------------------------------------------------------------------------

	'toggleSorting': function () {
		var result;
		switch (this.sorted()) {
			case 'UNSORTED':
				result = 'ASCENDING';
				break;
			case 'ASCENDING':
				result = 'DESCENDING';
				break;
			case 'DESCENDING':
				result = 'ASCENDING';
				break;
			default:
				result = 'UNSORTED';
				break;
		}
		
		this.setSorted(result);
		
		return result;
	},

	//-------------------------------------------------------------------------

	'sortElementClass': function () {
		return this.sorted().toLowerCase();
	},

	//-------------------------------------------------------------------------

	'updateSortIcon': function () {
		if (this.isSortable()) {
			MochiKit.DOM.removeElementClass(this.getId('sortLink'), 'ascending');
			MochiKit.DOM.removeElementClass(this.getId('sortLink'), 'descending');
			MochiKit.DOM.removeElementClass(this.getId('sortLink'), 'unsorted');

			MochiKit.DOM.addElementClass(this.getId('sortLink'), this.sortElementClass());
		}
	},

	//-------------------------------------------------------------------------

	'renderCell': function(aRowElement, anObject) {
		Clipperz.DOM.Helper.append(aRowElement, {tag:'td', cls:this.cssClass(), children:[{tag:'span', html:anObject[this.name()]}]});
	},

	//-----------------------------------------------------

	'handleClickOnSortingCriteria': function (anEvent) {
		anEvent.preventDefault();
		MochiKit.Signal.signal(this, 'sort', this);
	},

	//-----------------------------------------------------
	'__syntax_fix__' : 'syntax fix'
	
});

