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

Clipperz.PM.UI.Web.Components.GridComponent = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.GridComponent.superclass.constructor.apply(this, arguments);

	this._columnsManagers = args.columnsManagers;

	this._rowsObjects = [];
	this._noRowsGridComponent = null;
	
	this._slots = {
		'headerSlot':	this.getId('headerSlot')
	};

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.GridComponent, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.GridComponent component";
	},

	//-------------------------------------------------------------------------

	'rows': function () {
		throw Clipperz.Base.exception.AbstractMethod;
//		return this._rows;
	},

	//-------------------------------------------------------------------------

	'columnsManagers': function () {
		return this._columnsManagers;
	},
	
	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(), [
			{tag:'div', cls:'header', children:[
				{tag:'form', id:this.getId('searchForm'), cls:'search', children:[
					{tag:'div', cls:'search', children:[
						{tag:'input', type:'text', id:this.getId('search'), cls:'search', placeholder:"search", name:'textToSearch'/*, value:"clipperz"*/}
					]},
					{tag:'div', cls:'clearSearchButton', id:this.getId('clearSearch')},
//					{tag:'input', type:'button', cls:'searchButton', name:'searchButton', value:"search"},
					{tag:'div', cls:'headerSlot', id:this.getId('headerSlot')}
				]}
			]},
			{tag:'div', cls:'body', children:[
				{tag:'div', cls:'rows', id:this.getId('rows'), children:[
					{tag:'table', cellpadding:'0', cellspacing:'0', cls:'rows', children:[
						{tag:'thead', children:[
							{tag:'tr', id:this.getId('thead_tr'), children:[]}
						]},
						{tag:'tbody', id:this.getId('gridRows'), children:[]}
					]}
				]},
				{tag:'div', cls:'noRowsBlock', id:this.getId('noRowsBlock'), children:[]}
			]},
			{tag:'div', cls:'footer'}
		]);

		this.renderHeader();
		MochiKit.Signal.connect(this.getId('clearSearch'), 'onclick', this, 'clearSearchHandler');
	},

	//-------------------------------------------------------------------------

	'renderHeader': function () {
		var headerElement;
		
		headerElement = this.getElement('thead_tr');
		headerElement.innerHTML = "";

		MochiKit.Iter.forEach(this.columnsManagers(), function (aColumnManager) {
			aColumnManager.renderHeader(headerElement);
		});
	},

	//-------------------------------------------------------------------------

	'update': function (someObjects) {
		this._rowsObjects = someObjects
		this.refresh();
		this.focus();
	},

	'focus': function () {
		this.getElement('search').focus();
	},

	//-------------------------------------------------------------------------

	'startSearch': function () {
		MochiKit.DOM.addElementClass(this.getElement('search'), 'running');
	},

	'endSearch': function () {
		MochiKit.DOM.removeElementClass(this.getElement('search'), 'running');
	},

	//-------------------------------------------------------------------------

	'disconnectColumnManagersRowsSignals': function () {
		MochiKit.Iter.forEach(this.columnsManagers(), function (aColumnManager) {
			aColumnManager.disconnectRowsSignals();
		});
	},

	//-------------------------------------------------------------------------

	'refresh': function () {
		var gridRowsElement;
		var rowClass;

		this.disconnectColumnManagersRowsSignals();

		{
			MochiKit.DOM.removeElementClass(this.getElement('search'), 'disabled');
//			MochiKit.DOM.setNodeAttribute(this.getElement('search'), 'disabled', null);
			MochiKit.DOM.removeElementClass(this.element(), 'empty');
			MochiKit.DOM.removeElementClass(this.element(), 'noRows');
		}

		
		gridRowsElement = this.getElement('gridRows');
		gridRowsElement.innerHTML = "";

		MochiKit.DOM.removeElementClass(this.element(), 'empty');
		
		rowClass = 'odd';
		MochiKit.Iter.forEach(this._rowsObjects, MochiKit.Base.bind(function (aRowObject) {
			var cardRowElement;

			cardRowElement = this.append(gridRowsElement, {tag:'tr', id:this.getId(aRowObject['_reference']), cls:rowClass});
			MochiKit.Iter.forEach(this.columnsManagers(), function (aColumnManager) {
				aColumnManager.renderCell(cardRowElement, aRowObject);
			});
			
			rowClass = (rowClass == 'odd') ? 'even' : 'odd';
		}, this));
	},

	//-----------------------------------------------------------------------------
	
	'filterElement': function () {
		return this.getElement('search');
	},

	//-------------------------------------------------------------------------
	
	'shouldShowElementWhileRendering': function () {
		return false;
	},

	//-------------------------------------------------------------------------

	'selectRow': function (aRowObject) {
		MochiKit.DOM.addElementClass(this.getId(aRowObject['_reference']), 'selected');
	},

	'unselectRow': function (aRowObject) {
		MochiKit.DOM.removeElementClass(this.getId(aRowObject['_reference']), 'selected');
	},

	//-------------------------------------------------------------------------
/*	
	'passOpenDirectLogin': function(aDirectLoginReferenceId) {
		MochiKit.Signal.signal(this, 'openDirectLogin', aDirectLoginReferenceId);
	},
*/	
	//-------------------------------------------------------------------------

	'clearSearchHandler': function (anEvent) {
		var searchElement;

		anEvent.preventDefault();

		searchElement = this.getElement('search');
		searchElement.value = "";
		searchElement.focus();
	},

	//-------------------------------------------------------------------------

	'drawEmpty': function () {
		this.disconnectColumnManagersRowsSignals();
		MochiKit.DOM.addElementClass(this.getElement('search'), 'disabled');
//		MochiKit.DOM.setNodeAttribute(this.getElement('search'), 'disabled', 'disabled');

		gridRowsElement = this.getElement('gridRows');
		gridRowsElement.innerHTML = "";
		MochiKit.DOM.addElementClass(this.element(), 'empty');
	},

	//-------------------------------------------------------------------------

	'setNoRowsGridComponent': function (aComponent) {
		this.removeNoRowsGridComponent();
		this._noRowsGridComponent = aComponent;
		
		this.disconnectColumnManagersRowsSignals();
		MochiKit.DOM.addElementClass(this.getElement('search'), 'disabled');
//		MochiKit.DOM.setNodeAttribute(this.getElement('search'), 'disabled', 'disabled');

		gridRowsElement = this.getElement('gridRows');
		gridRowsElement.innerHTML = "";
		MochiKit.DOM.addElementClass(this.element(), 'noRows');
		
		if (aComponent != null) {
			aComponent.renderInNode(this.getElement('noRowsBlock'));
		}
	},

	'removeNoRowsGridComponent': function () {
		if (this._noRowsGridComponent != null) {
			this._noRowsGridComponent.remove();
			this._noRowsGridComponent = null;
		}
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
