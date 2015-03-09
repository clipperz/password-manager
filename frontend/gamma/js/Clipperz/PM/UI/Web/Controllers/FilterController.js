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

Clipperz.PM.UI.Web.Controllers.FilterController = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Controllers.FilterController.superclass.constructor.call(this, args);

	this._filterElements = [];
	this._filter = "";
	
	this._pendingSearchClicks = 0;
		
	return this;
};


Clipperz.Base.extend(Clipperz.PM.UI.Web.Controllers.FilterController, Object, {

	//-----------------------------------------------------------------------------

	'getFilter': function () {
		return this._filter;
	},

	'_setFilter': function (aFilterElement, aFilter) {
		if (aFilter != this._filter) {
			this._filter = aFilter;
			MochiKit.Signal.signal(this, 'filterUpdated', aFilter);
			this.updateFilterElements(aFilterElement, aFilter);
		}
	},
	
	'setFilter': function (aFilter) {
		this._setFilter(null, aFilter);
	},

	//-----------------------------------------------------------------------------

	'filterElements': function () {
		return this._filterElements;
	},

	'registerFilterElement': function (aFilterElement) {
//Clipperz.log("=== FilterController.registerFilterElement", aFilterElement);
		this._filterElements.push(aFilterElement);
		MochiKit.Signal.connect(aFilterElement, 'onkeydown', this, 'searchClickHandler');
		MochiKit.Signal.connect(aFilterElement, 'onfocus',  this, 'searchClickHandler');
	},
	
	'removeFilterElement': function (aFilterElement) {
		var i;
		var filterElements;
		for (i=0; i < filterElements; i++) {
			if (filterElements[i] == aFilterElement);
			filterElements.splice(i, 1);
// TODO unregister/disconnect filterElement	??	MochiKit.Signal.disconnect(this.grid().filterElement(), 'updateFilter', this.filterController(), 'handleUpdateFilter');
		}
	},
	
	'updateFilterElements': function (aSourceElement, aFilterString) {
		MochiKit.Iter.forEach(this.filterElements(),
			function (aFilterElement) {
				if (aFilterElement != aSourceElement) { 
					aFilterElement.value = aFilterString;
				}
			}
		);
		
		if (aSourceElement != null) {
			aSourceElement.focus();
		}
	},

	//-----------------------------------------------------------------------------

	'run': function () {
//Clipperz.log("=== FilterController.run");
	},

	//-----------------------------------------------------------------------------

	'pendingSearchClicks': function () {
		return this._pendingSearchClicks;	
	},

	'incrementPendingSearchClicks': function () {
		this._pendingSearchClicks++;
	},

	'decrementPendingSearchClicks': function () {
		this._pendingSearchClicks--;
	},

	//-----------------------------------------------------------------------------

	'searchClickHandler': function (anEvent) {
		if ((typeof(anEvent.key()) != 'undefined') && (anEvent.key().string == 'KEY_ENTER')) {
			anEvent.preventDefault();
		} else {
			var value;
			
			if ((typeof(anEvent.key()) != 'undefined') && (anEvent.key().string == 'KEY_ESCAPE')) {
				value = ""
			} else if ((typeof(anEvent.key()) != 'undefined') && (anEvent.key().string == 'KEY_ARROW_UP')) {
			} else if ((typeof(anEvent.key()) != 'undefined') && (anEvent.key().string == 'KEY_ARROW_DOWN')) {
			} else {
				value = null;
			}
			
			this.incrementPendingSearchClicks();
			MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, "searchClickDeferredHandler", anEvent.src(), value));
		}
	},

	//.........................................................................

	'searchClickDeferredHandler': function (aFilterElement, aValue) {
		if (aValue != null) {
			aFilterElement.value = aValue;
		}

		this.decrementPendingSearchClicks();
		if (this.pendingSearchClicks()==0) {
			this._setFilter(aFilterElement, aFilterElement.value);
		}
	},

	//-----------------------------------------------------------------------------	
	'syntaxFix': 'syntax fix'
});