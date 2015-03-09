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

Clipperz.PM.UI.Web.Controllers.GridController = function(args) {
	args = args || {};
	
	Clipperz.PM.UI.Web.Controllers.GridController.superclass.constructor.call(this, args);
	
	this._grid = null;
	this._user = null;
	this._sortedColumnManager = null;
	this._cachedObjects = null;
	this._filterController = args.filterController || null;

	this._deferredDisplaySelectedRowsInvocation = null;
	
	return this;
};

Clipperz.Base.extend(Clipperz.PM.UI.Web.Controllers.GridController, Object, {

	'toString': function() {
		return "Clipperz.PM.UI.Web.Controllers.GridController";
	},

	//-----------------------------------------------------------------------------

	'createGrid': function () {
		throw Clipperz.Base.exception.AbstractMethod;
	},
	
	'setupWithGrid': function (aGrid) {
		this._grid = aGrid;
		
		if (this._grid != null) {
			MochiKit.Iter.forEach(this.columnsManagers(), function (aColumnManager) {
				if (aColumnManager.isSortable()) {
					if (aColumnManager.isSorted()) {
						this.setSortedColumnManager(aColumnManager);
					}
					MochiKit.Signal.connect(aColumnManager, 'sort', this, 'handleColumnManagerSort');
				}
				MochiKit.Signal.connect(aColumnManager, 'selectRow', this, 'handleColumnManagerSelectRow');
				MochiKit.Signal.connect(aColumnManager, 'unselectRow', this, 'handleColumnManagerUnselectRow');
			}, this);
		}
	},

	'grid': function() {
		if (this._grid == null) {
			this.setupWithGrid(this.createGrid());
		}
		
		return this._grid;
	},
	
	'filterController': function () {
//Clipperz.log('GridController.filterController >>>', this._filterController);
		if (this._filterController == null) {
			this._filterController = new Clipperz.PM.UI.Web.Controllers.FilterController();
		}
//Clipperz.log('GridController.filterController <<<', this._filterController);
		return this._filterController;
	},

	//-----------------------------------------------------------------------------

	'columnsManagers': function () {
		return this.grid().columnsManagers();
	},

	'columnManagerWithName': function (aName) {
		var	managers;
		var result;
		
		managers = MochiKit.Base.filter(function (aManager) { return aManager.name() == aName; } , this.columnsManagers());
		
		if (managers.length == 1) {
			result = managers[0];
		} else if (managers.length == 0) {
			result = null;
		} else {
			throw "WTF!!!";
		}
		
		return result;
	},

	'sortedColumnManager': function () {
		return this._sortedColumnManager;
	},
	
	'setSortedColumnManager': function(aValue) {
		if (aValue.sorted() != 'UNSORTED') {
			this._sortedColumnManager = aValue;
		} else {
			this._sortedColumnManager = null;
		}
	},

	//-----------------------------------------------------------------------------

	'handleColumnManagerSort': function(aSelectedColumnManager) {
		MochiKit.Iter.forEach(this.columnsManagers(), function(aColumnManager) {
			if (aSelectedColumnManager != aColumnManager) {
				if (aColumnManager.isSortable()) {
					aColumnManager.setSorted('UNSORTED');
				}
			}
		});
		
		aSelectedColumnManager.toggleSorting();
		this.setSortedColumnManager(aSelectedColumnManager);

		this.displaySelectedRows(this.filterController().getFilter());
	},

	'handleColumnManagerSelectRow': function (aRowObject) {
		this.grid().selectRow(aRowObject);
	},

	'handleColumnManagerUnselectRow': function (aRowObject) {
		this.grid().unselectRow(aRowObject);
	},
	
	//-----------------------------------------------------------------------------

	'handleFilterUpdated': function (aFilter) {
		if (this.grid().isActive()) {
			this.displaySelectedRows(aFilter);
		}
	},

	//-----------------------------------------------------------------------------
	//	TODO: relying on user() in GridController, bad code smell :|
	//	mhh: a controller should have access to business logic object too. Otherwise it will fail its controller role. [Giulio Cesare]

	'setUser': function(anUser) {
		this._user = anUser;
	},
	
	'user': function() {
		return this._user;
	},

	//-----------------------------------------------------------------------------

	'run': function(args) {
//Clipperz.log("=== GridController.run");
		var deferredResult;

		this.setUser(args.user);
		args.slot.setContent(this.grid());
		this.filterController().registerFilterElement(this.grid().filterElement());
		MochiKit.Signal.connect(this.filterController(), 'filterUpdated', this, 'handleFilterUpdated');
		
		return this.displaySelectedRows();
	},

	//-----------------------------------------------------------------------------

	'handleGenericError': function(anError) {
		var result;
		
		if (anError instanceof MochiKit.Async.CancelledError) {
			result = anError;
		} else {
Clipperz.log("## GridController - GENERIC ERROR" + "\n" + "==>> " + anError + " <<==\n" + anError.stack);
			result = new MochiKit.Async.CancelledError(anError);
		}
	
		return result;
	},

	//-----------------------------------------------------------------------------
	
	'getRows': function () {
		throw Clipperz.Base.AbstractMethod;
	},
	
	//-----------------------------------------------------------------------------

	'setDeferredDisplaySelectedRowsInvocation': function (aDeferred) {
		if (this._deferredDisplaySelectedRowsInvocation != null) {
			this._deferredDisplaySelectedRowsInvocation.cancel();
		}
		
		this._deferredDisplaySelectedRowsInvocation = aDeferred;
	},
	
	//-----------------------------------------------------------------------------
	
	'resetDeferredDisplaySelectedRowsInvocation': function () {
		if (this._deferredDisplaySelectedRowsInvocation != null) {
			this._deferredDisplaySelectedRowsInvocation.cancel();
		}
	},
	
	//-----------------------------------------------------------------------------
	
	'_displaySelectedRows': function (aFilter, someRows) {
		var result;
		var delay;
		
		if ((aFilter != null) && (aFilter != '')) {
			var filter;
			var	filterRegExp;
			
			filter = aFilter.replace(/[^A-Za-z0-9]/g, "\\$&");
			filterRegExp = new RegExp(filter, "i");
			result = MochiKit.Base.filter(function (aCachedResult) { return filterRegExp.test(aCachedResult['_searchableContent'])}, someRows);
			delay = 0.002*result.length;

			this.setDeferredDisplaySelectedRowsInvocation(MochiKit.Async.callLater(delay, MochiKit.Base.method(this, "deferredDisplaySelectedRows", result)));
		} else {
			result = someRows;

			this.resetDeferredDisplaySelectedRowsInvocation();
			this.deferredDisplaySelectedRows(result);
		}

	},

	//-----------------------------------------------------------------------------
	
	'deferredDisplaySelectedRows': function (someRows) {
		if (this.sortedColumnManager() != null) {
			var comparator;
			var fieldName;
			
			fieldName = this.sortedColumnManager().name();
			comparator = this.sortedColumnManager().comparator();
			if (this.sortedColumnManager().sorted() == 'DESCENDING') {
				comparator = Clipperz.Base.reverseComparator(comparator);
			}

			someRows.sort(MochiKit.Base.partial(function(aKey, aComparator, aObject, bObject){
				return comparator(aObject[aKey], bObject[aKey]);
			}, this.sortedColumnManager().name(), comparator));
		}

		this.grid().update(someRows);
		this.grid().endSearch();
	},

	//-----------------------------------------------------------------------------

	'getCachedValues': function () {
		var deferredResult;

		if (this._cachedObjects != null) {
			deferredResult = MochiKit.Async.succeed(this._cachedObjects);
		} else {
			var objectCollectResultsConfiguration;
			
			objectCollectResultsConfiguration = {
				'_rowObject':			MochiKit.Async.succeed,
				'_reference':			MochiKit.Base.methodcaller('reference'),
				'_searchableContent':	MochiKit.Base.methodcaller('searchableContent')
			};

			MochiKit.Base.map(function (aColumnManager) {
				objectCollectResultsConfiguration[aColumnManager.name()] = aColumnManager.selector();
			}, this.columnsManagers());

			deferredResult = new Clipperz.Async.Deferred("GridController.getCachedValues", {trace:false});
			deferredResult.addMethod(this, 'getRows');
			deferredResult.addCallback(MochiKit.Base.map, Clipperz.Async.collectResults("GridController.getCachedValues - collectResults", objectCollectResultsConfiguration, {trace:false}));
			deferredResult.addCallback(Clipperz.Async.collectAll);
			deferredResult.addCallback(MochiKit.Base.bind(function (someRows) {
				this._cachedObjects = someRows;
				return this._cachedObjects;
			}, this));
			deferredResult.callback();
		}
		
		return deferredResult;
	},

	//-----------------------------------------------------------------------------

	'hasPendingChanges': function () {
		return this.user().hasPendingChanges();
	},

	'saveChanges': function () {
		this._cachedObjects = null;

		return Clipperz.Async.callbacks("GridController.saveChanges", [
			MochiKit.Base.method(this.user(), 'saveChanges'),
			MochiKit.Base.method(this, 'focus')
		], {trace:false});
	},
	
	'revertChanges': function () {
		return this.user().revertChanges();
	},
	
	//-----------------------------------------------------------------------------

	'displayEmptyContent': function () {
	},

	'hideEmptyContent': function () {
		this.grid().removeNoRowsGridComponent();
	},

	'displaySelectedRows': function (aFilter) {
		if ((aFilter != null) && (aFilter != '')){
			this.grid().startSearch();
		}

		return Clipperz.Async.callbacks("GridController.displaySelectedrows", [
			MochiKit.Base.method(this, 'getCachedValues'),
			MochiKit.Base.itemgetter('length'),
			Clipperz.Async.deferredIf("There are some items to show in the grid", [
				MochiKit.Base.method(this, 'hideEmptyContent'),
				MochiKit.Base.method(this, 'getCachedValues'),
				MochiKit.Base.method(this, '_displaySelectedRows', aFilter)
			], [
				MochiKit.Base.method(this, 'displayEmptyContent'),
				MochiKit.Base.method(this.grid(), 'endSearch')
			])
		], {trace:false});
	},

	//-----------------------------------------------------------------------------

	'focus': function () {
		return Clipperz.Async.callbacks("GridController.focus", [
			MochiKit.Base.method(this, 'displaySelectedRows', this.filterController().getFilter()),
			MochiKit.Base.method(this.grid(), 'focus')
		], {trace:false})
//*##*/	this.displaySelectedRows(this.filterController().getFilter());
//		this.grid().focus();
	},

	//=============================================================================

	'deleteAllCleanTextData': function () {
		this._cachedObjects = null;
		this.grid().drawEmpty();
	},

	//=============================================================================
	__syntaxFix__: "syntax fix"
});
