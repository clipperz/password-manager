/**
 * @class YAHOO.ext.grid.AbstractDataModel
 * @extends YAHOO.ext.util.Observable
 * This abstract class provides default implementations of the events required by the Grid. 
 It takes care of the creating the CustomEvents and provides some convenient methods for firing the events. <br><br>
 * @constructor
*/
YAHOO.ext.grid.AbstractDataModel = function(){
    /** Fires when a cell is updated - fireDirect sig: (this, rowIndex, columnIndex)
     * @private
     * @type YAHOO.util.CustomEvent
     * @deprecated Use addListener instead of accessing directly
     */
    this.onCellUpdated = new YAHOO.util.CustomEvent('onCellUpdated');
    /** Fires when all data needs to be revalidated - fireDirect sig: (thisd)
     * @private
     * @type YAHOO.util.CustomEvent 
     * @deprecated Use addListener instead of accessing directly
     */
    this.onTableDataChanged = new YAHOO.util.CustomEvent('onTableDataChanged');
    /** Fires when rows are deleted - fireDirect sig: (this, firstRowIndex, lastRowIndex)
     * @private
     * @type YAHOO.util.CustomEvent 
     * @deprecated Use addListener instead of accessing directly
     */
    this.onRowsDeleted = new YAHOO.util.CustomEvent('onRowsDeleted');
    /** Fires when a rows are inserted - fireDirect sig: (this, firstRowIndex, lastRowIndex)
     * @private
     * @type YAHOO.util.CustomEvent 
     * @deprecated Use addListener instead of accessing directly
     */
    this.onRowsInserted = new YAHOO.util.CustomEvent('onRowsInserted');
    /** Fires when a rows are updated - fireDirect sig: (this, firstRowIndex, lastRowIndex)
     * @private
     * @type YAHOO.util.CustomEvent 
     * @deprecated Use addListener instead of accessing directly
     */
    this.onRowsUpdated = new YAHOO.util.CustomEvent('onRowsUpdated');
    /** Fires when a sort has reordered the rows - fireDirect sig: (this, sortColumnIndex, 
     * @private
     * sortDirection = 'ASC' or 'DESC')
     * @type YAHOO.util.CustomEvent 
     * @deprecated Use addListener instead of accessing directly
     */
    this.onRowsSorted = new YAHOO.util.CustomEvent('onRowsSorted');
    
    this.events = {
      /**
       * @event cellupdated
       * Fires when a cell is updated
       * @param {DataModel} this
       * @param {Number} rowIndex
       * @param {Number} columnIndex
       */
      'cellupdated' : this.onCellUpdated,
      /**
       * @event datachanged
       * Fires when the entire data structure has changed
       * @param {DataModel} this
       */
      'datachanged' : this.onTableDataChanged,
      /**
       * @event rowsdeleted
       * Fires  when a range of rows have been deleted
       * @param {DataModel} this
       * @param {Number} firstRowIndex
       * @param {Number} lastRowIndex
       */
      'rowsdeleted' : this.onRowsDeleted,
      /**
       * @event rowsinserted
       * Fires when a range of rows have been inserted
       * @param {DataModel} this
       * @param {Number} firstRowIndex
       * @param {Number} lastRowIndex
       */
      'rowsinserted' : this.onRowsInserted,
      /**
       * @event rowsupdated
       * Fires when a range of rows have been updated
       * @param {DataModel} this
       * @param {Number} firstRowIndex
       * @param {Number} lastRowIndex
       */
      'rowsupdated' : this.onRowsUpdated,
      /**
       * @event rowssorted
       * Fires when the data has been sorted
       * @param {DataModel} this
       */
      'rowssorted' : this.onRowsSorted
    };
};

YAHOO.ext.grid.AbstractDataModel.prototype = {
    
    fireEvent : YAHOO.ext.util.Observable.prototype.fireEvent,
    on : YAHOO.ext.util.Observable.prototype.on,
    addListener : YAHOO.ext.util.Observable.prototype.addListener,
    delayedListener : YAHOO.ext.util.Observable.prototype.delayedListener,
    removeListener : YAHOO.ext.util.Observable.prototype.removeListener,
    purgeListeners : YAHOO.ext.util.Observable.prototype.purgeListeners,
    bufferedListener : YAHOO.ext.util.Observable.prototype.bufferedListener,
    
    /**
     *  Notifies listeners that the value of the cell at [row, col] has been updated
     * @deprecated
     * @private
     */
    fireCellUpdated : function(row, col){
        this.onCellUpdated.fireDirect(this, row, col);
    },
    
    /**
     *  Notifies listeners that all data for the grid may have changed - use as a last resort. This 
     * also wipes out all selections a user might have made.
     * @deprecated
     * @private
     */
    fireTableDataChanged : function(){
        this.onTableDataChanged.fireDirect(this);
    },
    
    /**
     *  Notifies listeners that rows in the range [firstRow, lastRow], inclusive, have been deleted
     * @deprecated
     * @private
     */
    fireRowsDeleted : function(firstRow, lastRow){
        this.onRowsDeleted.fireDirect(this, firstRow, lastRow);
    },
    
    /**
     *  Notifies listeners that rows in the range [firstRow, lastRow], inclusive, have been inserted
     * @deprecated
     * @private
     */
    fireRowsInserted : function(firstRow, lastRow){
        this.onRowsInserted.fireDirect(this, firstRow, lastRow);
    },
    
    /**
     *  Notifies listeners that rows in the range [firstRow, lastRow], inclusive, have been updated
     * @deprecated
     * @private
     */
    fireRowsUpdated : function(firstRow, lastRow){
        this.onRowsUpdated.fireDirect(this, firstRow, lastRow);
    },
    
    /**
     *  Notifies listeners that rows have been sorted and any indexes may be invalid
     * @deprecated
     * @private
     */
    fireRowsSorted : function(sortColumnIndex, sortDir, noRefresh){
        this.onRowsSorted.fireDirect(this, sortColumnIndex, sortDir, noRefresh);
    },
    
    /**
     * Empty interface method - Classes which extend AbstractDataModel should implement this method.
     * See {@link YAHOO.ext.DefaultDataModel#sort} for an example implementation.
    * @private
      */
    sort : function(sortInfo, columnIndex, direction, suppressEvent){
    	
    },
    
    /**
     * Interface method to supply info regarding the Grid's current sort state - if overridden,
     * this should return an object like this {column: this.sortColumn, direction: this.sortDir}.
     * @return {Object} 
      */
    getSortState : function(){
    	return {column: this.sortColumn, direction: this.sortDir};
    },
    
    /**
     * Empty interface method - Classes which extend AbstractDataModel should implement this method.
     * See {@link YAHOO.ext.DefaultDataModel} for an example implementation.
     * @private
     */
    getRowCount : function(){
    	
    },
    
    /**
     * Empty interface method - Classes which extend AbstractDataModel should implement this method to support virtual row counts.
     * @private
     */
    getTotalRowCount : function(){
    	return this.getRowCount();
    },
    
    
    /**
     * Empty interface method - Classes which extend AbstractDataModel should implement this method.
     * See {@link YAHOO.ext.DefaultDataModel} for an example implementation.
    * @private
      */
    getRowId : function(rowIndex){
    	
    },
    
    /**
     * Empty interface method - Classes which extend AbstractDataModel should implement this method.
     * See {@link YAHOO.ext.DefaultDataModel} for an example implementation.
     * @private
     */
    getValueAt : function(rowIndex, colIndex){
    	
    },
    
    /**
     * Empty interface method - Classes which extend AbstractDataModel should implement this method.
     * See {@link YAHOO.ext.DefaultDataModel} for an example implementation.
     * @private
     */
    setValueAt : function(value, rowIndex, colIndex){
    	
    },
    
    isPaged : function(){
        return false;
    }
};
