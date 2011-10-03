/**
 * @class YAHOO.ext.grid.AbstractColumnModel
 * @extends YAHOO.ext.util.Observable
 * This abstract class defines the ColumnModel interface and provides default implementations of the events required by the Grid. 
 * @constructor
*/
YAHOO.ext.grid.AbstractColumnModel = function(){
	// legacy events
	this.onWidthChange = new YAHOO.util.CustomEvent('widthChanged');
    this.onHeaderChange = new YAHOO.util.CustomEvent('headerChanged');
	this.onHiddenChange = new YAHOO.util.CustomEvent('hiddenChanged');
    
    this.events = {
        /**
	     * @event widthchange
	     * Fires when the width of a column changes
	     * @param {ColumnModel} this
	     * @param {Number} columnIndex The column index
	     * @param {Number} newWidth The new width
	     */
	    'widthchange': this.onWidthChange,
        /**
	     * @event headerchange
	     * Fires when the text of a header changes 
	     * @param {ColumnModel} this
	     * @param {Number} columnIndex The column index
	     * @param {Number} newText The new header text
	     */
	    'headerchange': this.onHeaderChange,
        /**
	     * @event hiddenchange
	     * Fires when a column is hidden or "unhidden"
	     * @param {ColumnModel} this
	     * @param {Number} columnIndex The column index
	     * @param {Number} hidden true if hidden, false otherwise
	     */
	    'hiddenchange': this.onHiddenChange
    };
};

YAHOO.ext.grid.AbstractColumnModel.prototype = {
	fireEvent : YAHOO.ext.util.Observable.prototype.fireEvent,
    on : YAHOO.ext.util.Observable.prototype.on,
    addListener : YAHOO.ext.util.Observable.prototype.addListener,
    delayedListener : YAHOO.ext.util.Observable.prototype.delayedListener,
    removeListener : YAHOO.ext.util.Observable.prototype.removeListener,
    purgeListeners : YAHOO.ext.util.Observable.prototype.purgeListeners,
    bufferedListener : YAHOO.ext.util.Observable.prototype.bufferedListener,
    
    fireWidthChange : function(colIndex, newWidth){
		this.onWidthChange.fireDirect(this, colIndex, newWidth);
	},
	
	fireHeaderChange : function(colIndex, newHeader){
		this.onHeaderChange.fireDirect(this, colIndex, newHeader);
	},
	
	fireHiddenChange : function(colIndex, hidden){
		this.onHiddenChange.fireDirect(this, colIndex, hidden);
	},
	
	/**
     * Interface method - Returns the number of columns.
     * @return {Number}
     */
    getColumnCount : function(){
        return 0;
    },
    
    /**
     * Interface method - Returns true if the specified column is sortable.
     * @param {Number} col The column index
     * @return {Boolean}
     */
    isSortable : function(col){
        return false;
    },
    
    /**
     * Interface method - Returns true if the specified column is hidden.
     * @param {Number} col The column index
     * @return {Boolean}
     */
    isHidden : function(col){
        return false;
    },
    
    /**
     * Interface method - Returns the sorting comparison function defined for the column (defaults to sortTypes.none).
     * @param {Number} col The column index
     * @return {Function}
     */
    getSortType : function(col){
        return YAHOO.ext.grid.DefaultColumnModel.sortTypes.none;
    },
    
    /**
     * Interface method - Returns the rendering (formatting) function defined for the column.
     * @param {Number} col The column index
     * @return {Function}
     */
    getRenderer : function(col){
        return YAHOO.ext.grid.DefaultColumnModel.defaultRenderer;
    },
    
    /**
     * Interface method - Returns the width for the specified column.
     * @param {Number} col The column index
     * @return {Number}
     */
    getColumnWidth : function(col){
        return 0;
    },
    
    /**
     * Interface method - Returns the total width of all columns.
     * @return {Number}
     */
    getTotalWidth : function(){
        return 0;
    },
    
    /**
     * Interface method - Returns the header for the specified column.
     * @param {Number} col The column index
     * @return {String}
     */
    getColumnHeader : function(col){
        return '';
    }
};
