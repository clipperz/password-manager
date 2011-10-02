
/**
 * @class YAHOO.ext.grid.DefaultDataModel
 * This is the default implementation of a DataModel used by the Grid. It works 
 * with multi-dimensional array based data. Using the event system in the base class 
 * {@link YAHOO.ext.grid.AbstractDataModel}, all updates to this DataModel are automatically
 * reflected in the user interface.
 * <br>Usage:<br>
 * <pre><code>
 * var myData = [
	["MSFT","Microsoft Corporation", "314,571.156", "32,187.000", "55000"],
	["ORCL", "Oracle Corporation", "62,615.266", "9,519.000", "40650"]
 * ];
 * var dataModel = new YAHOO.ext.grid.DefaultDataModel(myData);
 * </code></pre>
 * @extends YAHOO.ext.grid.AbstractDataModel
 * @constructor
 * @param {Array} data 
*/
YAHOO.ext.grid.DefaultDataModel = function(data){
    YAHOO.ext.grid.DefaultDataModel.superclass.constructor.call(this);
    /**@private*/
    this.data = data;
};
YAHOO.extendX(YAHOO.ext.grid.DefaultDataModel, YAHOO.ext.grid.AbstractDataModel, {
    /**
     * Returns the number of rows in the dataset
     * @return {Number}
     */
    getRowCount : function(){
        return this.data.length;
    },
        
    /**
     * Returns the ID of the specified row. By default it return the value of the first column. 
     * Override to provide more advanced ID handling. 
     * @return {Number}
     */
    getRowId : function(rowIndex){
        return this.data[rowIndex][0];
    },
    
    /**
     * Returns the column data for the specified row. 
     * @return {Array}
     */
    getRow : function(rowIndex){
        return this.data[rowIndex];
    },
    
    /**
     * Returns the column data for the specified rows as a 
     * multi-dimensional array: rows[3][0] would give you the value of row 4, column 0. 
     * @param {Array} indexes The row indexes to fetch
     * @return {Array}
     */
    getRows : function(indexes){
        var data = this.data;
        var r = [];
        for(var i = 0; i < indexes.length; i++){
           r.push(data[indexes[i]]);
        }
        return r;
    },
    
    /**
     * Returns the value at the specified data position
     * @param {Number} rowIndex
     * @param {Number} colIndex
     * @return {Object}
     */
    getValueAt : function(rowIndex, colIndex){
    	return this.data[rowIndex][colIndex];
    },
    
    /**
     * Sets the specified value at the specified data position
     * @param {Object} value The new value
     * @param {Number} rowIndex
     * @param {Number} colIndex
     */
    setValueAt: function(value, rowIndex, colIndex){
        this.data[rowIndex][colIndex] = value;
        this.fireCellUpdated(rowIndex, colIndex);
    },
    
    /**
     * @private
     * Removes the specified range of rows.
     * @param {Number} startIndex
     * @param {<i>Number</i>} endIndex (optional) Defaults to startIndex
     */
    removeRows: function(startIndex, endIndex){
        endIndex = endIndex || startIndex;
        this.data.splice(startIndex, endIndex-startIndex+1);
        this.fireRowsDeleted(startIndex, endIndex);
    },
    
    /**
     * Remove a row.
     * @param {Number} index
     */
    removeRow: function(index){
        this.data.splice(index, 1);
        this.fireRowsDeleted(index, index);
    },
    
    /**
     * @private
     * Removes all rows.
     */
    removeAll: function(){
    	var count = this.getRowCount();
    	if(count > 0){
        	this.removeRows(0, count-1);
    	}
    },
    
    /**
     * Query the DataModel rows by the filters defined in spec, for example...
     * <pre><code>
     * // column 1 starts with Jack, column 2 filtered by myFcn, column 3 equals 'Fred'
     * dataModel.filter({1: /^Jack.+/i}, 2: myFcn, 3: 'Fred'});
     * </code></pre> 
     * @param {Object} spec The spec is generally an object literal consisting of
     * column index and filter type. The filter type can be a string/number (exact match),
     * a regular expression to test using String.search() or a function to call. If it's a function, 
     * it will be called with the value for the specified column and an array of the all column 
     * values for that row: yourFcn(value, columnData). If it returns anything other than true, 
     * the row is not a match. If you have modified Object.prototype this method may fail.
     * @param {Boolean} returnUnmatched True to return rows which <b>don't</b> match the query instead
     * of rows that do match
     * @return {Array} An array of row indexes that match
     */
    query: function(spec, returnUnmatched){
        var d = this.data;
        var r = [];
        for(var i = 0; i < d.length; i++){
            var row = d[i];
            var isMatch = true;
            for(var col in spec){
                //if(typeof spec[col] != 'function'){
                    if(!isMatch) continue;
                    var filter = spec[col];
                    switch(typeof filter){
                        case 'string':
                        case 'number':
                        case 'boolean':
                          if(row[col] != filter){
                              isMatch = false;
                          }
                        break;
                        case 'function':
                          if(!filter(row[col], row)){
                              isMatch = false;
                          }
                        break;
                        case 'object':
                           if(filter instanceof RegExp){
                               if(String(row[col]).search(filter) === -1){
                                   isMatch = false;
                               }
                           }
                        break;
                    }
                //}
            }
            if(isMatch && !returnUnmatched){
                r.push(i);
            }else if(!isMatch && returnUnmatched){
                r.push(i);
            }
        }
        return r;
    },
    
    /**
     * Filter the DataModel rows by the query defined in spec, see {@link #query} for more details 
     * on the query spec.
     * @param {Object} query The query spec {@link #query}
     * @return {Number} The number of rows removed
     */
    filter: function(query){
        var matches = this.query(query, true);
        var data = this.data;
        // go thru the data setting matches to deleted
        // while not disturbing row indexes
        for(var i = 0; i < matches.length; i++){ 
            data[matches[i]]._deleted = true;
        }
        for(var i = 0; i < data.length; i++){
            while(data[i] && data[i]._deleted === true){
                this.removeRow(i);
            }
        }
        return matches.length;
    },
    
    /**
     * Adds a row to the dataset.
     * @param {Array} cellValues The array of values for the new row
     * @return {Number} The index of the added row
     */
    addRow: function(cellValues){
        this.data.push(cellValues);
        var newIndex = this.data.length-1;
        this.fireRowsInserted(newIndex, newIndex);
        this.applySort();
        return newIndex;
    },
    
    /**
     * @private
     * Adds a set of rows.
     * @param {Array} rowData This should be an array of arrays like the constructor takes
     */
    addRows: function(rowData){
        this.data = this.data.concat(rowData);
        var firstIndex = this.data.length-rowData.length;
        this.fireRowsInserted(firstIndex, firstIndex+rowData.length-1);
        this.applySort();
    },
    
    /**
     * Inserts a row a the specified location in the dataset.
     * @param {Number} index The index where the row should be inserted
     * @param {Array} cellValues The array of values for the new row
     * @return {Number} The index the row was inserted in
     */
    insertRow: function(index, cellValues){
        this.data.splice(index, 0, cellValues);
        this.fireRowsInserted(index, index);
        this.applySort();
        return index;
    },
    
    /**
     * @private
     * Inserts a set of rows.
     * @param {Number} index The index where the rows should be inserted
     * @param {Array} rowData This should be an array of arrays like the constructor takes
     */
    insertRows: function(index, rowData){
        /*
        if(index == this.data.length){ // try these two first since they are faster
            this.data = this.data.concat(rowData);
        }else if(index == 0){
            this.data = rowData.concat(this.data);
        }else{
            var newData = this.data.slice(0, index);
            newData.concat(rowData);
            newData.concat(this.data.slice(index));
            this.data = newData;
        }*/
        var args = rowData.concat();
        args.splice(0, 0, index, 0);
        this.data.splice.apply(this.data, args);
        this.fireRowsInserted(index, index+rowData.length-1);
        this.applySort();
    },
    
    /**
     * Applies the last used sort to the current data.
     */
    applySort: function(suppressEvent){
    	if(typeof this.sortColumn != 'undefined'){
    		this.sort(this.sortInfo, this.sortColumn, this.sortDir, suppressEvent);
    	}
    },
    
    /**
     * Sets the default sort info. Note: this function does not actually apply the sort.
     * @param {Function/Object} sortInfo A sort comparison function or null to use the default or A object that has a method getSortType(index) that returns a function like 
     *                                               a grid column model. 
     * @param {Number} columnIndex The column index to sort by
     * @param {String} direction The direction of the sort ('DESC' or 'ASC')
     */
    setDefaultSort: function(sortInfo, columnIndex, direction){
        this.sortInfo = sortInfo;
        this.sortColumn = columnIndex;
        this.sortDir = direction;
    },
    /**
     * Sorts the data by the specified column - Uses the sortType specified for the column in the passed columnModel.
     * @param {Function/Object} sortInfo A sort comparison function or null to use the default or A object that has a method getSortType(index) that returns a function like 
     *                                               a grid column model. 
     * @param {Number} columnIndex The column index to sort by
     * @param {String} direction The direction of the sort ('DESC' or 'ASC')
     */
    sort: function(sortInfo, columnIndex, direction, suppressEvent){
        // store these so we can maintain sorting when we load new data
        this.sortInfo = sortInfo;
        this.sortColumn = columnIndex;
        this.sortDir = direction;
        
        var dsc = (direction && direction.toUpperCase() == 'DESC');
        var sortType = null;
        if(sortInfo != null){
            if(typeof sortInfo == 'function'){
                sortType = sortInfo;
            }else if(typeof sortInfo == 'object'){
                sortType = sortInfo.getSortType(columnIndex);;
            }
        }
        var fn = function(cells, cells2){
            var v1 = sortType ? sortType(cells[columnIndex], cells) : cells[columnIndex];
            var v2 = sortType ? sortType(cells2[columnIndex], cells2) : cells2[columnIndex];
            if(v1 < v2)
    			return dsc ? +1 : -1;
    		if(v1 > v2)
    			return dsc ? -1 : +1;
    	    return 0;
        };
        this.data.sort(fn);
        if(!suppressEvent){
           this.fireRowsSorted(columnIndex, direction);
        }
    },
    
    /**
     * Calls passed function with each rows data - if the function returns false it stops.
     * @param {Function} fn
     * @param {Object} scope (optional)
     */ 
    each: function(fn, scope){
        var d = this.data;
        for(var i = 0, len = d.length; i < len; i++){
            if(fn.call(scope || window, d[i], i) === false) break;
        }
    }
});

/**
 * Alias to YAHOO.ext.grid.DefaultColumnModel.sortTypes
 * @static
 */
if(YAHOO.ext.grid.DefaultColumnModel){
    YAHOO.ext.grid.DefaultDataModel.sortTypes = YAHOO.ext.grid.DefaultColumnModel.sortTypes;
}
