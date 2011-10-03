
/**
 * @class YAHOO.ext.grid.JSONDataModel
 * This is an implementation of a DataModel used by the Grid. It works 
 * with JSON data.
 * <br>Example schema:
 * <pre><code>
 * var schema = {
 *     root: 'Results.Result',
 *     id: 'ASIN',
 *     fields: ['Author', 'Title', 'Manufacturer', 'ProductGroup']
 * };
 * </code></pre>
 * @extends YAHOO.ext.grid.LoadableDataModel
 * @constructor
*/
YAHOO.ext.grid.JSONDataModel = function(schema){
    YAHOO.ext.grid.JSONDataModel.superclass.constructor.call(this, YAHOO.ext.grid.LoadableDataModel.JSON);
    /**@private*/
    this.schema = schema;
};
YAHOO.extendX(YAHOO.ext.grid.JSONDataModel, YAHOO.ext.grid.LoadableDataModel, {
    /**
     * Overrides loadData in LoadableDataModel to process JSON data
     * @param {Object} data The JSON object to load
     * @param {Function} callback
     */
    loadData : function(data, callback, keepExisting){
    	var idField = this.schema.id;
    	var fields = this.schema.fields;
    	try{
        	if(this.schema.totalProperty){
                var v = parseInt(eval('data.' + this.schema.totalProperty), 10);
                if(!isNaN(v)){
                    this.totalCount = v;
                }
            }
        	var rowData = [];
    	    var root = eval('data.' + this.schema.root);
    	    for(var i = 0; i < root.length; i++){
    			var node = root[i];
    			var colData = [];
    			colData.node = node;
    			colData.id = (typeof node[idField] != 'undefined' && node[idField] !== '' ? node[idField] : String(i));
    			for(var j = 0; j < fields.length; j++) {
    			    var val = node[fields[j]];
    			    if(typeof val == 'undefined'){
    			        val = '';
    			    }
    	            if(this.preprocessors[j]){
    	                val = this.preprocessors[j](val);
    	            }
    	            colData.push(val);
    	        }
    	        rowData.push(colData);
    		}
    		if(keepExisting !== true){
    		  this.removeAll();
    		}
            this.addRows(rowData);
        	if(typeof callback == 'function'){
    	    	callback(this, true);
    	    }
          	this.fireLoadEvent();
    	}catch(e){
    		this.fireLoadException(e, null);
    		if(typeof callback == 'function'){
    	    	callback(this, false);
    	    }
    	}
    },
    
    /**
     * Overrides getRowId in DefaultDataModel to return the ID value of the specified node. 
     * @param {Number} rowIndex
     * @return {Number}
     */
    getRowId : function(rowIndex){
        return this.data[rowIndex].id;
    }
});
