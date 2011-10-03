/**
 * @class YAHOO.ext.grid.LoadableDataModel
 * This class extends DefaultDataModel and adds the core functionality to load data remotely. Generally you will want to use one of it's subclasses.<br><br>
 * @extends YAHOO.ext.grid.DefaultDataModel
 * @constructor
 * @param {String} dataType YAHOO.ext.grid.LoadableDataModel.XML, YAHOO.ext.grid.LoadableDataModel.TEXT or YAHOO.ext.grid.JSON
*/
YAHOO.ext.grid.LoadableDataModel = function(dataType){
    YAHOO.ext.grid.LoadableDataModel.superclass.constructor.call(this, []);
    
    /** Fires when a successful load is completed - fireDirect sig: (this)
     * @type YAHOO.util.CustomEvent 
     * @deprecated Use addListener instead of accessing directly
     * @private
     */
    this.onLoad = new YAHOO.util.CustomEvent('load');
    /** Fires when a load fails - fireDirect sig: (this, errorMsg, responseObj)
     * @type YAHOO.util.CustomEvent 
     * @deprecated Use addListener instead of accessing directly
     * @private
     */
    this.onLoadException = new YAHOO.util.CustomEvent('loadException');
    /**
       * @event load
       * Fires when new data has successfully been loaded
       * @param {DataModel} this
       */
    this.events['load'] = this.onLoad;
    /**
       * @event beforeload
       * Fires before a load takes place
       * @param {DataModel} this
       */
    this.events['beforeload'] = new YAHOO.util.CustomEvent('beforeload');
    /**
       * @event loadexception
       * Fires when there's an error loading data
       * @param {DataModel} this
       * @param {Exception} e The exception object or null
       * @param {Object} response The Connect response object
       */
    this.events['loadexception'] = this.onLoadException;
    
    /**@private*/
    this.dataType = dataType;
    /**@private*/
    this.preprocessors = [];
    /**@private*/
    this.postprocessors = [];
    
    // paging info
    /** The active page @type Number*/
    this.loadedPage = 1;
    /** True to use remote sorting, initPaging automatically sets this to true @type Boolean */
    this.remoteSort = false;
    /** The number of records per page @type Number*/
    this.pageSize = 0;
    /** The script/page to call to provide paged/sorted data @type String*/
    this.pageUrl = null;
    /** An object of key/value pairs to be passed as parameters
     * when loading pages/sorting @type Object*/
    this.baseParams = {};
    /** Maps named params to url parameters - Override to specify your own param names */
    this.paramMap = {'page':'page', 'pageSize':'pageSize', 'sortColumn':'sortColumn', 'sortDir':'sortDir'};
    
};
YAHOO.extendX(YAHOO.ext.grid.LoadableDataModel, YAHOO.ext.grid.DefaultDataModel, {
    
    /** @ignore */
    setLoadedPage: function(pageNum, userCallback){
        this.loadedPage = pageNum;
        if(typeof userCallback == 'function'){
            userCallback();
        }
    },
    
    /** Returns true if this model uses paging @return Boolean */
    isPaged: function(){
        return this.pageSize > 0;
    },
    
    /** Returns the total number of records available, override if needed @return {Number} */
    getTotalRowCount: function(){
        return this.totalCount || this.getRowCount();
    },
    
    /** Returns the number of records per page @return Number */
    getPageSize: function(){
        return this.pageSize;
    },
    
    /** Returns the total number of pages available @return Number */
    getTotalPages: function(){
        if(this.getPageSize() == 0 || this.getTotalRowCount() == 0){
            return 1;
        }
        return Math.ceil(this.getTotalRowCount()/this.getPageSize());
    },
    
    /** Initializes paging for this model.
     * @param {String} url
     * @param {Number} pageSize
     * @param {Object} baseParams (optional) Object containing key/value pairs to add to all requests
     */
    initPaging: function(url, pageSize, baseParams){
        this.pageUrl = url;
        this.pageSize = pageSize;
        this.remoteSort = true;
        if(baseParams) this.baseParams = baseParams;
    },
    
    /** @ignore */
    createParams: function(pageNum, sortColumn, sortDir){
        var params = {}, map = this.paramMap;
        for(var key in this.baseParams){
            if(typeof this.baseParams[key] != 'function'){
                params[key] = this.baseParams[key];
            }
        }
        params[map['page']] = pageNum;
        params[map['pageSize']] = this.getPageSize();
        params[map['sortColumn']] = (typeof sortColumn == 'undefined' ? '' : sortColumn);
        params[map['sortDir']] = sortDir || '';
        return params;
    },
    
    /**
     * Loads a page of data.
     * @param {Number} pageNum Which page to load. The first page is 1.
     * @param {Function} callback (optional) Optional callback when loading is complete
     * @param {Boolean} keepExisting (optional) true to keep existing data and append the new data
     */
    loadPage: function(pageNum, callback, keepExisting){
        var sort = this.getSortState();
        var params = this.createParams(pageNum, sort.column, sort.direction);
        this.load(this.pageUrl, params, this.setLoadedPage.createDelegate(this, [pageNum, callback]), 
                   keepExisting ? (pageNum-1) * this.pageSize : null);
    },
    
    /** @ignore */
    applySort: function(suppressEvent){
    	if(!this.remoteSort){
            YAHOO.ext.grid.LoadableDataModel.superclass.applySort.apply(this, arguments);
        }else if(!suppressEvent){
            var sort = this.getSortState();
            if(sort.column){
               this.fireRowsSorted(sort.column, sort.direction, true);
            }
        }
    },
    
    /** @ignore */
    resetPaging: function(){
    	this.loadedPage = 1;
    },
    
    /* Overridden sort method to use remote sorting if turned on */
    sort: function(sortInfo, columnIndex, direction, suppressEvent){
        if(!this.remoteSort){
            YAHOO.ext.grid.LoadableDataModel.superclass.sort.apply(this, arguments);
        }else{
            this.sortInfo = sortInfo;
            this.sortColumn = columnIndex;
            this.sortDir = direction;
            var params = this.createParams(this.loadedPage, columnIndex, direction);
            this.load(this.pageUrl, params, this.fireRowsSorted.createDelegate(this, [columnIndex, direction, true]));
        }
    },
    
    /**
     * Initiates the loading of the data from the specified URL - Failed load attempts will 
     * fire the {@link #loadexception} event.
     * @param {Object/String} url The url from which the data can be loaded
     * @param {<i>String/Object</i>} params (optional) The parameters to pass as either a url encoded string "param1=1&amp;param2=2" or as an object {param1: 1, param2: 2}
     * @param {<i>Function</i>} callback (optional) Callback when load is complete - called with signature (this, true for success, false for failure)
     * @param {<i>Number</i>} insertIndex (optional) if present, loaded data is inserted at the specified index instead of overwriting existing data
     */
    load: function(url, params, callback, insertIndex){
    	this.fireEvent('beforeload', this);
    	if(params && typeof params != 'string'){ // must be object
            var buf = [];
            for(var key in params){
                if(typeof params[key] != 'function'){
                    buf.push(encodeURIComponent(key), '=', encodeURIComponent(params[key]), '&');
                }
            }
            delete buf[buf.length-1];
            params = buf.join('');
        }
        var cb = {
            success: this.processResponse,
            failure: this.processException,
            scope: this,
    		argument: {callback: callback, insertIndex: insertIndex}
        };
        var method = params ? 'POST' : 'GET';
        this.transId = YAHOO.util.Connect.asyncRequest(method, url, cb, params);
    },
    
    /**@private*/
    processResponse: function(response){
        var cb = response.argument.callback;
        var keepExisting = (typeof response.argument.insertIndex == 'number');
        var insertIndex = response.argument.insertIndex;
        switch(this.dataType){
        	case YAHOO.ext.grid.LoadableDataModel.XML:
        		this.loadData(response.responseXML, cb, keepExisting, insertIndex);
        	break;
        	case YAHOO.ext.grid.LoadableDataModel.JSON:
        		var rtext = response.responseText;
        		try { // this code is a modified version of Yahoo! UI DataSource JSON parsing
    		        // Trim leading spaces
    		        while(rtext.substring(0,1) == " ") {
    		            rtext = rtext.substring(1, rtext.length);
    		        }
    		        // Invalid JSON response
    		        if(rtext.indexOf("{") < 0) {
    		            throw "Invalid JSON response";
    		        }
    		
    		        // Empty (but not invalid) JSON response
    		        if(rtext.indexOf("{}") === 0) {
    		            this.loadData({}, response.argument.callback);
    		            return;
    		        }
    		
    		        // Turn the string into an object literal...
    		        // ...eval is necessary here
    		        var jsonObjRaw = eval("(" + rtext + ")");
    		        if(!jsonObjRaw) {
    		            throw "Error evaling JSON response";
    		        }
    				this.loadData(jsonObjRaw, cb, keepExisting, insertIndex);
    		    } catch(e) {
    		        this.fireLoadException(e, response);
    				if(typeof cb == 'function'){
    			    	cb(this, false);
    			    }
    		   	}
        	break;
        	case YAHOO.ext.grid.LoadableDataModel.TEXT:
        		this.loadData(response.responseText, cb, keepExisting, insertIndex);
        	break;
        };
    },
    
    /**@private*/
    processException: function(response){
        this.fireLoadException(null, response);
        if(typeof response.argument.callback == 'function'){
            response.argument.callback(this, false);
        }
    },
    
    fireLoadException: function(e, responseObj){
        this.onLoadException.fireDirect(this, e, responseObj);
    },
    
    fireLoadEvent: function(){
        this.fireEvent('load', this.loadedPage, this.getTotalPages());
    },
    
    /**
     * Adds a preprocessor function to parse data before it is added to the Model - ie. Date.parse to parse dates.
     * @param {Number} columnIndex 
     * @param {Function} fn
     */
    addPreprocessor: function(columnIndex, fn){
        this.preprocessors[columnIndex] = fn;
    },
    
    /**
     * Gets the preprocessor function for the specified column.
     * @param {Number} columnIndex
     * @return {Function}
     */
    getPreprocessor: function(columnIndex){
        return this.preprocessors[columnIndex];
    },
    
    /**
     * Removes a preprocessor function.
     * @param {Number} columnIndex 
     */
    removePreprocessor: function(columnIndex){
        this.preprocessors[columnIndex] = null;
    },
    
    /**
     * Adds a postprocessor function to format data before updating the underlying data source (ie. convert date to string before updating XML document).
     * @param {Number} columnIndex 
     * @param {Function} fn
     */
    addPostprocessor: function(columnIndex, fn){
        this.postprocessors[columnIndex] = fn;
    },
    
    /**
     * Gets the postprocessor function for the specified column.
     * @param {Number} columnIndex
     * @return {Function}
     */
    getPostprocessor: function(columnIndex){
        return this.postprocessors[columnIndex];
    },
    
    /**
     * Removes a postprocessor function.
     * @param {Number} columnIndex
     */
    removePostprocessor: function(columnIndex){
        this.postprocessors[columnIndex] = null;
    },
    /**
     * Empty interface method - Called to process the data returned by the XHR - Classes which extend LoadableDataModel should implement this method.
     * See {@link YAHOO.ext.XMLDataModel} for an example implementation.
     */
    loadData: function(data, callback, keepExisting, insertIndex){
    	
    }
});

YAHOO.ext.grid.LoadableDataModel.XML = 'xml';
YAHOO.ext.grid.LoadableDataModel.JSON = 'json';
YAHOO.ext.grid.LoadableDataModel.TEXT = 'text';





