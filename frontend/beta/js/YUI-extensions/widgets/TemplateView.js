/**
 * @class YAHOO.ext.View
 * @extends YAHOO.ext.util.Observable
 * Create a "View" for an element based on a data model or UpdateManager and the supplied DomHelper template. 
 * This class also supports single and multi selection modes. <br>
 * Create a data model bound view:
<pre><code>
var dataModel = new YAHOO.ext.grid.XMLDataModel(...);
var view = new YAHOO.ext.View('my-element', 
           '&lt;div id="{0}"&gt;{2} - {1}&lt;/div&gt;', // auto create template
           dataModel, { 
              singleSelect: true, 
              selectedClass: 'ydataview-selected'
           });

// listen for node click?
view.on('click', function(vw, index, node, e){
    alert('Node "' + node.id + '" at index: ' + index + ' was clicked.');
});

// load XML data
dataModel.load('foobar.xml');
</code></pre>
For an example of creating a JSON/UpdateManager view, see {@link YAHOO.ext.JsonView}.
 * <br><br>
 * <b>Note: The root of your template must be a single node. Table/row implementations may work but are not supported due to
 * IE's limited insertion support with tables and Opera's faulty event bubbling.</b>
 * @constructor
 * Create a new View
 * @param {String/HTMLElement/Element} container The container element where the view is to be rendered.
 * @param {String/DomHelper.Template} tpl The rendering template or a string to create a template with
 * @param {DataModel} dataModel The bound data model
 * @param {Object} config The config object
*/
YAHOO.ext.View = function(container, tpl, dataModel, config){
    this.el = getEl(container, true);
    this.nodes = this.el.dom.childNodes;
    if(typeof tpl == 'string'){
        tpl = new YAHOO.ext.Template(tpl);
    }
    tpl.compile();
    /**
     * The template used by this View
     * @type {YAHOO.ext.DomHelper.Template}
     */
    this.tpl = tpl;
    this.setDataModel(dataModel);
    var CE = YAHOO.util.CustomEvent;
	/** @private */
	this.events = {
	    /**
         * @event beforeclick
         * Fires before a click is processed. Returns false to cancel the default action.
         * @param {YAHOO.ext.View} this
         * @param {Number} index The index of the target node
         * @param {HTMLElement} node The target node
         * @param {YAHOO.ext.EventObject} e The raw event object
         */
        'beforeclick' : true,
	    /**
         * @event click
         * Fires when a template node is clicked.
         * @param {YAHOO.ext.View} this
         * @param {Number} index The index of the target node
         * @param {HTMLElement} node The target node
         * @param {YAHOO.ext.EventObject} e The raw event object
         */
        'click' : true,
	    /**
         * @event dblclick
         * Fires when a template node is double clicked.
         * @param {YAHOO.ext.View} this
         * @param {Number} index The index of the target node
         * @param {HTMLElement} node The target node
         * @param {YAHOO.ext.EventObject} e The raw event object
         */
        'dblclick' : true,
	    /**
         * @event contextmenu
         * Fires when a template node is right clicked.
         * @param {YAHOO.ext.View} this
         * @param {Number} index The index of the target node
         * @param {HTMLElement} node The target node
         * @param {YAHOO.ext.EventObject} e The raw event object
         */
        'contextmenu' : true,
	    /**
         * @event selectionchange
         * Fires when the selected nodes change.
         * @param {YAHOO.ext.View} this
         * @param {Array} selections Array of the selected nodes
         */
        'selectionchange' : true,
        
        /**
         * @event beforeselect
         * Fires before a selection is made. If any handlers return false, the selection is cancelled.
         * @param {YAHOO.ext.View} this
         * @param {HTMLElement} node The node to be selected
         * @param {Array} selections Array of currently selected nodes
         */
        'beforeselect' : true
	};
	this.el.mon("click", this.onClick, this, true);
    this.el.mon("dblclick", this.onDblClick, this, true);
    this.el.mon("contextmenu", this.onContextMenu, this, true);
    
    /**
     * The css class to add to selected nodes
     * @type {YAHOO.ext.DomHelper.Template}
     */
    this.selectedClass = 'ydataview-selected';
    
    this.emptyText = '';
    
    this.selections = [];
    
    this.lastSelection = null;
    
    /**
     * The root property in the loaded json object that contains the data
     * @type {String}
     */
    this.jsonRoot = null;
    YAHOO.ext.util.Config.apply(this, config);
    if(this.renderUpdates || this.jsonRoot){
        var um = this.el.getUpdateManager();
        um.setRenderer(this);
    }
};

YAHOO.extendX(YAHOO.ext.View, YAHOO.ext.util.Observable, {
    /**
     * Returns the element this view is bound to.
     * @return {YAHOO.ext.Element}
     */
    getEl : function(){
        return this.el;  
    },
    
    render : function(el, response){
        this.clearSelections();
        this.el.update('');
        var o;
        try{
            o = YAHOO.ext.util.JSON.decode(response.responseText);
            if(this.jsonRoot){
                o = eval('o.' + this.jsonRoot);
            }
        }catch(e){}
        /**
         * The current json data or null
         */
        this.jsonData = o;
        this.beforeRender();
        this.refresh();
    },
    
    beforeRender : function(){
        
    },
    
    /**
     * Refreshes the view. 
     */
     refresh : function(){
        this.clearSelections();
        this.el.update('');
        this.html = [];
        if(this.renderUpdates || this.jsonRoot){
            var o = this.jsonData;
            if(o){
                for(var i = 0, len = o.length; i < len; i++) {
                	this.renderEach(o[i]);
                }
            }
        }else{
           this.dataModel.each(this.renderEach, this);
        }
        var strHtml;
        if(this.html.length > 0){
            strHtml = this.html.join('');
        }else{
            strHtml = this.emptyText;
        }
        this.el.update(strHtml);
        this.html = null;
        this.nodes = this.el.dom.childNodes;
        this.updateIndexes(0);
    },
    
    /**
     * Function to override to reformat the data that is sent to 
     * the template for each node.
     * @param {Array/Object} data The raw data (array of colData for a data model bound view or 
     * a JSON object for an UpdateManager bound view).
     * @param {Number} index The index of the data within the data model 
     */
    prepareData : function(data, index){
        return data;  
    },
    
    renderEach : function(data){
        this.html[this.html.length] = this.tpl.applyTemplate(this.prepareData(data));
    },
    
    /**
     * Refresh an individual node.
     * @param {Number} index 
     */
    refreshNode : function(index){
        this.refreshNodes(index, index);
    },
    
    refreshNodes : function(dm, startIndex, endIndex){
        this.clearSelections();
        var dm = this.dataModel;
        var ns = this.nodes;
        for(var i = startIndex; i <= endIndex; i++){
            var d = this.prepareData(dm.getRow(i), i);
            if(i < ns.length-1){
                var old = ns[i];
                this.tpl.insertBefore(old, d);
                this.el.dom.removeChild(old);
            }else{
                this.tpl.append(this.el.dom, d);
            }
        }
        this.updateIndexes(startIndex, endIndex);
    },
    
    deleteNodes : function(dm, startIndex, endIndex){
        this.clearSelections();
        if(startIndex == 0 && endIndex >= this.nodes.length-1){
            this.el.update('');
        }else{
            var el = this.el.dom;
            for(var i = startIndex; i <= endIndex; i++){
                el.removeChild(this.nodes[startIndex]);
            }
            this.updateIndexes(startIndex);
        }
    },
    
    insertNodes : function(dm, startIndex, endIndex){
        if(this.nodes.length == 0){
            this.refresh();
        }else{
            this.clearSelections();
            var t = this.tpl;
            var before = this.nodes[startIndex];
            var dm = this.dataModel;
            if(before){
                for(var i = startIndex; i <= endIndex; i++){
                    t.insertBefore(before, this.prepareData(dm.getRow(i), i));
                }
            }else{
                var el = this.el.dom;
                for(var i = startIndex; i <= endIndex; i++){
                    t.append(el, this.prepareData(dm.getRow(i), i));
                }
            }
            this.updateIndexes(startIndex);
        }
    },
    
    updateIndexes : function(dm, startIndex, endIndex){
        var ns = this.nodes;
        startIndex = startIndex || 0;
        endIndex = endIndex || ns.length-1;
        for(var i = startIndex; i <= endIndex; i++){
            ns[i].nodeIndex = i;
        }
    },
    
    /**
     * Changes the data model this view uses and refresh the view.
     * @param {DataModel} dataModel 
     */
     setDataModel : function(dm){
        if(!dm) return;
        this.unplugDataModel(this.dataModel);
        this.dataModel = dm;
        dm.on('cellupdated', this.refreshNode, this, true);
        dm.on('datachanged', this.refresh, this, true);
        dm.on('rowsdeleted', this.deleteNodes, this, true);
        dm.on('rowsinserted', this.insertNodes, this, true);
        dm.on('rowsupdated', this.refreshNodes, this, true);
        dm.on('rowssorted', this.refresh, this, true);
        this.refresh();
    },
    
    /**
     * Unplug the data model and stop updates.
     * @param {DataModel} dataModel 
     */
    unplugDataModel : function(dm){
        if(!dm) return;
        dm.removeListener('cellupdated', this.refreshNode, this);
        dm.removeListener('datachanged', this.refresh, this);
        dm.removeListener('rowsdeleted', this.deleteNodes, this);
        dm.removeListener('rowsinserted', this.insertNodes, this);
        dm.removeListener('rowsupdated', this.refreshNodes, this);
        dm.removeListener('rowssorted', this.refresh, this);
        this.dataModel = null;
    },
    
    /**
     * Returns the template node the passed child belongs to or null if it doesn't belong to one.
     * @param {HTMLElement} node
     * @return {HTMLElement} The template node 
     */
    findItemFromChild : function(node){
        var el = this.el.dom;
        if(!node || node.parentNode == el){
		    return node;
	    }
	    var p = node.parentNode;
	    while(p && p != el){
            if(p.parentNode == el){
            	return p;
            }
            p = p.parentNode;
        }
	    return null;
    },
    
    /** @ignore */
    onClick : function(e){
        var item = this.findItemFromChild(e.getTarget());
        if(item){
            var index = this.indexOf(item);
            if(this.onItemClick(item, index, e) !== false){
                this.fireEvent('click', this, index, item, e);
            }
        }else{
            this.clearSelections();
        }
    },

    /** @ignore */
    onContextMenu : function(e){
        var item = this.findItemFromChild(e.getTarget());
        if(item){
            this.fireEvent('contextmenu', this, this.indexOf(item), item, e);
        }
    },

    /** @ignore */
    onDblClick : function(e){
        var item = this.findItemFromChild(e.getTarget());
        if(item){
            this.fireEvent('dblclick', this, this.indexOf(item), item, e);
        }
    },
    
    onItemClick : function(item, index, e){
        if(this.fireEvent('beforeclick', this, index, item, e) !== false){
            if(this.multiSelect || this.singleSelect){
                if(this.multiSelect && e.shiftKey && this.lastSelection){
                    this.select(this.getNodes(this.indexOf(this.lastSelection), index), false);
                }else{
                    this.select(item, this.multiSelect && e.ctrlKey);
                    this.lastSelection = item;
                }
                e.preventDefault();
            }
            return true;
        }else{
            return false;
        }
    },
    
    /**
     * Get the number of selected nodes.
     * @return {Number}
     */
    getSelectionCount : function(){
        return this.selections.length;
    },
    
    /**
     * Get the currently selected nodes.
     * @return {Array} An array of HTMLElements
     */
    getSelectedNodes : function(){
        return this.selections;
    },
    
    /**
     * Get the indexes of the selected nodes.
     * @return {Array}
     */
    getSelectedIndexes : function(){
        var indexes = [];
        for(var i = 0, len = this.selections.length; i < len; i++) {
        	indexes.push(this.selections[i].nodeIndex);
        }
        return indexes;
    },
    
    /**
     * Clear all selections
     * @param {Boolean} suppressEvent (optional) true to skip firing of the selectionchange event
     */
    clearSelections : function(suppressEvent){
        if(this.multiSelect || this.singleSelect){
            YAHOO.util.Dom.removeClass(this.selections, this.selectedClass);
            this.selections = [];
            if(!suppressEvent){
                this.fireEvent('selectionchange', this, this.selections);
            }
        }
    },
    
    /**
     * Returns true if the passed node is selected
     * @param {HTMLElement/Number} node The node or node index
     * @return {Boolean}
     */
    isSelected : function(node){
        node = this.getNode(node);
        var s = this.selections;
        if(s.length < 1){
            return false;
        }
        if(s.indexOf){
            return s.indexOf(node) !== -1;
        }else{
            for(var i = 0, len = s.length; i < len; i++){
                if (s[i] == node){
                    return true;
                }
            }
            return false;
        }
    },
    
    /**
     * Selects nodes.
     * @param {Array/HTMLElement/String/Number} nodeInfo An HTMLElement template node, index of a template node, id of a template node or an array of any of those to select
     * @param {Boolean} keepExisting (optional) true to keep existing selections
     * @param {Boolean} suppressEvent (optional) true to skip firing of the selectionchange vent
     */
    select : function(nodeInfo, keepExisting, suppressEvent){
        if(!keepExisting){
            this.clearSelections(true);
        }
        if(nodeInfo instanceof Array){
            for(var i = 0, len = nodeInfo.length; i < len; i++) {
            	this.select(nodeInfo[i], true, true);
            }
        }else{
            var node = this.getNode(nodeInfo);
            if(node && !this.isSelected(node)){
                if(this.fireEvent('beforeselect', this, node, this.selections) !== false){
                    YAHOO.util.Dom.addClass(node, this.selectedClass);
                    this.selections.push(node);
                }
            }
        }
        if(!suppressEvent){
            this.fireEvent('selectionchange', this, this.selections);
        }
    },
    
    /**
     * Gets a template node.
     * @param {HTMLElement/String/Number} nodeInfo An HTMLElement template node, index of a template node or the id of a template node
     * @return {HTMLElement} The node or null if it wasn't found
     */
     getNode : function(nodeInfo){
        if(typeof nodeInfo == 'object'){
            return nodeInfo;
        }else if(typeof nodeInfo == 'string'){
            return document.getElementById(nodeInfo);
        }else if(typeof nodeInfo == 'number'){
            return this.nodes[nodeInfo];
        }
        return null;
    },
    
    /**
     * Gets a range template nodes.
     * @param {Number} startIndex
     * @param {Number} endIndex
     * @return {Array} An array of nodes
     */
    getNodes : function(start, end){
        var ns = this.nodes;
        start = start || 0;
        end = typeof end == 'undefined' ? ns.length-1 : end;
        var nodes = [];
        if(start <= end){
            for(var i = start; i <= end; i++) {
        	    nodes.push(ns[i]);
            }
        }else{
            for(var i = start; i >= end; i--) {
        	    nodes.push(ns[i]);
            }
        }
        return nodes;
    },
    
    /**
     * Finds the index of the passed node
     * @param {HTMLElement/String/Number} nodeInfo An HTMLElement template node, index of a template node or the id of a template node
     * @return {Number} The index of the node or -1
     */
     indexOf : function(node){
        node = this.getNode(node);
        if(typeof node.nodeIndex == 'number'){
            return node.nodeIndex;
        }
        var ns = this.nodes;
        for(var i = 0, len = ns.length; i < len; i++) {
        	if(ns[i] == node){
        	    return i;
        	}
        }
        return -1;
    }
});

/**
 * @class YAHOO.ext.JsonView
 * @extends YAHOO.ext.View
 * Shortcut class to create a JSON + UpdateManager template view. Usage:
<pre><code>
var view = new YAHOO.ext.JsonView('my-element', 
           '&lt;div id="{id}"&gt;{foo} - {bar}&lt;/div&gt;', // auto create template
           { multiSelect: true, jsonRoot: 'data' });

// listen for node click?
view.on('click', function(vw, index, node, e){
    alert('Node "' + node.id + '" at index: ' + index + ' was clicked.');
});

// direct load of JSON data
view.load('foobar.php');


// Example from my blog list
var tpl = new YAHOO.ext.Template(
    '&lt;div class="entry"&gt;' + 
       '&lt;a class="entry-title" href="{link}"&gt;{title}&lt;/a&gt;' +
       '&lt;h4&gt;{date} by {author} | {comments} Comments&lt;/h4&gt;{description}' +
    '&lt;/div&gt;&lt;hr /&gt;'
);

var moreView = new YAHOO.ext.JsonView('entry-list', tpl, {
   jsonRoot: 'posts'
});
moreView.on('beforerender', this.sortEntries, this, true);
moreView.load({
    url:'/blog/get-posts.php', 
    params: 'allposts=true',
    text:'Loading Blog Entries...'
});
</code></pre>
 * @constructor
 * Create a new JsonView
 * @param {String/HTMLElement/Element} container The container element where the view is to be rendered.
 * @param {DomHelper.Template} tpl The rendering template
 * @param {Object} config The config object
 */ 
YAHOO.ext.JsonView = function(container, tpl, config){
    var cfg = config || {};
    cfg.renderUpdates = true;
    YAHOO.ext.JsonView.superclass.constructor.call(this, container, tpl, null, cfg);        
    /**
     * @event beforerender
     * Fires before rendering of the downloaded json data.
     * @param {YAHOO.ext.View} this
     * @param {Object} data The json data loaded
     */
     this.events['beforerender'] = true;
     /**
     * @event load
     * Fires when data is loaded.
     * @param {YAHOO.ext.View} this
     * @param {Object} data The json data loaded
     * @param {Object} response The raw Connect response object
     */
     this.events['load'] = true;
     /**
     * @event loadexception
     * Fires when loading fails.
     * @param {YAHOO.ext.View} this
     * @param {Object} response The raw Connect response object
     */
     this.events['loadexception'] = true;
     this.el.getUpdateManager().on('update', this.onLoad, this, true);
     this.el.getUpdateManager().on('failure', this.onLoadException, this, true);
};
YAHOO.extendX(YAHOO.ext.JsonView, YAHOO.ext.View, {
    /**
     * Performs an async request, loading the JSON from the response. If params are specified it uses POST, otherwise it uses GET.
     * @param {Object/String/Function} url The url for this request or a function to call to get the url or a config object containing any of the following options:
<pre><code>
view.load({
    url: 'your-url.php',<br/>
    params: {param1: 'foo', param2: 'bar'}, // or a URL encoded string<br/>
    callback: yourFunction,<br/>
    scope: yourObject, //(optional scope)  <br/>
    discardUrl: false, <br/>
    nocache: false,<br/>
    text: 'Loading...',<br/>
    timeout: 30,<br/>
    scripts: false<br/>
});   
</code></pre>
     * The only required property is url. The optional properties nocache, text and scripts 
     * are shorthand for disableCaching, indicatorText and loadScripts and are used to set their associated property on this UpdateManager instance.
     * @param {<i>String/Object</i>} params (optional) The parameters to pass as either a url encoded string "param1=1&amp;param2=2" or an object {param1: 1, param2: 2}
     * @param {<i>Function</i>} callback (optional) Callback when transaction is complete - called with signature (oElement, bSuccess)
     * @param {<i>Boolean</i>} discardUrl (optional) By default when you execute an update the defaultUrl is changed to the last used url. If true, it will not store the url.
     */
    load : function(){
        var um = this.el.getUpdateManager();
        um.update.apply(um, arguments);
    },
    
    /**
     * Get the number of records in the current JSON dataset
     * @return {Number}
     */
    getCount : function(){
        return this.jsonData ? this.jsonData.length : 0;  
    },
    
    /**
     * Returns the JSON object for the specified node(s)
     * @param {HTMLElement/Array} node The node or an array of nodes
     * @return {Object/Array} If you pass in an array, you get an array back, otherwise 
     * you get the JSON object for the node
     */
    getNodeData : function(node){
        if(node instanceof Array){
            var data = [];
            for(var i = 0, len = node.length; i < len; i++){
                data.push(this.getNodeData(node[i]));
            }
            return data;
        }
        return this.jsonData[this.indexOf(node)] || null; 
    },
    
    beforeRender : function(){
        this.snapshot = this.jsonData;    
        if(this.sortInfo){
            this.sort.apply(this, this.sortInfo);    
        }
        this.fireEvent('beforerender', this, this.jsonData);
    },
    
    onLoad : function(el, o){
       this.fireEvent('load', this, this.jsonData, o);
    },
    
    onLoadException : function(el, o){
       this.fireEvent('loadexception', this, o);
    },
    
    /**
     * Filter the data by a specific property.
     * @param {String} property A property on your JSON objects
     * @param {String/RegExp} value Either string that the property values 
     * should start with or a RegExp to test against the property
     */
    filter : function(property, value){
        if(this.jsonData){
            var data = [];
            var ss = this.snapshot;
            if(typeof value == 'string'){
                var vlen = value.length;
                if(vlen == 0){
                    this.clearFilter();
                    return;
                }
                value = value.toLowerCase();
                for(var i = 0, len = ss.length; i < len; i++){
    				var o = ss[i];
    				if(o[property].substr(0, vlen).toLowerCase() == value){
    					data.push(o);
    				}
    			}
            }else if(value.exec){ // regex?
                for(var i = 0, len = ss.length; i < len; i++){
    				var o = ss[i];
    				if(value.test(o[property])){
    					data.push(o);
    				}
    			}
            }else{
                return;
            }
            this.jsonData = data;
    		this.refresh();
        }
	},
    
    /**
     * Filter by a function. The passed function will be called with each 
     * object in the current dataset. If the function returns true, the value is kept 
     * otherwise it is filtered.
     * @param {Function} fn
     * @param {Object} scope (optional) The scope of the function (defaults to this JsonView) 
     */
    filterBy : function(fn, scope){
        if(this.jsonData){
            var data = [];
            var ss = this.snapshot;
            for(var i = 0, len = ss.length; i < len; i++){
    			var o = ss[i];
    			if(fn.call(scope|| this, o)){
    				data.push(o);
    			}
    		}
    		this.jsonData = data;
    		this.refresh();
        }
    },
    
    /**
     * Clears the current filter.
     */
    clearFilter : function(){
        if(this.snapshot && this.jsonData != this.snapshot){
            this.jsonData = this.snapshot;
            this.refresh();
        }   
    },
    
    
    /**
     * Sorts the data for this view and refreshes it.
     * @param {String} property A property on your JSON objects to sort on
     * @param {String} direction (optional) desc or asc (defaults to asc) 
     * @param {Function} sortType (optional) A function to call to convert the data to a sortable value.
     */
    sort : function(property, dir, sortType){
        this.sortInfo = Array.prototype.slice.call(arguments, 0);
        if(this.jsonData){
            var p = property;
            var dsc = dir && dir.toLowerCase() == 'desc';
            var f = function(o1, o2){
            	var v1 = sortType ? sortType(o1[p]) : o1[p];
            	var v2 = sortType ? sortType(o2[p]) : o2[p];;
            	if(v1 < v2){
        			return dsc ? +1 : -1;
        		}else if(v1 > v2){
        			return dsc ? -1 : +1;
                }else{
        	    	return 0;
                }
            };
            this.jsonData.sort(f);
            this.refresh();
            if(this.jsonData != this.snapshot){
            	this.snapshot.sort(f);
            }
        }
    }
});
