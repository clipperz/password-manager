/**
 * @class YAHOO.ext.grid.Grid
 * @extends YAHOO.ext.util.Observable
 * This class represents the primary interface of a component based grid control.
 * <br><br>Usage:<pre><code>
 var grid = new YAHOO.ext.grid.Grid('my-container-id', dataModel, columnModel);
 // set any options
 grid.render();
 // or using a config
 var grid = new YAHOO.ext.grid.Grid('my-container-id', {
     dataModel: myDataModel,
     colModel: myColModel,
     selModel: mySelectionModel,
     autoSizeColumns: true,
     monitorWindowResize: false,
     trackMouseOver: true
 }).render();
 * </code></pre>
 * <b>Common Problems:</b><br/>
 * - Grid does not resize properly when going smaller: Setting overflow hidden on the container 
 * element will correct this<br/>
 * - If you get el.style[camel]= NaNpx or -2px or something related, be certain you have given your container element 
 * dimensions. The grid adapts to your container's size, if your container has no size defined then the results
 * are unpredictable.<br/>
 * - Do not render the grid into an element with display:none. Try using visibility:hidden. Otherwise there is no way for the 
 * grid to calculate dimensions/offsets.<br/>
 * @requires YAHOO.util.Dom
 * @requires YAHOO.util.Event
 * @requires YAHOO.util.CustomEvent 
 * @requires YAHOO.ext.Element
 * @requires YAHOO.ext.util.Browser
 * @requires YAHOO.ext.util.CSS
 * @requires YAHOO.ext.SplitBar 
 * @requires YAHOO.ext.EventObject 
 * @constructor
 * @param {String/HTMLElement/YAHOO.ext.Element} container The element into which this grid will be rendered - 
 * The container MUST have some type of size defined for the grid to fill. The container will be 
 * automatically set to position relative if it isn't already.
 * @param {Object} config A config object that sets properties on this grid OR the data model to bind to
 * @param {Object} colModel (optional) The column model with info about this grid's columns
 * @param {Object} selectionModel (optional) The selection model for this grid (defaults to DefaultSelectionModel)
 */
YAHOO.ext.grid.Grid = function(container, config, colModel, selectionModel){
	/** @private */
	this.container = YAHOO.ext.Element.get(container);
	this.container.update('');
	this.container.setStyle('overflow', 'hidden');
	this.id = this.container.id;
	this.rows = [];
    this.rowCount = 0;
    this.fieldId = null;
    var dataModel = config; // for legacy pre config support
    this.dataModel = dataModel;
    this.colModel = colModel;
    this.selModel = selectionModel;
	this.activeEditor = null;
	this.editingCell = null;
	
	
	if(typeof config == 'object' && !config.getRowCount){// must be config object
	    YAHOO.ext.util.Config.apply(this, config);
	}
	
	/** @private */
	this.setValueDelegate = this.setCellValue.createDelegate(this);
	
	/** @private */
	this.events = {
	    // raw events
	    /**
	     * @event click
	     * The raw click event for the entire grid.
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'click' : true,
	    /**
	     * @event dblclick
	     * The raw dblclick event for the entire grid.
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'dblclick' : true,
	    /**
	     * @event mousedown
	     * The raw mousedown event for the entire grid.
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'mousedown' : true,
	    /**
	     * @event mouseup
	     * The raw mouseup event for the entire grid.
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'mouseup' : true,
	    /**
	     * @event mouseover
	     * The raw mouseover event for the entire grid.
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'mouseover' : true,
	    /**
	     * @event mouseout
	     * The raw mouseout event for the entire grid.
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'mouseout' : true,
	    /**
	     * @event keypress
	     * The raw keypress event for the entire grid.
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'keypress' : true,
	    /**
	     * @event keydown
	     * The raw keydown event for the entire grid.
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'keydown' : true,
	    
	    // custom events
	    
	    /**
	     * @event cellclick
	     * Fires when a cell is clicked
	     * @param {Grid} this
	     * @param {Number} rowIndex
	     * @param {Number} columnIndex
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'cellclick' : true,
	    /**
	     * @event celldblclick
	     * Fires when a cell is double clicked
	     * @param {Grid} this
	     * @param {Number} rowIndex
	     * @param {Number} columnIndex
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'celldblclick' : true,
	    /**
	     * @event rowclick
	     * Fires when a row is clicked
	     * @param {Grid} this
	     * @param {Number} rowIndex
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'rowclick' : true,
	    /**
	     * @event rowdblclick
	     * Fires when a row is double clicked
	     * @param {Grid} this
	     * @param {Number} rowIndex
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'rowdblclick' : true,
	    /**
	     * @event headerclick
	     * Fires when a header is clicked
	     * @param {Grid} this
	     * @param {Number} columnIndex
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'headerclick' : true,
	    /**
	     * @event rowcontextmenu
	     * Fires when a row is right clicked
	     * @param {Grid} this
	     * @param {Number} rowIndex
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'rowcontextmenu' : true,
	    /**
        * @event cellcontextmenu
       * Fires when a cell is right clicked
       * @param {Grid} this
       * @param {Number} rowIndex
       * @param {Number} cellIndex
       * @param {YAHOO.ext.EventObject} e
       */
      'cellcontextmenu' : true,
	    /**
	     * @event headercontextmenu
	     * Fires when a header is right clicked
	     * @param {Grid} this
	     * @param {Number} columnIndex
	     * @param {YAHOO.ext.EventObject} e
	     */
	    'headercontextmenu' : true,
	    /**
	     * @event beforeedit
	     * Fires before a cell is edited
	     * @param {Grid} this
	     * @param {Number} rowIndex
	     * @param {Number} columnIndex
	     */
	    'beforeedit' : true,
	    /**
	     * @event afteredit
	     * Fires after a cell is edited
	     * @param {Grid} this
	     * @param {Number} rowIndex
	     * @param {Number} columnIndex
	     */
	    'afteredit' : true,
	    /**
	     * @event bodyscroll
	     * Fires when the body element is scrolled
	     * @param {Number} scrollLeft
	     * @param {Number} scrollTop
	     */
	    'bodyscroll' : true,
	    /**
	     * @event columnresize
	     * Fires when the user resizes a column
	     * @param {Number} columnIndex
	     * @param {Number} newSize
	     */
	    'columnresize' : true,
	    /**
	     * @event startdrag
	     * Fires when row(s) start being dragged 
	     * @param {Grid} this
	     * @param {YAHOO.ext.GridDD} dd The drag drop object
	     * @param {event} e The raw browser event
	     */
	    'startdrag' : true,
	    /**
	     * @event enddrag
	     * Fires when a drag operation is complete
	     * @param {Grid} this
	     * @param {YAHOO.ext.GridDD} dd The drag drop object
	     * @param {event} e The raw browser event
	     */
	    'enddrag' : true,
	    /**
	     * @event dragdrop
	     * Fires when dragged row(s) are dropped on a valid DD target 
	     * @param {Grid} this
	     * @param {YAHOO.ext.GridDD} dd The drag drop object
	     * @param {String} targetId The target drag drop object
	     * @param {event} e The raw browser event
	     */
	    'dragdrop' : true,
	    /**
	     * @event dragover
	     * Fires while row(s) are being dragged. "targetId" is the id of the Yahoo.util.DD object the selected rows are being dragged over.
	     * @param {Grid} this
	     * @param {YAHOO.ext.GridDD} dd The drag drop object
	     * @param {String} targetId The target drag drop object
	     * @param {event} e The raw browser event
	     */
	    'dragover' : true,
	    /**
	     * @event dragenter
	     *  Fires when the dragged row(s) first cross another DD target while being dragged 
	     * @param {Grid} this
	     * @param {YAHOO.ext.GridDD} dd The drag drop object
	     * @param {String} targetId The target drag drop object
	     * @param {event} e The raw browser event
	     */
	    'dragenter' : true,
	    /**
	     * @event dragout
	     * Fires when the dragged row(s) leave another DD target while being dragged 
	     * @param {Grid} this
	     * @param {YAHOO.ext.GridDD} dd The drag drop object
	     * @param {String} targetId The target drag drop object
	     * @param {event} e The raw browser event
	     */
	    'dragout' : true
	};
};

YAHOO.ext.grid.Grid.prototype = { 
    /** The minimum width a column can be resized to. (Defaults to 25)
	 * @type Number */
	minColumnWidth : 25,
	
	/** True to automatically resize the columns to fit their content <b>on initial render</b>
	 * @type Boolean */
	autoSizeColumns : false,
	
	/** True to measure headers with column data when auto sizing columns
	 * @type Boolean */
	autoSizeHeaders : false,
	
	/**
	 * True to autoSize the grid when the window resizes - defaults to true
	 */
	monitorWindowResize : true,
	
	/** If autoSizeColumns is on, maxRowsToMeasure can be used to limit the number of
	 * rows measured to get a columns size - defaults to 0 (all rows).
	 * @type Number */
	maxRowsToMeasure : 0,
	
	/** True to highlight rows when the mouse is over (default is false)
	 * @type Boolean */
	trackMouseOver : false,
	
	/** True to enable drag and drop of rows
	 * @type Boolean */
	enableDragDrop : false,
	
	/** True to stripe the rows (default is true)
	 * @type Boolean */
	stripeRows : true,
	/** True to fit the height of the grid container to the height of the data (defaults to false)
	 * @type Boolean */
	autoHeight : false,
	
	/** True to fit the width of the grid container to the width of the columns (defaults to false)
	 * @type Boolean */
	autoWidth : false,
	
	/**
	 * The view used by the grid. This can be set before a call to render(). 
	 * Defaults to a YAHOO.ext.grid.GridView or PagedGridView depending on the data model.
	 * @type Object
	 */
	view : null,
	
	/** A regular expression defining tagNames 
     * allowed to have text selection (Defaults to <code>/INPUT|TEXTAREA|SELECT/i</code>) */
    allowTextSelectionPattern : /INPUT|TEXTAREA|SELECT/i,
    
    /**
     * Called once after all setup has been completed and the grid is ready to be rendered.
     * @return {YAHOO.ext.grid.Grid} this
     */
    render : function(){
        if((!this.container.dom.offsetHeight || this.container.dom.offsetHeight < 20) 
                || this.container.getStyle('height') == 'auto'){
    	    this.autoHeight = true;   
    	}	       
    	if((!this.container.dom.offsetWidth || this.container.dom.offsetWidth < 20)){
    	    this.autoWidth = true;   
    	}	       
    	if(!this.view){
    	    if(this.dataModel.isPaged()){
    		    this.view = new YAHOO.ext.grid.PagedGridView();
    	    }else{
    	        this.view = new YAHOO.ext.grid.GridView();
    	    }
    	}
    	this.view.init(this);
        this.el = getEl(this.view.render(), true);
        var c = this.container;
        c.mon("click", this.onClick, this, true);
        c.mon("dblclick", this.onDblClick, this, true);
        c.mon("contextmenu", this.onContextMenu, this, true);
        c.mon("selectstart", this.cancelTextSelection, this, true);
        c.mon("mousedown", this.cancelTextSelection, this, true);
        c.mon("mousedown", this.onMouseDown, this, true);
        c.mon("mouseup", this.onMouseUp, this, true);
        if(this.trackMouseOver){
            this.el.mon("mouseover", this.onMouseOver, this, true);
            this.el.mon("mouseout", this.onMouseOut, this, true);
        }
        c.mon("keypress", this.onKeyPress, this, true);
        c.mon("keydown", this.onKeyDown, this, true);
        this.init();
        return this;
    },
    
    init : function(){
        this.rows = this.el.dom.rows;
        if(!this.disableSelection){
	        if(!this.selModel){
	            this.selModel = new YAHOO.ext.grid.DefaultSelectionModel(this);
	        }
	        this.selModel.init(this);
	        this.selModel.onSelectionChange.subscribe(this.updateField, this, true);
        }else{
            this.selModel = new YAHOO.ext.grid.DisableSelectionModel(this);
            this.selModel.init(this);
        }
        
        if(this.enableDragDrop){
            this.dd = new YAHOO.ext.grid.GridDD(this, this.container.dom);
        }
     },   

    /**
     * Resets the grid for use with a new configuration and/or data and column models. After calling this function
     * you will need to call render() again. Any listeners for this grid will be retained.
     * Warning: any listeners manually attached (not through the grid) to the grid's container 
     * element will be removed. 
     * @param {Object} config Standard config object with properties to set on this grid
     * @return {YAHOO.ext.grid.Grid} this
     */
    reset : function(config){
        this.destroy(false, true);
        YAHOO.ext.util.Config.apply(this, config);
        return this;
    },
    
    /**
     * Destroy this grid. 
     * @param {Boolean} removeEl True to remove the element
     */
    destroy : function(removeEl, keepListeners){
        var c = this.container;
        c.removeAllListeners();
        this.view.destroy();
        YAHOO.ext.EventManager.removeResizeListener(this.view.onWindowResize, this.view);
        this.view = null;
        this.colModel.purgeListeners();
        if(!keepListeners){
            this.purgeListeners();
        }
        c.update('');
        if(removeEl === true){
            c.remove();
        }
    },
    
    /**
     * Replace the current data model with a new one (experimental)
     * @param {DataModel} dm The new data model
     * @pram {Boolean} rerender true to render the grid rows from scratch
     */
    setDataModel : function(dm, rerender){
        this.view.unplugDataModel(this.dataModel);
        this.dataModel = dm;
        this.view.plugDataModel(dm);
        if(rerender){
            dm.fireEvent('datachanged');
        }
    },
    
    onMouseDown : function(e){
        this.fireEvent('mousedown', e);
    },
    
    onMouseUp : function(e){
        this.fireEvent('mouseup', e);
    },
    
    onMouseOver : function(e){
        this.fireEvent('mouseover', e);
    },
    
    onMouseOut : function(e){
        this.fireEvent('mouseout', e);
    },
    
    onKeyPress : function(e){
        this.fireEvent('keypress', e);
    },
    
    onKeyDown : function(e){
        this.fireEvent('keydown', e);
    },
    
    fireEvent : YAHOO.ext.util.Observable.prototype.fireEvent,
    on : YAHOO.ext.util.Observable.prototype.on,
    addListener : YAHOO.ext.util.Observable.prototype.addListener,
    delayedListener : YAHOO.ext.util.Observable.prototype.delayedListener,
    removeListener : YAHOO.ext.util.Observable.prototype.removeListener,
    purgeListeners : YAHOO.ext.util.Observable.prototype.purgeListeners,
    bufferedListener : YAHOO.ext.util.Observable.prototype.bufferedListener,
    
    onClick : function(e){
        this.fireEvent('click', e);
        var target = e.getTarget();
        var row = this.getRowFromChild(target);
        var cell = this.getCellFromChild(target);
        var header = this.getHeaderFromChild(target);
        if(cell){
            this.fireEvent('cellclick', this, row.rowIndex, cell.columnIndex, e);
        }
        if(row){
            this.fireEvent('rowclick', this, row.rowIndex, e);
        }
        if(header){
            this.fireEvent('headerclick', this, header.columnIndex, e);
        }
    },

    onContextMenu : function(e){
        var target = e.getTarget();
        var row = this.getRowFromChild(target);
        var cell = this.getCellFromChild(target);
        var header = this.getHeaderFromChild(target);
        if(cell){
            this.fireEvent('cellcontextmenu', this, row.rowIndex, cell.columnIndex, e);
        }
        if(row){
            this.fireEvent('rowcontextmenu', this, row.rowIndex, e);
        }
        if(header){
            this.fireEvent('headercontextmenu', this, header.columnIndex, e);
        }
        e.preventDefault();
    },

    onDblClick : function(e){
        this.fireEvent('dblclick', e);
        var target = e.getTarget();
        var row = this.getRowFromChild(target);
        var cell = this.getCellFromChild(target);
        if(row){
            this.fireEvent('rowdblclick', this, row.rowIndex, e);
        }
        if(cell){
            this.fireEvent('celldblclick', this, row.rowIndex, cell.columnIndex, e);
        }
    },
    
    /**
     * Starts editing the specified for the specified row/column
     * @param {Number} rowIndex
     * @param {Number} colIndex
     */
    startEditing : function(rowIndex, colIndex){
        var row = this.rows[rowIndex];
        var cell = row.childNodes[colIndex];
        this.stopEditing();
        setTimeout(this.doEdit.createDelegate(this, [row, cell]), 10);
    },
        
    /**
     * Stops any active editing
     */
    stopEditing : function(){
        if(this.activeEditor){
            this.activeEditor.stopEditing();
        }
    },
        
    /** @ignore */
    doEdit : function(row, cell){
        if(!row || !cell) return;
        var cm = this.colModel;
        var dm = this.dataModel;
        var colIndex = cell.columnIndex;
        var rowIndex = row.rowIndex;
        if(cm.isCellEditable(colIndex, rowIndex)){
           var ed = cm.getCellEditor(colIndex, rowIndex);
           if(ed){
               if(this.activeEditor){
                   this.activeEditor.stopEditing();
               }
               this.fireEvent('beforeedit', this, rowIndex, colIndex);
               this.activeEditor = ed;
               this.editingCell = cell;
               this.view.ensureVisible(row, true);
               try{
                   cell.focus();
               }catch(e){}
               ed.init(this, this.el.dom.parentNode, this.setValueDelegate);
               var value = dm.getValueAt(rowIndex, cm.getDataIndex(colIndex));
               // set timeout so firefox stops editing before starting a new edit
               setTimeout(ed.startEditing.createDelegate(ed, [value, row, cell]), 1);
           }   
        }  
    },
    
    setCellValue : function(value, rowIndex, colIndex){
         this.dataModel.setValueAt(value, rowIndex, this.colModel.getDataIndex(colIndex));
         this.fireEvent('afteredit', this, rowIndex, colIndex);
    },
    
    /** @ignore Called when text selection starts or mousedown to prevent default */
    cancelTextSelection : function(e){
        var target = e.getTarget();
        if(target && target != this.el.dom.parentNode && !this.allowTextSelectionPattern.test(target.tagName)){
            e.preventDefault();
        }
    },
    
    /**
     * Causes the grid to manually recalculate it's dimensions. Generally this is done automatically, 
     * but if manual update is required this method will initiate it.
     */
    autoSize : function(){
        this.view.updateWrapHeight();
        this.view.adjustForScroll();
    },
    
    /**
     * Scrolls the grid to the specified row
     * @param {Number/HTMLElement} row The row object or index of the row
     */
    scrollTo : function(row){
        if(typeof row == 'number'){
            row = this.rows[row];
        }
        this.view.ensureVisible(row, true);
    },
    
    /** @private */
    getEditingCell : function(){
        return this.editingCell;    
    },
    
    /**
     * Binds this grid to the field with the specified id. Initially reads and parses the comma 
     * delimited ids in the field and selects those items. All selections made in the grid
     * will be persisted to the field by their ids comma delimited.
     * @param {String} The id of the field to bind to
     */
    bindToField : function(fieldId){
        this.fieldId = fieldId;
        this.readField();
    },
    
    /** @private */
    updateField : function(){
        if(this.fieldId){
            var field = YAHOO.util.Dom.get(this.fieldId);
            field.value = this.getSelectedRowIds().join(',');
        }
    },
    
    /**
     * Causes the grid to read and select the ids from the bound field - See {@link #bindToField}.
     */
    readField : function(){
        if(this.fieldId){
            var field = YAHOO.util.Dom.get(this.fieldId);
            var values = field.value.split(',');
            var rows = this.getRowsById(values);
            this.selModel.selectRows(rows, false);
        }
    },
	
	/**
	 * Returns the table row at the specified index
	 * @param {Number} index
	 * @return {HTMLElement} 
	 */
    getRow : function(index){
        return this.rows[index];
    },
	
	/**
	 * Returns the rows that have the specified id(s). The id value for a row is provided 
	 * by the DataModel. See {@link YAHOO.ext.grid.DefaultDataModel#getRowId}.
	 * @param {String/Array} An id to find or an array of ids
	 * @return {HtmlElement/Array} If one id was passed in, it returns one result. 
	 * If an array of ids was specified, it returns an Array of HTMLElements
	 */
    getRowsById : function(id){
        var dm = this.dataModel;
        if(!(id instanceof Array)){
            for(var i = 0; i < this.rows.length; i++){
                if(dm.getRowId(i) == id){
                    return this.rows[i];
                }
            }
            return null;
        }
        var found = [];
        var re = "^(?:";
        for(var i = 0; i < id.length; i++){
            re += id[i];
            if(i != id.length-1) re += "|";
        }
        var regex = new RegExp(re + ")$");
        for(var i = 0; i < this.rows.length; i++){
            if(regex.test(dm.getRowId(i))){
                found.push(this.rows[i]);
            }
        }
        return found;
    },
    
    /**
	 * Returns the row that comes after the specified row - text nodes are skipped.
	 * @param {HTMLElement} row
	 * @return {HTMLElement} 
	 */
    getRowAfter : function(row){
        return this.getSibling('next', row);
    },
    
    /**
	 * Returns the row that comes before the specified row - text nodes are skipped.
	 * @param {HTMLElement} row
	 * @return {HTMLElement} 
	 */
    getRowBefore : function(row){
        return this.getSibling('previous', row);
    },
    
    /**
	 * Returns the cell that comes after the specified cell - text nodes are skipped.
	 * @param {HTMLElement} cell
	 * @param {Boolean} includeHidden
	 * @return {HTMLElement} 
	 */
    getCellAfter : function(cell, includeHidden){
        var next = this.getSibling('next', cell);
        if(next && !includeHidden && this.colModel.isHidden(next.columnIndex)){
            return this.getCellAfter(next);
        }
        return next;
    },
    
    /**
	 * Returns the cell that comes before the specified cell - text nodes are skipped.
	 * @param {HTMLElement} cell
	 * @param {Boolean} includeHidden
	 * @return {HTMLElement} 
	 */
    getCellBefore : function(cell, includeHidden){
        var prev = this.getSibling('previous', cell);
        if(prev && !includeHidden && this.colModel.isHidden(prev.columnIndex)){
            return this.getCellBefore(prev);
        }
        return prev;
    },
    
    /**
	 * Returns the last cell for the row - text nodes and hidden columns are skipped.
	 * @param {HTMLElement} row
	 * @param {Boolean} includeHidden
	 * @return {HTMLElement} 
	 */
    getLastCell : function(row, includeHidden){
        var cell = this.getElement('previous', row.lastChild);
        if(cell && !includeHidden && this.colModel.isHidden(cell.columnIndex)){
            return this.getCellBefore(cell);
        }
        return cell;
    },
    
    /**
	 * Returns the first cell for the row - text nodes and hidden columns are skipped.
	 * @param {HTMLElement} row
	 * @param {Boolean} includeHidden
	 * @return {HTMLElement} 
	 */
    getFirstCell : function(row, includeHidden){
        var cell = this.getElement('next', row.firstChild);
        if(cell && !includeHidden && this.colModel.isHidden(cell.columnIndex)){
            return this.getCellAfter(cell);
        }
        return cell;
    },
    
    /**
     * @private
     * Gets siblings, skipping text nodes
     * @param {String} type The direction to walk: 'next' or 'previous'
     * @param {HTMLElement} node
	 */
    getSibling : function(type, node){
        if(!node) return null;
        type += 'Sibling';
        var n = node[type];
        while(n && n.nodeType != 1){
            n = n[type];
        }
        return n;
    },
    
    /**
     * Returns node if node is an HTMLElement else walks the siblings in direction looking for 
     * a node that is an element
     * @param {String} direction The direction to walk: 'next' or 'previous'
     * @private
     */
    getElement : function(direction, node){
        if(!node || node.nodeType == 1) return node;
        else return this.getSibling(direction, node);
    },
    
    /**
     * @private
     */
    getElementFromChild : function(childEl, parentClass){
        if(!childEl || (YAHOO.util.Dom.hasClass(childEl, parentClass))){
		    return childEl;
	    }
	    var p = childEl.parentNode;
	    var b = document.body;
	    while(p && p != b){
            if(YAHOO.util.Dom.hasClass(p, parentClass)){
            	return p;
            }
            p = p.parentNode;
        }
	    return null;
    },
    
    /**
	 * Returns the row that contains the specified child element.
	 * @param {HTMLElement} childEl
	 * @return {HTMLElement} 
	 */
    getRowFromChild : function(childEl){
        return this.getElementFromChild(childEl, 'ygrid-row');
    },
    
    /**
	 * Returns the cell that contains the specified child element.
	 * @param {HTMLElement} childEl
	 * @return {HTMLElement} 
	 */
    getCellFromChild : function(childEl){
        return this.getElementFromChild(childEl, 'ygrid-col');
    },
    
    
    /**
     * Returns the header element that contains the specified child element.
     * @param {HTMLElement}  childEl
	 * @return {HTMLElement} 
	 */
     getHeaderFromChild : function(childEl){
        return this.getElementFromChild(childEl, 'ygrid-hd');
    },
    
    /**
     * Convenience method for getSelectionModel().getSelectedRows() - 
     * See <small>{@link YAHOO.ext.grid.DefaultSelectionModel#getSelectedRows}</small> for more details.
     * @return {Array}
     */
    getSelectedRows : function(){
        return this.selModel.getSelectedRows();
    },
    
    /**
     * Convenience method for getSelectionModel().getSelectedRows()[0] - 
     * See <small>{@link YAHOO.ext.grid.DefaultSelectionModel#getSelectedRows}</small> for more details.
     * @return {HTMLElement}
     */
    getSelectedRow : function(){
        if(this.selModel.hasSelection()){
            return this.selModel.getSelectedRows()[0];
        }
        return null;
    },
    
    /**
     * Get the selected row indexes
     * @return {Array} Array of indexes
     */
    getSelectedRowIndexes : function(){
        var a = [];
        var rows = this.selModel.getSelectedRows();
        for(var i = 0; i < rows.length; i++) {
        	a[i] = rows[i].rowIndex;
        }
        return a;
    },
    
    /**
     * Gets the first selected row or -1 if none are selected
     * @return {Number}
     */
    getSelectedRowIndex : function(){
        if(this.selModel.hasSelection()){
           return this.selModel.getSelectedRows()[0].rowIndex;
        }
        return -1;
    },
    
    /**
     * Convenience method for getSelectionModel().getSelectedRowIds()[0] - 
     * See <small>{@link YAHOO.ext.grid.DefaultSelectionModel#getSelectedRowIds}</small> for more details.
     * @return {String}
     */
    getSelectedRowId : function(){
        if(this.selModel.hasSelection()){
           return this.selModel.getSelectedRowIds()[0];
        }
        return null;
    },
    
    /**
     * Convenience method for getSelectionModel().getSelectedRowIds() - 
     * See <small>{@link YAHOO.ext.grid.DefaultSelectionModel#getSelectedRowIds}</small> for more details.
     * @return {Array}
     */
    getSelectedRowIds : function(){
        return this.selModel.getSelectedRowIds();
    },
    
    /**
     * Convenience method for getSelectionModel().clearSelections() - 
     * See <small>{@link YAHOO.ext.grid.DefaultSelectionModel#clearSelections}</small> for more details.
     */
    clearSelections : function(){
        this.selModel.clearSelections();
    },
    
        
    /**
     * Convenience method for getSelectionModel().selectAll() - 
     * See <small>{@link YAHOO.ext.grid.DefaultSelectionModel#selectAll}</small> for more details.
     */
    selectAll : function(){
        this.selModel.selectAll();
    },
    
        
    /**
     * Convenience method for getSelectionModel().getCount() - 
     * See <small>{@link YAHOO.ext.grid.DefaultSelectionModel#getCount}</small> for more details.
     * @return {Number}
     */
    getSelectionCount : function(){
        return this.selModel.getCount();
    },
    
    /**
     * Convenience method for getSelectionModel().hasSelection() - 
     * See <small>{@link YAHOO.ext.grid.DefaultSelectionModel#hasSelection}</small> for more details.
     * @return {Boolean}
     */
    hasSelection : function(){
        return this.selModel.hasSelection();
    },
    
    /**
     * Returns the grid's SelectionModel.
     * @return {SelectionModel}
     */
    getSelectionModel : function(){
        if(!this.selModel){
            this.selModel = new DefaultSelectionModel();
        }
        return this.selModel;
    },
    
    /**
     * Returns the grid's DataModel.
     * @return {DataModel}
     */
    getDataModel : function(){
        return this.dataModel;
    },
    
    /**
     * Returns the grid's ColumnModel.
     * @return {ColumnModel}
     */
    getColumnModel : function(){
        return this.colModel;
    },
    
    /**
     * Returns the grid's GridView object.
     * @return {GridView}
     */
    getView : function(){
        return this.view;
    },
    /**
     * Called to get grid's drag proxy text, by default returns this.ddText. 
     * @return {String}
     */
    getDragDropText : function(){
        return this.ddText.replace('%0', this.selModel.getCount());
    }
};
/**
 * Configures the text is the drag proxy (defaults to "%0 selected row(s)"). 
 * %0 is replaced with the number of selected rows.
 * @type String
 */
YAHOO.ext.grid.Grid.prototype.ddText = "%0 selected row(s)";
