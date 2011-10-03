/**
 * @class YAHOO.ext.grid.GridView
 * Default UI code used internally by the Grid. This is the object returned by {@link YAHOO.ext.grid.Grid#getView}.
 * @constructor
 */
YAHOO.ext.grid.GridView = function(){
	this.grid = null;
	this.lastFocusedRow = null;
	this.onScroll = new YAHOO.util.CustomEvent('onscroll');
	this.adjustScrollTask = new YAHOO.ext.util.DelayedTask(this._adjustForScroll, this);
	this.ensureVisibleTask = new YAHOO.ext.util.DelayedTask();
};

YAHOO.ext.grid.GridView.prototype = {
	init: function(grid){
		this.grid = grid;
	},
	
	fireScroll: function(scrollLeft, scrollTop){
		this.onScroll.fireDirect(this.grid, scrollLeft, scrollTop);
	},
	
	/**
	 * @private
	 * Utility method that gets an array of the cell renderers
	 */
	getColumnRenderers : function(){
    	var renderers = [];
    	var cm = this.grid.colModel;
        var colCount = cm.getColumnCount();
        for(var i = 0; i < colCount; i++){
            renderers.push(cm.getRenderer(i));
        }
        return renderers;
    },
    
    buildIndexMap : function(){
        var colToData = {};
        var dataToCol = {};
        var cm = this.grid.colModel;
        for(var i = 0, len = cm.getColumnCount(); i < len; i++){
            var di = cm.getDataIndex(i);
            colToData[i] = di;
            dataToCol[di] = i;
        }
        return {'colToData': colToData, 'dataToCol': dataToCol};
    },
    
    getDataIndexes : function(){
    	if(!this.indexMap){
            this.indexMap = this.buildIndexMap();
        }
        return this.indexMap.colToData;
    },
    
    getColumnIndexByDataIndex : function(dataIndex){
        if(!this.indexMap){
            this.indexMap = this.buildIndexMap();
        }
    	return this.indexMap.dataToCol[dataIndex];
    },
    
    updateHeaders : function(){
        var colModel = this.grid.colModel;
        var hcells = this.headers;
        var colCount = colModel.getColumnCount();
        for(var i = 0; i < colCount; i++){
            hcells[i].textNode.innerHTML = colModel.getColumnHeader(i);
        }
    },
    
    adjustForScroll : function(disableDelay){
        if(!disableDelay){
            this.adjustScrollTask.delay(50);
        }else{
            this._adjustForScroll();
        }
    },
    
    /**
     * Returns the rowIndex/columnIndex of the cell found at the passed page coordinates
     * @param {Number} x
     * @param {Number} y
     * @return {Array} [rowIndex, columnIndex]
     */
     getCellAtPoint : function(x, y){
        var colIndex = null;        
        var rowIndex = null;
        
        // translate page coordinates to local coordinates
        var xy = YAHOO.util.Dom.getXY(this.wrap);
        x = (x - xy[0]) + this.wrap.scrollLeft;
        y = (y - xy[1]) + this.wrap.scrollTop;
        
        var colModel = this.grid.colModel;
        var pos = 0;
        var colCount = colModel.getColumnCount();
        for(var i = 0; i < colCount; i++){
            if(colModel.isHidden(i)) continue;
            var width = colModel.getColumnWidth(i);
            if(x >= pos && x < pos+width){
                colIndex = i;
                break;
            }
            pos += width;
        }
        if(colIndex != null){
            rowIndex = (y == 0 ? 0 : Math.floor(y / this.getRowHeight()));
            if(rowIndex >= this.grid.dataModel.getRowCount()){
                return null;
            }
            return [colIndex, rowIndex];
        }
        return null;
    },
    
    /** @private */
    _adjustForScroll : function(){
        this.forceScrollUpdate();
        if(this.scrollbarMode == YAHOO.ext.grid.GridView.SCROLLBARS_OVERLAP){
            var adjustment = 0;
            if(this.wrap.clientWidth && this.wrap.clientWidth !== 0){
                adjustment = this.wrap.offsetWidth - this.wrap.clientWidth;
            }
            this.hwrap.setWidth(this.wrap.offsetWidth-adjustment);
        }else{
            this.hwrap.setWidth(this.wrap.offsetWidth);
        }
        this.bwrap.setWidth(Math.max(this.grid.colModel.getTotalWidth(), this.wrap.clientWidth));
    },

    /**
     * Focuses the specified row. The preferred way to scroll to a row is {@link #ensureVisible}.
     * @param {Number/HTMLElement} row The index of a row or the row itself
     */
     focusRow : function(row){
        if(typeof row == 'number'){
            row = this.getBodyTable().childNodes[row];
        }
        if(!row) return;
    	var left = this.wrap.scrollLeft;
    	try{ // try catch for IE occasional focus bug
    	    row.childNodes.item(0).hideFocus = true;
        	row.childNodes.item(0).focus();
        }catch(e){}
        this.ensureVisible(row);
        this.wrap.scrollLeft = left;
        this.handleScroll();
        this.lastFocusedRow = row;
    },

    /**
     * Scrolls the specified row into view. This call is automatically buffered (delayed), to disable
     * the delay, pass true for disableDelay. 
     * @param {Number/HTMLElement} row The index of a row or the row itself
     * @param {Boolean} disableDelay
     */
     ensureVisible : function(row, disableDelay){
        if(!disableDelay){
            this.ensureVisibleTask.delay(50, this._ensureVisible, this, [row]);
        }else{
            this._ensureVisible(row);
        }
    },

    /** @ignore */
    _ensureVisible : function(row){
        if(typeof row == 'number'){
            row = this.getBodyTable().childNodes[row];
        }
        if(!row) return;
    	var left = this.wrap.scrollLeft;
    	var rowTop = parseInt(row.offsetTop, 10); // parseInt for safari bug
        var rowBottom = rowTop + row.offsetHeight;
        var clientTop = parseInt(this.wrap.scrollTop, 10); // parseInt for safari bug
        var clientBottom = clientTop + this.wrap.clientHeight;
        if(rowTop < clientTop){
        	this.wrap.scrollTop = rowTop;
        }else if(rowBottom > clientBottom){
            this.wrap.scrollTop = rowBottom-this.wrap.clientHeight;
        }
        this.wrap.scrollLeft = left;
        this.handleScroll();
    },
    
    updateColumns : function(){
        this.grid.stopEditing();
        var colModel = this.grid.colModel;
        var hcols = this.headers;
        var colCount = colModel.getColumnCount();
        var pos = 0;
        var totalWidth = colModel.getTotalWidth();
        for(var i = 0; i < colCount; i++){
            if(colModel.isHidden(i)) continue;
            var width = colModel.getColumnWidth(i);
            hcols[i].style.width = width + 'px';
            hcols[i].style.left = pos + 'px';
            hcols[i].split.style.left = (pos+width-3) + 'px';
            this.setCSSWidth(i, width, pos);
            pos += width;
        }
        this.lastWidth = totalWidth;
        if(this.grid.autoWidth){
            this.grid.container.setWidth(totalWidth+this.grid.container.getBorderWidth('lr'));
            this.grid.autoSize();
        }
        this.bwrap.setWidth(Math.max(totalWidth, this.wrap.clientWidth));
        if(!YAHOO.ext.util.Browser.isIE){ // fix scrolling prob in gecko and opera
        	this.wrap.scrollLeft = this.hwrap.dom.scrollLeft;
        }
        this.syncScroll();
        this.forceScrollUpdate();
        if(this.grid.autoHeight){
            this.autoHeight();
            this.updateWrapHeight();
        }
    },
    
    setCSSWidth : function(colIndex, width, pos){
        var selector = ["#" + this.grid.id + " .ygrid-col-" + colIndex, ".ygrid-col-" + colIndex];
        YAHOO.ext.util.CSS.updateRule(selector, 'width', width + 'px');
        if(typeof pos == 'number'){
            YAHOO.ext.util.CSS.updateRule(selector, 'left', pos + 'px');
        }
    },
    
    /**
     * Set a css style for a column dynamically. 
     * @param {Number} colIndex The index of the column
     * @param {String} name The css property name
     * @param {String} value The css value
     */
     setCSSStyle : function(colIndex, name, value){
        var selector = ["#" + this.grid.id + " .ygrid-col-" + colIndex, ".ygrid-col-" + colIndex];
        YAHOO.ext.util.CSS.updateRule(selector, name, value);
    },
    
    handleHiddenChange : function(colModel, colIndex, hidden){
        if(hidden){
            this.hideColumn(colIndex);
        }else{
            this.unhideColumn(colIndex);
        }
        this.updateColumns();
    },
    
    hideColumn : function(colIndex){
        var selector = ["#" + this.grid.id + " .ygrid-col-" + colIndex, ".ygrid-col-" + colIndex];
        YAHOO.ext.util.CSS.updateRule(selector, 'position', 'absolute');
        YAHOO.ext.util.CSS.updateRule(selector, 'visibility', 'hidden');
        
        this.headers[colIndex].style.display = 'none';
        this.headers[colIndex].split.style.display = 'none';
    },
    
    unhideColumn : function(colIndex){
        var selector = ["#" + this.grid.id + " .ygrid-col-" + colIndex, ".ygrid-col-" + colIndex];
        YAHOO.ext.util.CSS.updateRule(selector, 'position', '');
        YAHOO.ext.util.CSS.updateRule(selector, 'visibility', 'visible');
        
        this.headers[colIndex].style.display = '';
        this.headers[colIndex].split.style.display = '';
    },
    
    getBodyTable : function(){
    	return this.bwrap.dom;
    },
    
    updateRowIndexes : function(firstRow, lastRow){
        var stripeRows = this.grid.stripeRows;
        var bt = this.getBodyTable();
        var nodes = bt.childNodes;
        firstRow = firstRow || 0;
        lastRow = lastRow || nodes.length-1;
        var re = /^(?:ygrid-row ygrid-row-alt|ygrid-row)/;
        for(var rowIndex = firstRow; rowIndex <= lastRow; rowIndex++){
            var node = nodes[rowIndex];
            if(stripeRows && (rowIndex+1) % 2 == 0){
        		node.className = node.className.replace(re, 'ygrid-row ygrid-row-alt');
        	}else{
        		node.className = node.className.replace(re, 'ygrid-row');
        	}
            node.rowIndex = rowIndex;
            nodes[rowIndex].style.top = (rowIndex * this.rowHeight) + 'px';
        }
    },

    insertRows : function(dataModel, firstRow, lastRow){
        this.updateBodyHeight();
        this.adjustForScroll(true);
        var renderers = this.getColumnRenderers();
        var dindexes = this.getDataIndexes();
        var colCount = this.grid.colModel.getColumnCount();
        var beforeRow = null;
        var bt = this.getBodyTable();
        if(firstRow < bt.childNodes.length){
            beforeRow = bt.childNodes[firstRow];
        }
        for(var rowIndex = firstRow; rowIndex <= lastRow; rowIndex++){
            var row = document.createElement('span');
            row.className = 'ygrid-row';
            row.style.top = (rowIndex * this.rowHeight) + 'px';
            this.renderRow(dataModel, row, rowIndex, colCount, renderers, dindexes);
            if(beforeRow){
            	bt.insertBefore(row, beforeRow);
            }else{
                bt.appendChild(row);
            }
        }
        this.updateRowIndexes(firstRow);
        this.adjustForScroll(true);
    },
    
    renderRow : function(dataModel, row, rowIndex, colCount, renderers, dindexes){
        for(var colIndex = 0; colIndex < colCount; colIndex++){
            var td = document.createElement('span');
            td.className = 'ygrid-col ygrid-col-' + colIndex + (colIndex == colCount-1 ? ' ygrid-col-last' : '');
            td.columnIndex = colIndex;
            td.tabIndex = 0;
            var span = document.createElement('span');
            span.className = 'ygrid-cell-text';
            td.appendChild(span);
            var val = renderers[colIndex](dataModel.getValueAt(rowIndex, dindexes[colIndex]), rowIndex, colIndex, td, dataModel);
            if(typeof val == 'undefined' || val === '') val = '&#160;';
            span.innerHTML = val;
            row.appendChild(td);
        }
    },
    
    deleteRows : function(dataModel, firstRow, lastRow){
        this.updateBodyHeight();
        // first make sure they are deselected
        this.grid.selModel.deselectRange(firstRow, lastRow);
        var bt = this.getBodyTable();
        var rows = []; // get references because the rowIndex will change
        for(var rowIndex = firstRow; rowIndex <= lastRow; rowIndex++){
            rows.push(bt.childNodes[rowIndex]);
        }
        for(var i = 0; i < rows.length; i++){
            bt.removeChild(rows[i]);
            rows[i] = null;
        }
        rows = null;
        this.updateRowIndexes(firstRow);
        this.adjustForScroll();
    },
    
    updateRows : function(dataModel, firstRow, lastRow){
        var bt = this.getBodyTable();
        var dindexes = this.getDataIndexes();
        var renderers = this.getColumnRenderers();
        var colCount = this.grid.colModel.getColumnCount();
        for(var rowIndex = firstRow; rowIndex <= lastRow; rowIndex++){
            var row = bt.rows[rowIndex];
            var cells = row.childNodes;
            for(var colIndex = 0; colIndex < colCount; colIndex++){
                var td = cells[colIndex];
                var val = renderers[colIndex](dataModel.getValueAt(rowIndex, dindexes[colIndex]), rowIndex, colIndex, td, dataModel);
                if(typeof val == 'undefined' || val === '') val = '&#160;';
                td.firstChild.innerHTML = val;
            }
        }
    },
    
    handleSort : function(dataModel, sortColumnIndex, sortDir, noRefresh){
		var	selectedRows;
        this.grid.selModel.syncSelectionsToIds();
        if(!noRefresh){
           this.updateRows(dataModel, 0, dataModel.getRowCount()-1);
        }
        this.updateHeaderSortState();
		selectedRows = this.grid.selModel.getSelectedRows();
		if (selectedRows.length > 0) {
			this.focusRow(selectedRows[0]);
		}
    },
    
    syncScroll : function(){
        this.hwrap.dom.scrollLeft = this.wrap.scrollLeft;
    },
    
    handleScroll : function(){
        this.syncScroll();
        this.fireScroll(this.wrap.scrollLeft, this.wrap.scrollTop);
        this.grid.fireEvent('bodyscroll', this.wrap.scrollLeft, this.wrap.scrollTop);
    },
    
    getRowHeight : function(){
        if(!this.rowHeight){
            var rule = YAHOO.ext.util.CSS.getRule(["#" + this.grid.id + " .ygrid-row", ".ygrid-row"]);
        	if(rule && rule.style.height){
        	    this.rowHeight = parseInt(rule.style.height, 10);
        	}else{
        	    this.rowHeight = 21;
        	}
        }
        return this.rowHeight;
    },
    
    renderRows : function(dataModel){
        this.grid.stopEditing();
        if(this.grid.selModel){
            this.grid.selModel.clearSelections();
        }
    	var bt = this.getBodyTable();
    	bt.innerHTML = '';
    	this.rowHeight = this.getRowHeight();
    	this.insertRows(dataModel, 0, dataModel.getRowCount()-1);
    },
    
    updateCell : function(dataModel, rowIndex, dataIndex){
        var colIndex = this.getColumnIndexByDataIndex(dataIndex);
        if(typeof colIndex == 'undefined'){ // not present in grid
            return;
        }
        var bt = this.getBodyTable();
        var row = bt.childNodes[rowIndex];
        var cell = row.childNodes[colIndex];
        var renderer = this.grid.colModel.getRenderer(colIndex);
        var val = renderer(dataModel.getValueAt(rowIndex, dataIndex), rowIndex, colIndex, cell, dataModel);
        if(typeof val == 'undefined' || val === '') val = '&#160;';
        cell.firstChild.innerHTML = val;
    },
    
    calcColumnWidth : function(colIndex, maxRowsToMeasure){
        var maxWidth = 0;
        var bt = this.getBodyTable();
        var rows = bt.childNodes;
        var stopIndex = Math.min(maxRowsToMeasure || rows.length, rows.length);
        if(this.grid.autoSizeHeaders){
            var h = this.headers[colIndex];
            var curWidth = h.style.width;
            h.style.width = this.grid.minColumnWidth+'px';
            maxWidth = Math.max(maxWidth, h.scrollWidth);
            h.style.width = curWidth;
        }
        for(var i = 0; i < stopIndex; i++){
            var cell = rows[i].childNodes[colIndex].firstChild;
            maxWidth = Math.max(maxWidth, cell.scrollWidth);
        }
        return maxWidth + /*margin for error in IE*/ 5;
    },
    
    /**
     * Autofit a column to it's content.
     * @param {Number} colIndex
     * @param {Boolean} forceMinSize true to force the column to go smaller if possible
     */
     autoSizeColumn : function(colIndex, forceMinSize){
        if(forceMinSize){
           this.setCSSWidth(colIndex, this.grid.minColumnWidth);
        }
        var newWidth = this.calcColumnWidth(colIndex);
        this.grid.colModel.setColumnWidth(colIndex,
            Math.max(this.grid.minColumnWidth, newWidth));
        this.grid.fireEvent('columnresize', colIndex, newWidth);
    },
    
    /**
     * Autofits all columns to their content and then expands to fit any extra space in the grid
     */
     autoSizeColumns : function(){
        var colModel = this.grid.colModel;
        var colCount = colModel.getColumnCount();
        var wrap = this.wrap;
        for(var i = 0; i < colCount; i++){
            this.setCSSWidth(i, this.grid.minColumnWidth);
            colModel.setColumnWidth(i, this.calcColumnWidth(i, this.grid.maxRowsToMeasure), true);
        }
        if(colModel.getTotalWidth() < wrap.clientWidth){
            var diff = Math.floor((wrap.clientWidth - colModel.getTotalWidth()) / colCount);
            for(var i = 0; i < colCount; i++){
                colModel.setColumnWidth(i, colModel.getColumnWidth(i) + diff, true);
            }
        }
        this.updateColumns();  
    },
    
    /**
     * Autofits all columns to the grid's width proportionate with their current size
     */
    fitColumns : function(){
        var cm = this.grid.colModel;
        var colCount = cm.getColumnCount();
        var cols = [];
        var width = 0;
        var i, w;
        for (i = 0; i < colCount; i++){
            if(!cm.isHidden(i) && !cm.isFixed(i)){
                w = cm.getColumnWidth(i);
                cols.push(i);
                cols.push(w);
                width += w;
            }
        }
        var frac = (this.wrap.clientWidth - cm.getTotalWidth())/width;
        while (cols.length){
            w = cols.pop();
            i = cols.pop();
            cm.setColumnWidth(i, Math.floor(w + w*frac), true);
        }
        this.updateColumns();
    }, 
    
    onWindowResize : function(){
        if(this.grid.monitorWindowResize){
            this.adjustForScroll();
            this.updateWrapHeight();
            this.adjustForScroll();
        }  
    },
    
    updateWrapHeight : function(){
        this.grid.container.beginMeasure();
        this.autoHeight();
        var box = this.grid.container.getSize(true);
        this.wrapEl.setHeight(box.height-this.footerHeight-parseInt(this.wrap.offsetTop, 10));
        this.pwrap.setSize(box.width, box.height);
        this.grid.container.endMeasure();
    },
    
    forceScrollUpdate : function(){
        var wrap = this.wrapEl;
        wrap.setWidth(wrap.getWidth(true));
        setTimeout(function(){ // set timeout so FireFox works
            wrap.setWidth('');
        }, 1);
    },
    
    updateHeaderSortState : function(){
        var state = this.grid.dataModel.getSortState();
        if(!state || typeof state.column == 'undefined') return;
        var sortColumn = this.getColumnIndexByDataIndex(state.column);
        var sortDir = state.direction;
        for(var i = 0, len = this.headers.length; i < len; i++){
            var h = this.headers[i];
            if(i != sortColumn){
                h.sortDesc.style.display = 'none';
                h.sortAsc.style.display = 'none';
                YAHOO.util.Dom.removeClass(h, 'ygrid-sort-col');
            }else{
                h.sortDesc.style.display = sortDir == 'DESC' ? 'block' : 'none';
                h.sortAsc.style.display = sortDir == 'ASC' ? 'block' : 'none';
                YAHOO.util.Dom.addClass(h, 'ygrid-sort-col');
            }
        }
    },

    unplugDataModel : function(dm){
        dm.removeListener('cellupdated', this.updateCell, this);
        dm.removeListener('datachanged', this.renderRows, this);
        dm.removeListener('rowsdeleted', this.deleteRows, this);
        dm.removeListener('rowsinserted', this.insertRows, this);
        dm.removeListener('rowsupdated', this.updateRows, this);
        dm.removeListener('rowssorted', this.handleSort, this);
    },
    
    plugDataModel : function(dm){
        dm.on('cellupdated', this.updateCell, this, true);
        dm.on('datachanged', this.renderRows, this, true);
        dm.on('rowsdeleted', this.deleteRows, this, true);
        dm.on('rowsinserted', this.insertRows, this, true);
        dm.on('rowsupdated', this.updateRows, this, true);
        dm.on('rowssorted', this.handleSort, this, true);
    },
    
    destroy : function(){
        this.unplugDataModel(this.grid.dataModel);
        var sp = this.splitters;
        if(sp){
            for(var i in sp){
                if(sp[i] && typeof sp[i] != 'function'){
                    sp[i].destroy(true);
                }
            }
        }
    },
    
    render : function(){
        var grid = this.grid;
        var container = grid.container.dom;
        var dataModel = grid.dataModel;
        this.plugDataModel(dataModel);
    
        var colModel = grid.colModel;
        colModel.onWidthChange.subscribe(this.updateColumns, this, true);
        colModel.onHeaderChange.subscribe(this.updateHeaders, this, true);
        colModel.onHiddenChange.subscribe(this.handleHiddenChange, this, true);
        
        if(grid.monitorWindowResize === true){
            YAHOO.ext.EventManager.onWindowResize(this.onWindowResize, this, true);
        }
        var autoSizeDelegate = this.autoSizeColumn.createDelegate(this);
        
        var colCount = colModel.getColumnCount();
        
        var dh = YAHOO.ext.DomHelper;
        this.pwrap = dh.append(container, 
            {tag: 'div', cls: 'ygrid-positioner', 
            style: 'position:relative;width:100%;height:100%;left:0;top:0;overflow:hidden;'}, true);
        var pos = this.pwrap.dom;
        
        //create wrapper elements that handle offsets and scrolling
        var wrap = dh.append(pos, {tag: 'div', cls: 'ygrid-wrap'});
        this.wrap = wrap;
        this.wrapEl = getEl(wrap, true);
        YAHOO.ext.EventManager.on(wrap, 'scroll', this.handleScroll, this, true);
        
        var hwrap = dh.append(pos, {tag: 'div', cls: 'ygrid-wrap-headers'});
        this.hwrap = getEl(hwrap, true);
        
        var bwrap = dh.append(wrap, {tag: 'div', cls: 'ygrid-wrap-body', id: container.id + '-body'});
        this.bwrap = getEl(bwrap, true);
        this.bwrap.setWidth(colModel.getTotalWidth());
        bwrap.rows = bwrap.childNodes;
        
        this.footerHeight = 0;
        var foot = this.appendFooter(this.pwrap.dom);
        if(foot){
            this.footer = getEl(foot, true);
            this.footerHeight = this.footer.getHeight();
        }
        this.updateWrapHeight();
        
        var hrow = dh.append(hwrap, {tag: 'span', cls: 'ygrid-hrow'});
        this.hrow = hrow;
        
        if(!YAHOO.ext.util.Browser.isGecko){
            // IE doesn't like iframes, we will leave this alone
            var iframe = document.createElement('iframe');
            iframe.className = 'ygrid-hrow-frame';
            iframe.frameBorder = 0;
            iframe.src = YAHOO.ext.SSL_SECURE_URL;
            hwrap.appendChild(iframe);
        }
        this.headerCtrl = new YAHOO.ext.grid.HeaderController(this.grid);
        this.headers = [];
        this.cols = [];
        this.splitters = [];
        
        var htemplate = dh.createTemplate({
           tag: 'span', cls: 'ygrid-hd ygrid-header-{0}', children: [{
                tag: 'span',
                cls: 'ygrid-hd-body',
                html: '<table border="0" cellpadding="0" cellspacing="0" title="{2}">' +
                      '<tbody><tr><td><span>{1}</span></td>' +
                      '<td><span class="sort-desc"></span><span class="sort-asc"></span></td>' +
                      '</tr></tbody></table>'
           }]
        });
        htemplate.compile();
        
        var ruleBuf = [];
                
        for(var i = 0; i < colCount; i++){
            var hd = htemplate.append(hrow, [i, colModel.getColumnHeader(i), colModel.getColumnTooltip(i) || '']);
            var spans = hd.getElementsByTagName('span');
            hd.textNode = spans[1];
            hd.sortDesc = spans[2];
            hd.sortAsc = spans[3];
    	    hd.columnIndex = i;
            this.headers.push(hd);
            if(colModel.isSortable(i)){
                this.headerCtrl.register(hd);
            }
            var split = dh.append(hrow, {tag: 'span', cls: 'ygrid-hd-split'});
            hd.split = split;
        	
        	if(colModel.isResizable(i) && !colModel.isFixed(i)){
            	YAHOO.util.Event.on(split, 'dblclick', autoSizeDelegate.createCallback(i+0, true));
            	var sb = new YAHOO.ext.SplitBar(split, hd, null, YAHOO.ext.SplitBar.LEFT);
            	sb.columnIndex = i;
            	sb.minSize = grid.minColumnWidth;
            	sb.onMoved.subscribe(this.onColumnSplitterMoved, this, true);
            	YAHOO.util.Dom.addClass(sb.proxy, 'ygrid-column-sizer');
            	YAHOO.util.Dom.setStyle(sb.proxy, 'background-color', '');
            	sb.dd._resizeProxy = function(){
            	    var el = this.getDragEl();
            	    YAHOO.util.Dom.setStyle(el, 'height', (hwrap.clientHeight+wrap.clientHeight-2) +'px');
            	};
            	this.splitters[i] = sb;
        	}else{
        	    split.style.cursor = 'default';
        	}
        	ruleBuf.push('#', container.id, ' .ygrid-col-', i, ' {\n}\n');        	
        }
        
        YAHOO.ext.util.CSS.createStyleSheet(ruleBuf.join(''));
        
        if(grid.autoSizeColumns){
            this.renderRows(dataModel);
            this.autoSizeColumns();
        }else{
            this.updateColumns();
            this.renderRows(dataModel);
        }
        
        for(var i = 0; i < colCount; i++){
            if(colModel.isHidden(i)){
                this.hideColumn(i);
            }
        }
        this.updateHeaderSortState();
        return this.bwrap;
    },
    
    onColumnSplitterMoved : function(splitter, newSize){
        this.grid.colModel.setColumnWidth(splitter.columnIndex, newSize);
        this.grid.fireEvent('columnresize', splitter.columnIndex, newSize);
    },
    
    appendFooter : function(parentEl){
        return null;  
    },
    
    autoHeight : function(){
        if(this.grid.autoHeight){
            var h = this.getBodyHeight();
            var c = this.grid.container;
            var total = h + (parseInt(this.wrap.offsetTop, 10)||0) + 
                    this.footerHeight + c.getBorderWidth('tb') + c.getPadding('tb')
                    + (this.wrap.offsetHeight - this.wrap.clientHeight);
            c.setHeight(total);
            
        }
    },
    
    getBodyHeight : function(){
        return this.grid.dataModel.getRowCount() * this.getRowHeight();;
    },
    
    updateBodyHeight : function(){
        this.getBodyTable().style.height = this.getBodyHeight() + 'px';
        if(this.grid.autoHeight){
            this.autoHeight();
            this.updateWrapHeight();
        }
    }
};
YAHOO.ext.grid.GridView.SCROLLBARS_UNDER = 0;
YAHOO.ext.grid.GridView.SCROLLBARS_OVERLAP = 1;
YAHOO.ext.grid.GridView.prototype.scrollbarMode = YAHOO.ext.grid.GridView.SCROLLBARS_UNDER;

YAHOO.ext.grid.GridView.prototype.fitColumnsToContainer = YAHOO.ext.grid.GridView.prototype.fitColumns;

YAHOO.ext.grid.HeaderController = function(grid){
	this.grid = grid;
	this.headers = [];
};

YAHOO.ext.grid.HeaderController.prototype = {
	register : function(header){
		this.headers.push(header);
		YAHOO.ext.EventManager.on(header, 'selectstart', this.cancelTextSelection, this, true);
        YAHOO.ext.EventManager.on(header, 'mousedown', this.cancelTextSelection, this, true);
        YAHOO.ext.EventManager.on(header, 'mouseover', this.headerOver, this, true);
        YAHOO.ext.EventManager.on(header, 'mouseout', this.headerOut, this, true);
        YAHOO.ext.EventManager.on(header, 'click', this.headerClick, this, true);
	},
	
	headerClick : function(e){
	    var grid = this.grid, cm = grid.colModel, dm = grid.dataModel;
	    grid.stopEditing();
        var header = grid.getHeaderFromChild(e.getTarget());
        var state = dm.getSortState();
        var direction = header.sortDir || 'ASC';
        if(typeof state.column != 'undefined' && 
                 grid.getView().getColumnIndexByDataIndex(state.column) == header.columnIndex){
            direction = (state.direction == 'ASC' ? 'DESC' : 'ASC');
        }
        header.sortDir = direction;
        dm.sort(cm, cm.getDataIndex(header.columnIndex), direction);
    },
    
    headerOver : function(e){
        var header = this.grid.getHeaderFromChild(e.getTarget());
        YAHOO.util.Dom.addClass(header, 'ygrid-hd-over');
        //YAHOO.ext.util.CSS.applyFirst(header, this.grid.id, '.ygrid-hd-over');
    },
    
    headerOut : function(e){
        var header = this.grid.getHeaderFromChild(e.getTarget());
        YAHOO.util.Dom.removeClass(header, 'ygrid-hd-over');
        //YAHOO.ext.util.CSS.revertFirst(header, this.grid.id, '.ygrid-hd-over');
    },
    
    cancelTextSelection : function(e){
    	e.preventDefault();
    }
};