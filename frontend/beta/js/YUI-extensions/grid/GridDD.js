
// kill dependency issue
if(YAHOO.util.DDProxy){
/**
 * @class YAHOO.ext.grid.GridDD
 * Custom implementation of YAHOO.util.DDProxy used internally by the grid
 * @extends YAHOO.util.DDProxy
 */
YAHOO.ext.grid.GridDD = function(grid, bwrap){
    this.grid = grid;
    var ddproxy = document.createElement('div');
    ddproxy.id = grid.container.id + '-ddproxy';
    ddproxy.className = 'ygrid-drag-proxy';
    document.body.insertBefore(ddproxy, document.body.firstChild);
    YAHOO.util.Dom.setStyle(ddproxy, 'opacity', .80);
    var ddicon = document.createElement('span');
    ddicon.className = 'ygrid-drop-icon ygrid-drop-nodrop';
    ddproxy.appendChild(ddicon);
    var ddtext = document.createElement('span');
    ddtext.className = 'ygrid-drag-text';
    ddtext.innerHTML = "&#160;";
    ddproxy.appendChild(ddtext);
    
    this.ddproxy = ddproxy;
    this.ddtext = ddtext;
    this.ddicon = ddicon;
    YAHOO.util.Event.on(bwrap, 'click', this.handleClick, this, true);
    YAHOO.ext.grid.GridDD.superclass.constructor.call(this, bwrap.id, 'GridDD', 
          {dragElId : ddproxy.id, resizeFrame: false});
          
    this.unlockDelegate = grid.selModel.unlock.createDelegate(grid.selModel);
};
YAHOO.extendX(YAHOO.ext.grid.GridDD, YAHOO.util.DDProxy);

YAHOO.ext.grid.GridDD.prototype.handleMouseDown = function(e){
    var row = this.grid.getRowFromChild(YAHOO.util.Event.getTarget(e));
    if(!row) return;
    if(this.grid.selModel.isSelected(row)){
        YAHOO.ext.grid.GridDD.superclass.handleMouseDown.call(this, e);
    }else {
        this.grid.selModel.unlock();
        YAHOO.ext.EventObject.setEvent(e);
        this.grid.selModel.rowClick(this.grid, row.rowIndex, YAHOO.ext.EventObject);
        YAHOO.ext.grid.GridDD.superclass.handleMouseDown.call(this, e);
        this.grid.selModel.lock();
    }
};

YAHOO.ext.grid.GridDD.prototype.handleClick = function(e){
    if(this.grid.selModel.isLocked()){
         setTimeout(this.unlockDelegate, 1);
         YAHOO.util.Event.stopEvent(e);
    }
};

/**
 * Updates the DD visual element to allow/not allow a drop
 * @param {Boolean} dropStatus True if drop is allowed on the target
 */
YAHOO.ext.grid.GridDD.prototype.setDropStatus = function(dropStatus){
    if(dropStatus === true){
        YAHOO.util.Dom.replaceClass(this.ddicon, 'ygrid-drop-nodrop', 'ygrid-drop-ok');
    }else{
        YAHOO.util.Dom.replaceClass(this.ddicon, 'ygrid-drop-ok', 'ygrid-drop-nodrop');
    }
};

YAHOO.ext.grid.GridDD.prototype.startDrag = function(e){
    this.ddtext.innerHTML = this.grid.getDragDropText();
    this.setDropStatus(false);
    this.grid.selModel.lock();
    this.grid.fireEvent('startdrag', this.grid, this, e);
};
       
YAHOO.ext.grid.GridDD.prototype.endDrag = function(e){
    YAHOO.util.Dom.setStyle(this.ddproxy, 'visibility', 'hidden');
    this.grid.fireEvent('enddrag', this.grid, this, e);
};

YAHOO.ext.grid.GridDD.prototype.autoOffset = function(iPageX, iPageY) {
    this.setDelta(-12, -20);
};

YAHOO.ext.grid.GridDD.prototype.onDragEnter = function(e, id) {
    this.setDropStatus(true);
    this.grid.fireEvent('dragenter', this.grid, this, id, e);
};

YAHOO.ext.grid.GridDD.prototype.onDragDrop = function(e, id) {
    this.grid.fireEvent('dragdrop', this.grid, this, id, e);
};

YAHOO.ext.grid.GridDD.prototype.onDragOver = function(e, id) {
    this.grid.fireEvent('dragover', this.grid, this, id, e);
};

YAHOO.ext.grid.GridDD.prototype.onDragOut = function(e, id) {
    this.setDropStatus(false);
    this.grid.fireEvent('dragout', this.grid, this, id, e);
};
};
