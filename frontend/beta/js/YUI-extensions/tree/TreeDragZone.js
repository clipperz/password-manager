YAHOO.ext.tree.TreeDragZone = function(tree, config){
    YAHOO.ext.tree.TreeDragZone.superclass.constructor.call(this, tree.getEl(), config);
    this.tree = tree;
};

YAHOO.extendX(YAHOO.ext.tree.TreeDragZone, YAHOO.ext.dd.DragZone, {
    ddGroup : 'TreeDD',
    
    onBeforeDrag : function(data, e){
        var n = data.node;
        return n && n.draggable && !n.disabled;
    },
    
    onInitDrag : function(e){
        var data = this.dragData;
        this.tree.getSelectionModel().select(data.node);
        this.proxy.update('');
        data.node.ui.appendDDGhost(this.proxy.ghost.dom);
        this.tree.fireEvent('startdrag', this.tree, data.node, e);
    },
    
    getRepairXY : function(e, data){
        return data.node.ui.getDDRepairXY();
    },
    
    onEndDrag : function(data, e){
        this.tree.fireEvent('enddrag', this.tree, data.node, e); 
    },
    
    onValidDrop : function(dd, e, id){
        this.tree.fireEvent('dragdrop', this.tree, this.dragData.node, dd, e);
        this.hideProxy();
    },
    
    beforeInvalidDrop : function(e, id){
        if(YAHOO.util.Anim){
            // this scrolls the original position back into view
            var sm = this.tree.getSelectionModel();
            sm.clearSelections();
            sm.select(this.dragData.node);
        }
    }
});
