YAHOO.ext.tree.TreeDropZone = function(tree, config){
    this.allowParentInsert = false;
    this.allowContainerDrop = false;
    this.appendOnly = false;
    YAHOO.ext.tree.TreeDropZone.superclass.constructor.call(this, tree.container, config);
    this.tree = tree;
    this.lastInsertClass = 'ytree-no-status';
    this.dragOverData = {};
};

YAHOO.extendX(YAHOO.ext.tree.TreeDropZone, YAHOO.ext.dd.DropZone, {
    ddGroup : 'TreeDD',
    
    expandDelay : 1000,
    
    expandNode : function(node){
        if(node.hasChildNodes() && !node.isExpanded()){
            node.expand(false, null, this.triggerCacheRefresh.createDelegate(this));
        }
    },
    
    queueExpand : function(node){
        this.expandProcId = this.expandNode.defer(this.expandDelay, this, [node]);
    },
    
    cancelExpand : function(){
        if(this.expandProcId){
            clearTimeout(this.expandProcId);
            this.expandProcId = false;
        }
    },
    
    isValidDropPoint : function(n, pt, dd, e, data){
        if(!n || !data){ return false; }
        var targetNode = n.node;
        var dropNode = data.node;
        // default drop rules
        if(!(targetNode && targetNode.isTarget && pt)){
            return false;
        }
        if(pt == 'append' && targetNode.allowChildren === false){
            return false;
        }
        if((pt == 'above' || pt == 'below') && (targetNode.parentNode && targetNode.parentNode.allowChildren === false)){
            return false;
        }
        if(dropNode && (targetNode == dropNode || dropNode.contains(targetNode))){
            return false;
        }
        // reuse the object
        var overEvent = this.dragOverData;
        overEvent.tree = this.tree;
        overEvent.target = targetNode;
        overEvent.data = data;
        overEvent.point = pt;
        overEvent.source = dd;
        overEvent.rawEvent = e;
        overEvent.dropNode = dropNode;
        overEvent.cancel = false;  
        var result = this.tree.fireEvent('nodedragover', overEvent);
        return overEvent.cancel === false && result !== false;
    },
    
    getDropPoint : function(e, n, dd){
        var tn = n.node;
        if(tn.isRoot){
            return tn.allowChildren !== false ? 'ap-pend' : false; // always append for root
        }
        var dragEl = n.ddel;
        var t = YAHOO.util.Dom.getY(dragEl), b = t + dragEl.offsetHeight;
        var y = YAHOO.util.Event.getPageY(e);
        var noAppend = tn.allowChildren === false || tn.isLeaf();
        if(this.appendOnly || tn.parentNode.allowChildren === false){
            return noAppend ? false : 'append';
        }
        var noBelow = false;
        if(!this.allowParentInsert){
            noBelow = tn.hasChildNodes() && tn.isExpanded();
        }
        var q = (b - t) / (noAppend ? 2 : 3);
        if(y >= t && y < t + q){
            return 'above';
        }else if(!noBelow && (noAppend || y >= b-q && y <= b)){
            return 'below';
        }else{
            return 'append';
        }
        return false;
    },
    
    onNodeEnter : function(n, dd, e, data){
        this.cancelExpand();
    },
    
    onNodeOver : function(n, dd, e, data){
        var pt = this.getDropPoint(e, n, dd);
        var node = n.node;
        
        // auto node expand check
        if(!this.expandProcId && pt == 'append' && node.hasChildNodes() && !n.node.isExpanded()){
            this.queueExpand(node);
        }else if(pt != 'append'){
            this.cancelExpand();
        }
        
        // set the insert point style on the target node
        var returnCls = this.dropNotAllowed;
        if(this.isValidDropPoint(n, pt, dd, e, data)){
           if(pt){
               var el = n.ddel;
               var cls, returnCls;
               if(pt == 'above'){
                   returnCls = n.node.isFirst() ? 'ytree-drop-ok-above' : 'ytree-drop-ok-between';
                   cls = 'ytree-drag-insert-above';
               }else if(pt == 'below'){
                   returnCls = n.node.isLast() ? 'ytree-drop-ok-below' : 'ytree-drop-ok-between';
                   cls = 'ytree-drag-insert-below';
               }else{
                   returnCls = 'ytree-drop-ok-append';
                   cls = 'ytree-drag-append';
               }
               if(this.lastInsertClass != cls){
                   YAHOO.util.Dom.replaceClass(el, this.lastInsertClass, cls);
                   this.lastInsertClass = cls;
               }
           }
       }
       return returnCls;
    },
    
    onNodeOut : function(n, dd, e, data){
        this.cancelExpand();
        this.removeDropIndicators(n);
    },
    
    onNodeDrop : function(n, dd, e, data){
        var point = this.getDropPoint(e, n, dd);
        var targetNode = n.node;
        targetNode.ui.startDrop();
        if(!this.isValidDropPoint(n, point, dd, e, data)){
            targetNode.ui.endDrop();
            return false;
        }
        // first try to find the drop node
        var dropNode = data.node || (dd.getTreeNode ? dd.getTreeNode(data, targetNode, point, e) : null);
        var dropEvent = {
            tree : this.tree,
            target: targetNode,
            data: data,
            point: point,
            source: dd,
            rawEvent: e,
            dropNode: dropNode,
            cancel: dropNode ? false : true   
        };
        var retval = this.tree.fireEvent('beforenodedrop', dropEvent);
        if(retval === false || dropEvent.cancel === true || !dropEvent.dropNode){
            targetNode.ui.endDrop();
            return false;
        }
        if(point == 'append' && !targetNode.isExpanded()){
            targetNode.expand(false, null, function(){
                this.completeDrop(dropEvent);
            }.createDelegate(this));
        }else{
            this.completeDrop(dropEvent);
        }
        return true;
    },
    
    completeDrop : function(de){
        var ns = de.dropNode, p = de.point, t = de.target;
        if(!(ns instanceof Array)){
            ns = [ns];
        }
        var n;
        for(var i = 0, len = ns.length; i < len; i++){
            n = ns[i];
            if(p == 'above'){
                t.parentNode.insertBefore(n, t);
            }else if(p == 'below'){
                t.parentNode.insertBefore(n, t.nextSibling);
            }else{
                t.appendChild(n);
            }
        }
        n.select(); // select and highlight the last insert
        if(this.tree.hlDrop){
            n.ui.highlight();
        }
        t.ui.endDrop();
        this.tree.fireEvent('nodedrop', de);
    },
    
    afterNodeMoved : function(dd, data, e, targetNode, dropNode){
        if(this.tree.hlDrop){
            dropNode.select();
            dropNode.ui.highlight();
        }
        this.tree.fireEvent('nodedrop', this.tree, targetNode, data, dd, e);
    },
    
    getTree : function(){
        return this.tree;
    },
    
    removeDropIndicators : function(n){
        if(n && n.ddel){
            var el = n.ddel;
            YAHOO.util.Dom.removeClass(el, 'ytree-drag-insert-above');
            YAHOO.util.Dom.removeClass(el, 'ytree-drag-insert-below');
            YAHOO.util.Dom.removeClass(el, 'ytree-drag-append');
            this.lastInsertClass = '_noclass';
        }
    },
    
    beforeDragDrop : function(target, e, id){
        this.cancelExpand();
        return true;
    },
    
    afterRepair : function(data){
        if(data){
            data.node.ui.highlight();
        }
        this.hideProxy();
    }    
});
