// kill drag drop dependency
if(YAHOO.util.DragDrop){
YAHOO.ext.dd.DropZone = function(el, config){
    YAHOO.ext.dd.DropZone.superclass.constructor.call(this, el, config);
};

YAHOO.extendX(YAHOO.ext.dd.DropZone, YAHOO.ext.dd.DropTarget, {
    getTargetFromEvent : function(e){
        return YAHOO.ext.dd.Registry.getTargetFromEvent(e);
    },
    
    onNodeEnter : function(n, dd, e, data){
        
    },
    
    onNodeOver : function(n, dd, e, data){
        return this.dropAllowed;
    },
    
    onNodeOut : function(n, dd, e, data){
        
    },
    
    onNodeDrop : function(n, dd, e, data){
        return false;
    },
    
    onContainerOver : function(n, dd, e, data){
        return this.dropNotAllowed;
    },
    
    onContainerDrop : function(n, dd, e, data){
        return false;
    },
    
    notifyEnter : function(dd, e, data){
        return this.dropNotAllowed;
    },
    
    notifyOver : function(dd, e, data){
        var n = this.getTargetFromEvent(e);
        if(!n){ // not over valid drop target
            if(this.lastOverNode){
                this.onNodeOut(this.lastOverNode, dd, e, data);
                this.lastOverNode = null;
            }
            return this.onContainerOver(dd, e, data);
        }
        if(this.lastOverNode != n){
            if(this.lastOverNode){
                this.onNodeOut(this.lastOverNode, dd, e, data);
            }
            this.onNodeEnter(n, dd, e, data);
            this.lastOverNode = n;
        }
        return this.onNodeOver(n, dd, e, data);
    },
    
    notifyOut : function(dd, e, data){
        if(this.lastOverNode){
            this.onNodeOut(this.lastOverNode, dd, e, data);
            this.lastOverNode = null;
        }
    },
    
    notifyDrop : function(dd, e, data){
        if(this.lastOverNode){
            this.onNodeOut(this.lastOverNode, dd, e, data);
            this.lastOverNode = null;
        }
        var n = this.getTargetFromEvent(e);
        return n ?
            this.onNodeDrop(n, dd, e, data) :
            this.onContainerDrop(n, dd, e, data);
    },
    
    triggerCacheRefresh : function(){
        YAHOO.util.DDM.refreshCache(this.groups);
    }  
});
}
