// kill drag drop dependency
if(YAHOO.util.DragDrop){

YAHOO.ext.dd.DropTarget = function(el, config){
    this.el = getEl(el);
    
    YAHOO.ext.util.Config.apply(this, config);
    
    if(this.containerScroll){
        YAHOO.ext.dd.ScrollManager.register(this.el);
    }
    
    YAHOO.ext.dd.DropTarget.superclass.constructor.call(this, this.el.dom, this.ddGroup || this.group, 
          {isTarget: true});

};

YAHOO.extendX(YAHOO.ext.dd.DropTarget, YAHOO.util.DDTarget, {
    isTarget : true,
    isNotifyTarget : true,
    dropAllowed : 'ydd-drop-ok',
    dropNotAllowed : 'ydd-drop-nodrop',
    
    notifyEnter : function(dd, e, data){
        if(this.overClass){
            this.el.addClass(this.overClass);
        }
        return this.dropAllowed;
    },
    
    notifyOver : function(dd, e, data){
        return this.dropAllowed;
    },
    
    notifyOut : function(dd, e, data){
        if(this.overClass){
            this.el.removeClass(this.overClass);
        }
    },
    
    notifyDrop : function(dd, e, data){
        return false;
    }
});
}
