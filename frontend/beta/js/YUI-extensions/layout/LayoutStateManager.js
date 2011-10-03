/*
 * Private internal class for reading and applying state
 */
YAHOO.ext.LayoutStateManager = function(layout){
     // default empty state
     this.state = {
        north: {},
        south: {},
        east: {},
        west: {}       
    };
};

YAHOO.ext.LayoutStateManager.prototype = {
    init : function(layout, provider){
        this.provider = provider;
        var state = provider.get(layout.id+'-layout-state');
        if(state){
            var wasUpdating = layout.isUpdating();
            if(!wasUpdating){
                layout.beginUpdate();
            }
            for(var key in state){
                if(typeof state[key] != 'function'){
                    var rstate = state[key];
                    var r = layout.getRegion(key);
                    if(r && rstate){
                        if(rstate.size){
                            r.resizeTo(rstate.size);
                        }
                        if(rstate.collapsed == true){
                            r.collapse(true);
                        }else{
                            r.expand(null, true);
                        }
                    }
                }
            }
            if(!wasUpdating){
                layout.endUpdate();
            }
            this.state = state; 
        }
        this.layout = layout;
        layout.on('regionresized', this.onRegionResized, this, true);
        layout.on('regioncollapsed', this.onRegionCollapsed, this, true);
        layout.on('regionexpanded', this.onRegionExpanded, this, true);
    },
    
    storeState : function(){
        this.provider.set(this.layout.id+'-layout-state', this.state);
    },
    
    onRegionResized : function(region, newSize){
        this.state[region.getPosition()].size = newSize;
        this.storeState();
    },
    
    onRegionCollapsed : function(region){
        this.state[region.getPosition()].collapsed = true;
        this.storeState();
    },
    
    onRegionExpanded : function(region){
        this.state[region.getPosition()].collapsed = false;
        this.storeState();
    }
};
