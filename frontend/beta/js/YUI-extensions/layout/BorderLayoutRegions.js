/*
 * These classes are private internal classes
 */
YAHOO.ext.CenterLayoutRegion = function(mgr, config){
    YAHOO.ext.CenterLayoutRegion.superclass.constructor.call(this, mgr, config, 'center');
    this.visible = true;
    this.minWidth = config.minWidth || 20;
    this.minHeight = config.minHeight || 20;
};

YAHOO.extendX(YAHOO.ext.CenterLayoutRegion, YAHOO.ext.LayoutRegion, {
    hide : function(){
        // center panel can't be hidden
    },
    
    show : function(){
        // center panel can't be hidden
    },
    
    getMinWidth: function(){
        return this.minWidth;
    },
    
    getMinHeight: function(){
        return this.minHeight;
    }
});


YAHOO.ext.NorthLayoutRegion = function(mgr, config){
    YAHOO.ext.NorthLayoutRegion.superclass.constructor.call(this, mgr, config, 'north', 'n-resize');
    if(this.split){
        this.split.placement = YAHOO.ext.SplitBar.TOP;
        this.split.orientation = YAHOO.ext.SplitBar.VERTICAL;
        this.split.el.addClass('ylayout-split-v');
    }
    if(typeof config.initialSize != 'undefined'){
        this.el.setHeight(config.initialSize);
    }
};
YAHOO.extendX(YAHOO.ext.NorthLayoutRegion, YAHOO.ext.SplitLayoutRegion, {
    getBox : function(){
        if(this.collapsed){
            return this.collapsedEl.getBox();
        }
        var box = this.el.getBox();
        if(this.split){
            box.height += this.split.el.getHeight();
        }
        return box;
    },
    
    updateBox : function(box){
        if(this.split && !this.collapsed){
            box.height -= this.split.el.getHeight();
            this.split.el.setLeft(box.x);
            this.split.el.setTop(box.y+box.height);
            this.split.el.setWidth(box.width);
        }
        if(this.collapsed){
            this.el.setWidth(box.width);
            var bodyWidth = box.width - this.el.getBorderWidth('rl');
            this.bodyEl.setWidth(bodyWidth);
            if(this.activePanel && this.panelSize){
                this.activePanel.setSize(bodyWidth, this.panelSize.height);
            }
        }
        YAHOO.ext.NorthLayoutRegion.superclass.updateBox.call(this, box);
    }
});

YAHOO.ext.SouthLayoutRegion = function(mgr, config){
    YAHOO.ext.SouthLayoutRegion.superclass.constructor.call(this, mgr, config, 'south', 's-resize');
    if(this.split){
        this.split.placement = YAHOO.ext.SplitBar.BOTTOM;
        this.split.orientation = YAHOO.ext.SplitBar.VERTICAL;
        this.split.el.addClass('ylayout-split-v');
    }
    if(typeof config.initialSize != 'undefined'){
        this.el.setHeight(config.initialSize);
    }
};
YAHOO.extendX(YAHOO.ext.SouthLayoutRegion, YAHOO.ext.SplitLayoutRegion, {
    getBox : function(){
        if(this.collapsed){
            return this.collapsedEl.getBox();
        }
        var box = this.el.getBox();
        if(this.split){
            var sh = this.split.el.getHeight();
            box.height += sh;
            box.y -= sh;
        }
        return box;
    },
    
    updateBox : function(box){
        if(this.split && !this.collapsed){
            var sh = this.split.el.getHeight();
            box.height -= sh;
            box.y += sh;
            this.split.el.setLeft(box.x);
            this.split.el.setTop(box.y-sh);
            this.split.el.setWidth(box.width);
        }
        if(this.collapsed){
            this.el.setWidth(box.width);
            var bodyWidth = box.width - this.el.getBorderWidth('rl');
            this.bodyEl.setWidth(bodyWidth);
            if(this.activePanel && this.panelSize){
                this.activePanel.setSize(bodyWidth, this.panelSize.height);
            }
        }
        YAHOO.ext.SouthLayoutRegion.superclass.updateBox.call(this, box);
    }
});

YAHOO.ext.EastLayoutRegion = function(mgr, config){
    YAHOO.ext.EastLayoutRegion.superclass.constructor.call(this, mgr, config, 'east', 'e-resize');
    if(this.split){
        this.split.placement = YAHOO.ext.SplitBar.RIGHT;
        this.split.orientation = YAHOO.ext.SplitBar.HORIZONTAL;
        this.split.el.addClass('ylayout-split-h');
    }
    if(typeof config.initialSize != 'undefined'){
        this.el.setWidth(config.initialSize);
    }
};
YAHOO.extendX(YAHOO.ext.EastLayoutRegion, YAHOO.ext.SplitLayoutRegion, {
    getBox : function(){
        if(this.collapsed){
            return this.collapsedEl.getBox();
        }
        var box = this.el.getBox();
        if(this.split){
            var sw = this.split.el.getWidth();
            box.width += sw;
            box.x -= sw;
        }
        return box;
    },
    
    updateBox : function(box){
        if(this.split && !this.collapsed){
            var sw = this.split.el.getWidth();
            box.width -= sw;
            this.split.el.setLeft(box.x);
            this.split.el.setTop(box.y);
            this.split.el.setHeight(box.height);
            box.x += sw;
        }
        if(this.collapsed){
            this.el.setHeight(box.height);
            var bodyHeight = this.config.titlebar ? box.height - (this.titleEl.getHeight()||0) : box.height;
            bodyHeight -= this.el.getBorderWidth('tb');
            this.bodyEl.setHeight(bodyHeight);
            if(this.activePanel && this.panelSize){
                this.activePanel.setSize(this.panelSize.width, bodyHeight);
            }
        }
        YAHOO.ext.EastLayoutRegion.superclass.updateBox.call(this, box);
    }
});

YAHOO.ext.WestLayoutRegion = function(mgr, config){
    YAHOO.ext.WestLayoutRegion.superclass.constructor.call(this, mgr, config, 'west', 'w-resize');
    if(this.split){
        this.split.placement = YAHOO.ext.SplitBar.LEFT;
        this.split.orientation = YAHOO.ext.SplitBar.HORIZONTAL;
        this.split.el.addClass('ylayout-split-h');
    }
    if(typeof config.initialSize != 'undefined'){
        this.el.setWidth(config.initialSize);
    }
};
YAHOO.extendX(YAHOO.ext.WestLayoutRegion, YAHOO.ext.SplitLayoutRegion, {
    getBox : function(){
        if(this.collapsed){
            return this.collapsedEl.getBox();
        }
        var box = this.el.getBox();
        if(this.split){
            box.width += this.split.el.getWidth();
        }
        return box;
    },
    
    updateBox : function(box){
        if(this.split && !this.collapsed){
            var sw = this.split.el.getWidth();
            box.width -= sw;
            this.split.el.setLeft(box.x+box.width);
            this.split.el.setTop(box.y);
            this.split.el.setHeight(box.height);
        }
        if(this.collapsed){
            this.el.setHeight(box.height);
            var bodyHeight = this.config.titlebar ? box.height - (this.titleEl.getHeight()||0) : box.height;
            bodyHeight -= this.el.getBorderWidth('tb');
            this.bodyEl.setHeight(bodyHeight);
            if(this.activePanel && this.panelSize){
                this.activePanel.setSize(this.panelSize.width, bodyHeight);
            }
        }
        YAHOO.ext.WestLayoutRegion.superclass.updateBox.call(this, box);
    }
});
