/**
 * @class YAHOO.ext.SplitLayoutRegion
 * @extends YAHOO.ext.LayoutRegion
 * Adds a splitbar and other (private) useful functionality to a LayoutRegion
 */
YAHOO.ext.SplitLayoutRegion = function(mgr, config, pos, cursor){
    this.cursor = cursor;
    YAHOO.ext.SplitLayoutRegion.superclass.constructor.call(this, mgr, config, pos);
    if(config.split){
        this.hide();
    }
};

YAHOO.extendX(YAHOO.ext.SplitLayoutRegion, YAHOO.ext.LayoutRegion, {
    applyConfig : function(config){
        YAHOO.ext.SplitLayoutRegion.superclass.applyConfig.call(this, config);
        if(config.split){
            if(!this.split){
                var splitEl = YAHOO.ext.DomHelper.append(this.mgr.el.dom, 
                        {tag: 'div', id: this.el.id + '-split', cls: 'ylayout-split ylayout-split-'+this.position, html: '&#160;'});
                /** The SplitBar for this region @type YAHOO.ext.SplitBar */
                this.split = new YAHOO.ext.SplitBar(splitEl, this.el);
                this.split.onMoved.subscribe(this.onSplitMove, this, true);
                this.split.useShim = config.useShim === true;
                YAHOO.util.Dom.setStyle([this.split.el.dom, this.split.proxy], 'cursor', this.cursor);
                this.split.getMaximumSize = this.getMaxSize.createDelegate(this);
            }
            if(typeof config.minSize != 'undefined'){
                this.split.minSize = config.minSize;
            }
            if(typeof config.maxSize != 'undefined'){
                this.split.maxSize = config.maxSize;
            }
        }
    },
    
    getMaxSize : function(){
         var cmax = this.config.maxSize || 10000;
         var center = this.mgr.getRegion('center');
         return Math.min(cmax, (this.el.getWidth()+center.getEl().getWidth())-center.getMinWidth());
    },
    
    onSplitMove : function(split, newSize){
        this.fireEvent('resized', this, newSize);
    },
    
    /** 
     * Returns the SplitBar for this region.
     * @return {YAHOO.ext.SplitBar}
     */
    getSplitBar : function(){
        return this.split;
    },
    
    hide : function(){
        if(this.split){
            this.split.el.setLocation(-2000,-2000);
            this.split.el.hide();
        }
        YAHOO.ext.SplitLayoutRegion.superclass.hide.call(this);
    },
    
    show : function(){
        if(this.split){
            this.split.el.show();
        }
        YAHOO.ext.SplitLayoutRegion.superclass.show.call(this);
    },
    
    beforeSlide: function(){
        if(YAHOO.ext.util.Browser.isGecko){// firefox overflow auto bug workaround
            this.bodyEl.clip();
            if(this.tabs) this.tabs.bodyEl.clip();
            if(this.activePanel){
                this.activePanel.getEl().clip();
                
                if(this.activePanel.beforeSlide){
                    this.activePanel.beforeSlide();
                }
            }
        }
    },
    
    afterSlide : function(){
        if(YAHOO.ext.util.Browser.isGecko){// firefox overflow auto bug workaround
            this.bodyEl.unclip();
            if(this.tabs) this.tabs.bodyEl.unclip();
            if(this.activePanel){
                this.activePanel.getEl().unclip();
                if(this.activePanel.afterSlide){
                    this.activePanel.afterSlide();
                }
            }
        }
    },
    
    slideOut : function(){
        if(!this.slideEl){
            this.slideEl = new YAHOO.ext.Actor(
                YAHOO.ext.DomHelper.append(this.mgr.el.dom, {tag: 'div', cls:'ylayout-slider'}));
            if(this.config.autoHide !== false){
                var slideInTask = new YAHOO.ext.util.DelayedTask(this.slideIn, this);
                this.slideEl.mon('mouseout', function(e){
                    var to = e.getRelatedTarget();
                    if(to && to != this.slideEl.dom && !YAHOO.util.Dom.isAncestor(this.slideEl.dom, to)){
                        slideInTask.delay(500);
                    }
                }, this, true);
                this.slideEl.mon('mouseover', function(e){
                    slideInTask.cancel();
                }, this, true);
            }
        }
        var sl = this.slideEl, c = this.collapsedEl, cm = this.cmargins;
        this.isSlid = true;
        this.snapshot = {
            'left': this.el.getLeft(true),
            'top': this.el.getTop(true),
            'colbtn': this.collapseBtn.isVisible(),
            'closebtn': this.closeBtn.isVisible()
        };
        this.collapseBtn.hide();
        this.closeBtn.hide();
        this.el.show();
        this.el.setLeftTop(0,0);
        sl.startCapture(true);
        var size;
        switch(this.position){
            case 'west':
                sl.setLeft(c.getRight(true));
                sl.setTop(c.getTop(true));
                size = this.el.getWidth();
            break;
            case 'east':
                sl.setRight(this.mgr.getViewSize().width-c.getLeft(true));
                sl.setTop(c.getTop(true));
                size = this.el.getWidth();
            break;
            case 'north':
                sl.setLeft(c.getLeft(true));
                sl.setTop(c.getBottom(true));
                size = this.el.getHeight();
            break;
            case 'south':
                sl.setLeft(c.getLeft(true));
                sl.setBottom(this.mgr.getViewSize().height-c.getTop(true));
                size = this.el.getHeight();
            break;
        }
        sl.dom.appendChild(this.el.dom);
        YAHOO.util.Event.on(document.body, 'click', this.slideInIf, this, true);
        sl.setSize(this.el.getWidth(), this.el.getHeight());
        this.beforeSlide();
        if(this.activePanel){
            this.activePanel.setSize(this.bodyEl.getWidth(), this.bodyEl.getHeight());
        }
        sl.slideShow(this.getAnchor(), size, this.slideDuration, null, false);
        sl.play(function(){
            this.afterSlide();
        }.createDelegate(this));
    },
    
    slideInIf : function(e){
        var t = YAHOO.util.Event.getTarget(e);
        if(!YAHOO.util.Dom.isAncestor(this.el.dom, t)){
            this.slideIn();
        }
    },
    
    slideIn : function(callback){
        if(this.isSlid && !this.slideEl.playlist.isPlaying()){
            YAHOO.util.Event.removeListener(document.body, 'click', this.slideInIf, this, true);
            this.slideEl.startCapture(true);
            this.slideEl.slideHide(this.getAnchor(), this.slideDuration, null);
            this.beforeSlide();
            this.slideEl.play(function(){
                this.isSlid = false;
                this.el.setPositioning(this.snapshot);
                this.collapseBtn.setVisible(this.snapshot.colbtn);
                this.closeBtn.setVisible(this.snapshot.closebtn);
                this.afterSlide();
                this.mgr.el.dom.appendChild(this.el.dom);
                if(typeof callback == 'function'){
                    callback();
                }
            }.createDelegate(this));
        }
    },
    
    animateExpand : function(){
        var em = this.margins, cm = this.cmargins;
        var c = this.collapsedEl, el = this.el;
        var direction, distance;
        switch(this.position){
            case 'west':
                direction = 'right';
                el.setLeft(-(el.getWidth() + (em.right+em.left)));
                el.setTop(c.getTop(true)-cm.top+em.top);
                distance = el.getWidth() + (em.right+em.left);
            break;
            case 'east':
                direction = 'left';
                el.setLeft(this.mgr.getViewSize().width + em.left);
                el.setTop(c.getTop(true)-cm.top+em.top);
                distance = el.getWidth() + (em.right+em.left);
            break;
            case 'north':
                direction = 'down';
                el.setLeft(em.left);
                el.setTop(-(el.getHeight() + (em.top+em.bottom)));
                distance = el.getHeight() + (em.top+em.bottom);
            break;
            case 'south':
                direction = 'up';
                el.setLeft(em.left);
                el.setTop(this.mgr.getViewSize().height + em.top);
                distance = el.getHeight() + (em.top+em.bottom);
            break;
        }
        this.beforeSlide();
        el.setStyle('z-index', '100');
        el.show();
        c.setLocation(-2000,-2000);
        c.hide();
        el.move(direction, distance, true, this.duration, function(){
            this.afterSlide();
            el.setStyle('z-index', '');
            if(this.split){
                this.split.el.show();
            }
            this.fireEvent('invalidated', this);
            this.fireEvent('expanded', this);
        }.createDelegate(this), this.config.easing || YAHOO.util.Easing.easeOut);
    },
    
    animateCollapse : function(){
        var em = this.margins, cm = this.cmargins;
        var c = this.collapsedEl, el = this.el;
        var direction, distance;
        switch(this.position){
            case 'west':
                direction = 'left';
                distance = el.getWidth() + (em.right+em.left);
            break;
            case 'east':
                direction = 'right';
                distance = el.getWidth() + (em.right+em.left);
            break;
            case 'north':
                direction = 'up';
                distance = el.getHeight() + (em.top+em.bottom);
            break;
            case 'south':
                direction = 'down';
                distance = el.getHeight() + (em.top+em.bottom);
            break;
        }
        this.el.setStyle('z-index', '100');
        this.beforeSlide();
        this.el.move(direction, distance, true, this.duration, function(){
            this.afterSlide();
            this.el.setStyle('z-index', '');
            this.el.setLocation(-20000,-20000);
            this.el.hide();
            this.collapsedEl.show();
            this.fireEvent('collapsed', this); 
        }.createDelegate(this), YAHOO.util.Easing.easeIn);
    },
    
    getAnchor : function(){
        switch(this.position){
            case 'west':
                return 'left';
            case 'east':
                return 'right';
            case 'north':
                return 'top';
            case 'south':
                return 'bottom';
        }
    }
});
