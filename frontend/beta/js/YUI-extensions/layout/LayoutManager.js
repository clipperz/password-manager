/**
 * @class YAHOO.ext.LayoutManager
 * @extends YAHOO.ext.util.Observable
 * Base class for layout managers.
 */
YAHOO.ext.LayoutManager = function(container){
    YAHOO.ext.LayoutManager.superclass.constructor.call(this);
    this.el = getEl(container, true);
    // ie scrollbar fix
    if(this.el.dom == document.body && YAHOO.ext.util.Browser.isIE){
        document.body.scroll = 'no';
    }
    this.id = this.el.id;
    this.el.addClass('ylayout-container');
    /** false to disable window resize monitoring @type Boolean */
    this.monitorWindowResize = true;
    this.regions = {};
    this.events = {
        /**
         * @event layout
         * Fires when a layout is performed. 
         * @param {YAHOO.ext.LayoutManager} this
         */
        'layout' : new YAHOO.util.CustomEvent(),
        /**
         * @event regionresized
         * Fires when the user resizes a region. 
         * @param {YAHOO.ext.LayoutRegion} region
         * @param {Number} newSize The new size (width for east/west, height for north/south)
         */
        'regionresized' : new YAHOO.util.CustomEvent(),
        /**
         * @event regioncollapsed
         * Fires when a region is collapsed. 
         * @param {YAHOO.ext.LayoutRegion} region
         */
        'regioncollapsed' : new YAHOO.util.CustomEvent(),
        /**
         * @event regionexpanded
         * Fires when a region is expanded.  
         * @param {YAHOO.ext.LayoutRegion} region
         */
        'regionexpanded' : new YAHOO.util.CustomEvent()
    };
    this.updating = false;
    YAHOO.ext.EventManager.onWindowResize(this.onWindowResize, this, true);
};

YAHOO.extendX(YAHOO.ext.LayoutManager, YAHOO.ext.util.Observable, {
    /**
     * Returns true if this layout is currently being updated
     * @return {Boolean}
     */
    isUpdating : function(){
        return this.updating; 
    },
    
    /**
     * Suspend the LayoutManager from doing auto-layouts while
     * making multiple add or remove calls
     */
    beginUpdate : function(){
        this.updating = true;    
    },
    
    /**
     * Restore auto-layouts and optionally disable the manager from performing a layout
     * @param {Boolean} noLayout true to disable a layout update 
     */
    endUpdate : function(noLayout){
        this.updating = false;
        if(!noLayout){
            this.layout();
        }    
    },
    
    layout: function(){
        
    },
    
    onRegionResized : function(region, newSize){
        this.fireEvent('regionresized', region, newSize);
        this.layout();
    },
    
    onRegionCollapsed : function(region){
        this.fireEvent('regioncollapsed', region);
    },
    
    onRegionExpanded : function(region){
        this.fireEvent('regionexpanded', region);
    },
        
    /**
     * Returns the size of the current view, This method normalizes document.body and element embedded layouts and
     * performs box-model adjustments.
     * @return {Object} The size as an object {width: (the width), height: (the height)}
     */
    getViewSize : function(){
        var size;
        if(this.el.dom != document.body){
            this.el.beginMeasure();
            size = this.el.getSize();
            this.el.endMeasure();
        }else{
            size = {width: YAHOO.util.Dom.getViewportWidth(), height: YAHOO.util.Dom.getViewportHeight()};
        }
        size.width -= this.el.getBorderWidth('lr')-this.el.getPadding('lr');
        size.height -= this.el.getBorderWidth('tb')-this.el.getPadding('tb');
        return size;
    },
    
    /**
     * Returns the element this layout is bound to.
     * @return {YAHOO.ext.Element}
     */
    getEl : function(){
        return this.el;
    },
    
    /**
     * Returns the specified region.
     * @param {String} target The region key
     * @return {YAHOO.ext.LayoutRegion}
     */
    getRegion : function(target){
        return this.regions[target.toLowerCase()];
    },
    
    onWindowResize : function(){
        if(this.monitorWindowResize){
            this.layout();
        }
    }
});
