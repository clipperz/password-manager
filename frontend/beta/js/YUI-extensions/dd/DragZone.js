// kill drag drop dependency
if(YAHOO.util.DragDrop){
/**
 * @class YAHOO.ext.dd.DragZone
 * @extends YAHOO.ext.dd.Source
 * This class provides a container DD instance that proxies for multiple child node sources.<br />
 * By default, this class requires that draggable child nodes are registered with
 * {@link YAHOO.ext.dd.Registry}.
 * @cfg {Boolean} containerScroll True to register this container with the Scrollmanager 
 * for auto scrolling during drag operations.
 * @constructor
 * @param {String/HTMLElement/Element} el The container element
 * @param {Object} config
 */
YAHOO.ext.dd.DragZone = function(el, config){
    YAHOO.ext.dd.DragZone.superclass.constructor.call(this, el, config);
    if(this.containerScroll){
        YAHOO.ext.dd.ScrollManager.register(this.el);
    }
};

YAHOO.extendX(YAHOO.ext.dd.DragZone, YAHOO.ext.dd.DragSource, {
    /**
     * Called when a mousedown occurs in this container. Looks in {@link YAHOO.ext.dd.Registry}
     * for a valid target to drag based on the mouse down. Override this method
     * to provide your own lookup logic (e.g. finding a child by class name). Make sure your returned
     * object has a "ddel" attribute (with an HTML Element) for other functions to work.
     * @param {EventObject} e The mouse down event
     * @return {Object} The dragData
     */
    getDragData : function(e){
        return YAHOO.ext.dd.Registry.getHandleFromEvent(e);
    },
    
    /**
     * Called once drag threshold has been reached to initialize the proxy element. By default, it clones the
     * this.dragData.ddel
     * @param {EventObject} e The current event
     * @return {Boolean} true to continue the drag, false to cancel
     */
    onInitDrag : function(e){
        this.proxy.update(this.dragData.ddel.cloneNode(true));
        return true;
    },
    
    /**
     * Called after a repair of an invalid drop. By default, highlights this.dragData.ddel 
     */
    afterRepair : function(){
        YAHOO.ext.Element.fly(this.dragData.ddel).highlight(this.hlColor || 'c3daf9');
        this.dragging = false;
    },

    /**
     * Called before a repair of an invalid drop to get the XY to animate to. By default returns
     * the XY of this.dragData.ddel
     * @param {EventObject} e The mouse up event
     * @return {Array} The xy location (e.g. [100, 200])
     */
    getRepairXY : function(e){
        return YAHOO.ext.Element.fly(this.dragData.ddel).getXY();  
    }
});
}
