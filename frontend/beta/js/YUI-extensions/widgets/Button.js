/**
 * @class YAHOO.ext.Button
 * @extends YAHOO.ext.util.Observable
 * Simple Button class
 * @cfg {String} text The button text
 * @cfg {Function} handler A function called when the button is clicked (can be used instead of click event)
 * @cfg {Object} scope The scope of the handler
 * @cfg {Number} minWidth The minimum width for this button (used to give a set of buttons a common width)
 * @constructor
 * Create a new button
 * @param {String/HTMLElement/Element} renderTo The element to append the button to
 * @param {Object} config The config object
 */
YAHOO.ext.Button = function(renderTo, config){
    YAHOO.ext.util.Config.apply(this, config);
    this.events = {
        /**
	     * @event click
	     * Fires when this button is clicked
	     * @param {Button} this
	     * @param {EventObject} e The click event
	     */
	    'click' : true  
    };
    if(renderTo){
        this.render(renderTo);
    }
};

YAHOO.extendX(YAHOO.ext.Button, YAHOO.ext.util.Observable, {
    render : function(renderTo){
        var btn;
        if(!this.dhconfig){
            if(!YAHOO.ext.Button.buttonTemplate){
                // hideous table template
                YAHOO.ext.Button.buttonTemplate = new YAHOO.ext.DomHelper.Template('<a href="#" class="ybtn-focus"><table border="0" cellpadding="0" cellspacing="0" class="ybtn-wrap"><tbody><tr><td class="ybtn-left">&#160;</td><td class="ybtn-center" unselectable="on">{0}</td><td class="ybtn-right">&#160;</td></tr></tbody></table></a>');
            }
            btn = YAHOO.ext.Button.buttonTemplate.append(
               getEl(renderTo).dom, [this.text], true);
            this.tbl = getEl(btn.dom.firstChild, true);
        }else{
            btn = YAHOO.ext.DomHelper.append(this.footer.dom, this.dhconfig, true);
        }
        this.el = btn;
        this.autoWidth();
        btn.addClass('ybtn');
        btn.mon('click', this.onClick, this, true);
        btn.on('mouseover', this.onMouseOver, this, true);
        btn.on('mouseout', this.onMouseOut, this, true);
        btn.on('mousedown', this.onMouseDown, this, true);
        btn.on('mouseup', this.onMouseUp, this, true);
    },
    /**
     * Returns the buttons element
     * @return {YAHOO.ext.Element}
     */
    getEl : function(){
        return this.el;  
    },
    
    /**
     * Destroys this Button. 
     */
    destroy : function(){
        this.el.removeAllListeners();
        this.purgeListeners();
        this.el.update('');
        this.el.remove();
    },
    
    autoWidth : function(){
        if(this.tbl){
            this.el.setWidth('auto');
            this.tbl.setWidth('auto');
            if(this.minWidth){
                 if(this.tbl.getWidth() < this.minWidth){
                     this.tbl.setWidth(this.minWidth);
                 }
            }
            this.el.setWidth(this.tbl.getWidth());
        } 
    },
    /**
     * Sets this buttons click handler
     * @param {Function} handler The function to call when the button is clicked
     * @param {Object} scope (optional) Scope for the function passed above
     */
    setHandler : function(handler, scope){
        this.handler = handler;
        this.scope = scope;  
    },
    
    /**
     * Set this buttons text
     * @param {String} text
     */
    setText : function(text){
        this.text = text;
        this.el.dom.firstChild.firstChild.firstChild.childNodes[1].innerHTML = text;
        this.autoWidth();
    },
    
    /**
     * Get the text for this button
     * @return {String}
     */
    getText : function(){
        return this.text;  
    },
    
    /**
     * Show this button
     */
    show: function(){
        this.el.setStyle('display', '');
    },
    
    /**
     * Hide this button
     */
    hide: function(){
        this.el.setStyle('display', 'none'); 
    },
    
    /**
     * Convenience function for boolean show/hide
     * @param {Boolean} visible true to show/false to hide
     */
    setVisible: function(visible){
        if(visible) {
            this.show();
        }else{
            this.hide();
        }
    },
    
    /**
     * Focus the button
     */
    focus : function(){
        this.el.focus();    
    },
    
    /**
     * Disable this button
     */
    disable : function(){
        this.el.addClass('ybtn-disabled');
        this.disabled = true;
    },
    
    /**
     * Enable this button
     */
    enable : function(){
        this.el.removeClass('ybtn-disabled');
        this.disabled = false;
    },
    
    onClick : function(e){
        e.preventDefault();
        if(!this.disabled){
            this.fireEvent('click', this, e);
            if(this.handler){
                this.handler.call(this.scope || this, this, e);
            }
        }
    },
    onMouseOver : function(e){
        if(!this.disabled){
            this.el.addClass('ybtn-over');
        }
    },
    onMouseOut : function(e){
        this.el.removeClass('ybtn-over');
    },
    onMouseDown : function(){
        if(!this.disabled){
            this.el.addClass('ybtn-click');
        }
    },
    onMouseUp : function(){
        this.el.removeClass('ybtn-click');
    }    
});
