/**
 * @class YAHOO.ext.Toolbar
 * Basic Toolbar used by the Grid to create the paging toolbar. This class is reusable but functionality
 * is limited. Look for more functionality in a future version. 
 * @constructor
 * @param {String/HTMLElement/Element} container
 * @param {Array} buttons (optional) array of button configs or elements to add
 */ 
 YAHOO.ext.Toolbar = function(container, buttons){
    this.el = getEl(container, true);
    var div = document.createElement('div');
    div.className = 'ytoolbar';
    var tb = document.createElement('table');
    tb.border = 0;
    tb.cellPadding = 0; 
    tb.cellSpacing = 0;
    div.appendChild(tb);
    var tbody = document.createElement('tbody');
    tb.appendChild(tbody);
    var tr = document.createElement('tr');
    tbody.appendChild(tr);
    this.el.dom.appendChild(div);
    this.tr = tr;
    if(buttons){
        this.add.apply(this, buttons);
    }
};

YAHOO.ext.Toolbar.prototype = {
    /**
     * Adds element(s) to the toolbar - this function takes a variable number of 
     * arguments of mixed type and adds them to the toolbar...
     * 
     * @param {Mixed} arg If arg is a ToolbarButton, it is added. If arg is a string, it is wrapped 
     * in a ytb-text element and added unless the text is "separator" in which case a separator
     * is added. Otherwise, it is assumed the element is an HTMLElement and it is added directly.
     */
    add : function(){
        for(var i = 0; i < arguments.length; i++){
            var el = arguments[i];
            var td = document.createElement('td');
            this.tr.appendChild(td);
            if(el instanceof YAHOO.ext.ToolbarButton){
                el.init(td);
            }else if(el instanceof Array){
                this.addButton(el);
            }else if(typeof el == 'string'){
                var span = document.createElement('span');
                if(el == 'separator'){
                    span.className = 'ytb-sep';
                }else{
                    span.innerHTML = el;
                    span.className = 'ytb-text';
                }
                td.appendChild(span);
            }else if(typeof el == 'object' && el.nodeType){ // must be element?
                td.appendChild(el);
            }else if(typeof el == 'object'){ // must be button config?
                this.addButton(el);
            }
        }
    },
    
    /**
     * Returns the element for this toolbar
     * @return {YAHOO.ext.Element}
     */
    getEl : function(){
        return this.el;  
    },
    
    /**
     * Adds a separator
     */
    addSeparator : function(){
        var td = document.createElement('td');
        this.tr.appendChild(td);
        var span = document.createElement('span');
        span.className = 'ytb-sep';
        td.appendChild(span);
    },
    
    /**
     * Add a button (or buttons), see {@link YAHOO.ext.ToolbarButton} for more info on the config
     * @param {Object/Array} config A button config or array of configs
     * @return {YAHOO.ext.ToolbarButton/Array}
     */
    addButton : function(config){
        if(config instanceof Array){
            var buttons = [];
            for(var i = 0, len = config.length; i < len; i++) {
            	buttons.push(this.addButton(config[i]));
            }
            return buttons;
        }
        var b = config;
        if(!(config instanceof YAHOO.ext.ToolbarButton)){
             b = new YAHOO.ext.ToolbarButton(config);
        }
        this.add(b);
        return b;
    },
    
    /**
     * Adds text to the toolbar
     * @param {String} text The text to add
     * @return {HTMLElement} The span element created which you can use to update the text.
     */
    addText : function(text){
        var td = document.createElement('td');
        this.tr.appendChild(td);
        var span = document.createElement('span');
        span.className = 'ytb-text';
        span.innerHTML = text;
        td.appendChild(span);
        return span;
    },
    
    /**
     * Inserts a button (or buttons) at the specified index
     * @param {Number} index The index where the buttons are to be inserted
     * @param {Object/Array} config A button config or array of configs
     * @return {YAHOO.ext.ToolbarButton/Array}
     */
    insertButton : function(index, config){
        if(config instanceof Array){
            var buttons = [];
            for(var i = 0, len = config.length; i < len; i++) {
               buttons.push(this.insertButton(index + i, config[i]));
            }
            return buttons;
        }
        var b = new YAHOO.ext.ToolbarButton(config);
        var td = document.createElement('td');
        var nextSibling = this.tr.childNodes[index];
        if (nextSibling)
           this.tr.insertBefore(td, nextSibling);
       else
           this.tr.appendChild(td);
        b.init(td);
        return b;
    }
};

/**
 * @class YAHOO.ext.ToolbarButton
 * A toolbar button. The config has the following options:
 * <ul>
 * <li>className - The CSS class for the button. Use this to attach a background image for an icon.</li>
 * <li>text - The button's text</li>
 * <li>tooltip - The buttons tooltip text</li>
 * <li>click - function to call when the button is clicked</li>
 * <li>mouseover - function to call when the mouse moves over the button</li>
 * <li>mouseout - function to call when the mouse moves off the button</li>
 * <li>scope - The scope of the above event handlers</li>
 * <li></li>
 * <li></li>
 * @constructor
 * @param {Object} config
 */
YAHOO.ext.ToolbarButton = function(config){
    YAHOO.ext.util.Config.apply(this, config);
};

YAHOO.ext.ToolbarButton.prototype = {
    /** @private */
    init : function(appendTo){
        var element = document.createElement('span');
        element.className = 'ytb-button';
        if(this.id){
            element.id = this.id;
        }
        this.setDisabled(this.disabled === true);
        var inner = document.createElement('span');
        inner.className = 'ytb-button-inner ' + (this.className || this.cls);
        inner.unselectable = 'on';
        if(this.tooltip){
            element.setAttribute('title', this.tooltip);
        }
        if(this.style){
           YAHOO.ext.DomHelper.applyStyles(inner, this.style);
        } 
        element.appendChild(inner);
        appendTo.appendChild(element);
        this.el = getEl(element, true);
        this.el.unselectable();
        inner.innerHTML = (this.text ? this.text : '&#160;');
        this.inner = inner;
        this.el.mon('click', this.onClick, this, true);    
        this.el.mon('mouseover', this.onMouseOver, this, true);    
        this.el.mon('mouseout', this.onMouseOut, this, true);
    },
    
    /**
     * Sets this buttons click handler
     * @param {Function} click The function to call when the button is clicked
     * @param {Object} scope (optional) Scope for the function passed above
     */
    setHandler : function(click, scope){
        this.click = click;
        this.scope = scope;  
    },
    
    /**
     * Set this buttons text
     * @param {String} text
     */
    setText : function(text){
        this.inner.innerHTML = text;    
    },
    
    /**
     * Set this buttons tooltip text
     * @param {String} text
     */
    setTooltip : function(text){
        this.el.dom.title = text;    
    },
    
    /**
     * Show this button
     */
    show: function(){
        this.el.dom.parentNode.style.display = '';
    },
    
    /**
     * Hide this button
     */
    hide: function(){
        this.el.dom.parentNode.style.display = 'none';  
    },
    
    /**
     * Disable this button
     */
    disable : function(){
        this.disabled = true;
        if(this.el){
            this.el.addClass('ytb-button-disabled');
        }
    },
    
    /**
     * Enable this button
     */
    enable : function(){
        this.disabled = false;
        if(this.el){
            this.el.removeClass('ytb-button-disabled');
        }
    },
    
    /**
     * Returns true if this button is disabled.
     * @return {Boolean}
     */
    isDisabled : function(){
        return this.disabled === true;
    },
    
    setDisabled : function(disabled){
        if(disabled){
            this.disable();
        }else{
            this.enable();
        }
    },
    
    /** @private */
    onClick : function(){
        if(!this.disabled && this.click){
            this.click.call(this.scope || window, this);
        }
    },
    
    /** @private */
    onMouseOver : function(){
        if(!this.disabled){
            this.el.addClass('ytb-button-over');
            if(this.mouseover){
                this.mouseover.call(this.scope || window, this);
            }
        }
    },
    
    /** @private */
    onMouseOut : function(){
        this.el.removeClass('ytb-button-over');
        if(!this.disabled){
            if(this.mouseout){
                this.mouseout.call(this.scope || window, this);
            }
        }
    }
};
