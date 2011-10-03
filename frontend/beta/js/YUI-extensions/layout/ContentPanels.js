/**
 * @class YAHOO.ext.ContentPanel
 * @extends YAHOO.ext.util.Observable
 * A basic ContentPanel element.
 * @cfg {Boolean} fitToFrame True for this panel to manually adjust it's size when the region resizes  (defaults to false)
 * @cfg {Boolean/Object} autoCreate True to auto generate the DOM element for this panel, or a DomHelper config of the element to create
 * @cfg {Boolean} closable True if the panel can be closed/removed
 * @cfg {Boolean} background True if the panel should not be activated when it is added (defaults to false)
 * @cfg {String/HTMLElement/Element} resizeEl An element to resize if fitToFrame is true (instead of this panel's element)
 * @cfg {Toolbar} toolbar A toolbar for this panel
 * @cfg {Boolean} autoScroll True to scroll overflow in this panel (use with fitToFrame)
 * @cfg {String} title The title for this panel
 * @cfg {Array} adjustments Values to <b>add</b> to the width/height when doing a fitToFrame (default is [0, 0])
 * @constructor
 * Create a new ContentPanel.
 * @param {String/HTMLElement/Element} el The container element for this panel
 * @param {String/Object} config A string to set only the title or a config object
 * @param {String} content (optional) Set the HTML content for this panel
 */
YAHOO.ext.ContentPanel = function(el, config, content){
    YAHOO.ext.ContentPanel.superclass.constructor.call(this);
    this.el = getEl(el, true);
    if(!this.el && config && config.autoCreate){
        if(typeof config.autoCreate == 'object'){
            if(!config.autoCreate.id){
                config.autoCreate.id = el;
            }
            this.el = YAHOO.ext.DomHelper.append(document.body,
                        config.autoCreate, true);
        }else{
            this.el = YAHOO.ext.DomHelper.append(document.body,
                        {tag: 'div', cls: 'ylayout-inactive-content', id: el}, true);
        }
    }
    this.closable = false;
    this.loaded = false;
    this.active = false;
    if(typeof config == 'string'){
        this.title = config;
    }else{
        YAHOO.ext.util.Config.apply(this, config);
    }
    if(this.resizeEl){
        this.resizeEl = getEl(this.resizeEl, true);
    }else{
        this.resizeEl = this.el;
    }
    this.events = {
        /**
         * @event activate
         * Fires when this panel is activated. 
         * @param {YAHOO.ext.ContentPanel} this
         */
        'activate' : new YAHOO.util.CustomEvent('activate'),
        /**
         * @event deactivate
         * Fires when this panel is activated. 
         * @param {YAHOO.ext.ContentPanel} this
         */
        'deactivate' : new YAHOO.util.CustomEvent('deactivate') 
    };
    if(this.autoScroll){
        this.resizeEl.setStyle('overflow', 'auto');
    }
    if(content){
        this.setContent(content);
    }
};

YAHOO.extendX(YAHOO.ext.ContentPanel, YAHOO.ext.util.Observable, {
    setRegion : function(region){
        this.region = region;
        if(region){
           this.el.replaceClass('ylayout-inactive-content', 'ylayout-active-content'); 
        }else{
           this.el.replaceClass('ylayout-active-content', 'ylayout-inactive-content'); 
        } 
    },
    
    /**
     * Returns the toolbar for this Panel if one was configured
     * @return {YAHOO.ext.Toolbar} 
     */
    getToolbar : function(){
        return this.toolbar;
    },
    
    setActiveState : function(active){
        this.active = active;
        if(!active){
            this.fireEvent('deactivate', this);
        }else{
            this.fireEvent('activate', this);
        }
    },
    /**
     * Updates this panel's element
     * @param {String} content The new content
     * @param {<i>Boolean</i>} loadScripts (optional) true to look for and process scripts
    */
    setContent : function(content, loadScripts){
        this.el.update(content, loadScripts);
    },
    
    /**
     * Get the {@link YAHOO.ext.UpdateManager} for this panel. Enables you to perform Ajax updates.
     * @return {YAHOO.ext.UpdateManager} The UpdateManager
     */
    getUpdateManager : function(){
        return this.el.getUpdateManager();
    },
    
    /**
     * Set a URL to be used to load the content for this panel.
     * @param {String/Function} url The url to load the content from or a function to call to get the url
     * @param {<i>String/Object</i>} params (optional) The string params for the update call or an object of the params. See {@link YAHOO.ext.UpdateManager#update} for more details. (Defaults to null)
     * @param {<i>Boolean</i>} loadOnce (optional) Whether to only load the content once. If this is false it makes the Ajax call every time this panel is activated. (Defaults to false)
     * @return {YAHOO.ext.UpdateManager} The UpdateManager
     */
    setUrl : function(url, params, loadOnce){
        if(this.refreshDelegate){
            this.removeListener('activate', this.refreshDelegate);
        }
        this.refreshDelegate = this._handleRefresh.createDelegate(this, [url, params, loadOnce]);
        this.on('activate', this._handleRefresh.createDelegate(this, [url, params, loadOnce]));
        return this.el.getUpdateManager();
    },
    
    _handleRefresh : function(url, params, loadOnce){
        if(!loadOnce || !this.loaded){
            var updater = this.el.getUpdateManager();
            updater.update(url, params, this._setLoaded.createDelegate(this));
        }
    },
    
    _setLoaded : function(){
        this.loaded = true;
    }, 
    
    /**
     * Returns this panel's id
     * @return {String} 
     */
    getId : function(){
        return this.el.id;
    },
    
    /**
     * Returns this panel's element
     * @return {YAHOO.ext.Element} 
     */
    getEl : function(){
        return this.el;
    },
    
    adjustForComponents : function(width, height){
        if(this.toolbar){
            var te = this.toolbar.getEl();
            height -= te.getHeight();
            te.setWidth(width);
        }
        if(this.adjustments){
            width += this.adjustments[0];
            height += this.adjustments[1];
        }
        return {'width': width, 'height': height};
    },
    
    setSize : function(width, height){
        if(this.fitToFrame){
            var size = this.adjustForComponents(width, height);
            this.resizeEl.setSize(this.autoWidth ? 'auto' : size.width, size.height);
        }
    },
    
    /**
     * Returns this panel's title
     * @return {String} 
     */
    getTitle : function(){
        return this.title;
    },
    
    /**
     * Set this panel's title
     * @param {String} title
     */
    setTitle : function(title){
        this.title = title;
        if(this.region){
            this.region.updatePanelTitle(this, title);
        }
    },
    
    /**
     * Returns true is this panel was configured to be closable
     * @return {Boolean} 
     */
    isClosable : function(){
        return this.closable;
    },
    
    beforeSlide : function(){
        this.el.clip();
        this.resizeEl.clip();
    },
    
    afterSlide : function(){
        this.el.unclip();
        this.resizeEl.unclip();
    },
    
    /**
     *   Force a content refresh from the URL specified in the setUrl() method.
     *   Will fail silently if the setUrl method has not been called.
     *   This does not activate the panel, just updates its content.
     */
    refresh : function(){
        if(this.refreshDelegate){
           this.loaded = false;
           this.refreshDelegate();
        }
    },
    
    /**
     * Destroys this panel
     */
    destroy : function(){
        this.el.removeAllListeners();
        var tempEl = document.createElement('span');
        tempEl.appendChild(this.el.dom);
        tempEl.innerHTML = '';
        this.el = null;
    }
});

/**
 * @class YAHOO.ext.GridPanel
 * @extends YAHOO.ext.ContentPanel
 * @constructor
 * Create a new GridPanel.
 * @param {YAHOO.ext.grid.Grid} grid The grid for this panel
 * @param {String/Object} config A string to set only the title or a config object
 */
YAHOO.ext.GridPanel = function(grid, config){
    this.wrapper = YAHOO.ext.DomHelper.append(document.body, // wrapper for IE7 strict & safari scroll issue
        {tag: 'div', cls: 'ylayout-grid-wrapper ylayout-inactive-content'}, true);
    this.wrapper.dom.appendChild(grid.container.dom);
    YAHOO.ext.GridPanel.superclass.constructor.call(this, this.wrapper, config);
    if(this.toolbar){
        this.toolbar.el.insertBefore(this.wrapper.dom.firstChild);
    }
    grid.monitorWindowResize = false; // turn off autosizing
    grid.autoHeight = false;
    grid.autoWidth = false;
    this.grid = grid;
    this.grid.container.replaceClass('ylayout-inactive-content', 'ylayout-component-panel');
};

YAHOO.extendX(YAHOO.ext.GridPanel, YAHOO.ext.ContentPanel, {
    getId : function(){
        return this.grid.id;
    },
    
    /**
     * Returns the grid for this panel
     * @return {YAHOO.ext.grid.Grid} 
     */
    getGrid : function(){
        return this.grid;    
    },
    
    setSize : function(width, height){
        var grid = this.grid;
        var size = this.adjustForComponents(width, height);
        grid.container.setSize(size.width, size.height);
        grid.autoSize();
    },
    
    beforeSlide : function(){
        this.grid.getView().wrapEl.clip();
    },
    
    afterSlide : function(){
        this.grid.getView().wrapEl.unclip();
    },
    
    destroy : function(){
        this.grid.getView().unplugDataModel(this.grid.getDataModel());
        this.grid.container.removeAllListeners();
        YAHOO.ext.GridPanel.superclass.destroy.call(this);
    }
});


/**
 * @class YAHOO.ext.NestedLayoutPanel
 * @extends YAHOO.ext.ContentPanel
 * @constructor
 * Create a new NestedLayoutPanel.
 * @param {YAHOO.ext.BorderLayout} layout The layout for this panel
 * @param {String/Object} config A string to set only the title or a config object
 */
YAHOO.ext.NestedLayoutPanel = function(layout, config){
    YAHOO.ext.NestedLayoutPanel.superclass.constructor.call(this, layout.getEl(), config);
    layout.monitorWindowResize = false; // turn off autosizing
    this.layout = layout;
    this.layout.getEl().addClass('ylayout-nested-layout');
};

YAHOO.extendX(YAHOO.ext.NestedLayoutPanel, YAHOO.ext.ContentPanel, {
    setSize : function(width, height){
        var size = this.adjustForComponents(width, height);
        this.layout.getEl().setSize(size.width, size.height);
        this.layout.layout();
    },
    
    /**
     * Returns the nested BorderLayout for this panel
     * @return {YAHOO.ext.BorderLayout} 
     */
    getLayout : function(){
        return this.layout;
    }
});
