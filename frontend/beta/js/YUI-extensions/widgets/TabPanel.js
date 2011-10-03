/**
 * @class YAHOO.ext.TabPanel
 * @extends YAHOO.ext.util.Observable
 * Creates a lightweight TabPanel component using Yahoo! UI.
 * <br><br>
 * Usage:
 * <pre><code>
    <font color="#008000">// basic tabs 1, built from existing content</font>
    var tabs = new YAHOO.ext.TabPanel('tabs1');
    tabs.addTab('script', "View Script");
    tabs.addTab('markup', "View Markup");
    tabs.activate('script');
    
    <font color="#008000">// more advanced tabs, built from javascript</font>
    var jtabs = new YAHOO.ext.TabPanel('jtabs');
    jtabs.addTab('jtabs-1', "Normal Tab", "My content was added during construction.");
    
    <font color="#008000">// set up the UpdateManager</font>
    var tab2 = jtabs.addTab('jtabs-2', "Ajax Tab 1");
    var updater = tab2.getUpdateManager();
    updater.setDefaultUrl('ajax1.htm');
    tab2.onActivate.subscribe(updater.refresh, updater, true);
    
    <font color="#008000">// Use setUrl for Ajax loading</font>
    var tab3 = jtabs.addTab('jtabs-3', "Ajax Tab 2");
    tab3.setUrl('ajax2.htm', null, true);
    
    <font color="#008000">// Disabled tab</font>
    var tab4 = jtabs.addTab('tabs1-5', "Disabled Tab", "Can't see me cause I'm disabled");
    tab4.disable();
    
    jtabs.activate('jtabs-1');
}
 * </code></pre>
 * @requires YAHOO.ext.Element
 * @requires YAHOO.ext.UpdateManager
 * @requires YAHOO.util.Dom
 * @requires YAHOO.util.Event
 * @requires YAHOO.util.CustomEvent 
 * @requires YAHOO.util.Connect (optional)
 * @constructor
 * Create new TabPanel.
 * @param {String/HTMLElement/Element} container The id, DOM element or YAHOO.ext.Element container where this TabPanel is to be rendered. 
 * @param {Boolean} config Config object to set any properties for this TabPanel or true to render the tabs on the bottom. 
 */
YAHOO.ext.TabPanel = function(container, config){
    /**
    * The container element for this TabPanel.
    * @type YAHOO.ext.Element
    */
    this.el = getEl(container, true);
    /** The position of the tabs. Can be 'top' or 'bottom' @type String */
    this.tabPosition = 'top';
    this.currentTabWidth = 0;
    /** The minimum width of a tab (ignored if resizeTabs is not true). @type Number */
    this.minTabWidth = 40;
    /** The maximum width of a tab (ignored if resizeTabs is not true). @type Number */
    this.maxTabWidth = 250;
    /** The preferred (default) width of a tab (ignored if resizeTabs is not true). @type Number */
    this.preferredTabWidth = 175;
    /** Set this to true to enable dynamic tab resizing. @type Boolean */
    this.resizeTabs = false;
    /** Set this to true to turn on window resizing monitoring (ignored if resizeTabs is not true). @type Boolean */
    this.monitorResize = true;
    
    if(config){
        if(typeof config == 'boolean'){
            this.tabPosition = config ? 'bottom' : 'top';
        }else{
            YAHOO.ext.util.Config.apply(this, config);
        }
    }
    if(this.tabPosition == 'bottom'){
        this.bodyEl = getEl(this.createBody(this.el.dom));
        this.el.addClass('ytabs-bottom');
    }
    this.stripWrap = getEl(this.createStrip(this.el.dom), true);
    this.stripEl = getEl(this.createStripList(this.stripWrap.dom), true);
    this.stripBody = getEl(this.stripWrap.dom.firstChild.firstChild, true);
    if(YAHOO.ext.util.Browser.isIE){
        YAHOO.util.Dom.setStyle(this.stripWrap.dom.firstChild, 'overflow-x', 'hidden');
    }
    if(this.tabPosition != 'bottom'){
    /** The body element that contains TabPaneItem bodies. 
     * @type YAHOO.ext.Element
     */
      this.bodyEl = getEl(this.createBody(this.el.dom));
      this.el.addClass('ytabs-top');
    }
    this.items = [];
    
    this.bodyEl.setStyle('position', 'relative');
    
    // add indexOf to array if it isn't present
    if(!this.items.indexOf){
        this.items.indexOf = function(o){
            for(var i = 0, len = this.length; i < len; i++){
                if(this[i] == o) return i;
            }
            return -1;
        }
    }
    this.active = null;
    this.onTabChange = new YAHOO.util.CustomEvent('TabItem.onTabChange');
    this.activateDelegate = this.activate.createDelegate(this);
    
    this.events = {
        /**
         * @event tabchange
         * Fires when the active tab changes
         * @param {YAHOO.ext.TabPanel} this
         * @param {YAHOO.ext.TabPanelItem} activePanel The new active tab
         */
        'tabchange': this.onTabChange,
        /**
         * @event beforetabchange
         * Fires before the active tab changes, set cancel to true on the "e" parameter to cancel the change
         * @param {YAHOO.ext.TabPanel} this
         * @param {Object} e Set cancel to true on this object to cancel the tab change
         * @param {YAHOO.ext.TabPanelItem} tab The tab being changed to
         */
        'beforetabchange' : new YAHOO.util.CustomEvent('beforechange')
    };
    
    YAHOO.ext.EventManager.onWindowResize(this.onResize, this, true);
    this.cpad = this.el.getPadding('lr');
    this.hiddenCount = 0;
}

YAHOO.ext.TabPanel.prototype = {
    fireEvent : YAHOO.ext.util.Observable.prototype.fireEvent,
    on : YAHOO.ext.util.Observable.prototype.on,
    addListener : YAHOO.ext.util.Observable.prototype.addListener,
    delayedListener : YAHOO.ext.util.Observable.prototype.delayedListener,
    removeListener : YAHOO.ext.util.Observable.prototype.removeListener,
    purgeListeners : YAHOO.ext.util.Observable.prototype.purgeListeners,
    /**
     * Creates a new TabPanelItem by looking for an existing element with the provided id - if it's not found it creates one.
     * @param {String} id The id of the div to use <b>or create</b>
     * @param {String} text The text for the tab
     * @param {<i>String</i>} content (optional) Content to put in the TabPanelItem body
     * @param {<i>Boolean</i>} closable (optional) True to create a close icon on the tab
     * @return {YAHOO.ext.TabPanelItem} The created TabPanelItem
     */
    addTab : function(id, text, content, closable){
        var item = new YAHOO.ext.TabPanelItem(this, id, text, closable);
        this.addTabItem(item);
        if(content){
            item.setContent(content);
        }
        return item;
    },
    
    /**
     * Returns the TabPanelItem with the specified id/index
     * @param {String/Number} id The id or index of the TabPanelItem to fetch.
     * @return {YAHOO.ext.TabPanelItem}
     */
    getTab : function(id){
        return this.items[id];
    },
    
    /**
     * Hides the TabPanelItem with the specified id/index
     * @param {String/Number} id The id or index of the TabPanelItem to hide.
     */
    hideTab : function(id){
        var t = this.items[id];
        if(!t.isHidden()){
           t.setHidden(true);
           this.hiddenCount++;
           this.autoSizeTabs();
        }
    },
    
    /**
     * "Unhides" the TabPanelItem with the specified id/index
     * @param {String/Number} id The id or index of the TabPanelItem to unhide.
     */
    unhideTab : function(id){
        var t = this.items[id];
        if(t.isHidden()){
           t.setHidden(false);
           this.hiddenCount--;
           this.autoSizeTabs();
        }
    },
    
    /**
     * Add an existing TabPanelItem.
     * @param {YAHOO.ext.TabPanelItem} item The TabPanelItem to add
     */
    addTabItem : function(item){
        this.items[item.id] = item;
        this.items.push(item);
        if(this.resizeTabs){
           item.setWidth(this.currentTabWidth || this.preferredTabWidth)
           this.autoSizeTabs();
        }else{
            item.autoSize();
        }
    },
        
    /**
     * Remove a TabPanelItem.
     * @param {String/Number} id The id or index of the TabPanelItem to remove.
     */
    removeTab : function(id){
        var items = this.items;
        var tab = items[id];
        if(!tab) return;
        var index = items.indexOf(tab);
        if(this.active == tab && items.length > 1){
            var newTab = this.getNextAvailable(index);
            if(newTab)newTab.activate();
        }
        this.stripEl.dom.removeChild(tab.pnode.dom);
        if(tab.bodyEl.dom.parentNode == this.bodyEl.dom){ // if it was moved already prevent error
            this.bodyEl.dom.removeChild(tab.bodyEl.dom);
        }
        items.splice(index, 1);
        delete this.items[tab.id];
        tab.fireEvent('close', tab);
        tab.purgeListeners();
        this.autoSizeTabs();
    },
    
    getNextAvailable : function(start){
        var items = this.items;
        var index = start;
        // look for a next tab that will slide over to
        // replace the one being removed
        while(index < items.length){
            var item = items[++index];
            if(item && !item.isHidden()){
                return item;
            }
        }
        // if one isn't found select the previous tab (on the left)
        var index = start;
        while(index >= 0){
            var item = items[--index];
            if(item && !item.isHidden()){
                return item;
            }
        }
        return null;
    },
    
    /**
     * Disable a TabPanelItem. <b>It cannot be the active tab, if it is this call is ignored.</b>. 
     * @param {String/Number} id The id or index of the TabPanelItem to disable.
     */
    disableTab : function(id){
        var tab = this.items[id];
        if(tab && this.active != tab){
            tab.disable();
        }
    },
    
    /**
     * Enable a TabPanelItem that is disabled.
     * @param {String/Number} id The id or index of the TabPanelItem to enable.
     */
    enableTab : function(id){
        var tab = this.items[id];
        tab.enable();
    },
    
    /**
     * Activate a TabPanelItem. The currently active will be deactivated. 
     * @param {String/Number} id The id or index of the TabPanelItem to activate.
     */
    activate : function(id){
        var tab = this.items[id];
        if(tab == this.active){
            return tab;
        } 
        var e = {};
        this.fireEvent('beforetabchange', this, e, tab);
        if(e.cancel !== true && !tab.disabled){
            if(this.active){
                this.active.hide();
            }
            this.active = this.items[id];
            this.active.show();
            this.onTabChange.fireDirect(this, this.active);
        }
        return tab;
    },
    
    /**
     * Get the active TabPanelItem
     * @return {YAHOO.ext.TabPanelItem} The active TabPanelItem or null if none are active.
     */
    getActiveTab : function(){
        return this.active;
    },
    
    /**
     * Updates the tab body element to fit the height of the container element
     * for overflow scrolling
     * @param {Number} targetHeight (optional) Override the starting height from the elements height
     */
    syncHeight : function(targetHeight){
        var height = (targetHeight || this.el.getHeight())-this.el.getBorderWidth('tb')-this.el.getPadding('tb');
        var bm = this.bodyEl.getMargins();
        var newHeight = height-(this.stripWrap.getHeight()||0)-(bm.top+bm.bottom);
        this.bodyEl.setHeight(newHeight);
        return newHeight; 
    },
    
    onResize : function(){
        if(this.monitorResize){
            this.autoSizeTabs();
        }
    },

    /**
     * Disables tab resizing while tabs are being added (if resizeTabs is false this does nothing)
     */
    beginUpdate : function(){
        this.updating = true;    
    },
    
    /**
     * Stops an update and resizes the tabs (if resizeTabs is false this does nothing)
     */
    endUpdate : function(){
        this.updating = false;
        this.autoSizeTabs();  
    },
    
    /**
     * Manual call to resize the tabs (if resizeTabs is false this does nothing)
     */
    autoSizeTabs : function(){
        var count = this.items.length;
        var vcount = count - this.hiddenCount;
        if(!this.resizeTabs || count < 1 || vcount < 1 || this.updating) return;
        var w = Math.max(this.el.getWidth() - this.cpad, 10);
        var availWidth = Math.floor(w / vcount);
        var b = this.stripBody;
        if(b.getWidth() > w){
            var tabs = this.items;
            this.setTabWidth(Math.max(availWidth, this.minTabWidth));
            if(availWidth < this.minTabWidth){
                /*if(!this.sleft){    // incomplete scrolling code
                    this.createScrollButtons();
                }
                this.showScroll();
                this.stripClip.setWidth(w - (this.sleft.getWidth()+this.sright.getWidth()));*/
            }
        }else{
            if(this.currentTabWidth < this.preferredTabWidth){
                this.setTabWidth(Math.min(availWidth, this.preferredTabWidth));
            }
        }
    },
    
    /**
     * Returns the number of tabs
     * @return {Number}
     */
     getCount : function(){
         return this.items.length;  
     },
    
    /**
     * Resizes all the tabs to the passed width
     * @param {Number} The new width
     */
    setTabWidth : function(width){
        this.currentTabWidth = width;
        for(var i = 0, len = this.items.length; i < len; i++) {
        	if(!this.items[i].isHidden())this.items[i].setWidth(width);
        }
    },
    
    /**
     * Destroys this TabPanel
     * @param {Boolean} removeEl (optional) True to remove the element from the DOM as well
     */
    destroy : function(removeEl){
        YAHOO.ext.EventManager.removeResizeListener(this.onResize, this);
        for(var i = 0, len = this.items.length; i < len; i++){
            this.items[i].purgeListeners();
        }
        if(removeEl === true){
            this.el.update('');
            this.el.remove();
        }
    }
};

/**
* @class YAHOO.ext.TabPanelItem
* @extends YAHOO.ext.util.Observable
*/ 
YAHOO.ext.TabPanelItem = function(tabPanel, id, text, closable){
    /**
     * The TabPanel this TabPanelItem belongs to
     * @type YAHOO.ext.TabPanel
     */
    this.tabPanel = tabPanel;
    /**
     * The id for this TabPanelItem
     * @type String
     */
    this.id = id;
    /** @private */
    this.disabled = false;
    /** @private */
    this.text = text;
    /** @private */
    this.loaded = false;
    this.closable = closable;
    
    /** 
     * The body element for this TabPanelItem
     * @type YAHOO.ext.Element
     */
    this.bodyEl = getEl(tabPanel.createItemBody(tabPanel.bodyEl.dom, id));
    this.bodyEl.setVisibilityMode(YAHOO.ext.Element.VISIBILITY);
    this.bodyEl.setStyle('display', 'block');
    this.bodyEl.setStyle('zoom', '1');
    this.hideAction();
    
    var els = tabPanel.createStripElements(tabPanel.stripEl.dom, text, closable);
    /** @private */
    this.el = getEl(els.el, true);
    this.inner = getEl(els.inner, true);
    this.textEl = getEl(this.el.dom.firstChild.firstChild.firstChild, true);
    this.pnode = getEl(els.el.parentNode, true);
    this.el.mon('click', this.onTabClick, this, true);
    /** @private */
    if(closable){
        var c = getEl(els.close, true);
        c.dom.title = this.closeText;
        c.addClassOnOver('close-over');
        c.mon('click', this.closeClick, this, true);
     }
    
    // these two are now private and deprecated
    this.onActivate = new YAHOO.util.CustomEvent('TabItem.onActivate');
    this.onDeactivate = new YAHOO.util.CustomEvent('TabItem.onDeactivate');
    
    this.events = {
         /**
         * @event activate
         * Fires when this tab becomes the active tab
         * @param {YAHOO.ext.TabPanel} tabPanel
         * @param {YAHOO.ext.TabPanelItem} this
         */
        'activate': this.onActivate,
        /**
         * @event beforeclose
         * Fires before this tab is closed. To cancal the close, set cancel to true on e. (e.cancel = true)
         * @param {YAHOO.ext.TabPanelItem} this
         * @param {Object} e Set cancel to true on this object to cancel the close.
         */
        'beforeclose': new YAHOO.util.CustomEvent('beforeclose'),
        /**
         * @event close
         * Fires when this tab is closed
         * @param {YAHOO.ext.TabPanelItem} this
         */
         'close': new YAHOO.util.CustomEvent('close'),
        /**
         * @event deactivate
         * Fires when this tab is no longer the active tab
         * @param {YAHOO.ext.TabPanel} tabPanel
         * @param {YAHOO.ext.TabPanelItem} this
         */
         'deactivate' : this.onDeactivate  
    };
    this.hidden = false;
};

YAHOO.ext.TabPanelItem.prototype = {
    fireEvent : YAHOO.ext.util.Observable.prototype.fireEvent,
    on : YAHOO.ext.util.Observable.prototype.on,
    addListener : YAHOO.ext.util.Observable.prototype.addListener,
    delayedListener : YAHOO.ext.util.Observable.prototype.delayedListener,
    removeListener : YAHOO.ext.util.Observable.prototype.removeListener,
    purgeListeners : function(){
       YAHOO.ext.util.Observable.prototype.purgeListeners.call(this);
       this.el.removeAllListeners(); 
    },
    /**
     * Show this TabPanelItem - this <b>does not</b> deactivate the currently active TabPanelItem.
     */
    show : function(){
        this.pnode.addClass('on');
        this.showAction();
        if(YAHOO.ext.util.Browser.isOpera){
            this.tabPanel.stripWrap.repaint();
        }
        this.onActivate.fireDirect(this.tabPanel, this); 
    },
    
    /**
     * Returns true if this tab is the active tab
     * @return {Boolean}
     */
    isActive : function(){
        return this.tabPanel.getActiveTab() == this;  
    },
    
    /**
     * Hide this TabPanelItem - if you don't activate another TabPanelItem this could look odd.
     */
    hide : function(){
        this.pnode.removeClass('on');
        this.hideAction();
        this.onDeactivate.fireDirect(this.tabPanel, this); 
    },
    
    hideAction : function(){
        this.bodyEl.setStyle('position', 'absolute');
        this.bodyEl.setLeft('-20000px');
        this.bodyEl.setTop('-20000px');
        this.bodyEl.hide();
    },
    
    showAction : function(){
        this.bodyEl.setStyle('position', 'relative');
        this.bodyEl.setTop('');
        this.bodyEl.setLeft('');
        this.bodyEl.show();
        this.tabPanel.el.repaint.defer(1);
    },
    
    /**
     * Set the tooltip (title attribute) for the tab
     * @param {String} tooltip
     */
    setTooltip : function(text){
        this.textEl.dom.title = text;
    },
    
    onTabClick : function(e){
        e.preventDefault();
        this.tabPanel.activate(this.id);
    },
    
    getWidth : function(){
        return this.inner.getWidth();  
    },
    
    setWidth : function(width){
        var iwidth = width - this.pnode.getPadding("lr");
        this.inner.setWidth(iwidth);
        this.textEl.setWidth(iwidth-this.inner.getPadding('lr'));
        this.pnode.setWidth(width);
    },
    
    setHidden : function(hidden){
        this.hidden = hidden;
        this.pnode.setStyle('display', hidden ? 'none' : '');  
    },
    
    /**
     * Returns true if this tab is "hidden"
     * @return {Boolean}
     */
    isHidden : function(){
        return this.hidden;  
    },
    
    /**
     * Returns the text for this tab
     * @return {String}
     */
    getText : function(){
        return this.text;
    },
    
    autoSize : function(){
        this.el.beginMeasure();
        this.textEl.setWidth(1);
        this.setWidth(this.textEl.dom.scrollWidth+this.pnode.getPadding("lr")+this.inner.getPadding('lr'));
        this.el.endMeasure();
    },
    
    /**
     * Sets the text for the tab (Note: this also sets the tooltip)
     * @param {String} text
     */
    setText : function(text){
        this.text = text;
        this.textEl.update(text);
        this.textEl.dom.title = text;
        if(!this.tabPanel.resizeTabs){
            this.autoSize();
        }
    },
    /**
     * Activate this TabPanelItem - this <b>does</b> deactivate the currently active TabPanelItem.
     */
    activate : function(){
        this.tabPanel.activate(this.id);
    },
    
    /**
     * Disable this TabPanelItem - this call is ignore if this is the active TabPanelItem.
     */
    disable : function(){
        if(this.tabPanel.active != this){
            this.disabled = true;
            this.pnode.addClass('disabled');
        }
    },
    
    /**
     * Enable this TabPanelItem if it was previously disabled.
     */
    enable : function(){
        this.disabled = false;
        this.pnode.removeClass('disabled');
    },
    
    /**
     * Set the content for this TabPanelItem.
     * @param {String} content The content
     * @param {Boolean} loadScripts true to look for and load scripts
     */
    setContent : function(content, loadScripts){
        this.bodyEl.update(content, loadScripts);
    },
    
    /**
     * Get the {@link YAHOO.ext.UpdateManager} for the body of this TabPanelItem. Enables you to perform Ajax updates.
     * @return {YAHOO.ext.UpdateManager} The UpdateManager
     */
    getUpdateManager : function(){
        return this.bodyEl.getUpdateManager();
    },
    
    /**
     * Set a URL to be used to load the content for this TabPanelItem.
     * @param {String/Function} url The url to load the content from or a function to call to get the url
     * @param {<i>String/Object</i>} params (optional) The string params for the update call or an object of the params. See {@link YAHOO.ext.UpdateManager#update} for more details. (Defaults to null)
     * @param {<i>Boolean</i>} loadOnce (optional) Whether to only load the content once. If this is false it makes the Ajax call every time this TabPanelItem is activated. (Defaults to false)
     * @return {YAHOO.ext.UpdateManager} The UpdateManager
     */
    setUrl : function(url, params, loadOnce){
        if(this.refreshDelegate){
            this.onActivate.unsubscribe(this.refreshDelegate);
        }
        this.refreshDelegate = this._handleRefresh.createDelegate(this, [url, params, loadOnce]);
        this.onActivate.subscribe(this.refreshDelegate);
        return this.bodyEl.getUpdateManager();
    },
    
    /** @private */
    _handleRefresh : function(url, params, loadOnce){
        if(!loadOnce || !this.loaded){
            var updater = this.bodyEl.getUpdateManager();
            updater.update(url, params, this._setLoaded.createDelegate(this));
        }
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
    
    /** @private */
    _setLoaded : function(){
        this.loaded = true;
    },
    
    /** @private */
    closeClick : function(e){
        var e = {};
        this.fireEvent('beforeclose', this, e);
        if(e.cancel !== true){
            this.tabPanel.removeTab(this.id);
        }
    },
    /**
     * The text displayed in the tooltip for the close icon.
     * @type String
     */
    closeText : 'Close this tab'
};

/** @private */
YAHOO.ext.TabPanel.prototype.createStrip = function(container){
    var strip = document.createElement('div');
    strip.className = 'ytab-wrap';
    container.appendChild(strip);
    return strip;
};
/** @private */
YAHOO.ext.TabPanel.prototype.createStripList = function(strip){
    // div wrapper for retard IE
    strip.innerHTML = '<div class="ytab-strip-wrap"><table class="ytab-strip" cellspacing="0" cellpadding="0" border="0"><tbody><tr></tr></tbody></table></div>';
    return strip.firstChild.firstChild.firstChild.firstChild;
};
/** @private */
YAHOO.ext.TabPanel.prototype.createBody = function(container){
    var body = document.createElement('div');
    YAHOO.util.Dom.generateId(body, 'tab-body');
    YAHOO.util.Dom.addClass(body, 'yui-ext-tabbody');
    container.appendChild(body);
    return body;
};
/** @private */
YAHOO.ext.TabPanel.prototype.createItemBody = function(bodyEl, id){
    var body = YAHOO.util.Dom.get(id);
    if(!body){
        body = document.createElement('div');
        body.id = id;
    }
    YAHOO.util.Dom.addClass(body, 'yui-ext-tabitembody');
    bodyEl.insertBefore(body, bodyEl.firstChild);
    return body;
};
/** @private */
YAHOO.ext.TabPanel.prototype.createStripElements = function(stripEl, text, closable){
    var td = document.createElement('td');
    stripEl.appendChild(td);
    if(closable){
        td.className = "ytab-closable";
        if(!this.closeTpl){
            this.closeTpl = new YAHOO.ext.Template(
               '<a href="#" class="ytab-right"><span class="ytab-left"><em class="ytab-inner">' +
               '<span unselectable="on" title="{text}" class="ytab-text">{text}</span>' +
               '<div unselectable="on" class="close-icon">&#160;</div></em></span></a>'
            );
        }
        var el = this.closeTpl.overwrite(td, {'text': text});
        var close = el.getElementsByTagName('div')[0];
        var inner = el.getElementsByTagName('em')[0];
        return {'el': el, 'close': close, 'inner': inner};
    } else {
        if(!this.tabTpl){
            this.tabTpl = new YAHOO.ext.Template(
               '<a href="#" class="ytab-right"><span class="ytab-left"><em class="ytab-inner">' +
               '<span unselectable="on" title="{text}" class="ytab-text">{text}</span></em></span></a>'
            );
        }
        var el = this.tabTpl.overwrite(td, {'text': text});
        var inner = el.getElementsByTagName('em')[0];
        return {'el': el, 'inner': inner};
    }
};
