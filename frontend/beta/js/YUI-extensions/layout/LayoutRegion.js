/**
 * @class YAHOO.ext.LayoutRegion
 * @extends YAHOO.ext.util.Observable
 * This class represents a region in a layout manager.
 * @cfg {Boolean} collapsible False to disable collapsing (defaults to true)
 * @cfg {Boolean} floatable False to disable floating (defaults to true)
 * @cfg {Object} margins Margins for the element (defaults to {top: 0, left: 0, right:0, bottom: 0})
 * @cfg {Object} cmargins Margins for the element when collapsed (defaults to: north/south {top: 2, left: 0, right:0, bottom: 2} or east/west {top: 0, left: 2, right:2, bottom: 0})
 * @cfg {String} tabPosition 'top' or 'bottom' (defaults to 'bottom')
 * @cfg {Boolean} alwaysShowTabs True to always display tabs even when only 1 panel (defaults to false)
 * @cfg {Boolean} autoScroll True to enable overflow scrolling (defaults to false)
 * @cfg {Boolean} titlebar True to display a title bar (defaults to true)
 * @cfg {String} title The title for the region (overrides panel titles)
 * @cfg {Boolean} animate True to animate expand/collapse (defaults to false)
 * @cfg {Float} duration The duration of the expand/collapse animation in seconds 
 * @cfg {Float} slideDuration The duration of the slide out/in when collapsed in seconds
 * @cfg {Boolean} autoHide False to disable disable autoHide when the mouse leaves the "floated" region (defaults to true)
 * @cfg {Boolean} preservePanels True to preserve removed panels so they can be readded later (defaults to false)
 * @cfg {Boolean} closeOnTabs True to place the close icon on the tabs instead of the region titlebar (defaults to false)
 * @cfg {Boolean} hideTabs True to hide the tab strip (defaults to false)
 * @cfg {Boolean} resizeTabs True to enable automatic tab resizing. This will resize the tabs so they are all the same size and fit within 
 * the space available, similar to FireFox 1.5 tabs (defaults to false)
 * @cfg {Number} minTabWidth The minimum tab width (defaults to 40)
 * @cfg {Number} preferredTabWidth The preferred tab width (defaults to 150)
 */
YAHOO.ext.LayoutRegion = function(mgr, config, pos){
    YAHOO.ext.LayoutRegion.superclass.constructor.call(this, mgr, config, pos, true);
    var dh = YAHOO.ext.DomHelper;
    /** This regions container element @type YAHOO.ext.Element */
    this.el = dh.append(mgr.el.dom, {tag: 'div', cls: 'ylayout-panel ylayout-panel-' + this.position}, true);
    /** This regions title element @type YAHOO.ext.Element */
    this.titleEl = dh.append(this.el.dom, {tag: 'div', unselectable: 'on', cls: 'yunselectable ylayout-panel-hd ylayout-title-'+this.position, children:[
        {tag: 'span', cls: 'yunselectable ylayout-panel-hd-text', unselectable: 'on', html: '&#160;'},
        {tag: 'div', cls: 'yunselectable ylayout-panel-hd-tools', unselectable: 'on'}
    ]}, true);
    this.titleEl.enableDisplayMode();
    /** This regions title text element @type HTMLElement */
    this.titleTextEl = this.titleEl.dom.firstChild;
    this.tools = getEl(this.titleEl.dom.childNodes[1], true);
    this.closeBtn = this.createTool(this.tools.dom, 'ylayout-close');
    this.closeBtn.enableDisplayMode();
    this.closeBtn.on('click', this.closeClicked, this, true);
    this.closeBtn.hide();
    /** This regions body element @type YAHOO.ext.Element */
    this.bodyEl = dh.append(this.el.dom, {tag: 'div', cls: 'ylayout-panel-body'}, true);
    this.visible = false;
    this.collapsed = false;
    this.hide();
    this.on('paneladded', this.validateVisibility, this, true);
    this.on('panelremoved', this.validateVisibility, this, true);
    
    this.applyConfig(config);
};

YAHOO.extendX(YAHOO.ext.LayoutRegion, YAHOO.ext.BasicLayoutRegion, {
    applyConfig : function(config){
        if(config.collapsible && this.position != 'center' && !this.collapsedEl){
            var dh = YAHOO.ext.DomHelper;
            this.collapseBtn = this.createTool(this.tools.dom, 'ylayout-collapse-'+this.position);
            this.collapseBtn.mon('click', this.collapse, this, true);
            /** This regions collapsed element @type YAHOO.ext.Element */
            this.collapsedEl = dh.append(this.mgr.el.dom, {tag: 'div', cls: 'ylayout-collapsed ylayout-collapsed-'+this.position, children:[
                {tag: 'div', cls: 'ylayout-collapsed-tools'}
            ]}, true);
            if(config.floatable !== false){
               this.collapsedEl.addClassOnOver('ylayout-collapsed-over');
               this.collapsedEl.mon('click', this.collapseClick, this, true);
            }
            this.expandBtn = this.createTool(this.collapsedEl.dom.firstChild, 'ylayout-expand-'+this.position);
            this.expandBtn.mon('click', this.expand, this, true);
        }
        if(this.collapseBtn){
            this.collapseBtn.setVisible(config.collapsible == true);
        }
        this.cmargins = config.cmargins || this.cmargins || 
                         (this.position == 'west' || this.position == 'east' ? 
                             {top: 0, left: 2, right:2, bottom: 0} : 
                             {top: 2, left: 0, right:0, bottom: 2});
        this.margins = config.margins || this.margins || {top: 0, left: 0, right:0, bottom: 0};
        this.bottomTabs = config.tabPosition != 'top';
        this.autoScroll = config.autoScroll || false;
        if(this.autoScroll){
            this.bodyEl.setStyle('overflow', 'auto');
        }else{
            this.bodyEl.setStyle('overflow', 'hidden');
        }
        if((!config.titlebar && !config.title) || config.titlebar === false){
            this.titleEl.hide();
        }else{
            this.titleEl.show();
            if(config.title){
                this.titleTextEl.innerHTML = config.title;
            }
        }
        this.duration = config.duration || .30;
        this.slideDuration = config.slideDuration || .45;
        this.config = config;
        if(config.collapsed){
            this.collapse(true);
        }
    },
    /**
     * Returns true if this region is currently visible.
     * @return {Boolean}
     */
    isVisible : function(){
        return this.visible;
    },
    
    getBox : function(){
        var b;
        if(!this.collapsed){
            b = this.el.getBox(false, true);
        }else{
            b = this.collapsedEl.getBox(false, true);
        }
        return b;
    },
    
    getMargins : function(){
        return this.collapsed ? this.cmargins : this.margins;
    },
    
    highlight : function(){
        this.el.addClass('ylayout-panel-dragover'); 
    },
    
    unhighlight : function(){
        this.el.removeClass('ylayout-panel-dragover'); 
    },
    
    updateBox : function(box){
        this.box = box;
        if(!this.collapsed){
            this.el.dom.style.left = box.x + 'px';
            this.el.dom.style.top = box.y + 'px';
            this.el.setSize(box.width, box.height);
            var bodyHeight = this.titleEl.isVisible() ? box.height - (this.titleEl.getHeight()||0) : box.height;
            bodyHeight -= this.el.getBorderWidth('tb');
            bodyWidth = box.width - this.el.getBorderWidth('rl');
            this.bodyEl.setHeight(bodyHeight);
            this.bodyEl.setWidth(bodyWidth);
            var tabHeight = bodyHeight;
            if(this.tabs){
                tabHeight = this.tabs.syncHeight(bodyHeight);
                if(YAHOO.ext.util.Browser.isIE) this.tabs.el.repaint();
            }
            this.panelSize = {width: bodyWidth, height: tabHeight};
            if(this.activePanel){
                this.activePanel.setSize(bodyWidth, tabHeight);
            }
        }else{
            this.collapsedEl.dom.style.left = box.x + 'px';
            this.collapsedEl.dom.style.top = box.y + 'px';
            this.collapsedEl.setSize(box.width, box.height);
        }
        if(this.tabs){
            this.tabs.autoSizeTabs();
        }
    },
    
    /**
     * Returns the container element for this region.
     * @return {YAHOO.ext.Element}
     */
    getEl : function(){
        return this.el;
    },
    
    /**
     * Hides this region.
     */
    hide : function(){
        if(!this.collapsed){
            this.el.dom.style.left = '-2000px';
            this.el.hide();
        }else{
            this.collapsedEl.dom.style.left = '-2000px';
            this.collapsedEl.hide();
        }
        this.visible = false;
        this.fireEvent('visibilitychange', this, false);
    },
    
    /**
     * Shows this region if it was previously hidden.
     */
    show : function(){
        if(!this.collapsed){
            this.el.show();
        }else{
            this.collapsedEl.show();
        }
        this.visible = true;
        this.fireEvent('visibilitychange', this, true);
    },
    
    closeClicked : function(){
        if(this.activePanel){
            this.remove(this.activePanel);
        }
    },
    
    collapseClick : function(e){
       if(this.isSlid){
           e.stopPropagation();
           this.slideIn();
       }else{
           e.stopPropagation();
           this.slideOut();
       }   
    },
    
    /**
     * Collapses this region.
     * @param {Boolean} skipAnim (optional) true to collapse the element without animation (if animate is true)
     */
    collapse : function(skipAnim){
        if(this.collapsed) return;
        this.collapsed = true;
        if(this.split){
            this.split.el.hide();
        }
        if(this.config.animate && skipAnim !== true){
            this.fireEvent('invalidated', this);    
            this.animateCollapse();
        }else{
            this.el.setLocation(-20000,-20000);
            this.el.hide();
            this.collapsedEl.show();
            this.fireEvent('collapsed', this);
            this.fireEvent('invalidated', this); 
        }
    },
    
    animateCollapse : function(){
        // overridden
    },
    
    /**
     * Expand this region if it was previously collapsed.
     * @param {YAHOO.ext.EventObject} e The event that triggered the expand (or null if calling manually)
     * @param {Boolean} skipAnim (optional) true to expand the element without animation (if animate is true)
     */
    expand : function(e, skipAnim){
        if(e) e.stopPropagation();
        if(!this.collapsed) return;
        if(this.isSlid){
            this.slideIn(this.expand.createDelegate(this));
            return;
        }
        this.collapsed = false;
        this.el.show();
        if(this.config.animate && skipAnim !== true){
            this.animateExpand();
        }else{
            if(this.split){
                this.split.el.show();
            }
            this.collapsedEl.setLocation(-2000,-2000);
            this.collapsedEl.hide();
            this.fireEvent('invalidated', this);
            this.fireEvent('expanded', this);
        }
    },
    
    animateExpand : function(){
        // overridden
    },
    
    initTabs : function(){
        this.bodyEl.setStyle('overflow', 'hidden');
        var ts = new YAHOO.ext.TabPanel(this.bodyEl.dom, this.bottomTabs);
        if(this.config.hideTabs){
            ts.stripWrap.setDisplayed(false);
        }
        this.tabs = ts;
        ts.resizeTabs = this.config.resizeTabs === true;
        ts.minTabWidth = this.config.minTabWidth || 40;
        ts.maxTabWidth = this.config.maxTabWidth || 250;
        ts.preferredTabWidth = this.config.preferredTabWidth || 150;
        ts.monitorResize = false;
        ts.bodyEl.setStyle('overflow', this.config.autoScroll ? 'auto' : 'hidden');
        this.panels.each(this.initPanelAsTab, this);
    },
    
    initPanelAsTab : function(panel){
        var ti = this.tabs.addTab(panel.getEl().id, panel.getTitle(), null, 
                    this.config.closeOnTab && panel.isClosable());
        ti.on('activate', function(){ 
              this.setActivePanel(panel); 
        }, this, true);
        if(this.config.closeOnTab){
            ti.on('beforeclose', function(t, e){
                e.cancel = true;
                this.remove(panel);
            }, this, true);
        }
        return ti;
    },
    
    updatePanelTitle : function(panel, title){
        if(this.activePanel == panel){
            this.updateTitle(title);
        }
        if(this.tabs){
            this.tabs.getTab(panel.getEl().id).setText(title);
        }
    },
    
    updateTitle : function(title){
        if(this.titleTextEl && !this.config.title){
            this.titleTextEl.innerHTML = (typeof title != 'undefined' && title.length > 0 ? title : "&#160;");
        }
    },
    
    setActivePanel : function(panel){
        panel = this.getPanel(panel);
        if(this.activePanel && this.activePanel != panel){
            this.activePanel.setActiveState(false);
        }
        this.activePanel = panel;
        panel.setActiveState(true);
        if(this.panelSize){
            panel.setSize(this.panelSize.width, this.panelSize.height);
        }
        this.closeBtn.setVisible(!this.config.closeOnTab && !this.isSlid && panel.isClosable());
        this.updateTitle(panel.getTitle());
        this.fireEvent('panelactivated', this, panel);
    },
    
    /**
     * Show the specified panel.
     * @param {Number/String/ContentPanel} panelId The panels index, id or the panel itself
     * @return {YAHOO.ext.ContentPanel} The shown panel or null
     */
    showPanel : function(panel){
        if(panel = this.getPanel(panel)){
            if(this.tabs){
                this.tabs.activate(panel.getEl().id);
            }else{
                this.setActivePanel(panel);
            }
        }
        return panel;
    },
    
    /**
     * Get the active panel for this region.
     * @return {YAHOO.ext.ContentPanel} The active panel or null
     */
    getActivePanel : function(){
        return this.activePanel;
    },
    
    validateVisibility : function(){
        if(this.panels.getCount() < 1){
            this.updateTitle('&#160;');
            this.closeBtn.hide();
            this.hide();
        }else{
            if(!this.isVisible()){
                this.show();
            }
        }
    },
    
    /**
     * Add the passed ContentPanel(s)
     * @param {ContentPanel...} panel The ContentPanel(s) to add (you can pass more than one)
     * @return {YAHOO.ext.ContentPanel} The panel added (if only one was added)
     */
    add : function(panel){
        if(arguments.length > 1){
            for(var i = 0, len = arguments.length; i < len; i++) {
            	this.add(arguments[i]);
            }
            return null;
        }
        if(this.hasPanel(panel)){
            this.showPanel(panel);
            return panel;
        }
        panel.setRegion(this);
        this.panels.add(panel);
        if(this.panels.getCount() == 1 && !this.config.alwaysShowTabs){
            this.bodyEl.dom.appendChild(panel.getEl().dom);
            if(panel.background !== true){
                this.setActivePanel(panel);
            }
            this.fireEvent('paneladded', this, panel);
            return panel;
        }
        if(!this.tabs){
            this.initTabs();
        }else{
            this.initPanelAsTab(panel);
        }
        if(panel.background !== true){
            this.tabs.activate(panel.getEl().id);
        }
        this.fireEvent('paneladded', this, panel);
        return panel;
    },
    
    /**
     * Hides the tab for the specified panel.
     * @param {Number/String/ContentPanel} panel The panels index, id or the panel itself
     */
    hidePanel : function(panel){
        if(this.tabs && (panel = this.getPanel(panel))){
            this.tabs.hideTab(panel.getEl().id);
        }
    },
    
    /**
     * Unhides the tab for a previously hidden panel.
     * @param {Number/String/ContentPanel} panel The panels index, id or the panel itself
     */
    unhidePanel : function(panel){
        if(this.tabs && (panel = this.getPanel(panel))){
            this.tabs.unhideTab(panel.getEl().id);
        }
    },
    
    clearPanels : function(){
        while(this.panels.getCount() > 0){
             this.remove(this.panels.first());
        }
    },
    
    /**
     * Removes the specified panel. If preservePanel is not true (either here or in the config), the panel is destroyed.
     * @param {Number/String/ContentPanel} panel The panels index, id or the panel itself
     * @param {Boolean} preservePanel Overrides the config preservePanel option
     * @return {YAHOO.ext.ContentPanel} The panel that was removed
     */
    remove : function(panel, preservePanel){
        panel = this.getPanel(panel);
        if(!panel){
            return null;
        }
        var e = {};
        this.fireEvent('beforeremove', this, panel, e);
        if(e.cancel === true){
            return null;
        }
        preservePanel = (typeof preservePanel != 'undefined' ? preservePanel : (this.config.preservePanels === true || panel.preserve === true));
        var panelId = panel.getId();
        this.panels.removeKey(panelId);
        if(preservePanel){
            document.body.appendChild(panel.getEl().dom);
        }
        if(this.tabs){
            this.tabs.removeTab(panel.getEl().id);
        }else if (!preservePanel){
            this.bodyEl.dom.removeChild(panel.getEl().dom);
        }
        if(this.panels.getCount() == 1 && this.tabs && !this.config.alwaysShowTabs){
            var p = this.panels.first();
            var tempEl = document.createElement('span'); // temp holder to keep IE from deleting the node
            tempEl.appendChild(p.getEl().dom);
            this.bodyEl.update('');
            this.bodyEl.dom.appendChild(p.getEl().dom);
            tempEl = null;
            this.updateTitle(p.getTitle());
            this.tabs = null;
            this.bodyEl.setStyle('overflow', this.config.autoScroll ? 'auto' : 'hidden');
            this.setActivePanel(p);
        }
        panel.setRegion(null);
        if(this.activePanel == panel){
            this.activePanel = null;
        }
        if(this.config.autoDestroy !== false && preservePanel !== true){
            try{panel.destroy();}catch(e){}
        }
        this.fireEvent('panelremoved', this, panel);
        return panel;
    },
    
    /**
     * Returns the TabPanel component used by this region
     * @return {YAHOO.ext.TabPanel}
     */
    getTabs : function(){
        return this.tabs;    
    },
    
    createTool : function(parentEl, className){
        var btn = YAHOO.ext.DomHelper.append(parentEl, {tag: 'div', cls: 'ylayout-tools-button', 
            children: [{tag: 'div', cls: 'ylayout-tools-button-inner ' + className, html: '&#160;'}]}, true);
        btn.addClassOnOver('ylayout-tools-button-over');
        return btn;
    }
});
