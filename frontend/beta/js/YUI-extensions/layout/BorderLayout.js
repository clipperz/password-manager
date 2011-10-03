/**
 * @class YAHOO.ext.BorderLayout
 * @extends YAHOO.ext.LayoutManager
 * This class represents a common layout manager used in desktop applications. For screenshots and more details,
 * please see: <br><br>
 * <a href="http://www.jackslocum.com/yui/2006/10/19/cross-browser-web-20-layouts-with-yahoo-ui/">Cross Browser Layouts - Part 1</a><br>
 * <a href="http://www.jackslocum.com/yui/2006/10/28/cross-browser-web-20-layouts-part-2-ajax-feed-viewer-20/">Cross Browser Layouts - Part 2</a><br><br>
 * Example:
 <pre><code>
 var layout = new YAHOO.ext.BorderLayout(document.body, {
    north: {
        initialSize: 25,
        titlebar: false
    },
    west: {
        split:true,
        initialSize: 200,
        minSize: 175,
        maxSize: 400,
        titlebar: true,
        collapsible: true
    },
    east: {
        split:true,
        initialSize: 202,
        minSize: 175,
        maxSize: 400,
        titlebar: true,
        collapsible: true
    },
    south: {
        split:true,
        initialSize: 100,
        minSize: 100,
        maxSize: 200,
        titlebar: true,
        collapsible: true
    },
    center: {
        titlebar: true,
        autoScroll:true,
        resizeTabs: true,
        minTabWidth: 50,
        preferredTabWidth: 150
    }
});

// shorthand
var CP = YAHOO.ext.ContentPanel;

layout.beginUpdate();
layout.add('north', new CP('north', 'North'));
layout.add('south', new CP('south', {title: 'South', closable: true}));
layout.add('west', new CP('west', {title: 'West'}));
layout.add('east', new CP('autoTabs', {title: 'Auto Tabs', closable: true}));
layout.add('center', new CP('center1', {title: 'Close Me', closable: true}));
layout.add('center', new CP('center2', {title: 'Center Panel', closable: false}));
layout.getRegion('center').showPanel('center1');
layout.endUpdate();
</code></pre>
* @constructor
* Create a new BorderLayout
* @param {String/HTMLElement/Element} container The container this layout is bound to
* @param {Object} config Configuration options
 */
YAHOO.ext.BorderLayout = function(container, config){
    config = config || {};
    YAHOO.ext.BorderLayout.superclass.constructor.call(this, container);
    this.factory = config.factory || YAHOO.ext.BorderLayout.RegionFactory;
    /** 
     * True to hide the center panel while performing layouts. This helps when the center region contains 
     * heavy components such as a yui-ext grid. 
     * @type Boolean
     */
    this.hideOnLayout = config.hideOnLayout || false;
    for(var i = 0, len = this.factory.validRegions.length; i < len; i++) {
    	var target = this.factory.validRegions[i];
    	if(config[target]){
    	    this.addRegion(target, config[target]);
    	}
    }
    //this.dragOverDelegate = YAHOO.ext.EventManager.wrap(this.onDragOver, this, true);
};

YAHOO.extendX(YAHOO.ext.BorderLayout, YAHOO.ext.LayoutManager, {
    /**
     * Creates and adds a new region if it doesn't already exist.
     * @param {String} target The target region key (north, south, east, west or center).
     * @param {Object} config The regions config object
     * @return {BorderLayoutRegion} The new region
     */
    addRegion : function(target, config){
        if(!this.regions[target]){
            var r = this.factory.create(target, this, config);
    	    this.regions[target] = r;
    	    r.on('visibilitychange', this.layout, this, true);
            r.on('paneladded', this.layout, this, true);
            r.on('panelremoved', this.layout, this, true);
            r.on('invalidated', this.layout, this, true);
            r.on('resized', this.onRegionResized, this, true);
            r.on('collapsed', this.onRegionCollapsed, this, true);
            r.on('expanded', this.onRegionExpanded, this, true);
        }
        return this.regions[target];
    },
    
    /**
     * Performs a layout update.
     */
    layout : function(){
        if(this.updating) return;
        //var bench = new YAHOO.ext.util.Bench();
	    //bench.start('Layout...');
        var size = this.getViewSize();
        var w = size.width, h = size.height;
        var centerW = w, centerH = h, centerY = 0, centerX = 0;
        var x = 0, y = 0;
        
        var rs = this.regions;
        var n = rs['north'], s = rs['south'], west = rs['west'], e = rs['east'], c = rs['center'];
        if(this.hideOnLayout){
            c.el.setStyle('display', 'none');
        }
        if(n && n.isVisible()){
            var b = n.getBox();
            var m = n.getMargins();
            b.width = w - (m.left+m.right);
            b.x = m.left;
            b.y = m.top;
            centerY = b.height + b.y + m.bottom;
            centerH -= centerY;
            n.updateBox(this.safeBox(b));
        }
        if(s && s.isVisible()){
            var b = s.getBox();
            var m = s.getMargins();
            b.width = w - (m.left+m.right);
            b.x = m.left;
            var totalHeight = (b.height + m.top + m.bottom);
            b.y = h - totalHeight + m.top;
            centerH -= totalHeight;
            s.updateBox(this.safeBox(b));
        }
        if(west && west.isVisible()){
            var b = west.getBox();
            var m = west.getMargins();
            b.height = centerH - (m.top+m.bottom);
            b.x = m.left;
            b.y = centerY + m.top;
            var totalWidth = (b.width + m.left + m.right);
            centerX += totalWidth;
            centerW -= totalWidth;
            west.updateBox(this.safeBox(b));
        }
        if(e && e.isVisible()){
            var b = e.getBox();
            var m = e.getMargins();
            b.height = centerH - (m.top+m.bottom);
            var totalWidth = (b.width + m.left + m.right);
            b.x = w - totalWidth + m.left;
            b.y = centerY + m.top;
            centerW -= totalWidth;
            e.updateBox(this.safeBox(b));
        }
        if(c){
            var m = c.getMargins();
            var centerBox = {
                x: centerX + m.left,
                y: centerY + m.top,
                width: centerW - (m.left+m.right),
                height: centerH - (m.top+m.bottom)
            };
            if(this.hideOnLayout){
                c.el.setStyle('display', 'block');
            }
            c.updateBox(this.safeBox(centerBox));
        }
        this.el.repaint();
        this.fireEvent('layout', this);
        //bench.stop();
	    //alert(bench.toString());
    },
    
    safeBox : function(box){
        box.width = Math.max(0, box.width);
        box.height = Math.max(0, box.height);
        return box;
    },
    
    /**
     * Adds a ContentPanel (or subclass) to this layout.
     * @param {String} target The target region key (north, south, east, west or center).
     * @param {YAHOO.ext.ContentPanel} panel The panel to add
     * @return {YAHOO.ext.ContentPanel} The added panel
     */
    add : function(target, panel){
        target = target.toLowerCase();
        return this.regions[target].add(panel);
    },
    
    /**
     * Adds a ContentPanel (or subclass) to this layout.
     * @param {String} target The target region key (north, south, east, west or center).
     * @param {Number/String/YAHOO.ext.ContentPanel} panel The index, id or panel to remove
     * @return {YAHOO.ext.ContentPanel} The removed panel
     */
    remove : function(target, panel){
        target = target.toLowerCase();
        return this.regions[target].remove(panel);
    },
    
    /**
     * Searches all regions for a panel with the specified id
     * @param {String} panelId
     * @return {YAHOO.ext.ContentPanel} The panel or null if it wasn't found
     */
    findPanel : function(panelId){
        var rs = this.regions;
        for(var target in rs){
            if(typeof rs[target] != 'function'){
                var p = rs[target].getPanel(panelId);
                if(p){
                    return p;
                }
            }
        }
        return null;
    },
    
    /**
     * Searches all regions for a panel with the specified id and activates (shows) it.
     * @param {String/ContentPanel} panelId The panels id or the panel itself
     * @return {YAHOO.ext.ContentPanel} The shown panel or null
     */
    showPanel : function(panelId) {
      var rs = this.regions;
      for(var target in rs){
         var r = rs[target];
         if(typeof r != 'function'){
            if(r.hasPanel(panelId)){
               return r.showPanel(panelId);
            }
         }
      }
      return null;
   },
   
   /**
     * Restores this layouts state using YAHOO.ext.state.Manager or the state provided by the passed provider.
     * @param {YAHOO.ext.state.Provider} provider (optional) An alternate state provider
     */
    restoreState : function(provider){
        if(!provider){
            provider = YAHOO.ext.state.Manager;
        }
        var sm = new YAHOO.ext.LayoutStateManager();
        sm.init(this, provider);
    }
});

YAHOO.ext.BorderLayout.RegionFactory = {};
YAHOO.ext.BorderLayout.RegionFactory.validRegions = ['north','south','east','west','center'];
YAHOO.ext.BorderLayout.RegionFactory.create = function(target, mgr, config){
    target = target.toLowerCase();
    if(config.lightweight || config.basic){
        return new YAHOO.ext.BasicLayoutRegion(mgr, config, target);
    }
    switch(target){
        case 'north':
            return new YAHOO.ext.NorthLayoutRegion(mgr, config);
        case 'south':
            return new YAHOO.ext.SouthLayoutRegion(mgr, config);
        case 'east':
            return new YAHOO.ext.EastLayoutRegion(mgr, config);
        case 'west':
            return new YAHOO.ext.WestLayoutRegion(mgr, config);
        case 'center':
            return new YAHOO.ext.CenterLayoutRegion(mgr, config);
    }
    throw 'Layout region "'+target+'" not supported.';
};
