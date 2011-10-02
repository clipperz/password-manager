/**
 * @class YAHOO.ext.Resizable
 * @extends YAHOO.ext.util.Observable
 * <p>Applies drag handles to an element to make it resizable. The drag handles are inserted into the element 
 * and positioned absolute. Some elements, such as a textarea or image, don't support this. To overcome that, you can wrap
 * the textarea in a div and set "resizeChild" to true (or the id of the textarea), <b>or</b> set wrap:true in your config and
 * the element will be wrapped for you automatically.</p><br/>
 * Here's a Resizable with every possible config option and it's default value:
<pre><code>
var resizer = new YAHOO.ext.Resizable('element-id', {
    resizeChild : false,
    adjustments : [0, 0],
    minWidth : 5,
    minHeight : 5,
    maxWidth : 10000,
    maxHeight : 10000,
    enabled : true,
    wrap: false, // true to wrap the element
    width: null, // initial size
    height: null, // initial size
    animate : false,
    duration : .35,
    dynamic : false,
    handles : false,
    multiDirectional : false,
    disableTrackOver : false,
    easing : YAHOO.util.Easing ? YAHOO.util.Easing.easeOutStrong : null,
    widthIncrement : 0,
    heightIncrement : 0,
    pinned : false,
    width : null,
    height : null,
    preserveRatio : false,
    transparent: false,
    minX: 0,
    minY: 0,
    draggable: false
});
resizer.on('resize', myHandler);
</code></pre>
* <p>
 * To hide a particular handle, set it's display to none in CSS, or through script:<br>
 * resizer.east.setDisplayed(false);
 * </p>
 * @constructor
 * Create a new resizable component
 * @param {String/HTMLElement/YAHOO.ext.Element} el The id or element to resize
 * @param {Object} config configuration options
  */
YAHOO.ext.Resizable = function(el, config){
    this.el = getEl(el);
    
    if(config && config.wrap){
        config.resizeChild = this.el;
        this.el = this.el.wrap(typeof config.wrap == 'object' ? config.wrap : null);
        this.el.id = this.el.dom.id = config.resizeChild.id + '-rzwrap';
        this.el.setStyle('overflow', 'hidden');
        this.el.setPositioning(config.resizeChild.getPositioning());
        config.resizeChild.clearPositioning();
        if(!config.width || !config.height){
            var csize = config.resizeChild.getSize();
            //csize.width -= config.adjustments[0];
            //csize.height -= config.adjustments[1];
            this.el.setSize(csize.width, csize.height);
        }
        if(config.pinned && !config.adjustments){
            config.adjustments = 'auto';
        }
    }
    
    this.proxy = this.el.createProxy({tag: 'div', cls: 'yresizable-proxy', id: this.el.id + '-rzproxy'})
    this.proxy.unselectable();
    
    // the overlay traps mouse events while dragging and fixes iframe issue
    this.overlay = this.el.createProxy({tag: 'div', cls: 'yresizable-overlay', html: '&#160;'});
    this.overlay.unselectable();
    this.overlay.enableDisplayMode('block');
    this.overlay.mon('mousemove', this.onMouseMove, this, true);
    this.overlay.mon('mouseup', this.onMouseUp, this, true);
    
    YAHOO.ext.util.Config.apply(this, config, {
        /** True to resizeSize the first child or id/element to resize @type YAHOO.ext.Element */
        resizeChild : false,
        /** String "auto" or an array [width, height] with values to be <b>added</b> to the resize operation's new size. @type Array/String */
        adjustments : [0, 0],
        /** The minimum width for the element @type Number */
        minWidth : 5,
        /** The minimum height for the element @type Number */
        minHeight : 5,
        /** The maximum width for the element @type Number */
        maxWidth : 10000,
        /** The maximum height for the element @type Number */
        maxHeight : 10000,
        /** false to disable resizing @type Boolean */
        enabled : true,
        /** True to animate the resize (not compatible with dynamic sizing) @type Boolean */
        animate : false,
        /** Animation duration @type Float */
        duration : .35,
        /** True to resize the element while dragging instead of using a proxy @type Boolean */
        dynamic : false,
        // these 3 are only available at config time
        /** String consisting of the resize handles to display. Valid handles are 
         * n (north), s (south) e (east), w (west), ne (northeast), nw (northwest), se (southeast), sw (southwest) 
         * and all (which applies them all). If this is blank it defaults to "e,s,se". Handles can be delimited using
         * a space, comma or semi-colon. This is only applied at config time.  @type String*/
        handles : false,
        multiDirectional : false,
        /** true to disable mouse tracking. This is only applied at config time.  @type Boolean*/
        disableTrackOver : false,
        /** Animation easing @type YAHOO.util.Easing */
        easing : YAHOO.util.Easing ? YAHOO.util.Easing.easeOutStrong : null,
        /** The increment to snap the width resize in pixels (dynamic must be true) @type Number */
        widthIncrement : 0,
        /** The increment to snap the height resize in pixels (dynamic must be true) @type Number */
        heightIncrement : 0,
        /** true to pin the resize handles. This is only applied at config time.  @type Boolean*/
        pinned : false,
        /** The initial width for the element @type Number */
        width : null,
        /** The initial height for the element @type Number */
        height : null,
        /** true to preserve the initial size ratio.  @type Boolean*/
        preserveRatio : false,
        /** true for transparent handles. This is only applied at config time.  @type Boolean*/
        transparent: false,
        /** The minimum allowed page X for the element (only used for west resizing, defaults to 0) @type Number */
        minX: 0,
        /** The minimum allowed page Y for the element (only used for north resizing, defaults to 0) @type Number */
        minY: 0,
        /** convenience to initialize drag drop.  @type Boolean*/
        draggable: false
    });
    
    if(this.pinned){
        this.disableTrackOver = true;
        this.el.addClass('yresizable-pinned');    
    }
    // if the element isn't positioned, make it relative
    var position = this.el.getStyle('position');
    if(position != 'absolute' && position != 'fixed'){
        this.el.setStyle('position', 'relative');
    }
    if(!this.handles){ // no handles passed, must be legacy style
        this.handles = 's,e,se';
        if(this.multiDirectional){
            this.handles += ',n,w';
        }
    }
    if(this.handles == 'all'){
        this.handles = 'n s e w ne nw se sw';
    }
    var hs = this.handles.split(/\s*?[,;]\s*?| /);
    var ps = YAHOO.ext.Resizable.positions;
    for(var i = 0, len = hs.length; i < len; i++){
        if(hs[i] && ps[hs[i]]){
            var pos = ps[hs[i]];
            this[pos] = new YAHOO.ext.Resizable.Handle(this, pos, this.disableTrackOver, this.transparent);
        }
    }
    // legacy
    this.corner = this.southeast;
    
    this.activeHandle = null;
    
    if(this.resizeChild){
        if(typeof this.resizeChild == 'boolean'){
            this.resizeChild = YAHOO.ext.Element.get(this.el.dom.firstChild, true);
        }else{
            this.resizeChild = YAHOO.ext.Element.get(this.resizeChild, true);
        }
    }
    
    if(this.adjustments == 'auto'){
        var rc = this.resizeChild;
        var hw = this.west, he = this.east, hn = this.north, hs = this.south;
        if(rc && (hw || hn)){
            rc.setRelativePositioned();
            rc.setLeft(hw ? hw.el.getWidth() : 0);
            rc.setTop(hn ? hn.el.getHeight() : 0);
        }
        this.adjustments = [
            (he ? -he.el.getWidth() : 0) + (hw ? -hw.el.getWidth() : 0),
            (hn ? -hn.el.getHeight() : 0) + (hs ? -hs.el.getHeight() : 0) -1 
        ];
    }
    
    if(this.draggable){
        this.dd = this.dynamic ? 
            this.el.initDD(null) : this.el.initDDProxy(null, {dragElId: this.proxy.id});
        this.dd.setHandleElId(this.resizeChild ? this.resizeChild.id : this.el.id);
    }
    
    // public events
    this.events = {
        /**
         * @event beforeresize
         * Fired before resize is allowed. Set enabled to false to cancel resize. 
         * @param {YAHOO.ext.Resizable} this
         * @param {YAHOO.ext.EventObject} e The mousedown event
         */
        'beforeresize' : new YAHOO.util.CustomEvent(),
        /**
         * @event resize
         * Fired after a resize. 
         * @param {YAHOO.ext.Resizable} this
         * @param {Number} width The new width
         * @param {Number} height The new height
         * @param {YAHOO.ext.EventObject} e The mouseup event
         */
        'resize' : new YAHOO.util.CustomEvent()
    };
    
    if(this.width !== null && this.height !== null){
        this.resizeTo(this.width, this.height);
    }else{
        this.updateChildSize();
    }
};

YAHOO.extendX(YAHOO.ext.Resizable, YAHOO.ext.util.Observable, {
    /**
     * Perform a manual resize
     * @param {Number} width
     * @param {Number} height
     */
    resizeTo : function(width, height){
        this.el.setSize(width, height);
        this.updateChildSize();
        this.fireEvent('resize', this, width, height, null);
    },
    
    startSizing : function(e){
        this.fireEvent('beforeresize', this, e);
        if(this.enabled){ // 2nd enabled check in case disabled before beforeresize handler
            this.resizing = true;
            this.startBox = this.el.getBox();
            this.startPoint = e.getXY();
            this.offsets = [(this.startBox.x + this.startBox.width) - this.startPoint[0],
                            (this.startBox.y + this.startBox.height) - this.startPoint[1]];
            this.proxy.setBox(this.startBox);
            
            this.overlay.setSize(YAHOO.util.Dom.getDocumentWidth(), YAHOO.util.Dom.getDocumentHeight());
            this.overlay.show();
            
            if(!this.dynamic){
                this.proxy.show();
            }
        }
    },
    
    onMouseDown : function(handle, e){
        if(this.enabled){
            e.stopEvent();
            this.activeHandle = handle;
            this.overlay.setStyle('cursor', handle.el.getStyle('cursor'));
            this.startSizing(e);
        }          
    },
    
    onMouseUp : function(e){
        var size = this.resizeElement();
        this.resizing = false;
        this.handleOut();
        this.overlay.hide();
        this.fireEvent('resize', this, size.width, size.height, e);
    },
    
    updateChildSize : function(){
        if(this.resizeChild){
            var el = this.el;
            var child = this.resizeChild;
            var adj = this.adjustments;
            if(el.dom.offsetWidth){
                var b = el.getSize(true);
                child.setSize(b.width+adj[0], b.height+adj[1]);
            }
            // Second call here for IE
            // The first call enables instant resizing and
            // the second call corrects scroll bars if they
            // exist
            if(YAHOO.ext.util.Browser.isIE){
                setTimeout(function(){
                    if(el.dom.offsetWidth){
                        var b = el.getSize(true);
                        child.setSize(b.width+adj[0], b.height+adj[1]);
                    }
                }, 10);
            }
        }
    },
    
    snap : function(value, inc, min){
        if(!inc || !value) return value;
        var newValue = value;
        var m = value % inc;
        if(m > 0){
            if(m > (inc/2)){
                newValue = value + (inc-m);
            }else{
                newValue = value - m;
            }
        }
        return Math.max(min, newValue);
    },
    
    resizeElement : function(){
        var box = this.proxy.getBox();
        //box.width = this.snap(box.width, this.widthIncrement);
        //box.height = this.snap(box.height, this.heightIncrement);
        //if(this.multiDirectional){
            this.el.setBox(box, false, this.animate, this.duration, null, this.easing);
        //}else{
        //    this.el.setSize(box.width, box.height, this.animate, this.duration, null, this.easing);
        //}
        this.updateChildSize();
        this.proxy.hide();
        return box;
    },
    
    constrain : function(v, diff, m, mx){
        if(v - diff < m){
            diff = v - m;    
        }else if(v - diff > mx){
            diff = mx - v; 
        }
        return diff;                
    },
    
    onMouseMove : function(e){
        if(this.enabled){
            try{// try catch so if something goes wrong the user doesn't get hung
            
            //var curXY = this.startPoint;
            var curSize = this.curSize || this.startBox;
            var x = this.startBox.x, y = this.startBox.y;
            var ox = x, oy = y;
            var w = curSize.width, h = curSize.height;
            var ow = w, oh = h;
            var mw = this.minWidth, mh = this.minHeight;
            var mxw = this.maxWidth, mxh = this.maxHeight;
            var wi = this.widthIncrement;
            var hi = this.heightIncrement;
            
            var eventXY = e.getXY();
            var diffX = -(this.startPoint[0] - Math.max(this.minX, eventXY[0]));
            var diffY = -(this.startPoint[1] - Math.max(this.minY, eventXY[1]));
            
            var pos = this.activeHandle.position;
            
            switch(pos){
                case 'east':
                    w += diffX; 
                    w = Math.min(Math.max(mw, w), mxw);
                    break;
                case 'south':
                    h += diffY;
                    h = Math.min(Math.max(mh, h), mxh);
                    break;
                case 'southeast':
                    w += diffX; 
                    h += diffY;
                    w = Math.min(Math.max(mw, w), mxw);
                    h = Math.min(Math.max(mh, h), mxh);
                    break;
                case 'north':
                    diffY = this.constrain(h, diffY, mh, mxh);
                    y += diffY;
                    h -= diffY;
                    break;
                case 'west':
                    diffX = this.constrain(w, diffX, mw, mxw);
                    x += diffX;
                    w -= diffX;
                    break;
                case 'northeast':
                    w += diffX; 
                    w = Math.min(Math.max(mw, w), mxw);
                    diffY = this.constrain(h, diffY, mh, mxh);
                    y += diffY;
                    h -= diffY;
                    break;
                case 'northwest':
                    diffX = this.constrain(w, diffX, mw, mxw);
                    diffY = this.constrain(h, diffY, mh, mxh);
                    y += diffY;
                    h -= diffY;
                    x += diffX;
                    w -= diffX;
                    break;
               case 'southwest':
                    diffX = this.constrain(w, diffX, mw, mxw);
                    h += diffY;
                    h = Math.min(Math.max(mh, h), mxh);
                    x += diffX;
                    w -= diffX;
                    break;
            }
            
            var sw = this.snap(w, wi, mw);
            var sh = this.snap(h, hi, mh);
            if(sw != w || sh != h){
                switch(pos){
                    case 'northeast':
                        y -= sh - h;
                    break;
                    case 'north':
                        y -= sh - h;
                        break;
                    case 'southwest':
                        x -= sw - w;
                    break;
                    case 'west':
                        x -= sw - w;
                        break;
                    case 'northwest':
                        x -= sw - w;
                        y -= sh - h;
                    break;
                }
                w = sw;
                h = sh;
            }
            
            if(this.preserveRatio){
                switch(pos){
                    case 'southeast':
                    case 'east':
                        h = oh * (w/ow);
                        h = Math.min(Math.max(mh, h), mxh);
                        w = ow * (h/oh);
                       break;
                    case 'south':
                        w = ow * (h/oh);
                        w = Math.min(Math.max(mw, w), mxw);
                        h = oh * (w/ow);
                        break;
                    case 'northeast':
                        w = ow * (h/oh);
                        w = Math.min(Math.max(mw, w), mxw);
                        h = oh * (w/ow);
                    break;
                    case 'north':
                        var tw = w;
                        w = ow * (h/oh);
                        w = Math.min(Math.max(mw, w), mxw);
                        h = oh * (w/ow);
                        x += (tw - w) / 2;
                        break;
                    case 'southwest':
                        h = oh * (w/ow);
                        h = Math.min(Math.max(mh, h), mxh);
                        var tw = w;
                        w = ow * (h/oh);
                        x += tw - w;
                        break;
                    case 'west':
                        var th = h;
                        h = oh * (w/ow);
                        h = Math.min(Math.max(mh, h), mxh);
                        y += (th - h) / 2;
                        var tw = w;
                        w = ow * (h/oh);
                        x += tw - w;
                       break;
                    case 'northwest':
                        var tw = w;
                        var th = h;
                        h = oh * (w/ow);
                        h = Math.min(Math.max(mh, h), mxh);
                        w = ow * (h/oh);
                        y += th - h;
                         x += tw - w;
                       break;
                        
                }
            }
            this.proxy.setBounds(x, y, w, h);
            if(this.dynamic){
                this.resizeElement();
            }
            }catch(e){}
        }
    },
    
    handleOver : function(){
        if(this.enabled){
            this.el.addClass('yresizable-over');
        }
    },
    
    handleOut : function(){
        if(!this.resizing){
            this.el.removeClass('yresizable-over');
        }
    },
    
    /**
     * Returns the element this component is bound to.
     * @return {YAHOO.ext.Element}
     */
    getEl : function(){
        return this.el;
    },
    
    /**
     * Returns the resizeChild element (or null).
     * @return {YAHOO.ext.Element}
     */
    getResizeChild : function(){
        return this.resizeChild;
    },
    
    /**
     * Destroys this resizable. If the element was wrapped and 
     * removeEl is not true then the wrap remains.
     * @param {Boolean} removeEl (optional) true to remove the element from the DOM
     */
    destroy : function(removeEl){
        this.proxy.remove();
        this.overlay.removeAllListeners();
        this.overlay.remove();
        var ps = YAHOO.ext.Resizable.positions;
        for(var k in ps){
            if(typeof ps[k] != 'function' && this[ps[k]]){
                var h = this[ps[k]];
                h.el.removeAllListeners();
                h.el.remove();
            }
        }
        if(removeEl){
            this.el.update('');
            this.el.remove();
        }
    }
});

// hash to map config positions to true positions
YAHOO.ext.Resizable.positions = {
    n: 'north', s: 'south', e: 'east', w: 'west', se: 'southeast', sw: 'southwest', nw: 'northwest', ne: 'northeast' 
};


YAHOO.ext.Resizable.Handle = function(rz, pos, disableTrackOver, transparent){
    if(!this.tpl){
        // only initialize the template if resizable is used
        var tpl = YAHOO.ext.DomHelper.createTemplate(
            {tag: 'div', cls: 'yresizable-handle yresizable-handle-{0}', html: '&#160;'}
        );
        tpl.compile();
        YAHOO.ext.Resizable.Handle.prototype.tpl = tpl;
    }
    this.position = pos;
    this.rz = rz;
    this.el = this.tpl.append(rz.el.dom, [this.position], true);
    this.el.unselectable();
    if(transparent){
        this.el.setOpacity(0);
    }
    this.el.mon('mousedown', this.onMouseDown, this, true);
    if(!disableTrackOver){
        this.el.mon('mouseover', this.onMouseOver, this, true);
        this.el.mon('mouseout', this.onMouseOut, this, true);
    }
};

YAHOO.ext.Resizable.Handle.prototype = {
    afterResize : function(rz){
        // do nothing    
    },
    
    onMouseDown : function(e){
        this.rz.onMouseDown(this, e);
    },
    
    onMouseOver : function(e){
        this.rz.handleOver(this, e);
    },
    
    onMouseOut : function(e){
        this.rz.handleOut(this, e);
    }  
};



