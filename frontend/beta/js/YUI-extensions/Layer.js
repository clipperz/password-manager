/**
 * @class YAHOO.ext.Layer
 * @extends YAHOO.ext.Element
 * An extended Element object that supports a shadow and shim, constrain to viewport and
 * automatic maintaining of shadow/shim positions.
 * @cfg {Boolean} shim False to disable the iframe shim in browsers which need one (defaults to true)
 * @cfg {String/Boolean} shadow True to create a shadow element with default class "ylayer-shadow" or 
 * you can pass a string with a css class name. False turns off the shadow.
 * @cfg {Object} dh DomHelper object config to create element with (defaults to {tag: 'div', cls: 'ylayer'}).  
 * @cfg {Boolean} constrain False to disable constrain to viewport (defaults to true)
 * @cfg {String} cls CSS class to add to the element
 * @cfg {Number} zindex Starting z-index (defaults to 11000!)
 * @cfg {Number} shadowOffset Offset for the shadow (defaults to 3)
 * @constructor
 * @param {Object} config
 * @param {String/HTMLElement} existingEl (optional) Uses an existing dom element. If the element is not found it creates it.
 */
YAHOO.ext.Layer = function(config, existingEl){
    config = config || {};
    var dh = YAHOO.ext.DomHelper;
    if(existingEl){
        this.dom = YAHOO.util.Dom.get(existingEl);
    }
    if(!this.dom){
        var o = config.dh || {tag: 'div', cls: 'ylayer'};
        this.dom = dh.insertBefore(document.body.firstChild, o);
    }
    if(config.cls){
        this.addClass(config.cls);
    }
    this.constrain = config.constrain !== false;
    this.visibilityMode = YAHOO.ext.Element.VISIBILITY;
    this.id = YAHOO.util.Dom.generateId(this.dom);
    var zindex = (config.zindex || parseInt(this.getStyle('z-index'), 10)) || 11000;
    this.setAbsolutePositioned(zindex);
    if(config.shadow){
        var cls = (typeof config.shadow == 'string' ? config.shadow : 'ylayer-shadow');
        this.shadow = dh.insertBefore(this.dom, 
            {tag: 'div', cls: cls}, true);
        this.shadowOffset = config.shadowOffset || 3;
        this.shadow.setAbsolutePositioned(zindex-1);
    }else{
        this.shadowOffset = 0;
    }
    var b = YAHOO.ext.util.Browser;
    if(config.shim !== false && (b.isIE || (b.isGecko && b.isMac))){
        this.shim = this.createShim();
        this.shim.setOpacity(0);
        this.shim.setAbsolutePositioned(zindex-2);
    }
    this.hide();
};
YAHOO.extendX(YAHOO.ext.Layer, YAHOO.ext.Element, {
    sync : function(doShow){
        if(this.isVisible() && (this.shadow || this.shim)){
            var b = this.getBox();
            if(this.shim){
                if(doShow){
                   this.shim.show();
                }
                this.shim.setBox(b);
            }
            if(this.shadow){
                if(doShow){
                    this.shadow.show();
                }
                b.x += this.shadowOffset;
                b.y += this.shadowOffset;
                this.shadow.setBox(b);
            }
        }
    },
    
    syncLocalXY : function(){
        var l = this.getLeft(true);
        var t = this.getTop(true);
        if(this.shim){
            this.shim.setLeftTop(l, t);
        }
        if(this.shadow){
            this.shadow.setLeftTop(l + this.shadowOffset, 
              t + this.shadowOffset);
        }
    },
    
    hideUnders : function(negOffset){
        if(this.shadow){
            this.shadow.hide();
            if(negOffset){
                this.shadow.setLeftTop(-10000,-10000);
            }
        }
        if(this.shim){
           this.shim.hide();
           if(negOffset){
               this.shim.setLeftTop(-10000,-10000);
           }
        }
    },
    
    constrainXY : function(){
        if(this.constrain){
            var vw = YAHOO.util.Dom.getViewportWidth(),
                vh = YAHOO.util.Dom.getViewportHeight();
            var xy = this.getXY();
            var x = xy[0], y = xy[1];   
            var w = this.dom.offsetWidth+this.shadowOffset, h = this.dom.offsetHeight+this.shadowOffset;
            // only move it if it needs it
            var moved = false;
            // first validate right/bottom
            if(x + w > vw){
                x = vw - w;
                moved = true;
            }
            if(y + h > vh){
                y = vh - h;
                moved = true;
            }
            // then make sure top/left isn't negative
            if(x < 0){
                x = 0;
                moved = true;
            }
            if(y < 0){
                y = 0;
                moved = true;
            }
            if(moved){
                xy = [x, y];
                this.lastXY = xy;
                this.beforeAction();
                YAHOO.ext.Layer.superclass.setXY.call(this, xy);
                this.sync(true);
            }
        }
    },
    
    setVisible : function(v, a, d, c, e){
        if(this.lastXY){
            YAHOO.ext.Layer.superclass.setXY.call(this, this.lastXY);
        }
        if(a && v){
            var cb = function(){
                this.sync(true);
                if(c){
                    c();
                }
            }.createDelegate(this);
            YAHOO.ext.Layer.superclass.setVisible.call(this, true, true, d, cb, e);
        }else{
            if(!v){
                this.hideUnders(true);
            }
            var cb = c;
            if(a){
                cb = function(){
                    this.setLeftTop(-10000,-10000);
                    if(c){
                        c();
                    }
                }.createDelegate(this);
            }
            YAHOO.ext.Layer.superclass.setVisible.call(this, v, a, d, cb, e);
            if(v){
                this.sync(true);
            }else if(!a){
                this.setLeftTop(-10000,-10000);
            }
        }
    },
    
    beforeAction : function(){
        if(this.shadow){
            this.shadow.hide();
        }
    },
    
    setXY : function(xy, a, d, c, e){
        this.lastXY = xy;
        this.beforeAction();
        var cb = this.createCB(c);
        YAHOO.ext.Layer.superclass.setXY.call(this, xy, a, d, cb, e);
        if(!a){
            cb();
        }
    },
    
    createCB : function(c){
        var el = this;
        return function(){
            el.constrainXY();
            el.sync(true);
            if(c){
                c();
            }
        };
    },
    
    setX : function(x, a, d, c, e){
        this.setXY([x, this.getY()], a, d, c, e);
    },
    
    setY : function(y, a, d, c, e){
        this.setXY([this.getX(), y], a, d, c, e);
    },
    
    setSize : function(w, h, a, d, c, e){
        this.beforeAction();
        var cb = this.createCB(c);
        YAHOO.ext.Layer.superclass.setSize.call(this, w, h, a, d, cb, e);
        if(!a){
            cb();
        }
    },
    
    setWidth : function(w, a, d, c, e){
        this.beforeAction();
        var cb = this.createCB(c);
        YAHOO.ext.Layer.superclass.setWidth.call(this, w, a, d, cb, e);
        if(!a){
            cb();
        }
    },
    
    setHeight : function(h, a, d, c, e){
        this.beforeAction();
        var cb = this.createCB(c);
        YAHOO.ext.Layer.superclass.setHeight.call(this, h, a, d, cb, e);
        if(!a){
            cb();
        }
    },
    
    setBounds : function(x, y, w, h, a, d, c, e){
        this.beforeAction();
        var cb = this.createCB(c);
        if(!a){
            YAHOO.ext.Layer.superclass.setXY.call(this, [x, y]);
            YAHOO.ext.Layer.superclass.setSize.call(this, w, h, a, d, cb, e);
            cb();
        }else{
            YAHOO.ext.Layer.superclass.setBounds.call(this, x, y, w, h, a, d, cb, e);
        }
        return this;
    }
});
