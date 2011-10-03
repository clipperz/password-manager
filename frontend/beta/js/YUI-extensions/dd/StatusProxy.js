YAHOO.ext.dd.StatusProxy = function(config){
    YAHOO.ext.util.Config.apply(this, config);
    this.id = this.id || YAHOO.util.Dom.generateId();
    this.el = new YAHOO.ext.Layer({
        dh: {
            id: this.id, tag: 'div', cls: 'ydd-drag-proxy '+this.dropNotAllowed, children: [
                {tag: 'div', cls: 'ydd-drop-icon'},
                {tag: 'div', cls: 'ydd-drag-ghost'}
            ]
        }, 
        shadow: !config || config.shadow !== false
    });
    /*this.el = YAHOO.ext.DomHelper.insertBefore(document.body.firstChild, {
        id: this.id, tag: 'div', cls: 'ydd-drag-proxy '+this.dropNotAllowed, children: [
            {tag: 'div', cls: 'ydd-drop-icon'},
            {tag: 'div', cls: 'ydd-drag-ghost'}
        ]
    }, true);*/
    this.ghost = getEl(this.el.dom.childNodes[1]);
    this.dropStatus = this.dropNotAllowed;
};

YAHOO.ext.dd.StatusProxy.prototype = {
    dropAllowed : 'ydd-drop-ok',
    dropNotAllowed : 'ydd-drop-nodrop',
    /**
     * Updates the DD visual element to allow/not allow a drop
     * @param {String} cssClass The css class for the new drop status indicator image
     */
    setStatus : function(cssClass){
        cssClass = cssClass || this.dropNotAllowed;
        if(this.dropStatus != cssClass){
            this.el.replaceClass(this.dropStatus, cssClass);
            this.dropStatus = cssClass;
        }
    },
    
    reset : function(clearGhost){
        this.el.dom.className = 'ydd-drag-proxy ' + this.dropNotAllowed;
        this.dropStatus = this.dropNotAllowed;
        if(clearGhost){
            this.ghost.update('');
        }
    },
    
    update : function(html){
        if(typeof html == 'string'){
            this.ghost.update(html);
        }else{
            this.ghost.update('');
            html.style.margin = '0';
            this.ghost.dom.appendChild(html);
        }        
    },
    
    getEl : function(){
        return this.el;
    },
    
    getGhost : function(){
        return this.ghost;
    },
    
    hide : function(clear){
        this.el.hide();
        if(clear){
            this.reset(true);
        }
    },
    
    stop : function(){
        if(this.anim && this.anim.isAnimated()){
            this.anim.stop();
        }
    },
    
    show : function(){
        this.el.show();
    },
    
    sync : function(){
        this.el.syncLocalXY();  
    },
    
    repair : function(xy, callback, scope){
        this.callback = callback;
        this.scope = scope;
        if(xy && this.animRepair !== false && YAHOO.util.Anim){
            this.el.addClass('ydd-drag-repair');
            this.el.hideUnders(true);
            if(!this.anim){
                this.anim = new YAHOO.util.Motion(this.el.dom, {}, this.repairDuration || .5, YAHOO.util.Easing.easeOut);
                this.anim.onComplete.subscribe(this.afterRepair, this, true);
            }
            this.anim.attributes = {points: {to:xy}};
            this.anim.animate();
        }else{
            this.afterRepair();
        }
    },
    
    afterRepair : function(){
        this.hide(true);
        if(typeof this.callback == 'function'){
            this.callback.call(this.scope || this);
        }
        this.callback == null;
        this.scope == null;
    }
};
