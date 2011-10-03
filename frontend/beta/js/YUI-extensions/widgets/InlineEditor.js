YAHOO.ext.InlineEditor = function(config, existingEl){
    YAHOO.ext.util.Config.apply(this, config);
    var dh = YAHOO.ext.DomHelper;
    this.wrap = dh.append(this.container || document.body, {
        tag:'div', 
        cls:'yinline-editor-wrap'
    }, true);
    
    this.textSizeEl = dh.append(document.body, {
            tag: 'div',
            cls: 'yinline-editor-sizer ' + (this.cls || '')
        });
    if(YAHOO.ext.util.Browser.isSafari){ // extra padding for safari's textboxes
        this.textSizeEl.style.padding = '4px';
        YAHOO.util.Dom.setStyle(this.textSizeEl, 'padding-right', '10px');
    }
    
    if(!YAHOO.ext.util.Browser.isGecko){  // no one else needs FireFox cursor fix
        this.wrap.setStyle('overflow', 'hidden');
    }
    
    if(existingEl){
        this.el = getEl(existingEl);
    }
    if(!this.el){
        this.id = this.id || YAHOO.util.Dom.generateId();
        if(!this.multiline){
            this.el = this.wrap.createChild({
                tag: 'input', 
                name: this.name || this.id, 
                id: this.id, 
                type: this.type || 'text',
                autocomplete: 'off',
                value: this.value || '',
                cls: 'yinline-editor ' + (this.cls || ''),
                maxlength: this.maxLength || ''
            });
        }else{
            this.el = this.wrap.createChild({
                tag: 'textarea', 
                name: this.name || this.id, 
                id: this.id,
                html: this.value || '',
                cls: 'yinline-editor yinline-editor-multiline ' + (this.cls || ''),
                wrap: 'none'
            });
        }
    }else{
        this.wrap.dom.appendChild(this.el.dom);
    }
    this.el.addKeyMap([{
        key: [10, 13],
        fn: this.onEnter,
        scope: this
    },{
        key: 27,
        fn: this.onEsc,
        scope: this
    }]);
    this.el.mon('keyup', this.onKeyUp, this, true);
    this.el.on('blur', this.onBlur, this, true);
    this.el.swallowEvent('keydown');
    this.events = {
        'startedit' : true,
        'beforecomplete' : true,
        'complete' : true
    };
    this.editing = false;
    this.autoSizeTask = new YAHOO.ext.util.DelayedTask(this.autoSize, this);
};

YAHOO.extendX(YAHOO.ext.InlineEditor, YAHOO.ext.util.Observable, {
    onEnter : function(k, e){
        if(this.multiline && (e.ctrlKey || e.shiftKey)){
            return;
        }else{
            this.completeEdit();
            e.stopEvent();
        }
    },
    
    onEsc : function(){
        if(this.ignoreNoChange){
            this.revert(true);
        }else{
            this.revert(false);
            this.completeEdit();
        }
    },
    
    onBlur : function(){
        if(this.editing && this.completeOnBlur !== false){
            this.completeEdit();
        }
    },
    
    startEdit : function(el, value){
        this.boundEl = YAHOO.util.Dom.get(el);
        if(this.hideEl !== false){
            this.boundEl.style.visibility = 'hidden';
        }
        var v = value || this.boundEl.innerHTML;
        this.startValue = v;
        this.setValue(v);
        this.moveTo(YAHOO.util.Dom.getXY(this.boundEl));
        this.editing = true;
        if(YAHOO.ext.QuickTips){
            YAHOO.ext.QuickTips.disable();
        }
        this.show.defer(10, this);
    },
    
    onKeyUp : function(e){
        var k = e.getKey();
        if(this.editing && (k < 33 || k > 40) && k != 27){
            this.autoSizeTask.delay(50);
        }
    },
    
    completeEdit : function(){
        var v = this.getValue();
        if(this.revertBlank !== false && v.length < 1){
            v = this.startValue;
            this.revert();
        }
        if(v == this.startValue && this.ignoreNoChange){
            this.hide();
        }
        if(this.fireEvent('beforecomplete', this, v, this.startValue) !== false){
            if(this.updateEl !== false && this.boundEl){
                this.boundEl.innerHTML = v;
            }
            this.hide();
            this.fireEvent('complete', this, v, this.startValue);
        }
    },
    
    revert : function(hide){
        this.setValue(this.startValue);
        if(hide){
            this.hide();
        } 
    },
    
    show : function(){
        this.autoSize();
        this.wrap.show();
        this.el.focus();
        if(this.selectOnEdit !== false){
            this.el.dom.select();
        }
    },
    
    hide : function(){
        this.editing = false;
        this.wrap.hide();
        this.wrap.setLeftTop(-10000,-10000);
        this.el.blur();
        if(this.hideEl !== false){
            this.boundEl.style.visibility = 'visible';
        }
        if(YAHOO.ext.QuickTips){
            YAHOO.ext.QuickTips.enable();
        }
    },
    
    setValue : function(v){
        this.el.dom.value = v;
    },
    
    getValue : function(){
        return this.el.dom.value;  
    },
    
    autoSize : function(){
        var el = this.el;
        var wrap = this.wrap;
        var v = el.dom.value;
        var ts = this.textSizeEl;
        if(v.length < 1){
            ts.innerHTML = "&#160;&#160;";
        }else{
            v = v.replace(/[<> ]/g, '&#160;');
            if(this.multiline){
                v = v.replace(/\n/g, '<br />&#160;');
            }
            ts.innerHTML = v;
        }
        var ww = wrap.dom.offsetWidth;
        var wh = wrap.dom.offsetHeight;
        var w = ts.offsetWidth;
        var h = ts.offsetHeight;
        // lots of magic numbers in this block - wtf?
        // the logic is to prevent the scrollbars from flashing
        // in firefox. Updates the right element first
        // so there's never overflow.
        if(ww > w+4){
            el.setWidth(w+4);
            wrap.setWidth(w+8);
        }else{
            wrap.setWidth(w+8);
            el.setWidth(w+4);
        }
        if(wh > h+4){
            el.setHeight(h);
            wrap.setHeight(h+4);
        }else{
            wrap.setHeight(h+4);
            el.setHeight(h);
        }
    },
    
    moveTo : function(xy){
        this.wrap.setXY(xy);
    }
});
