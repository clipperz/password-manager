/**
 * The TreeNode UI implementation is separate from the 
 * tree implementation. Unless you are customizing the tree UI,
 * you should never have to use this directly.
 */
YAHOO.ext.tree.TreeNodeUI = function(node){
    this.node = node;
    this.rendered = false;
    this.animating = false;
};

YAHOO.ext.tree.TreeNodeUI.prototype = {
    emptyIcon : Ext.BLANK_IMAGE_URL,
    
    removeChild : function(node){
        if(this.rendered){
            this.ctNode.removeChild(node.ui.getEl());
        } 
    },
    
    beforeLoad : function(){
         YAHOO.util.Dom.addClass(this.elNode, 'ytree-node-loading'); 
    },
    
    afterLoad : function(){
         YAHOO.util.Dom.removeClass(this.elNode, 'ytree-node-loading'); 
    },
    
    onTextChange : function(node, text, oldText){
        if(this.rendered){
            this.textNode.innerHTML = text;
        }
    },
    
    onDisableChange : function(node, state){
        this.disabled = state;
        if(state){
            YAHOO.util.Dom.addClass(this.elNode, 'ytree-node-disabled');
        }else{
            YAHOO.util.Dom.removeClass(this.elNode, 'ytree-node-disabled');
        } 
    },
    
    onSelectedChange : function(state){
        if(state){
            this.focus();
            YAHOO.util.Dom.addClass(this.elNode, 'ytree-selected');
        }else{
            this.blur();
            YAHOO.util.Dom.removeClass(this.elNode, 'ytree-selected');
        } 
    },
    
    onMove : function(tree, node, oldParent, newParent, index, refNode){
        this.childIndent = null;
        if(this.rendered){
            var targetNode = newParent.ui.getContainer();
            if(!targetNode){//target not rendered
                this.holder = document.createElement('div');
                this.holder.appendChild(this.wrap);
                return;
            }
            var insertBefore = refNode ? refNode.ui.getEl() : null;
            if(insertBefore){
                targetNode.insertBefore(this.wrap, insertBefore);
            }else{
                targetNode.appendChild(this.wrap);
            }
            this.node.renderIndent(true);
        }
    },
    
    remove : function(){
        if(this.rendered){
            this.holder = document.createElement('div');
            this.holder.appendChild(this.wrap);
        }  
    },
    
    fireEvent : function(){
        this.node.fireEvent.apply(this.node, arguments);  
    },
    
    initEvents : function(){
        this.node.on('move', this.onMove, this, true);
        //this.node.on('hiddenchange', this.onHiddenChange, this, true);
        
        // these were optimized out but a custom UI could use them
        //this.node.on('remove', this.onChildRemoved, this, true);
        //this.node.on('selectedstatechange', this.onSelectedChange, this, true);
        //this.node.on('disabledchange', this.onDisableChange, this, true);
        //this.node.on('textchange', this.onTextChange, this, true);
        
        var E = YAHOO.util.Event;
        var a = this.anchor;
        
        var el = YAHOO.ext.Element.fly(a);
        
        if(YAHOO.ext.util.Browser.isOpera){ // opera render bug ignores the CSS
            el.setStyle('text-decoration', 'none');
        }
        
        el.mon('click', this.onClick, this, true);
        el.mon('dblclick', this.onDblClick, this, true);
        el.mon('contextmenu', this.onContextMenu, this, true);
        
        //el.on('focus', function(){
        //    this.node.getOwnerTree().getSelectionModel().select(this.node);
        //}, this, true);
        
        var icon = YAHOO.ext.Element.fly(this.iconNode);
        icon.mon('click', this.onClick, this, true);
        icon.mon('dblclick', this.onDblClick, this, true);
        icon.mon('contextmenu', this.onContextMenu, this, true);
        E.on(this.ecNode, 'click', this.ecClick, this, true);
        
        if(this.node.disabled){
            YAHOO.util.Dom.addClass(this.elNode, 'ytree-node-disabled');
        }
        if(this.node.hidden){
            YAHOO.util.Dom.addClass(this.elNode, 'ytree-node-disabled');
        }
        var dd = this.node.ownerTree.enableDD || this.node.ownerTree.enableDrag || this.node.ownerTree.enableDrop;
        if(dd && (!this.node.isRoot || this.node.ownerTree.rootVisible)){
            YAHOO.ext.dd.Registry.register(this.elNode, {
                node: this.node,
                handles: [this.iconNode, this.textNode],
                isHandle: false
            });
        }
    },
    
    hide : function(){
        if(this.rendered){
            this.wrap.style.display = 'none';
        }  
    },
    
    show : function(){
        if(this.rendered){
            this.wrap.style.display = '';
        } 
    },
    
    onContextMenu : function(e){
        e.preventDefault();
        this.focus();
        this.fireEvent('contextmenu', this.node, e);
    },
    
    onClick : function(e){
        if(this.dropping){
            return;
        }
        if(this.fireEvent('beforeclick', this.node, e) !== false){
            if(!this.disabled && this.node.attributes.href){
                this.focus();
                this.fireEvent('click', this.node, e);
                return;
            }
            e.preventDefault();
            if(this.disabled){
                return;
            }
            this.focus();
            this.fireEvent('click', this.node, e);
        }else{
            e.stopEvent();
        }
    },
    
    onDblClick : function(e){
        e.preventDefault();
        if(this.disabled){
            return;
        }
        if(!this.animating && this.node.hasChildNodes()){
            this.node.toggle();
        }
        this.fireEvent('dblclick', this.node, e);
    },
    
    ecClick : function(e){
        if(!this.animating && this.node.hasChildNodes()){
            this.node.toggle();
        }
    },
    
    startDrop : function(){
        this.dropping = true;
    },
    
    // delayed drop so the click event doesn't get fired on a drop
    endDrop : function(){ 
       setTimeout(function(){
           this.dropping = false;
       }.createDelegate(this), 50); 
    },
    
    expand : function(){
        this.updateExpandIcon();
        this.ctNode.style.display = '';
    },
    
    focus : function(){
        try{
            this.anchor.focus();
        }catch(e){} 
    },
    
    blur : function(){
        try{
            this.anchor.blur();
        }catch(e){} 
    },
    
    animExpand : function(callback){
        if(this.animating && this.anim){
            this.anim.stop();
        }
        this.animating = true;
        this.updateExpandIcon();
        var ct = this.ctNode;
        var cs = ct.style;
        cs.position = 'absolute';
        cs.visibility = 'hidden';
        cs.display = '';
        var h = ct.clientHeight;
        cs.overflow = 'hidden';
        cs.height = '1px';
        cs.position = '';
        cs.visibility = '';
        var anim = new YAHOO.util.Anim(ct, {
            height: {to: h}
        }, this.node.ownerTree.duration || .25, YAHOO.util.Easing.easeOut);
        anim.onComplete.subscribe(function(){
            cs.overflow = '';
            cs.height = '';
            this.animating = false;
            this.anim = null;
            if(typeof callback == 'function'){
                callback();
            }
        }, this, true);
        this.anim = anim;
        anim.animate();
    },
    
    highlight : function(){
        var tree = this.node.getOwnerTree();
        var hlColor = tree.hlColor || 'C3DAF9';
        var hlBaseColor = tree.hlBaseColor || 'FFFFFF';
        var anim = new YAHOO.util.ColorAnim(this.wrap, {
            backgroundColor: {from: hlColor, to: hlBaseColor}
        }, .75, YAHOO.util.Easing.easeNone);
        anim.onComplete.subscribe(function(){
            YAHOO.util.Dom.setStyle(this.wrap, 'background-color', '');
        }, this, true);
        anim.animate(); 
    },
    
    collapse : function(){
        this.updateExpandIcon();
        this.ctNode.style.display = 'none';
    },
    
    animCollapse : function(callback){
        if(this.animating && this.anim){
            this.anim.stop();
        }
        this.animating = true;
        this.updateExpandIcon();
        var ct = this.ctNode;
        var cs = ct.style;
        cs.height = ct.offsetHeight +'px';
        cs.overflow = 'hidden';
        var anim = new YAHOO.util.Anim(ct, {
            height: {to: 1}
        }, this.node.ownerTree.duration || .25, YAHOO.util.Easing.easeOut);
        anim.onComplete.subscribe(function(){
            cs.display = 'none';
            cs.overflow = '';
            cs.height = '';
            this.animating = false;
            this.anim = null;
            if(typeof callback == 'function'){
                callback();
            }
        }, this, true);
        this.anim = anim;
        anim.animate();
    },
    
    getContainer : function(){
        return this.ctNode;  
    },
    
    getEl : function(){
        return this.wrap;  
    },
    
    appendDDGhost : function(ghostNode){
        ghostNode.appendChild(this.elNode.cloneNode(true));
    },
    
    getDDRepairXY : function(){
        return YAHOO.util.Dom.getXY(this.iconNode);  
    },
    
    onRender : function(){
        this.render();    
    },
    
    render : function(bulkRender){
        var n = this.node;
        var targetNode = n.parentNode ? 
              n.parentNode.ui.getContainer() : n.ownerTree.container.dom;
        if(!this.rendered){
            this.rendered = true;
            var a = n.attributes;
        
            // add some indent caching, this helps performance when rendering a large tree
            this.indentMarkup = '';
            if(n.parentNode){
                this.indentMarkup = n.parentNode.ui.getChildIndent();
            }
            
            var buf = ['<li class="ytree-node"><div class="ytree-node-el ', n.attributes.cls,'">',
                '<span class="ytree-node-indent">',this.indentMarkup,'</span>',
                '<img src="', this.emptyIcon, '" class="ytree-ec-icon">',
                '<img src="', a.icon || this.emptyIcon, '" class="ytree-node-icon',(a.icon ? ' ytree-node-inline-icon' : ''),'" unselectable="on">',
                '<a href="',a.href ? a.href : '#','" tabIndex="1" ',
                 a.hrefTarget ? ' target="'+a.hrefTarget+'"' : '','><span unselectable="on">',n.text,'</span></a></div>',
                '<ul class="ytree-node-ct" style="display:none;"></ul>',
                '</li>'];
                
            if(bulkRender !== true && n.nextSibling && n.nextSibling.ui.getEl()){
                this.wrap = YAHOO.ext.DomHelper.insertHtml('beforeBegin', 
                                    n.nextSibling.ui.getEl(), buf.join(''));
            }else{
                this.wrap = YAHOO.ext.DomHelper.insertHtml('beforeEnd', targetNode, buf.join(''));
            }
            this.elNode = this.wrap.childNodes[0];
            this.ctNode = this.wrap.childNodes[1];
            var cs = this.elNode.childNodes;
            this.indentNode = cs[0];
            this.ecNode = cs[1];
            this.iconNode = cs[2];
            this.anchor = cs[3];
            this.textNode = cs[3].firstChild;
            if(a.qtip){
               if(this.textNode.setAttributeNS){
                   this.textNode.setAttributeNS('y', 'qtip', a.qtip);
                   if(a.qtipTitle){
                       this.textNode.setAttributeNS('y', 'qtitle', a.qtipTitle);
                   }
               }else{
                   this.textNode.setAttribute('y:qtip', a.qtip);
                   if(a.qtipTitle){
                       this.textNode.setAttribute('y:qtitle', a.qtipTitle);
                   }
               } 
            }
            this.initEvents();
            //this.renderIndent(); cached above now instead call updateExpandIcon
            this.updateExpandIcon();
        }else{
            if(bulkRender === true) {
                targetNode.appendChild(this.wrap);
            }
        }
    },
    
    getAnchor : function(){
        return this.anchor;
    },
    
    getTextEl : function(){
        return this.textNode;
    },
    
    getIconEl : function(){
        return this.iconNode;
    },
    
    updateExpandIcon : function(){
        if(this.rendered){
            var n = this.node;
            var cls = n.isLast() ? "ytree-elbow-end" : "ytree-elbow";
            var hasChild = n.hasChildNodes();
            if(hasChild){
                cls += n.expanded ? '-minus' : '-plus';
                var c1 = n.expanded ? 'ytree-node-collapsed' : 'ytree-node-expanded';
                var c2 = n.expanded ? 'ytree-node-expanded' : 'ytree-node-collapsed';
                YAHOO.util.Dom.removeClass(this.elNode, 'ytree-node-leaf');
                YAHOO.util.Dom.replaceClass(this.elNode, c1, c2);
            }else{
                YAHOO.util.Dom.replaceClass(this.elNode, 'ytree-node-expanded', 'ytree-node-leaf');
            }
            this.ecNode.className = 'ytree-ec-icon '+cls;
        }
    },
    
    getChildIndent : function(){
        if(!this.childIndent){
            var buf = [];
            var p = this.node;
            while(p){
                if(!p.isRoot || (p.isRoot && p.ownerTree.rootVisible)){
                    if(!p.isLast()) {
                        buf.unshift('<img src="'+this.emptyIcon+'" class="ytree-elbow-line">');
                    } else {
                        buf.unshift('<img src="'+this.emptyIcon+'" class="ytree-icon">');
                    }
                }
                p = p.parentNode;
            }
            this.childIndent = buf.join('');
        }
        return this.childIndent;
    },
    
    renderIndent : function(){
        if(this.rendered){
            var indent = '';
            var p = this.node.parentNode;
            if(p){
                indent = p.ui.getChildIndent();
            }
            if(this.indentMarkup != indent){ // don't rerender if not required
                this.indentNode.innerHTML = indent;
                this.indentMarkup = indent;
            }
            this.updateExpandIcon();
        }
    }
};

YAHOO.ext.tree.RootTreeNodeUI = function(){
    YAHOO.ext.tree.RootTreeNodeUI.superclass.constructor.apply(this, arguments);
};
YAHOO.extendX(YAHOO.ext.tree.RootTreeNodeUI, YAHOO.ext.tree.TreeNodeUI);
YAHOO.ext.tree.RootTreeNodeUI.prototype.render = function(){
    if(!this.rendered){
        var targetNode = this.node.ownerTree.container.dom;
        this.node.expanded = true;
        targetNode.innerHTML = '<div class="ytree-root-node"></div>';
        this.wrap = this.ctNode = targetNode.firstChild;
    }
};
YAHOO.ext.tree.RootTreeNodeUI.prototype.collapse = function(){
};
