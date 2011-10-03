/**
 * @class YAHOO.ext.tree.TreeNode
 * @extends YAHOO.ext.data.Node
 * @cfg {Boolean} leaf true if this node is a leaf and does not have or cannot have children
 * @cfg {Boolean} expanded true to start the node expanded
 * @cfg {Boolean} draggable false to make this node undraggable if DD is on (default to true)
 * @cfg {Boolean} isTarget false if this node cannot be drop on
 * @cfg {Boolean} disabled true to start the node disabled
 * @constructor
 * @param {Object} attributes The attributes/config for the node 
 */
YAHOO.ext.tree.TreeNode = function(attributes){
    attributes = attributes || {};
    if(typeof attributes == 'string'){
        attributes = {text: attributes};
    }
    this.el = null;
    this.childrenRendered = false;
    this.rendered = false;
    YAHOO.ext.tree.TreeNode.superclass.constructor.call(this, attributes);
    this.expanded = attributes.expanded === true;
    this.isTarget = attributes.isTarget !== false;
    this.draggable = attributes.draggable !== false && attributes.allowDrag !== false;
    this.allowChildren = attributes.allowChildren !== false && attributes.allowDrop !== false;
    this.text = attributes.text;
    this.disabled = attributes.disabled === true;
    
    YAHOO.ext.util.Config.apply(this.events, {
        'textchange' : true,
        'beforeexpand' : true,
        'beforecollapse' : true,
        'expand' : true,
        'disabledchange' : true,
        'collapse' : true,
        'beforeclick':true,
        'click':true,
        'dblclick':true,
        'contentmenu':true,
        'beforechildrenrendered':true
    });
    
    var uiClass = this.attributes.uiProvider || YAHOO.ext.tree.TreeNodeUI;
    this.ui = new uiClass(this);
};
YAHOO.extendX(YAHOO.ext.tree.TreeNode, YAHOO.ext.data.Node, {
    isExpanded : function(){
        return this.expanded;
    },
    
    getUI : function(){
        return this.ui;    
    },
    
    setFirstChild : function(node){
        var of = this.firstChild;
        YAHOO.ext.tree.TreeNode.superclass.setFirstChild.call(this, node);
        if(this.childrenRendered && of && node != of){
            of.renderIndent(true, true);  
        }
        if(this.rendered){
            this.renderIndent(true, true);
        }
    },
    
    setLastChild : function(node){
        var ol = this.lastChild;
        YAHOO.ext.tree.TreeNode.superclass.setLastChild.call(this, node);
        if(this.childrenRendered && ol && node != ol){
            ol.renderIndent(true, true);    
        }
        if(this.rendered){
            this.renderIndent(true, true);
        }
    },
    
    // these methods are overridden to provide lazy rendering support
    appendChild : function(){
        var node = YAHOO.ext.tree.TreeNode.superclass.appendChild.apply(this, arguments);
        if(node && this.childrenRendered){
            node.render();
        }
        this.ui.updateExpandIcon();
        return node;
    },
    
    removeChild : function(node){
        this.ownerTree.getSelectionModel().unselect(node);
        YAHOO.ext.tree.TreeNode.superclass.removeChild.apply(this, arguments);
        // if it's been rendered remove dom node
        if(this.childrenRendered){
            node.ui.remove();
        }
        if(this.childNodes.length < 1){
            this.collapse(false, false);
        }else{
            this.ui.updateExpandIcon();
        }
        return node;
    },
    
    insertBefore : function(node, refNode){
        var newNode = YAHOO.ext.tree.TreeNode.superclass.insertBefore.apply(this, arguments);
        if(newNode && refNode && this.childrenRendered){
            node.render();
        }
        this.ui.updateExpandIcon();
        return newNode;
    },
    
    setText : function(text){
        var oldText = this.text;
        this.text = text;
        this.attributes.text = text;
        if(this.rendered){ // event without subscribing
            this.ui.onTextChange(this, text, oldText);
        }
        this.fireEvent('textchange', this, text, oldText);
    },
    
    select : function(){
        this.getOwnerTree().getSelectionModel().select(this);
    },
    
    unselect : function(){
        this.getOwnerTree().getSelectionModel().unselect(this);
    },
    
    isSelected : function(){
        return this.getOwnerTree().getSelectionModel().isSelected(node);  
    },
    
    expand : function(deep, anim, callback){
        if(!this.expanded){
            if(this.fireEvent('beforeexpand', this, deep, anim) === false){
                return;
            }
            if(!this.childrenRendered){
                this.renderChildren();
            }
            this.expanded = true;
            if((this.getOwnerTree().animate && anim !== false) || anim){
                this.ui.animExpand(function(){
                    this.fireEvent('expand', this);
                    if(typeof callback == 'function'){
                        callback(this);
                    }
                    if(deep === true){
                        this.expandChildNodes(true);
                    }
                }.createDelegate(this));
                return;
            }else{
                this.ui.expand();
                this.fireEvent('expand', this);
                if(typeof callback == 'function'){
                    callback(this);
                }
            }
        }else{
           if(typeof callback == 'function'){
               callback(this);
           } 
        }
        if(deep === true){
            this.expandChildNodes(true);
        }
    },
    
    collapse : function(deep, anim){
        if(this.expanded && (!this.isRoot || (this.isRoot && this.getOwnerTree().rootVisible))){
            if(this.fireEvent('beforecollapse', this, deep, anim) === false){
                return;
            }
            this.expanded = false;
            if((this.getOwnerTree().animate && anim !== false) || anim){
                this.ui.animCollapse(function(){
                    this.fireEvent('collapse', this);
                    if(deep === true){
                        this.collapseChildNodes(true);
                    }
                }.createDelegate(this));
                return;
            }else{
                this.ui.collapse();
                this.fireEvent('collapse', this);
            }
        }
        if(deep === true){
            var cs = this.childNodes;
            for(var i = 0, len = cs.length; i < len; i++) {
            	cs[i].collapse(true)
            }
        }
    },
    
    delayedExpand : function(delay){
        if(!this.expandProcId){
            this.expandProcId = this.expand.defer(delay, this);
        } 
    },
    
    cancelExpand : function(){
        if(this.expandProcId){
            clearTimeout(this.expandProcId);
        }
        this.expandProcId = false; 
    },
    
    toggle : function(){
        if(this.expanded){
            this.collapse();
        }else{
            this.expand();
        }
    },
    
    ensureVisible : function(){
        if(this.parentNode){
            this.parentNode.bubble(function(){
                this.expand(false, false);
            });
        }  
    },
    
    expandChildNodes : function(deep){
        var cs = this.childNodes;
        for(var i = 0, len = cs.length; i < len; i++) {
        	cs[i].expand(deep);
        }
    },
    
    collapseChildNodes : function(deep){
        var cs = this.childNodes;
        for(var i = 0, len = cs.length; i < len; i++) {
        	cs[i].expand(deep);
        }
    },
    
    disable : function(){
        this.disabled = true;
        this.unselect();
        if(this.rendered && this.ui.onDisableChange){ // event without subscribing
            this.ui.onDisableChange(this, true);
        }
        this.fireEvent('disabledchange', this, true);
    },
    
    enable : function(){
        this.disabled = false;
        if(this.rendered && this.ui.onDisableChange){ // event without subscribing
            this.ui.onDisableChange(this, false);
        }
        this.fireEvent('disabledchange', this, false); 
    },
    
    renderChildren : function(suppressEvent){
        if(suppressEvent !== false){
            this.fireEvent('beforechildrenrendered', this);
        }
        var cs = this.childNodes;
        for(var i = 0, len = cs.length; i < len; i++){
            cs[i].render(true);
        }
        this.childrenRendered = true;
    },
    
    sort : function(fn, scope){
        YAHOO.ext.tree.TreeNode.superclass.sort.apply(this, arguments);
        if(this.childrenRendered){
            var cs = this.childNodes;
            for(var i = 0, len = cs.length; i < len; i++){
                cs[i].render(true);
            }
        }  
    },
    
    render : function(bulkRender){
        this.ui.render(bulkRender);
        if(!this.rendered){
            this.rendered = true;
            if(this.expanded){
                this.expanded = false;
                this.expand(false, false);
            }
        }
    },
    
    renderIndent : function(deep, refresh){
        if(refresh){
            this.ui.childIndent = null;
        }
        this.ui.renderIndent();
        if(deep === true && this.childrenRendered){
            var cs = this.childNodes;
            for(var i = 0, len = cs.length; i < len; i++){
                cs[i].renderIndent(true, refresh);
            }
        }
    }
});
