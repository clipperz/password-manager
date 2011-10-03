YAHOO.namespace('ext.tree');

YAHOO.ext.tree.TreePanel = function(el, config){
   YAHOO.ext.tree.TreePanel.superclass.constructor.call(this);
   this.el = getEl(el);
   this.id = this.el.id;
   YAHOO.ext.util.Config.apply(this, config || {}, {
       rootVisible : true,
       lines : true,
       enableDD : false,
       hlDrop : true/*,
       hlColor: null,
       ddGroup : 'TreeDD'
       hlBaseColor : 'FFFFFF'*/
       
   });
   YAHOO.ext.util.Config.apply(this.events, {
        'beforeload' : true,
        'load' : true,
        'textchange' : true,
        'beforeexpand' : true,
        'beforecollapse' : true,
        'expand' : true,
        'collapse' : true,
        'disabledchange' : true,
        'beforeclick':true,
        'click':true,
        'dblclick':true,
        'contentmenu':true,
        'beforechildrenrendered':true,
       /**
	     * @event startdrag
	     * Fires when a node starts being dragged 
	     * @param {YAHOO.ext.tree.TreePanel} this
	     * @param {YAHOO.ext.tree.TreeNode} node
	     * @param {event} e The raw browser event
	     */
	    'startdrag' : true,
	    /**
	     * @event enddrag
	     * Fires when a drag operation is complete
	     * @param {YAHOO.ext.tree.TreePanel} this
	     * @param {YAHOO.ext.tree.TreeNode} node
	     * @param {event} e The raw browser event
	     */
	    'enddrag' : true,
	    /**
	     * @event dragdrop
	     * Fires when a dragged node is dropped on a valid DD target 
	     * @param {YAHOO.ext.tree.TreePanel} this
	     * @param {YAHOO.ext.tree.TreeNode} node
	     * @param {DD} dd The dd it was dropped on
	     * @param {event} e The raw browser event
	     */
	    'dragdrop' : true,
	    /**
	     * @event beforenodedrop
	     * Fires when a DD object is dropped on a node in this tree for preprocessing. This event can cancel.
	     * @param {Object} dropEvent
	     */
	    'beforenodedrop' : true,
	    /**
	     * @event nodedrop
	     * Fires after a DD object is dropped on a node in this tree 
	     * @param {Object} dropEvent
	     */
	    'nodedrop' : true,
	     /**
	     * @event nodedragover
	     * Fires when a tree node is being target
	     * @param {Object} dragOverEvent
	     */
	    'nodedragover' : true
   });
   if(this.singleExpand){
       this.on('beforeexpand', this.restrictExpand, this, true);
   }
   // problem with safari and animation
   // I am investigating
   if(YAHOO.ext.util.Browser.isSafari){
       this.animate = false;
   }     
};
YAHOO.extendX(YAHOO.ext.tree.TreePanel, YAHOO.ext.data.Tree, {
    restrictExpand : function(node){
        var p = node.parentNode;
        if(p){
            if(p.expandedChild && p.expandedChild.parentNode == p){
                p.expandedChild.collapse();
            }
            p.expandedChild = node;
        }    
    },
    
    setRootNode : function(node){
        YAHOO.ext.tree.TreePanel.superclass.setRootNode.call(this, node);
        if(!this.rootVisible){
            node.ui = new YAHOO.ext.tree.RootTreeNodeUI(node);
        }
        return node;
    },
    
    getEl : function(){
        return this.el;  
    },
    
    getLoader : function(){
        return this.loader;    
    },
    
    expandAll : function(){
        this.root.expand(true);
    },
    
    collapseAll : function(){
        this.root.collapse(true);  
    },
    
    getSelectionModel : function(){
        if(!this.selModel){
            this.selModel = new YAHOO.ext.tree.DefaultSelectionModel();
        }
        return this.selModel;
    },
    
    expandPath : function(path, attr, callback){
        attr = attr || 'id';
        var keys = path.split(this.pathSeparator);
        var curNode = this.root;
        if(curNode.attributes[attr] != keys[1]){ // invalid root
            if(callback){
                callback(false, null);
            }
            return;
        }
        var index = 1;
        var f = function(){
            if(++index == keys.length){
                if(callback){
                    callback(true, curNode);
                }
                return;
            }
            var c = curNode.findChild(attr, keys[index]);
            if(!c){
                if(callback){
                    callback(false, curNode);
                }
                return;
            }
            curNode = c;
            c.expand(false, false, f);
        }
        curNode.expand(false, false, f);
    },
    
    selectPath : function(path, attr, callback){
        attr = attr || 'id';
        var keys = path.split(this.pathSeparator);
        var v = keys.pop();
        if(keys.length > 0){
            var f = function(success, node){
                if(success && node){
                    var n = node.findChild(attr, v);
                    if(n){
                        n.select();
                        if(callback){
                            callback(true, n);
                        }
                    }
                }else{
                    if(callback){
                        callback(false, n);
                    }
                }
            };
            this.expandPath(keys.join(this.pathSeparator), attr, f);
        }else{
            this.root.select();
            if(callback){
                callback(true, this.root);
            }
        }
    },
    
    render : function(){
        this.container = this.el.createChild({tag:'ul', 
               cls:'ytree-root-ct ' + 
               (this.lines ? 'ytree-lines' : 'ytree-no-lines')});
        
        if(this.containerScroll){
            YAHOO.ext.dd.ScrollManager.register(this.el);    
        }
            
        if((this.enableDD || this.enableDrop) && !this.dropZone){
           this.dropZone = new YAHOO.ext.tree.TreeDropZone(this, this.dropConfig || {
               ddGroup: this.ddGroup || 'TreeDD'
           });
        }
        if((this.enableDD || this.enableDrag) && !this.dragZone){
           this.dragZone = new YAHOO.ext.tree.TreeDragZone(this, this.dragConfig || {
               ddGroup: this.ddGroup || 'TreeDD',
               scroll: this.ddScroll
           });
        }
        this.getSelectionModel().init(this);
        this.root.render();
        if(!this.rootVisible){
            this.root.renderChildren();
        }
        return this;
    }
});
