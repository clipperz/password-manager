YAHOO.ext.tree.DefaultSelectionModel = function(){
   this.selNode = null;
   
   this.events = {
       'selectionchange' : true
   };
};

YAHOO.extendX(YAHOO.ext.tree.DefaultSelectionModel, YAHOO.ext.util.Observable, {
    init : function(tree){
        this.tree = tree;
        tree.el.mon('keydown', this.onKeyDown, this, true);
        tree.on('click', this.onNodeClick, this, true);
    },
    
    onNodeClick : function(node, e){
        this.select(node);
    },
    
    select : function(node){
        if(this.selNode && this.selNode != node){
            this.selNode.ui.onSelectedChange(false);
        }
        this.selNode = node;
        node.ui.onSelectedChange(true);
        this.fireEvent('selectionchange', this, node);
        return node;
    },
    
    unselect : function(node){
        if(this.selNode == node){
            this.clearSelections();
        }    
    },
    
    clearSelections : function(){
        var n = this.selNode;
        if(n){
            n.ui.onSelectedChange(false);
            this.selNode = null;
            this.fireEvent('selectionchange', this, null);
        }
        return n;
    },
    
    getSelectedNode : function(){
        return this.selNode;    
    },
    
    isSelected : function(node){
        return this.selNode == node;  
    },
    
    onKeyDown : function(e){
        var s = this.selNode || this.lastSelNode;
        // undesirable, but required
        var sm = this;
        if(!s){
            return;
        }
        var k = e.getKey();
        //alert(k)
        switch(k){
             case e.DOWN:
                 e.preventDefault();
                 if(s.firstChild && s.isExpanded()){
                     this.select(s.firstChild, e);
                 }else if(s.nextSibling){
                     this.select(s.nextSibling, e);
                 }else if(s.parentNode){
                     s.parentNode.bubble(function(){
                         if(this.nextSibling){
                             sm.select(this.nextSibling, e);
                             return false;
                         }
                     });
                 }
             break;
             case e.UP:
                 e.preventDefault();
                 var ps = s.previousSibling;
                 if(ps){
                     if(!ps.isExpanded()){
                         this.select(ps, e);
                     }else{
                         var lc = ps.lastChild;
                         while(lc && lc.isExpanded()){
                             lc = lc.lastChild; 
                         }
                         this.select(lc, e);
                     }
                 }else if(s.parentNode && (this.tree.rootVisible || !s.parentNode.isRoot)){
                     this.select(s.parentNode, e);
                 }
             break;
             case e.RIGHT:
                 e.preventDefault();
                 if(s.hasChildNodes()){
                     if(!s.isExpanded()){
                         s.expand();
                     }else if(s.firstChild){
                         this.select(s.firstChild, e);
                     }
                 }
             break;
             case e.LEFT:
                 e.preventDefault();
                 if(s.hasChildNodes() && s.isExpanded()){
                     s.collapse();
                 }else if(s.parentNode && (this.tree.rootVisible || s.parentNode != this.tree.getRootNode())){
                     this.select(s.parentNode, e);
                 }
             break;
        };
    }
});

YAHOO.ext.tree.MultiSelectionModel = function(){
   this.selNodes = [];
   this.selMap = {};
   this.events = {
       'selectionchange' : true
   };
};

YAHOO.extendX(YAHOO.ext.tree.MultiSelectionModel, YAHOO.ext.util.Observable, {
    init : function(tree){
        this.tree = tree;
        tree.el.mon('keydown', this.onKeyDown, this, true);
        tree.on('click', this.onNodeClick, this, true);
    },
    
    onNodeClick : function(node, e){
        this.select(node, e, e.ctrlKey);
    },
    
    select : function(node, e, keepExisting){
        if(keepExisting !== true){
            this.clearSelections(true);
        }
        this.selNodes.push(node);
        this.selMap[node.id] = node;
        this.lastSelNode = node;
        node.ui.onSelectedChange(true);
        this.fireEvent('selectionchange', this, this.selNodes);
        return node;
    },
    
    unselect : function(node){
        if(this.selMap[node.id]){
            node.ui.onSelectedChange(false);
            var sn = this.selNodes;
            var index = -1;
            if(sn.indexOf){
                index = sn.indexOf(node);
            }else{
                for(var i = 0, len = sn.length; i < len; i++){
                    if(sn[i] == node){
                        index = i;
                        break;
                    }
                }
            }
            if(index != -1){
                this.selNodes.splice(index, 1);
            }
            delete this.selMap[node.id];
            this.fireEvent('selectionchange', this, this.selNodes);
        }
    },
    
    clearSelections : function(suppressEvent){
        var sn = this.selNodes;
        if(sn.length > 0){
            for(var i = 0, len = sn.length; i < len; i++){
                sn[i].ui.onSelectedChange(false);
            }
            this.selNodes = [];
            this.selMap = {};
            if(suppressEvent !== true){
                this.fireEvent('selectionchange', this, this.selNodes);
            }
        }
    },
    
    isSelected : function(node){
        return this.selMap[node.id] ? true : false;  
    },
    
    getSelectedNodes : function(){
        return this.selNodes;    
    },
    
    onKeyDown : YAHOO.ext.tree.DefaultSelectionModel.prototype.onKeyDown
});
