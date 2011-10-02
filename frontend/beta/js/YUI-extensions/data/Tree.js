YAHOO.namespace('ext.data');

/**
 * @class YAHOO.ext.data.Tree
 * @extends YAHOO.ext.util.Observable
 * The class represents a tree data structure and bubbles all the events for it's nodes. The nodes
 * in the tree have most standard DOM functionality.
 * @constructor
 * @param {Node} root (optional) The root node 
 */
YAHOO.ext.data.Tree = function(root){
   this.nodeHash = {};
   this.root = null;
   if(root){
       this.setRootNode(root);
   }
   this.events = {
       'append' : true,
       'remove' : true,
       'move' : true,
       'insert' : true,
       'beforeappend' : true,
       'beforeremove' : true,
       'beforemove' : true,
       'beforeinsert' : true
   };
};

YAHOO.extendX(YAHOO.ext.data.Tree, YAHOO.ext.util.Observable, {
    pathSeparator: '/',
    
    getRootNode : function(){
        return this.root;
    },
    
    setRootNode : function(node){
        this.root = node;
        node.ownerTree = this;
        node.isRoot = true;
        return node;
    },
    
    getNodeById : function(id){
        return this.nodeHash[id];  
    },
    
    registerNode : function(node){
        this.nodeHash[node.id] = node;
    },
    
    unregisterNode : function(node){
        delete this.nodeHash[node.id];
    },
    
    toString : function(){
        return '[Tree'+(this.id?' '+this.id:'')+']';
    }  
});

/**
 * @class YAHOO.ext.tree.Node
 * @extends YAHOO.ext.util.Observable
 * @cfg {String} text The text for this node
 * @cfg {String} id The id for this node
 * @constructor
 * @param {Object} attributes The attributes/config for the node 
 */
YAHOO.ext.data.Node = function(attributes){
    this.attributes = attributes || {};
    this.leaf = this.attributes.leaf;
    this.id = this.attributes.id;
    if(!this.id){
        this.id = YAHOO.util.Dom.generateId(null, 'ynode-');
        this.attributes.id = this.id;
    }
    
    this.childNodes = [];
    if(!this.childNodes.indexOf){ // indexOf is a must
        this.childNodes.indexOf = function(o){
            for(var i = 0, len = this.length; i < len; i++){
                if(this[i] == o) return i;
            }
            return -1;
        };
    }
    this.parentNode = null;
    this.firstChild = null;
    this.lastChild = null;
    this.previousSibling = null;
    this.nextSibling = null;
    
    this.events = {
       'append' : true,
       'remove' : true,
       'move' : true,
       'insert' : true,
       'beforeappend' : true,
       'beforeremove' : true,
       'beforemove' : true,
       'beforeinsert' : true
   };
};

YAHOO.extendX(YAHOO.ext.data.Node, YAHOO.ext.util.Observable, {
    fireEvent : function(evtName){
        // first do standard event for this node
        if(YAHOO.ext.data.Node.superclass.fireEvent.apply(this, arguments) === false){
            return false;
        }
        // then bubble it up to the tree if the event wasn't cancelled
        if(this.ownerTree){
            if(this.ownerTree.fireEvent.apply(this.ownerTree, arguments) === false){
                return false;
            }
        }
        return true;
    },
    
    isLeaf : function(){
        return this.leaf === true;  
    },
    
    setFirstChild : function(node){
        this.firstChild = node;  
    },
    
    setLastChild : function(node){
        this.lastChild = node;
    },
    
    isLast : function(){
       return (!this.parentNode ? true : this.parentNode.lastChild == this);   
    },
    
    isFirst : function(){
       return (!this.parentNode ? true : this.parentNode.firstChild == this);   
    },
    
    hasChildNodes : function(){
        return !this.isLeaf() && this.childNodes.length > 0;
    },
    
    appendChild : function(node){
        var multi = false;
        if(node instanceof Array){
            multi = node;
        }else if(arguments.length > 1){
            multi = arguments;
        }
        // if passed an array or multiple args do them one by one
        if(multi){
            for(var i = 0, len = multi.length; i < len; i++) {
            	this.appendChild(multi[i]);
            }
        }else{
            if(this.fireEvent('beforeappend', this.ownerTree, this, node) === false){
                return false;
            }
            var index = this.childNodes.length;
            var oldParent = node.parentNode;
            // it's a move, make sure we move it cleanly
            if(oldParent){
                if(node.fireEvent('beforemove', node.getOwnerTree(), node, oldParent, this, index) === false){
                    return false;
                }
                oldParent.removeChild(node);
            }
            var index = this.childNodes.length;
            if(index == 0){
                this.setFirstChild(node);
            }
            this.childNodes.push(node);
            node.parentNode = this;
            var ps = this.childNodes[index-1];
            if(ps){
                node.previousSibling = ps;
                ps.nextSibling = node;
            }
            this.setLastChild(node);
            node.setOwnerTree(this.getOwnerTree());
            this.fireEvent('append', this.ownerTree, this, node, index);
            if(oldParent){
                node.fireEvent('move', this.ownerTree, node, oldParent, this, index);
            }
            return node;
        }
    },
    
    removeChild : function(node){
        var index = this.childNodes.indexOf(node);
        if(index == -1){
            return false;
        }
        if(this.fireEvent('beforeremove', this.ownerTree, this, node) === false){
            return false;
        }
            
        // remove it from childNodes collection
        this.childNodes.splice(index, 1);
        
        // update siblings
        if(node.previousSibling){
            node.previousSibling.nextSibling = node.nextSibling;
        }
        if(node.nextSibling){
            node.nextSibling.previousSibling = node.previousSibling;
        }
        
        // update child refs
        if(this.firstChild == node){
            this.setFirstChild(node.nextSibling);
        }
        if(this.lastChild == node){
            this.setLastChild(node.previousSibling);
        }
        
        node.setOwnerTree(null);
        // clear any references from the node
        node.parentNode = null;
        node.previousSibling = null;
        node.nextSibling = null;
        this.fireEvent('remove', this.ownerTree, this, node);
        return node;
    },
    
    insertBefore : function(node, refNode){
        if(!refNode){ // like standard Dom, refNode can be null for append
            return this.appendChild(node);
        }
        // nothing to do
        if(node == refNode){
            return false;
        }
        
        if(this.fireEvent('beforeinsert', this.ownerTree, this, node, refNode) === false){
            return false;
        }
        var index = this.childNodes.indexOf(refNode);
        var oldParent = node.parentNode;
        var refIndex = index;
        
        // when moving internally, indexes will change after remove
        if(oldParent == this && this.childNodes.indexOf(node) < index){
            refIndex--;
        }
        
        // it's a move, make sure we move it cleanly
        if(oldParent){
            if(node.fireEvent('beforemove', node.getOwnerTree(), node, oldParent, this, index, refNode) === false){
                return false;
            }
            oldParent.removeChild(node);
        }
        if(refIndex == 0){
            this.setFirstChild(node);
        }
        this.childNodes.splice(refIndex, 0, node);
        node.parentNode = this;
        var ps = this.childNodes[refIndex-1];
        if(ps){
            node.previousSibling = ps;
            ps.nextSibling = node;
        }
        node.nextSibling = refNode;
        node.setOwnerTree(this.getOwnerTree());
        this.fireEvent('insert', this.ownerTree, this, node, refNode);
        if(oldParent){
            node.fireEvent('move', this.ownerTree, node, oldParent, this, refIndex, refNode);
        }
        return node;
    },
    
    item : function(index){
        return this.childNodes[index];  
    },
    
    replaceChild : function(newChild, oldChild){
        this.insertBefore(newChild, oldChild);
        this.removeChild(oldChild);
        return oldChild;
    },
    
    indexOf : function(child){
        return this.childNodes.indexOf(child);  
    },
    
    getOwnerTree : function(){
        // if it doesn't have one, look for one
        if(!this.ownerTree){
            var p = this;
            while(p){
                if(p.ownerTree){
                    this.ownerTree = p.ownerTree;
                    break;
                }
                p = p.parentNode;
            }
        }
        return this.ownerTree;
    },
    
    setOwnerTree : function(tree){
        // if it's move, we need to update everyone
        if(tree != this.ownerTree){
            if(this.ownerTree){
                this.ownerTree.unregisterNode(this);
            }
            this.ownerTree = tree;
            var cs = this.childNodes;
            for(var i = 0, len = cs.length; i < len; i++) {
            	cs[i].setOwnerTree(tree);
            }
            if(tree){
                tree.registerNode(this);
            }
        }
    },
    
    getPath : function(attr){
        attr = attr || 'id';
        var p = this.parentNode;
        var b = [this.attributes[attr]];
        while(p){
            b.unshift(p.attributes[attr]);
            p = p.parentNode;
        }
        var sep = this.getOwnerTree().pathSeparator;
        return sep + b.join(sep);
    },
    
    bubble : function(fn, scope, args){
        var p = this;
        while(p){
            if(fn.call(scope || p, args || p) === false){
                break;
            }
            p = p.parentNode;
        }
    },
    
    cascade : function(fn, scope, args){
        if(fn.call(scope || this, args || this) !== false){
            var cs = this.childNodes;
            for(var i = 0, len = cs.length; i < len; i++) {
            	cs[i].cascade(fn, scope, args);
            }
        }
    },
    
    eachChild : function(fn, scope, args){
        var cs = this.childNodes;
        for(var i = 0, len = cs.length; i < len; i++) {
        	if(fn.call(scope || this, args || cs[i]) === false){
        	    break;
        	}
        }
    },
    
    findChild : function(attribute, value){
        var cs = this.childNodes;
        for(var i = 0, len = cs.length; i < len; i++) {
        	if(cs[i].attributes[attribute] == value){
        	    return cs[i];
        	}
        }
        return null;
    },
    
    /**
     * Sorts this nodes children using the supplied sort function
     * @param {Function} fn
     * @param {Object} scope
     */
    sort : function(fn, scope){
        var cs = this.childNodes;
        var len = cs.length;
        if(len > 0){
            var sortFn = scope ? function(){fn.apply(scope, arguments);} : fn;
            cs.sort(sortFn);
            for(var i = 0; i < len; i++){
                var n = cs[i];
                n.previousSibling = cs[i-1];
                n.nextSibling = cs[i+1];
                if(i == 0){
                    this.setFirstChild(n);
                }
                if(i == len-1){
                    this.setLastChild(n);
                }
            }
        }
    },
    
    contains : function(node){
        return node.isAncestor(this);
    },
    
    isAncestor : function(node){
        var p = this.parentNode;
        while(p){
            if(p == node){
                return true;
            }
            p = p.parentNode;
        }
        return false;
    },
    
    toString : function(){
        return '[Node'+(this.id?' '+this.id:'')+']';
    }
});
