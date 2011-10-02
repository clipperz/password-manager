YAHOO.ext.tree.AsyncTreeNode = function(config){
    this.loaded = false;
    this.loading = false;
    YAHOO.ext.tree.AsyncTreeNode.superclass.constructor.apply(this, arguments);
    this.events['beforeload'] = true;
    this.events['load'] = true;
};
YAHOO.extendX(YAHOO.ext.tree.AsyncTreeNode, YAHOO.ext.tree.TreeNode, {
    expand : function(deep, anim, callback){
        if(this.loading){ // if an async load is already running, waiting til it's done
            var timer;
            var f = function(){
                if(!this.loading){ // done loading
                    clearInterval(timer);
                    this.expand(deep, anim, callback);
                }
            }.createDelegate(this);
            timer = setInterval(f, 200);
        }
        if(!this.loaded){
            if(this.fireEvent('beforeload', this) === false){
                return;
            }
            this.loading = true;
            this.ui.beforeLoad(this);
            var loader = this.loader || this.attributes.loader || this.getOwnerTree().getLoader();
            if(loader){
                loader.load(this, this.loadComplete.createDelegate(this, [deep, anim, callback]));
                return;
            }
        }
        YAHOO.ext.tree.AsyncTreeNode.superclass.expand.call(this, deep, anim, callback);
    },
    
    isLoading : function(){
        return this.loading;  
    },
    
    loadComplete : function(deep, anim, callback){
        this.loading = false;
        this.loaded = true;
        this.ui.afterLoad(this);
        this.fireEvent('load', this);
        this.expand(deep, anim, callback);
    },
    
    isLoaded : function(){
        return this.loaded;
    },
    
    hasChildNodes : function(){
        if(!this.isLeaf() && !this.loaded){
            return true;
        }else{
            return YAHOO.ext.tree.AsyncTreeNode.superclass.hasChildNodes.call(this);
        }
    }
});
