YAHOO.ext.tree.TreeLoader = function(config){
    this.baseParams = {};
    this.requestMethod = 'POST';
    YAHOO.ext.util.Config.apply(this, config);
    
    this.events = {
        'beforeload' : true,
        'load' : true,
        'loadexception' : true  
    };
};

YAHOO.extendX(YAHOO.ext.tree.TreeLoader, YAHOO.ext.util.Observable, {
    load : function(node, callback){
        if(node.attributes.children){ // preloaded json children
            var cs = node.attributes.children;
            for(var i = 0, len = cs.length; i < len; i++){
                node.appendChild(this.createNode(cs[i]));
            }
            if(typeof callback == 'function'){
                callback();
            }
        }else if(this.dataUrl){
            this.requestData(node, callback);
        }
    },
    
    getParams: function(node){
        var buf = [], bp = this.baseParams;
        for(var key in bp){
            if(typeof bp[key] != 'function'){
                buf.push(encodeURIComponent(key), '=', encodeURIComponent(bp[key]), '&');
            }
        }
        buf.push('node=', encodeURIComponent(node.id));
        return buf.join('');
    },
    
    requestData : function(node, callback){
        if(this.fireEvent('beforeload', this, node, callback) !== false){
            var params = this.getParams(node);
            var cb = {
                success: this.handleResponse,
                failure: this.handleFailure,
                scope: this,
        		argument: {callback: callback, node: node}
            };
            this.transId = YAHOO.util.Connect.asyncRequest(this.requestMethod, this.dataUrl, cb, params);
        }else{
            // if the load is cancelled, make sure we notify 
            // the node that we are done
            if(typeof callback == 'function'){
                callback();
            }
        }
    },
    
    isLoading : function(){
        return this.transId ? true : false;  
    },
    
    abort : function(){
        if(this.isLoading()){
            YAHOO.util.Connect.abort(this.transId);
        }
    },
    
    createNode : function(attr){
        if(this.applyLoader !== false){
            attr.loader = this;
        }
        return(attr.leaf ? 
                        new YAHOO.ext.tree.TreeNode(attr) : 
                        new YAHOO.ext.tree.AsyncTreeNode(attr));  
    },
    
    processResponse : function(response, node, callback){
        var json = response.responseText;
        try {
            var o = eval('('+json+')');
	        for(var i = 0, len = o.length; i < len; i++){
	            node.appendChild(this.createNode(o[i])); 
	        }
	        if(typeof callback == 'function'){
                callback();
            }
        }catch(e){
            this.handleFailure(response);
        }
    },
    
    handleResponse : function(response){
        this.transId = false;
        var a = response.argument;
        this.processResponse(response, a.node, a.callback);
        this.fireEvent('load', this, a.node, response);
    },
    
    handleFailure : function(response){
        this.transId = false;
        var a = response.argument;
        this.fireEvent('loadexception', this, a.node, response);
        if(typeof a.callback == 'function'){
            a.callback();
        }
    }
});
