YAHOO.ext.tree.TreeSorter = function(tree, config){
    YAHOO.ext.util.Config.apply(this, config);
    tree.on('beforechildrenrendered', this.doSort, this, true);
    tree.on('append', this.updateSort, this, true);
    tree.on('insert', this.updateSort, this, true);
    
    var dsc = this.dir && this.dir.toLowerCase() == 'desc';
    var p = this.property || 'text';
    var sortType = this.sortType;
    var fs = this.folderSort;
    var cs = this.caseSensitive === true;
    
    this.sortFn = function(n1, n2){
        if(fs){
            if(n1.leaf && !n2.leaf){
                return 1;
            }
            if(!n1.leaf && n2.leaf){
                return -1;
            }
        }
    	var v1 = sortType ? sortType(n1) : (cs ? n1[p] : n1[p].toUpperCase());
    	var v2 = sortType ? sortType(n2) : (cs ? n2[p] : n2[p].toUpperCase());
    	if(v1 < v2){
			return dsc ? +1 : -1;
		}else if(v1 > v2){
			return dsc ? -1 : +1;
        }else{
	    	return 0;
        }
    };
};

YAHOO.ext.tree.TreeSorter.prototype = {
    doSort : function(node){
        node.sort(this.sortFn);
    },
    
    compareNodes : function(n1, n2){
        
        return (n1.text.toUpperCase() > n2.text.toUpperCase() ? 1 : -1);
    },
    
    updateSort : function(tree, node){
        if(node.childrenRendered){
            this.doSort.defer(1, this, [node]);
        }
    }
};
