/**
 * @class YAHOO.ext.grid.XMLDataModel
 * This is an implementation of a DataModel used by the Grid. It works 
 * with XML data. 
 * <br>Example schema from Amazon search:
 * <pre><code>
 * var schema = {
 *     tagName: 'Item',
 *     id: 'ASIN',
 *     fields: ['Author', 'Title', 'Manufacturer', 'ProductGroup']
 * };
 * </code></pre>
 * @extends YAHOO.ext.grid.LoadableDataModel
 * @constructor
 * @param {Object} schema The schema to use
 * @param {XMLDocument} xml An XML document to load immediately
*/
YAHOO.ext.grid.XMLDataModel = function(schema, xml){
    YAHOO.ext.grid.XMLDataModel.superclass.constructor.call(this, YAHOO.ext.grid.LoadableDataModel.XML);
    /**@private*/
    this.schema = schema;
    this.xml = xml;
    if(xml){
        this.loadData(xml);
    }
    this.idSeed = 0;
};
YAHOO.extendX(YAHOO.ext.grid.XMLDataModel, YAHOO.ext.grid.LoadableDataModel, {
    
    getDocument: function(){
       return this.xml;    
    },
    
    /**
     * Overrides loadData in LoadableDataModel to process XML
     * @param {XMLDocument} doc The document to load
     * @param {<i>Function</i>} callback (optional) callback to call when loading is complete
     * @param {<i>Boolean</i>} keepExisting (optional) true to keep existing data
     * @param {<i>Number</i>} insertIndex (optional) if present, loaded data is inserted at the specified index instead of overwriting existing data
     */
    loadData: function(doc, callback, keepExisting, insertIndex){
    	this.xml = doc;
    	var idField = this.schema.id;
    	var fields = this.schema.fields;
    	if(this.schema.totalTag){
    	    this.totalCount = null;
    	    var totalNode = doc.getElementsByTagName(this.schema.totalTag);
    	    if(totalNode && totalNode.item(0) && totalNode.item(0).firstChild) {
                var v = parseInt(totalNode.item(0).firstChild.nodeValue, 10);
                if(!isNaN(v)){
                    this.totalCount = v;
                }
        	}
    	}
    	var rowData = [];
    	var nodes = doc.getElementsByTagName(this.schema.tagName);
        if(nodes && nodes.length > 0) {
    	    for(var i = 0; i < nodes.length; i++) {
    	        var node = nodes.item(i);
    	        var colData = [];
    	        colData.node = node;
    	        colData.id = this.getNamedValue(node, idField, String(++this.idSeed));
    	        for(var j = 0; j < fields.length; j++) {
    	            var val = this.getNamedValue(node, fields[j], "");
    	            if(this.preprocessors[j]){
    	                val = this.preprocessors[j](val);
    	            }
    	            colData.push(val);
    	        }
    	        rowData.push(colData);
    	    }
        }
        if(keepExisting !== true){
           YAHOO.ext.grid.XMLDataModel.superclass.removeAll.call(this);
    	}
    	if(typeof insertIndex != 'number'){
    	    insertIndex = this.getRowCount();
    	}
        YAHOO.ext.grid.XMLDataModel.superclass.insertRows.call(this, insertIndex, rowData);
        if(typeof callback == 'function'){
        	callback(this, true);
        }
        this.fireLoadEvent();
    },
    
    /**
     * Adds a row to this DataModel and syncs the XML document
     * @param {String} id The id of the row, if null the next row index is used
     * @param {Array} cellValues The cell values for this row
     * @return {Number} The index of the new row (if the model is sorted this index may not be accurate)
     */
    addRow: function(id, cellValues){
        var node = this.createNode(this.xml, id, cellValues);
        cellValues.id = id || ++this.idSeed;
        cellValues.node = node;
        return YAHOO.ext.grid.XMLDataModel.superclass.addRow.call(this, cellValues);
    },
    
    /**
     * Inserts a row into this DataModel and syncs the XML document
     * @param {Number} index The index to insert the row
     * @param {String} id The id of the row, if null the next row index is used
     * @param {Array} cellValues The cell values for this row
     * @return {Number} The index of the new row (if the model is sorted this index may not be accurate)
     */
    insertRow: function(index, id, cellValues){
        var node = this.createNode(this.xml, id, cellValues);
        cellValues.id = id || ++this.idSeed;
        cellValues.node = node;
        return YAHOO.ext.grid.XMLDataModel.superclass.insertRow.call(this, index, cellValues);
    },
    
    /**
     * Removes the row from DataModel and syncs the XML document
     * @param {Number} index The index of the row to remove
     */
    removeRow: function(index){
        var node = this.data[index].node;
        node.parentNode.removeChild(node);
        YAHOO.ext.grid.XMLDataModel.superclass.removeRow.call(this, index, index);
    },
    
    getNode: function(rowIndex){
        return this.data[rowIndex].node;
    },
    
    /**
     * Override this method to define your own node creation routine for when new rows are added.
     * By default this method clones the first node and sets the column values in the newly cloned node.
     * In many instances this will not work and you will have to create the node manually.
     * @param {XMLDocument} xmlDoc The xml document being used by this model
     * @param {String/Number} id The row id
     * @param {Array} colData The column data for the new node
     * @return {XMLNode} The created node
     */
    createNode: function(xmlDoc, id, colData){
        var template = this.data[0].node;
        var newNode = template.cloneNode(true);
        var fields = this.schema.fields;
        for(var i = 0, len = fields.length; i < len; i++){
            var nodeValue = colData[i];
            if(this.postprocessors[i]){
                nodeValue = this.postprocessors[i](nodeValue);
            }
            this.setNamedValue(newNode, fields[i], nodeValue);
        }
        if(id){
            this.setNamedValue(newNode, this.schema.idField, id);
        }
        template.parentNode.appendChild(newNode);
        return newNode;
    },
    
    /**
     * @private
     * Convenience function looks for value in attributes, then in children tags - also 
     * normalizes namespace matches (ie matches ns:tag, FireFox matches tag and not ns:tag).
     */
    getNamedValue: function(node, name, defaultValue){
    	if(!node || !name){
    		return defaultValue;
    	}
    	var nodeValue = defaultValue;
        var attrNode = node.attributes.getNamedItem(name);
        if(attrNode) {
        	nodeValue = attrNode.value;
        } else {
            var childNode = node.getElementsByTagName(name);
            if(childNode && childNode.item(0) && childNode.item(0).firstChild) {
                nodeValue = childNode.item(0).firstChild.nodeValue;
        	}else{
        	    // try to strip namespace for FireFox
        	    var index = name.indexOf(':');
        	    if(index > 0){
        	        return this.getNamedValue(node, name.substr(index+1), defaultValue);
        	    }
        	}
        }
        return nodeValue;
    },
    
    /**
     * @private
     * Convenience function set a value in the underlying xml node.
     */
    setNamedValue: function(node, name, value){
    	if(!node || !name){
    		return;
    	}
    	var attrNode = node.attributes.getNamedItem(name);
        if(attrNode) {
        	attrNode.value = value;
        	return;
        }
        var childNode = node.getElementsByTagName(name);
        if(childNode && childNode.item(0) && childNode.item(0).firstChild) {
            childNode.item(0).firstChild.nodeValue = value;
        }else{
    	    // try to strip namespace for FireFox
    	    var index = name.indexOf(':');
    	    if(index > 0){
    	        this.setNamedValue(node, name.substr(index+1), value);
    	    }
    	}
    },
    
    /**
     * Overrides DefaultDataModel.setValueAt to update the underlying XML Document
     * @param {Object} value The new value
     * @param {Number} rowIndex
     * @param {Number} colIndex
     */
    setValueAt: function(value, rowIndex, colIndex){
        var node = this.data[rowIndex].node;
        if(node){
            var nodeValue = value;
            if(this.postprocessors[colIndex]){
                nodeValue = this.postprocessors[colIndex](value);
            }
            this.setNamedValue(node, this.schema.fields[colIndex], nodeValue);
        }
        YAHOO.ext.grid.XMLDataModel.superclass.setValueAt.call(this, value, rowIndex, colIndex);
    },
    
    /**
     * Overrides getRowId in DefaultDataModel to return the ID value of the specified node. 
     * @param {Number} rowIndex
     * @return {Number}
     */
    getRowId: function(rowIndex){
        return this.data[rowIndex].id;
    },
    
    addRows : function(rowData){   
        for(var j = 0, len = rowData.length; j < len; j++){
           var cellValues = rowData[j];
           var id = ++this.idSeed; 
           var node = this.createNode(this.xml, id, cellValues);       
           cellValues.node=node;
           cellValues.id = cellValues.id || id;
           YAHOO.ext.grid.XMLDataModel.superclass.addRow.call(this,cellValues);
        }
    },   

   insertRows : function(index, rowData){
       // copy original array so it is not reversed 
       rowData = rowData.slice(0).reverse();
       for(var j = 0, len = rowData.length; j < len; j++){
          var cellValues = rowData[j];
          var id = ++this.idSeed; 
          var node = this.createNode(this.xml, id, cellValues);
          cellValues.id = cellValues.id || id;
          cellValues.node = node;
          YAHOO.ext.grid.XMLDataModel.superclass.insertRow.call(this, index, cellValues);
       }
   }
});

YAHOO.ext.grid.XMLQueryDataModel = function(){
   YAHOO.ext.grid.XMLQueryDataModel.superclass.constructor.apply(this, arguments);
};
YAHOO.extendX(YAHOO.ext.grid.XMLQueryDataModel, YAHOO.ext.grid.XMLDataModel, {
    getNamedValue: function(node, name, defaultValue){
    	if(!node || !name){
    		return defaultValue;
    	}
    	var nodeValue = defaultValue;
    	var childNode = cssQuery(name, node);
    	if(childNode && childNode[0]) {
            nodeValue = childNode[0].firstChild.nodeValue;
    	}
        return nodeValue;
    }
});
