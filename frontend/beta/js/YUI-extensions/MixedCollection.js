/**
 * @class YAHOO.ext.util.MixedCollection
 * A Collection class that maintains both numeric indexes and keys and exposes events.<br>
 * @constructor
 * @param {Boolean} allowFunctions True if the addAll function should add function references
 * to the collection.
 */
YAHOO.ext.util.MixedCollection = function(allowFunctions){
    this.items = [];
    this.keys = [];
    this.events = {
        /**
         * @event clear
         * Fires when the collection is cleared.
         */
        'clear' : new YAHOO.util.CustomEvent('clear'),
        /**
         * @event add
         * Fires when an item is added to the collection.
         * @param {Number} index The index at which the item was added.
         * @param {Object} o The item added.
         * @param {String} key The key associated with the added item.
         */
        'add' : new YAHOO.util.CustomEvent('add'),
        /**
         * @event replace
         * Fires when an item is replaced in the collection.
         * @param {String} key he key associated with the new added.
         * @param {Object} old The item being replaced.
         * @param {Object} new The new item.
         */
        'replace' : new YAHOO.util.CustomEvent('replace'),
        /**
         * @event remove
         * Fires when an item is removed from the collection.
         * @param {Object} o The item being removed.
         * @param {String} key (optional) The key associated with the removed item.
         */
        'remove' : new YAHOO.util.CustomEvent('remove')
    }
    this.allowFunctions = allowFunctions === true;
};

YAHOO.extendX(YAHOO.ext.util.MixedCollection, YAHOO.ext.util.Observable, {
    allowFunctions : false,
   
/**
 * Adds an item to the collection.
 * @param {String} key The key to associate with the item
 * @param {Object} o The item to add.
 * @return {Object} The item added.
 */
    add : function(key, o){
        if(arguments.length == 1){
            o = arguments[0];
            key = this.getKey(o);
        }
        this.items.push(o);
        if(typeof key != 'undefined' && key != null){
            this.items[key] = o;
            this.keys.push(key);
        }
        this.fireEvent('add', this.items.length-1, o, key);
        return o;
    },
   
/**
  * MixedCollection has a generic way to fetch keys if you implement getKey.
    <pre><code>
    // normal way
    var mc = new YAHOO.ext.util.MixedCollection();
    mc.add(someEl.dom.id, someEl);
    mc.add(otherEl.dom.id, otherEl);
    //and so on
    
    // using getKey
    var mc = new YAHOO.ext.util.MixedCollection();
    mc.getKey = function(el){
       return el.dom.id;
    }
    mc.add(someEl);
    mc.add(otherEl);
    // etc
    </code>
 * @param o {Object} The item for which to find the key.
 * @return {Object} The key for the passed item.
 */
    getKey : function(o){
         return null; 
    },
   
/**
 * Replaces an item in the collection.
 * @param {String} key The key associated with the item to replace, or the item to replace.
 * @param o {Object} o (optional) If the first parameter passed was a key, the item to associate with that key.
 * @return {Object}  The new item.
 */
    replace : function(key, o){
        if(arguments.length == 1){
            o = arguments[0];
            key = this.getKey(o);
        }
        if(typeof this.items[key] == 'undefined'){
            return this.add(key, o);
        }
        var old = this.items[key];
        if(typeof key == 'number'){ // array index key
            this.items[key] = o;
        }else{
            var index = this.indexOfKey(key);
            this.items[index] = o;
            this.items[key] = o;
        }
        this.fireEvent('replace', key, old, o);
        return o;
    },
   
/**
 * Adds all elements of an Array or an Object to the collection.
 * @param {Object/Array} objs An Object containing properties which will be added to the collection, or
 * an Array of values, each of which are added to the collection.
 */
    addAll : function(objs){
        if(arguments.length > 1 || objs instanceof Array){
            var args = arguments.length > 1 ? arguments : objs;
            for(var i = 0, len = args.length; i < len; i++){
                this.add(args[i]);
            }
        }else{
            for(var key in objs){
                if(this.allowFunctions || typeof objs[key] != 'function'){
                    this.add(objs[key], key);
                }
            }
        }
    },
   
/**
 * Executes the specified function once for every item in the collection, passing each
 * item as the first and only parameter.
 * @param {Function} fn The function to execute for each item.
 * @param {Object} scope (optional) The scope in which to execute the function.
 */
    each : function(fn, scope){
        for(var i = 0, len = this.items.length; i < len; i++){
            fn.call(scope || window, this.items[i]);
        }
    },
   
/**
 * Executes the specified function once for every key in the collection, passing each
 * key, and its associated item as the first two parameters.
 * @param {Function} fn The function to execute for each item.
 * @param {Object} scope (optional) The scope in which to execute the function.
 */
    eachKey : function(fn, scope){
        for(var i = 0, len = this.keys.length; i < len; i++){
            fn.call(scope || window, this.keys[i], this.items[i]);
        }
    },
   
/**
 * Returns the first item in the collection which elicits a true return value from the
 * passed selection function.
 * @param {Function} fn The selection function to execute for each item.
 * @param {Object} scope (optional) The scope in which to execute the function.
 * @return {Object} The first item in the collection which returned true from the selection function.
 */
    find : function(fn, scope){
        for(var i = 0, len = this.items.length; i < len; i++){
            if(fn.call(scope || window, this.items[i])){
                return this.items[i];
            }
        }
        return null;
    },
   
/**
 * Inserts an item at the specified index in the collection.
 * @param {Number} index The index to insert the item at.
 * @param {String} key The key to associate with the new item, or the item itself.
 * @param {Object} o  (optional) If the second parameter was a key, the new item.
 * @return {Object} The item inserted.
 */
    insert : function(index, key, o){
        if(arguments.length == 2){
            o = arguments[1];
            key = this.getKey(o);
        }
        if(index >= this.items.length){
            return this.add(o, key);
        }
        this.items.splice(index, 0, o);
        if(typeof key != 'undefined' && key != null){
            this.items[key] = o;
            this.keys.splice(index, 0, key);
        }
        this.fireEvent('add', index, o, key);
        return o;
    },
   
/**
 * Removed an item from the collection.
 * @param {Object} o The item to remove.
 * @return {Object} The item removed.
 */
    remove : function(o){
        var index = this.indexOf(o);
        this.items.splice(index, 1);
        if(typeof this.keys[index] != 'undefined'){
            var key = this.keys[index];
            this.keys.splice(index, 1);
            delete this.items[key];
        }
        this.fireEvent('remove', o);
        return o;
    },
   
/**
 * Remove an item from a specified index in the collection.
 * @param {Number} index The index within the collection of the item to remove.
 */
    removeAt : function(index){
        this.items.splice(index, 1);
        var key = this.keys[index];
        if(typeof key != 'undefined'){
             this.keys.splice(index, 1);
             delete this.items[key];
        }
        this.fireEvent('remove', o, key);
    },
   
/**
 * Removed an item associated with the passed key fom the collection.
 * @param {String} key The key of the item to remove.
 */
    removeKey : function(key){
        var o = this.items[key];
        var index = this.indexOf(o);
        this.items.splice(index, 1);
        this.keys.splice(index, 1);
        delete this.items[key];
        this.fireEvent('remove', o, key);
    },
   
/**
 * Returns the number of items in the collection.
 * @return {Number} the number of items in the collection.
 */
    getCount : function(){
        return this.items.length; 
    },
   
/**
 * Returns index within the collection of the passed Object.
 * @param {Object} o The item to find the index of.
 * @return {Number} index of the item.
 */
    indexOf : function(o){
        if(!this.items.indexOf){
            for(var i = 0, len = this.items.length; i < len; i++){
                if(this.items[i] == o) return i;
            }
            return -1;
        }else{
            return this.items.indexOf(o);
        }
    },
   
/**
 * Returns index within the collection of the passed key.
 * @param {String} key The key to find the index of.
 * @return {Number} index of the key.
 */
    indexOfKey : function(key){
        if(!this.keys.indexOf){
            for(var i = 0, len = this.keys.length; i < len; i++){
                if(this.keys[i] == key) return i;
            }
            return -1;
        }else{
            return this.keys.indexOf(key);
        }
    },
   
/**
 * Returns the item associated with the passed key.
 * @param {String/Number} key The key or index of the item.
 * @return {Object} The item associated with the passed key.
 */
    item : function(key){
        return this.items[key];
    },
   
/**
 * Returns true if the collection contains the passed Object as an item.
 * @param {Object} o  The Object to look for in the collection.
 * @return {Boolean} True if the collection contains the Object as an item.
 */
    contains : function(o){
        return this.indexOf(o) != -1;
    },
   
/**
 * Returns true if the collection contains the passed Object as a key.
 * @param {String} key The key to look for in the collection.
 * @return {Boolean} True if the collection contains the Object as a key.
 */
    containsKey : function(key){
        return typeof this.items[key] != 'undefined';
    },
   
/**
 * Removes all items from the collection.
 */
    clear : function(o){
        this.items = [];
        this.keys = [];
        this.fireEvent('clear');
    },
   
/**
 * Returns the first item in the collection.
 * @return {Object} the first item in the collection..
 */
    first : function(){
        return this.items[0]; 
    },
   
/**
 * Returns the last item in the collection.
 * @return {Object} the last item in the collection..
 */
    last : function(){
        return this.items[this.items.length];   
    }
});
/**
 * Returns the item associated with the passed key or index.
 * @method
 * @param {String/Number} key The key or index of the item.
 * @return {Object} The item associated with the passed key.
 */
YAHOO.ext.util.MixedCollection.prototype.get = YAHOO.ext.util.MixedCollection.prototype.item;
