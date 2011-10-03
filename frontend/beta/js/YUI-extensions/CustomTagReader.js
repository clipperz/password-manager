/**
 * @class YAHOO.ext.CustomTagReader
 * Utility class to normalize reading of custom tags across browsers.
 */
YAHOO.ext.CustomTagReader = function(namespace){
    this.namespace = namespace;
};
YAHOO.ext.CustomTagReader.prototype = {
    getAttribute : function(el, name, defaultValue){
        return (this.useNS ?
            v = el.getAttributeNS(this.namespace, name) : null) ||
            el.getAttribute(this.namespace+':'+name) ||
            el.getAttribute(name);
    },
    
    getElements : function(tagName, targetEl){
        targetEl = targetEl || document.body;
        var els;
        if(this.useNS){ // no namespaces in IE
           els = targetEl.getElementsByTagNameNS(this.namespace, tagName);
        }
        if(!els || els.length < 1){ // ie6, firefox 1.5, firefox 2 depending on doc type
           els = targetEl.getElementsByTagName(this.namespace+':'+tagName);
        }
        if(!els || els.length < 1){ // everyone else
           els = targetEl.getElementsByTagName(tagName);
        }
        return els;
    },
    
    eachElement : function(tagName, targetEl, fn, scope){
        var els = this.getElements(tagName, targetEl);
        for(var i = 0, len = els.length; i < len; i++) {
        	var el = els[i];
        	fn.call(scope || el, el);
        }  
    },
    
    useNS : (!YAHOO.ext.util.Browser.isIE && document.getElementsByTagNameNS) ? true : false
};
