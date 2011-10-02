/**
 * @class YAHOO.ext.DomHelper
 * Utility class for working with DOM and/or Templates. It transparently supports using HTML fragments or DOM. 
 * For more information see <a href="http://www.jackslocum.com/yui/2006/10/06/domhelper-create-elements-using-dom-html-fragments-or-templates/">this blog post with examples</a>.
 * @singleton
 */
YAHOO.ext.DomHelper = new function(){
    /**@private*/
    var d = document;
    var tempTableEl = null;
    /** True to force the use of DOM instead of html fragments @type Boolean */
    this.useDom = false;
    var emptyTags = /^(?:base|basefont|br|frame|hr|img|input|isindex|link|meta|nextid|range|spacer|wbr|audioscope|area|param|keygen|col|limittext|spot|tab|over|right|left|choose|atop|of)$/i;
    /**
     * Applies a style specification to an element
     * @param {String/HTMLElement} el The element to apply styles to
     * @param {String/Object/Function} styles A style specification string eg "width:100px", or object in the form {width:"100px"}, or
     * a function which returns such a specification.
     */
    this.applyStyles = function(el, styles){
        if(styles){
           var D = YAHOO.util.Dom;
           if (typeof styles == "string"){
               var re = /\s?([a-z\-]*)\:([^;]*);?/gi;
               var matches;
               while ((matches = re.exec(styles)) != null){
                   D.setStyle(el, matches[1], matches[2]);
               }
           }else if (typeof styles == "object"){
               for (var style in styles){
                  D.setStyle(el, style, styles[style]);
               }
           }else if (typeof styles == "function"){
                YAHOO.ext.DomHelper.applyStyles(el, styles.call());
           }
        }
    }; 
    
    // build as innerHTML where available
    /** @ignore */
    var createHtml = function(o){
        var b = '';
        b += '<' + o.tag;
        for(var attr in o){
            if(attr == 'tag' || attr == 'children' || attr == 'html' || typeof o[attr] == 'function') continue;
            if(attr == 'style'){
                var s = o['style'];
                if(typeof s == 'function'){
                    s = s.call();
                }
                if(typeof s == 'string'){
                    b += ' style="' + s + '"';
                }else if(typeof s == 'object'){
                    b += ' style="';
                    for(var key in s){
                        if(typeof s[key] != 'function'){
                            b += key + ':' + s[key] + ';';
                        }
                    }
                    b += '"';
                }
            }else{
                if(attr == 'cls'){
                    b += ' class="' + o['cls'] + '"';
                }else if(attr == 'htmlFor'){
                    b += ' for="' + o['htmlFor'] + '"';
                }else{
                    b += ' ' + attr + '="' + o[attr] + '"';
                }
            }
        }
        if(emptyTags.test(o.tag)){
            b += ' />';
        }else{
            b += '>';
            if(o.children){
                for(var i = 0, len = o.children.length; i < len; i++) {
                    b += createHtml(o.children[i], b);
                }
            }
            if(o.html){
                b += o.html;
            }
            b += '</' + o.tag + '>';
        }
        return b;
    }
    
    // build as dom
    /** @ignore */
    var createDom = function(o, parentNode){
        var el = d.createElement(o.tag);
        var useSet = el.setAttribute ? true : false; // In IE some elements don't have setAttribute
        for(var attr in o){
            if(attr == 'tag' || attr == 'children' || attr == 'html' || attr == 'style' || typeof o[attr] == 'function') continue;
            if(attr=='cls'){
                el.className = o['cls'];
            }else{
                if(useSet) el.setAttribute(attr, o[attr]);
                else el[attr] = o[attr];
            }
        }
        YAHOO.ext.DomHelper.applyStyles(el, o.style);
        if(o.children){
            for(var i = 0, len = o.children.length; i < len; i++) {
             	createDom(o.children[i], el);
            }
        }
        if(o.html){
            el.innerHTML = o.html;
        }
        if(parentNode){
           parentNode.appendChild(el);
        }
        return el;
    };
    
    /**
     * @ignore
     * Nasty code for IE's broken table implementation 
     */
    var insertIntoTable = function(tag, where, el, html){
        if(!tempTableEl){
            tempTableEl = document.createElement('div');
        }
        var node;
        if(tag == 'table' || tag == 'tbody'){
           tempTableEl.innerHTML = '<table><tbody>'+html+'</tbody></table>';
           node = tempTableEl.firstChild.firstChild.firstChild;
        }else{
           tempTableEl.innerHTML = '<table><tbody><tr>'+html+'</tr></tbody></table>';
           node = tempTableEl.firstChild.firstChild.firstChild.firstChild;
        }
        if(where == 'beforebegin'){
            el.parentNode.insertBefore(node, el);
            return node;
        }else if(where == 'afterbegin'){
            el.insertBefore(node, el.firstChild);
            return node;
        }else if(where == 'beforeend'){
            el.appendChild(node);
            return node;
        }else if(where == 'afterend'){
            el.parentNode.insertBefore(node, el.nextSibling);
            return node;
        }
    } 
    
    /**
     * Inserts an HTML fragment into the Dom
     * @param {String} where Where to insert the html in relation to el - beforeBegin, afterBegin, beforeEnd, afterEnd.
     * @param {HTMLElement} el The context element
     * @param {String} html The HTML fragmenet
     * @return {HTMLElement} The new node
     */
    this.insertHtml = function(where, el, html){
        where = where.toLowerCase();
        if(el.insertAdjacentHTML){
            var tag = el.tagName.toLowerCase();
            if(tag == 'table' || tag == 'tbody' || tag == 'tr'){
               return insertIntoTable(tag, where, el, html);
            }
            switch(where){
                case 'beforebegin':
                    el.insertAdjacentHTML(where, html);
                    return el.previousSibling;
                case 'afterbegin':
                    el.insertAdjacentHTML(where, html);
                    return el.firstChild;
                case 'beforeend':
                    el.insertAdjacentHTML(where, html);
                    return el.lastChild;
                case 'afterend':
                    el.insertAdjacentHTML(where, html);
                    return el.nextSibling;
            }
            throw 'Illegal insertion point -> "' + where + '"';
        }
        var range = el.ownerDocument.createRange();
        var frag;
        switch(where){
             case 'beforebegin':
                range.setStartBefore(el);
                frag = range.createContextualFragment(html);
                el.parentNode.insertBefore(frag, el);
                return el.previousSibling;
             case 'afterbegin':
                if(el.firstChild){ // faster
                    range.setStartBefore(el.firstChild);
                }else{
                    range.selectNodeContents(el);
                    range.collapse(true);
                }
                frag = range.createContextualFragment(html);
                el.insertBefore(frag, el.firstChild);
                return el.firstChild;
            case 'beforeend':
                if(el.lastChild){
                    range.setStartAfter(el.lastChild); // faster
                }else{
                    range.selectNodeContents(el);
                    range.collapse(false);
                }
                frag = range.createContextualFragment(html);
                el.appendChild(frag);
                return el.lastChild;
            case 'afterend':
                range.setStartAfter(el);
                frag = range.createContextualFragment(html);
                el.parentNode.insertBefore(frag, el.nextSibling);
                return el.nextSibling;
            }
            throw 'Illegal insertion point -> "' + where + '"';
    };
    
    /**
     * Creates new Dom element(s) and inserts them before el
     * @param {String/HTMLElement/Element} el The context element
     * @param {Object} o The Dom object spec (and children)
     * @param {<i>Boolean</i>} returnElement (optional) true to return a YAHOO.ext.Element
     * @return {HTMLElement} The new node
     */
    this.insertBefore = function(el, o, returnElement){
        el = el.dom ? el.dom : YAHOO.util.Dom.get(el);
        var newNode;
        if(this.useDom){
            newNode = createDom(o, null);
            el.parentNode.insertBefore(newNode, el);
        }else{
            var html = createHtml(o);
            newNode = this.insertHtml('beforeBegin', el, html);
        }
        return returnElement ? YAHOO.ext.Element.get(newNode, true) : newNode;
    };
    
    /**
     * Creates new Dom element(s) and inserts them after el
     * @param {String/HTMLElement/Element} el The context element
     * @param {Object} o The Dom object spec (and children)
     * @param {<i>Boolean</i>} returnElement (optional) true to return a YAHOO.ext.Element
     * @return {HTMLElement} The new node
     */
    this.insertAfter = function(el, o, returnElement){
        el = el.dom ? el.dom : YAHOO.util.Dom.get(el);
        var newNode;
        if(this.useDom){
            newNode = createDom(o, null);
            el.parentNode.insertBefore(newNode, el.nextSibling);
        }else{
            var html = createHtml(o);
            newNode = this.insertHtml('afterEnd', el, html);
        }
        return returnElement ? YAHOO.ext.Element.get(newNode, true) : newNode;
    };
    
    /**
     * Creates new Dom element(s) and appends them to el
     * @param {String/HTMLElement/Element} el The context element
     * @param {Object} o The Dom object spec (and children)
     * @param {<i>Boolean</i>} returnElement (optional) true to return a YAHOO.ext.Element
     * @return {HTMLElement} The new node
     */
    this.append = function(el, o, returnElement){
        el = el.dom ? el.dom : YAHOO.util.Dom.get(el);
        var newNode;
        if(this.useDom){
            newNode = createDom(o, null);
            el.appendChild(newNode);
        }else{
            var html = createHtml(o);
            newNode = this.insertHtml('beforeEnd', el, html);
        }
        return returnElement ? YAHOO.ext.Element.get(newNode, true) : newNode;
    };
    
    /**
     * Creates new Dom element(s) and overwrites the contents of el with them
     * @param {String/HTMLElement/Element} el The context element
     * @param {Object} o The Dom object spec (and children)
     * @param {<i>Boolean</i>} returnElement (optional) true to return a YAHOO.ext.Element
     * @return {HTMLElement} The new node
     */
    this.overwrite = function(el, o, returnElement){
        el = el.dom ? el.dom : YAHOO.util.Dom.get(el);
        el.innerHTML = createHtml(o);
        return returnElement ? YAHOO.ext.Element.get(el.firstChild, true) : el.firstChild;
    };
    
    /**
     * Creates a new YAHOO.ext.DomHelper.Template from the Dom object spec 
     * @param {Object} o The Dom object spec (and children)
     * @return {YAHOO.ext.DomHelper.Template} The new template
     */
    this.createTemplate = function(o){
        var html = createHtml(o);
        return new YAHOO.ext.DomHelper.Template(html);
    };
}();

/**
* @class YAHOO.ext.DomHelper.Template
* Represents an HTML fragment template.
* For more information see <a href="http://www.jackslocum.com/yui/2006/10/06/domhelper-create-elements-using-dom-html-fragments-or-templates/">this blog post with examples</a>. 
* <br>
* <b>This class is also available as YAHOO.ext.Template</b>.
* @constructor
* @param {String/Array} html The HTML fragment or an array of fragments to join('') or multiple arguments to join('')
*/
YAHOO.ext.DomHelper.Template = function(html){
    if(html instanceof Array){
        html = html.join(''); 
    }else if(arguments.length > 1){
        html = Array.prototype.join.call(arguments, '');
    }
    /**@private*/
    this.html = html;
};
YAHOO.ext.DomHelper.Template.prototype = {
    /**
     * Returns an HTML fragment of this template with the specified values applied
     * @param {Object} values The template values. Can be an array if your params are numeric (i.e. {0}) or an object (i.e. {foo: 'bar'})
     * @return {String}
     */
    applyTemplate : function(values){
        if(this.compiled){
            return this.compiled(values);
        }
        var empty = '';
        var fn = function(match, index){
            if(typeof values[index] != 'undefined'){
                return values[index];
            }else{
                return empty;
            }
        }
        return this.html.replace(this.re, fn);
    },
    
    /**
    * The regular expression used to match template variables 
    * @type RegExp
    * @property 
    */
    re : /\{([\w|-]+)\}/g,
    
    /**
     * Compiles the template into an internal function, eliminating the RegEx overhead
     */
    compile : function(){
        var body = ["this.compiled = function(values){ return ['"];
        body.push(this.html.replace(this.re, "', values['$1'], '"));
        body.push("'].join('');};");
        eval(body.join(''));
        return this;
    },
   
    /**
     * Applies the supplied values to the template and inserts the new node(s) before el
     * @param {String/HTMLElement/Element} el The context element
     * @param {Object} values The template values. Can be an array if your params are numeric (i.e. {0}) or an object (i.e. {foo: 'bar'})
     * @param {<i>Boolean</i>} returnElement (optional) true to return a YAHOO.ext.Element
     * @return {HTMLElement} The new node
     */
    insertBefore: function(el, values, returnElement){
        el = el.dom ? el.dom : YAHOO.util.Dom.get(el);
        var newNode = YAHOO.ext.DomHelper.insertHtml('beforeBegin', el, this.applyTemplate(values));
        return returnElement ? YAHOO.ext.Element.get(newNode, true) : newNode;
    },
    
    /**
     * Applies the supplied values to the template and inserts the new node(s) after el
     * @param {String/HTMLElement/Element} el The context element
     * @param {Object} values The template values. Can be an array if your params are numeric (i.e. {0}) or an object (i.e. {foo: 'bar'})
     * @param {<i>Boolean</i>} returnElement (optional) true to return a YAHOO.ext.Element
     * @return {HTMLElement} The new node
     */
    insertAfter : function(el, values, returnElement){
        el = el.dom ? el.dom : YAHOO.util.Dom.get(el);
        var newNode = YAHOO.ext.DomHelper.insertHtml('afterEnd', el, this.applyTemplate(values));
        return returnElement ? YAHOO.ext.Element.get(newNode, true) : newNode;
    },
    
    /**
     * Applies the supplied values to the template and append the new node(s) to el
     * @param {String/HTMLElement/Element} el The context element
     * @param {Object} values The template values. Can be an array if your params are numeric (i.e. {0}) or an object (i.e. {foo: 'bar'})
     * @param {<i>Boolean</i>} returnElement (optional) true to return a YAHOO.ext.Element
     * @return {HTMLElement} The new node
     */
    append : function(el, values, returnElement){
        el = el.dom ? el.dom : YAHOO.util.Dom.get(el);
        var newNode = YAHOO.ext.DomHelper.insertHtml('beforeEnd', el, this.applyTemplate(values));
        return returnElement ? YAHOO.ext.Element.get(newNode, true) : newNode;
    },
    
    /**
     * Applies the supplied values to the template and overwrites the content of el with the new node(s)
     * @param {String/HTMLElement/Element} el The context element
     * @param {Object} values The template values. Can be an array if your params are numeric (i.e. {0}) or an object (i.e. {foo: 'bar'})
     * @param {<i>Boolean</i>} returnElement (optional) true to return a YAHOO.ext.Element
     * @return {HTMLElement} The new node
     */
    overwrite : function(el, values, returnElement){
        el = el.dom ? el.dom : YAHOO.util.Dom.get(el);
        el.innerHTML = '';
        var newNode = YAHOO.ext.DomHelper.insertHtml('beforeEnd', el, this.applyTemplate(values));
        return returnElement ? YAHOO.ext.Element.get(newNode, true) : newNode;
    }
};
/**
 * Alias for applyTemplate
 * @method
 */
YAHOO.ext.DomHelper.Template.prototype.apply = YAHOO.ext.DomHelper.Template.prototype.applyTemplate;

YAHOO.ext.Template = YAHOO.ext.DomHelper.Template;
