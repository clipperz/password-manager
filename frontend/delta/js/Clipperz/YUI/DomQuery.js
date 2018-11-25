/*

Copyright 2008-2018 Clipperz Srl

This file is part of Clipperz, the online password manager.
For further information about its features and functionalities please
refer to http://www.clipperz.com.

* Clipperz is free software: you can redistribute it and/or modify it
  under the terms of the GNU Affero General Public License as published
  by the Free Software Foundation, either version 3 of the License, or 
  (at your option) any later version.

* Clipperz is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of 
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Clipperz. If not, see http://www.gnu.org/licenses/.

*/

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.YUI) == 'undefined') { Clipperz.YUI = {}; }


/*
 * yui-ext 0.40
 * Copyright(c) 2006, Jack Slocum.
 */

/**
 * @class Clipperz.YUI.DomQuery
 * Provides high performance selector/xpath processing by compiling queries into reusable functions.
 * New pseudo classes and matchers can be plugged. It works on HTML and XML documents (if a content node is passed in).
 * @singleton
 */
Clipperz.YUI.DomQuery = function(){
    var cache = {}, simpleCache = {}, valueCache = {};
    var nonSpace = /\S/;
    var trimRe = /^\s*(.*?)\s*$/;
    var tplRe = /\{(\d+)\}/g;
    var modeRe = /^(\s?[\/>]\s?|\s|$)/;
    var clsRes = {};
    
    function child(p, index){
        var i = 0;
        var n = p.firstChild;
        while(n){
            if(n.nodeType == 1){
               i++;
               if(i == index){
                   return n;
               }
            }
            n = n.nextSibling;
        }
        return null;
    };
    
    function next(d){
        var n = d.nextSibling;
        while(n && n.nodeType != 1){
            n = n.nextSibling;
        }
        return n;
    };
    
    function prev(d){
        var n = d.previousSibling;
        while(n && n.nodeType != 1){
            n = n.previousSibling;
        }
        return n;
    };
    
    function clean(d){
        var n = d.firstChild, ni = -1;
 	    while(n){
 	        var nx = n.nextSibling;
 	        if(n.nodeType == 3 && !nonSpace.test(n.nodeValue)){
 	            d.removeChild(n);
 	        }else{
 	            n.nodeIndex = ++ni;
 	        }
 	        n = nx;
 	    }
 	    return this;
 	};
    
    function byClassName(c, a, v){
        if(!v){
            return c;
        }
        var re = clsRes[v];
        if(!re){
            re = new RegExp('(?:^|\\s)(?:' + v + ')(?:\\s|$)');
            clsRes[v] = re;
        }
        var r = [];
        for(var i = 0, ci; ci = c[i]; i++){
            if(re.test(ci.className)){
                r[r.length] = ci;
            }
        }
        return r;
    };
    
    function convert(c){
        if(c.slice){
            return c;
        }
        var r = [];
        for(var i = 0, l = c.length; i < l; i++){
            r[r.length] = c[i];
        }
        return r;
    };
    
    function attrValue(n, attr){
        if(!n.tagName && typeof n.length != 'undefined'){
            n = n[0];
        }
        if(!n){
            return null;
        }
        if(attr == 'for'){
            return n.htmlFor;
        }
        if(attr == 'class' || attr == 'className'){
            return n.className;
        }
        return n.getAttribute(attr) || n[attr];
          
    };
    
    function getNodes(ns, mode, tagName){
        var result = [], cs;
        if(!ns){
            return result;
        }
        mode = mode ? mode.replace(trimRe, '$1') : '';
        tagName = tagName || '*';
        if(ns.tagName || ns == document){
            ns = [ns];   
        }
        if(mode != '/' && mode != '>'){
            for(var i = 0, ni; ni = ns[i]; i++){
                cs = ni.getElementsByTagName(tagName);
                result = concat(result, cs);
            }
        }else{
            for(var i = 0, ni; ni = ns[i]; i++){
                var cn = ni.getElementsByTagName(tagName);
                for(var j = 0, cj; cj = cn[j]; j++){
                    if(cj.parentNode == ni){
                        result[result.length] = cj;
                    }
                }
            }
            
        }
        return result;
    };
    
    function concat(a, b){
        if(b.slice){
            return a.concat(b);
        }
        for(var i = 0, l = b.length; i < l; i++){
            a[a.length] = b[i];
        }
        return a;
    }
    
    function byTag(cs, tagName){
        if(cs.tagName || cs == document){
            cs = [cs];
        }
        if(!tagName){
            return cs;
        }
        var r = []; tagName = tagName.toLowerCase();
        for(var i = 0, ci; ci = cs[i]; i++){
            if(ci.nodeType == 1 && ci.tagName.toLowerCase()==tagName){
                r[r.length] = ci;
            }
        }
        return r; 
    };
    
    function byId(cs, attr, id){
        if(cs.tagName || cs == document){
            cs = [cs];
        }
        if(!id){
            return cs;
        }
        var r = [];
        for(var i = 0, l = cs.length; i < l; i++){
            var ci = cs[i];
            if(ci && ci.id == id){
                r[r.length] = ci;
            }
        }
        return r; 
    };
    
    function byAttribute(cs, attr, value, op, custom){
        var r = [], st = custom=='{';
        var f = Clipperz.YUI.DomQuery.operators[op];
        for(var i = 0, l = cs.length; i < l; i++){
            var a;
            if(st){
                a = Clipperz.YUI.DomQuery.getStyle(cs[i], attr);
            }
            else if(attr == 'class' || attr == 'className'){
                a = cs[i].className;
            }else if(attr == 'for'){
                a = cs[i].htmlFor;
            }else{
                a = cs[i].getAttribute(attr);
            }
            if((f && f(a, value)) || (!f && a)){
                r[r.length] = cs[i];
            }
        }
        return r;
    };
    
    function byPseudo(cs, name, value){
        return Clipperz.YUI.DomQuery.pseudos[name](cs, value);
    };
    
    // This is for IE MSXML which does not support expandos.
    // IE runs the same speed using setAttribute, however FF slows way down
    // and Safari completely fails so they need to continue to use expandos.
    // Branched at load time for faster execution.
    var isIE = window.ActiveXObject;
    var addAttr = isIE ? 
           function(n, a, v){
              n.setAttribute(a, v);
           } : 
           function(n, a, v){
              n[a] = v;
           };
    var getAttr = isIE ? 
           function(n, a){
              return n.getAttribute(a);
           } : 
           function(n, a){
              return n[a];
           };
    var clearAttr = isIE ? 
           function(n, a){
              n.removeAttribute(a);
           } : 
           function(n, a, v){
              delete n[a];
           };
    
    function nodup(cs){
        if(!cs.length){
            return cs;
        }
        addAttr(cs[0], '_nodup', true);
        var r = [cs[0]];
        for(var i = 1, len = cs.length; i < len; i++){
            var c = cs[i];
            if(!getAttr(c, '_nodup')){
                addAttr(c, '_nodup', true);
                r[r.length] = c;
            }
        }
        for(var i = 0, len = cs.length; i < len; i++){
            clearAttr(cs[i], '_nodup');
        }
        return r;
    }
    
    function quickDiff(c1, c2){
        if(!c1.length){
            return c2;
        }
        for(var i = 0, len = c1.length; i < len; i++){
            addAttr(c1[i], '_qdiff', true);
        }
        var r = [];
        for(var i = 0, len = c2.length; i < len; i++){
            if(!getAttr(c2[i], '_qdiff')){
                r[r.length] = c2[i];
            }
        }
        for(var i = 0, len = c1.length; i < len; i++){
            clearAttr(c1[i], '_qdiff');
        }
        return r;
    }
    
    function quickId(ns, mode, root, id){
        if(ns == root){
           var d = root.ownerDocument || root;
           return d.getElementById(id);
        }
        ns = getNodes(ns, mode, '*');
        return byId(ns, null, id);
    }
    
    return {
        getStyle : function(el, name){
            return YAHOO.util.Dom.getStyle(el, name);  
        },
        /**
         * Compiles a selector/xpath query into a reusable function. The returned function
         * takes one parameter "root" (optional), which is the context node from where the query should start. 
         * @param {String} selector The selector/xpath query
         * @param {String} type (optional) Either 'select' (the default) or 'simple' for a simple selector match
         * @return {Function}
         */
        compile : function(path, type){
            // strip leading slashes
            while(path.substr(0, 1)=='/'){
                path = path.substr(1);
            }
            type = type || 'select';
            
            var fn = ['var f = function(root){\n var mode; var n = root || document;\n'];
            var q = path, mode, lq;
            var tk = Clipperz.YUI.DomQuery.matchers;
            var tklen = tk.length;
            var mm;
            while(q && lq != q){
                lq = q;
                var tm = q.match(/^(#)?([\w-\*]+)/);
                if(type == 'select'){
                    if(tm){
                        if(tm[1] == '#'){
                            fn[fn.length] = 'n = quickId(n, mode, root, "'+tm[2]+'");';
                        }else{
                            fn[fn.length] = 'n = getNodes(n, mode, "'+tm[2]+'");';
                        }
                        q = q.replace(tm[0], '');
                    }else{
                        fn[fn.length] = 'n = getNodes(n, mode, "*");';
                    }
                }else{
                    if(tm){
                        if(tm[1] == '#'){
                            fn[fn.length] = 'n = byId(n, null, "'+tm[2]+'");';
                        }else{
                            fn[fn.length] = 'n = byTag(n, "'+tm[2]+'");';
                        }
                        q = q.replace(tm[0], '');
                    }
                }
                while(!(mm = q.match(modeRe))){
                    var matched = false;
                    for(var j = 0; j < tklen; j++){
                        var t = tk[j];
                        var m = q.match(t.re);
                        if(m){
                            fn[fn.length] = t.select.replace(tplRe, function(x, i){
                                                    return m[i];
                                                });
                            q = q.replace(m[0], '');
                            matched = true;
                            break;
                        }
                    }
                    // prevent infinite loop on bad selector
                    if(!matched){
                        throw 'Error parsing selector, parsing failed at "' + q + '"';
                    }
                }
                if(mm[1]){
                    fn[fn.length] = 'mode="'+mm[1]+'";';
                    q = q.replace(mm[1], '');
                }
            }
            fn[fn.length] = 'return nodup(n);\n}';
            eval(fn.join(''));
            return f;
        },
        
        /**
         * Selects a group of elements.
         * @param {String} selector The selector/xpath query
         * @param {Node} root (optional) The start of the query (defaults to document).
         * @return {Array}
         */
        select : function(path, root, type){
            if(!root || root == document){
                root = document;
            }
            if(typeof root == 'string'){
                root = document.getElementById(root);
            }
            var paths = path.split(',');
            var results = [];
            for(var i = 0, len = paths.length; i < len; i++){
                var p = paths[i].replace(trimRe, '$1');
                if(!cache[p]){
                    cache[p] = Clipperz.YUI.DomQuery.compile(p);
                    if(!cache[p]){
                        throw p + ' is not a valid selector';
                    }
                }
                var result = cache[p](root);
                if(result && result != document){
                    results = results.concat(result);
                }
            }
            return results;
        },
        
        /**
         * Selects a single element.
         * @param {String} selector The selector/xpath query
         * @param {Node} root (optional) The start of the query (defaults to document).
         * @return {Element}
         */
        selectNode : function(path, root){
            return Clipperz.YUI.DomQuery.select(path, root)[0];
        },
        
        /**
         * Selects the value of a node, optionally replacing null with the defaultValue.
         * @param {String} selector The selector/xpath query
         * @param {Node} root (optional) The start of the query (defaults to document).
         * @param {String} defaultValue
         */
        selectValue : function(path, root, defaultValue){
            path = path.replace(trimRe, '$1');
            if(!valueCache[path]){
                valueCache[path] = Clipperz.YUI.DomQuery.compile(path, 'simple');
            }
            var n = valueCache[path](root);
            n = n[0] ? n[0] : n;
            var v = (n && n.firstChild ? n.firstChild.nodeValue : null);
            return (v === null ? defaultValue : v);
        },
        
        /**
         * Selects the value of a node, parsing integers and floats.
         * @param {String} selector The selector/xpath query
         * @param {Node} root (optional) The start of the query (defaults to document).
         * @param {Number} defaultValue
         * @return {Number}
         */
        selectNumber : function(path, root, defaultValue){
            var v = Clipperz.YUI.DomQuery.selectValue(path, root, defaultValue || 0);
            return parseFloat(v);
        },
        
        /**
         * Returns true if the passed element(s) match the passed simple selector (e.g. div.some-class or span:first-child)
         * @param {String/HTMLElement/Array} el An element id, element or array of elements
         * @param {String} selector The simple selector to test
         * @return {Boolean}
         */
        is : function(el, ss){
            if(typeof el == 'string'){
                el = document.getElementById(el);
            }
            var isArray = (el instanceof Array);
            var result = Clipperz.YUI.DomQuery.filter(isArray ? el : [el], ss);
            return isArray ? (result.length == el.length) : (result.length > 0);
        },
        
        /**
         * Filters an array of elements to only include matches of a simple selector (e.g. div.some-class or span:first-child)
         * @param {Array} el An array of elements to filter
         * @param {String} selector The simple selector to test
         * @param {Boolean} nonMatches If true, it returns the elements that DON'T match 
         * the selector instead of the ones that match
         * @return {Array}
         */
        filter : function(els, ss, nonMatches){
            ss = ss.replace(trimRe, '$1');
            if(!simpleCache[ss]){
                simpleCache[ss] = Clipperz.YUI.DomQuery.compile(ss, 'simple');
            }
            var result = simpleCache[ss](els);
            return nonMatches ? quickDiff(result, els) : result;
        },
        
        /**
         * Collection of matching regular expressions and code snippets. 
         */
        matchers : [{
                re: /^\.([\w-]+)/,
                select: 'n = byClassName(n, null, "{1}");'
            }, {
                re: /^\:([\w-]+)(?:\(((?:[^\s>\/]*|.*?))\))?/,
                select: 'n = byPseudo(n, "{1}", "{2}");'
            },{
                re: /^(?:([\[\{])(?:@)?([\w-]+)\s?(?:(=|.=)\s?['"]?(.*?)["']?)?[\]\}])/,
                select: 'n = byAttribute(n, "{2}", "{4}", "{3}", "{1}");'
            }, {
                re: /^#([\w-]+)/,
                select: 'n = byId(n, null, "{1}");'
            },{
                re: /^@([\w-]+)/,
                select: 'return {firstChild:{nodeValue:attrValue(n, "{1}")}};'
            }
        ],
        
        /**
         * Collection of operator comparison functions. The default operators are =, !=, ^=, $=, *= and %=.
         * New operators can be added as long as the match the format <i>c</i>= where <i>c<i> is any character other than space, &gt; &lt;.
         */
        operators : {
            '=' : function(a, v){
                return a == v;
            },
            '!=' : function(a, v){
                return a != v;
            },
            '^=' : function(a, v){
                return a && a.substr(0, v.length) == v;
            },
            '$=' : function(a, v){
                return a && a.substr(a.length-v.length) == v;
            },
            '*=' : function(a, v){
                return a && a.indexOf(v) !== -1;
            },
            '%=' : function(a, v){
                return (a % v) == 0;
            }
        },
        
        /**
         * Collection of "pseudo class" processors. Each processor is passed the current nodeset (array)
         * and the argument (if any) supplied in the selector.
         */
        pseudos : {
            'first-child' : function(c){
                var r = [];
                for(var i = 0, l = c.length; i < l; i++){
                    var ci = c[i];
                    if(!prev(ci)){
                        r[r.length] = ci;
                    }
                }
                return r;
            },
            
            'last-child' : function(c){
                var r = [];
                for(var i = 0, l = c.length; i < l; i++){
                    var ci = c[i];
                    if(!next(ci)){
                        r[r.length] = ci;
                    }
                }
                return r;
            },
            
            'nth-child' : function(c, a){
                var r = [];
                if(a != 'odd' && a != 'even'){
                    for(var i = 0, ci; ci = c[i]; i++){
                        var m = child(ci.parentNode, a);
                        if(m == ci){
                            r[r.length] = m;
                        }
                    }
                    return r;
                }
                var p;
                // first let's clean up the parent nodes
                for(var i = 0, l = c.length; i < l; i++){
                    var cp = c[i].parentNode;
                    if(cp != p){
                        clean(cp);
                        p = cp;
                    }
                }
                // then lets see if we match
                for(var i = 0, l = c.length; i < l; i++){
                    var ci = c[i], m = false;
                    if(a == 'odd'){
                        m = ((ci.nodeIndex+1) % 2 == 1);
                    }else if(a == 'even'){
                        m = ((ci.nodeIndex+1) % 2 == 0);
                    }
                    if(m){
                        r[r.length] = ci;
                    }
                }
                return r;
            },
            
            'only-child' : function(c){
                var r = [];
                for(var i = 0, l = c.length; i < l; i++){
                    var ci = c[i];
                    if(!prev(ci) && !next(ci)){
                        r[r.length] = ci;
                    }
                }
                return r;
            },
            
            'empty' : function(c){
                var r = [];
                for(var i = 0, l = c.length; i < l; i++){
                    var ci = c[i];
                    if(!ci.firstChild){
                        r[r.length] = ci;
                    }
                }
                return r;
            },
            
            'contains' : function(c, v){
                var r = [];
                for(var i = 0, l = c.length; i < l; i++){
                    var ci = c[i];
                    if(ci.innerHTML.indexOf(v) !== -1){
                        r[r.length] = ci;
                    }
                }
                return r;
            },
            
            'checked' : function(c){
                var r = [];
                for(var i = 0, l = c.length; i < l; i++){
                    if(c[i].checked == 'checked'){
                        r[r.length] = c[i];
                    }
                }
                return r;
            },
            
            'not' : function(c, ss){
                return Clipperz.YUI.DomQuery.filter(c, ss, true);
            },
            
            'odd' : function(c){
                return this['nth-child'](c, 'odd');
            },
            
            'even' : function(c){
                return this['nth-child'](c, 'even');
            },
            
            'nth' : function(c, a){
                return c[a-1];
            },
            
            'first' : function(c){
                return c[0];
            },
            
            'last' : function(c){
                return c[c.length-1];
            },
            
            'has' : function(c, ss){
                var s = Clipperz.YUI.DomQuery.select;
                var r = [];
                for(var i = 0, ci; ci = c[i]; i++){
                    if(s(ss, ci).length > 0){
                        r[r.length] = ci;
                    }
                }
                return r;
            },
            
            'next' : function(c, ss){
                var is = Clipperz.YUI.DomQuery.is;
                var r = [];
                for(var i = 0, ci; ci = c[i]; i++){
                    var n = next(ci);
                    if(n && is(n, ss)){
                        r[r.length] = ci;
                    }
                }
                return r;
            },
            
            'prev' : function(c, ss){
                var is = Clipperz.YUI.DomQuery.is;
                var r = [];
                for(var i = 0, ci; ci = c[i]; i++){
                    var n = prev(ci);
                    if(n && is(n, ss)){
                        r[r.length] = ci;
                    }
                }
                return r;
            }
        }
    };
}();

/**
 * Selects an array of DOM nodes by CSS/XPath selector. Shorthand of {@link Clipperz.YUI.DomQuery#select}
 * @param {String} path The selector/xpath query
 * @param {Node} root (optional) The start of the query (defaults to document).
 * @return {Array}
 * @member Ext
 * @method query
 */
Clipperz.YUI.query = Clipperz.YUI.DomQuery.select;
