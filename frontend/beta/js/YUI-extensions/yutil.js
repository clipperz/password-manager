YAHOO.namespace('ext', 'ext.util', 'ext.grid', 'ext.dd', 'ext.tree', 'ext.data', 'ext.form');
if(typeof Ext == 'undefined'){
    Ext = YAHOO.ext;
}
YAHOO.ext.Strict = (document.compatMode == 'CSS1Compat');
YAHOO.ext.SSL_SECURE_URL = 'javascript:false';
YAHOO.ext.BLANK_IMAGE_URL = 'http:/'+'/www.yui-ext.com/blog/images/s.gif';

// for old browsers
window.undefined = undefined;
/**
 * @class Function
 * These functions are available on every Function object (any javascript function).
 */
 //
 /**
 * Creates a callback that passes arguments[0], arguments[1], arguments[2], ...
 * Call directly on any function. Example: <code>myFunction.createCallback(myarg, myarg2)</code>
 * Will create a function that is bound to those 2 args.
 * @return {Function} The new function
*/
Function.prototype.createCallback = function(/*args...*/){
    // make args available, in function below
    var args = arguments;
    var method = this;
    return function() {
        return method.apply(window, args);
    };
};

/**
 * Creates a delegate (callback) that sets the scope to obj.
 * Call directly on any function. Example: <code>this.myFunction.createDelegate(this)</code>
 * Will create a function that is automatically scoped to this.
 * @param {Object} obj (optional) The object for which the scope is set
 * @param {<i>Array</i>} args (optional) Overrides arguments for the call. (Defaults to the arguments passed by the caller)
 * @param {<i>Boolean/Number</i>} appendArgs (optional) if True args are appended to call args instead of overriding, 
 *                                             if a number the args are inserted at the specified position
 * @return {Function} The new function
 */
Function.prototype.createDelegate = function(obj, args, appendArgs){
    var method = this;
    return function() {
        var callArgs = args || arguments;
        if(appendArgs === true){
            callArgs = Array.prototype.slice.call(arguments, 0);
            callArgs = callArgs.concat(args);
        }else if(typeof appendArgs == 'number'){
            callArgs = Array.prototype.slice.call(arguments, 0); // copy arguments first
            var applyArgs = [appendArgs, 0].concat(args); // create method call params
            Array.prototype.splice.apply(callArgs, applyArgs); // splice them in
        }
        return method.apply(obj || window, callArgs);
    };
};

/**
 * Calls this function after the number of millseconds specified.
 * @param {Number} millis The number of milliseconds for the setTimeout call
 * @param {Object} obj (optional) The object for which the scope is set
 * @param {<i>Array</i>} args (optional) Overrides arguments for the call. (Defaults to the arguments passed by the caller)
 * @param {<i>Boolean/Number</i>} appendArgs (optional) if True args are appended to call args instead of overriding, 
 *                                             if a number the args are inserted at the specified position
 * @return {Number} The timeout id that can be used with clearTimeout
 */
Function.prototype.defer = function(millis, obj, args, appendArgs){
    return setTimeout(this.createDelegate(obj, args, appendArgs), millis);
};
/**
 * Create a combined function call sequence of the original function + the passed function.
 * The resulting function returns the results of the original function.
 * The passed fcn is called with the parameters of the original function
 * @param {Function} fcn The function to sequence
 * @param {<i>Object</i>} scope (optional) The scope of the passed fcn (Defaults to scope of original function or window)
 * @return {Function} The new function
 */
Function.prototype.createSequence = function(fcn, scope){
    if(typeof fcn != 'function'){
        return this;
    }
    var method = this;
    return function() {
        var retval = method.apply(this || window, arguments);
        fcn.apply(scope || this || window, arguments);
        return retval;
    };
};

/*
 * IE will leak if this isn't here
 */
YAHOO.util.Event.on(window, 'unload', function(){
    var p = Function.prototype;
    delete p.createSequence;
    delete p.defer;
    delete p.createDelegate;
    delete p.createCallback;
    delete p.createInterceptor;
});

/**
 * Creates an interceptor function. The passed fcn is called before the original one. If it returns false, the original one is not called.
 * The resulting function returns the results of the original function.
 * The passed fcn is called with the parameters of the original function.
 * @addon
 * @param {Function} fcn The function to call before the original
 * @param {<i>Object</i>} scope (optional) The scope of the passed fcn (Defaults to scope of original function or window)
 * @return {Function} The new function
 */
Function.prototype.createInterceptor = function(fcn, scope){
    if(typeof fcn != 'function'){
        return this;
    }
    var method = this;
    return function() {
        fcn.target = this;
        fcn.method = method;
        if(fcn.apply(scope || this || window, arguments) === false){
            return;
        }
        return method.apply(this || window, arguments);;
    };
};

/**
 * @class YAHOO.ext.util.Browser
 * @singleton
 */
YAHOO.ext.util.Browser = new function(){
	var ua = navigator.userAgent.toLowerCase();
	/** @type Boolean */
	this.isOpera = (ua.indexOf('opera') > -1);
   	/** @type Boolean */
	this.isSafari = (ua.indexOf('webkit') > -1);
   	/** @type Boolean */
	this.isIE = (window.ActiveXObject);
   	/** @type Boolean */
	this.isIE7 = (ua.indexOf('msie 7') > -1);
   	/** @type Boolean */
	this.isGecko = !this.isSafari && (ua.indexOf('gecko') > -1);
	
	if(ua.indexOf("windows") != -1 || ua.indexOf("win32") != -1){
	    /** @type Boolean */
	    this.isWindows = true;
	}else if(ua.indexOf("macintosh") != -1){
		/** @type Boolean */
	    this.isMac = true;
	}
	if(this.isIE && !this.isIE7){
        try{
            document.execCommand("BackgroundImageCache", false, true);
        }catch(e){}
    }
}();

 /**
 * Enable custom handler signature and event cancelling. Using fireDirect() instead of fire() calls the subscribed event handlers 
 * with the exact parameters passed to fireDirect, instead of the usual (eventType, args[], obj). IMO this is more intuitive 
 * and promotes cleaner code. Also, if an event handler returns false, it is returned by fireDirect and no other handlers will be called.<br>
 * Example:<br><br><pre><code>
 * if(beforeUpdateEvent.fireDirect(myArg, myArg2) !== false){
 *     // do update
 * }</code></pre>
 */
YAHOO.util.CustomEvent.prototype.fireDirect = function(){
    var len=this.subscribers.length;
    for (var i=0; i<len; ++i) {
        var s = this.subscribers[i];
        if(s){
            var scope = (s.override) ? s.obj : this.scope;
            if(s.fn.apply(scope, arguments) === false){
                return false;
            }
        }
    }
    return true;
};

/**
 * @class YAHOO-
 * Additional functionality for the global YAHOO object.
 * @singleton
 */
/**
 * Prints all arguments to a resizable, movable, scrolling region without
 * the need to include separate js or css. Double click it to hide it.
 * @param {Mixed} arg1
 * @param {Mixed} arg2
 * @param {Mixed} etc
 * @static
 */
YAHOO.print = function(arg1, arg2, etc){
    if(!YAHOO.ext._console){
        var cs = YAHOO.ext.DomHelper.insertBefore(document.body.firstChild,
        {tag: 'div',style:'width:250px;height:350px;overflow:auto;border:3px solid #c3daf9;' +
                'background:white;position:absolute;right:5px;top:5px;' +
                'font:normal 8pt arial,verdana,helvetica;z-index:50000;padding:5px;'}, true);
        if(YAHOO.ext.Resizable){
            new YAHOO.ext.Resizable(cs, {
                transparent:true,
                handles: 'all',
                pinned:true, 
                adjustments: [0,0], 
                wrap:true, 
                draggable:(YAHOO.util.DD ? true : false)
            });
        }
        cs.on('dblclick', cs.hide);
        YAHOO.ext._console = cs;
    }
    var m = '';
    for(var i = 0, len = arguments.length; i < len; i++) {
    	m += (i == 0 ? '' : ', ') + arguments[i];
    }
    m += '<hr noshade style="color:#eeeeee;" size="1">'
    var d = YAHOO.ext._console.dom;
    d.innerHTML = m + d.innerHTML;
    d.scrollTop = 0;
    YAHOO.ext._console.show();
};

/**
 * Applies the passed C#/DomHelper style format (e.g. "The variable {0} is equal to {1}") before calling {@link YAHOO#print}
 * @param {String} format
 * @param {Mixed} arg1
 * @param {Mixed} arg2
 * @param {Mixed} etc
 * @static
 */
YAHOO.printf = function(format, arg1, arg2, etc){
    var args = Array.prototype.slice.call(arguments, 1);
    YAHOO.print(format.replace(
      /\{\{[^{}]*\}\}|\{(\d+)(,\s*([\w.]+))?\}/g,
      function(m, a1, a2, a3) {
        if (m.chatAt == '{') {
          return m.slice(1, -1);
        }
        var rpl = args[a1];
        if (a3) {
          var f = eval(a3);
          rpl = f(rpl);
        }
        return rpl ? rpl : '';
      }));
}

YAHOO._timers = {};
/**
 * If a timer with specified name doesn't exist it is started. If one exists and reset is not true
 * it prints the ellapsed time since the start using YAHOO.printf.
 * @param {String} name
 * @param {Boolean} reset true to reset an existing timer
 */
YAHOO.timer = function(name, reset){
    var t = new Date().getTime();
    if(YAHOO._timers[name] && !reset){
        YAHOO.printf("{0} : {1} ms", name, t-YAHOO._timers[name]);
    }else{
        YAHOO._timers[name] = t;
    }
}
/**
 * Extends one class with another class and optionally overrides members with the passed literal. This class
 * also adds the function "override()" to the class that can be used to override 
 * members on an instance.
 * @param {Object} subclass The class inheriting the functionality
 * @param {Object} superclass The class being extended
 * @param {Object} overrides (optional) A literal with members
 * @static
 */
YAHOO.extendX = function(subclass, superclass, overrides){
    YAHOO.extend(subclass, superclass);
    subclass.override = function(o){
        YAHOO.override(subclass, o);
    };
    if(!subclass.prototype.override){
        subclass.prototype.override = function(o){
            for(var method in o){
                this[method] = o[method];
            }  
        };
    }
    if(overrides){
        subclass.override(overrides);
    }
};

/**
 * Creates namespaces but does not assume YAHOO is the root.
 * @param {String} namespace1
 * @param {String} namespace2
 * @param {String} etc
 * @static
 */
YAHOO.namespaceX = function(){
    var a = arguments, len = a.length, i;
    YAHOO.namespace.apply(YAHOO, a);
    for(i = 0; i < len; i++){
        var p = a[i].split('.')[0];
        if(p != 'YAHOO' && YAHOO[p]){
            eval(p + ' = YAHOO.' + p);
            delete YAHOO[p];
        }
    }
};

YAHOO.override = function(origclass, overrides){
    if(overrides){
        var p = origclass.prototype;
        for(var method in overrides){
            p[method] = overrides[method];
        }
    }
};

/**
 * @class YAHOO.ext.util.DelayedTask
 * Provides a convenient method of performing setTimeout where a new
 * timeout cancels the old timeout. An example would be performing validation on a keypress.
 * You can use this class to buffer
 * the keypress events for a certain number of milliseconds, and perform only if they stop
 * for that amount of time.
 * @constructor The parameters to this constructor serve as defaults and are not required.
 * @param {<i>Function</i>} fn (optional) The default function to timeout
 * @param {<i>Object</i>} scope (optional) The default scope of that timeout
 * @param {<i>Array</i>} args (optional) The default Array of arguments
 */
YAHOO.ext.util.DelayedTask = function(fn, scope, args){
    var timeoutId = null;
    
    /**
     * Cancels any pending timeout and queues a new one
     * @param {Number} delay The milliseconds to delay
     * @param {Function} newFn (optional) Overrides function passed to constructor
     * @param {Object} newScope (optional) Overrides scope passed to constructor
     * @param {Array} newArgs (optional) Overrides args passed to constructor
     */
    this.delay = function(delay, newFn, newScope, newArgs){
        if(timeoutId){
            clearTimeout(timeoutId);
        }
        fn = newFn || fn;
        scope = newScope || scope;
        args = newArgs || args;
        timeoutId = setTimeout(fn.createDelegate(scope, args), delay);
    };
    
    /**
     * Cancel the last queued timeout
     */
    this.cancel = function(){
        if(timeoutId){
            clearTimeout(timeoutId);
            timeoutId = null;
        }
    };
};

/**
 * @class YAHOO.ext.util.Observable
 * Abstract base class that provides a common interface for publishing events. Subclasses are expected to 
 * to have a property "events" with all the events defined.<br>
 * For example:
 * <pre><code>
 var Employee = function(name){
    this.name = name;
    this.events = {
        'fired' : new YAHOO.util.CustomEvent('fired'),
        'quit' : true // lazy initialize the CustomEvent
    }
 }
 YAHOO.extend(Employee, YAHOO.ext.util.Observable);
</code></pre>
 */
YAHOO.ext.util.Observable = function(){};
YAHOO.ext.util.Observable.prototype = {
    /**
     * Fires the specified event with the passed parameters (minus the event name).
     * @param {String} eventName
     * @param {Object...} args Variable number of parameters are passed to handlers
     * @return {Boolean} returns false if any of the handlers return false otherwise it returns true
     */
    fireEvent : function(){
        var ce = this.events[arguments[0].toLowerCase()];
        if(typeof ce == 'object'){
            return ce.fireDirect.apply(ce, Array.prototype.slice.call(arguments, 1));
        }else{
            return true;
        }
    },
    /**
     * Appends an event handler to this component
     * @param {String}   eventName     The type of event to listen for
     * @param {Function} handler        The method the event invokes
     * @param {<i>Object</i>}   scope  (optional) The scope (this object) for the handler
     * @param {<i>boolean</i>}  override (optional) If true, scope becomes the scope
     */
    addListener : function(eventName, fn, scope, override){
        eventName = eventName.toLowerCase();
        var ce = this.events[eventName];
        if(!ce){
            // added for a better message when subscribing to wrong event
            throw 'You are trying to listen for an event that does not exist: "' + eventName + '".';
        }
        if(typeof ce == 'boolean'){
            ce = new YAHOO.util.CustomEvent(eventName);
            this.events[eventName] = ce;
        }
        ce.subscribe(fn, scope, override);
    },
    
    /**
     * Appends an event handler to this component that is delayed the specified number of milliseconds. This
     * is useful for events that modify the DOM and need to wait for the browser to catch up.
     * @param {String}   eventName     The type of event to listen for
     * @param {Function} handler        The method the event invokes
     * @param {<i>Object</i>}   scope  (optional) The scope (this object) for the handler
     * @param {<i>Number</i>}  delay (optional) The number of milliseconds to delay (defaults to 1 millisecond)
     * @return {Function} The wrapped function that was created (can be used to remove the listener)
     */
    delayedListener : function(eventName, fn, scope, delay){
        var newFn = function(){
            setTimeout(fn.createDelegate(scope, arguments), delay || 1);
        }
        this.addListener(eventName, newFn);
        return newFn;
    },
    
    /**
     * Appends an event handler to this component that is buffered. If the event is triggered more than once
     * in the specified time-frame, only the last one actually fires.
     * @param {String}   eventName     The type of event to listen for
     * @param {Function} handler        The method the event invokes
     * @param {<i>Object</i>}   scope  (optional) The scope (this object) for the handler
     * @param {<i>Number</i>}  millis (optional) The number of milliseconds to buffer (defaults to 250)
     * @return {Function} The wrapped function that was created (can be used to remove the listener)
     */
    bufferedListener : function(eventName, fn, scope, millis){
        var task = new YAHOO.ext.util.DelayedTask();
        var newFn = function(){
            task.delay(millis || 250, fn, scope, Array.prototype.slice.call(arguments, 0));
        }
        this.addListener(eventName, newFn);
        return newFn;
    },
    
    /**
     * Removes a listener
     * @param {String}   eventName     The type of event to listen for
     * @param {Function} handler        The handler to remove
     * @param {<i>Object</i>}   scope  (optional) The scope (this object) for the handler
     */
    removeListener : function(eventName, fn, scope){
        var ce = this.events[eventName.toLowerCase()];
        if(typeof ce == 'object'){
            ce.unsubscribe(fn, scope);
        }
    },
    
    /**
     * Removes all listeners for this object
     */
    purgeListeners : function(){
        for(var evt in this.events){
            if(typeof this.events[evt] == 'object'){
                 this.events[evt].unsubscribeAll();
            }
        }
    }
};
/**
 * Appends an event handler to this element (shorthand for addListener)
 * @param {String}   eventName     The type of event to listen for
 * @param {Function} handler        The method the event invokes
 * @param {<i>Object</i>}   scope  (optional) The scope (this object) for the handler
 * @param {<i>boolean</i>}  override (optional) If true, scope becomes the scope
 * @method
 */
YAHOO.ext.util.Observable.prototype.on = YAHOO.ext.util.Observable.prototype.addListener;

/**
 * Starts capture on the specified Observable. All events will be passed
 * to the supplied function with the event name + standard signature of the event 
 * <b>before</b> the event is fired. If the supplied function returns false,
 * the event will not fire.
 * @param {Observable} o The Observable to capture
 * @param {Function} fn The function to call
 * @param {Object} scope (optional) The scope (this object) for the fn
 * @static
 */
YAHOO.ext.util.Observable.capture = function(o, fn, scope){
    o.fireEvent = o.fireEvent.createInterceptor(fn, scope);  
};

/**
 * Removes <b>all</b> added captures from the Observable. 
 * @param {Observable} o The Observable to release
 * @static
 */
YAHOO.ext.util.Observable.releaseCapture = function(o){
    o.fireEvent = YAHOO.ext.util.Observable.prototype.fireEvent;  
};

/**
 * @class YAHOO.ext.util.Config
 * Class with one useful method
 * @singleton
 */
YAHOO.ext.util.Config = {
    /**
     * Copies all the properties of config to obj.
     * @param {Object} obj The receiver of the properties
     * @param {Object} config The source of the properties
     * @param {Object} defaults A different object that will also be applied for default values
     * @return {Object} returns obj
     */
    apply : function(obj, config, defaults){
        if(defaults){
            this.apply(obj, defaults);
        }
        if(config){
            for(var prop in config){
                obj[prop] = config[prop];
            }
        }
        return obj;
    }
};

if(!String.escape){
    String.escape = function(string) {
        return string.replace(/('|\\)/g, "\\$1");
    };
};

String.leftPad = function (val, size, ch) {
    var result = new String(val);
    if (ch == null) {
        ch = " ";
    }
    while (result.length < size) {
        result = ch + result;
    }
    return result;
};

// workaround for Safari anim duration speed problems
if(YAHOO.util.AnimMgr && YAHOO.ext.util.Browser.isSafari){
    YAHOO.util.AnimMgr.fps = 500;  
}

// add ability for callbacks instead of events for animations
if(YAHOO.util.Anim){
    YAHOO.util.Anim.prototype.animateX = function(callback, scope){
        var f = function(){
            this.onComplete.unsubscribe(f);
            if(typeof callback == 'function'){
                callback.call(scope || this, this);
            }
        };
        this.onComplete.subscribe(f, this, true);
        this.animate();
    };
}

// workaround for Safari 1.3 not supporting hasOwnProperty
if(YAHOO.util.Connect && YAHOO.ext.util.Browser.isSafari){
    YAHOO.util.Connect.setHeader = function(o){
		for(var prop in this._http_header){
		    // if(this._http_header.hasOwnProperty(prop)){
			if(typeof this._http_header[prop] != 'function'){
				o.conn.setRequestHeader(prop, this._http_header[prop]);
			}
		}
		delete this._http_header;
		this._http_header = {};
		this._has_http_headers = false;
	};   
}
/**
 * A simple enhancement to drag drop that allows you to constrain the movement of the
 * DD or DDProxy object to a particular element.<br /><br />
 * 
 * Usage:
 <pre><code>
 var dd = new YAHOO.util.DDProxy("dragDiv1", "proxytest",  
                { dragElId: "existingProxyDiv" });
 dd.startDrag = function(){
     this.constrainTo('parent-id');
 }; 
 </code></pre>
 * Or you can initalize it using the {@link YAHOO.ext.Element} object:
 <pre><code>
 getEl('dragDiv1').initDDProxy('proxytest', {dragElId: "existingProxyDiv"}, {
     startDrag : function(){
         this.constrainTo('parent-id');
     }
 });
 </code></pre>
 */
if(YAHOO.util.DragDrop){
    /**
     * Provides default constraint padding to "constrainTo" elements (defaults to {left: 0, right:0, top:0, bottom:0}).
     * @type Object
     */
    YAHOO.util.DragDrop.prototype.defaultPadding = {left:0, right:0, top:0, bottom:0};
    
    /**
     * Initializes the drag drop object's constraints to restrict movement to a certain element.
     * @param {String/HTMLElement/Element} constrainTo The element to constrain to.
     * @param {Object/Number} pad (optional) Pad provides a way to specify "padding" of the constraints, 
     * and can be either a number for symmetrical padding (4 would be equal to {left:4, right:4, top:4, bottom:4}) or
     * an object containing the sides to pad. For example: {right:10, bottom:10}
     * @param {Boolean} inContent (optional) Constrain the draggable in the content box of the element (inside padding and borders)
     */
    YAHOO.util.DragDrop.prototype.constrainTo = function(constrainTo, pad, inContent){
        if(typeof pad == 'number'){
            pad = {left: pad, right:pad, top:pad, bottom:pad};
        }
        pad = pad || this.defaultPadding;
        var b = getEl(this.getEl()).getBox();
        var ce = getEl(constrainTo);
        var c = ce.dom == document.body ? { x: 0, y: 0,
                width: YAHOO.util.Dom.getViewportWidth(),
                height: YAHOO.util.Dom.getViewportHeight()} : ce.getBox(inContent || false);
        var topSpace = b.y - c.y;
        var leftSpace = b.x - c.x;

        this.resetConstraints();
        this.setXConstraint(leftSpace - (pad.left||0), // left
                c.width - leftSpace - b.width - (pad.right||0) //right
        );
        this.setYConstraint(topSpace - (pad.top||0), //top
                c.height - topSpace - b.height - (pad.bottom||0) //bottom
        );
    } 
}
