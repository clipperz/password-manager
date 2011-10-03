
/**
 * @class YAHOO.ext.EventManager
 * Registers event handlers that want to receive a normalized EventObject instead of the standard browser event and provides 
 * several useful events directly.
 * See {@link YAHOO.ext.EventObject} for more details on normalized event objects.
 * @singleton
 */
YAHOO.ext.EventManager = new function(){
    var docReadyEvent;
    var docReadyProcId;
    var docReadyState = false;
    this.ieDeferSrc = false;
    var resizeEvent;
    var resizeTask;
    
    var fireDocReady = function(){
        if(!docReadyState){
            docReadyState = true;
            if(docReadyProcId){
                clearInterval(docReadyProcId);
            }
            if(docReadyEvent){
                docReadyEvent.fire();
            }
        }
    };
    
    var initDocReady = function(){
        docReadyEvent = new YAHOO.util.CustomEvent('documentready');
        if(document.addEventListener) {
            YAHOO.util.Event.on(document, "DOMContentLoaded", fireDocReady);
        }else if(YAHOO.ext.util.Browser.isIE){
            // inspired by  http://www.thefutureoftheweb.com/blog/2006/6/adddomloadevent
            document.write('<s'+'cript id="ie-deferred-loader" defer="defer" src="' +
                        (YAHOO.ext.EventManager.ieDeferSrc || YAHOO.ext.SSL_SECURE_URL) + '"></s'+'cript>');
            YAHOO.util.Event.on('ie-deferred-loader', 'readystatechange', function(){
                if(this.readyState == 'complete'){
                    fireDocReady();
                }
            });
        }else if(YAHOO.ext.util.Browser.isSafari){ 
            docReadyProcId = setInterval(function(){
                var rs = document.readyState;
                if(rs == 'loaded' || rs == 'complete') {
                    fireDocReady();     
                 }
            }, 10);
        }
        // no matter what, make sure it fires on load
        YAHOO.util.Event.on(window, 'load', fireDocReady);
    };
    /** 
     * Places a simple wrapper around an event handler to override the browser event 
     * object with a YAHOO.ext.EventObject
     * @param {Function} fn        The method the event invokes
     * @param {Object}   scope    An object that becomes the scope of the handler
     * @param {boolean}  override If true, the obj passed in becomes
     *                             the execution scope of the listener
     * @return {Function} The wrapped function
     */
    this.wrap = function(fn, scope, override){
        var wrappedFn = function(e){
            YAHOO.ext.EventObject.setEvent(e);
            fn.call(override ? scope || window : window, YAHOO.ext.EventObject, scope);
        };
        return wrappedFn;
    };
    
    /**
     * Appends an event handler
     *
     * @param {Object}   element        The html element to assign the 
     *                             event to
     * @param {String}   eventName     The type of event to append
     * @param {Function} fn        The method the event invokes
     * @param {Object}   scope    An object that becomes the scope of the handler
     * @param {boolean}  override If true, the obj passed in becomes
     *                             the execution scope of the listener
     * @return {Function} The wrapper function created (to be used to remove the listener if necessary)
     */
    this.addListener = function(element, eventName, fn, scope, override){
        var wrappedFn = this.wrap(fn, scope, override);
        YAHOO.util.Event.addListener(element, eventName, wrappedFn);
        return wrappedFn;
    };
    
    /**
     * Removes an event handler
     *
     * @param {Object}   element        The html element to remove the 
     *                             event from
     * @param {String}   eventName     The type of event to append
     * @param {Function} wrappedFn        The wrapper method returned when adding the listener
     * @return {Boolean} True if a listener was actually removed
     */
    this.removeListener = function(element, eventName, wrappedFn){
        return YAHOO.util.Event.removeListener(element, eventName, wrappedFn);
    };
    
    /**
     * Appends an event handler (shorthand for addListener)
     *
     * @param {Object}   element        The html element to assign the 
     *                             event to
     * @param {String}   eventName     The type of event to append
     * @param {Function} fn        The method the event invokes
     * @param {Object}   scope    An arbitrary object that will be 
     *                             passed as a parameter to the handler
     * @param {boolean}  override If true, the obj passed in becomes
     *                             the execution scope of the listener
     * @return {Function} The wrapper function created (to be used to remove the listener if necessary)
     * @method
     */
    this.on = this.addListener;
    
    /**
     * Fires when the document is ready (before onload and before images are loaded). Can be 
     * accessed shorthanded Ext.onReady().
     * @param {Function} fn        The method the event invokes
     * @param {Object}   scope    An  object that becomes the scope of the handler
     * @param {boolean}  override If true, the obj passed in becomes
     *                             the execution scope of the listener
     */
    this.onDocumentReady = function(fn, scope, override){
        if(docReadyState){ // if it already fired
            fn.call(override? scope || window : window, scope);
            return;
        }
        if(!docReadyEvent){
            initDocReady();
        }
        docReadyEvent.subscribe(fn, scope, override);
    }
    
    /**
     * Fires when the window is resized and provides resize event buffering (50 milliseconds), passes new viewport width and height to handlers.
     * @param {Function} fn        The method the event invokes
     * @param {Object}   scope    An object that becomes the scope of the handler
     * @param {boolean}  override If true, the obj passed in becomes
     *                             the execution scope of the listener
     */
    this.onWindowResize = function(fn, scope, override){
        if(!resizeEvent){
            resizeEvent = new YAHOO.util.CustomEvent('windowresize');
            resizeTask = new YAHOO.ext.util.DelayedTask(function(){
                resizeEvent.fireDirect(YAHOO.util.Dom.getViewportWidth(), YAHOO.util.Dom.getViewportHeight());
            });
            YAHOO.util.Event.on(window, 'resize', function(){
                resizeTask.delay(50);
            });
        }
        resizeEvent.subscribe(fn, scope, override);
    };
    
    /**
     * Removes the passed window resize listener.
     * @param {Function} fn        The method the event invokes
     * @param {Object}   scope    The scope of handler
     */
    this.removeResizeListener = function(fn, scope){
        if(resizeEvent){
            resizeEvent.unsubscribe(fn, scope);
        }
    };
    
    this.fireResize = function(){
        if(resizeEvent){
            resizeEvent.fireDirect(YAHOO.util.Dom.getViewportWidth(), YAHOO.util.Dom.getViewportHeight());
        }   
    };
};

YAHOO.ext.onReady = YAHOO.ext.EventManager.onDocumentReady;

/**
 * @class YAHOO.ext.EventObject
 * EventObject exposes the Yahoo! UI Event functionality directly on the object
 * passed to your event handler. It exists mostly for convenience. It also fixes the annoying null checks automatically to cleanup your code 
 * (All the YAHOO.util.Event methods throw javascript errors if the passed event is null).
 * To get an EventObject instead of the standard browser event,
 * your must register your listener thru the {@link YAHOO.ext.EventManager} or directly on an Element
 * with {@link YAHOO.ext.Element#addManagedListener} or the shorthanded equivalent {@link YAHOO.ext.Element#mon}.<br>
 * Example:
 * <pre><code>
 fu<>nction handleClick(e){ // e is not a standard event object, it is a YAHOO.ext.EventObject
    e.preventDefault();
    var target = e.getTarget();
    ...
 }
 var myDiv = getEl('myDiv');
 myDiv.mon('click', handleClick);
 //or
 YAHOO.ext.EventManager.on('myDiv', 'click', handleClick);
 YAHOO.ext.EventManager.addListener('myDiv', 'click', handleClick);
 </code></pre>
 * @singleton
 */
YAHOO.ext.EventObject = new function(){
    /** The normal browser event */ 
    this.browserEvent = null;
    /** The button pressed in a mouse event */ 
    this.button = -1;
    /** True if the shift key was down during the event */ 
    this.shiftKey = false;
    /** True if the control key was down during the event */ 
    this.ctrlKey = false;
    /** True if the alt key was down during the event */ 
    this.altKey = false;
    
    /** Key constant @type Number */
    this.BACKSPACE = 8;
    /** Key constant @type Number */
    this.TAB = 9;
    /** Key constant @type Number */
    this.RETURN = 13;
    /** Key constant @type Number */
    this.ESC = 27;
    /** Key constant @type Number */
    this.SPACE = 32;
    /** Key constant @type Number */
    this.PAGEUP = 33;
    /** Key constant @type Number */
    this.PAGEDOWN = 34;
    /** Key constant @type Number */
    this.END = 35;
    /** Key constant @type Number */
    this.HOME = 36;
    /** Key constant @type Number */
    this.LEFT = 37;
    /** Key constant @type Number */
    this.UP = 38;
    /** Key constant @type Number */
    this.RIGHT = 39;
    /** Key constant @type Number */
    this.DOWN = 40;
    /** Key constant @type Number */
    this.DELETE = 46;
    /** Key constant @type Number */
    this.F5 = 116;

       /** @private */ 
    this.setEvent = function(e){
        if(e == this){ // already wrapped
            return this;
        }
        this.browserEvent = e;
        if(e){
            this.button = e.button;
            this.shiftKey = e.shiftKey;
            this.ctrlKey = e.ctrlKey;
            this.altKey = e.altKey;
        }else{
            this.button = -1;
            this.shiftKey = false;
            this.ctrlKey = false;
            this.altKey = false;
        }
        return this;
    };
    
    /**
     * Stop the event. Calls YAHOO.util.Event.stopEvent() if the event is not null.
     */ 
    this.stopEvent = function(){
        if(this.browserEvent){
            YAHOO.util.Event.stopEvent(this.browserEvent);
        }
    };
    
    /**
     * Prevents the browsers default handling of the event. Calls YAHOO.util.Event.preventDefault() if the event is not null.
     */ 
    this.preventDefault = function(){
        if(this.browserEvent){
            YAHOO.util.Event.preventDefault(this.browserEvent);
        }
    };
    
    /** @private */
    this.isNavKeyPress = function(){
        return (this.browserEvent.keyCode && this.browserEvent.keyCode >= 33 && this.browserEvent.keyCode <= 40);
    };
    
    /**
     * Cancels bubbling of the event. Calls YAHOO.util.Event.stopPropagation() if the event is not null.
     */ 
    this.stopPropagation = function(){
        if(this.browserEvent){
            YAHOO.util.Event.stopPropagation(this.browserEvent);
        }
    };
    
    /**
     * Gets the key code for the event. Returns value from YAHOO.util.Event.getCharCode() if the event is not null.
     * @return {Number}
     */ 
    this.getCharCode = function(){
        if(this.browserEvent){
            return YAHOO.util.Event.getCharCode(this.browserEvent);
        }
        return null;
    };
    
    /**
     * Returns a browsers key for a keydown event
     * @return {Number} The key code
     */
    this.getKey = function(){
        if(this.browserEvent){
            return this.browserEvent.keyCode || this.browserEvent.charCode;
        }
        return null;
    };
    
    /**
     * Gets the x coordinate of the event. Returns value from YAHOO.util.Event.getPageX() if the event is not null.
     * @return {Number}
     */ 
    this.getPageX = function(){
        if(this.browserEvent){
            return YAHOO.util.Event.getPageX(this.browserEvent);
        }
        return null;
    };
    
    /**
     * Gets the y coordinate of the event. Returns value from YAHOO.util.Event.getPageY() if the event is not null.
     * @return {Number}
     */ 
    this.getPageY = function(){
        if(this.browserEvent){
            return YAHOO.util.Event.getPageY(this.browserEvent);
        }
        return null;
    };
    
    /**
     * Gets the time of the event. Returns value from YAHOO.util.Event.getTime() if the event is not null.
     * @return {Number}
     */ 
    this.getTime = function(){
        if(this.browserEvent){
            return YAHOO.util.Event.getTime(this.browserEvent);
        }
        return null;
    };
    
    /**
     * Gets the page coordinates of the event. Returns value from YAHOO.util.Event.getXY() if the event is not null.
     * @return {Array} The xy values like [x, y]
     */ 
    this.getXY = function(){
        if(this.browserEvent){
            return YAHOO.util.Event.getXY(this.browserEvent);
        }
        return [];
    };
    
    /**
     * Gets the target for the event. Returns value from YAHOO.util.Event.getTarget() if the event is not null.
     * @return {HTMLelement}
     */ 
    this.getTarget = function(){
        if(this.browserEvent){
            return YAHOO.util.Event.getTarget(this.browserEvent);
        }
        return null;
    };
    
    /**
     * Walk up the DOM looking for a particular target - if the default target matches, it is returned.
     * @param {String} className The class name to look for or null
     * @param {String} tagName (optional) The tag name to look for
     * @return {HTMLelement}
     */ 
    this.findTarget = function(className, tagName){
        if(tagName) tagName = tagName.toLowerCase();
        if(this.browserEvent){
            function isMatch(el){
               if(!el){
                   return false;
               }
               if(className && !YAHOO.util.Dom.hasClass(el, className)){
                   return false;
               }
               if(tagName && el.tagName.toLowerCase() != tagName){
                   return false;
               }
               return true;
            };
            
            var t = this.getTarget();
            if(!t || isMatch(t)){
    		    return t;
    	    }
    	    var p = t.parentNode;
    	    var b = document.body;
    	    while(p && p != b){
                if(isMatch(p)){
                	return p;
                }
                p = p.parentNode;
            }
    	}
        return null;
    };
    /**
     * Gets the related target. Returns value from YAHOO.util.Event.getRelatedTarget() if the event is not null.
     * @return {HTMLElement}
     */ 
    this.getRelatedTarget = function(){
        if(this.browserEvent){
            return YAHOO.util.Event.getRelatedTarget(this.browserEvent);
        }
        return null;
    };
    
    /**
     * Normalizes mouse wheel delta across browsers
     * @return {Number} The delta 
     */
    this.getWheelDelta = function(){
        var e = this.browserEvent;
        var delta = 0;
        if(e.wheelDelta){ /* IE/Opera. */
            delta = e.wheelDelta/120;
            /* In Opera 9, delta differs in sign as compared to IE. */
            if(window.opera) delta = -delta;
        }else if(e.detail){ /* Mozilla case. */
            delta = -e.detail/3;
        }
        return delta;
    };
    
    /**
     * Returns true if the control, shift or alt key was pressed during this event.
     * @return {Boolean}
     */ 
    this.hasModifier = function(){
        return this.ctrlKey || this.altKey || this.shiftKey;
    };
    
    /**
     * Returns true if the target of this event equals el or is a child of el
     * @param {String/HTMLElement/Element} el
     * @return {Boolean}
     */
    this.within = function(el){
        el = getEl(el);
        var t = this.getTarget();
        return t && el && (el.dom == t || YAHOO.util.Dom.isAncestor(el.dom, t));
    }
}();
            
    
