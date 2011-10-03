/**
 * @class YAHOO.ext.Element
 * Wraps around a DOM element and provides convenient access to Yahoo 
 * UI library functionality (and more).<br><br>
 * Usage:<br>
 * <pre><code>
 * var el = YAHOO.ext.Element.get('myElementId');
 * // or the shorter
 * var el = getEl('myElementId');
 * </code></pre>
 * Using YAHOO.ext.Element.get() instead of calling the constructor directly ensures you get the same object 
 * each call instead of constructing a new one.<br><br>
 * For working with collections of Elements, see <a href="YAHOO.ext.CompositeElement.html">YAHOO.ext.CompositeElement</a>
 * @requires YAHOO.util.Dom
 * @requires YAHOO.util.Event
 * @requires YAHOO.util.CustomEvent 
 * @requires YAHOO.util.Anim (optional) to support animation
 * @requires YAHOO.util.Motion (optional) to support animation
 * @requires YAHOO.util.Easing (optional) to support animation
 * @constructor Create a new Element directly.
 * @param {String/HTMLElement} element
 * @param {<i>Boolean</i>} forceNew (optional) By default the constructor checks to see if there is already an instance of this element in the cache and if there is it returns the same instance. This will skip that check (useful for extending this class).
 */
YAHOO.ext.Element = function(element, forceNew){
    var dom = typeof element == 'string' ? 
            document.getElementById(element) : element;
    if(!dom){ // invalid id/element
        return null;
    }
    if(!forceNew && YAHOO.ext.Element.cache[dom.id]){ // element object already exists
        return YAHOO.ext.Element.cache[dom.id];
    }
    /**
     * The DOM element
     * @type HTMLElement
     */
    this.dom = dom;
    
    /**
     * The DOM element ID
     * @type String
     */
    this.id = dom.id;
        
    /**
     * The element's default display mode @type String
     */
    this.originalDisplay = YAHOO.util.Dom.getStyle(dom, 'display') || '';
    if(this.autoDisplayMode){
        if(this.originalDisplay == 'none'){
            this.setVisibilityMode(YAHOO.ext.Element.DISPLAY);
        }
    }
    if(this.originalDisplay == 'none'){
        this.originalDisplay = '';
    }
}

YAHOO.ext.Element.prototype = {    
    visibilityMode : 1,
    /**
     * The default unit to append to CSS values where a unit isn't provided (Defaults to px).
     * @type String
     */
    defaultUnit : 'px',
    /**
     * Sets the elements visibility mode. When setVisible() is called it
     * will use this to determine whether to set the visibility or the display property.
     * @param visMode Element.VISIBILITY or Element.DISPLAY
     * @return {YAHOO.ext.Element} this
     */
    setVisibilityMode : function(visMode){
        this.visibilityMode = visMode;
        return this;
    },
    /**
     * Convenience method for setVisibilityMode(Element.DISPLAY)
     * @param {String} display (optional) What to set display to when visible
     * @return {YAHOO.ext.Element} this
     */
    enableDisplayMode : function(display){
        this.setVisibilityMode(YAHOO.ext.Element.DISPLAY);
        if(typeof display != 'undefined') this.originalDisplay = display;
        return this;
    },
    
    /**
     * Perform Yahoo UI animation on this element. 
     * @param {Object} args The YUI animation control args
     * @param {<i>Float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeBoth)
     * @param {<i>Function</i>} animType (optional) YAHOO.util.Anim subclass to use. For example: YAHOO.util.Motion
     * @return {YAHOO.ext.Element} this
     */
    animate : function(args, duration, onComplete, easing, animType, stopAnims){
        this.anim(args, duration, onComplete, easing, animType);
        return this;
    },
    
    /**
     * @private Internal animation call
     */
    anim : function(args, duration, onComplete, easing, animType){
        animType = animType || YAHOO.util.Anim;
        var anim = new animType(this.dom, args, duration || .35, 
                easing || YAHOO.util.Easing.easeBoth);
        if(onComplete){
             anim.onComplete.subscribe(function(){
                if(typeof onComplete == 'function'){
                    onComplete.call(this);
                }else if(onComplete instanceof Array){
                    for(var i = 0; i < onComplete.length; i++){
                        var fn = onComplete[i];
                        if(fn) fn.call(this);
                    }
                }
            }, this, true);
        }
        anim.animate();
        return anim;
    },
    
    /**
     * Scrolls this element into view within the passed container.
     * @param {<i>String/HTMLElement/Element</i>} container (optional) The container element to scroll (defaults to document.body)
     * @return {YAHOO.ext.Element} this
     */
    scrollIntoView : function(container){
        var c = getEl(container || document.body, true);
        var cp = c.getStyle('position');
        var restorePos = false;
        if(cp != 'relative' && cp != 'absolute'){
            c.setStyle('position', 'relative');
            restorePos = true;
        }
        var el = this.dom;
        var childTop = parseInt(el.offsetTop, 10);
        var childBottom = childTop + el.offsetHeight;
        var containerTop = parseInt(c.dom.scrollTop, 10); // parseInt for safari bug
        var containerBottom = containerTop + c.dom.clientHeight;
        if(childTop < containerTop){
        	c.dom.scrollTop = childTop;
        }else if(childBottom > containerBottom){
            c.dom.scrollTop = childBottom-c.dom.clientHeight;
        }
        if(restorePos){
            c.setStyle('position', cp);
        }
        return this;
    },
        
    /** 
     * Measures the elements content height and updates height to match. Note, this function uses setTimeout and 
     * the new height may not be available immediately.
     * @param {<i>Boolean</i>} animate (optional) Animate the transition (Default is false)
     * @param {<i>Float</i>} duration (optional) Length of the animation. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeOut for hiding or YAHOO.util.Easing.easeIn for showing)
     * @return {YAHOO.ext.Element} this
     */
    autoHeight : function(animate, duration, onComplete, easing){
        var oldHeight = this.getHeight();
        this.clip();
        this.setHeight(1); // force clipping
        setTimeout(function(){
            var height = parseInt(this.dom.scrollHeight, 10); // parseInt for Safari
            if(!animate){
                this.setHeight(height);
                this.unclip();
                if(typeof onComplete == 'function'){
                    onComplete();
                }
            }else{
                this.setHeight(oldHeight); // restore original height
                this.setHeight(height, animate, duration, function(){
                    this.unclip();
                    if(typeof onComplete == 'function') onComplete();
                }.createDelegate(this), easing);
            }
        }.createDelegate(this), 0);
        return this;
    },
    
    contains : function(el){
        if(!el){return false;}
        return YAHOO.util.Dom.isAncestor(this.dom, el.dom ? el.dom : el);
    },
    
    /**
     * Checks whether the element is currently visible using both visibility and display properties.
     * @param {<i>Boolean</i>} deep True to walk the dom and see if parent elements are hidden.
     * @return {Boolean} true if the element is currently visible 
     */
    isVisible : function(deep) {
        var vis = YAHOO.util.Dom.getStyle(this.dom, 'visibility') != 'hidden' 
               && YAHOO.util.Dom.getStyle(this.dom, 'display') != 'none';
        if(!deep || !vis){
            return vis;
        }
        var p = this.dom.parentNode;
        while(p && p.tagName.toLowerCase() != 'body'){
            if(YAHOO.util.Dom.getStyle(p, 'visibility') == 'hidden' || YAHOO.util.Dom.getStyle(p, 'display') == 'none'){
                return false;
            }
            p = p.parentNode;
        }
        return true;
    },
    
    /**
     * Selects child nodes based on the passed CSS selector (the selector should not contain an id)
     * @param {String} selector The CSS selector
     * @param {Boolean} unique true to create a unique YAHOO.ext.Element for each child (defaults to a shared flyweight object)
     * @return {CompositeElement/CompositeElementLite} The composite element
     */
    select : function(selector, unique){
        return YAHOO.ext.Element.select('#' + this.dom.id + ' ' + selector, unique);  
    },
    
    /**
     * Initializes a YAHOO.util.DD object for this element.
     * @param {String} group The group the DD object is member of
     * @param {Object} config The DD config object
     * @param {Object} overrides An object containing methods to override/implement on the DD object
     * @return {YAHOO.util.DD} The DD object
     */
    initDD : function(group, config, overrides){
        var dd = new YAHOO.util.DD(YAHOO.util.Dom.generateId(this.dom), group, config);
        return YAHOO.ext.util.Config.apply(dd, overrides);
    },
   
    /**
     * Initializes a YAHOO.util.DDProxy object for this element.
     * @param {String} group The group the DDProxy object is member of
     * @param {Object} config The DDProxy config object
     * @param {Object} overrides An object containing methods to override/implement on the DDProxy object
     * @return {YAHOO.util.DDProxy} The DDProxy object
     */
    initDDProxy : function(group, config, overrides){
        var dd = new YAHOO.util.DDProxy(YAHOO.util.Dom.generateId(this.dom), group, config);
        return YAHOO.ext.util.Config.apply(dd, overrides);
    },
   
    /**
     * Initializes a YAHOO.util.DDTarget object for this element.
     * @param {String} group The group the DDTarget object is member of
     * @param {Object} config The DDTarget config object
     * @param {Object} overrides An object containing methods to override/implement on the DDTarget object
     * @return {YAHOO.util.DDTarget} The DDTarget object
     */
    initDDTarget : function(group, config, overrides){
        var dd = new YAHOO.util.DDTarget(YAHOO.util.Dom.generateId(this.dom), group, config);
        return YAHOO.ext.util.Config.apply(dd, overrides);
    },
   
    /**
     * Sets the visibility of the element (see details). If the visibilityMode is set to Element.DISPLAY, it will use 
     * the display property to hide the element, otherwise it uses visibility. The default is to hide and show using the visibility property.
     * @param {Boolean} visible Whether the element is visible
     * @param {<i>Boolean</i>} animate (optional) Fade the element in or out (Default is false)
     * @param {<i>Float</i>} duration (optional) How long the fade effect lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeOut for hiding or YAHOO.util.Easing.easeIn for showing)
     * @return {YAHOO.ext.Element} this
     */
     setVisible : function(visible, animate, duration, onComplete, easing){
        //if(this.isVisible() == visible) return; // nothing to do
        if(!animate || !YAHOO.util.Anim){
            if(this.visibilityMode == YAHOO.ext.Element.DISPLAY){
                this.setDisplayed(visible);
            }else{
                YAHOO.util.Dom.setStyle(this.dom, 'visibility', visible ? 'visible' : 'hidden');
            }
        }else{
            // make sure they can see the transition
            this.setOpacity(visible?0:1);
            YAHOO.util.Dom.setStyle(this.dom, 'visibility', 'visible');
            if(this.visibilityMode == YAHOO.ext.Element.DISPLAY){
                this.setDisplayed(true);
            }
            var args = {opacity: { from: (visible?0:1), to: (visible?1:0) }};
            var anim = new YAHOO.util.Anim(this.dom, args, duration || .35, 
                easing || (visible ? YAHOO.util.Easing.easeIn : YAHOO.util.Easing.easeOut));
            anim.onComplete.subscribe((function(){
                if(this.visibilityMode == YAHOO.ext.Element.DISPLAY){
                    this.setDisplayed(visible);
                }else{
                    YAHOO.util.Dom.setStyle(this.dom, 'visibility', visible ? 'visible' : 'hidden');
                }
            }).createDelegate(this));
            if(onComplete){
                anim.onComplete.subscribe(onComplete);
            }
            anim.animate();
        }
        return this;
    },
    
    /**
     * Returns true if display is not "none"
     * @return {Boolean}
     */
    isDisplayed : function() {
        return YAHOO.util.Dom.getStyle(this.dom, 'display') != 'none';
    },
    
    /**
     * Toggles the elements visibility or display, depending on visibility mode.
     * @param {<i>Boolean</i>} animate (optional) Fade the element in or out (Default is false)
     * @param {<i>float</i>} duration (optional) How long the fade effect lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeOut for hiding or YAHOO.util.Easing.easeIn for showing)
     * @return {YAHOO.ext.Element} this
     */
    toggle : function(animate, duration, onComplete, easing){
        this.setVisible(!this.isVisible(), animate, duration, onComplete, easing);
        return this;
    },
    
    /**
     * Sets the css display. Uses originalDisplay if value is a boolean true.
     * @param {Boolean} value Boolean to display the element using it's default display or a string to set the display directly
     * @return {YAHOO.ext.Element} this
     */
    setDisplayed : function(value) {
        if(typeof value == 'boolean'){
           value = value ? this.originalDisplay : 'none';
        }
        YAHOO.util.Dom.setStyle(this.dom, 'display', value);
        return this;
    },
    
    /**
     * Tries to focus the element. Any exceptions are caught.
     * @return {YAHOO.ext.Element} this
     */
    focus : function() {
        try{
            this.dom.focus();
        }catch(e){}
        return this;
    },
    
    /**
     * Tries to blur the element. Any exceptions are caught.
     * @return {YAHOO.ext.Element} this
     */
    blur : function() {
        try{
            this.dom.blur();
        }catch(e){}
        return this;
    },
    
    /**
     * Add a CSS class to the element.
     * @param {String/Array} className The CSS class to add or an array of classes
     * @return {YAHOO.ext.Element} this
     */
    addClass : function(className){
        if(className instanceof Array){
            for(var i = 0, len = className.length; i < len; i++) {
            	this.addClass(className[i]);
            }
        }else{
            if(!this.hasClass(className)){
                this.dom.className = this.dom.className + ' ' + className;
            }
        }
        return this;
    },
    
    /**
     * Adds the passed className to this element and removes the class from all siblings
     * @param {String} className The className to add
     * @return {YAHOO.ext.Element} this
     */
    radioClass : function(className){
        var siblings = this.dom.parentNode.childNodes;
        for(var i = 0; i < siblings.length; i++) {
        	var s = siblings[i];
        	if(s.nodeType == 1){
        	    YAHOO.util.Dom.removeClass(s, className);
        	}
        }
        this.addClass(className);
        return this;
    },
    /**
     * Removes a CSS class from the element.
     * @param {String/Array} className The CSS class to remove or an array of classes
     * @return {YAHOO.ext.Element} this
     */
    removeClass : function(className){
        if(!className){
            return this;
        }
        if(className instanceof Array){
            for(var i = 0, len = className.length; i < len; i++) {
            	this.removeClass(className[i]);
            }
        }else{
            var re = new RegExp('(?:^|\\s+)' + className + '(?:\\s+|$)', 'g');
            var c = this.dom.className;
            if(re.test(c)){
                this.dom.className = c.replace(re, ' ');
            }
        }
        return this;
    },
    
    /**
     * Toggles (adds or removes) the passed class.
     * @param {String} className
     * @return {YAHOO.ext.Element} this
     */
    toggleClass : function(className){
        if(this.hasClass(className)){
            this.removeClass(className);
        }else{
            this.addClass(className);
        }
        return this;
    },
    
    /**
     * Checks if a CSS class is in use by the element.
     * @param {String} className The CSS class to check
     * @return {Boolean} true or false
     */
    hasClass : function(className){
        var re = new RegExp('(?:^|\\s+)' + className + '(?:\\s+|$)');
        return re.test(this.dom.className);
    },
    
    /**
     * Replaces a CSS class on the element with another.
     * @param {String} oldClassName The CSS class to replace
     * @param {String} newClassName The replacement CSS class
     * @return {YAHOO.ext.Element} this
     */
    replaceClass : function(oldClassName, newClassName){
        this.removeClass(oldClassName);
        this.addClass(newClassName);
        return this;
    },
    
    /**
       * Normalizes currentStyle and ComputedStyle.
       * @param {String} property The style property whose value is returned.
       * @return {String} The current value of the style property for this element.
       */
    getStyle : function(name){
        return YAHOO.util.Dom.getStyle(this.dom, name);
    },
    
    /**
       * Wrapper for setting style properties, also takes single object parameter of multiple styles
       * @param {String/Object} property The style property to be set or an object of multiple styles.
       * @param {String} val (optional) The value to apply to the given property or null if an object was passed.
       * @return {YAHOO.ext.Element} this
     */
    setStyle : function(name, value){
        if(typeof name == 'string'){
            YAHOO.util.Dom.setStyle(this.dom, name, value);
        }else{
            var D = YAHOO.util.Dom;
            for(var style in name){
                if(typeof name[style] != 'function'){
                   D.setStyle(this.dom, style, name[style]);
                }
            }
        }
        return this;
    },
    
    /**
     * More flexible version of {@link #setStyle} for setting style properties.
     * @param {String/Object/Function} styles A style specification string eg "width:100px", or object in the form {width:"100px"}, or
     * a function which returns such a specification.
     * @return {YAHOO.ext.Element} this
     */
    applyStyles : function(style){
       YAHOO.ext.DomHelper.applyStyles(this.dom, style);
    },
    
    /**
       * Gets the current X position of the element based on page coordinates.  Element must be part of the DOM tree to have page coordinates (display:none or elements not appended return false).
       @ return {Number} The X position of the element
       */
    getX : function(){
        return YAHOO.util.Dom.getX(this.dom);
    },
    
    /**
       * Gets the current Y position of the element based on page coordinates.  Element must be part of the DOM tree to have page coordinates (display:none or elements not appended return false).
       @ return {Number} The Y position of the element
       */
    getY : function(){
        return YAHOO.util.Dom.getY(this.dom);
    },
    
    /**
       * Gets the current position of the element based on page coordinates.  Element must be part of the DOM tree to have page coordinates (display:none or elements not appended return false).
       @ return {Array} The XY position of the element
       */
    getXY : function(){
        return YAHOO.util.Dom.getXY(this.dom);
    },
    
    /**
       * Sets the X position of the element based on page coordinates.  Element must be part of the DOM tree to have page coordinates (display:none or elements not appended return false).
       @param {Number} The X position of the element
      * @param {<i>Boolean</i>} animate (optional) Animate the transition (Default is false)
     * @param {<i>float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeBoth)
     * @return {YAHOO.ext.Element} this
     */
    setX : function(x, animate, duration, onComplete, easing){
        if(!animate || !YAHOO.util.Anim){
            YAHOO.util.Dom.setX(this.dom, x);
        }else{
            this.setXY([x, this.getY()], animate, duration, onComplete, easing);
        }
        return this;
    },
    
    /**
       * Sets the Y position of the element based on page coordinates.  Element must be part of the DOM tree to have page coordinates (display:none or elements not appended return false).
       @param {Number} The Y position of the element
      * @param {<i>Boolean</i>} animate (optional) Animate the transition (Default is false)
     * @param {<i>float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeBoth)
     * @return {YAHOO.ext.Element} this
      */
    setY : function(y, animate, duration, onComplete, easing){
        if(!animate || !YAHOO.util.Anim){
            YAHOO.util.Dom.setY(this.dom, y);
        }else{
            this.setXY([this.getX(), y], animate, duration, onComplete, easing);
        }
        return this;
    },
    
    /**
     * Set the element's left position directly using CSS style (instead of setX())
     * @param {String} left The left CSS property value
     * @return {YAHOO.ext.Element} this
     */
    setLeft : function(left){
        YAHOO.util.Dom.setStyle(this.dom, 'left', this.addUnits(left));
        return this;
    },
    
    /**
     * Set the element's top position directly using CSS style (instead of setY())
     * @param {String} top The top CSS property value
     * @return {YAHOO.ext.Element} this
     */
    setTop : function(top){
        YAHOO.util.Dom.setStyle(this.dom, 'top', this.addUnits(top));
        return this;
    },
    
    /**
     * Set the element's css right style
     * @param {String} right The right CSS property value
     * @return {YAHOO.ext.Element} this
     */
    setRight : function(right){
        YAHOO.util.Dom.setStyle(this.dom, 'right', this.addUnits(right));
        return this;
    },
    
    /**
     * Set the element's css bottom style
     * @param {String} bottom The bottom CSS property value
     * @return {YAHOO.ext.Element} this
     */
    setBottom : function(bottom){
        YAHOO.util.Dom.setStyle(this.dom, 'bottom', this.addUnits(bottom));
        return this;
    },
    
    /**
     * Set the position of the element in page coordinates, regardless of how the element is positioned.
     * The element must be part of the DOM tree to have page coordinates (display:none or elements not appended return false).
     * @param {Array} pos Contains X & Y [x, y] values for new position (coordinates are page-based)
     * @param {<i>Boolean</i>} animate (optional) Animate the transition (Default is false)
     * @param {<i>float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeBoth)
      * @return {YAHOO.ext.Element} this
       */
    setXY : function(pos, animate, duration, onComplete, easing){
        if(!animate || !YAHOO.util.Anim){
            YAHOO.util.Dom.setXY(this.dom, pos);
        }else{
            this.anim({points: {to: pos}}, duration, onComplete, easing, YAHOO.util.Motion);
        }
        return this;
    },
    
    /**
     * Set the position of the element in page coordinates, regardless of how the element is positioned.
     * The element must be part of the DOM tree to have page coordinates (display:none or elements not appended return false).
     * @param {Number} x X value for new position (coordinates are page-based)
     * @param {Number} y Y value for new position (coordinates are page-based)
     * @param {<i>Boolean</i>} animate (optional) Animate the transition (Default is false)
     * @param {<i>float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeBoth)
     * @return {YAHOO.ext.Element} this
     */
    setLocation : function(x, y, animate, duration, onComplete, easing){
        this.setXY([x, y], animate, duration, onComplete, easing);
        return this;
    },
    
    /**
     * Set the position of the element in page coordinates, regardless of how the element is positioned.
     * The element must be part of the DOM tree to have page coordinates (display:none or elements not appended return false).
     * @param {Number} x X value for new position (coordinates are page-based)
     * @param {Number} y Y value for new position (coordinates are page-based)
     * @param {<i>Boolean</i>} animate (optional) Animate the transition (Default is false)
     * @param {<i>float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeBoth)
     * @return {YAHOO.ext.Element} this
     */
    moveTo : function(x, y, animate, duration, onComplete, easing){
        //YAHOO.util.Dom.setStyle(this.dom, 'left', this.addUnits(x));
        //YAHOO.util.Dom.setStyle(this.dom, 'top', this.addUnits(y));
        this.setXY([x, y], animate, duration, onComplete, easing);
        return this;
    },
    
    /**
       * Returns the region of the given element.
       * The element must be part of the DOM tree to have a region (display:none or elements not appended return false).
       * @return {Region} A YAHOO.util.Region containing "top, left, bottom, right" member data.
       */
    getRegion : function(){
        return YAHOO.util.Dom.getRegion(this.dom);
    },
    
    /**
     * Returns the offset height of the element
     * @param {Boolean} contentHeight (optional) true to get the height minus borders and padding
     * @return {Number} The element's height
     */
    getHeight : function(contentHeight){
        var h = this.dom.offsetHeight;
        return contentHeight !== true ? h : h-this.getBorderWidth('tb')-this.getPadding('tb');
    },
    
    /**
     * Returns the offset width of the element
     * @param {Boolean} contentWidth (optional) true to get the width minus borders and padding
     * @return {Number} The element's width
     */
    getWidth : function(contentWidth){
        var w = this.dom.offsetWidth;
        return contentWidth !== true ? w : w-this.getBorderWidth('lr')-this.getPadding('lr');
    },
    
    /**
     * Returns the size of the element
     * @param {Boolean} contentSize (optional) true to get the width/size minus borders and padding
     * @return {Object} An object containing the element's size {width: (element width), height: (element height)}
     */
    getSize : function(contentSize){
        return {width: this.getWidth(contentSize), height: this.getHeight(contentSize)};
    },
    
    /** @private */
    adjustWidth : function(width){
        if(typeof width == 'number'){
            if(this.autoBoxAdjust && !this.isBorderBox()){
               width -= (this.getBorderWidth('lr') + this.getPadding('lr'));
            }
            if(width < 0){
                width = 0;
            }
        }
        return width;
    },
    
    /** @private */
    adjustHeight : function(height){
        if(typeof height == 'number'){
           if(this.autoBoxAdjust && !this.isBorderBox()){
               height -= (this.getBorderWidth('tb') + this.getPadding('tb'));
           }
           if(height < 0){
               height = 0;
           }
        }
        return height;
    },
    
    /**
     * Set the width of the element
     * @param {Number} width The new width
     * @param {<i>Boolean</i>} animate (optional) Animate the transition (Default is false)
     * @param {<i>float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeOut if width is larger or YAHOO.util.Easing.easeIn if it is smaller)
     * @return {YAHOO.ext.Element} this
     */
    setWidth : function(width, animate, duration, onComplete, easing){
        width = this.adjustWidth(width);
        if(!animate || !YAHOO.util.Anim){
            this.dom.style.width = this.addUnits(width); 
            //YAHOO.util.Dom.setStyle(this.dom, 'width', this.addUnits(width));
        }else{
            this.anim({width: {to: width}}, duration, onComplete, 
                easing || (width > this.getWidth() ? YAHOO.util.Easing.easeOut : YAHOO.util.Easing.easeIn));
        }
        return this;
    },
    
    /**
     * Set the height of the element
     * @param {Number} height The new height
     * @param {<i>Boolean</i>} animate (optional) Animate the transition (Default is false)
     * @param {<i>float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeOut if height is larger or YAHOO.util.Easing.easeIn if it is smaller)
     * @return {YAHOO.ext.Element} this
     */
     setHeight : function(height, animate, duration, onComplete, easing){
        height = this.adjustHeight(height);
        if(!animate || !YAHOO.util.Anim){
            this.dom.style.height = this.addUnits(height); 
            //YAHOO.util.Dom.setStyle(this.dom, 'height', this.addUnits(height));
        }else{
            this.anim({height: {to: height}}, duration, onComplete,  
                   easing || (height > this.getHeight() ? YAHOO.util.Easing.easeOut : YAHOO.util.Easing.easeIn));
        }
        return this;
    },
    
    /**
     * Set the size of the element. If animation is true, both width an height will be animated concurrently.
     * @param {Number} width The new width
     * @param {Number} height The new height
     * @param {<i>Boolean</i>} animate (optional) Animate the transition (Default is false)
     * @param {<i>float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeBoth)
     * @return {YAHOO.ext.Element} this
     */
     setSize : function(width, height, animate, duration, onComplete, easing){
        width = this.adjustWidth(width); height = this.adjustHeight(height);
        if(!animate || !YAHOO.util.Anim){
            this.dom.style.width = this.addUnits(width);
            this.dom.style.height = this.addUnits(height);
        }else{
            this.anim({width: {to: width}, height: {to: height}}, duration, onComplete, easing);
        }
        return this;
    },
    
    /**
     * Sets the element's position and size in one shot. If animation is true then width, height, x and y will be animated concurrently.
     * @param {Number} x X value for new position (coordinates are page-based)
     * @param {Number} y Y value for new position (coordinates are page-based)
     * @param {Number} width The new width
     * @param {Number} height The new height
     * @param {<i>Boolean</i>} animate (optional) Animate the transition (Default is false)
     * @param {<i>float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeBoth)
     * @return {YAHOO.ext.Element} this
     */
    setBounds : function(x, y, width, height, animate, duration, onComplete, easing){
        if(!animate || !YAHOO.util.Anim){
            this.setSize(width, height);
            this.setLocation(x, y);
        }else{
            width = this.adjustWidth(width); height = this.adjustHeight(height);
            this.anim({points: {to: [x, y]}, width: {to: width}, height: {to: height}}, duration, onComplete, easing, YAHOO.util.Motion);
        }
        return this;
    },
    
    /**
     * Sets the element's position and size the the specified region. If animation is true then width, height, x and y will be animated concurrently.
     * @param {YAHOO.util.Region} region The region to fill
     * @param {<i>Boolean</i>} animate (optional) Animate the transition (Default is false)
     * @param {<i>float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeBoth)
     * @return {YAHOO.ext.Element} this
     */
    setRegion : function(region, animate, duration, onComplete, easing){
        this.setBounds(region.left, region.top, region.right-region.left, region.bottom-region.top, animate, duration, onComplete, easing);
        return this;
    },
    
    /**
     * Appends an event handler to this element
     * @param {String}   eventName     The type of event to listen for
     * @param {Function} handler        The method the event invokes
     * @param {<i>Object</i>}   scope  (optional)  An arbitrary object that will be 
     *                             passed as a parameter to the handler
     * @param {<i>boolean</i>}  override (optional) If true, the obj passed in becomes
     *                             the execution scope of the listener
     * @return {YAHOO.ext.Element} this
     */
    addListener : function(eventName, handler, scope, override){
        YAHOO.util.Event.addListener(this.dom, eventName, handler, scope || this, true);
        return this;
    },
    /**
     * Appends an event handler to this element that is buffered. If the event is triggered more than once
     * in the specified time-frame, only the last one actually fires.
     * @param {String}   eventName     The type of event to listen for
     * @param {Function} handler        The method the event invokes
     * @param {<i>Object</i>}   scope  (optional) The scope (this object) for the handler
     * @param {<i>Number</i>}  millis (optional) The number of milliseconds to buffer (defaults to 250)
     * @return {Function} The wrapped function that was created (can be used to remove the listener)
     */
    bufferedListener : function(eventName, fn, scope, millis){
        var task = new YAHOO.ext.util.DelayedTask();
        scope = scope || this;
        var newFn = function(e){
            task.delay(millis || 250, fn, scope, Array.prototype.slice.call(arguments, 0));
        }
        this.addListener(eventName, newFn);
        return newFn;
    },
    
    
    /**
     * Appends an event handler to this element. The difference between this function and addListener is this
     * function prevents the default action, and if set stops propagation (bubbling) as well
     * @param {String}   eventName     The type of event to listen for
     * @param {Boolean}   stopPropagation     Whether to also stopPropagation (bubbling) 
     * @param {Function} handler        The method the event invokes
     * @param {<i>Object</i>}   scope  (optional)  An arbitrary object that will be 
     *                             passed as a parameter to the handler
     * @param {<i>boolean</i>}  override (optional) If true, the obj passed in becomes
     *                             the execution scope of the listener
     * @return {YAHOO.ext.Element} this
     */
    addHandler : function(eventName, stopPropagation, handler, scope, override){
        var fn = YAHOO.ext.Element.createStopHandler(stopPropagation, handler, scope || this, true);
        YAHOO.util.Event.addListener(this.dom, eventName, fn);
        return fn;
    },
    
    /**
     * Appends an event handler to this element (Same as addListener)
     * @param {String}   eventName     The type of event to listen for
     * @param {Function} handler        The method the event invokes
     * @param {<i>Object</i>}   scope (optional)   An arbitrary object that will be 
     *                             passed as a parameter to the handler
     * @param {<i>boolean</i>}  override (optional) If true, the obj passed in becomes
     *                             the execution scope of the listener
     * @return {YAHOO.ext.Element} this
     */
    on : function(eventName, handler, scope, override){
        YAHOO.util.Event.addListener(this.dom, eventName, handler, scope || this, true);
        return this;
    },
    
    /**
     * Append a managed listener - See {@link YAHOO.ext.EventObject} for more details. Use mon() for a shorter version.
     * @param {String}   eventName     The type of event to listen for
     * @param {Function} fn        The method the event invokes
     * @param {<i>Object</i>}   scope  (optional)  An arbitrary object that will be 
     *                             passed as a parameter to the handler
     * @param {<i>boolean</i>}  override (optional) If true, the obj passed in becomes
     *                             the execution scope of the listener
     * @return {Function} The EventManager wrapped function that can be used to remove the listener
     */
    addManagedListener : function(eventName, fn, scope, override){
        return YAHOO.ext.EventManager.on(this.dom, eventName, fn, scope || this, true);
    },
    
    /** 
     * Append a managed listener (shorthanded for {@link #addManagedListener}) 
     * @param {String}   eventName     The type of event to listen for
     * @param {Function} fn        The method the event invokes
     * @param {<i>Object</i>}   scope  (optional)  An arbitrary object that will be 
     *                             passed as a parameter to the handler
     * @param {<i>boolean</i>}  override (optional) If true, the obj passed in becomes
     *                             the execution scope of the listener
     * @return {Function} The EventManager wrapped function that can be used to remove the listener
     */
    mon : function(eventName, fn, scope, override){
        return YAHOO.ext.EventManager.on(this.dom, eventName, fn, scope || this, true);
    },
    /**
     * Removes an event handler from this element
     * @param {String} sType the type of event to remove
     * @param {Function} fn the method the event invokes
     * @param {Object} scope
     * @return {YAHOO.ext.Element} this
     */
    removeListener : function(eventName, handler, scope){
        YAHOO.util.Event.removeListener(this.dom, eventName, handler);
        return this;
    },
    
    /**
     * Removes all previous added listeners from this element
     * @return {YAHOO.ext.Element} this
     */
    removeAllListeners : function(){
        YAHOO.util.Event.purgeElement(this.dom);
        return this;
    },
    
    
    /**
     * Set the opacity of the element
     * @param {Float} opacity The new opacity. 0 = transparent, .5 = 50% visibile, 1 = fully visible, etc
     * @param {<i>Boolean</i>} animate (optional) Animate (fade) the transition (Default is false)
     * @param {<i>Float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeOut if height is larger or YAHOO.util.Easing.easeIn if it is smaller)
     * @return {YAHOO.ext.Element} this
     */
     setOpacity : function(opacity, animate, duration, onComplete, easing){
        if(!animate || !YAHOO.util.Anim){
            YAHOO.util.Dom.setStyle(this.dom, 'opacity', opacity);
        }else{
            this.anim({opacity: {to: opacity}}, duration, onComplete, easing);
        }
        return this;
    },
    
    /**
     * Gets the left X coordinate
     * @param {Boolean} local True to get the local css position instead of page coordinate
     * @return {Number}
     */
    getLeft : function(local){
        if(!local){
            return this.getX();
        }else{
            return parseInt(this.getStyle('left'), 10) || 0;
        }
    },
    
    /**
     * Gets the right X coordinate of the element (element X position + element width)
     * @param {Boolean} local True to get the local css position instead of page coordinate
     * @return {Number}
     */
    getRight : function(local){
        if(!local){
            return this.getX() + this.getWidth();
        }else{
            return (this.getLeft(true) + this.getWidth()) || 0;
        }
    },
    
    /**
     * Gets the top Y coordinate
     * @param {Boolean} local True to get the local css position instead of page coordinate
     * @return {Number}
     */
    getTop : function(local) {
        if(!local){
            return this.getY();
        }else{
            return parseInt(this.getStyle('top'), 10) || 0;
        }
    },
    
    /**
     * Gets the bottom Y coordinate of the element (element Y position + element height)
     * @param {Boolean} local True to get the local css position instead of page coordinate
     * @return {Number}
     */
    getBottom : function(local){
        if(!local){
            return this.getY() + this.getHeight();
        }else{
            return (this.getTop(true) + this.getHeight()) || 0;
        }
    },
    
    /**
    * Set the element as absolute positioned with the specified z-index
    * @param {<i>Number</i>} zIndex (optional)
    * @return {YAHOO.ext.Element} this
     */
    setAbsolutePositioned : function(zIndex){
        this.setStyle('position', 'absolute');
        if(zIndex){
            this.setStyle('z-index', zIndex);
        }
        return this;
    },
    
    /**
    * Set the element as relative positioned with the specified z-index
    * @param {<i>Number</i>} zIndex (optional)
    * @return {YAHOO.ext.Element} this
     */
    setRelativePositioned : function(zIndex){
        this.setStyle('position', 'relative');
        if(zIndex){
            this.setStyle('z-index', zIndex);
        }
        return this;
    },
    
    /**
    * Clear positioning back to the default when the document was loaded
    * @return {YAHOO.ext.Element} this
     */
    clearPositioning : function(){
        this.setStyle('position', '');
        this.setStyle('left', '');
        this.setStyle('right', '');
        this.setStyle('top', '');
        this.setStyle('bottom', '');
        return this;
    },
    
    /**
    * Gets an object with all CSS positioning properties. Useful along with setPostioning to get 
    * snapshot before performing an update and then restoring the element.
    * @return {Object} 
    */
    getPositioning : function(){
        return {
            'position' : this.getStyle('position'),
            'left' : this.getStyle('left'),
            'right' : this.getStyle('right'),
            'top' : this.getStyle('top'),
            'bottom' : this.getStyle('bottom')
        };
    },
    
    /**
     * Gets the width of the border(s) for the specified side(s)
     * @param {String} side Can be t, l, r, b or any combination of those to add multiple values. For example, 
     * passing lr would get the border (l)eft width + the border (r)ight width.
     * @return {Number} The width of the sides passed added together
     */
    getBorderWidth : function(side){
        return this.addStyles(side, YAHOO.ext.Element.borders);
    },
    
    /**
     * Gets the width of the padding(s) for the specified side(s)
     * @param {String} side Can be t, l, r, b or any combination of those to add multiple values. For example, 
     * passing lr would get the padding (l)eft + the padding (r)ight.
     * @return {Number} The padding of the sides passed added together
     */
    getPadding : function(side){
        return this.addStyles(side, YAHOO.ext.Element.paddings);
    },
    
    /**
    * Set positioning with an object returned by getPositioning().
    * @param {Object} posCfg
    * @return {YAHOO.ext.Element} this
     */
    setPositioning : function(positionCfg){
        if(positionCfg.position)this.setStyle('position', positionCfg.position);
        if(positionCfg.left)this.setLeft(positionCfg.left);
        if(positionCfg.right)this.setRight(positionCfg.right);
        if(positionCfg.top)this.setTop(positionCfg.top);
        if(positionCfg.bottom)this.setBottom(positionCfg.bottom);
        return this;
    },
    
    
    /**
     * Quick set left and top adding default units
     * @return {YAHOO.ext.Element} this
     */
     setLeftTop : function(left, top){
        this.dom.style.left = this.addUnits(left);
        this.dom.style.top = this.addUnits(top);
        return this;
    },
    
    /**
     * Move this element relative to it's current position.
     * @param {String} direction Possible values are: 'l','left' - 'r','right' - 't','top','up' - 'b','bottom','down'.
     * @param {Number} distance How far to move the element in pixels
     * @param {<i>Boolean</i>} animate (optional) Animate the movement (Default is false)
     * @param {<i>Float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. 
     * @return {YAHOO.ext.Element} this
     */
     move : function(direction, distance, animate, duration, onComplete, easing){
        var xy = this.getXY();
        direction = direction.toLowerCase();
        switch(direction){
            case 'l':
            case 'left':
                this.moveTo(xy[0]-distance, xy[1], animate, duration, onComplete, easing);
                break;
           case 'r':
           case 'right':
                this.moveTo(xy[0]+distance, xy[1], animate, duration, onComplete, easing);
                break;
           case 't':
           case 'top':
           case 'up':
                this.moveTo(xy[0], xy[1]-distance, animate, duration, onComplete, easing);
                break;
           case 'b':
           case 'bottom':
           case 'down':
                this.moveTo(xy[0], xy[1]+distance, animate, duration, onComplete, easing);
                break;
        }
        return this;
    },
    
    /**
     *  Store the current overflow setting and clip overflow on the element - use {@link #unclip} to remove
     * @return {YAHOO.ext.Element} this
     */
    clip : function(){
        if(!this.isClipped){
           this.isClipped = true;
           this.originalClip = {
               'o': this.getStyle('overflow'), 
               'x': this.getStyle('overflow-x'),
               'y': this.getStyle('overflow-y')
           };
           this.setStyle('overflow', 'hidden');
           this.setStyle('overflow-x', 'hidden');
           this.setStyle('overflow-y', 'hidden');
        }
        return this;
    },
    
    /**
     *  Return clipping (overflow) to original clipping before clip() was called
     * @return {YAHOO.ext.Element} this
     */
    unclip : function(){
        if(this.isClipped){
            this.isClipped = false;
            var o = this.originalClip;
            if(o.o){this.setStyle('overflow', o.o);}
            if(o.x){this.setStyle('overflow-x', o.x);}
            if(o.y){this.setStyle('overflow-y', o.y);}
        }
        return this;
    },
    
    /**
     * Align this element with another element.
     * @param {String/HTMLElement/YAHOO.ext.Element} element The element to align to.
     * @param {String} position The position to align to. Possible values are 'tl' - top left, 'tr' - top right, 'bl' - bottom left, and 'br' - bottom right. 
     * @param {<i>Array</i>} offsets (optional) Offset the positioning by [x, y]
     * @param {<i>Boolean</i>} animate (optional) Animate the movement (Default is false)
     * @param {<i>Float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. 
     * @return {YAHOO.ext.Element} this
     */
     alignTo : function(element, position, offsets, animate, duration, onComplete, easing){
        var otherEl = getEl(element);
        if(!otherEl){
            return this; // must not exist
        }
        offsets = offsets || [0, 0];
        var r = otherEl.getRegion();
        position = position.toLowerCase();
        switch(position){
           case 'bl':
                this.moveTo(r.left + offsets[0], r.bottom + offsets[1], 
                            animate, duration, onComplete, easing);
                break;
           case 'br':
                this.moveTo(r.right + offsets[0], r.bottom + offsets[1], 
                            animate, duration, onComplete, easing);
                break;
           case 'tl':
                this.moveTo(r.left + offsets[0], r.top + offsets[1], 
                            animate, duration, onComplete, easing);
                break;
           case 'tr':
                this.moveTo(r.right + offsets[0], r.top + offsets[1], 
                            animate, duration, onComplete, easing);
                break;
        }
        return this;
    },
    
    /**
    * Clears any opacity settings from this element. Required in some cases for IE.
    * @return {YAHOO.ext.Element} this
     */
    clearOpacity : function(){
        if (window.ActiveXObject) {
            this.dom.style.filter = '';
        } else {
            this.dom.style.opacity = '';
            this.dom.style['-moz-opacity'] = '';
            this.dom.style['-khtml-opacity'] = '';
        }
        return this;
    },
    
    /**
    * Hide this element - Uses display mode to determine whether to use "display" or "visibility". See {@link #setVisible}.
    * @param {<i>Boolean</i>} animate (optional) Animate (fade) the transition (Default is false)
     * @param {<i>Float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeBoth)
    * @return {YAHOO.ext.Element} this
      */
    hide : function(animate, duration, onComplete, easing){
        this.setVisible(false, animate, duration, onComplete, easing);
        return this;
    },
    
    /**
    * Show this element - Uses display mode to determine whether to use "display" or "visibility". See {@link #setVisible}.
    * @param {<i>Boolean</i>} animate (optional) Animate (fade in) the transition (Default is false)
     * @param {<i>Float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeBoth)
     * @return {YAHOO.ext.Element} this
     */
    show : function(animate, duration, onComplete, easing){
        this.setVisible(true, animate, duration, onComplete, easing);
        return this;
    },
    
    /**
     * @private Test if size has a unit, otherwise appends the default 
     */
    addUnits : function(size){
        if(size === '' || size == 'auto' || typeof size == 'undefined'){
            return size;
        }
        if(typeof size == 'number' || !YAHOO.ext.Element.unitPattern.test(size)){
            return size + this.defaultUnit;
        }
        return size;
    },
    
    /**
     * Temporarily enables offsets (width,height,x,y) for an element with display:none, use endMeasure() when done.
     * @return {YAHOO.ext.Element} this
     */
    beginMeasure : function(){
        var el = this.dom;
        if(el.offsetWidth || el.offsetHeight){
            return this; // offsets work already
        }
        var changed = [];
        var p = this.dom; // start with this element
        while((!el.offsetWidth && !el.offsetHeight) && p && p.tagName && p.tagName.toLowerCase() != 'body'){
            if(YAHOO.util.Dom.getStyle(p, 'display') == 'none'){
                changed.push({el: p, visibility: YAHOO.util.Dom.getStyle(p, 'visibility')});
                p.style.visibility = 'hidden';
                p.style.display = 'block';
            }
            p = p.parentNode;
        }
        this._measureChanged = changed;
        return this;
               
    },
    
    /**
     * Restores displays to before beginMeasure was called
     * @return {YAHOO.ext.Element} this
     */
    endMeasure : function(){
        var changed = this._measureChanged;
        if(changed){
            for(var i = 0, len = changed.length; i < len; i++) {
            	var r = changed[i];
            	r.el.style.visibility = r.visibility;
                r.el.style.display = 'none';
            }
            this._measureChanged = null;
        }
        return this;
    },
    
    /**
    * Update the innerHTML of this element, optionally searching for and processing scripts
    * @param {String} html The new HTML
    * @param {<i>Boolean</i>} loadScripts (optional) true to look for and process scripts
    * @param {Function} callback For async script loading you can be noticed when the update completes
    * @return {YAHOO.ext.Element} this
     */
    update : function(html, loadScripts, callback){
        if(typeof html == 'undefined'){
            html = '';
        }
        if(loadScripts !== true){
            this.dom.innerHTML = html;
            if(typeof callback == 'function'){
                callback();
            }
            return this;
        }
        var id = YAHOO.util.Dom.generateId();
        var dom = this.dom;
        
        html += '<span id="' + id + '"></span>';
        
        YAHOO.util.Event.onAvailable(id, function(){
            var hd = document.getElementsByTagName("head")[0];
            var re = /(?:<script.*?>)((\n|\r|.)*?)(?:<\/script>)/img; 
            var srcRe = /\ssrc=([\'\"])(.*?)\1/i;
            var match;
            while(match = re.exec(html)){
                var srcMatch = match[0].match(srcRe);
                if(srcMatch && srcMatch[2]){
                   var s = document.createElement("script");
                   s.src = srcMatch[2];
                   hd.appendChild(s);
                }else if(match[1] && match[1].length > 0){
                   eval(match[1]);
                }                     
            }
            var el = document.getElementById(id);
            if(el){el.parentNode.removeChild(el);}
            if(typeof callback == 'function'){
                callback();
            }
        });
        dom.innerHTML = html.replace(/(?:<script.*?>)((\n|\r|.)*?)(?:<\/script>)/img, '');
        return this;
    },
    
    /**
     * Direct access to the UpdateManager update() method (takes the same parameters).
     * @param {String/Function} url The url for this request or a function to call to get the url
     * @param {<i>String/Object</i>} params (optional) The parameters to pass as either a url encoded string "param1=1&amp;param2=2" or an object {param1: 1, param2: 2}
     * @param {<i>Function</i>} callback (optional) Callback when transaction is complete - called with signature (oElement, bSuccess)
     * @param {<i>Boolean</i>} discardUrl (optional) By default when you execute an update the defaultUrl is changed to the last used url. If true, it will not store the url.
     * @return {YAHOO.ext.Element} this
     */
    load : function(){
        var um = this.getUpdateManager();
        um.update.apply(um, arguments);
        return this;
    },
    
    /**
    * Gets this elements UpdateManager
    * @return {YAHOO.ext.UpdateManager} The UpdateManager
    */
    getUpdateManager : function(){
        if(!this.updateManager){
            this.updateManager = new YAHOO.ext.UpdateManager(this);
        }
        return this.updateManager;
    },
    
    /**
     * Disables text selection for this element (normalized across browsers)
     * @return {YAHOO.ext.Element} this
     */
    unselectable : function(){
        this.dom.unselectable = 'on';
        this.swallowEvent('selectstart', true);
        this.applyStyles('-moz-user-select:none;-khtml-user-select:none;');
        return this;
    },
    
    /**
    * Calculates the x, y to center this element on the screen
    * @param {Boolean} offsetScroll True to offset the documents current scroll position
    * @return {Array} The x, y values [x, y]
    */
    getCenterXY : function(offsetScroll){
        var centerX = Math.round((YAHOO.util.Dom.getViewportWidth()-this.getWidth())/2);
        var centerY = Math.round((YAHOO.util.Dom.getViewportHeight()-this.getHeight())/2);
        if(!offsetScroll){
            return [centerX, centerY];
        }else{
            var scrollX = document.documentElement.scrollLeft || document.body.scrollLeft || 0;
            var scrollY = document.documentElement.scrollTop || document.body.scrollTop || 0;
            return[centerX + scrollX, centerY + scrollY];
        }
    },
    
    /**
    * Centers the Element in either the viewport, or another Element.
    * @param {String/HTMLElement/YAHOO.ext.Element} centerIn (optional) The element in which to center the element.
    */
    center : function(centerIn) {
        if(!centerIn){
            this.setXY(this.getCenterXY(true));
        }else{
            var box = YAHOO.ext.Element.get(centerIn).getBox();
            this.setXY([box.x + (box.width / 2) - (this.getWidth() / 2),
                   box.y + (box.height / 2) - (this.getHeight() / 2)]);
        }
        return this;
    },

    /**
    * Gets an array of child YAHOO.ext.Element objects by tag name
    * @param {String} tagName
    * @return {Array} The children
    */
    getChildrenByTagName : function(tagName){
        var children = this.dom.getElementsByTagName(tagName);
        var len = children.length;
        var ce = new Array(len);
        for(var i = 0; i < len; ++i){
            ce[i] = YAHOO.ext.Element.get(children[i], true);
        }
        return ce;
    },
    
    /**
    * Gets an array of child YAHOO.ext.Element objects by class name and optional tagName
    * @param {String} className
    * @param {<i>String</i>} tagName (optional)
    * @return {Array} The children
    */
    getChildrenByClassName : function(className, tagName){
        var children = YAHOO.util.Dom.getElementsByClassName(className, tagName, this.dom);
        var len = children.length;
        var ce = new Array(len);
        for(var i = 0; i < len; ++i){
            ce[i] = YAHOO.ext.Element.get(children[i], true);
        }
        return ce;
    },
    
    /**
     * Tests various css rules/browsers to determine if this element uses a border box
     * @return {Boolean}
     */
    isBorderBox : function(){
        if(typeof this.bbox == 'undefined'){
            var el = this.dom;
            var b = YAHOO.ext.util.Browser;
            var strict = YAHOO.ext.Strict;
            this.bbox = ((b.isIE && !strict && el.style.boxSizing != 'content-box') || 
               (b.isGecko && YAHOO.util.Dom.getStyle(el, "-moz-box-sizing") == 'border-box') || 
               (!b.isSafari && YAHOO.util.Dom.getStyle(el, "box-sizing") == 'border-box'));
        }
        return this.bbox; 
    },
    
    /**
     * Return a box {x, y, width, height} that can be used to set another elements
     * size/location to match this element. 
     * @param {Boolean} contentBox (optional) If true a box for the content of the element is returned. 
     * @param {Boolean} local (optional) If true the element's left and top are returned instead of page x/y.
     * @return {Object}
     */
    getBox : function(contentBox, local){
        var xy;
        if(!local){
            xy = this.getXY();
        }else{
            var left = parseInt(YAHOO.util.Dom.getStyle('left'), 10) || 0;
            var top = parseInt(YAHOO.util.Dom.getStyle('top'), 10) || 0;
            xy = [left, top];
        }
        var el = this.dom;
        var w = el.offsetWidth;
        var h = el.offsetHeight;
        if(!contentBox){
            return {x: xy[0], y: xy[1], width: w, height: h};
        }else{
            var l = this.getBorderWidth('l')+this.getPadding('l');
            var r = this.getBorderWidth('r')+this.getPadding('r');
            var t = this.getBorderWidth('t')+this.getPadding('t');
            var b = this.getBorderWidth('b')+this.getPadding('b');
            return {x: xy[0]+l, y: xy[1]+t, width: w-(l+r), height: h-(t+b)};
        }
    },
    
    /**
     * Sets the element's box. Use getBox() on another element to get a box obj. If animate is true then width, height, x and y will be animated concurrently.
     * @param {Object} box The box to fill {x, y, width, height}
     * @param {<i>Boolean</i>} adjust (optional) Whether to adjust for box-model issues automatically
     * @param {<i>Boolean</i>} animate (optional) Animate the transition (Default is false)
     * @param {<i>float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeBoth)
     * @return {YAHOO.ext.Element} this
     */
    setBox : function(box, adjust, animate, duration, onComplete, easing){
        var w = box.width, h = box.height;
        if((adjust && !this.autoBoxAdjust) && !this.isBorderBox()){
           w -= (this.getBorderWidth('lr') + this.getPadding('lr'));
           h -= (this.getBorderWidth('tb') + this.getPadding('tb'));
        }
        this.setBounds(box.x, box.y, w, h, animate, duration, onComplete, easing);
        return this;
    },
    
    /**
     * Forces the browser to repaint this element
     * @return {YAHOO.ext.Element} this
     */
     repaint : function(){
        var dom = this.dom;
        YAHOO.util.Dom.addClass(dom, 'yui-ext-repaint');
        setTimeout(function(){
            YAHOO.util.Dom.removeClass(dom, 'yui-ext-repaint');
        }, 1);
        return this;
    },
    
    /**
     * Returns an object with properties top, left, right and bottom representing the margins of this element unless sides is passed, 
     * then it returns the calculated width of the sides (see getPadding)
     * @param {String} sides (optional) Any combination of l, r, t, b to get the sum of those sides
     * @return {Object/Number}
     */
    getMargins : function(side){
        if(!side){
            return {
                top: parseInt(this.getStyle('margin-top'), 10) || 0,
                left: parseInt(this.getStyle('margin-left'), 10) || 0,
                bottom: parseInt(this.getStyle('margin-bottom'), 10) || 0,
                right: parseInt(this.getStyle('margin-right'), 10) || 0
            };
        }else{
            return this.addStyles(side, YAHOO.ext.Element.margins);
         }
    },
    
    addStyles : function(sides, styles){
        var val = 0;
        for(var i = 0, len = sides.length; i < len; i++){
             var w = parseInt(this.getStyle(styles[sides.charAt(i)]), 10);
             if(!isNaN(w)) val += w;
        }
        return val;
    },
    
    /**
     * Creates a proxy element of this element
     * @param {String/Object} config The class name of the proxy element or a DomHelper config object
     * @param {<i>String/HTMLElement</i>} renderTo (optional) The element or element id to render the proxy to (defaults to document.body)
     * @param {<i>Boolean</i>} matchBox (optional) True to align and size the proxy to this element now (defaults to false)
     * @return {YAHOO.ext.Element} The new proxy element
     */
    createProxy : function(config, renderTo, matchBox){
        if(renderTo){
            renderTo = YAHOO.util.Dom.get(renderTo);
        }else{
            renderTo = document.body;
        }
        config = typeof config == 'object' ? 
            config : {tag : 'div', cls: config};
        var proxy = YAHOO.ext.DomHelper.append(renderTo, config, true);
        if(matchBox){
           proxy.setBox(this.getBox());
        }
        return proxy;
    },
    
    /**
     * Puts a mask over this element to disable user interaction. Requires core.css.
     * This method can only be applied to elements which accept child nodes.
     * @return {Element} The message element
     */
    mask : function(){
        if(this.getStyle('position') == 'static'){
            this.setStyle('position', 'relative');
        }
        if(!this._mask){
            this._mask = YAHOO.ext.DomHelper.append(this.dom, {tag:'div', cls:'ext-el-mask'}, true);
        }
        this.addClass('ext-masked');
        this._mask.setDisplayed(true);
        return this._mask;
    },
    
    /**
     * Removes a previously applied mask. If removeEl is true the mask overlay is destroyed, otherwise
     * it is cached for reuse.
     */
    unmask : function(removeEl){
        if(this._mask){
            removeEl === true ?
               this._mask.remove() : this._mask.setDisplayed(false);
        }
        this.removeClass('ext-masked');
    },
    
    /**
     * Creates an iframe shim for this element to keep selects and other windowed objects from
     * showing through.
     * @return {YAHOO.ext.Element} The new shim element
     */
    createShim : function(){
        var config = {
            tag : 'iframe', 
            frameBorder:'no', 
            cls: 'yiframe-shim', 
            style: 'position:absolute;visibility:hidden;left:0;top:0;overflow:hidden;', 
            src: YAHOO.ext.SSL_SECURE_URL
        };
        var shim = YAHOO.ext.DomHelper.insertBefore(this.dom, config, true);
        shim.setOpacity(.01);
        shim.setBox(this.getBox());
        return shim;
    },
    
    /**
     * Removes this element from the DOM and deletes it from the cache
     */
    remove : function(){
        this.dom.parentNode.removeChild(this.dom);
        delete YAHOO.ext.Element.cache[this.dom.id];
    },
    
    /**
     * Sets up event handlers to add and remove a css class when the mouse is over this element
     * @param {String} className
     * @return {YAHOO.ext.Element} this
     */
    addClassOnOver : function(className){
        this.on('mouseover', function(){
            this.addClass(className);
        }, this, true);
        this.on('mouseout', function(){
            this.removeClass(className);
        }, this, true);
        return this;
    },
    
    /**
     * Stops the specified event from bubbling and optionally prevent's the default action
     * @param {String} eventName
     * @param {Boolean} preventDefault (optional) true to prevent the default action too
     * @return {YAHOO.ext.Element} this
     */
    swallowEvent : function(eventName, preventDefault){
        var fn = function(e){
            e.stopPropagation();
            if(preventDefault){
                e.preventDefault();
            }
        };
        this.mon(eventName, fn);
        return this;
    },
    
    /**
     * Sizes this element to it's parent element's dimensions performing 
     * neccessary box adjustments. 
     * @param {Boolean} monitorResize (optional) If true maintains the fit when the browser window is resized.
     * @param {String/HTMLElment/Element} targetParent (optional) The target parent, default to the parentNode.
     * @return {YAHOO.ext.Element} this
     */
    fitToParent : function(monitorResize, targetParent){
        var p = getEl(targetParent || this.dom.parentNode);
        p.beginMeasure(); // in case parent is display:none
        var box = p.getBox(true, true);
        p.endMeasure();
        this.setSize(box.width, box.height);
        if(monitorResize === true){
            YAHOO.ext.EventManager.onWindowResize(this.fitToParent, this, true);
        }
        return this;
    },
    
    /**
     * Gets the next sibling, skipping text nodes
     * @return {HTMLElement} The next sibling or null
	 */
    getNextSibling : function(){
        var n = this.dom.nextSibling;
        while(n && n.nodeType != 1){
            n = n.nextSibling;
        }
        return n;
    },
    
    /**
     * Gets the previous sibling, skipping text nodes
     * @return {HTMLElement} The previous sibling or null
	 */
    getPrevSibling : function(){
        var n = this.dom.previousSibling;
        while(n && n.nodeType != 1){
            n = n.previousSibling;
        }
        return n;
    },
    
    
    /**
     * Appends the passed element(s) to this element
     * @param {String/HTMLElement/Array/Element/CompositeElement} el
     * @return {YAHOO.ext.Element} this
     */
    appendChild: function(el){
        el = getEl(el);
        el.appendTo(this);
        return this;
    },
    
    /**
     * Creates the passed DomHelper config and appends it to this element or optionally inserts it before the passed child element.
     * @param {Object} config DomHelper element config object
     * @param {<i>HTMLElement</i>} insertBefore (optional) a child element of this element
     * @return {YAHOO.ext.Element} The new child element
     */
    createChild: function(config, insertBefore){
        var c;
        if(insertBefore){
            c = YAHOO.ext.DomHelper.insertBefore(insertBefore, config, true);
        }else{
            c = YAHOO.ext.DomHelper.append(this.dom, config, true);
        }
        return c;
    },
    
    /**
     * Appends this element to the passed element
     * @param {String/HTMLElement/Element} el The new parent element
     * @return {YAHOO.ext.Element} this
     */
    appendTo: function(el){
        var node = getEl(el).dom;
        node.appendChild(this.dom);
        return this;
    },
    
    /**
     * Inserts this element before the passed element in the DOM
     * @param {String/HTMLElement/Element} el The element to insert before
     * @return {YAHOO.ext.Element} this
     */
    insertBefore: function(el){
        var node = getEl(el).dom;
        node.parentNode.insertBefore(this.dom, node);
        return this;
    },
    
    /**
     * Inserts this element after the passed element in the DOM
     * @param {String/HTMLElement/Element} el The element to insert after
     * @return {YAHOO.ext.Element} this
     */
    insertAfter: function(el){
        var node = getEl(el).dom;
        node.parentNode.insertBefore(this.dom, node.nextSibling);
        return this;
    },
    
    /**
     * Creates and wraps this element with another element
     * @param {Object} config (optional) DomHelper element config object for the wrapper element or null for an empty div
     * @return {Element} The newly created wrapper element
     */
    wrap: function(config){
        if(!config){
            config = {tag: 'div'};
        }
        var newEl = YAHOO.ext.DomHelper.insertBefore(this.dom, config, true);
        newEl.dom.appendChild(this.dom);
        return newEl;
    },
    
    /**
     * Replaces the passed element with this element
     * @param {String/HTMLElement/Element} el The element to replace
     * @return {YAHOO.ext.Element} this
     */
    replace: function(el){
        el = getEl(el);
        this.insertBefore(el);
        el.remove();
        return this;
    },
    
    /**
     * Inserts an html fragment into this element
     * @param {String} where Where to insert the html in relation to the this element - beforeBegin, afterBegin, beforeEnd, afterEnd.
     * @param {String} html The HTML fragment
     * @return {HTMLElement} The inserted node (or nearest related if more than 1 inserted)
     */
    insertHtml : function(where, html){
        return YAHOO.ext.DomHelper.insertHtml(where, this.dom, html);
    },
    
    /**
     * Sets the passed attributes as attributes of this element (a style attribute can be a string, object or function)
     * @param {Object} o The object with the attributes
     * @return {YAHOO.ext.Element} this
     */
    set : function(o){
        var el = this.dom;
        var useSet = el.setAttribute ? true : false;
        for(var attr in o){
            if(attr == 'style' || typeof o[attr] == 'function') continue;
            if(attr=='cls'){
                el.className = o['cls'];
            }else{
                if(useSet) el.setAttribute(attr, o[attr]);
                else el[attr] = o[attr];
            }
        }
        YAHOO.ext.DomHelper.applyStyles(el, o.style);
        return this;
    },
    
    /**
     * Convenience method for constructing a KeyMap
     * @param {Number/Array/Object/String} key Either a string with the keys to listen for, the numeric key code, array of key codes or an object with the following options: 
     *                                  {key: (number or array), shift: (true/false), ctrl: (true/false), alt: (true/false)}
     * @param {Function} fn The function to call
     * @param {Object} scope (optional) The scope of the function
     * @return {YAHOO.ext.KeyMap} The KeyMap created
     */
    addKeyListener : function(key, fn, scope){
        var config;
        if(typeof key != 'object' || key instanceof Array){
            config = {
                key: key,
                fn: fn,
                scope: scope 
            };
        }else{
            config = {
                key : key.key,
                shift : key.shift,
                ctrl : key.ctrl,
                alt : key.alt,
                fn: fn,
                scope: scope
            };
        }
        var map = new YAHOO.ext.KeyMap(this, config);
        return map; 
    },
    
    /**
     * Creates a KeyMap for this element
     * @param {Object} config The KeyMap config. See {@link YAHOO.ext.KeyMap} for more details
     * @return {YAHOO.ext.KeyMap} The KeyMap created
     */
    addKeyMap : function(config){
        return new YAHOO.ext.KeyMap(this, config);
    },
    
    /**
     * Returns true if this element is scrollable.
     * @return {Boolean}
     */
     isScrollable : function(){
        var dom = this.dom;
        return dom.scrollHeight > dom.clientHeight || dom.scrollWidth > dom.clientWidth;
    },
    
    /**
     * Scrolls this element the specified scroll point. It does NOT do bounds checking so if you scroll to a weird value it will try to do it. For auto bounds checking, use scroll().
     * @param {String} side Either 'left' for scrollLeft values or 'top' for scrollTop values.
     * @param {Number} value The new scroll value
     * @param {<i>Boolean</i>} animate (optional) Animate the scroll (Default is false)
     * @param {<i>Float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. 
     * @return {Element} this
     */
     
    scrollTo : function(side, value, animate, duration, onComplete, easing){
        var prop = side.toLowerCase() == 'left' ? 'scrollLeft' : 'scrollTop';
        if(!animate || !YAHOO.util.Anim){
            this.dom[prop] = value;
        }else{
            var to = prop == 'scrollLeft' ? [value, this.dom.scrollTop] : [this.dom.scrollLeft, value];
            this.anim({scroll: {'to': to}}, duration, onComplete, easing || YAHOO.util.Easing.easeOut, YAHOO.util.Scroll);
        }
        return this;
    },
    
    /**
     * Scrolls this element the specified direction. Does bounds checking to make sure the scroll is 
     * within this elements scrollable range.
     * @param {String} direction Possible values are: 'l','left' - 'r','right' - 't','top','up' - 'b','bottom','down'.
     * @param {Number} distance How far to scroll the element in pixels
     * @param {<i>Boolean</i>} animate (optional) Animate the scroll (Default is false)
     * @param {<i>Float</i>} duration (optional) How long the animation lasts. (Defaults to .35 seconds)
     * @param {<i>Function</i>} onComplete (optional) Function to call when animation completes.
     * @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. 
     * @return {Boolean} Returns true if a scroll was triggered or false if the element 
     * was scrolled as far as it could go.
     */
     scroll : function(direction, distance, animate, duration, onComplete, easing){
         if(!this.isScrollable()){
             return;
         }
         var el = this.dom;
         var l = el.scrollLeft, t = el.scrollTop;
         var w = el.scrollWidth, h = el.scrollHeight;
         var cw = el.clientWidth, ch = el.clientHeight;
         direction = direction.toLowerCase();
         var scrolled = false;
         switch(direction){
             case 'l':
             case 'left':
                 if(w - l > cw){
                     var v = Math.min(l + distance, w-cw);
                     this.scrollTo('left', v, animate, duration, onComplete, easing);
                     scrolled = true;
                 }
                 break;
            case 'r':
            case 'right':
                 if(l > 0){
                     var v = Math.max(l - distance, 0);
                     this.scrollTo('left', v, animate, duration, onComplete, easing);
                     scrolled = true;
                 }
                 break;
            case 't':
            case 'top':
            case 'up':
                 if(t > 0){
                     var v = Math.max(t - distance, 0);
                     this.scrollTo('top', v, animate, duration, onComplete, easing);
                     scrolled = true;
                 }
                 break;
            case 'b':
            case 'bottom':
            case 'down':
                 if(h - t > ch){
                     var v = Math.min(t + distance, h-ch);
                     this.scrollTo('top', v, animate, duration, onComplete, easing);
                     scrolled = true;
                 }
                 break;
         }
         return scrolled;
    },
    
    /**
     * Return the CSS color for the specified CSS attribute. rgb, 3 digit (like #fff) and valid values
     * are convert to standard 6 digit hex color. 
     * @param {String} attr The css attribute
     * @param {String} defaultValue The default value to use when a valid color isn't found
     * @param {String} prefix (optional) defaults to #. Use an empty string when working with 
     * YUI color anims.
     */
    getColor : function(attr, defaultValue, prefix){
        var v = this.getStyle(attr);
        if(!v || v == 'transparent' || v == 'inherit') {
            return defaultValue;
        }
        var color = typeof prefix == 'undefined' ? '#' : prefix;  
        if(v.substr(0, 4) == 'rgb('){
            var rvs = v.slice(4, v.length -1).split(',');
            for(var i = 0; i < 3; i++){
                var h = parseInt(rvs[i]).toString(16);
                if(h < 16){
                    h = '0' + h;   
                }
                color += h;
            }
        } else {  
            if(v.substr(0, 1) == '#'){
                if(v.length == 4) {
                    for(var i = 1; i < 4; i++){
                        var c = v.charAt(i);
                        color +=  c + c;
                    }
                }else if(v.length == 7){
                    color += v.slice(1, 6);
                }
            }
        }
        return(color.length > 5 ? color.toLowerCase() : defaultValue);  
    },
    
    /**
     * Highlights the Element by setting a color (defaults to background-color) and then 
     * fading back to the original color. If no original color is available, you should 
     * provide an "endColor" option which will be cleared after the animation. The available options 
     * for the "options" parameter are listed below (with their default values): <br/>
<pre><code>
el.highlight('ff0000', {<br/>
    attr: 'background-color',<br/>
    endColor: (current color) or 'ffffff'<br/>
    callback: yourFunction,<br/>
    scope: yourObject,<br/>
    easing: YAHOO.util.Easing.easeNone, <br/>
    duration: .75<br/>
});   
</code></pre>
     * @param {String} color (optional) The highlight color. Should be a 6 char hex color (no #). (defaults to ffff9c)
     * @param {Object} options (optional) Object literal with any of the options listed above
     */
    highlight : function(color, options){
        color = color || 'ffff9c';
        options = options || {};
        attr = options.attr || 'background-color';
        var origColor = this.getColor(attr);
        endColor = (options.endColor || origColor) || 'ffffff';
        var dom = this.dom;
        var cb = function(){
            YAHOO.util.Dom.setStyle(dom, attr, origColor || '');
            if(options.callback){
                options.callback.call(options.scope || window);
            }
        };
        var o = {};
        o[attr] = {from: color, to: endColor};
        this.anim(o, options.duration || .75, cb, options.easing || YAHOO.util.Easing.easeNone, YAHOO.util.ColorAnim);
        return this;
    }
};

/**
 * true to automatically adjust width and height settings for box-model issues (default to true)
 */
YAHOO.ext.Element.prototype.autoBoxAdjust = true;
/**
 * true to automatically detect display mode and use display instead of visibility with show()/hide() (defaults to false).
 * To enable this globally:<pre><code>YAHOO.ext.Element.prototype.autoDisplayMode = true;</code></pre>
 */
YAHOO.ext.Element.prototype.autoDisplayMode = true;

YAHOO.ext.Element.unitPattern = /\d+(px|em|%|en|ex|pt|in|cm|mm|pc)$/i;
/**
 * Visibility mode constant - Use visibility to hide element
 * @static
 * @type Number
 */
YAHOO.ext.Element.VISIBILITY = 1;
/**
 * Visibility mode constant - Use display to hide element
 * @static
 * @type Number
 */
YAHOO.ext.Element.DISPLAY = 2;

YAHOO.ext.Element.blockElements = /^(?:address|blockquote|center|dir|div|dl|fieldset|form|h\d|hr|isindex|menu|ol|ul|p|pre|table|dd|dt|li|tbody|tr|td|thead|tfoot|iframe)$/i;
YAHOO.ext.Element.borders = {l: 'border-left-width', r: 'border-right-width', t: 'border-top-width', b: 'border-bottom-width'};
YAHOO.ext.Element.paddings = {l: 'padding-left', r: 'padding-right', t: 'padding-top', b: 'padding-bottom'};
YAHOO.ext.Element.margins = {l: 'margin-left', r: 'margin-right', t: 'margin-top', b: 'margin-bottom'};
        
/**
 * @private Call out to here so we make minimal closure
 */
YAHOO.ext.Element.createStopHandler = function(stopPropagation, handler, scope, override){
    return function(e){
        if(e){
            if(stopPropagation){
                YAHOO.util.Event.stopEvent(e);
            }else {
                YAHOO.util.Event.preventDefault(e);
            }
        }
        handler.call(override && scope ? scope : window, e, scope);
    };
};

/**
 * @private
 */
YAHOO.ext.Element.cache = {};

/**
 * Static method to retreive Element objects. Uses simple caching to consistently return the same object. 
 * Automatically fixes if an object was recreated with the same id via AJAX or DOM.
 * @param {String/HTMLElement/Element} el The id of the element or the element to wrap (must have an id). If you pass in an element, it is returned
 * @return {Element} The element object
 * @static
 */
YAHOO.ext.Element.get = function(){
    var doc = document; // prevent IE dom lookup on every call to getEl
    var docEl;
    var E = YAHOO.ext.Element;
    var D = YAHOO.util.Dom;
    
    return function(el){
        if(!el){ return null; }
        if(el instanceof E){
            if(el != docEl){
                el.dom = doc.getElementById(el.id); // refresh dom element in case no longer valid
                E.cache[el.id] = el; // in case it was created directly with Element(), let's cache it
            }
            return el;
        }else if(el.isComposite){
            return el;
        }else if(el instanceof Array){
            return E.select(el);
        }else if(el == doc){
            // create a bogus element object representing the document object
            if(!docEl){
                var f = function(){};
                f.prototype = E.prototype;
                docEl = new f();
                docEl.dom = doc;
            }
            return docEl;
        }
        var key = el;
        if(typeof el != 'string'){ // must be an element
            D.generateId(el, 'elgen-');
            key = el.id;
        }
        var element = E.cache[key];
        if(!element){
            element = new E(key);
            if(!element.dom) return null;
            E.cache[key] = element;
        }else{
            element.dom = doc.getElementById(key);
        }
        return element;
    };
}();

/*
 * Gets the globally shared flyweight Element. Use sparingly for 
 * bulk operations where a unique instance isn't needed.
 * Do not store a reference to this element - the dom node
 * can be overwritten by other code.
 */
YAHOO.ext.Element.fly = function(el){
    var E = YAHOO.ext.Element;
    if(typeof el == 'string'){
        el = document.getElementById(el);
    }
    if(!E._flyweight){
        var f = function(){};
        f.prototype = E.prototype;
        E._flyweight = new f();
    }
    E._flyweight.dom = el;
    return E._flyweight;
}

/*
 * Shorthand function for YAHOO.ext.Element.get()
 */
getEl = YAHOO.ext.Element.get;

YAHOO.util.Event.addListener(window, 'unload', function(){ 
    YAHOO.ext.Element.cache = null;
});

