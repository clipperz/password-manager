
/**
 * @class YAHOO.ext.Actor
 * Provides support for syncing and chaining of Element Yahoo! UI based animation and some common effects. Actors support "self-play" without an Animator.<br><br>
 * <b>Note: Along with the animation methods defined below, this class inherits and captures all of the "set" or animation methods of {@link YAHOO.ext.Element}. "get" methods are not captured and execute immediately.</b>
 * <br><br>Usage:<br>
 * <pre><code>
 * var actor = new YAHOO.ext.Actor('myElementId');
 * actor.startCapture(true);
 * actor.moveTo(100, 100, true);
 * actor.squish();
 * actor.play();
 * <br>
 * // or to start capturing immediately, with no Animator (the null second param)
 * <br>
 * var actor = new YAHOO.ext.Actor('myElementId', null, true);
 * actor.moveTo(100, 100, true);
 * actor.squish();
 * actor.play();
 * </code></pre>
 * @extends YAHOO.ext.Element
 * @requires YAHOO.ext.Element
 * @requires YAHOO.util.Dom
 * @requires YAHOO.util.Event
 * @requires YAHOO.util.CustomEvent 
 * @requires YAHOO.util.Anim
 * @requires YAHOO.util.ColorAnim
 * @requires YAHOO.util.Motion
 * @className YAHOO.ext.Actor
 * @constructor
 * Create new Actor.
 * @param {String/HTMLElement} el The dom element or element id 
 * @param {<i>YAHOO.ext.Animator</i>} animator (optional) The Animator that will capture this Actor's actions
 * @param {<i>Boolean</i>} selfCapture (optional) Whether this actor should capture it's own actions to support self playback without an animator (defaults to false)
 */
YAHOO.ext.Actor = function(element, animator, selfCapture){
    this.el = YAHOO.ext.Element.get(element, true); // cache el object for playback
    YAHOO.ext.Actor.superclass.constructor.call(this, element, true);
    this.onCapture = new YAHOO.util.CustomEvent('Actor.onCapture');
    if(animator){
        /**
        * The animator used to sync this actor with other actors
        * @member YAHOO.ext.Actor
        */
        animator.addActor(this);
    }
    /**
    * Whether this actor is currently capturing
    * @member YAHOO.ext.Actor
    */
    this.capturing = selfCapture;
    this.playlist = selfCapture ? new YAHOO.ext.Animator.AnimSequence() : null;
};

YAHOO.extendX(YAHOO.ext.Actor, YAHOO.ext.Element);

/**
 * Captures an action for this actor. Generally called internally but can be called directly.
 * @param {YAHOO.ext.Actor.Action} action
 */
YAHOO.ext.Actor.prototype.capture = function(action){
    if(this.playlist != null){
        this.playlist.add(action);
    }
    this.onCapture.fireDirect(this, action);
    return action;
};

/** @ignore */
YAHOO.ext.Actor.overrideAnimation = function(method, animParam, onParam){
    return function(){
        if(!this.capturing){
            return method.apply(this, arguments);
        }
        var args = Array.prototype.slice.call(arguments, 0);
        if(args[animParam] === true){
            return this.capture(new YAHOO.ext.Actor.AsyncAction(this, method, args, onParam));
        }else{
            return this.capture(new YAHOO.ext.Actor.Action(this, method, args));
        }
    };
}

/** @ignore */
YAHOO.ext.Actor.overrideBasic = function(method){
    return function(){
        if(!this.capturing){
            return method.apply(this, arguments);
        }
        var args = Array.prototype.slice.call(arguments, 0);
        return this.capture(new YAHOO.ext.Actor.Action(this, method, args));
    };
}

// All of these methods below are marked "ignore" because JSDoc treats them as fields, not function. How brilliant. The Element methods are documented anyway though.
/** Capturing override - See {@link YAHOO.ext.Element#setVisibilityMode} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setVisibilityMode = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.setVisibilityMode);
/** Capturing override - See {@link YAHOO.ext.Element#enableDisplayMode} for method details.
 * @method */
YAHOO.ext.Actor.prototype.enableDisplayMode = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.enableDisplayMode);
/** Capturing override - See {@link YAHOO.ext.Element#focus} for method details.
 * @method */
YAHOO.ext.Actor.prototype.focus = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.focus);
/** Capturing override - See {@link YAHOO.ext.Element#addClass} for method details.
 * @method */
YAHOO.ext.Actor.prototype.addClass = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.addClass);
/** Capturing override - See {@link YAHOO.ext.Element#removeClass} for method details.
 * @method */
YAHOO.ext.Actor.prototype.removeClass = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.removeClass);
/** Capturing override - See {@link YAHOO.ext.Element#replaceClass} for method details.
 * @method */
YAHOO.ext.Actor.prototype.replaceClass = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.replaceClass);
/** Capturing override - See {@link YAHOO.ext.Element#setStyle} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setStyle = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.setStyle);
/** Capturing override - See {@link YAHOO.ext.Element#setLeft} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setLeft = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.setLeft);
/** Capturing override - See {@link YAHOO.ext.Element#setTop} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setTop = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.setTop);
/** Capturing override - See {@link YAHOO.ext.Element#setAbsolutePositioned} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setAbsolutePositioned = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.setAbsolutePositioned);
/** Capturing override - See {@link YAHOO.ext.Element#setRelativePositioned} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setRelativePositioned = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.setRelativePositioned);
/** Capturing override - See {@link YAHOO.ext.Element#clearPositioning} for method details.
 * @method */
YAHOO.ext.Actor.prototype.clearPositioning = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.clearPositioning);
/** Capturing override - See {@link YAHOO.ext.Element#setPositioning} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setPositioning = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.setPositioning);
/** Capturing override - See {@link YAHOO.ext.Element#clip} for method details.
 * @method */
YAHOO.ext.Actor.prototype.clip = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.clip);
/** Capturing override - See {@link YAHOO.ext.Element#unclip} for method details.
 * @method */
YAHOO.ext.Actor.prototype.unclip = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.unclip);
/** Capturing override - See {@link YAHOO.ext.Element#clearOpacity} for method details.
 * @method */
YAHOO.ext.Actor.prototype.clearOpacity = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.clearOpacity);
/** Capturing override - See {@link YAHOO.ext.Element#update} for method details.
 * @method */
YAHOO.ext.Actor.prototype.update = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.update);
/** Capturing override - See {@link YAHOO.ext.Element#remove} for method details.
 * @method */
YAHOO.ext.Actor.prototype.remove = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.remove);
YAHOO.ext.Actor.prototype.fitToParent = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.fitToParent);
YAHOO.ext.Actor.prototype.appendChild = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.appendChild);
YAHOO.ext.Actor.prototype.createChild = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.createChild);
YAHOO.ext.Actor.prototype.appendTo = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.appendTo);
YAHOO.ext.Actor.prototype.insertBefore = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.insertBefore);
YAHOO.ext.Actor.prototype.insertAfter = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.insertAfter);
YAHOO.ext.Actor.prototype.wrap = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.wrap);
YAHOO.ext.Actor.prototype.replace = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.replace);
YAHOO.ext.Actor.prototype.insertHtml = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.insertHtml);
YAHOO.ext.Actor.prototype.set = YAHOO.ext.Actor.overrideBasic(YAHOO.ext.Actor.superclass.set);

/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#load} for method details.
 * @method */
YAHOO.ext.Actor.prototype.load = function(){
   if(!this.capturing){
        return YAHOO.ext.Actor.superclass.load.apply(this, arguments);
   }
   var args = Array.prototype.slice.call(arguments, 0);
   return this.capture(new YAHOO.ext.Actor.AsyncAction(this, YAHOO.ext.Actor.superclass.load, 
        args, 2));
};

/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#animate} for method details.
 * @method */
YAHOO.ext.Actor.prototype.animate = function(args, duration, onComplete, easing, animType){
    if(!this.capturing){
        return YAHOO.ext.Actor.superclass.animate.apply(this, arguments);
    }
    return this.capture(new YAHOO.ext.Actor.AsyncAction(this, YAHOO.ext.Actor.superclass.animate, 
        [args, duration, onComplete, easing, animType], 2));
};

/** Capturing and animation syncing override - See {@link YAHOO.ext.Element#setVisible} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setVisible = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.setVisible, 1, 3);
/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#toggle} for method details.
 * @method */
YAHOO.ext.Actor.prototype.toggle = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.toggle, 0, 2);
/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#setXY} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setXY = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.setXY, 1, 3);
/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#setLocation} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setLocation = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.setLocation, 2, 4);
/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#setWidth} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setWidth = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.setWidth, 1, 3);
/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#setHeight} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setHeight = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.setHeight, 1, 3);
/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#setSize} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setSize = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.setSize, 2, 4);
/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#setBounds} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setBounds = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.setBounds, 4, 6);
/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#setOpacity} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setOpacity = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.setOpacity, 1, 3);
/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#moveTo} for method details.
 * @method */
YAHOO.ext.Actor.prototype.moveTo = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.moveTo, 2, 4);
/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#move} for method details.
 * @method */
YAHOO.ext.Actor.prototype.move = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.move, 2, 4);
/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#alignTo} for method details.
 * @method */
YAHOO.ext.Actor.prototype.alignTo = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.alignTo, 3, 5);
/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#hide} for method details.
 * @method */
YAHOO.ext.Actor.prototype.hide = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.hide, 0, 2);
/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#show} for method details.
 * @method */
YAHOO.ext.Actor.prototype.show = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.show, 0, 2);

/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#setBox} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setBox = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.setBox, 2, 4);

/**Capturing and animation syncing override - See {@link YAHOO.ext.Element#autoHeight} for method details.
 * @method */
YAHOO.ext.Actor.prototype.autoHeight = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.autoHeight, 0, 2);
/** Capturing override - See {@link YAHOO.ext.Element#setX} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setX = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.setX, 1, 3);
/** Capturing override - See {@link YAHOO.ext.Element#setY} for method details.
 * @method */
YAHOO.ext.Actor.prototype.setY = YAHOO.ext.Actor.overrideAnimation(YAHOO.ext.Actor.superclass.setY, 1, 3);

/**
 * Start self capturing calls on this Actor. All subsequent calls are captured and executed when play() is called.
 */
YAHOO.ext.Actor.prototype.startCapture = function(){
    this.capturing = true;
    this.playlist = new YAHOO.ext.Animator.AnimSequence();
 };
 
 /**
 * Stop self capturing calls on this Actor.
 */
 YAHOO.ext.Actor.prototype.stopCapture = function(){
     this.capturing = false;
 };

/**
 * Clears any calls that have been self captured.
 */
YAHOO.ext.Actor.prototype.clear = function(){
    this.playlist = new YAHOO.ext.Animator.AnimSequence();
};

/**
 * Starts playback of self captured calls.
 * @param {<i>Function</i>} oncomplete (optional) Callback to execute when playback has completed
 */
YAHOO.ext.Actor.prototype.play = function(oncomplete){
    this.capturing = false;
    if(this.playlist){
        this.playlist.play(oncomplete);
    }
 };

/**
 * Capture a function call.
 * @param {Function} fcn The function to call
 * @param {<i>Array</i>} args (optional) The arguments to call the function with
 * @param {<i>Object</i>} scope (optional) The scope of the function
 */
YAHOO.ext.Actor.prototype.addCall = function(fcn, args, scope){
    if(!this.capturing){
        fcn.apply(scope || this, args || []);
    }else{
        this.capture(new YAHOO.ext.Actor.Action(scope, fcn, args || []));
    }
};

/**
 * Capture an async function call.
 * @param {Function} fcn The function to call
 * @param {Number} callbackIndex The index of the callback parameter on the passed function. A CALLBACK IS REQUIRED.
 * @param {<i>Array</i>} args The arguments to call the function with
 * @param {<i>Object</i>} scope (optional) The scope of the function
 */
YAHOO.ext.Actor.prototype.addAsyncCall = function(fcn, callbackIndex, args, scope){
    if(!this.capturing){
        fcn.apply(scope || this, args || []);
    }else{
       this.capture(new YAHOO.ext.Actor.AsyncAction(scope, fcn, args || [], callbackIndex));
    }
 },
 
/**
 * Capture a pause (in seconds).
 * @param {Number} seconds The seconds to pause
 */
YAHOO.ext.Actor.prototype.pause = function(seconds){
    this.capture(new YAHOO.ext.Actor.PauseAction(seconds));
 };
 
/**
* Shake this element from side to side
*/
YAHOO.ext.Actor.prototype.shake = function(){
    this.move('left', 20, true, .05);
    this.move('right', 40, true, .05);
    this.move('left', 40, true, .05);
    this.move('right', 20, true, .05);
};

/**
* Bounce this element from up and down
*/
YAHOO.ext.Actor.prototype.bounce = function(){
    this.move('up', 20, true, .05);
    this.move('down', 40, true, .05);
    this.move('up', 40, true, .05);
    this.move('down', 20, true, .05);
};

/**
* Show the element using a "blinds" effect
* @param {String} anchor The part of the element that it should appear to exapand from. 
                        The short/long options currently are t/top, l/left
* @param {<i>Number</i>} newSize (optional) The size to animate to. (Default to current size)
* @param {<i>Float</i>} duration (optional) How long the effect lasts (in seconds)
* @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeOut)
*/
YAHOO.ext.Actor.prototype.blindShow = function(anchor, newSize, duration, easing){
    var size = this.getSize();
    this.clip();
    anchor = anchor.toLowerCase();
    switch(anchor){
        case 't':
        case 'top':
            this.setHeight(1);
            this.setVisible(true);
            this.setHeight(newSize || size.height, true, duration || .5, null, easing || YAHOO.util.Easing.easeOut);
        break;
        case 'l':
        case 'left':
            this.setWidth(1);
            this.setVisible(true);
            this.setWidth(newSize || size.width, true, duration || .5, null, easing || YAHOO.util.Easing.easeOut);
        break;
    }
    this.unclip();
    return size;
};

/**
* Hide the element using a "blinds" effect
* @param {String} anchor The part of the element that it should appear to collapse to.
                        The short/long options are t/top, l/left, b/bottom, r/right.
* @param {<i>Float</i>} duration (optional) How long the effect lasts (in seconds)
* @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeIn)
*/
YAHOO.ext.Actor.prototype.blindHide = function(anchor, duration, easing){
    var size = this.getSize();
    this.clip();
    anchor = anchor.toLowerCase();
    switch(anchor){
        case 't':
        case 'top':
            this.setSize(size.width, 1, true, duration || .5, null, easing || YAHOO.util.Easing.easeIn);
            this.setVisible(false);
        break;
        case 'l':
        case 'left':
            this.setSize(1, size.height, true, duration || .5, null, easing || YAHOO.util.Easing.easeIn);
            this.setVisible(false);
        break;
        case 'r':
        case 'right':
            this.animate({width: {to: 1}, points: {by: [size.width, 0]}}, 
            duration || .5, null, YAHOO.util.Easing.easeIn, YAHOO.util.Motion);
            this.setVisible(false);
        break;
        case 'b':
        case 'bottom':
            this.animate({height: {to: 1}, points: {by: [0, size.height]}}, 
            duration || .5, null, YAHOO.util.Easing.easeIn, YAHOO.util.Motion);
            this.setVisible(false);
        break;
    }
    return size;
};

/**
* Show the element using a "slide in" effect - In order for this effect to work the element MUST have a child element container that can be "slid" otherwise a blindShow effect is rendered. 
* @param {String} anchor The part of the element that it should appear to slide from. 
                        The short/long options currently are t/top, l/left
* @param {<i>Number</i>} newSize (optional) The size to animate to. (Default to current size)
* @param {<i>Float</i>} duration (optional) How long the effect lasts (in seconds)
* @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeOuth)
*/
YAHOO.ext.Actor.prototype.slideShow = function(anchor, newSize, duration, easing, clearPositioning){
    var size = this.getSize();
    this.clip();
    var firstChild = this.dom.firstChild;
    if(!firstChild || (firstChild.nodeName && "#TEXT" == firstChild.nodeName.toUpperCase())) { // can't do a slide with only a textnode
        this.blindShow(anchor, newSize, duration, easing);
        return;
    }
    var child = YAHOO.ext.Element.get(firstChild, true);
    var pos = child.getPositioning();
    this.addCall(child.setAbsolutePositioned, null, child);
    this.setVisible(true);
    anchor = anchor.toLowerCase();
    switch(anchor){
        case 't':
        case 'top':
            this.addCall(child.setStyle, ['right', ''], child);
            this.addCall(child.setStyle, ['top', ''], child);
            this.addCall(child.setStyle, ['left', '0px'], child);
            this.addCall(child.setStyle, ['bottom', '0px'], child);
            this.setHeight(1);
            this.setHeight(newSize || size.height, true, duration || .5, null, easing || YAHOO.util.Easing.easeOut);
        break;
        case 'l':
        case 'left':
            this.addCall(child.setStyle, ['left', ''], child);
            this.addCall(child.setStyle, ['bottom', ''], child);
            this.addCall(child.setStyle, ['right', '0px'], child);
            this.addCall(child.setStyle, ['top', '0px'], child);
            this.setWidth(1);
            this.setWidth(newSize || size.width, true, duration || .5, null, easing || YAHOO.util.Easing.easeOut);
        break;
        case 'r':
        case 'right':
            this.addCall(child.setStyle, ['left', '0px'], child);
            this.addCall(child.setStyle, ['top', '0px'], child);
            this.addCall(child.setStyle, ['right', ''], child);
            this.addCall(child.setStyle, ['bottom', ''], child);
            this.setWidth(1);
            this.setWidth(newSize || size.width, true, duration || .5, null, easing || YAHOO.util.Easing.easeOut);
        break;
        case 'b':
        case 'bottom':
            this.addCall(child.setStyle, ['right', ''], child);
            this.addCall(child.setStyle, ['top', '0px'], child);
            this.addCall(child.setStyle, ['left', '0px'], child);
            this.addCall(child.setStyle, ['bottom', ''], child);
            this.setHeight(1);
            this.setHeight(newSize || size.height, true, duration || .5, null, easing || YAHOO.util.Easing.easeOut);
        break;
    }
    if(clearPositioning !== false){
      this.addCall(child.setPositioning, [pos], child);
    }
    this.unclip();
    return size;
};

/**
* Hide the element using a "slide in" effect - In order for this effect to work the element MUST have a child element container that can be "slid" otherwise a blindHide effect is rendered. 
* @param {String} anchor The part of the element that it should appear to slide to.
                        The short/long options are t/top, l/left, b/bottom, r/right.
* @param {<i>Float</i>} duration (optional) How long the effect lasts (in seconds)
* @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeIn)
*/
YAHOO.ext.Actor.prototype.slideHide = function(anchor, duration, easing){
    var size = this.getSize();
    this.clip();
    var firstChild = this.dom.firstChild;
    if(!firstChild || (firstChild.nodeName && "#TEXT" == firstChild.nodeName.toUpperCase())) { // can't do a slide with only a textnode
        this.blindHide(anchor, duration, easing);
        return;
    }
    var child = YAHOO.ext.Element.get(firstChild, true);
    var pos = child.getPositioning();
    this.addCall(child.setAbsolutePositioned, null, child);
    anchor = anchor.toLowerCase();
    switch(anchor){
        case 't':
        case 'top':
            this.addCall(child.setStyle, ['right', ''], child);
            this.addCall(child.setStyle, ['top', ''], child);
            this.addCall(child.setStyle, ['left', '0px'], child);
            this.addCall(child.setStyle, ['bottom', '0px'], child);
            this.setSize(size.width, 1, true, duration || .5, null, easing || YAHOO.util.Easing.easeIn);
            this.setVisible(false);
        break;
        case 'l':
        case 'left':
            this.addCall(child.setStyle, ['left', ''], child);
            this.addCall(child.setStyle, ['bottom', ''], child);
            this.addCall(child.setStyle, ['right', '0px'], child);
            this.addCall(child.setStyle, ['top', '0px'], child);
            this.setSize(1, size.height, true, duration || .5, null, easing || YAHOO.util.Easing.easeIn);
            this.setVisible(false);
        break;
        case 'r':
        case 'right':
            this.addCall(child.setStyle, ['right', ''], child);
            this.addCall(child.setStyle, ['bottom', ''], child);
            this.addCall(child.setStyle, ['left', '0px'], child);
            this.addCall(child.setStyle, ['top', '0px'], child);
            this.setSize(1, size.height, true, duration || .5, null, easing || YAHOO.util.Easing.easeIn);
            this.setVisible(false);
        break;
        case 'b':
        case 'bottom':
            this.addCall(child.setStyle, ['right', ''], child);
            this.addCall(child.setStyle, ['top', '0px'], child);
            this.addCall(child.setStyle, ['left', '0px'], child);
            this.addCall(child.setStyle, ['bottom', ''], child);
            this.setSize(size.width, 1, true, duration || .5, null, easing || YAHOO.util.Easing.easeIn);
            this.setVisible(false);
        break;
    }
    this.addCall(child.setPositioning, [pos], child);
    return size;
};

/**
* Hide the element by "squishing" it into the corner
* @param {<i>Float</i>} duration (optional) How long the effect lasts (in seconds)
*/
YAHOO.ext.Actor.prototype.squish = function(duration){
    var size = this.getSize();
    this.clip();
    this.setSize(1, 1, true, duration || .5);
    this.setVisible(false);
    return size;
};

/**
* Fade an element in
* @param {<i>Float</i>} duration (optional) How long the effect lasts (in seconds)
*/
YAHOO.ext.Actor.prototype.appear = function(duration){
    this.setVisible(true, true, duration);
};

/**
* Fade an element out
* @param {<i>Float</i>} duration (optional) How long the effect lasts (in seconds)
*/
YAHOO.ext.Actor.prototype.fade = function(duration){
    this.setVisible(false, true, duration);
};

/**
* Blink the element as if it was clicked and then collapse on it's center
* @param {<i>Float</i>} duration (optional) How long the effect lasts (in seconds)
*/
YAHOO.ext.Actor.prototype.switchOff = function(duration){
    this.clip();
    this.setVisible(false, true, .1);
    this.clearOpacity();
    this.setVisible(true);
    this.animate({height: {to: 1}, points: {by: [0, this.getHeight()/2]}}, 
            duration || .5, null, YAHOO.util.Easing.easeOut, YAHOO.util.Motion);
    this.setVisible(false);
};

/**
* Highlight the element using a background color (or passed attribute) animation
* @param {String} color (optional) The color to use for the highlight
* @param {<i>String</i>} fromColor (optional) If the element does not currently have a background color, you will need to pass in a color to animate from
* @param {<i>Float</i>} duration (optional) How long the effect lasts (in seconds)
* @param {<i>String</i>} attribute (optional) Specify a CSS attribute to use other than background color - camelCase
*/
YAHOO.ext.Actor.prototype.highlight = function(color, fromColor, duration, attribute){
    attribute = attribute || 'background-color';
    var original = this.getStyle(attribute);
    fromColor = fromColor || ((original && original != '' && original != 'transparent') ? original : '#FFFFFF');
    var cfg = {};
    cfg[attribute] = {to: color, from: fromColor};
    this.setVisible(true);
    this.animate(cfg, duration || .5, null, YAHOO.util.Easing.bounceOut, YAHOO.util.ColorAnim);
    this.setStyle(attribute, original);
};

/**
* Fade the element in and out the specified amount of times
* @param {<i>Number</i>} count (optional) How many times to pulse (Defaults to 3)
* @param {<i>Float</i>} duration (optional) How long the effect lasts (in seconds)
*/
YAHOO.ext.Actor.prototype.pulsate = function(count, duration){
    count = count || 3;
    for(var i = 0; i < count; i++){
        this.toggle(true, duration || .25);
        this.toggle(true, duration || .25);
    }
};

/**
* Fade the element as it is falling from it's current position
* @param {<i>Float</i>} duration (optional) How long the effect lasts (in seconds)
*/
YAHOO.ext.Actor.prototype.dropOut = function(duration){
    this.animate({opacity: {to: 0}, points: {by: [0, this.getHeight()]}}, 
            duration || .5, null, YAHOO.util.Easing.easeIn, YAHOO.util.Motion);
    this.setVisible(false);
};

/**
* Hide the element in a way that it appears as if it is flying off the screen
* @param {String} anchor The part of the page that the element should appear to move to. 
                        The short/long options are t/top, l/left, b/bottom, r/right, tl/top-left, 
                        tr/top-right, bl/bottom-left or br/bottom-right.
* @param {<i>Float</i>} duration (optional) How long the effect lasts (in seconds)
* @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeIn)
*/
YAHOO.ext.Actor.prototype.moveOut = function(anchor, duration, easing){
    var Y = YAHOO.util;
    var vw = Y.Dom.getViewportWidth();
    var vh = Y.Dom.getViewportHeight();
    var cpoints = this.getCenterXY()
    var centerX = cpoints[0];
    var centerY = cpoints[1];
    var anchor = anchor.toLowerCase();
    var p;
    switch(anchor){
        case 't':
        case 'top':
            p = [centerX, -this.getHeight()];
        break;
        case 'l':
        case 'left':
            p = [-this.getWidth(), centerY];
        break;
        case 'r':
        case 'right':
            p = [vw+this.getWidth(), centerY];
        break;
        case 'b':
        case 'bottom':
            p = [centerX, vh+this.getHeight()];
        break;
        case 'tl':
        case 'top-left':
            p = [-this.getWidth(), -this.getHeight()];
        break;
        case 'bl':
        case 'bottom-left':
            p = [-this.getWidth(), vh+this.getHeight()];
        break;
        case 'br':
        case 'bottom-right':
            p = [vw+this.getWidth(), vh+this.getHeight()];
        break;
        case 'tr':
        case 'top-right':
            p = [vw+this.getWidth(), -this.getHeight()];
        break;
    }
    this.moveTo(p[0], p[1], true, duration || .35, null, easing || Y.Easing.easeIn);
    this.setVisible(false);
};

/**
* Show the element in a way that it appears as if it is flying onto the screen
* @param {String} anchor The part of the page that the element should appear to move from. 
                        The short/long options are t/top, l/left, b/bottom, r/right, tl/top-left, 
                        tr/top-right, bl/bottom-left or br/bottom-right.
* @param {<i>Array</i>} to (optional) Array of x and y position to move to like [x, y] (Defaults to center screen)
* @param {<i>Float</i>} duration (optional) How long the effect lasts (in seconds)
* @param {<i>Function</i>} easing (optional) YAHOO.util.Easing method to use. (Defaults to YAHOO.util.Easing.easeOut)
*/
YAHOO.ext.Actor.prototype.moveIn = function(anchor, to, duration, easing){
    to = to || this.getCenterXY();
    this.moveOut(anchor, .01);
    this.setVisible(true);
    this.setXY(to, true, duration || .35, null, easing || YAHOO.util.Easing.easeOut);
};
/**
* Show a ripple of exploding, attenuating borders to draw attention to an Element.
* @param {<i>Number<i>} color (optional) The color of the border.
* @param {<i>Number</i>} count (optional) How many ripples.
* @param {<i>Float</i>} duration (optional) How long each ripple takes to expire
*/
YAHOO.ext.Actor.prototype.frame = function(color, count, duration){
    color = color || "red";
    count = count || 3;
    duration = duration || .5;
    var frameFn = function(callback){
        var box = this.getBox();
        var animFn = function(){ 
            var proxy = this.createProxy({
                 tag:"div",
                 style:{
                    visbility:"hidden",
                    position:"absolute",
                    'z-index':"35000", // yee haw
                    border:"0px solid " + color
                 }
              });
            var scale = proxy.isBorderBox() ? 2 : 1;
            proxy.animate({
                top:{from:box.y, to:box.y - 20},
                left:{from:box.x, to:box.x - 20},
                borderWidth:{from:0, to:10},
                opacity:{from:1, to:0},
                height:{from:box.height, to:(box.height + (20*scale))},
                width:{from:box.width, to:(box.width + (20*scale))}
            }, duration, function(){
                proxy.remove();
            });
            if(--count > 0){
                 animFn.defer((duration/2)*1000, this);
            }else{
                if(typeof callback == 'function'){
                    callback();
                }
            }
       }
       animFn.call(this);
   }
   this.addAsyncCall(frameFn, 0, null, this);
};

YAHOO.ext.Actor.Action = function(actor, method, args){
      this.actor = actor;
      this.method = method;
      this.args = args;
  }
  
YAHOO.ext.Actor.Action.prototype = {
    play : function(onComplete){
        this.method.apply(this.actor || window, this.args);
        onComplete();
    }  
};


YAHOO.ext.Actor.AsyncAction = function(actor, method, args, onIndex){
    YAHOO.ext.Actor.AsyncAction.superclass.constructor.call(this, actor, method, args);
    this.onIndex = onIndex;
    this.originalCallback = this.args[onIndex];
}
YAHOO.extendX(YAHOO.ext.Actor.AsyncAction, YAHOO.ext.Actor.Action);

YAHOO.ext.Actor.AsyncAction.prototype.play = function(onComplete){
    var callbackArg = this.originalCallback ? 
                        this.originalCallback.createSequence(onComplete) : onComplete;
    this.args[this.onIndex] = callbackArg;
    this.method.apply(this.actor, this.args);
};


YAHOO.ext.Actor.PauseAction = function(seconds){
    this.seconds = seconds;
};
YAHOO.ext.Actor.PauseAction.prototype = {
    play : function(onComplete){
        setTimeout(onComplete, this.seconds * 1000);
    }
};
