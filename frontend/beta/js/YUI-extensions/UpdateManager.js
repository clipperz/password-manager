/**
 * @class YAHOO.ext.UpdateManager
 * @extends YAHOO.ext.util.Observable
 * Provides AJAX-style update for Element object using Yahoo 
 * UI library YAHOO.util.Connect functionality.<br><br>
 * Usage:<br>
 * <pre><code>
 * // Get it from a YAHOO.ext.Element object
 * var el = getEl('foo');
 * var mgr = el.getUpdateManager();
 * mgr.update('http://myserver.com/index.php', 'param1=1&amp;param2=2');
 * ...
 * mgr.formUpdate('myFormId', 'http://myserver.com/index.php');
 * <br>
 * // or directly (returns the same UpdateManager instance)
 * var mgr = new YAHOO.ext.UpdateManager('myElementId');
 * mgr.startAutoRefresh(60, 'http://myserver.com/index.php');
 * mgr.on('update', myFcnNeedsToKnow);
 * <br>
 * </code></pre>
 * @requires YAHOO.ext.Element
 * @requires YAHOO.util.Dom
 * @requires YAHOO.util.Event
 * @requires YAHOO.util.CustomEvent 
 * @requires YAHOO.util.Connect
 * @constructor
 * Create new UpdateManager directly.
 * @param {String/HTMLElement/YAHOO.ext.Element} el The element to update 
 * @param {<i>Boolean</i>} forceNew (optional) By default the constructor checks to see if the passed element already has an UpdateManager and if it does it returns the same instance. This will skip that check (useful for extending this class).
 */
YAHOO.ext.UpdateManager = function(el, forceNew){
    el = YAHOO.ext.Element.get(el);
    if(!forceNew && el.updateManager){
        return el.updateManager;
    }
    /**
     * The Element object
     * @type YAHOO.ext.Element
     */
    this.el = el;
    /**
     * Cached url to use for refreshes. Overwritten every time update() is called unless 'discardUrl' param is set to true.
     * @type String
     */
    this.defaultUrl = null;
    this.beforeUpdate = new YAHOO.util.CustomEvent('UpdateManager.beforeUpdate');
    this.onUpdate = new YAHOO.util.CustomEvent('UpdateManager.onUpdate');
    this.onFailure = new YAHOO.util.CustomEvent('UpdateManager.onFailure');
    
    this.events = {
        /**
         * @event beforeupdate
         * Fired before an update is made, return false from your handler and the update is cancelled. 
         * @param {YAHOO.ext.Element} el
         * @param {String/Object/Function} url
         * @param {String/Object} params
         */
        'beforeupdate': this.beforeUpdate,
        /**
         * @event update
         * Fired after successful update is made. 
         * @param {YAHOO.ext.Element} el
         * @param {Object} oResponseObject The YAHOO.util.Connect response Object
         */
        'update': this.onUpdate,
        /**
         * @event failure
         * Fired on update failure. Uses fireDirect with signature: (oElement, oResponseObject)
         * @param {YAHOO.ext.Element} el
         * @param {Object} oResponseObject The YAHOO.util.Connect response Object
         */
        'failure': this.onFailure 
    };
    
    /**
     * Blank page URL to use with SSL file uploads (Defaults to YAHOO.ext.UpdateManager.defaults.sslBlankUrl or 'about:blank'). 
     * @type String
     */
    this.sslBlankUrl = YAHOO.ext.UpdateManager.defaults.sslBlankUrl;
    /**
     * Whether to append unique parameter on get request to disable caching (Defaults to YAHOO.ext.UpdateManager.defaults.disableCaching or false). 
     * @type Boolean
     */
    this.disableCaching = YAHOO.ext.UpdateManager.defaults.disableCaching;
    /**
     * Text for loading indicator (Defaults to YAHOO.ext.UpdateManager.defaults.indicatorText or '&lt;div class="loading-indicator"&gt;Loading...&lt;/div&gt;'). 
     * @type String
     */
    this.indicatorText = YAHOO.ext.UpdateManager.defaults.indicatorText;
    /**
     * Whether to show indicatorText when loading (Defaults to YAHOO.ext.UpdateManager.defaults.showLoadIndicator or true). 
     * @type String
     */
    this.showLoadIndicator = YAHOO.ext.UpdateManager.defaults.showLoadIndicator;
    /**
     * Timeout for requests or form posts in seconds (Defaults to YAHOO.ext.UpdateManager.defaults.timeout or 30 seconds). 
     * @type Number
     */
    this.timeout = YAHOO.ext.UpdateManager.defaults.timeout;
    
    /**
     * True to process scripts in the output (Defaults to YAHOO.ext.UpdateManager.defaults.loadScripts (false)). 
     * @type Boolean
     */
    this.loadScripts = YAHOO.ext.UpdateManager.defaults.loadScripts;
    
    /**
     * YAHOO.util.Connect transaction object of current executing transaction
     */
    this.transaction = null;
    
    /**
     * @private
     */
    this.autoRefreshProcId = null;
    /**
     * Delegate for refresh() prebound to 'this', use myUpdater.refreshDelegate.createCallback(arg1, arg2) to bind arguments
     * @type Function
     */
    this.refreshDelegate = this.refresh.createDelegate(this);
    /**
     * Delegate for update() prebound to 'this', use myUpdater.updateDelegate.createCallback(arg1, arg2) to bind arguments
     * @type Function
     */
    this.updateDelegate = this.update.createDelegate(this);
    /**
     * Delegate for formUpdate() prebound to 'this', use myUpdater.formUpdateDelegate.createCallback(arg1, arg2) to bind arguments
     * @type Function
     */
    this.formUpdateDelegate = this.formUpdate.createDelegate(this);
    /**
     * @private
     */
    this.successDelegate = this.processSuccess.createDelegate(this);
    /**
     * @private
     */
     this.failureDelegate = this.processFailure.createDelegate(this);
     
     /**
      * The renderer for this UpdateManager. Defaults to {@link YAHOO.ext.UpdateManager.BasicRenderer}. 
      */
      this.renderer = new YAHOO.ext.UpdateManager.BasicRenderer();
};

YAHOO.ext.UpdateManager.prototype = {
    fireEvent : YAHOO.ext.util.Observable.prototype.fireEvent,
    on : YAHOO.ext.util.Observable.prototype.on,
    addListener : YAHOO.ext.util.Observable.prototype.addListener,
    delayedListener : YAHOO.ext.util.Observable.prototype.delayedListener,
    removeListener : YAHOO.ext.util.Observable.prototype.removeListener,
    purgeListeners : YAHOO.ext.util.Observable.prototype.purgeListeners,
    bufferedListener : YAHOO.ext.util.Observable.prototype.bufferedListener,
    /**
     * Get the Element this UpdateManager is bound to
     * @return {YAHOO.ext.Element} The element
     */
    getEl : function(){
        return this.el;
    },
    
    /**
     * Performs an async request, updating this element with the response. If params are specified it uses POST, otherwise it uses GET.
     * @param {Object/String/Function} url The url for this request or a function to call to get the url or a config object containing any of the following options:
<pre><code>
um.update({<br/>
    url: 'your-url.php',<br/>
    params: {param1: 'foo', param2: 'bar'}, // or a URL encoded string<br/>
    callback: yourFunction,<br/>
    scope: yourObject, //(optional scope)  <br/>
    discardUrl: false, <br/>
    nocache: false,<br/>
    text: 'Loading...',<br/>
    timeout: 30,<br/>
    scripts: false<br/>
});   
</code></pre>
     * The only required property is url. The optional properties nocache, text and scripts 
     * are shorthand for disableCaching, indicatorText and loadScripts and are used to set their associated property on this UpdateManager instance.
     * @param {<i>String/Object</i>} params (optional) The parameters to pass as either a url encoded string "param1=1&amp;param2=2" or an object {param1: 1, param2: 2}
     * @param {<i>Function</i>} callback (optional) Callback when transaction is complete - called with signature (oElement, bSuccess, oResponse)
     * @param {<i>Boolean</i>} discardUrl (optional) By default when you execute an update the defaultUrl is changed to the last used url. If true, it will not store the url.
     */
    update : function(url, params, callback, discardUrl){
        if(this.beforeUpdate.fireDirect(this.el, url, params) !== false){
            if(typeof url == 'object'){ // must be config object
                var cfg = url;
                url = cfg.url;
                params = params || cfg.params;
                callback = callback || cfg.callback;
                discardUrl = discardUrl || cfg.discardUrl;
                if(callback && cfg.scope){
                    callback = callback.createDelegate(cfg.scope);
                }
                if(typeof cfg.nocache != 'undefined'){this.disableCaching = cfg.nocache};
                if(typeof cfg.text != 'undefined'){this.indicatorText = '<div class="loading-indicator">'+cfg.text+'</div>'};
                if(typeof cfg.scripts != 'undefined'){this.loadScripts = cfg.scripts};
                if(typeof cfg.timeout != 'undefined'){this.timeout = cfg.timeout};
            }
            this.showLoading();
            if(!discardUrl){
                this.defaultUrl = url;
            }
            if(typeof url == 'function'){
                url = url();
            }
            if(typeof params == 'function'){
                params = params();
            }
            if(params && typeof params != 'string'){ // must be object
                var buf = [];
                for(var key in params){
                    if(typeof params[key] != 'function'){
                        buf.push(encodeURIComponent(key), '=', encodeURIComponent(params[key]), '&');
                    }
                }
                delete buf[buf.length-1];
                params = buf.join('');
            }
            var callback = {
                success: this.successDelegate,
                failure: this.failureDelegate,
                timeout: (this.timeout*1000),
                argument: {'url': url, 'form': null, 'callback': callback, 'params': params}
            };
            var method = params ? 'POST' : 'GET';
            if(method == 'GET'){
                url = this.prepareUrl(url);
            }
            this.transaction = YAHOO.util.Connect.asyncRequest(method, url, callback, params);
        }
    },
    
    /**
     * Performs an async form post, updating this element with the response. If the form has the attribute enctype="multipart/form-data", it assumes it's a file upload.
     * Uses this.sslBlankUrl for SSL file uploads to prevent IE security warning. See YUI docs for more info. 
     * @param {String/HTMLElement} form The form Id or form element
     * @param {<i>String</i>} url (optional) The url to pass the form to. If omitted the action attribute on the form will be used.
     * @param {<i>Boolean</i>} reset (optional) Whether to try to reset the form after the update
     * @param {<i>Function</i>} callback (optional) Callback when transaction is complete - called with signature (oElement, bSuccess, oResponse)
     */
    formUpdate : function(form, url, reset, callback){
        if(this.beforeUpdate.fireDirect(this.el, form, url) !== false){
            formEl = YAHOO.util.Dom.get(form);
            this.showLoading();
            if(typeof url == 'function'){
                url = url();
            }
            if(typeof params == 'function'){
                params = params();
            }
            url = url || formEl.action;
            var callback = {
                success: this.successDelegate,
                failure: this.failureDelegate,
                timeout: (this.timeout*1000),
                argument: {'url': url, 'form': form, 'callback': callback, 'reset': reset}
            };
            var isUpload = false;
            var enctype = formEl.getAttribute('enctype');
            if(enctype && enctype.toLowerCase() == 'multipart/form-data'){
                isUpload = true;
            }
            YAHOO.util.Connect.setForm(form, isUpload, this.sslBlankUrl);
            this.transaction = YAHOO.util.Connect.asyncRequest('POST', url, callback);
        }
    },
    
    /**
     * Refresh the element with the last used url or defaultUrl. If there is no url, it returns immediately
     * @param {Function} callback (optional) Callback when transaction is complete - called with signature (oElement, bSuccess)
     */
    refresh : function(callback){
        if(this.defaultUrl == null){
            return;
        }
        this.update(this.defaultUrl, null, callback, true);
    },
    
    /**
     * Set this element to auto refresh.
     * @param {Number} interval How often to update (in seconds).
     * @param {<i>String/Function</i>} url (optional) The url for this request or a function to call to get the url (Defaults to the last used url)
     * @param {<i>String/Object</i>} params (optional) The parameters to pass as either a url encoded string "&param1=1&param2=2" or as an object {param1: 1, param2: 2}
     * @param {<i>Function</i>} callback (optional) Callback when transaction is complete - called with signature (oElement, bSuccess)
     * @param {<i>Boolean</i>} refreshNow (optional) Whether to execute the refresh now, or wait the interval
     */
    startAutoRefresh : function(interval, url, params, callback, refreshNow){
        if(refreshNow){
            this.update(url || this.defaultUrl, params, callback, true);
        }
        if(this.autoRefreshProcId){
            clearInterval(this.autoRefreshProcId);
        }
        this.autoRefreshProcId = setInterval(this.update.createDelegate(this, [url || this.defaultUrl, params, callback, true]), interval*1000);
    },
    
    /**
     * Stop auto refresh on this element.
     */
     stopAutoRefresh : function(){
        if(this.autoRefreshProcId){
            clearInterval(this.autoRefreshProcId);
        }
    },
    
    /**
     * Called to update the element to "Loading" state. Override to perform custom action.
     */
    showLoading : function(){
        if(this.showLoadIndicator){
            this.el.update(this.indicatorText);
        }
    },
    
    /**
     * Adds unique parameter to query string if disableCaching = true
     * @private
     */
    prepareUrl : function(url){
        if(this.disableCaching){
            var append = '_dc=' + (new Date().getTime());
            if(url.indexOf('?') !== -1){
                url += '&' + append;
            }else{
                url += '?' + append;
            }
        }
        return url;
    },
    
    /**
     * @private
     */
    processSuccess : function(response){
        this.transaction = null;
        if(response.argument.form && response.argument.reset){
            try{ // put in try/catch since some older FF releases had problems with this
                response.argument.form.reset();
            }catch(e){}
        }
        if(this.loadScripts){
            this.renderer.render(this.el, response, this, 
                this.updateComplete.createDelegate(this, [response]));
        }else{
            this.renderer.render(this.el, response, this);
            this.updateComplete(response);
        }
    },
    
    updateComplete : function(response){
        this.fireEvent('update', this.el, response);
        if(typeof response.argument.callback == 'function'){
            response.argument.callback(this.el, true, response);
        }
    },
    
    /**
     * @private
     */
    processFailure : function(response){
        this.transaction = null;
        this.onFailure.fireDirect(this.el, response);
        if(typeof response.argument.callback == 'function'){
            response.argument.callback(this.el, false, response);
        }
    },
    
    /**
     * Set the content renderer for this UpdateManager. See {@link YAHOO.ext.UpdateManager.BasicRenderer#render} for more details.
     * @param {Object} renderer The object implementing the render() method
     */
    setRenderer : function(renderer){
        this.renderer = renderer;
    },
    
    getRenderer : function(){
       return this.renderer;  
    },
    
    /**
     * Set the defaultUrl used for updates
     * @param {String/Function} defaultUrl The url or a function to call to get the url
     */
    setDefaultUrl : function(defaultUrl){
        this.defaultUrl = defaultUrl;
    },
    
    /**
     * Aborts the executing transaction
     */
    abort : function(){
        if(this.transaction){
            YAHOO.util.Connect.abort(this.transaction);
        }
    },
    
    /**
     * Returns true if an update is in progress
     * @return {Boolean}
     */
    isUpdating : function(){
        if(this.transaction){
            return YAHOO.util.Connect.isCallInProgress(this.transaction);
        }
        return false;
    }
};

/**
 * @class YAHOO.ext.UpdateManager.defaults
 * The defaults collection enables customizing the default properties of UpdateManager
 */
   YAHOO.ext.UpdateManager.defaults = {
       /**
         * Timeout for requests or form posts in seconds (Defaults 30 seconds). 
         * @type Number
         */
         timeout : 30,
         
         /**
         * True to process scripts by default (Defaults to false). 
         * @type Boolean
         */
        loadScripts : false,
         
        /**
        * Blank page URL to use with SSL file uploads (Defaults to 'javascript:false'). 
        * @type String
        */
        sslBlankUrl : (YAHOO.ext.SSL_SECURE_URL || 'javascript:false'),
        /**
         * Whether to append unique parameter on get request to disable caching (Defaults to false). 
         * @type Boolean
         */
        disableCaching : false,
        /**
         * Whether to show indicatorText when loading (Defaults to true). 
         * @type Boolean
         */
        showLoadIndicator : true,
        /**
         * Text for loading indicator (Defaults to '&lt;div class="loading-indicator"&gt;Loading...&lt;/div&gt;'). 
         * @type String
         */
        indicatorText : '<div class="loading-indicator">Loading...</div>'
   };

/**
 * Static convenience method, Usage: 
 * <pre><code>YAHOO.ext.UpdateManager.updateElement('my-div', 'stuff.php');</code></pre>
 * @param {String/HTMLElement/YAHOO.ext.Element} el The element to update
 * @param {String} url The url
 * @param {<i>String/Object</i>} params (optional) Url encoded param string or an object of name/value pairs
 * @param {<i>Object</i>} options (optional) A config object with any of the UpdateManager properties you want to set - for example: {disableCaching:true, indicatorText: 'Loading data...'}
 * @static
 */
YAHOO.ext.UpdateManager.updateElement = function(el, url, params, options){
    var um = getEl(el, true).getUpdateManager();
    YAHOO.ext.util.Config.apply(um, options);
    um.update(url, params, options ? options.callback : null);
};
// alias for backwards compat
YAHOO.ext.UpdateManager.update = YAHOO.ext.UpdateManager.updateElement;
/**
 * @class YAHOO.ext.UpdateManager.BasicRenderer
 * Default Content renderer. Updates the elements innerHTML with the responseText.
 */ 
YAHOO.ext.UpdateManager.BasicRenderer = function(){};

YAHOO.ext.UpdateManager.BasicRenderer.prototype = {
    /**
     * This is called when the transaction is completed and it's time to update the element - The BasicRenderer 
     * updates the elements innerHTML with the responseText - To perform a custom render (i.e. XML or JSON processing), 
     * create an object with a "render(el, response)" method and pass it to setRenderer on the UpdateManager.
     * @param {YAHOO.ext.Element} el The element being rendered
     * @param {Object} response The YUI Connect response object
     * @param {UpdateManager} updateManager The calling update manager
     * @param {Function} callback A callback that will need to be called if loadScripts is true on the UpdateManager
     */
     render : function(el, response, updateManager, callback){
        el.update(response.responseText, updateManager.loadScripts, callback);
    }
};
