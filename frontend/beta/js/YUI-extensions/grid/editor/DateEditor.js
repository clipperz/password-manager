/**
 * @class YAHOO.ext.grid.DateEditor
 * @extends YAHOO.ext.grid.CellEditor
Provides a date editor field, and optionally a DatePicker. The DateEditor provides a method to override (showCalendar) if you don't want to use the built in DatePicker control. The reason I chose to use my own DatePicker control rather than the nice YUI Calendar component is my control was very easy to override events to make it work well with the grid. It's also only 5k compressed, while the YUI Calendar is 40k compressed. The DatePicker supports left/right keys to move months, up/down keys to move years and the mouse wheel to quickly go through the months. The DateEditor supports the following configuration options:
<ul class="list">
<li><i>format</i> - The date format for the editor. The format is identical to <a href="http://www.php.net/date">PHP date()</a> and text is allowed. Credit for that goes to <a style="font-weight:normal;" href="http://www.xaprb.com/blog/2006/05/14/javascript-date-formatting-benchmarks/">this fantastic date library</a>. This format is for the editor only and doesn't affect the rendering of the cell when not in edit mode. Your rendering function can use any date format it wants.</li>
<li><i>minValue</i> - The minimum allowed date. Can be either a Javascript date object or a string date in the specified format.</li>
<li><i>maxValue</i> - The maximum allowed date. Can be either a Javascript date object or a string date in the specified format.</li>
<li><i>minText</i> - The tooltip to display when the date in the cell is before minValue.</li>
<li><i>maxText</i> - The tooltip to display when the date in the cell is after maxValue.</li>
<li><i>invalidText</i> - The text to display when the date in the field is invalid (for example: 02/31/06)</li>
<li><i>disabledDays</i> - An array of days to disable, 0 based. For example, [0, 6] disables Sunday and Saturday.</li>
<li><i>disabledDaysText</i> - The tooltip to display when the date in the cell (or DatePicker) falls on a disabled day.</li>
<li><i>disabledDates</i> - An array of "dates" to disable, as strings. These strings will be used to build a dynamic regular expression so they are very powerful. For example, ["03/08/2003", "09/16/2003"] would disable those dates, but ["03/08", "09/16"] would disable them for every year. If you are using short years, you will want to use ^ to tell the regular expression to only match the beginning like ["^03/08"]. To disable March of 2006: ["03/../2006"] or every March ["^03"]. In order to support regular expressions, if you are using a date format that has "." in it, you will have to  escape the dot when restricting dates. For example: ["03\\.08\\.03"].</li>
<li><i>disabledDatesText</i> - The tooltip to display when the date in the cell (or DatePicker) falls on a disabled date.</li>
<li><i>allowBlank</i> - True if the cell is allowed to be empty.</li>
<li><i>blankText</i> - The tooltip (error message) to display when the cell is empty and is not allowed to be.</li>
<li><i>validator</i> - Any custom validation function you want called. The function must return true if the data is valid or an error message otherwise.</li>
<li><i>validationDelay</i> - The delay in milliseconds for validation. Each time the user types something the field is validated after a specified delay, setting this value allows you to customize that delay (for example, if your custom validation routine is slow).</li>
</ul>
For more information on using this editor, see <a href="http://www.jackslocum.com/yui/2006/09/10/adding-built-in-editing-support-to-the-yahoo-ui-extensions-grid/">this blog post</a>.
* @constructor
* Create a new DateEditor
* @param {Object} config
 */
YAHOO.ext.grid.DateEditor = function(config){
    var div = document.createElement('span');
    div.className = 'ygrid-editor ygrid-editor-container';
    
    var element = document.createElement('input');
    element.type = 'text';
    element.tabIndex = 1;
    element.setAttribute('autocomplete', 'off');
    div.appendChild(element);
    
    var pick = document.createElement('span');
    pick.className = 'pick-button';
    div.appendChild(pick);
    
    document.body.appendChild(div);
    
    this.div = getEl(div, true);
    this.element = getEl(element, true);
    this.pick = getEl(pick, true);
    
    this.colIndex = null;
    this.rowIndex = null;
    this.grid = null;
    this.editing = false;
    this.originalValue = null;
    this.initialized = false;
    this.callback = null;
    
    this.cal = null;
    this.mouseDownHandler = YAHOO.ext.EventManager.wrap(this.handleMouseDown, this, true);
    
    YAHOO.ext.util.Config.apply(this, config);
    if(typeof this.minValue == 'string') this.minValue = this.parseDate(this.minValue);
    if(typeof this.maxValue == 'string') this.maxValue = this.parseDate(this.maxValue);
    this.ddMatch = /ddnone/;
    if(this.disabledDates){
        var dd = this.disabledDates;
        var re = "(?:";
        for(var i = 0; i < dd.length; i++){
            re += dd[i];
            if(i != dd.length-1) re += "|";
        }
        this.ddMatch = new RegExp(re + ")");
    }
};

YAHOO.ext.grid.DateEditor.prototype = {
    init : function(grid, bodyElement, callback){
        if(this.initialized) return;
        
        this.initialized = true;
        this.callback = callback;
        this.grid = grid;
        bodyElement.appendChild(this.div.dom);
        this.initEvents();
    },
    
    initEvents : function(){
         var stopOnEnter = function(e){
            if(e.browserEvent.keyCode == e.RETURN){
                this.stopEditing(true);
            }else if(e.browserEvent.keyCode == e.ESC){
                this.setValue(this.originalValue);
                this.stopEditing(true);
            }
        }
        this.element.mon('keydown', stopOnEnter, this, true);
        var vtask = new YAHOO.ext.util.DelayedTask(this.validate, this);
        this.element.mon('keyup', vtask.delay.createDelegate(vtask, [this.validationDelay]));
        this.pick.on('click', this.showCalendar, this, true);
    },
    
    startEditing : function(value, row, cell){
        this.originalValue = value;
        this.rowIndex = row.rowIndex;
        this.colIndex = cell.columnIndex;
        this.cell = cell;
        this.setValue(value);
        this.validate();
        var cellbox = getEl(cell, true).getBox();
        this.div.setBox(cellbox, true);
        this.element.setWidth(cellbox.width-this.pick.getWidth());
        this.editing = true;
        YAHOO.util.Event.on(document, "mousedown", this.mouseDownHandler);
        this.show();
    },
     
     stopEditing : function(focusCell){
         if(this.editing){
             YAHOO.util.Event.removeListener(document, "mousedown", this.mouseDownHandler);
             this.editing = false;
             var newValue = this.getValue();
             this.hide();
             //if(focusCell){try{this.cell.focus();}catch(e){}}// try to give the cell focus so keyboard nav still works
             if(this.originalValue != newValue){
                this.callback(newValue, this.rowIndex, this.colIndex);
             }
         }
     },
    
    setValue : function(value){
        this.element.dom.value = this.formatDate(value);
        this.validate();
    },
    
    getValue : function(){
        if(!this.validate()){
           return this.originalValue;
       }else{
           var value = this.element.dom.value;
           if(value.length < 1){
               return value;
           } else{
               return this.parseDate(value);
           }
       }   
    },
    
    show : function() {
        this.div.show();
        this.element.focus();
        this.validate();
    },
    
    hide : function(){
        try{
            this.element.dom.blur();
        }catch(e){}
        this.div.hide();
    },
    
    validate : function(){
        var dom = this.element.dom;
        var value = dom.value;
        if(value.length < 1){ // if it's blank
             if(this.allowBlank){
                 dom.title = '';
                 this.element.removeClass('ygrid-editor-invalid');
                 return true;
             }else{
                 dom.title = this.blankText;
                 this.element.addClass('ygrid-editor-invalid');
                 return false;
             }
        }
        value = this.parseDate(value);
        if(!value){
            dom.title = this.invalidText.replace('%0', dom.value).replace('%1', this.format);
            this.element.addClass('ygrid-editor-invalid');
            return false;
        }
        var time = value.getTime();
        if(this.minValue && time < this.minValue.getTime()){
            dom.title = this.minText.replace('%0', this.formatDate(this.minValue));
            this.element.addClass('ygrid-editor-invalid');
            return false;
        }
        if(this.maxValue && time > this.maxValue.getTime()){
            dom.title = this.maxText.replace('%0', this.formatDate(this.maxValue));
            this.element.addClass('ygrid-editor-invalid');
            return false;
        }
        if(this.disabledDays){
            var day = value.getDay();
            for(var i = 0; i < this.disabledDays.length; i++) {
            	if(day === this.disabledDays[i]){
            	    dom.title = this.disabledDaysText;
                    this.element.addClass('ygrid-editor-invalid');
                    return false;
            	}
            }
        }
        var fvalue = this.formatDate(value);
        if(this.ddMatch.test(fvalue)){
            dom.title = this.disabledDatesText.replace('%0', fvalue);
            this.element.addClass('ygrid-editor-invalid');
            return false;
        }
        var msg = this.validator(value);
        if(msg !== true){
            dom.title = msg;
            this.element.addClass('ygrid-editor-invalid');
            return false;
        }
        dom.title = '';
        this.element.removeClass('ygrid-editor-invalid');
        return true;
    },
    
    handleMouseDown : function(e){
        var t = e.getTarget();
        var dom = this.div.dom;
        if(t != dom && !YAHOO.util.Dom.isAncestor(dom, t)){
            this.stopEditing();
        }
    },
    
    showCalendar : function(value){
        if(this.cal == null){
            this.cal = new YAHOO.ext.DatePicker(this.div.dom.parentNode.parentNode);
        }
        this.cal.minDate = this.minValue;
        this.cal.maxDate = this.maxValue;
        this.cal.disabledDatesRE = this.ddMatch;
        this.cal.disabledDatesText = this.disabledDatesText;
        this.cal.disabledDays = this.disabledDays;
        this.cal.disabledDaysText = this.disabledDaysText;
        this.cal.format = this.format;
        if(this.minValue){
            this.cal.minText = this.minText.replace('%0', this.formatDate(this.minValue));
        }
        if(this.maxValue){
            this.cal.maxText = this.maxText.replace('%0', this.formatDate(this.maxValue));
        }
        var r = this.div.getRegion();
        this.cal.show(r.left, r.bottom, this.getValue(), this.setValue.createDelegate(this));
    },
    
    parseDate : function(value){
        if(!value || value instanceof Date) return value;
        return Date.parseDate(value, this.format);
    },
    
    formatDate : function(date){
        if(!date || !(date instanceof Date)) return date;
        return date.format(this.format);
    }
};

YAHOO.ext.grid.DateEditor.prototype.format = 'm/d/y';
YAHOO.ext.grid.DateEditor.prototype.disabledDays = null;
YAHOO.ext.grid.DateEditor.prototype.disabledDaysText = '';
YAHOO.ext.grid.DateEditor.prototype.disabledDates = null;
YAHOO.ext.grid.DateEditor.prototype.disabledDatesText = '';
YAHOO.ext.grid.DateEditor.prototype.allowBlank = true;
YAHOO.ext.grid.DateEditor.prototype.minValue = null;
YAHOO.ext.grid.DateEditor.prototype.maxValue = null;
YAHOO.ext.grid.DateEditor.prototype.minText = 'The date in this field must be after %0';
YAHOO.ext.grid.DateEditor.prototype.maxText = 'The date in this field must be before %0';
YAHOO.ext.grid.DateEditor.prototype.blankText = 'This field cannot be blank';
YAHOO.ext.grid.DateEditor.prototype.invalidText = '%0 is not a valid date - it must be in the format %1';
YAHOO.ext.grid.DateEditor.prototype.validationDelay = 200;
YAHOO.ext.grid.DateEditor.prototype.validator = function(){return true;};
