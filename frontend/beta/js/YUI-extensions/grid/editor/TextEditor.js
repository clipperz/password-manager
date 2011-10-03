/**
 * @class YAHOO.ext.grid.TextEditor
 * @extends YAHOO.ext.grid.CellEditor
Provides basic text editing for a cells and supports the following configuration options:
<ul class="list">
<li><i>allowBlank</i> - True if the cell is allowed to be empty.</li>
<li><i>minLength</i> - The minimum length the cell will accept.</li>
<li><i>maxLength</i> - The maximum length the cell will allow.</li>
<li><i>minText</i> - The tooltip to display when the length of the value in the cell is below the minimum.</li>
<li><i>maxText</i> - The tooltip to display when the length of the value in the cell is above the maximum.</li>
<li><i>selectOnFocus</i> - True to select the text when the editor is activated.</li>
<li><i>blankText</i> - The tooltip (error message) to display when the cell is empty and is not allowed to be.</li>
<li><i>regex</i> - A regular expression to match if the value is valid. If the regex.test(value) returns false, the value is considered invalid.</li>
<li><i>regexText</i> - The tooltip (error message) to display when regex does not match.</li>
<li><i>validator</i> - Any custom validation function you want called. The function must return true if the data is valid or an error message otherwise.</li>
<li><i>validationDelay</i> - The delay in milliseconds for validation. Each time the user types something the field is validated after a specified delay, setting this value allows you to customize that delay (for example, if your custom validation routine is slow).</li>
</ul>
For more information on using this editor, see <a href="http://www.jackslocum.com/yui/2006/09/10/adding-built-in-editing-support-to-the-yahoo-ui-extensions-grid/">this blog post</a>.
* @constructor
* Create a new TextEditor
* @param {Object} config
 */
YAHOO.ext.grid.TextEditor = function(config){
    var element = document.createElement('input');
    element.type = 'text';
    element.className = 'ygrid-editor ygrid-text-editor';
    element.setAttribute('autocomplete', 'off');
    document.body.appendChild(element);
    YAHOO.ext.grid.TextEditor.superclass.constructor.call(this, element);
    YAHOO.ext.util.Config.apply(this, config);
};
YAHOO.extendX(YAHOO.ext.grid.TextEditor, YAHOO.ext.grid.CellEditor);

YAHOO.ext.grid.TextEditor.prototype.validate = function(){
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
    if(value.length < this.minLength){
        dom.title = this.minText.replace('%0', this.minLength);
        this.element.addClass('ygrid-editor-invalid');
        return false;
    }
    if(value.length > this.maxLength){
        dom.title = this.maxText.replace('%0', this.maxLength);
        this.element.addClass('ygrid-editor-invalid');
        return false;
    }
    var msg = this.validator(value);
    if(msg !== true){
        dom.title = msg;
        this.element.addClass('ygrid-editor-invalid');
        return false;
    }
    if(this.regex && !this.regex.test(value)){
        dom.title = this.regexText;
        this.element.addClass('ygrid-editor-invalid');
        return false;
    }
    dom.title = '';
    this.element.removeClass('ygrid-editor-invalid');
    return true;
};

YAHOO.ext.grid.TextEditor.prototype.initEvents = function(){
    YAHOO.ext.grid.TextEditor.superclass.initEvents.call(this);
    var vtask = new YAHOO.ext.util.DelayedTask(this.validate, this);
    this.element.mon('keyup', vtask.delay.createDelegate(vtask, [this.validationDelay]));
};

YAHOO.ext.grid.TextEditor.prototype.show = function(){
    this.element.dom.title = '';
    YAHOO.ext.grid.TextEditor.superclass.show.call(this);
    this.element.focus();
    if(this.selectOnFocus){
        try{
            this.element.dom.select();
        }catch(e){}
    }
    this.validate(this.element.dom.value);
};

YAHOO.ext.grid.TextEditor.prototype.getValue = function(){
   if(!this.validate()){
       return this.originalValue;
   }else{
       return this.element.dom.value;
   }   
};

YAHOO.ext.grid.TextEditor.prototype.allowBlank = true;
YAHOO.ext.grid.TextEditor.prototype.minLength = 0;
YAHOO.ext.grid.TextEditor.prototype.maxLength = Number.MAX_VALUE;
YAHOO.ext.grid.TextEditor.prototype.minText = 'The minimum length for this field is %0';
YAHOO.ext.grid.TextEditor.prototype.maxText = 'The maximum length for this field is %0';
YAHOO.ext.grid.TextEditor.prototype.selectOnFocus = true;
YAHOO.ext.grid.TextEditor.prototype.blankText = 'This field cannot be blank';
YAHOO.ext.grid.TextEditor.prototype.validator = function(){return true;};
YAHOO.ext.grid.TextEditor.prototype.validationDelay = 200;
YAHOO.ext.grid.TextEditor.prototype.regex = null;
YAHOO.ext.grid.TextEditor.prototype.regexText = '';
