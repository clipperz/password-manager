/**
 * @class YAHOO.ext.grid.CheckboxEditor
 * @extends YAHOO.ext.grid.CellEditor
Provides a checkbox for editing boolean values. It currently has no configuration options.<br><br>
For more information on using this editor, see <a href="http://www.jackslocum.com/yui/2006/09/10/adding-built-in-editing-support-to-the-yahoo-ui-extensions-grid/">this blog post</a>.
* @constructor
* Create a new CheckboxEditor
 */
YAHOO.ext.grid.CheckboxEditor = function(){
    var div = document.createElement('span');
    div.className = 'ygrid-editor ygrid-checkbox-editor';
    var cb = document.createElement('input');
    cb.type = 'checkbox';
    cb.setAttribute('autocomplete', 'off');
    div.appendChild(cb);
    document.body.appendChild(div);
    YAHOO.ext.grid.CheckboxEditor.superclass.constructor.call(this, div);
    div.tabIndex = '';
    cb.tabIndex = 1;
    this.cb = getEl(cb, true);
};

YAHOO.extendX(YAHOO.ext.grid.CheckboxEditor, YAHOO.ext.grid.CellEditor);

YAHOO.ext.grid.CheckboxEditor.prototype.fitToCell = function(box){
    this.element.setBox(box, true);
};

YAHOO.ext.grid.CheckboxEditor.prototype.setValue = function(value){
     this.cb.dom.checked = (value === true || value === 'true' || value === 1 || value === '1');
};

YAHOO.ext.grid.CheckboxEditor.prototype.getValue = function(){
     return this.cb.dom.checked;
};

YAHOO.ext.grid.CheckboxEditor.prototype.show = function(){
    this.element.show();
    this.cb.focus();
};

YAHOO.ext.grid.CheckboxEditor.prototype.initEvents = function(){
    var stopOnEnter = function(e){
        if(e.browserEvent.keyCode == e.RETURN){
            this.stopEditing(true);
        }else if(e.browserEvent.keyCode == e.ESC){
            this.setValue(this.originalValue);
            this.stopEditing(true);
        }
    }
    this.cb.mon('keydown', stopOnEnter, this, true);
    this.cb.on('blur', this.stopEditing, this, true);
};

YAHOO.ext.grid.CheckboxEditor.prototype.hide = function(){
    try{
        this.cb.dom.blur();
    }catch(e){}
    this.element.hide();
};
