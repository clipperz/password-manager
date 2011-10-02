/**
 * @class YAHOO.ext.grid.SelectEditor
 * @extends YAHOO.ext.grid.CellEditor
Creates an editor out of an existing select field. You can create the select element through DOM in Javascript and pass it to the SelectEditor's constructor <b>or</b> an easier way is like this:
<br><br>
Define the select field in your document, giving it the ygrid-editor class.
<pre><code>
&lt;select id="light" class="ygrid-editor"&gt;
	&lt;option value="Shade"&gt;Shade&lt;/option&gt;
	&lt;option value="Mostly Shady"&gt;Mostly Shady&lt;/option&gt;
	&lt;option value="Sun or Shade"&gt;Sun or Shade&lt;/option&gt;
	&lt;option value="Mostly Sunny"&gt;Mostly Sunny&lt;/option&gt;
	&lt;option value="Sunny"&gt;Sunny&lt;/option&gt;
&lt;/select&gt;
</code></pre>
Create the SelectEditor object, passing in the id of your select field.
<pre><code>
var editor = new YAHOO.ext.grid.SelectEditor('light'); 
</code></pre>
For more information on using this editor, see <a href="http://www.jackslocum.com/yui/2006/09/10/adding-built-in-editing-support-to-the-yahoo-ui-extensions-grid/">this blog post</a>.
* @constructor
* Create a new SelectEditor
* @param {HTMLElement/String} element
 */
YAHOO.ext.grid.SelectEditor = function(element){
    element.hideFocus = true;
    YAHOO.ext.grid.SelectEditor.superclass.constructor.call(this, element);
    this.element.swallowEvent('click');
};
YAHOO.extendX(YAHOO.ext.grid.SelectEditor, YAHOO.ext.grid.CellEditor);

YAHOO.ext.grid.SelectEditor.prototype.fitToCell = function(box){
    if(YAHOO.ext.util.Browser.isGecko){
        box.height -= 3;
    }
    this.element.setBox(box, true);
};
