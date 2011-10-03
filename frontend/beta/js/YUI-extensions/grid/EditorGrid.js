/**
 * @class YAHOO.ext.grid.EditorGrid
 * @extends YAHOO.ext.grid.Grid
 * Shortcut class for creating and editable grid.
 * @param {String/HTMLElement/YAHOO.ext.Element} container The element into which this grid will be rendered - 
 * The container MUST have some type of size defined for the grid to fill. The container will be 
 * automatically set to position relative if it isn't already.
 * @param {Object} dataModel The data model to bind to
 * @param {Object} colModel The column model with info about this grid's columns
 */
YAHOO.ext.grid.EditorGrid = function(container, dataModel, colModel){
    YAHOO.ext.grid.EditorGrid.superclass.constructor.call(this, container, dataModel, 
                      colModel, new YAHOO.ext.grid.EditorSelectionModel());
    this.container.addClass('yeditgrid');
};
YAHOO.extendX(YAHOO.ext.grid.EditorGrid, YAHOO.ext.grid.Grid);
