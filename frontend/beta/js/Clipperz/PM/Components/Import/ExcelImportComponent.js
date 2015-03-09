/*

Copyright 2008-2015 Clipperz Srl

This file is part of Clipperz, the online password manager.
For further information about its features and functionalities please
refer to http://www.clipperz.com.

* Clipperz is free software: you can redistribute it and/or modify it
  under the terms of the GNU Affero General Public License as published
  by the Free Software Foundation, either version 3 of the License, or 
  (at your option) any later version.

* Clipperz is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of 
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Clipperz. If not, see http://www.gnu.org/licenses/.

*/

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.Components) == 'undefined') { Clipperz.PM.Components = {}; }
if (typeof(Clipperz.PM.Components.Import) == 'undefined') { Clipperz.PM.Components.Import = {}; }

//#############################################################################

Clipperz.PM.Components.Import.ExcelImportComponent = function(anElement, args) {
	args = args || {};

	this._steps = ['EXCEL_EDIT', 'CSV_COLUMNS', 'CSV_HEADER', 'CSV_TITLE', 'CSV_NOTES', 'CSV_FIELDS', 'PREVIEW', 'IMPORT'];

    Clipperz.PM.Components.Import.ExcelImportComponent.superclass.constructor.call(this, anElement, args);

	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Import.ExcelImportComponent, Clipperz.PM.Components.Import.CSVImportComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.Import.ExcelImportComponent component";
	},

	//-------------------------------------------------------------------------

	'render': function() {
//MochiKit.Logging.logDebug(">>> Import.ExcelImportComponent.render");
		this.domHelper().append(this.element(), {tag:'div', cls:'excelImportWizard', children:[
			{tag:'h3', htmlString:Clipperz.PM.Strings['Excel_ImportWizard_Title']},
			{tag:'div', cls:'importSteps', id:this.getId('importSteps')},
			{tag:'div', cls:'importStepBlocks', children:[
				{tag:'div', cls:'step_0', id:this.getId('step_0'), children:[
					{tag:'div', children:[
						{tag:'div', cls:'importOptionsDescription', htmlString:Clipperz.PM.Strings['importOptions_excel_description']},
						{tag:'div', cls:'importOptionsParameters', children:[]},
						this.textAreaConfig()
					]}
				]},
				{tag:'div', cls:'step_1', id:this.getId('step_1'), children:[]},
				{tag:'div', cls:'step_2', id:this.getId('step_2'), children:[]},
				{tag:'div', cls:'step_3', id:this.getId('step_3'), children:[]},
				{tag:'div', cls:'step_4', id:this.getId('step_4'), children:[]},
				{tag:'div', cls:'step_5', id:this.getId('step_5'), children:[]},
				{tag:'div', cls:'step_6', id:this.getId('step_6'), children:[
					{tag:'div', children:[
						{tag:'div', id:this.getId('previewDiv'), html:"preview"}
					]}
				]},
				{tag:'div', cls:'step_7', id:this.getId('step_7'), children:[
					{tag:'div', children:[
						{tag:'h4', html:"done"}
					]}
				]}
			]},
			{tag:'div', cls:'importOptionsButtons', children:[
				{tag:'table', children:[
					{tag:'tbody', children:[
						{tag:'tr', children:[
							{tag:'td', html:'&nbsp;'},
							{tag:'td', children:[
								{tag:'div', id:this.getId('backActionButton')}
							]},
							{tag:'td', html:'&nbsp;'},
							{tag:'td', children:[
								{tag:'div', id:this.getId('nextActionButton')}
							]},
							{tag:'td', html:'&nbsp;'}
						]}
					]}
				]}
			]}
		]});
		
		this.updateSteps();
		
		this.setBackButton(new YAHOO.ext.Button(this.getDom('backActionButton'), {text:"back", handler:this.backAction, scope:this}));
		this.setNextButton(new YAHOO.ext.Button(this.getDom('nextActionButton'), {text:"next", handler:this.nextAction, scope:this}));
		
		this.getElement('step_0').setVisibilityMode(YAHOO.ext.Element.DISPLAY).show()
		this.getElement('step_1').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.getElement('step_2').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.getElement('step_3').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.getElement('step_4').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.getElement('step_5').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.getElement('step_6').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		this.getElement('step_7').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
//MochiKit.Logging.logDebug("<<< Import.ExcelImportComponent.render");
	},

	//-------------------------------------------------------------------------

	'csvProcessor': function() {
		return new Clipperz.CSVProcessor({
//			quoteChar:		this.getDom('CSV_inputOptions_quote').value,
//			escapeChar:		this.getDom('CSV_inputOptions_escape').value,
			separatorChar:	'\t',
			binary:true
		});
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

