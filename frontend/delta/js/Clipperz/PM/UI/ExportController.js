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

Clipperz.Base.module('Clipperz.PM.UI');

Clipperz.PM.UI.ExportController = function(args) {
	this._type			= args['type']			|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._recordsInfo	= args['recordsInfo']	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._target		= Clipperz.PM.Crypto.randomKey();
	
	this._style = "body {"+
			"	margin: 0;"+
			"	padding: 0;"+
			"	font-family: monospace;"+
			"}"+
			""+
			"p {"+
			"	padding-left: 1em;"+
			"}"+
			""+
			"h1 {"+
			"	color: #ff9900;"+
			"	background: black;"+
			"	box-shadow: 0px 5px 6px 0 rgba(0, 0, 0, 0.15);"+
			"	margin: 0;"+
			"	padding:1em;"+
			"}"+
			""+
			".progressBar {"+
			"	position: absolute;"+
			"	width: 100%;"+
			"	margin-top: 0px;"+
			"	"+
			"}"+
			""+
			"#completed {"+
			"	background: #ff9900;"+
			"	color: white;"+
			"	width: 0;"+
			"	overflow: hidden;"+
			"	font-size: 0.8em;"+
			"	box-shadow: 0px 4px 6px 0 rgba(0, 0, 0, 0.15);"+
			"}"+
			""+
			"#printableUl {"+
			"	width:100%;"+
			"	height:80%;"+
			"	margin: 0;"+
			"	padding: 0;"+
			"	list-style-type: none;"+
			"}"+
			""+
			"#printableUl li {"+
			"	border: 1px solid #1863a1;"+
			"	margin: 1em;"+
			""+
			"}"+
			""+
			"#printableUl li .label {"+
			"	background: #1863a1;"+
			"	color: white;"+
			"	display: block;"+
			"	padding: 1em;"+
			"}"+
			""+
			"#printableUl li dl {"+
			"	padding: 1em;"+
			"}"+
			""+
			"#printableUl li dl dt {"+
			"	color: darkgray;"+
			"}"+
			""+
			"#printableUl li dl dd {"+
			"	padding: 0;"+
			"	margin: 0 0 .5em 0;"+
			"}"+
			""+
			"#printableUl li .notes {"+
			"	font-style: italic;"+
			"	padding: 1em 0 0 1em;"+
			"	display: block;"+
			"}"+
			"";
	
	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.ExportController.prototype, {

	'toString': function() {
		return "Clipperz.PM.UI.ExportController";
	},

	//-----------------------------------------------------------------------------

	'type': function () {
		return this._type;
	},

	'recordsInfo': function () {
		return this._recordsInfo;
	},

	'target': function () {
		return this._target;
	},

	//=============================================================================

	'setWindowTitle': function (aWindow, aTitle) {
		aWindow.document.title = aTitle;
	},
	
	'setWindowBody': function (aWindow, anHTML) {
		aWindow.document.body.innerHTML = anHTML;
	},

	//=============================================================================

	'initialWindowSetup': function (aWindow) {
		var dom = MochiKit.DOM.DIV({'id': 'main'},
			MochiKit.DOM.H1("Clipperz Exported Data (loading...)"),
			MochiKit.DOM.DIV({'class': 'progressBar'},
				MochiKit.DOM.DIV({'id': 'completed'},
					MochiKit.DOM.P({'style': 'margin:0; padding:0; text-align:center;'}, MochiKit.DOM.SPAN({'id': 'nCompleted'},"0"),"/",MochiKit.DOM.SPAN({'id': 'nTotal'},"") )
				)
			)
		);
		
		aWindow.document.getElementsByTagName('head')[0].appendChild( MochiKit.DOM.STYLE(this._style) );
		
		this.setWindowTitle(aWindow, "Clipperz Exported Data (loading...)");
		this.setWindowBody (aWindow, MochiKit.DOM.toHTML(dom));
	},

	//-----------------------------------------------------------------------------
	
	'updateWindowWithHTMLContent': function (aWindow, anHtml) {
		this.setWindowBody(aWindow, anHtml);
	},
	
	'updateWindowJSON': function (aWindow, exportedJSON) {
		var dom = MochiKit.DOM.DIV({'id': 'main'},
			MochiKit.DOM.H1("Clipperz Exported Data"),
			MochiKit.DOM.P("You can now save the following data and load it at any time using the Clipperz import feature."),
			MochiKit.DOM.TEXTAREA({'style': 'width:100%; height:80%'}, Clipperz.Base.serializeJSON(exportedJSON))
		);
		
		this.setWindowTitle(aWindow, "Clipperz Exported Data");
		this.setWindowBody(aWindow, MochiKit.DOM.toHTML(dom));
	},
	
	'updateWindowPrintable': function (aWindow, exportedJSON) {
		var dom = MochiKit.DOM.DIV({'id': 'main'},
			MochiKit.DOM.H1("Clipperz Exported Data"),
			MochiKit.DOM.P("You can now print this page and store it in a safe place."),
			MochiKit.DOM.UL({'id': 'printableUl'},
				exportedJSON.map(function(card){
					var label = (card.label.indexOf('')>=0) ? card.label.slice(0,card.label.indexOf('')).trim() : card.label;
					var notes = (card.data.notes) ? MochiKit.DOM.SPAN({'class': 'notes'}, card.data.notes) : "";
					
					return MochiKit.DOM.LI({},
						MochiKit.DOM.SPAN({'class': 'label'}, label),
						notes,
						MochiKit.DOM.DL({},
							Object.keys(card.currentVersion.fields).map(function(key) {
								return [
									MochiKit.DOM.DT(card.currentVersion.fields[key].label),
									MochiKit.DOM.DD(card.currentVersion.fields[key].value),
								];
							})
						)
					);
				})
			)
		);
		
		this.setWindowTitle(aWindow, "Clipperz Exported Data");
		this.setWindowBody(aWindow, MochiKit.DOM.toHTML(dom));
	},
	
	'updateWindowError': function (aWindow, errorMessage) {
		this.setWindowBody(aWindow,
			"<h3>Error</h3>"+
			"<p>The following error occured while exporting your data:</p>"+
			"<code>"+errorMessage+"</code>"
		);
	},

	//=============================================================================

	'runExportJSON': function (aWindow) {
		var deferredResult;
		var exportedRecords;
		
		var totalRecords = this.recordsInfo().length;
		
		exportedRecords = 0;

		deferredResult = new Clipperz.Async.Deferred("DirectLoginRunner.exportJSON", {trace:false});
		deferredResult.addMethod(this, 'initialWindowSetup', aWindow);
		deferredResult.addCallback(function() { return "Export Data"});
		deferredResult.addMethod(this, 'setWindowTitle', aWindow);
		
		deferredResult.addMethod( this, function() { return this.recordsInfo(); });
		deferredResult.addCallback( MochiKit.Base.map, function(recordIn) {
			var dr = new Clipperz.Async.Deferred("DirectLoginRunner.exportJSON__exportRecord", {trace:false});
			dr.addMethod(recordIn._rowObject, 'export');
			dr.addCallback(MochiKit.Base.method(this, function (exportedRecord) {
				var percentage = Math.round(100*exportedRecords/totalRecords);
				
				aWindow.document.getElementById('nCompleted').innerText = ++exportedRecords;
				aWindow.document.getElementById('nTotal').innerText = totalRecords;
				aWindow.document.getElementById('completed').style.width = percentage+'%';
				
				return exportedRecord;
			}));
			dr.callback();
			return dr;
		});
		
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addMethod( this, function(exportedJSONIn) {
// console.log('return',exportedJSONIn);

			sortedJSON = exportedJSONIn.sort( function(a,b) { return a.label.toUpperCase().localeCompare(b.label.toUpperCase()); } );

			switch (this.type()) {
				case 'json':
					this.updateWindowJSON(aWindow,exportedJSONIn);
					break;
				case 'printable':
					this.updateWindowPrintable(aWindow,exportedJSONIn);
					break;
				default:
					this.updateWindowError(aWindow,"ExportController.runExportJSON: invalid value '"+this.type()+"' for parameter 'type'.");
			}
		});
		
		deferredResult.callback();

		return deferredResult;
	},

	//=============================================================================

	'run': function () {
		var newWindow;

		newWindow = window.open("", this.target());

		return this.runExportJSON(newWindow);
	},
	
	//=============================================================================

	'test': function () {
		var iFrame;
		var newWindow;

		iFrame = MochiKit.DOM.createDOM('iframe');
		MochiKit.DOM.appendChildNodes(MochiKit.DOM.currentDocument().body, iFrame);

		newWindow = iFrame.contentWindow;

		return this.runDirectLogin(newWindow);
	},
	
	//=============================================================================
	__syntaxFix__: "syntax fix"
});

//-----------------------------------------------------------------------------

Clipperz.PM.UI.ExportController.exportJSON = function (recordsInfoIn, typeIn) {
	var	runner;
	
	runner = new Clipperz.PM.UI.ExportController({type:typeIn, recordsInfo: recordsInfoIn});
	return runner.run();
};
