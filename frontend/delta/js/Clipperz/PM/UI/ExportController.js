/*

Copyright 2008-2018 Clipperz Srl

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

"use strict";
Clipperz.Base.module('Clipperz.PM.UI');

//	https://github.com/eligrey/FileSaver.js
//	https://github.com/eligrey/Blob.js

Clipperz.PM.UI.ExportController = function(args) {
	this._processedRecords = 0;
	this._totalCardsToExport = 0;
	
	this._style =
		"body {" +
			"font-family: 'DejaVu Sans Mono', monospace;" +
			"margin: 0px;" +
		"}" +

		"header {" +
			"padding: 10px;" +
			"border-bottom: 2px solid black;" +
		"}" +
	
		"header p span {" +
//			"padding: 0px 4px;" +
			"font-weight: bold;" +
		"}" +

		"h1 {" +
			"margin: 0px;" +
		"}" +

		"h2 {" +
			"margin: 0px;" +
			"padding-top: 10px;" +
		"}" +

		"h3 {" +
			"margin: 0px;" +
		"}" +

		"h5 {" +
			"margin: 0px;" +
			"color: gray;" +
		"}" +

		"ul {" +
			"margin: 0px;" +
			"padding: 0px;" +
		"}" +

		"div > ul > li {" +
			"border-bottom: 1px solid black;" +
			"padding: 10px;" +
		"}" +

		"div > ul > li.archived {" +
			"background-color: #ddd;" +
		"}" +


		"ul > li > ul > li {" +
			"font-size: 9pt;" +
			"display: inline-block;" +
		"}" +

		"ul > li > ul > li:after {" +
			"content: \",\";" +
			"padding-right: 5px;" +
		"}" +

		"ul > li > ul > li:last-child:after {" +
			"content: \"\";" +
			"padding-right: 0px;" +
		"}" +

		"dl {" +
		"}" +

		"dt {" +
			"color: gray;" +
			"font-size: 9pt;" +
		"}" +

		"dd {" +
			"margin: 0px;" +
			"margin-bottom: 5px;" +
			"padding-left: 10px;" +
			"font-size: 13pt;" +
		"}" +

		"div > div {" +
			"background-color: black;" +
			"color: white;" +
			"padding: 10px;" +
		"}" +

		"li p, dd.hidden {" +
			"white-space: pre-wrap;" +
			"word-wrap: break-word;" +
			"font-family: monospace;" +
		"}" +
	
		"textarea {" +
			"display: none" +
//			"width: 100%;" +
//			"height: 200px;" +
		"}" +

		"a {" +
			"color: white;" +
		"}" +

		"@media print {" +
			"div > div, header > div {" +
				"display: none !important;" +
			"}" +

			"div > ul > li.archived {" +
				"color: #ddd;" +
			"}" +

			"ul > li {" +
				"page-break-inside: avoid;" +
			"}	" +
		"}" +
	
		"";
	
	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.ExportController.prototype, {

	'toString': function() {
		return "Clipperz.PM.UI.ExportController";
	},

	//=============================================================================

	'reportRecordExport': function (aRecordData) {
		var percentage;
		
		this._processedRecords = this._processedRecords + 1;
		percentage = Math.round(100 * this._processedRecords / this._totalCardsToExport);

//console.log("PROCESSING " + exportedCardsCount + "/" + totalCardsToExport + " - " + percentage + "%");
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'updateProgress', percentage);

		return MochiKit.Async.succeed(aRecordData);
	},

	//=============================================================================

	'renderCardToHtml': function (jsonCardData) {
		var	label = Clipperz.PM.DataModel.Record.extractLabelFromFullLabel(jsonCardData.label);
		var allTags = MochiKit.Base.keys(Clipperz.PM.DataModel.Record.extractTagsFromFullLabel(jsonCardData.label));
		var regularTags = MochiKit.Base.filter(Clipperz.PM.DataModel.Record.isRegularTag, allTags);
		var	isArchived = MochiKit.Iter.some(allTags, MochiKit.Base.partial(MochiKit.Base.objEqual, Clipperz.PM.DataModel.Record.archivedTag));

		return MochiKit.DOM.LI({'class': isArchived ? 'archived' : ""},
			MochiKit.DOM.H2({}, label),
			(regularTags.length > 0) ? MochiKit.DOM.UL({}, MochiKit.Base.map(function (tag) { return MochiKit.DOM.LI({}, tag);}, regularTags)): null,
			MochiKit.DOM.DIV({},
				MochiKit.DOM.DL({},
					MochiKit.Base.map(function(key) {
						var	isHiddenField = jsonCardData.currentVersion.fields[key]['hidden'];

						return [
							MochiKit.DOM.DT({}, jsonCardData.currentVersion.fields[key]['label']),
							MochiKit.DOM.DD((isHiddenField ? {'class':'hidden'} : {}), jsonCardData.currentVersion.fields[key]['value']),
						];
					}, MochiKit.Base.keys(jsonCardData.currentVersion.fields))
				)
			),
			jsonCardData.data.notes ? MochiKit.DOM.P({}, jsonCardData.data.notes) : null
		);
	},

	'renderToHtml': function (jsonData) {
		var	title;
		var	style;
		var	now;
		var	dateString;
		var	timeString
		var	body;

		title = "Clipperz data";
		style = this._style;
		now  = new XDate();
		dateString = now.toString("MMM d, yyyy");
		timeString = now.toString("HH:mm");

		body = MochiKit.DOM.DIV({},
			MochiKit.DOM.HEADER({},
				MochiKit.DOM.H1({}, "Your data on Clipperz"),
				MochiKit.DOM.H5({}, "Export generated on " + dateString + " at " + timeString),
				MochiKit.DOM.DIV({},
					MochiKit.DOM.P({}, "Security warning - This file lists the content of all your cards in a printer-friendly format"),
					MochiKit.DOM.P({}, [
						"Beware: ",
						MochiKit.DOM.SPAN({'class':'warning'}, "all data are unencrypted!"),
						" Therefore make sure to properly store and manage this file. We recommend to delete it as soon as it is no longer needed."
					]),
					MochiKit.DOM.P({}, "If you are going to print its content on paper, store the printout in a safe and private place!"),
					MochiKit.DOM.P({}, "And, if you need to access your data when no Internet connection is available, please consider the much safer option of creating an offline copy.")
				)
			),

			MochiKit.DOM.UL({}, MochiKit.Base.map(this.renderCardToHtml, jsonData)),
			MochiKit.DOM.DIV({},
//				MochiKit.DOM.H3({}, "JSON content"),
//				MochiKit.DOM.DIV({},
//					MochiKit.DOM.P({}, "Instructions on how to use JSON content"),
//					MochiKit.DOM.P({}, "The JSON version of your data may be useful if you want to move the whole content of your Clipperz account to a new Clipperz account or recover a card that has been accidentally deleted. Just follow these instructions:"),
//					MochiKit.DOM.OL({},
//						MochiKit.DOM.LI({}, "Login to your Clipperz account and go to \"Data > Import\"."),
//						MochiKit.DOM.LI({}, "Select the JSON option."),
//						MochiKit.DOM.LI({}, "Copy and paste the JSON content in the form.")
//					),
//					MochiKit.DOM.P({}, "Of course, the unencrypted JSON content won't be transmitted to the Clipperz server.")
//				),
				MochiKit.DOM.TEXTAREA({}, Clipperz.Base.serializeJSON(jsonData)),
				MochiKit.DOM.FOOTER({},
					MochiKit.DOM.P({},
						"This file has been downloaded from ",
						MochiKit.DOM.A({'href':'https://clipperz.is'} ,"clipperz.is"),
						", a service by Clipperz Srl. - ",
						MochiKit.DOM.A({'href':'https://clipperz.is/terms_service/'}, "Terms of service"),
						" - ",
						MochiKit.DOM.A({'href':'https://clipperz.is/privacy_policy/'}, "Privacy policy")
					)
//					MochiKit.DOM.H4({}, "Clipperz - keep it to yourself")
				)
			)
		);

		return '<html><head><title>' + title + '</title><style type="text/css">' + style + '</style><meta http-equiv="Content-Type" content="text/html; charset=utf-8" /></head><body>' + MochiKit.DOM.toHTML(body) + '</body></html>';
	},

	//----------------------------------------------------------------------------

	'saveResult': function (exportedJSON) {
		var blob;
		var sortedJSON;
		var now  = new XDate();
		var	dateString = now.toString('yyyyMMdd');
		
		sortedJSON = MochiKit.Iter.sorted(exportedJSON, function(a,b) { return a.label.toUpperCase().localeCompare(b.label.toUpperCase()); } );
		blob = new Blob([this.renderToHtml(sortedJSON)], {type: "text/html;charset=utf-8"});
		saveAs(blob, dateString + '-Clipperz_Export.html');
	},

	//=============================================================================
	
	'run': function (records) {
		var deferredResult;
		var self = this;

		this._totalCardsToExport = records.length;
		deferredResult = new Clipperz.Async.Deferred("ExportController.run", {trace:false});
		deferredResult.addCallback(MochiKit.Base.map, function(aRecord) {
			var innerDeferredResult;
			
			innerDeferredResult = new Clipperz.Async.Deferred("ExportController.run__exportRecord", {trace:false});
			innerDeferredResult.addMethod(aRecord, 'export');
			innerDeferredResult.addMethod(self, 'reportRecordExport');
			innerDeferredResult.callback();

			return innerDeferredResult;
		});
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addMethod(this, 'saveResult');
		deferredResult.callback(records);

		return deferredResult;
	},
	
	//=============================================================================
	__syntaxFix__: "syntax fix"
});
