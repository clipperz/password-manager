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
if (typeof(Clipperz.PM.Components.Panels) == 'undefined') { Clipperz.PM.Components.Panels = {}; }

//#############################################################################

Clipperz.PM.Components.Panels.DataPanel = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Panels.DataPanel.superclass.constructor.call(this, anElement, args);

	this._progressWidth = 400;


	this.render();
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Panels.DataPanel, Clipperz.PM.Components.Panels.BasePanel, {

	'toString': function() {
		return "Clipperz.PM.Components.DataPanel component";
	},

	//-------------------------------------------------------------------------

	'render': function() {
		MochiKit.Signal.disconnectAllTo(this);
		Clipperz.NotificationCenter.unregister(this);
		this.element().update("");

		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'table', border:'0', cellspacing:'0', cellpadding:'0', children:[
			{tag:'tbody', children:[
				{tag:'tr', children:[
					{tag:'td', valign:'top', width:'200', children:[
						{tag:'ul', id:"dataSubMenu", cls:'subMenu', children:[
							{tag:'li', id:'offlineCopyTab', htmlString:Clipperz.PM.Strings['offlineCopyTabLabel']},
							{tag:'li', id:'sharingTab', htmlString:Clipperz.PM.Strings['sharingTabLabel']},
							{tag:'li', id:'importTab', htmlString:Clipperz.PM.Strings['importTabLabel']},
							{tag:'li', id:'printingTab', htmlString:Clipperz.PM.Strings['printingTabLabel']}
						]}
					]},
					{tag:'td', valign:'top', children:[
						{tag:'ul', cls:'clipperzTabPanels', children:[
							{tag:'li', id:this.getId('offlineCopyPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['offlineCopyTabTitle']},
									{tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['offlineCopyTabDescription']},
									{tag:'div', id:this.getId('offlineCopyLinkBox'), children:[
										{tag:'a', id:'offlineCopyLink', href:'#', htmlString:Clipperz.PM.Strings['offlineCopyDownloadLinkLabel']}
									]},
									{tag:'div', id:this.getId('offlineCopyLinkBox_read-only'), children:[
										{tag:'span', cls:'read-only', htmlString:Clipperz.PM.Strings['offlineCopyDownloadLinkLabel']}
									]}
								]}
							]},
							{tag:'li', id:this.getId('sharingPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['sharingTabTitle']},
									{tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['sharingTabDescription']}
								]}
							]},
							{tag:'li', id:this.getId('importPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'div', id:this.getId('importPanelMainComponent')}
								]}
							]},
							{tag:'li', id:this.getId('printingPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['printingTabTitle']},

									{tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['printingTabDescription']},
									{tag:'div', id:this.getId('printingLinkBox'), children:[
										{tag:'a', id:'printingLink', href:'#', htmlString:Clipperz.PM.Strings['printingLinkLabel']}
									]},

									{tag:'hr'},

									{tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['exportTabDescription']},
									{tag:'div', id:this.getId('exportLinkBox'), children:[
										{tag:'a', id:'exportLink', href:'#', htmlString:Clipperz.PM.Strings['exportLinkLabel']}
									]}
								]}
							]}
						]}
					]}
				]}
			]}
		]});

		this.tabPanelController().setUp();

		if (Clipperz.PM.Proxy.defaultProxy.isReadOnly()) {
			this.getElement('offlineCopyLinkBox').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		} else {
			this.getElement('offlineCopyLinkBox_read-only').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
			MochiKit.Signal.connect('offlineCopyLink', 'onclick', this, 'downloadOfflineCopy');
		}

		new Clipperz.PM.Components.Import.MainComponent(this.getElement('importPanelMainComponent'), {user:this.user()});
		
		MochiKit.Signal.connect('printingLink', 'onclick', this, 'printAllData');
		MochiKit.Signal.connect('exportLink', 'onclick', this, 'exportAllData');

		Clipperz.NotificationCenter.register(null, 'switchLanguage', this, 'switchLanguageHandler');
	},
	
	//-------------------------------------------------------------------------

	'tabPanelController': function() {
		if (this._tabPanelController == null) {
			var tabPanelControllerConfig;
			
			tabPanelControllerConfig = {}
			tabPanelControllerConfig['offlineCopyTab'] = this.getId('offlineCopyPanel');
			tabPanelControllerConfig['sharingTab'] = this.getId('sharingPanel');
			tabPanelControllerConfig['importTab'] = this.getId('importPanel');
			tabPanelControllerConfig['printingTab'] = this.getId('printingPanel');
			this._tabPanelController = new Clipperz.PM.Components.TabPanel.TabPanelController({
												name: 'dataTabPanel',
												config:tabPanelControllerConfig,
												selectedTab:'offlineCopyTab'
			});
		}
		
		return this._tabPanelController;
	},
	
	//-------------------------------------------------------------------------

	'downloadOfflineCopy': function(anEvent) {
		var downloadHref;
		
		downloadHref = window.location.href.replace(/\/[^\/]*$/,'') + Clipperz_dumpUrl;

		if (Clipperz_IEisBroken == true) {
			window.open(downloadHref, "");
		} else {
			var	deferredResult;
			var newWindow;

			newWindow = window.open("", "");

			anEvent.preventDefault();

			deferredResult = new MochiKit.Async.Deferred();
			deferredResult.addCallback(MochiKit.Base.method(this.user().connection(), 'message'), 'echo', {'echo':"echo"});
			deferredResult.addCallback(function(aWindow) {
				aWindow.location.href = downloadHref;
			}, newWindow);
			deferredResult.callback();
		}
	},

	//-------------------------------------------------------------------------

	'compareRecords': function(a, b) {
		return MochiKit.Base.compare(a.label().toLowerCase(), b.label().toLowerCase());
	},

	'logo': function() {
		var result;
	
		if (Clipperz_IEisBroken == true) {
			if (Clipperz.PM.Proxy.defaultProxy.isReadOnly()) {
				result = "<span><span class=\"clipperzLogoSpan\">clipper</span><span class=\"clipperzLoggoZSpan\">z</span></span>";
			} else {
				result = "<img src=\"./images/exportLogo.png\" />";
			}
		} else {
			result = "<img src=\"data:image/png;charset=utf-8;base64,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\" />";
		}

		return result;
	},

	'progressWidth': function() {
		return this._progressWidth;
	},

	'updateProgress': function(aProgressComponent, aPercentage) {
		
		
	},
	
	//-------------------------------------------------------------------------

	'printAllData': function(anEvent) {
		var newWindow;
		var allRecords;

//MochiKit.Logging.logDebug(">>> printAllData");
		newWindow = window.open("", "");
		newWindow.document.write(
"<html>" +
"<header>" +
"	<title>Clipperz export data</title>" +
"<style>" +
"#exportedData {" +
"	width: 600px;"	+
"}" +
".ext-mb-progress-wrap {" +
"	border:1px solid #6593cf;" +
"	margin-bottom: 10px;" +
"	width: " + this.progressWidth() + "px;" +
"}" +
".ext-mb-progress {" +
"	height:18px;" +
"	background:transparent url(./images/default/basic-dialog/progress2.gif) repeat-x 1px 1px;" +
"}" +
".ext-mb-progress-bar {" +
"	height:18px;" +
"	overflow:hidden;" +
"	width:0;" +
"	background:#8bb8f3;" +
"}" +
"body {" +
"	font-family: sans-serif;" +
"}" +
"div#logo {" +
"	border-bottom: 1px dotted #aaaaaa;" +
"	margin-bottom: 15px;" +
"}" +
"div.recordBlock h2 {" +
"	font-size: 14pt;" +
"	margin: 0px;" +
"	padding: 0px;" +
"	border: 0px;" +
"	color: #666666;" +
"}" +
"div.recordBlock div.recordNotes p {" +
"	margin: 0px;" +
"	padding: 0px;" +
"	border: 0px;" +
"	color: #aaaaaa;" +
"	font-size: 10pt;" +
"	line-height: 18pt;" +
"	border-left: 1px solid #aaaaaa;" +
"	padding-left: 10px;" +
"}" +
"div.recordBlock dl {" +
"	margin: 0px;" +
"	padding: 0px;" +
"	border: 0px;" +
"	color: #999999;" +
"	padding-bottom: 10px;" +
"	border-bottom: 1px dotted #aaaaaa;" +
"	margin-top: 10px;" +
"	margin-bottom: 15px;" +
"}" +
"div.recordBlock dl dt {" +
"	display: block;" +
"	float: left;" +
"	min-width: 100px;" +
"	white-space: nowrap;" +
"	overflow: hidden;" +
"	margin-right: 10px;" +
"	font-size: 10pt;" +
"	line-height: 18pt;" +
"}" +
"div.recordBlock dl dd {" +
"	color: #666666;" +
"	font-size: 10pt;" +
"	line-height: 18pt;" +
"}" +
"" +
"</style>" +
"" +
"<!--[if IE]>" +
"<style>" +
"div.recordBlock dl dt {" +
"	width: 100px;" +
"}" +
"</style>" +
"<![endif]-->" +
"" +
"</header>" +
"<body>" +
"	<div id=\"logo\">" + this.logo() +
"	<div id=\"progressWrapper\"><div class=\"ext-mb-progress-wrap\"><div class=\"ext-mb-progress\"><div id=\"progress\" class=\"ext-mb-progress-bar\">&#160;</div></div></div></div>" +
"   </div>" +
"</body>" +
"</html>"
		);

		anEvent.preventDefault();

		allRecords = MochiKit.Base.values(this.user().records());
		allRecords.sort(this.compareRecords);
		
/*
		deferredResult = new MochiKit.Async.Deferred();
		MochiKit.Iter.forEach(allRecords, MochiKit.Base.partial(function(aDeferredResult, aWindow, aRecord) {
			var printerRecord;
			
			printerRecord = new Clipperz.PM.Components.Printing.Record({record:aRecord});
			aDeferredResult.addCallback(MochiKit.Base.method(printerRecord, 'deferredDrawToWindow', aWindow));
		}, deferredResult, newWindow));
		deferredResult.callback();

		return deferredResult;
*/		

		MochiKit.DOM.withWindow(newWindow, MochiKit.Base.bind(function(someRecords) {
			var currentWindow;
			var	deferredResult;
			var progressBar;
			var progressWrapper;
			var i, c;

			currentWindow = MochiKit.DOM.currentWindow();
			progressBar = MochiKit.DOM.getElement('progress');
			progressWrapper = MochiKit.DOM.getElement('progressWrapper');
			
			deferredResult = new MochiKit.Async.Deferred();
			c = someRecords.length;
			for (i=0; i<c; i++) {
				deferredResult.addCallback(function(aWindow, aRecord) {
					var printerRecord;
			
					printerRecord = new Clipperz.PM.Components.Printing.Record({record:aRecord});
					return printerRecord.deferredDrawToWindow(aWindow);
				}, currentWindow, someRecords[i])
				deferredResult.addCallback(MochiKit.Base.bind(function(aProgressBar, aProgress) {
					MochiKit.Style.setElementDimensions(aProgressBar, {w:aProgress * this.progressWidth()});
				}, this, progressBar, ((i+1)/c)));
				deferredResult.addCallback(MochiKit.Async.wait, 0.2);
			}

			deferredResult.addCallback(function(aWindow, aProgressWrapper) {
				MochiKit.DOM.replaceChildNodes(aProgressWrapper);
				MochiKit.Style.hideElement(aProgressWrapper);
				aWindow.stop();
			}, currentWindow, progressWrapper);


			deferredResult.callback();
		}, this, allRecords));
	},
	
	//-------------------------------------------------------------------------

	'exportAllData': function(anEvent) {
//		var	deferredResult;
		var newWindow;
		var allRecords;

//MochiKit.Logging.logDebug(">>> printAllData");
		newWindow = window.open("", "");
		newWindow.document.write(
"<html>" +
"<header>" +
"	<title>Clipperz export data</title>" +
"<style>" +
"#exportedData {" +
"	width: 600px;"	+
"}" +
".ext-mb-progress-wrap {" +
"	margin-top:4px;" +
"	margin-bottom: 10px;" +
"	border:1px solid #6593cf;" +
"	width: " + this.progressWidth() + "px;" +
"}" +
".ext-mb-progress {" +
"	height:18px;" +
"	background:transparent url(./images/default/basic-dialog/progress2.gif) repeat-x 1px 1px;" +
"}" +
".ext-mb-progress-bar {" +
"	height:18px;" +
"	overflow:hidden;" +
"	width:0;" +
"	background:#8bb8f3;" +
"}" +
"</style>" +
"" +
"<!--[if IE]>" +
"<style>" +
"</style>" +
"<![endif]-->" +
"" +
"</header>" +
"<body>" +
"	<div id=\"logo\">" + this.logo() + "</div>" +
"	<div id=\"progressWrapper\">" +
"		<div class=\"description\">" + Clipperz.PM.Strings['exportDataInProgressDescription'] + "</div>" +
"		<div class=\"ext-mb-progress-wrap\"><div class=\"ext-mb-progress\"><div id=\"progress\" class=\"ext-mb-progress-bar\">&#160;</div></div></div>" +
"	</div>" +
"	<div id=\"textareaWrapper\">" +
"		<div class=\"description\">" + Clipperz.PM.Strings['exportDataDescription'] + "</div>" +
"		<textarea id=\"exportedData\" cols=\"80\" rows=\"20\">[</textarea>" +
"	</div>" +
"</body>" +
"</html>"
		);

		anEvent.preventDefault();

		allRecords = MochiKit.Base.values(this.user().records());
		allRecords.sort(this.compareRecords);

		MochiKit.DOM.withWindow(newWindow, MochiKit.Base.bind(function(someRecords) {
			var currentWindow;
			var	deferredResult;
			var textareaWrapper;
			var	textarea;
			var progressBar;
			var progressWrapper;
			var i, c;

			currentWindow = MochiKit.DOM.currentWindow();
			textarea = MochiKit.DOM.getElement('exportedData');
			textareaWrapper = MochiKit.DOM.getElement('textareaWrapper');
			MochiKit.Style.hideElement(textareaWrapper);
			progressBar = MochiKit.DOM.getElement('progress');
			progressWrapper = MochiKit.DOM.getElement('progressWrapper');
			
			deferredResult = new MochiKit.Async.Deferred();

//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("exportAllData - 1: " + res); return res;});
			c = someRecords.length;
			for (i=0; i<c; i++) {
				deferredResult.addCallback(MochiKit.Base.method(someRecords[i], 'deferredData'));
				deferredResult.addCallback(MochiKit.Base.method(someRecords[i], 'exportedData'));
				deferredResult.addCallback(MochiKit.Base.bind(function(aTextarea, aProgressBar, aProgress, someRecordExportedData) {
					aTextarea.value = aTextarea.value + "\n" + someRecordExportedData + ",";
					MochiKit.Style.setElementDimensions(aProgressBar, {w:aProgress * this.progressWidth()});
				}, this, textarea, progressBar, ((i+1)/c)));
				deferredResult.addCallback(MochiKit.Async.wait, 0.2);
			}
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("exportAllData - 2: " + res); return res;});
			deferredResult.addCallback(function(aTextareaWrapper, aTextarea, aProgressWrapper) {
				aTextarea.value = aTextarea.value.slice(0, -1) + "\n]";
//				MochiKit.DOM.replaceChildNodes(aProgressWrapper);
				MochiKit.Style.hideElement(aProgressWrapper);
				MochiKit.Style.showElement(aTextareaWrapper);
				aTextarea.focus();
				aTextarea.select();
			}, textareaWrapper, textarea, progressWrapper);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("exportAllData - 3: " + res); return res;});
			deferredResult.addBoth(function(aWindow) {
				aWindow.stop();
			}, currentWindow);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("exportAllData - 4: " + res); return res;});
			
			deferredResult.callback();
		}, this, allRecords));
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
	
});
	
