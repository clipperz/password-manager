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

Clipperz.PM.Components.Panels.ToolsPanel = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.Panels.ToolsPanel.superclass.constructor.call(this, anElement, args);

	this._generateButtonElement = null;
	this._needsRenderingUponTabSwitch = false;
	
	this.render();
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.Panels.ToolsPanel, Clipperz.PM.Components.Panels.BasePanel, {

	'toString': function() {
		return "Clipperz.PM.Components.Panels.ToolsPanel component";
	},

	//-------------------------------------------------------------------------

	'render': function() {
		var bookmarkletUrl;

//MochiKit.Logging.logDebug(">>> ToolsPanel.render");
		Clipperz.NotificationCenter.unregister(this);
		MochiKit.Signal.disconnectAllTo(this);
		
		if (Clipperz_IEisBroken == true) {
			bookmarkletUrl = bookmarklet_ie;
		} else {
			bookmarkletUrl = bookmarklet;
		}
		
		this.element().update("");
		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'table', border:'0', cellspacing:'0', cellpadding:'0', children:[
			{tag:'tbody', children:[
				{tag:'tr', children:[
					{tag:'td', valign:'top', width:'200', children:[
						{tag:'ul', id:"dataSubMenu", cls:'subMenu', children:[
							{tag:'li', id:this.getId('passwordGenerator'), htmlString:Clipperz.PM.Strings['passwordGeneratorTabLabel']},
							{tag:'li', id:this.getId('bookmarklet'), htmlString:Clipperz.PM.Strings['bookmarkletTabLabel']},
							{tag:'li', id:this.getId('compact'), htmlString:Clipperz.PM.Strings['compactTabLabel']},
							{tag:'li', id:this.getId('httpAuth'), htmlString:Clipperz.PM.Strings['httpAuthTabLabel']}
						]}
					]},
					{tag:'td', valign:'top', children:[
						{tag:'ul', cls:'clipperzTabPanels', children:[
							{tag:'li', id:this.getId('passwordGeneratorPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['passwordGeneratorTabTitle']},
									{tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['paswordGeneratorTabDescription']},
									
									//---------------------------------------------------
									{tag:'div', children:[
										{tag:'form', id:this.getId('passwordGeneratorForm'), cls:'passwordGenerator', children:[
											{tag:'input', type:'text', cls:'clipperz_passwordGenerator_password', id:this.getId('passwordField')},
											{tag:'table', children:[
												{tag:'tbody', children:[
													{tag:'tr', children:[
														{tag:'td', width:'20%', children:[
															{tag:'input', type:'checkbox', name:'lowercase', id:this.getId('lowercase'), checked:true},
															{tag:'span', htmlString:Clipperz.PM.Strings['passwordGeneratorLowercaseLabel']}
														]},
														{tag:'td', width:'20%', children:[
															{tag:'input', type:'checkbox', name:'uppercase', id:this.getId('uppercase'), checked:true},
															{tag:'span', htmlString:Clipperz.PM.Strings['passwordGeneratorUppercaseLabel']}
														]},
														{tag:'td', width:'20%', children:[
															{tag:'input', type:'checkbox', name:'numbers', id:this.getId('numbers'), checked:true},
															{tag:'span', htmlString:Clipperz.PM.Strings['passwordGeneratorNumberLabel']}
														]},
														{tag:'td', width:'20%', children:[
															{tag:'input', type:'checkbox', name:'symbols', id:this.getId('symbols'), checked:true},
															{tag:'span', htmlString:Clipperz.PM.Strings['passwordGeneratorSymbolLabel']}
														]},
														{tag:'td', width:'20%', children:[
															{tag:'span', cls:'passwordGeneratorLength', children:[
																{tag:'span', htmlString:Clipperz.PM.Strings['passwordGeneratorLengthLabel']},
																{tag:'span', id:this.getId('passwordLength'), cls:'passwordGeneratorLengthValue', html:'0'}
															]}
														]}
													]}
												]}
											]}
										]},
										{tag:'div', id:this.getId('generateRandomPassword')}
									]}
									//---------------------------------------------------
									
								]}
							]},
							{tag:'li', id:this.getId('bookmarkletPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['bookmarkletTabTitle']},
									{tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['bookmarkletTabDescription']},
									{tag:'a', href:bookmarkletUrl, cls:'bookmarkletLink', htmlString:Clipperz.PM.Strings['bookmarkletTabBookmarkletTitle']},
									{tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['bookmarkletTabInstructions']}
								]}
							]},
							{tag:'li', id:this.getId('compactPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['compactTabTitle']},
									{tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['compactTabDescription']}
								]}
							]},
							{tag:'li', id:this.getId('httpAuthPanel'), children:[
								{tag:'div', cls:'clipperzSubPanel', children:[
									{tag:'h5', htmlString:Clipperz.PM.Strings['httpAuthTabTitle']},
									{tag:'div', cls:'panelDescription', htmlString:Clipperz.PM.Strings['httpAuthTabDescription']},
									{tag:'div', cls:'bookmarkletConfiguration', children:[Clipperz.PM.Strings['httpAuthBookmarkletConfiguration']]}
								]}
							]}
						]}
					]}
				]}
			]}
		]});

		new Clipperz.PM.Components.PasswordEntropyDisplay(this.getElement('passwordField'));

		MochiKit.Signal.connect(this.getId('lowercase'), 'onclick', this, 'updatePasswordValue');
		MochiKit.Signal.connect(this.getId('uppercase'), 'onclick', this, 'updatePasswordValue');
		MochiKit.Signal.connect(this.getId('numbers'),   'onclick', this, 'updatePasswordValue');
		MochiKit.Signal.connect(this.getId('symbols'),   'onclick', this, 'updatePasswordValue');

		MochiKit.Signal.connect(this.getDom('passwordField'), 'onkeyup',  this, 'updatePasswordLengthLabel');
		MochiKit.Signal.connect(this.getDom('passwordField'), 'onchange', this, 'updatePasswordLengthLabel');
		MochiKit.Signal.connect(this.getDom('passwordField'), 'onblur',   this, 'updatePasswordLengthLabel');

		this.setGenerateButtonElement(new YAHOO.ext.Button(this.getDom('generateRandomPassword'), {text:Clipperz.PM.Strings['passwordGeneratorTabButtonLabel'], handler:this.updatePasswordValue, scope:this}));

		this.setNeedsRenderingUponTabSwitch(false);
		this.tabPanelController().setUp();
		Clipperz.NotificationCenter.register(null, 'tabSelected', this, 'tabSelectedHandler');
		Clipperz.NotificationCenter.register(null, 'switchLanguage', this, 'switchLanguageHandler');
//MochiKit.Logging.logDebug("<<< ToolsPanel.render");
	},

	//-------------------------------------------------------------------------

	'needsRenderingUponTabSwitch': function() {
		return this._needsRenderingUponTabSwitch;
	},
	
	'setNeedsRenderingUponTabSwitch': function(aValue) {
		this._needsRenderingUponTabSwitch = aValue;
	},
	
	'tabSelectedHandler': function(anEvent) {
		if (this.needsRenderingUponTabSwitch()) {
			this.render();
		}

		if (anEvent.parameters() == this.getId('httpAuth')) {
			var textarea;

			textarea = document.getElementById("httpAuthDefaultConfiguration");
			textarea.focus();
			textarea.select();
		}
	},
	
	//-------------------------------------------------------------------------

	'tabPanelController': function() {
		if (this._tabPanelController == null) {
			var tabPanelControllerConfig;
			
			tabPanelControllerConfig = {}
			tabPanelControllerConfig[this.getId('passwordGenerator')] = this.getId('passwordGeneratorPanel');
			tabPanelControllerConfig[this.getId('bookmarklet')] = this.getId('bookmarkletPanel');
			tabPanelControllerConfig[this.getId('compact')] = this.getId('compactPanel');
			tabPanelControllerConfig[this.getId('httpAuth')] = this.getId('httpAuthPanel');
			this._tabPanelController = new Clipperz.PM.Components.TabPanel.TabPanelController({ config:tabPanelControllerConfig, selectedTab:this.getId('passwordGenerator') });
		}
		
		return this._tabPanelController;
	},

	//-------------------------------------------------------------------------

	'generateButtonElement': function() {
		return this._generateButtonElement;
	},
	
	'setGenerateButtonElement': function(aValue) {
		this._generateButtonElement = aValue;
	},
	
	//-------------------------------------------------------------------------

	'updatePasswordValue': function(anEvent) {
		var	randomBytes;
		var	randomValue;
		var charset;
		var charsetBitSize;
		var stringValue;
		var	blockIndex;

//MochiKit.Logging.logDebug(">>> updatePasswordValue");
		randomBytes = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(50);
		stringValue = "";
		blockIndex = 0;

		charset = "";
		if (this.getDom('lowercase').checked) {
			charset += Clipperz.PM.Strings['passwordGeneratorLowercaseCharset'];
		}
		if (this.getDom('uppercase').checked) {
			charset += Clipperz.PM.Strings['passwordGeneratorUppercaseCharset'];
		}
		if (this.getDom('numbers').checked) {
			charset += Clipperz.PM.Strings['passwordGeneratorNumberCharset'];
		}
		if (this.getDom('symbols').checked) {
			charset += Clipperz.PM.Strings['passwordGeneratorSymbolCharset'];
		}

		charsetBitSize = 0;
		while (Math.pow(2, charsetBitSize) < charset.length) {
			charsetBitSize ++;
		}

		if (charsetBitSize > 0) {
			while (Clipperz.PM.Crypto.passwordEntropy(stringValue) < 128) {
				if (((blockIndex + 1)*charsetBitSize) > (randomBytes.length() * 8)) {
					randomBytes = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(50);
					blockIndex = 0;
				}
				randomValue = randomBytes.bitBlockAtIndexWithSize(blockIndex*charsetBitSize, charsetBitSize);
				if (randomValue < charset.length) {
					stringValue += charset.charAt(randomValue);
				}
			
				blockIndex ++;
			}
		} else {
			stringValue = "";
		}
		
		this.getElement('passwordField').dom.focus()
		this.getElement('passwordField').dom.value = stringValue;

		
		if (anEvent.src) {
			anEvent.src().focus();
		} else {
			this.generateButtonElement().el.focus();
		}
		
		this.setNeedsRenderingUponTabSwitch(true);
		
		return false;
//MochiKit.Logging.logDebug("<<< updatePasswordValue");
	},
	
	//-----------------------------------------------------

	'updatePasswordLengthLabel': function() {
		this.getElement('passwordLength').update(this.getDom('passwordField').value.length);
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});
	
