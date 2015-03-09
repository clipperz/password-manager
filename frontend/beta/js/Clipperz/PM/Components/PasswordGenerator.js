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

Clipperz.PM.Components.PasswordGenerator = function(anElement, aFieldValueComponent, args) {
	args = args || {};

//MochiKit.Logging.logDebug(">>> new TextFormField");
	Clipperz.PM.Components.PasswordGenerator.superclass.constructor.call(this, anElement, args);

	this._fieldValueComponent = aFieldValueComponent;
	this._panelButton = null;
	this.render();
//MochiKit.Logging.logDebug("<<< new TextFormField");
	
	return this;
};

YAHOO.extendX(Clipperz.PM.Components.PasswordGenerator, Clipperz.PM.Components.BaseComponent, {
	
	'toString': function() {
		return "Clipperz.PM.Components.PasswordGenerator";
	},

	//-----------------------------------------------------

	'fieldValueComponent': function() {
		return this._fieldValueComponent;
	},
	
	//-----------------------------------------------------

	'render': function() {
		MochiKit.Signal.disconnectAllTo(this);
		
//		this._panelButton = new YAHOO.ext.Button(this.element().dom, {text:Clipperz.PM.Strings['passwordGeneratorButtonLabel'], handler:this.openPasswordPanel, scope:this});
		MochiKit.Signal.connect(this.element().dom, 'onmouseenter', this, 'onMouseEnter');
		MochiKit.Signal.connect(this.element().dom, 'onmouseleave', this, 'onMouseLeave');
		MochiKit.Signal.connect(this.element().dom, 'onclick', this, 'openPasswordPanel');
	},

	//-----------------------------------------------------

	'onMouseEnter': function() {
		this.element().addClass('hover');
	},
	
	'onMouseLeave': function() {
		this.element().removeClass('hover');
	},

	//-----------------------------------------------------

	'panelButton': function() {
		return this._panelButton;
	},

	//-----------------------------------------------------
	
	'openPasswordPanel': function() {
		var passwordGeneratorElement;
		var passwordGeneratorDialog;
		var cancelButton;
		var okButton;
		var cancelFunction;
		var okFunction;
		
//MochiKit.Logging.logDebug(">>> PasswordGenerator.openPasswordPanel");
		passwordGeneratorElement = Clipperz.YUI.DomHelper.append(document.body, {tag:'div', id:'passwordGenerator', children:[
			{tag:'div', cls:'ydlg-hd', htmlString:Clipperz.PM.Strings['passwordGeneratorPanelTitle']},
			{tag:'div', cls:'ydlg-bd', children:[
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
				]}
			]},
			{tag:'div', cls:'ydlg-ft'}
		]}, true);

		new Clipperz.PM.Components.PasswordEntropyDisplay(this.getElement('passwordField'));

		MochiKit.Signal.connect(this.getId('lowercase'), 'onclick', this, 'updatePasswordValue');
		MochiKit.Signal.connect(this.getId('uppercase'), 'onclick', this, 'updatePasswordValue');
		MochiKit.Signal.connect(this.getId('numbers'),   'onclick', this, 'updatePasswordValue');
		MochiKit.Signal.connect(this.getId('symbols'),   'onclick', this, 'updatePasswordValue');

		MochiKit.Signal.connect(this.getDom('passwordField'), 'onkeyup',  this, 'updatePasswordLengthLabel');
		MochiKit.Signal.connect(this.getDom('passwordField'), 'onchange', this, 'updatePasswordLengthLabel');
		MochiKit.Signal.connect(this.getDom('passwordField'), 'onblur',   this, 'updatePasswordLengthLabel');

		this.updatePasswordValue();
		
		passwordGeneratorDialog = new YAHOO.ext.BasicDialog(
			passwordGeneratorElement, { 
				autoCreate:false,
				closable:false,
				modal:true,
				autoTabs:false,
				resizable:false,
				fixedcenter:true,
				constraintoviewport:false,
				width:320,
				height:130,
				shadow:true,
				minWidth:200,
				minHeight:100
			}
		);

		cancelFunction = MochiKit.Base.partial(MochiKit.Base.bind(this.cancelPasswordPanel, this), passwordGeneratorDialog);
		passwordGeneratorDialog.addKeyListener(27, cancelFunction);
		cancelButton = passwordGeneratorDialog.addButton(Clipperz.PM.Strings['passwordGeneratorPanelCancelLabel'], cancelFunction, this);

		okFunction = MochiKit.Base.partial(MochiKit.Base.bind(this.okPasswordPanel, this), passwordGeneratorDialog);
		passwordGeneratorDialog.addKeyListener([10, 13], okFunction);
		okButton = passwordGeneratorDialog.addButton(Clipperz.PM.Strings['passwordGeneratorPanelOkLabel'], okFunction, this);

		MochiKit.Signal.connect(this.getId('passwordGeneratorForm'), 'onsubmit', okFunction);

		passwordGeneratorDialog.setDefaultButton(okButton);

		this.fieldValueComponent().mainComponent().mainPanel().exitModalView();
		this.fieldValueComponent().mainComponent().scrollToTop();

//		passwordGeneratorDialog.show(this.panelButton().getEl());
		passwordGeneratorDialog.show(this.element());
		this.onMouseLeave();
	},
	
	//-----------------------------------------------------

	'cancelPasswordPanel': function(aPasswordGeneratorPanel) {
		this.fieldValueComponent().mainComponent().mainPanel().enterModalView();
		aPasswordGeneratorPanel.hide(MochiKit.Base.bind(function() {
			aPasswordGeneratorPanel.destroy(true);
			MochiKit.Signal.disconnectAllTo(this);
		}, this));
	},

	//-----------------------------------------------------

	'updatePasswordLengthLabel': function() {
		this.getElement('passwordLength').update(this.getDom('passwordField').value.length);
	},
	
	//-----------------------------------------------------

	'okPasswordPanel': function(aPasswordGeneratorPanel, anEvent) {
//MochiKit.Logging.logDebug(">>> PasswordGenerator.okPasswordPanel");

		if (anEvent.stop) {
			anEvent.stop();
		}
		
		this.fieldValueComponent().inputElement().dom.focus();
		this.fieldValueComponent().inputElement().dom.value = this.getElement('passwordField').dom.value;
		this.getElement('passwordField').dom.focus();
		this.cancelPasswordPanel(aPasswordGeneratorPanel);
		
		return false;
	},

	//-----------------------------------------------------

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

		
		if (anEvent) {
			anEvent.src().focus();
		} else {
			this.element().focus();
		}
		
		return false;
//MochiKit.Logging.logDebug("<<< updatePasswordValue");
	},
	
	//-----------------------------------------------------
	__syntaxFix__: '__syntaxFix__'
});
