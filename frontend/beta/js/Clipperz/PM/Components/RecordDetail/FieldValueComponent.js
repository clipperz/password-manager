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
if (typeof(Clipperz.PM.Components.RecordDetail) == 'undefined') { Clipperz.PM.Components.RecordDetail = {}; }

//#############################################################################

Clipperz.PM.Components.RecordDetail.FieldValueComponent = function(anElement, args) {
	args = args || {};

    Clipperz.PM.Components.RecordDetail.FieldValueComponent.superclass.constructor.call(this, anElement, args);

	this._inputElement = null;
	this._scrambledStatus = 'SCRAMBLED'; //	'UNSCRAMBLED'
	
	this.render();
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.RecordDetail.FieldValueComponent, Clipperz.PM.Components.RecordDetail.AbstractFieldSubComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.RecordDetail.FieldValueComponent component";
	},

	//-------------------------------------------------------------------------

	'value': function() {
		return this.recordField().value();
	},

	'setValue': function(aValue) {
		this.recordField().setValue(aValue);
	},
	
	//-------------------------------------------------------------------------

	'inputElement': function() {
		return this._inputElement;
	},
	
	'setInputElement': function(aValue) {
		this._inputElement = aValue;
	},

	//-------------------------------------------------------------------------

	'scrambledStatus': function() {
		return this._scrambledStatus;
	},
	
	'setScrambledStatus': function(aValue) {
		this._scrambledStatus = aValue;
	},
	
	//-------------------------------------------------------------------------

	'handleTypeChange': function() {
//MochiKit.Logging.logDebug(">>> handling type change - " + this.recordField().type());
		this.synchronizeComponentValues();
		this.update();
	},
	
	//-------------------------------------------------------------------------

	'addrUrl': function() {
		var result;
		
		result = "http://maps.google.com/maps?q=" + this.value().split(' ').join('+');

		return result;
	},

	//-------------------------------------------------------------------------

	'updateViewMode': function() {
		var scarmbledStatus;
		
		scrambledStatus = this.scrambledStatus() || 'SCRAMBLED';

		this.element().update("");
		if (this.recordField().hidden() == false) {
			switch(this.recordField().type()) {
				case 'TXT':
				case 'PWD':
					Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'span', html:this.value()});
					break;
				case 'URL':
					var	urlLocation;
					
					urlLocation = Clipperz.Base.sanitizeString(this.value());
					if (! (/^(https?|ftp|svn):\/\//.test(urlLocation))) {
						urlLocation = 'http://' + urlLocation;
					}
					Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'a', href:urlLocation, html:this.value(), target:'_blank'});
					break;
				case 'DATE':
					Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'span', html:this.value()});
					break;
				case 'ADDR':
					Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'a', href:this.addrUrl(), html:this.value(), target:'_blank'});
					break;
			}
		} else {
			var tableElement;
			var tdElement;
			var inputElement;
			var passwordElementConfiguration;

			if (scrambledStatus == 'SCRAMBLED') {
				var	scrambledInputElement;

				if ((Clipperz_IEisBroken === true) && (Clipperz.PM.Proxy.defaultProxy.isReadOnly())) {
					scrambledInputElement = {tag:'input', type:'password', value:"this.value()"};
				} else {
					scrambledInputElement = {tag:'input', type:'text', cls:'scrambledField', title:Clipperz.PM.Strings['recordDetailPasswordFieldTooltipLabel'], value:"this.value()"};
				}

				passwordElementConfiguration = 
					{tag:'table', border:'0', cellspacing:'2', cellpadding:'0', children:[
						{tag:'tbody', children:[
							{tag:'tr', children:[
								{tag:'td', valign:'top', children:[
									scrambledInputElement,
									{tag:'a', cls:'scrambleLink', id:this.getId('scrambleLink'), href:'#', htmlString:Clipperz.PM.Strings['recordDetailPasswordFieldUnscrambleLabel']}
								]},
								{tag:'td', valign:'top', children:[
									{tag:'span', cls:'scrambledFieldLabel', htmlString:Clipperz.PM.Strings['recordDetailPasswordFieldHelpLabel']}
								]}
							]}
						]}
					]};
			} else {
				passwordElementConfiguration =
					{tag:'div', children:[
						{tag:'input', type:'text', cls:'unscrambledField', value:"this.value()"},
						{tag:'a', cls:'scrambleLink', id:this.getId('scrambleLink'), href:'#', htmlString:Clipperz.PM.Strings['recordDetailPasswordFieldScrambleLabel']}
					]};
			}
			
			tableElement = Clipperz.YUI.DomHelper.append(this.element().dom, passwordElementConfiguration, true);

			inputElement = tableElement.getChildrenByTagName('input')[0];
			inputElement.dom.value = this.value();
			inputElement.wrap({tag:'div', cls:'passwordBackground'}).setStyle('background-position', "0px -" + Math.min(128, Clipperz.PM.Crypto.passwordEntropy(this.value())) + "px");

			MochiKit.Signal.connect(inputElement.dom, 'onfocus', this, 'selectHiddenFieldOnFocus');
			MochiKit.Signal.connect(this.getDom('scrambleLink'), 'onclick', this, 'toggleScramble');
		}
	},

	//-------------------------------------------------------------------------
	
	'updateEditMode': function() {
		var inputElement;
		var scarmbledStatus;
		
		scrambledStatus = this.scrambledStatus() || 'SCRAMBLED';
		
		this.element().update("");
		switch(this.recordField().type()) {
			case 'TXT':
			case 'URL':
			case 'ADDR':
				inputElement = Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'input', type:'text', value:"this.value()"}, true);
				inputElement.dom.value = this.value();
				break;
			case 'PWD':
				Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'table', width:'100%', cellpadding:'0', cellspacing:'0', children:[
					{tag:'tbody', children:[
						{tag:'tr', children:[
							{tag:'td', valign:'top', children:[
								{tag:'input', type:((scrambledStatus == 'SCRAMBLED') ? 'password' : 'text'), id:this.getId('passwordInputElement'), value:"this.value()"},
								{tag:'a', cls:'scrambleLink', id:this.getId('scrambleLink'), href:'#', html:(scrambledStatus == 'SCRAMBLED' ? Clipperz.PM.Strings['recordDetailPasswordFieldUnscrambleLabel'] : Clipperz.PM.Strings['recordDetailPasswordFieldScrambleLabel'])}
							]},
							{tag:'td', valign:'top', children:[
								{tag:'div', id:this.getId('passwordGenerator'), cls:'Clipperz_PasswordGenerator_button', html:'&nbsp;'}
							]}
						]}
					]}
				]})
				inputElement = this.getElement('passwordInputElement');
				inputElement.dom.value = this.value();
				new Clipperz.PM.Components.PasswordEntropyDisplay(this.getElement('passwordInputElement'));
				new Clipperz.PM.Components.PasswordGenerator(this.getElement('passwordGenerator'), this);
				MochiKit.Signal.connect(this.getDom('scrambleLink'), 'onclick', this, 'toggleScramble');
				break;
//			case 'NOTE':
//				inputElement = Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'textarea', rows:'5', html:this.value()}, true);
//				break
			case 'DATE':
				inputElement = Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'input', type:'text', value:"this.value()"}, true);
				inputElement.dom.value = this.value();
				break;
		}

		this.setInputElement(inputElement);
	},

	//-------------------------------------------------------------------------

	'synchronizeComponentValues': function() {
//MochiKit.Logging.logDebug(">>> FieldValueComponent.synchronizeComponentValues");
		if (this.inputElement() != null) {
			var value;
			
			switch(this.recordField().type()) {
				case 'TXT':
				case 'URL':
				case 'ADDR':
				case 'PWD':
				case 'DATE':
					value = this.inputElement().dom.value;
					break;
			}
			this.setValue(value);
		}
//MochiKit.Logging.logDebug("<<< FieldValueComponent.synchronizeComponentValues");
	},

	//-------------------------------------------------------------------------

	'selectHiddenFieldOnFocus': function(anEvent) {
		anEvent.src().select();
	},

	//-------------------------------------------------------------------------

	'toggleScramble': function(anEvent) {
		this.synchronizeComponentValues();
		
		if (this.scrambledStatus() == 'SCRAMBLED') {
			this.setScrambledStatus('UNSCRAMBLED');
		} else {
			this.setScrambledStatus('SCRAMBLED');
		};
		
		this.update();
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

