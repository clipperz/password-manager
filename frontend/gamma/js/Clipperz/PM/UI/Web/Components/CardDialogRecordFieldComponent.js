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

Clipperz.Base.module('Clipperz.PM.UI.Web.Components');

Clipperz.PM.UI.Web.Components.CardDialogRecordFieldComponent = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.CardDialogRecordFieldComponent.superclass.constructor.apply(this, arguments);

	this._reference = args.reference	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._actionType = null;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.CardDialogRecordFieldComponent, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.CardDialogRecordFieldComponent component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function() {
		this.append(this.element(), [
			{tag:'td', cls:'fieldState'},
			{tag:'td', cls:'fieldLabel', children:[
				{tag:'input', cls:'label', id:this.getId('label')}
			]},
			{tag:'td', cls:'fieldLock', children:[
				{tag:'div', cls:'unlocked', id:this.getId('isHidden')}
			]},
			{tag:'td', cls:'fieldValue', children:[
				{tag:'div', cls:'unlocked', id:this.getId('valueWrapper'), children:[
					{tag:'input', type:'text', cls:'value', id:this.getId('value')}
				]}
			]},
			{tag:'td', cls:'fieldAction', children:[
				{tag:'a', href:'#', id:this.getId('actionLink'), html:'&nbsp;'}
			]},
			{tag:'td', cls:'fieldAddDelete', children:[
				{tag:'div', cls:'delete', children:[
					{tag:'span', children:[
						{tag:'a', href:'#', id:this.getId('delete'), html:"delete"}
					]}
				]}
			]}
		]);

		MochiKit.Signal.connect(this.getId('label'),		'onkeyup',	MochiKit.Base.partial(MochiKit.Signal.signal, this, 'changedValue'));
		MochiKit.Signal.connect(this.getId('isHidden'),		'onclick',	this, 'toggleIsHidden');
		MochiKit.Signal.connect(this.getId('value'),		'onkeyup',	MochiKit.Base.partial(MochiKit.Signal.signal, this, 'changedValue'));
		MochiKit.Signal.connect(this.getId('actionLink'),	'onclick',	this, 'handleActionLink');
		MochiKit.Signal.connect(this.getId('delete'),		'onclick',	this, 'deleteField');
//		MochiKit.Signal.connect(this.getId('delete'),		'onclick',	MochiKit.Base.partial(MochiKit.Signal.signal, this, 'deleteField', this.reference()));
	},

	//-------------------------------------------------------------------------

	'shouldShowElementWhileRendering': function () {
		return false;
	},

	//=========================================================================

	'reference': function () {
		return this._reference;
	},

	//=========================================================================

	'label': function () {
		return this.getElement('label').value;
	},

	'setLabel': function (aValue) {
//		this.getElement('label').value = Clipperz.Base.sanitizeString(aValue);
		this.getElement('label').value = aValue;
	},

	//=========================================================================

	'value': function () {
		return this.getElement('value').value;
	},

	'setValue': function (aValue) {
//		this.getElement('value').value = Clipperz.Base.sanitizeString(aValue);
		this.getElement('value').value = aValue;
	},

	//-------------------------------------------------------------------------

	'actionType': function () {
		return this._actionType;
	},

	'setActionType': function (anActionType) {
		this._actionType = anActionType;
		
		switch (this._actionType) {
			case 'NONE':
				MochiKit.Style.hideElement(this.getId('actionLink'));
				MochiKit.DOM.setElementClass(this.getId('actionLink'), '');
				break;
			case 'URL':
				MochiKit.Style.showElement(this.getId('actionLink'));
				MochiKit.DOM.setElementClass(this.getId('actionLink'), 'url');
				break;
			case 'BITCOIN':
				MochiKit.Style.showElement(this.getId('actionLink'));
				MochiKit.DOM.setElementClass(this.getId('actionLink'), 'url');
				break;
			case 'EMAIL':
				MochiKit.Style.showElement(this.getId('actionLink'));
				MochiKit.DOM.setElementClass(this.getId('actionLink'), 'email');
				break;
			case 'PASSWORD':
				MochiKit.Style.showElement(this.getId('actionLink'));
				MochiKit.DOM.setElementClass(this.getId('actionLink'), 'password');
				break;
		}
	},

	//=========================================================================

	'isHidden': function () {
//		return this.getElement('value').value;
		return MochiKit.DOM.hasElementClass(this.getElement('isHidden'), 'locked');
	},

	'setIsHidden': function (aValue) {
//		this.getElement('value').value = Clipperz.Base.sanitizeString(aValue);
		MochiKit.DOM.setElementClass(this.getElement('isHidden'), (aValue ? 'locked': 'unlocked'));
		MochiKit.DOM.setElementClass(this.getElement('valueWrapper'), (aValue ? 'locked': 'unlocked'));
	},

	'toggleIsHidden': function (anEvent) {
		anEvent.preventDefault();
		
		this.setIsHidden(! this.isHidden());
		MochiKit.Signal.signal(this, 'changedValue');
	},

	//=========================================================================

	'handleActionLink': function (anEvent) {
		anEvent.preventDefault();
		MochiKit.Signal.signal(this, 'performAction', this.reference(), anEvent.target());
	},

	//=========================================================================

	'deleteField': function (anEvent) {
		anEvent.preventDefault();
		
		MochiKit.Signal.signal(this, 'deleteField', this.reference());
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});
