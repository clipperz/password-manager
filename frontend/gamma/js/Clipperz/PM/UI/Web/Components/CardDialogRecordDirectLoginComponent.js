/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz's Javascript Crypto Library.
Javascript Crypto Library provides web developers with an extensive
and efficient set of cryptographic functions. The library aims to
obtain maximum execution speed while preserving modularity and
reusability.
For further information about its features and functionalities please
refer to http://www.clipperz.com

* Javascript Crypto Library is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Javascript Crypto Library is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Javascript Crypto Library.  If not, see
  <http://www.gnu.org/licenses/>.

*/

Clipperz.Base.module('Clipperz.PM.UI.Web.Components');

Clipperz.PM.UI.Web.Components.CardDialogRecordDirectLoginComponent = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.CardDialogRecordDirectLoginComponent.superclass.constructor.apply(this, arguments);

	this._reference = args.reference	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._faviconComponent = null;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.CardDialogRecordDirectLoginComponent, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.CardDialogRecordDirectLoginComponent component";
	},

	//-------------------------------------------------------------------------

	'reference': function () {
		return this._reference;
	},

	//-------------------------------------------------------------------------

	'renderSelf': function() {
//console.log(">>> CardDialogRecordDirectLoginComponent.renderSelf");
		this.append(this.element(), [
			{tag:'div', cls:'cardDialogRecordDirectLoginComponent_favicon', children:[
				{tag:'img', cls:'favicon', id:this.getId('favicon')}
			]},
			{tag:'div', cls:'cardDialogRecordDirectLoginComponent_label', children:[
				{tag:'input', id:this.getId('label'), type:'text'}
			]},
			{tag:'div', cls:'open', children:[
				{tag:'span', children:[
					{tag:'a', href:'open', id:this.getId('open'), html:'&nbsp;'}
				]}
			]},
			{tag:'div', cls:'edit', children:[
				{tag:'span', children:[
					{tag:'a', href:'edit', id:this.getId('edit'), html:"edit"}
				]}
			]},
			{tag:'div', cls:'delete', children:[
				{tag:'span', children:[
					{tag:'a', href:'delete', id:this.getId('delete'), html:"delete"}
				]}
			]}
/*
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
			{tag:'td', cls:'fieldAddDelete', children:[
				{tag:'div', cls:'delete', children:[
					{tag:'span', children:[
						{tag:'a', href:'#', id:this.getId('delete'), html:"delete"}
					]}
				]}
			]}
*/
		]);

		MochiKit.Signal.connect(this.getId('label'),	'onkeyup',	MochiKit.Base.partial(MochiKit.Signal.signal, this, 'changedValue'));
		MochiKit.Signal.connect(this.getId('open'),		'onclick',	this, 'openDirectLogin');
		MochiKit.Signal.connect(this.getId('edit'),		'onclick',	this, 'editDirectLogin');
		MochiKit.Signal.connect(this.getId('delete'),	'onclick',	this, 'deleteDirectLogin');
	},

	//-------------------------------------------------------------------------

	'shouldShowElementWhileRendering': function () {
		return false;
	},

	//-------------------------------------------------------------------------
	
	'faviconComponent': function () {
		if (this._faviconComponent == null) {
//console.log("created the FAVICON component");
			this._faviconComponent = new Clipperz.PM.UI.Common.Components.FaviconComponent({element:this.getId('favicon')});
		}
		
		return this._faviconComponent;
	},

	//=========================================================================

	'label': function () {
		return this.getElement('label').value;
	},

	'setLabel': function (aValue) {
		this.getElement('label').value = Clipperz.Base.sanitizeString(aValue);
	},

	//-------------------------------------------------------------------------

	'favicon': function () {
//		return this.getElement('favicon').src;
		return this.faviconComponent().src();
	},

	'setFavicon': function (aValue) {
//		this.getElement('favicon').src = Clipperz.Base.sanitizeString(aValue);
		this.faviconComponent().setSrc(Clipperz.Base.sanitizeString(aValue));
	},

	//=========================================================================

	'openDirectLogin': function (anEvent) {
		anEvent.preventDefault();
		
		MochiKit.Signal.signal(this, 'openDirectLogin', this.reference());
	},

	//-------------------------------------------------------------------------

	'editDirectLogin': function (anEvent) {
		anEvent.preventDefault();
		
		MochiKit.Signal.signal(this, 'editDirectLogin', this.reference());
//console.log("EDIT DIRECT LOGIN");
	},

	//-------------------------------------------------------------------------

	'deleteDirectLogin': function (anEvent) {
		anEvent.preventDefault();
		
		MochiKit.Signal.signal(this, 'deleteDirectLogin', this.reference());
//console.log("DELETE DIRECT LOGIN");
	},



	//=========================================================================
	__syntaxFix__: "syntax fix"
});
