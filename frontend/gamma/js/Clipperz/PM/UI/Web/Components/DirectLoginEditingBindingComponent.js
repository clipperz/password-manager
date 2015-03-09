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

Clipperz.PM.UI.Web.Components.DirectLoginEditingBindingComponent = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.DirectLoginEditingBindingComponent.superclass.constructor.apply(this, arguments);

	this._formFieldName				= args.formFieldName	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._fields					= args.fields			|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._initiallySelectedFieldKey	= args.selectedFieldKey	|| null;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.DirectLoginEditingBindingComponent, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.DirectLoginEditingBindingComponent component";
	},

	//-------------------------------------------------------------------------

	'formFieldName': function () {
		return this._formFieldName;
	},

	//-------------------------------------------------------------------------

	'fields': function () {
		return this._fields;
	},

	//-------------------------------------------------------------------------

	'selectedValue': function () {
		var result;
		
		result = this.getElement('select').value;
		
		if (result == '---') {
			result = null;
		}

		return result;
	},

	'initiallySelectedFieldKey': function () {
		return this._initiallySelectedFieldKey;
	},

	//=========================================================================

	'renderSelf': function() {
		var initiallySelectedOptions;

		this.append(this.element(), {tag:'div', id:this.getId('div'), cls:'binding', children:[
			{tag:'span', cls:'formFieldName', html:this.formFieldName()},
			{tag:'span', cls:'fieldLock', id:this.getId('isHidden'), children:[
				{tag:'a', href:'#', id:this.getId('showHide'), html:'&nbsp;'}
			]},
			{tag:'input', id:this.getId('input'), cls:'formFieldExampleValue', disabled:true, value:''},
			{tag:'select', name:this.formFieldName(), id:this.getId('select'), cls:'formFieldMatchinCardField', children:
				MochiKit.Base.flattenArguments(
					{tag:'option', value:'---', html:"---"},
					MochiKit.Base.map(
						MochiKit.Base.bind(function (aField) { return {tag:'option', value:aField['reference'], html:aField['label']}; }, this),
						this.fields()
					)	
				)
			}
		]});

		MochiKit.Signal.connect(this.getElement('select'),	 'onchange', this, 'handleSelectChange');
		MochiKit.Signal.connect(this.getElement('showHide'), 'onclick',	 this, 'handleShowHide');

		if (! MochiKit.Base.isUndefinedOrNull(this.initiallySelectedFieldKey())) {
			initiallySelectedOptions = MochiKit.Selector.findChildElements(this.element(), ['option[value=' + this.initiallySelectedFieldKey() + ']']);
			if (initiallySelectedOptions.length == 1) {
				MochiKit.DOM.updateNodeAttributes(initiallySelectedOptions[0], {selected:true});
				this.handleSelectChange();
			}
		}
	},

	//-------------------------------------------------------------------------

	'setFieldValue': function (aValue) {
		this.getElement('input').value = aValue;
	},

	'isHidden': function () {
		return MochiKit.DOM.hasElementClass(this.getElement('div'), 'locked');
	},

	'setIsHidden': function (aValue) {
		if (aValue == true) {
			MochiKit.DOM.addElementClass(this.getElement('div'), 'locked');
			MochiKit.DOM.addElementClass(this.getElement('div'), 'showLocked');
		} else {
			MochiKit.DOM.removeElementClass(this.getElement('div'), 'locked');
			MochiKit.DOM.removeElementClass(this.getElement('div'), 'showLocked');
		}
	},

	'isShowLocked': function () {
		return MochiKit.DOM.hasElementClass(this.getElement('div'), 'showLocked');
	},

	//-------------------------------------------------------------------------

	'handleSelectChange': function (anEvent) {
//		this.getElement('input').value = this.valueOfField(anEvent.src().value);
		MochiKit.Signal.signal(this, 'bindChange', this);
	},

	'handleShowHide': function (anEvent) {
		anEvent.preventDefault();

		if (this.isShowLocked()) {
			MochiKit.DOM.removeElementClass(this.getElement('div'), 'showLocked');
		} else {
			MochiKit.DOM.addElementClass(this.getElement('div'), 'showLocked');
		}
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});








