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

Clipperz.PM.UI.Web.Components.DirectLoginEditingFormValueComponent = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.DirectLoginEditingFormValueComponent.superclass.constructor.apply(this, arguments);

	this._formFieldName		= args.formFieldName	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._fieldOptions		= args.fieldOptions		|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._initialValue		= args.initialValue		|| null;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.DirectLoginEditingFormValueComponent, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.DirectLoginEditingFormValueComponent component";
	},

	//-------------------------------------------------------------------------

	'formFieldName': function () {
		return this._formFieldName;
	},

	//-------------------------------------------------------------------------

	'fieldOptions': function () {
		return this._fieldOptions;
	},

	'fieldType': function () {
		return this.fieldOptions()['type'];
	},

	'optionValues': function () {
		return MochiKit.Base.map(function (anOptionValue) {
			return {
				'label': anOptionValue['label'] || anOptionValue['value'],
				'value': anOptionValue['value']
			}
		}, this.fieldOptions()['options']);
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

	'initialValue': function () {
		return this._initialValue;
	},

	//=========================================================================

	'renderSelf': function() {
		this.append(this.element(), {tag:'div', id:this.getId('div'), cls:'formValue', children:[
			{tag:'span', cls:'formFieldName', html:this.formFieldName()},
			{tag:'div', id:this.getId('values')}
		]});

		if ((this.fieldType() == 'radio') || (this.fieldType() == 'select')) {
			this.append(this.getElement('values'),
				{tag:'select', name:this.formFieldName(), id:this.getId('select'), cls:'formFieldMatchinCardField', children:
					MochiKit.Base.flattenArguments(
//						{tag:'option', value:'---', html:"---"},
						MochiKit.Base.map(
							MochiKit.Base.bind(function (aValue) { return {tag:'option', value:aValue['value'], html:aValue['label']}; }, this),
							this.optionValues()
						)	
					)
				}
			);
			
			MochiKit.Signal.connect(this.getElement('select'),	 'onchange', this, 'handleSelectChange');

			if (! MochiKit.Base.isUndefinedOrNull(this.initialValue())) {
				var initiallySelectedOptions;
				initiallySelectedOptions = MochiKit.Selector.findChildElements(this.element(), ['option[value=' + this.initialValue() + ']']);
				if (initiallySelectedOptions.length == 1) {
					MochiKit.DOM.updateNodeAttributes(initiallySelectedOptions[0], {selected:true});
					this.handleSelectChange();
				} else {
					Clipperz.DOM.Helper.insertBefore(this.getElement('select').childNodes[0], {tag:'option', value:'---', html:"", selected:true});
				}
			} else {
				Clipperz.DOM.Helper.insertBefore(this.getElement('select').childNodes[0], {tag:'option', value:'---', html:"", selected:true});
			}
		} else if (this.fieldType() == 'checkbox') {
			this.append(this.getElement('values'),
				{tag:'input', type:'checkbox', name:this.formFieldName(), id:this.getId('checkbox'), cls:'formFieldMatchinCardField'}
			);

			MochiKit.Signal.connect(this.getElement('checkbox'), 'onchange', this, 'handleSelectChange');

			if (this.initialValue()) {
				MochiKit.DOM.updateNodeAttributes(this.getElement('checkbox'), {checked:true});
			}
		} else {
			WTF = TODO;
		}
	},

	//=========================================================================

	'handleSelectChange': function (anEvent) {
		var options;

		options = {};
		
		options['fieldName'] = this.formFieldName();

		if (this.fieldType() == 'checkbox') {
			options['selectedValue'] = (this.getElement('checkbox').checked ? 1 : null);
		} else {
			options['selectedValue'] = this.selectedValue();
		}

		MochiKit.Signal.signal(this, 'formValueChange', options);
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});








