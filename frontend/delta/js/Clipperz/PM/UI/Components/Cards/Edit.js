/*

Copyright 2008-2013 Clipperz Srl

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

'use strict';
Clipperz.Base.module('Clipperz.PM.UI.Components.Cards');

Clipperz.PM.UI.Components.Cards.Edit = React.createClass({

	//============================================================================

	propTypes: {
//		'label':	React.PropTypes.string /*.isRequired */ ,
//		'loading':	React.PropTypes.bool,
	},

	//----------------------------------------------------------------------------

	record: function () {
		return this.props['_record'];
	},

	//============================================================================

	handleChange: function (anObject , aMethodName) {
		var	reference = this.props['_reference'];
		var	method = MochiKit.Base.method(anObject, aMethodName);
		
		return function (anEvent) {
			method(anEvent.target.value);
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', reference);
//			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'refreshCardEditToolbar', reference);
		};
	},

	removeField: function (aField) {
		var	reference = this.props['_reference'];
		var	record = this.record();
		
		return function (anEvent) {
			record.removeField(aField);
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', reference);
		};
	},

	addNewField: function (anEvent) {
		var	reference = this.props['_reference'];

		this.record().addField({'label':"", 'value':"", 'isHidden':false});
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', reference);
	},

	//============================================================================

	renderLabel: function (aLabel) {
		return	React.DOM.input({'className':'cardLabel', 'onChange':this.handleChange(this.record(), 'setLabel'), 'defaultValue':aLabel, 'key':this.props['_reference'] + '_label'});
	},
	
	renderNotes: function (someNotes) {
		return	React.DOM.textarea({'className':'cardNotes', 'onChange':this.handleChange(this.record(), 'setNotes'), 'defaultValue':someNotes, 'key':this.props['_reference'] + '_notes'});
	},

	//............................................................................

	renderTag: function (aTag) {
		return	React.DOM.div({'className':'cardTag'}, aTag);
	},
	
	renderTags: function (someTags) {
		var	tags;

		tags = MochiKit.Base.filter(Clipperz.PM.DataModel.Record.isRegularTag, someTags).sort(Clipperz.Base.caseInsensitiveCompare);
		return	React.DOM.div({'className':'cardTags'}, MochiKit.Base.map(this.renderTag, tags));
	},

	//............................................................................

	renderField: function (aField) {
		var	ref = aField['_reference'];
		var	cardFieldClasses = {};
		var	cardFieldValueClasses = {};
		var	field = aField['_field'];

//console.log("RENDER FIELD", aField);
		cardFieldClasses['cardField'] = true;
		cardFieldClasses[aField['actionType']] = true;
		cardFieldClasses['hidden'] = aField['isHidden'];
		
		cardFieldValueClasses['fieldValue'] = true;
		cardFieldValueClasses[aField['actionType']] = true;
		cardFieldValueClasses['hidden'] = aField['isHidden'];
		
		return	React.DOM.div({'className':React.addons.classSet(cardFieldClasses), 'key':ref}, [
			React.DOM.div({'className':'fieldValues'}, [
				React.DOM.span({'className':'removeField', 'onClick':this.removeField(field)}, "delete"),
				React.DOM.input({'className':'fieldLabel', 'onChange':this.handleChange(field, 'setLabel'), 'defaultValue':aField['label']}),
				React.DOM.textarea({'className':React.addons.classSet(cardFieldValueClasses), 'onChange':this.handleChange(field, 'setValue'), 'defaultValue':aField['value']}),
			]),
			React.DOM.div({'className':'fieldAction action'}, aField['actionType'].toLowerCase())
		]);
	},

	renderFields: function (someFields) {
		return	React.DOM.div({'className':'cardFields'}, MochiKit.Base.map(this.renderField, someFields));
	},

	renderAddNewField: function () {
		return	React.DOM.div({'className':'newCardField', 'onClick':this.addNewField}, "Add new field");
	},

	//............................................................................

	renderDirectLogin: function (aDirectLogin) {
		return	React.DOM.div({'className':'cardDirectLogin', 'key':aDirectLogin['_reference']}, [
			React.DOM.span({'className':'directLoginLabel'}, aDirectLogin['label']),
			React.DOM.div({'className':'directLoginAction action'}, 'DIRECT LOGIN')
		]);
	},
	
	renderDirectLogins: function (someDirectLogins) {
		return	React.DOM.div({'className':'cardDirectLogins'}, MochiKit.Base.map(this.renderDirectLogin, someDirectLogins));
	},
	
	//............................................................................

	render: function () {
		var	classes = {
			'edit':	true
		}

//console.log("RENDER CARD EDIT");
		return	React.DOM.div({'className':'editWrapper'}, [
			React.DOM.div({'className':'mask'}),
			React.DOM.div({'className':React.addons.classSet(classes)},[
				Clipperz.PM.UI.Components.Cards.EditToolbar(this.props),
				React.DOM.div({'className':'content'}, [
					this.renderLabel(this.props['label']),
					this.renderTags(this.props['tags']),
					this.renderNotes(this.props['notes']),
					this.renderFields(this.props['fields']),
					this.renderAddNewField(),
					this.renderDirectLogins(this.props['directLogins'])
				])
			]),
			this.props['ask'] ? Clipperz.PM.UI.Components.DialogBox(this.props['ask']) : null
		]);
	},
	
	//=========================================================================
});
