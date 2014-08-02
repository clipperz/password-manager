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

Clipperz.PM.UI.Components.Cards.View = React.createClass({

	//============================================================================

	propTypes: {
		'label':	React.PropTypes.string.isRequired,
		'loading':	React.PropTypes.bool,
	},

	//----------------------------------------------------------------------------

	renderEmpty: function () {
		return	React.DOM.h4({}, "EMPTY");
	},
	
	//----------------------------------------------------------------------------

	renderLoading: function () {
		return	React.DOM.div({className:'loading'},[
			this.renderLabel(),
			React.DOM.h5({className:'message'}, "loading")
/*
			React.DOM.div({className:'overlay'}, [
				React.DOM.div({className:'spinner'}, [
					React.DOM.div({className:'bar01'}),
					React.DOM.div({className:'bar02'}),
					React.DOM.div({className:'bar03'}),
					React.DOM.div({className:'bar04'}),
					React.DOM.div({className:'bar05'}),
					React.DOM.div({className:'bar06'}),
					React.DOM.div({className:'bar07'}),
					React.DOM.div({className:'bar08'}),
					React.DOM.div({className:'bar09'}),
					React.DOM.div({className:'bar10'}),
					React.DOM.div({className:'bar11'}),
					React.DOM.div({className:'bar12'})
				])
			])
*/
		]);
	},
	
	//----------------------------------------------------------------------------

	renderLabel: function (aLabel) {
		return	React.DOM.h3({'className':'cardLabel'}, aLabel);
	},
	
	renderNotes: function (someNotes) {
		return	React.DOM.div({'className':'cardNotes'}, someNotes);
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
		var	cardFieldClasses = {};
		var	cardFieldValueClasses = {};
		
		cardFieldClasses['cardField'] = true;
		cardFieldClasses[aField['actionType']] = true;
		cardFieldClasses['hidden'] = aField['isHidden'];
		
		cardFieldValueClasses['fieldValue'] = true;
		cardFieldValueClasses[aField['actionType']] = true;
		cardFieldValueClasses['hidden'] = aField['isHidden'];
		
		return	React.DOM.div({'className':React.addons.classSet(cardFieldClasses)}, [
			React.DOM.div({'className':'fieldValues'}, [
				React.DOM.div({'className':'fieldLabel'}, aField['label']),
				React.DOM.div({'className':React.addons.classSet(cardFieldValueClasses)}, aField['value']),
			]),
			React.DOM.div({'className':'fieldAction action'}, aField['actionType'].toLowerCase())
		]);
	},

	renderFields: function (someFields) {
		return	React.DOM.div({'className':'cardFields'}, MochiKit.Base.map(this.renderField, someFields));
	},

	//............................................................................

	renderDirectLogin: function (aDirectLogin) {
		return	React.DOM.div({'className':'cardDirectLogin'}, [
			React.DOM.span({'className':'directLoginLabel'}, aDirectLogin['label']),
			React.DOM.div({'className':'directLoginAction action'}, 'DIRECT LOGIN')
		]);
	},
	
	renderDirectLogins: function (someDirectLogins) {
		return	React.DOM.div({'className':'cardDirectLogins'}, MochiKit.Base.map(this.renderDirectLogin, someDirectLogins));
	},
	
	//............................................................................

	renderCard: function () {
		var	classes = {
			'view':		true,
			'archived':	this.props['_isArchived']
		}
	
		return	React.DOM.div({'className':React.addons.classSet(classes)},[
			Clipperz.PM.UI.Components.Cards.Toolbar(this.props),
			React.DOM.div({'className':'content'}, [
				this.renderLabel(this.props['label']),
				this.renderTags(this.props['tags']),
				this.renderNotes(this.props['notes']),
				this.renderFields(this.props['fields']),
				this.renderDirectLogins(this.props['directLogins'])
			])
		]);
	},
	
	//----------------------------------------------------------------------------

	render: function () {
		var	result;

		if (this.props['loading'] == true) {
			result = this.renderLoading();
		} else if (this.props['label']) {
			result = this.renderCard();
		} else {
			result = this.renderEmpty();
		}
		
		return result;
	},

	//=========================================================================
});
