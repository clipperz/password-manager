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

'use strict';
Clipperz.Base.module('Clipperz.PM.UI.Components.Cards');

Clipperz.PM.UI.Components.Cards.ViewClass = React.createClass({

	//============================================================================

	propTypes: {
		'label':	React.PropTypes.string /*.isRequired */ ,
		'loading':	React.PropTypes.bool,
	},

	getInitialState: function () {
		return {};
	},
	
	//----------------------------------------------------------------------------

	handleDirectLoginClick: function (aDirectLogin) {
		var	directLoginParameters;

//console.log("PROPS", this.props);
		directLoginParameters = {
			'record': this.props['_reference'],
			'directLogin': aDirectLogin['_reference'],
		};

		return function (anEvent) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'runDirectLogin', directLoginParameters);
		};
	},

	//----------------------------------------------------------------------------

	handlePasswordFieldAction: function (aField) {
		var	self = this;
		var newState = {}

		newState[aField['_reference']] = !this.state[aField['_reference']];

		return function () {
			var fieldReference = aField['_reference'];
			var currentTimeout;

			if (newState[fieldReference]) {
				currentTimeout = setTimeout(function(){
					var newState;

					newState = {};
					newState[fieldReference] = false;

					self.setState(newState);
				}, Clipperz.PM.UI.Components.Cards.ViewClass.automaticRescrambleTimeout);
			}

			if (self.state['currentTimeout']) {
				clearTimeout(self.state['currentTimeout']);
				delete self.state['currentTimeout'];
			}

			if (currentTimeout) {
				newState['currentTimeout'] = currentTimeout;
			}

			self.setState(newState);
		};
	},

	handleLinkFieldAction: function (aField) {
		return function () {
			var url;

			url = aField['value'];
			if (/^https?\:\/\//.test(url) == false) {
				url = 'http://' + url;
			}

			window.open(url);
		}
	},

	handleFieldAction: function (aField) {
		var	result;

		if (aField['actionType'] == 'PASSWORD') {
			result = this.handlePasswordFieldAction(aField);
		} else if (aField['actionType'] == 'URL') {
			result = this.handleLinkFieldAction(aField);
		} else {
			result = MochiKit.Base.noop;
		};
		
		return result;
	},

	//----------------------------------------------------------------------------

	renderEmpty: function () {
//		return	React.DOM.h4({}, "EMPTY");
		return	React.DOM.h4({}, "");
	},
	
	//----------------------------------------------------------------------------

	renderLoading: function () {
		return	React.DOM.div({className:'loading'},[
			this.renderLabel(),
//			React.DOM.h5({className:'message'}, "loading")

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
		]);
	},
	
	//----------------------------------------------------------------------------

	renderLabel: function (aLabel) {
		return	React.DOM.h3({'className':'cardLabel'}, aLabel);
	},
	
	renderNotes: function (someNotes) {
		var	result;

//console.log("NOTES", someNotes);
		if (someNotes != "") {
			result = React.DOM.div({'className':'cardNotes'}, [
				React.DOM.div({}, someNotes)
			]);
		} else {
			result = null;
		}

		return	result;
	},

	//............................................................................

//	renderTag: function (aTag) {
//		return	React.DOM.div({'className':'cardTag'}, aTag);
//	},
	
	renderTags: function (someTags) {
		var	tags;
		var	result;

//console.log("TAGS", someTags);
		tags = MochiKit.Base.filter(Clipperz.PM.DataModel.Record.isRegularTag, someTags).sort(Clipperz.Base.caseInsensitiveCompare);

		if (tags.length > 0) {
			result = Clipperz.PM.UI.Components.Cards.TagEditor({'selectedTags':tags, 'readOnly':true });
		} else {
			result = null;
		}

		return result;
	},

	//............................................................................

	renderField: function (aField) {
		var	cardFieldClasses = {};
		var	cardFieldValueClasses = {};
		var cardFieldActionClasses = {};
		
		cardFieldClasses['cardField'] = true;
		cardFieldClasses[aField['actionType']] = true;
		cardFieldClasses['hidden'] = aField['isHidden'];
		
		cardFieldValueClasses['fieldValue'] = true;
		cardFieldValueClasses[aField['actionType']] = true;
		cardFieldValueClasses['hidden'] = aField['isHidden'];
		cardFieldValueClasses['visible'] = this.state[aField['_reference']];

		cardFieldActionClasses['action'] = true;
		cardFieldActionClasses[aField['actionType']] = true;
		cardFieldActionClasses['active'] = this.state[aField['_reference']];

		return	React.DOM.div({'className':Clipperz.PM.UI.Components.classNames(cardFieldClasses)}, [
			React.DOM.div({'className':'fieldEditAction'}, null),
			React.DOM.div({'className':'fieldValues'}, [
				React.DOM.div({'className':'fieldLabel'}, aField['label']),
				React.DOM.textarea({
					'readOnly': true,
					'onClick': function(e) { e.target.select(); },
					'className':Clipperz.PM.UI.Components.classNames(cardFieldValueClasses),
					'value': aField['value']
				}),
			]),
			React.DOM.div({'className':'fieldAction'}, [
				React.DOM.span({'className':Clipperz.PM.UI.Components.classNames(cardFieldActionClasses), 'onClick':this.handleFieldAction(aField)}, aField['actionType'].toLowerCase() == 'password' ? 'view password' : aField['actionType'].toLowerCase())
			])
		]);
	},

	renderFields: function (someFields) {
		return	React.DOM.div({'className':'cardFields'}, MochiKit.Base.map(this.renderField, someFields));
	},

	//............................................................................

	renderDirectLogin: function (aDirectLogin) {
		return	React.DOM.div({'className':'cardDirectLogin', 'onClick':this.handleDirectLoginClick(aDirectLogin)}, [
			React.DOM.span({'className':'directLoginLabel'}, aDirectLogin['label']),
//			React.DOM.div({'className':'directLoginAction action'}, 'DIRECT LOGIN')
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
	
		return	React.DOM.div({'className':Clipperz.PM.UI.Components.classNames(classes)},[
			Clipperz.PM.UI.Components.Cards.CommandToolbar(this.props),
			React.DOM.div({'className':'content'}, [
				this.renderLabel(this.props['label']),
				this.renderTags(this.props['tags']),
				this.renderFields(this.props['fields']),
				this.renderNotes(this.props['notes']),
				this.renderDirectLogins(this.props['directLogins'])
			]),
			this.props['ask'] ? Clipperz.PM.UI.Components.DialogBox(this.props['ask']) : null
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

Clipperz.PM.UI.Components.Cards.ViewClass.automaticRescrambleTimeout = 5000;

Clipperz.PM.UI.Components.Cards.View = React.createFactory(Clipperz.PM.UI.Components.Cards.ViewClass);
