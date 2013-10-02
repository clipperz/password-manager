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

Clipperz.PM.UI.Components.CardDetail = React.createClass({

	getDefaultProps: function () {
		return {
//			searchDelay: 0.3
		}
	},

	propTypes: {
		card: React.PropTypes.object.isRequired
	},

	getInitialState: function () {
		return {
//			showSearch: false,
//			searchTimer: null,
			unmaskedFields: new Clipperz.Set(),
			starred: false
		};
	},

	handleDirectLoginClick: function (aDirectLoginReference, anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'runDirectLogin', {record:this.props.card['reference'], directLogin:aDirectLoginReference});
	},

	toggleFieldVisibility: function (aField, anEvent) {
		var unmaskedFields;
		var fieldReference;

		unmaskedFields = this.state['unmaskedFields'];
		fieldReference = aField['reference']
		if (unmaskedFields.contains(fieldReference)) {
			unmaskedFields.remove(fieldReference)
		} else {
			unmaskedFields.add(fieldReference)
		}

		this.setState({'unmaskedFields': unmaskedFields});
	},

	handleGoAction: function (aField, anEvent) {
		var newWindow;

		newWindow = MochiKit.DOM.currentWindow().open(aField['value'], '_blank');
		newWindow.focus();
	},

	handleEmailAction: function (aField, anEvent) {
		MochiKit.DOM.currentWindow().location = 'mailto:' + aField['value'];
	},

	//=========================================================================

	normalizeFieldValue: function (aValue) {
		var	result = [];
		var	rows = aValue.split('\n');

		for (var i = 0; i < rows.length; i++) {
			if (i > 0) {
				result.push(React.DOM.br());
			}
			result.push(rows[i].replace(/[\s]/g, '\u00A0'));
		}

		return result;
	},

	renderFieldActionButton: function (aField) {
//		var	actionLabel;
		var result;

		if (aField['actionType'] == 'URL') {
			result = 	React.DOM.div({className:'actionWrapper', onClick:MochiKit.Base.method(this, 'handleGoAction', aField)}, [
							React.DOM.a({className:aField['actionType']}, "go")
						]);
		} else if (aField['actionType'] == 'PASSWORD') {
			var icon;

			if (this.state['unmaskedFields'].contains(aField['reference'])) {
				icon = "unlocked";
			} else {
				icon = "locked";
			}
			result =	React.DOM.div({className:'actionWrapper', onClick:MochiKit.Base.method(this, 'toggleFieldVisibility', aField)}, [
							React.DOM.a({className:aField['actionType']}, icon)
						]);
		} else if (aField['actionType'] == 'EMAIL') {
			result =	React.DOM.div({className:'actionWrapper', onClick:MochiKit.Base.method(this, 'handleEmailAction', aField)}, [
							React.DOM.a({className:aField['actionType']}, "email")
						]);
		} else {
			result = null;
		}

		return result;
	},

	renderField: function (aField) {
//console.log("FIELD", aField);
		var fieldExtraClass;

		fieldExtraClass = aField['actionType'];
		if (this.state['unmaskedFields'].contains(aField['reference'])) {
			fieldExtraClass = fieldExtraClass + ' unlocked';
		}

		return	React.DOM.div({className:'listItem ' + fieldExtraClass, key:aField['reference']}, [
					React.DOM.div({className:'fieldWrapper'}, [
						React.DOM.div({className:'fieldInnerWrapper'}, [
							React.DOM.div({className:'labelWrapper'}, React.DOM.span({className:'label'}, aField['label'])),
							React.DOM.div({className:'valueWrapper'}, React.DOM.span({className:'value ' + fieldExtraClass}, this.normalizeFieldValue(aField['value'])))
						])
					]),
					this.renderFieldActionButton(aField)
//					React.DOM.div({className:'actionWrapper'}, [
//						React.DOM.div({className:aField['actionType']}, actionLabel)
//					])
				]);
	},

	renderDirectLogin: function (aDirectLogin) {
//console.log("DIRECT LOGIN", aDirectLogin);
		return	React.DOM.div({className:'listItem', onClick:MochiKit.Base.method(this, 'handleDirectLoginClick', aDirectLogin['reference'])}, [
					React.DOM.div({className:'labelWrapper'}, React.DOM.span({className:'label'}, aDirectLogin['label'])),
					React.DOM.div({className:'faviconWrapper'}, React.DOM.img({className:'favicon', src:aDirectLogin['favicon']})),
					React.DOM.div({className:'directLoginLinkWrapper'}, React.DOM.span({className:'directLoginLink'}, "go"))
				]);
	},

	handleBackClick: function (anEvent) {
//		window.history.back();
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'goBack');
	},

	handleStarClick: function (anEvent) {
		this.setState({starred: !this.state['starred']});
	},

	//=========================================================================

	render: function () {
		var card = this.props.card;
//		var starredStatus = (this.state['starred'] ? "starred" : "unstarred");

		if ((typeof(card['fields']) != 'undefined') && (card['notes'] != '')) {
			card['fields'].push({ 'actionType': 'NOTES', 'isHidden': false, 'label': "notes", 'reference': "notes", 'value': card['notes'] })
		}

		return	React.DOM.div({className:'cardDetail'}, [
			React.DOM.div({className:'header'}, [
				React.DOM.div({className:'titleWrapper'}, React.DOM.div({className:'title'}, card.title)),
				React.DOM.div({className:'backWrapper'},  React.DOM.a({className:'button back', onClick:this.handleBackClick}, "back")),
//				React.DOM.div({className:'starWrapper'},  React.DOM.a({className:'star', onClick:this.handleStarClick}, starredStatus))
			]),
			React.DOM.div({className:'content'}, [
				card.fields			? React.DOM.div({className:'fields'},		MochiKit.Base.map(this.renderField,			card.fields)) : null,
				card.directLogins	? React.DOM.div({className:'directLogins'},	MochiKit.Base.map(this.renderDirectLogin,	card.directLogins)): null
			]),
			React.DOM.div({className:'footer'}, [
/*				
//				React.DOM.a({className:'cancel'}, "cancel"),
//				React.DOM.a({className:'save'},   "save")

				React.DOM.a({className:'cancel button'}, "failed"),
				React.DOM.a({className:'save button'},   "done")
*/
			])
		]);
	}

	//=========================================================================
});
