/*

Copyright 2008-2018 Clipperz Srl

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

Clipperz.Base.module('Clipperz.PM.UI.Components');

Clipperz.PM.UI.Components.DialogBoxClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.DialogBox',

	propTypes: {
		'info':		React.PropTypes.object.isRequired,
		'deferred':	React.PropTypes.object.isRequired
	},

	//-------------------------------------------------------------------------
/*
	handleKeyDown: function (anEvent) {
console.log("DIALOG BOX - key DOWN", anEvent);
	},

	handleKeyPress: function (anEvent) {
console.log("DIALOG BOX - key PRESS", anEvent);
	},

	handleKeyUp: function (anEvent) {
console.log("DIALOG BOX - key UP", anEvent);
	},
*/
	//-------------------------------------------------------------------------

	handleAnswerButton: function (anEvent) {
		this.props['info']['possibleAnswers'][anEvent.currentTarget.dataset['answerKey']]['answer'](this.props['deferred']);
	},

	renderAnswerButton: function (anAnswerInfoKey) {
		var	answerInfo = this.props['info']['possibleAnswers'][anAnswerInfoKey];
		var	classes = {
			'button':		true,
			'isDefault':	answerInfo['isDefault'],
			'disabled':		answerInfo['disabled']
		};
		
		return	React.DOM.div({
			'className':Clipperz.PM.UI.Components.classNames(classes),
			'onClick':this.handleAnswerButton,
			'data-answer-key':anAnswerInfoKey
		}, answerInfo['label']);
	},

	//=========================================================================

	render: function () {
//console.log("DIALOG BOX", this.props);
		return	React.DOM.div({'className':'dialogBox' /*, 'onKeyDown':this.handleKeyDown, 'onKeyPress':this.handleKeyPress, 'onKeyUp':this.handleKeyUp */ }, [
			React.DOM.div({'className':'mask'}),
			React.DOM.div({'className':'dialog'}, [
				React.DOM.h3({'className': 'message'}, this.props['info']['question']),
				this.props['info']['description']
					? React.DOM.div({'className':'description'}, this.props['info']['description'])
					: null,
				React.DOM.div({'className': 'answers'}, MochiKit.Base.map(this.renderAnswerButton, MochiKit.Base.keys(this.props['info']['possibleAnswers'])))
			])
		]);
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.DialogBox = React.createFactory(Clipperz.PM.UI.Components.DialogBoxClass);
