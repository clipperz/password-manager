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

Clipperz.PM.UI.Components.RegistrationWizard = React.createClass({

	getDefaultProps: function () {
		return {
			steps: [
				{name:'CREDENTIALS',			label:'registration',	_label:'credentials',	description:"Choose your credentails"},
				{name:'PASSWORD_VERIFICATION',	label:'registration',	_label:'verify',		description:"Verify your passphrase"},
				{name:'TERMS_OF_SERVICE',		label:'registration',	_label:'terms',			description:"Check our terms of service"}
			],
			disabled: false,
			template: Clipperz.PM.UI.Components.PageTemplate
		}
	},

	getInitialState: function () {
		return {
			currentStep: this.props['steps'][0]['name'],
			username: '',
			passphrase: '',
			verify_passphrase: '',
			no_password_recovery: false,
			agree_terms_of_service: false
		};
	},

	'propTypes': {
//		steps:		React.PropTypes.array,
		disabled:	React.PropTypes.bool,
		template:	React.PropTypes.func
	},

	//=========================================================================

	currentStepIndex: function () {
		return this.indexOfStepNamed(this.state['currentStep']);
	},

	indexOfStepNamed: function (aStepName) {
		var stepConfiguration;
		var	result;

		stepConfiguration = this.props['steps'].filter(function (aConfig) { return aConfig['name'] == aStepName})[0];
		result = this.props['steps'].indexOf(stepConfiguration);
		return result;
	},

	//=========================================================================

	statusClassForStep: function (aStep) {
		var	currentStepIndex = this.currentStepIndex();
		var stepIndex = this.indexOfStepNamed(aStep['name']);
		var	result;

		if (stepIndex < currentStepIndex) {
			result = 'left';
		} else if (stepIndex == currentStepIndex) {
			result = 'center';
		} else {
			result = 'right';
		}

		return result;
	},

	//=========================================================================

	handleBackClick: function (anEvent) {
		var nextStep;
		anEvent.preventDefault();

		if (this.currentStepIndex() > 0) {
			nextStep = this.props['steps'][this.currentStepIndex() - 1];
			this.setState({currentStep: nextStep['name']});
		} else {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'goBack');
		}
	},

	handleForwardClick: function (anEvent) {
		var nextStep;
		anEvent.preventDefault();

		if (this.canMoveForward()) {

			if (this.currentStepIndex() < this.props['steps'].length - 1) {
				nextStep = this.props['steps'][this.currentStepIndex() + 1];
				this.setState({currentStep: nextStep['name']});
			} else {
				MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'registerNewUser', {
					username: this.state['username'],
					passphrase: this.state['passphrase']
				})
			}
		}
	},

	//-------------------------------------------------------------------------

	canMoveForward: function () {
		var result;
		var currentStep;

		result = false;
		currentStep = this.state['currentStep'];
		if (currentStep == 'CREDENTIALS') {
			result = ((this.state['username'] != '') && (this.state['passphrase'] != ''));
		} else if (currentStep == 'PASSWORD_VERIFICATION') {
			result = (this.state['passphrase'] == this.state['verify_passphrase']);
		} else if (currentStep == 'TERMS_OF_SERVICE') {
			result = (this.state['no_password_recovery'] && this.state['agree_terms_of_service']);
		}

		return result && !this.props['disabled'];
	},

	//=========================================================================

	handleChange: function (anEvent) {
		var	refs = this.refs;
		var refName = MochiKit.Base.filter(function (aRefName) { return refs[aRefName].getDOMNode() == anEvent.target}, MochiKit.Base.keys(this.refs))[0];
		var newState = {};

		if ((event.target.type == 'checkbox') || (event.target.type == 'radio')) {
			newState[refName] = event.target.checked;
		} else {
			newState[refName] = event.target.value;
		}
	    this.setState(newState);
	},

	//=========================================================================

	renderIndexStep: function (aStep) {
		return	React.DOM.div({'className':'stepIndexItem ' + this.statusClassForStep(aStep)}, '.');
	},

	renderButtons: function () {
		return [
			React.DOM.a({className:'back    button step_' + (this.currentStepIndex() - 1), onClick:this.handleBackClick},    '<<'),
			React.DOM.a({className:'forward button step_' + (this.currentStepIndex() + 1) + ' ' + (this.canMoveForward() ? 'enabled' : 'disabled'), onClick:this.handleForwardClick}, '>>')
		];
	},

	render_CREDENTIALS: function () {
		return	React.DOM.div(null,[
					React.DOM.label({'for':'name'}, "username"),
					React.DOM.input({'type':'text', 'name':'name', 'ref':'username', 'placeholder':"username", 'key':'username', 'autoCapitalize':'none'/*, value:this.state.username*/}),
					React.DOM.label({'for':'passphrase'}, "passphrase"),
					React.DOM.input({'type':'password', 'name':'passphrase', 'ref':'passphrase', 'placeholder':"passphrase", 'key':'passphrase'/*, value:this.state.passphrase*/})
				]);
	},

	render_PASSWORD_VERIFICATION: function () {
		return	React.DOM.div(null,[
					React.DOM.label({'for':'verify_passphrase'}, "passphrase"),
					React.DOM.input({'type':'password', 'name':'verify_passphrase', 'ref':'verify_passphrase', 'placeholder':"verify passphrase", 'key':'verify_passphrase'})
				]);
	},

	render_TERMS_OF_SERVICE: function () {
		return	React.DOM.div(null, [
					React.DOM.div({className:'checkboxBlock'}, [
						React.DOM.label({'for':'no_password_recovery'}, "I understand that Clipperz will not be able to recover a lost passphrase."),
						React.DOM.input({'type':'checkbox', 'name':'no_password_recovery', 'ref':'no_password_recovery', 'key':'no_password_recovery'}),
						React.DOM.p(null, "I understand that Clipperz will not be able to recover a lost passphrase.")
					]),
					React.DOM.div({className:'checkboxBlock'}, [
						React.DOM.label({'for':'agree_terms_of_service'}, "I have read and agreed to the Terms of Service."),
						React.DOM.input({'type':'checkbox', 'name':'agree_terms_of_service', 'ref':'agree_terms_of_service', 'key':'agree_terms_of_service'}),
						React.DOM.p(null, [
							"I have read and agreed to the ",
							React.DOM.a({href:'https://clipperz.com/terms_service/', target:'_blank'}, "Terms of Service.")
						])
					])
				]);
	},

	renderStep: function (aStep) {
		return	React.DOM.div({'className':'step' + ' ' + aStep['name'] + ' ' + this.statusClassForStep(aStep) + ' step_' + this.currentStepIndex()}, [
					React.DOM.h1(null, aStep['label']),
					React.DOM.p(null, aStep['description']),
					this['render_' + aStep['name']].apply(),
					React.DOM.div({'className':'stepIndex'}, MochiKit.Base.map(this.renderIndexStep, this.props['steps'])),
					React.DOM.div({'className':'buttons'}, this.renderButtons())
				]);
	},

	_render: function () {
		return	React.DOM.div({'className':'registrationForm'},[
					React.DOM.form({onChange: this.handleChange}, [
						React.DOM.div({'className':'steps'}, MochiKit.Base.map(this.renderStep, this.props['steps']))
					])
				]);
	},

	render: function () {
		return	new this.props.template({'innerComponent': this._render()});
	},

	//=========================================================================

	setInitialFocus: function () {
		this.refs['username'].getDOMNode().focus();
	},

	componentDidUpdate: function (prevProps, prevState, rootNode) {
		if (prevState['currentStep'] != this.state['currentStep']) {
			if (this.state['currentStep'] == 'CREDENTIALS') {
				this.refs['passphrase'].getDOMNode().select();
			} else if (this.state['currentStep'] == 'PASSWORD_VERIFICATION') {
				this.refs['verify_passphrase'].getDOMNode().select();
			}
		}
	}

	//=========================================================================
});
