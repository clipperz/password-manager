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

Clipperz.Base.module('Clipperz.PM.UI.Components.Pages');

Clipperz.PM.UI.Components.Pages.RegistrationPageClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.Pages.RegistrationPage',

	getDefaultProps: function () {
		return {
			steps: [
				{name:'CREDENTIALS',			label:'registration',	_label:'credentials',	description:"Choose your credentials"},
				{name:'PASSWORD_VERIFICATION',	label:'registration',	_label:'verify',		description:"Verify your passphrase"},
				{name:'TERMS_OF_SERVICE',		label:'registration',	_label:'terms',			description:"Check our terms of service"}
			],
			disabled: false,
//			template: Clipperz.PM.UI.Components.PageTemplate
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

	handleLoginLinkClick: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'goBack');
	},

	handleBackClick: function (anEvent) {
		var nextStep;
		anEvent.preventDefault();

		if (this.currentStepIndex() > 0) {
			nextStep = this.props['steps'][this.currentStepIndex() - 1];
			this.setState({currentStep: nextStep['name']});
		} else {
//			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'goBack');
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

	toggleCheckbox: function (aCheckboxRef, anEvent) {
		var	newState = {};

		this.refs[aCheckboxRef].checked = ! this.refs[aCheckboxRef].checked;
		newState[aCheckboxRef] = this.refs[aCheckboxRef].checked;
		this.setState(newState);
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
		var refName = MochiKit.Base.filter(function (aRefName) { return refs[aRefName] == anEvent.target}, MochiKit.Base.keys(this.refs))[0];
		var newState = {};

		if ((anEvent.target.type == 'checkbox') || (anEvent.target.type == 'radio')) {
			newState[refName] = anEvent.target.checked;
		} else {
			newState[refName] = anEvent.target.value;
		}
		this.setState(newState);
	},

	//=========================================================================

	handleKeyDown: function (anEvent) {
		switch (anEvent.keyCode) {
			case  9: // tab
			case 13: // enter
				if (this.canMoveForward()) {
					this.handleForwardClick(anEvent);
				};
				break;
			case 27: // escape
				break;
		}
	},

	//=========================================================================

	renderIndexStep: function (aStep) {
		return	React.DOM.div({'key':'indexStep' + aStep['name'], 'className':'stepIndexItem ' + this.statusClassForStep(aStep)}, '.');
	},

	renderButtons: function () {
		return [
			React.DOM.a({'key':'backButton', 'className':'back    button step_' + (this.currentStepIndex() - 1), onClick:this.handleBackClick},    '<<'),
			React.DOM.a({'key':'forwardButton', 'className':'forward button step_' + (this.currentStepIndex() + 1) + ' ' + (this.canMoveForward() ? 'enabled' : 'disabled'), onClick:this.handleForwardClick}, '>>')
		];
	},

	render_CREDENTIALS: function () {
		return	React.DOM.div({'key':'credentials'},[
					React.DOM.label({'key':'usernameLabel', 'htmlFor':'name'}, "username"),
					React.DOM.input({'key':'username', 'type':'text', 'name':'name', 'ref':'username', 'placeholder':"username", 'autoCapitalize':'none' /*, value:this.state.username*/}),
					React.DOM.label({'key':'passphraseLabel', 'htmlFor':'passphrase'}, "passphrase"),
					React.DOM.input({'key':'passphrase', 'type':'password', 'name':'passphrase', 'ref':'passphrase', 'placeholder':"passphrase" /* , 'onKeyDown':this.handleKeyDown /*, value:this.state.passphrase*/})
				]);
	},

	render_PASSWORD_VERIFICATION: function () {
		return	React.DOM.div({'key':'passwordVerification'},[
					React.DOM.label({'key':'verifyPassphraseLabel', 'htmlFor':'verify_passphrase'}, "passphrase"),
					React.DOM.input({'key':'verifyPassphrase', 'type':'password', 'name':'verify_passphrase', 'ref':'verify_passphrase', 'placeholder':"verify passphrase" })
				]);
	},

	render_TERMS_OF_SERVICE: function () {
		return	React.DOM.div({'key':'termsOfService'}, [
					React.DOM.div({'key':'termsOfService_choice_1', 'className':'checkboxBlock'}, [
						React.DOM.label({'key':'termsOfService_label_1', 'htmlFor':'no_password_recovery'}, "I understand that Clipperz is unable to recover a lost passphrase."),
						React.DOM.input({'key':'no_password_recovery', 'type':'checkbox', 'name':'no_password_recovery', 'ref':'no_password_recovery', 'id':'no_password_recovery', 'tabIndex':'0'}),
						React.DOM.p({'key':'termsOfService_description_1', 'onClick':MochiKit.Base.method(this, 'toggleCheckbox', 'no_password_recovery')}, "I understand that Clipperz is unable to recover a lost passphrase.")
					]),
					React.DOM.div({'key':'termsOfService_choice_2', 'className':'checkboxBlock'}, [
						React.DOM.label({'key':'termsOfService_label_2', 'htmlFor':'agree_terms_of_service'}, "I have read and agreed to the Terms of Service."),
						React.DOM.input({'key':'agree_terms_of_service', 'type':'checkbox', 'name':'agree_terms_of_service', 'ref':'agree_terms_of_service', 'id':'agree_terms_of_service', 'tabIndex':'1'}),
						React.DOM.p({'key':'termsOfService_description_2'},  [
							React.DOM.span({'key':'termsOfService_description_2_p1', 'onClick':MochiKit.Base.method(this, 'toggleCheckbox', 'agree_terms_of_service')}, "I have read and agreed to the "),
							React.DOM.a({'key':'termsOfService_description_2_p2', 'onClick':this.showUrl('/terms_service/')}, "Terms of Service.")
						])
					])
				]);
	},

	renderStep: function (aStep) {
		return	React.DOM.div({'key':'step' + aStep['name'], 'className':'step' + ' ' + aStep['name'] + ' ' + this.statusClassForStep(aStep) + ' step_' + this.currentStepIndex()}, [
//!					React.DOM.h1(null, aStep['label']),
					React.DOM.p({'key':'stepDescription'}, aStep['description']),
					React.DOM.div({'key':'stepBody', 'className':'stepBody'}, this['render_' + aStep['name']].apply()),
					React.DOM.div({'key':'stepIndex', 'className':'stepIndex'}, MochiKit.Base.map(this.renderIndexStep, this.props['steps'])),
					React.DOM.div({'key':'stepBottons', 'className':'buttons'}, this.renderButtons())
				]);
	},

	showUrl: function (anUrl) {
		return function () {
			window.open(anUrl, 'clipperz_about');
		}
	},

	render: function () {
		return	React.DOM.div({'className':'registrationForm content'},[
					Clipperz.PM.UI.Components.AccountStatus(MochiKit.Base.update(this.props['proxyInfo'])),
					React.DOM.header({'key':'header'}, [
						React.DOM.div({'className':'headerContent'}, [
							React.DOM.h3({}, 'clipperz'),
							React.DOM.h5({}, 'keep it to yourself'),
						])
					]),
					React.DOM.div({'key':'body', 'className':'form  body'}, [
						React.DOM.div({'className':'bodyContent'}, [
							React.DOM.form({'key':'registrationForm', 'autoComplete':'off', 'onChange':this.handleChange, 'onKeyDown':this.handleKeyDown }, [
								React.DOM.div({'key':'steps', 'className':'steps'}, MochiKit.Base.map(this.renderStep, this.props['steps']))
							]),
							React.DOM.a({'key':'login', 'className':'loginLink', 'onClick':this.handleLoginLinkClick}, "login"),
						])
					]),
					React.DOM.div({'key':'afterBody', 'className':'afterBody'}),
					React.DOM.div({'className':'other', 'key':'other'}, [
						React.DOM.div({'className':'otherContent'}, [
							React.DOM.div({'key':'links', 'className':'links'}, [
								React.DOM.ul({}, [
									React.DOM.li({'key':'about',   'onClick':this.showUrl('/about/')}, "About"),
									React.DOM.li({'key':'terms',   'onClick':this.showUrl('/terms_service/')}, "Terms of service"),
									React.DOM.li({'key':'privacy', 'onClick':this.showUrl('/privacy_policy/')}, "Privacy"),
								])
							]),
						])
					]),
					React.DOM.footer({'key':'footer'}, [
						React.DOM.div({'className':'footerContent'}, [
							React.DOM.div({'key':'applicationVersion', 'className':'applicationVersion'}, [
								React.DOM.span({'key':'applicationVersionLabel'}, "application version"),
								React.DOM.a({'key':'applicationVersionLink', 'href':'https://github.com/clipperz/password-manager/commit/' + Clipperz_version, 'target':'github'}, Clipperz_version)
							])
						])
					])
				]);
	},

//	render: function () {
//		return	new this.props.template({'innerComponent': this._render()});
//	},

	//=========================================================================

	setInitialFocus: function () {
		this.refs['username'].focus();
	},

	componentDidUpdate: function (prevProps, prevState, rootNode) {
		if (prevState['currentStep'] != this.state['currentStep']) {
			if (this.state['currentStep'] == 'CREDENTIALS') {
				this.refs['passphrase'].select();
			} else if (this.state['currentStep'] == 'PASSWORD_VERIFICATION') {
				this.refs['verify_passphrase'].select();
			}
		}
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.Pages.RegistrationPage = React.createFactory(Clipperz.PM.UI.Components.Pages.RegistrationPageClass);
