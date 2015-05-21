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

"use strict";
Clipperz.Base.module('Clipperz.PM.UI.Components.ExtraFeatures');

var _steps = ['Input', 'CsvColumns', 'CsvLabels', 'CsvTitles', 'CsvNotes', 'CsvHidden', 'Preview'];
var _stepNames = ['Input', 'Columns', 'Labels', 'Titles', 'Notes','Hidden','Preview'];

Clipperz.PM.UI.Components.ExtraFeatures.DataImportClass = React.createClass({
	_steps: _steps,
	_stepNames: _stepNames,
	_relevantSteps: {
		'csv': _steps,
		'json': [_steps[0], _steps[6]]
	},
	
	getInitialState: function() {
		return {
			'currentStep': this._steps[0],
			'importContext': new Clipperz.PM.UI.ImportContext(),
			'nextStepCallback': null,
			'error': null
		};
	},

	//=========================================================================

	getStepIndex: function(aStep) {
		return this._steps.indexOf(aStep);
	},
	
	getStepAfter: function() {
		return this._steps[this.getStepIndex(this.state.currentStep) + 1];
	},

	getStepBefore: function() {
		return this._steps[this.getStepIndex(this.state.currentStep) - 1];
	},
	
	isStepRelevant: function(aStep, aFormat) {
		if (!aFormat) {
			return true
		} else {
			return (this._relevantSteps[aFormat].indexOf(aStep) >= 0);
		}
	},
	
	//--------------------------------------------------------------------------
	
	goToStep: function(aStep) {
		this.setState({
			'currentStep': aStep,
			'nextStepCallback': null,
			'error': null
		});
	},
	
	handleNextStepOnClick: function() {
		if (this.state.nextStepCallback) {
			var newImportContext = this.state.nextStepCallback();
			
			if (newImportContext) {
				MochiKit.Base.update(this.state.importContext, newImportContext);
				
				if (this.state.currentStep == 'Input' && this.state.importContext.format == 'json') {
					this.goToStep('Preview');
				} else if (this.state.currentStep == 'Preview') {
					this.state.importContext.resetContext();
					this.goToStep('Input');
				} else {
					this.goToStep(this.getStepAfter());
				}
			} else {
				if (this.state.currentStep == "Input") {
					this.setState({'error': "unrecognized input format."});
				} else {
					this.setState({'error': "unknown error."});
				}
			}
		}
	},
	
	handleBackOnClick: function() {
		if (this.state.importContext.format == 'json' && this.state.currentStep == 'Preview') {
			delete this.state.importContext.format;
			this.goToStep('Input');
		} else if (this.state.currentStep != this._steps[0]) {
			this.goToStep(this.getStepBefore());
		}
	},

	setNextStepCallback: function(aFunction) {
		this.setState({'nextStepCallback': aFunction});
	},	
	
	getStepNavbarClass: function(aStep) {
		var result;
		
		if (aStep == this.state.currentStep) {
			result = 'active';
		} else if (this.state.importContext.format == 'json' && (aStep>=1&&aStep<=5) ) {
			result = 'disabled';
		} else {
			result = 'inactive';
		}
		
		return result;
	},
	
	//=========================================================================

	render: function () {
		return	React.DOM.div({className:'extraFeature dataImport'}, [
			React.DOM.h1({}, "Import"),
			React.DOM.div({'className': 'content'}, [
				React.DOM.ul({'className': 'stepNavbar'},
					MochiKit.Base.map(MochiKit.Base.bind(function(aStep){
						var className;
						
						if (this.isStepRelevant(aStep,this.state.importContext.format)) {
							className = (aStep == this.state.currentStep) ? 'active' : 'inactive';
						} else {
							className = 'disabled';
						}
						
						return React.DOM.li({
							'className': className
						}, this._stepNames[this.getStepIndex(aStep)]);
					}, this),this._steps)
				),
				new Clipperz.PM.UI.Components.ExtraFeatures.DataImport[this.state.currentStep]({
					'importContext': this.state.importContext,
					'setNextStepCallback': this.setNextStepCallback,
				}),
				React.DOM.a({
					'className': 'button'+((this.state.currentStep == this._steps[0]) ? ' disabled' : ''),
					'onClick': this.handleBackOnClick,
				}, "Back"),
				React.DOM.a({
					'className': 'button'+((! this.state.nextStepCallback) ? ' disabled' : ''),
					'onClick': this.handleNextStepOnClick,
				}, "Next"),
				(this.state.error) ? React.DOM.p({'className': 'error'}, "Error: " + this.state.error) : null
			])
		]);
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImportClass);
