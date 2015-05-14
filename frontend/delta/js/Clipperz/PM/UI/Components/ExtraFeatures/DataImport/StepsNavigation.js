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
Clipperz.Base.module('Clipperz.PM.UI.Components.ExtraFeatures.DataImport');

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.StepsNavigationClass = React.createClass({
	
	_stepsInfo: [
		{
			id: 'input',
			name: 'Input',
			formats: ['json', 'csv']
		},
		{
			id: 'csv-columns',
			name: 'Columns',
			formats: ['csv']
		},
		{
			id: 'csv-labels',
			name: 'Labels',
			formats: ['csv']
		},
		{
			id: 'csv-titles',
			name: 'Titles',
			formats: ['csv']
		},
		{
			id: 'csv-notes',
			name: 'Notes',
			formats: ['csv']
		},
		{
			id: 'csv-hidden',
			name: 'Hidden',
			formats: ['csv']
		},
		{
			id: 'preview',
			name: 'Preview',
			formats: ['json', 'csv']
		}
	],

	render: function() {
		var navigationButtons;
		
		if (this.props.prevStep && this.props.nextStep) {
			navigationButtons = [
				React.DOM.button({'onClick': MochiKit.Base.partial(this.props.goToStepCallback, this.props.prevStep)}, "Back"),
				React.DOM.span({}, " - "),
				React.DOM.button({'onClick': MochiKit.Base.partial(this.props.goToStepCallback, this.props.nextStep), 'disabled': this.props.nextDisabled }, "Next")
			];
		} else {
			
		}
		
		return React.DOM.div({},[
			React.DOM.ul({'className': 'stepsOverview'}, 
				MochiKit.Base.map(MochiKit.Base.bind(function(aStep) {
					var className;

					className = (aStep.id == this.props.stepId) ? 'active' : 'inactive';
					className = (MochiKit.Base.findValue(aStep.formats,this.props.format)>= 0) ? className+' enabled' : className+' disabled';
					
					// TODO: replace with proper CSS
					var style = (aStep.id == this.props.stepId) ? {'display': 'inline-block', 'textDecoration': 'underline'} : {'display': 'inline-block'};
					return React.DOM.li({'className': className, 'style': style}, aStep.name);
				},this), this._stepsInfo)
			),
			navigationButtons
		]);
	}

});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.StepsNavigation = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.StepsNavigationClass);