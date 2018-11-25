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

"use strict";
Clipperz.Base.module('Clipperz.PM.UI.Components.ExtraFeatures');

Clipperz.PM.UI.Components.ExtraFeatures.DataImportClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.DataImport',

	getInitialState: function() {
		return {
			'importContext': new Clipperz.PM.UI.ImportContext(this),
		};
	},

	componentWillUnmount: function () {
		this.state['importContext'].release();
		this.setState({'importContext': null})
	},

	//=========================================================================

	importContext: function () {
		return this.state.importContext;
	},

	//=========================================================================

	componentWithName: function (aName) {
		var	result;
		var	path;
		var i, c;

		path = aName.split('.');

		result = Clipperz.PM.UI.Components.ExtraFeatures.DataImport;
		c = path.length;
		for (i=0; i<c; i++) {
			result = result[path[i]];
		}

		return result;
	},

	renderNavbar: function (currentStep) {
		return React.DOM.ul({'className': 'stepNavbar' + ' ' + currentStep},
			MochiKit.Base.map(MochiKit.Base.bind(function(aStep){
//				return React.DOM.li({'className': this.importContext().stepStatus(aStep)}, this.importContext().stepName(aStep));
				return React.DOM.li({'className': this.importContext().stepStatus(aStep)}, '.');
			}, this),this.importContext().steps())
		)
	},

	render: function () {
		var currentStep = this.importContext().currentStep().replace('.','_');

		return React.DOM.div({className:'extraFeature dataImport'}, [
			React.DOM.div({'className':'header'}, [
				React.DOM.h1({}, "Import"),
			]),
			React.DOM.div({'className': 'content' + ' ' + currentStep + ' ' + this.importContext().inputFormat()}, [
				React.DOM.div({'className': 'step' + ' ' + currentStep}, [
					new this.componentWithName(this.importContext().currentStep())({'importContext': this.importContext()}),
				]),
				this.renderNavbar(currentStep),
				React.DOM.div({'className': 'buttons' + ' ' + currentStep}, [
					React.DOM.a({
						'className': 'button back ' + this.importContext().backButtonStatus(),
						'onClick': this.importContext().goBackHandler()
					}, React.DOM.span({}, "Back")),
					React.DOM.a({
						'className': 'button next ' + this.importContext().forwardButtonStatus(),
						'onClick': this.importContext().goForwardHandler()
					}, React.DOM.span({}, "Next"))
				])
			])
		]);
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImportClass);
