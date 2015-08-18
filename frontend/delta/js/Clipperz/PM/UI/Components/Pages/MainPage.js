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
Clipperz.Base.module('Clipperz.PM.UI.Components.Pages');

Clipperz.PM.UI.Components.Pages.MainPageClass = React.createClass({

	getDefaultProps: function () {
		return {
			featureSet: 'FULL',
			features:	[]
		};
	},

	propTypes: {
		'tags':				React.PropTypes.object,
		'allTags':			React.PropTypes.array,
		'messageBox':		React.PropTypes.object.isRequired,
		'featureSet':		React.PropTypes.oneOf(['FULL', 'EXPIRED', 'TRIAL']).isRequired,
		'features':			React.PropTypes.array.isRequired,
		'userInfo':			React.PropTypes.object.isRequired,
		'accountInfo':		React.PropTypes.object.isRequired,
		'style':			React.PropTypes.oneOf(Clipperz_PM_UI_availableStyles).isRequired,
//		'mediaQueryStyle':	React.PropTypes.oneOf(['extra-short', 'narrow', 'wide', 'extra-wide']).isRequired,
//		'cards':			React.PropTypes.deferred.isRequired
	},

	getInitialState: function () {
		return {
//			shouldStoreDataLocally: false
		};
	},

	//=========================================================================

	render: function () {
		var	classes = {
			'mainPage': true
		};
		classes[this.props['style']] = true;

		return	React.DOM.div({'key':'mainPage', 'className':Clipperz.PM.UI.Components.classNames(classes)}, [
			this.props['style'] != 'extra-wide' ? Clipperz.PM.UI.Components.Panels.SelectionPanel(this.props) : null,
			Clipperz.PM.UI.Components.Panels.MainPanel(this.props),
			Clipperz.PM.UI.Components.Panels.ExtraFeaturesPanel(this.props),
			this.props['ask'] ? Clipperz.PM.UI.Components.DialogBox(this.props['ask']) : null
		]);
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.Pages.MainPage = React.createFactory(Clipperz.PM.UI.Components.Pages.MainPageClass);
