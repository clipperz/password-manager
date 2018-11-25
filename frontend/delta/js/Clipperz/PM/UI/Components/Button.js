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
Clipperz.Base.module('Clipperz.PM.UI.Components');

Clipperz.PM.UI.Components.ButtonClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.Button',

	propTypes: {
		'eventName':			React.PropTypes.string.isRequired,
		'label':				React.PropTypes.string.isRequired,
		'handler':				React.PropTypes.func.isRequired,
		'className':			React.PropTypes.string,
		'badgeTopContent':		React.PropTypes.number,
		'badgeTopLevel':		React.PropTypes.string,
		'badgeBottomContent':	React.PropTypes.number,
		'badgeBottomLevel':		React.PropTypes.string,
	},

	//=========================================================================

	render: function () {
		var badgeTop;
		var badgeBottom;

		var	classes = {
			'button': true
		};
		if (typeof(this.props['className']) != 'undefined') {
			classes[this.props['className']] = true;
		};

		badgeTop = null;
		if (this.props['badgeTopContent']) {
			badgeTop = React.DOM.span({'className': Clipperz.PM.UI.Components.classNames('badge', 'top', this.props['badgeTopLevel'])}, this.props['badgeTopContent']);
		};

		badgeBottom = null;
		if (this.props['badgeBottomContent']) {
			badgeBottom = React.DOM.span({'className': Clipperz.PM.UI.Components.classNames('badge', 'bottom', this.props['badgeBottomLevel'])}, this.props['badgeBottomContent']);
		};

	
		return	React.DOM.div({'className':Clipperz.PM.UI.Components.classNames(classes), onClick:this.props['handler']}, [
			React.DOM.div({'className':this.props['eventName']}, [
				React.DOM.h3({'className':'label'}, this.props['label']),
				badgeTop,
				badgeBottom,
			])
		]);
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.Button = React.createFactory(Clipperz.PM.UI.Components.ButtonClass);