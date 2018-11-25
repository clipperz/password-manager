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

Clipperz.PM.UI.Components.RadialProgressIndicatorClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.RadialProgressIndicator',

	propTypes: {
		'progress':		React.PropTypes.number.isRequired,
	},

	getInitialState: function() {
		return {};
	},

	//=========================================================================

	getPathDefinition: function(aRadius, aProgress) {
		aProgress = (aProgress <= 1) ? aProgress : 1;
		aProgress = (aProgress >= 0) ? aProgress : 0;

		var pi = Math.PI;
		var radiantAngle = 2 * pi * aProgress;

		var x = Math.sin(radiantAngle) * aRadius;
		var y = Math.cos(radiantAngle) * - aRadius;
		var mid = (aProgress > 0.5) ? 1 : 0;

		return 'M 0 0 ' +									// Start from origin
		       'v ' + (-aRadius) +' ' + 					// Straight vertical up
		       'A ' + aRadius + ' ' + aRadius + ' 1 ' +		// Arc, vertical radius, horizontal radius, xAxisRotate (?)...
		       mid + ' 1 ' +								// ...lrge arc flag, 
		       x  + ' ' +									// ...destination x
		       y  + ' z';									// ...destination y, close path

		//[x,y].forEach(function( d ){
		//  d = Math.round( d * 1e3 ) / 1e3;
		//});
	},

	//=========================================================================

	getAdditionalClassesString: function(aList) {
		var result;

		aList = aList || [];
		result = aList.join(' ');

		return ' '+result;
	},

	render: function () {
		var border;

		var additionalClasses = this.getAdditionalClassesString(this.props['additionalClasses']);

		var radius = Clipperz.PM.UI.Components.DEFAULT_RADIUS;
		var borderSize = (this.props['border']) ? this.props['border'] : 0;
		var boxSize = 2 * (radius + borderSize) + 2;
		var center  = boxSize/2;

		border = null;
		if (this.props['border']) {
			border = React.DOM.circle({
				'className': 'border',
				'cx': center,
				'cy': center,
				'r':  radius+this.props['border'],
			});
		}

		return React.DOM.svg({
			'className': 'radialProgressIndicator' + additionalClasses,
			'viewBox': '0 0 ' + boxSize + ' ' + boxSize,
		}, [
			border,
			React.DOM.circle({
				'className': 'background',
				'cx': center,
				'cy': center,
				'r':  radius,
			}),
			React.DOM.path({
				'className': 'progress',
				'transform': 'translate(' + (center) + ', ' + (center) + ')',
				'd': this.getPathDefinition(radius, this.props['progress'])
			})
		])
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.RadialProgressIndicator = React.createFactory(Clipperz.PM.UI.Components.RadialProgressIndicatorClass);

Clipperz.PM.UI.Components.DEFAULT_RADIUS = 10;		// You can resize the SVG object with CSS anyway