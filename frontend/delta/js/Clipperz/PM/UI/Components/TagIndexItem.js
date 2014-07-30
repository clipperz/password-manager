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

'use strict';
Clipperz.Base.module('Clipperz.PM.UI.Components');

Clipperz.PM.UI.Components.TagIndexItem = React.createClass({

	//=========================================================================

	propTypes: {
		'label':	React.PropTypes.string.isRequired,
		'count':	React.PropTypes.number.isRequired,
	},

	handleClick: function (anEvent) {
//console.log("TAG INDEX ITEM - handle click TAG", anEvent.currentTarget.dataset.tag);
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'tagSelected', anEvent.currentTarget.dataset.tag);
	},
	
	render: function () {
		return	React.DOM.li({'onClick': this.handleClick, 'data-tag':this.props['label']}, [
			React.DOM.span({'className':'tagLabel'}, this.props['label']),
			React.DOM.span({'className':'tagCount'}, this.props['count'])
		]);
	},

	//=========================================================================
});
