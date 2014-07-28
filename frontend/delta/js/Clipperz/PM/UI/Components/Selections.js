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

Clipperz.PM.UI.Components.Selections = React.createClass({

	//=========================================================================

	selectAll: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectAllCards');
	},
	
	selectRecent: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectRecentCards');
	},

	render: function () {
//console.log("Selections", this.props);
		return	React.DOM.div({'key':'selections', 'id':'selections'}, [
			React.DOM.ul({'className':'defaultSet'}, [
				React.DOM.li({'className':'allCards', onClick: this.selectAll}, "All"),
				React.DOM.li({'className':'recentCards', onClick: this.selectRecent},  "Recent")
			]),
			React.DOM.div({'className':'search'}, [
				React.DOM.form({'className':'searchForm'}, [
					React.DOM.label({'htmlFor':'searchValue'}, 'search'),
					React.DOM.input({'type':'text', 'id':'searchValue', 'name':'search'})
				])
			]),
			React.DOM.ul({'className':'tagList'}, MochiKit.Base.map(function (aTag) { return Clipperz.PM.UI.Components.TagIndexItem({'label':aTag}); }, this.props['tags'] ? this.props['tags'] : []))
		]);
	}

	//=========================================================================
});
