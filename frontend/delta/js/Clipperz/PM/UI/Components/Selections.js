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

	selectUntaggedCards: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectUntaggedCards');
	},

	handleCheckboxChanges: function (anEvent) {
		if (!this.props['shouldIncludeArchivedCards']) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'showArchivedCards');
		} else {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'hideArchivedCards');
		}
	},

	handleSearchChange: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'search', anEvent.currentTarget.value);
	},
	
	render: function () {
		var	tagInfo;
		var	tags;
		var	archivedCardsCount;
		var	selectedCardCount;
		var	filterType;

		tagInfo = this.props['tags'] ? this.props['tags'] : {};
		tags = MochiKit.Base.filter(Clipperz.PM.DataModel.Record.isRegularTag, MochiKit.Base.keys(tagInfo)).sort(Clipperz.Base.caseInsensitiveCompare);
		archivedCardsCount = this.props['archivedCardsCount'];
		selectedCardCount = this.props['cards'] ? this.props['cards'].length : 0;
		filterType = (this.props['filter'] && this.props['filter']['type']) ? this.props['filter']['type'] : 'ALL';

		return	React.DOM.div({'key':'selections', 'id':'selections', 'className':filterType}, [
			React.DOM.ul({'className':'defaultSet'}, [
				React.DOM.li({'className':'allCards', 'onClick': this.selectAll}, "All"),
				React.DOM.li({'className':'recentCards', 'onClick': this.selectRecent}, "Recent"),
				React.DOM.li({'className':'untaggedCards', 'onClick': this.selectUntaggedCards}, "Untagged - " + this.props['untaggedCardsCount'])
			]),
			React.DOM.div({'className':'search'}, [
				React.DOM.form({'className':'searchForm'}, [
					React.DOM.div({}, [
						React.DOM.label({'htmlFor':'searchValue'}, 'search'),
						React.DOM.input({'type':'text', 'id':'searchValue', 'onFocus':this.handleSearchChange, 'onChange':this.handleSearchChange, 'name':'search'})
					]),
					React.DOM.div({}, [ React.DOM.span({'className':'count'}, selectedCardCount) ])
				])
			]),
			React.DOM.ul({'className':'tagList'}, MochiKit.Base.map(function (aTag) {return Clipperz.PM.UI.Components.TagIndexItem({'label':aTag, 'count':tagInfo[aTag]}); }, tags)),
			React.DOM.div({'className':'showArchivedCards', 'onClick':this.handleCheckboxChanges}, [
				React.DOM.input({'type':'checkbox', 'checked':this.props['shouldIncludeArchivedCards'] ? 'checked' : null}),
				React.DOM.span({'className':'label'}, "Show archived cards"),
				archivedCardsCount > 0 ? React.DOM.span({'className':'count'}, archivedCardsCount) : null
			]),
		]);
	}

	//=========================================================================
});
