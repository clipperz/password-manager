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

Clipperz.PM.UI.Components.CardList = React.createClass({

	getDefaultProps: function () {
		return {
			selectedCard: null,
			searchDelay: 0.3
		}
	},

	propTypes: {
		searchDelay: React.PropTypes.number
	},

	getInitialState: function () {
		return {
			showSearch: false,
			searchTimer: null,
			searchText: '',
//			passphrase: '',
//			pin: ''
		};
	},

	//=========================================================================

	toggleSearch: function (anEvent) {
		var	showSearchBox;

		showSearchBox = !this.state.showSearch;

		this.setState({showSearch: showSearchBox});

		if (showSearchBox) {
			MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'focusOnSearchField'));
		}
	},

	updateSearchText: function (anEvent) {
		var	searchText;

		searchText = anEvent.target.value;
//console.log(">>> updateSearchText", searchText);

		if ((this.state['searchTimer'] != null) && (searchText != this.state['searchText'])) {
			this.state['searchTimer'].cancel();
		}

		if (searchText != this.state['searchText']) {
			this.state['searchText'] = searchText;
			this.state['searchTimer'] = MochiKit.Async.callLater(this.props['searchDelay'], MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'searchCards', searchText);
		}
	},

	focusOnSearchField: function () {
console.log("focusOnSearchField", this.refs['searchField']);
		this.refs['searchField'].getDOMNode.focus();
	},

	searchBox: function () {
		var result;

		if (this.state.showSearch) {
			result =	React.DOM.div({className:'searchBox'}, [
							React.DOM.div(null, [
								React.DOM.input({type:'search', placeholder:"search", ref:'searchField', onChange:this.updateSearchText})
							])
						]);
		} else {
			result = null;
		}

		return result;
	},

	//=========================================================================

	showPreferences: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'showPreferences', anEvent);
	},

	//=========================================================================

	cardItem: function (aRecordReference) {
		var	reference = aRecordReference['_reference'];
		var	selectedCard = (reference == this.props.selectedCard);

		//	TODO: verify if it is possible to put the onClick handler on the container 'div', instead of adding it to each 'div' item.
		return	React.DOM.div({className:'listItem', key:reference, onClick:MochiKit.Base.method(this, 'handleClickOnCardDetail', reference)}, [
					React.DOM.div({className:'labelWrapper'}, React.DOM.span({className:'label'}, aRecordReference.label)),
//					React.DOM.div({className:'labelWrapper'}, React.DOM.span({className:'label'}, aRecordReference.label + ' ' + aRecordReference.label + ' ' + aRecordReference.label + ' ' + aRecordReference.label + ' ' + aRecordReference.label)),
					React.DOM.div({className:'faviconWrapper'}, aRecordReference.favicon ? React.DOM.img({className:'favicon', src:aRecordReference.favicon}) : React.DOM.div({className:'favicon'}, '\u00A0')),
					React.DOM.div({className:'detailLinkWrapper'}, React.DOM.span({className:'detailLink ' + (selectedCard ? 'icon-spin' : '')}, (selectedCard ? "loading" : "detail")))
				]);
	},

	handleClickOnCardDetail: function (aRecordReference, anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'showRecord', aRecordReference);
	},

	cardListItems: function () {
		var	list;
		var	result;

		list = this.props['cardList'];

		if (typeof(list) != 'undefined') {
			result = MochiKit.Base.map(MochiKit.Base.method(this, 'cardItem'), list);
		} else {
			result = null;
		}

		return result;
	},

	//=========================================================================

	handleChange: function (anEvent) {
//		var	refs = this.refs;
//		var refName = MochiKit.Base.filter(function (aRefName) { return refs[aRefName].getDOMNode() == anEvent.target}, MochiKit.Base.keys(this.refs))[0];
//		var newState = {};
//
//		newState[refName] = event.target.value;
//	    this.setState(newState);
	},

	//=========================================================================

	render: function() {
		return	React.DOM.div(null, [
					React.DOM.div({className:'header'}, [
						React.DOM.a({className:'account'}, 'clipperz'),
						React.DOM.div({className:'features'}, [
//							React.DOM.a({className:'addCard'}, 'add'),
							React.DOM.a({className:'search ' + (this.state.showSearch ? 'selected' : ''), onClick:this.toggleSearch}, 'search'),
							React.DOM.a({className:'settings', onClick:this.showPreferences}, 'settings')
						]),
//						this.searchBox()
					]),
					this.searchBox(),
					React.DOM.div({className:'content cardList'}, this.cardListItems()),
				]);
	}

	//=========================================================================
});
