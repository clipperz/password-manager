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
Clipperz.Base.module('Clipperz.PM.UI.Components.Cards');

Clipperz.PM.UI.Components.Cards.List = React.createClass({

	//=========================================================================

	propTypes: {
		'cards':		React.PropTypes.array,
		'selectedCard':	React.PropTypes.string
	},

	handleClick: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'cardSelected', {'reference':anEvent.currentTarget.dataset.reference, 'label':anEvent.currentTarget.dataset.label});
	},
	
	renderItem: function (anItem) {
		var	classes = {
			'selected':	this.props['selectedCard'] ? this.props['selectedCard']['_reference'] == anItem['_reference'] : false,
			'archived':	anItem['_isArchived']
		};		

		return	React.DOM.li({'className':React.addons.classSet(classes), 'onClick': this.handleClick, 'key':anItem['_reference'], 'data-reference':anItem['_reference'], 'data-label':anItem['label']}, [
			React.DOM.span({'className':'favicon'}, [ React.DOM.img({src:anItem['favicon']})]),
			React.DOM.span({'className':'label'}, anItem['label']),
//			React.DOM.span({'className':'action'}, 'show detail')
		]);
	},

	render: function () {
		var	cards = this.props['cards'] ? this.props['cards'] : [];
		var	classes = {
			'cardList':		true,
			'loadingCard':	this.props['selectedCard'] && this.props['selectedCard']['_reference'] && this.props['selectedCard']['loading']
		};		
		classes[this.props['style']] = true;

		return	React.DOM.div({'key':'cardList', 'className':React.addons.classSet(classes)}, [
					React.DOM.ul({}, MochiKit.Base.map(this.renderItem, cards))
				]);
	},
	
	//=========================================================================
});
