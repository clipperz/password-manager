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
Clipperz.Base.module('Clipperz.PM.UI.Components.Cards');

Clipperz.PM.UI.Components.Cards.ListClass = React.createClass({

	//=========================================================================

	displayName: 'Clipperz.PM.UI.Components.Cards.List',

	propTypes: {
		'cards':		React.PropTypes.array,
		'featureSet':	React.PropTypes.oneOf(['FULL', 'EXPIRED', 'TRIAL']).isRequired,
		'features':		React.PropTypes.array.isRequired,
		'selectedCard':	React.PropTypes.object
	},

	features: function () {
		return this.props['features'];
	},
	
	isFeatureEnabled: function (aValue) {
		return (this.features().indexOf(aValue) > -1);
	},

	handleClick: function (anEvent) {
		if (this.isFeatureEnabled('CARD_DETAILS')) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'selectCard', {'cardInfo':{'reference':anEvent.currentTarget.dataset.reference, 'label':anEvent.currentTarget.dataset.label}, 'update':true});
		}
	},
	
	renderItem: function (anItem) {
		var	result;

		if (anItem['_isBrandNew'] == true) {
			result = null;
		} else {
			var	classes = {
				'selected':	this.props['selectedCard'] ? this.props['selectedCard']['_reference'] == anItem['_reference'] : false,
				'archived':	anItem['_isArchived']
			};		

			result = React.DOM.li({'className':Clipperz.PM.UI.Components.classNames(classes), 'onClick': this.handleClick, 'key':anItem['_reference'], 'data-reference':anItem['_reference'], 'data-label':anItem['label']}, [
//				React.DOM.span({'className':'favicon'}, Clipperz.PM.UI.Components.Cards.FavIcon({'src':anItem['favicon']})),
				React.DOM.span({'className':'label'}, anItem['label']),
				anItem['hasBeenCertified'] != '' ? React.DOM.span({'className':'certificateStatus'}, 'certificate') : null,
				anItem['attachmentsCount'] ? React.DOM.span({'className':'attachmentsCount'}, 'attachment') : null,
			]);
		}
		
		return result;
	},

	render: function () {
		var	cards = this.props['cards'] ? this.props['cards'] : [];
		var	classes = {
			'cardList':		true,
			'loadingCard':	this.props['selectedCard'] && this.props['selectedCard']['_reference'] && this.props['selectedCard']['loading'],
			'EXPIRED':		this.props['featureSet'] == 'EXPIRED',
		};		
		classes[this.props['style']] = true;

		return	React.DOM.div({'key':'cardList', 'className':Clipperz.PM.UI.Components.classNames(classes)}, [
					React.DOM.div({'className':'cardListInnerWrapper'}, [
						this.isFeatureEnabled('LIST_CARDS') ? React.DOM.ul({}, MochiKit.Base.map(this.renderItem, cards)) : null
					])
				]);
	},
	
	//=========================================================================
});

Clipperz.PM.UI.Components.Cards.List = React.createFactory(Clipperz.PM.UI.Components.Cards.ListClass);