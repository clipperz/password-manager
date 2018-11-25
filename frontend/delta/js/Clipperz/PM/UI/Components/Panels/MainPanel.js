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
Clipperz.Base.module('Clipperz.PM.UI.Components.Panels');

Clipperz.PM.UI.Components.Panels.MainPanelClass = React.createClass({

	//=========================================================================

	displayName: 'Clipperz.PM.UI.Components.Panels.MainPanel',

	propTypes: {
		'allTags':				React.PropTypes.array,
		'messageBox':			React.PropTypes.object.isRequired,
		'featureSet':			React.PropTypes.oneOf(['FULL', 'EXPIRED', 'TRIAL']).isRequired,
		'features':				React.PropTypes.array.isRequired,
		'style':				React.PropTypes.oneOf(Clipperz_PM_UI_availableStyles).isRequired,
		'attachmentQueueInfo':	React.PropTypes.object.isRequired,
	},

	style: function () {
		return this.props['style'];
	},

	featureSet: function () {
		return this.props['featureSet'];
	},

	features: function () {
		return this.props['features'];
	},
	
	isFeatureEnabled: function (aValue) {
		return (this.features().indexOf(aValue) > -1);
	},

	handleMaskClick: function () {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'maskClick');
	},

	handleAddCardClick: function () {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'addCardClick');
	},

	//----------------------------------------------------------------------------

	renderToolbarFrame: function (anInnerComponent) {
		return React.DOM.div({'className':'cardToolbarFrame'}, [
			this.renderToolbar(),
			anInnerComponent
		]);
	},

	renderCardFrame: function (firstColumnComponents, secondColumnComponents) {
		var	addCardButton;
		var	cardColumnClasses;

//		if ((this.props['featureSet'] != 'EXPIRED') && (this.props['featureSet'] != 'OFFLINE')) {
		if (this.isFeatureEnabled('ADD_CARD')) {
			addCardButton = React.DOM.div({'className': 'addCardButton', 'onClick':this.handleAddCardClick}, 'add card');
		} else {
			addCardButton = null;
		}

		cardColumnClasses = {
			'cardListColumn':	true,
			'column':			true,
			'addCard':			this.isFeatureEnabled('ADD_CARD')
		}
		return React.DOM.div({'key':'cardContent', 'className':'cardContent'}, [
			React.DOM.div({'className':Clipperz.PM.UI.Components.classNames(cardColumnClasses)}, [addCardButton, firstColumnComponents]),
			React.DOM.div({'className':'cardDetail column right'}, secondColumnComponents)
		])
	},

	//----------------------------------------------------------------------------

	renderToolbar: function () {
		var	cardToolbarProps;
		
		cardToolbarProps = MochiKit.Base.merge(this.props, {
			'key':				'toolbar',
			'style':			this.style(),
//			'enableSidePanels':	(this.props['featureSet'] != 'EXPIRED')
			'enableSidePanels':	true,
		});
		
		return Clipperz.PM.UI.Components.CardToolbar(cardToolbarProps);
	},

	renderExpiredPanel: function () {
		return this.featureSet() == 'EXPIRED' ? Clipperz.PM.UI.Components.ExpiredPanel(this.props) : null;
	},

	//----------------------------------------------------------------------------

	renderCardDetail: function () {
		return Clipperz.PM.UI.Components.Cards.Detail(this.props);
	},

	//----------------------------------------------------------------------------

	renderExtraWide: function () {
		return [
			React.DOM.div({'className':'selection subpanel'}, [Clipperz.PM.UI.Components.Selections(this.props)]),
			React.DOM.div({'className':'cardContent subpanel'}, [
				this.renderToolbarFrame(
					this.renderCardFrame(
						[Clipperz.PM.UI.Components.Cards.List(this.props)],
						[
							this.renderExpiredPanel(),
							this.renderCardDetail()
						]
					)
				)
			])
		]
	},

	//----------------------------------------------------------------------------

	renderWide: function () {
		return [
			this.renderToolbarFrame(
				this.renderCardFrame(
					[Clipperz.PM.UI.Components.Cards.List(this.props)],
					[
						this.renderExpiredPanel(),
						this.renderCardDetail()
					]
				)
			)
		];
	},

	//----------------------------------------------------------------------------

	renderNarrow: function () {
		return this.renderCardFrame(
			this.renderToolbarFrame([
				this.renderExpiredPanel(),
				Clipperz.PM.UI.Components.Cards.List(this.props),
			]),
			this.renderCardDetail()
		);
	},
	
	//----------------------------------------------------------------------------

	renderLayout: function (aLayout) {
		var	result;
		
		if (aLayout == 'extra-wide') {
			result = this.renderExtraWide();
		} else if (aLayout == 'wide') {
			result = this.renderWide();
		} else if (aLayout == 'narrow') {
			result = this.renderNarrow();
//		} else if (aLayout == 'extra-short') {
//			result = this.renderNarrow();
		} else {
			Clipperz.Base.exception.raise('UnknownType');
		}
		
		return result;
	},

	render: function () {
//console.log("MainPanel.render", this.props['showHelp']);
		var	classes = {
			'panel':	true,
			'left':		this.props['selectionPanelStatus'] == 'OPEN',
			'right':	this.props['settingsPanelStatus']  == 'OPEN',
			'open':		this.props['selectionPanelStatus'] == 'OPEN' || this.props['settingsPanelStatus']  == 'OPEN'
		};
		classes[this.style()] = true;

		return	React.DOM.div({'id':'mainPanel', 'className':Clipperz.PM.UI.Components.classNames(classes), 'key':'mainPanel'}, [
			React.DOM.div({'className':'mask', 'onClick':this.handleMaskClick, 'onTouchEnd':this.handleMaskClick, 'key':'mask'}),
			this.props['showHelp'] ? Clipperz.PM.UI.Components.Help(this.props) : null,
			React.DOM.div({'className':'container', 'key':'container'},
//				this.style() == 'extra-wide' ?  this.renderExtraWide() : this.renderOther()
				this.renderLayout(this.style())
			)
		]);
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.Panels.MainPanel = React.createFactory(Clipperz.PM.UI.Components.Panels.MainPanelClass);
