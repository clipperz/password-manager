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

'use strict';
Clipperz.Base.module('Clipperz.PM.UI.Components.Cards');

Clipperz.PM.UI.Components.Cards.CommandToolbar = React.createClass({

	//============================================================================

	propTypes: {
//		'label':	React.PropTypes.string.isRequired,
//		'loading':	React.PropTypes.bool,
		'features':		React.PropTypes.array.isRequired,
	},

	features: function () {
		return this.props['features'];
	},
	
	isFeatureEnabled: function (aValue) {
		return (this.features().indexOf(aValue) > -1);
	},

	//----------------------------------------------------------------------------

	getInitialState: function() {
		return {'showCommandMenu': false };
	},

	//----------------------------------------------------------------------------
//	EDIT_CARD	-> archive, edit
//	DELETE_CARD	-> delete
//	ADD_CARD	-> clone
	commands: function () {
		var	archiveLabel = this.props['_isArchived'] ? "restore" : "archive";
		return {
			'delete':	{ 'label': "delete",		'broadcastEvent': 'deleteCard',			'enabled': this.isFeatureEnabled('DELETE_CARD')},
			'archive':	{ 'label': archiveLabel,	'broadcastEvent': 'toggleArchiveCard',	'enabled': this.isFeatureEnabled('EDIT_CARD')},
//			'share':	{ 'label': "share",			'broadcastEvent': 'shareCard' },
			'clone':	{ 'label': "clone",			'broadcastEvent': 'cloneCard',			'enabled': this.isFeatureEnabled('ADD_CARD')},
			'edit':		{ 'label': "edit",			'broadcastEvent': 'editCard',			'enabled': this.isFeatureEnabled('EDIT_CARD')}
		};
	},

	//----------------------------------------------------------------------------

	exit: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'goBackToMainPage', {'reference':this.props['_reference']});
	},
	
	toggleMenu: function (anEvent) {
		this.setState({'showCommandMenu': !this.state['showCommandMenu'] });
	},
	
	selectCommandItem: function (anEvent) {
//console.log("SELECT COMMAND ITEM", anEvent.currentTarget.dataset['broadcastEvent'], this.props['_reference']);
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, anEvent.currentTarget.dataset['broadcastEvent'], {'reference':this.props['_reference']});
	},

	//----------------------------------------------------------------------------

	renderCommands: function (shouldReverseCommandOrder) {
		var	commandHandler	= this.selectCommandItem;
		var	commandValues	= MochiKit.Base.values(this.commands());
		
		if (shouldReverseCommandOrder == true) {
			commandValues = MochiKit.Iter.reversed(commandValues);
		}
		
		return	React.DOM.ul({}, MochiKit.Base.map(function (aCommand) {
					var classes = {};
					classes[aCommand['broadcastEvent']] = true;
					classes['enabled'] = aCommand['enabled'];
					classes['disabled'] = !aCommand['enabled'];

					return React.DOM.li({'className':React.addons.classSet(classes), 'onClick':aCommand['enabled'] ? commandHandler : null, 'data-broadcast-event':aCommand['broadcastEvent']}, [React.DOM.span({}, aCommand['label'])]);
				}, commandValues));
	},

	//----------------------------------------------------------------------------

	renderNarrow: function () {
		return [
			React.DOM.div({}, [
				React.DOM.div({'className':'back', 'onClick': this.exit}, 'back'),
				React.DOM.div({'className':'cardMenuOptions', 'onClick':this.toggleMenu}, 'commands'),
				React.DOM.div({'className':React.addons.classSet({'commandMenu':true, 'show':this.state['showCommandMenu']})}, [
					React.DOM.div({'className':'commandMenuMask', 'onClick':this.toggleMenu}),
					React.DOM.div({'className':'commandMenu'}, this.renderCommands(true))
				])
			])
		]
	},
	
	renderOther: function () {
		return [this.renderCommands()];
	},

	//----------------------------------------------------------------------------

	renderLayout: function (aLayout) {
		var	result;
		
		if (aLayout == 'narrow') {
			result = this.renderNarrow();
		} else {
			result = this.renderOther();
		}
		
		return result;
	},

	render: function () {
		var style = this.props['style'];
		var	classes = {
			'cardDetailToolbar':	true,
			'commands':				true,
		};
		classes[style] = true;

		return React.DOM.div({'className':React.addons.classSet(classes)}, this.renderLayout(style));
	},

	//=========================================================================
});
