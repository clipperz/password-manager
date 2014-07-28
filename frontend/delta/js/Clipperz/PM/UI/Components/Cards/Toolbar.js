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

Clipperz.PM.UI.Components.Cards.Toolbar = React.createClass({

	//============================================================================

	propTypes: {
//		'label':	React.PropTypes.string.isRequired,
//		'loading':	React.PropTypes.bool,
	},

	//----------------------------------------------------------------------------

	getInitialState: function() {
		return {'showCommandMenu': false };
	},
	
	//----------------------------------------------------------------------------

	commands: function () {
		return {
			'delete': {
				'label': "delete",
				'broadcastEvent': 'deleteCard'
			},
			'archive': {
				'label': "archive",
				'broadcastEvent': 'archiveCard'
			},
//			'share': {
//				'label': "share",
//				'broadcastEvent': 'shareCard'
//			},
			'edit': {
				'label': "edit",
				'broadcastEvent': 'editCard'
			}
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
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, anEvent.target.dataset['broadcastEvent'], {'reference':this.props['_reference']});
	},

	//----------------------------------------------------------------------------

	renderCommands: function () {
		var	commandHandler = this.selectCommandItem;
		
		return	React.DOM.ul({}, MochiKit.Base.map(function (aCommand) {
					return React.DOM.li({}, [React.DOM.span({'onClick':commandHandler, 'data-broadcast-event':aCommand['broadcastEvent']}, aCommand['label'])]);
				}, MochiKit.Base.values(this.commands())));
	},

	//----------------------------------------------------------------------------

	renderNarrow: function () {
		return [
			React.DOM.div({}, [
				React.DOM.div({'className':'back', 'onClick': this.exit}, 'back'),
				React.DOM.div({'className':'cardMenuOptions', 'onClick':this.toggleMenu}, 'commands'),
				React.DOM.div({'className':React.addons.classSet({'commandMenu':true, 'show':this.state['showCommandMenu']})}, [
					React.DOM.div({'className':'commandMenuMask', 'onClick':this.toggleMenu}),
					React.DOM.div({'className':'commandMenu'}, this.renderCommands())
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
		};
		classes[style] = true;

		return React.DOM.div({'className':React.addons.classSet(classes)}, this.renderLayout(style));
	},

	//=========================================================================
});
