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

Clipperz.PM.UI.Components.HelpClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.Help',

	close: function (anEvent) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'closeHelp');
	},

	//=========================================================================

	render: function () {
		return	React.DOM.div({'className':'help', 'key':'help'}, [
			React.DOM.div({'className':'helpBox'}, [
				React.DOM.header({}, [
					React.DOM.h3({}, "Keyboard shortcuts"),
					React.DOM.div({'className':'button close', 'onClick':this.close}, "close")
				]),
				React.DOM.div({'className':'helpContent'}, [
					React.DOM.div({'className':'helpBlock'}, [
						React.DOM.h4({}, "Search"),
						React.DOM.dl({}, [
							React.DOM.dt({}, [
								React.DOM.span({'className':'key'}, '/')
							]), React.DOM.dd({}, "search cards"),
							React.DOM.dt({}, [
								React.DOM.span({'className':'key'}, '*'),
//								React.DOM.span({'className':'operator'}, 'then'),
//								React.DOM.span({'className':'key'}, 'a'),
							]), React.DOM.dd({}, "select all cards"),
						])
					]),
					React.DOM.div({'className':'helpBlock'}, [
						React.DOM.h4({}, "Navigation"),
						React.DOM.dl({}, [
							React.DOM.dt({}, [
								React.DOM.span({'className':'key'}, 'h'),
								React.DOM.span({'className':'operator'}, 'or'),
								React.DOM.span({'className':'key'}, '<left>'),
								React.DOM.span({'className':'operator'}, 'or'),
								React.DOM.span({'className':'key'}, '<esc>'),
							]), React.DOM.dd({}, "exit current selection"),
							React.DOM.dt({}, [
								React.DOM.span({'className':'key'}, 'l'),
								React.DOM.span({'className':'operator'}, 'or'),
								React.DOM.span({'className':'key'}, '<right>'),
								React.DOM.span({'className':'operator'}, 'or'),
								React.DOM.span({'className':'key'}, '<enter>'),
							]), React.DOM.dd({}, "select detail"),
							React.DOM.dt({}, [
								React.DOM.span({'className':'key'}, 'k'),
								React.DOM.span({'className':'operator'}, '/'),
								React.DOM.span({'className':'key'}, 'j'),
							]), React.DOM.dd({}, "previous/next card"),
						])
					]),

					React.DOM.div({'className':'helpBlock'}, [
						React.DOM.h4({}, "Misc"),
						React.DOM.dl({}, [
							React.DOM.dt({}, [
								React.DOM.span({'className':'key'}, 'l o c k'),
							]), React.DOM.dd({}, "lock application"),
						])
					]),

/*

	Mousetrap.bind(['left',  'h', 'esc'],	MochiKit.Base.method(this, 'exitCurrentSelection'));
	Mousetrap.bind(['right', 'l', 'enter'],	MochiKit.Base.method(this, 'selectDetail'));

	Mousetrap.bind(['up',    'k'],			MochiKit.Base.method(this, 'selectPreviousCard'));
	Mousetrap.bind(['down',  'j'],			MochiKit.Base.method(this, 'selectNextCard'));


	Mousetrap.bind(['/'],					MochiKit.Base.method(this, 'focusOnSearch'));
	Mousetrap.bind(['* a'],					MochiKit.Base.method(this, 'selectAllCards_handler'));

	Mousetrap.bind(['?'],					MochiKit.Base.method(this, 'showHelp_handler'));
*/
				])
			])
		]);
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.Help = React.createFactory(Clipperz.PM.UI.Components.HelpClass);
