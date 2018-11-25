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

Clipperz.Base.module('Clipperz.PM.UI.Components.Pages');

Clipperz.PM.UI.Components.Pages.ErrorPageClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.Pages.ErrorPage',

	render: function () {
//console.log("ERROR PAGE", this.props);
		return	React.DOM.div({}, [
			React.DOM.header({}, [
				React.DOM.h2({}, 'clipperz')
			]),
			React.DOM.div({}, [
				React.DOM.div({'className':'error-box'}, [
					React.DOM.div({}, [
						React.DOM.p({}, "Ops!"),
						React.DOM.p({}, "Sorry, something went wrong."),
						React.DOM.p({}, "Please reload."),
					]),
					React.DOM.div({'className':'error-message'}, [
						React.DOM.p({}, this.props['error'] ? this.props['error']['message'] : '')
					])
				])
			])
		])
	}
});

Clipperz.PM.UI.Components.Pages.ErrorPage = React.createFactory(Clipperz.PM.UI.Components.Pages.ErrorPageClass);
