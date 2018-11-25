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

Clipperz.Base.module('Clipperz.PM.UI.Components');

Clipperz.PM.UI.Components.CheckboxClass = React.createClass({
//	http://development.tobypitman.com/iphoneCheckboxes/iphoneCheckboxes2.html

	displayName: 'Clipperz.PM.UI.Components.Checkbox',

	propTypes: {
		'checked':		React.PropTypes.bool.isRequired,
		'id':			React.PropTypes.string.isRequired,
		'eventHandler':	React.PropTypes.func.isRequired
	},

	//=========================================================================

	render: function () {
		return	React.DOM.div({className:'checkbox', onClick:this.props['eventHandler']}, [
					React.DOM.input({'name':this.props['id'], 'id':this.props['id'], 'value':this.props['id'], 'type':'checkbox', 'checked':this.props['checked']}),
					React.DOM.label({'className':'check', 'htmlFor':this.props['id']}),
					React.DOM.label({'className':'info', 'htmlFor':this.props['id']}, "enable local storage")
				]);
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.Checkbox = React.createFactory(Clipperz.PM.UI.Components.CheckboxClass);
