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

Clipperz.Base.module('Clipperz.PM.UI.Components.Cards');

Clipperz.PM.UI.Components.Cards.Detail = React.createClass({

	viewComponentProps: function () {
		var	result;
		
		result = this.props['selectedCard'];
		if (result) {
			result['style'] = this.props['style'];
			result['ask'] = (this.props['style'] == 'narrow') ? this.props['ask'] : null;
			result['showGlobalMask'] = this.props['showGlobalMask'];
		}
		
		return result;
	},
	
	render: function () {
		var	result;
		if (this.props['mode'] == 'edit') {
			result = Clipperz.PM.UI.Components.Cards.Edit(this.viewComponentProps());
		} else {
			result = Clipperz.PM.UI.Components.Cards.View(this.viewComponentProps());
		}
		
		return result;
	},
});
