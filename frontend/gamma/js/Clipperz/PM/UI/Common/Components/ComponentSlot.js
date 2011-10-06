/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz Community Edition.
Clipperz Community Edition is an online password manager.
For further information about its features and functionalities please
refer to http://www.clipperz.com.

* Clipperz Community Edition is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Clipperz Community Edition is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Clipperz Community Edition.  If not, see
  <http://www.gnu.org/licenses/>.

*/

Clipperz.Base.module('Clipperz.PM.UI.Common.Components');

//#############################################################################


Clipperz.PM.UI.Common.Components.ComponentSlot = function(aComponent, aSlotName) {
	this._component = aComponent;
	this._slotName = aSlotName;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Common.Components.ComponentSlot, Object, {

	//-------------------------------------------------------------------------

	'slotName': function() {
		return this._slotName;
	},

	'component': function() {
		return this._component;
	},

	//-------------------------------------------------------------------------

	'setContent': function(aComponent) {
		this.component().setComponentForSlotNamed(aComponent, this.slotName());
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});
