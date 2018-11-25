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


if (typeof(Clipperz) == 'undefined') {
	Clipperz = {};
}

//#############################################################################

Clipperz.Set = function(args) {
	args = args || {};
//	MochiKit.Base.bindMethods(this);

	if (args.items != null) {
		this._items = args.items.slice();
	} else {
		this._items = [];
	}

	return this;
}

//=============================================================================

Clipperz.Set.prototype = MochiKit.Base.update(null, {

	//-------------------------------------------------------------------------

	'toString': function() {
		return "Clipperz.Set";
	},

	//-------------------------------------------------------------------------

	'items': function() {
		return this._items;
	},

	//-------------------------------------------------------------------------

	'popAnItem': function() {
		var result;
	
		if (this.size() > 0) {
			result = this.items().pop();
		} else {
			result = null;
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'allItems': function() {
		return this.items();
	},

	//-------------------------------------------------------------------------

	'contains': function(anItem) {
		return (this.indexOf(anItem) != -1);
	},
	
	//-------------------------------------------------------------------------

	'indexOf': function(anItem) {
		var	result;
		var	i, c;
		
		result = -1;

		c = this.items().length;
		for (i=0; (i<c) && (result == -1); i++) {
			if (this.items()[i] === anItem) {
				result = i;
			}
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'add': function(anItem) {
		if (anItem.constructor == Array) {
			MochiKit.Base.map(MochiKit.Base.bind(this,add, this), anItem);
		} else {
			if (! this.contains(anItem)) {
				this.items().push(anItem);
			}
		}
	},

	//-------------------------------------------------------------------------

	'debug': function() {
		var	i, c;
		
		result = -1;

		c = this.items().length;
		for (i=0; i<c; i++) {
			alert("[" + i + "] " + this.items()[i].label);
		}
	},

	//-------------------------------------------------------------------------

	'remove': function(anItem) {
		if (anItem.constructor == Array) {
			MochiKit.Base.map(MochiKit.Base.bind(this.remove, this), anItem);
		} else {
			var	itemIndex;

			itemIndex = this.indexOf(anItem);
			if (itemIndex != -1) {
				this.items().splice(itemIndex, 1);
			}
		}
	},

	//-------------------------------------------------------------------------

	'size': function() {
		return this.items().length;
	},

	//-------------------------------------------------------------------------

	'empty': function() {
		this.items().splice(0, this.items().length);
	},
	
	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
	
	//-------------------------------------------------------------------------
});

