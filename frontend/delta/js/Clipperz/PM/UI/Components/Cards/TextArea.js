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

Clipperz.PM.UI.Components.Cards.TextAreaClass = React.createClass({

	//----------------------------------------------------------------------------

	displayName: 'Clipperz.PM.UI.Components.Cards.TextArea',

	componentDidMount: function() {
		this.recalculateSize();
		this.getDOMNode().addEventListener('input', this.handleKeyDown, false);
		this.getDOMNode().addEventListener('keydown', this.handleKeyDown, false);
	},

	componentDidUpdate: function () {
		this.recalculateSize();
	},

	componentWillUnmount: function() {
		this.getDOMNode().removeEventListener('input', this.recalculateSize, false);
		this.getDOMNode().removeEventListener('keydown', this.handleKeyDown, false);
	},

//	componentDidUpdate: function(prevProps) {
//		if (prevProps.style || prevProps.value !== this.props.value || this.props.value == null) {
//			this.recalculateSize();
//		}
//	},

//	onChange: function(e) {
//		if (this.props.onChange) {
//			this.props.onChange(e);
//		}
//
//		if (this.props.value === undefined) {
//			// controlled mode
//			this.recalculateSize();
//		}
//	},

	handleKeyDown: function (anEvent) {
		switch (anEvent.keyCode) {
			case 27: // escape
				Mousetrap.trigger('esc');
				break;
		}
	},

	//----------------------------------------------------------------------------

	recalculateSize: function () {
		this.recalculateSize_1()
	},

	//	http://maximilianhoffmann.com/posts/autoresizing-textareas
	recalculateSize_1: function () {
		var node = this.getDOMNode();

		node.style.height = '33px';
		node.style.height = node.scrollHeight + 'px';
		window.scrollTo(window.scrollLeft, (node.scrollTop + node.scrollHeight));
	},
/*
	recalculateSize_2: function() {
		var diff;
		var node = this.getDOMNode();

		if (window.getComputedStyle) {
			var styles = window.getComputedStyle(node);

			// If the textarea is set to border-box, it's not necessary to subtract the padding.
			if (
					styles.getPropertyValue('box-sizing') === "border-box"
				||	styles.getPropertyValue('-moz-box-sizing') === "border-box"
				||	styles.getPropertyValue('-webkit-box-sizing') === "border-box"
			) {
				diff = 0;
			} else {
				diff = (	parseInt(styles.getPropertyValue('padding-bottom') || 0, 10)
						+	parseInt(styles.getPropertyValue('padding-top')    || 0, 10)
				);
			}
		} else {
			diff = 0;
		}

//		var node = this.getDOMNode();
		node.style.height = 'auto';
		node.style.height = (node.scrollHeight - diff) + 'px';
	},
*/
	//----------------------------------------------------------------------------

	render: function() {
		return React.DOM.textarea(this.props, this.props.children);
	},

});

Clipperz.PM.UI.Components.Cards.TextArea = React.createFactory(Clipperz.PM.UI.Components.Cards.TextAreaClass);
