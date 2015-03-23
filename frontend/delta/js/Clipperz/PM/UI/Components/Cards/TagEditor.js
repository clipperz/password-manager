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

Clipperz.PM.UI.Components.Cards.TagEditorClass = React.createClass({

	//============================================================================

	propTypes: {
		'allTags':				React.PropTypes.array,
		'selectedTags':			React.PropTypes.array.isRequired,
		'readOnly':				React.PropTypes.bool.isRequired,
		'updateTagsCallback':	React.PropTypes.func,
	},

	//----------------------------------------------------------------------------

	isReadOnly: function () {
		return this.props['readOnly'];
	},

	//----------------------------------------------------------------------------

	listOfTagsNotUsedYet: function () {
		var	selectedTags = this.props['selectedTags'];
//console.log("ALL TAGS", this.props['allTags']);
//console.log("SELECTED TAGS", this.props['selectedTags']);
		return MochiKit.Base.filter(function (aTag) { return selectedTags.indexOf(aTag) == -1 }, this.props['allTags']);
//		return this.props['allTags'];
	},
	
	//----------------------------------------------------------------------------

	removeTagHandler: function (anEvent) {
		this.removeTag(anEvent.currentTarget.dataset['label']);
	},

	addTag: function (aTag) {
//console.log("ADD TAG", aTag);
		//	TODO: here we may need to include the record or its reference to let the MainController handle it properly.
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'addTag', aTag);
	},
	
	removeTag: function (aTag) {
//console.log("REMOVE TAG", aTag);
		//	TODO: here we may need to include the record or its reference to let the MainController handle it properly.
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'removeTag', aTag);
	},

	//----------------------------------------------------------------------------

	addTagValue: function (anEvent) {
		this.addTag(anEvent.currentTarget.value);
		anEvent.currentTarget.value = "";
	},

	handleKeyDown: function (anEvent) {
		switch (anEvent.keyCode) {
			
			case 9: // tab
				this.addTagValue(anEvent);
				break;
			
			case 13: // enter
				this.addTagValue(anEvent);
				anEvent.preventDefault();
				break;
/*			
			case 27: // escape
				console.log("ESCAPE");
//				if (this.state.isOpen) {
//					this.closeOnEscape();
//				} else {
//					this.clearValue();
//				}
				break;
			
			case 38: // up
				console.log("UP");
//				this.focusPreviousOption();
				break;
			
			case 40: // down
				console.log("DOWN");
//				this.focusNextOption();
				break;
			
			default:
				return;
*/
		}
	},

	handleBlur: function (anEvent) {
		this.addTagValue(anEvent);
	},

	//----------------------------------------------------------------------------

	renderTag: function (aTag) {
		return	React.DOM.li({'className':'tag'}, [
			React.DOM.span({'className':'tagLabel'}, aTag),
			this.isReadOnly() ? null : React.DOM.span({'className':'tagRemoveButton', 'onClick':this.removeTagHandler, 'data-label':aTag}, 'remove tag')
		])
	},
	
	renderEditField: function () {
		return	[
					React.DOM.input({'type':'text', 'list':'tagListData', 'onKeyDown':this.handleKeyDown, 'onBlur':this.handleBlur, 'placeholder': "tag"}),
					React.DOM.datalist({'id':'tagListData'}, MochiKit.Base.map(function (aTag) { return React.DOM.option({}, aTag); }, this.listOfTagsNotUsedYet()))
				];
	},
	
	render: function () {
		var	classes = {
			'tagEditor':	true,
			'readOnly':		this.props['readOnly'],
			'readWrite':	!this.props['readOnly']
		};
		
		return	React.DOM.div({'className':Clipperz.PM.UI.Components.classNames(classes)}, [
			React.DOM.ul({},[
				MochiKit.Base.map(this.renderTag, this.props['selectedTags']),
			]),
			this.isReadOnly() ? null : this.renderEditField()
		]);
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.Cards.TagEditor = React.createFactory(Clipperz.PM.UI.Components.Cards.TagEditorClass);