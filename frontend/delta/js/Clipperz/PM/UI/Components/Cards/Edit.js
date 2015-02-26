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

Clipperz.PM.UI.Components.Cards.Edit = React.createClass({

	//============================================================================

	propTypes: {
//		'label':	React.PropTypes.string /*.isRequired */ ,
//		'loading':	React.PropTypes.bool,
	},

	getInitialState: function() {
		return {
			'draggedFieldReference': null,
			'fromFieldPosition': -1,
			'toFieldPosition': -1,
			'dropPosition': -1,
		};
	},

	//----------------------------------------------------------------------------

	record: function () {
		return this.props['_record'];
	},

	fields: function () {
		return this.props['fields'];
	},

	//============================================================================

	positionOfField: function (aFieldReference) {
		return MochiKit.Base.map(MochiKit.Base.itemgetter('_reference'), this.fields()).indexOf(aFieldReference);
	},
	
	//============================================================================

	dragStart: function (anEvent) {
		var	fieldReference = anEvent.currentTarget.dataset['reference'];
		var fieldPosition = this.positionOfField(fieldReference);
		
		var x = anEvent.clientX - anEvent.currentTarget.getBoundingClientRect().left;
		var y = anEvent.clientY - anEvent.currentTarget.getBoundingClientRect().top;
		anEvent.dataTransfer.setDragImage(anEvent.currentTarget, x, y);

		MochiKit.Async.callLater(0.1, MochiKit.Base.bind(this.setState, this, {
			'draggedFieldReference': fieldReference,
			'fromFieldPosition': fieldPosition,
			'toFieldPosition': -1,
			'dropPosition': -1
		}));
	},
/*
	drag: function (anEvent) {
//console.log("DRAG", anEvent);
	},
	drop: function (anEvent) {
console.log("DROP");	//, anEvent);
	},
*/
	dragEnd: function (anEvent) {
		if (this.state['toFieldPosition'] != -1) {
			var	reference = this.props['_reference'];
//console.log("MOVE FIELD POSITION", this.state['toFieldPosition'], this.state['draggedFieldReference']);
			Clipperz.Async.callbacks("Clipperz.PM.UI.Components.Cards.Edit.dragEnd-moveFieldToPosition", [
				MochiKit.Base.method(this.record(), 'moveFieldToPosition', this.state['draggedFieldReference'], this.state['toFieldPosition']),
				MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', reference),
			], {trace:false});
		} else {
//console.log("CANCELLED FIELD MOVE");
		}

		this.setState({
			'draggedFieldReference': null,
			'fromFieldPosition': -1,
			'toFieldPosition': -1,
			'dropPosition': -1
		})
	},

	//............................................................................
/*
	dragEnter: function (anEvent) {
//console.log("DRAG ENTER", anEvent.currentTarget.dataset['reference'], this.positionOfField(anEvent.currentTarget.dataset['reference']));
//		this.setState({'toFieldPosition': this.positionOfField(anEvent.currentTarget.dataset['reference'])});
	},
*/
	dragOver: function (anEvent) {
		var	toFieldPosition;
		var	dropPosition;
		
		if (typeof(anEvent.currentTarget.dataset['index']) != 'undefined') {
			var	y = anEvent.clientY - anEvent.currentTarget.getBoundingClientRect().top;
			var	h = anEvent.currentTarget.getBoundingClientRect().height;
			var	hoveringIndex;
			var	draggingIndex;
			var	isHoveringTopPart;
			
			hoveringIndex = +anEvent.currentTarget.dataset['index'];
			draggingIndex = +this.state['fromFieldPosition'];
			
			isHoveringTopPart = (y < h/2);
			
			if (isHoveringTopPart) {
				dropPosition = hoveringIndex;
			} else {
				dropPosition = hoveringIndex + 1;
			}
			
			if (hoveringIndex > draggingIndex) {
				dropPosition = dropPosition - 1;
			}
			
			toFieldPosition = -1;
//console.log(hoveringIndex, draggingIndex, isHoveringTopPart, dropPosition);
//console.log("isHoveringTopPart", isHoveringTopPart);
		} else {
			dropPosition = anEvent.currentTarget.dataset['dropIndex'];
			toFieldPosition = dropPosition;
		}

		if ((dropPosition != this.state['dropPosition']) || (toFieldPosition != this.state['toFieldPosition'])) {
			this.setState({'dropPosition': dropPosition, 'toFieldPosition':toFieldPosition});
		}
		
		anEvent.stopPropagation();
	},
/*
	dragLeave: function (anEvent) {
//console.log("DRAG LEAVE", anEvent.currentTarget.dataset['reference'], this.positionOfField(anEvent.currentTarget.dataset['reference']));
//		this.setState({'dropPosition': -1});
	},
*/
	//============================================================================
/*
	dragStartDropTarget: function (anEvent) {
//console.log("TARGET: DRAG START");
	},

	dragDropTarget: function (anEvent) {
//console.log("TARGET: DRAG");
	},

	dropDropTarget: function (anEvent) {
//console.log("TARGET: DROP");
	},

	dragEndDropTarget: function (anEvent) {
//console.log("TARGET: DRAG END");
	},

	//............................................................................

	dragEnterDropTarget: function (anEvent) {
//console.log("TARGET: DRAG ENTER");
	},
*/
	dragOverDropTarget: function (anEvent) {
		var	toFieldPosition = anEvent.currentTarget.dataset['dropIndex'];
		
		if (toFieldPosition != this.state['toFieldPosition']) {
//console.log("TARGET: DRAG OVER - READY TO DROP", anEvent.currentTarget.dataset['dropIndex']);
			this.setState({'toFieldPosition':toFieldPosition});
		}

		anEvent.stopPropagation();
	},
	
	dragLeaveDropTarget: function (anEvent) {
//console.log("TARGET: DRAG LEAVE");
		if (-1 != this.state['toFieldPosition']) {
//console.log("READY TO DROP", anEvent.currentTarget.dataset['dropIndex']);
			MochiKit.Async.callLater(0.5, MochiKit.Base.bind(function () {
//console.log("TARGET: DRAG LEAVE #####");
				this.setState({'toFieldPosition':-1});
			}, this))
		}
	},

	//============================================================================

	handleChange: function (anObject , aMethodName) {
		var	reference = this.props['_reference'];
		var	method = MochiKit.Base.method(anObject, aMethodName);
		
		return function (anEvent) {
			method(anEvent.target.value);
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', reference);
		};
	},

	removeField: function (aField) {
		var	reference = this.props['_reference'];
		var	record = this.record();
		
		return function (anEvent) {
			record.removeField(aField);
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', reference);
		};
	},

	addNewField: function (anEvent) {
		var	reference = this.props['_reference'];

		this.record().addField({'label':"", 'value':"", 'isHidden':false});
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', reference);
	},

	toggleLock: function (aField) {
		var	reference = this.props['_reference'];
		
		return function (anEvent) {
//console.log("FIELD", aField.isHidden(), aField);
//			aField.setIsHidden(!aField.isHidden());
//			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', reference);

			return Clipperz.Async.callbacks("Clipperz.PM.UI.Components.Cards.Edit.toggleLock", [
				MochiKit.Base.method(aField, 'isHidden'),
				MochiKit.Base.operator.lognot,
				MochiKit.Base.method(aField, 'setIsHidden'),
				function (aValue) {
					MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', reference);
				},
			], {trace:false});

		};
	},

	//============================================================================

	renderLabel: function (aLabel) {
		return	React.DOM.input({'className':'cardLabel', 'onChange':this.handleChange(this.record(), 'setLabel'), 'defaultValue':aLabel, 'key':this.props['_reference'] + '_label', 'placeholder': "card title"});
	},
	
	renderNotes: function (someNotes) {
//		return	React.DOM.textarea({'className':'cardNotes', 'onChange':this.handleChange(this.record(), 'setNotes'), 'defaultValue':someNotes, 'key':this.props['_reference'] + '_notes', 'placeholder': "notes"});
		return	Clipperz.PM.UI.Components.Cards.TextArea({'className':'cardNotes', 'onChange':this.handleChange(this.record(), 'setNotes'), 'defaultValue':someNotes, 'key':this.props['_reference'] + '_notes', 'placeholder': "notes"});
	},

	//............................................................................

	renderTags: function (someTags) {
		var	tags;
		var	allTags;

		tags = MochiKit.Base.filter(Clipperz.PM.DataModel.Record.isRegularTag, someTags).sort(Clipperz.Base.caseInsensitiveCompare);
		allTags = tags;
		return	Clipperz.PM.UI.Components.Cards.TagEditor({'selectedTags':tags, 'allTags':allTags, 'readOnly':false });
	},

	//............................................................................

	renderField: function (aField) {
		var	ref = aField['_reference'];
		var	cardFieldClasses = {};
		var	cardFieldValueClasses = {};
		var	field = aField['_field'];

//console.log("RENDER FIELD", aField);
		cardFieldClasses['cardField'] = true;
		cardFieldClasses[aField['actionType']] = true;
		cardFieldClasses['hidden'] = aField['isHidden'];
		if (this.state['draggedFieldReference'] == aField['_reference']) {
			cardFieldClasses['dragged'] = true;
		}

		cardFieldValueClasses['fieldValue'] = true;
		cardFieldValueClasses[aField['actionType']] = true;
		cardFieldValueClasses['hidden'] = aField['isHidden'];

		return	React.DOM.div({'className':React.addons.classSet(cardFieldClasses), 'draggable':true, 'key':ref,
								'data-reference':ref,
								'data-document-id':ref,
								'data-index':this.positionOfField(ref),

								'onDragStart':this.dragStart,
//								'onDrag':this.drag,
//								'onDragEnter':this.dragEnter,
								'onDragOver':this.dragOver,
//								'onDragLeave':this.dragLeave,
//								'onDrop':this.drop,
								'onDragEnd':this.dragEnd
		}, [
			React.DOM.div({'className':'fieldEditAction'}, [
				React.DOM.span({'className':'removeField', 'onClick':this.removeField(field)}, "remove field"),
				React.DOM.span({'className':'toggleLock', 'onClick':this.toggleLock(field)}, aField['isHidden'] ? "locked" : "unlocked"),
			]),
			React.DOM.div({'className':'fieldValues'}, [
				React.DOM.div({'className':'fieldLabel'}, [
					React.DOM.input({'_className_':'_fieldLabel_', 'onChange':this.handleChange(field, 'setLabel'), 'defaultValue':aField['label'], 'placeholder': "label"}),
				]),
				React.DOM.div({'className':'fieldValue'}, [
//					React.DOM.textarea({'className':React.addons.classSet(cardFieldValueClasses), 'onChange':this.handleChange(field, 'setValue'), 'defaultValue':aField['value'], 'placeholder': "value"}),
					Clipperz.PM.UI.Components.Cards.TextArea({'className':React.addons.classSet(cardFieldValueClasses), 'onChange':this.handleChange(field, 'setValue'), 'defaultValue':aField['value'], 'placeholder': "value"}),
				])
			]),
			React.DOM.div({'className':'fieldAction action'}, aField['actionType'].toLowerCase())
		]);
	},

	updateRenderedFieldsWithDropArea: function (someRenderedFields) {
		var	dragFrom = this.state['fromFieldPosition']
		var dropTo = this.state['dropPosition'];
		
		var dropAreaPositionIndex = dropTo != -1 ? dropTo : dragFrom;
		var	dropArea =	React.DOM.div({'className':'dropArea', 'key':'fieldDropArea',
							'data-drop-index':dropAreaPositionIndex,

//							'onDragStart':this.dragStartDropTarget,
//							'onDrag':this.dragDropTarget,
//							'onDragEnter':this.dragEnterDropTarget,
							'onDragOver': this.dragOverDropTarget,
							'onDragLeave': this.dragLeaveDropTarget,
//							'onDrop': this.dropDropTarget,
//							'onDragEnd':this.dragEndDropTarget
							
						});

		var dropAreaNodeIndex = (dropAreaPositionIndex < dragFrom) ? dropAreaPositionIndex : dropAreaPositionIndex + 1;
//console.log("DROP", dropTo, dropAreaPositionIndex);
		someRenderedFields.splice(dropAreaNodeIndex, 0, dropArea);
		
		return someRenderedFields;
	},
	
	renderFields: function (someFields) {
		var	renderedFields;
		
		renderedFields = MochiKit.Base.map(this.renderField, someFields);
		
		if (this.state['draggedFieldReference'] != null) {
			renderedFields = this.updateRenderedFieldsWithDropArea(renderedFields);
		}
		return	React.DOM.div({'className':'cardFields' /*, 'dropzone':'move'*/}, renderedFields);
	},

	renderAddNewField: function () {
		return	React.DOM.div({'className':'newCardField', 'onClick':this.addNewField}, "add new field");
	},

	//............................................................................

	renderDirectLogin: function (aDirectLogin) {
		return	React.DOM.div({'className':'cardDirectLogin', 'key':aDirectLogin['_reference']}, [
			React.DOM.span({'className':'directLoginLabel'}, aDirectLogin['label']),
//			React.DOM.div({'className':'directLoginAction action'}, 'DIRECT LOGIN')
		]);
	},
	
	renderDirectLogins: function (someDirectLogins) {
		return	React.DOM.div({'className':'cardDirectLogins'}, MochiKit.Base.map(this.renderDirectLogin, someDirectLogins));
	},
	
	//............................................................................

	render: function () {
		var	classes = {
			'edit':	true
		}

//console.log("RENDER CARD EDIT", this.props['showGlobalMask']);
		return	React.DOM.div({'className':'editWrapper'}, [
			this.props['showGlobalMask'] ? null : React.DOM.div({'className':'mask'}),
			React.DOM.div({'className':React.addons.classSet(classes)},[
				Clipperz.PM.UI.Components.Cards.EditToolbar(this.props),
				React.DOM.div({'className':'content'}, [
					this.renderLabel(this.props['label']),
					this.renderTags(this.props['tags']),
					this.renderFields(this.fields()),
					this.renderAddNewField(),
					this.renderNotes(this.props['notes']),
					this.renderDirectLogins(this.props['directLogins'])
				])
			]),
			this.props['ask'] ? Clipperz.PM.UI.Components.DialogBox(this.props['ask']) : null
		]);
	},
	
	//=========================================================================
});
