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

Clipperz.PM.UI.Components.Cards.EditClass = React.createClass({

	//============================================================================

	displayName: 'Clipperz.PM.UI.Components.Cards.Edit',

	propTypes: {
		'allTags':	React.PropTypes.array,
//		'label':	React.PropTypes.string /*.isRequired */ ,
//		'loading':	React.PropTypes.bool,
	},

	getInitialState: function() {
		return {
			'draggedFieldReference': null,
			'passwordGeneratorFieldReference': null,
			'fromFieldPosition': -1,
			'toFieldPosition': -1,
			'dropPosition': -1,

			'skippedFiles': [],
		};
	},

	//----------------------------------------------------------------------------

	record: function () {
		return this.props['_record'];
	},

	fields: function () {
		return this.props['fields'];
	},

	reference: function() {
		return this.props['_reference'];
	},

	//============================================================================

	positionOfField: function (aFieldReference) {
		return MochiKit.Base.map(MochiKit.Base.itemgetter('_reference'), this.fields()).indexOf(aFieldReference);
	},
	
	//============================================================================

	fieldMoveStart: function(aFieldReference, aFieldPosition, touchStyle) {
		MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'setState', {
			'draggedFieldReference': aFieldReference,
			'fromFieldPosition': aFieldPosition,
			'touchStyle': touchStyle,
			'toFieldPosition': -1,
			'dropPosition': -1
		}));
	},

	dragStart: function (anEvent) {
		var	fieldReference = anEvent.currentTarget.dataset['reference'];
		var fieldPosition = this.positionOfField(fieldReference);
		var dragElement = MochiKit.DOM.getElement(fieldReference);
		
		var x = anEvent.clientX - dragElement.getBoundingClientRect().left;
		var y = anEvent.clientY - dragElement.getBoundingClientRect().top;
//		anEvent.dataTransfer.setDragImage(anEvent.currentTarget, x, y);
		anEvent.dataTransfer.setDragImage(dragElement, x, y);

		anEvent.dataTransfer.setData('Text', ""); // Firefox wants this to be defined

		this.fieldMoveStart(fieldReference, fieldPosition, false);
	},

	touchStart: function (anEvent) {
		var fieldReference = anEvent.currentTarget.dataset['reference'];
		var fieldPosition = this.positionOfField(fieldReference);
		var dragElement = MochiKit.DOM.getElement(fieldReference);

		this.fieldMoveStart(fieldReference, fieldPosition, true);

		anEvent.preventDefault();
		anEvent.stopPropagation();
	},


/*
	drag: function (anEvent) {
//console.log("DRAG", anEvent);
	},
	drop: function (anEvent) {
console.log("DROP");	//, anEvent);
	},
*/
	fieldMoveEnd: function (anEvent) {
		var draggedElement = MochiKit.DOM.getElement(anEvent.target.dataset['reference']);
		var	dragPosition = this.state['dropPosition'];	//	this.state['toFieldPosition']

		draggedElement.style.top = '';

		if (dragPosition != -1) {
			var	reference = this.props['_reference'];
//console.log("MOVE FIELD POSITION", this.state['toFieldPosition'], this.state['draggedFieldReference']);
			Clipperz.Async.callbacks("Clipperz.PM.UI.Components.Cards.Edit.fieldMoveEnd-moveFieldToPosition", [
				MochiKit.Base.method(this.record(), 'moveFieldToPosition', this.state['draggedFieldReference'], dragPosition),
				MochiKit.Base.partial(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', reference),
			], {trace:false});

		} else {
//console.log("CANCELED FIELD MOVE");
		}

		// Delayed because a quick touch would prevent the state to update correctly otherwise (don't know why)
		MochiKit.Async.callLater(0.1, MochiKit.Base.method(this, 'setState', {
				'draggedFieldReference': null,
				'fromFieldPosition': -1,
				'toFieldPosition': -1,
				'dropPosition': -1
			})
		);
	},

	//............................................................................
/*
	dragEnter: function (anEvent) {
//console.log("DRAG ENTER", anEvent.currentTarget.dataset['reference'], this.positionOfField(anEvent.currentTarget.dataset['reference']));
//		this.setState({'toFieldPosition': this.positionOfField(anEvent.currentTarget.dataset['reference'])});
	},
*/
	handleOverField: function(aTarget, aClientY) {
// console.log("OVER FIELD", aTarget.dataset['index']);

		var	toFieldPosition;
		var	dropPosition;

		var dragElement = MochiKit.DOM.getElement(aTarget.dataset['reference']);
// console.log('target', aTarget);
// console.log('element', dragElement);
		var	y = aClientY - dragElement.getBoundingClientRect().top;
		var	h = dragElement.getBoundingClientRect().height;

		var	hoveringIndex;
		var	draggingIndex;
		var	isHoveringTopPart;
		
		hoveringIndex = +aTarget.dataset['index'];
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

//console.log("-- ", dropPosition, this.state['dropPosition'], toFieldPosition, this.state['toFieldPosition']);
		if ((dropPosition != this.state['dropPosition']) || (toFieldPosition != this.state['toFieldPosition'])) {
			this.setState({'dropPosition': dropPosition, 'toFieldPosition': toFieldPosition});
		}
	},

	dragOver: function (anEvent) {
//console.log("DRAG OVER", anEvent);
//console.log("DRAG OVER", anEvent.currentTarget.dataset['index']);
		var	toFieldPosition;
		var	dropPosition;

		if (typeof(anEvent.currentTarget.dataset['index']) != 'undefined') {
			this.handleOverField(anEvent.currentTarget, anEvent.clientY);

//console.log(hoveringIndex, draggingIndex, isHoveringTopPart, dropPosition);
//console.log("isHoveringTopPart", isHoveringTopPart);

		} else {
			// TODO: this case is not handled by touch events
			// (shouldn't this never happen anyway?)

			dropPosition = anEvent.currentTarget.dataset['dropIndex'];
			toFieldPosition = dropPosition;

			//console.log("-- ", dropPosition, this.state['dropPosition'], toFieldPosition, this.state['toFieldPosition']);
			if ((dropPosition != this.state['dropPosition']) || (toFieldPosition != this.state['toFieldPosition'])) {
				this.setState({'dropPosition': dropPosition, 'toFieldPosition': toFieldPosition});
			}
		}

		anEvent.stopPropagation();
	},

	touchMove: function(anEvent) {
		var hoveredElement;

		var touch = anEvent.touches[0];
		var draggedElement = MochiKit.DOM.getElement(anEvent.target.dataset['reference']);
		
		this.setDraggedElementTopValue(draggedElement, touch.clientY);

		hoveredElement = this.getElementBelowDraggedElement(touch.clientX, touch.clientY, draggedElement);

		if (hoveredElement) {
			if (hoveredElement.className == 'dropArea') {
				this.handleOverDropTarget(hoveredElement.dataset['dropIndex']);
			} else {
				var cardFieldElement = this.getCardFieldFromElement(hoveredElement);

				if (cardFieldElement) {
					this.handleOverField(cardFieldElement, anEvent.touches[0].clientY);
				} else {
					// console.log(hoveredElement);
				}
			}
		}

		anEvent.preventDefault();
		anEvent.stopPropagation();
	},

	getElementBelowDraggedElement: function(aClientX, aClientY, aDraggedElement) {
		var result;

		aDraggedElement.style.display = 'none';
		result = document.elementFromPoint(aClientX, aClientY);
		aDraggedElement.style.display = '';

		return result;
	},

	// TODO: This method uses empirical values, it could meke sense to reference actual
	// elements' measures
	setDraggedElementTopValue: function(aDraggedElement, aClientY) {
		var topValue;
		
		topValue = aClientY - aDraggedElement.getBoundingClientRect().height;
		topValue = (this.props.style != 'narrow') ? topValue-40 : topValue+54;
		aDraggedElement.style.top = topValue+'px';
	},

	/** Returns anElement itself if it is a card field, or the closest ancestor of
	 *  anElement holding the data-index property, or false if no such an ancestor
	 *  exists. */
	getCardFieldFromElement: function(anElement) {
		var result;

		if (typeof(anElement.dataset['index']) != 'undefined') {
			result = anElement;
		} else {
			var parentElement = anElement.parentElement;
			if (parentElement) {
				result = this.getCardFieldFromElement(parentElement);
			} else {
				result = false;
			}
		}

		return result;
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
	handleOverDropTarget: function(toFieldPosition) {
//console.log("OVER DROP TARGET", toFieldPosition);

		if (toFieldPosition != this.state['toFieldPosition']) {
//console.log("TARGET: DRAG OVER - READY TO DROP", anEvent.currentTarget.dataset['dropIndex']);
			this.setState({'toFieldPosition':toFieldPosition});
		}
	},

	dragOverDropTarget: function (anEvent) {
//console.log("DRAG OVER DROP TARGET", anEvent.currentTarget.dataset['dropIndex']/*, anEvent*/);
		var	toFieldPosition = anEvent.currentTarget.dataset['dropIndex'];
		
		this.handleOverDropTarget(toFieldPosition);

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

	setValueFromPasswordGenerator: function (aField, aTextAreaRef) {
		var	reference = this.props['_reference'];
		var	self = this;

		return function (aValue) {
			aField.setValue(aValue);
			React.findDOMNode(self.refs[aTextAreaRef]).value = aValue;

			self.setState({'passwordGeneratorFieldReference':null});
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', reference);
		};
	},

	handleChange: function (anObject , aMethodName) {
		var	reference = this.props['_reference'];
		var	method = MochiKit.Base.method(anObject, aMethodName);
		
		return function (anEvent) {
			method(anEvent.target.value);
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'refreshCardEditDetail', reference);
		};
	},

	handleKeyDown: function (aField) {
		var	self = this;

		return function (anEvent) {

			switch (anEvent.keyCode) {
				case 9: // tab
					var	fieldReferences = MochiKit.Base.map(function (aValue) { return aValue['_reference']}, self.fields());
					var	fieldIndex = fieldReferences.indexOf(aField.reference());
					if (fieldIndex == fieldReferences.length - 1) {
						Clipperz.Async.callbacks("Clipperz.PM.UI.Components.Cards.Edit.handleKeyDown", [
							MochiKit.Base.method(aField, 'isEmpty'),
							Clipperz.Async.deferredIf('isEmpty',[
							], [
								MochiKit.Base.method(anEvent, 'preventDefault'),
								MochiKit.Base.method(self, 'addNewField'),
//	TODO: set the focus to the newly created field
//	hints: http://stackoverflow.com/questions/24248234/react-js-set-input-value-from-sibling-component
							])
						], {trace:false});
					}

					break;
			}
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

	showPasswordGenerator: function (aField) {
		var result;

		if (aField['actionType'] == 'PASSWORD') {
			var	reference = this.props['_reference'];
			var	self = this;

			result = function (anEvent) {
				self.setState({'passwordGeneratorFieldReference':aField['_reference']});
			};
		} else {
			result = null;
		}
		
		return result;
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

	closePasswordGenerator: function () {
		this.setState({'passwordGeneratorFieldReference': null});
	},

	removeDirectLogin: function(aDirectLoginReference) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'removeDirectLogin', {'record':this.record(), 'directLoginReference':aDirectLoginReference});
	},

	//============================================================================

	renderLabel: function (aLabel) {
		return	React.DOM.input({'className':'cardLabel', 'autoFocus':true, 'onChange':this.handleChange(this.record(), 'setLabel'), 'defaultValue':aLabel, 'key':this.props['_reference'] + '_label', 'placeholder': "card title"});
	},
	
	renderNotes: function (someNotes) {
//		return	React.DOM.textarea({'className':'cardNotes', 'onChange':this.handleChange(this.record(), 'setNotes'), 'defaultValue':someNotes, 'key':this.props['_reference'] + '_notes', 'placeholder': "notes"});
		return	React.DOM.div({'className':'cardNotes'}, [
			Clipperz.PM.UI.Components.Cards.TextArea({'onChange':this.handleChange(this.record(), 'setNotes'), 'defaultValue':someNotes, 'key':this.props['_reference'] + '_notes', 'placeholder': "notes"})
		]);
	},

	//............................................................................

	cleanupTags: function (someTags) {
		return MochiKit.Base.filter(Clipperz.PM.DataModel.Record.isRegularTag, someTags || []).sort(Clipperz.Base.caseInsensitiveCompare);
	},

	renderTags: function (someTags) {
		return	Clipperz.PM.UI.Components.Cards.TagEditor({'selectedTags':this.cleanupTags(someTags), 'allTags':this.cleanupTags(this.props['allTags']), 'readOnly':false });
	},

	//............................................................................

	renderField: function (aField) {
		var	ref = aField['_reference'];
		var	cardFieldClasses = {};
		var	cardFieldValueClasses = {};
		var	field = aField['_field'];
		var	fieldValueRef = ref + '_textarea';

//console.log("RENDER FIELD", aField);
		cardFieldClasses['cardField'] = true;
		cardFieldClasses[aField['actionType']] = true;
		cardFieldClasses['hidden'] = aField['isHidden'];
		if (this.state['draggedFieldReference'] == aField['_reference']) {
			if (this.state.touchStyle) {
				cardFieldClasses['draggedTouch'] = true;
			} else {
				cardFieldClasses['dragged'] = true;
			}
		}

		cardFieldValueClasses['fieldValue'] = true;
		cardFieldValueClasses[aField['actionType']] = true;
		cardFieldValueClasses['hidden'] = aField['isHidden'];

		return	React.DOM.div({'className':Clipperz.PM.UI.Components.classNames(cardFieldClasses), 'id':ref, 'key':ref,
								'data-reference':ref,
								'data-index':this.positionOfField(ref),
								'onDragOver':this.dragOver,
		}, [
			React.DOM.div({'className':'fieldEditAction'}, [
				React.DOM.span({'className':'removeField', 'onClick':this.removeField(field)}, "remove field"),
				React.DOM.div({'className':'dragHandlerContainer',
								'draggable':true, 
								'data-reference':ref,
								'data-document-id':ref,
								'data-index':this.positionOfField(ref),

								'onDragStart':this.dragStart,
								'onDragEnd':this.fieldMoveEnd,

								'onTouchStart': this.touchStart,
								'onTouchMove': this.touchMove,
								'onTouchEnd': this.fieldMoveEnd,
				}, React.DOM.span({
					'className':'dragHandler',
					'data-reference':ref,
					'data-document-id':ref,
					'data-index':this.positionOfField(ref),
				}, ' '))
			]),
			React.DOM.div({'className':'fieldValues'}, [
				React.DOM.div({'className':'fieldLabel'}, [
					React.DOM.input({'_className_':'_fieldLabel_', 'onChange':this.handleChange(field, 'setLabel'), 'defaultValue':aField['label'], 'placeholder': "label"}),
				]),
				React.DOM.div({'className':'fieldValue'}, [
					(ref == this.state['passwordGeneratorFieldReference']) ? Clipperz.PM.UI.Components.Cards.PasswordGenerator({'field':aField, 'setValueCallback':this.setValueFromPasswordGenerator(field, fieldValueRef), 'closeClallback':this.closePasswordGenerator, 'preferences':this.props['preferences']}) : null,
					Clipperz.PM.UI.Components.Cards.TextArea({'className':Clipperz.PM.UI.Components.classNames(cardFieldValueClasses), 'onChange':this.handleChange(field, 'setValue'), 'onKeyDown':this.handleKeyDown(field), 'defaultValue':aField['value'], 'placeholder':(aField['actionType'].toLowerCase() == 'password')?'':"value", 'ref':fieldValueRef}),
				])
			]),
			React.DOM.div({'className':'fieldAction'}, [
				React.DOM.span({'className':'action ' + aField['actionType'], 'onClick':this.showPasswordGenerator(aField)}, aField['actionType'].toLowerCase() == 'password' ? 'password generator' : aField['actionType'].toLowerCase()),
				React.DOM.span({'className':'toggleLock', 'onClick':this.toggleLock(field)}, aField['isHidden'] ? "locked" : "unlocked")
			])
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
//		return	React.DOM.div({'className':'newCardField', 'onClick':this.addNewField}, "add new field");
		return	React.DOM.div({'className':'newCardField', 'onClick':this.addNewField}, [
			React.DOM.div({'className':'fieldGhostShadow'}, [
				React.DOM.div({'className':'label'}, ""),
				React.DOM.div({'className':'value'}, ""),
			]),
			React.DOM.div({'className':'addNewFieldButton'}, "add new field"),
		]);
	},

	//............................................................................

	renderDirectLogin: function (aDirectLogin) {
		return	React.DOM.div({'className':'cardDirectLogin', 'key':aDirectLogin['_reference']}, [
			React.DOM.a({
				'className': 'removeDirectLogin',
				'onClick': MochiKit.Base.method(this, 'removeDirectLogin', aDirectLogin['_reference'])
			}, "remove field"),
			React.DOM.span({'className':'directLoginLabel'}, aDirectLogin['label']),
//			React.DOM.div({'className':'directLoginAction action'}, 'DIRECT LOGIN')
		]);
	},
	
	renderDirectLogins: function (someDirectLogins) {
		return	React.DOM.div({'className':'cardDirectLogins'}, MochiKit.Base.map(this.renderDirectLogin, someDirectLogins));
	},
	
	//............................................................................

	handleRemoveAttachment: function(anAttachment) {
		// MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'cancelAttachment', anAttachment);
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'removeAttachment', {'record':this.record(), 'attachment': anAttachment});
	},

	//............................................................................

	uploadFiles: function(someFiles) {
		var i;

//console.log("uploadFiles", someFiles);
		var newSkippedFiles = [];

		for (i = 0; i < someFiles.length; i++) {
			var file = someFiles[i];
//console.log("uploadFiles - file", file);

			if (file.size <= Clipperz.PM.DataModel.Attachment.MAX_ATTACHMENT_SIZE) {
				MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'addAttachment', {'record':this.record(), 'file':file});
			} else {
				newSkippedFiles.push(file);
				this.setState({'skippedFiles': newSkippedFiles});
			}
		}

		// TODO: check compatibility with all browsers
		this.refs['attachmentInput'].value = null;
	},

	//............................................................................

	handleFileSelect: function(anEvent) {
		this.uploadFiles(anEvent.target.files);
	},

	//............................................................................

	handleOnDrop: function (anEvent) {
		anEvent.preventDefault();

		this.uploadFiles(anEvent.dataTransfer.files);
	},
	
	handleOnDragOver: function (anEvent) {
		// Somehow necessary:
		// http://enome.github.io/javascript/2014/03/24/drag-and-drop-with-react-js.html
		// https://code.google.com/p/chromium/issues/detail?id=168387
		// http://www.quirksmode.org/blog/archives/2009/09/the_html5_drag.html
		anEvent.stopPropagation();
		anEvent.preventDefault();
		anEvent.dataTransfer.dropEffect = 'copy'; // Explicitly show this is a copy.
	},

	//............................................................................

	renderSkippedFiles: function () {
		var result;

		result = null;
		if (this.state['skippedFiles'].length > 0) {
			result = React.DOM.div({'className': 'skippedFiles'},[
				React.DOM.p({}, "The following files exceed the size limit of " + filesize(Clipperz.PM.DataModel.Attachment.MAX_ATTACHMENT_SIZE)),
				React.DOM.ul({},
					MochiKit.Base.map(function(aFile) {
						return React.DOM.li({}, [
							React.DOM.span({'className': 'filename'}, aFile.name),
							React.DOM.span({}, " (" + filesize(aFile.size) + ")"),
						]);
					}, this.state['skippedFiles'])
				),
				React.DOM.a({
					'onClick': MochiKit.Base.method(this, 'setState', {'skippedFiles': []}),
				}, 'close'),
			]);
		}

		return result;
	},

	renderAttachmentProgress: function (aStatus, aServerStatus, aProgress) {
		var result;

		var queueOperationsInProgress = (aStatus && aStatus != 'DONE' && aStatus != 'CANCELED' && aStatus != 'FAILED');

		result = null;
		if (aStatus == 'UPLOADING' || aStatus == 'DOWNLOADING') {
			result = Clipperz.PM.UI.Components.RadialProgressIndicator({
				'progress': aProgress,
				'border': 1
			});
		} else if (queueOperationsInProgress) {
			result = Clipperz.PM.UI.Components.RadialProgressIndicator({
				'progress': 0,
				'border': 1,
				'additionalClasses': ['waiting'],
			});
		}

		return result;
	},

	renderAttachmentStatus: function (aStatus, aServerStatus, aProgress) {
		var result;

		var status = aStatus ? aStatus : false;
		var queueOperationsInProgress = (status && (status != 'DONE' && status != 'CANCELED' && status != 'FAILED'));

		result = null;

		if (status == 'FAILED') {
			result = React.DOM.span({'className': 'failed'}, "failed");
		} else if (status == 'UPLOADING' || status == 'DOWNLOADING') {
			var actionSymbol = (status == 'UPLOADING') ? "\u2b06" : "\u2b07";
			result = React.DOM.span({'className': 'progressStatus'}, actionSymbol + Math.floor(aProgress*100) + '%');
		} else if (aServerStatus != 'AVAILABLE' && ! this.props['_isBrandNew']) {
			switch(status) {
				case 'CANCELED':
					result = React.DOM.span({'className': 'broken'}, "canceled");
					break;
				case 'DONE':
					result = React.DOM.span({'className': 'done'}, "done");
					break;
				case false:
					result = React.DOM.span({'className': 'broken'}, "failed");
					break;
				default:
					result = React.DOM.span({'className': 'waiting'}, "\u2b06waiting");
			}
		} else if (queueOperationsInProgress) {
			result = React.DOM.span({'className': 'waiting'}, "\u2b07waiting");
		} else if (this.props['_isBrandNew']) {
			result = React.DOM.span({'className': 'waiting'}, "waiting save");
		}

		return result;
	},

	renderAttachmentActions: function (aStatus, aServerStatus, anAttachment) {
		var result;

		result = null;
		if (aStatus  != 'DOWNLOADING') {
			result = React.DOM.a({
				'className': 'remove',
				'onClick': MochiKit.Base.method(this, 'handleRemoveAttachment', anAttachment),
			}, "remove field");
		}

		return result;
	},

	renderAttachment: function (anAttachment) {
		var queueInfo = this.props['attachmentQueueInfo'].elementFetchCallback(anAttachment._reference) || [];
		var queueStatus = queueInfo['status'];
		var serverStatus = this.props['attachmentServerStatus'][anAttachment._reference];
		var queueOperationsInProgress = (queueStatus && queueStatus != 'DONE' && queueStatus != 'CANCELED' && queueStatus != 'FAILED');
		
		var broken = (! serverStatus && ! queueOperationsInProgress && ! this.props['_isBrandNew']);

		return React.DOM.li({
			'className': (broken) ? 'broken' : '',
			'key': anAttachment._reference
		}, [
			React.DOM.span({'className': 'contentType'}, Clipperz.PM.DataModel.Attachment.contentTypeIcon(anAttachment.contentType)),
			React.DOM.span({'className': 'meta'}, [
				React.DOM.span({'className': 'name'}, anAttachment.name),
				React.DOM.span({'className': 'size'}, filesize(anAttachment.size)),
			]),
			React.DOM.span({'className': 'status'},		this.renderAttachmentStatus  (queueStatus, serverStatus, queueInfo['requestProgress'])),
			React.DOM.span({'className': 'progress'},	this.renderAttachmentProgress(queueStatus, serverStatus, queueInfo['requestProgress'])),
			React.DOM.span({'className': 'actions'},	this.renderAttachmentActions (queueStatus, serverStatus, anAttachment['_attachment'])),
		])
	},

	renderAttachments: function(someAttachments) {
		return React.DOM.div({'className':'cardAttachmentWrapper'}, [
			React.DOM.div({'className': 'cardAttachments'}, [
				React.DOM.h3({'className': 'summaryText'}, "Attachments"),
//				React.DOM.p({'className': 'summaryText'}, someAttachments.length + ' files attached'),
				this.renderSkippedFiles(),
				React.DOM.ul({'className': 'attachmentList'},
					MochiKit.Base.map(MochiKit.Base.method(this, 'renderAttachment'), someAttachments)
				)
			]),
			React.DOM.div({
				'className': 'cardUploadAttachments',
				'onClick': MochiKit.Base.bind(function() { this.refs['attachmentInput'].click() }, this),
				'onDragOver': this.handleOnDragOver,
				'onDrop': this.handleOnDrop,
			},[
				React.DOM.p({}, "Drag and drop your files here"),
				React.DOM.p({}, "or"),
				React.DOM.input({
					'type':			'file',
					'id':			'attachmentInput',
					'className':	'attachmentInput',
					'name':			'attachmentInput',
					'ref':			'attachmentInput',
					'onChange':		this.handleFileSelect,
					'multiple':		true
				}),
				React.DOM.a({
					'className': 'button',
					'onDragOver': this.handleOnDragOver,
					'onDrop': this.handleOnDrop,
				}, "select files"),
			])
		])
	},

	//............................................................................

	render: function () {
		var	classes = {
			'edit':	true
		}

		return	React.DOM.div({'className':'editWrapper'}, [
			this.props['showGlobalMask'] ? null : React.DOM.div({'className':'mask'}),
			React.DOM.div({'className':Clipperz.PM.UI.Components.classNames(classes)},[
				Clipperz.PM.UI.Components.Cards.CommandToolbar(this.props),
				Clipperz.PM.UI.Components.Cards.EditToolbar(this.props),
				React.DOM.div({'className':'content'}, [
					this.renderLabel(this.props['label']),
					this.renderTags(this.props['tags']),
					this.renderFields(this.fields()),
					this.renderAddNewField(),
					this.renderAttachments(MochiKit.Base.values(this.props['attachments'])),
					this.renderNotes(this.props['notes']),
					this.renderDirectLogins(this.props['directLogins']),
				])
			]),
			this.props['ask'] ? Clipperz.PM.UI.Components.DialogBox(this.props['ask']) : null
		]);
	},
	
	//=========================================================================
});

Clipperz.PM.UI.Components.Cards.Edit = React.createFactory(Clipperz.PM.UI.Components.Cards.EditClass);