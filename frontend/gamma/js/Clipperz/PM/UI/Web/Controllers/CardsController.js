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

Clipperz.Base.module('Clipperz.PM.UI.Web.Controllers');

Clipperz.PM.UI.Web.Controllers.CardsController = function() {
	Clipperz.PM.UI.Web.Controllers.CardsController.superclass.constructor.apply(this, arguments);

	MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'cardDialogComponentClosed', this, 'handleHideCard');

	return this;
}

Clipperz.Base.extend(Clipperz.PM.UI.Web.Controllers.CardsController, Clipperz.PM.UI.Web.Controllers.GridController, {

	'toString': function() {
		return "Clipperz.PM.UI.Web.Controllers.CardsController";
	},

	'createGrid': function () {
		var grid;
		
		grid = new Clipperz.PM.UI.Web.Components.GridComponent({columnsManagers: [
			new Clipperz.PM.UI.Web.Components.FaviconColumnManager({
				'name':			'Cards.favicon',
				'selector': 	 MochiKit.Base.methodcaller('favicon'),
				'cssClass':		'favicon'
			}),
			new Clipperz.PM.UI.Web.Components.LinkColumnManager({
				'name':			'Cards.title',
				'selector':		 MochiKit.Base.methodcaller('label'),
				'label':		'title',
				'cssClass':		'title',
				'comparator':	 Clipperz.Base.caseInsensitiveCompare,
				'sortable':	 	 true,
				'sorted': 		'ASCENDING',
//				'actionMethod':	 function(anObject, anEvent) { MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'editCard', {objectData:anObject, element:anEvent.src()})}
				'actionMethod':	 MochiKit.Base.method(this, 'handleShowCard')
			}),
			new Clipperz.PM.UI.Web.Components.DirectLoginsColumnManager({
				'name':			'Cards.directLogins',
				'selector':	 	 MochiKit.Base.methodcaller('directLoginReferences'),
				'label':		'direct logins',
				'cssClass':		'directLogin'
			}),
			new Clipperz.PM.UI.Web.Components.DateColumnManager({
				'name':			'Cards.latestUpdate',
				'selector':	 	 MochiKit.Base.methodcaller('updateDate'),
				'label':		'latest update',
				'cssClass':		'latestUpdate',
				'format':		'd-m-Y',
				'comparator':	 MochiKit.Base.compare,
				'sortable':		 true,
				'sorted': 		'UNSORTED'
			}),
			new Clipperz.PM.UI.Web.Components.DeleteObjectColumnManager({
				'name':			'Cards.delete',
				'selector':		 MochiKit.Base.noop,
				'cssClass':		'delete',
//				'actionMethod':  function(anObject, anEvent) { MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'deleteCard', {objectData:anObject, element:anEvent.src()})}
				'actionMethod':  MochiKit.Base.method(this, 'handleDeleteCard')
			})
		]});

		grid.setComponentForSlotNamed(new Clipperz.PM.UI.Web.Components.BookmarkletComponent(), 'headerSlot');

		return grid;
	},

	//-----------------------------------------------------------------------------

	'getRows': function () {
		//TODO relying on user() in GridController, bad code smell :|
		return this.user().getRecords();
	},

	//=============================================================================

	'displayEmptyContent': function () {
		var	emptyGridComponent;
		
		emptyGridComponent = new Clipperz.PM.UI.Web.Components.CreateNewCardSplashComponent();

		return Clipperz.Async.callbacks("CardsController.displayEmptyContent", [
			MochiKit.Base.method(this.grid(), 'setNoRowsGridComponent', emptyGridComponent),
			MochiKit.Base.bind(Clipperz.PM.UI.Web.Controllers.CardsController.superclass.displayEmptyContent, this)
		], {trace:false});
	},

	'displaySelectedRows': function (aFilter) {
		this.columnManagerWithName('Cards.directLogins').hideDirectLoginListPopup();
		
		return Clipperz.PM.UI.Web.Controllers.CardsController.superclass.displaySelectedRows.apply(this, arguments);
	},

	//=============================================================================

	'handleShowCard': function (anObject, anEvent) {
		var cardDialogController;

		cardDialogController = new Clipperz.PM.UI.Web.Controllers.CardDialogController({record:anObject, delegate:this});
		this.grid().selectRow(anObject);

		cardDialogController.run(anEvent.src());
	},

	//.........................................................................

	'handleHideCard': function () {
		this.focus();
	},

	//-----------------------------------------------------------------------------

	'addCard': function (aSourceElement) {
		return Clipperz.Async.callbacks("CardsController.addCard", [
			Clipperz.Async.collectResults("CardsController.addCard <inner results>", {
				'record': MochiKit.Base.method(this.user(), 'createNewRecord'),
				'delegate':	MochiKit.Base.partial(MochiKit.Async.succeed, this)
			}, {trace:false}),
			function (someParameters) {
				return new Clipperz.PM.UI.Web.Controllers.CardDialogController(someParameters);
			},
			MochiKit.Base.methodcaller('run', aSourceElement)
		], {trace:false});
	},

	//-----------------------------------------------------------------------------

	'handleDeleteCard': function (anObject, anEvent) {
		var deferredResult;
		var confirmationDialog;
		
//		confirmationDialog = new Clipperz.PM.UI.Common.Components.SimpleMessagePanel({
		confirmationDialog = new Clipperz.PM.UI.Common.Components.MessagePanelWithProgressBar({
			'title':	"Delete Card",
			'text':		"Do you want to delete …",
			'type':		'ALERT',
			'buttons': [
				{text:"Cancel",	result:'CANCEL'},
				{text:"Delete", result:'OK', isDefault:true}
			],
			'canCancelWhileProcessing':	false
		});

		deferredResult = new Clipperz.Async.Deferred("AppController.handleDeleteCard", {trace:false});
		deferredResult.addCallback(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'initProgress', {'steps':5}),
		deferredResult.addMethod(this.grid(), 'selectRow', anObject);
		deferredResult.addMethod(confirmationDialog, 'deferredShowModal', {
			'openFromElement':			anEvent.src(),
			'onOkCloseToElement':		null,	//	MochiKit.DOM.currentDocument().body,
			'onCancelCloseToElement':	anEvent.src()
		});
//		deferredResult.addCallback(function () { Clipperz.log("DELETE: " + anObject.toString(), anObject); });
		deferredResult.addMethod(this.user(), 'deleteRecord', anObject);
		deferredResult.addBothPass(MochiKit.Base.method(this.grid(), 'unselectRow', anObject));
		deferredResult.addMethod(this, 'saveChanges');
		deferredResult.addMethod(confirmationDialog, 'deferredDone');
		deferredResult.addErrbackPass(function (anError) {
			var result;
			
			if (! (anError instanceof MochiKit.Async.CancelledError)) {
				result = confirmationDialog.deferredError({
					'type':		'ALERT',
					'title':	"Error",
					'text':		Clipperz.PM.Strings.errorDescriptionForException(anError),
					'buttons':	[{text:"Close", result:'CANCEL', isDefault:true}]
				})
			} else {
				result = anError;
			}
			
			return result;
		});
		deferredResult.callback();

		return deferredResult;
	},

	//=============================================================================
	__syntaxFix__: "syntax fix"
});
