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

/*
Clipperz.Base.module('Clipperz.PM.UI.Web.Controllers');

Clipperz.PM.UI.Web.Controllers.DirectLoginsController = function() {
	Clipperz.PM.UI.Web.Controllers.DirectLoginsController.superclass.constructor.apply(this, arguments);
	
	return this;
};

Clipperz.Base.extend(Clipperz.PM.UI.Web.Controllers.DirectLoginsController, Clipperz.PM.UI.Web.Controllers.GridController, {

	'createGrid': function () {
		return new Clipperz.PM.UI.Web.Components.GridComponent({columnsManagers: [
			new Clipperz.PM.UI.Web.Components.FaviconColumnManager({
				'name':			'DirectLogins.favicon',
				'selector': 	 MochiKit.Base.methodcaller('favicon'),
				'cssClass': 	'favicon'
			}),
//			new Clipperz.PM.UI.Web.Components.LinkColumnManager({
			new Clipperz.PM.UI.Web.Components.DirectLoginColumnManager({
				'name':			'DirectLogins.title',
				'selector':		 MochiKit.Base.methodcaller('label'),
				'label':		'title',
				'cssClass':		'title',
				'comparator': 	 Clipperz.Base.caseInsensitiveCompare,
				'sortable':		 true,
				'sorted': 		'ASCENDING',
				'actionMethod':	 MochiKit.Base.methodcaller('runDirectLogin')
			}),
//			new Clipperz.PM.UI.Web.Components.TextColumnManager({ //should be StrengthColumnManager
//				'label':	'strength',
//				'cssClass':	'title',
//				'selector':	 MochiKit.Base.methodcaller('label') //should be 'strength' or a strenght evaluation function
//			}),
			new Clipperz.PM.UI.Web.Components.LinkColumnManager({
				'name':			'DirectLogins.cardTitle',
				'selector':		 MochiKit.Base.compose(MochiKit.Base.methodcaller('label'), MochiKit.Base.methodcaller('record')),
				'label':		'card',
				'cssClass':		'cardTitle',
				'comparator': 	 Clipperz.Base.caseInsensitiveCompare,
				'sortable':		 true,
				'sorted': 		'UNSORTED',
				'actionMethod':	 MochiKit.Base.method(this, 'handleShowCard')
			}),
//			new Clipperz.PM.UI.Web.Components.TextColumnManager({ //should be StrengthColumnManager
//				'label':	'last access',
//				'cssClass':	'title',
//				'selector':	 MochiKit.Base.methodcaller('label')
//		//					'sortable':	 true,
//		//					'sorted': 	'UNSORTED'
//			}),
//			new Clipperz.PM.UI.Web.Components.TextColumnManager({
//				'label':	'commands',
//				'cssClass':	'title',
//				'selector':	 MochiKit.Base.methodcaller('label'), //should be a function for commands display
//			}),
			new Clipperz.PM.UI.Web.Components.DeleteObjectColumnManager({
				'name':			'DirectLogins.delete',
				'selector':		 MochiKit.Base.noop,
				'cssClass':		'delete',
//				'actionMethod':  function(anObject, anEvent) { MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'deleteDirectLogin', {objectData:anObject, element:anEvent.src()})}
				'actionMethod':  MochiKit.Base.method(this, 'handleDeleteDirectLogin')
			})
		]});

	},
	
	//-----------------------------------------------------------------------------

	'getRows': function () {
		//	TODO: relying on user() in GridController, bad code smell :|
		return this.user().getDirectLogins();
	},

	//-----------------------------------------------------------------------------

	'handleShowCard': function (anObject, anEvent) {
		var cardDialogController;

		cardDialogController = new Clipperz.PM.UI.Web.Controllers.CardDialogController({record:anObject.record()})
		cardDialogController.run(anEvent.src());
	},

	//-----------------------------------------------------------------------------

	'handleDeleteDirectLogin': function (anObject, anEvent) {
		var deferredResult;
		var confirmationDialog;
		
		confirmationDialog = new Clipperz.PM.UI.Common.Components.SimpleMessagePanel({
			title:	"Delete DirectLogin",
			text:	"Do you want to delete …",
			type:	'ALERT',
			buttons: [
				{text:"Cancel",	result:'CANCEL', isDefault:true},
				{text:"Delete", result:'OK'}
			]
		});

		deferredResult = new Clipperz.Async.Deferred("AppController.handleDeleteCard", {trace:false});
//		deferredResult = confirmationDialog.deferredShow({openFromElement:anEvent.src(), onOkCloseToElement:MochiKit.DOM.currentDocument().body, onCancelCloseToElement:anEvent.src()});
		deferredResult.addMethod(confirmationDialog, 'deferredShow', {
			'openFromElement':			anEvent.src(),
			'onOkCloseToElement':		null,	//	MochiKit.DOM.currentDocument().body,
			'onCancelCloseToElement':	anEvent.src()
		});
		deferredResult.addCallback(function () { Clipperz.log("DELETE: " + anObject.toString(), anObject); });
		deferredResult.addErrbackPass(function () { Clipperz.log("skip deletion: " + anObject.toString(), anObject); });
		deferredResult.callback();

		return deferredResult;
	},

	//-----------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
*/