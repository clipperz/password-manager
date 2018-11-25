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

Clipperz.PM.UI.Components.Cards.ViewClass = React.createClass({

	//============================================================================

	displayName: 'Clipperz.PM.UI.Components.Cards.View',

	propTypes: {
		'label':	React.PropTypes.string /*.isRequired */ ,
		'loading':	React.PropTypes.bool,
//		'proxyInfo': React.PropTypes.object.isRequired,
	},

	getInitialState: function () {
		return {
//			'showCertificatePreview': false,
		};
	},

	downloadCertificate: function (anEvent) {
		if (this.isCertificatePublished()) {
//			console.log("DOWNLOAD CERTIFICATE");
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'downloadCertificate', this.props['_reference']);
		}
	},
	
	previewCertificate: function (anEvent) {
		if (this.isCertificatePublished()) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'showCertificatePreview', this.props['_reference']);
//			this.setState({'showCertificatePreview':true});
		}
	},
	
	hideCertificatePreview: function (anEvent) {
//		this.setState({'showCertificatePreview':false});
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'hideCertificatePreview', this.props['_reference']);
	},

	//----------------------------------------------------------------------------

	handleDirectLoginClick: function (aDirectLogin) {
		var	directLoginParameters;

//console.log("PROPS", this.props);
		directLoginParameters = {
			'record': this.props['_reference'],
			'directLogin': aDirectLogin['_reference'],
		};

		return function (anEvent) {
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'runDirectLogin', directLoginParameters);
		};
	},

	//----------------------------------------------------------------------------

	handlePasswordFieldAction: function (aField) {
		var	self = this;
		var newState = {}

		newState[aField['_reference']] = !this.state[aField['_reference']];

		return function () {
			var fieldReference = aField['_reference'];
			var currentTimeout;

			if (newState[fieldReference]) {
				currentTimeout = setTimeout(function(){
					var newState;

					newState = {};
					newState[fieldReference] = false;

					self.setState(newState);
				}, Clipperz.PM.UI.Components.Cards.ViewClass.automaticRescrambleTimeout);
			}

			if (self.state['currentTimeout']) {
				clearTimeout(self.state['currentTimeout']);
				delete self.state['currentTimeout'];
			}

			if (currentTimeout) {
				newState['currentTimeout'] = currentTimeout;
			}

			self.setState(newState);
		};
	},

	handleLinkFieldAction: function (aField) {
		return function () {
			var url;

			url = aField['value'];
			if (/^https?\:\/\//.test(url) == false) {
				url = 'http://' + url;
			}

			window.open(url);
		}
	},

	handleFieldAction: function (aField) {
		var	result;

		if (aField['actionType'] == 'PASSWORD') {
			result = this.handlePasswordFieldAction(aField);
		} else if (aField['actionType'] == 'URL') {
			result = this.handleLinkFieldAction(aField);
		} else {
			result = MochiKit.Base.noop;
		};
		
		return result;
	},

	//----------------------------------------------------------------------------
	
	handleGetAttachment: function (anAttachment) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'getAttachment', anAttachment);
	},
	
	handleCancelDownload: function (anAttachment) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'cancelAttachment', anAttachment);
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'closeAttachment', anAttachment);
	},

	//----------------------------------------------------------------------------

	renderEmpty: function () {
//		return	React.DOM.h4({}, "EMPTY");
		return	React.DOM.h4({}, "");
	},
	
	//----------------------------------------------------------------------------

	renderLoading: function () {
		return	React.DOM.div({className:'loading'},[
			this.renderLabel(),
//			React.DOM.h5({className:'message'}, "loading")

			React.DOM.div({className:'overlay'}, [
				React.DOM.div({className:'spinner'}, [
					React.DOM.div({className:'bar01'}),
					React.DOM.div({className:'bar02'}),
					React.DOM.div({className:'bar03'}),
					React.DOM.div({className:'bar04'}),
					React.DOM.div({className:'bar05'}),
					React.DOM.div({className:'bar06'}),
					React.DOM.div({className:'bar07'}),
					React.DOM.div({className:'bar08'}),
					React.DOM.div({className:'bar09'}),
					React.DOM.div({className:'bar10'}),
					React.DOM.div({className:'bar11'}),
					React.DOM.div({className:'bar12'})
				])
			])
		]);
	},
	
	//----------------------------------------------------------------------------

	hasCertificate: function () {
		return ((typeof(this.props['certificateInfo']) != 'undefined') && (this.props['certificateInfo'] != null));
	},

	isCertificatePublished: function () {
		return (this.props['certificateInfo'] && (this.props['certificateInfo']['status'] == 'published'));
	},

	//----------------------------------------------------------------------------

	renderCertificatePreview: function () {
		return React.DOM.div({'className':'certificatePreview'}, [
			React.DOM.div({'className':'mask'}),
			React.DOM.div({'className':'previewContent'}, [
				React.DOM.header({}, [
					React.DOM.span({'onClick': this.hideCertificatePreview}, "close")
				]),
				React.DOM.div({'className':'preview'}, Clipperz.PM.UI.Components.Cards.CertificateRenderer(this.props)),
				React.DOM.footer({}, null)
			])
		]);
	},

	renderCertificateInfo: function (someCertificateInfo) {
		var	result;
		
		if (this.hasCertificate()) {
			var description;
			var	statusDescription;
			var	dateLabel;
			var	dateValue;
			var	transactionInfo;
			var classes = {
				'cardCertificateInfo':	true,
				'published':			this.isCertificatePublished(),
				'requested':			!this.isCertificatePublished(),
			};

			if (this.isCertificatePublished()) {
				description = "This card has been registered on the Bitcoin blockchain";
				statusDescription = "confirmed";
				dateLabel = "Registration date";
				dateValue = (new XDate(someCertificateInfo['creationDate'])).toString("MMM d, yyyy - HH:mm");
				transactionInfo = React.DOM.span({}, someCertificateInfo['txID']);
			} else {
				description = "This card will soon be registered on the Bitcoin blockchain";
				statusDescription = "pending";
				dateLabel = "Request date";
				dateValue = (new XDate(someCertificateInfo['requestDate'])).toString("MMM d, yyyy - HH:mm");
				transactionInfo = React.DOM.span({}, "N.A.");
			}
			
			result = React.DOM.div({'className': Clipperz.PM.UI.Components.classNames(classes)}, [
				React.DOM.div({}, [
					React.DOM.h3({}, "certificate"),
					React.DOM.p({}, description),
				]),
				React.DOM.div({'className':'info'}, [
					React.DOM.div({'className':'details'}, [
						React.DOM.dl({}, [ React.DOM.dt({}, dateLabel),		React.DOM.dd({}, dateValue) ]),
						React.DOM.dl({}, [ React.DOM.dt({}, 'transaction'),	React.DOM.dd({'className':'transactionInfo'}, transactionInfo) ]),
						React.DOM.dl({}, [ React.DOM.dt({}, 'status'),		React.DOM.dd({}, statusDescription) ]),
					]),
					React.DOM.div({'className':'links'}, [
						React.DOM.a({'className':'certificate', 'onClick':this.downloadCertificate}, "certificate"),
						React.DOM.a({'className':'preview', 'onClick':this.previewCertificate}, "preview"),
					]),
				]),
			])

		} else {
			result = null;
		}
		
		return result;
	},

	//----------------------------------------------------------------------------

	renderLabel: function (aLabel) {
		return	React.DOM.h3({'className':'cardLabel'}, aLabel);
	},
	
	renderNotes: function (someNotes) {
		var	result;

//console.log("NOTES", someNotes);
		if (someNotes != "") {
			result = React.DOM.div({'className':'cardNotes'}, [
				React.DOM.div({}, someNotes)
			]);
		} else {
			result = null;
		}

		return	result;
	},

	//............................................................................

//	renderTag: function (aTag) {
//		return	React.DOM.div({'className':'cardTag'}, aTag);
//	},
	
	renderTags: function (someTags) {
		var	tags;
		var	result;

//console.log("TAGS", someTags);
		tags = MochiKit.Base.filter(Clipperz.PM.DataModel.Record.isRegularTag, someTags).sort(Clipperz.Base.caseInsensitiveCompare);

		if (tags.length > 0) {
			result = Clipperz.PM.UI.Components.Cards.TagEditor({'selectedTags':tags, 'readOnly':true });
		} else {
			result = null;
		}

		return result;
	},

	onClickOnFieldValue: function (event) {
		var	succeeded;
		var	element;

		element = event.target;

//		event.target.focus();
//		event.target.select();
//		event.target.selectionStart = 0;
//		event.target.selectionEnd = event.target.value.length;
//		event.stopPropagation();
//		event.preventDefault();

		element.focus();
		element.setSelectionRange(0, element.value.length);
		event.stopPropagation();
		event.preventDefault();

//		selectedText = element.value;

		succeeded = document.execCommand('copy');
		if (succeeded === true) {
			window.getSelection().removeAllRanges();
			MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'copyFieldValueFeedback');
		}
	},

	//............................................................................

	renderField: function (aField) {
		var	cardFieldClasses = {};
		var	cardFieldValueClasses = {};
		var cardFieldActionClasses = {};
		
		cardFieldClasses['cardField'] = true;
		cardFieldClasses[aField['actionType']] = true;
		cardFieldClasses['hidden'] = aField['isHidden'];
		
		cardFieldValueClasses['fieldValue'] = true;
		cardFieldValueClasses[aField['actionType']] = true;
		cardFieldValueClasses['hidden'] = aField['isHidden'];
		cardFieldValueClasses['visible'] = this.state[aField['_reference']];

		cardFieldActionClasses['action'] = true;
		cardFieldActionClasses[aField['actionType']] = true;
		cardFieldActionClasses['active'] = this.state[aField['_reference']];

		return	React.DOM.div({'className':Clipperz.PM.UI.Components.classNames(cardFieldClasses)}, [
			React.DOM.div({'className':'fieldEditAction'}, null),
			React.DOM.div({'className':'fieldValues'}, [
				React.DOM.div({'className':'fieldLabel'}, aField['label']),
				Clipperz.PM.UI.Components.Cards.TextArea({
					'readOnly': true,
					// 'onMouseUp': function(e) { e.target.focus(); e.target.select(); e.stopPropagation(); e.preventDefault();},
//					'onClick': function(e) { e.target.focus(); e.target.select(); e.target.selectionStart = 0; e.target.selectionEnd = e.target.value.length; e.stopPropagation(); e.preventDefault(); },
					'onClick': this.onClickOnFieldValue,
					'className':Clipperz.PM.UI.Components.classNames(cardFieldValueClasses),
					'value': aField['value'],
					'rows': 1
				}),
			]),
			React.DOM.div({'className':'fieldAction'}, [
				React.DOM.span({'className':Clipperz.PM.UI.Components.classNames(cardFieldActionClasses), 'onClick':this.handleFieldAction(aField)}, aField['actionType'].toLowerCase() == 'password' ? 'view password' : aField['actionType'].toLowerCase())
			])
		]);
	},

	renderFields: function (someFields) {
		return	React.DOM.div({'className':'cardFields'}, MochiKit.Base.map(this.renderField, someFields));
	},

	//............................................................................

	renderDirectLogin: function (aDirectLogin) {
		return	React.DOM.div({'className':'cardDirectLogin', 'onClick':this.handleDirectLoginClick(aDirectLogin)}, [
			React.DOM.span({'className':'directLoginLabel'}, aDirectLogin['label']),
//			React.DOM.div({'className':'directLoginAction action'}, 'DIRECT LOGIN')
		]);
	},
	
	renderDirectLogins: function (someDirectLogins) {
		return	React.DOM.div({'className':'cardDirectLogins'}, MochiKit.Base.map(this.renderDirectLogin, someDirectLogins));
	},
	
	//............................................................................

	renderAttachmentProgress: function(aStatus, aServerStatus, aProgress) {
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

	renderAttachmentStatus: function(aStatus, aServerStatus, aProgress) {
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

	renderAttachmentActions: function(aStatus, aServerStatus, anAttachment) {
		var result;

		var queueOperationsInProgress = (aStatus && aStatus != 'DONE' && aStatus != 'CANCELED' && aStatus != 'FAILED');

		result = null;
		if (this.props['proxyInfo']['proxyType'] != 'OFFLINE_COPY') {
			if (aServerStatus == 'AVAILABLE' && ! queueOperationsInProgress) {
				result = React.DOM.a({
					'className': 'download',
					'onClick': MochiKit.Base.method(this, 'handleGetAttachment', anAttachment),
				}, "\u2b07");
			} else if (aServerStatus == 'AVAILABLE' && queueOperationsInProgress) {
				result = React.DOM.a({
					'className': 'cancel',
					'onClick': MochiKit.Base.method(this, 'handleCancelDownload', anAttachment)
				}, "remove field");
			}
		}

		return result;
	},

	renderAttachment: function (anAttachment) {
		var	result;

		if (this.props['attachmentQueueInfo'].elementFetchCallback != null) {
			var queueInfo = this.props['attachmentQueueInfo'].elementFetchCallback(anAttachment._reference) || [];
			var queueStatus = queueInfo['status'];
			var serverStatus = this.props['attachmentServerStatus'][anAttachment._reference];
			var queueOperationsInProgress = (queueStatus && queueStatus != 'DONE' && queueStatus != 'CANCELED' && queueStatus != 'FAILED');

			var broken = (! serverStatus && ! queueOperationsInProgress && ! this.props['_isBrandNew']);

			var status				= this.renderAttachmentStatus(queueStatus, serverStatus, queueInfo['requestProgress']);
			var actions				= this.renderAttachmentActions(queueStatus, serverStatus, anAttachment['_attachment']);
			var progressIndicator	= this.renderAttachmentProgress(queueStatus, serverStatus, queueInfo['requestProgress']);

			result = React.DOM.li({
				'className': (broken) ? 'broken' : '',
				'key': anAttachment._reference
			}, [
				React.DOM.span({'className': 'contentType'}, Clipperz.PM.DataModel.Attachment.contentTypeIcon(anAttachment.contentType)),
				React.DOM.span({'className': 'meta'}, [
					React.DOM.span({'className': 'name'}, anAttachment.name),
					React.DOM.span({'className': 'size'}, filesize(anAttachment.size)),
				]),
				React.DOM.span({'className': 'status'}, status),
				React.DOM.span({'className': 'progress'}, progressIndicator),
				React.DOM.span({'className': 'actions'}, actions),
			])
		} else {
			result = null;
		}
		
		return result;
	},

	renderAttachments: function(someAttachments) {
		var result;

		if (someAttachments.length > 0) {
			result = React.DOM.div({'className': 'cardAttachments'}, [
				React.DOM.h3({'className': 'summaryText'}, "Attachments"),
//				React.DOM.p({'className': 'summaryText'}, someAttachments.length + ' files attached'),
				React.DOM.ul({'className': 'attachmentList'},
					MochiKit.Base.map(MochiKit.Base.method(this, 'renderAttachment'), someAttachments)
				)
			]);
		} else {
			result = [];
		}

		return result;
	},

	//............................................................................

	renderCard: function () {
		var	classes = {
			'view':		true,
			'archived':	this.props['_isArchived'],
			'registered': this.hasCertificate()
		}
	
		return	React.DOM.div({'className':Clipperz.PM.UI.Components.classNames(classes)},[
			Clipperz.PM.UI.Components.Cards.CommandToolbar(this.props),
			React.DOM.div({'className':'content'}, [
				this.renderCertificateInfo(this.props['certificateInfo']),
				this.renderLabel(this.props['label']),
//React.DOM.div({}, "ID:" + this.props['ID']),
//React.DOM.div({}, "Reference:" + this.props['_reference']),
				this.renderTags(this.props['tags']),
				this.renderFields(this.props['fields']),
				this.renderAttachments(MochiKit.Base.values(this.props['attachments'])),
				this.renderNotes(this.props['notes']),
				this.renderDirectLogins(this.props['directLogins']),
			]),
			this.props['ask'] ? Clipperz.PM.UI.Components.DialogBox(this.props['ask']) : null,
			this.props['showCertificatePreview'] ? this.renderCertificatePreview() : null
		]);
	},
	
	//----------------------------------------------------------------------------

	render: function () {
		var	result;
//console.log("VIEW", this.props);
		if (this.props['loading'] == true) {
			result = this.renderLoading();
		} else if (this.props['_reference']) {
			result = this.renderCard();
		} else {
			result = this.renderEmpty();
		}

		return result;
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.Cards.ViewClass.automaticRescrambleTimeout = 5000;

Clipperz.PM.UI.Components.Cards.View = React.createFactory(Clipperz.PM.UI.Components.Cards.ViewClass);
