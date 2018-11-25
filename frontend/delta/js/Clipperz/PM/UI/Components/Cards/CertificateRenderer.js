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

var	textHeight = 12;
var	outputSpacing = 8;

var	arrowLength = 70;
var	arrowWidth = 8;
var	arrowHalfHeight = 2;

Clipperz.PM.UI.Components.Cards.CertificateRendererClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.Cards.CertificateRenderer',

	//==============================================================================
	
	renderTransactionSchema: function () {
		var outputDistance = 10;
//		var	numberOfAttachments = this.props['attachments'].length;
		var	numberOfAttachments = 0;
		var	numberOfOutputs = 1 + 1 + numberOfAttachments + 1;
		var	height = numberOfOutputs * (textHeight + outputSpacing) + outputDistance;
		var	rectWidth = arrowLength * 2;

		var arrow = function (x, y) {
			var	pathItems = function (items) {
				return (MochiKit.Base.map(function (coordinates) { return coordinates[0] + "," + coordinates[1]; }, items)).join(" ");
			}
			
			return [
				React.DOM.line({'x1':x, 'y1':y, 'x2':x + arrowLength, 'y2':y, 'stroke':'black', 'strokeWidth':1}),
				React.DOM.polygon({'points':pathItems([[x + arrowLength, y], [x + arrowLength - arrowWidth, y + arrowHalfHeight], [x + arrowLength - arrowWidth, y - arrowHalfHeight]]), 'fill':'black', 'stroke':'black', 'strokeWidth':1}),
			];
		};
		
		var outputArrow = function (args) {
			var	index = args[0];
			var label = args[1];

			return [
//				arrow(arrowLength + rectWidth, ((index + 0.5) * (textHeight + outputSpacing))),
				React.DOM.text({'x':(2 * arrowLength) + rectWidth + 15, 'y':(((index + 1) * textHeight) + (index + 0.5) * outputSpacing), 'fill':'black'}, label),
			];
		};

		return React.DOM.svg({'viewBox':'0 0 630 ' + height, 'width':'100%', 'height':height, 'fill':'currentcolor'}, [
			arrow(0, height/2),
			React.DOM.rect({'x':arrowLength, 'y':0, 'width':rectWidth, 'height':height, 'fill':'blue', 'opacity':0.3}),
			React.DOM.rect({'x':arrowLength, 'y':0, 'width':rectWidth, 'height':height, 'fill':'none', 'stroke':'black', 'strokeWidth':1, 'opacity':1}),

			React.DOM.g({'transform':'matrix(-0.2, 0, 0, ' + ((numberOfOutputs - 1) * 0.1225) + ', ' + ((arrowLength * 4) + 10) +', 0)'}, [
				React.DOM.path({'d':'M 19.8,25.8 C 19.8,11.2 15.6,2.2 0,0.4 L 0.4,0 C 23,0.4 29.2,7.2 29.2,29 L 29.2,59.2 C 29.2,72.6 30.6,79 45.2,82.6 L 45.2,83 C 30.8,86.6 29.2,93 29.2,106.2 L 29.2,138.4 C 29.2,159.4 21.4,165.4 0.4,166 L 0,165.601 C 16,163.201 19.8,155.201 19.8,140 L 19.8,107.8 C 19.8,94.2 21.6,86.4 36.6,83 L 36.6,82.6 C 21.4,79 19.8,70.6 19.8,57 L 19.8,25.8 z '})
			]),

			arrow((arrowLength + rectWidth), ((numberOfOutputs - 1) * (textHeight + outputSpacing) / 2)),
			React.DOM.text({'x':(arrowLength + rectWidth) + 5, 'y':((numberOfOutputs - 1) * (textHeight + outputSpacing) / 2) - 2, 'fill':'black', 'style':{'fontSize':'0.6em'}}, 'MULTI-SIG'),
			
			arrow((arrowLength + rectWidth), ((numberOfOutputs - 0.5) * (textHeight + outputSpacing)) + outputDistance),
			React.DOM.text({'x':(arrowLength + rectWidth) + 5, 'y':((numberOfOutputs - 0.5) * (textHeight + outputSpacing)) - 2 + outputDistance, 'fill':'black', 'style':{'fontSize':'0.6em'}}, 'OP_RETURN'),
			React.DOM.text({'x':(2 * arrowLength) + rectWidth + 5, 'y':((numberOfOutputs * textHeight) + (numberOfOutputs - 0.5) * outputSpacing) + outputDistance, 'fill':'black'}, '"CLIPPERZ ' + this.props['certificateInfo']['version'] + ' REG"'),

			this.props['certificateDetails'] ? MochiKit.Base.map(outputArrow, 
				MochiKit.Base.zip(
					MochiKit.Iter.range(0, numberOfOutputs),
					[this.props['certificateDetails']['transaction']['card.address'], this.props['certificateDetails']['transaction']['metadata.address']]
/*
					MochiKit.Base.concat(
						[this.props['certificateDetails']['transaction']['card.address'], this.props['certificateDetails']['transaction']['metadata.address']],
						MochiKit.Base.map(
							MochiKit.Base.itemgetter('address'),
							MochiKit.Base.values(this.props['certificateDetails']['transaction']['attachments'])
						)
					)
*/
				)
			): null,
		]);
	},

	//==============================================================================

	renderCertificateDetails: function () {
		return React.DOM.ul({'className':'transactionInfo'}, [
			React.DOM.li({}, /*"OUT00 - " + */"BTC address of card: " + this.props['certificateDetails']['transaction']['card.address']),
			React.DOM.li({}, [
				React.DOM.div({'className':'metadata'}, [
					React.DOM.header({}, 'Card data'),
					React.DOM.dl({'className':'title'}, [React.DOM.dt({}, "Title"), React.DOM.dd({}, this.props['certificateDetails']['metadata']['label'])]),
					MochiKit.Base.map(function (aField) { return React.DOM.dl({'className':'field'}, [React.DOM.dt({}, aField['label']), React.DOM.dd({}, aField['value'])]); }, this.props['certificateDetails']['metadata']['fields']),
					this.props['certificateDetails']['metadata']['notes'] ? React.DOM.dl({'className':'notes'}, [React.DOM.dt({}, "Notes"), React.DOM.dd({}, this.props['certificateDetails']['metadata']['notes'])]) : null,
					React.DOM.ul({'className':'attachments'},
						MochiKit.Base.map(
							function (anAttachmentInfo) {
//								var	numberPadding = function (aValue) {
//									return ("00" + aValue).substr(-2);
//								}
								return React.DOM.li({}, [
									React.DOM.dl({'className':'file'}, [React.DOM.dt({}, "File"),     React.DOM.dd({}, anAttachmentInfo['name'])]),
									React.DOM.dl({'className':'size'}, [React.DOM.dt({}, "Size"),     React.DOM.dd({}, filesize(anAttachmentInfo['size']))]),
									React.DOM.dl({'className':'type'}, [React.DOM.dt({}, "Filetype"), React.DOM.dd({}, anAttachmentInfo['contentType'])]),
									React.DOM.dl({'className':'hash'}, [React.DOM.dt({}, "Sha256"),   React.DOM.dd({}, anAttachmentInfo['hash'])]),
//									React.DOM.div({'className':'address'}, /*"OUT" + numberPadding(anAttachmentInfo[2]) + " - " + */"File BTC address: " + anAttachmentInfo[1]['address'])
								]);
							}, this.props['certificateDetails']['metadata']['attachments']
//							}, MochiKit.Base.zip(
//								this.props['certificateDetails']['metadata']['attachments'],
//								MochiKit.Base.values(this.props['certificateDetails']['transaction']['attachments']),
//								MochiKit.Iter.range(2, this.props['certificateDetails']['metadata']['attachments'].length + 2)
//							)
						)
					),
					React.DOM.div({'className':'address'}, /*"OUT01 - " + */"BTC address from card data: " + this.props['certificateDetails']['transaction']['metadata.address'])
				])
			])
		]);
	},

	renderSpinner: function () {
		return React.DOM.div({'className':'spinner'}, [
			React.DOM.div({'className':'bar01'}),
			React.DOM.div({'className':'bar02'}),
			React.DOM.div({'className':'bar03'}),
			React.DOM.div({'className':'bar04'}),
			React.DOM.div({'className':'bar05'}),
			React.DOM.div({'className':'bar06'}),
			React.DOM.div({'className':'bar07'}),
			React.DOM.div({'className':'bar08'}),
			React.DOM.div({'className':'bar09'}),
			React.DOM.div({'className':'bar10'}),
			React.DOM.div({'className':'bar11'}),
			React.DOM.div({'className':'bar12'}),
		]);
	},

	componentDidMount: function () {
//		this.renderTransactionDiagram(this.refs['canvas']);
	},
	
	render: function () {
//console.log("CERTIFICATE RENDERER", this.props, NETWORK);
console.log("CERTIFICATE RENDERER", this.props);
		var	transactionInspectorPath = (NETWORK.pubKeyHash == 111) ? 'tBTC' : 'BTC';
		var	certificateInfo;
		
		if (this.props['certificateDetails']) {
			certificateInfo = this.props['certificateDetails'];
			certificateInfo['tx'] = this.props['certificateInfo']['txID'];
			certificateInfo['requestDate'] =  this.props['certificateInfo']['requestDate'];
			certificateInfo['creationDate'] = this.props['certificateInfo']['creationDate'];
		} else {
			certificateInfo = null;
		}

		return React.DOM.div({'className':'certificateContent'}, [
			React.DOM.div({}, [
				React.DOM.header({}, [
					React.DOM.div({'className':'title'}, [
						React.DOM.h3({}, "clipperz"),
						React.DOM.h1({}, "Blockchain certificate")
					]),
					React.DOM.div({'className':'info'}, [
//						React.DOM.dl({'className':'cardLabel'}, [React.DOM.dt({}, "Card"), React.DOM.dd({}, this.props['label'])]),
						React.DOM.dl({'className':'cardLabel'}, [React.DOM.dt({}, "Card"), React.DOM.dd({}, this.props['certificateDetails'] ? this.props['certificateDetails']['metadata']['label'] : "")]),
						React.DOM.dl({}, [React.DOM.dt({}, "Requested on"), React.DOM.dd({}, (new XDate(this.props['certificateInfo']['requestDate'])).toString("MMM d, yyyy - HH:mm"))]),
						React.DOM.dl({}, [React.DOM.dt({}, "Validated on"), React.DOM.dd({}, (new XDate(this.props['certificateInfo']['creationDate'])).toString("MMM d, yyyy - HH:mm"))]),
						React.DOM.dl({'className':'version'}, [React.DOM.dt({}, "Clipperz protocol version"), React.DOM.dd({}, this.props['certificateInfo']['version'])]),
					]),
				]),
				React.DOM.div({'className':'details'}, this.props['certificateDetails'] ? this.renderCertificateDetails() : this.renderSpinner()),
			]),
			React.DOM.div({'className':'transactionInfo'}, [
				React.DOM.div({'className':'description'}, [
					React.DOM.p({}, "The following transaction has been added to the Bitcoin blockchain."),
					React.DOM.p({}, [
						React.DOM.span({}, "TX ID: "),
						React.DOM.a({'href':'https://clipperz.is/tx/' + transactionInspectorPath + '/' + this.props['certificateInfo']['txID']}, this.props['certificateInfo']['txID'])
					]),
				]),
				this.renderTransactionSchema(),
			]),
			React.DOM.div({'className':'reviewInfo'}, [
				React.DOM.p({}, '----'),
				React.DOM.p({'className':'verify'}, [
					React.DOM.span({}, "You can check the authenticity of this certificate at "),
					React.DOM.a({'href':'https://clipperz.is/verify'}, "clipperz.is/verify")
				]),
				React.DOM.p({'className':'instructions'}, [
					React.DOM.span({}, "If you want to perform this check by yourself, just follow "),
					React.DOM.a({'href':'https://clipperz.is/verify_howto'}, "these instructions")
				]),
			]),
			certificateInfo ? React.DOM.textarea({'className':'certificateDetails', 'value':Clipperz.Base.serializeJSON(certificateInfo), 'readOnly':true}) : null,
		]);
	},

	//===========================================================================
});

Clipperz.PM.UI.Components.Cards.CertificateRenderer = React.createFactory(Clipperz.PM.UI.Components.Cards.CertificateRendererClass);