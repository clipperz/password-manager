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
Clipperz.Base.module('Clipperz.PM.UI.Components.ExtraFeatures');

Clipperz.PM.UI.Components.ExtraFeatures.OTPClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.OTP',

	getInitialState: function() {
		return {
//			'selectedOTPs': [],
			'labelBeingEdited': null,
			'otpLabel': '',
		};
	},

// 	propTypes: {
//	},

	//=========================================================================

	handleNew: function() {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'createNewOTP');
	},
	
	handleDelete: function (anOtpReference) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'deleteOTPs', [anOtpReference]);
	},

	//-------------------------------------------------------------------------

	enableOtpLabelEditing: function(anOTP) {
		var newState = this.state;

		newState['labelBeingEdited'] = anOTP.reference();
		newState['otpLabel'] = anOTP.label();

		this.setState(newState);
	},
	
	updateOtpLabel: function (anOTP, anEvent) {
		var newState = this.state;
		var	newLabel = anEvent.target.value

		newState['otpLabel'] = newLabel;

		this.setState(newState);
	},
	
	handleKeyPressed: function (anOTP, anEvent) {
		switch (anEvent.keyCode) {
			case  9: // tab
				this.handleLabelSave(anOTP);
				//	TODO: edit label of next OTP
				break;
			case 13: // enter
				this.handleLabelSave(anOTP);
			case 27: // escape
				this.handleLabelCancel(anOTP);
				break;
		}
	},

	handleLabelSave: function (anOTP) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'changeOTPLabel', {'reference':anOTP.reference(), 'label':this.state['otpLabel']});
		this.handleLabelCancel()
	},

	handleLabelCancel: function() {
		var newState;

		newState = this.state;
		newState['labelBeingEdited'] = null;

		this.setState(newState);
	},

	//=========================================================================

	handlePrint: function () {
		this.printOneTimePasswords();
	},

	printOneTimePasswords: function () {
		var newWindow;

		var filteredOtpList = MochiKit.Base.filter(MochiKit.Base.bind(function (anOTP) {
			return (this.props.userInfo.otpsDetails[anOTP.reference()]['status'] == 'ACTIVE');
		}, this), this.props.userInfo.otpList);

		newWindow = window.open("", "");
		newWindow.document.write(
			'<!DOCTYPE html>' +
			'<html lang="en">' +
				'<head>' +
					'<meta charset="utf-8">' +
					'<title>Active One Time Passwords - Clipperz</title>' +
					'<style>' +
						'body { margin: 0; padding: 0.5cm; }' +
						'ul { font-size: 0.24cm;    font-family: monospace;    list-style-type: none;    padding: 1em;    border: 1px solid;    display: inline-block;    background: lightblue;    border-radius: 10px;    margin: 0.2cm;   width: 7cm;     height: 3.6cm; vertical-align: bottom;}' +
						'h2 { font-size: 0.24cm;    margin: 0;    padding-right: 0.15cm;}' +
						'h2 span { display: inline-block;   width: 50%;}' +
						'h2 span.right { text-align: right; }' +
						'li { line-height: 0.34cm; }' +
						'li span { display: block; }' +
						'span.password { font-family: monospace; font-size: 16pt; padding-bottom: 5px; }' +
						'span.label { font-family: sans-serif; font-size: 12pt; }' +
					'</style>' +
				'</head>' +
				'<body>' +
					// '<ul>' +
					// 	MochiKit.Base.map(function (anOTP) {
					// 		return	'<li>' +
					// 					'<span class="password">' + anOTP.password() + '</span>' +
					// 					'<span class="label">' + anOTP.label() + '</span>' +
					// 				'</li>';
					// 	}, filteredOtpList).join('') +
					// '</ul>' +
					this.renderPrintedOtpList(filteredOtpList) +
				'</body>' +
			'</html>'
		);

		newWindow.document.close();
		newWindow.focus();
		newWindow.print();
		newWindow.close();
	},

	renderPrintedOtpList: function(aList) {
		var result;
		var i;

		var date = new Date();
		var header = '<h2><span class="left">Clipperz OTPs</span><span class="right">'+date.getDate()+'/'+date.getMonth()+'/'+date.getFullYear()+'</span></h2>'

		result = '<ul>'+header;
		for (i in aList) {
			if (i%10 == 0 && i != 0) {
				result += '</ul><ul>'+header;
			}

			result += '<li>\u2610 '+aList[i].password()+'</li>';
		}

		return result;
	},

	//=========================================================================

	renderOtpRows: function() {
		var result;
		var	defaultOtpLabel;

		defaultOtpLabel = "…";
		
		if (this.props.userInfo.otpList) {
			result = MochiKit.Base.map(MochiKit.Base.bind(function (anOTP) {
				var	reference = anOTP.reference();
				var	otpDetailInfo = this.props.userInfo.otpsDetails[reference];
				var	labelComponent;
				var	otpStatusInfo;
				var	otpClasses;
				var	optLabel;

				otpClasses = {
					'otpDetail': true,
				};
				otpClasses[otpDetailInfo['status']] = true;

				if (otpDetailInfo['status'] === 'USED') {
					otpStatusInfo = React.DOM.div({'key':'otpStatusInfo', 'className':'otpStatusInfo'}, [
						React.DOM.span({'key':'otpStatus', 'className':'otpStatus'}, otpDetailInfo['status']),
						React.DOM.span({'key':'requestDate', 'className':'requestDate'}, otpDetailInfo['usageDate']),
						React.DOM.span({'key':'connectionIp', 'className':'connectionIp'}, otpDetailInfo['connection']['ip']),
						React.DOM.span({'key':'connectionBrowser', 'className':'connectionBrowser'}, otpDetailInfo['connection']['browser']),
					])
				} else if (otpDetailInfo['status'] === 'DISABLED') {
						otpStatusInfo = React.DOM.div({'key':'otpStatusInfo', 'className':'otpStatusInfo'}, [
							React.DOM.span({'key':'otpStatus', 'className':'otpStatus'}, otpDetailInfo['status']),
							React.DOM.span({'key':'requestDate', 'className':'requestDate'}, otpDetailInfo['usageDate']),
//							React.DOM.span({'className':'connectionIp'}, otpDetailInfo['connection']['ip']),
//							React.DOM.span({'className':'connectionBrowser'}, otpDetailInfo['connection']['browser']),
						])
				} else if (otpDetailInfo['status'] === 'ACTIVE') {
					otpStatusInfo = null;
				} else {
					throw "Clipperz.PM.UI.Components.ExtraFeatures.OTP.renderOtpRows: unrecognized OTP status: " + otpDetailInfo['status'];
				}

				if (reference == this.state.labelBeingEdited) {
					labelComponent = React.DOM.input({
						'key': 'input',
						'autoFocus':true,
						'value':this.state.otpLabel,
						'placeholder': "custom label",
						'onChange':MochiKit.Base.partial(this.updateOtpLabel, anOTP),
						'onKeyDown':MochiKit.Base.partial(this.handleKeyPressed, anOTP),
					});
				} else {
					labelComponent = React.DOM.span({
						'key': 'span',
						'onClick':MochiKit.Base.partial(this.enableOtpLabelEditing, anOTP),
						'className': Clipperz.PM.UI.Components.classNames({'customLabel':anOTP.label()})
					}, (anOTP.label()) ? anOTP.label() : defaultOtpLabel)
				}

				return React.DOM.li({
					'key':'otp-' + reference,
					'className':Clipperz.PM.UI.Components.classNames(otpClasses)
				}, [
					React.DOM.div({'key':'otpAction', 'className':'otpAction'}, [
						React.DOM.a({'key':'a', 'onClick':MochiKit.Base.partial(this.handleDelete, reference)}, 'remove OTP'),
					]),
					React.DOM.div({'key':'otpInfo', 'className':'otpInfo'}, [
						React.DOM.div({'key':'otpPassword', 'className':'otpPassword'}, anOTP.password()),
						React.DOM.div({'key':'otpLabel', 'className':'otpLabel'}, labelComponent),
						otpStatusInfo,
					]),
				]);
			}, this), this.props.userInfo.otpList);
		} else {
			result = React.DOM.li({}, React.DOM.div({}, "..."));
		}

		return result;
	},

	render: function () {
		return	React.DOM.div({'key':'extraFeatureOTP', 'className':'extraFeature OTP'}, [
			React.DOM.div({'key':'header', 'className':'header'}, [
				React.DOM.h1({'key':'h1'}, "One-Time Passwords"),
			]),
			React.DOM.div({'key':'content', 'className':'content'}, [
				React.DOM.div({'key': 'div'},[
					React.DOM.div({'key':'description', 'className':'description'}, [
						React.DOM.p({'key': 'p'}, "A one-time password works like your regular passphrase, but it can be used only once. Strongly recommended when accessing your Clipperz account from unsecure devices where keyloggers may be installed."),
					]),
					React.DOM.a({'key':'button', 'className':'button', 'onClick':this.handlePrint}, "Print"),

					React.DOM.ul({'key':'otpList', 'className':'otpList'}, this.renderOtpRows()),
					React.DOM.div({'key':'actions', 'className':'actions'}, [
						React.DOM.a({'key':'newButton', 'onClick': this.handleNew}, "create new OTP"),
					]),
				])
			])
		]);
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.ExtraFeatures.OTP = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.OTPClass);
