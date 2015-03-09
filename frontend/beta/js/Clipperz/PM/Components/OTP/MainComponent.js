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

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.Components) == 'undefined') { Clipperz.PM.Components = {}; }
if (typeof(Clipperz.PM.Components.OTP) == 'undefined') { Clipperz.PM.Components.OTP = {}; }

//#############################################################################

Clipperz.PM.Components.OTP.MainComponent = function(anElement, args) {
	args = args || {};

//MochiKit.Logging.logDebug("new OTP.MainComponent");
    Clipperz.PM.Components.OTP.MainComponent.superclass.constructor.call(this, anElement, args);

	this._user = args.user;
	this._shouldRender = true;

	this._deleteButton = null;
	this._printButton = null;

	Clipperz.NotificationCenter.register(null, 'tabSelected', this, 'tabSelectedHandler');
//	Clipperz.NotificationCenter.register(null, 'oneTimePasswordAdded', this, 'render');
	
	return this;
}

//=============================================================================

YAHOO.extendX(Clipperz.PM.Components.OTP.MainComponent, Clipperz.PM.Components.BaseComponent, {

	'toString': function() {
		return "Clipperz.PM.Components.OTP.MainComponent component";
	},

	//-------------------------------------------------------------------------

	'render': function() {
//MochiKit.Logging.logDebug("### OTP.MainComponent.render");
		Clipperz.NotificationCenter.unregister(this);
		MochiKit.Signal.disconnectAllTo(this);

		if (Clipperz.PM.Proxy.defaultProxy.isReadOnly()) {
			this.element().update("");
			this.domHelper().append(this.element(), {tag:'div', cls:'oneTimePasswordReadOnlyMessage', htmlString:Clipperz.PM.Strings['oneTimePasswordReadOnlyMessage']});
		} else {
			var deferredResult;

			deferredResult = new MochiKit.Async.Deferred();

//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OTP.MainComponent.render - 1: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.bind(function() {
				this.element().update("");
				Clipperz.YUI.DomHelper.append(this.element(), {tag:'div', htmlString:Clipperz.PM.Strings['oneTimePasswordLoadingMessage']});
			}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OTP.MainComponent.render - 2: " + res); return res;});
			deferredResult.addCallback(MochiKit.Base.method(this.user(), 'loadOneTimePasswords'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OTP.MainComponent.render - 3: " + res); return res;});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OTP.MainComponent.render - 3.1: " + Clipperz.Base.serializeJSON(res.serializedData())); return res;});
			deferredResult.addCallback(MochiKit.Base.bind(function(aResult) {
				var	tbodyElement;
				var	oneTimePasswordReferenceKeys;
				var imageExtension;
				var isThereAnyActiveOneTimePassword;

				isThereAnyActiveOneTimePassword = false;
				
				this.element().update("");
				Clipperz.YUI.DomHelper.append(this.element(), {tag:'div', id:'oneTimePasswordList', children:[
					{tag:'div', id:'oneTimePasswords_header', children:[
						{tag:'table', width:'100%', children:[
							{tag:'tbody', children:[
								{tag:'tr', children:[
									{tag:'td', width:'10%', children:[
										{tag:'div', id:this.getId('createNewOneTimePasswordButton')}
									]},
									{tag:'td', width:'40%', children:[
										{tag:'div', id:this.getId('deleteSelectedOneTimePasswordButton')}
									]},
									{tag:'td', width:'50%', align:'right', children:[
										{tag:'div', id:this.getId('printOneTimePasswordButton')}
									]}
								]}
							]}
						]},
						{tag:'div', children:[
							{tag:'ul', children:[
								{tag:'li', children:[
									{tag:'span', htmlString:Clipperz.PM.Strings['oneTimePasswordSelectionLink_selectLabel']}
								]},
								{tag:'li', children:[
									{tag:'a', href:'#', id:this.getId('selectAllOneTimePasswords_link'), htmlString:Clipperz.PM.Strings['oneTimePasswordSelectionLink_all']}
								]},
								{tag:'li', children:[
									{tag:'a', href:'#', id:this.getId('selectNoneOneTimePasswords_link'), htmlString:Clipperz.PM.Strings['oneTimePasswordSelectionLink_none']}
								]},
								{tag:'li', children:[
									{tag:'a', href:'#', id:this.getId('selectUsedOneTimePasswords_link'), htmlString:Clipperz.PM.Strings['oneTimePasswordSelectionLink_used']}
								]},
								{tag:'li', children:[
									{tag:'a', href:'#', id:this.getId('selectUnusedOneTimePasswords_link'), htmlString:Clipperz.PM.Strings['oneTimePasswordSelectionLink_unused']}
								]}
							]}
						]}
					]},
					{tag:'form', id:this.getId('oneTimePasswords_form'), children:[
						{tag:'table', cls:'oneTimePassword', cellspacing:'0', cellpadding:'2', children:[
							{tag:'tbody', id:this.getId('oneTimePasswords_tbody'), children:[
							]}
						]}
					]}
				]});

				imageExtension = (Clipperz_IEisBroken == true) ? 'gif': 'png';

				tbodyElement = this.getElement('oneTimePasswords_tbody');
				oneTimePasswordReferenceKeys = MochiKit.Base.keys(this.user().oneTimePasswordManager().oneTimePasswords()).reverse();
				c = oneTimePasswordReferenceKeys.length;
				if (c>0) {
					for (i=0; i<c; i++) {
						var otpReference;
						var currentOTP;
						var loginSessionInfoConfig;

						imageExtension = (Clipperz_IEisBroken == true) ? 'gif': 'png';

						otpReference = oneTimePasswordReferenceKeys[i];
						currentOTP = this.user().oneTimePasswordManager().oneTimePasswords()[otpReference];

						switch (currentOTP.status()) {
							case 'USED':
								var loginSessionInfo;

								loginSessionInfo = currentOTP.connectionInfo();
								try {
									var ip;

									ip = (currentOTP.connectionInfo()['ip'].match(/^\d{1,3}(.\d{1,3}){3}$/)) ? currentOTP.connectionInfo()['ip'] : Clipperz.PM.Strings['unknown_ip'];

									loginSessionInfoConfig = [
										{tag:'div', cls:'oneTimePassword_usageDateDescription', children:[
											{tag:'span', cls:'value', html:Clipperz.PM.Date.getElapsedTimeDescription(currentOTP.usageDate())}
										]},
										{tag:'div', cls:'oneTimePassword_usageDetails', children:[
											{tag:'img', cls:'flag', title:Clipperz.PM.Strings['countries'][ loginSessionInfo['country']], src:Clipperz.PM.Strings['icons_baseUrl'] + "/flags/" + loginSessionInfo['country'].toLowerCase() + "." +  imageExtension, width:'32', height:'32'},
											{tag:'img', cls:'browser', title:Clipperz.PM.Strings['browsers'][ loginSessionInfo['browser']], src:Clipperz.PM.Strings['icons_baseUrl'] + "/browsers/" + loginSessionInfo['browser'].toLowerCase() + "." + imageExtension, width:'32', height:'32'},
											{tag:'img', cls:'operatingSystem', title:Clipperz.PM.Strings['operatingSystems'][loginSessionInfo['operatingSystem']], src:Clipperz.PM.Strings['icons_baseUrl'] + "/operatingSystems/" + loginSessionInfo['operatingSystem'].toLowerCase() + "." + imageExtension, width:'32', height:'32'}
										]},
										{tag:'div', cls:'oneTimePassword_usageDate', html:Clipperz.PM.Date.formatDateWithTemplate(currentOTP.usageDate(), Clipperz.PM.Strings['fullDate_format'])},
										{tag:'div', cls:'oneTimePassword_IP', children:[
											{tag:'span', cls:'oneTimePassword_IPLabel', htmlString:Clipperz.PM.Strings['loginHistoryIPLabel']},
											{tag:'span', cls:'oneTimePassword_IPValue', html:ip}
										]}
									];
								} catch(exception) {
									MochiKit.Logging.logWarning("an error occured while showing the One Time Password session details");
									loginSessionInfoConfig = [];
								}
								break;
							case 'DISABLED':
								loginSessionInfoConfig = [
									{tag:'span', cls:'disabledOneTimePassword', htmlString:Clipperz.PM.Strings['disabledOneTimePassword_warning']}
								];
								break;
							case 'ACTIVE':
							default:
								loginSessionInfoConfig = [];
								break;
						}


						if (currentOTP.isExpired() == false) {
							isThereAnyActiveOneTimePassword = true;
						};

						
						this.domHelper().append(tbodyElement, {tag:'tr', cls:(currentOTP.isExpired() ? 'oneTimePassword_used': 'oneTimePassword_new'), children:[
							{tag:'td', valign:'top', children:[
								{tag:'input', type:'checkbox', cls:'otpCheckbox', name:currentOTP.reference()}
							]},
							{tag:'td', valign:'top', children:[
								{tag:'span', cls:'oneTimePassword_value', html:currentOTP.password()}
							]},
							{tag:'td', valign:'top', children:[
								{tag:'div', cls:'oneTimePassword_usageStats', children:loginSessionInfoConfig}
							]}
						]});
					}
				} else {
					this.domHelper().append(tbodyElement, {tag:'tr', children:[
						{tag:'td', children:[
							{tag:'div', cls:'oneTimePassword_noPasswordPresent', htmlString:Clipperz.PM.Strings['oneTimePasswordNoPasswordAvailable']}
						]}
					]});
				}

				new YAHOO.ext.Button(this.getDom('createNewOneTimePasswordButton'), {text:Clipperz.PM.Strings['createNewOTPButtonLabel'], handler:this.createNewOneTimePassword, scope:this});
				this.setDeleteButton(new YAHOO.ext.Button(this.getDom('deleteSelectedOneTimePasswordButton'), {text:Clipperz.PM.Strings['deleteOTPButtonLabel'], handler:this.deleteSelectedOneTimePasswords, scope:this}));
				this.setPrintButton(new YAHOO.ext.Button(this.getDom('printOneTimePasswordButton'), {text:Clipperz.PM.Strings['printOTPButtonLabel'], handler:this.printOneTimePasswords, scope:this}));

				MochiKit.Signal.connect(this.getId('selectAllOneTimePasswords_link'),	'onclick', this, 'selectAllOneTimePasswords');
				MochiKit.Signal.connect(this.getId('selectNoneOneTimePasswords_link'),	'onclick', this, 'selectNoneOneTimePasswords');
				MochiKit.Signal.connect(this.getId('selectUsedOneTimePasswords_link'),	'onclick', this, 'selectUsedOneTimePasswords');
				MochiKit.Signal.connect(this.getId('selectUnusedOneTimePasswords_link'),'onclick', this, 'selectUnusedOneTimePasswords');

				MochiKit.Base.map(MochiKit.Base.bind(function(aCheckbox) {
					MochiKit.Signal.connect(aCheckbox, 'onclick', this, 'handleCheckboxClick');
				}, this), this.oneTimePasswordCheckboxes());

				this.updateDeleteButtonStatus();

				if (isThereAnyActiveOneTimePassword == true) {
					this.printButton().enable();
				} else {
					this.printButton().disable();
				}
				
//				Clipperz.NotificationCenter.register(null, 'oneTimePasswordAdded', this, 'render');
				Clipperz.NotificationCenter.register(null, 'oneTimePassword_saveChanges_done', this, 'render');

			}, this));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("OTP.MainComponent.render - 4: " + res); return res;});

			deferredResult.callback();
		}
	},

	//-------------------------------------------------------------------------

	'printOneTimePasswords': function() {
		var newWindow;
		var activeOneTimePasswords;

//MochiKit.Logging.logDebug(">>> printAllData");
		newWindow = window.open("", "");
		newWindow.document.write(
"<html>" +
"<header>" +
"	<title>Clipperz One Time Password</title>" +
"<style>" +
"div.oneTimePassword_print h2 {" +
"	font-family: monospace;" +
"	font-weight: normal;" +
"	padding: 10px 20px;" +
"}" +
"</style>" +
"" +
"<!--[if IE]>" +
"<style>" +
"</style>" +
"<![endif]-->" +
"" +
"</header>" +
"<body>" +
"</body>" +
"</html>"
		);

		activeOneTimePasswords = MochiKit.Base.filter(function(aOneTimePassword) {return (aOneTimePassword.isExpired() == false)}, MochiKit.Base.values(this.user().oneTimePasswordManager().oneTimePasswords()).reverse());

		MochiKit.Iter.forEach(activeOneTimePasswords, MochiKit.Base.partial(function(aWindow, aOneTimePassword) {
			MochiKit.DOM.withWindow(aWindow, MochiKit.Base.partial(function(aOneTimePassword) {
				var newBlock;

				newBlock = MochiKit.DOM.DIV({'class': 'oneTimePassword_print'},
					MochiKit.DOM.H2(null, aOneTimePassword.password())
				);
				MochiKit.DOM.appendChildNodes(MochiKit.DOM.currentDocument().body, newBlock);
			
			}, aOneTimePassword));
		}, newWindow));
	},

	//-------------------------------------------------------------------------
	
	'generateRandomBase32OTPValue': function(aButton) {
		var randomValue;
		var	result;

		randomValue = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(160/8);
		result = randomValue.toBase32String();
		result = result.replace(/.{4}\B/g, '$&' + ' ');
		result = result.replace(/(.{4} ){2}/g, '$&' + '- ');

		return result;
	},
	
	//-------------------------------------------------------------------------

	'createNewOneTimePassword': function() {
		var newOneTimePassword;
		var password;
		
		password = this.generateRandomBase32OTPValue();
		newOneTimePassword = new Clipperz.PM.DataModel.OneTimePassword({
									user:this.user(),
									password:password
		});
		this.user().oneTimePasswordManager().addOneTimePassword(newOneTimePassword);
		Clipperz.PM.Components.MessageBox.showProgressPanel(MochiKit.Base.method(newOneTimePassword, 'saveChanges'), null, this.getDom('createNewOneTimePasswordButton'));
	},
	
	//-------------------------------------------------------------------------
	
	'oneTimePasswordCheckboxes': function() {
		return MochiKit.DOM.getElementsByTagAndClassName('input', 'otpCheckbox', this.getId('oneTimePasswords_tbody'));
	},

	'checkedOneTimePasswordCheckboxes': function() {
		return MochiKit.Base.filter(function(aCheckbox) {return (aCheckbox.checked == true)}, this.oneTimePasswordCheckboxes());
	},
	
	//-------------------------------------------------------------------------

	'selectAllOneTimePasswords': function(anEvent) {
		var checkboxes;
		var i,c;

		anEvent.stop();
		checkboxes = this.oneTimePasswordCheckboxes();
		c = checkboxes.length;
		for (i=0; i<c; i++) {
			checkboxes[i].checked = true;
		}

		this.updateDeleteButtonStatus();
	},
	
	'selectNoneOneTimePasswords': function(anEvent) {
		var checkboxes;
		var i,c;
		
		anEvent.stop();
		checkboxes = this.oneTimePasswordCheckboxes();
		c = checkboxes.length;
		for (i=0; i<c; i++) {
			checkboxes[i].checked = false;
		}

		this.updateDeleteButtonStatus();
	},

	'selectUsedOneTimePasswords': function(anEvent) {
		var checkboxes;
		var oneTimePasswordManager;
		var i,c;
		
		anEvent.stop();
		oneTimePasswordManager = this.user().oneTimePasswordManager();
		checkboxes = this.oneTimePasswordCheckboxes();
		c = checkboxes.length;
		for (i=0; i<c; i++) {
			var matchingOneTimePassword;

			matchingOneTimePassword = oneTimePasswordManager.oneTimePasswordWithReference(checkboxes[i].name);
			checkboxes[i].checked = matchingOneTimePassword.isExpired();
		}

		this.updateDeleteButtonStatus();
	},

	'selectUnusedOneTimePasswords': function(anEvent) {
		var checkboxes;
		var oneTimePasswordManager;
		var i,c;
		
		anEvent.stop();
		oneTimePasswordManager = this.user().oneTimePasswordManager();
		checkboxes = this.oneTimePasswordCheckboxes();
		c = checkboxes.length;
		for (i=0; i<c; i++) {
			var matchingOneTimePassword;

			matchingOneTimePassword = oneTimePasswordManager.oneTimePasswordWithReference(checkboxes[i].name);
			checkboxes[i].checked = !matchingOneTimePassword.isExpired();
		}

		this.updateDeleteButtonStatus();
	},

	//-------------------------------------------------------------------------

	'handleCheckboxClick': function(anEvent) {
		this.updateDeleteButtonStatus();
	},
	
	//-------------------------------------------------------------------------

	'deleteSelectedOneTimePasswords': function() {
		var deferredResult;
		var otpToDelete;
		var i,c;
		
		otpToDelete = this.checkedOneTimePasswordCheckboxes();
		c = otpToDelete.length;
		for (i=0; i<c; i++) {
//MochiKit.Logging.logDebug("otp to delete: " + otpToDelete[i].name);
			this.user().oneTimePasswordManager().deleteOneTimePasswordWithReference(otpToDelete[i].name);
		};

		deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("ActiveOTPPanel.deleteSelectedOneTimePasswords - 0: " + res); return res;});
		deferredResult.addCallback(Clipperz.PM.Components.MessageBox.showProgressPanel, MochiKit.Base.method(this.user().oneTimePasswordManager(), 'saveChanges'), null, null);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("ActiveOTPPanel.deleteSelectedOneTimePasswords - 1: " + res); return res;});
		deferredResult.addCallback(MochiKit.Base.method(this, 'render'));
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("ActiveOTPPanel.deleteSelectedOneTimePasswords - 2: " + res); return res;});
		deferredResult.callback();
	},
	
	//-------------------------------------------------------------------------

	'user': function() {
		return this._user;
	},
	
	//-------------------------------------------------------------------------

	'shouldRender': function() {
		return this._shouldRender;
	},
	
	'setShouldRender': function(aValue) {
		this._shouldRender = aValue;
	},
	
	'tabSelectedHandler': function(anEvent) {
		if ((this.shouldRender()) && (anEvent.source().selectedTab() == 'manageOTPTab')) {
			this.render();
			this.setShouldRender(false);
		}
	},

	//-------------------------------------------------------------------------

	'deleteButton': function() {
		return this._deleteButton;
	},
	
	'setDeleteButton': function(aValue) {
		this._deleteButton = aValue;
	},

	'updateDeleteButtonStatus': function() {
		if (this.checkedOneTimePasswordCheckboxes().length > 0) {
			this.deleteButton().enable();
		} else {
			this.deleteButton().disable();
		}
	},
	
	//-------------------------------------------------------------------------

	'printButton': function() {
		return this._printButton;
	},
	
	'setPrintButton': function(aValue) {
		this._printButton = aValue;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

