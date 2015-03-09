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
if (typeof(Clipperz.PM.DataModel) == 'undefined') { Clipperz.PM.DataModel = {}; }


//#############################################################################

Clipperz.PM.DataModel.DirectLoginReference = function(args) {
	args = args || {};

//MochiKit.Logging.logDebug(">>> new DirectLoginReference: " + Clipperz.Base.serializeJSON(MochiKit.Base.keys(args)));
//MochiKit.Logging.logDebug(">>> new DirectLoginReference - record: " + args.record);
	this._user = args.user;
	
	if (args.directLogin != null) {
		this._reference = args.directLogin.reference();
		this._recordReference = args.directLogin.record().reference();
		this._label = args.directLogin.label();
		this._favicon = args.directLogin.favicon() || null;

		this._directLogin = args.directLogin;
		this._record = args.directLogin.record();
	} else {
		this._reference = args.reference;
		this._recordReference = args.record;
		this._label = args.label;
		this._favicon = Clipperz.Base.sanitizeFavicon(args.favicon) || null;
	
		this._directLogin = null;
		this._record = null;
	}

	this._fixedFavicon = null;
	
	return this;
}

Clipperz.PM.DataModel.DirectLoginReference.prototype = MochiKit.Base.update(null, {

	'user': function() {
		return this._user;
	},
	
	//-------------------------------------------------------------------------

	'reference': function() {
		return this._reference;
	},

	//-------------------------------------------------------------------------

	'synchronizeValues': function(aDirectLogin) {
		this._label = aDirectLogin.label();
		this._favicon = aDirectLogin.favicon();
	},

	//-------------------------------------------------------------------------

	'label': function() {
		return this._label;
	},

	//-------------------------------------------------------------------------

	'recordReference': function() {
		return this._recordReference;
	},

	//-------------------------------------------------------------------------

	'record': function() {
//MochiKit.Logging.logDebug(">>> DirectLoginReference.record");
		if (this._record == null) {
			this._record = this.user().records()[this.recordReference()];
		}
		
//MochiKit.Logging.logDebug("<<< DirectLoginReference.record");
		return this._record;
	},
	
	//-------------------------------------------------------------------------

	'favicon': function() {
		return this._favicon;
	},

	//-------------------------------------------------------------------------

	'fixedFavicon': function() {
		var result;
		
		if (this._fixedFavicon == null) {
			result = this.favicon();

			if (Clipperz_IEisBroken && (this.user().preferences().disableUnsecureFaviconLoadingForIE()) && (result.indexOf('https://') != 0)) {
				result = Clipperz.PM.Strings['defaultFaviconUrl_IE'];
				this.setFixedFavicon(result);
			}
		} else {
			result = this._fixedFavicon;
		}
		
		return result;
	},

	'setFixedFavicon': function(aValue) {
		this._fixedFavicon = aValue;
	},
	
	//-------------------------------------------------------------------------

	'setupJumpPageWindow': function(aWindow) {
//MochiKit.Logging.logDebug(">>> DirectLoginReference.setupJumpPageWindow - " + aWindow);
		try {
			MochiKit.DOM.withWindow(aWindow, MochiKit.Base.bind(function() {
				MochiKit.DOM.appendChildNodes(MochiKit.DOM.currentDocument().body,
					MochiKit.DOM.H1(null, "Loading " + this.label())
				);
			}, this));
		} catch(e) {
			MochiKit.Logging.logDebug("EXCEPTION: " + e);
		}
//MochiKit.Logging.logDebug("<<< DirectLoginReference.setupJumpPageWindow");
	},

	//-------------------------------------------------------------------------
	
	'deferredDirectLogin': function() {
		var deferredResult;

//MochiKit.Logging.logDebug(">>> DirectLoginReference.deferredDirectLogin - " + this);
		deferredResult = new MochiKit.Async.Deferred();
//MochiKit.Logging.logDebug("--- DirectLoginReference.deferredDirectLogin - 1");
		deferredResult.addCallback(MochiKit.Base.method(this.record(), 'deferredData'));
//MochiKit.Logging.logDebug("--- DirectLoginReference.deferredDirectLogin - 2");
		deferredResult.addCallback(function(aRecord, aDirectLoginReference) {
			return aRecord.directLogins()[aDirectLoginReference];
		}, this.record(), this.reference());
//MochiKit.Logging.logDebug("--- DirectLoginReference.deferredDirectLogin - 3");
		deferredResult.callback();
//MochiKit.Logging.logDebug("<<< DirectLoginReference.deferredDirectLogin");
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'handleMissingFaviconImage': function(anEvent) {
//MochiKit.Logging.logDebug(">>> DirectLoginReference.handleMissingFaviconImage");
		anEvent.stop();
		MochiKit.Signal.disconnectAll(anEvent.src());
		this.setFixedFavicon(Clipperz.PM.Strings['defaultFaviconUrl']);
//MochiKit.Logging.logDebug("--- DirectLoginReference.handleMissingFaviconImage - fixedFavicon: " + this.fixedFavicon());
//MochiKit.Logging.logDebug("--- DirectLoginReference.handleMissingFaviconImage - anEvent.src().src: " + anEvent.src().src);
//		MochiKit.DOM.swapDOM(anEvent.src(), MochiKit.DOM.IMG({src:'this.fixedFavicon()'}));
		anEvent.src().src = this.fixedFavicon();
//MochiKit.Logging.logDebug("<<< DirectLoginReference.handleMissingFaviconImage");
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
	
});

