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



Clipperz.PM.Components.MessageBoxImplementation = function() {
	this._step = 0;
	this._steps = 0;
	
	return this;
};

//YAHOO.extendX(Clipperz.PM.Components.MessageBoxImplementation, Clipperz.PM.Components.BaseComponent, {
Clipperz.PM.Components.MessageBoxImplementation.prototype = MochiKit.Base.update(null, {
	
	'toString': function() {
		return "Clipperz.PM.Components.MessageBox";
	},

	//-----------------------------------------------------

	'step': function() {
		return this._step;
	},
	
	'setStep': function(aValue) {
		if (aValue == 'next') {
			this._step = this._step + 1;
		} else {
			this._step = aValue;
		}
		
		if (this._step > this.steps()) {
//MochiKit.Logging.logDebug("overstepping: " + this._step + " (" + this.steps() + ")");
			this._step = this.steps();
		}
	},
	
	//-----------------------------------------------------

	'steps': function() {
		return this._steps;
	},
	
	'setSteps': function(aValue) {
		if (aValue.constructor == String) {
			if (aValue.charAt(0) == '+') {
				this._steps += aValue.substring(1)*1;
			} else if (aValue.charAt(0) == '-') {
				this._steps -= aValue.substring(1)*1;
			} else {
				this._steps = aValue.substring(1)*1;
			}
		} else {
			this._steps = aValue;
		}
	},
	
	//-----------------------------------------------------

	'deferredShow': function(aConfiguration, anAnimationTargetElement, aValue) {
		this.show(aConfiguration, anAnimationTargetElement);
		
		return aValue;
	},
	
	'show': function(aConfiguration, anAnimationTargetElement) {
		var	messageBoxConfiguration;
		
		messageBoxConfiguration = MochiKit.Base.clone(aConfiguration);
		messageBoxConfiguration.msg = messageBoxConfiguration.text;
		messageBoxConfiguration.animEl = anAnimationTargetElement;
		messageBoxConfiguration.progress = messageBoxConfiguration.showProgressBar;
		messageBoxConfiguration.closable = messageBoxConfiguration.showCloseButton;
		this.setSteps(aConfiguration.steps || 0);
		this.setStep(aConfiguration.step || 0);
		delete messageBoxConfiguration.buttons;
		
		Clipperz.YUI.MessageBox.show(messageBoxConfiguration);
	},

	//-----------------------------------------------------

	'update': function(someValues) {
//MochiKit.Logging.logDebug(">>> MessageBox.update");
		if (someValues.title) {
			Clipperz.YUI.MessageBox.getDialog().setTitle(someValues.title);
		};

		if (someValues.text) {
			Clipperz.YUI.MessageBox.updateText(someValues.text);
		};
		
		if (typeof(someValues.showProgressBar) != 'undefined') {
            Clipperz.YUI.MessageBox.progressElement().setDisplayed(someValues.showProgressBar);
            Clipperz.YUI.MessageBox.updateProgress(0);
		};

		if (typeof(someValues.steps) != 'undefined') {
			this.setSteps(someValues.steps);
		};

		if (typeof(someValues.step) != 'undefined') {
			this.setStep(someValues.step);
		} else {
			this.setStep('next');
		}
		Clipperz.YUI.MessageBox.updateProgress(this.step() / this.steps());
		

		if (typeof(someValues.fn) != 'undefined') {
            Clipperz.YUI.MessageBox.opt().fn = someValues.fn;
		};

		if (typeof(someValues.scope) != 'undefined') {
            Clipperz.YUI.MessageBox.opt().scope = someValues.scope;
		};
		
		if (someValues.buttons) {
			Clipperz.YUI.MessageBox.updateButtons(someValues.buttons);
		};
		
//		if (someValues.title) {
//			Clipperz.YUI.MessageBox.getDialog().setTitle(someValues.title + " [" + this.step() + " / " + this.steps() + "]");
//		};
		
//MochiKit.Logging.logDebug("--- MessageBox.update - step: " + this.step() + " / " + this.steps() + " - " + someValues.text);
//MochiKit.Logging.logDebug("<<< MessageBox.update");
	},

	//-----------------------------------------------------
	
	'hide': function(anAnimationTargetElement) {
		if (anAnimationTargetElement) {
			Clipperz.YUI.MessageBox.getDialog().animateTarget = anAnimationTargetElement;
		}
		
		Clipperz.YUI.MessageBox.hide();
	},
	
	//-----------------------------------------------------
	__syntaxFix__: '__syntaxFix__'
});


//##########################################################

_clipperz_pm_components_messageBox = null;

Clipperz.PM.Components.MessageBox = function() {
	if (_clipperz_pm_components_messageBox == null) {
		_clipperz_pm_components_messageBox = new Clipperz.PM.Components.MessageBoxImplementation();
	}
	
	return _clipperz_pm_components_messageBox;
}

//---------------------------------------------------------

Clipperz.PM.Components.MessageBox.showProgressPanel = function(aCallback, anErrback, anActivationItem) {
	var deferredResult;

	deferredResult = new MochiKit.Async.Deferred();
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.PM.Components.MessageBox.showProgressPanel - 0: " + res); return res;});
	deferredResult.addCallback(MochiKit.Base.method(Clipperz.PM.Components.MessageBox(), 'deferredShow'),
		{
			title: "",
			text: "",
			width:240,
			showProgressBar:true,
			showCloseButton:false,
			fn:MochiKit.Base.method(deferredResult, 'cancel'),
			scope:this,
			buttons:{
			//	'ok':Clipperz.PM.Strings['loginMessagePanelInitialButtonLabel']
			}
		},
		anActivationItem
	);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.PM.Components.MessageBox.showProgressPanel - 1: " + res); return res;});
	deferredResult.addCallback(aCallback);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.PM.Components.MessageBox.showProgressPanel - 2: " + res); return res;});
	deferredResult.addCallback(MochiKit.Async.wait, 0.5);
	deferredResult.addCallback(function(res) {
		Clipperz.PM.Components.MessageBox().hide(YAHOO.ext.Element.get(anActivationItem));
		return res;
	});
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.PM.Components.MessageBox.showProgressPanel - 3: " + res); return res;});
	deferredResult.addErrback(anErrback);
//deferredResult.addBoth(function(res) {MochiKit.Logging.logDebug("Clipperz.PM.Components.MessageBox.showProgressPanel - 4: " + res); return res;});
	deferredResult.callback();
	
	return deferredResult;
};

