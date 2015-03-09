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

Clipperz.Base.module('Clipperz.PM.UI.Common.Controllers');

Clipperz.PM.UI.Common.Controllers.ProgressBarController = function(args) {
	args = args || {};

	this._numberOfSteps	= 0;
	this._currentStep	= 0;

	MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'initProgress',		this, 'initProgressHandle');
	MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'updateProgress',	this, 'updateProgressHandle');
	MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'advanceProgress',	this, 'advanceProgressHandle');
	MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'progressDone',		this, 'progressDoneHandle');

	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.Common.Controllers.ProgressBarController.prototype, {

	'toString': function() {
		return "Clipperz.PM.UI.Common.Controllers.ProgressBarController";
	},

	//-----------------------------------------------------------------------------
	
	'numberOfSteps': function() {
		return this._numberOfSteps;
	},
	
	'setNumberOfSteps': function (aValue) {
		this._numberOfSteps = aValue;
	},

	'updateNumberOfSteps': function (aValue) {
		this._numberOfSteps += aValue;
	},

	//-----------------------------------------------------------------------------

	'currentStep': function() {
		return this._currentStep;
	},

	'advanceCurrentStep': function () {
		this._currentStep ++;
	},

	//-----------------------------------------------------------------------------

	'completedPercentage': function () {
		var result;
//Clipperz.log(">>> completedPercentage" + this.currentStep() + "/" + this.numberOfSteps(), this.currentStep() / this.numberOfSteps());
		if (this.numberOfSteps() == 0) {
			result = 0;
		} else {
			result = (Math.min(100, 100 * (this.currentStep() / this.numberOfSteps())));
		}
//Clipperz.log("<<< completedPercentage", result);
		return result;
	},

	//-----------------------------------------------------------------------------

	'resetStatus': function () {
		this._numberOfSteps	= 0;
		this._currentStep	= 0;
	},

	//-----------------------------------------------------------------------------
	
	'updateProgress': function () {
//Clipperz.log(">>> updateProgress: " + this.completedPercentage() + "%");
		MochiKit.Signal.signal(this, 'updateProgress', this.completedPercentage());
	},

	//=============================================================================

	'initProgressHandle': function (anEvent) {
//Clipperz.log(">>> initProgressHandle - steps: " + (anEvent != null ? anEvent['steps'] : 0));
		this.resetStatus();
		if (anEvent != null) {
			this.setNumberOfSteps(anEvent['steps']);
		}
		MochiKit.Signal.signal(this, 'initProgress');
		this.updateProgress();
	},

	//.............................................................................

	'updateProgressHandle': function (anEvent) {
		this.updateNumberOfSteps(anEvent['extraSteps']);
//Clipperz.log("=== updateProgressHandle - steps: " + this.numberOfSteps() + " (extra " + anEvent['extraSteps'] + ")");
		this.updateProgress();
	},

	//.............................................................................

	'advanceProgressHandle': function (anEvent) {
		this.advanceCurrentStep();
//Clipperz.log("--- advanceProgressHandle: " + this.currentStep() + "/" + this.numberOfSteps());
		this.updateProgress();
	},

	//.............................................................................

	'progressDoneHandle': function (anEvent) {
//Clipperz.log("<<< progressDoneHandle: " + this.currentStep() + "/" + this.numberOfSteps());
		this.resetStatus();
		MochiKit.Signal.signal(this, 'progressDone');
	},

	//=============================================================================
	__syntaxFix__: "syntax fix"
});

Clipperz.PM.UI.Common.Controllers.ProgressBarController.defaultController = new Clipperz.PM.UI.Common.Controllers.ProgressBarController();
