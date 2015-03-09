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

Clipperz.Base.module('Clipperz.PM.UI.Web.Components');

Clipperz.PM.UI.Web.Components.RulerComponent = function(args) {
	args = args || {};
	Clipperz.PM.UI.Web.Components.RulerComponent.superclass.constructor.apply(this, arguments);

	this._translationContext	= args.translationContext	|| Clipperz.Base.exception.raise('MandatoryParameter');
//	this._steps					= args.steps				|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._steps					= args.steps;

	this._currentStep = -1;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.RulerComponent, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.RulerComponent component";
	},

	//-------------------------------------------------------------------------

	'resetStatus': function (args) {
		args = args || {};

		if (this.currentStep() != 0) {
			var shouldAnimateTransition;
		
			shouldAnimateTransition = args.animateTransition || false;

			if (shouldAnimateTransition) {
				this.moveToFirstStep(MochiKit.Base.method(this, 'cursorMoved'));
			} else {
				this._currentStep = 0;
				this.cursorMoved();
			}
		}
	},
	
	//-------------------------------------------------------------------------

	'translationContext': function () {
		return this._translationContext;
	},

	'steps': function () {
		return this._steps;
	},

	'setSteps': function (aValue) {
		this._steps = aValue;
		this.renderStepsComponents();
		this.resetStatus();
	},

	'translatedStepDescription': function (aStep) {
		return Clipperz.PM.Strings.getValue(this.translationContext() + '.' + aStep + '.' + 'name');
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.setElement(this.append(MochiKit.DOM.currentDocument().body, [
			{tag:'div', id:this.getId('rulerWrapper'), cls:'rulerWrapper fixed', children:[
				{tag:'div', cls:'ruler', children:[
					{tag:'a', href:'#', id:this.getId('exit'), cls:'exit', html:'&nbsp;'},
					{tag:'a', href:'#', id:this.getId('smallPreviousButton'),	cls:'smallButton previous', html:'&nbsp;'},
					{tag:'a', href:'#', id:this.getId('smallNextButton'),		cls:'smallButton next',		html:'&nbsp;'},
					{tag:'div', cls:'marker', id:this.getId('marker'), children:[
						{tag:'div', cls:'previous', id:this.getId('previousButton')},
						{tag:'div', cls:'markerBody'},
						{tag:'div', cls:'next', id:this.getId('nextButton')}
					]},
					{tag:'div', cls:'steps', id:this.getId('stepsFrame')},
//					{tag:'div', cls:'steps' + ' ' + 'steps_' + this.steps().length, children:[
//						{tag:'ul', id:this.getId('steps'), children:MochiKit.Base.map(MochiKit.Base.bind(function (aStep) { return {tag:'li', children:[{tag:'span', html:this.translatedStepDescription(aStep)}]}}, this), this.steps())}
//					]},
					{tag:'div', cls:'dots', id:this.getId('dotsFrame')}
//					{tag:'div', cls:'dots' + ' ' + 'steps_' + this.steps().length, children:[
//						{tag:'ul', id:this.getId('dots'), children:MochiKit.Base.map(function (aStep) { return {tag:'li', children:[{tag:'span', html:'*'}]}}, this.steps())}
//					]}
				]}
			]}
		]));

		MochiKit.Signal.connect(this.getElement('exit'),				'onclick', this, 'handleExit');

		MochiKit.Signal.connect(this.getElement('previousButton'),		'onclick', this, 'handlePrevious');
		MochiKit.Signal.connect(this.getElement('smallPreviousButton'),	'onclick', this, 'handlePrevious');

		MochiKit.Signal.connect(this.getElement('nextButton'),			'onclick', this, 'handleNext');
		MochiKit.Signal.connect(this.getElement('smallNextButton'),		'onclick', this, 'handleNext');
		
		this.enablePrevious(false);
		this.enableNext(false);
		
//		this.cursorMoved();
	},

	//.........................................................................

	'renderStepsComponents': function () {
		var	stepsFrame;
		var dotsFrame;
	
		stepsFrames = this.getElement('stepsFrame');
		MochiKit.DOM.setElementClass(stepsFrames, 'steps');
		MochiKit.DOM.addElementClass(stepsFrames, 'steps_' + this.steps().length);
		
		stepsFrames.innerHTML = "";
		this.append(stepsFrames, {tag:'ul', id:this.getId('steps'), children:MochiKit.Base.map(
			MochiKit.Base.bind(function (aStep) { return {tag:'li', children:[{tag:'span', html:this.translatedStepDescription(aStep)}]}}, this),
			this.steps())}
		);

		dotsFrames = this.getElement('dotsFrame');
		MochiKit.DOM.setElementClass(dotsFrames, 'dots');
		MochiKit.DOM.addElementClass(dotsFrames, 'steps_' + this.steps().length);

		dotsFrames.innerHTML = "";
		this.append(dotsFrames, {tag:'ul', id:this.getId('dots'), children:MochiKit.Base.map(
			function (aStep) { return {tag:'li', children:[{tag:'span', html:'*'}]}; },
			this.steps())}
		);
	},

	//-------------------------------------------------------------------------

	'handleExit': function (anEvent) {
		anEvent.preventDefault();
		
		MochiKit.Signal.signal(this, 'exit');
	},

	//-------------------------------------------------------------------------

	'handlePrevious': function (anEvent) {
		anEvent.preventDefault();

//		if (!MochiKit.DOM.hasElementClass(this.getElement('previousButton'), 'disabled')) {
//			this.moveBackward();
//		}
		
		MochiKit.Signal.signal(this, 'moveBackward');
	},
	
	'handleNext': function (anEvent) {
		anEvent.preventDefault();

//		if (!MochiKit.DOM.hasElementClass(this.getElement('nextButton'), 'disabled')) {
//			this.moveForward();
//		}

		MochiKit.Signal.signal(this, 'moveForward');
	},

	//-------------------------------------------------------------------------

	'currentStep': function () {
		return this._currentStep;
	},

	'markerInitialOffset': function () {
		return -246;
	},

	'markerStepOffset': function () {
		return 410 / (this.steps().length - 1);
//		return 100;
	},
	
	//-------------------------------------------------------------------------

	'moveToFirstStep': function (aCallback) {
		var	stepsToMove;
		
		stepsToMove = this._currentStep;
		this._currentStep = 0;

		this.enablePrevious(false);
		this.enableNext(false);
//		MochiKit.Signal.signal(this, 'moveBackward');
		MochiKit.Base.map(
			function (anElement) { MochiKit.DOM.removeElementClass(anElement, 'selected'); },
			MochiKit.Selector.findChildElements(this.element(), ['li.selected'])
		);
		new MochiKit.Visual.Move(this.getElement('marker'), {
			x:-(this.markerStepOffset() * stepsToMove),
			mode:'relative',
			duration:(0.5 * (stepsToMove/2)),
//			afterFinish:MochiKit.Base.method(this, 'cursorMoved')
			afterFinish:MochiKit.Base.compose(MochiKit.Base.method(this, 'cursorMoved'), aCallback)
		});
	},

	'moveBackward': function (aCallback) {
		this._currentStep --;

		this.enablePrevious(false);
		this.enableNext(false);
//		MochiKit.Signal.signal(this, 'moveBackward');
		MochiKit.Base.map(
			function (anElement) { MochiKit.DOM.removeElementClass(anElement, 'selected'); },
			MochiKit.Selector.findChildElements(this.element(), ['li.selected'])
		);
		new MochiKit.Visual.Move(this.getElement('marker'), {
			x:-this.markerStepOffset(),
			mode:'relative',
			duration:0.5,
//			afterFinish:MochiKit.Base.method(this, 'cursorMoved')
			afterFinish:MochiKit.Base.compose(MochiKit.Base.method(this, 'cursorMoved'), aCallback)
		});
	},
	
	'moveForward': function (aCallback) {
		this._currentStep ++;

		if (this._currentStep < this.steps().length) {
			this.enablePrevious(false);
			this.enableNext(false);
//			MochiKit.Signal.signal(this, 'moveForward');
			MochiKit.Base.map(
				function (anElement) { MochiKit.DOM.removeElementClass(anElement, 'selected'); },
				MochiKit.Selector.findChildElements(this.element(), ['li.selected'])
			);
			new MochiKit.Visual.Move(this.getElement('marker'), {
				x:this.markerStepOffset(),
				mode:'relative',
				duration:0.5,
//				afterFinish:MochiKit.Base.method(this, 'cursorMoved')
				afterFinish:MochiKit.Base.compose(MochiKit.Base.method(this, 'cursorMoved'), aCallback)
			});
		} else {
			MochiKit.Signal.signal(this, 'done');
		}
	},
	
	//-------------------------------------------------------------------------

	'enablePrevious': function (aValue) {
		if (aValue == true) {
			MochiKit.DOM.removeElementClass(this.getElement('previousButton'), 'disabled');
			MochiKit.DOM.removeElementClass(this.getElement('smallPreviousButton'), 'disabled');
		} else {
			MochiKit.DOM.addElementClass(this.getElement('previousButton'), 'disabled');
			MochiKit.DOM.addElementClass(this.getElement('smallPreviousButton'), 'disabled');
		}
	},

//	'disablePrevious': function () {
//		MochiKit.DOM.addElementClass(this.getElement('previousButton'), 'disabled');
//	},

	//.........................................................................
	
	'enableNext': function (aValue) {
		if (aValue == true) {
			MochiKit.DOM.removeElementClass(this.getElement('nextButton'), 'disabled');
			MochiKit.DOM.removeElementClass(this.getElement('smallNextButton'), 'disabled');
		} else {
			MochiKit.DOM.addElementClass(this.getElement('nextButton'), 'disabled');
			MochiKit.DOM.addElementClass(this.getElement('smallNextButton'), 'disabled');
		}
	},

//	'disableNext': function () {
//		MochiKit.DOM.addElementClass(this.getElement('nextButton'), 'disabled');
//	},

	//-------------------------------------------------------------------------

	'cursorMoved': function () {
		MochiKit.Style.setElementPosition(this.getElement('marker'), {x:this.markerStepOffset() * this.currentStep() + this.markerInitialOffset()})
		MochiKit.Signal.signal(this, 'cursorMoved');
		
		MochiKit.DOM.addElementClass(this.getElement('steps').childNodes[this.currentStep()], 'selected');
		MochiKit.DOM.addElementClass(this.getElement('dots').childNodes[this.currentStep()], 'selected');
	},

	//-------------------------------------------------------------------------

	'setDisplayMode': function (aValue) {
		MochiKit.DOM.removeElementClass(this.getElement('rulerWrapper'), 'fixed');
		MochiKit.DOM.removeElementClass(this.getElement('rulerWrapper'), 'scrollable');
		MochiKit.DOM.addElementClass(this.getElement('rulerWrapper'), aValue);
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
