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
if (typeof(Clipperz.Visual) == 'undefined') { Clipperz.Visual = {}; }

Clipperz.Visual.VERSION = "0.1";
Clipperz.Visual.NAME = "Clipperz.Visual";

MochiKit.Base.update(Clipperz.Visual, {

	//-------------------------------------------------------------------------

	'__repr__': function () {
		return "[" + this.NAME + " " + this.VERSION + "]";
	},

	//-------------------------------------------------------------------------

	'toString': function () {
		return this.__repr__();
	},

	//-------------------------------------------------------------------------

	'deferredResize': function (anElement, someOptions) {
		var deferredResult;
		var moveTransition;
		var scaleTransition;
		var duration;
		
		duration = someOptions.duration || 0.5;

		deferredResult = new Clipperz.Async.Deferred("Visual.deferredResize", {trace:false});
		deferredResult.addCallback(MochiKit.Async.succeed, arguments[arguments.length - 1]);

		moveTransition	= MochiKit.Visual.Transitions.linear;	//MochiKit.Visual.Transitions.sinoidal;
		scaleTransition	= MochiKit.Visual.Transitions.linear;	//MochiKit.Visual.Transitions.sinoidal;

		MochiKit.Style.setElementPosition(anElement, {x:someOptions.from.position.x, y:someOptions.from.position.y }, 'px');

		new MochiKit.Visual.Parallel([
			new MochiKit.Visual.Move(anElement, {x:someOptions.to.position.x, y:someOptions.to.position.y, mode:'absolute', transition:moveTransition, sync:true}),
			new Clipperz.Visual.Resize(anElement, {fromSize:{h:someOptions.from.dimensions.h, w:someOptions.from.dimensions.w}, toSize:{h:someOptions.to.dimensions.h, w:someOptions.to.dimensions.w}, transition:scaleTransition, scaleContent:false, scaleFromCenter:false, restoreAfterFinish:true, sync:true})
		], {duration:duration, afterFinish:MochiKit.Base.method(deferredResult, 'callback')})

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'deferredAnimation': function (anAnimation, someParameters, someOptions) {
		var	deferredResult;
		var afterFinishCallback;
		var options;

		deferredResult = new Clipperz.Async.Deferred("Clipperz.Visual.deferredAnimation", {trace:false});
		deferredResult.addCallback(MochiKit.Async.succeed, arguments[arguments.length - 1]);

		if (MochiKit.Base.isUndefinedOrNull(someOptions)) {
			options = {}
		} else {
			options = someOptions;
		}
		
		if (MochiKit.Base.isUndefinedOrNull(someOptions['afterFinish'])) {
			options['afterFinish'] = MochiKit.Base.noop;
		}

		MochiKit.Base.update(options, {
			'afterFinish': MochiKit.Base.compose(options['afterFinish'], MochiKit.Base.method(deferredResult, 'callback'))
		});

		new anAnimation(someParameters, options);

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'deferredAnimations': function (aSinchronizationType, someAnimations, someOptions) {
		var deferredResult;
		var	options;

		deferredResult = new Clipperz.Async.Deferred("Visual.deferredParallelAnimations", {trace:false});
		deferredResult.addCallback(MochiKit.Async.succeed, arguments[arguments.length - 1]);

		options = someOptions;
		if (MochiKit.Base.isUndefinedOrNull(someOptions['afterFinish'])) {
			options['afterFinish'] = MochiKit.Base.noop;
		}
		MochiKit.Base.update(options, {
			'afterFinish': MochiKit.Base.compose(options['afterFinish'], MochiKit.Base.method(deferredResult, 'callback'))
		});
		
		new aSinchronizationType(someAnimations, options)

		return deferredResult;
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

});

//#############################################################################

/** @id Clipperz.Visual.Resize */
Clipperz.Visual.Resize = function (element, percent, options) {
	var cls = arguments.callee;
	if (!(this instanceof cls)) {
		return new cls(element, percent, options);
	}
	this.__init__(element, percent, options);
};

Clipperz.Visual.Resize.prototype = new MochiKit.Visual.Base();

MochiKit.Base.update(Clipperz.Visual.Resize.prototype, {
	__class__ : Clipperz.Visual.Resize,

	__init__: function (element, options) {
		this.element = MochiKit.DOM.getElement(element);
		options = MochiKit.Base.update({
			scaleX: true,
			scaleY: true,
			scaleContent: true,
			scaleFromCenter: false,
			scaleMode: 'box',  // 'box' or 'contents' or {} with provided values
            syntax_fix: 'syntax fix'
		}, options);

		this.start(options);
	},

	setup: function () {
		this.restoreAfterFinish = this.options.restoreAfterFinish || false;
		this.elementPositioning = MochiKit.Style.getStyle(this.element, 'position');

		var ma = MochiKit.Base.map;
		var b = MochiKit.Base.bind;
		this.originalStyle = {};
		ma(b(function (k) { this.originalStyle[k] = this.element.style[k]; }, this), ['top', 'left', 'width', 'height', 'fontSize']);

		this.originalTop = this.element.offsetTop;
		this.originalLeft = this.element.offsetLeft;

		var fontSize = MochiKit.Style.getStyle(this.element, 'font-size') || '100%';
		ma(b(function (fontSizeType) {
			if (fontSize.indexOf(fontSizeType) > 0) {
				this.fontSize = parseFloat(fontSize);
				this.fontSizeType = fontSizeType;
			}
		}, this), ['em', 'px', '%']);

		this.factor = 1;

		this.dims = [this.options.fromSize.h, this.options.fromSize.w];
	},

	update: function (position) {
		this.setDimensions(	(this.options.toSize.h - this.options.fromSize.h) * position + this.options.fromSize.h,
							(this.options.toSize.w - this.options.fromSize.w) * position + this.options.fromSize.w);
	},

	finish: function () {
		if (this.restoreAfterFinish) {
			MochiKit.Style.setStyle(this.element, this.originalStyle);
		}
	},

	setDimensions: function (height, width) {
		var d = {};
		var r = Math.round;
		if (/MSIE/.test(navigator.userAgent)) {
			r = Math.ceil;
		}
		if (this.options.scaleX) {
			d.width = r(width) + 'px';
		}
		if (this.options.scaleY) {
			d.height = r(height) + 'px';
		}
		if (this.options.scaleFromCenter) {
			var topd = (height - this.dims[0])/2;
			var leftd = (width - this.dims[1])/2;
			if (this.elementPositioning == 'absolute') {
				if (this.options.scaleY) {
					d.top = this.originalTop - topd + 'px';
				}
				if (this.options.scaleX) {
					d.left = this.originalLeft - leftd + 'px';
				}
			} else {
				if (this.options.scaleY) {
					d.top = -topd + 'px';
				}
				if (this.options.scaleX) {
					d.left = -leftd + 'px';
				}
			}
		}
		MochiKit.Style.setStyle(this.element, d);
	}
});

//=============================================================================

Clipperz.Visual.squize = function (element, /* optional */ options) {
	var d = MochiKit.DOM;
	var v = MochiKit.Visual;
	var s = MochiKit.Style;

	element = d.getElement(element);
	options = MochiKit.Base.update({
		moveTransition: v.Transitions.sinoidal,
		scaleTransition: v.Transitions.sinoidal,
		opacityTransition: v.Transitions.none,
		scaleContent: true,
		scaleFromCenter: false,
		scaleX: true,
		scaleY: true
	}, options);
	var oldStyle = {
		top: element.style.top,
		left: element.style.left,
		height: element.style.height,
		width: element.style.width,
		opacity: s.getStyle(element, 'opacity')
	};

	var dims = s.getElementDimensions(element, true);
	var moveX, moveY;

	moveX = options.scaleX ? dims.w/2 : 0;
	moveY = options.scaleY ? dims.h/2 : 0;

	var elemClip;
	
	var optionsParallel = MochiKit.Base.update({
		beforeStartInternal: function (effect) {
			s.makePositioned(effect.effects[0].element);
			elemClip = s.makeClipping(effect.effects[0].element);
		},
		afterFinishInternal: function (effect) {
			s.hideElement(effect.effects[0].element);
			s.undoClipping(effect.effects[0].element, elemClip);
			s.undoPositioned(effect.effects[0].element);
			s.setStyle(effect.effects[0].element, oldStyle);
		}
	}, options);

	return new v.Parallel(
		[new v.Opacity(element, {
			sync: true, to: 0.0, from: 1.0,
			transition: options.opacityTransition
		 }),
		 new v.Scale(element, /Opera/.test(navigator.userAgent) ? 1 : 0, {
			scaleMode: {originalHeight: dims.h, originalWidth: dims.w},
			sync: true, transition: options.scaleTransition,
			scaleContent: options.scaleContent,
			scaleFromCenter: options.scaleFromCenter,
			restoreAfterFinish: true,
			scaleX: options.scaleX,
			scaleY: options.scaleY
		 }),
		 new v.Move(element, {
			 x: moveX, y: moveY, sync: true, transition: options.moveTransition
		 })
		], optionsParallel
	);
};

//-----------------------------------------------------------------------------

Clipperz.Visual.expand = function (element, /* optional */ options) {
	var d = MochiKit.DOM;
	var v = MochiKit.Visual;
	var s = MochiKit.Style;

	element = d.getElement(element);
	options = MochiKit.Base.update({
//		direction: 'center',
		moveTransition: v.Transitions.sinoidal,
		scaleTransition: v.Transitions.sinoidal,
		opacityTransition: v.Transitions.none,
		scaleContent: true,
		scaleFromCenter: false,
		scaleX: true,
		scaleY: true
	}, options);
	var oldStyle = {
		top: element.style.top,
		left: element.style.left,
		height: element.style.height,
		width: element.style.width,
		opacity: s.getStyle(element, 'opacity')
	};

	var dims = s.getElementDimensions(element, true);
	var moveX, moveY;
	
	moveX = options.scaleX ? dims.w/2 : 0;
	moveY = options.scaleY ? dims.h/2 : 0;

	var elemClip;
	
	var optionsParallel = MochiKit.Base.update({
		beforeStartInternal: function (effect) {
			s.makePositioned(effect.effects[0].element);
			elemClip = s.makeClipping(effect.effects[0].element);
		},
		afterFinishInternal: function (effect) {
			s.hideElement(effect.effects[0].element);
			s.undoClipping(effect.effects[0].element, elemClip);
			s.undoPositioned(effect.effects[0].element);
			s.setStyle(effect.effects[0].element, oldStyle);
		}
	}, options);

	return new v.Parallel(
		[new v.Opacity(element, {
			sync: true, to: 0.0, from: 1.0,
			transition: options.opacityTransition
		 }),
		 new v.Scale(element, /Opera/.test(navigator.userAgent) ? 1 : 0, {
			scaleMode: {originalHeight: dims.h, originalWidth: dims.w},
			sync: true, transition: options.scaleTransition,
			scaleContent: options.scaleContent,
			scaleFromCenter: options.scaleFromCenter,
			restoreAfterFinish: true,
			scaleX: options.scaleX,
			scaleY: options.scaleY
		 }),
		 new v.Move(element, {
			 x: moveX, y: moveY, sync: true, transition: options.moveTransition
		 })
		], optionsParallel
	);
};

//=============================================================================

