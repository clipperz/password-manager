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

"use strict";
Clipperz.Base.module('Clipperz.Async');

Clipperz.Async.VERSION = "0.1";
Clipperz.Async.NAME = "Clipperz.Async";

Clipperz.Async.Deferred = function(aName, args) {
	args = args || {};

	Clipperz.Async.Deferred.superclass.constructor.call(this, args.canceller);

	this._args = args;
	this._name = aName || "Anonymous deferred";
	this._count = 0;
	this._shouldTrace = ((CLIPPERZ_DEFERRED_TRACING_ENABLED === true) || (args.trace === true));
	this._vars = null;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.Async.Deferred, MochiKit.Async.Deferred, {

	'name': function () {
		return this._name;
	},

	'args': function () {
		return this._args;
	},

	//-----------------------------------------------------------------------------

	'callback': function (aValue) {
		if (this._shouldTrace) {
			Clipperz.log("CALLBACK " + this._name, aValue);
		}

		if (this.chained == false) {
			var deferredName = this._name;
			
			this.addErrback(function(aResult) {
				var message;
				
				if (! (aResult instanceof MochiKit.Async.CancelledError)) {
					message = "ERROR [" + deferredName + "]";
				} else {
					message = "CANCELLED - " + deferredName;
				}
				Clipperz.log(message, aResult);

				return aResult;
			});

			if (this._shouldTrace) {
				var resultMessage;

				resultMessage = "RESULT   " + this._name + " <==";
//				this.addCallback(function(aResult) {
				Clipperz.Async.Deferred.superclass.addCallback.call(this, function(aResult) {
					Clipperz.log(resultMessage, aResult);
					return aResult;
				});
			}
		}

		if (CLIPPERZ_DEFERRED_CALL_LOGGING_ENABLED === true) {
			Clipperz.log("callback " + this._name, this);
		}

		return Clipperz.Async.Deferred.superclass.callback.apply(this, arguments);
	},

	//-----------------------------------------------------------------------------

	'addCallback': function () {
		var message;
		
		if (this._shouldTrace) {
			this._count ++;
			message = "[" + this._count + "] " + this._name + " ";
//			this.addBoth(function(aResult) {Clipperz.log(message + "-->", aResult); return aResult;});
			this.addCallbacks(
				function(aResult) {Clipperz.log("-OK- " + message + "-->"/*, aResult*/); return aResult;},
				function(aResult) {if (! (aResult instanceof MochiKit.Async.CancelledError)) { Clipperz.log("FAIL " + message + "-->"/*, aResult*/);}; return aResult;}
			);
		}

		Clipperz.Async.Deferred.superclass.addCallback.apply(this, arguments);

		if (this._shouldTrace) {
//			this.addBoth(function(aResult) {Clipperz.log(message + "<--", aResult); return aResult;});
			this.addCallbacks(
				function(aResult) {Clipperz.log("-OK- " + message + "<--", aResult); return aResult;},
				function(aResult) {if (! (aResult instanceof MochiKit.Async.CancelledError)) { Clipperz.log("FAIL " + message + "<--", aResult);}; return aResult;}
			);
		}
	},

	//=============================================================================

	'addCallbackPass': function() {
		var passFunction;

		passFunction = MochiKit.Base.partial.apply(null, arguments);

		this.addCallback(function() {
			var result;

			result = arguments[arguments.length -1];
			passFunction();

			return result;
		});
	},

	//-----------------------------------------------------------------------------

	'addErrbackPass': function() {
		var passFunction;

		passFunction = MochiKit.Base.partial.apply(null, arguments);

		this.addErrback(function() {
			var result;

			result = arguments[arguments.length -1];
			passFunction();

			return result;
		});
	},

	//-----------------------------------------------------------------------------

	'addBothPass': function() {
		var passFunction;

		passFunction = MochiKit.Base.partial.apply(null, arguments);

		this.addBoth(function() {
			var result;

			result = arguments[arguments.length -1];
			passFunction();

			return result;
		});
	},

	//-----------------------------------------------------------------------------

	'addIf': function (aThenBlock, anElseBlock) {
		this.addCallback(MochiKit.Base.bind(function (aValue) {
			var deferredResult;

			if (!MochiKit.Base.isUndefinedOrNull(aValue) && aValue) {
				deferredResult = Clipperz.Async.callbacks(this._name + " <then>", aThenBlock, null, aValue);
			} else {
				deferredResult = Clipperz.Async.callbacks(this._name + " <else>", anElseBlock, null, aValue);
			}
			
			return deferredResult;
		}))
	},

	//-----------------------------------------------------------------------------

	'addMethod': function () {
		this.addCallback(MochiKit.Base.method.apply(this, arguments));
	},

	//-----------------------------------------------------------------------------

	'addMethodcaller': function () {
		this.addCallback(MochiKit.Base.methodcaller.apply(this, arguments));
	},

	//=============================================================================

	'addLog': function (aLog) {
		if (CLIPPERZ_DEFERRED_LOGGING_ENABLED) {
			this.addBothPass(function(res) {Clipperz.log(aLog + " ", res);});
		}
	},

	//=============================================================================
	
	'acquireLock': function (aLock) {
//		this.addCallback(function (aResult) {
//			return Clipperz.Async.callbacks("Clipperz.Async.acquireLock", [
//				MochiKit.Base.method(aLock, 'acquire'),
//				MochiKit.Base.partial(MochiKit.Async.succeed, aResult)
//			], {trace:false});
//		});

		this.addCallback(MochiKit.Base.method(aLock, 'acquire'));
	},

	'releaseLock': function (aLock) {
//		this.addCallback(function (aResult) {
//			return Clipperz.Async.callbacks("Clipperz.Async.release <ok>", [
//				MochiKit.Base.method(aLock, 'release'),
//				MochiKit.Base.partial(MochiKit.Async.succeed, aResult)
//			], {trace:false});
//		});
//		this.addErrback(function (aResult) {
///Clipperz.log("releaseLock.addErrback:", aResult);
//			return Clipperz.Async.callbacks("Clipperz.Async.release <fail>", [
//				MochiKit.Base.method(aLock, 'release'),
//				MochiKit.Base.partial(MochiKit.Async.fail, aResult)
//			], {trace:false});
//		});
		
//		this.addBothPass(MochiKit.Base.method(aLock, 'release'));
		this.addCallbackPass(MochiKit.Base.method(aLock, 'release'));
		this.addErrback(function (anError) {
			aLock.release();

			return anError;
		});
	},

	//=============================================================================

	'collectResults': function (someRequests) {
		this.addCallback(Clipperz.Async.collectResults(this._name + " <collect results>", someRequests, this._args));
	},

	'addCallbackList': function (aRequestList) {
		this.addCallback(Clipperz.Async.callbacks, this._name + " <callback list>", aRequestList, this._args);
	},

	//=============================================================================

	'vars': function () {
		if (this._vars == null) {
			this._vars = {}
		}
		
		return this._vars;
	},

	'setValue': function (aKey) {
		this.addCallback(MochiKit.Base.bind(function (aValue) {
			this.vars()[aKey] = aValue;
			return aValue;
		}, this));
	},
	
	'getValue': function (aKey) {
		this.addCallback(MochiKit.Base.bind(function () {
			return this.vars()[aKey];
		}, this));
	},

	'values': function () {
		this.addCallback(MochiKit.Base.bind(function () {
			return this.vars();
		}, this));
	},

	//=============================================================================

	'wait': function (someSeconds) {
		this.addCallback(MochiKit.Async.wait, someSeconds);
	},

	//=============================================================================
	
	__syntaxFix__: "syntax fix"
});

//#############################################################################

Clipperz.Async.DeferredSynchronizer = function(aName, someMethods) {
	this._name = aName || "Anonymous deferred Synchronizer";
	this._methods  = someMethods;

	this._numberOfMethodsDone	= 0;
	this._methodResults = [];

	this._result = new Clipperz.Async.Deferred("Clipperz.Async.DeferredSynchronizer # " + this.name(), {trace:false});
	this._result.addMethod(this, 'methodResults');
	this._result.addCallback(function(someResults) {
		var	cancels;
		var errors;
		var result;

		cancels = MochiKit.Base.filter(function(aResult) { return (aResult instanceof MochiKit.Async.CancelledError)}, someResults);
		
		if (cancels.length == 0) {
			errors = MochiKit.Base.filter(function(aResult) { return (aResult instanceof Error)}, someResults);
		
			if (errors.length == 0) {
//				result = MochiKit.Async.succeed(someResults);
				result = someResults;
			} else {
				result = MochiKit.Async.fail(someResults);
			}
		} else {
			result = MochiKit.Async.fail(cancels[0]);
		}

		return result;
	}/*, this._methodResults */);
	
	return this;
}

MochiKit.Base.update(Clipperz.Async.DeferredSynchronizer.prototype, {

	//-----------------------------------------------------------------------------

	'name': function() {
		return this._name;
	},

	//-----------------------------------------------------------------------------

	'methods': function() {
		return this._methods;
	},
	
	'methodResults': function() {
		return this._methodResults;
	},

	//-----------------------------------------------------------------------------

	'result': function() {
		return this._result;
	},

	//-----------------------------------------------------------------------------

	'numberOfMethodsDone':function() {
		return this._numberOfMethodsDone;
	},

	'incrementNumberOfMethodsDone': function() {
		this._numberOfMethodsDone ++;
	},

	//-----------------------------------------------------------------------------
	
	'run': function(args, aValue) {
		var deferredResults;
		var i, c;
		
		deferredResults = [];
		args = args || {};

		c = this.methods().length;
		for (i=0; i<c; i++) {
			var deferredResult;
			var	methodCalls;
			var ii, cc;

//Clipperz.log("TYPEOF", typeof(this.methods()[i]));
			if (typeof(this.methods()[i]) == 'function') {
				methodCalls = [ this.methods()[i] ];
			} else {
				methodCalls = this.methods()[i];
			}
			
			cc = methodCalls.length;
			deferredResult = new Clipperz.Async.Deferred("Clipperz.Async.DeferredSynchronizer.run => " + this.name() + "[" + i + "]", args);
			for (ii=0; ii<cc; ii++) {
				deferredResult.addCallback(methodCalls[ii]);
			}
			deferredResult.addBoth(MochiKit.Base.method(this, 'handleMethodCallDone', i));

			deferredResults.push(deferredResult);
		}
		
		for (i=0; i<c; i++) {
			deferredResults[i].callback(aValue);
		}
		
		return this.result();
	},
	
	//-----------------------------------------------------------------------------

	'handleMethodCallDone': function(anIndexValue, aResult) {
		this.incrementNumberOfMethodsDone();
		this.methodResults()[anIndexValue] = aResult;
		
		if (this.numberOfMethodsDone() < this.methods().length) {
			//	nothing to do here other than possibly log something
		} else if (this.numberOfMethodsDone() == this.methods().length) {
			this.result().callback();
		} else if (this.numberOfMethodsDone() > this.methods().length) {
			alert("Clipperz.Async.Deferred.handleMethodCallDone -> WTF!");
			//	WTF!!! :(
		}
		
	},

	//-----------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});

//#############################################################################

MochiKit.Base.update(Clipperz.Async, {

	'callbacks': function (aName, someFunctions, someArguments, aCallbackValue) {
		var deferredResult;
		var i, c;
		
		deferredResult = new Clipperz.Async.Deferred(aName, someArguments);
		c = someFunctions.length;
		for (i=0; i<c; i++) {
			deferredResult.addCallback(someFunctions[i]);
		}
		deferredResult.callback(aCallbackValue);

		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'forkAndJoin': function (aName, someMethods, args) {
		return MochiKit.Base.partial(function (aName, someMethods, args, aValue) {
			var synchronizer;
			var	result;
		
			args = args || {};
			synchronizer = new Clipperz.Async.DeferredSynchronizer(aName, someMethods);
			result = synchronizer.run(args, aValue);
		
			return result;
		}, aName, someMethods, args);
	},
	
	//-------------------------------------------------------------------------

	'collectResults': function(aName, someRequests, args) {
		return MochiKit.Base.partial(function(aName, someRequests, args, aValue) {
			var deferredResult;
			var requestKeys;
			var methods;

			requestKeys = MochiKit.Base.keys(someRequests);
			methods = MochiKit.Base.values(someRequests);

			deferredResult = new Clipperz.Async.Deferred(aName, args);
			deferredResult.addCallback(Clipperz.Async.forkAndJoin(aName + " [inner forkAndJoin]", methods, args));
			deferredResult.addBoth(function(someResults) {
				var returnFunction;
				var results;
				var i,c;
				var result;

				if (someResults instanceof MochiKit.Async.CancelledError) {
					returnFunction = MochiKit.Async.fail;
					result = someResults;
				} else {
					if (someResults instanceof Error) {
						returnFunction = MochiKit.Async.fail;
						results = someResults['message'];
					} else {
						returnFunction = MochiKit.Async.succeed;
						results = someResults;
					}

					result = {};
			
					c = requestKeys.length;
					for (i=0; i<c; i++) {
						result[requestKeys[i]] = results[i];
					}
				} 

				return returnFunction.call(null, result);
			});
			deferredResult.callback(aValue);
		
			return deferredResult;
		}, aName, someRequests, args);
	},

	//-------------------------------------------------------------------------

	'collectAll': function (someDeferredObjects) {
		var deferredResult;
		
		deferredResult = new MochiKit.Async.DeferredList(someDeferredObjects, false, false, false);
		deferredResult.addCallback(function (aResultList) {
			return MochiKit.Base.map(function (aResult) {
				if (aResult[0]) {
					return aResult[1];
				} else {
					throw aResult[1];
				}
			}, aResultList);
		});
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'setItem': function (anObject, aKey, aValue) {
		anObject[aKey] = aValue;
			
		return anObject;
	},

	'setItemOnObject': function (aKey, aValue, anObject) {
		anObject[aKey] = aValue;
			
		return anObject;
	},

	'setDeferredItemOnObject': function (aKey, aDeferredFunction, anObject) {
		return Clipperz.Async.callbacks("Clipperz.Async.setDeferredItemOnObject", [
			aDeferredFunction,
			MochiKit.Base.partial(Clipperz.Async.setItem, anObject, aKey)
		], {trace:false}, anObject);
	},

	//-------------------------------------------------------------------------

	'deferredIf': function (aName, aThenBlock, anElseBlock) {
		return function (aValue) {
			var deferredResult;

			if (!MochiKit.Base.isUndefinedOrNull(aValue) && aValue) {
				deferredResult = Clipperz.Async.callbacks(aName + " <then>", aThenBlock, null, aValue);
			} else {
				deferredResult = Clipperz.Async.callbacks(aName + " <else>", anElseBlock, null, aValue);
			}
			
			return deferredResult;
		}
	},

	//-------------------------------------------------------------------------

	'log': function(aMessage, aResult) {
		if (CLIPPERZ_DEFERRED_LOGGING_ENABLED) {
			Clipperz.log(aMessage + " ", aResult);
		}
		
		return aResult;
	},
	
	//=========================================================================

	'deferredCompare': function (aComparator, aDeferred, bDeferred) {
		var deferredResult;

		deferredResult = new Clipperz.Async.Deferred("Clipperz.Async.deferredCompare", {trace:false});
		deferredResult.addCallback(Clipperz.Async.collectAll, [aDeferred, bDeferred]);
		deferredResult.addCallback(function (someResults) {
			var result;

			if (aComparator(someResults[0], someResults[1]) > 0) {
				result = MochiKit.Async.succeed();
			} else {
				result = MochiKit.Async.fail();
			};

			return result;
		});
		deferredResult.callback();
		
		return deferredResult;
	},

	//-------------------------------------------------------------------------

	'insertIntoSortedArray': function (anObject, aDeferredComparator, aSortedResult) {
		var deferredResult;
		var i, c;

		if (aSortedResult.length == 0) {
			deferredResult = MochiKit.Async.succeed([anObject]);
		} else {
			deferredResult = new Clipperz.Async.Deferred("Clipperz.Async.insertIntoSortedArray", {trace:false});
			c = aSortedResult.length + 1;
			for (i=0; i<c; i++) {
				deferredResult.addCallback(function (aDeferredComparator, aObject, bObject, aContext) {
					var innerDeferredResult;
					
					innerDeferredResult = new Clipperz.Async.Deferred("Clipperz.Async.insertIntoSortedArray <inner compare>", {trace:false});
					innerDeferredResult.addCallback(aDeferredComparator, aObject, bObject);
					innerDeferredResult.addErrback(MochiKit.Async.fail, aContext);
					innerDeferredResult.callback();
					
					return innerDeferredResult;
				}, aDeferredComparator, anObject, aSortedResult[i], i);
			}
			deferredResult.addMethod(aSortedResult, 'push', anObject);
			deferredResult.addErrback(function (anError) {
				aSortedResult.splice(anError.message, 0, anObject);
			})
			deferredResult.addBoth(MochiKit.Async.succeed, aSortedResult);
			deferredResult.callback();
		}
		
		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	'deferredSort': function (aDeferredComparator, someObjects) {
		var deferredResult;
		var i, c;
				
		deferredResult = new Clipperz.Async.Deferred("Clipperz.Async.deferredSort", {trace:false});
		c = someObjects.length;
		for (i=0; i<c; i++) {
			deferredResult.addCallback(Clipperz.Async.insertIntoSortedArray, someObjects[i], aDeferredComparator);
			if ((i % 50) == 0) {
//	Clipperz.log("######### sort wait ##########");
				deferredResult.addCallback(MochiKit.Async.wait, 0.5);
			}
		}		
		deferredResult.callback([]);

		return deferredResult;
	},

	//=========================================================================

	'deferredFilter': function (aFunction, someObjects) {
		var	deferredResult;
		var	i, c;

		deferredResult = new Clipperz.Async.Deferred("Clipperz.Async.deferredFilter", {trace:false});
		c = someObjects.length;
		for (i=0; i<c; i++) {
			deferredResult.addCallback(function (aFunction, anObject, anIndex, aResult) {
				var innerDeferredResult;
				
				innerDeferredResult = new Clipperz.Async.Deferred("Clipperz.Async.deferredFilter <inner - " + anIndex + ">", {trace:false});
				innerDeferredResult.addCallback(aFunction, anObject);
				innerDeferredResult.addCallback(function (aFilterResult) {
					if (aFilterResult) {
						aResult.push(anObject);
					};
				});
				innerDeferredResult.addBoth(MochiKit.Async.succeed, aResult);
				innerDeferredResult.callback();

				return innerDeferredResult;
			}, aFunction, someObjects[i], i);
		}
		deferredResult.callback([]);
		
		return deferredResult;
	},
	
	'forEach': function (aFunction) {
		return MochiKit.Base.partial(function (aFunction, anIterable) {
			MochiKit.Iter.forEach(anIterable, aFunction);
		}, aFunction);
	},

	//=========================================================================

	'or': function (someValues) {
		return Clipperz.Async.callbacks("Clipperz.Async.or", [
			MochiKit.Base.values,
			MochiKit.Base.flattenArguments,
//function (aValue) { Clipperz.log("Record.hasAnyCleanTextData - flatten", aValue); return aValue; },
			function(someInnerValues) {
				return MochiKit.Iter.some(someInnerValues, MochiKit.Base.operator.identity);
			}
		], {trace:false}, someValues);
	},

	//=========================================================================

	'clearResult': function () {},
	
	//=========================================================================
	__syntaxFix__: "syntax fix"
});


//#############################################################################

var CLIPPERZ_DEFERRED_LOGGING_ENABLED = true;
var CLIPPERZ_DEFERRED_TRACING_ENABLED = false;
var CLIPPERZ_DEFERRED_CALL_LOGGING_ENABLED = false;
