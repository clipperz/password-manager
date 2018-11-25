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
Clipperz.Base.module('Clipperz');


Clipperz.CSVProcessor = function(args) {
	args = args || {};

//	this._status		= undefined;
//	this._error_input	= undefined;
//	this._string		= undefined;
//	this._fields		= undefined;

	this._quoteChar		= args['quoteChar'] 	||	"\0x42";
	this._eol			= args['eol']			||	"";
	this._escapeChar	= args['escapeChar']	||	"\0x42";
	this._separatorChar	= args['separatorChar']	||	",";
	this._binary		= args['binary']		||	false;
	this._alwaysQuote	= args['alwaysQuote']	||	false;

	return this;
}

//=============================================================================

Clipperz.CSVProcessor.prototype = MochiKit.Base.update(null, {

	//-------------------------------------------------------------------------
	
	'quoteChar': function() {
		return this._quoteChar;
	},
	
	//-------------------------------------------------------------------------
	
	'eol': function() {
		return this._eol;
	},

	//-------------------------------------------------------------------------

	'escapeChar': function() {
		return this._escapeChar;
	},
	
	//-------------------------------------------------------------------------

	'separatorChar': function() {
		return this._separatorChar;
	},
	
	'setSeparatorChar': function(aValue) {
		this._separatorChar = aValue;
	},
	
	//-------------------------------------------------------------------------
	
	'binary': function() {
		return this._binary;
	},

	//-------------------------------------------------------------------------

	'alwaysQuote': function() {
		return this._alwaysQuote;
	},
	
	//-------------------------------------------------------------------------
/*
	'parse': function(aValue) {
		var result;
		var lines;
		var parameter;
		
//Clipperz.logDebug(">>> CSVProcessor.parse");
		result = [];
		
		lines = aValue.replace(/\r?\n/g, "\n").replace(/^\n*                    /g, "").replace(/\n$/g, "");;
		parameter = {
			line: lines
		}

		do {
			var fields;
			
			fields = this.parseLine(parameter);
			
			if (fields != null) {
				result.push(fields);
			}
			
			parameter.line = parameter.line.replace(/^\n*                        /g, "").replace(/\n$/g, "");

//Clipperz.logDebug("line: '" + parameter.line + "'");
		} while (parameter.line != "");
//Clipperz.logDebug("--- CSVProcessor.parse - result: " + Clipperz.Base.serializeJSON(result));
//Clipperz.logDebug("<<< CSVProcessor.parse");

		return result;
	},
*/
	//-------------------------------------------------------------------------

	'deferredParse_core': function(aContext) {
		var deferredResult;
		
		if (aContext.line == "") {
			deferredResult = MochiKit.Async.succeed(aContext.result);
		} else {
			var fields;
			
			fields = this.parseLine(aContext);
			if (fields != null) {
				aContext.result.push(fields);
			}

			aContext.line = aContext.line.replace(/^\n*/g, "").replace(/\n$/g, "");
			
			deferredResult = new Clipperz.Async.Deferred("CVSProcessor.deferredParse_core");
//			deferredResult.addCallback(Clipperz.NotificationCenter.deferredNotification, this, 'importProcessorProgressUpdate', {status:'processing', size:aContext.size, progress:(aContext.size - aContext.line.length)});
			deferredResult.addCallbackPass(MochiKit.Signal.signal, this, 'importProcessorProgressUpdate', {status:'processing', size:aContext.size, progress:(aContext.size - aContext.line.length)});
			deferredResult.addCallback(MochiKit.Async.wait, 0.2);
			deferredResult.addMethod(this, 'deferredParse_core')
			deferredResult.callback(aContext);
		}
		
		return deferredResult;
	},
	
	//.........................................................................
	
	'deferredParse': function(aValue) {
		var deferredResult;
		var lines;
		var context;

		lines = aValue.replace(/\r?\n/g, "\n").replace(/^\n*/g, "").replace(/\n$/g, "");

		context = {
			line: lines,
			size: lines.length,
			result: []
		}

		deferredResult = new Clipperz.Async.Deferred("CSVProcessor.deferredParse");
		deferredResult.addMethod(this, 'deferredParse_core');
		deferredResult.callback(context);

		return deferredResult;
	},
	
	//-------------------------------------------------------------------------

	'parseLine': function(aParameter) {
		var result;
		var palatable;
		var line;
		var processedField;
		
		result = [];

		do {
			processedField = this.parseField(aParameter);
			if (processedField != null) {
				result.push(processedField)
			};
		} while (processedField != null);

		return result;
	},

	//-------------------------------------------------------------------------

	'parseField': function(aParameter) {
		var result;
		
		var inQuotes;
		var validRegExp;
		var singleQuoteBeginRegexp;
		var escapedQuoteBeginRegexp;
		var singleQuoteCommaEndRegexp;
		var singleQuoteNewLineEndRegexp;
		var commaBeginRegexp;
		var newlineRegexp;
		
		
		singleQuoteBeginRegexp		= new RegExp("^" + '\\' + this.quoteChar());
		escapedQuoteBeginRegexp		= new RegExp("^" + '\\' + this.escapeChar() + '\\' + this.quoteChar());
		singleQuoteCommaEndRegexp	= new RegExp("^" + '\\' + this.quoteChar() + '\\' + this.separatorChar());
		singleQuoteNewLineEndRegexp	= new RegExp("^" + '\\' + this.quoteChar() + "\n");
		commaBeginRegexp			= new RegExp("^" + '\\' + this.separatorChar());
		newlineRegexp				= new RegExp("^\n");

		inQuotes = false;
		
//Clipperz.logDebug("#################################### '" + aParameter.line + "'");
		if (aParameter.line == "") {
			if (aParameter.isThereAnEmptyFinalField == true) {
				aParameter.isThereAnEmptyFinalField = false;
				result = "";
			} else {
				result = null;
			}
		} else {
			if (this.binary()) {
				validRegexp = /^./;
//				validRegexp = /^[^\\]/;
			} else {
				validRegexp = /^[\t\040-\176]/;
			}
		
			try {
				var done;
			
				done = false;
				result = "";
			
				while (!done) {
					if (aParameter.line.length < 1) {
//Clipperz.logDebug("---> 1: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
						if (inQuotes == true) {
//Clipperz.logDebug("---> 1.1: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
							throw new Error("CSV Parsing error; end of string, missing closing double-quote...");
						} else {
//Clipperz.logDebug("---> 1.2: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
							done = true;
						}
					} else if (escapedQuoteBeginRegexp.test(aParameter.line)) {
//Clipperz.logDebug("---> 2.1: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
						result += this.quoteChar();
						aParameter.line = aParameter.line.substr(2, aParameter.line.length - 1);
//Clipperz.logDebug("<--- 2.2: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
					} else if (singleQuoteBeginRegexp.test(aParameter.line)) {
//Clipperz.logDebug("---> 3: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
						if (inQuotes == true) {
							if (aParameter.line.length == 1) {
//Clipperz.logDebug("---> 3.1: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
								aParameter.line = '';
								done = true;
							} else if (singleQuoteCommaEndRegexp.test(aParameter.line)) {
//Clipperz.logDebug("---> 3.3: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
								aParameter.line = aParameter.line.substr(2, aParameter.line.length - 1);
								done = true;
//Clipperz.logDebug("<--- 3.3: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
							} else if (singleQuoteNewLineEndRegexp.test(aParameter.line)) {
								aParameter.line = aParameter.line.substr(1, aParameter.line.length - 1);
								done = true;
							} else {
								throw new Error("CSV Parsing error; double-quote, followed by undesirable character (bad character sequence)... " + aParameter.line);
							}
						} else {
//Clipperz.logDebug("---> 4: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
							if (result == "") {
//Clipperz.logDebug("---> 4.1: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
								inQuotes = true;
								aParameter.line = aParameter.line.substr(1, aParameter.line.length - 1);
//Clipperz.logDebug("<--- 4.1: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
							} else {
								throw new Error("CSV Parsing error; double-quote, outside of double-quotes (bad character sequence)...");
							}
						}
					} else if (commaBeginRegexp.test(aParameter.line)) {
//Clipperz.logDebug("---> 5: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
						if (inQuotes) {
//Clipperz.logDebug("---> 5.1: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
							result += aParameter.line.substr(0 ,1);
							aParameter.line = aParameter.line.substr(1, aParameter.line.length - 1);
//Clipperz.logDebug("<--- 5.1: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
						} else {
//Clipperz.logDebug("---> 5.2: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
							aParameter.line = aParameter.line.substr(1, aParameter.line.length - 1);
							if (newlineRegexp.test(aParameter.line) || aParameter.line == "") {
//Clipperz.logDebug("######");
								aParameter.isThereAnEmptyFinalField = true;
							};
							done = true;
//Clipperz.logDebug("<--- 5.2: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
						}
					} else if (validRegexp.test(aParameter.line)) {
//Clipperz.logDebug("---> 6: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
						result += aParameter.line.substr(0, 1);
						aParameter.line = aParameter.line.substr(1, aParameter.line.length - 1);
//Clipperz.logDebug("<--- 6: '" + aParameter.line.replace(/\n/g, "\\n") + "'");
					} else if (newlineRegexp.test(aParameter.line)) {
						if (inQuotes == true) {
							result += aParameter.line.substr(0 ,1);
							aParameter.line = aParameter.line.substr(1, aParameter.line.length - 1);
						} else {
							if (result == "") {
								if (aParameter.isThereAnEmptyFinalField == true) {
									aParameter.isThereAnEmptyFinalField = false;
								} else {
									result = null;
								}
							}
							
							done = true;
						}
					} else { 
						throw new Error("CSV Parsing error; an undesirable character... '" + aParameter.line.substr(0,1) + "'");
					}
				}
			} catch(exception) {
				Clipperz.logError(exception.message);
//				result = null;
				throw exception;
			}
		}

//if (result != null) {
//	Clipperz.logDebug("<=== result: '" + result.replace(/\n/g, "\\n") + "'");
//} else {
//	Clipperz.logDebug("<=== result: NULL");
//}
		
		return result;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});


