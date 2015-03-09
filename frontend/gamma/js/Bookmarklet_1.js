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

_cble = null;

//-----------------------------------------------------------------------------

isLoginForm = function(aForm) {
	var inputFields;
	var passwordFieldsFound;
	var i,c;

//console.log("is login form: " + aForm.name + " (" + aForm.id + ")");
	passwordFieldsFound = 0;
	inputFields = aForm.elements;
	c = inputFields.length;
	for (i=0; i<c; i++) {
		if (inputFields[i].type == "password") {
			passwordFieldsFound ++;
		}
    }
//console.log("number of password fields found: " + passwordFieldsFound);
	return (passwordFieldsFound == 1);
};

//-----------------------------------------------------------------------------

findLoginForm = function(aDocument, aLevel) {
	var	result;
	var	documentForms;
	var i,c;

	result = null;

	try {
		documentForms = aDocument.getElementsByTagName('form');

		c = documentForms.length;
		for (i=0; (i<c) && (result == null); i++) {
			if (isLoginForm(documentForms[i])) {
				result = documentForms[i];
			}
		}

		if ((result == null) && (aLevel == 0)) {
			var iFrames;
		
			iFrames = aDocument.getElementsByTagName('iframe');
			c = iFrames.length;
			for (i=0; (i<c) && (result == null); i++) {
				result = findLoginForm(iFrames[i].contentDocument, (aLevel + 1));
			}
		}
	} catch (e) {
		_cble = e;
	}
	
	return result;
};

//-----------------------------------------------------------------------------

inputElementValues = function(anInputElement) {
	var	result;

//	if ((anInputElement instanceof HTMLInputElement) && (anInputElement.getAttribute('name') != null)) {
	if ((anInputElement.tagName.toLowerCase() == "input") && (anInputElement.getAttribute('name') != null)) {
		result = {};
		result.type = anInputElement.getAttribute('type') || "text";
		result.name = anInputElement.getAttribute('name');
//		result.value = anInputElement.getAttribute('value');
		result.value = anInputElement.value;
		if (anInputElement.type.toLowerCase() == 'radio') {
			result.checked = anInputElement.checked;
		}
//	} else if ((anInputElement instanceof HTMLSelectElement) && (anInputElement.getAttribute('name') != null)) {
	} else if ((anInputElement.tagName.toLowerCase() == 'select') && (anInputElement.getAttribute('name') != null)) {
		var	options;
		var c,i;
		
//console.log("input element values: %o", anInputElement);
		result = {};
		result.type = "select";
		result.name = anInputElement.getAttribute('name');

		result.options = [];
		options = anInputElement.options;
		c = options.length;
		for (i=0; i<c; i++) {
			var	option;
			
			option = {};
			option.selected = options[i].selected;
			option.label = options[i].label || options[i].innerHTML;
			option.value = options[i].value;
			result.options.push(option);
		}
	} else {
		result = null;
	}

	return result;
};

//-----------------------------------------------------------------------------

formParameters = function(aLoginForm) {
	var	result;
	var	i, c;
	var	action;

	if (aLoginForm == null) {
		result = null;
	} else {
		var	radioValues;
		var	radioValueName;
		
		result = {};
		radioValues = {};
		
		action = aLoginForm.action;
		if (action.constructor != String) {
			action = aLoginForm.getAttribute('action');
		}

		if (/^https?\:\/\/.*/.test(action)) {
			action = action;
		} else if (/^\/.*/.test(action)) {
			action = window.location.protocol + '/' + '/' + window.location.hostname + action;
		} else {
			action = window.location.href.replace(/\/[^\/]*$/, '/' + action);
		}

		result.attributes = {};
		result.attributes.action = action;
		result.attributes.method = aLoginForm.getAttribute('method');

		result.inputs = [];
		c = aLoginForm.elements.length;
		for (i=0; i<c; i++) {
			var	inputElement;
			var	elementValues;
		
			inputElement = aLoginForm.elements[i];
			elementValues = inputElementValues(inputElement);
			if (elementValues != null) {
				if (elementValues.type != "radio") {
					result.inputs.push(elementValues);
				} else {
					var	radioValue;
					var	values;
					
					radioValue = radioValues[elementValues.name];
					if (radioValue == null) {
						radioValue = {};
						radioValue.name = elementValues.name;
						radioValue.type = "radio";
						radioValue.options = [];

						radioValues[elementValues.name] = radioValue;
					}
					
					values = {};
					values.value = elementValues.value;
					values.checked = elementValues.checked;
					
					radioValue.options.push(values);
				}
			}
		}
		
		for (radioValueName in radioValues) {
			if (typeof(radioValues[radioValueName]) != "function") {
				result.inputs.push(radioValues[radioValueName]);
			}
		}
	}
	
	return result;
};

//-----------------------------------------------------------------------------

pageParameters = function() {
	var result;
	
	result = {};
	result['title'] = document.title;
//<link rel="icon" href="http://example.com/favicon.ico" type="image/x-icon">

	return result;
};

//-----------------------------------------------------------------------------

reprString = function (o) { 
 	return ('"' + o.replace(/(["\\])/g, '\\$1') + '"'
			).replace(/[\f]/g, "\\f"
			).replace(/[\b]/g, "\\b"
			).replace(/[\n]/g, "\\n"
			).replace(/[\t]/g, "\\t"
			).replace(/[\r]/g, "\\r");
};

//-----------------------------------------------------------------------------

serializeJSON = function (o) {
	var objtype = typeof(o);
	if (objtype == "number" || objtype == "boolean") {
		return o + "";
	} else if (o === null) {
		return "null";
	}

//	var m = MochiKit.Base;
//	var reprString = m.reprString;
	if (objtype == "string") {
		return reprString(o);
	}

	//	recurse
	var me = arguments.callee;
	//	array
	if (objtype != "function" && typeof(o.length) == "number") {
		var res = [];
		for (var i = 0; i < o.length; i++) {
			var val = me(o[i]);
			if (typeof(val) != "string") {
				val = "undefined";
			}
			res.push(val);
		}
		return "[" + res.join(",\n") + "]";
	}

	//	undefined is outside of the spec
	if (objtype == "undefined") {
//		throw new TypeError("undefined can not be serialized as JSON");
		throw new TypeError("error");
	}

	//	generic object code path
	res = [];
	for (var k in o) {
		if (typeof(o[k]) != "function") {
			var useKey;
			if (typeof(k) == "number") {
				useKey = '"' + k + '"';
			} else if (typeof(k) == "string") {
				useKey = reprString(k);
			} else {
				//	skip non-string or number keys
				continue;
			}

			val = me(o[k]);
			if (typeof(val) != "string") {
				//	skip non-serializable values
				continue;
			}
			res.push(useKey + ":" + " " + val);
		}
	}

	return "{" + res.join(",\n") + "}";
};

//-----------------------------------------------------------------------------

closeBookmarklet = function() {
	var	bookmarkletDiv;

	bookmarkletDiv = document.getElementById("clipperz_bookmarklet");
	bookmarkletDiv.parentNode.removeChild(bookmarkletDiv);
};

//-----------------------------------------------------------------------------

logFormParameters = function(someParameters, anException) {
	var	newDiv;
	var base_url;
	var	help_url;
//	var	base_image_url;
	var	logo_image_url;
	var	background_image_url;
	var	close_image_url;
	var	bookmarklet_textarea;
	var innerHTML;

//
//	Obsolete: image -> base64 encoding done here: http://www.motobit.com/util/base64-decoder-encoder.asp
//	conversion done using the Filemark Maker application: http://www.insanelygreattees.com/news/?p=51
//
	
	base_url = "http://www.clipperz.com/";
	help_url = base_url + "help/bookmarklet";
//	base_image_url = base_url + "files/clipperz.com/bookmarklet/";
//	logo_image_url = base_image_url + "logo.png";
	logo_image_url = "data:image/png;charset=utf-8;base64,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";
//	background_image_url = base_image_url + "background.png";
	background_image_url = "data:image/png;charset=utf-8;base64,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Llccuo/bX7mtHavu9cv5eoDJDda5Pqy1sepe31SbHYj247Uj1V/nst/6md4GjwFsDetFqTkGrWO9729auKY364HqwumpX1VTtRqnqc71dtDWm7aAq2/Y+bq53o7Ucufr5noTs+7jjG2zgtVEph2n5uuZqpo6cOCOj+39yK2fn5nZ8dler39Tr9ff1+v1rtmk5weukOFwOD8cDo4NB4OfnFta+Pe33n3x8SNHDh2uUagWazVayzVqQzdeY9uMaaY9ITWxml45Zm644c4b9u05+Bcz03NfrOpNbcLzAVvKcHlx8czXjh7//pePHDn0eo2itdQ6lmvt0nFskwarvYxrh2q6quYO3vrFz+/c+dG/r15v54TPA2xxw+Fw/uTJo1964aWHH6+qszUKV3O0l41jLw8nmXg2itVMVW3/9ME//KPt26//cq/Xm5vgOYAQvV5vdnZ21xf2XPfx08feev75zrc3ZQ9rkgmrvQxsQjVTVdtu+/h9X9i1a/8/9nq9NY9/8y37655776zP3HFb7d+/p3bs0DJIs7Bwto4efbuee/aFeuzRZ+qVl4+u+f5wWIMTJ177k/995bFvVNWZGk1bZ2t1edhMW5ds3GB196xmaxSruX37fu2mGw/c+a/tDfXp6an60z/7/brv/ruq3++v/4hAnMFgUA8/9FQ9+MAjtbS0fP724XA4//Jr3/zd48d/8GqNonWmqs7V6r7WWPtZ4y4Jm2A1pyrM1Cha2z9x02//1fT07K82d5yenqoH/+GP6+7P3V6dgQsI1+v16pc+dWN9+lduricf/04NBsPm9tkd266/9o03v/eftbpv1d58rxpjmThJsJpTF2aqaq6q5vbu/eWb9u659e+qeufHqC/9+R/U3Z+7fcynARLccMPe2r17Rz3z9A/P3zY9PfuJhbMnvnHmzPH3au27hGOf4jDO+qx7GkOz2T67b89tv9U+deHmW/bXffffNc7rAsLcd/9ddfMt+1u39Kb2XX/w7qraXqvbRu1zNC95yTXJhlL77PWZqpqdm939G+073HPvnfas4EOi3+/XPffeuea27XPX/XqNVmCztTrcTNWFf5J3cc8x5mtb7x3CuX5/+hfad/rMHbeN+fBAou7vfH9q5kBVbatRsGZr7YR1ycYNVvtPcM4vCfv9qb3tO+3fv2fMhwcSdX/np3pT19dowmpOe2omrO4nuFyUcfewmssmXNNVNdPr9be37+g8K/hw6f7O9/pTzXTV7F+NHauqyZaE7T2sZsoC6Gomq+a4Ypvu3Y+D6XcOgK7uR0t1+3HRJt10735CA0BX+93Bsaerqsmmom4lncYOrGe9yeqKB6vqwkkLoGviUDUmnbAAfpaN3hG8ome6t1+ICQvYyHr/W9ZVWRIC/CybthoTLCDGJKc1AIzrii4Ju2tRAQPez0a9uGJ/Swhwsd6vGxfdFHtYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXEECwghmABMQQLiCFYQAzBAmIIFhBDsIAYggXE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";
//	close_image_url = base_image_url + "close.png";
	close_image_url = "data:image/png;charset=utf-8;base64,iVBORw0KGgoAAAANSUhEUgAAABEAAAARCAIAAAC0D9CtAAAABGdBTUEAANbY1E9YMgAAABl0RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAGiSURBVHjaYvz//z8DiYAFU+jhg5cXLtx9/+4LLy+nlpa8hpYsIyMjTj03rj/u71l74fxdZEEFRfG8wkBrG224CCPcbfv2XKivWfjr1x+s7snM8U1IcoOwmeA24NEABNOnbN698yyKHqCTIBrcPU2WrKiIjHYEsh2c9BctK0tMcYeomdC37sePXyAW0G0P7r8wM8yBoFs3n/wHgwP7L/79+xfI+Pnzl6VpHkR27+7zQBGQPcie3rr5JIRh76DHxASS3bHtzN8//yCCF87fgbrt/fsvcD3Ll+4/eOASnPvg/suO1hVwLjACoHqA8QAXBfrB1k4HOaDjEl3hXF4+LqgeYMTBRZNS3CFOAtoAEUlIdGNmgQaVlrYcVA8wpoHmQUT377v469fvTRuOR4W1zZi25cf3X6DAAPuHi4vd1l4XEadHj1wtypsB0QY0Fe5pZHZOvn9svAsifoBJAxjTEDZcETLbw8s0Js4ZPe0AATCmgRH35vUn5BQAdFJSqgdQAzylMqLlBWBMHztyDRgPoHTNxwX0NNAP/PzcyGoYycg/AAEGABYVzxqE3YcJAAAAAElFTkSuQmCCCg==";
	
//	newDiv.parentNode.removeChild(newDiv);
	newDiv = document.createElement('div');
	newDiv.setAttribute("id", "clipperz_bookmarklet");
//	newDiv.setAttribute("style", "width:270px; height:400px; padding:20px 0px 0px 20px; margin:0px; border:0px; background-color:transparent; background-repeat:no-repeat; position:absolute; z-index:20000; top:40px; left:40px; background-image:url(" + background_image_url + ");");

	innerHTML = "";
	innerHTML +=	"<style>div#ClipperzBackgroundDIV { width:290px; height:420px; padding:20px 0px 0px 20px; margin:0px; border:0px; background-color:transparent; background-repeat:no-repeat; position:absolute; z-index:20000; top:40px; left:40px; background-image:url(" + background_image_url + ") }</style>";
	innerHTML +=	"<div style=\"border:0px; margin:0px; padding:0px; padding-left:10px;\">" +
						"<img  style=\"padding-top:5px;\" src=\"" + logo_image_url + "\">" +
						"<a href=\"javascript:closeBookmarklet();\">" + 
							"<img style=\"padding-left:28px; padding-bottom:10px;\" border=0 src=\"" + close_image_url + "\">" +
						"</a>" +
					"</div>";
						
	if ((someParameters != null) && (anException == null)) {
		innerHTML +=	"<div style=\"width:255px; border-top:1px dotted #336;\">" +
							"<div style=\"line-height:10pt; margin-right:10px; margin-top:5px; padding:5px 10px; color:#666; text-align:left; font-family:sans-serif;\">" +
								"<p style=\"margin:0px; font-weight:bold; font-size:10pt; font-family:sans-serif;\">How to add a new card or a direct login to an existing card for this website:</p>" +
								"<ol style=\"padding:0px 0px 0px 20px; font-size:9pt; font-family:sans-serif;\">" +
									"<li>Copy the content of the text area below (Ctrl-C)</li>" +
									"<li>Go to your Clipperz account</li>" +
									"<li>Click \"Add new card\" or select the related card</li>" +
									"<li>Paste the direct login configuration (Ctrl-V)</li>" +
									"<li>Complete and review the details, then click \"Save\"</li>" +
								"</ol>" +
							"</div>" +
						"</div>";
		innerHTML +=	"<textarea id=\"bookmarklet_textarea\" style=\"border:2px solid #333366; font-family:sans-serif; font-size:8pt; color:#336; width:240px; height:135px; padding:4px; background-color:white; margin:0px 10px;\">" +
							serializeJSON(someParameters) +
						"</textarea>";
	} else if ((someParameters == null) && (anException == null)) {
		innerHTML += "<div>No login form has been found on the page</div><div>Get some help <a href=\"#\">here</a></div>";
	} else {
		innerHTML += "<div>An error happened while processing the page</div><div>Get some help <a href=\"#\">here</a></div><div>" + anException.name + " - " + anException.message + "</div>";
	}
	
//	newDiv.innerHTML = "<div style='width:290px; height:420px; padding:20px 0px 0px 20px; margin:0px; border:0px; background-color:transparent; background-repeat:no-repeat; position:absolute; z-index:20000; top:40px; left:40px; background-image:url(" + background_image_url + ")'>" + innerHTML + "</div>";
	newDiv.innerHTML = "<div id=\"ClipperzBackgroundDIV\">" + innerHTML + "</div>";
	
	document.body.appendChild(newDiv);
	
	if ((someParameters != null) && (anException == null)) {
		bookmarklet_textarea = document.getElementById("bookmarklet_textarea");
		bookmarklet_textarea.focus();
		bookmarklet_textarea.select();
	}
};

//-----------------------------------------------------------------------------

getLoginFormConfiguration = function() {
	var	parameters;

	try {
		parameters = {};
		parameters.page = pageParameters();
		parameters.form = formParameters(findLoginForm(document, 0));
		parameters.version = "0.2.3";
		logFormParameters(parameters, _cble);
	} catch (e) {
	//	parameters = "No login form has been found"
		logFormParameters(parameters, e);
	}
}

//-----------------------------------------------------------------------------

getLoginFormConfiguration();



