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

clipperz_copiedContentToClipboard = false;

//#############################################################################

// Simple Set Clipboard System
// Author: Joseph Huckaby

var ZeroClipboard = {
	
	version: "1.0.4",
	clients: {}, // registered upload clients on page, indexed by id
//	moviePath: 'ZeroClipboard.swf', // URL to movie
//	moviePath: 'data:application/octet-stream;charset=utf-8;base64,Q1dTCYgGAAB4nH1V61LbRhTeXV2OJNsYczFgIEC4JCGABSRpSy+B2JDQQtWJocl0BqK1tUZqhOSRZAj/8ih9hz5AXsGZTl+ndHUhxU2nO56z3/l05uye7+wc6xr6DeUxqpcUhFCNXF9ffxjNcYhRo8U8NruO0IeRLTkm+BpEa4i8/+t3GRGUrRf5tktDe41dMC8Kpd14Q/DM911GPfHCd6zCLyzwa67Tafo0sAppuOWEHZdeyY1O4ERMbnajyPcKIYteUM+qdYPQD/It12m9jX2XBcSx5EYUON6ZwunOEXsXAY+O94HsAu8iFnjULe1mYN/je5u2WLnvAsnRNGrZ6rnfDZlxwQKxRV1XSd1ulPJ1/9KDBB13pDCiZ2ygEdsGj2WHvsXU3dc7taM3e/tHanjDZVqEV2HEzpUGa3V5dVd4Ocfz+5d1/5w6nnbgU4sF+17bFwPfjzT3k691aEDPGb92KBvNX1krmrOjqLNVrVLLb7K1ln9e3WlsVjd0/Um12XXcyPFy6ZHdyHHDkT5tt1JtF/rJerqn6Wu+F/ErsWCmPyiRjrYi54KlgZP/k2Tq9gPYSh5APZOYBVra2VicAtfyn+4qZwHt2E4rVJvszPH2HNcVD2lkS5eOxW3b9f1AtplzZkeKFdDLl/wkibodmyrUsmq241raYdye5ECpdrBf+2GQf0ncA4c3gJelHRrHjd03xs+7L9UMHh9lZN149aOSwuOfcnFO3qQmbb0V44aoSVEXlHeikXQzz1/bpzek8E50XN6o4r/KLfQpU/pMx/J/yz9cxmWhDOOyhMqD5SkJjQnKYnmpIlfuVe5XHlSWKw8rC6QoEayoWk7MFwaKg6Whx4AJEBGIBEQGQQFJBaKBlAMpD6QA0gCQIsglICNAxkGYAFIBMgkwDWQGhFkgc0DuApkHsqDmMZAVIKtA1oBUgejqOmc21E1uHwF5DOQJ4C+AfAnkKyBbQL4G8g2Qb4F8B+o2wA6QZ4DrIOyCsAfCcw0lUwR/ZvgiOP3IhwgWcDZIsCipIpJkPmskBBiBgpGiIqQJOJdYPqbiOaRiUcRIQ5hrIfT055ySiKYVzMLKkKmuDM8aAyInyW1yNCOF22Q5I8Xb5FhGSjLR8n/gnv5q+U+9t4/MojnYLtEhc3jljjGCGT4Z7bXL7bG9Udwefz9xjk9Vhl3crtjqqXwifS8hWzblJToZmylTbk+vYuMOjsECWjZnkuDZvTmcwbscGvOiKVcxXThZNGVjkQebS+17pmLcJwl8sI1v4PI2uYEPt4UbuLItxrCwoplgrMagaOZTENf2NK0NryE8gWRCVE3p6ae5j3aOFwyZb8of6RT3FYn7Y9yvmjlDx1mKGncSfVTC9Z/v6QyZE7q5rpsburmpm490U+K/12g6WXYc+nR4CKX/H31zGG1z5m9Pt5okCg==',
//	moviePath: 'http://localhost:8000/tests/js/tests/Bookmarklet/ZeroClipboard.swf',
	moviePath: 'http://www.clipperz.com/files/clipperz.com/bookmarklet/0.3.0/ZeroClipboard_1.0.4.swf',
//	moviePath: './ZeroClipboard.swf',
	nextId: 1, // ID of next movie
	
	$: function(thingy) {
		// simple DOM lookup utility function
		if (typeof(thingy) == 'string') thingy = document.getElementById(thingy);
		if (!thingy.addClass) {
			// extend element with a few useful methods
			thingy.hide = function() { this.style.display = 'none'; };
			thingy.show = function() { this.style.display = ''; };
			thingy.addClass = function(name) { this.removeClass(name); this.className += ' ' + name; };
			thingy.removeClass = function(name) {
				this.className = this.className.replace( new RegExp("\\s*" + name + "\\s*"), " ").replace(/^\s+/, '').replace(/\s+$/, '');
			};
			thingy.hasClass = function(name) {
				return !!this.className.match( new RegExp("\\s*" + name + "\\s*") );
			}
		}
		return thingy;
	},
	
	setMoviePath: function(path) {
		// set path to ZeroClipboard.swf
		this.moviePath = path;
	},
	
	dispatch: function(id, eventName, args) {
		// receive event from flash movie, send to client		
		var client = this.clients[id];
		if (client) {
			client.receiveEvent(eventName, args);
		}
	},
	
	register: function(id, client) {
		// register new client to receive events
		this.clients[id] = client;
	},
	
	getDOMObjectPosition: function(obj) {
		// get absolute coordinates for dom element
		var info = {
			left: 0, 
			top: 0, 
			width: obj.width ? obj.width : obj.offsetWidth, 
			height: obj.height ? obj.height : obj.offsetHeight
		};

		while (obj) {
			info.left += obj.offsetLeft;
			info.top += obj.offsetTop;
			obj = obj.offsetParent;
		}

		return info;
	},
	
	Client: function(elem) {
		// constructor for new simple upload client
		this.handlers = {};
		
		// unique ID
		this.id = ZeroClipboard.nextId++;
		this.movieId = 'ZeroClipboardMovie_' + this.id;
		
		// register client with singleton to receive flash events
		ZeroClipboard.register(this.id, this);
		
		// create movie
		if (elem) this.glue(elem);
	}
};

ZeroClipboard.Client.prototype = {
	
	id: 0, // unique ID for us
	ready: false, // whether movie is ready to receive events or not
	movie: null, // reference to movie object
	clipText: '', // text to copy to clipboard
	handCursorEnabled: true, // whether to show hand cursor, or default pointer cursor
	cssEffects: true, // enable CSS mouse effects on dom container
	handlers: null, // user event handlers
	
	glue: function(elem) {
		// glue to DOM element
		// elem can be ID or actual DOM element object
//console.log(">>> glue");
		this.domElement = ZeroClipboard.$(elem);
		
		// float just above object, or zIndex 99 if dom element isn't set
		var zIndex = 99;
		if (this.domElement.style.zIndex) {
			zIndex = parseInt(this.domElement.style.zIndex) + 1;
		}
		
		// find X/Y position of domElement
		var box = ZeroClipboard.getDOMObjectPosition(this.domElement);
		
		// create floating DIV above element
		this.div = document.createElement('div');
		var style = this.div.style;
		style.position = 'absolute';
		style.left = '' + box.left + 'px';
		style.top = '' + box.top + 'px';
		style.width = '' + box.width + 'px';
		style.height = '' + box.height + 'px';
		style.zIndex = zIndex;
		
		// style.backgroundColor = '#f00'; // debug
		
		var body = document.getElementsByTagName('body')[0];
		body.appendChild(this.div);
		
		this.div.innerHTML = this.getHTML( box.width, box.height );
//console.log("<<< glue");
	},
	
	getHTML: function(width, height) {
		// return HTML for movie
		var html = '';
		var flashvars = 'id=' + this.id +
			'&width=' + width +
			'&height=' + height;
			
		if (navigator.userAgent.match(/MSIE/)) {
			// IE gets an OBJECT tag
			var protocol = location.href.match(/^https/i) ? 'https://' : 'http://';
			html += '<object classid="clsid:d27cdb6e-ae6d-11cf-96b8-444553540000" codebase="'+protocol+'download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=9,0,0,0" width="'+width+'" height="'+height+'" id="'+this.movieId+'" align="middle"><param name="allowScriptAccess" value="always" /><param name="allowFullScreen" value="false" /><param name="movie" value="'+ZeroClipboard.moviePath+'" /><param name="loop" value="false" /><param name="menu" value="false" /><param name="quality" value="best" /><param name="bgcolor" value="#ffffff" /><param name="flashvars" value="'+flashvars+'"/><param name="wmode" value="transparent"/></object>';
		}
		else {
			// all other browsers get an EMBED tag
			html += '<embed id="'+this.movieId+'" src="'+ZeroClipboard.moviePath+'" loop="false" menu="false" quality="best" bgcolor="#ffffff" width="'+width+'" height="'+height+'" name="'+this.movieId+'" align="middle" allowScriptAccess="always" allowFullScreen="false" type="application/x-shockwave-flash" pluginspage="http://www.macromedia.com/go/getflashplayer" flashvars="'+flashvars+'" wmode="transparent" />';
		}
		return html;
	},
	
	hide: function() {
		// temporarily hide floater offscreen
		if (this.div) {
			this.div.style.left = '-2000px';
		}
	},
	
	show: function() {
		// show ourselves after a call to hide()
		this.reposition();
	},
	
	destroy: function() {
		// destroy control and floater
		if (this.domElement && this.div) {
			this.hide();
			this.div.innerHTML = '';
			
			var body = document.getElementsByTagName('body')[0];
			try { body.removeChild( this.div ); } catch(e) {;}
			
			this.domElement = null;
			this.div = null;
		}
	},
	
	reposition: function(elem) {
		// reposition our floating div, optionally to new container
		// warning: container CANNOT change size, only position
		if (elem) {
			this.domElement = ZeroClipboard.$(elem);
			if (!this.domElement) this.hide();
		}
		
		if (this.domElement && this.div) {
			var box = ZeroClipboard.getDOMObjectPosition(this.domElement);
			var style = this.div.style;
			style.left = '' + box.left + 'px';
			style.top = '' + box.top + 'px';
		}
	},
	
	setText: function(newText) {
		// set text to be copied to clipboard
		this.clipText = newText;
		if (this.ready) this.movie.setText(newText);
	},
	
	addEventListener: function(eventName, func) {
		// add user event listener for event
		// event types: load, queueStart, fileStart, fileComplete, queueComplete, progress, error, cancel
		eventName = eventName.toString().toLowerCase().replace(/^on/, '');
		if (!this.handlers[eventName]) this.handlers[eventName] = [];
		this.handlers[eventName].push(func);
	},
	
	setHandCursor: function(enabled) {
		// enable hand cursor (true), or default arrow cursor (false)
		this.handCursorEnabled = enabled;
		if (this.ready) this.movie.setHandCursor(enabled);
	},
	
	setCSSEffects: function(enabled) {
		// enable or disable CSS effects on DOM container
		this.cssEffects = !!enabled;
	},
	
	receiveEvent: function(eventName, args) {
		// receive event from flash
		eventName = eventName.toString().toLowerCase().replace(/^on/, '');
				
		// special behavior for certain events
		switch (eventName) {
			case 'load':
				// movie claims it is ready, but in IE this isn't always the case...
				// bug fix: Cannot extend EMBED DOM elements in Firefox, must use traditional function
				this.movie = document.getElementById(this.movieId);
				if (!this.movie) {
					var self = this;
					setTimeout( function() { self.receiveEvent('load', null); }, 1 );
					return;
				}
				
				// firefox on pc needs a "kick" in order to set these in certain cases
				if (!this.ready && navigator.userAgent.match(/Firefox/) && navigator.userAgent.match(/Windows/)) {
					var self = this;
					setTimeout( function() { self.receiveEvent('load', null); }, 100 );
					this.ready = true;
					return;
				}
				
				this.ready = true;
				this.movie.setText( this.clipText );
				this.movie.setHandCursor( this.handCursorEnabled );
				break;
			
			case 'mouseover':
				if (this.domElement && this.cssEffects) {
					this.domElement.addClass('hover');
					if (this.recoverActive) this.domElement.addClass('active');
				}
				break;
			
			case 'mouseout':
				if (this.domElement && this.cssEffects) {
					this.recoverActive = false;
					if (this.domElement.hasClass('active')) {
						this.domElement.removeClass('active');
						this.recoverActive = true;
					}
					this.domElement.removeClass('hover');
				}
				break;
			
			case 'mousedown':
				if (this.domElement && this.cssEffects) {
					this.domElement.addClass('active');
				}
				break;
			
			case 'mouseup':
				if (this.domElement && this.cssEffects) {
					this.domElement.removeClass('active');
					this.recoverActive = false;
				}
				break;
		} // switch eventName
		
		if (this.handlers[eventName]) {
			for (var idx = 0, len = this.handlers[eventName].length; idx < len; idx++) {
				var func = this.handlers[eventName][idx];
			
				if (typeof(func) == 'function') {
					// actual function reference
					func(this, args);
				}
				else if ((typeof(func) == 'object') && (func.length == 2)) {
					// PHP style object + method, i.e. [myObject, 'myMethod']
					func[0][ func[1] ](this, args);
				}
				else if (typeof(func) == 'string') {
					// name of function
					window[func](this, args);
				}
			} // foreach event handler defined
		} // user defined handler for event
	}
	
};

//#############################################################################

var clip = null;

function $(id) { return document.getElementById(id); }

function initClip() {
//console.log(">>> initClip");
	clip = new ZeroClipboard.Client();
	clip.setHandCursor( true );
	
//	clip.addEventListener('load', my_load);
//	clip.addEventListener('mouseOver', my_mouse_over);
	clip.addEventListener('complete', my_complete);
	
//	clip.glue( 'd_clip_button' );
//console.log("<<< initClip");
}

//function my_load(client) {
//	console.log("Flash movie loaded and ready."); 
//}

//function my_mouse_over(client) {
//	// we can cheat a little here -- update the text on mouse over
//	clip.setText( $('fe_text').value );
//}

function my_complete(client, text) {
//	console.log("Copied text to clipboard: ... ");
//	console.log("Copied text to clipboard: " + text );
	clipperz_copiedContentToClipboard = true;
	showTooltip();
}

//function debugstr(msg) {
//	var p = document.createElement('p');
//	p.innerHTML = msg;
//	$('d_debug').appendChild(p);
//}

//#############################################################################

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

selectFaviconURL = function () {
	var result;
	var links;
	var link;
	var i;

//	<link	rel="icon"			type="image/x-icon"				href="http://example.com/favicon.ico" />
//	<link	rel="icon"			type="image/vnd.microsoft.icon"	href="http://example.com/image.ico" />
//	<link	rel="SHORTCUT ICON"	type="image/x-icon"				href="/horde/imp/graphics/favicon.ico" />
	links = document.getElementsByTagName("head")[0].getElementsByTagName('link');

	i = 0;
	link = null;
	while ((link == null) && (i < links.length)) {
		if ((links[i].rel.toLowerCase() == 'icon') || (links[i].rel.toLowerCase() == 'shortcut icon')) {
			link = links[i];
		}

		i++;
	}
	
	if (link != null) {
		result = link.href;
	} else {
		result = "http://" + window.location.hostname + "/favicon.ico";
	}

	return result;
}

//-----------------------------------------------------------------------------

pageParameters = function() {
	var result;
	
	result = {};
	result['title'] = document.title;
	result['favicon'] = selectFaviconURL();
	result['url'] = window.location.href;

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

getLoginFormConfiguration = function() {
	var	parameters;

	parameters = {};
	parameters.page = pageParameters();
	parameters.form = formParameters(findLoginForm(document, 0));
	parameters.version = "0.3.0";

	return parameters;
}

//#############################################################################

//-----------------------------------------------------------------------------

closeClick = function () {
	var	bookmarkletDiv;

	bookmarkletDiv = document.getElementById("clipperzBookmarkletWrapper");
	bookmarkletDiv.parentNode.removeChild(bookmarkletDiv);
};

//-----------------------------------------------------------------------------

logFormParameters = function(someParameters, anException) {
	var showException;
	var	message;

	if ((someParameters != null) && (someParameters.form != null) && (anException == null)) {
		showException = false;
		message = traslatableTexts['noExceptionMessage'];
	} else {
		showException = true
		message = traslatableTexts['exceptionMessage'];
	}

	var newCSS = document.createElement('link');
	newCSS.setAttribute("type", "text/css");
	newCSS.setAttribute("rel", "stylesheet");
	newCSS.setAttribute("media", "screen");
	newCSS.setAttribute("href", "http://www.clipperz.com/files/clipperz.com/bookmarklet/0.3.0/Bookmarklet.css");
	document.getElementsByTagName("head")[0].appendChild(newCSS);

	var innerHTML;

	innerHTML = "";
	innerHTML +=	"<div id='clipperzBookmarklet'>" +
						"<div id='clipperzBookmarkletClose'></div>" +
						"<div id='clipperzBookmarkletResult'>" +
							"<div id='clipperzBookmarkletResultIcon' class=" + ((showException == false) ? 'ok' : 'fail') + "></div>" +
							"<p id='clipperzBookmarkletResultText'>" + message + "</p>" +
						"</div>";

	if (showException == false) {
		innerHTML +=	"<div id='clipperzBookmarletButton'><span>" + traslatableTexts['copy'] + "</span></div>" +
						"<div id='clipperzBookmarletAfterCopyHint' class='hidden'>" +
							"<p id='clipperzBookmarkletHintText'>Lorem ipsum</p>" +
						"</div>";
	}

	innerHTML +=	"</div>";

	var newDiv = document.createElement('div');
	newDiv.setAttribute("id", "clipperzBookmarkletWrapper");
	newDiv.innerHTML = innerHTML;
	document.body.appendChild(newDiv);

	$('clipperzBookmarkletClose').onclick = closeClick;
	
	if (showException == false) {
		$('clipperzBookmarletButton').onclick = showTooltip;
		setTimeout("clip.glue('clipperzBookmarletButton');", 1000);
	}
}

showTooltip = function () {
	if (clipperz_copiedContentToClipboard == true) {
//console.log("SUCCEED");
		$('clipperzBookmarkletHintText').innerHTML = traslatableTexts['successfulMessage'];
		$('clipperzBookmarletAfterCopyHint').className = 'visible';
	} else {
//console.log("FAIL");
		$('clipperzBookmarkletHintText').innerHTML = traslatableTexts['failMessage'];
	}
}

//#############################################################################

traslatableTexts = {
	'noExceptionMessage':	"@BOOKMARKLET_NO_EXCEPTION_MESSAGE@",
	'exceptionMessage':		"@BOOKMARKLET_EXCEPTION_MESSAGE@",
	'copy':					"@BOOKMARKLET_COPY@",
	'successfulMessage':	"@BOOKMARKLET_SUCCESSFUL_MESSAGE@",
	'failMessage':			"@BOOKMARKLET_FAIL_MESSAGE@"

//	'noExceptionMessage':	"The direct login configuration has been collected.",
//	'exceptionMessage':		"Sorry! There was an error while processing the page.",
//	'copy':					"copy",
//	'successfulMessage':	"DONE!",
//	'failMessage':			"Failed! :("
}

//#############################################################################

runBookmarklet = function () {
	var	parameters;

	try {
		initClip();

		parameters = getLoginFormConfiguration();
//console.log("configuration", serializeJSON(parameters))
		clip.setText(serializeJSON(parameters));
		clip.setHandCursor( true );
//		clip.glue('clipperzBookmarletButton');

		logFormParameters(parameters, _cble);
	} catch (e) {
		logFormParameters(parameters, e);
	}
	
};

//#############################################################################

if (document.body != null) {
	runBookmarklet();
};

//#############################################################################
