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

addEvent(window, "load", sortables_init);

var SORT_COLUMN_INDEX;

function sortables_init() {
    // Find all tables with class sortable and make them sortable
    if (!document.getElementsByTagName) return;
    tbls = document.getElementsByTagName("table");
    for (ti=0;ti<tbls.length;ti++) {
        thisTbl = tbls[ti];
        if (((' '+thisTbl.className+' ').indexOf("sortable") != -1) && (thisTbl.id)) {
            //initTable(thisTbl.id);
            ts_makeSortable(thisTbl);
        }
    }
}

function ts_makeSortable(table) {
    if (table.rows && table.rows.length > 0) {
        var firstRow = table.rows[0];
    }
    if (!firstRow) return;
    
    // We have a first row: assume it's the header, and make its contents clickable links
    for (var i=0;i<firstRow.cells.length;i++) {
        var cell = firstRow.cells[i];
        var txt = ts_getInnerText(cell);
        cell.innerHTML = '<a href="#" class="sortheader" '+ 
        'onclick="ts_resortTable(this, '+i+');return false;">' + 
        txt+'<span class="sortarrow">&nbsp;&nbsp;&nbsp;</span></a>';
    }
}

function ts_getInnerText(el) {
	if (typeof el == "string") return el;
	if (typeof el == "undefined") { return el };
	if (el.innerText) return el.innerText;	//Not needed but it is faster
	var str = "";
	
	var cs = el.childNodes;
	var l = cs.length;
	for (var i = 0; i < l; i++) {
		switch (cs[i].nodeType) {
			case 1: //ELEMENT_NODE
				str += ts_getInnerText(cs[i]);
				break;
			case 3:	//TEXT_NODE
				str += cs[i].nodeValue;
				break;
		}
	}
	return str;
}

function ts_resortTable(lnk,clid) {
    // get the span
    var span;
    for (var ci=0;ci<lnk.childNodes.length;ci++) {
        if (lnk.childNodes[ci].tagName && lnk.childNodes[ci].tagName.toLowerCase() == 'span') span = lnk.childNodes[ci];
    }
    var spantext = ts_getInnerText(span);
    var td = lnk.parentNode;
    var column = clid || td.cellIndex;
    var table = getParent(td,'TABLE');

    // Work out a type for the column
    if (table.rows.length <= 1) return;
    var itm = ts_getInnerText(table.rows[1].cells[column]);
    sortfn = ts_sort_caseinsensitive;
    if (itm.match(/^\d\d[\/-]\d\d[\/-]\d\d\d\d$/)) {
		sortfn = ts_sort_date;
	}
    if (itm.match(/^\d\d[\/-]\d\d[\/-]\d\d$/)) {
		sortfn = ts_sort_date;
	}
    if (itm.match(/^[£$]/)) {
		sortfn = ts_sort_currency;
	}
    if (itm.match(/^[\d\.]+$/)) {
		sortfn = ts_sort_numeric;
	}
    SORT_COLUMN_INDEX = column;
    var firstRow = new Array();
    var newRows = new Array();
    for (i=0;i<table.rows[0].length;i++) {
		firstRow[i] = table.rows[0][i];
	}

    for (j=1;j<table.rows.length;j++) {
		newRows[j-1] = table.rows[j];
	}
    newRows.sort(sortfn);

    if (span.getAttribute("sortdir") == 'down') {
        ARROW = '&nbsp;&nbsp;&uarr;';
        newRows.reverse();
        span.setAttribute('sortdir','up');
    } else {
        ARROW = '&nbsp;&nbsp;&darr;';
        span.setAttribute('sortdir','down');
    }
    
    // We appendChild rows that already exist to the tbody, so it moves them rather than creating new ones
    // don't do sortbottom rows
    for (i=0;i<newRows.length;i++) {
		if (!newRows[i].className || (newRows[i].className && (newRows[i].className.indexOf('sortbottom') == -1))) {
 			table.tBodies[0].appendChild(newRows[i]);
		}
	}
    // do sortbottom rows only
    for (i=0;i<newRows.length;i++) { 
		if (newRows[i].className && (newRows[i].className.indexOf('sortbottom') != -1)) {
			table.tBodies[0].appendChild(newRows[i]);
		}
	}
    
    // Delete any other arrows there may be showing
    var allspans = document.getElementsByTagName("span");
    for (var ci=0;ci<allspans.length;ci++) {
        if (allspans[ci].className == 'sortarrow') {
            if (getParent(allspans[ci],"table") == getParent(lnk,"table")) { // in the same table as us?
                allspans[ci].innerHTML = '&nbsp;&nbsp;&nbsp;';
            }
        }
    }
        
    span.innerHTML = ARROW;
}

function getParent(el, pTagName) {
	if (el == null) return null;
	else if (el.nodeType == 1 && el.tagName.toLowerCase() == pTagName.toLowerCase())	// Gecko bug, supposed to be uppercase
		return el;
	else
		return getParent(el.parentNode, pTagName);
}
function ts_sort_date(a,b) {
    // y2k notes: two digit years less than 50 are treated as 20XX, greater than 50 are treated as 19XX
    aa = ts_getInnerText(a.cells[SORT_COLUMN_INDEX]);
    bb = ts_getInnerText(b.cells[SORT_COLUMN_INDEX]);
    if (aa.length == 10) {
        dt1 = aa.substr(6,4)+aa.substr(3,2)+aa.substr(0,2);
    } else {
        yr = aa.substr(6,2);
        if (parseInt(yr) < 50) { yr = '20'+yr; } else { yr = '19'+yr; }
        dt1 = yr+aa.substr(3,2)+aa.substr(0,2);
    }
    if (bb.length == 10) {
        dt2 = bb.substr(6,4)+bb.substr(3,2)+bb.substr(0,2);
    } else {
        yr = bb.substr(6,2);
        if (parseInt(yr) < 50) { yr = '20'+yr; } else { yr = '19'+yr; }
        dt2 = yr+bb.substr(3,2)+bb.substr(0,2);
    }
    if (dt1==dt2) return 0;
    if (dt1<dt2) return -1;
    return 1;
}

function ts_sort_currency(a,b) { 
    aa = ts_getInnerText(a.cells[SORT_COLUMN_INDEX]).replace(/[^0-9.]/g,'');
    bb = ts_getInnerText(b.cells[SORT_COLUMN_INDEX]).replace(/[^0-9.]/g,'');
    return parseFloat(aa) - parseFloat(bb);
}

function ts_sort_numeric(a,b) { 
    aa = parseFloat(ts_getInnerText(a.cells[SORT_COLUMN_INDEX]));
    if (isNaN(aa)) aa = 0;
    bb = parseFloat(ts_getInnerText(b.cells[SORT_COLUMN_INDEX])); 
    if (isNaN(bb)) bb = 0;
    return aa-bb;
}

function ts_sort_caseinsensitive(a,b) {
    aa = ts_getInnerText(a.cells[SORT_COLUMN_INDEX]).toLowerCase();
    bb = ts_getInnerText(b.cells[SORT_COLUMN_INDEX]).toLowerCase();
    if (aa==bb) return 0;
    if (aa<bb) return -1;
    return 1;
}

function ts_sort_default(a,b) {
    aa = ts_getInnerText(a.cells[SORT_COLUMN_INDEX]);
    bb = ts_getInnerText(b.cells[SORT_COLUMN_INDEX]);
    if (aa==bb) return 0;
    if (aa<bb) return -1;
    return 1;
}


function addEvent(elm, evType, fn, useCapture)
// addEvent and removeEvent
// cross-browser event handling for IE5+,  NS6 and Mozilla
// By Scott Andrew
{
  if (elm.addEventListener){
    elm.addEventListener(evType, fn, useCapture);
    return true;
  } else if (elm.attachEvent){
    var r = elm.attachEvent("on"+evType, fn);
    return r;
  } else {
    alert("Handler could not be removed");
  }
} 




if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.Profile) == 'undefined') { Clipperz.Profile = {}; }

Clipperz.Profile.VERSION = "0.1";
Clipperz.Profile.NAME = "Clipperz.Profile";

MochiKit.Base.update(Clipperz.Profile, {

	//-------------------------------------------------------------------------

	'__repr__': function () {
		var	status;
		
		if (Clipperz.Profile.isEnabled == true) {
			status = ENABLED;
		} else {
			status = DISABLED;
		}
		
		return "[" + this.NAME + " " + this.VERSION + " - " + status + "]";
	},

	//-------------------------------------------------------------------------

	'toString': function () {
		return this.__repr__();
	},

	//-------------------------------------------------------------------------

	'isEnabled': function() {
		return false;
	},
	
	//-------------------------------------------------------------------------

	'initialValues': function() {
		return {iters:0, total:0, min:Number.MAX_VALUE, max:0}
	},
	
	//-------------------------------------------------------------------------

	'start': function(aName) {},
	'stop': function(aName) {},
	'dump': function(aName) {},
	'profileData': function(aName, aKey) {
		var	result;

		if (typeof(aName) != 'undefined') {
			result = this.initialValues();

			if (typeof(aKey) != 'undefined') {
				result = result[aKey];
			}
		} else {
			result = null;
		}
		
		return result;
		
	},
	'resetProfileData': function() {},
	//-------------------------------------------------------------------------

	'end': function(aName) {
		Clipperz.Profile.stop(aName);
	},
	
	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});



if ((typeof(clipperz_profiling_enabled) != 'undefined') && (clipperz_profiling_enabled == true)) {

var _clipperz_profile_profiles = {};
var _clipperz_profile_pns = [];


MochiKit.Base.update(Clipperz.Profile, {

	//-------------------------------------------------------------------------

	'isEnabled': function() {
		return true;
	},

	//-------------------------------------------------------------------------

	'start': function(aName) {
		if (!_clipperz_profile_profiles[aName]) {
			_clipperz_profile_profiles[aName] = this.initialValues();
			_clipperz_profile_pns[_clipperz_profile_pns.length] = aName;
		} else {
			if (_clipperz_profile_profiles[aName]["start"]) {
				Clipperz.Profile.stop(aName);
			}
		}

		_clipperz_profile_profiles[aName].end = null;
		_clipperz_profile_profiles[aName].start = new Date();
	},
	
	//-------------------------------------------------------------------------

	'stop': function(aName) {
		if ((_clipperz_profile_profiles[aName]) && (_clipperz_profile_profiles[aName]["start"])) {
			with(_clipperz_profile_profiles[aName]) {
				var now;
				var	elapsedTime;
				
				now = new Date();
				elapsedTime = (now - start);
				
				end = now;
				min = Math.min(min, elapsedTime);
				max = Math.max(max, elapsedTime);
				total += elapsedTime;
				start = null;
				iters++;
			}
		} else {
			// oops! bad call to end(), what should we do here?
			return true;
		}
	},
	
	//-------------------------------------------------------------------------

	'dump': function(appendToDoc) {
//		var tbl = document.createElement("table");
		var tbl = MochiKit.DOM.TABLE(null, MochiKit.DOM.TBODY());
		tbl.className = 'sortable';
		tbl.id = "profileOutputTable_table";
		with(tbl.style){
			border = "1px solid black";
			borderCollapse = "collapse";
		}
		var hdr = tbl.createTHead();
		var hdrtr = hdr.insertRow(0);
		// document.createElement("tr");
		var cols = ["Identifier","#","Min", "Avg","Max","Total"];
		for(var x=0; x<cols.length; x++){
			var ntd = hdrtr.insertCell(x);
			with(ntd.style){
				backgroundColor = "#225d94";
				color = "white";
				borderBottom = "1px solid black";
				borderRight = "1px solid black";
				fontFamily = "tahoma";
				fontWeight = "bolder";
				paddingLeft = paddingRight = "5px";
			}
			ntd.appendChild(document.createTextNode(cols[x]));
		}

		for(var x=0; x < _clipperz_profile_pns.length; x++){
			var prf = _clipperz_profile_profiles[_clipperz_profile_pns[x]];
			this.end(_clipperz_profile_pns[x]);
			if(prf.iters>0){
				var bdytr = tbl.insertRow(true);
				var vals = [_clipperz_profile_pns[x], prf.iters, prf.min, parseInt(Math.round(prf.total/prf.iters)), prf.max, prf.total];
				for(var y=0; y<vals.length; y++){
					var cc = bdytr.insertCell(y);
					cc.appendChild(document.createTextNode(vals[y]));
					with(cc.style){
						borderBottom = "1px solid gray";
						paddingLeft = paddingRight = "5px";
						if(x%2){
							backgroundColor = "#e1f1ff";
						}
						if(y>0){
							textAlign = "right";
							borderRight = "1px solid gray";
						}else{
							borderRight = "1px solid black";
						}
					}
				}
			}
		}

		if(appendToDoc){
			var ne = document.createElement("div");
			ne.id = "profileOutputTable";
			with(ne.style){
				fontFamily = "Courier New, monospace";
				fontSize = "12px";
				lineHeight = "16px";
				borderTop = "1px solid black";
				padding = "10px";
			}
			if(document.getElementById("profileOutputTable")){
				MochiKit.DOM.swapDOM("profileOutputTable", ne);
			}else{
				document.body.appendChild(ne);
			}
			ne.appendChild(tbl);
		}

		return tbl;
	},

	//-------------------------------------------------------------------------

	'profileData': function(aName, aKey) {
		var	result;

		if (typeof(aName) == 'undefined') {
			result = _clipperz_profile_profiles;
		} else {
			if (typeof(_clipperz_profile_profiles[aName]) != 'undefined') {
				result = _clipperz_profile_profiles[aName];
			} else {
				result = {};
			}
		}

		if (typeof(aKey) != 'undefined') {
			if (aKey == "average") {
				result = Math.round(Clipperz.Profile.profileData(aName, 'total')/Clipperz.Profile.profileData(aName, 'iters'));
			} else {
				if (typeof(result[aKey]) != 'undefined') {
					result = result[aKey];
				} else {
					result = 0;
				}
			}
		}
		
		return result;
	},

	//-------------------------------------------------------------------------

	'resetProfileData': function() {
		_clipperz_profile_profiles = {};
		_clipperz_profile_pns = [];
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});

}
