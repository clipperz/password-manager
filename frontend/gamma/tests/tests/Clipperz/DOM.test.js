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

try {
	var template;
	var templateArgs;
	var element;
	var rowElement;
	var tdElement;
	
	template = Clipperz.DOM.Helper.createTemplate(
		[
			{tag:'td', children:[{tag:'img', cls:'favicon', src:'{faviconUrl}'}]},
			{tag:'td', children:[{tag:'a', href:'#', cls:'title', html:'{cardTitle}'}]},
			{tag:'td', children:[{tag:'span', cls:'directLogins', id:'{directLoginsID}'}]},
			{tag:'td', children:[{tag:'span', cls:'latestUpdate', html:'{latestUpdate}'}]},
			{tag:'td'}
		]
	);

	templateArgs = {
		cardTitle: "Amazon.com",
		directLoginsID: "Clipperz_PM_Components_directLogins_58",
		faviconUrl: "http://www.amazon.com/favicon.ico",
		latestUpdate: "",
		reference: "13a5e52976337ab210903cd04872588e1b21fb72bc183e91aa25c494b8138551"
	}

	element = Clipperz.DOM.Helper.append(document.body, {tag:'table', children:[
		{tag:'theader'},
		{tag:'tbody', children:[
			{tag:'tr', id:'testRow'}
		]},
		{tag:'tfooter'}
	]});
	ok(element != null, "created the TABLE");

	rowElement = Clipperz.DOM.get('testRow');
	ok(testRow != null, "created the ROW");
	
	template.append(rowElement, templateArgs);
	tdElement = Clipperz.DOM.get(templateArgs['directLoginsID']);
	ok(tdElement != null, "created the TD");

} catch (err) {
	var s = "test suite failure!\n";
	var o = {};
	var k = null;
	for (k in err) {
		// ensure unique keys?!
		if (!o[k]) {
			s +=  k + ": " + err[k] + "\n";
			o[k] = err[k];
		}
	}
	ok ( false, s );
}
