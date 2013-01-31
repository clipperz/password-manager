/*

Copyright 2008-2013 Clipperz Srl

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

function _pm_logEvent(anEvent) {
//	console.log("####", anEvent);
	
	anEvent.preventDefault();
}

function handleGenericDeferredError(anError) {
	var result;
	
	if (anError instanceof MochiKit.Async.CancelledError) {
		result = anError;
	} else {
MochiKit.Logging.logError("## MainController - GENERIC ERROR" + "\n" + "==>> " + anError + " <<==\n" + anError.stack);
		result = new MochiKit.Async.CancelledError(anError);
	}

	return result;
}


Clipperz.PM.RunTime = {};


function run() {
	Clipperz.PM.Strings.Languages.initSetup();

	Clipperz.PM.RunTime.mainController = new Clipperz.PM.UI.Mobile.Controllers.MainController();
	Clipperz.PM.RunTime.mainController.run();
}

// if (navigator.standalone == false) {
// 	window.localStorage.setItem('PIN', '1234');
// 	alert("Saved PIN");
// } else {
// 	alert (window.localStorage.getItem('PIN'));
// }


MochiKit.DOM.addLoadEvent(run);
