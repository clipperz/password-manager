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
//console.log(anError);
		result = new MochiKit.Async.CancelledError(anError);
	}

	return result;
}


Clipperz.PM.RunTime = {};


function run() {
	var shouldShowRegistrationForm;
	var useCompactDesign;
	var controllerParameters;

	controllerParameters = {};

//	MochiKit.DOM.removeElement('javaScriptAlert');
	Clipperz.PM.Strings.Languages.initSetup();

	if (window.location.search.indexOf('registration') != -1) {
		shouldShowRegistrationForm = true;
	} else {
		shouldShowRegistrationForm = false;
	}

	if (window.location.search.indexOf('autocomplete') != -1) {
		controllerParameters['autocomplete'] = 'on'
	}

	if (window.location.search.indexOf('compact') != -1) {
		useCompactDesign = true;
	} else {
		useCompactDesign = false;
	}
	
	if (useCompactDesign == true) {
		Clipperz.PM.RunTime.mainController = new Clipperz.PM.UI.Compact.Controllers.MainController(controllerParameters);
	} else {
		Clipperz.PM.RunTime.mainController = new Clipperz.PM.UI.Web.Controllers.MainController(controllerParameters);
	}

	Clipperz.PM.RunTime.mainController.run(shouldShowRegistrationForm);

	//Clipperz.log("HASH: " + window.location.hash);
//	if (window.location.hash != "") {
//		window.location.hash = ""
//	}
//	Clipperz.log("HASH cleaned");
//	#credentials=base64encoded({username:'joe', passphrase:'clipperz'})
//	MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'doLogin', {username:'joe', passphrase:'clipperz'});
}

MochiKit.DOM.addLoadEvent(run);
