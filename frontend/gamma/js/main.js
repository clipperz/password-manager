/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz's Javascript Crypto Library.
Javascript Crypto Library provides web developers with an extensive
and efficient set of cryptographic functions. The library aims to
obtain maximum execution speed while preserving modularity and
reusability.
For further information about its features and functionalities please
refer to http://www.clipperz.com

* Javascript Crypto Library is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Javascript Crypto Library is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Javascript Crypto Library.  If not, see
  <http://www.gnu.org/licenses/>.

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
//	var iPhoneDesign;

//	MochiKit.Signal.connect(document.body, 'onkeydown', _pm_logEvent);
//	MochiKit.Signal.connect(document.body, 'onkeypress', _pm_logEvent);
//	MochiKit.Signal.connect(document.body, 'onclick', _pm_logEvent);

	controllerParameters = {};

	MochiKit.DOM.removeElement('javaScriptAlert');
	Clipperz.PM.Strings.Languages.initSetup();

	if (window.location.search.indexOf('registration') != -1) {
		shouldShowRegistrationForm = true;
	} else {
		shouldShowRegistrationForm = false;
	}

	if (window.location.search.indexOf('autocomplete') != -1) {
		controllerParameters['autocomplete'] = 'on'
	}

//	if ((window.location.search.indexOf('iPhone') != -1) || (navigator.userAgent.match('iPhone'))) {
//		iPhoneDesign = true;
//	} else {
//		iPhoneDesign = false;
//	}

	if (window.location.search.indexOf('compact') != -1) {
		useCompactDesign = true;
	} else {
		useCompactDesign = false;
	}
	
	if (useCompactDesign == true) {
		Clipperz.PM.RunTime.mainController = new Clipperz.PM.UI.Compact.Controllers.MainController(controllerParameters);
//	} else if (iPhoneDesign == true) {
//		Clipperz.PM.RunTime.mainController = new Clipperz.PM.UI.iPhone.Controllers.MainController();
	} else {
		Clipperz.PM.RunTime.mainController = new Clipperz.PM.UI.Web.Controllers.MainController(controllerParameters);
	}

	Clipperz.PM.RunTime.mainController.run(shouldShowRegistrationForm);

//Clipperz.log("HASH: " + window.location.hash);
if (window.location.hash != "") {
	window.location.hash = ""
}
//Clipperz.log("HASH cleaned");
//	#credentials=base64encoded({username:'joe', passphrase:'clipperz'})
//	MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'doLogin', {username:'joe', passphrase:'clipperz'});
}

MochiKit.DOM.addLoadEvent(run);
