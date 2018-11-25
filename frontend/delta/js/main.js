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

//React.initializeTouchEvents(true);

Clipperz.PM.RunTime = {};

function initOnMediaQuery () {
	MQ.init(MochiKit.Base.map(function (queryStyle) {
		return {
			context: queryStyle,
			match: function () {
				MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'matchMediaQuery', queryStyle);
			},
			unmatch: function () {
				MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'unmatchMediaQuery', queryStyle);
			},
		}
//	}, ['extra-short', 'narrow', 'wide', 'extra-wide']));
	}, Clipperz_PM_UI_availableStyles));
}

function run () {
	var parameters = {};

	Clipperz.PM.Strings.Languages.initSetup();

	if ((window.location.search.indexOf('registration') != -1) || (window.location.hash.indexOf('registration') != -1)) {
		parameters['shouldShowRegistrationForm'] = true;
	} else {
		parameters['shouldShowRegistrationForm'] = false;
	}

	Clipperz.PM.DataModel.devicePreferences = new Clipperz.PM.DataModel.DevicePreferences({});

	Clipperz.PM.RunTime.mainController = new Clipperz.PM.UI.MainController();
	initOnMediaQuery();
	Clipperz.PM.RunTime.mainController.run(parameters);
}

MochiKit.DOM.addLoadEvent(run);

/* * /
MochiKit.DOM.addLoadEvent(simulateLogin);
function simulateLogin () {
	var getURLParameter = function (name) {
		return decodeURIComponent((new RegExp('[?|&]' + name + '=' + '([^&;]+?)(&|#|;|$)').exec(location.search)||[,""])[1].replace(/\+/g, '%20'))||null;
	}
	
	Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();
	
	if ((getURLParameter('u') != null) && (getURLParameter('p'))) {
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'doLogin', {username:getURLParameter('u'), passphrase:getURLParameter('p')});
	}
}
/ * */