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

Clipperz.Base.module('Clipperz.PM.UI.Components.Panels');

Clipperz.PM.UI.Components.Panels.ExtraFeaturesPanel = React.createClass({

	settingsToggleHandler: function (anEvent) {
//console.log("settingsToggleHandler");
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'toggleSettingsPanel');
	},

	//=========================================================================

	render: function () {
		var	classes = {
			'panel': true,
			'right': true,
			'open': this.props['settingsPanelStatus'] == 'OPEN'
		}

		return	React.DOM.div({key:'extraFeaturesPanel', id:'extraFeaturesPanel', className:React.addons.classSet(classes)}, [
			React.DOM.header({}, [
				React.DOM.div({className:'settingsToggle'}, [
					Clipperz.PM.UI.Components.Button({eventName:'settingsToggleButton', label:"menu", handler:this.settingsToggleHandler})
				])
			]),
			React.DOM.h2({}, "Extra features")
		]);
/*
		<div id="extraFeaturesPanel" class="panel extraFeatures">

			<div class="warnings">
				<ul>
					<li>Synchronize local data</li>
				</ul>
			</div>

			<ul>
				<li>Account</li>
				<li>Subscription</li>
			</ul>

			<ul>
				<li>Local Data</li>
				<li>OTP</li>
			</ul>

			<div class="donation">
				<a>Make a donation</a>
			</div>
		</div>
*/

	}

	//=========================================================================
});
