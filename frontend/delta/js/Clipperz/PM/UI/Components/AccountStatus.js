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
Clipperz.Base.module('Clipperz.PM.UI.Components');

Clipperz.PM.UI.Components.AccountStatusClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.AccountStatus',

	propTypes: {
//		'currentSubscriptionType':		React.PropTypes.oneOf(['EARLY_ADOPTER', 'FRIEND', 'FAN', 'DEVOTEE', 'PATRON', 'TRIAL', 'TRIAL_EXPIRED', 'PAYMENT_FAILED_2', 'EXPIRED', 'PAYMENT_FAILED', 'VERIFYING_PAYMENT', 'VERIFYING_PAYMENT_2']).isRequired,
		'expirationDate':				React.PropTypes.string /* .isRequired */,
		'referenceDate':				React.PropTypes.string /* .isRequired */,
		'featureSet':					React.PropTypes.oneOf(['TRIAL', 'EXPIRED', 'FULL']) /* .isRequired */ ,
		'proxyType':					React.PropTypes.oneOf(['ONLINE', 'OFFLINE', 'OFFLINE_COPY']),
		'isExpired':					React.PropTypes.bool /* .isRequired */ ,
		'isExpiring':					React.PropTypes.bool /* .isRequired */ ,
		'paymentVerificationPending':	React.PropTypes.bool /* .isRequired */ ,
	},

	//=========================================================================

	render: function () {
//console.log("AccountStatus props", this.props);
		var	accountInfoClasses = {
			'accountStatus':	true,
			'isExpiring':		this.props['isExpiring'],
			'isExpired':		this.props['isExpired'],
		};
		accountInfoClasses[this.props['featureSet']] = true;
		
		var proxyInfoClasses = {
			'proxyInfo':	true
		}
		proxyInfoClasses[this.props['proxyType']] = true;
		proxyInfoClasses['withReferenceDate'] = (this.props['referenceDate'] != null);

		return	React.DOM.div({'key':'miscInfo', 'className':'miscInfo'}, [
			React.DOM.div({'key':'miscInfo_1', 'className':Clipperz.PM.UI.Components.classNames(proxyInfoClasses)}, [
				React.DOM.span({'key':'proxyDescription', 'className':'proxyDescription'}, this.props['proxyTypeDescription']),
				React.DOM.span({'key':'referenceDate', 'className':'referenceDate'}, this.props['referenceDate'])
			]),
			React.DOM.div({'key':'miscInfo_2', 'className':Clipperz.PM.UI.Components.classNames(accountInfoClasses)}, [
				React.DOM.span({'key':'level', 'className':'level'}, Clipperz.PM.DataModel.Feature['featureSets'][this.props['featureSet']]),
				React.DOM.span({'key':'expirationMessage', 'className':'expirationMessage'}, "expiring on"),
				React.DOM.span({'key':'expirationDate', 'className':'expirationDate'}, this.props['expirationDate'])
			])
		]);
		
	}

	//=========================================================================
});

Clipperz.PM.UI.Components.AccountStatus = React.createFactory(Clipperz.PM.UI.Components.AccountStatusClass);