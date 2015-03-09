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

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.Strings) == 'undefined') { Clipperz.PM.Strings = {}; }

Clipperz.PM.Strings.messagePanelConfigurations = {


	//-------------------------------------------------------------------------
	//
	// Registration - connection
	//
	'registration_verify': function() {
		return {
			'title': null,
			'text':  Clipperz.PM.Strings['connectionRegistrationSendingRequestMessageText']
		}
	},

	'registration_sendingCredentials': function() {
		return {
			'title': null,
			'text':  Clipperz.PM.Strings['connectionRegistrationSendingCredentialsMessageText']
		}
	},

	//-------------------------------------------------------------------------
	//
	// One Time Password login message panel
	//

	'OTP_login_start': function() {
		return  {
			'title': Clipperz.PM.Strings['OTPloginMessagePanelInitialTitle'],
			'text':  Clipperz.PM.Strings['OTPloginMessagePanelInitialText'],
			'steps': '+3',
			'buttons': {}
		}
	},

	'OTP_login_loadingOTP': function() {
		return {
			'title': Clipperz.PM.Strings['OTPloginMessagePanelLoadingTitle'],
			'text':  Clipperz.PM.Strings['OTPloginMessagePanelLoadingText']
		}
	},

	'OTP_login_extractingPassphrase': function() {
		return {
			'title': Clipperz.PM.Strings['OTPloginMessagePanelProcessingTitle'],
			'text':  Clipperz.PM.Strings['OTPloginMessagePanelProcessingText']
		}
	},
	
	
	//-------------------------------------------------------------------------
	//
	// Login message panel
	//
	'login_start': function() {
		return  {
			'title': Clipperz.PM.Strings['loginMessagePanelInitialTitle'],
			'text':  Clipperz.PM.Strings['loginMessagePanelInitialText'],
			'steps': '+7',
			'buttons': {
				'ok': Clipperz.PM.Strings['loginMessagePanelInitialButtonLabel']
			}
		}
	},

	'login_connected': function() {
		return {
			'title': Clipperz.PM.Strings['loginMessagePanelConnectedTitle'],
			'text':  Clipperz.PM.Strings['loginMessagePanelConnectedText'],
			'buttons': {}
		}
	},

	'login_failed':	function() {
		return {
			'title': Clipperz.PM.Strings['loginMessagePanelFailureTitle'],
			'text':  Clipperz.PM.Strings['loginMessagePanelFailureText'],
			'button': Clipperz.PM.Strings['loginMessagePanelFailureButtonLabel']
		}
	},
	
	//-------------------------------------------------------------------------
	//
	// Login message panel - connection
	//
	'connection_sendingCredentials': function() {
		return {
			'title': Clipperz.PM.Strings['connectionLoginSendingCredentialsMessageTitle'],
			'text':  Clipperz.PM.Strings['connectionLoginSendingCredentialsMessageText']
		}
	},

	'connection_credentialVerification': function() {
		return {
			'title': Clipperz.PM.Strings['connectionLoginCredentialsVerificationMessageTitle'],
			'text':  Clipperz.PM.Strings['connectionLoginCredentialsVerificationMessageText']
		}
	},

	'connection_loggedIn': function() {
		return {
			'title': Clipperz.PM.Strings['connectionLoginDoneMessageTitle'],
			'text':  Clipperz.PM.Strings['connectionLoginDoneMessageText']
		}
	},
	
	//-------------------------------------------------------------------------
	//
	//	Login message panel - user
	//
	'connection_upgrading': function() {
		return {
			'title': Clipperz.PM.Strings['userLoginPanelUpgradingUserCredentialsMessageTitle'],
			'text':  Clipperz.PM.Strings['userLoginPanelUpgradingUserCredentialsMessageText'],
			'steps': '+1'
		}
	},
	
	'connection_done': function() {
		return {
			'title': Clipperz.PM.Strings['userLoginPanelConnectedMessageTitle'],
			'text':  Clipperz.PM.Strings['userLoginPanelConnectedMessageText']
		}
	},
	
	'connection_tryOlderSchema': function() {
		return {
			'title': Clipperz.PM.Strings['userLoginPanelTryingAnOlderConnectionSchemaMessageTitle'],
			'text': Clipperz.PM.Strings['userLoginPanelTryingAnOlderConnectionSchemaMessageText'],
			'steps': '+4'
		}
	},
	
	'connection_loadingUserData': function() {
		return {
			'title': Clipperz.PM.Strings['userLoginPanelLoadingUserDataMessageTitle'],
			'text': Clipperz.PM.Strings['userLoginPanelLoadingUserDataMessageText']
		}
	},
	
	'connection_decryptingUserData': function() {
		return {
			'title': Clipperz.PM.Strings['userLoginPanelDecryptingUserDataMessageTitle'],
			'text': Clipperz.PM.Strings['userLoginPanelDecryptingUserDataMessageText'],
			'steps': '+1'
		}
	},
	
	'connection_decryptingUserStatistics': function() {
		return {
			'title': Clipperz.PM.Strings['userLoginPanelDecryptingUserStatisticsMessageTitle'],
			'text': Clipperz.PM.Strings['userLoginPanelDecryptingUserStatisticsMessageText']
		}
	},
	
	'collectingEntropy': function() {
		return {
			'text': Clipperz.PM.Strings['panelCollectingEntryopyMessageText'],
			'steps': '+1'
		}
	},

	//-------------------------------------------------------------------------
	//
	// Cards block - delete card panel
	//
	'deleteRecord_collectData': function() {
		return {
			'title': Clipperz.PM.Strings['deleteRecordPanelCollectRecordDataMessageTitle'],
			'text': Clipperz.PM.Strings['deleteRecordPanelCollectRecordDataMessageText']
		}
	},
	
	'deleteRecord_encryptData': function() {
		return {
			'title': Clipperz.PM.Strings['deleteRecordPanelEncryptUserDataMessageTitle'],
			'text': Clipperz.PM.Strings['deleteRecordPanelEncryptUserDataMessageText']
		}
	},
	
	'deleteRecord_sendingData': function() {
		return {
			'title': Clipperz.PM.Strings['deleteRecordPanelSendingDataToTheServerMessageTitle'],
			'text': Clipperz.PM.Strings['deleteRecordPanelSendingDataToTheServerMessageText']
		}
	},
	
	'deleteRecord_updatingInterface': function() {
		return {
			'title': Clipperz.PM.Strings['deleteRecordPanelUpdatingTheInterfaceMessageTitle'],
			'text': Clipperz.PM.Strings['deleteRecordPanelUpdatingTheInterfaceMessageText']
		}
	},


	//-------------------------------------------------------------------------
	//
	//	Cards block - save card panel
	//
	'saveCard_collectRecordInfo': function() {
		return {
			'title': Clipperz.PM.Strings['recordSaveChangesPanelCollectRecordInfoMessageTitle'],
			'text': Clipperz.PM.Strings['recordSaveChangesPanelCollectRecordInfoMessageText']
		}
	},

	'saveCard_encryptUserData': function() {
		return {
			'title': Clipperz.PM.Strings['recordSaveChangesPanelEncryptUserDataMessageTitle'],
			'text': Clipperz.PM.Strings['recordSaveChangesPanelEncryptUserDataMessageText']
		}
	},

	'saveCard_encryptRecordData': function() {
		return {
			'title': Clipperz.PM.Strings['recordSaveChangesPanelEncryptRecordDataMessageTitle'],
			'text': Clipperz.PM.Strings['recordSaveChangesPanelEncryptRecordDataMessageText']
		}
	},

	'saveCard_encryptRecordVersions': function() {
		return {
			'title': Clipperz.PM.Strings['recordSaveChangesPanelEncryptRecordVersionDataMessageTitle'],
			'text': Clipperz.PM.Strings['recordSaveChangesPanelEncryptRecordVersionDataMessageText']
		}
	},

	'saveCard_sendingData': function() {
		return {
			'title': Clipperz.PM.Strings['recordSaveChangesPanelSendingDataToTheServerMessageTitle'],
			'text': Clipperz.PM.Strings['recordSaveChangesPanelSendingDataToTheServerMessageText']
		}
	},

	'saveCard_updatingInterface': function() {
		return {
			'title': Clipperz.PM.Strings['recordSaveChangesPanelUpdatingTheInterfaceMessageTitle'],
			'text': Clipperz.PM.Strings['recordSaveChangesPanelUpdatingTheInterfaceMessageText']
		}
	},

	//-------------------------------------------------------------------------
	//
	//	Account panel - user preferences
	//
	'account_savingPreferences_1': function() {
		return {
			'title': Clipperz.PM.Strings['accountPreferencesSavingPanelTitle_Step1'],
			'text': Clipperz.PM.Strings['accountPreferencesSavingPanelText_Step1'],
			'steps': '+3'
		}
	},

	'account_savingPreferences_2': function() {
		return {
			'title': Clipperz.PM.Strings['accountPreferencesSavingPanelTitle_Step2'],
			'text': Clipperz.PM.Strings['accountPreferencesSavingPanelText_Step2']
		}
	},


	//-------------------------------------------------------------------------
	//
	//	Account panel - change credentials
	//
	'changeCredentials_encryptingData': function() {
		return {
			'title': Clipperz.PM.Strings['changeCredentialsPanelEncryptingDataMessageTitle'],
			'text':  Clipperz.PM.Strings['changeCredentialsPanelEncryptingDataMessageText']
		}
	},

	'changeCredentials_creatingNewCredentials': function() {
		return {
			'title': Clipperz.PM.Strings['changeCredentialsPanelCreatingNewCredentialsMessageTitle'],
			'text':  Clipperz.PM.Strings['changeCredentialsPanelCreatingNewCredentialsMessageText']
		}
	},

	'changeCredentials_sendingCredentials': function() {
		return {
			'title': Clipperz.PM.Strings['changeCredentialsPanelSendingNewCredentialsToTheServerMessageTitle'],
			'text':  Clipperz.PM.Strings['changeCredentialsPanelSendingNewCredentialsToTheServerMessageText']
		}
	},

	'changeCredentials_done': function() {
		return {
			'title': Clipperz.PM.Strings['changeCredentialsPanelDoneMessageTitle'],
			'text':  Clipperz.PM.Strings['changeCredentialsPanelDoneMessageText']
		}
	},
	
	
	//-------------------------------------------------------------------------
	//
	//	Account panel - change credentials
	//
	'saveOTP_encryptUserData': function() {
		return {
			'title': Clipperz.PM.Strings['saveOTP_encryptUserDataTitle'],
			'text':  Clipperz.PM.Strings['saveOTP_encryptUserDataText'],
			'steps': '+4'
		}
	},
	
	'saveOTP_encryptOTPData': function() {
		return {
			'title': Clipperz.PM.Strings['saveOTP_encryptOTPDataTitle'],
			'text':  Clipperz.PM.Strings['saveOTP_encryptOTPDataText']
		}
	},
	
	'saveOTP_sendingData': function() {
		return {
			'title': Clipperz.PM.Strings['saveOTP_sendingDataTitle'],
			'text':  Clipperz.PM.Strings['saveOTP_sendingDataText']
		}
	},

	'saveOTP_updatingInterface': function() {
		return {
			'title': Clipperz.PM.Strings['saveOTP_updatingInterfaceTitle'],
			'text':  Clipperz.PM.Strings['saveOTP_updatingInterfaceText']
		}
	},

	
	//-------------------------------------------------------------------------
	//
	//	Data panel - processingImportData
	//
	'parseImportData': function() {
		return {
			'title': Clipperz.PM.Strings['importData_parsingDataTitle'],
			'text':  Clipperz.PM.Strings['importData_parsingDataText']
		}
	},

	'previewImportData': function() {
		return {
			'title': Clipperz.PM.Strings['importData_previewingDataTitle'],
			'text':  Clipperz.PM.Strings['importData_previewingDataText']
		}
	},
	
	'processingImportData': function() {
		return {
			'title': Clipperz.PM.Strings['importData_processingDataTitle'],
			'text':  Clipperz.PM.Strings['importData_processingDataText']
		}
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"

}
