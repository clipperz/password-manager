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
if (typeof(Clipperz.PM.Strings.Languages) == 'undefined') { Clipperz.PM.Strings.Languages = {}; }

//=============================================================================
//
//		E N G L I S H   A M E R I C A N  ( en_US )
//
//=============================================================================

Clipperz.PM.Strings.Languages['en-us'] = {

//	Login page - description
'clipperzServiceDescription': "\
	<!-- 	FIX CSS		DONE	-->	\
	<h2>Keep it to yourself!</h2>\
	<ul>\
		<li>\
			<h3>Clipperz is:</h3>\
			<ul>\
				<li><p>a secure and simple password manager</p></li>\
				<li><p>an effective single sign-on solution</p></li>\
				<li><p>a digital vault for your personal data</p></li>\
			</ul>\
		</li>\
		<li>\
			<h3>With Clipperz you can:</h3>\
			<ul>\
				<li><p>store and manage your passwords and online credentials</p></li>\
				<li><p>login to your web services without entering any username or password</p></li>\
				<li><p>protect all your sensitive data: codes for burglar alarms, PINs, credit card numbers, …</p></li>\
				<li><p>share secrets with family members and associates (coming soon)</p></li>\
			</ul>\
		</li>\
		<li>\
			<h3>Clipperz benefits:</h3>\
			<ul>\
				<li><p>free and completely anonymous</p></li>\
				<li><p>access it any time from any computer</p></li>\
				<li><p>no software to download and nothing to install</p></li>\
				<li><p>avoid keeping secrets on your PC or on paper</p></li>\
			</ul>\
		</li>\
		<li>\
			<h3>Clipperz security:</h3>\
			<ul>\
				<li><p>your secrets are locally encrypted by your browser before being uploaded to Clipperz</p></li>\
				<li><p>the encryption key is a passphrase known only to you</p></li>\
				<li><p>Clipperz hosts your sensitive data in encrypted form and could never actually access the data in its plain form</p></li>\
				<li><p>Clipperz is built upon standard encryption schemes, nothing fancies or homemade</p></li>\
				<li><p>you can review the source code anytime you like, but you need to know nothing about cryptography to be an happy user!</p></li>\
			</ul>\
		</li>\
		<li>\
			<a href=\"http://www.clipperz.com\" target=\"_blank\">Learn more</a>\
		</li>\
	</ul>",

		
'loginFormTitle':								"login with your Clipperz account",
'loginFormUsernameLabel':						"username",
'loginFormPassphraseLabel':						"passphrase",
'loginFormDontHaveAnAccountLabel':				"don\'t have an account?",
'loginFormCreateOneLabel':						"create one",
'loginFormForgotYourCredentialsLabel':			"forgot your credentials?",
'loginFormAarghThatsBadLabel':					"aargh! that\'s bad!",
'loginFormAfraidOfMaliciousScriptsLabel':		"afraid of malicious scripts?",
'loginFormVerifyTheCodeLabel':					"verify the code",
'loginFormButtonLabel':							"Login",
'loginFormOneTimePasswordCheckboxLabel':    	"use a one-time passphrase",
'loginFormOneTimePasswordCheckboxDescription':  "",

// Login page - language selection
'loginPanelSwithLanguageDescription':			"<h5>Switch to your preferred language</h5>",

// Login page - browser compatibility
'browserCompatibilityDescription':				"<p>Have a better and safer Clipperz experience with Firefox. However Clipperz works just fine also with Opera, Safari and MS Internet Explorer!</p>",

// Login with OTP - message panel
'OTPloginMessagePanelInitialTitle':				"Logging in using a one-time passphrase",
'OTPloginMessagePanelInitialText':				"Sending OTP credentials …",
'OTPloginMessagePanelLoadingTitle':				"Logging in using a one-time passphrase",
'OTPloginMessagePanelLoadingText':				"Fetching encrypted authentication data from the server …",
'OTPloginMessagePanelProcessingTitle':			"Logging in using a one-time passphrase",
'OTPloginMessagePanelProcessingText':			"Local decryption of authentication data",

// Regular login - message panel
'loginMessagePanelInitialTitle':				"Logging in …",
'loginMessagePanelInitialText':					"---",
'loginMessagePanelInitialButtonLabel':			"Cancel",
'loginMessagePanelConnectedTitle':				"Connected",
'loginMessagePanelConnectedText':				"Done",
'loginMessagePanelFailureTitle':				"Error",
'loginMessagePanelFailureText':					"Login failed",
'loginMessagePanelFailureButtonLabel':			"Close",

// Regular login - message panel - connection
'connectionLoginSendingCredentialsMessageTitle':		"Verifying credentials",
'connectionLoginSendingCredentialsMessageText':			"Sending credentials",
'connectionLoginCredentialsVerificationMessageTitle':	"Verifying credentials",
'connectionLoginCredentialsVerificationMessageText':	"Performing SRP authentication",
'connectionLoginDoneMessageTitle':						"Verifying credentials",
'connectionLoginDoneMessageText':						"Connected",
	
//	Regular login - message panel - user
'userLoginPanelUpgradingUserCredentialsMessageTitle':	   				"Verifying credentials",
'userLoginPanelUpgradingUserCredentialsMessageText':	   				"Upgrading your credentials to a new authentication schema",
'userLoginPanelConnectedMessageTitle':					   				"User authenticated",
'userLoginPanelConnectedMessageText':					   				"Successfully logged in",
'userLoginPanelTryingAnOlderConnectionSchemaMessageTitle': 				"Verifying credentials",
'userLoginPanelTryingAnOlderConnectionSchemaMessageText':  				"Trying an older authentication schema",
'userLoginPanelLoadingUserDataMessageTitle':			   				"User authenticated",
'userLoginPanelLoadingUserDataMessageText':				   				"Downloading encrypted card headers from Clipperz",
'userLoginPanelDecryptingUserDataMessageTitle':							"User authenticated",
'userLoginPanelDecryptingUserDataMessageText':							"Local decryption of card headers",
'userLoginPanelDecryptingUserStatisticsMessageTitle':					"User authenticated",
'userLoginPanelDecryptingUserStatisticsMessageText':					"Local decryption of usage statistics",
	
//	Registration page - splash alert
'splashAlertTitle':	"Welcome to Clipperz!",
'splashAlertText': "\
	<!-- 	FIX CSS		DONE!	-->	\
	<p>Some security advice</p>\
	<ul>\
		<li><p>Storing your data at Clipperz is as secure as the passphrase you choose to protect them. Nobody can access them unless they know your passphrase.</p></li>\
		<li><p>If you are going to use Clipperz for safeguarding sensitive and critical information please make sure to use a strong passphrase. The longer the better!</p></li>\
		<li><p>Clipperz will not be able to recover a lost passphrase!</p></li>\
	</ul>\
	<p>For any further information, please refer to <a href=\"http://www.clipperz.com\" target=\"_blank\">Clipperz</a> website.</p>",
'splashAlertCloseButtonLabel':	"Ok",
	
// Registration page - form	
'registrationFormTitle':								"create your account",
'registrationFormUsernameLabel':						"username",
'registrationFormPassphraseLabel':						"passphrase",
'registrationFormRetypePassphraseLabel':				"re-enter passphrase",
'registrationFormSafetyCheckLabel':						"I understand that Clipperz will not be able to recover a lost passphrase.",
'registrationFormTermsOfServiceCheckLabel':				"I have read and agreed to the <a href='https://www.clipperz.com/terms_service' target='_blank'>Terms of Service</a>.",
'registrationFormDoYouAlreadyHaveAnAccountLabel':		"do you already have an account?",
'registrationFormSimplyLoginLabel':						"simply login",
'registrationFormButtonLabel':							"Register",

// Registration page - warning messages
'registrationFormWarningMessageNotMatchingPassphrases':	"Your passphrases don't match, please re-type them.",
'registrationFormWarningMessageSafetyCheckNotSelected':	"Please read and check all the boxes below.",
'registrationFormWarningMessageTermsOfServiceCheckNotSelected':	"You need to agree to the Terms of Service.",

// Registration page - message panel	
'registrationMessagePanelInitialTitle':					"Creating account …",
'registrationMessagePanelInitialText':					"---",
'registrationMessagePanelInitialButtonLabel':			"Cancel",
'registrationMessagePanelRegistrationDoneTitle':		"Registration",
'registrationMessagePanelRegistrationDoneText':			"Done",
'registrationMessagePanelFailureTitle':					"Registration failed",
'registrationMessagePanelFailureButtonLabel':			"Close",

// Registration page - message panel - connection
'connectionRegistrationSendingRequestMessageText':		"Verifying credentials",
'connectionRegistrationSendingCredentialsMessageText':	"Sending credentials",

// Registration page - splash panel
'registrationSplashPanelTitle':				"Security advice",
'registrationSplashPanelDescription':		"<p>These are your Clipperz credentials, take good care of them. Clipperz will never display your username and passphrase a second time!</p>",
'registrationSplashPanelUsernameLabel':		"username",
'registrationSplashPanelPassphraseLabel':	"passphrase",
	
'registrationSplashPanelShowPassphraseButtonLabel':	"show passphrase",
	
//	Header links
'donateHeaderLinkLabel':					"donate",
'creditsHeaderLinkLabel':					"credits",
'feedbackHeaderLinkLabel':					"feedback",
'helpHeaderLinkLabel':						"help",
'forumHeaderLinkLabel':						"forum",
	
//	Menu labels
'recordMenuLabel':							"cards",
'accountMenuLabel':							"account",
'dataMenuLabel':							"data",
'contactsMenuLabel':						"contacts",
'toolsMenuLabel':							"tools",
'logoutMenuLabel':							"logout",
'lockMenuLabel':							"lock",

//	Lock dialog
'lockTitle':								"The account is locked",
'lockDescription':							"<p>To unlock your account, please enter your passphrase.</p>",
'unlockButtonLabel':						"Unlock",
		
//	Account panel - change passphrase
'changePasswordTabLabel':					"Change your passphrase",
'changePasswordTabTitle':					"Change your passphrase",

'changePasswordFormUsernameLabel':			"username",
'changePasswordFormOldPassphraseLabel':		"old passphrase",
'changePasswordFormNewPassphraseLabel':		"new passphrase",
'changePasswordFormRetypePassphraseLabel':	"re-enter new passphrase",
'changePasswordFormSafetyCheckboxLabel':	"I understand that Clipperz will not be able to recover a lost passphrase.",
'changePasswordFormSubmitLabel':			"Change passphrase",

//	Account panel - change passphrase - warning messages
'changePasswordFormWrongUsernameWarning':			"Wrong username",
'changePasswordFormWrongPassphraseWarning':			"Wrong passphrase",
'changePasswordFormWrongRetypePassphraseWarning':	"Your passphrases don't match, please re-type them.",
'changePasswordFormSafetyCheckWarning':				"Please read and check the box below.",

//	Account panel - change passphrase - progress dialog
'changePasswordFormProgressDialogTitle':								"Changing user credentials",
'changePasswordFormProgressDialogEmptyText':							"---",
'changePasswordFormProgressDialogConnectedMessageTitle':				"Connected",
'changePasswordFormProgressDialogConnectedMessageText':					"Done",
'changePasswordFormProgressDialogErrorMessageTitle':					"Error",
'changePasswordFormProgressDialogErrorMessageText':						"Credentials change failed!",

'changeCredentialsPanelEncryptingDataMessageTitle':		   				"Changing your passphrase",
'changeCredentialsPanelEncryptingDataMessageText':						"Local encryption of card headers",
'changeCredentialsPanelCreatingNewCredentialsMessageTitle':				"Changing your passphrase",
'changeCredentialsPanelCreatingNewCredentialsMessageText':				"Updating your credentials",
'changeCredentialsPanelSendingNewCredentialsToTheServerMessageTitle':	"Changing your passphrase",
'changeCredentialsPanelSendingNewCredentialsToTheServerMessageText':	"Uploading your encrypted credentials to Clipperz",
'changeCredentialsPanelDoneMessageTitle':								"Changing your passphrase",
'changeCredentialsPanelDoneMessageText':								"Done",

//	Account panel - OTP
'manageOTPTabLabel':		"Manage your one-time passphrases",
'manageOTPTabTitle':		"Manage your one-time passphrases",

'manageOTPTabDescription':	"\
	<p>A one-time passphrase works like your regular passphrase, but can be used only once.</p>\
	<p>If the same passphrase is used again at a later stage in a login attempt it will be rejected and the login process will fail.</p>\
	<p>Immediately after a successful login, your one-time passphrase will be deleted preventing any fraudulent access.</p>\
	<p>One-time passphrases are an excellent choice if one is concerned about keyloggers or spyware infections that may be collecting data from compromised machines.</p>\
	<p><b>It's strongly advisable to use one-time passphrases when accessing Clipperz from public terminals, such as Internet cafes and libraries.</b></p>",

//	Account panel - OTP - OTP table
'oneTimePasswordReadOnlyMessage': "\
	<h6>Sorry!</h6>\
	<p>You cannot manage your one-time passphrases when using the offline version of Clipperz.</p>",

'oneTimePasswordLoadingMessage':	"\
	<h6>Loading data</h6>\
	<p>Please wait …</p>",

'oneTimePasswordNoPasswordAvailable':	"\
	<h6>No one-time passphrase available</h6>\
	<p>Click the “New” button above to add one-time passphrases to your account.</p>",

'createNewOTPButtonLabel':					"New",
'deleteOTPButtonLabel':						"Delete",
'printOTPButtonLabel':						"Print",

'disabledOneTimePassword_warning':			"disabled",

'oneTimePasswordSelectionLink_selectLabel':	"Select:",
'oneTimePasswordSelectionLink_all':			"all",
'oneTimePasswordSelectionLink_none':		"none",
'oneTimePasswordSelectionLink_used':		"used",
'oneTimePasswordSelectionLink_unused':		"unused",

//Account panel - OTP - saving new OTP dialog
'saveOTP_encryptUserDataTitle':				"Saving one-time passphrase",
'saveOTP_encryptUserDataText':				"Processing new OTP credentials …",
'saveOTP_encryptOTPDataTitle':				"Saving one-time passphrase",
'saveOTP_encryptOTPDataText':				"Local encryption of authentication data …",
'saveOTP_sendingDataTitle':					"Saving one-time passphrase",
'saveOTP_sendingDataText':					"Sending authentication data to the server …",
'saveOTP_updatingInterfaceTitle':			"Saving one-time passphrase",
'saveOTP_updatingInterfaceText':			"Updating interface",
                                    		
// Account panel - preferences
'accountPreferencesLabel':					"Preferences",
'accountPreferencesTabTitle':				"Preferences",

'accountPreferencesLanguageTitle':			"Language",
'accountPreferencesLanguageDescription':	"<p>Choose your preferred language from the list below.</p>",

'showDonationReminderPanelTitle':			"Donation reminders",
'showDonationReminderPanelDescription':		"<p>Show donation reminders</p>",
	
'saveUserPreferencesFormSubmitLabel':		"Save",
'cancelUserPreferencesFormSubmitLabel':		"Cancel",

// Account panel - preferences - saving dialog
'accountPreferencesSavingPanelTitle_Step1':		"Saving preferences",
'accountPreferencesSavingPanelText_Step1':		"Local encryption of your preferences",
'accountPreferencesSavingPanelTitle_Step2':		"Saving preferences",
'accountPreferencesSavingPanelText_Step2':		"Sending encrypted preferences to Clipperz",

//	Account panel - login history
'accountLoginHistoryLabel':						"Login history",
'loginHistoryTabTitle':							"Login history",

'loginHistoryReadOnlyMessage':		"\
	<h6>Sorry!</h6>\
	<p>The login history is not available while using the offline version of Clipperz.</p>",
	
'loginHistoryLoadingMessage':		"\
	<h6>Loading data</h6>\
	<p>Please wait …</p>",
	
'loginHistoryLoadedMessage':		"\
	<h6>Your latest 10 logins</h6>\
	<p></p>",

'loginHistoryIPLabel':						"IP",
'loginHistoryTimeLabel':					"date",
'loginHistoryCurrentSessionText':			"current session",
'loginHistoryReloadButtonLabel':			"Reload login history",

//	Account panel - delete account
'deleteAccountTabLabel':					"Delete your account",
'deleteAccountTabTitle':					"Delete your account",

'deleteAccountFormUsernameLabel': 			"username",
'deleteAccountFormPassphraseLabel':			"passphrase",
'deleteAccountFormSafetyCheckboxLabel':		"I understand that all my data will be deleted and that this action is irreversible.",
'deleteAccountFormSubmitLabel':				"Delete my account",

//Account panel - delete account - warnings
'deleteAccountFormWrongUsernameWarning':	"Wrong username",
'deleteAccountFormWrongPassphraseWarning':	"Wrong passphrase",
'deleteAccountFormSafetyCheckWarning':		"Please read and check the box below.",

//Account panel - delete account - confirmation
'accountPanelDeletingAccountPanelConfirmationTitle':	"ATTENTION",
'accountPanelDeleteAccountPanelConfirmationText':		"Are your sure you want to delete your account?",
'accountPanelDeleteAccountPanelConfirmButtonLabel':		"Yes",
'accountPanelDeleteAccountPanelDenyButtonLabel':		"No",

//Account panel - delete account - confirmation
'accountPanelDeletingAccountPanelProgressTitle':		"Deleting the account data",
'accountPanelDeletingAccountPanelProgressText':			"The operation could take long, please be patient.",

//Data panel - offline copy
'offlineCopyTabLabel':									"Offline copy",
'offlineCopyTabTitle':									"Offline copy",
	
'offlineCopyTabDescription': "\
	<!-- 	FIX CSS		DONE!	-->	\
	<p>With just one click you can dump all your encrypted data from Clipperz servers to your hard disk and create a read-only offline version of Clipperz to be used when you are not connected to the Internet.</p>\
	<p>The read-only version is as secure as the read-and-write one and will not expose your data to higher risks since they both share the same code and security architecture.</p>\
	<ol>\
		<li><p>Click the link below to start the download.</p></li>\
		<li><p>The browser will ask you what to do with the “Clipperz_YYYYMMDD.html” file. Save it on your hard disk.</p></li>\
		<li><p>Double click on the downloaded file to launch the offline version in your browser.</p></li>\
		<li><p>Enter the usual username and passphrase.</p></li>\
	</ol>",

'offlineCopyDownloadLinkLabel':		"Download",

//	Data panel - offline copy - not updated
'offlineCopyDownloadWarning':		"\
	<!-- 	FIX CSS		DONE!	-->	\
	<h4><a href=\"#\" id=\"offlineCopyDownloadWarningLink\">Update your “offline copy”!</a></h4>\
	<p>You have recently created or modified one or more cards, it would be wise to download a new copy of the offline version.</p>",

'offlineCopyDownloadOk':								"",

//	Data panel - sharing
'sharingTabLabel':					"Sharing",
'sharingTabTitle':					"Sharing",

'sharingTabDescription':			"\
	<p>Quite often a confidential piece of information needs to be shared with one or more persons.</p>\
	<p>This could be as simple as giving your colleague the access code of your voice mailbox when you are out of the office, or as complicated as enabling the entitled heirs to access your safe deposit box at the local bank when you pass on.</p>\
	<p>Clipperz can make sharing your secrets a secure and straightforward process.</p>\
	<p></p>\
	<p><b>Coming soon …</b></p>",
	
// 	Data panel - import
'importTabLabel':					"Import",
'importTabTitle':					"Import",

'importTabDescription':				"<p>You can bulk import data to your Clipperz account from several file formats.</p>",
	
//	Data panel - export
'printingTabLabel':					"Export",
'printingTabTitle':					"Export",

'printingTabDescription':			"\
	<h5>Printing</h5>\
	<p>Click on the link below to open a new window displaying all your cards in a printable format.</p>\
	<p>If you are going to print for backup purposes, please consider the safer option provided by the “offline copy”.</p>",

'printingLinkLabel':				"Printable version",

'exportTabDescription':				"\
	<h5>Exporting to JSON</h5>\
	<p>JSON enables a “lossless” export of your cards. All the information will be preserved, including direct login configurations.</p>\
	<p>This custom format it’s quite convenient if you need to move some of all of your cards to a different Clipperz account. Or if you want to restore a card that has been accidentally deleted.</p>\
	<p>Click on the link below to start the export process.</p>",

'exportLinkLabel':					"Export to JSON",

'exportDataInProgressDescription':	"<h4>Exporting, please wait while your data are being processed …</h4>",

'exportDataDescription':			"\
	<h4>Instructions</h4>\
	<p>Copy the text below to your favorite editor and save it. (e.g. “clipperz_export_20071217.json”)</p>",
	
//	Contacts panel
'contactsTabLabel':					"Contacts",
'contactsTabTitle':					"Contacts",

//Tools panel - password generator
'passwordGeneratorTabLabel':		"Password generator",
'bookmarkletTabLabel':				"Bookmarklet",
'compactTabLabel':					"Compact edition",
'httpAuthTabLabel':					"HTTP authentication",

'passwordGeneratorTabTitle':		"Password generator",
'bookmarkletTabTitle':				"Bookmarklet",
'compactTabTitle':					"Compact edition",
'httpAuthTabTitle':					"HTTP authentication",
	
	
//	Tools panel - password generator - description
'paswordGeneratorTabDescription':	"<p></p>",
'passwordGeneratorTabButtonLabel':	"Generate password",

//	Tools panel - bookmarklet
'bookmarkletTabLabel':				"Bookmarklet",
'bookmarkletTabTitle':				"Bookmarklet",
		
'bookmarkletTabDescription':		"\
	<!-- 	FIX CSS		DONE!	-->	\
	<p>A bookmarklet is a simple “one-click” tool that can perform very useful tasks. It can be saved and used like a normal web page bookmark.</p>\
	<p>The Clipperz bookmarklet will help you to quickly create new cards and new “direct logins” within existing cards.</p>\
	<p><b>Please note that the bookmarklet does not include any information related to your account (e.g. your username or passphrase), the bookmarklet is a general tool containing the same code for every Clipperz user.</b></p>\
	<h3>How to install the bookmarklet</h3>\
	<h>Firefox, Camino, Opera, Safari</h5>\
	<ol>\
		<li><p>Make sure that the “Bookmarks Bar” is displayed by selecting “View > Toolbars > Bookmarks”, or similar menu items, from the browser menu.</p></li>\
		<li><p>Drag and drop the “Add to Clipperz” link below to the bookmark bar.</p></li>\
	</ol>\
	\
	<h5>Internet Explorer</h5>\
	<ol>\
		<li><p>Make sure that the “Links” toolbar is displayed by selecting “View > Toolbars > Links” from the browser menu.</p></li>\
		<li><p>Right-click on the “Add to Clipperz” link below.</p></li>\
		<li><p>Select “Add to favorites” from the contextual menu.</p></li>\
		<li><p>Click “Yes” for any security message that pops up.</p></li>\
		<li><p>Open the “Links” folder and click “OK”</p></li>\
	</ol>",
	
'bookmarkletTabBookmarkletTitle':	"Add to Clipperz",

//	Tools panel - bookmarklet - instructions
'bookmarkletTabInstructions':		"\
	<!-- 	FIX CSS		DONE!	-->	\
	<h3>How to create a new card inclusive of a “direct login” link to an online service</h3>\
	<ol>\
		<li><p>Open the web page where the login form is hosted. (this is the page where you usually enter your sign-in credentials)</p></li>\
		<li><p>Launch the bookmarklet by clicking on it: a pop-up window will appear over the web page.</p></li>\
		<li><p>Copy to the clipboard the content of the large text area within the pop-up. (ctrl-C)</p></li>\
		<li><p>Enter your Clipperz account and click on the <b>Add new card</b> button.</p></li>\
		<li><p>Select the “Direct login” template and paste the content of the clipboard to the large text area in the form. (ctrl-V)</p></li>\
		<li><p>Press the <b>Create</b> button, complete and review the details, then click <b>Save</b>.</p></li>\
	</ol>\
	\
	<h3>How to add a “direct login” link to an existing card</h3>\
	<ol>\
		<li><p>Same as above.</p></li>\
		<li><p>Same as above.</p></li>\
		<li><p>Same as above.</p></li>\
		<li><p>Enter your Clipperz account and select the card containing the credentials for the web service you just visited and click the <b>Edit</b> button.</p></li>\
		<li><p>Paste the content of the clipboard to the large text area in the “Direct logins” section. (ctrl-V)</p></li>\
		<li><p>Press the <b>Add direct login</b> button, review the details and then click <b>Save</b>.</p></li>\
	</ol>\
	\
	<p></p>\
	<p>Further information about the bookmarklet are <a href=\"http://www.clipperz.com/support/user_guide/bookmarklet\" target=\"_blank\">available here</a>.</p>",

//	Tools panel - Compact - instructions
'compactTabDescription':				"\
	<!-- 	FIX CSS		DONE!	-->	\
	<p>Clipperz Compact is a special version of Clipperz designed to be opened in the Firefox sidebar.</p>\
	<p>Its purpose is to keep your collection of “direct logins” always at hand. Read more <a href=\"http://www.clipperz.com/support/user_guide/clipperz_compact\", target=\"blank\">here</a></p>\
	\
	<h3>How to launch Clipperz Compact in the sidebar</h3>\
	<ol>\
		<li><p>Get Firefox! Sidebars are only available in Firefox and you need to switch to Firefox in order to enjoy the convenience of Clipperz Compact.</p></li>\
		<li>\
			<p>Add the following URL to Firefox bookmarks, or even better, drag it to the bookmark bar.</p>\
			<div id=\"compactLinkBox\"><a href=\"./index.html?compact\" target=\"_search\">Clipperz Compact</a></div>\
		</li>\
		<li><p>Change the properties of the bookmark so that “load this bookmark in the sidebar” is checked.</p></li>\
	</ol>\
	\
	<h5>Added bonus: Clipperz Compact works also in Opera’s panel.</h5>",

//	Tools panel - HTTP authentication - instructions
'httpAuthTabDescription':					"\
	<!-- 	FIX CSS		DONE!	-->	\
	<p>HTTP authentication is a method designed to allow a web browser to provide credentials – in the form of a username and password – including them in a website address (HTTP or HTTPS URL).</p>\
	<p>Nowadays it is rarely used, but it can still be found on small, private websites. You can tell that a website is protected by HTTP authentication when the browser displays a pop-up window to enter username and password.</p>\
	<p>Unfortunately the Clipperz bookmarklet does not work on websites that use HTTP authentication. However you can still create a “direct login”.</p>\
	\
	<h3>How to create a “direct login” for a website that uses HTTP authentication</h3>\
	<ol>\
		<li><p>Store website URL, username and password in a new card.</p></li>\
		<li><p>Copy the configuration below and paste it to the large text area in the “Direct logins” section of the new card.</p></li>\
		<li><p>Press the <b>Add direct login</b> button, bind URL, username and password fields and then click <b>Save</b>.</p></li>\
	</ol>\
	\
	<h5><a href=\"http://support.microsoft.com/kb/834489\" target=\"_blank\">Warning: Internet Explorer does not support HTTP authentication.</a></h5>",
	
// Direct logins block
'mainPanelDirectLoginBlockLabel':			"Direct logins",
'directLinkReferenceShowButtonLabel':		"show",
	
// Direct logins - blank slate
'mainPanelDirectLoginBlockDescription':		"\
	<!-- 	FIX CSS		DONE!	-->	\
	<p>Add “direct logins” to sign in to your web accounts without typing usernames and passwords!</p>\
	<p>“Direct logins” greatly enhance your password security since you can:</p>\
	<ul>\
		<li><p>conveniently adopt and enter complex passwords;</p></li>\
		<li><p>never re-use the same and easy-to-guess password.</p></li>\
	</ul>\
	<p>Simple and quick configuration with the <b>Clipperz bookmarklet</b>.</p>\
	<a href=\"http://www.clipperz.com/support/user_guide/direct_logins\" target=\"_blank\">Learn more about “direct logins”</a>",

// Cards block		
'mainPanelRecordsBlockLabel':				"Cards",
'mainPanelAddRecordButtonLabel':			"Add new card",
'mainPanelRemoveRecordButtonLabel':			"Delete card",

// Cards block - filter tabs
'mainPanelRecordFilterBlockAllLabel':		"all",
'mainPanelRecordFilterBlockTagsLabel':		"tags",
'mainPanelRecordFilterBlockSearchLabel':	"search",
	
// Cards block - blank slate
'recordDetailNoRecordAtAllTitle':			"Welcome to Clipperz!",
'recordDetailNoRecordAtAllDescription':		"\
	<h5>Get started by adding cards to your account.</h5>\
	<p>Cards are simple and flexible forms where you can store your passwords and any other confidential data.</p>\
	<p>Cards could contain credentials for accessing a web site, the combination of your bicycle lock, details of your credit card, …</p>\
	\
	<h5>Don't forget the Clipperz bookmarklet!</h5>\
	<p>Before you start, install the “Add to Clipperz” bookmarklet: it will make creating cards easier and more fun.</p>\
	<p>Go to the “Tools” tab to discover how to install it and how it use it.</p>\
	<p></p>\
	<p>Then simply click the <b>\"Add new card\"</b> button and enjoy your Clipperz account.</p>\
	<p></p>\
	<a  href=\"http://www.clipperz.com/support/user_guide/managing_cards\" target=\"_blank\">Learn more about creating and managing cards</a>",

// Cards block - new card wizard - bookmarklet configuration
'newRecordWizardTitleBox': 					"\
	<h5>Please select a template</h5>\
	<p>Cards are simple and flexible forms where you can store passwords or any other confidential data.</p>\
	<p>Start choosing one of the templates below. You can always customize your cards later by adding or removing fields.</p>",
	
'newRecordWizardBookmarkletConfigurationTitle':			"Direct login",
'newRecordWizardBookmarkletConfigurationDescription':	"\
	<p>Paste below the configuration code generated by the Clipperz bookmarklet.</p>\
	<p>A new card complete with a direct login to your web account will be created.</p>",

'newRecordWizardCreateButtonLabel':						"Create",
'newRecordWizardCancelButtonLabel':						"Cancel",
	
//	Create new card - Donation splash
'donateSplashPanelTitle':								"Support Clipperz, make a donation today!",
'donateSplashPanelDescription': 						"\
	<!-- 	FIX CSS		DONE!	-->	\
	<p>A few good reasons to make a donation:</p>\
	<ul>\
		<li><p>support the development of new features</p></li>\
		<li><p>keep Clipperz free</p></li>\
		<li><p>show appreciation for our hard work</p></li>\
	</ul>\
	<p>For any further information, please visit our <a href=\"http://www.clipperz.com/donations\" target=\"_blank\">Donations page</a>.</p>\
	<p><b>Ready to donate?</b></p>",

'donateCloseButtonLabel':								"Not yet",
'donateDonateButtonLabel':								"Yes",
	
// Card templates
'recordTemplates': {

//Web password
	'WebAccount': {
		'title': 		"Web password",
		'description':	"<p>A simple card to store login credentials for your online services.</p>",
		'fields': [
			{label:"Web address", type:'URL'},
			{label:"Username or email", type:'TXT'},
			{label:"Password", type:'PWD'}
		]
	},

//Bank account	
	'BankAccount': {
		'title':		"Bank account",
		'description':	"<p>Safely store your bank account number and online banking credentials.</p>",
		'fields': [
			{label:"Bank", type:'TXT'},
			{label:"Account number", type:'TXT'},
			{label:"Bank website", type:'URL'},
			{label:"Online banking ID", type:'TXT'},
			{label:"Online banking password", type:'PWD'}
		]
	},

// 	Credit card
	'CreditCard': {
		'title':		"Credit card",
		'description':	"<p>Card number, expire date, CVV2 and PIN always at hand with Clipperz.</p>",
		'fields': [
			{label:"Type (Visa, AmEx, …)", type:'TXT'},
			{label:"Number", type:'TXT'},
			{label:"Owner name", type:'TXT'},
			{label:"Expiry date", type:'TXT'},
			{label:"CVV2", type:'TXT'},
			{label:"PIN", type:'PWD'},
			{label:"Card website", type:'URL'},
			{label:"Username", type:'TXT'},
			{label:"Password", type:'PWD'}
		]
	},

// 	Address book entry	
	'AddressBookEntry': {
		'title':		"Address book entry",
		'description':	"<p>Clipperz could also work as your new private address book. Use this template to easily add a new entry.</p>",
		'fields': [
			{label:"Name", type:'TXT'},
			{label:"Email", type:'TXT'},
			{label:"Phone", type:'TXT'},
			{label:"Mobile", type:'TXT'},
			{label:"Address", type:'ADDR'},
		]
	},

//Custom card
	'Custom': {
		'title': 		"Custom card",
		'description':	"<p>No matter which kind of confidential data you need to protect, create a custom card to match your needs.</p>",
		'fields': [
			{label:"Label 1", type:'TXT'},
			{label:"Label 2", type:'TXT'},
			{label:"Label 3", type:'TXT'}
		]
	}
},


'recordFieldTypologies': {
	'TXT': {
		description: "simple text field",
		shortDescription: "text"
	},
	'PWD': {
		description: "simple text field, with default status set to hidden",
		shortDescription: "password"
	},
	'URL': {
		description: "simple text field in edit mode, that became an active url in view mode",
		shortDescription: "web address"
	},
	'DATE': {
		description: "a value set with a calendar helper",
		shortDescription: "date"
	},
	'ADDR': {
		description: "just like the URL, but the active link points to Google Maps (or similar service) passing the address value as argument",
		shortDescription: "street address"
	},
	'CHECK': {
		description: "check description",
		shortDescription: "check"
	},
	'RADIO': {
		description: "radio description",
		shortDescription: "radio"
	},
	'SELECT': {
		description: "select description",
		shortDescription: "select"
	}
},

// Cards block - new card - warnings
'newRecordPanelGeneralExceptionTitle':						"Error",
'newRecordPanelGeneralExceptionMessage':					"The configuration text is not valid. Make sure to get your text from the bookmarklet pop-up and retry.",
'newRecordPanelWrongBookmarkletVersionExceptionTitle':		"Error",
'newRecordPanelWrongBookmarkletVersionExceptionMessage':	"The configuration text has been generated by an old version of the bookmarklet. Please update your bookmarklet and retry.",
'newRecordPanelExceptionPanelCloseButtonLabel':				"Cancel",

// Cards block - delete card
'mainPanelDeletingRecordPanelConfirmationTitle':			"Deleting selected card",
'mainPanelDeleteRecordPanelConfirmationText':				"Do your really want to delete the selected card?",
'mainPanelDeleteRecordPanelConfirmButtonLabel':				"Yes",
'mainPanelDeleteRecordPanelDenyButtonLabel':				"No",
'mainPanelDeletingRecordPanelInitialTitle':					"Deleting selected card",
'mainPanelDeletingRecordPanelInitialText':					"---",
'mainPanelDeletingRecordPanelCompletedText':				"Done",

// Cards block - delete card panel
'deleteRecordPanelCollectRecordDataMessageTitle':		   	"Delete card",
'deleteRecordPanelCollectRecordDataMessageText':		   	"Updating card list",
'deleteRecordPanelEncryptUserDataMessageTitle':			   	"Delete card",
'deleteRecordPanelEncryptUserDataMessageText':			   	"Local encryption of card headers",
'deleteRecordPanelSendingDataToTheServerMessageTitle':	   	"Delete card",
'deleteRecordPanelSendingDataToTheServerMessageText':	   	"Uploading encrypted card headers to Clipperz",
'deleteRecordPanelUpdatingTheInterfaceMessageTitle':	   	"Delete card",
'deleteRecordPanelUpdatingTheInterfaceMessageText':		   	"Updating the interface",
	
// Cards block - no record selected
'recordDetailNoRecordSelectedTitle':						"No card selected",
'recordDetailNoRecordSelectedDescription':					"<p>Please select a card from the list on the left.</p>",

// Cards block - loading messages	
'recordDetailLoadingRecordMessage':							"Downloading encrypted card from Clipperz",
'recordDetailDecryptingRecordMessage':						"Local decryption of card\'s data",
'recordDetailLoadingRecordVersionMessage':					"Downloading latest card version",
'recordDetailDecryptingRecordVersionMessage':				"Local decryption of latest version",
'recordDetailLoadingErrorMessageTitle':						"Error while downloading the card",
	
// Cards block - card details
'recordDetailNotesLabel':									"Notes",
'recordDetailLabelFieldColumnLabel':						"Field label",
'recordDetailDataFieldColumnLabel':							"Field data",
'recordDetailTypeFieldColumnLabel':							"Type",

'recordDetailSavingChangesMessagePanelInitialTitle':		"Saving card",
'recordDetailSavingChangesMessagePanelInitialText':			"---",

'recordDetailRemoveFieldButtonLabel':						"-",
'recordDetailAddFieldButtonLabel':							"Add new field",
'recordDetailPasswordFieldHelpLabel':						"click the stars to select the password and then Ctrl-C to copy",

'recordDetailPasswordFieldScrambleLabel':					"scramble",
'recordDetailPasswordFieldUnscrambleLabel':					"unscramble",
	
'recordDetailDirectLoginBlockTitle':						"Direct logins",
'recordDetailNewDirectLoginDescription':					"<p>Direct login configuration</p>",

'recordDetailDirectLoginBlockNoDirectLoginConfiguredDescription':	"\
	<p>Does this card contain credentials to access an online service?</p>\
	<p>Use the bookmarklet to configure a “direct login” from Clipperz with just one click!</p>",

'recordDetailDeleteDirectLoginButtonLabel':					"-",
'recordDetailAddNewDirectLoginButtonLabel':					"Add new direct login",

'recordDetailEditButtonLabel':								"Edit",
'recordDetailSaveButtonLabel':								"Save",
'recordDetailCancelButtonLabel':							"Cancel",

'newRecordTitleLabel':										"_new card_",
'newDirectLoginLabelSuffix':								"",

// Cards block - save card panel
'recordSaveChangesPanelCollectRecordInfoMessageTitle':			"Save card",
'recordSaveChangesPanelCollectRecordInfoMessageText':			"Updating card headers",
'recordSaveChangesPanelEncryptUserDataMessageTitle':			"Save card",
'recordSaveChangesPanelEncryptUserDataMessageText':				"Local encryption of card headers",
'recordSaveChangesPanelEncryptRecordDataMessageTitle':			"Save card",
'recordSaveChangesPanelEncryptRecordDataMessageText':			"Local encryption of card's data",
'recordSaveChangesPanelEncryptRecordVersionDataMessageTitle':	"Save card",
'recordSaveChangesPanelEncryptRecordVersionDataMessageText':	"Local encryption of card's version data",
'recordSaveChangesPanelSendingDataToTheServerMessageTitle':		"Save card",
'recordSaveChangesPanelSendingDataToTheServerMessageText':		"Uploading encrypted card's header to Clipperz",
'recordSaveChangesPanelUpdatingTheInterfaceMessageTitle':		"Save card",
'recordSaveChangesPanelUpdatingTheInterfaceMessageText':		"Updating the interface",

//	Password Generator strings
'passwordGeneratorPanelTitle':									"Password generator",
'passwordGeneratorPanelOkLabel':								"Ok",
'passwordGeneratorPanelCancelLabel':							"Cancel",

'passwordGeneratorLowercaseLabel':								"abc",
'passwordGeneratorUppercaseLabel':								"ABC",
'passwordGeneratorNumberLabel':									"012",
'passwordGeneratorSymbolLabel':									"@#$",

'passwordGeneratorLengthLabel':									"length:",


//Miscellaneous strings	

//'DWRUtilLoadingMessage':										"Loading data …",
'comingSoon':													"coming soon …",
'panelCollectingEntryopyMessageText':							"Collecting entropy",
'directLoginConfigurationCheckBoxFieldSelectedValue':			"Yes",
'directLoginConfigurationCheckBoxFieldNotSelectedValue':		"No",
	

		
// NEW - Import panel
'importFormats':	{
	'CSV': {
		'label':		"CSV",
		'description':	"<p>A widely recognized file format that stores tabular data. Several password managers can export data to this format.</p>"
	},
	'Excel': {
		'label':		"Excel",
		'description':	"<p>The popular spreadsheet from Microsoft. Storing passwords in Excel files is very common but not advisable.</p>"
	},
	'KeePass': {
		'label':		"KeePass",
		'description':	"<p>The custom TXT file created by KeePass password manager.</p>"
	},
	'PasswordPlus': {
		'label':		"Password Plus",
		'description':	"<p>The custom CSV format produced by Password Plus, a password manager mostly used on mobile devices.</p>"
	},
	'Roboform': {
		'label':		"RoboForm",
		'description':	"<p>The special HTML file created by Roboform password manager when displaying Passcard and Safenotes for printing.</p>"
	},
	'ClipperzExport': {
		'label':		"JSON",
		'description':	"<p>The file created by Clipperz itself in JSON format. It preserves all information contained in your cards, even direct login configurations.</p>"
	}
},

//	JSON
'Clipperz_ImportWizard_Title':				"JSON import",
'importOptions_clipperz_description':		"<p>Open the JSON file exported from Clipperz in a text editor. Then copy and paste its content to the text area below.</p>",

//	CSV
'CSV_ImportWizard_Title':					"CSV import",
'importOptions_csv_description_':			"\
	<p>Open the CSV file in a text editor. Then copy and paste its content to the text area below.</p>\
	<p>Please select the special characters used within your file.</p>",

//	Excel
'Excel_ImportWizard_Title':					"Excel import",
'importOptions_excel_description_':			"<p>Open the Excel file and select the cells you want to import. Then copy and paste them to the text area below.</p>",

//	KeePass
'KeePass_ImportWizard_Title':				"KeePass import",
'importOptions_keePass_description_':		"<p>Open the TXT file created by Keepass in a text editor. Then copy and paste its content to the text area below.</p>",
	
//	PasswordPlus
'PasswordPlus_ImportWizard_Title':			"Password Plus import",
'importOptions_passwordPlus_description':	"<p>Open the CSV file created by PasswordPlus in a text editor. Then copy and paste its content to the text area below.</p>",

//	RoboForm
'RoboForm_ImportWizard_Title':				"RoboForm import",
'importOptions_roboForm_description':		"<p>Open the HTML file created by RoboForm in a text editor. Then copy and paste its content to the text area below.</p>",
	
	
'importData_parsingDataTitle':				"Import",
'importData_parsingDataText':				"Parsing data …",

'importData_previewingDataTitle':			"Import",
'importData_previewingDataText':			"Processing data …",

'importData_processingDataTitle':			"Import",
'importData_processingDataText':			"Creating new cards …",
	
'ImportWizard': {
	'EDIT':									"edit",
	'PREVIEW':								"preview",
	'IMPORT':								"import",
	
	'KEEPASS_SETTINGS':						"settings",
	
	'CSV_EDIT':								"paste",
	'CSV_COLUMNS':							"columns",
	'CSV_HEADER':							"labels",
	'CSV_TITLE':							"titles",
	'CSV_NOTES':							"notes",
	'CSV_FIELDS':							"types",
	
	'EXCEL_EDIT':							"edit"
},

'CSV_ImportWizard_Columns':			"<p>Select the columns you want to import.</p>",
'CSV_ImportWizard_Header':			"<p>If the first row of the CSV file contains field labels, tick off the checkbox below.</p>",
'CSV_ImportWizard_Header_Settings_firstRowHeaderLabel':	"Use the first row as labels?",
'CSV_ImportWizard_Title':			"<p>Select the column that contains titles of the cards you are importing. (mandatory)</p>",
'CSV_ImportWizard_Notes':			"<p>Select the column that represents a \"notes\" field. (optional)</p>",
'CSV_ImportWizard_Notes_Settings_noSelectionLabel':		"\"notes\" field not present",
'CSV_ImportWizard_Fields':			"<p>Select the correct type for each column from the drop down lists.</p>",
'CSV_ImportWizard_Fields_MissingLabelWarning':			"Missing label",

'importData_importConfirmation_title':					"Import",
'importData_importConfirmation_text':					"Do you want to import __numberOfRecords__ cards?",
	

//	Vulnerability warning
'VulnerabilityWarning_Panel_title':			"Vulnerability warning",
'VulnerabilityWarning_Panel_message':		"The action as been aborted due to a catched vulnerability",
'VulnerabilityWarning_Panel_buttonLabel':	"Close",


	
//	All the loginInfo panel infos

'WELCOME_BACK':						"Welcome back!",
	
'currentConnectionText':			"You are connected from ip&nbsp;__ip__, apparently from __country__, using __browser__ on __operatingSystem__.",
'latestConnectionText':				"Your latest connection was __elapsedTimeDescription__ (__time__) from ip&nbsp;__ip__, apparently from __country__, using __browser__ on __operatingSystem__.",

'fullLoginHistoryLinkLabel':		"show login history",
	
'elapsedTimeDescriptions': {
	'MORE_THAN_A_MONTH_AGO':		"more than a month ago",
	'MORE_THAN_A_WEEK_AGO':			"more than a week ago",
	'MORE_THAN_*_WEEKS_AGO':		"more than __elapsed__ weeks ago",
	'YESTERDAY':					"yesterday",
	'*_DAYS_AGO':					"__elapsed__ days ago",
	'ABOUT_AN_HOUR_AGO':			"about an hour ago",
	'*_HOURS_AGO':					"__elapsed__ hours ago",
	'JUST_A_FEW_MINUTES_AGO':		"just a few minutes ago",
	'ABOUT_*_MINUTES_AGO':			"about __elapsed__ minutes ago"
},

'unknown_ip':						"unknown",
	
'countries': {
	'--':			"unknown",
	'AD':			"Andorra",
	'AE':			"United Arab Emirates",
	'AF':			"Afghanistan",
	'AG':			"Antigua and Barbuda",
	'AI':			"Anguilla",
	'AL':			"Albania",
	'AM':			"Armenia",
	'AN':			"Netherlands Antilles",
	'AO':			"Angola",
	'AP':			"Non-Spec Asia Pas Location",
	'AR':			"Argentina",
	'AS':			"American Samoa",
	'AT':			"Austria",
	'AU':			"Australia",
	'AW':			"Aruba",
	'AX':			"Aland Islands",
	'AZ':			"Azerbaijan",
	'BA':			"Bosnia and Herzegowina",
	'BB':			"Barbados",
	'BD':			"Bangladesh",
	'BE':			"Belgium",
	'BF':			"Burkina Faso",
	'BG':			"Bulgaria",
	'BH':			"Bahrain",
	'BI':			"Burundi",
	'BJ':			"Benin",
	'BM':			"Bermuda",
	'BN':			"Brunei Darussalam",
	'BO':			"Bolivia",
	'BR':			"Brazil",
	'BS':			"Bahamas",
	'BT':			"Bhutan",
	'BW':			"Botswana",
	'BY':			"Belarus",
	'BZ':			"Belize",
	'CA':			"Canada",
	'CD':			"Congo the Democratic Republic of the",
	'CF':			"Central African Republic",
	'CH':			"Switzerland",
	'CI':			"Cote D'ivoire",
	'CK':			"Cook Islands",
	'CL':			"Chile",
	'CM':			"Cameroon",
	'CN':			"China",
	'CO':			"Colombia",
	'CR':			"Costa Rica",
	'CS':			"Serbia and Montenegro",
	'CU':			"Cuba",
	'CY':			"Cyprus",
	'CZ':			"Czech Republic",
	'DE':			"Germany",
	'DJ':			"Djibouti",
	'DK':			"Denmark",
	'DO':			"Dominican Republic",
	'DZ':			"Algeria",
	'EC':			"Ecuador",
	'EE':			"Estonia",
	'EG':			"Egypt",
	'ER':			"Eritrea",
	'ES':			"Spain",
	'ET':			"Ethiopia",
	'EU':			"European Union",
	'FI':			"Finland",
	'FJ':			"Fiji",
	'FM':			"Micronesia Federated States of",
	'FO':			"Faroe Islands",
	'FR':			"France",
	'GA':			"Gabon",
	'GB':			"United Kingdom",
	'GD':			"Grenada",
	'GE':			"Georgia",
	'GF':			"French Guiana",
	'GG':			"Guernsey",
	'GH':			"Ghana",
	'GI':			"Gibraltar",
	'GL':			"Greenland",
	'GM':			"Gambia",
	'GP':			"Guadeloupe",
	'GR':			"Greece",
	'GT':			"Guatemala",
	'GU':			"Guam",
	'GW':			"Guinea-Bissau",
	'GY':			"Guyana",
	'HK':			"Hong Kong",
	'HN':			"Honduras",
	'HR':			"Croatia (Local Name: Hrvatska)",
	'HT':			"Haiti",
	'HU':			"Hungary",
	'ID':			"Indonesia",
	'IE':			"Ireland",
	'IL':			"Israel",
	'IM':			"Isle of Man",
	'IN':			"India",
	'IO':			"British Indian Ocean Territory",
	'IQ':			"Iraq",
	'IR':			"Iran (Islamic Republic of)",
	'IS':			"Iceland",
	'IT':			"Italy",
	'JE':			"Jersey",
	'JM':			"Jamaica",
	'JO':			"Jordan",
	'JP':			"Japan",
	'KE':			"Kenya",
	'KG':			"Kyrgyzstan",
	'KH':			"Cambodia",
	'KI':			"Kiribati",
	'KN':			"Saint Kitts and Nevis",
	'KR':			"Korea Republic of",
	'KW':			"Kuwait",
	'KY':			"Cayman Islands",
	'KZ':			"Kazakhstan",
	'LA':			"Lao People's Democratic Republic",
	'LB':			"Lebanon",
	'LC':			"Saint Lucia",
	'LI':			"Liechtenstein",
	'LK':			"Sri Lanka",
	'LR':			"Liberia",
	'LS':			"Lesotho",
	'LT':			"Lithuania",
	'LU':			"Luxembourg",
	'LV':			"Latvia",
	'LY':			"Libyan Arab Jamahiriya",
	'MA':			"Morocco",
	'MC':			"Monaco",
	'MD':			"Moldova Republic of",
	'MG':			"Madagascar",
	'MH':			"Marshall Islands",
	'MK':			"Macedonia the Former Yugoslav Republic of",
	'ML':			"Mali",
	'MM':			"Myanmar",
	'MN':			"Mongolia",
	'MO':			"Macau",
	'MP':			"Northern Mariana Islands",
	'MR':			"Mauritania",
	'MS':			"Montserrat",
	'MT':			"Malta",
	'MU':			"Mauritius",
	'MV':			"Maldives",
	'MW':			"Malawi",
	'MX':			"Mexico",
	'MY':			"Malaysia",
	'MZ':			"Mozambique",
	'NA':			"Namibia",
	'NC':			"New Caledonia",
	'NF':			"Norfolk Island",
	'NG':			"Nigeria",
	'NI':			"Nicaragua",
	'NL':			"Netherlands",
	'NO':			"Norway",
	'NP':			"Nepal",
	'NR':			"Nauru",
	'NU':			"Niue",
	'NZ':			"New Zealand",
	'OM':			"Oman",
	'PA':			"Panama",
	'PE':			"Peru",
	'PF':			"French Polynesia",
	'PG':			"Papua New Guinea",
	'PH':			"Philippines",
	'PK':			"Pakistan",
	'PL':			"Poland",
	'PR':			"Puerto Rico",
	'PS':			"Palestinian Territory Occupied",
	'PT':			"Portugal",
	'PW':			"Palau",
	'PY':			"Paraguay",
	'QA':			"Qatar",
	'RO':			"Romania",
	'RS':			"Serbia",
	'RU':			"Russian Federation",
	'RW':			"Rwanda",
	'SA':			"Saudi Arabia",
	'SB':			"Solomon Islands",
	'SC':			"Seychelles",
	'SD':			"Sudan",
	'SE':			"Sweden",
	'SG':			"Singapore",
	'SI':			"Slovenia",
	'SK':			"Slovakia (Slovak Republic)",
	'SL':			"Sierra Leone",
	'SM':			"San Marino",
	'SN':			"Senegal",
	'SR':			"Suriname",
	'SV':			"El Salvador",
	'SY':			"Syrian Arab Republic",
	'SZ':			"Swaziland",
	'TC':			"Turks and Caicos Islands",
	'TG':			"Togo",
	'TH':			"Thailand",
	'TJ':			"Tajikistan",
	'TM':			"Turkmenistan",
	'TN':			"Tunisia",
	'TO':			"Tonga",
	'TR':			"Turkey",
	'TT':			"Trinidad and Tobago",
	'TV':			"Tuvalu",
	'TW':			"Taiwan Province of China",
	'TZ':			"Tanzania United Republic of",
	'UA':			"Ukraine",
	'UG':			"Uganda",
	'US':			"United States",
	'UY':			"Uruguay",
	'UZ':			"Uzbekistan",
	'VA':			"Holy See (Vatican City State)",
	'VE':			"Venezuela",
	'VG':			"Virgin Islands (British)",
	'VI':			"Virgin Islands (U.S.)",
	'VN':			"Viet Nam",
	'VU':			"Vanuatu",
	'WF':			"Wallis and Futuna Islands",
	'WS':			"Samoa",
	'YE':			"Yemen",
	'ZA':			"South Africa",
	'ZM':			"Zambia",
	'ZW':			"Zimbabwe",
	'ZZ':			"Reserved"
},

'browsers': {
	'UNKNOWN':		"Unknown",
	'MSIE':			"Internet Explorer",
	'FIREFOX':		"Firefox",
	'OPERA':		"Opera",
	'SAFARI':		"Safari",
	'OMNIWEB':		"OmniWeb",
	'CAMINO':		"Camino",
	'CHROME':		"Chrome"
},

'operatingSystems': {
	'UNKNOWN':		"Unknown",
	'WINDOWS':		"Windows",
	'MAC':			"Mac",
	'LINUX':		"Linux",
	'IPHONE':		"iPhone",
	'MOBILE':		"Mobile",
	'OPENBSD':		"OpenBSD",
	'FREEBSD':		"FreeBSD",
	'NETBSD':		"NetBSD"
},
	
	
//	Calendar texts
'calendarStrings': {
	'months': 	{
		'0':	"January",
		'1':	"February",
		'2':	"March",
		'3':	"April",
		'4':	"May",
		'5':	"June",
		'6':	"July",
		'7':	"August",
		'8':	"September",
		'9':	"October",
		'10':	"November",
		'11':	"December"
	},
	'shortMonths':	{
		'0':	"Jan",
		'1':	"Feb",
		'2':	"Mar",
		'3':	"Apr",
		'4':	"May",
		'5':	"Jun",
		'6':	"Jul",
		'7':	"Aug",
		'8':	"Sep",
		'9':	"Oct",
		'10':	"Nov",
		'11':	"Dec"
	},

	'days':	{
		'0':	"Sunday",
		'1':	"Monday",
		'2':	"Tuesday",
		'3':	"Wednesday",
		'4':	"Thursday",
		'5':	"Friday",
		'6':	"Saturday"
	},

	'shortDays':	{
		'0':	"Sun",
		'1':	"Mon",
		'2':	"Tue",
		'3':	"Wed",
		'4':	"Thu",
		'5':	"Fri",
		'6':	"Sat"
	},

	'veryShortDays':	{
		'0':	"Su",
		'1':	"Mo",
		'2':	"Tu",
		'3':	"We",
		'4':	"Th",
		'5':	"Fr",
		'6':	"Sa"
	},

	'amDesignation':	"am",
	'pmDesignation':	"pm"
},


// Date format
'fullDate_format':	"l, F d, Y H:i:s",
	
__syntaxFix__: "syntax fix"

}
