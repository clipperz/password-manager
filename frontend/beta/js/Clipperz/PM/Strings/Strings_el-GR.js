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

//=============================================================================
//
//		G R E E K  (el_GR)
//
//=============================================================================

Clipperz.PM.Strings.Languages['el-gr'] = MochiKit.Base.merge(Clipperz.PM.Strings.Languages['en-us'], {

//-----------------------------------------------------
//	Login page - description
	'clipperzServiceDescriptionConfig': [
			{tag:'h2', html:'Κρατήστε το για τον Εαυτό Σας'},
			{tag:'ul', children:[
				{tag:'li', children:[
					{tag:'h3', html:'Το Clipperz είναι:'},
					{tag:'ul', children:[
						{tag:'li', children:[{tag:'span', html:'Ένας ασφαλής και απλός τρόπος διαχείρησης όλων των κωδικών πρόσβασης σας'}]},
						{tag:'li', children:[{tag:'span', html:'Μια αποτελεσματική λύση πρόσβασης σε δεδομένα/εφαρμογές με μοναδικό κωδικό'}]},
						{tag:'li', children:[{tag:'span', html:'Μια ψηφιακή θυρίδα για τα απόρρητα δεδομένα σας'}]}
					]}
				]},
				{tag:'li', children:[
					{tag:'h3', html:'Με το Clipperz μπορείτε:'},
					{tag:'ul', children:[
						{tag:'li', children:[{tag:'span', html:'Να αποθηκεύσετε και να διαχειριστείτε όλους τους κωδικούς πρόσβασης και τα online πιστοποιητικά/διαπιστευτήρια σας'}]},
						{tag:'li', children:[{tag:'span', html:'Να έχετε πρόσβαση (login) στις υπηρεσίες διαδικτύου χωρίς την εισαγωγή oνομάτων λογαρισμών χρήστη (username),ή, κωδικών πρόσβασης (passwords)'}]},
						{tag:'li', children:[{tag:'span', html:'Να προστατεύσετε όλα τα προσωπικά δεδομένα σας: κωδικούς συναγερμών, PINs, αριθμούς πιστωτικών καρτών, ...'}]},
						{tag:'li', children:[{tag:'span', html:'Να μοιραστείτε δεδομένα με μέλη της οικογένεια σας και τους συνεργάτες σας (σύντομα στην διάθεση σας)'}]}
					]}
				]},
				{tag:'li', children:[
					{tag:'h3', html:'Τα πλεονεκτήματα του Clipperz είναι:'},
					{tag:'ul', children:[
						{tag:'li', children:[{tag:'span', html:'Είναι δωρεάν και προσφέρει πρόσβαση ανώνυμα'}]},
						{tag:'li', children:[{tag:'span', html:'Μπορεί να χρησιμοποιηθεί οποαδήποτε ώρα και από οποιοδήποτε τερματικό'}]},
						{tag:'li', children:[{tag:'span', html:'Δεν απαιτεί την φόρτωση και εγκατάσταση οποιουδήποτε λογισμικού'}]},
						{tag:'li', children:[{tag:'span', html:'Αποφεύγετε την διατήριση απορρήτων στον υπολογιστή σας ή σε έντυπη μορφή'}]}
					]}
				]},
				{tag:'li', children:[
					{tag:'h3', html:'Η ασφάλεια που παρέχει το Clipperz:'},
					{tag:'ul', children:[
						{tag:'li', children:[{tag:'span', html:'Τα απόρρητα δεδομένα σας κωδικοποιούνται τοπικά από τον διακομιστή σας (browser) πρίν να φορτωθούν στο Clipperz'}]},
						{tag:'li', children:[{tag:'span', html:'Το κλειδί της κωδικοποίησης είναι μία φράση-κωδικός γνωστή μόνο σε εσάς'}]},
						{tag:'li', children:[{tag:'span', html:'Το Clipperz φυλάσσει τα προσωπικά σας δεδομένα σε κωδικοποιημένη μορφή, και δεν μπορεί να έχει πρόσβαση σε αυτά στην αρχική τους μορφή'}]},
						{tag:'li', children:[{tag:'span', html:'Το Clipperz χρησιμοποιεί επίσημες /πρότυπες μεθόδους κωδικοποίησης, και όχι αόριστα και εφάνταστα μοντέλα'}]},
						{tag:'li', children:[{tag:'span', html:'Έχετε πρόσβαση στον πηγαίο κώδικα οποτεδήποτε το θελήσετε, και δεν χρειάζετε να γνωρίζετε τίποτα από κρυπτογράφηση για να είστε ένας ευχαριστημένος χρήστης!'}]}
					]}
				]},
				{tag:'li', children:[
					{tag:'a', href:"http://www.clipperz.com", target:'_blank', html:'Μάθετε περισσότερα'}
				]}
			]}
		],

// Login page - form	
	'loginFormTitle':							"Συνδεθείτε με τον Clipperz λογαριασμό σας",
	'loginFormUsernameLabel':					"Όνομα χρήστη",
	'loginFormPassphraseLabel':					"Κωδική φράση",
	'loginFormDontHaveAnAccountLabel':			"Δεν έχετε δημιουργήσει λογαριασμό?",
	'loginFormCreateOneLabel':					"Δημιουργήστε έναν",
	'loginFormForgotYourCredentialsLabel':		"Ξεχάσατε τα διαπιστευτήριά σας?",
	'loginFormAarghThatsBadLabel':				"Ααααργκ! Αυτό είναι κακό!",
	'loginFormAfraidOfMaliciousScriptsLabel':	"φοβάστε κακόβουλα προγράμματα (scripts)?",
	'loginFormVerifyTheCodeLabel':				"Επαληθεύστε τον κωδικό",
	'loginFormButtonLabel':						"Σύνδεση",

// Login page - language selection
	'loginPanelSwithLanguageDescriptionConfig':	[
		{tag:'h5', html:"Αλλάξτε στην γλώσσα προτήμησης σας"}
	],

// Login page - browser compatibility
	'browserCompatibilityDescriptionConfig':	[
		{tag:'p', html:"Έχετε μία καλύτερη και πιό ασφαλή Clipperz εμπειρία χρησιμοποιόντας τον Firefox. Ωστόσο το Clipperz συνεργάζετε άψογα με Opera και MS Internet Explorer!"}
	], 

// Login message panel
	'loginMessagePanelInitialTitle':			"Γίνεται σύνδεση ...",
	'loginMessagePanelInitialButtonLabel':		"Ακύρωση",
	'loginMessagePanelConnectedTitle':			"Συνδεθήκατε",
	'loginMessagePanelConnectedText':			"Ολοκληρώθηκε",
	'loginMessagePanelFailureTitle':			"Λάθος",
	'loginMessagePanelFailureText':				"Η σύνδεση χρήστη απέτυχε",
	'loginMessagePanelFailureButtonLabel':		"Κλείσιμο",

// Login message panel - connection
	'connectionLoginSendingCredentialsMessageTitle':		"Γίνεται επαλήθευση διαπιστευτηρίων",
	'connectionLoginSendingCredentialsMessageText':			"Αποστέλλονται διαπιστευτήρια",
	'connectionLoginCredentialsVerificationMessageTitle':	"Γίνεται επαλήθευση διαπιστευτηρίων",
	'connectionLoginCredentialsVerificationMessageText':	"Εκτέλεση πιστοποίησης SRP ",
	'connectionLoginDoneMessageTitle':						"Γίνεται επαλήθευση διαπιστευτηρίων",
	'connectionLoginDoneMessageText':						"Συνδεδεμένος",
	
//	Login message panel - user
	'userLoginPanelUpgradingUserCredentialsMessageTitle':	   				"Γίνεται επαλήθευση διαπιστευτηρίων",
	'userLoginPanelUpgradingUserCredentialsMessageText':	   				"Αναβάθμηση των διαπιστευτηρίων σας σε ένα νέο σζήμα πιστοποίησης",
	'userLoginPanelConnectedMessageTitle':					   				"Χρήστης πιστοποιήθηκε ",
	'userLoginPanelConnectedMessageText':					   				"Συνδεθήκατε με επιτυχία",
	'userLoginPanelTryingAnOlderConnectionSchemaMessageTitle': 				"Γίνεται επαλήθευση διαπιστευτηρίων",
	'userLoginPanelTryingAnOlderConnectionSchemaMessageText':  				"Trying an older authentication schema",
	'userLoginPanelLoadingUserDataMessageTitle':			   				"Χρήστης πιστοποιήθηκε ",
	'userLoginPanelLoadingUserDataMessageText':				   				"Downloading encrypted card headers from Clipperz",
	'userLoginPanelDecryptingUserDataMessageTitle':							"Χρήστης πιστοποιήθηκε ",
	'userLoginPanelDecryptingUserDataMessageText':							"Local decryption of card headers",
	'userLoginPanelDecryptingUserStatisticsMessageTitle':					"Χρήστης πιστοποιήθηκε ",
	'userLoginPanelDecryptingUserStatisticsMessageText':					"Local decryption of usage statistics",
	
//-----------------------------------------------------	
//	Registration page - splash alert
	'splashAlertTitle':	"Καλώς ήλθατε στο Clipperz!",
	'splashAlertTextConfig': [
			{tag:'p', html:'Μερικές συμβουλές ασφαλείας'},
			{tag:'ul', children:[
				{tag:'li', children:[{tag:'span', html:'Η αποθήκευση των δεδομένων σας στο Clipperz είναι τόσο ασφαλής, όσο η κωδική φράση που επιλέγετε για να τα προστατεύσετε. Κανένας δεν θα έχει πρόσβαση σε αυτά, εκτός αν γνωρίζει την κωδική φράση σας.'}]},
				{tag:'li', children:[{tag:'span', html:'Αν πρόκειται να χρησιμοποιήσετε το Clipperz για ασφαλή προστασία ευαίσθητων ή σημαντικών πληροφοριών, βεβαιωθείτε ότι θα χρησιμοποιήσετε μία “γερή” κωδική φράση. Όσο μεγαλύτερη, τόσο καλύτερη!'}]},
				{tag:'li', children:[{tag:'span', html:'Το Clipperz δεν θα έχει τη δυνατότητα να ανακτήσει μία χαμένη κωδική φράση!'}]}
			]},
			{tag:'p', html:'Για περισσότερες πληροφορίες, παρακαλώ ανατρέξτε στο <a href=\"http://www.clipperz.com\" target=\"_blank\">Clipperz</a>.'}
		],
	'splashAlertCloseButtonLabel':	"Εντάξει",
	
// Registration page - form	
	'registrationFormTitle':								"Δημιουργήστε λογαριασμό",
	'registrationFormUsernameLabel':						"Όνομα χρήστη",
	'registrationFormPassphraseLabel':						"Κωδική φράση",
	'registrationFormRetypePassphraseLabel':				"Εισάγετε ξανά την κωδική φράση",
	'registrationFormSafetyCheckLabel':						"Κατανοώ πως το Clipperz δεν θα μπορεί να ανακτήσει μία χαμένη κωδική φράση.",
	'registrationFormTermsOfServiceCheckLabel':				"Έχω διαβάσει και αποδέχομαι τους Όρους Χρήσης <a href='https://www.clipperz.com/terms_service' target='_blank'>Όρους Χρήσης</a>.",
	'registrationFormDoYouAlreadyHaveAnAccountLabel':		"Έχετε ήδη έναν λογαριασμό?",
	'registrationFormSimplyLoginLabel':						"απλώς συνδεθείτε",
	'registrationFormButtonLabel':							"Εγγραφείτε",

// Registration page - warning messages
	'registrationFormWarningMessageNotMatchingPassphrases':	"Οι κωδικές φράσεις που εισάγατε δεν ταιριάζουν. Παρακαλώ ξαναπροσπαθήστε.",
	'registrationFormWarningMessageSafetyCheckNotSelected':	"Παρακαλώ διαβάστε και επιλέξτε όλες τις παρακάτω επιλογές.",
	'registrationFormWarningMessageTermsOfServiceCheckNotSelected':	"Πρέπει να αποδεχθείτε τους Όρους Χρήσης.",

// Registration message panel	
	'registrationMessagePanelInitialTitle':					"Δημιουργία λογαριασμού ...",
	'registrationMessagePanelInitialButtonLabel':			"Ακύρωση",
	'registrationMessagePanelRegistrationDoneTitle':		"Εγγραφή",
	'registrationMessagePanelRegistrationDoneText':			"Ολοκληρώθηκε",
	'registrationMessagePanelFailureTitle':					"Η εγγραφή απέτυχε",
	'registrationMessagePanelFailureButtonLabel':			"Κλείσιμο",

// Registration - connection
	'connectionRegistrationSendingRequestMessageText':		"Γίνεται επαλήθευση διαπιστευτηρίων",
	'connectionRegistrationSendingCredentialsMessageText':	"Αποστέλλονται διαπιστευτήρια",

//-----------------------------------------------------	
// Registration splash panel
	'registrationSplashPanelTitle':	"Συμβουλές Ασφαλείας",
	'registrationSplashPanelDescriptionConfig': [					
			{tag:'p', html:'Αυτά είναι τα διαπιστευτήριά σας στο Clipperz, δείτε τα προσεκτικά. Το Clipperz δεν θα απεικονίσει το όνομα χρήστη και την κωδική σας φράση δεύτερη φορά!'}
		],
	'registrationSplashPanelUsernameLabel':		"όνομα χρήστη",	
	'registrationSplashPanelPassphraseLabel':	"κωδική φράση",
	
//-----------------------------------------------------	
//	Header links
	'donateHeaderLinkLabel':	"donate",
	'creditsHeaderLinkLabel':	"credits",
	'feedbackHeaderLinkLabel':	"feedback", 
	'helpHeaderLinkLabel':		"Βοήθεια", 
	'forumHeaderLinkLabel':		"forum",	
	
//-----------------------------------------------------	
//	Menu labels
	'recordMenuLabel':		"cards",
	'accountMenuLabel':		"Λογαριασμός",
	'dataMenuLabel':		"Δεδομένα",
	'contactsMenuLabel':	"Επαφές",
	'bookmarkletMenuLabel':	"bookmarklet",
	'logoutMenuLabel':		"Αποσύνδεση",
	'lockMenuLabel':		"lock",

//-----------------------------------------------------
//	Lock dialog
	'lockTitle':				"The account is locked",
	'lockDescriptionConfig':	[
		{tag:'p', html:'To unlock your account, please insert your passphrase'}
	],
	'unlockButtonLabel':		"Unlock",
		
//-----------------------------------------------------
//	Account panel - change passphrase
	'changePasswordTabLabel':	"Αλλάξτε την κωδική φράση σας",
	'changePasswordTabTitle':	"Αλλάξτε την κωδική φράση σας",

//	Account panel - change passphrase - form
	'changePasswordFormUsernameLabel':			"όνομα χρήστη",
	'changePasswordFormOldPassphraseLabel':		"παλαιά κωδική φράση",
	'changePasswordFormNewPassphraseLabel':		"νέα κωδική φράση",
	'changePasswordFormRetypePassphraseLabel':	"Εισάγετε ξανά τη νέα κωδική φράση",
	'changePasswordFormSafetyCheckboxLabel':	"Κατανοώ πως το Clipperz δεν θα μπορεί να ανακτήσει μία χαμένη κωδική φράση.",
	'changePasswordFormSubmitLabel':			"Αλλάξτε την κωδική φράση σας",

//	Account panel - change passphrase - warnings
	'changePasswordFormWrongUsernameWarning':			"Λάθος όνομα χρήστη",	
	'changePasswordFormWrongPassphraseWarning':			"Λάθος κωδική φράση",
	'changePasswordFormWrongRetypePassphraseWarning':	"Οι κωδικές φράσεις που εισάγατε δεν ταιριάζουν. Παρακαλώ ξαναπροσπαθήστε.",
	'changePasswordFormSafetyCheckWarning':				"Παρακαλώ διαβάστε και επιλέξτε όλες τις παρακάτω επιλογές.",

//	Account panel - change passphrase - progress dialog
	'changePasswordFormProgressDialogTitle':								"Γίνεται αλλαγή διαπιστευτηρίων χρήστη",
	'changePasswordFormProgressDialogConnectedMessageTitle':				"Συνδεδεμένος",
	'changePasswordFormProgressDialogConnectedMessageText':					"Ολοκληρώθηκε",
	'changePasswordFormProgressDialogErrorMessageTitle':					"Σφάλμα",
	'changePasswordFormProgressDialogErrorMessageText':						"Απέτυχε η αλλαγή διαπιστευτηρίων!",
	
	'changeCredentialsPanelEncryptingDataMessageTitle':		   				"Γίνεται αλλαγή της κωδικής φράσης σας",
	'changeCredentialsPanelEncryptingDataMessageText':						"Local encryption of card headers",
	'changeCredentialsPanelCreatingNewCredentialsMessageTitle':				"Γίνεται αλλαγή της κωδικής φράσης σας",
	'changeCredentialsPanelCreatingNewCredentialsMessageText':				"Γίνεται ανανέωση των διαπιστευτηρίων σας",
	'changeCredentialsPanelSendingNewCredentialsToTheServerMessageTitle':	"Γίνεται αλλαγή της κωδικής φράσης σας",
	'changeCredentialsPanelSendingNewCredentialsToTheServerMessageText':	"Uploading your encrypted credentials to Clipperz",
	'changeCredentialsPanelDoneMessageTitle':								"Γίνεται αλλαγή της κωδικής φράσης σας",
	'changeCredentialsPanelDoneMessageText':								"Ολοκληρώθηκε",
			
//-----------------------------------------------------
//	Account panel - manage OTP
	'manageOTPTabLabel':		"Manage your one-time passphrases",
	'manageOTPTabTitle':		"Manage your one-time passphrases",	

//	Account panel - manage OTP - description	
	'manageOTPTabDescriptionConfig':	[
		{tag:'p', html:"A one-time passphrase works like your regular passphrase, but can be used only once."},
		{tag:'p', html:"If the same passphrase is used again at a later stage in a login attempt it will be rejected and the login process will fail."},
		{tag:'p', html:"Immediately after a successful login, your one-time passphrase will be deleted preventing any fraudulent access."},
		{tag:'p', html:"One-time passwords are an excellent choice if one is concerned about keyloggers or spyware infections that may be collecting data from compromised machines."},
		{tag:'p', html:"<b>It's strongly advisable to use one-time passphrases when accessing Clipperz from public terminals, such as Internet cafes and libraries.</b>"},
		{tag:'p', html:""},
		{tag:'p', html:"<b>Coming soon ...</b>"}
	],

//-----------------------------------------------------
// Account panel - user preferences
	'accountPreferencesLabel':			"Προτιμήσεις",
	'accountPreferencesTabTitle':		"Προτιμήσεις",

// Account panel - user preferences - description
	'accountPreferencesLanguageTitle':	"Επιλογή Γλώσσας",
	'accountPreferencesLanguageDescriptionConfig': [
		{tag:'p', html:"Choose your preferred language from the list below."}
	],

	'accountPreferencesInterfaceTitle':	"Interface customization",
	'accountPreferencesInterfaceDescriptionConfig': [
		{tag:'p', html:"Tune the Clipperz interface to your needs."}
	],

// Account panel - user preferences - form
	'saveUserPreferencesFormSubmitLabel':						"Αποθήκευση",
	'cancelUserPreferencesFormSubmitLabel':						"Ακύρωση",

// Account panel - user preferences - panel
	'accountPreferencesSavingPanelTitle_Step1':		"Saving preferences",
	'accountPreferencesSavingPanelText_Step1':		"Local encryption of your preferences",
	'accountPreferencesSavingPanelTitle_Step2':		"Saving preferences",
	'accountPreferencesSavingPanelText_Step2':		"Sending encrypted preferences to Clipperz",
	
//-----------------------------------------------------
//	Account panel - delete account
	'deleteAccountTabLabel':	"Διαγράψτε τον λογαριασμό σας",	
	'deleteAccountTabTitle':	"Γίνεται διαγραφή του λογαριασμού σας",

//	Account panel - delete account - form
	'deleteAccountFormUsernameLabel': 		"όνομα χρήστη",
	'deleteAccountFormPassphraseLabel':		"κωδική φράση",
	'deleteAccountFormSafetyCheckboxLabel':	"Κατανοώ πως όλα τα δεδομένα μου θα διαγραφούν και πως αυτή η πράξη είναι μη αναστρέψιμη.",
	'deleteAccountFormSubmitLabel':			"Διαγράψτε τον λογαριασμό μου",

//	Account panel - delete account - warnings
	'deleteAccountFormWrongUsernameWarning':	"λάθος όνομα χρήστη",
	'deleteAccountFormWrongPassphraseWarning':	"λάθος κωδική φράση",
	'deleteAccountFormSafetyCheckWarning':		"Παρακαλώ διαβάστε και επιλέξτε την παρακάτω επιλογή.",

//	Account panel - delete account - confirmation
	'accountPanelDeletingAccountPanelConfirmationTitle':	"ΠΡΟΣΟΧΗ",
	'accountPanelDeleteAccountPanelConfirmationText':		"Είστε σίγουρος/η ότι θέλετε να διαγράψετε αυτόν τον λογαριασμό?",
	'accountPanelDeleteAccountPanelConfirmButtonLabel':		"Ναι",
	'accountPanelDeleteAccountPanelDenyButtonLabel':		"Όχι",

//-----------------------------------------------------
//	Data panel - offline copy
	'offlineCopyTabLabel':	"Offline copy",
	'offlineCopyTabTitle':	"Offline copy",
	
// 	Data panel - offline copy - description
	'offlineCopyTabDescriptionConfig': [
		{tag:'p', html:"With just one click you can dump all your encrypted data from Clipperz servers to your hard disk and create a read-only offline version of Clipperz to be used when you are not connected to the Internet."},
		{tag:'p', html:"The read-only version is as secure as the read-and-write one and will not expose your data to higher risks since they both share the same code and security architecture."},
		{tag:'ol', children:[
			{tag:'li', children:[{tag:'span', html:"Click the link below to download the offline copy."}]},
			{tag:'li', children:[{tag:'span', html:"The browser will ask you what to do with the “Clipperz_YYYYMMDD.zip” file. Save it on your hard disk."}]},
			{tag:'li', children:[{tag:'span', html:"Unzip the file to reveal the “Clipperz_YYYYMMDD” folder."}]},
			{tag:'li', children:[{tag:'span', html:"Open the “Clipperz_YYYYMMDD” folder and double click on the “index.html” file."}]},
			{tag:'li', children:[{tag:'span', html:"Enter the usual username and passphrase and access your private data without an Internet connection."}]}
		]}
	],
	'offlineCopyDownloadLinkLabel':	"Download",

//-----------------------------------------------------
//	Data panel - sharing
	'sharingTabLabel':	"Sharing",
	'sharingTabTitle':	"Sharing",

//	Data panel - sharing - description
	'sharingTabDescriptionConfig':	[
		{tag:'p', html:"Quite often a confidential piece of information needs to be shared with one or more persons."},
		{tag:'p', html:"This could be as simple as giving your colleague the access code of your voice mailbox when you are out of the office, or as complicated as enabling the entitled heirs to access your safe deposit box at the local bank when you pass on."},
		{tag:'p', html:"Clipperz can make sharing your secrets a secure and straightforward process."},
		{tag:'p', html:""},
		{tag:'p', html:"<b>Coming soon ...</b>"}
	],
	
//-----------------------------------------------------
// 	Data panel - import
	'importTabLabel':	"Εισαγωγή",
	'importTabTitle':	"Εισαγωγή",

// 	Data panel - import - description
	'importTabDescriptionConfig':	[
		{tag:'p', html:"<b>Σύντομα κοντά σας ...</b>"}
	],

//-----------------------------------------------------
//	Data panel - export
	'printingTabLabel':	"Εξαγωγή",
	'printingTabTitle':	"Εξαγωγή",

//	Data panel - export - description “”
		'printingTabDescriptionConfig':	[
		{tag:'p', html:"<b>Print your data</b>"},
		{tag:'p', html:"Clicking on the link below will open a new window displaying all your cards in a printable format."},
		{tag:'p', html:"If you are going to print for backup purposes, please consider the more safe option provided by the creating an “offline copy”."}
	],
	'printingLinkLabel':	"Έκδοση Εκτύπωσης",
	
//-------------------------------------------------------------------
//	Contacts panel
	'contactsTabLabel':	"Contacts",
	'contactsTabTitle':	"Contacts",

//-------------------------------------------------------------------
//	Bookmarklet panel
	'bookmarkletTabLabel':				"Bookmarklet",
	'bookmarkletTabTitle':				"Bookmarklet",

//	Bookmarklet panel - description
	'bookmarkletTabDescriptionConfig':	[
		{tag:'p', html:"A bookmarklet is a simple “one-click” tool that can perform very useful tasks. It can be saved and used like a normal web page bookmark."},
		{tag:'p', html:"The Clipperz bookmarklet will help you to quickly create new cards and new “direct logins” within existing cards."}, 
		{tag:'p', html:"<b>Please note that the bookmarklet does not include any information related to your account (e.g. your username or passphrase), the bookmarklet is a general tool containing the same code for every Clipperz user.</b>"},
		{tag:'div', children:[
			{tag:'p', html:"To install the bookmarklet <b>drag</b> the link below to the bookmark bar of your browser."}
		]}
	],
	'bookmarkletTabBookmarkletTitle':	"Προσθήκη στο Clipperz",

//	Bookmarklet panel - instructions
	'bookmarkletTabInstructionsConfig':	[
		{tag:'h3', html:"How to create a new card inclusive of a “direct login” link to an online service"},
		{tag:'ol', children:[
			{tag:'li', children:[{tag:'span', html:"Open the web page where the login form is hosted. (this is the page where you usually enter your sign-in credentials)"}]},
			{tag:'li', children:[{tag:'span', html:"Launch the bookmarklet by clicking on it: a pop-up window will appear over the web page."}]},
			{tag:'li', children:[{tag:'span', html:"Copy to the clipboard the content of the large text area within the pop-up. (ctrl-C)"}]},
			{tag:'li', children:[{tag:'span', html:"Enter your Clipperz account and click on the <b>Add new card</b> button."}]},
			{tag:'li', children:[{tag:'span', html:"Select the “Direct login” template and paste the content of the clipboard to the large text area in the form. (ctrl-V)"}]},
			{tag:'li', children:[{tag:'span', html:"Press the <b>Create</b> button, complete and review the details, then click <b>Save</b>."}]}
		]},
		{tag:'h3', html:"How to add a “direct login” link to an existing card"},
		{tag:'ol', children:[
			{tag:'li', children:[{tag:'span', html:"Same as above."}]},
			{tag:'li', children:[{tag:'span', html:"Same as above."}]},
			{tag:'li', children:[{tag:'span', html:"Same as above."}]},
			{tag:'li', children:[{tag:'span', html:"Enter your Clipperz account and select the card containing the credentials for the web service you just visited and click the <b>Edit</b> button."}]},
			{tag:'li', children:[{tag:'span', html:"Paste the content of the clipboard to the large text area in the “Direct logins” section. (ctrl-V)"}]},
			{tag:'li', children:[{tag:'span', html:"Press the <b>Add direct login</b> button, review the details and then click <b>Save</b>."}]}
		]},
		{tag:'p', html:""},
		{tag:'p', html:"Further information about the bookmarklet are <a href=\"http://www.clipperz.com/support/user_guide/bookmarklet\" target=\"_blank\">available here</a>."}
	],

//-------------------------------------------------------------------
// Direct logins block
	'mainPanelDirectLoginBlockLabel':		"Απευθείας σύνδεση",
	'directLinkReferenceShowButtonLabel':	"Επίδειξη",
	
// Direct logins - blank slate	“”
	'mainPanelDirectLoginBlockDescriptionConfig':	[	
		{tag:'p', html:"Add “direct logins” to sign in to your web accounts without typing usernames and passwords!"},
		{tag:'p', html:"“Direct logins” greatly enhance your password security since you can:"},
		{tag:'ul', children:[
			{tag:'li', children:[{tag:'span', html:"conveniently adopt and enter complex passwords;"}]},
			{tag:'li', children:[{tag:'span', html:"never re-use the same and easy-to-guess password."}]}
		]},
		{tag:'p', html:"Simple and quick configuration with the Clipperz <b>bookmarklet</b>."},
		{tag:'a', href:"http://www.clipperz.com/support/user_guide/direct_logins", target:'_blank', html:'Learn more about “direct logins”'}		
	],

//-------------------------------------------------------------------
// Cards block		
	'mainPanelRecordsBlockLabel':		"Κάρτες",
	'mainPanelAddRecordButtonLabel':	"Προσθήκη νέας Κάρτας ",		
	'mainPanelRemoveRecordButtonLabel':	"Διαγραφή κάρτας",		

// Cards block - filter tabs
	'mainPanelRecordFilterBlockAllLabel':		"Όλα",
	'mainPanelRecordFilterBlockTagsLabel':		"Επιλογές",
	'mainPanelRecordFilterBlockSearchLabel':	"Αναζήτηση",
	
// Cards block - blank slate
	'recordDetailNoRecordAtAllTitle':				"Welcome to Clipperz!",
	'recordDetailNoRecordAtAllDescriptionConfig':	[
			{tag:'h5', html:'Get started by adding cards to your account.'},
			{tag:'p', html:'Cards are simple and flexible forms where you can store your passwords and any other confidential data.'},
			{tag:'p', html:'Cards could contain credentials for accessing a web site, the combination of your bicycle lock, details of your credit card, ...'},
			{tag:'h5', html:'Don\'t forget the bookmarklet!'},
			{tag:'p', html:'Before you start, install the “Add to Clipperz” bookmarklet: it will make creating cards easier and more fun.'},
			{tag:'p', html:'Go to the bookmarklet tab to discover how to install it and how it use it.'},
			{tag:'p', html:''},
			{tag:'p', html:'Then simply click the <b>"Add new card"</b> button and enjoy your Clipperz account.'},
			{tag:'p', html:''},
			{tag:'a', href:"http://www.clipperz.com/support/user_guide/managing_cards", target:'_blank', html:'Learn more about creating and managing cards'}
		],

// Cards block - new card wizard - bookmarklet configuration
	'newRecordWizardTitleBoxConfig': [
		{tag:'h5', html:"Please select a template"},
		{tag:'p', html:'Cards are simple and flexible forms where you can store passwords or any other confidential data.'}, 
		{tag:'p', html:'Start choosing one of the template below. You can always customize your cards later by adding or removing fields.'}
	],
	
	'newRecordWizardBookmarkletConfigurationTitle':						"Απευθείας σύνδεση",
	'newRecordWizardBookmarkletConfigurationDescriptionConfig':			[
		{tag:'p', html:"Paste below the configuration code generated by the Clipperz bookmarklet."},
		{tag:'p', html:"A new card complete with a direct login to your web account will be created."}
	],

	'newRecordWizardCreateButtonLabel':	"Δημιουργία",
	'newRecordWizardCancelButtonLabel':	"Ακύρωση",

//-------------------------------------------------------------------
// Card templates
//-------------------------------------------------------------------

	'recordTemplates': {

//	Web password
		'WebAccount': {
			'title': "Web password",
			'description': [
				{tag:'p', html:"A simple card to store login credentials for your online services."}
			],
			'fields': [
				{label:"Διεύθυνση δικτύου", type:'URL'},
				{label:"Χρήστης ή διεύθυνση ηλεκτρονικού ταχυδρομείου", type:'TXT'},
				{label:"Κωδικός Πράσβασης", type:'PWD'}
			]
		},

//	Bank account	
		'BankAccount': {
			'title': "Bank account",
			'description': [
				{tag:'p', html:"Safely store your bank account number and online banking credentials."}
			],
			'fields': [
				{label:"Τράπεζα", type:'TXT'},
				{label:"Αριθμός λογαριασμού", type:'TXT'},
				{label:"Ιστοσελίδα τράπεζας", type:'URL'},
				{label:"Αρ. Ηλεκτρονικής τράπεζας (ID)", type:'TXT'},
				{label:"Κώδικος Ηλεκτρονικής τράπεζας", type:'PWD'}
			]
		},

// 	Credit card
		'CreditCard': {
			'title': "Credit card",
			'description': [
				{tag:'p', html:"Card number, expire date, CVV2 and PIN always at hand with Clipperz."}
			],
			'fields': [
				{label:"Τύπος Κάρτας (Visa, AmEx,...)", type:'TXT'},
				{label:"Αριθμός κάρτα", type:'TXT'},
				{label:"Ονοματεπώνυμο κατόχου", type:'TXT'},
				{label:"Ημερομηνία λήξης", type:'TXT'},
				{label:"CVV2", type:'TXT'},
				{label:"Κωδικός Αυτόματης ταμείακης μηχανης (ΑΤΜ)", type:'PWD'},
				{label:"Ιστοσελίδα κάρτας", type:'URL'},
				{label:"Χρήστης", type:'TXT'},
				{label:"Κωδικός Πρόσβασης", type:'PWD'}
			]
		},

// 	Address book entry	
		'AddressBookEntry': {
			'title': "Address book entry",
			'description': [
				{tag:'p', html:"Clipperz could also work as your new private address book. Use this template to easily add a new entry."}
			],
			'fields': [
				{label:"Όνομα", type:'TXT'},
				{label:"Ηλετρονικό ταχυδρομείο", type:'TXT'},
				{label:"Τηλέφωνο", type:'TXT'},
				{label:"Κινητο τηλέφωνο", type:'TXT'},			
				{label:"Διεύθυνση", type:'ADDR'},
			]
		},

//	Custom card
		'Custom': {
			'title': "Custom card",
			'description': [
				{tag:'p', html:"No matter which kind of confidential data you need to protect, create a custom card to match your needs."}
			],
			'fields': [
				{label:"Περιγραφή 1", type:'TXT'},
				{label:"Περιγραφή 2", type:'TXT'},
				{label:"Περιγραφή 3", type:'TXT'}
			]
		}
	},


	'recordFieldTypologies': {
		'TXT': {
			description: 'simple text field',
			shortDescription: '΄Κείμενο'
		},
		'PWD': {
			description: 'simple text field, with default status set to hidden',
			shortDescription: 'Κωδικός Πρόσβασης'
		},
		'URL': {
			description: 'simple text field in edit mode, that became an active url in view mode',
			shortDescription: 'Διεύθυνση ηλεκτρονικού ταχυδρομείου'
		},
		'DATE': {
			description: 'a value set with a calendar helper',
			shortDescription: 'Ημερομηνία'
		},
		'ADDR': {
			description: 'just like the URL, but the active link points to Google Maps (or similar service) passing the address value as argument',
			shortDescription: 'Διεύθυνση'
		},
		'CHECK': {
			description: 'check description',
			shortDescription: 'check'
		},
		'RADIO': {
			description: 'radio description',
			shortDescription: 'radio'
		},
		'SELECT': {
			description: 'select description',
			shortDescription: 'select'
		}
	},

// Cards block - new card - warnings
	'newRecordPanelGeneralExceptionTitle':						"Σφάλμα",
	'newRecordPanelGeneralExceptionMessage':					"The configuration text is not valid. Make sure to get your text from the bookmarklet pop-up and retry.",
	'newRecordPanelWrongBookmarkletVersionExceptionTitle':		"Σφάλμα",
	'newRecordPanelWrongBookmarkletVersionExceptionMessage':	"The configuration text has been generated by an old version of the bookmarklet. Please update your bookmarklet and retry.",
	'newRecordPanelExceptionPanelCloseButtonLabel':				"Ακύρωση",

// Cards block - delete card
	'mainPanelDeletingRecordPanelConfirmationTitle':	"Διαγραφή επιλεγμένης κάρτας",
	'mainPanelDeleteRecordPanelConfirmationText':		"Είστε σίγουρος ότι θέλετε να διαγράψετε την επιλεγμένη κάρτα?",
	'mainPanelDeleteRecordPanelConfirmButtonLabel':		"Ναι",
	'mainPanelDeleteRecordPanelDenyButtonLabel':		"Όχι",
	'mainPanelDeletingRecordPanelInitialTitle':			"Διαγραφή επιλεγμένης κάρτας ",
	'mainPanelDeletingRecordPanelCompletedText':		"Ολοκλήρωση",

// Cards block - delete card panel
	'deleteRecordPanelCollectRecordDataMessageTitle':		   				"Διαγραφή κάρτας",
	'deleteRecordPanelCollectRecordDataMessageText':		   				"Φόρτωση λίστα κάρτας",
	'deleteRecordPanelEncryptUserDataMessageTitle':			   				"Διαγραφή κάρτας",
	'deleteRecordPanelEncryptUserDataMessageText':			   				"Local encryption of card headers",
	'deleteRecordPanelSendingDataToTheServerMessageTitle':	   				"Διαγραφή κάρτας",
	'deleteRecordPanelSendingDataToTheServerMessageText':	   				"Uploading encrypted card headers to Clipperz",
	'deleteRecordPanelUpdatingTheInterfaceMessageTitle':	   				"Διαγραφή κάρτας",
	'deleteRecordPanelUpdatingTheInterfaceMessageText':		   				"Φόρτωση επιφάνειας",
	
// Cards block - no record selected
	'recordDetailNoRecordSelectedTitle':			"Δεν έχει επιλεγεί κάποια κάρτα",
	'recordDetailNoRecordSelectedDescriptionConfig':	[
			{tag:'p', html:'Παρακαλώ επιλέξτε μια κάρτα από αυτές που βρίσκονται στα αριστερά σας'}
		],

// Cards block - loading messages	
	'recordDetailLoadingRecordMessage':				"Downloading encrypted card from Clipperz",
	'recordDetailDecryptingRecordMessage':			"Τοπικη αποκωδικοποίηση αρχείων κάρτας",
	'recordDetailLoadingRecordVersionMessage':		"Φόρτωση τελευταίας έκδοσης κάρτας",
	'recordDetailDecryptingRecordVersionMessage':	"Τοπική αποκωδικοποίηση της τελευταίας έκδοσης",
	'recordDetailLoadingErrorMessageTitle':			"Σφάλμα στη φόρτωση της κάρτας",
	
// Cards block - card details
	'recordDetailNotesLabel':						"Σημειώσης",
	'recordDetailLabelFieldColumnLabel':			"Περιγραφή πεδίου",
	'recordDetailDataFieldColumnLabel':				"Στοιχεία πεδίου",
	'recordDetailTypeFieldColumnLabel':				"Τύπος",

	'recordDetailSavingChangesMessagePanelInitialTitle':	"Αποθήκευση κάρτας",

	'recordDetailAddFieldButtonLabel':			"Προσθέστε νέο πεδίο",
	'recordDetailPasswordFieldHelpLabel':		"Για αντιγραφή του κωδικού στο clipboard επιλέξτε τα αστεράκια και μετα Ctrl-C",

	'recordDetailDirectLoginBlockTitle':		"Κωδικός Πρόσβασης",
	'recordDetailNewDirectLoginDescriptionConfig':	[					
		{tag:'p', html:'Επικύρωση κωδικου πρόσβασης'}
	],

	'recordDetailDirectLoginBlockNoDirectLoginConfiguredDescriptionConfig':	[
		{tag:'p', html:"Does this card contain credentials to access an online service?"},
		{tag:'p', html:"Use the bookmarklet to configure a “direct login” from Clipperz with just one click!"}
	],
	'recordDetailAddNewDirectLoginButtonLabel':					"Προσθέστε νέο κωδικό πρόσβασης",

	'recordDetailEditButtonLabel':		"Edit",
	'recordDetailSaveButtonLabel':		"Αποθήκευση",
	'recordDetailCancelButtonLabel':	"Ακύρωση",

	'newRecordTitleLabel':					"_Νέα κάρτα_",

// Cards block - save card panel
	'recordSaveChangesPanelCollectRecordInfoMessageTitle':			"Αποθήκευση κάρτας",
	'recordSaveChangesPanelCollectRecordInfoMessageText':			"Updating card headers",
	'recordSaveChangesPanelEncryptUserDataMessageTitle':			"Αποθήκευση κάρτας",
	'recordSaveChangesPanelEncryptUserDataMessageText':				"Local encryption of card headers",
	'recordSaveChangesPanelEncryptRecordDataMessageTitle':			"Αποθήκευση κάρτας",
	'recordSaveChangesPanelEncryptRecordDataMessageText':			"Local encryption of card's data",
	'recordSaveChangesPanelEncryptRecordVersionDataMessageTitle':	"Αποθήκευση κάρτας",
	'recordSaveChangesPanelEncryptRecordVersionDataMessageText':	"Local encryption of card's version data",
	'recordSaveChangesPanelSendingDataToTheServerMessageTitle':		"Αποθήκευση κάρτας",
	'recordSaveChangesPanelSendingDataToTheServerMessageText':		"Uploading encrypted card's header to Clipperz",
	'recordSaveChangesPanelUpdatingTheInterfaceMessageTitle':		"Αποθήκευση κάρτας",
	'recordSaveChangesPanelUpdatingTheInterfaceMessageText':		"Φόρτωση επιφάνειας",

// Exit page	
	'exitConfig': [
			{tag:'h2', html:'<b>Goodbye! Thanks for using Clipperz.</b>'},
			
			{tag:'ul', children:[
				{tag:'li', children:[
					{tag:'h3', html:'Remember:'},
					{tag:'ul', children:[
						{tag:'li', children:[{tag:'span', html:'Bookmark this page to safely connect to Clipperz in the future (if you haven\'t already done it)'}]},
						{tag:'li', children:[{tag:'span', html:'Clipperz will never send you an email, because we never asked your email address (and we never will), so never open an email that says it\'s from Clipperz'}]}
					]}
				]}
			]},
			{tag:'p', html:""},
			{tag:'p', html:"In 10 seconds you will be redirected to a Wikipedia page where you can read about a major security issue ..."}
		],

//-------------------------------------------------------------------
//	Miscellaneous strings	
//-------------------------------------------------------------------	

//	'DWRUtilLoadingMessage':						"Φόρτωση δεδομένων ...",
	'comingSoon':									"Σύντομα κοντά σας ...",
	'panelCollectingEntryopyMessageText':			"Collecting entropy",
	'directLoginConfigurationCheckBoxFieldSelectedValue':		"Ναι",
	'directLoginConfigurationCheckBoxFieldNotSelectedValue':	"Όχι",
										
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});

