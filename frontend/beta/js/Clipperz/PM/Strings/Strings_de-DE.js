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

Clipperz.PM.Strings.Languages['de-DE'.toLowerCase()] = MochiKit.Base.merge(Clipperz.PM.Strings.Languages['en-us'], {
'clipperzServiceDescription':	"<h2>Privatsphäre für Deine Daten</h2> <ul> <li> <h3>Clipperz heißt:</h3> <ul> <li> <p>sichere und einfache zu bedienene Passwortverwaltung</p> </li> <li> <p>eine effektive Lösung des einmaligen Anmeldens</p> </li> <li> <p>eine digitale Brieftasche für Deine vertraulichen Daten</p> </li> </ul> </li> <li> <h3>Clipperz bietet Dir:</h3> <ul> <li> <p>einfaches Speichern und Verwalten von Passwörtern und Webzugangsdaten</p> </li> <li> <p>schnelles unkompliziertes einloggen, ohne Eingabe des Benutzernamen und Passworts, bei Webdiensten</p> </li> <li> <p>Schutz aller Deiner persönlichen Daten: Zutrittscodes, PINs, Benutzernamen, Passwörter, Kreditkartennummern, &hellip;</p> </li> <li> <p>Deine Geheimnisse mit Familienmitgliedern und Freunden zu teilen (mehr dazu in Kürze)</p> </li> </ul> </li> <li> <h3>Clipperz ist:</h3> <ul> <li> <p>kostenlos und absolut anonym</p> </li> <li> <p>unkomplizierter Zugriff zu jeder Zeit von jedem Rechner</p> </li> <li> <p>ohne Download und Installation verwendbar</p> </li> <li> <p>ein Schutz gegen das Speichern von Passwörtern auf Deinem PC oder das Notieren auf Papier</p> </li> </ul> </li> <li> <h3>Clipperz Sicherheit:</h3> <ul> <li> <p>Deine sensiblen persönlichen Informationen werden lokal durch Deinen Browser verschlüsselt, bevor sie an Clipperz über das Internet gesendet werden</p> </li> <li> <p>Der Schlüssel für diese Daten ist der Sicherheitssatz, den nur Du kennst</p> </li> <li> <p>Clipperz speichert Deine sensiblen Daten nur in verschlüsselter Form und kann zu keinem Zeitpunkt diese entschlüssel und in ihrer ursprünglichen Klartextform zugänglich machen</p> </li> <li> <p>Clipperz basiert auf standart Verschlüsselungsverfahren. Nichts ausergewöhnliches – oder hand gestricktes</p> </li> <li> <p>Du kannst den Quellcode zu jeder Zeit anschauen, aber Du brauchst nichts von Kryptographie zu verstehen um ein glücklicher Anwender zu sein!</p> </li> </ul> </li> <li> <a href=\"http://www.clipperz.com\" target=\"_blank\">Weitere Informationen</a> </li> </ul> ",
'loginFormTitle':	"Login zu Deinem Clipperz Konto",
'loginFormUsernameLabel':	"Benutzernamen",
'loginFormPassphraseLabel':	"Sicherheitssatz",
'loginFormDontHaveAnAccountLabel':	"Du hast noch kein Clipperz Konto?",
'loginFormCreateOneLabel':	"Konto erstellen",
'loginFormForgotYourCredentialsLabel':	"Zugangsdaten vergessen?",
'loginFormAarghThatsBadLabel':	"Misst! Dass ist schlecht!",
'loginFormAfraidOfMaliciousScriptsLabel':	"verängstigt von bösartigen Scripts?",
'loginFormVerifyTheCodeLabel':	"begutachte den Quellcode",
'loginFormButtonLabel':	"Einloggen",
'loginPanelSwithLanguageDescription':	"<h5>Wechsel zu Deiner vervorzugten Sprache</h5> ",
'browserCompatibilityDescription':	"<p>Bessere und sicherere Clipperz-Erfahrung mit Firefox. Clipperz funktioniert auch mit Safari, Opera und MS Internet Explorer!</p> ",
'loginMessagePanelInitialTitle':	"Du wirst eingeloggt…",
'loginMessagePanelInitialButtonLabel':	"Abbruch",
'loginMessagePanelConnectedTitle':	"Verbunden",
'loginMessagePanelConnectedText':	"Fertig",
'loginMessagePanelFailureTitle':	"Fehler",
'loginMessagePanelFailureText':	"Login fehlgeschlagen",
'loginMessagePanelFailureButtonLabel':	"Schließen",
'connectionLoginSendingCredentialsMessageTitle':	"Prüfe Zugangsdaten",
'connectionLoginSendingCredentialsMessageText':	"Sende Zugangsdaten",
'connectionLoginCredentialsVerificationMessageTitle':	"Prüfe Zugangsdaten",
'connectionLoginCredentialsVerificationMessageText':	"Führe SRP Authentifizierung durch",
'connectionLoginDoneMessageTitle':	"Prüfe Zugangsdaten",
'connectionLoginDoneMessageText':	"Verbunden",
'userLoginPanelUpgradingUserCredentialsMessageTitle':	"Prüfe Zugangsdaten",
'userLoginPanelUpgradingUserCredentialsMessageText':	"Aktualisierung Deiner Zugangsdaten auf ein neues Authentifizierungsschema",
'userLoginPanelConnectedMessageTitle':	"Benutzer authentifiziert",
'userLoginPanelConnectedMessageText':	"Login erfolgreich",
'userLoginPanelTryingAnOlderConnectionSchemaMessageTitle':	"Prüfe Zugangsdaten",
'userLoginPanelTryingAnOlderConnectionSchemaMessageText':	"Probiere älteres Authentifizierungsschema",
'userLoginPanelLoadingUserDataMessageTitle':	"Benutzer authentifiziert",
'userLoginPanelLoadingUserDataMessageText':	"Lade verschlüsselte Kartendaten von Clipperz",
'userLoginPanelDecryptingUserDataMessageTitle':	"Benutzer authentifiziert",
'userLoginPanelDecryptingUserDataMessageText':	"Lokale Entschlüsselung der Kartendaten",
'userLoginPanelDecryptingUserStatisticsMessageTitle':	"Benutzer authentifiziert",
'userLoginPanelDecryptingUserStatisticsMessageText':	"Lokale Entschlüsselung der Benutzerstatisik",
'splashAlertTitle':	"Willkommen bei Clipperz!",
'splashAlertText':	"<p>Sicherheitshinweis</p> <ul> <li> <p>Die Speicherung von Informationen bei Clipperz ist so sicher, wie der Sicherheitssatz den Du zum Schutz gewählt hast. Ist der Sicherheitssatz nicht bekannt, können keine Informationen abgefragt werden.</p> </li> <li> <p>Solltest Du Clipperz nutzen, um sensible und kritische persönliche Daten abzuspeichern, so empfehlen wir in jedem Fall die Nutzung eines langen Sicherheitssatzes als Passwort und die Nutzung von Sonderzeichen, Zahlen, Groß- und Kleinbuchstaben.</p> </li> <li> <p>Clipperz kann einen verlorenen Sicherheitssatz nicht wiederherstellen!</p> </li> </ul> <p>Weitere Informationen findest Du bei <a href=\"http://www.clipperz.com\" target=\"_blank\">Clipperz</a>.</p> ",
'splashAlertCloseButtonLabel':	"Ok",
'registrationFormTitle':	"Erstelle Dein Konto",
'registrationFormUsernameLabel':	"Benutzernamen",
'registrationFormPassphraseLabel':	"Sicherheitssatz",
'registrationFormRetypePassphraseLabel':	"Wiederhole Sicherheitssatz",
'registrationFormSafetyCheckLabel':	"Ich akzeptiere dass es Clipperz nicht möglich ist, einen verlorenen Sicherheitssatz wiederherzustellen.",
'registrationFormTermsOfServiceCheckLabel':	"Ich habe die <a href='http://www.clipperz.com/terms_of_service' target='_blank'>Nutzungsbedingungen</a> gelesen, verstanden und akzeptiere diese.",
'registrationFormDoYouAlreadyHaveAnAccountLabel':	"Hast Du bereits einen Zugang?",
'registrationFormSimplyLoginLabel':	"Einloggen",
'registrationFormButtonLabel':	"Anmelden",
'registrationFormWarningMessageNotMatchingPassphrases':	"Deine Sicherheitssätze stimmen nicht überein. Bitte erneut eingeben.",
'registrationFormWarningMessageSafetyCheckNotSelected':	"Bitte lese die Bedingungen und akzeptiere die Auswahlboxen weiter unten.",
'registrationFormWarningMessageTermsOfServiceCheckNotSelected':	"Du musst die Nutzungsbedingungen akzeptieren.",
'registrationMessagePanelInitialTitle':	"Benutzer wird angelegt…",
'registrationMessagePanelInitialButtonLabel':	"Abbruch",
'registrationMessagePanelRegistrationDoneTitle':	"Anmeldung",
'registrationMessagePanelRegistrationDoneText':	"Fertig",
'registrationMessagePanelFailureTitle':	"Anmerldung fehlgeschlagen",
'registrationMessagePanelFailureButtonLabel':	"Schließen",
'connectionRegistrationSendingRequestMessageText':	"Zugangsdaten werden geprüft",
'connectionRegistrationSendingCredentialsMessageText':	"Sende Zugangsdaten",
'registrationSplashPanelTitle':	"Sicherheitshinweis",
'registrationSplashPanelDescription':	"<p>Dies sind Deine Clipperz Zugangsdaten, pass sehr gut auf sie auf. Clipperz wird diese kein zweites und weiteres mal anzeigen!</p> ",
'registrationSplashPanelUsernameLabel':	"Benutzernamen",
'registrationSplashPanelPassphraseLabel':	"Schlüsselsatz",
'donateHeaderLinkLabel':	"spende",
'creditsHeaderLinkLabel':	"credits",
'feedbackHeaderLinkLabel':	"feedback",
'helpHeaderLinkLabel':	"hilfe",
'forumHeaderLinkLabel':	"forum",
'recordMenuLabel':	"Karten",
'accountMenuLabel':	"Benutzer",
'dataMenuLabel':	"Daten",
'contactsMenuLabel':	"Kontakt",
'bookmarkletMenuLabel':	"Bookmarklet",
'logoutMenuLabel':	"Ausloggen",
'lockMenuLabel':	"Sperren",
'lockTitle':	"Dieses Konto ist gesperrt",
'lockDescription':	"<p>Bitte gebe Deinen Sicherheitssatz ein, um das Clipperz-Konto zu entsperren.</p> ",
'unlockButtonLabel':	"Entsperren",
'changePasswordTabLabel':	"Sicherheitssatz ändern",
'changePasswordTabTitle':	"Sicherheitssatz ändern",
'changePasswordFormUsernameLabel':	"Benutzername",
'changePasswordFormOldPassphraseLabel':	"Alter Sicherheitssatz",
'changePasswordFormNewPassphraseLabel':	"Neuer Sicherheitssatz",
'changePasswordFormRetypePassphraseLabel':	"Wiederholdung neuen Sicherheitssatz",
'changePasswordFormSafetyCheckboxLabel':	"Ich akzeptiere dass es Clipperz nicht möglich ist, einen verlorenen Sicherheitssatz wiederherzustellen.",
'changePasswordFormSubmitLabel':	"Sicherheitssatz ändern",
'changePasswordFormWrongUsernameWarning':	"Falscher Benutzername",
'changePasswordFormWrongPassphraseWarning':	"Falscher Sicherheitssatz",
'changePasswordFormWrongRetypePassphraseWarning':	"Deine Sicherheitssätze stimmen nicht überein. Bitte erneut eingeben.",
'changePasswordFormSafetyCheckWarning':	"Bitte ließ die folgenden Hinweise und akzeptiere diese.",
'changePasswordFormProgressDialogTitle':	"Ändere Zugangsdaten",
'changePasswordFormProgressDialogConnectedMessageTitle':	"Verbunden",
'changePasswordFormProgressDialogConnectedMessageText':	"Fertig",
'changePasswordFormProgressDialogErrorMessageTitle':	"Fehler",
'changePasswordFormProgressDialogErrorMessageText':	"Ändern der Zugangsdaten fehlgeschlagen!",
'changeCredentialsPanelEncryptingDataMessageTitle':	"Ändere Sicherheitssatz",
'changeCredentialsPanelEncryptingDataMessageText':	"Lokale Verschlüsselung der Kartendaten",
'changeCredentialsPanelCreatingNewCredentialsMessageTitle':	"Ändere Sicherheitssatz",
'changeCredentialsPanelCreatingNewCredentialsMessageText':	"Aktualisiere Zugangsdaten",
'changeCredentialsPanelSendingNewCredentialsToTheServerMessageTitle':	"Ändere Sicherheitssatz",
'changeCredentialsPanelSendingNewCredentialsToTheServerMessageText':	"Sende verschlüsselte Zugangsdaten zu Clipperz",
'changeCredentialsPanelDoneMessageTitle':	"Ändere Sicherheitssatz",
'changeCredentialsPanelDoneMessageText':	"Fertig",
'manageOTPTabLabel':	"Verwaltung des Sicheitssatzes für einmaliges Anmelden",
'manageOTPTabTitle':	"Verwaltung des Sicheitssatzes für einmaliges Anmelden",
'manageOTPTabDescription':	"<p>Der Sicherheitssatz für einmaliges Anmelden funktoniert wie Dein regulärer Sicherheitssatz, nur dass er nur einmal verwendet werden kann.</p> <p>Sollte der gleiche Sicherheitssatz zu einem späteren Zeitpunkt nocheinmal genutzt werden, wird dieser automatisch zurückgewießen und der Einlogvorgang scheitert.</p> <p>Sofort nach einem erfolgreichen Login wird der Sicherheitssatz für einmaliges Anmelden gelöscht und somit verhindert dass er ungewollt verwendet wird.</p> <p>Die Nutzung von Sicherheitssätzen für einmaliges Anmelden sind eine ideale Möglichkeit wenn Du verunsichert bist ob Trojaner, Spyware oder ähnliches auf Deinem Rechner vorhanden ist.</p> <p> <b>Es wird empfohlen Sicherheitssätze für einmaliges Anmelden beim Zugiff auf Clipperz zu verwenden, wenn man sich an öffentlichen Rechnern befindet, wie in Internet Cafes oder Bücherreien.</b> </p> <p> </p> <p> <b>Mehr dazu in Kürze ...</b> </p> ",
'accountPreferencesLabel':	"Einstellungen",
'accountPreferencesTabTitle':	"Einstellungen",
'accountPreferencesLanguageTitle':	"Sprachenauswahl",
'accountPreferencesLanguageDescription':	"<p>Wähle Deine bevorzugte Sprache, aus der unten stehenden Liste.</p> ",
'accountPreferencesInterfaceTitle':	"Personalisiere Dein persönliches Clipperz-Erscheinungsbild",
'accountPreferencesInterfaceDescription':	"<p>Passe dass Clipperz-Erscheinungsbild an Deine Wünsche an.</p> ",
'saveUserPreferencesFormSubmitLabel':	"Speichern",
'cancelUserPreferencesFormSubmitLabel':	"Abbruch",
'accountPreferencesSavingPanelTitle_Step1':	"Speichere Einstellungen",
'accountPreferencesSavingPanelText_Step1':	"Lokale Verschlüsselung der Einstellungen",
'accountPreferencesSavingPanelTitle_Step2':	"Speichere Einstellungen",
'accountPreferencesSavingPanelText_Step2':	"Sende verschlüsselte Einstellungen",
'deleteAccountTabLabel':	"Konto löschen",
'deleteAccountTabTitle':	"Konto löschen",
'deleteAccountFormUsernameLabel':	"Benutzername",
'deleteAccountFormPassphraseLabel':	"Sicherheitssatz",
'deleteAccountFormSafetyCheckboxLabel':	"Ich bin mir bewusst, dass alle meine Daten gelöscht werden und dieser Vorgang in keinem Falle rückgängig gemacht werden kann.",
'deleteAccountFormSubmitLabel':	"Konto löschens",
'deleteAccountFormWrongUsernameWarning':	"Falscher Benutzername",
'deleteAccountFormWrongPassphraseWarning':	"Falscher Sicherheitssatz",
'deleteAccountFormSafetyCheckWarning':	"Bitte lese die Bedingungen und akzeptiere die Auswahlboxen weiter unten.",
'accountPanelDeletingAccountPanelConfirmationTitle':	"ACHTUNG",
'accountPanelDeleteAccountPanelConfirmationText':	"Bist Du sicher, dass Du den Zugang löschen möchtest?",
'accountPanelDeleteAccountPanelConfirmButtonLabel':	"Ja",
'accountPanelDeleteAccountPanelDenyButtonLabel':	"Nein",
'offlineCopyTabLabel':	"Offline Kopie",
'offlineCopyTabTitle':	"Offline Kopie",
'offlineCopyTabDescription':	"<p>Mit nur einem Klick kannst Du alle Deine verschlüsselten Daten von dem Clipperz Server auf Deine Festplatte speichern und somit eine “nur lesbare” Offline Version anlegen. Diese Version ist auch dann verwendbar, wenn Du keine Verbindung ins Internet hast. (Zum Beispiel zum Speichern von Login-Informationen bei einem Hotspot)</p> <p>Die “nur lesbare” Version ist genauso sicher, wie die änderbare Version auf dem Server. Deine Daten werden niemals entschlüsselt gespeichert - beide Versionen verwenden die gleiche Art der Verschlüsselung und Entschlüsselung direkt im Browser.</p> <ol> <li> <p>Klicke auf den untenstehenden Link um die Offline Version herunterzuladen.</p> </li> <li> <p>Der Browser fragt Dich, was Du mit der Datei “Clipperz_YYYYMMDD.zip” machen möchtest. Speichere Sie auf Deine Festplatte.</p> </li> <li> <p>Unzip (dekomprimiere) die Datei. Du erhälst das Verzeichnis “Clipperz_YYYYMMDD”.</p> </li> <li> <p>Öffne das Verzeichnis “Clipperz_YYYYMMDD” und mache einen Doppelklick auf die Datei “index.html”.</p> </li> <li> <p>Gib Deinen Clipperz Benutzernamen und Sicherheitsschlüssel ein, um Zugriff auf Deine persönlichen Daten auch ohne Internetzugang zu erhalten.</p> </li> </ol> ",
'offlineCopyDownloadLinkLabel':	"Download",
'sharingTabLabel':	"Freigabe für gemeinsame Nutzung",
'sharingTabTitle':	"Freigabe für gemeinsame Nutzung",
'sharingTabDescription':	"<p>Häufig muss eine vertrauenswürdige Information mit mehreren Personen geteilt werden.</p> <p>Dies sollte so einfach sein, wie einem Kollegen die PIN für den Anrufbeantworter zu geben, wenn Du im Urlaub bist; jedoch so schwierig, wie berechtigten Erben Zugriff auf das Ersparte bei der Bank zu geben.</p> <p>Clipperz ermöglicht die einfache Freigabe für gemeinsam genutzte Informationen, an berechtigte Personen, durch einen einfachen Prozess.</p> <p> </p> <p> <b>Mehr dazu in Kürze ...</b> </p> ",
'importTabLabel':	"Import",
'importTabTitle':	"Import",
'importTabDescription':	"<p> <b>In Kürze ...</b> </p> ",
'printingTabLabel':	"Export",
'printingTabTitle':	"Export",
'printingTabDescription':	"<p> <b>Drucke deine Kartendaten</b> </p> <p>Klicke auf den untenstehenden Link. Es öffnet sich ein Fenster mit Deinen Kartendaten in einem druckerfreundlichen Format.</p> <p>Wenn Du den Ausdruck aus Absicherungsgründen nutzen möchtest, nutze lieber die Variante der sichereren “Offline Kopie”. Ein Ausdruck ist wie eine Notiz auf dem Scheibtisch und könnte von jedem ohne weiteres gelesen werden. Bewahre diesen an einem sicheren, für andere nicht zugänglichen Ort auf!</p> ",
'printingLinkLabel':	"Druckerfreundliches Format",
'contactsTabLabel':	"Kontakte",
'contactsTabTitle':	"Kontakte",
'bookmarkletTabLabel':	"Bookmarklet",
'bookmarkletTabTitle':	"Bookmarklet",
'bookmarkletTabDescription':	"<p>Ein Bookmarklet ist ein Werkezug, welches Dir mit einem Mausklick wichtige Funktionen ermöglicht. Es kann gespeichert und verwendet werden wie eine ganz normale Webseite, die Du in Deine Favoriten gespeichert hast.</p> <p>Das clipperz Bookmarklet ermöglicht Dir schnell und einfach neue Karten und Direkt-Logins für bestehende Karten anzulegen.</p> <p> <b>Bitte beachte: Das Bookmarklet enthält keine Informationen zu Deinem Zugang (wie Benutzernamen oder Passwort), das Bookmarklet ist ein generisches Werzeug, welches für jeden Clipperz Anwender den gleichen Code beinhaltet.</b> </p> <div> <p>Um das Bookmarklet zu installieren <b>ziehe</b> den unten stehenden Link in die Lesezeichen-Leiste Deines Browsers.</p> </div> ",
'bookmarkletTabBookmarkletTitle':	"Zu Clipperz hinzufügen",
'bookmarkletTabInstructions':	"<h3>Anlegen einer neuen Karte für das Direkt Login bei einem Webservice</h3> <ol> <li> <p>Öffne die Webseite, auf der das Anmeldeforumlar vorhanden ist. (Das ist die Seite, auf der Du normal Deine Zugangsdaten einträgst)</p> </li> <li> <p>Öffne das Boormarklet durch anklicken: ein Pop-Up Fenster erscheint und bietet Dir die Karteninformationen an.</p> </li> <li> <p>Kopiere von diesem Fenster den Inhalt des größten Textfeldes durch in die Zwischenablage (Makieren und STRG+C)</p> </li> <li> <p>Öffne Deinen Clipperz Zugang und wähle <b>Neue Karte anlegen</b>.</p> </li> <li> <p>Füge den Inhalt Deiner Zwischenablage in das Textfeld ein (STRG+V) und ergänze optional einen <b>Titel</b>.</p> </li> <li> <p>Drücke die <b>Anlegen</b> Schaltfläche, kontrolliere nocheinmal die Details, und wähle anschließend <b>Speichern<b>.</p> </li> </ol> <h3>Direkt Login Funktionalität zu einer bestehenden Karte ergänzen</h3> <ol> <li> <p>Gleich wie oben.</p> </li> <li> <p>Gleich wie oben.</p> </li> <li> <p>Gleich wie oben.</p> </li> <li> <p>Öffne Deinen Clipperz Zugang und wähle die Karte, die Du ändern möchtest. Klicke anschließend auf <b>Bearbeiten</b>.</p> </li> <li> <p>Füge den Inhalt Deiner Zwischenablage in das Textfeld für “Direkt Login” ein (STRG+V).</p> </li> <li> <p>Drücke auf <b>Direkt Login hinzufügen</b>, kontrolliere die Angabgen und wähle <b>Speichern</b>.</p> </li> </ol> <p> </p> <p>Weitere Informationen über das Bookmarklet findest Du <a href=\"http://www.clipperz.com/support/user_guide/bookmarklet\" target=\"_blank\">hier</a>.</p> ",
'mainPanelDirectLoginBlockLabel':	"Direktes Login",
'directLinkReferenceShowButtonLabel':	"zeigen",
'mainPanelDirectLoginBlockDescription':	"<p>Add “direct logins” to sign in to your web accounts without typing usernames and passwords!</p> <p>“Direct logins” greatly enhance your password security since you can:</p> <ul> <li> <p>conveniently adopt and enter complex passwords;</p> </li> <li> <p>never re-use the same and easy-to-guess password.</p> </li> </ul> <p>Simple and quick configuration with the Clipperz <b>bookmarklet</b>.</p> <a href=\"http://www.clipperz.com/support/user_guide/direct_logins\" target=\"_blank\">Learn more about “direct logins”</a> ",
'mainPanelRecordsBlockLabel':	"Karten",
'mainPanelAddRecordButtonLabel':	"Neue Karte anlegen",
'mainPanelRemoveRecordButtonLabel':	"Karte löschen",
'mainPanelRecordFilterBlockAllLabel':	"all",
'mainPanelRecordFilterBlockTagsLabel':	"tags",
'mainPanelRecordFilterBlockSearchLabel':	"search",
'recordDetailNoRecordAtAllTitle':	"Willkommen bei Clipperz!",
'recordDetailNoRecordAtAllDescription':	"<h5>Beginne mit dem Hinzufügen von Karten zu Deinem Zugang.</h5> <p>Karten sind einfache und flexible Formulare, bei denen Du Deine Passwörter und andere vertrauenswürde Daten speichern kannst.</p> <p>Karten können Zugangsinformationen für eine WebSite, die Kombination Deines Fahrradschlosses, oder Daten Deiner Kreditkarte enthalten, ...</p> <h5>Vergiss nicht das Bookmarklet</h5> <p>Bevor Du beginnst, installiere Dir das “Add to Clipperz” Bookmarklet: Es vereinfacht Dir das anlegen von Karten und verbessert somit den Komfor.</p> <p>Gehe zum “Bookmarklet” Tabulator um herauszufinden, wie es installiert und verwendet werden kann.</p> <p> </p> <p>Dann klicke einfach auf den <b>Neue Karte anlegen</b> Button und genieße Deinen Clipperz Zugang.</p> <p> </p> <a href=\"http://www.clipperz.com/support/user_guide/managing_cards\" target=\"_blank\">Näheres zum Erstellen und Verwalten von Karten lernen</a> ",
'newRecordWizardTitleBox':	"<h5>Bitte wähle eine Vorlage</h5> <p>Karten sind einfache und flexible Formulare, bei denen Du Passwörter oder jede Art von vertraulichen Informationen speichern kannst.</p> <p>Beginne mit der Auswahl einer der unten stehenden Vorlagen. Zu jeder Zeit kannst Du Deine Karten  durch hinzufügen oder entfernen von Feldern verändern.</p> ",
'newRecordWizardBookmarkletConfigurationTitle':	"Direktes Login",
'newRecordWizardBookmarkletConfigurationDescription':	"<p>Füge bitte unten den Konfigurationscode ein, den das Clipperz Bookmarklet erzeugt hat.</p> <p>Eine neue Karte mit einem vollständigen Direkt Login zu dem gewählten Webzugang wird angelegt.</p> ",
'newRecordWizardCreateButtonLabel':	"Anlegen",
'newRecordWizardCancelButtonLabel':	"Abbruch",
'recordTemplates':	{
	'WebAccount':	{
		'title':	"Web Zugangsdaten",
		'description':	"<p>Eine einfache Karte, die die Login Informationen für einen Online Service speichert.</p> ",
		'fields':	{
			'URL':	"Web Adresse",
			'TXT':	"Benutzername / E-Mail",
			'PWD':	"Passwort"
		}
	},
	'BankAccount':	{
		'title':	"Bank Zugangsdaten",
		'description':	"<p>Speichere geschützt Deine Online Banking Zugangsdaten.</p> ",
		'fields':	{
			'TXT':	"Bank",
			'TXT':	"Kontonummer",
			'URL':	"Web Adresse",
			'TXT':	"Online Zugangsdaten",
			'PWD':	"Online Passwort"
		}
	},
	'CreditCard':	{
		'title':	"Kreditkarte",
		'description':	"<p>Kartennummer, CVV2, Ablaufdatum und PIN zu jeder Zeit abrufbar bei Clipperz.</p> ",
		'fields':	{
			'TXT':	"Art (Visa, AmEx, ...)",
			'TXT':	"Nummer",
			'TXT':	"Inhaber",
			'TXT':	"Ablaufdatum",
			'TXT':	"CVV2",
			'PWD':	"PIN",
			'URL':	"Webseite",
			'TXT':	"Online Zugangsdaten",
			'PWD':	"Passwort"
		}
	},
	'AddressBookEntry':	{
		'title':	"Adressbuch Eintrag",
		'description':	"<p>Clipperz kann auch als Dein neues privates Adressbuch agieren. Nutze diese Vorlage um einfach eine neuen Eintrag anzulegen.</p> ",
		'fields':	{
			'TXT':	"Name",
			'TXT':	"Email",
			'TXT':	"Telefon",
			'TXT':	"Handy",
			'ADDR':	"Adresse"
		}
	},
	'Custom':	{
		'title':	"Benutzerdefinierte Karte",
		'description':	"<p>Egal welche Art von vertraulichen Informationen Du speichern musst, mit der benutzerdefinierten Karte kannst Du diese Informationen speichern.</p> ",
		'fields':	{
			'TXT':	"Feldname 1",
			'TXT':	"Feldname 2",
			'TXT':	"Feldname 3"
		}
	}
},
'recordFieldTypologies':	{
	'TXT':	{
		'description':	"simple text field",
		'shortDescription':	"Text"
	},
	'PWD':	{
		'description':	"simple text field, with default status set to hidden",
		'shortDescription':	"Passwort"
	},
	'URL':	{
		'description':	"simple text field in edit mode, that became an active url in view mode",
		'shortDescription':	"Webadresse"
	},
	'DATE':	{
		'description':	"a value set with a calendar helper",
		'shortDescription':	"Datum"
	},
	'ADDR':	{
		'description':	"just like the URL, but the active link points to Google Maps (or similar service) passing the address value as argument",
		'shortDescription':	"Postanschrift"
	},
	'CHECK':	{
		'description':	"check description",
		'shortDescription':	"check"
	},
	'RADIO':	{
		'description':	"radio description",
		'shortDescription':	"radio"
	},
	'SELECT':	{
		'description':	"select description",
		'shortDescription':	"select"
	}
},
'newRecordPanelGeneralExceptionTitle':	"Fehler",
'newRecordPanelGeneralExceptionMessage':	"Der Konfigurationstext ist nicht gültig. Stelle sicher, dass Du den Text des Bookmarket Pop-Up eingefügt hast und versuch es nocheinmal.",
'newRecordPanelWrongBookmarkletVersionExceptionTitle':	"Fehler",
'newRecordPanelWrongBookmarkletVersionExceptionMessage':	"Der Konfigurationstext wurde von einer älteren Version des Bookmarklets erstellt. Bitte aktualisiere Dein Bookmarklet und probiere es erneut.",
'newRecordPanelExceptionPanelCloseButtonLabel':	"Abbruch",
'mainPanelDeletingRecordPanelConfirmationTitle':	"Lösche ausgewählte Karte",
'mainPanelDeleteRecordPanelConfirmationText':	"Möschtest Du wirklich die ausgewählte Karte löschen?",
'mainPanelDeleteRecordPanelConfirmButtonLabel':	"Ja",
'mainPanelDeleteRecordPanelDenyButtonLabel':	"Nein",
'mainPanelDeletingRecordPanelInitialTitle':	"Lösche ausgewählte Karte",
'mainPanelDeletingRecordPanelCompletedText':	"Fertig",
'deleteRecordPanelCollectRecordDataMessageTitle':	"Karte löschen",
'deleteRecordPanelCollectRecordDataMessageText':	"Aktualisiere Kartenliste",
'deleteRecordPanelEncryptUserDataMessageTitle':	"Karte löschen",
'deleteRecordPanelEncryptUserDataMessageText':	"Lokale Verschlüsselung der Karten Kopfdaten",
'deleteRecordPanelSendingDataToTheServerMessageTitle':	"Karte löschen",
'deleteRecordPanelSendingDataToTheServerMessageText':	"Lade verschlüsselte Karten Kopfdaten zu Clipperz hoch",
'deleteRecordPanelUpdatingTheInterfaceMessageTitle':	"Karte löschen",
'deleteRecordPanelUpdatingTheInterfaceMessageText':	"Aktualisiere Benutzerschnittstelle",
'recordDetailNoRecordSelectedTitle':	"Keine Karte ausgewählt",
'recordDetailNoRecordSelectedDescription':	"<p>Bitte wähle aus der linken Liste eine Karte aus.</p> ",
'recordDetailLoadingRecordMessage':	"Lade verschlüsselte Karte von Clipperz runter",
'recordDetailDecryptingRecordMessage':	"Lokale entschlüsselung der Kartendaten",
'recordDetailLoadingRecordVersionMessage':	"Herunterladen der aktuellsten Kartenversion",
'recordDetailDecryptingRecordVersionMessage':	"Lokale Entschlüsselung der aktuellen Version",
'recordDetailLoadingErrorMessageTitle':	"Fehler beim Herunterladen der Karte",
'recordDetailNotesLabel':	"Notiz",
'recordDetailLabelFieldColumnLabel':	"Feld Namen",
'recordDetailDataFieldColumnLabel':	"Feld Daten",
'recordDetailTypeFieldColumnLabel':	"Art",
'recordDetailSavingChangesMessagePanelInitialTitle':	"Speichere Karte",
'recordDetailAddFieldButtonLabel':	"Neues Feld hinzufügen",
'recordDetailDirectLoginBlockTitle':	"Direkt Logins",
'recordDetailNewDirectLoginDescription':	"<p>Direkt Login Konfiguration</p> ",
'recordDetailDirectLoginBlockNoDirectLoginConfiguredDescription':	"<p>Enthält diese Karte Informationen um Zugriff auf ein Online Service zu erhalten?</p> <p>Verwende das Bookmarklet um ein “Direkt Login” mit nur einem Klick von Clipperz zu konfigurieren.</p> ",
'recordDetailAddNewDirectLoginButtonLabel':	"Neues Direktlogin hinzufügen",
'recordDetailEditButtonLabel':	"Bearbeiten",
'recordDetailSaveButtonLabel':	"Speichern",
'recordDetailCancelButtonLabel':	"Abbruch",
'newRecordTitleLabel':	"_neue Karte_",
'newDirectLoginLabelSuffix':	"",
'recordSaveChangesPanelCollectRecordInfoMessageTitle':	"Karte speichern",
'recordSaveChangesPanelCollectRecordInfoMessageText':	"Aktualisierung der Karten Kopfdaten",
'recordSaveChangesPanelEncryptUserDataMessageTitle':	"Karte speichern",
'recordSaveChangesPanelEncryptUserDataMessageText':	"Lokale Verschlüsselung der Karten Kopfdaten",
'recordSaveChangesPanelEncryptRecordDataMessageTitle':	"Karte speichern",
'recordSaveChangesPanelEncryptRecordDataMessageText':	"Lokale Verschlüsselung der Karten Informationen",
'recordSaveChangesPanelEncryptRecordVersionDataMessageTitle':	"Karte speichern",
'recordSaveChangesPanelEncryptRecordVersionDataMessageText':	"Lokale Verschlüsselung der Karten Versions Informationen",
'recordSaveChangesPanelSendingDataToTheServerMessageTitle':	"Karte speichern",
'recordSaveChangesPanelSendingDataToTheServerMessageText':	"Verschlüsselte Karten Kopfdaten auf Clipperz hochladen",
'recordSaveChangesPanelUpdatingTheInterfaceMessageTitle':	"Karte speichern",
'recordSaveChangesPanelUpdatingTheInterfaceMessageText':	"Aktualisierung der Benutzerschnittstelle",
'exit':	"<h2> <b>Auf Wiedersehen! Danke, dass Du Clipperz verwendet hast.</b> </h2> <ul> <li> <h3>Hinweis:</h3> <ul> <li> <p>Speichere diese Seite in Deine Favoriten, damit Du auch in Zukunft dich sicher mit Clipperz verbinden kannst (solltest Du dies nicht bereits getan haben)</p> </li> <li> <p>Clipperz wird Dir niemals eine E-Mail senden, weil wir Dich niemals nach Deiner E-Mail Anschrift gefragt haben (und dies auch nie werden) – öffne daher niemals eine Mail, die wvon Clipperz zu sein scheint</p> </li> </ul> </li> </ul> <p> </p> <p>In 10 Sekunden wirdst Du auf eine Seite von Wikipedia umgeleitet, wo Du über eine herausragende Sicherheitslücke informiert wirst.</p> ",
//'DWRUtilLoadingMessage':	"Lade Daten ...",
'comingSoon':	"In Kürze ...",
'panelCollectingEntryopyMessageText':	"Sammlung",
'directLoginConfigurationCheckBoxFieldSelectedValue':	"Ja",
'directLoginConfigurationCheckBoxFieldNotSelectedValue':	"Nein",

__syntaxFix__: "syntax fix"
});
