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

Clipperz.PM.Strings.Languages['fr-FR'.toLowerCase()] = MochiKit.Base.merge(Clipperz.PM.Strings.Languages['en-us'], {
'clipperzServiceDescription':	"<h2>Gardez le pour vous!</h2> <ul> <li> <h3>Clipperz est:</h3> <ul> <li> <p>un gestionnaire de mots de passe sécurisé</p> </li> <li> <p>une solution fiable d’authentification unique</p> </li> <li> <p>une chambre forte numérique pour vos données confidentielles</p> </li> </ul> </li> <li> <h3>Avec Clipperz vous pouvez:</h3> <ul> <li> <p>enregistrer et gérer vos mots de passe et autres informations d'identification en ligne</p> </li> <li> <p>vous identifier sur vos sites web sans avoir besoin de saisir votre nom d’utilisateur ou votre mot de passe</p> </li> <li> <p>protéger toutes vos données sensibles: codes d’alarmes, PINs (téléphone portable), carte de crédits, &hellip;</p> </li> <li> <p>partager certaines données confidentielles avec vos proches et vos associés (bientôt disponible!)</p> </li> </ul> </li> <li> <h3>Les avantages de Clipperz:</h3> <ul> <li> <p>c'est gratuit et totalement anonyme</p> </li> <li> <p>accéder à vos données depuis n’importe quel ordinateur</p> </li> <li> <p>vous n'avez besoin d'aucun logiciel et rien n'est à installer sur votre poste</p> </li> <li> <p>cela vous évite de garder des mots de passe sur votre PC ou sur des bouts de papier</p> </li> </ul> </li> <li> <h3>La sécurité de Clipperz:</h3> <ul> <li> <p>vos mots de passe sont localement chiffrés par votre navigateur avant d’être téléchargé à Clipperz</p> </li> <li> <p>la clé de cryptage est une phrase secrète détenue uniquement par vous même</p> </li> <li> <p>héberge vos données sensibles dans des formulaires cryptés et ne peuvent être consultées directement en clair</p> </li> <li> <p>est développé sur des standard de cryptage réputé sans aucune fantaisie ni de bricolage fait maison</p> </li> <li> <p>vous pouvez consulter le code source autant que vous le souhaitez</p> </li> </ul> </li> <li> <a href=\"http://www.clipperz.com\" target=\"_blank\">Plus d’informations</a> </li> </ul> ",
'loginFormTitle':	"identifiez-vous avec votre compte Clipperz",
'loginFormUsernameLabel':	"nom d’utilisateur",
'loginFormPassphraseLabel':	"phrase secrète",
'loginFormDontHaveAnAccountLabel':	"vous n’avez pas de compte?",
'loginFormCreateOneLabel':	"créez votre compte",
'loginFormForgotYourCredentialsLabel':	"vous avez oublié votre certificat?",
'loginFormAarghThatsBadLabel':	"c’est fort dommage pour vous!",
'loginFormAfraidOfMaliciousScriptsLabel':	"vous avez peur des script malicieux?",
'loginFormVerifyTheCodeLabel':	"vérifiez le code",
'loginFormButtonLabel':	"S’identifer",
'loginPanelSwithLanguageDescription':	"<p>Sélectionnez votre langue préféré</p> ",
'browserCompatibilityDescription':	"<p>Have a better and safer Clipperz experience with Firefox. However Clipperz works just fine also with Opera and MS Internet Explorer!</p> ",
'loginMessagePanelInitialTitle':	"En cours d’identification ...",
'loginMessagePanelInitialButtonLabel':	"Annuler",
'loginMessagePanelConnectedTitle':	"Connecté",
'loginMessagePanelConnectedText':	"Terminé",
'loginMessagePanelFailureTitle':	"Erreur",
'loginMessagePanelFailureText':	"Identification échoué",
'loginMessagePanelFailureButtonLabel':	"Fermer",
'connectionLoginSendingCredentialsMessageTitle':	"Vérification des identifiants",
'connectionLoginSendingCredentialsMessageText':	"Envoi des identifiants",
'connectionLoginCredentialsVerificationMessageTitle':	"Vérification des identifiants",
'connectionLoginCredentialsVerificationMessageText':	"Authentification SRP en cours",
'connectionLoginDoneMessageTitle':	"Vérification des identifiants",
'connectionLoginDoneMessageText':	"Connecté",
'userLoginPanelUpgradingUserCredentialsMessageTitle':	"Vérification des identifiants",
'userLoginPanelUpgradingUserCredentialsMessageText':	"Mise à jour de vos identifiants à un nouveau schéma d’authentification",
'userLoginPanelConnectedMessageTitle':	"Utilisateur identifié",
'userLoginPanelConnectedMessageText':	"Vous vous êtes identifié avec succès",
'userLoginPanelTryingAnOlderConnectionSchemaMessageTitle':	"Vérification des identifiants",
'userLoginPanelTryingAnOlderConnectionSchemaMessageText':	"Nous essayons un ancien schéma d’authentification",
'userLoginPanelLoadingUserDataMessageTitle':	"Utilisateur identifié",
'userLoginPanelLoadingUserDataMessageText':	"Téléchargement des cartes d’en-têtes chiffrés depuis Clipperz",
'userLoginPanelDecryptingUserDataMessageTitle':	"Utilisateur identifié",
'userLoginPanelDecryptingUserDataMessageText':	"Décryptage local des en-têtes chiffrés",
'userLoginPanelDecryptingUserStatisticsMessageTitle':	"Utilisateur identifié",
'userLoginPanelDecryptingUserStatisticsMessageText':	"Décryptage local des statistiques",
'splashAlertTitle':	"Bienvenue sur Clipperz!",
'splashAlertText':	"<p>Conseils de sécurité</p> <ul> <li> <p>Le stockage de vos mots de passe sur Clipperz est aussi sécurisé que la phrase que vous avez sélectionnée pour les protéger. Personne ne peux y accéder tant que personne ne connait votre phrase secrète.</p> </li> <li> <p>Si vous allez utiliser Clipperz pour sauvegarder des informations sensibles et critiques assurez-vous d’utiliser une phrase secrète compliquée. Plus elle sera longue mieux ce sera!</p> </li> <li> <p>Clipperz ne sera pas capable de récupérer votre phrase secrète!</p> </li> </ul> <p>Pour toute autre information, veuillez vous référer au site <a href=\"http://www.clipperz.com\" target=\"_blank\">Clipperz</a>.</p> ",
'splashAlertCloseButtonLabel':	"Ok",
'registrationFormTitle':	"créer votre compte",
'registrationFormUsernameLabel':	"nom d’utilisateur",
'registrationFormPassphraseLabel':	"phrase secrète",
'registrationFormRetypePassphraseLabel':	"re-saisissez votre phrase secrète",
'registrationFormSafetyCheckLabel':	"J’accepte que Clipperz ne pourra pas récupérer ma phrase secrète.",
'registrationFormTermsOfServiceCheckLabel':	"J’ai lu et j’accepte les <a href='http://www.clipperz.com/terms_of_service' target='_blank'>Conditions d’Utilisation du Service</a>.",
'registrationFormDoYouAlreadyHaveAnAccountLabel':	"avez-vous déjà un compte?",
'registrationFormSimplyLoginLabel':	"identifiez-vous",
'registrationFormButtonLabel':	"S’inscrire",
'registrationFormWarningMessageNotMatchingPassphrases':	"Vos phrases secrètes ne correspondent pas, veuillez les saisir à nouveau.",
'registrationFormWarningMessageSafetyCheckNotSelected':	"Veuillez lire et cocher les cases-à-cocher ci-dessous.",
'registrationFormWarningMessageTermsOfServiceCheckNotSelected':	"Vous devez accepter les “Conditions d’Utilisation du Service”.",
'registrationMessagePanelInitialTitle':	"Création du compte en cours ...",
'registrationMessagePanelInitialButtonLabel':	"Annuler",
'registrationMessagePanelRegistrationDoneTitle':	"Enregistrement",
'registrationMessagePanelRegistrationDoneText':	"Terminé",
'registrationMessagePanelFailureTitle':	"Enregistrement échoué",
'registrationMessagePanelFailureButtonLabel':	"Fermer",
'connectionRegistrationSendingRequestMessageText':	"Vérification en cours des identifiants",
'connectionRegistrationSendingCredentialsMessageText':	"Envoi des identifiants",
'registrationSplashPanelTitle':	"Conseils de sécurité",
'registrationSplashPanelDescription':	"<p>Ce sont vos identifiants Clipperz, gardez les biens. Clipperz ne va plus jamais vous montrer votre nom d’utilisateur et votre phrase secrète!</p> ",
'registrationSplashPanelUsernameLabel':	"nom d’utilisateur",
'registrationSplashPanelPassphraseLabel':	"phrase secrète",
'registrationSplashPanelShowPassphraseButtonLabel':	"afficher la phrase secrète",
'donateHeaderLinkLabel':	"faites un don",
'creditsHeaderLinkLabel':	"crédits",
'feedbackHeaderLinkLabel':	"votre avis",
'helpHeaderLinkLabel':	"aide",
'forumHeaderLinkLabel':	"forum",
'recordMenuLabel':	"cartes",
'accountMenuLabel':	"compte",
'dataMenuLabel':	"données",
'contactsMenuLabel':	"contacts",
'toolsMenuLabel':	"outils",
'logoutMenuLabel':	"déconnexion",
'lockMenuLabel':	"verrouiller",
'lockTitle':	"Le compte est verrouillé",
'lockDescription':	"<p>Pour déverrouiller votre compte, veuillez saisir votre phrase secrète</p> ",
'unlockButtonLabel':	"Déverrouiller",
'changePasswordTabLabel':	"Changer votre phrase secrète",
'changePasswordTabTitle':	"Changer votre phrase secrète",
'changePasswordFormUsernameLabel':	"nom d’utilisateur",
'changePasswordFormOldPassphraseLabel':	"ancienne phrase secrète",
'changePasswordFormNewPassphraseLabel':	"nouvelle phrase secrète",
'changePasswordFormRetypePassphraseLabel':	"re-saisissez phrase secrète",
'changePasswordFormSafetyCheckboxLabel':	"Je sais que Clipperz ne pourra pas récupérer ma phrase secrète.",
'changePasswordFormSubmitLabel':	"Changer ma phrase secrète",
'changePasswordFormWrongUsernameWarning':	"Nom d’utilisateur incorrect",
'changePasswordFormWrongPassphraseWarning':	"Phrase secrète incorrect",
'changePasswordFormWrongRetypePassphraseWarning':	"Votre phrase secrète ne correspond pas, veuillez la saisir à nouveau",
'changePasswordFormSafetyCheckWarning':	"Veuillez lire et cocher la case-à-cocher ci-dessous",
'changePasswordFormProgressDialogTitle':	"Changement des identifiants utilisateurs",
'changePasswordFormProgressDialogConnectedMessageTitle':	"Connecté",
'changePasswordFormProgressDialogConnectedMessageText':	"Terminé",
'changePasswordFormProgressDialogErrorMessageTitle':	"Erreur",
'changePasswordFormProgressDialogErrorMessageText':	"Changement de identifiants échoué!",
'changeCredentialsPanelEncryptingDataMessageTitle':	"Changement de votre phrase secrète",
'changeCredentialsPanelEncryptingDataMessageText':	"Cryptage local des identifiants",
'changeCredentialsPanelCreatingNewCredentialsMessageTitle':	"Modifier votre phrase secrète",
'changeCredentialsPanelCreatingNewCredentialsMessageText':	"Mettre à jour vos identifiants",
'changeCredentialsPanelSendingNewCredentialsToTheServerMessageTitle':	"Modifier votre phrase secrète",
'changeCredentialsPanelSendingNewCredentialsToTheServerMessageText':	"Téléchargement de vos identifiants sur Clipperz.com",
'changeCredentialsPanelDoneMessageTitle':	"Modifier votre phrase secrète",
'changeCredentialsPanelDoneMessageText':	"Terminé",
'manageOTPTabLabel':	"Mettre à jour votre phrase secrète à usage unique",
'manageOTPTabTitle':	"Mettre à jour votre phrase secrète à usage unique",
'manageOTPTabDescription':	"<p>Une phrase secrète à usage unique fonctionne comme votre phrase secrète habituelle, mais elle ne peut être utilisé qu'une seule fois.</p> <p>Si la phrase secrète est utilisé de nouveau et que vous tenté de vous identifier à nouveau, vous serez rejeté et le processus d'identification échouera.</p> <p>Juste après une identification correcte, votre phrase secrète à usage unique sera effacée pour interdire tout accès frauduleux.</p> <p>Les phrases secrètes à usage unique sont un excellent choix si vous craignez qu'un logiciel espion ne vole vos données après avoir infecté votre machine.</p> <p> <b>Il est fortement recommandé d'utiliser des phrases secrètes à usage unique lorsque vous accédez à Clipperz depuis un terminal public, comme un cybercafé ou une borne Internet.</b> </p> ",
'accountPreferencesLabel':	"Préférences",
'accountPreferencesTabTitle':	"Préférences",
'accountPreferencesLanguageTitle':	"Choix de la langue",
'accountPreferencesLanguageDescription':	"<p>Choisissez la langue d'affichage de Clipperz dans la liste suivante.</p> ",
'accountPreferencesInterfaceTitle':	"Personnalisation de l'interface",
'accountPreferencesInterfaceDescription':	"<p>Ajustez l'interface de Clipperz à vos besoins.</p> ",
'saveUserPreferencesFormSubmitLabel':	"Enregistrer",
'cancelUserPreferencesFormSubmitLabel':	"Annuler",
'accountPreferencesSavingPanelTitle_Step1':	"Enregistrement des préférences",
'accountPreferencesSavingPanelText_Step1':	"Chiffrement local de vos préférences",
'accountPreferencesSavingPanelTitle_Step2':	"Enregistrement des préférences",
'accountPreferencesSavingPanelText_Step2':	"Transmission des préférences chiffrées au serveur",
'deleteAccountTabLabel':	"Supprimer votre compte",
'deleteAccountTabTitle':	"Supprimer votre compte",
'deleteAccountFormUsernameLabel':	"nom d’utilisateur",
'deleteAccountFormPassphraseLabel':	"phrase secrète",
'deleteAccountFormSafetyCheckboxLabel':	"Je sais que toute mes données seront supprimés et que cette action sera irréversible.",
'deleteAccountFormSubmitLabel':	"Supprimer mon compte",
'deleteAccountFormWrongUsernameWarning':	"Nom d’utilisateur incorrect",
'deleteAccountFormWrongPassphraseWarning':	"Phrase secrète incorrect",
'deleteAccountFormSafetyCheckWarning':	"Veuillez lire et cocher la case-à-cocher ci-dessous.",
'accountPanelDeletingAccountPanelConfirmationTitle':	"ATTENTION",
'accountPanelDeleteAccountPanelConfirmationText':	"Êtes-vous sûr de vouloir supprimer ce compte?",
'accountPanelDeleteAccountPanelConfirmButtonLabel':	"Oui",
'accountPanelDeleteAccountPanelDenyButtonLabel':	"Non",
'offlineCopyTabLabel':	"Copie locale",
'offlineCopyTabTitle':	"Copie locale",
'offlineCopyTabDescription':	"<p>D'un seul click, vous pouvez télécharger toutes vos données chiffrées des serveurs Clipperz sur votre disque dur, créant ainsi une version déconnecté de Clipperz utilisable lorsque vous n'êtes pas connectés à Internet.</p> <p>Cette version en lecture seule est aussi sécurisée que la version en lecture-écriture, et n'expose pas vos données à un risque plus élevé. Elles partagent en effet le même code et la même architecture de sécurité.</p> <ol> <li> <p>Cliquez sur le liens ci-dessous pour lancer le téléchargement.</p> </li> <li> <p>Votre navigateur vous demandera que faire du fichier “Clipperz_YYYYMMDD.html”. Sauvez le sur votre disque dur.</p> </li> <li> <p>Puis double-cliquez sur le fichier téléchargé pour lancer la version déconnectée dans votre navigateur.</p> </li> <li> <p>Utilisez vos nom d’utilisateur et phrase secrète habituels.</p> </li> </ol> ",
'offlineCopyDownloadLinkLabel':	"Télécharger",
'sharingTabLabel':	"Partager",
'sharingTabTitle':	"Partager",
'sharingTabDescription':	"<p>De temps en temps il est nécessaire de partager des parties de vos informations confidentiels avec une ou plusieurs personnes.</p> <p>Cela pourrait être aussi simple que l’octroi à un collègue de votre code d’accès à votre messagerie vocale quand vous êtes hors du bureau, ou aussi compliqués que la permission d’ayant droit aux héritiers pour avoir accès à votre boîte de coffre-fort à la banque locale.</p> <p>Clipperz vous permez donc de partager vos mots de passe grâce à un processe sûr et direct.</p> <p> </p> <p> <b>Prochainement disponible ...</b> </p> ",
'importTabLabel':	"Importer",
'importTabTitle':	"Importer",
'importTabDescription':	"<p> <b>Prochainement disponible ...</b> </p> ",
'printingTabLabel':	"Exporter",
'printingTabTitle':	"Exporter",
'printingTabDescription':	"<p> <b>Version d’impression</b> </p> <p>En cliquant sur ce bouton vous ouvrirez une fenêtre contenant vos en-têtes cryptés dans un format d’impression.</p> <p>Si vous imprimez pour des raisons de sauvegarde personnel, veuillez vous tourner vers une solution plus sécurisé comme la version “Copie locale”.</p> ",
'printingLinkLabel':	"Version imprimable",
'contactsTabLabel':	"Contacts",
'contactsTabTitle':	"Contacts",
'passwordGeneratorTabLabel':	"Générateur de mot de passe",
'bookmarkletTabLabel':	"Bookmarklet",
'passwordGeneratorTabTitle':	"Générateur de mot de passe",
'bookmarkletTabTitle':	"Bookmarklet",
'paswordGeneratorTabDescription':	"<p> </p> ",
'passwordGeneratorTabButtonLabel':	" Générer un mot de passe",
'bookmarkletTabDescription':	"<p>Un bookmarklet est un outil “clique unique” simple qui peut exécuter des tâches très utiles. Il peut être sauvegardé et utilisé comme un signet de page Web normal.</p> <p>Le bookmarklet Clipperz vous aidera à rapidement créer de nouvelles cartes et des nouveaux “accès directs” avec vos cartes existantes.</p> <p> <b>Notez s’il vous plaît que le bookmarklet n’inclut pas d’informations liées à votre compte (par exemple votre nom d’utilisateur ou votre phrase secrète), le bookmarklet est un outil général contenant le même code pour chaque utilisateur Clipperz.</b> </p> <div> <p>Pour installer le bookmarklet <b>glissez & déposez</b> le lien ci-dessous dans votre bar de navigation.</p> </div> ",
'bookmarkletTabBookmarkletTitle':	"Ajouter à Clipperz",
'bookmarkletTabInstructions':	"<h3>Comment créer une carte comprenant un lien de “accès direct” à un service en ligne</h3> <ol> <li> <p>Ouvrir la page Web où le site affiche un formulaire d’identification. (C’est la page où vous entrez d’habitude vos informations d'identification)</p> </li> <li> <p>Lancer le bookmarklet en cliquant dessus : une fenêtre contextuelle apparait alors sur la page Web.</p> </li> <li> <p>Copier dans le presse-papiers le contenu du grand secteur de texte dans le menu contextuel. (Ctrl-C)</p> </li> <li> <p>Entrer dans votre compte Clipperz et cliquer sur <b>“Ajouter une nouvelle carte”</b>.</p> </li> <li> <p>Coller le presse-papiers dans la zone de texte et rajouter un titre (facultatif). (Ctrl-V)</p> </li> <li> <p>Cliquer sur le bouton <b>“Créer”</b>, repassez en revu les détails et cliquer <b>“Sauver”</b>. De plus amples informations sont disponibles <a href=\"http://www.clipperz.com/support/user_guide/bookmarklet\" target=\"_blank\">ici</a>.</p> </li> </ol> <h3>Comment ajouter un lien de “accès direct” à une carte existante</h3> <ol> <li> <p>Idem que précédemment.</p> </li> <li> <p>Idem que précédemment..</p> </li> <li> <p>Idem que précédemment.</p> </li> <li> <p>Entrer dans votre compte Clipperz et sélectionner la carte crypté qui contient les identifiants pour le service web que vous venez tout juste de visiter et cliquer sur <b>“Modifier”</b>.</p> </li> <li> <p>Coller le presse-papier dans la zone de texte <b>“Accès directs”</b>. (ctrl-V)</p> </li> <li> <p>Cliquer sur <b>“Ajouter un accès direct”</b>, repassez en revu les détails et cliquer <b>“Sauver”</b>.</p> </li> </ol> ",
'mainPanelDirectLoginBlockLabel':	"Accès directs",
'directLinkReferenceShowButtonLabel':	"voir",
'mainPanelDirectLoginBlockDescription':	"<p>Ajouter une “accès direct” pour vous identifier sans avoir besoin de taper vos identifiants de connexions!</p> <p>Les “accès directs” augmente considérablement la sécurité de vos mots passes dans la mesure où vous pouvez:</p> <ul> <li> <p>adopter et saisir des mots de passe complexes;</p> </li> <li> <p>ne réutilisez le même mot de passe et ne mettez plus de mots de passe facile à deviner.</p> </li> </ul> <p>Simple et rapide à configurer avec le Clipperz <b>bookmarklet</b>.</p> <a href=\"http://www.clipperz.com/support/user_guide/direct_logins\" target=\"_blank\">Pour en savoir plus sur les “accès directs”</a> ",
'mainPanelRecordsBlockLabel':	"Cartes",
'mainPanelAddRecordButtonLabel':	"Ajouter une nouvelle carte",
'mainPanelRemoveRecordButtonLabel':	"Supprimer une carte",
'mainPanelRecordFilterBlockAllLabel':	"tous",
'mainPanelRecordFilterBlockTagsLabel':	"tags",
'mainPanelRecordFilterBlockSearchLabel':	"chercher",
'recordDetailNoRecordAtAllTitle':	"Bienvenue sur Clipperz!",
'recordDetailNoRecordAtAllDescription':	"<h5>Commencez par ajouter des cartes à votre compte.</h5> <p>Les cartes sont des formulaires flexibles ou vous pouvez enregistrer vos mots de passe et autres informations confidentielles.</p> <p>Par exemple, une carte peut contenir les informations d\"identification pour un site web, la combinaison de votre antivol de bicyclette, les détails de votre carte bancaire, ...</p> <h5>N'oubliez pas le bookmarklet!</h5> <p>Avant de commencer, installez le bookmarklet “Ajouter à Clipperz”: il rends la création de carte plus facile et amusante.</p> <p>Allez à l'onglet “outils” pour découvrir comment l'installer et l'utiliser.</p> <p> </p> <p>Ensuite, cliquez tout simplement sur le bouton “Ajouter une nouvelle carte” et profitez de votre compte Clipperz.</p> <p> </p> <a href=\"http://www.clipperz.com/support/user_guide/managing_cards\" target=\"_blank\">En savoir plus sur la création et la gestion des cartes.</a> ",
'newRecordWizardTitleBox':	"<h5>Choisissez un modèle</h5> <p>Les cartes sont des formulaires flexibles ou vous pouvez enregistrer vos mots de passe et autres informations confidentielles.</p> <p>Commencez par choisir un des modèles ci-dessous. Vous pourrez toujours adapter vos cartes plus tard en ajoutant ou supprimant des champs.</p> ",
'newRecordWizardBookmarkletConfigurationTitle':	"Accès directs",
'newRecordWizardBookmarkletConfigurationDescriptionConfig':	"<p>Collez ici le code de configuration généré par le bookmarlet Clipperz.</p> <p>Cela créera une nouvelle carte avec un accès direct à votre site web.</p> ",
'newRecordWizardCreateButtonLabel':	"Créer",
'newRecordWizardCancelButtonLabel':	"Annuler",
'recordTemplates':	{
	'WebAccount':	{
		'title':	"Mot de passe web",
		'description':	"<p>Une carte toute simple pour enregistrer les informations d'identification de vos services en ligne.</p> ",
		'fields':	{
			'URL':	"Adresse web",
			'TXT':	"Nom d'utilisateur ou email",
			'PWD':	"Mot de passe"
		}
	},
	'BankAccount':	{
		'title':	"Compte bancaire",
		'description':	"<p>Enregistrer de façon sécurisée vos numéros de compte bancaire et vos identifiants de service bancaire en ligne.</p> ",
		'fields':	{
			'TXT':	"Banque",
			'TXT':	"N° de compte",
			'URL':	"Adresse web du site",
			'TXT':	"Identifiant",
			'PWD':	"Mot de passe"
		}
	},
	'CreditCard':	{
		'title':	"Carte bancaire",
		'description':	"<p>Ayez toujours sous la main votre numéro de carte, la date d'expiration, le code de contrôle et le code secret avec Clipperz</p> ",
		'fields':	{
			'TXT':	"Type (Visa, AmEx, ...)",
			'TXT':	"Numéro",
			'TXT':	"Nom du porteur",
			'TXT':	"Date de validité",
			'TXT':	"Code de contrôle",
			'PWD':	"Code secret",
			'URL':	"Site web",
			'TXT':	"Identifiant",
			'PWD':	"Mot de passe"
		}
	},
	'AddressBookEntry':	{
		'title':	"Carnet d'adresse",
		'description':	"<p>Clipperz peux aussi être utilisé comme un nouveau carnet d'adresse privé. Utilisez ce modèle pour ajouter des contacts facilement.</p> ",
		'fields':	{
			'TXT':	"Nom",
			'TXT':	"Adresse eMail",
			'TXT':	"Téléphone",
			'TXT':	"Mobile",
			'ADDR':	"Adresse"
		}
	},
	'Custom':	{
		'title':	"Carte personnalisée",
		'description':	"<p>Quelles que soient les données confidentielles que vous souhaitez protéger, créez une carte personnalisée adaptée à vos besoins.</p> ",
		'fields':	{
			'TXT':	"Titre du champ 1",
			'TXT':	"Titre du champ 2",
			'TXT':	"Titre du champ 3"
		}
	}
},
'recordFieldTypologies':	{
	'TXT':	{
		'description':	"simple text field",
		'shortDescription':	"texte"
	},
	'PWD':	{
		'description':	"simple text field, with default status set to hidden",
		'shortDescription':	"mot de passe"
	},
	'URL':	{
		'description':	"simple text field in edit mode, that became an active url in view mode",
		'shortDescription':	"adresse de site web"
	},
	'DATE':	{
		'description':	"a value set with a calendar helper",
		'shortDescription':	"date"
	},
	'ADDR':	{
		'description':	"just like the URL, but the active link points to Google Maps (or similar service) passing the address value as argument",
		'shortDescription':	"adresse"
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
'newRecordPanelGeneralExceptionTitle':	"Erreur",
'newRecordPanelGeneralExceptionMessage':	"Le code de configuration n'est pas valide. Copiez le depuis la fenêtre du bookmarklet, et essayez à nouveau.",
'newRecordPanelWrongBookmarkletVersionExceptionTitle':	"Erreur",
'newRecordPanelWrongBookmarkletVersionExceptionMessage':	"Ce code de configuration provient d'un ancien bookmarklet. Mettez votre bookmarklet à jour, puis essayez de nouveau.",
'newRecordPanelExceptionPanelCloseButtonLabel':	"Annuler",
'mainPanelDeletingRecordPanelConfirmationTitle':	"Suppression de la carte sélectionnée",
'mainPanelDeleteRecordPanelConfirmationText':	"Voulez vous vraiment supprimer la carte sélectionnée?",
'mainPanelDeleteRecordPanelConfirmButtonLabel':	"Oui",
'mainPanelDeleteRecordPanelDenyButtonLabel':	"Non",
'mainPanelDeletingRecordPanelInitialTitle':	"Suppression de la carte",
'mainPanelDeletingRecordPanelCompletedText':	"Terminé",
'deleteRecordPanelCollectRecordDataMessageTitle':	"Supprimer la carte",
'deleteRecordPanelCollectRecordDataMessageText':	"Mise à jour de la liste des cartes",
'deleteRecordPanelEncryptUserDataMessageTitle':	"Supprimer la carte",
'deleteRecordPanelEncryptUserDataMessageText':	"Chiffrement local des en-têtes de carte",
'deleteRecordPanelSendingDataToTheServerMessageTitle':	"Supprimer la carteDelete card",
'deleteRecordPanelSendingDataToTheServerMessageText':	"Transmission des en-têtes chiffrées à Clipperz",
'deleteRecordPanelUpdatingTheInterfaceMessageTitle':	"Supprimer la carte",
'deleteRecordPanelUpdatingTheInterfaceMessageText':	"Mise à jour de l'interface",
'recordDetailNoRecordSelectedTitle':	"Aucune carte sélectionnée",
'recordDetailNoRecordSelectedDescription':	"<p>Veuillez selectionner une carte dans la liste de gauche.</p> ",
'recordDetailLoadingRecordMessage':	"Téléchargement de la carte chiffrée depuis Clipperz",
'recordDetailDecryptingRecordMessage':	"Déchiffrement local des informations de la carte",
'recordDetailLoadingRecordVersionMessage':	"Téléchargement de la dernière version de la carte",
'recordDetailDecryptingRecordVersionMessage':	"Déchiffrement local de la dernière version",
'recordDetailLoadingErrorMessageTitle':	"Erreur lors du téléchargement de la carte",
'recordDetailNotesLabel':	"Notes",
'recordDetailLabelFieldColumnLabel':	"Titre du champ",
'recordDetailDataFieldColumnLabel':	"Données du champ",
'recordDetailTypeFieldColumnLabel':	"Type",
'recordDetailSavingChangesMessagePanelInitialTitle':	"Enregistrement de la carte",
'recordDetailAddFieldButtonLabel':	"Ajouter un champ",
'recordDetailPasswordFieldHelpLabel':	"pour copier le mot de passe, cliquez sur les étoiles, puis Ctrl-C",
'recordDetailPasswordFieldScrambleLabel':	"dissimuler",
'recordDetailPasswordFieldUnscrambleLabel':	"en clair",
'recordDetailDirectLoginBlockTitle':	"Accès directs",
'recordDetailNewDirectLoginDescription':	"<p>Configuration de l'accès direct</p> ",
'recordDetailDirectLoginBlockNoDirectLoginConfiguredDescription':	"<p>Cette carte contient-elle des informations d'identification pour un service en ligne?</p> <p>Utilisez le bookmarklet pour configurer un “accès direct” depuis Clipperz en un seul click!</p> ",
'recordDetailAddNewDirectLoginButtonLabel':	"Ajouter un accès direct",
'recordDetailEditButtonLabel':	"Modifier",
'recordDetailSaveButtonLabel':	"Sauver",
'recordDetailCancelButtonLabel':	"Annuler",
'newRecordTitleLabel':	"_nouvelle carte_",
'recordSaveChangesPanelCollectRecordInfoMessageTitle':	"Enregistrement de la carte",
'recordSaveChangesPanelCollectRecordInfoMessageText':	"Updating card headers",
'recordSaveChangesPanelEncryptUserDataMessageTitle':	"Sauvegarde de la carte",
'recordSaveChangesPanelEncryptUserDataMessageText':	"Cryptage local des en-têtes chiffrés",
'recordSaveChangesPanelEncryptRecordDataMessageTitle':	"Sauvegarde de la carte",
'recordSaveChangesPanelEncryptRecordDataMessageText':	"Cryptage local des en-têtes chiffrés",
'recordSaveChangesPanelEncryptRecordVersionDataMessageTitle':	"Sauvegarde de la carte",
'recordSaveChangesPanelEncryptRecordVersionDataMessageText':	"Cryptage local des données de version de la carte",
'recordSaveChangesPanelSendingDataToTheServerMessageTitle':	"Sauvegarde de la carte",
'recordSaveChangesPanelSendingDataToTheServerMessageText':	"Téléchargement des cartes d’en-têtes chiffrés depuis Clipperz",
'recordSaveChangesPanelUpdatingTheInterfaceMessageTitle':	"Sauvegarde de la carte",
'recordSaveChangesPanelUpdatingTheInterfaceMessageText':	"Mise à jour de l'interface",
'passwordGeneratorPanelTitle':	"Générateur de mot de passe",
'passwordGeneratorPanelOkLabel':	"Ok",
'passwordGeneratorPanelCancelLabel':	"Annuler",
'passwordGeneratorLengthLabel':	"longueur:",
//'DWRUtilLoadingMessage':	"Chargement des données ...",
'comingSoon':	"Bientôt en ligne ...",
'panelCollectingEntryopyMessageText':	"Rassemblement d'entropie",
'directLoginConfigurationCheckBoxFieldSelectedValue':	"Oui",
'directLoginConfigurationCheckBoxFieldNotSelectedValue':	"Non",

__syntaxFix__: "syntax fix"
});
