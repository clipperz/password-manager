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

Clipperz.PM.Strings.Languages['ru-RU'.toLowerCase()] = MochiKit.Base.merge(Clipperz.PM.Strings.Languages['en-us'], {
'clipperzServiceDescription':	"<h2>Держи всегда при себе!</h2> <ul> <li> <h3>Clipperz - это:</h3> <ul> <li> <p>безопасный и простой менеджер паролей</p> </li> <li> <p>эффективное решение для единой регистрации</p> </li> <li> <p>цифровое хранилище ваших конфиденциальных данных</p> </li> </ul> </li> <li>\
			<h3>C помощью Clipperz вы можете:</h3> <ul>\
				<li> <p>хранить и управлять вашими паролями</p> </li> <li> <p>входить в любые веб-сервисы без ввода логина и пароля</p> </li> <li> <p>защитить все важные данные: коды охранной сигнализации, PINы, номера кредитных карт, и пр.</p> </li> <li> <p>открыть доступ к паролям вашим членам семьи, друзьям, коллегам</p> </li> </ul> </li> <li>\
			<h3>Преимущества Clipperz:</h3> <ul>\
				<li> <p>бесплатный и абсолютно анонимный</p> </li> <li> <p>доступен в любое время и с любого компьютера</p> </li> <li> <p>не нужно ничего устанавливать</p> </li> <li> <p>не надо хранить секреты на компьютере или бумаге</p> </li> </ul> </li> <li>\
			<h3>Безопасность Clipperz:</h3> <ul>\
				<li> <p>все данные перед отправкой в Clipperz шифруются в браузере</p> </li> <li> <p>пароль знаете только вы</p> </li> <li> <p>Clipperz управляет важными данными в зашифрованном виде и никогда не получит доступ к ним в открытом виде</p> </li> <li> <p>Clipperz основан на стандартных криптографических протоколах: без каких-либо самоделок</p> </li> <li> <p>вы можете просмотреть исходный код, но вам ничего не надо знать о криптографии, чтобы просто использовать его!</p> </li> </ul> </li> <li>\
			<a href=\"http://www.clipperz.com\" target=\"_blank\">Подробнее</a> </li> </ul> ",
'loginFormTitle':	"войти в аккаунт Clipperz",
'loginFormUsernameLabel':	"логин",
'loginFormPassphraseLabel':	"пароль",
'loginFormDontHaveAnAccountLabel':	"у вас еще нет аккаунта?",
'loginFormCreateOneLabel':	"создать сейчас",
'loginFormForgotYourCredentialsLabel':	"забыли пароль?",
'loginFormAarghThatsBadLabel':	"это плохо!",
'loginFormAfraidOfMaliciousScriptsLabel':	"опасаетесь вредоносных скриптов?",
'loginFormVerifyTheCodeLabel':	"проверить",
'loginFormButtonLabel':	"Войти",
'loginFormOneTimePasswordCheckboxLabel':	"использовать одноразовый пароль",
'loginPanelSwithLanguageDescription':	"<h5>Переключить язык</h5> ",
'browserCompatibilityDescription':	"<p>Clipperz адаптирован для Firefox. Но также совместим с Opera и MS Internet Explorer!</p> ",
'OTPloginMessagePanelInitialTitle':	"Вход",
'OTPloginMessagePanelInitialText':	"Передача данных учетной записи ...",
'OTPloginMessagePanelLoadingTitle':	"Вход",
'OTPloginMessagePanelLoadingText':	"Запрос аутентификационных данных с сервера ...",
'OTPloginMessagePanelProcessingTitle':	"Вход",
'OTPloginMessagePanelProcessingText':	"Расшифровка аутентификационных данных",
'loginMessagePanelInitialTitle':	"Вход ...",
'loginMessagePanelInitialButtonLabel':	"Отмена",
'loginMessagePanelConnectedTitle':	"Соединен",
'loginMessagePanelConnectedText':	"Выполнен",
'loginMessagePanelFailureTitle':	"Ошибка",
'loginMessagePanelFailureText':	"Ошибка при попытке входа",
'loginMessagePanelFailureButtonLabel':	"Закрыть",
'connectionLoginSendingCredentialsMessageTitle':	"Проверка учетной записи",
'connectionLoginSendingCredentialsMessageText':	"Передача данных ...",
'connectionLoginCredentialsVerificationMessageTitle':	"Проверка учетной записи",
'connectionLoginCredentialsVerificationMessageText':	"Выполняем SRP-аутентификацию ...",
'connectionLoginDoneMessageTitle':	"Проверка учетной записи",
'connectionLoginDoneMessageText':	"Соединено",
'userLoginPanelUpgradingUserCredentialsMessageTitle':	"Проверка учетной записи",
'userLoginPanelUpgradingUserCredentialsMessageText':	"Обновляем полномочия к новой схеме аутентификации",
'userLoginPanelConnectedMessageTitle':	"Пользователь аутентифицирован",
'userLoginPanelConnectedMessageText':	"Успешный вход",
'userLoginPanelTryingAnOlderConnectionSchemaMessageTitle':	"Проверка учетной записи",
'userLoginPanelTryingAnOlderConnectionSchemaMessageText':	"Пробуем войти по старой схеме",
'userLoginPanelLoadingUserDataMessageTitle':	"Пользователь аутентифицирован",
'userLoginPanelLoadingUserDataMessageText':	"Загрузка зашифрованных заголовков карточек",
'userLoginPanelDecryptingUserDataMessageTitle':	"Пользователь аутентифицирован",
'userLoginPanelDecryptingUserDataMessageText':	"Расшифровка заголовков карточек",
'userLoginPanelDecryptingUserStatisticsMessageTitle':	"Пользователь аутентифицирован",
'userLoginPanelDecryptingUserStatisticsMessageText':	"Расшифровка статистики",
'splashAlertTitle':	"Добро пожаловать в Clipperz!",
'splashAlertText':	"<p>Несколько советов по безопасности:</p> <ul> <li> <p>Хранение данных в Clipperz зависит от того, насколько безопасен выбранный вами пароль</p> </li> <li> <p>Если собираетесь использовать Clipperz для защиты важных или критических данных, то убедитесь, что выбрали сложный пароль</p> </li> <li> <p>Clipperz не сможет восстановить забытый пароль</p> </li> </ul> <p>За дополнительной информацией обратитесь на <a href=\"http://www.clipperz.com\" target=\"_blank\">сайт</a> Clipperz.</p> ",
'splashAlertCloseButtonLabel':	"OK",
'registrationFormTitle':	"создать аккаунт",
'registrationFormUsernameLabel':	"имя пользователя",
'registrationFormPassphraseLabel':	"пароль",
'registrationFormRetypePassphraseLabel':	"повторите пароль",
'registrationFormSafetyCheckLabel':	"Я понимаю, что Clipperz не сможет восстановить забытый пароль",
'registrationFormTermsOfServiceCheckLabel':	"Я прочитал и согласен с <a href='https://www.clipperz.com/terms_service' target='_blank'>Условиями предоставления услуг</a>.",
'registrationFormDoYouAlreadyHaveAnAccountLabel':	"у вас уже есть аккаунт?",
'registrationFormSimplyLoginLabel':	"просто авторизируйтесь",
'registrationFormButtonLabel':	"Зарегистрировать",
'registrationFormWarningMessageNotMatchingPassphrases':	"Пароли не совпадают, пожайлуста, повторите ввод",
'registrationFormWarningMessageSafetyCheckNotSelected':	"Прочитайте и проверьте все поля ниже",
'registrationFormWarningMessageTermsOfServiceCheckNotSelected':	"Вы должны принять Условия предоставления услуг",
'registrationMessagePanelInitialTitle':	"Создание аккаунта",
'registrationMessagePanelInitialButtonLabel':	"Отмена",
'registrationMessagePanelRegistrationDoneTitle':	"Регистрация",
'registrationMessagePanelRegistrationDoneText':	"Выполнено",
'registrationMessagePanelFailureTitle':	"Ошибка регистрации",
'registrationMessagePanelFailureButtonLabel':	"Закрыть",
'connectionRegistrationSendingRequestMessageText':	"Проверка учетной записи",
'connectionRegistrationSendingCredentialsMessageText':	"Передача данных",
'registrationSplashPanelTitle':	"Совет",
'registrationSplashPanelDescription':	"<p>Это ваши данные учетной записи, позаботесь об их безопасности. Clipperz никогда больше не покажет ваш логин и пароль!</p> ",
'registrationSplashPanelUsernameLabel':	"имя пользователя",
'registrationSplashPanelPassphraseLabel':	"пароль",
'registrationSplashPanelShowPassphraseButtonLabel':	"показать пароль",
'donateHeaderLinkLabel':	"помочь проекту",
'creditsHeaderLinkLabel':	"авторы",
'feedbackHeaderLinkLabel':	"обратная связь",
'helpHeaderLinkLabel':	"помощь",
'forumHeaderLinkLabel':	"форум",
'recordMenuLabel':	"карточки",
'accountMenuLabel':	"аккаунт",
'dataMenuLabel':	"данные",
'contactsMenuLabel':	"контакты",
'toolsMenuLabel':	"инструменты",
'logoutMenuLabel':	"выход",
'lockMenuLabel':	"заблокировать",
'lockTitle':	"Аккаунт заблокирован",
'lockDescription':	"<p>Введите пароль для разблокировки</p> ",
'unlockButtonLabel':	"разблокировать",
'changePasswordTabLabel':	"Изменить пароль",
'changePasswordTabTitle':	"Изменить пароль",
'changePasswordFormUsernameLabel':	"логин",
'changePasswordFormOldPassphraseLabel':	"старый пароль",
'changePasswordFormNewPassphraseLabel':	"новый пароль",
'changePasswordFormRetypePassphraseLabel':	"повторите пароль",
'changePasswordFormSafetyCheckboxLabel':	"Я понимаю, что Clipperz не сможет восстановить забытый пароль.",
'changePasswordFormSubmitLabel':	"Изменить",
'changePasswordFormWrongUsernameWarning':	"Неправильный логин",
'changePasswordFormWrongPassphraseWarning':	"Неверный пароль",
'changePasswordFormWrongRetypePassphraseWarning':	"Пароли не совпадают, пожайлуста, повторите ввод.",
'changePasswordFormSafetyCheckWarning':	"Прочитайте и проверьте все поля ниже.",
'changePasswordFormProgressDialogTitle':	"Изменение учетной записи",
'changePasswordFormProgressDialogConnectedMessageTitle':	"Соединено",
'changePasswordFormProgressDialogConnectedMessageText':	"Выполнено",
'changePasswordFormProgressDialogErrorMessageTitle':	"Ошибка",
'changePasswordFormProgressDialogErrorMessageText':	"Ошибка изменения учетной записи!",
'changeCredentialsPanelEncryptingDataMessageTitle':	"Изменение пароля",
'changeCredentialsPanelEncryptingDataMessageText':	"Шифрование заголовков карточек",
'changeCredentialsPanelCreatingNewCredentialsMessageTitle':	"Изменение пароля",
'changeCredentialsPanelCreatingNewCredentialsMessageText':	"Обновление учетной записи",
'changeCredentialsPanelSendingNewCredentialsToTheServerMessageTitle':	"Изменение пароля",
'changeCredentialsPanelSendingNewCredentialsToTheServerMessageText':	"Загрузка данных учетной записи в Clipperz",
'changeCredentialsPanelDoneMessageTitle':	"Изменение пароля",
'changeCredentialsPanelDoneMessageText':	"Выполнено",
'manageOTPTabLabel':	"Управление одноразовыми паролями",
'manageOTPTabTitle':	"Управление одноразовыми паролями",
'manageOTPTabDescription':	"<p>Одноразовый пароль работает так же, как ваш обычный пароль, но может быть использован только один раз.</p> <p>Если один и тот же пароль будет использоваться снова, он будет отклонен при попытке войти.</p> <p>Сразу после входа одноразовый пароль будет удален, чтобы предотвратить любую несанкционированную попытку доступа.</p> <p>Одноразовые пароли - удачный выбор для тех, кто обеспокоен кейлоггерами или spyware, которые могут украсть пароль.</p> <p> <b>Строго рекомендуется использовать одноразовые пароли для доступа к Clipperz с чужих компьютеров, интернет-кафе и компьютеров общего пользования.</b> </p> ",
'oneTimePasswordReadOnlyMessage':	"<h6>Извините!</h6> <p>Вы не можете управлять одноразовыми паролями в автономной версии Clipperz.</p> ",
'oneTimePasswordLoadingMessage':	"<h6>Загрузка данных</h6> <p>Подождите, пожайлуста ...</p> ",
'oneTimePasswordNoPasswordAvailable':	"<h6>Нет доступных одноразовых паролей.</h6> <p>Нажмите кнопку “Создать”, чтобы создать одноразовый пароль.</p> ",
'createNewOTPButtonLabel':	"создать",
'deleteOTPButtonLabel':	"удалить",
'printOTPButtonLabel':	"печать",
'disabledOneTimePassword_warning':	"запрещено",
'oneTimePasswordSelectionLink_selectLabel':	"Выбрать:",
'oneTimePasswordSelectionLink_all':	"все",
'oneTimePasswordSelectionLink_none':	"ни одного",
'oneTimePasswordSelectionLink_used':	"использованные",
'oneTimePasswordSelectionLink_unused':	"неиспользованные",
'saveOTP_encryptUserDataTitle':	"Сохранение одноразовых паролей",
'saveOTP_encryptUserDataText':	"Обработка новых данных учетной записи ...",
'saveOTP_encryptOTPDataTitle':	"одноразовыми паролями",
'saveOTP_encryptOTPDataText':	"Шифрование аутентификационных данных ...",
'saveOTP_sendingDataTitle':	"одноразовыми паролями",
'saveOTP_sendingDataText':	"Передача аутентификационных данных на сервер ...",
'saveOTP_updatingInterfaceTitle':	"одноразовыми паролями",
'saveOTP_updatingInterfaceText':	"Обновление интерфейса ...",
'accountPreferencesLabel':	"Настройки",
'accountPreferencesTabTitle':	"Настройки",
'accountPreferencesLanguageTitle':	"Выбор языка",
'accountPreferencesLanguageDescription':	"<p>Выберите ваш язык из списка.</p> ",
'showDonationReminderPanelTitle':	"Напоминания о пожертвованиях",
'showDonationReminderPanelDescription':	"<p>Показывать напоминания о пожертвованиях</p> ",
'saveUserPreferencesFormSubmitLabel':	"Сохранить",
'cancelUserPreferencesFormSubmitLabel':	"Отмена",
'accountPreferencesSavingPanelTitle_Step1':	"Сохранение настроек",
'accountPreferencesSavingPanelText_Step1':	"Шифрование настроек",
'accountPreferencesSavingPanelTitle_Step2':	"Сохранение настроек",
'accountPreferencesSavingPanelText_Step2':	"Передача зашифрованных настроек в Clipperz",
'accountLoginHistoryLabel':	"История входов",
'loginHistoryTabTitle':	"История входов",
'loginHistoryReadOnlyMessage':	"<h6>Извините!</h6> <p>История входов не доступна в автономной версии Clipperz.</p> ",
'loginHistoryLoadingMessage':	"<h6>Загрузка данных</h6> <p>Подождите, пожайлуста ...</p> ",
'loginHistoryLoadedMessage':	"<h6>Здесь можно посмотреть информацию о последних авторизациях в Clipperz</h6> <p> </p> ",
'loginHistoryIPLabel':	"IP",
'loginHistoryTimeLabel':	"дата",
'loginHistoryCurrentSessionText':	"текущая сессия",
'loginHistoryReloadButtonLabel':	"Обновить историю",
'deleteAccountTabLabel':	"Удалить аккаунт",
'deleteAccountTabTitle':	"Удалить аккаунт",
'deleteAccountFormUsernameLabel':	"логин",
'deleteAccountFormPassphraseLabel':	"пароль",
'deleteAccountFormSafetyCheckboxLabel':	"Я понимаю, что все данные этого аккаунта будут безвозвратно удалены.",
'deleteAccountFormSubmitLabel':	"Удалить аккаунт",
'deleteAccountFormWrongUsernameWarning':	"Неверный логин",
'deleteAccountFormWrongPassphraseWarning':	"Неверный пароль",
'deleteAccountFormSafetyCheckWarning':	"Прочтите и отметьте все поля ниже.",
'accountPanelDeletingAccountPanelConfirmationTitle':	"ВНИМАНИЕ",
'accountPanelDeleteAccountPanelConfirmationText':	"Вы уверены, что хотите удалить аккаунт? Операция необратима.",
'accountPanelDeleteAccountPanelConfirmButtonLabel':	"Да",
'accountPanelDeleteAccountPanelDenyButtonLabel':	"Нет",
'offlineCopyTabLabel':	"Автономная копия",
'offlineCopyTabTitle':	"Автономная копия",
'offlineCopyTabDescription':	"<p>Одним кликом вы можете сохранить все зашифрованные данные с серверов Clipperz на жесткий диск и создать автономную версию, которую будете использовать при отсутствии подключения к Интернету.</p> <p>Версия доступна только для чтения. Она так же безопасна, как и онлайн версия, и не подвергает ваши данные риску, так как использует тот же код и архитектуру.</p> <ol> <li> <p>Нажмите на ссылку ниже, чтобы загрузить ее.</p> </li> <li> <p>Браузер спросит вас, что сделать с файлом “Clipperz_YYYYMMDD.html”. Сохраните его на ваш жесткий диск.</p> </li> <li> <p>Запустите загруженную автономную версию в вашем браузере.</p> </li> <li> <p>Введите, как обычно, свой логин и пароль.</p> </li> </ol> ",
'offlineCopyDownloadLinkLabel':	"Скачать",
'offlineCopyDownloadWarning':	"<h4> <a href=\"#\" id=\"offlineCopyDownloadWarningLink\">Обновите вашу автономную копию!</a> </h4> <p>Вы недавно создали или изменили одну или более карточек: рекомендуется скачать новую автономную копию.</p> ",
'sharingTabLabel':	"Совместное использование",
'sharingTabTitle':	"Совместное использование",
'sharingTabDescription':	"<p>Достаточно часто конфиденциальную информацию нужно предоставить другим людям.</p> <p>Может быть, просто дать коллеге ключ доступа к вашей почте, когда вас нету в офисе, или даже открыть доступ наследникам к вашему счету в местном банке, на случай, если с вами что-нибудь случится.</p> <p>Clipperz поможет сделать совместное использование ваших секретов безопасной и простой процедурой.</p> <p> </p> <p> <b>Раздел в разработке...</b> </p> ",
'importTabLabel':	"Импорт",
'importTabTitle':	"Импорт",
'importTabDescription':	"<p>Вы можете импортировать данные целиком, в ваш аккаунт Clipperz, из файлов различных форматов.</p>",
'printingTabLabel':	"Экспорт",
'printingTabTitle':	"Экспорт",
'printingTabDescription':	"<p> <b>Печать ваших данных</b> </p> <p>Нажмите по ссылке, чтобы открыть новое окно со всеми вашими карточками для печати.</p> <p>Если вы собираетесь распечатать в резервных целях, пожалуйста, рассмотрите более безопасный вариант, как создание “автономной копии”.</p> ",
'printingLinkLabel':	"Версия для печати",
'contactsTabLabel':	"Контакты",
'contactsTabTitle':	"Контакты",
'passwordGeneratorTabLabel':	"Генератор паролей",
'passwordGeneratorTabTitle':	"Генератор паролей",
'passwordGeneratorTabButtonLabel':	"сгенерировать",
'bookmarkletTabLabel':	"Букмарклет",
'bookmarkletTabTitle':	"Букмарклет",
'bookmarkletTabDescription':	"<p>Букмарклет - это простой инструмент, который помещается в браузер, как обычная закладка, и может выполнять различные действия на текущей странице.</p> <p>Букмарклет от Clipperz поможет вам быстро создать новые карточки и быстрый вход внутри существующих карточек, на основе страницы с формой авторизации вашего ресурса.</p> <p> <b>Учтите, что закладка букмарклета не содержит какой-либо информации, связанной с аккаунтом (например, имя или пароль), букмарклет содержит один и тот же код для каждого пользователя.</b> </p> <h3>Как установить букмарклет</h3> <h5>Firefox, Camino, Opera, Safari</h5> <ol> <li> <p>Убедитесь, что панель закладок отображается в вашем браузере.</p> </li> <li> <p>Перетащите ссылку “Добавить в Clipperz” на панель закладок.</p> </li> </ol> <h5>Internet Explorer</h5> <ol> <li> <p>Убедитесь, что отображается панель избранного.</p> </li> <li> <p>Нажмите правой кнопкой на ссылку “Добавить в Clipperz”.</p> </li> <li> <p>Выберите “Добавить в избранное...” из контекстного меню.</p> </li> <li> <p>Нажмите “Да”, для любых сообщений, которые появятся.</p> </li> <li> <p>Выберите папку “Панель избранного” и нажмите “Добавить”.</p> </li> </ol> ",
'bookmarkletTabBookmarkletTitle':	"Добавить в Clipperz",
'bookmarkletTabInstructions':	"<h3>Как создать новую карточку с ссылками “прямого подключения” в онлайновый сервис</h3> <ol> <li> <p>Откройте веб-страницу с формой входа. (обычно на этой страницы вы вводите данные вашей учетной записи)</p> </li> <li> <p>Запустите закладку, нажав на нее: появится всплывающее окно.</p> </li> <li> <p>Скопируйте в буфер обмена содержимое текстового поля из всплывающего окна. (ctrl-C)</p> </li> <li> <p>Войдите в ваш аккаунт и нажмите “Добавить новую карточку”.</p> </li> <li> <p>Выберите шаблон “Прямого подключения” и вставьте в текстовое поле содержимое буфера обмена. (ctrl-V)</p> </li> <li> <p>Нажмите кнопку “Создать”, проверьте правильность и нажмите “Сохранить”.</p> </li> </ol> <h3>Как создать ссылку “Быстрого входа” в существующей карточке</h3> <ol> <li> <p>Тоже самое, как и выше.</p> </li> <li> <p>Тоже самое, как и выше.</p> </li> <li> <p>Тоже самое, как и выше.</p> </li> <li> <p>Войдите в ваш аккаунт и выберите карточку с данными учетной записи для только что посещенного веб-сервиса и нажмите “Редактировать”.</p> </li> <li> <p>Вставьте содержимое буфера обмена в текстовое поле в разделе “Быстрого входа”. (ctrl-V)</p> </li> <li> <p>Нажмите «Добавить новый Быстрый вход», проверьте правильность и нажмите “Сохранить”.</p> </li> </ol> <p> </p> <p>Подробнее о букмарклете <a href=\"http://www.clipperz.com/support/user_guide/bookmarklet\" target=\"_blank\">здесь</a>.</p> ",
'mainPanelDirectLoginBlockLabel':	"Быстрый вход",
'directLinkReferenceShowButtonLabel':	"показать",
'mainPanelDirectLoginBlockDescription':	"<p>Добавьте «Быстрый вход», чтобы срау заходить на веб-сервисы без ввода имени и пароля!</p> <p>«Быстрый вход» значительно усиливает безопасность, так как:</p> <ul> <li> <p>можно удобно использовать большие и сложные пароли.</p> </li> <li> <p>легко пользоваться уникальными паролями на разных ресурсах.</p> </li> </ul> <p>Легко и быстро их можно создать с помощью букмарклета Clipperz.</p> <a href=\"http://www.clipperz.com/support/user_guide/direct_logins\" target=\"_blank\">Подробнее о быстром входе</a> ",
'mainPanelRecordsBlockLabel':	"Карточки",
'mainPanelAddRecordButtonLabel':	"Добавить новую карточку",
'mainPanelRemoveRecordButtonLabel':	"Удалить карточку",
'mainPanelRecordFilterBlockAllLabel':	"все",
'mainPanelRecordFilterBlockTagsLabel':	"теги",
'mainPanelRecordFilterBlockSearchLabel':	"поиск",
'recordDetailNoRecordAtAllTitle':	"Добро пожаловать в Clipperz!",
'recordDetailNoRecordAtAllDescription':	"<h5>Начните работать, добавив карточку.</h5> <p>Карточки - это простой и гибкий инструмент, с помощью которого вы можете хранить пароли и любую другую информацию.</p> <p>Карточки могут содержать учетные записи для доступа к веб-сайтам, код для замка от велосипеда, данные кредитной карточки и т.д.</p> <h5>Не забывайте о букмарклете!</h5> <p>Перед началом работы установите букмарклет от Clipperz: создание карточек станет простым и быстрым.</p> <p>Откройте раздел \"Инструменты\"->\"Букмарклет\", чтобы узнать, как установить и использовать его.</p> <p> </p> <a href=\"http://www.clipperz.com/support/user_guide/managing_cards\" target=\"_blank\">Подробнее о создании и управлении закладками</a> ",
'newRecordWizardTitleBox':	"<h5>Пожайлуста, выберите шаблон</h5> <p>Карточки - это простой и гибкий инструмент, с помощью которого вы можете хранить пароли и любую другую информацию.</p> <p>Выберите один из шаблонов. Вы всегда сможете настроить ваши карточки, добавляя или удаляя поля.</p> ",
'newRecordWizardBookmarkletConfigurationTitle':	"Быстрый вход",
'newRecordWizardBookmarkletConfigurationDescription':	"<p>Вставьте конфигурационный код, сгенерированный с помощью букмарклета</p> <p>Будет создана новая карточка с поддержкой быстрого входа.</p> ",
'newRecordWizardCreateButtonLabel':	"Создать",
'newRecordWizardCancelButtonLabel':	"Отмена",
'donateSplashPanelTitle':	"Поддержите Clipperz, сделайте пожертвование сегодня!",
'donateSplashPanelDescription':	"<p>Несколько причин сделать пожертвование:</p> <ul> <li> <p>поддержка развития новых функций</p> </li> <li> <p>оставить Clipperz бесплатным</p> </li> <li> <p>показать признательность нашей упорной работе</p> </li> </ul> <p>Для дополнительной информации посетите нашу <a href=\"http://www.clipperz.com/donations\" target=\"_blank\">страницу пожертвований</a>.</p> <p> <b>Готовы пожертвовать?</b> </p> ",
'donateCloseButtonLabel':	"Еще нет",
'donateDonateButtonLabel':	"Да",
	'recordTemplates':	{
		'WebAccount':	{
			'title':	"Интернет аккаунт",
			'description':	"Простая форма для хранения учетной записи в онлайн сервисе."
		},
		'BankAccount':	{
			'title':	"Счет банка",
			'description':	"Безопасное хранение номера вашего счета и учетной записи для онлайн доступа."
		},
		'CreditCard':	{
			'title':	"Кредитная карта",
			'description':	"Номер карты, срок действия, CCV2 и PIN будут всегда при вас."
		},
		'AddressBookEntry':	{
			'title':	"Запись адресной книги",
			'description':	"Clipperz может также работать, как записная книга. Используйте этот шаблон, чтобы легко добавить новую запись."
		},
		'Custom':	{
			'title':	"Пользовательская карточка",
			'description':	"Не важно, какие данные нужно защитить, просто создайте карточку."
		}
	},
	'recordFieldTypologies':	{
		'TXT':	{
			'description':	"simple text field",
			'shortDescription':	"текст"
		},
		'PWD':	{
			'description':	"simple text field, with default status set to hidden",
			'shortDescription':	"пароль"
		},
		'URL':	{
			'description':	"simple text field in edit mode, that became an active url in view mode",
			'shortDescription':	"URL"
		},
		'DATE':	{
			'description':	"a value set with a calendar helper",
			'shortDescription':	"дата"
		},
		'ADDR':	{
			'description':	"just like the URL, but the active link points to Google Maps (or similar service) passing the address value as argument",
			'shortDescription':	"адрес"
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
'newRecordPanelGeneralExceptionTitle':	"Ошибка",
'newRecordPanelGeneralExceptionMessage':	"Конфигурационный текст неверен. Убедитесь, что вы взяли его из окна закладки и попробуйте снова.",
'newRecordPanelWrongBookmarkletVersionExceptionTitle':	"Ошибка",
'newRecordPanelWrongBookmarkletVersionExceptionMessage':	"Конфигурационный текст был сгенерирован с помощью старой весии закладок. Пожайлуста, обновите вашу закладку и попробуйте снова.",
'newRecordPanelExceptionPanelCloseButtonLabel':	"Отмена",
'mainPanelDeletingRecordPanelConfirmationTitle':	"Удаление выбранной карточки",
'mainPanelDeleteRecordPanelConfirmationText':	"Вы действительно хотите удалить эту карточку?",
'mainPanelDeleteRecordPanelConfirmButtonLabel':	"Да",
'mainPanelDeleteRecordPanelDenyButtonLabel':	"Нет",
'mainPanelDeletingRecordPanelInitialTitle':	"Удаление выбранной карточки",
'mainPanelDeletingRecordPanelCompletedText':	"Выполнено",
'deleteRecordPanelCollectRecordDataMessageTitle':	"Удаление карточки",
'deleteRecordPanelCollectRecordDataMessageText':	"Обновление списка карточек",
'deleteRecordPanelEncryptUserDataMessageTitle':	"Удаление карточки",
'deleteRecordPanelEncryptUserDataMessageText':	"Шифрование заголовков карточек",
'deleteRecordPanelSendingDataToTheServerMessageTitle':	"Удаление карточки",
'deleteRecordPanelSendingDataToTheServerMessageText':	"Передача зашифрованных заголовков карчточек в Clipperz",
'deleteRecordPanelUpdatingTheInterfaceMessageTitle':	"Удаление карточки",
'deleteRecordPanelUpdatingTheInterfaceMessageText':	"Обновление интерфейса",
'recordDetailNoRecordSelectedTitle':	"Не выбрана карточка",
'recordDetailNoRecordSelectedDescription':	"<p>Пожайлуста, выберите карточку из списка слева.</p> ",
'recordDetailLoadingRecordMessage':	"Загрузка зашифрованных карточек из Clipperz",
'recordDetailDecryptingRecordMessage':	"Расшифровка данных карточек",
'recordDetailLoadingRecordVersionMessage':	"Загрузка последней версии карточкиn",
'recordDetailDecryptingRecordVersionMessage':	"Расшифровка",
'recordDetailLoadingErrorMessageTitle':	"Ошибка при загрузку",
'recordDetailNotesLabel':	"Примечания",
'recordDetailLabelFieldColumnLabel':	"Название поля",
'recordDetailDataFieldColumnLabel':	"Значение поля",
'recordDetailTypeFieldColumnLabel':	"Тип",
'recordDetailSavingChangesMessagePanelInitialTitle':	"Сохранение карточки",
'recordDetailAddFieldButtonLabel':	"Добавить новое поле",
'recordDetailPasswordFieldHelpLabel':	"чтобы скопировать пароль в буфер обмена, нажмите на звездочку, затем Ctrl-C",
'recordDetailPasswordFieldScrambleLabel':	"спрятать",
'recordDetailPasswordFieldUnscrambleLabel':	"показать",
'recordDetailDirectLoginBlockTitle':	"Быстрый вход",
'recordDetailNewDirectLoginDescription':	"<p>Настройка быстрого входа</p> ",
'recordDetailDirectLoginBlockNoDirectLoginConfiguredDescription':	"<p>В этой карточке есть данные для доступа в онлайн сервис?</p> <p>Используйте букмарклет, чтобы добавить «Быстрый вход»!</p> ",
'recordDetailAddNewDirectLoginButtonLabel':	"Добавить новый «Быстрый вход»",
'recordDetailEditButtonLabel':	"Изменить",
'recordDetailSaveButtonLabel':	"Сохранить",
'recordDetailCancelButtonLabel':	"Отмена",
'newRecordTitleLabel':	"_новую карточку_",
'recordSaveChangesPanelCollectRecordInfoMessageTitle':	"Сохранение карточки",
'recordSaveChangesPanelCollectRecordInfoMessageText':	"Обновление заголовков карточек",
'recordSaveChangesPanelEncryptUserDataMessageTitle':	"Сохранение карточки",
'recordSaveChangesPanelEncryptUserDataMessageText':	"Шифрование заголовков карточки",
'recordSaveChangesPanelEncryptRecordDataMessageTitle':	"Сохранение карточки",
'recordSaveChangesPanelEncryptRecordDataMessageText':	"Шифровани данных карточки",
'recordSaveChangesPanelEncryptRecordVersionDataMessageTitle':	"Сохранение карточки",
'recordSaveChangesPanelEncryptRecordVersionDataMessageText':	"Шифрование данных версии карточки",
'recordSaveChangesPanelSendingDataToTheServerMessageTitle':	"Сохранение карточки",
'recordSaveChangesPanelSendingDataToTheServerMessageText':	"Передача зашифрованного заголовка карточки в Clipperz",
'recordSaveChangesPanelUpdatingTheInterfaceMessageTitle':	"Сохранение карточки",
'recordSaveChangesPanelUpdatingTheInterfaceMessageText':	"Обновление интерфейса",
'passwordGeneratorPanelTitle':	"Генератор паролей",
'passwordGeneratorPanelOkLabel':	"OK",
'passwordGeneratorPanelCancelLabel':	"Отмена",
'passwordGeneratorLengthLabel':	"длина:",
'DWRUtilLoadingMessage':	"Загрузка данных ...",
'comingSoon':	"вскоре ...",
'panelCollectingEntryopyMessageText':	"Определение энтропии",
'directLoginConfigurationCheckBoxFieldSelectedValue':	"Да",
'directLoginConfigurationCheckBoxFieldNotSelectedValue':	"Нет",
'WELCOME_BACK':	"Добро пожаловать снова!",
'currentConnectionText':	"Сейчас вы подключились с ip&nbsp;__ip__, очевидно __country__, используя __browser__ на __operatingSystem__.",
'latestConnectionText':	"Последнее соединение было __elapsedTimeDescription__ (__time__) с ip&nbsp;__ip__, очевидно __country__, используя __browser__ на __operatingSystem__.",
'fullLoginHistoryLinkLabel':	"показать полную историю входов",

'exportTabDescription':
	"<h5>JSON экспорт</h5>\
	<p>JSON включает в себя полную информацию про ваши карты, в том числе и конфигурации быстрого входа.</p>\
	<p>Это формат, разработанный для Clipperz, и наиболее удобен для работы с ним. Например, если вам нужно переместить все свои карты на другую учетную запись Clipperz, или если вы хотите восстановить карточку, которая была случайно удалена.</p>\
	<p>Нажмите на ссылку ниже, чтобы начать экспорт.</p>",

'exportLinkLabel':					"JSON экспорт",

'exportDataInProgressDescription':	"<h4>Экспорт, пожалуста, подождите, пока обрабатываются ваши данные…</h4>",

'exportDataDescription':			
	"<h4>Инструкция</h4>\
	<p>Скопируйте текст ниже в любой текстовый редактор и сохраните его. (напр. “clipperz_export_20071217.json”)</p>",
	

'elapsedTimeDescriptions':	{
	'MORE_THAN_A_MONTH_AGO':	"за месяц",
	'MORE_THAN_A_WEEK_AGO':	"за неделю",
	'MORE_THAN_*_WEEKS_AGO':	"за несколько __elapsed__ недель",
	'YESTERDAY':	"вчера",
	'*_DAYS_AGO':	"__elapsed__ дней(-я)",
	'ABOUT_AN_HOUR_AGO':	"за час",
	'*_HOURS_AGO':	"__elapsed__ часов(-а)",
	'JUST_A_FEW_MINUTES_AGO':	"несколько минут",
	'ABOUT_*_MINUTES_AGO':	"около __elapsed__ минут"
},
'unknown_ip':	"неизвестный",

// NEW - Import panel
'importFormats':	{
	'CSV': {
		'label':		"CSV",
		'description':	"<p>Распространенный формат для хранения табличных данных. Большинство менеджеров паролей поддерживают экспорт в этот формат.</p>"
	},
	'Excel': {
		'label':		"Excel",
		'description':	"<p>Популярный формат таблиц от Microsoft. Хранение паролей в Excel очень распространено, но это не рекомендуется.</p>"
	},
	'KeePass': {
		'label':		"KeePass",
		'description':	"<p>TXT файл, созданный менеджером паролей KeePass.</p>"
	},
	'PasswordPlus': {
		'label':		"Password Plus",
		'description':	"<p>CSV файл из Password Plus, менеджер паролей, который часто используется на мобильных устройствах.</p>"
	},
	'Roboform': {
		'label':		"RoboForm",
		'description':	"<p>Специальный HTML файл созданный менеджером Roboform когда он показывает Passcard и Safenotes для печати.</p>"
	},
	'ClipperzExport': {
		'label':		"JSON",
		'description':	"<p>Файл, созданный самим Clipperz в формате JSON. Он содержит всю информацию, содержащуюся в вашей карте, в том числе информацию о быстром входе.</p>"
	}
},

'compactTabLabel': "Компактная версия",
'httpAuthTabLabel': "HTTP авторизация",
'compactTabTitle': "Компактная версия",
'httpAuthTabTitle': "HTTP авторизация",

//	Tools panel - Compact - instructions
'compactTabDescription':				"\
	<!-- 	FIX CSS		DONE!	-->	\
	<p>Clipperz Compact - это специальная версия Clipperz предназначенная, чтобы открываться в боковой панели Firefox.</p>\
	<p>Она создана, чтобы держать быстрый вход всегда рядом. <a href=\"http://www.clipperz.com/support/user_guide/clipperz_compact\", target=\"blank\">Подробнее</a></p>\
	\
	<h3>Как запустить Clipperz Compact в боковой панели</h3>\
	<ol>\
		<li><p>Используйте Firefox! Боковые панели доступны только в Firefox, поэтому вы должны использовать именно его, если вы хотите воспользоваться Clipperz Compact.</p></li>\
		<li>\
			<p>Добавьте следующий URL в закладки Firefox, или просто перетащите на панель закладок.</p>\
			<div id=\"compactLinkBox\"><a href=\"https://www.clipperz.com/beta/index.html?compact\" target=\"_search\">Clipperz Compact</a></div>\
		</li>\
		<li><p>Измените свойства этой закладки, включив опцию “Загружать закладку в боковой панели”.</p></li>\
	</ol>\
	\
	<h5>Clipperz Compact теперь работает и в Opera.</h5>",

//	Tools panel - HTTP authentication - instructions
'httpAuthTabDescription':					"\
	<!-- 	FIX CSS		DONE!	-->	\
	<p>HTTP авторизация - метод, который позволяет браузеру предоставлять имя пользователя и пароль веб-сайту, включая их в адрес сайта (HTTP или HTTPS URL).</p>\
	<p>В настоящее время он редко используется, но она все еще можно встретить на небольших, частных веб-сайтов. Можно определить, что сайт использует HTTP авторизацию, когда браузер отображает всплывающее окно для ввода имени пользователя и пароля.</p>\
	<p>К сожалению, букмарклет Clipperz не работает в веб-сайтах, которые используют HTTP авторизацию. Но вы все равно можете создать быстрый вход</p>\
	\
	<h3>Как создать быстрый вход для сайта, использующего HTTP авторизацию</h3>\
	<ol>\
		<li><p>Сохраните URL сайта, имя пользователя и пароль в новой карточке.</p></li>\
		<li><p>Скопируйте конфигурацию, написанную ниже, и вставьте ее в текстовое поле для конфигурации быстрого входа в созданной карте.</p></li>\
		<li><p>Нажмите <b>Добавить «быстрый вход»</b>, указав URL, имя пользователя и пароль в появившихся полях и нажмите <b>Сохранить</b>.</p></li>\
	</ol>\
	\
	<h5><a href=\"http://support.microsoft.com/kb/834489\" target=\"_blank\">Внимание: Internet Explorer не поддерживает HTTP авторизацию.</a></h5>",


'calendarStrings':	{
	'months':	{
		'0':	"Январь",
		'1':	"Февраль",
		'2':	"Март",
		'3':	"Апрель",
		'4':	"Май",
		'5':	"Июнь",
		'6':	"Июль",
		'7':	"Август",
		'8':	"Сентябрь",
		'9':	"Октябрь",
		'10':	"Ноябрь",
		'11':	"Декабрь"
	},
	'shortMonths':	{
		'0':	"Янв",
		'1':	"Фев",
		'2':	"Мар",
		'3':	"Апр",
		'4':	"Май",
		'5':	"Июн",
		'6':	"Июл",
		'7':	"Авг",
		'8':	"Сен",
		'9':	"Окт",
		'10':	"Ноя",
		'11':	"Дек"
	},
	'days':	{
		'0':	"Воскресенье",
		'1':	"Понедельник",
		'2':	"Вторник",
		'3':	"Среда",
		'4':	"Четверг",
		'5':	"Пятница",
		'6':	"Суббота"
	},
	'shortDays':	{
		'0':	"Вс",
		'1':	"Пн",
		'2':	"Вт",
		'3':	"Ср",
		'4':	"Чт",
		'5':	"Пт",
		'6':	"Сб"
	},
	'amDesignation':	"am",
	'pmDesignation':	"pm"
},

__syntaxFix__: "syntax fix"
});