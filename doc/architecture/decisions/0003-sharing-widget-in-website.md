# 3. Sharing Widget in Website

Date: 2023-08-24


## Status

Accepted


## Context

We want to allow the users to share simple one-time-secrets with each other. We want the action to be simple on the user end, while keeping a high-security level during the whole process.


## Decision

### Positioning

To keep this feature open to everybody, even people without a Clipperz account, we will insert this feature in a standalone page inside the Clipperz website.

### UI/UX

The interface will be very simple: the users will be presented with an area where they can insert their secret, a pre-generated PIN that can be changed with a button and a selection where they can choose how much time the secret must be kept in the server before being deleted.
Once these information are inserted, a button will be clickable to initiate the save of the encrypted secret on the server and create the link to share.

The link will lead to the redeem page that will just contain an input to insert the pin and a button to initiate the retrieving of the secret. Once the pin is inserted and the button pressed, the secret will be redeemed and shown to the user.

#### Integration with password generator

In the password generator widget (also inserted as a standalone page in the website) there will be also the possibility to share the secret generated. In this case the are to insert the secret will be prefilled and disabled, to prevent the user to change it by mistake. This maybe be relevant as the secret could have already be copied and used, therefore is important that it does not change during the sharing process.

### Technical details

#### Frontend

Once the information (secret, pin and duration) are confirmed the secret will be encrypted: firstly a random 32 byte key will be created and then used to encrypt the secret.
A json record containing the encrypted secret, the duration (in milliseconds) and the version will then be sent to to the backend; the response will contain the id with which the secret can be redeemed.
The random key will then be encrypted with the pin and used, alongside the id, to create the link to share.
The link will have the format "`{redeem page url}`/`{id}`#`{encrypted key}`"; the key will be inserted as a fragment in the url so that it will never be sent anywhere and just stay locally in the browser.

On the redeem page, the id and encrypted key will be extracted from the url.
Once the pin is inserted and the confirmation button is clicked, a request to retrieve the secret will be made to the backend using the id, that will return a blob containing the encrypted secret.
The pin will then be used to decrypt the encrypted key and the result will be used to decrypt the secret.
The response will also contain the version, that will allow to decern how to execute the operation mentioned above.
If the pin inserted is not correct an error will be shown.

The choice to perform the request only after the user input and not immediately at the page load is to prevent the secret from being lost, as it will be deleted from the backend as soon as it is returned.

The versioning will allow us to change how the secret is encoded and encrypted and change it easily, while keeping the redeem process working for secrets already saved in the backend.

<!-- For the encryption and decryption the [AES CRT algorithm](https://datatracker.ietf.org/doc/html/rfc3686#:~:text=AES%2DCTR%20encryption%20is%20the,stream%20bits%20are%20simply%20discarded.) will be used. -->

#### Backend

Once the request to save a secret arrives, the duration inside the json record will be converted to a UTC formatted date/time, representing the expiration date; a random UUID will be generated and the whole record will be saved under that id. Finally, a response will be returned containing the UUID.

When a redeem requests arrives, the expiration will be checked and if it is before the current date the secret will be deleted and a Gone response (410) will be returned. If this does not happen, the secret will be returned and then deleted.
In case the secret is not found with the id contained in the request, a Not Found response (404) will be returned.

The backend will not check periodically for the expiration dates of secrets as they take a very small amount of resources.

## Consequences

https://www.clipperz.is/share