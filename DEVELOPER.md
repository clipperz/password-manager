# Development setup

/delta version is now able to run in "development mode", that is without relying on an external server for providing data. This allows you to modify the code and see the effect of your changes without connecting to a backend. (this is even more important now that the protocol between the client and the server has been updated, while the open-source backends have not yet been updated.)

Below are the instructions to run Clipperz in "development mode".

## Run build scripts

Once you have an updated local copy of the repository, the first goal is to build the main `index.html` file. You need to execute this step:

* the first time;
* every time you change either `html/index_template.html` or `properties/delta.properties.json`; 
  (the first file contain the basic structure of the index.html file, while the second lists all the files required to build the whole application)

Please note that since JSON syntax does not allow comments, we have adopted the convention that string values with the `-- ` prefix are filtered out during the build step; this allows to leave entries in the property file, while excluding them from being used by the application.

This is the actual command to execute:

	`./scripts/build --frontends delta --backends dev`

## Compile SCSS files

The above build script does not compile style files in the `./frontend/delta/scss/` directory because of some limitations in the Python library currently used. Therefore you are in charge of compiling `./frontend/delta/scss/clipperz.scss` and save it to `./frontend/delta/css/clipperz.css`.

It may be convenient to use a tool like [LiveReload][LR] capable of both compiling SCSS files and also automate the reloading of the page when something changes in the workarea. LiveReload can also be configured to create the source-map file to allow seeing the correct reference to the scss file into the Inspector.

[LR]: http://www.livereload.com


## Activate a proxy

In order to let the browser load the files directly from the workarea locations, there is a tiny script behaving as a proxy, that just load the resources from the right place in the workarea.
You can execute it with the following command:

	`./scripts/dev-proxy`
	
This will start an http listener on port 8888. To access the application just go to:

	http://localhost:8888/delta/index.html


## Test data

In order to load data locally, you need to apply a few changes to the `html/index_template.html` file.
Near the bottom of that file you will spot a commented out DIV element with class "testBlock". Just enable this component to change the proxy and load data locally.

The actual data is stored in `./frontend/delta/js/TestData/User.data.js`. If you look inside this file, you will see a JSON like structure with four user data:

	- 'catchAllUser'
	- 'f527cdd90d0d47f8524b4e165398ad1455eba515d04abd101d1e93b3c6ae0674' -> joe/clipperz
	- '38d2354c878a06fbdcccc493a23fc6d9be06eebb4f66952bbc1b736824b123f9' -> tag/tag
	- '994ea33d94058425c90ddc4efe6776ac692e91361e388c98134f0d0fc2a012d8' -> a/a

The first entry is used for completing the authentication procedure when no matching record is found; the other two are actual accounts you can use to login. If you want to add more data, you can simply create a new account on the online version of the application, download the offline copy, and copy/paste the data out of the offline copy and into the `User.data.js` file.


## Automatic login

In order to streamline the development as much as possible, it is possible to automate the login phase by passing username and password as query parameters:

	http://localhost:8888/delta/index.html?u=joe&p=clipperz
	
Loading the application with this url will automatically log you in using the `joe/clipperz` credentials.  
To enable automatic logins you need to uncomment the final part of the `js/main.js` file; it contains a function called `simulateLogin` and the matching registration for the `loadEvent` callback.


## Making changes

### Editing JS files

Any change you do to a Javascript file already in use by the application will be picked up as soon as you save it and reload the browser page. (this activity may be automated using something like LiveReload)

### Adding other JS files

If you want to add other JS files you will have to include the reference to the new files in the `properties/delta.properties.json` file (the syntax should be pretty straightforward) and rebuild the `index.html` file using the build script.

### SCSS files

Any change to the SCSS files (as long as they are included –directly or indirectly– by clipperz.scss) will be visible as soon as you compile `scss/clipperz.scss` and reload the page on the browser. If you are using LiveReload, you can configure it to compile all files and reload the browser whenever you save any changes.


# bitcoinJS-lib

## bitcoinJS-lib + bip32-utils + bip32-wallet -> browserify
> npm install bitcoinjs-lib
> npm install bip32-utils
> npm install bip32-wallet

> browserify Workarea/clipperz/gcsolaroli\@bitbucket/password-manager/frontend/delta/js/bitcoinjs-lib/npm.js -s bitcoin > Workarea/clipperz/gcsolaroli\@bitbucket/password-manager/frontend/delta/js/bitcoinjs-lib/bitcoinjs-lib-2.1.4.js

## bitcoinJS-lib raw
> browserify -r bitcoinjs-lib:bigi -s bitcoin -d > Workarea/clipperz/gcsolaroli\@bitbucket/password-manager/frontend/delta/js/bitcoinjs-lib/bitcoinjs-lib-2.1.4.js

## BigInteger
> npm install bigi@1.4.0
> browserify -r bigi -s BigInteger > Workarea/clipperz/gcsolaroli\@bitbucket/password-manager/frontend/delta/js/bitcoinjs-lib/bigi-1.4.0.js

## bops-browser
> npm install bops-browser
> browserify -r bops-browser -s bops > Workarea/clipperz/gcsolaroli\@bitbucket/password-manager/frontend/delta/js/bops-browser/bops-browser-0.6.0.js

## buffer
> npm buffer
> browserify -r buffer -s buffer > Workarea/clipperz/gcsolaroli\@bitbucket/password-manager/frontend/delta/js/buffer/buffer-4.3.0.js


## custom browserify packaging
> npm install bitcoinjs-lib@2.2.0
> browserify Workarea/clipperz/gcsolaroli\@bitbucket/password-manager/frontend/delta/js/npm/config.js -s npm > Workarea/clipperz/gcsolaroli\@bitbucket/password-manager/frontend/delta/js/npm/npm.js