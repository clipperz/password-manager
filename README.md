# Clipperz Password Manager #

This is the code of [Clipperz][clipperz] [Password Manager][password-manager]

More details 

ALL the code included in this project, if not otherwise stated, is released with
the AGPL licence (see LICENCE.txt), and all rights are reserved to Clipperz src.


## CONTRIBUTIONS ##

Clipperz is very interested in accepting patches in the form of pull requests, but in
order to avoid jeopardizing the ownership of the code base, we will require every
developer to sign Clipperz [Contribution Agreement][contributor-agreement]


## BUILDING ##

In order to build the deployable version, you need to invoke the following command:

	./scripts/build clean install debug --frontends beta --backends php


The output will be available in the `target` folder, with a separate folder for each build backend (initially this will be just a `php` folder).
The script, invoked with these parameters, will build both the full version (`install` -> index.html) and the debug version (index_debug.html) of the /beta frontend using the PHP backend.

At the moment this is the only combination that (kind of) works, but this script will be gradually extended to be able to build also the /gamma frontend (whose code is already in the repository) and possibly also other backends (Python AppEngine being the very first candidate, and a JavaScript version per node.js another interesting option)


## INSTALLING ##

### PHP + MySQL backend ###

At the moment the only backend that the build script can successfully create is the PHP + MySQL one.

#### PHP ####
Once the project has been successfully build, the application needs to be moved in a location where the web server can run it. Everything that is needed is located into `target/php`.

#### MySQL ####
The application needs a simple MySQL database; to configure all the credentials to access the previously allocated DB, edit the file found in `php/configuration.php`. You need to edit the file actually used by the web server; this will usually be the one moved into the right place in the previous step.
Once the application is in place, and the DB credentials have been configured, you should initialize the DB itself; in order to do so, just point your browser at the following url: `http://<host>/<path>/php/setup/index.php`.
Here you will find the standard [POG][pog] setup page: it should be enough to click the "POG me up!" button at the bottom of the page, and then the "Process" button on the next page.
The POG interface will allow also a very basic access to the DB data that may be useful to check that the application is actually writing something on the DB (even if you will not be able to make much sense out of the data you will see, as they are all encrypted!)

More information about bilding the PHP backend may be found in the `doc/install.php.txt` file.


## WARNING ##

The resulting application has not been fully tested, so there may be still problems due to the new build script or some other changes that were done due to the new repository structure. So, for the moment, **use it at your own risk!**


[clipperz]:              http://www.clipperz.com
[password-manager]:      https://www.clipperz.com/beta
[contributor-agreement]: http://www.clipperz.com/open_source/contributor_agreement
[pog]:                   http://www.phpobjectgenerator.com/