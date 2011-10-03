# ![Clipperz icon][icon] Clipperz Password Manager - COMMUNITY EDITION


[Clipperz Community Edition][CE] allows you to host on your own server a web service identical to [Clipperz online password manager][clipperz]. It supports multiple backends (PHP/MySQL, Python/AppEngine, …) and you can contribute your own.

Please note that Clipperz Community Edition is not suitable for mass deployments since it lacks several critical capabilities such as bot protection. If you want to offer Clipperz password manager to a wide audience, please wait for the upcoming Provider Edition.

In any case if you want to use Clipperz’s software in a commercial, for-profit environment, please contact us to inquire about licensing options for commercial applications.

ALL the code included in this project, if not otherwise stated, is released with the **AGPL v.3 license**  (see `LICENSE.txt`), and all rights are reserved to Clipperz Srl.


## Why a Community Edition

But why would you prefer running Clipperz Community Edition from your own server instead of using the free and anonymous online service provided by its developers?

* “Clipperz security architecture is great, but I prefer to store my data on my hardware. I just feel better this way!”
* “Clipperz is cool, but it would be even cooler it it had a Scala backend. I'm going to write it!"
* “I would like to modify the look & feel of Clipperz and embed this powerful password manager within my family intranet.”
* “Clipperz works nicely, but I would love to play with the source code in order to improve feature X and add new features Y and Z.”
* …

Whatever is your motivation, we would love to hear from you about how and where you use Clipperz Community Edition. [Get in contact!][contact]


## Warnings

Please note that Clipperz Community Edition may not be suitable for mass deployments, depending on how robust is the backend you select. 

As an example, the current PHP backend lacks several critical capabilities such as bot protection and cuncurrent sessions management.


## Community Edition vs Workgroup Edition

This Community Edition is aimed to **individual users**, but you may be interested to know that we are working on a **Workgroup Edition** targeting organizations that need a
password manager for their workforce (creating and managing employees accounts, defining groups and sharing policies, ...).

The Workgroup Edition development is quite challenging and, while the overall design is already in place, it will still require a fairly large amount of resources for analysis and implementation.

If your organization is interested in such edition, you can subscribe a "pledge" to sustain its development in exchange for a very generous user license with unlimited upgrades and other benefits. Again, [get in contact][contact]!


## Contributions

Your contributions to Clipperz Community Edition are very welcome! In order to avoid jeopardizing the ownership of the code base, we will require every developer to sign the Clipperz [Contributor Agreement][CA]

This enables a single entity to represent the aggregated code base and gives the community flexibility to act as a whole to changing situations.

The CA establishes a joint copyright assignment in which the contributor retains copyright ownership while also granting those rights to Clipperz Srl. With the CA in place, the aggregated code base within any Clipperz open source project is protected by both the distribution license and copyright law.

Please [download][CA] and review the Contributor Agreement for a complete understanding of its terms and conditions. You may send your signed and completed CA to Clipperz by scanning your completed form and emailing the image or by fax. Please retain a copy for your records. **Thanks!**


## Building

In order to build the deployable version, you need to invoke the following command:

	./scripts/build clean install debug --frontends beta --backends php

The output will be available in the `target` folder, with a separate folder for each build backend (initially this will be just a `php` folder).
The script, invoked with these parameters, will build both the full version (`install` -> index.html) and the debug version (index_debug.html) of the /beta frontend using the PHP backend.

At the moment this is the only combination that works, but this script will be gradually extended to be able to build also the [/gamma frontend][gamma] (whose code is already in the repository) and possibly also other backends (Python AppEngine being the very first candidate, and a Javascript version per node.js another interesting option)


## Installing

### PHP + MySQL backend

At the moment the only backend that the build script can successfully create is the PHP + MySQL one.

#### PHP
Once the project has been successfully build, the application needs to be moved in a location where the web server can run it. Everything that is needed is located into `target/php`.

#### MySQL
The application needs a simple MySQL database; to configure all the credentials to access the previously allocated DB, edit the file found in `php/configuration.php`. You need to edit the file actually used by the web server; this will usually be the one moved into the right place in the previous step.
Once the application is in place, and the DB credentials have been configured, you should initialize the DB itself; in order to do so, just point your browser at the following url: `http://<host>/<path>/php/setup/index.php`.
Here you will find the standard [POG][pog] setup page: it should be enough to click the "POG me up!" button at the bottom of the page, and then the "Process" button on the next page.
The POG interface will allow also a very basic access to the DB data that may be useful to check that the application is actually writing something on the DB (even if you will not be able to make much sense out of the data you will see, as they are all encrypted!)

More information about building the PHP backend may be found in the `doc/install.php.txt` file.


## Disclaimer

The resulting application has not been fully tested, so there may be still problems due to the new build script or some other changes that were done due to the new repository structure. So, for the moment, **use it at your own risk!**

[icon]: http://0.gravatar.com/avatar/2a9fae49ced80a42830a206f88ea1022?size=100
[CE]: http://clipperz.com/open_source/clipperz_community_edition
[clipperz]: http://www.clipperz.com 
[contact]: http://clipperz.com/contact
[CA]: http://www.clipperz.com/open_source/contributor_agreement
[pog]: http://www.phpobjectgenerator.com/