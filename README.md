# ![Clipperz icon][icon] CLIPPERZ - Online Password Manager

[icon]: ./Icon.png

## What does Clipperz do?

Clipperz is a smart online vault where you can store confidential data without worrying about security. It can be used to save and manage passwords, private notes, burglar alarm codes, credit and debit card details, PINs, software keys, …
Since passwords are the most common type of private information that you need to protect, we have added a great deal of functionality to make Clipperz a great [online password manager][home]. Read more on the [Clipperz website][home].

**Clipperz makes the Internet the most convenient and safe place to keep you most precious and sensitive data.**

[home]: https://clipperz.is

## Why an open source version of Clipperz?

Because we want to enable as many people as possible to play with the very same code that is powering [Clipperz online service][app]. The goal is building trust. Trust in the code, not in its developers!

So we released the frontend code under an open source license. That was not enough. In order to allow anyone not just to inspect the code running in the user browser, but also to analyze the traffic it generates between the client (the user's browser) and the Clipperz server, we also made available several backends that are easy to deploy.  
You can choose among the available backends (PHP/MySQL, Python/AppEngine, …) or [contribute][CA] your own.  
Whatever is your motivation for playing with Clipperz code, we would love to hear from you: [get in contact][contact]!

# Security warning

The open source version of Clipperz is suitable for **testing and educational purposes only**.

As an example, the current PHP backend lacks several critical capabilities such as bot protection and concurrent sessions management, moreover it could be vulnerable to serious threats (SQL injections, remote code execution, ...).

Please note: the actual Clipperz service use a far more robust backend, but the communication protocol between backend and frontend is of course identical.

[app]: https://clipperz.is/app/
[CA]: https://clipperz.is/open_source/contributor_agreement/
[contact]: https://clipperz.is/about/contacts/


## Donations
If you like what Clipperz is building, its openness and its view of cryptography as a powerful tool for liberty, then you may consider making a donation. 

Our favorite payment method is clearly Bitcoin, but you can also send your donation via credit card, Paypal or bank transfer. In all cases there will be no link between your real identity and your encrypted data stored on Clipperz.

**To make your donation visit [this page][donations]. Thanks!**

[donations]: https://clipperz.is/donations


## License
ALL the code included in this project, if not otherwise stated, is released with the [AGPL v3][agpl] license (see `LICENSE.txt`), and all rights are reserved to Clipperz Srl. For any use not allowed by the AGPL license, please [contact us][contact] to inquire about licensing options for commercial applications.

[agpl]: http://www.gnu.org/licenses/agpl.html


## Contributions
Your contributions to Clipperz are very welcome! In order to avoid jeopardizing the ownership of the code base, we will require every developer to sign the Clipperz [Contributor Agreement][CA]

This enables a single entity to represent the aggregated code base and gives the community flexibility to act as a whole to changing situations.

The CA establishes a joint copyright assignment in which the contributor retains copyright ownership while also granting those rights to Clipperz Srl. With the CA in place, the aggregated code base within any Clipperz open source project is protected by both the distribution license and copyright law.

Please [download][CA] and review the Contributor Agreement for a complete understanding of its terms and conditions. You may send your signed and completed CA to Clipperz by scanning your completed form and emailing the image or by fax. Please retain a copy for your records. **Thanks!**


## Building
In order to build the deployable version, you need to invoke the following command:

    git clone https://github.com/clipperz/password-manager.git
    cd password-manager
    ./scripts/build install --backends php python --frontends delta
  
The output will be available in the `target` folder, with a separate folder for each backend (currently the available options are `php` and `python`).
The script, invoked with these parameters, will build both the full version (`install` -> index.html) and the debug version (index_debug.html) of the specified frontends.

Besides PHP and Python, more backends are in the works, most notably a node.js version.


### Developing
To support the development of the application, a few extra tools have been built, the most useful one being `dev-proxy`.
This script, located in `scripts/dev-proxy`, is invoked without any parameters (to simplify daily usage) and all its configurations are hard coded into the actual code it executes: `scripts/proxy/main.py`.

The aim of this proxy is to mask the actual location of frontend JS files from the actual backend handling requests.

When executed, this script will start listening on localhost:8888.

All 'backend' requests (whose path starts with either `/json` or `/dump`) will be forwarded to the actual backend, that is configured as a `ReverseProxyResource` (in the current code: `proxy.ReverseProxyResource('localhost', 8084, '/java-backend')`).
All other requests (html files, Javascript code, CSS stylesheets and other resources) will be handled by reading the resource from the filesystem; the proxy is aware of the layout of the project, so it will be able to locate the right resource in the right place.

The only file that needs to be `build`, and not read directly from the file system, is the `index.html` file.

In order to build this file, the following command should be executed:

  ./scripts/build --frontends delta --backends dev


Once the index.html file has been built and a backend is running and has been correctly configured in the proxy script, it is possible to access the application at the following URL:

- `http://localhost:8888/delta/index.html`


## Installing

### PHP + MySQL backend

* **PHP**
  Once the project has been successfully build, the application needs to be moved in a location where the web server can run it. Everything that is needed is located into `target/php`.

* **MySQL**
  The application needs a simple MySQL database; to configure all the credentials to access the previously allocated DB, edit the file found in `php/configuration.php`. You need to edit the file actually used by the web server; this will usually be the one moved into the right place in the previous step.  
  Once the application is in place, and the DB credentials have been configured, you should initialize the DB itself; in order to do so, just point your browser at the following url: `http://<host>/<path>/php/setup/index.php`.  
  Here you will find the standard [POG][pog] setup page: it should be enough to click the "POG me up!" button at the bottom of the page, and then the "Process" button on the next page.  
  The POG interface will allow also a very basic access to the DB data that may be useful to check that the application is actually writing something on the DB (even if you will not be able to make much sense out of the data you will see, as they are all encrypted!)  
  More information about building the PHP backend may be found in the `doc/install.php.txt` file.


## Disclaimer

This application has not been fully tested, so there may be still problems due to the new build script or to the new repository structure. So, for the moment, **use it at your own risk!**


[pog]: http://www.phpobjectgenerator.com/
