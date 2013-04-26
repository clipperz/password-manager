# ![Clipperz icon][icon] CLIPPERZ - Online Password Manager

[icon]: ./Icon.png

##What does Clipperz do?

Clipperz is an online vault where you can store confidential data without worrying about security. It can be used to save and manage passwords, private notes, burglar alarm codes, credit and debit card details, PINs, software keys, …
Since passwords are the most common type of private information that you need to protect, we have added a great deal of functionality to make Clipperz a great [online password manager][home] thus solving the “password fatigue” problem.

**Clipperz makes the Internet the most convenient and safe place to keep you most precious and sensitive data.**

Read more on the [Clipperz website][home].

[home]: http://www.clipperz.com

## Why an open source version

Because we want to enable as many people as possible to play with our code. So that you can start trusting it, the code not the developers.

In order to allow you to inspect the code and analyze the traffic it generates between client and server, we had to provide an easy way to locally deploy the whole service.

Feel free to host on your machine a web service identical to [Clipperz online password manager][home]. You can choose among **multiple backends** (PHP/MySQL, Python/AppEngine, …) or you can [contribute][CA] your own. 

Whatever is your motivation, we would love to hear from you: [get in contact!][contact]

[CA]: http://www.clipperz.com/open_source/contributor_agreement
[contact]: http://www.clipperz.com/about/contacts


## Donations
If you like what Clipperz is building, its openness and its view of cryptography as a powerful tool for liberty, then you may consider making a donation. 

Our favorite payment method is clearly Bitcoin ([learn why here][why]), but you can also send your donation via credit card, Paypal or bank transfer. In all cases there will be no link between your real identity and your encrypted data stored on Clipperz.

**To make your donation visit [this page][donations]. Thanks!**

[why]: http://www.clipperz.com/pricing/why_bitcoin
[donations]: http://www.clipperz.com/donations


## License
ALL the code included in this project, if not otherwise stated, is released with the [AGPL v3][agpl] license  (see `LICENSE.txt`), and all rights are reserved to Clipperz Srl. For any use not allowed by the AGPL license, please [contact us][contact] to inquire about licensing options for commercial applications.

[agpl]: http://www.gnu.org/licenses/agpl.html


## Warnings
Please note that the open source version of Clipperz Password Manager may not be suitable for mass deployments, depending on how robust is the backend you select. As an example, the current PHP backend lacks several critical capabilities such as bot protection and concurrent sessions management.

## Contributions
Your contributions to Clipperz are very welcome! In order to avoid jeopardizing the ownership of the code base, we will require every developer to sign the Clipperz [Contributor Agreement][CA]

This enables a single entity to represent the aggregated code base and gives the community flexibility to act as a whole to changing situations.

The CA establishes a joint copyright assignment in which the contributor retains copyright ownership while also granting those rights to Clipperz Srl. With the CA in place, the aggregated code base within any Clipperz open source project is protected by both the distribution license and copyright law.

Please [download][CA] and review the Contributor Agreement for a complete understanding of its terms and conditions. You may send your signed and completed CA to Clipperz by scanning your completed form and emailing the image or by fax. Please retain a copy for your records. **Thanks!**


## Building
In order to build the deployable version, you need to invoke the following command:

    git clone git@github.com:clipperz/password-manager.git
    cd password-manager
    ./scripts/build install --backends php python --frontends beta gamma
	
The output will be available in the `target` folder, with a separate folder for each backend (currently the available options are `php` and `python`).
The script, invoked with these parameters, will build both the full version (`install` -> index.html) and the debug version (index_debug.html) of the specified frontends.

Besides PHP and Python, more backends are in the works, most notably a node.js version.


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

The resulting application has not been fully tested, so there may be still problems due to the new build script or some other changes that were done due to the new repository structure. So, for the moment, **use it at your own risk!**


[pog]: http://www.phpobjectgenerator.com/