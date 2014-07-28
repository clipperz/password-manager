# Development setup

/delta version has been improved to allow the execution of its code without relyinig on any external server for providing data.
This means that you will be able to test the code and see the effect of your changes right without having to have a backend running; this is even more important now, as the protocol between the client and the server has been updated, while the open-source backends have not been updated yet.

## Build scripts

In order to start using /delta in development mode you need to build the main index.html file; you need to execute this step only if you change either `html/index_template.html` or `properties/delta.properties.json`; the first file contain the basic structure of the index.html file, while the second lists all the files required to build the whole application.

As JSON syntax does not allow comments, we have adopted the convention that string values with the "-- " prefix are filtered out during the build step; this allows to leave entries in the property file, while excluding them from being used by the application.

This is the actual command to execute:

	./scripts/build --frontends delta --backends dev


## SCSS

The build script includes the option to build the CSS file from the `scss/*` files; unfortunatelly, the Python library used to implement this step is sub-par with the official SCSS implementation (done in Ruby).
For this reason, the current script generates the css file, but names it `css/_clipperz.css`.
In order to build the actual `css/clippez.css` file (included in the repository for convenience, but it is just the output of the compilation of the `scss/clipperz.scss` file) you can use any tool supporting the full SCSS specification.
I am using LiveReload application, as this will both compile SCSS files and also automate the reloading of the page when something change in the workarea.

LiveReload can also be configured to create the source-map file to allow seing the correct reference to the scss file into the Inspector.


## Proxy script

In order to let the browser load the files directly from the workare locations, there is a tiny script behaving as a proxy, that just load the resources from the right place in the workarea.
You can execute it with the following command:

	./scripts/dev-proxy
	
This will start an http listener on port 8888; in order to access the application you will have to go to the following url in your browser:

	http://localhost:8888/delta/index.html


## Test data

In order to load data locally, you will have to apply a few simple changes to the `html/index_template.html` file.
If you look near the bottom of that file you will spot a commented out DIV element with the class "testBlock"; you need to enable this component in order to change proxy and start load data locally.

The actual data is stored in the `js/TestData/User.data.js`; if you look inside this file, you will see a JSON like structure with three user data:

    - 'catchAllUser'
	- 'f527cdd90d0d47f8524b4e165398ad1455eba515d04abd101d1e93b3c6ae0674' -> joe/clipperz
	- '38d2354c878a06fbdcccc493a23fc6d9be06eebb4f66952bbc1b736824b123f9' -> tag/tag


The first entry is used for completing the authentication procedure when no matching record is found; the other two are actual accounts you can use to login. If you want to add more data, you can simply create a new account on the online version of the application, download the offline copy, and copy/paste the data out of the offline copy and into the `User.data.js` file.


## Automatic login

In order to streamline the development as much as possible, it is possible to automate the login phase passing username and password as query parameters:

	http://localhost:8888/delta/index.html?u=joe&p=clipperz
	
Loading the application with this url will automatically login using the joe/clipperz credentials.

In order to enable this behaviour you need to uncomment the final part of the `js/main.js` file; it contains a function called `simulateLogin` and the matching registration for the `loadEvent` callback.


## Making changes

### Editing JS files

Any change you do to a Javascript file already in use by the application will be picked up as soon as you save it (and reaload the browser page; this activity may be automated using something like LiveReload).

### Adding other JS files

If you want to add other JS files you will have to include the reference to the new files in the `properties/delta.properties.json` file (the syntax should be preatty straight forward) and rebuild the index.html file using the build script.

### SCSS files

Any change to the SCSS files (as long as they are included –directly or indirecly– by clipperz.scss) will be visible as soon as you compile `scss/clipperz.scss` and reload the page on the browser. If you are using LiveReload, you can configure it to compile all files and reload the browser whenever you save any changes.

