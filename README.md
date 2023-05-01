# Clipperz 

## What is Clipperz
Clipperz is an online password manager; the current application is available [here](https://clipperz.is). Given its age, it is high time to reimplement and improve it. This branch contains the new version of Clipperz, written in Purescript and Scala.

## Epsilon version (ε)
The frontend uses Purescript; the [Concur](https://github.com/purescript-concur) library is used to build the UI, the [Affjax](https://github.com/purescript-contrib/purescript-affjax) library to communicate with the backend and the [subtlecrypto](https://github.com/clipperz/purescript-subtlecrypto/tree/clipperz) library that provides the basic cryptographic functions, creating a wrapping for Purescript of the ones implementd in browsers.

The backend uses Scala, in particular the [ZIO](https://github.com/zio/zio) library in combination with [zio-json](https://github.com/zio/zio-json) and [zio-http](https://github.com/dream11/zio-http) to build the server and communicate with the frontend.

The implementation of SRP v6a is self implemented both in Purescript and Scala.

### Current status - 29/08/2022
In the current stage of development the backend is almost complete. The only feature missing is Hashcash, all the others are present (some still with a basic implementation that will be improved).
Regarding the frontend only the login and signup functionallity are fully working.
Another important thing yet to be implemented is a cryptographical sicure prng.

## License
ALL the code included in this project, if not otherwise stated, is released with the [AGPL v3](http://www.gnu.org/licenses/agpl.html) license (see `LICENSE.txt`), and all rights are reserved to Clipperz Srl. For any use not allowed by the AGPL license, please [contact us][contact] to inquire about licensing options for commercial applications.

## Developing
Necessary tools:
- `NodeJS`: if on a Unix like system, our advice is to use [`nvm`](http://nvm.sh), otherwise you can find [here](https://nodejs.org/en/) the official distribution
- [`Scala 3 + sbt`](https://www.scala-lang.org/download/)

### Building and running the application
All the building and running of the application is managed by sbt, that under the hood uses yarn to manage the Purescript side.
On a console, in the base folder run `sbt` to open the sbt console, after that the command `r` (or `runAll`) builds the whole project, both Scala and Purescript and then starts the Scala server that also serves the frontend at `localhost:8090/index.html`.
 
### Running tests
Command to run inside of the sbt console to execute tests:
- Scala
	- `Test/compile`: compiles the tests
	- `test`: runs all the Scala tests
	- `Test/test {path of the test suit}` (ex: `Test/test is.clipperz.backend.SrpFunctionsConversionsSuite`): run a specific test suite
	- `Test/run`: show a list of all the test suits from which you can choose one to run
- Purescript
	- `testPurescript`: starts a server on `localhost:1234` that runs all the Purescript tests on browser and prints the result in the browser console
- `t` (or `testAll`): run `test` first and `testPurescript` after that. Note that if a Scala test doesn't pass `testPurescript` will not be executed.

### Docker images
```
sbt docker:publishLocal
```

or

```
docker build . --platform=linux/amd64 -t clipperz --build-arg CURRENT_COMMIT_ARG=`git rev-parse HEAD`
```

To run:
```
docker run -p 8080:8080 -v ${PWD}/target/archive/user:/archive/user -v ${PWD}/target/archive/blob:/archive/blob clipperz
```
