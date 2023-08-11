const Main = require ("../../../target/output.purescript/PasswordGeneratorMain")

// import "../scss/main.scss";

function main () {
    Main.main();
}

console.log('Starting app - ' + window.location.hash);
main();
