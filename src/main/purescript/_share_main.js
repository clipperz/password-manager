
const Main = require ("../../../target/output.purescript/ShareMain")

function main () {
    Main.main();
}

console.log('Starting app - ' + window.location.hash);
main();
