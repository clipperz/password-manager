
const Main = require ("../../../target/output.purescript/RedeemMain")

function main () {
    Main.main();
}

console.log('Starting app - ' + window.location.hash);
main();
