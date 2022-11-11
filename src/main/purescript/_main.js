const { Even } = require("../../../target/output.purescript/Data.Int")
const { delay } = require("../../../target/output.purescript/Effect.Aff")
const Main = require ("../../../target/output.purescript/Main")

function main () {
    /*
        Here we could add variables such as

        var baseUrl = process.env.BASE_URL;

        Parcel will replace `process.env.BASE_URL`
        with the string contents of the BASE_URL environment
        variable at bundle/build time.
        A .env file can also be used to override shell variables
        for more information, see https://en.parceljs.org/env.html

        These variables can be supplied to the Main.main function.
        However, you will need to change the type to accept variables, by default it is an Effect.
        You will probably want to make it a function from String -> Effect ()
    */
    window.document.onkeydown = ev => {
        console.log(ev)
        if (ev.target.nodeName === "BODY") {
            if (ev.key === "/") {
                document.getElementById("generalFilter").focus()
                ev.preventDefault()
            } else if (ev.key === "*") {
                document.getElementById("generalFilter").value = ""
                document.getElementById("generalFilter").focus()
                document.getElementById("generalFilter").blur()
                ev.preventDefault()
            }
        }
    }

    Main.main();
}

console.log('Starting app');
main();
