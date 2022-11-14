const { Even } = require("../../../target/output.purescript/Data.Int")
const { delay } = require("../../../target/output.purescript/Effect.Aff")
const Main = require ("../../../target/output.purescript/Main")

function addKeyDownEventBlocker() {
    const observer = new MutationObserver(mutations => {
        mutations.forEach(function(mutation) {
            for(var i = 0; i < mutation.addedNodes.length; i++)
                mutation.addedNodes.forEach(node => {
                    try {
                        console.log(node.id)
                        if (node.classList.contains("cardForm")) {
                            console.log("Adding evl to " + node)
                            node.addEventListener("keydown", ev => {
                                console.log("From listener " + ev)
                                ev.stopImmediatePropagation();
                            })
                        } else {
                            document.getElementById("card").addEventListener("keydown", ev => {
                                console.log("From listener' " + ev)
                                ev.stopImmediatePropagation();
                            })
                        }
                    } catch (err) {
                    }
                })
        })
    });
    
    observer.observe(document.body, {
        childList: true,
        subtree: true,
    });
}

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
    addKeyDownEventBlocker();

    const lastFourKeys = Array(4);
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
            } else if (ev.type === "keydown") {
                if (lastFourKeys.push(ev.key) > 4) {
                    lastFourKeys.shift()
                }
                if (lastFourKeys.reduce((a, b) => a + b) === "lock") {
                    document.getElementById("lockButton").dispatchEvent(new MouseEvent("click", {
                        bubbles: true,
                        cancelable: true,
                        view: window,
                      }))
                }
                document.getElementById("cardsManager").dispatchEvent(new KeyboardEvent("keydown", ev))
            }
        }
    }

    Main.main();
}

console.log('Starting app');
main();
