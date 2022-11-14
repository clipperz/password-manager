const { Even } = require("../../../target/output.purescript/Data.Int")
const { delay } = require("../../../target/output.purescript/Effect.Aff")
const Main = require ("../../../target/output.purescript/Main")

function addKeyDownEventBubblingBlocker() {
    const observer = new MutationObserver(mutations => {
        mutations.forEach(function(mutation) {
            for(var i = 0; i < mutation.addedNodes.length; i++)
                mutation.addedNodes.forEach(node => {
                    try {
                        if (node.classList.contains("cardForm")) {
                            node.addEventListener("keydown", ev => {
                                ev.stopImmediatePropagation();
                            })
                        } else {
                            document.getElementById("card").addEventListener("keydown", ev => {
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

function addShortcutsManagement() {
    Mousetrap.bind("/", function(ev) {
        document.getElementById("generalFilter").focus();
        ev.preventDefault();
    })
    Mousetrap.bind("*", function(ev) {
        document.getElementById("generalFilter").value = "";
        document.getElementById("generalFilter").focus();
        document.getElementById("generalFilter").blur();
    })
    Mousetrap.bind("l o c  k", function(ev) {
        document.getElementById("lockButton").dispatchEvent(new MouseEvent("click", {
            bubbles: true,
            cancelable: true,
            view: window,
        }))
    })
    window.document.onkeydown = ev => {
        if (ev.target.nodeName === "BODY") {
            if (ev.type === "keydown") {
                document.getElementById("cardsManager").dispatchEvent(new KeyboardEvent("keydown", ev))
            }
        }
    }
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
    addKeyDownEventBubblingBlocker();
    addShortcutsManagement();

    Main.main();
}

console.log('Starting app');
main();
