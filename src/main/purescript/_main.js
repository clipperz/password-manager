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
    Mousetrap.bind(["a", "s", "d", "w", "left", "up", "right", "down", "esc", "enter"], function(ev) {
        console.log(ev.key)
        if (ev.key === "Escape" && !document.getElementById("shortcutsHelp").classList.contains("hidden")) {
            try {
                document.getElementById("shortcutsHelp").classList.add("hidden")
            } catch (error) {}
        } else {
            if (document.getElementById("cardForm") == null && ev.target.nodeName === "BODY") {
                document.getElementById("cardsManager").dispatchEvent(new KeyboardEvent("keydown", ev))
            }
        }
    })
    Mousetrap.bind("?", function(ev) {
        console.log(document.getElementById("shortcutsHelp").classList)
        document.getElementById("shortcutsHelp").classList.remove("hidden")
        console.log(document.getElementById("shortcutsHelp").classList)
    })
    Mousetrap.bind("l o c k", function(ev) { // order is important
        document.getElementById("lockButton").dispatchEvent(new MouseEvent("click", {
            bubbles: true,
            cancelable: true,
            view: window,
        }))
    })
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
