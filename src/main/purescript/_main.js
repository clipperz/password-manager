const Main = require ("../../../target/output.purescript/Main")

function addEventBubblingBlockers() {
    const observer = new MutationObserver(mutations => {
        mutations.forEach(function(mutation) {
            for(var i = 0; i < mutation.addedNodes.length; i++)
                mutation.addedNodes.forEach(node => {
                    for (let item of document.forms) {
                        item.addEventListener("submit", ev => ev.preventDefault())
                    }
                    try {
                        if (node.classList.contains("cardForm")) {
                            node.addEventListener("keydown", ev => {
                                ev.stopImmediatePropagation();
                            })
                        } else {
                            for (let item of document.getElementsByClassName("dropFile")) {
                                ["drop", "dragover"].forEach(eventName => item.addEventListener(eventName, ev => { ev.stopPropagation(); ev.preventDefault();} ))
                            }
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
        // console.log(ev.key)
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
        // console.log(document.getElementById("shortcutsHelp").classList)
        document.getElementById("shortcutsHelp").classList.remove("hidden")
        // console.log(document.getElementById("shortcutsHelp").classList)
    })
    Mousetrap.bind("l o c k", function(ev) { // order is important
        document.getElementById("lockButton").dispatchEvent(new MouseEvent("click", {
            bubbles: true,
            cancelable: true,
            view: window,
        }))
    })
}

function addPreventDefaults() {
    /*
        Dragover events default behaviour must be prevented, but doing so in Purescript causes perfomance problems.
    */
    document.addEventListener("dragover", ev => ev.preventDefault())
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
    addEventBubblingBlockers();
    addShortcutsManagement();
    addPreventDefaults();

    let hash = window.location.hash;

    if (hash === "#registration") {
        console.log("registration");
        Main.registration();
    } else if (hash === "#share…") {
        console.log("share");
        Main.share("11111111111111");
    } else {
        console.log("main");
        Main.main();
    }
}

console.log('Starting app - ' + window.location.hash);
main();
