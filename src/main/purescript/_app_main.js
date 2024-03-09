const Main = require ("../../../target/output.purescript/AppMain")

var Mousetrap = require('../js/Mousetrap');

import "../scss/main.scss";

function addEventBubblingBlockers() {
    const observer = new MutationObserver(mutations => {
        mutations.forEach(function(mutation) {
            for (let item of document.forms) {
                item.addEventListener("submit", ev => ev.preventDefault())
            }
            for(var i = 0; i < mutation.addedNodes.length; i++)
                mutation.addedNodes.forEach(node => {
                    try {
                        document.getElementById("cardForm").addEventListener("keydown", ev => ev.stopImmediatePropagation() )
						// document.getElementById("new-tag").addEventListener("keydown", ev => {if(ev.keyCode == 13) { console.log("Enter pressend"); ev.stopImmediatePropagation() }})
                        document.getElementById("card").addEventListener("keydown", ev => ev.stopImmediatePropagation() )
                        for (let item of document.getElementsByClassName("dropFile")) {
                            ["drop", "dragover"].forEach(eventName => item.addEventListener(eventName, ev => { ev.stopPropagation(); ev.preventDefault();} ))
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

window.addEventListener("dragover",function(e){
	e = e || event;
	e.preventDefault();
  },false);
  window.addEventListener("drop",function(e){
	e = e || event;
	e.preventDefault();
  },false);

function addPreventDefaults() {
    /*
        Dragover events default behaviour must be prevented, but doing so in Purescript causes perfomance problems.
    */
    document.addEventListener("dragover", ev => ev.preventDefault())
}

function debugAnimation() {
	window.operationDelay = 0;
	window.addEventListener("keydown", (e) => {
		if (e.key === "Shift") { window.operationDelay = 2000; }
	})
	window.addEventListener("keyup", (e) => {
		if (e.key === "Shift") { window.operationDelay = 0; }
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
    addEventBubblingBlockers();
    addPreventDefaults();
	debugAnimation();
    Main.main();
}

console.log('Starting app - ' + window.location.hash);
main();
