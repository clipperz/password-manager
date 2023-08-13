
const Main = require ("../../../target/output.purescript/RedeemMain")

function addEventBubblingBlockers() {
    const observer = new MutationObserver(mutations => {
        mutations.forEach(function(mutation) {
            for (let item of document.forms) {
                item.addEventListener("submit", ev => ev.preventDefault())
            }
        })
    });
    observer.observe(document.body, {
        childList: true,
        subtree: true,
    });
}

function main () {
	addEventBubblingBlockers();
    Main.main();
}

console.log('Starting app - ' + window.location.hash);
main();
