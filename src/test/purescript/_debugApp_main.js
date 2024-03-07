const Main = require ("../../../target/output.purescript/Test.DebugAppMain")

var Mousetrap = require('../../main/js/Mousetrap');

import "../../main/scss/main.scss";

function addShortcutsManagement() {
	Mousetrap.bind('ctrl+alt+m', function(e) {
		document.getElementById("DEBUG_MODIFY").click()
	});
	Mousetrap.bind('command+enter', function(e) {
		document.getElementById("DEBUG_LOAD").click()
	});
}

function main () {
	addShortcutsManagement();
    Main.main();
}

main();