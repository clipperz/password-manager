"use strict"

const logTest = function(s) {
    var txt = document.getElementById("test");
    txt.innerHTML += "<span>" + s + "</span>" + "<br>";
	return;
}

export {
    logTest
}
