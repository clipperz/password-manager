"use strict"

const decodeHTML = function(s) {
    var txt = document.createElement("textarea");
    txt.innerHTML = s;
    return txt.value;
}

export {
    decodeHTML
}
