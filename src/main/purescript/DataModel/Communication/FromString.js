"use strict"

const toDocument = function(str) {
    return function() {
        let doc = new Document();
        let elem = doc.createElement("body");
        elem.innerHTML = str;
        doc.append(elem);
        return doc;
    };
}

export {
    toDocument
}
