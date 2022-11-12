"use strict"

const _readBlob = function(ref) {
    return window.blobs[ref];
}

const _readUserCard = function() {
    return JSON.stringify(window.userCard);
}

export {
    _readBlob,
    _readUserCard
}
