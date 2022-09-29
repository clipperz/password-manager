
"use strict"

// let appState = '''{"toll":null,"sessionKey":null,"proxy":{"values":["http://localhost:8090"],"tag":"OnlineProxy"},"p":null,"c":null}''';
let appState = ""

const getJsonState = function() {
    return function() {
        // console.log("GET - " + appState);
        return appState;
    }
}

const updateJsonState = function(n) {
    return function() {
        appState = n;
        // console.log("SET - " + appState);
        return appState;
    }
}

export {
    getJsonState,
    updateJsonState
}
