"use strict";

var handle = null;

function resetTimer(time) { clearTimeout(handle); handle = setTimer(time)}

function setTimer(time, listeners) {
    return setTimeout(ev => {
        for (const key in listeners) {
            if (Object.hasOwnProperty.call(listeners, key)) {
                document.removeEventListener(key, listeners[key])
            }
        }
        document.getElementById("lockButton").dispatchEvent(new MouseEvent("click", {
            bubbles: true,
            cancelable: true,
            view: window,
        }))
    }, time * 60000)
}

const activateTimer = function(time) {
    return function() {
        // console.log("activate timer of " + time + " minutes")
        const listener = _ => resetTimer(time)
        document.addEventListener("click", listener)
        document.addEventListener("keydown", listener)
        handle = setTimer(time, {"click": listener, "keydown": listener})
        return;
    }
}

const stopTimer = function() {
    clearTimeout(handle);
    return;
}

export {
    activateTimer,
    stopTimer
}
